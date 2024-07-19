// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.build;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.sun.javadoc.ClassDoc;
import com.sun.javadoc.Doc;
import com.sun.javadoc.DocErrorReporter;
import com.sun.javadoc.MethodDoc;
import com.sun.javadoc.RootDoc;
import com.sun.javadoc.Tag;

import nexj.core.meta.Argument;
import nexj.core.meta.Attribute;
import nexj.core.meta.Event;
import nexj.core.meta.GenericType;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Type;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.IndentingXMLWriter;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.XMLUtil;
import nexj.core.util.XMLWriter;
import nexj.core.version.Version;

/**
 * This class implements JavaDoc entry points to generate .meta files from a
 * subset of the presentation layer Java source files. Note: Prints error and
 * warning messages to System.err.
 */
public class PresentationLayerDoclet
{
   // constants

   /**
    * The command line argument to specify output directory
    */
   public final static String OUTPUT_DIR_OPTION = "-metaclassOutput";

   /**
    * This XML file will be generated in the output directory and contain a list
    * of generated metaclasses.
    */
   public final static String METADATA_FILENAME = "presentationLayer.metadata";

   /**
    * The event tag name
    */
   protected final static String EVENT_TAG = "event";

   /**
    * The attribute tag name
    */
   protected final static String ATTRIBUTE_TAG = "attribute";

   /**
    * The description tag name
    */
   protected final static String DESCRIPTION_TAG = "description";

   /**
    * The argument tag name
    */
   protected final static String ARG_TAG = "arg";

   /**
    * The return tag name
    */
   protected final static String RET_TAG = "ret";

   /**
    * The example tag name
    */
   protected final static String EXAMPLE_TAG = "example";

   /**
    * The type alias tag name
    */
   protected final static String TYPE_ALIAS_TAG = "class";

   /**
    * Pattern used to match whitespace
    */
   private final static Pattern WHITESPACE_PATTERN = Pattern.compile("\\s");

   /**
    * Pattern used to match words
    */
   private final static Pattern WORD_PATTERN = Pattern.compile("\\w+");

   /**
    * Matches a newline: \r\n, or \n.  
    */
   private final static String NEWLINE_REGEX = "\r?\n";

   /**
    * The string to use to separate lines in generated comments
    */
   private final static String NEWLINE = "\r\n";

   /**
    * Classes that should be considered part of the presentation layer classes
    * although they may not implement any scripting events.
    */
   private final static Set s_requiredClassSet = new HashHolder();

   static
   {
      s_requiredClassSet.add("nexj.core.controller.ApplicationController");
      s_requiredClassSet.add("nexj.core.controller.FormController");
   }

   // associations

   /**
    * The output directory to write .meta files to.
    */
   protected final File m_outputDir;

   /**
    * String[String]. Map from Java class name to its scripting type alias.
    */
   private Lookup m_typeAliasMap;

   // constructor

   /**
    * Create a new instance with the given output directory.
    * 
    * @param outputDir The output directory to use. When
    *           {@link #process(RootDoc)} is called output will be placed in
    *           this directory.
    */
   public PresentationLayerDoclet(File outputDir)
   {
      m_outputDir = outputDir;
   }

   // operations

   /**
    * Entry point when run as a doclet. Called by JavaDoc. Uses
    * -metaclassOutput.
    * 
    * @see com.sun.javadoc.Doclet#start(RootDoc)
    */
   public static boolean start(RootDoc root)
   {
      String[][] optionArray = root.options();
      String sPath = "";

      for (int i = 0; i < optionArray.length; i++)
      {
         String[] cur = optionArray[i];

         if (PresentationLayerDoclet.OUTPUT_DIR_OPTION.equals(cur[0]))
         {
            sPath = cur[1];
         }
      }

      File outputDir = new File(sPath);

      if (!outputDir.canWrite())
      {
         return false;
      }

      new PresentationLayerDoclet(outputDir).process(root);

      return true;
   }

   /**
    * Called by JavaDoc to validate the user supplied options
    * 
    * @see com.sun.javadoc.Doclet#validOptions(String[][], DocErrorReporter)
    */
   public static boolean validOptions(String[][] optionArray, DocErrorReporter reporter)
   {
      return true;
   }

   /**
    * Called by JavaDoc when parsing arguments
    * 
    * @see com.sun.javadoc.Doclet#optionLength(String)
    */
   public static int optionLength(String sOption)
   {
      if (OUTPUT_DIR_OPTION.equals(sOption))
      {
         return 2;
      }

      return 0;
   }

   /**
    * Process the Java classes that JavaDoc processed: select the subset that
    * are applicable to the scripting layer classes and then generate .meta
    * files in the output directory for that subset.
    * 
    * @param root The DocRoot as supplied by JavaDoc. This contains all
    *           ClassDoc's for the processed packages.
    */
   public void process(RootDoc root)
   {
      final List scriptingClassDocList = findScriptingClasses(root.classes());
      final List treeList = new ArrayList();
      final long lStart = System.currentTimeMillis();

      // Calculate the tree roots in the inheritance hierarchy of only the scripting classes
      // Warning: ~ O(n^3)
      outer: for (Iterator it = scriptingClassDocList.iterator(); it.hasNext();)
      {
         ClassDoc candidate = (ClassDoc)it.next();

         for (Iterator innerIter = treeList.iterator(); innerIter.hasNext();)
         {
            ClassDoc existingRoot = (ClassDoc)innerIter.next();

            if (existingRoot.subclassOf(candidate))
            {
               innerIter.remove();
            }
            else if (candidate.subclassOf(existingRoot))
            {
               continue outer;
            }
         }

         treeList.add(candidate);
      }

      if (System.currentTimeMillis() - lStart > 1000)
      {
         warning(null, "Performance problem: " + getClass().getName());
      }

      for (ListIterator it = treeList.listIterator(); it.hasNext();)
      {
         it.set(new ClassHierarchy((ClassDoc)it.next()));
      }

      FileOutputStream fosStream = null;
      IndentingXMLWriter xmlWriter = null;

      try
      {
         fosStream = new FileOutputStream(new File(m_outputDir, METADATA_FILENAME));
         xmlWriter = new IndentingXMLWriter(new OutputStreamWriter(new BufferedOutputStream(fosStream), XMLUtil.ENCODING));

         m_typeAliasMap = findTypeAliases(scriptingClassDocList);

         startMetadata(xmlWriter);

         for (Iterator it = treeList.iterator(); it.hasNext();)
         {
            ClassHierarchy tree = (ClassHierarchy)it.next();

            tree.calculateMembers(scriptingClassDocList);
            tree.write();
            tree.writeMetadata(xmlWriter);
         }

         endMetadata(xmlWriter);
      }
      catch (IOException ioe)
      {
         error("Unexpected error. Output is not complete", ioe);
      }
      finally
      {
         closeOutput(fosStream, xmlWriter);
      }
   }

   /**
    * Generate the first part of the XML metadata - the part before the
    * ClassRefs
    */
   protected void startMetadata(XMLWriter writer) throws IOException
   {
      writer.openElement("Metadata");
      writer.writeAttribute("name", "NexJ Presentation Layer Documentation");
      writer.writeAttribute("version", Version.RELEASE);
      writer.writeAttribute("namespace", "http://www.nexjsystems.com/ns/presentationLayerDoc");
      writer.writeAttribute("revision", "0");
      writer.closeElement();

      writer.openElement("Classes");
      writer.closeElement();
   }

   /**
    * Generate the last part of the XML metadata - the part after the ClassRefs
    */
   protected void endMetadata(XMLWriter writer) throws IOException
   {
      writer.endElement("Classes");
      writer.endElement("Metadata");
   }

   /**
    * Find classes with event tags.
    * 
    * @param classArray All classes as provided by JavaDoc
    * @return A list of ClassDoc's that have scripting events
    */
   protected List findScriptingClasses(ClassDoc[] classArray)
   {
      List classDocList = new ArrayList();

      for (int i = 0; i < classArray.length; i++)
      {
         ClassDoc classDoc = classArray[i];

         if (isScriptingType(classDoc))
         {
            classDocList.add(classDoc);
         }
      }

      return classDocList;
   }

   /**
    * Get type alias information from the list of classes
    * 
    * @see #TYPE_ALIAS_TAG
    * 
    * @param classDocList A list of ClassDoc
    * @return A map (String[String]) from Java class name to its scripting type
    *         alias.
    */
   protected Lookup findTypeAliases(List classDocList)
   {
      Lookup typeAliasMap = new HashTab();

      for (Iterator iterator = classDocList.iterator(); iterator.hasNext();)
      {
         ClassDoc classDoc = (ClassDoc)iterator.next();

         if (classDoc.tags(TYPE_ALIAS_TAG).length == 1)
         {
            typeAliasMap.put(classDoc.name(), classDoc.tags(TYPE_ALIAS_TAG)[0].text());
         }
      }

      return typeAliasMap;
   }

   /**
    * Determine if the given class has scheme events
    * 
    * @param classDoc The class to check
    * @return Whether the class being examined is a presentation layer scripting
    *         type.
    */
   protected boolean isScriptingType(ClassDoc classDoc)
   {
      if (s_requiredClassSet.contains(classDoc.qualifiedName()))
      {
         return true;
      }

      if (classDoc.tags(DESCRIPTION_TAG).length > 0 || classDoc.tags(TYPE_ALIAS_TAG).length > 0)
      {
         return true;
      }

      MethodDoc[] methodDocArray = classDoc.methods();

      for (int i = 0; i < methodDocArray.length; i++)
      {
         MethodDoc method = methodDocArray[i];

         if (method.tags(EVENT_TAG).length > 0 || method.tags(ATTRIBUTE_TAG).length > 0)
         {
            return true;
         }
      }

      return false;
   }

   /**
    * Add the scripting events defined in the given ClassDoc to the given
    * metaclass.
    * 
    * @param clazz (Output) The metaclass to add events to
    * @param classDoc The class to process. May be an inner class.
    */
   protected void addMembers(Metaclass clazz, ClassDoc classDoc) throws IOException
   {
      MethodDoc[] methodArray = classDoc.methods();

      for (int nMethod = 0; nMethod < methodArray.length; nMethod++)
      {
         addEvents(clazz, methodArray[nMethod]);
         addAttributes(clazz, methodArray[nMethod]);
      }
   }

   /**
    * Add the scripting events defined in the given MethodDoc to the given
    * metaclass.
    * 
    * @param clazz (Output) The metaclass to add events to
    * @param methodDoc The method to process.
    */
   protected void addEvents(Metaclass clazz, MethodDoc methodDoc)
   {
      if (methodDoc.tags(EVENT_TAG).length == 0)
      {
         return;
      }

      Tag[] tagArray = methodDoc.tags();
      ScriptingEvent scriptingEvent = new ScriptingEvent(methodDoc);
      boolean bSeenEvent = false;

      for (int i = 0; i < tagArray.length; i++)
      {
         Tag tag = tagArray[i];

         if (EVENT_TAG.equals(tag.name().substring(1)))
         {
            if (bSeenEvent)
            {
               // Move on to the next event
               scriptingEvent.addToClass(clazz);
               scriptingEvent = new ScriptingEvent(methodDoc);
            }
            else
            {
               bSeenEvent = true;
            }
         }

         if (bSeenEvent)
         {
            scriptingEvent.applyTag(tag);
         }
      }

      scriptingEvent.addToClass(clazz);
   }

   /**
    * Add the scripting attributes defined in the given MethodDoc to the given
    * metaclass.
    * 
    * @param clazz (Output) The metaclass to add events to
    * @param methodDoc The method to process.
    */
   protected void addAttributes(Metaclass clazz, MethodDoc methodDoc)
   {
      Tag[] attributes = methodDoc.tags(ATTRIBUTE_TAG);

      for (int i = 0; i < attributes.length; i++)
      {
         String sBody = attributes[i].text();
         Matcher matcher = WORD_PATTERN.matcher(sBody);

         if (!matcher.find())
         {
            error(methodDoc, "Attribute tag without a name");

            continue;
         }

         Attribute attr = new Attribute(matcher.group());

         if (!matcher.find())
         {
            warning(methodDoc, "Attribute " + attr.getName() + " does not include type");
         }
         else
         {
            attr.setType(new TransientType(matcher.group()));

            int nStart = matcher.end();

            // Optional modifiers
            while (matcher.find())
            {
               String sKeyword = matcher.group().trim();

               if ("readonly".equalsIgnoreCase(sKeyword))
               {
                  attr.setReadOnly(true);
               }
               else if ("static".equalsIgnoreCase(sKeyword))
               {
                  attr.setStatic(true);
               }
               else if ("collection".equalsIgnoreCase(sKeyword))
               {
                  attr.setCollection(true);
               }
               else
               {
                  break;
               }

               nStart = matcher.end();
            }

            // The description is the rest
            attr.setDescription(sBody.substring(nStart).trim());
         }

         clazz.addAttribute(attr);
         attr.setDeclarator(clazz);
      }
   }

   /**
    * Get an alias from the type alias map. If the given name isn't present in
    * the map then the original name is returned.
    * 
    * @param sName The original class name
    * @return Non null string
    */
   protected String getAlias(String sName)
   {
      if (m_typeAliasMap.contains(sName))
      {
         return (String)m_typeAliasMap.get(sName);
      }

      return sName;
   }

   /**
    * Helper method to report a tag warning in the given method
    * 
    * @param doc Entity identifying the location of the error
    * @param sMessage Description of the problem
    */
   protected void warning(Doc doc, String sMessage)
   {
      log(false, doc, sMessage, null);
   }

   /**
    * Helper method to report a tag error in the given method
    * 
    * @param doc Entity identifying the location of the error
    * @param sMessage Description of the problem
    */
   protected void error(Doc doc, String sMessage)
   {
      log(true, doc, sMessage, null);
   }

   /**
    * Helper method to report a exception
    * 
    * @param sMessage Description of the problem
    * @param e Exception caught
    */
   protected void error(String sMessage, Throwable e)
   {
      log(true, null, sMessage, e);
   }

   /**
    * Helper method to report an error
    * 
    * @param doc Entity identifying the location of the error. May be null.
    * @param sMessage Description of the problem
    * @param e Exception caught. May be null.
    */
   protected void error(Doc doc, String sMessage, Throwable e)
   {
      log(true, doc, sMessage, e);
   }

   /**
    * Print an error or warning message to System.err.
    * 
    * @param bError True to indicate an error, false to indicate a warning
    * @param doc Entity identifying the location of the error. May be null.
    * @param sMessage Message to display. Not null
    * @param e Exception to log. May be null.
    */
   protected void log(boolean bError, Doc doc, String sMessage, Throwable e)
   {
      String sPrefix = bError ? "Scripting error: " : "Scripting warning: ";

      if (e != null)
      {
         sMessage += ": " + ObjUtil.getMessage(e);
      }

      if (doc != null)
      {
         System.err.println(sPrefix + doc + ": " + sMessage);
      }
      else
      {
         System.err.println(sPrefix + sMessage);
      }
   }

   /**
    * Closes a writer and its underlying output stream. The writer is closed
    * first.
    * 
    * Warning: if an exception is thrown by both stream and writer then only
    * stream's error will be reported.
    * 
    * @param stream The output stream. May be null 
    * @param writer The writer. May be null
    */
   protected void closeOutput(OutputStream stream, Writer writer)
   {
      try
      {
         try
         {
            if (writer != null)
            {
               writer.close();
            }
         }
         finally 
         {
            if (stream != null)
            {
               stream.close();
            }
         }
      }
      catch (IOException ioe)
      {
         error("Error closing output file", ioe);
      }
   }

   // inner classes

   /**
    * Represents a class hierarchy. Also corresponds to a set of .meta files
    * specified by the hierarchy's members.
    */
   private class ClassHierarchy
   {
      // associations

      /**
       * The class hierarchy's root.
       */
      private final ClassDoc m_root;

      /**
       * Set of ClassDoc's that are descendants of root.
       */
      private final Set m_memberSet = new HashHolder();

      // constructor

      /**
       * Create a class hierarchy with the given root.
       * 
       * @param root The hierarchy root to use.
       */
      public ClassHierarchy(ClassDoc root)
      {
         m_root = root;
      }

      // operations

      /**
       * From the list of classes with scripting layer events select those that
       * are part of this hierarchy.
       * 
       * @param classList List of ClassDoc
       */
      public void calculateMembers(List classList)
      {
         for (Iterator it = classList.iterator(); it.hasNext();)
         {
            ClassDoc candidate = (ClassDoc)it.next();

            if (candidate.subclassOf(m_root))
            {
               m_memberSet.add(candidate);
            }
         }

         // Ensure all classes between the memberSet members and the tree root
         // are included in the memberSet, regardless of whether they have
         // scripting layer events.
         for (Iterator it = m_memberSet.iterator(); it.hasNext();)
         {
            for (ClassDoc classDoc = ((ClassDoc)it.next()).superclass(); classDoc != null && !m_memberSet.contains(classDoc)
               && !classDoc.name().equals("Object"); classDoc = classDoc.superclass())
            {
               m_memberSet.add(classDoc);
            }
         }
      }

      /**
       * Generate .meta output for all the members
       */
      public void write()
      {
         for (Iterator it = m_memberSet.iterator(); it.hasNext();)
         {
            try
            {
               write((ClassDoc)it.next());
            }
            catch (Throwable e)
            {
               error("Unexpected exception", e);
            }
         }
      }

      /**
       * Generate output for the given Java class with Scheme events
       * 
       * @param classDoc The class to process
       */
      private void write(ClassDoc classDoc) throws IOException
      {
         Metaclass clazz;
         Tag[] tagArray;

         // Set up the current class
         clazz = new Metaclass(getAlias(classDoc.name()));
         tagArray = classDoc.tags(DESCRIPTION_TAG);

         // Class @description
         if (tagArray.length > 0)
         {
            clazz.setDescription(tagArray[0].text());

            if (tagArray.length != 1)
            {
               warning(classDoc, "Multiple description tags found for " + clazz.getName());
            }
         }

         if (classDoc.superclass() != null)
         {
            clazz.setBase(new Metaclass(getAlias(classDoc.superclass().name())));

            if (clazz.getName().equals(clazz.getBase().getName()))
            {
               error(classDoc, "Type aliases cause circular hierarchy");
               clazz.setBase(null);
            }
         }

         // Process the methods
         addMembers(clazz, classDoc);

         ClassDoc[] innerTypeArray = classDoc.innerClasses();

         for (int i = 0; i < innerTypeArray.length; i++)
         {
            addMembers(clazz, innerTypeArray[i]);
         }

         innerTypeArray = classDoc.interfaces();

         for (int i = 0; i < innerTypeArray.length; i++)
         {
            addMembers(clazz, innerTypeArray[i]);
         }

         // Export to file
         FileOutputStream fosStream = null;
         IndentingXMLWriter xmlWriter = null;

         try
         {
            fosStream = new FileOutputStream(new File(m_outputDir, clazz.getName() + ".meta"));
            xmlWriter = new IndentingXMLWriter(new OutputStreamWriter(new BufferedOutputStream(fosStream), XMLUtil.ENCODING));

            new XMLMetadataExporter(xmlWriter).exportMetaclass(clazz);
         }
         finally
         {
            closeOutput(fosStream, xmlWriter);
         }
      }

      /**
       * Write ClassRef XML for the tree's members
       */
      public void writeMetadata(XMLWriter writer) throws IOException
      {
         for (Iterator it = m_memberSet.iterator(); it.hasNext();)
         {
            writer.openElement("ClassRef");
            writer.writeAttribute("resource", getAlias(((ClassDoc)it.next()).name()) + ".meta");
            writer.closeEmptyElement();
         }
      }
   }

   /**
    * A collection of data applicable to a single scripting event.
    * 
    * Typically, after instantiation {@link ScriptingEvent#applyTag(Tag)} is
    * called one or more times followed by a call to
    * {@link ScriptingEvent#addToClass(Metaclass)}.
    */
   private class ScriptingEvent
   {
      /**
       * If there is an error with this event then no output will be generated
       * for it, and this field will become false.
       */
      private boolean m_bValid = true;

      /**
       * If there is an example tag it will be stored here, otherwise null.
       */
      private String m_sExample;

      // associations

      /**
       * The current scripting event defined in a tag of method
       */
      private final Event m_event;

      /**
       * A list of String arrays. The first item corresponds to the return value
       * and may be null. If its not null it has at most 2 elements - type and
       * description. The remainder correspond to arguments and have between 1
       * and 3 elements - name, type and description.
       */
      private final List m_argList;

      /**
       * The Java method this Scripting event is defined in. Used when reporting
       * errors.
       */
      private MethodDoc m_methodDoc;

      // constructor

      /**
       * Create a new scripting event. It initially is empty and has its name
       * set to methodDoc's name.
       * 
       * @param methodDoc The Java method that defined this event. Used for
       *           initialization and error reporting.
       */
      public ScriptingEvent(MethodDoc methodDoc)
      {
         m_event = new Event(methodDoc.name());
         m_argList = new ArrayList();
         m_argList.add(null);
         m_methodDoc = methodDoc;
      }

      // operations

      /**
       * Apply the given tag to this event.
       * 
       * @param tag The tag to process. Not necessarily a recognized tag.
       */
      public void applyTag(Tag tag)
      {
         if (!m_bValid)
         {
            return;
         }

         String sTagName = tag.name().substring(1);

         if (EVENT_TAG.equals(sTagName))
         {
            String[] parts = WHITESPACE_PATTERN.split(tag.text());

            if (parts.length > 0 && parts[0].length() > 0)
            {
               m_event.setName(parts[0]);
            }
         }
         else if (ARG_TAG.equals(sTagName))
         {
            String[] parts = WHITESPACE_PATTERN.split(tag.text(), 3);

            if (parts.length == 0)
            {
               error(m_methodDoc, "@arg tag with no name");
               m_bValid = false;

               return;
            }
            else if (parts.length == 1)
            {
               warning(m_methodDoc, "@arg tag with no type");
            }

            m_argList.add(parts);
         }
         else if (RET_TAG.equals(sTagName))
         {
            String[] parts = WHITESPACE_PATTERN.split(tag.text(), 2);

            if (parts.length == 0)
            {
               warning(m_methodDoc, "@ret tag with no type");
            }

            m_argList.set(0, parts);
         }
         else if (DESCRIPTION_TAG.equals(sTagName))
         {
            m_event.setDescription(tag.text());
         }
         else if (EXAMPLE_TAG.equals(sTagName))
         {
            m_sExample = tag.text().replaceAll(NEWLINE_REGEX, NEWLINE); 
         }
      }

      /**
       * Adds this event to the metaclass and generates SchemeDoc in the
       * description field.
       * 
       * @param clazz (Output) The metaclass to add events to
       */
      public void addToClass(Metaclass clazz)
      {
         if (!m_bValid)
         {
            return;
         }

         String sOrigDescription = m_event.getDescription();

         StringBuilder buf = new StringBuilder(sOrigDescription != null ? sOrigDescription + NEWLINE : "");
         Iterator it = m_argList.iterator();
         String[] retTagContents = (String[])it.next();

         while (it.hasNext())
         {
            String[] argTagContents = (String[])it.next();
            String sArgName = argTagContents[0];

            // vararg
            if (sArgName.endsWith("..."))
            {
               m_event.setVarArg(true);
               sArgName = sArgName.substring(0, sArgName.length() - 3);
            }

            try
            {
               m_event.addArgument(new Argument(sArgName));
            }
            catch (Throwable t)
            {
               error(m_methodDoc, "Could not add argument", t);

               return;
            }

            buf.append("@arg ");
            buf.append(sArgName);

            for (int i = 1; i < argTagContents.length; i++)
            {
               buf.append(' ');
               buf.append(argTagContents[i]);
            }

            buf.append(NEWLINE);
         }

         if (retTagContents != null)
         {
            buf.append("@ret");

            for (int i = 0; i < retTagContents.length; i++)
            {
               buf.append(' ');
               buf.append(retTagContents[i]);
            }

            buf.append(NEWLINE);
         }

         if (m_sExample != null)
         {
            buf.append("@example ");
            buf.append(m_sExample);
            buf.append(NEWLINE);
         }

         if (buf.length() > 0)
         {
            m_event.setDescription(buf.substring(0));
         }

         try
         {
            m_event.setDeclarator(clazz);
            clazz.addEvent(m_event);
         }
         catch (Throwable ex)
         {
            error(m_methodDoc, "Could not add event", ex);
         }
      }
   }

   private static class TransientType extends GenericType
   {
      public TransientType(String sName)
      {
         super(sName);
      }

      /**
       * @see nexj.core.meta.Type#getBaseType()
       */
      public Type getBaseType()
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see nexj.core.meta.Type#isPrimitive()
       */
      public boolean isPrimitive()
      {
         throw new UnsupportedOperationException();
      }
   }
}