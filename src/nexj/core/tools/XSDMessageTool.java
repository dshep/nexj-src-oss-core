// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.tools;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataLoaderDispatcher;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.format.xml.NameResolver;
import nexj.core.meta.integration.format.xml.XSDMessageExporter;
import nexj.core.meta.integration.format.xml.XSDMessageImporter;
import nexj.core.meta.integration.service.Interface;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.HashHolder;
import nexj.core.util.IOUtil;
import nexj.core.util.Logger;
import nexj.core.util.URIUtil;
import nexj.core.util.URLUtil;
import nexj.core.util.XMLUtil;

/**
 * Exports/imports Messages to/from XSD and WSDL.
 */
public class XSDMessageTool extends GenericTool
{
   // constants

   /**
    * No operation command.
    */
   protected final static int COMMAND_NOP = 0;

   /**
    * Import XSD to Messages.
    */
   protected final static int COMMAND_IMPORT = 1;

   /**
    * Export Messages to XSD/WSDL.
    */
   protected final static int COMMAND_EXPORT = 2;

   /**
    * Export Channel to XSD/WSDL.
    */
   protected final static int COMMAND_EXPORT_CHANNEL = 3;

   /**
    * Export Interface to XSD/WSDL.
    */
   protected final static int COMMAND_EXPORT_INTERFACE = 4;


   // attributes

   /**
    * The command to execute, one of the COMMAND_* constants.
    */
   protected int m_nCommand = COMMAND_NOP;

   /**
    * An optional prefix to use for output message file names.
    */
   private String m_sOptionalFilePrefix;


   // associations

   /**
    * The items (from the command arguments) to import/export.
    */
   protected List m_itemList;

   /**
    * Directory for .message file output.
    */
   private File m_targetDir;

   /**
    * The metadata repository.
    */
   protected Metadata m_metadata;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The class logger.
    */
   public final static Logger s_logger = Logger.getLogger(XSDMessageTool.class);


   // operations

   /**
    * @see nexj.core.tools.GenericTool#execute(java.lang.String)
    */
   protected void execute(String sCommand) throws Exception
   {
      if (sCommand != null)
      {
         if (m_nCommand == COMMAND_NOP)
         {
            // First argument is always the command
            if ("import".equals(sCommand))
            {
               m_nCommand = COMMAND_IMPORT;
            }
            else if ("export".equals(sCommand))
            {
               m_nCommand = COMMAND_EXPORT;
            }
            else if ("exportchannel".equals(sCommand))
            {
               m_nCommand = COMMAND_EXPORT_CHANNEL;
            }
            else if ("exportinterface".equals(sCommand))
            {
               m_nCommand = COMMAND_EXPORT_INTERFACE;
            }
            else
            {
               throw new IllegalStateException("Unknown command: " + sCommand);
            }
         }
         else
         {
            if (m_itemList == null)
            {
               m_itemList = new ArrayList();
            }

            m_itemList.add(sCommand);
         }
      }
   }

   /**
    * @see nexj.core.tools.GenericTool#end()
    */
   protected void end() throws Exception
   {
      if (m_nCommand == COMMAND_IMPORT)
      {
         if (m_itemList == null)
         {
            executeImport(null);
         }
         else
         {
            for (Iterator itr = m_itemList.iterator(); itr.hasNext(); )
            {
               executeImport((String)itr.next());
            }
         }
      }
      else
      {
         if (m_itemList == null || m_itemList.size() != 1)
         {
            throw new IllegalArgumentException("Must specify *one* metadata object after export command");
         }

         String sMetaName = (String)m_itemList.get(0);
         MetadataObject metaObj;

         m_metadata = new MetadataLoaderDispatcher().load(null, null, XMLMetadataLoader.DOCUMENTATION_INCLUDED, null);
         m_context = (InvocationContext)m_metadata.getComponent("System.InvocationContext").getInstance(null);

         switch (m_nCommand)
         {
            case COMMAND_EXPORT:
               metaObj = m_metadata.getMessage(sMetaName);
               break;

            case COMMAND_EXPORT_CHANNEL:
               metaObj = m_metadata.getChannel(sMetaName);
               break;

            case COMMAND_EXPORT_INTERFACE:
               metaObj = m_metadata.getInterface(sMetaName);
               break;

            default:
               throw new IllegalStateException();
         }

         executeExport(metaObj);
      }
   }

   /**
    * Exports a Message, Interface, or Channel metadata object to an
    * XSD or WSDL file.
    * 
    * @param metaObj The metadata object to export.
    * @throws Exception If an error occurs.
    */
   protected void executeExport(MetadataObject metaObj) throws Exception
   {
      XSDMessageExporter exporter = new XSDMessageExporter(m_context);
      String sFile;
      File file;

      if (hasProperty("xsd.file"))
      {
         sFile = getProperty("xsd.file");
         exporter.setOutputMode(XSDMessageExporter.OUTMODE_XSD);
      }
      else if (hasProperty("wsdl.file"))
      {
         sFile = getProperty("wsdl.file");
         exporter.setOutputMode(XSDMessageExporter.OUTMODE_WSDL);

         if (hasProperty("wsdl.host"))
         {
            exporter.setHostName(getProperty("wsdl.host"));
         }
         else
         {
            exporter.setHostName(URIUtil.getHostPort(m_metadata.getHTTPRoot()));
         }

         if (hasProperty("wsdl.httpRoot"))
         {
            exporter.setContextRoot(getProperty("wsdl.httpRoot"));
         }
         else
         {
            exporter.setContextRoot(m_metadata.getHTTPContextRoot());
         }
      }
      else
      {
         throw new IllegalArgumentException("Missing required property xsd.file or wsdl.file");
      }

      if (URLUtil.isURL(sFile))
      {
         file = URLUtil.fileFromURL(new URL(sFile));
      }
      else
      {
         file = new File(sFile);
      }

      Writer writer = null;

      try
      {
         writer = IOUtil.openBufferedWriter(file, XMLUtil.ENCODING);

         if (metaObj instanceof Channel)
         {
            exporter.export((Channel)metaObj, writer);
         }
         else if (metaObj instanceof Interface)
         {
            exporter.export((Interface)metaObj, writer);
         }
         else if (metaObj instanceof Message)
         {
            exporter.export((Message)metaObj, writer);
         }
         else
         {
            throw new IllegalArgumentException();
         }
      }
      finally
      {
         if (writer != null)
         {
            writer.close();
         }
      }
   }

   /**
    * Imports an XSD or WSDL file to Message metadata.
    * 
    * @param sRootElement Import only this top-level element from the schema; null
    *                     to import all top-level elements from the schema.
    * @throws Exception If an error occurs.
    */
   protected void executeImport(String sRootElement) throws Exception
   {
      String sFile = null;

      if (hasProperty("xsd.file"))
      {
         sFile = getRequiredProperty("xsd.file");
      }
      else if (hasProperty("wsdl.file"))
      {
         sFile = getRequiredProperty("wsdl.file");
      }
      else
      {
         throw new IllegalArgumentException("Missing required property xsd.file or wsdl.file");
      }

      URL xsdURL;

      if (URLUtil.isURL(sFile))
      {
         xsdURL = new URL(sFile);
      }
      else
      {
         File file = new File(sFile);
         
         if (!file.isFile() || !file.exists())
         {
            throw new IllegalArgumentException("Invalid XSD file or URL");
         }

         xsdURL = file.toURL();
      }
      
      m_targetDir = new File(getRequiredProperty("out.dir"));
      
      if (!m_targetDir.isDirectory() || !m_targetDir.exists())
      {
         throw new IllegalArgumentException("Target directory is invalid or does not exist");
      }
      
      m_sOptionalFilePrefix = getProperty("msg.prefix");
      
      Metadata metadata = Repository.getMetadata();

      Message[] messages;
      
      final NameResolver fMsgNameResolver = new NameResolver()
      {
         private Set m_processedNameSet = new HashHolder();

         /**
          * @see nexj.core.meta.integration.format.xml.NameResolver.NameTransformer#transform(java.lang.String)
          */
         protected String transform(String defName)
         {
            return (m_sOptionalFilePrefix == null) ? defName :
               m_sOptionalFilePrefix + defName;
         }

         /**
          * @see nexj.core.meta.integration.format.xml.NameResolver#isValid(java.lang.String)
          */
         protected boolean isValid(String sName)
         {
            return m_processedNameSet.add(sName.toLowerCase(Locale.ENGLISH));
         }
      };
      
      if (sRootElement == null)
      {
         messages = XSDMessageImporter.createMessageParts(xsdURL, metadata, fMsgNameResolver);
      }
      else
      {
         Message part = XSDMessageImporter.createMessagePart(xsdURL, metadata, sRootElement, fMsgNameResolver);
         messages = new Message[] {part};
      }

      importMessages(messages, m_targetDir);
   }

   /**
    * Writes Message metadata to files in the given directory.
    * 
    * @param messages The Messages to write.
    * @param targetDir The directory in which the message files will be created.
    * @throws IOException If an error occurs.
    */
   private void importMessages(Message[] messages, File targetDir) throws IOException
   {
      for (int i = 0; i < messages.length; ++i)
      {
         Message msg = messages[i];
         String sFileName = msg.getName() + ".message";
         File newFile = new File(targetDir, sFileName);

         if (newFile.exists())
         {
            int nSeqNumber = 0;

            do
            {
               ++nSeqNumber;
               sFileName = msg.getName() + nSeqNumber + ".message";
               newFile = new File(targetDir, sFileName);
            }
            while (newFile.exists());
         }

         StringWriter sw = new StringWriter();
         XMLMetadataExporter exporter = new XMLMetadataExporter(sw);
         Writer writer = null;
         
         try
         {
            exporter.exportMessage(msg);
            
            String sContents = sw.toString();

            sw = null;
            sContents = XMLUtil.formatXML(sContents);
            writer = new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(newFile)), XMLUtil.ENCODING);
            writer.write(sContents);
         }
         finally
         {
            if (writer != null)
            {
               writer.close();
            }
         }
      }
   }

   /**
    * @see nexj.core.tools.GenericTool#getOptionUsage()
    */
   protected String[] getOptionUsage()
   {
      return new String[]
      {
         "-Dnexj.meta.url=<repository path>",
         "-Dxsd.file=<XSD file path>",
         "-Dwsdl.file=<WSDL file path, use instead of xsd.file to get WSDL output>",
         "-Dwsdl.host=<endpoint host.domain.ext:port>",
         "-Dwsdl.httpRoot=<endpoing http context root path>",
         "-Dout.dir=<output directory>",
         "-Dmsg.prefix=<message prefix>",
      };
   }

   /**
    * @see nexj.core.tools.GenericTool#getCommandUsage()
    */
   protected String[] getCommandUsage()
   {
      return new String[]
      {
         "import ...  - import message(s). Followed by optional root level elements to process.",
         "export ...  - export message(s). Followed by the messages to export.",
         "exportchannel ... - export channel(s). Followed by the channels to export.",
         "exportinterface ... - export interface(s). Followed by the interfaces to export."
      };
   }

   public static void main(String[] args)
   {
      new XSDMessageTool().run(args);
   }
}
