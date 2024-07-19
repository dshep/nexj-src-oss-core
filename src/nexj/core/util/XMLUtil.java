// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.ref.SoftReference;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.sax.SAXTransformerFactory;
import javax.xml.transform.sax.TransformerHandler;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.apache.xerces.impl.Constants;
import org.apache.xerces.parsers.DOMParser;
import org.apache.xerces.parsers.IntegratedParserConfiguration;
import org.apache.xerces.parsers.SAXParser;
import org.apache.xerces.parsers.XMLGrammarPreparser;
import org.apache.xerces.util.SymbolTable;
import org.apache.xerces.util.XMLGrammarPoolImpl;
import org.apache.xerces.xni.XMLResourceIdentifier;
import org.apache.xerces.xni.XNIException;
import org.apache.xerces.xni.grammars.XMLGrammarDescription;
import org.apache.xerces.xni.parser.XMLEntityResolver;
import org.apache.xerces.xni.parser.XMLErrorHandler;
import org.apache.xerces.xni.parser.XMLInputSource;
import org.apache.xerces.xni.parser.XMLParseException;
import org.apache.xerces.xni.parser.XMLParserConfiguration;
import org.apache.xerces.xni.parser.XMLPullParserConfiguration;
import org.w3c.dom.Attr;
import org.w3c.dom.Comment;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.ContentHandler;
import org.xml.sax.DTDHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.ext.LexicalHandler;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Transformer and parsing functions
 */
public class XMLUtil
{
   // constants

   /**
    * The character encoding to use with XML files.
    */
   public final static String ENCODING = IOUtil.ENCODING;

   /**
    * The XML Schema namespace.
    */
   public final static String XSD_NAMESPACE = "http://www.w3.org/2001/XMLSchema";

   /**
    * The XML Schema instance namespace.
    */
   public final static String XSI_NAMESPACE = "http://www.w3.org/2001/XMLSchema-instance";

   /** Xerces default symbol table size */
   protected final static int SYMBOL_TABLE_SIZE = 4111;

   /**
    * Xerces input buffer size.
    */
   protected final static Integer BUFFER_SIZE = new Integer(8192);

   protected final static String NAMESPACES_FEATURE =
      "http://xml.org/sax/features/namespaces";

   protected final static String VALIDATION_FEATURE =
      "http://xml.org/sax/features/validation";

   protected final static String SCHEMA_VALIDATION_FEATURE =
      "http://apache.org/xml/features/validation/schema";

   protected final static String SCHEMA_FULL_CHECKING_FEATURE =
      "http://apache.org/xml/features/validation/schema-full-checking";

   protected final static String WHITESPACE_INCLUDING_FEATURE =
      "http://apache.org/xml/features/dom/include-ignorable-whitespace";
   
   protected final static String NODE_DEFERRAL_FEATURE =
      "http://apache.org/xml/features/dom/defer-node-expansion";

   protected final static String EXTERNAL_DTD_LOADING_FEATURE =
      "http://apache.org/xml/features/nonvalidating/load-external-dtd";

   protected final static String HONOR_ALL_SCHEMALOCATIONS_FEATURE =
      "http://apache.org/xml/features/honour-all-schemaLocations";

   protected final static String SYMBOL_TABLE_PROPERTY =
       Constants.XERCES_PROPERTY_PREFIX + Constants.SYMBOL_TABLE_PROPERTY;
   
   protected final static String BUFFER_SIZE_PROPERTY =
       Constants.XERCES_PROPERTY_PREFIX + Constants.BUFFER_SIZE_PROPERTY;
   
   protected final static String LEXICAL_HANDLER_PROPERTY =
      Constants.SAX_PROPERTY_PREFIX + Constants.LEXICAL_HANDLER_PROPERTY;

   protected final static String SECURITY_MANAGER_PROPERTY =
      Constants.XERCES_PROPERTY_PREFIX + Constants.SECURITY_MANAGER_PROPERTY;

   /**
    * Value used instead of a null key when doing hash table lookups.
    */
   private final static Object NULL_KEY = Null.VALUE;

   /**
    * Pattern for matching a string consisting only of spaces.
    */
   private final static Pattern s_spacePattern = Pattern.compile("\\s*"); 

   /**
    * Secure entity resolver that throws an exception on any resolution attempt.
    */
   private final static XMLEntityResolver SECURE_RESOLVER = new XMLEntityResolver()
   {
      public XMLInputSource resolveEntity(XMLResourceIdentifier res) throws XNIException, IOException
      {
         if (res.getExpandedSystemId() != null)
         {
            throw new XMLException("err.xml.missingResourceMapping", new Object[]{res.getExpandedSystemId()});
         }

         return null;
      }
   };

   /**
    * The default error handler.
    */
   private final static XMLErrorHandler ERROR_HANDLER = new XMLErrorHandler()
   {
      public void warning(String sDomain, String sKey, XMLParseException e) throws XNIException
      {
      }

      public void error(String sDomain, String sKey, XMLParseException e) throws XNIException
      {
         throw e;
      }

      public void fatalError(String sDomain, String sKey, XMLParseException e) throws XNIException
      {
         throw e;
      }
   };

   /**
    * Empty default handler.
    */
   public final static DefaultHandler DEFAULT_HANDLER = new DefaultHandler()
   {
      /**
       * Ensures that the default behaviour (when no ErrorHandler is provided to the
       * parser) is that schema validation errors will get thrown.
       * @see org.xml.sax.helpers.DefaultHandler#error(org.xml.sax.SAXParseException)
       */
      public void error(SAXParseException e) throws SAXException
      {
         throw e;
      }

      /**
       * @see org.xml.sax.helpers.DefaultHandler#resolveEntity(java.lang.String, java.lang.String)
       */
      public InputSource resolveEntity(String sPublicId, String sSystemId) throws IOException, SAXException
      {
         throw new XMLException("err.xml.missingResourceMapping", new Object[]{sSystemId});
      }
   };

   // associations
   
   /**
    * Map for associating a grammar URL with a 
    * stack of DOM parser instances - List[URL] of SoftReference to DOMParser.
    * The access to this map must be synchronized through the map object itself.
    */
   private final static Lookup s_domParserMap = new HashTab(16);
   
   /**
    * Map for associating a grammar URL with a 
    * stack of SAX parser instances - List[URL] of SoftReference to SAXParser.
    * The access to this map must be synchronized through the map object itself.
    */
   private final static Lookup s_saxParserMap = new HashTab(16);

   // constructors
   
   /**
    * Prevents instantiation.
    */
   protected XMLUtil()
   {
   }
   
   // operations

   /**
    * Converts a null key to a non-null object.
    * @param key The key.
    */
   private static Object getKey(LookupDeque key)
   {
      if (key == null || key.size() == 0)
      {
         return NULL_KEY;
      }

      return new ParserPoolKey(key);
   }

   /**
    * Retrieves a Xerces parser configuration with an optional grammar pool.
    * @param schemaURLDeque The mapping of schema URLs to other URLs that will
    * be used as the actual locations of those schemas, in the schema parse order.
    * @return The parser configuration object.
    */
   private static XMLParserConfiguration getParserConfiguration(LookupDeque schemaURLDeque) throws IOException
   {
      IntegratedParserConfiguration cfg;

      // Load the grammar if specified

      if (schemaURLDeque != null && schemaURLDeque.size() != 0)
      {
         SymbolTable symbolTable = new SymbolTable(SYMBOL_TABLE_SIZE);
         XMLGrammarPreparser preparser = new XMLGrammarPreparser(symbolTable);
         XMLGrammarPoolImpl grammarPool = new XMLGrammarPoolImpl();

         preparser.registerPreparser(XMLGrammarDescription.XML_SCHEMA, null);
         preparser.setGrammarPool(grammarPool);
         preparser.setProperty(BUFFER_SIZE_PROPERTY, BUFFER_SIZE);
         preparser.setFeature(NAMESPACES_FEATURE, true);
         preparser.setFeature(VALIDATION_FEATURE, true);
         preparser.setFeature(SCHEMA_VALIDATION_FEATURE, true);
         preparser.setFeature(NODE_DEFERRAL_FEATURE, false);
         preparser.setErrorHandler(ERROR_HANDLER);
         preparser.setEntityResolver(new MappedResolver(schemaURLDeque));

         for (Iterator itr = schemaURLDeque.valueIterator(); itr.hasNext(); )
         {
            URL xsdURL = (URL)itr.next();
            InputStream istream = URLUtil.openStream(xsdURL);

            try
            {
               preparser.preparseGrammar(XMLGrammarDescription.XML_SCHEMA, 
                  new XMLInputSource(null, xsdURL.toExternalForm(), null, istream, ENCODING));
            }
            finally
            {
               IOUtil.close(istream);
            }
         }

         grammarPool.lockPool();
         cfg = new IntegratedParserConfiguration(symbolTable, grammarPool);
         cfg.setFeature(VALIDATION_FEATURE, true);
         cfg.setFeature(SCHEMA_VALIDATION_FEATURE, true);
      }
      else
      {
         cfg = new IntegratedParserConfiguration(new SymbolTable(SYMBOL_TABLE_SIZE));
      }

      cfg.setFeature(EXTERNAL_DTD_LOADING_FEATURE, false);
      cfg.setErrorHandler(ERROR_HANDLER);
      cfg.setEntityResolver(SECURE_RESOLVER);

      // Enable FEATURE_SECURE_PROCESSING
      cfg.setProperty(SECURITY_MANAGER_PROPERTY, new org.apache.xerces.util.SecurityManager());

      return cfg;
   }

   /**
    * Parses an input XML document to DOM with optional validation.
    * The implementation uses parser pooling.
    * @param source The XMLInputSource - this is Xerces-specific.
    * @param schemaURLDeque The mapping of schema URLs to other URLs that will
    * be used as the actual locations of those schemas, in the schema parse order.
    * @return The resulting DOM.
    */
   private static Document parse(XMLInputSource source, LookupDeque schemaURLDeque)
   {
      DOMParser parser = null;

      // Try to get a parser for the given grammar from the pool

      synchronized (s_domParserMap)
      {
         List parserList = (List)s_domParserMap.get(getKey(schemaURLDeque));

         if (parserList != null)
         {
            while (parserList.size() > 0 && parser == null)
            {
               parser = (DOMParser)((SoftReference)parserList.remove(parserList.size() - 1)).get();
            }
         }
      }

      // Set the class loader, so that Xerces loads its classes dynamically
      // in the same place and class derivation problems are avoided.
      // This allows our Xerces to override the J2EE container one, assuming
      // that the application class loader has been configured to try
      // the parent class loader last.
      ClassLoader classLoaderSaved = Thread.currentThread().getContextClassLoader();
      Thread.currentThread().setContextClassLoader(XMLUtil.class.getClassLoader());

      // Parse the source stream
      Document doc;

      try
      {
         // If the attempt to retrieve a parser failed, create a new one

         if (parser == null)
         {
            parser = new DOMParser(getParserConfiguration(schemaURLDeque));
            parser.setProperty(BUFFER_SIZE_PROPERTY, BUFFER_SIZE);
         }

         parser.parse(source);
         doc = parser.getDocument();
      }
      catch (XMLParseException e)
      {
         throw convertException(e);
      }
      catch (Exception e)
      {
         throw new XMLException("err.xml.parse", e); 
      }
      finally
      {
         // Put the parser back into the pool, using a soft reference

         if (parser != null)
         {
            try
            {
               parser.reset();

               synchronized (s_domParserMap)
               {
                  List parserList = (List)s_domParserMap.get(getKey(schemaURLDeque));

                  if (parserList == null)
                  {
                     parserList = new ArrayList(16);
                     s_domParserMap.put(getKey(schemaURLDeque), parserList);
                  }

                  parserList.add(new SoftReference(parser));
               }
            }
            catch (Exception e)
            {
            }
         }

         Thread.currentThread().setContextClassLoader(classLoaderSaved);
      }

      return doc;
   }

   /**
    * Parses an input XML document to SAX events with optional validation.
    * The implementation uses parser pooling.
    * @param source The XMLInputSource - this is Xerces-specific.
    * @param handler The SAX event handler.
    * @param resolver The entity resolver.
    * @param schemaURLDeque The mapping of schema URLs to other URLs that will
    * be used as the actual locations of those schemas, in the schema parse order.
    */
   private static void parse(XMLInputSource source, Object handler, EntityResolver resolver, LookupDeque schemaURLDeque)
   {
      SAXParser parser = null;
      XMLParserConfiguration config = null;

      // Try to get a parser for the given grammar from the pool

      synchronized (s_saxParserMap)
      {
         List holderList = (List)s_saxParserMap.get(getKey(schemaURLDeque));

         if (holderList != null)
         {
            SAXHolder holder = null;

            while (holderList.size() > 0 && holder == null)
            {
               holder = (SAXHolder)((SoftReference)holderList.remove(holderList.size() - 1)).get();
            }

            if (holder != null)
            {
               parser = holder.getParser();
               config = holder.getConfig();
            }
         }
      }

      ClassLoader classLoaderSaved = Thread.currentThread().getContextClassLoader();
      Thread.currentThread().setContextClassLoader(XMLUtil.class.getClassLoader());

      // Parse the source stream

      try
      {
         // If the attempt to retrieve a parser failed, create a new one

         if (parser == null)
         {
            config = getParserConfiguration(schemaURLDeque);
            parser = new SAXParser(config);
            parser.setProperty(BUFFER_SIZE_PROPERTY, BUFFER_SIZE);
         }

         parser.setContentHandler((handler instanceof ContentHandler) ? (ContentHandler)handler : DEFAULT_HANDLER);
         parser.setErrorHandler((handler instanceof ErrorHandler) ? (ErrorHandler)handler : DEFAULT_HANDLER);
         parser.setDTDHandler((handler instanceof DTDHandler) ? (DTDHandler)handler : DEFAULT_HANDLER);
         parser.setEntityResolver((resolver != null) ? resolver : DEFAULT_HANDLER);

         if (handler instanceof LexicalHandler)
         {
            parser.setProperty(LEXICAL_HANDLER_PROPERTY, handler);
         }

         if (handler instanceof IncrementalHandler && config instanceof XMLPullParserConfiguration)
         {
            IncrementalHandler incrementalHandler = (IncrementalHandler)handler;
            XMLPullParserConfiguration pullConfig = (XMLPullParserConfiguration)config;

            pullConfig.setInputSource(source);
            parser.reset();

            while (!incrementalHandler.isComplete() && pullConfig.parse(false)) ;
         }
         else
         {
            parser.parse(source);
         }
      }
      catch (XMLParseException e)
      {
         throw convertException(e);
      }
      catch (Exception e)
      {
         throw new XMLException("err.xml.parse", e); 
      }
      finally
      {
         // Put the parser back into the pool, using a soft reference

         if (parser != null)
         {
            try
            {
               parser.setContentHandler(DEFAULT_HANDLER);
               parser.setEntityResolver(DEFAULT_HANDLER);
               parser.setErrorHandler(DEFAULT_HANDLER);
               parser.setDTDHandler(DEFAULT_HANDLER);
               parser.setProperty(LEXICAL_HANDLER_PROPERTY, null);
               parser.reset();

               if (config instanceof XMLPullParserConfiguration)
               {
                  ((XMLPullParserConfiguration)config).cleanup();
               }

               synchronized (s_saxParserMap)
               {
                  List holderList = (List)s_saxParserMap.get(getKey(schemaURLDeque));

                  if (holderList == null)
                  {
                     holderList = new ArrayList(16);
                     s_saxParserMap.put(getKey(schemaURLDeque), holderList);
                  }

                  holderList.add(new SoftReference(new SAXHolder(parser, config)));
               }
            }
            catch (Exception e)
            {
            }
         }

         Thread.currentThread().setContextClassLoader(classLoaderSaved);
      }
   }

   /**
    * Converts an XMLParseException from Xerces to an XMLParserException.
    * @param e The XMLParseException to convert.
    * @return The XMLParserException.
    */
   protected static XMLParserException convertException(XMLParseException e)
   {
      String sMsg = ObjUtil.getMessage(e);

      if (sMsg == null)
      {
         sMsg = "";
      }

      int i = sMsg.indexOf(':');

      if (i > 0)
      {
         sMsg = sMsg.substring(i + 1).trim();
      }

      return new XMLParserException(sMsg, e.getLineNumber(), e.getColumnNumber(), e);
   }

   /**
    * Converts a SAXParseException from Xerces to an XMLParserException.
    * @param e The SAXParseException to convert.
    * @return The XMLParserException.
    */
   public static XMLParserException convertException(SAXParseException e)
   {
      String sMsg = ObjUtil.getMessage(e);

      if (sMsg == null)
      {
         sMsg = "";
      }

      int i = sMsg.indexOf(':');

      if (i > 0)
      {
         sMsg = sMsg.substring(i + 1).trim();
      }

      return new XMLParserException(sMsg, e.getLineNumber(), e.getColumnNumber(), e);
   }

   /**
    * Parses an XML character input stream into DOM without validation.
    * Uses parser pooling.
    * @param reader The input stream.
    * @return The resulting DOM.
    */
   public static Document parse(Reader reader)
   {
      return XMLUtil.parse(new XMLInputSource(null, null, null, reader, ENCODING), null);
   }

   /**
    * Parses an XML character input stream into DOM with optional validation.
    * Uses parser pooling.
    * @param reader The input stream.
    * @param xsdURL The URL of the XSD file for validation.
    * @return The resulting DOM.
    */
   public static Document parse(Reader reader, URL xsdURL)
   {
      LinkedHashTab schemaURLDeque = null;

      if (xsdURL != null)
      {
         schemaURLDeque = new LinkedHashTab(1);
         schemaURLDeque.put(xsdURL.toExternalForm(), xsdURL);
      }

      return XMLUtil.parse(new XMLInputSource(null, null, null, reader, ENCODING), schemaURLDeque);
   }

   /**
    * Parses an XML character input stream into DOM with optional validation.
    * Uses parser pooling.
    * @param reader The character input stream.
    * @param schemaURLDeque The mapping of schema URLs to other URLs that will
    * be used as the actual locations of those schemas, in the schema parse order.
    * @return The resulting DOM.
    */
   public static Document parse(Reader reader, LookupDeque schemaURLDeque)
   {
      return parse(new XMLInputSource(null, null, null, reader, ENCODING), schemaURLDeque);
   }

   /**
    * Parses an XML character input stream into SAX events without validation.
    * Uses parser pooling.
    * @param reader The input stream.
    * @param handler The SAX event handler. EntityResolver::resolveEntity() override is ignored.
    */
   public static void parse(Reader reader, DefaultHandler handler)
   {
      parse(new XMLInputSource(null, null, null, reader, ENCODING), handler, null, null);
   }

   /**
    * Parses an XML character input stream into SAX events with optional validation.
    * Uses parser pooling.
    * @param reader The input stream.
    * @param handler The SAX event handler. EntityResolver::resolveEntity() override is ignored.
    * @param xsdURL The URL of the XSD file for validation.
    */
   public static void parse(Reader reader, DefaultHandler handler, URL xsdURL)
   {
      LinkedHashTab schemaURLDeque = null;

      if (xsdURL != null)
      {
         schemaURLDeque = new LinkedHashTab(1);
         schemaURLDeque.put(xsdURL.toExternalForm(), xsdURL);
      }

      parse(new XMLInputSource(null, null, null, reader, ENCODING), handler, null, schemaURLDeque);
   }

   /**
    * Parses an XML character input stream into SAX events with optional validation.
    * Uses parser pooling.
    * @param reader The character input stream.
    * @param handler The SAX event handler. EntityResolver::resolveEntity() override is ignored.
    * @param schemaURLDeque The mapping of schema URLs to other URLs that will
    * be used as the actual locations of those schemas, in the schema parse order.
    */
   public static void parse(Reader reader, DefaultHandler handler, LookupDeque schemaURLDeque)
   {
      parse(new XMLInputSource(null, null, null, reader, ENCODING), handler, null, schemaURLDeque);
   }

   /**
    * Converts a parser InputSource for Xerces XMLInputSource.
    * @param isrc The input source.
    * @return The XMLInputSource object.
    */
   private static XMLInputSource toXMLInputSource(InputSource isrc)
   {
      if (isrc.getByteStream() != null)
      {
         return new XMLInputSource(isrc.getPublicId(), isrc.getSystemId(), null,
            isrc.getByteStream(), isrc.getEncoding());
      }
      else if (isrc.getCharacterStream() != null)
      {
         return new XMLInputSource(isrc.getPublicId(), isrc.getSystemId(), null,
            isrc.getCharacterStream(), isrc.getEncoding());
      }
      else
      {
         return new XMLInputSource(isrc.getPublicId(), isrc.getSystemId(), null);
      }
   }

   /**
    * Parses an XML input source into DOM with optional validation.
    * Uses parser pooling.
    * @param isrc The XML input source.
    * @param schemaURLDeque The mapping of schema URLs to other URLs that will
    * be used as the actual locations of those schemas, in the schema parse order.
    * @return The resulting DOM.
    */
   public static Document parse(InputSource isrc, LookupDeque schemaURLDeque)
   {
      return parse(toXMLInputSource(isrc), schemaURLDeque);
   }
   
   /**
    * Parses an XML input source into DOM with optional validation.
    * Uses parser pooling.
    * @param isrc The XML input source.
    * @param xsdURL The URL of the XSD file for validation. Can be null.
    * @return The resulting DOM.
    */
   public static Document parse(InputSource isrc, URL xsdURL)
   {
      LinkedHashTab schemaURLDeque = null;

      if (xsdURL != null)
      {
         schemaURLDeque = new LinkedHashTab(1);
         schemaURLDeque.put(xsdURL.toExternalForm(), xsdURL);
      }

      return parse(toXMLInputSource(isrc), schemaURLDeque);
   }

   /**
    * Parses an XML input source into SAX events with optional validation.
    * Uses parser pooling.
    * @param isrc The XML input source.
    * @param handler The SAX event handler. EntityResolver::resolveEntity() override is ignored.
    * @param schemaURLDeque The mapping of schema URLs to other URLs that will
    * be used as the actual locations of those schemas, in the schema parse order.
    */
   public static void parse(InputSource isrc, DefaultHandler handler, LookupDeque schemaURLDeque)
   {
      parse(toXMLInputSource(isrc), handler, null, schemaURLDeque);
   }

   /**
    * Parses an XML input source into SAX events with optional validation.
    * Uses parser pooling.
    * @param isrc The XML input source.
    * @param handler The SAX event handler. EntityResolver::resolveEntity() override is ignored.
    * @param xsdURL The URL of the XSD file for validation. Can be null.
    */
   public static void parse(InputSource isrc, DefaultHandler handler, URL xsdURL)
   {
      LinkedHashTab schemaURLDeque = null;

      if (xsdURL != null)
      {
         schemaURLDeque = new LinkedHashTab(1);
         schemaURLDeque.put(xsdURL.toExternalForm(), xsdURL);
      }

      parse(toXMLInputSource(isrc), handler, null, schemaURLDeque);
   }

   /**
    * Parses an XML input source into DOM without validation.
    * Uses parser pooling.
    * @param isrc The XML input source.
    * @return The resulting DOM.
    */
   public static Document parse(InputSource isrc)
   {
      return parse(isrc, (LookupDeque)null);
   }
   
   /**
    * Parses an XML input source into SAX events without validation.
    * Uses parser pooling.
    * @param isrc The XML input source.
    * @param handler The SAX event handler.
    */
   public static void parse(InputSource isrc, DefaultHandler handler)
   {
      parse(isrc, handler, (URL)null);
   }

   /**
    * Releases the cached XML parsers.
    */
   public static void cleanup()
   {
      synchronized (s_domParserMap)
      {
         s_domParserMap.clear();
      }

      synchronized (s_saxParserMap)
      {
         s_saxParserMap.clear();
      }
   }

   public static List getChildElements(Element parent)
   {
      NodeList children = parent.getChildNodes();
      List childList = new ArrayList(children.getLength());

      for (int i = 0; i < children.getLength(); ++i)
      {
         Node node = children.item(i);
         
         if (node.getNodeType() == Node.ELEMENT_NODE)
         {
            childList.add(node);
         }
      }
      
      return childList;
   }
   
   public static int countChildElements(Element parent)
   {
      int nCount = 0;

      for (Node node = parent.getFirstChild(); node != null; node = node.getNextSibling())
      {
         if (node.getNodeType() == Node.ELEMENT_NODE)
         {
            ++nCount;
         }
      }

      return nCount;
   }
   
   public static void sortNode(Node node, final String sAttributeName)
   {
      List list = new ArrayList();
      NodeList nodeList = node.getChildNodes();
      
      for (int i = 0; i < nodeList.getLength(); i++)
      {
         list.add(nodeList.item(i));
      }
      
      Collections.sort(list, new Comparator()
      {
         public int compare(Object left, Object right)
         {
            Node leftNode = (Node)left;
            Node rightNode = (Node)right;
            String sLeftName = null;
            String sRightName = null;
            
            if (sAttributeName == null)
            {
               sLeftName = leftNode.getNodeName();
               sRightName = rightNode.getNodeName();
            }
            else
            {
               if (leftNode.getAttributes() != null)
               {
                  Attr attr = (Attr)leftNode.getAttributes().getNamedItem(sAttributeName);
                  
                  if (attr != null)
                  {
                     sLeftName = attr.getValue();
                  }
               }

               if (rightNode.getAttributes() != null)
               {
                  Attr attr = (Attr)rightNode.getAttributes().getNamedItem(sAttributeName);
                  
                  if (attr != null)
                  {
                     sRightName = attr.getValue();
                  }
               }
            }
            
            if (sLeftName == null)
            {
               if (sRightName == null)
               {
                  return 0;
               }
               
               return -1;
            }
            
            if (sRightName == null)
            {
               return 1;
            }

            return sLeftName.compareTo(sRightName);
         }
      });
      
      for (Iterator iter = list.iterator(); iter.hasNext(); )
      {
         node.removeChild((Node)iter.next());
      }
      
      for (Iterator iter = list.iterator(); iter.hasNext(); )
      {
         node.insertBefore((Node)iter.next(), null);
      }
   }
   
   /**
    * Removes whitespace areas from the DOM element.
    * @param element Element to normalize.
    */
   public static void normalize(Element element)
   {
      ArrayList removeList = new ArrayList();

      if (element.hasChildNodes())
      {
         for (Node child = element.getFirstChild(); child != null; child = child.getNextSibling())
         {
            if (child.getNodeType() == Node.ELEMENT_NODE)
            {
               normalize((Element)child);
            }
            else if (child.getNodeType() == Node.TEXT_NODE)
            {
               String s = child.getNodeValue();

               if (s_spacePattern.matcher(s).matches())
               {
                  removeList.add(child);
               }
            }
         }
      }

      if (!removeList.isEmpty())
      {
         for (Iterator iter = removeList.iterator(); iter.hasNext(); )
         {
            element.removeChild((Node)iter.next());
         }
      }
   }

   /**
    * @return The formatted XML string.
    */
   public static String formatXML(Source src, boolean bHeader) throws XMLException
   {
      StringWriter writer = new StringWriter(16384);
      
      formatXML(src, bHeader, writer);

      return writer.toString();
   }
   
   /**
    * Formats an XML document contained in a given transformation source.
    * @param src The transformation source.
    * @param bHeader True to output the XML declaration header.
    * @throws XMLException if an error occurs.
    */
   public static void formatXML(Source src, boolean bHeader, Writer writer) throws XMLException
   {
      ClassLoader classLoaderSaved = Thread.currentThread().getContextClassLoader();
   
      try
      {
         ClassLoader inverseClassLoader = InverseURLClassLoader.getInstance(XMLUtil.class.getClassLoader(), "org\\.apache\\..*");
         Thread.currentThread().setContextClassLoader(inverseClassLoader);
         
         TransformerFactory factory = (TransformerFactory)inverseClassLoader.loadClass("org.apache.xalan.processor.TransformerFactoryImpl").newInstance();
         Transformer transformer = factory.newTransformer();

         transformer.setOutputProperty(OutputKeys.METHOD, "xml");
         transformer.setOutputProperty(OutputKeys.INDENT, "yes");

         if (!bHeader)
         {
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
         }

         if (src instanceof DOMSource)
         {
            Node node = ((DOMSource)src).getNode();

            if (node instanceof Document)
            {
               DocumentType type = ((Document)node).getDoctype();

               if (type != null)
               {
                  if (type.getPublicId() != null)
                  {
                     transformer.setOutputProperty(OutputKeys.DOCTYPE_PUBLIC, type.getPublicId());
                  }

                  if (type.getSystemId() != null)
                  {
                     transformer.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM, type.getSystemId());
                  }
               }
            }
         }

         transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "3");
         transformer.setOutputProperty("{http://xml.apache.org/xslt}line-separator", "\r\n");

         transformer.transform(src, new StreamResult(writer));
      }
      catch (Exception e)
      {
         throw new XMLException("err.xml.format", e);
      }
      finally
      {
         Thread.currentThread().setContextClassLoader(classLoaderSaved);
      }
   }

   /**
    * Formats an XML document contained in a given transformation source.
    * @param src The transformation source.
    * @return The formatted XML string.
    * @throws XMLException if an error occurs.
    */
   public static String formatXML(Source src) throws XMLException
   {
      return formatXML(src, false);
   }

   /**
    * Formats the XML document contained in an element.
    * @param element The element.
    * @return The formatted XML string.
    * @throws XMLException if an error occurs.
    */
   public static String formatXML(Element element) throws XMLException
   {
      normalize(element);

      return formatXML(new DOMSource(element), false);
   }

   /**
    * Formats the XML document contained in a string for better readability.
    * @param sXML The string to format.
    * @return The formatted string.
    * @throws XMLException if an error occurs.
    */
   public static String formatXML(String sXML) throws XMLException
   {
      return formatXML(parse(new StringReader(sXML), (URL)null).getDocumentElement());
   }

   /**
    * Create an XSLT template instance from a given URL.
    * @param url The URL.
    * @throws XMLException if an error occurs. 
    */
   public static Templates getTemplate(URL url) throws XMLException
   {
      InputStream is = null;
      
      try
      {
         is = URLUtil.openStream(url);

         return getTemplate(new StreamSource(is));
      }
      catch (XMLException e)
      {
         throw e;
      }
      catch (IOException e)
      {
         throw new XMLException("err.xml.template", e);
      }
      finally
      {
         IOUtil.close(is);
      }
   }
   
   /**
    * Gets an XSLT template instance from a given transformation source.
    * @param src The transformation source.
    * @throws XMLException if an error occurs.
    */
   public static Templates getTemplate(Source src) throws XMLException
   {
      ClassLoader classLoaderSaved = Thread.currentThread().getContextClassLoader();
      Thread.currentThread().setContextClassLoader(XMLUtil.class.getClassLoader());
      
      try
      {                              
         TransformerFactory factory = (TransformerFactory)Class.forName(
          "org.apache.xalan.processor.TransformerFactoryImpl").newInstance();
         
         return factory.newTemplates(src);
      }
      catch (Exception e)
      {
         throw new XMLException("err.xml.template", e);
      }
      finally
      {
         Thread.currentThread().setContextClassLoader(classLoaderSaved);
      }
   }
   
   /**
    * Gets an XSL transformer handler from a given XSLT template.
    * @param template The XSLT template.
    * @return The transformation handler.
    * @throws XMLException if an error occurs.
    */
   public static TransformerHandler getTransformerHandler(Templates template) throws XMLException
   {
      ClassLoader classLoaderSaved = Thread.currentThread().getContextClassLoader();
      Thread.currentThread().setContextClassLoader(XMLUtil.class.getClassLoader());
      
      try
      {                              
         SAXTransformerFactory factory = (SAXTransformerFactory)Class.forName(
            "org.apache.xalan.processor.TransformerFactoryImpl").newInstance();

         return factory.newTransformerHandler(template);
      }
      catch (Exception e)
      {
         throw new XMLException("err.xml.transformer", e);
      }
      finally
      {
         Thread.currentThread().setContextClassLoader(classLoaderSaved);
      }
   }

   /**
    * Executes a pipe of XSLT transformations.
    * @param result The transformation result adapter.
    * @param source The input source.
    * @param handlers The XSLT handler array, in order of execution.
    * @param xsdURL The XSD URL, can be null.
    */
   public static void transform(Result result, InputSource source, TransformerHandler[] handlers, URL xsdURL) throws XMLException
   {
      for (int i = 1; i < handlers.length; ++i)
      {
         handlers[i - 1].setResult(new SAXResult(handlers[i]));
      }

      handlers[handlers.length - 1].setResult(result);

      LinkedHashTab schemaURLDeque = null;

      if (xsdURL != null)
      {
         schemaURLDeque = new LinkedHashTab(1);
         schemaURLDeque.put(xsdURL.toExternalForm(), xsdURL);
      }

      parse(toXMLInputSource(source), handlers[0], null, schemaURLDeque);
   }

   public static boolean getBooleanAttr(Node node, String sName)
   {
      return getBooleanAttr(node, sName, false);
   }

   /**
    * Returns the Boolean value of the specified node attribute.
    * @param node The DOM node containing the attribute.
    * @param sName The name of the attribute.
    * @return The value of the attribute; null if attribute
    * was not found.
    */
   public static Boolean getBooleanObjAttr(Node node, String sName)
   {
      String sValue = getStringAttr(node, sName);
      
      if (sValue == null)
      {
         return null;
      }

      return Boolean.valueOf(parseBoolean(sValue, node, sName));
   }

   /**
    * Gets the value of an integer attribute.
    * 
    * @param node  The DOM node containing the attribute.
    * @param sName The name of the attribute.
    * @return The value of the attribute; 0 if the attribute is missing.
    * @throws XMLException If the value is not a valid integer.
    */
   public static int getIntAttr(Node node, String sName)
   {
      return getIntAttr(node, sName, 0);
   }

   /**
    * Gets the Integer value of the specified node attribute.
    * 
    * @param node  The DOM node containing the attribute.
    * @param sName The name of the attribute.
    * @return The value of the attribute; null if the attribute was not found.
    * @throws XMLException If the attribute value is not a valid integer.
    */
   public static Integer getIntegerObjAttr(Node node, String sName)
   {
      String sValue = getStringAttr(node, sName);
      
      if (sValue == null)
      {
         return null;
      }

      return new Integer(parseInt(sValue, node, sName));
   }

   public static float getFloatAttr(Node node, String sName)
   {
      return getFloatAttr(node, sName, 0);
   }

   public static Float getFloatObjAttr(Node node, String sName)
   {
      String sValue = getStringAttr(node, sName);
      
      if (sValue == null)
      {
         return null;
      }
      
      return new Float(parseFloat(sValue, node, sName));
   }

   /**
    * Parse attribute containing comma delimited string
    */ 
   public static String[] getListAttr(Node node, String sName)
   {
      String[] list = null;
      String value = getStringAttr(node, sName);

      if (value != null && value.length() > 0)
      {
         list = value.split(",");
      }
      
      return list;
   }
   
   public static List findChildElements(Node parent, String sName)
   {
      NodeList children = parent.getChildNodes();
      List retVal = new ArrayList(children.getLength());

      for (int i = 0; i < children.getLength(); ++i)
      {
         Node node = children.item(i);
         
         if (node.getNodeType() == Node.ELEMENT_NODE && node.getNodeName().equals(sName))
         {
            retVal.add(node);
         }
      }
      return retVal;
   }

   /**
    * Find all elements with name sName under node parent.
    * @param parent Node to search.
    * @param sName Name to search.
    * @return List of elements.
    */
   public static List findElementRecur(Node parent, String sName)
   {
      List list = new ArrayList();
      
      findElementsRecur(parent, sName, list);
      
      return list;
   }
   
   /**
    * Find all elements with name sName under node parent.
    * @param parent Node to search.
    * @param sName Name to search.
    * @param list List of elements to fill.
    */
   private static void findElementsRecur(Node parent, String sName, List list)
   {
      if (parent.getNodeType() != Node.ELEMENT_NODE)
      {
         return;
      }

      if (sName.equals(parent.getNodeName()))
      {
         list.add(parent);
      }

      for (Node node = parent.getFirstChild(); node != null; node = node.getNextSibling())
      {
         findElementsRecur(node, sName, list);
      }      
   }

   /**
    * Finds a direct descendant element of the given node.
    * 
    * @param parent The node whose children should be searched.
    * @param sName  The name of the node to find.
    * @return The matched element; null if no matching node is found.
    */
   public static Element findChildElement(Node parent, String sName)
   {
      return findChildElement(parent, sName, null, null, false);
   }

   /**
    * Finds a direct descendant element of the given node.
    * 
    * @param parent The node whose children should be searched.
    * @param sName  The name of the node to find. May be null.
    * @param sAttribute Optional attribute name, by which to search. Can be null.
    * @param sValue Optional attribute value, by which to search. Can be null.
    * @return The matched element; null if no matching node is found.
    */
   public static Element findChildElement(Node parent, String sName, String sAttribute, String sValue)
   {
      return findChildElement(parent, sName, sAttribute, sValue, false);
   }
   
   public static Element findChildElementByName(Node parent, String sName, String sNameAttributeValue)
   {
      return findChildElement(parent, sName, "name", sNameAttributeValue, false);
   }
   
   /**
    * Finds a direct descendant element of the given node.
    * 
    * @param parent The node whose children should be searched.
    * @param sName  The name of the node to find. May be null.
    * @param sAttribute Optional attribute name, by which to search. Can be null.
    * @param sValue Optional attribute value, by which to search. Can be null.
    * @param bDefaultFirst If true and no children match, return the first child element.
    * @return The matched element; null if no matching node is found.
    */
   public static Element findChildElement(Node parent, String sName, String sAttribute, String sValue, boolean bDefaultFirst)
   {
      Node firstNode = null;

      for (Node node = parent.getFirstChild(); node != null; node = node.getNextSibling())
      {
         if (node.getNodeType() != Node.ELEMENT_NODE)
         {
            continue;
         }
         
         if (firstNode == null)
         {
            firstNode = node;
         }

         if (sName != null && !sName.equals(node.getNodeName()))
         {
            continue;
         }
         
         if (sAttribute == null)
         {
            return (Element)node;
         }
         else if (findAttribute(node, sAttribute, sValue))
         {
            return (Element)node;
         }
      }

      if (bDefaultFirst)
      {
         return (Element)firstNode;
      }

      return null;
   }
   
   /**
    * @param sName Null to search all elements
    * @param sAttribute Null to filter no attribute
    * @return The first element found that matches criteria
    */
   public static Element findElementRecur(Node startNode, String sName, String sAttribute, String sValue)
   {
      if (startNode.getNodeType() != Node.ELEMENT_NODE)
      {
         return null;
      }

      boolean tagMatches = true;

      if (sName != null && !sName.equals(startNode.getNodeName()))
      {
         tagMatches = false;
      }
      
      if (tagMatches)
      {
         if (sAttribute == null || findAttribute(startNode, sAttribute, sValue))
         {
            return (Element)startNode; 
         }
      }
      
      for (Node node = startNode.getFirstChild(); node != null; node = node.getNextSibling())
      {
         Element retVal = findElementRecur(node, sName, sAttribute, sValue);
         
         if (retVal != null)
         {
            return retVal;
         }
      }
      
      return null;
   }
   
   public static Element findChildElementByName(Node parent, String sName)
   {
      return findChildElement(parent, "name", sName, false);
   }

   /**
    * Checks to see if an attribute with the given name and value exists
    * on the given node.
    * 
    * @param node The node whose attributes shall be searched.
    * @param sAttribute The name of the attribute to look for.
    * @param sValue The value to match.
    * @return True if an attribute is found whose name and value match
    * the given parameters; false otherwise.
    */
   public static boolean findAttribute(Node node, String sAttribute, String sValue)
   {
      NamedNodeMap attrs = node.getAttributes();

      if (attrs != null)
      {
         Attr name = (Attr)attrs.getNamedItem(sAttribute);

         if (name != null && sValue.equals(name.getValue()))
         {
            return true;
         }
      }
      
      return false;
   }
   
   public static Element findChildElement(Node parent, String sAttribute, String sValue, boolean bDefaultFirst)
   {
      return findChildElement(parent, null, sAttribute, sValue, bDefaultFirst);
   } 

   /**
    * Gets the only element in a container.
    * @param parent The element container.
    * @param bRequired True to throw an exception if the element is missing.
    * @return The child element of null if it is missing.
    * @throws XMLException if there are more than one child elements
    * or a required child element does not exist.
    */
   public static Element getOnlyChildElement(Element parent, boolean bRequired)
   {
      Element element = null;

      for (Node node = parent.getFirstChild();
         node != null;
         node = node.getNextSibling())
      {
         if (node.getNodeType() == Node.ELEMENT_NODE)
         {
            if (element != null)
            {
               throw new XMLException("err.xml.multipleChildElements", new Object[]{parent.getNodeName()}); 
            }

            element = (Element)node;
         }
      }

      if (element == null && bRequired)
      {
         throw new XMLException("err.xml.missingOnlyRequiredElement", new Object[]{parent.getNodeName()});
      }

      return element;
   }

   /**
    * Finds a child element by its subelement with a given value.
    * @param parent The parent node.
    * @param sName The element name. Can be null for any name.
    * @param sSubName The subelement name.
    * @param sSubValue The subelement value.
    * @return The element, or null is not found.
    */
   public static Element findChildElementBySubelement(Node parent, String sName, String sSubName, String sSubValue)
   {
      for (Node node = parent.getFirstChild(); node != null; node = node.getNextSibling())
      {
         if (node.getNodeType() == Node.ELEMENT_NODE)
         {
            if (sName == null || sName.equals(node.getNodeName()))
            {
               for (Node sub = node.getFirstChild(); sub != null; sub = sub.getNextSibling())
               {
                  if (sub.getNodeType() == Node.ELEMENT_NODE && sub.getNodeName().equals(sSubName))
                  {
                     if (ObjUtil.equal(getElementValue((Element)sub), sSubValue))
                     {
                        return (Element)node;
                     }
                  }
               }
            }
         }
      }
      
      return null;
   }
   
   /**
    * Finds a child element by its subelement with a given value.
    * @param parent The parent node.
    * @param sName The element name. Can be null for any name.
    * @param sValuePrefix The prefix of the element value to match against.
    * @return The element, or null is not found.
    */
   public static Element findChildElementByElementValuePrefix(Node parent, String sName, String sValuePrefix)
   {
      for (Node node = parent.getFirstChild(); node != null; node = node.getNextSibling())
      {
         if (node.getNodeType() == Node.ELEMENT_NODE)
         {
            if (sName == null || sName.equals(node.getNodeName()))
            {
               if (getElementValue((Element)node).startsWith(sValuePrefix))
               {
                  return (Element)node;
               }
            }
         }
      }
      
      return null;
   }

   /**
    * Gets the first element in a chain of nodes.
    * @param node The first node.
    * @return The first element, or null if not found.
    */
   public static Element findFirstElement(Node node)
   {
      while (node != null)
      {
         if (node.getNodeType() == Node.ELEMENT_NODE)
         {
            return (Element)node;
         }
         
         node = node.getNextSibling();
      }
      
      return null;
   }
   
   /**
    * Checks if an element contains the specified child element
    * and invokes the handler on it.
    * @param parent The parent element.
    * @param sName The child element name.
    * @param bRequired True if the child element is required.
    * @param handler The element handler to invoke.
    * @throws XMLException if the required child element does not exist.
    */
   public static void withFirstChildElement(Element parent, String sName, boolean bRequired, ElementHandler handler)
   {
      for (Node node = parent.getFirstChild();
         node != null;
         node = node.getNextSibling())
      {
         if (node.getNodeType() == Node.ELEMENT_NODE &&
            node.getNodeName().equals(sName))
         {
            handler.handleElement((Element)node);
            return;
         }
      }
      
      if (bRequired)
      {
         throw new XMLException("err.xml.missingRequiredElement", 
            new Object[]{sName, parent.getNodeName()});
      }
   }

   /**
    * Finds a direct descendant comment of the given node.
    * 
    * @param parent The node whose children should be searched.
    * @param sCommentText The text of the comment to find.
    * @return The matched comment; null if no matching node is found.
    */
   public static Comment findChildComment(Node parent, String sCommentText)
   {
      for (Node node = parent.getFirstChild(); node != null; node = node.getNextSibling())
      {
         if (node.getNodeType() != Node.COMMENT_NODE)
         {
            continue;
         }

         if (node.getTextContent().contains(sCommentText))
         {
            return (Comment)node;
         }
      }

      return null;
   }
   
   /**
    * Invokes the handler on all matching child elements.
    * @param parent The parent element.
    * @param sName The child element name. Null to iterate over all elements.
    * @param handler The element handler to invoke.
    */
   public static void forEachChildElement(Element parent, String sName, ElementHandler handler)
   {
      Node node = parent.getFirstChild();
      
      while (node != null)
      {
         Node next = node.getNextSibling();

         if (node.getNodeType() == Node.ELEMENT_NODE &&
            (sName == null || sName.equals(node.getNodeName())))
         {
            handler.handleElement((Element)node);
         }

         node = next;
      }
   }
   
   /**
    * Invokes the handler on all elements in the tree rooted at parent whose
    * name matches sName.
    * 
    * @param parent  The root element in the tree.
    * @param sName   Handler is invoked for elements matching this name. If null,
    *                matches all elements.
    * @param handler The element handler to invoke.
    */
   public static void forEachElementRecur(Node parent, String sName, ElementHandler handler)
   {
      if (parent.getNodeType() != Node.ELEMENT_NODE)
      {
         return;
      }
      
      if (sName == null || sName.equals(parent.getNodeName()))
      {
         handler.handleElement((Element)parent);
      }
      
      for (Node node = parent.getFirstChild(); node != null; node = node.getNextSibling())
      {
         forEachElementRecur(node, sName, handler);
      }
   }

   public static List forEachElementRecur(Node parent, String sName, FilterHandler handler)
   {
      List list = new ArrayList();
      
      forEachElementRecur(parent, sName, handler, list);
      
      return list;
   }
   
   private static void forEachElementRecur(Node parent, String nodeName, FilterHandler handler, List list)
   {
      if (parent.getNodeType() != Node.ELEMENT_NODE)
      {
         return;
      }
      
      if (nodeName == null || nodeName.equals(parent.getNodeName()))
      {
         Object filterObject = handler.filterElement((Element)parent);
         
         if (filterObject != null)
         {
            list.add(filterObject);
         }
      }
      
      for (Node node = parent.getFirstChild(); node != null; node = node.getNextSibling())
      {
         forEachElementRecur(node, nodeName, handler, list);
      }
   }

   /**
    * Adds a child element.
    * @param parent The parent element.
    * @param orderArray The possible child element name array, in order of occurrence.
    * Can be null to append the element at the end. 
    * @param element The element to add.
    */
   public static void addChildElement(Node parent, String[] orderArray, Element element)
   {
      String sName = element.getNodeName();
      
      for (Node node = parent.getLastChild(); node != null; node = node.getPreviousSibling())
      {
         if (node.getNodeType() == Node.ELEMENT_NODE && node.getNodeName().equals(sName))
         {
            // skip non elements (i.e. comments)
            for (Node nextNode = node.getNextSibling(); nextNode != null; nextNode = nextNode.getNextSibling())
            {
               if (nextNode.getNodeType() == Node.ELEMENT_NODE)
               {
                  parent.insertBefore(element, nextNode);
                  return;
               }
            }
            
            // if EOD reached
            parent.appendChild(element);
            
            return;
         }
      }

      if (orderArray != null)
      {
         int i;
   
         for (i = 0; i < orderArray.length; ++i)
         {
            if (orderArray[i].equals(sName))
            {
               ++i;
               break;
            }
         }
   
         for (; i < orderArray.length; ++i)
         {
            sName = orderArray[i];
            
            for (Node node = parent.getFirstChild(); node != null; node = node.getNextSibling())
            {
               if (node.getNodeType() == Node.ELEMENT_NODE && node.getNodeName().equals(sName))
               {
                  parent.insertBefore(element, node);

                  return;
               }
            }
         }
      }

      parent.appendChild(element);
   }

   /**
    * Creates and adds a child element.
    * @param parent The parent element.
    * @param orderArray The possible child element name array, in order of occurrence.
    * @param sName The element name.
    * @return The created element.
    */
   public static Element addChildElement(Node parent, String[] orderArray, String sName)
   {
      Element element = parent.getOwnerDocument().createElement(sName);
      
      addChildElement(parent, orderArray, element);
      
      return element;
   }

   /**
    * Creates and adds a child element with a value.
    * @param parent The parent element.
    * @param orderArray The possible child element name array, in order of occurrence.
    * @param sName The element name.
    * @param sValue The element value.
    * @return The created element.
    */
   public static Element addChildElement(Node parent, String[] orderArray, String sName, String sValue)
   {
      Element element = addChildElement(parent, orderArray, sName);
      
      setElementValue(element, sValue);

      return element;
   }
   
   /**
    * Sets/adds a child element.
    * @param parent The parent element.
    * @param orderArray The possible child element name array, in order of occurrence.
    * @param sName The element name.
    * @param sAttribute Optional attribute name, by which to search. Can be null.
    * @param sAttrValue Optional attribute value, by which to search. Can be null.
    * @param sValue The element value.
    * @param bOverwrite True to overwrite the existing value.
    * @return The created element.
    */
   public static Element setChildElement(Node parent, String[] orderArray,
      String sName, String sAttribute, String sAttrValue, String sValue, boolean bOverwrite)
   {
      Element element = findChildElement(parent, sName, sAttribute, sAttrValue);

      if (element == null)
      {
         element = addChildElement(parent, orderArray, sName);

         if (sAttribute != null)
         {
            element.setAttribute(sAttribute, sAttrValue);
         }

         bOverwrite = true;
      }
      
      if (bOverwrite)
      {
         setElementValue(element, sValue);
      }
      
      return element;
   }
   
   /**
    * Sets/adds a child element.
    * @param parent The parent element.
    * @param orderArray The possible child element name array, in order of occurrence.
    * @param sName The element name.
    * @param sValue The element value.
    * @param bOverwrite True to overwrite the existing value.
    * @return The created element.
    */
   public static Element setChildElement(Node parent, String[] orderArray, String sName,
      String sValue, boolean bOverwrite)
   {
      return setChildElement(parent, orderArray, sName, null, null, sValue, bOverwrite);
   }

   /**
    * Sets a DOM element value.
    * @param element The element which value to set.
    * @param sValue The element value.
    */
   public static void setElementValue(Element element, String sValue)
   {
      while (element.getFirstChild() != null)
      {
         element.removeChild(element.getFirstChild());
      }

      if (sValue != null)
      {
         element.appendChild(element.getOwnerDocument().createTextNode(sValue));
      }
   }
   
   /**
    * Return the value of an element.
    * @param element The element.
    */
   public static String getElementValue(Element element)
   {
      String sValue = null;

      for (Node node = element.getFirstChild(); node != null; node = node.getNextSibling())
      {
         if (node.getNodeType() == Node.TEXT_NODE || node.getNodeType() == Node.CDATA_SECTION_NODE)
         {
            String sNodeValue = node.getNodeValue();
            
            if (sNodeValue != null)
            {
               if (sValue == null)
               {
                  sValue = sNodeValue;
               }
               else
               {
                  sValue += sNodeValue;
               }
            }
         }
      }
      
      return sValue;
   }

   /**
    * Removes a node and deletes the trailing spaces.
    * @param node The node to remove. Can be null.
    * @return True if the node was non-null.
    */
   public static boolean removeNode(Node node)
   {
      if (node != null && node.getParentNode() != null)
      {
      loop:
         for (;;)
         {
            Node next = node.getNextSibling();

            node.getParentNode().removeChild(node);
            node = next;

            if (node == null  || node.getNodeType() != Node.TEXT_NODE)
            {
               break;
            }

            String s = node.getNodeValue();

            if (s != null)
            {
               for (int i = 0, n = s.length(); i < n; ++i)
               {
                  if (!Character.isWhitespace(s.charAt(i)))
                  {
                     break loop;
                  }
               }
            }
         }

         return true;      
      }

      return false;
   }
   
   /**
    * Sets an attribute value.
    * @param element The element on which to set the attribute.
    * @param sAttribute The attribute name.
    * @param sValue The attribute value. Null to remove the attribute.
    * @param bOverride True to override an existing value.
    */
   public static void setAttribute(Element element, String sAttribute, String sValue, boolean bOverride)
   {
      if (bOverride || !element.hasAttribute(sAttribute))
      {
         if (sValue == null)
         {
            element.removeAttribute(sAttribute);
         }
         else
         {
            element.setAttribute(sAttribute, sValue);
         }
      }
   }

   /**
    * Returns the value of the specified node attribute.
    * @param node The DOM node containing the attribute.
    * @param sName The name of the attribute.
    * @return The value of the attribute, or null
    * if the attribute was not found. 
    */
   public static String getStringAttr(Node node, String sName)
   {
      NamedNodeMap attrMap = node.getAttributes();

      if (attrMap != null)
      {
         Attr attr = (Attr)attrMap.getNamedItem(sName);
         
         if (attr != null)
         {
            String sValue = attr.getValue();
            
            if (sValue != null && sValue.length() == 0)
            {
               return null;
            }
            
            return sValue;
         }
      }
      
      return null;
   }
   
   /**
    * Returns the value of a string attribute.
    * @param node The DOM node containing the attribute.
    * @param sName The name of the attribute.
    * @param sDefValue The default value if the attribute is missing.
    * @return The value of the attribute.
    */
   public static String getStringAttr(Node node, String sName, String sDefValue)
   {
      String sValue = getStringAttr(node, sName);
      
      if (sValue != null)
      {
         return sValue;
      }
      
      return sDefValue;
   }

   /**
    * Returns the value of the specified node attribute.
    * @param node The DOM node containing the attribute.
    * @param sName The name of the attribute.
    * @return The value of the attribute.
    * @throws XMLException if the attribute was not found or the value is empty. 
    */
   public static String getReqStringAttr(Node node, String sName)
   {
      NamedNodeMap attrMap = node.getAttributes();

      if (attrMap != null)
      {
         Attr attr = (Attr)attrMap.getNamedItem(sName);
         
         if (attr != null)
         {
            String value = attr.getValue();
            
            if (value != null)
            {
               return value;
            }
         }
      }
      
      throw new XMLException("err.xml.missingRequiredAttribute",
         new Object[] {sName, node.getNodeName()});
   }
   
   /**
    * Returns the value of an integer attribute.
    * @param node The DOM node containing the attribute.
    * @param sName The name of the attribute.
    * @param nDefValue The default value if the attribute is missing.
    * @return The value of the attribute.
    * @throws XMLException If the value is not a valid integer.
    */
   public static int getIntAttr(Node node, String sName, int nDefValue)
   {
      String sValue = getStringAttr(node, sName);
      
      if (sValue != null)
      {
         return parseInt(sValue, node, sName);
      }

      return nDefValue;
   }

   /**
    * Returns the value of the specified integer attribute.
    * @param node The DOM node containing the attribute.
    * @param sName The name of the attribute.
    * @return The value of the attribute.
    * @throws XMLException if the attribute was not found or
    * the value is not a valid integer. 
    */
   public static int getReqIntAttr(Node node, String sName)
   {
      return parseInt(getReqStringAttr(node, sName), node, sName);
   }

   /**
    * Parses an integer value.
    * @param sValue The string to parse.
    * @param node The element node for error reporting.
    * @param sName The attribute name for error reporting.
    * @return The integer value.
    * @throws XMLException if the string is not a valid integer. 
    */
   protected static int parseInt(String sValue, Node node, String sName)
   {
      try
      {
         return Integer.parseInt(sValue);
      }
      catch (Exception e)
      {
         throw new XMLException("err.xml.integerAttr", 
            new Object[]{sName, sValue, node.getNodeName()}, e);
      }
   }
   
   /**
    * Returns the value of a long attribute.
    * @param node The DOM node containing the attribute.
    * @param sName The name of the attribute.
    * @param lDefValue The default value if the attribute is missing.
    * @return The value of the attribute.
    * @throws XMLException If the value if not a valid long.
    */
   public static long getLongAttr(Node node, String sName, long lDefValue)
   {
      String sValue = getStringAttr(node, sName);
      
      if (sValue != null)
      {
         return parseLong(sValue, node, sName);
      }

      return lDefValue;
   }

   /**
    * Returns the value of the specified long attribute.
    * @param node The DOM node containing the attribute.
    * @param sName The name of the attribute.
    * @return The value of the attribute.
    * @throws XMLException if the attribute was not found or
    * the value is not a valid long. 
    */
   public static long getReqLongAttr(Node node, String sName)
   {
      return parseLong(getReqStringAttr(node, sName), node, sName);
   }

   /**
    * Parses a long value.
    * @param sValue The string to parse.
    * @param node The element node for error reporting.
    * @param sName The attribute name for error reporting.
    * @return The long value.
    * @throws XMLException if the string is not a valid long. 
    */
   protected static long parseLong(String sValue, Node node, String sName)
   {
      try
      {
         return Long.parseLong(sValue);
      }
      catch (Exception e)
      {
         throw new XMLException("err.xml.longAttr", 
            new Object[]{sName, sValue, node.getNodeName()}, e);
      }
   }

   /**
    * Returns the value of a float attribute.
    * @param node The DOM node containing the attribute.
    * @param sName The name of the attribute.
    * @param fDefValue The default value if the attribute is missing.
    * @return The value of the attribute.
    * @throws XMLException If the value is not a valid floating point number.
    */
   public static float getFloatAttr(Node node, String sName, float fDefValue)
   {
      String sValue = getStringAttr(node, sName);
      
      if (sValue != null)
      {
         return parseFloat(sValue, node, sName);
      }

      return fDefValue;
   }

   /**
    * Returns the value of the specified float attribute.
    * @param node The DOM node containing the attribute.
    * @param sName The name of the attribute.
    * @return The value of the attribute.
    * @throws XMLException if the attribute was not found or
    * the value is not a valid floating point number. 
    */
   public static float getReqFloatAttr(Node node, String sName)
   {
      return parseFloat(getReqStringAttr(node, sName), node, sName);
   }

   /**
    * Parses a floating point value.
    * @param sValue The string to parse.
    * @param node The element node for error reporting.
    * @param sName The attribute name for error reporting.
    * @return The float value.
    * @throws XMLException if the string is not a valid floating point number. 
    */
   protected static float parseFloat(String sValue, Node node, String sName)
   {
      try
      {
         return Float.parseFloat(sValue);
      }
      catch (Exception e)
      {
         throw new XMLException("err.xml.floatAttr", 
            new Object[]{sName, sValue, node.getNodeName()}, e);
      }
   }

   /**
    * Returns the value of a boolean attribute.
    * @param node The DOM node containing the attribute.
    * @param sName The name of the attribute.
    * @param bDefValue The default value if the attribute is missing.
    * @return The value of the attribute.
    * @throws XMLException If the value is not a valid boolean.
    */
   public static boolean getBooleanAttr(Node node, String sName, boolean bDefValue)
   {
      String sValue = getStringAttr(node, sName);
      
      if (sValue != null)
      {
         return parseBoolean(sValue, node, sName);
      }

      return bDefValue;
   }

   /**
    * Returns the value of the specified boolean attribute.
    * @param node The DOM node containing the attribute.
    * @param sName The name of the attribute.
    * @return The value of the attribute.
    * @throws XMLException if the attribute was not found or
    * the value is not a valid boolean. 
    */
   public static boolean getReqBooleanAttr(Node node, String sName)
   {
      return parseBoolean(getReqStringAttr(node, sName), node, sName);
   }

   /**
    * Parses a boolean value.
    * @param sValue The string to parse.
    * @param node The element node for error reporting.
    * @param sName The attribute name for error reporting.
    * @return The boolean value.
    * @throws XMLException if the string is is not a valid boolean. 
    */
   protected static boolean parseBoolean(String sValue, Node node, String sName)
   {
      try
      {
         return StringUtil.parseBoolean(sValue);
      }
      catch (IllegalArgumentException e)
      {
         throw new XMLException("err.xml.booleanAttr",
            new Object[]{sName, sValue, node.getNodeName()});
      }
   }

   /**
    * Determines if the name is a valid XML 1.0 name.
    * @param sName The name to check.
    * @return True if the name is valid.
    */
   public static boolean isValidName(String sName)
   {
      if (StringUtil.isEmpty(sName))
      {
         return false;
      }
   
      if (!isValidNameStartChar(sName.codePointAt(0)))
      {
         return false;
      }
   
      for (int i = 1, nCount = sName.length(); i < nCount; i++)
      {
         if (!isValidNameChar(sName.codePointAt(0)))
         {
            return false;
         }
      }
   
      return true;
   }

   /**
    * Determines if the code point is a valid name character, according to XML 1.0
    * @param n The code point to check.
    * @return True if the code point is a valid name character.
    */
   public static boolean isValidNameChar(int n)
   {
      return isValidNameStartChar(n)
        || n == '-'
        || n == '.'
        || (n >= '0' && n <= '9')
        || n == 0xb7
        || (n >= 0x300 && n <= 0x36f)
        || (n >= 0x203f && n <= 0x2040);
   }

   /**
    * Determines if the code point is a valid name start character, according to XML 1.0.
    * @param n The code point to check.
    * @return True if the code point is a valid name start character.
    */
   public static boolean isValidNameStartChar(int n)
   {
      return n == ':'
         || (n >= 'A' && n <= 'Z')
         || n == '_'
         || (n >= 'a' && n <= 'z')
         || (n >= 0xc0 && n <= 0xd6)
         || (n >= 0xd8 && n <= 0xf6)
         || (n >= 0xf8 && n <= 0x2ff)
         || (n >= 0x370 && n <= 0x37d)
         || (n >= 0x37f && n <= 0x1fff)
         || (n >= 0x200c && n <= 0x200d)
         || (n >= 0x2070 && n <= 0x218f)
         || (n >= 0x2c00 && n <= 0x2fef)
         || (n >= 0x3001 && n <= 0xd7ff)
         || (n >= 0xf900 && n <= 0xfdcf)
         || (n >= 0xfdf0 && n <= 0xfffd)
         || (n >= 0x10000 && n <= 0xeffff);
   }

   // inner classes

   /**
    * SAX parser and configuration holder.
    */
   private static class SAXHolder
   {
      // associations
      
      /**
       * The SAX parser.
       */
      private SAXParser m_parser;

      /**
       * The XML parser configuration.
       */
      private XMLParserConfiguration m_config;

      // constructors
      
      /**
       * Constructs the SAX holder.
       */
      public SAXHolder(SAXParser parser, XMLParserConfiguration config)
      {
         m_parser = parser;
         m_config = config;
      }

      // operations

      /**
       * @return The SAX parser.
       */
      public SAXParser getParser()
      {
         return m_parser;
      }
      
      /**
       * @return The XML parser configuration.
       */
      public XMLParserConfiguration getConfig()
      {
         return m_config;
      }
   }
   
   /**
    * Interface implemented by incremental SAX handlers. 
    */
   public interface IncrementalHandler
   {
      /**
       * @return True if the parsing is complete.
       */
      boolean isComplete();
   }
   
   /**
    * Interface implemented by DOM element handlers.
    */
   public interface ElementHandler
   {
      /**
       * Handles the specified DOM element.
       * @param element The DOM element to handle.
       */
      void handleElement(Element element);
   }

   public interface FilterHandler
   {
      Object filterElement(Element element);
   }

   /**
    * A key to use for the parser pool.
    */
   private static class ParserPoolKey
   {
      // attributes

      /**
       * The cached hash code.
       */
      protected int m_nHashCode;

      // associations

      /**
       * The schema map to key on.
       */
      protected LookupDeque m_schemaMap;

      // constructors

      /**
       * Constructs a new key for the parser pool.
       * @param schemaMap The schema map to key on.
       */
      public ParserPoolKey(LookupDeque schemaMap)
      {
         m_schemaMap = schemaMap;
      }

      // operations

      /**
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         if (!(obj instanceof ParserPoolKey))
         {
            return false;
         }

         ParserPoolKey other = (ParserPoolKey)obj;

         if (m_schemaMap.size() != other.m_schemaMap.size())
         {
            return false;
         }

         Lookup.Iterator selfIterator = m_schemaMap.iterator();
         Lookup.Iterator otherIterator = other.m_schemaMap.iterator();

         while (selfIterator.hasNext())
         {
            if (!selfIterator.next().equals(otherIterator.next()))
            {
               return false;
            }

            if (!ObjUtil.equal(selfIterator.getValue(), otherIterator.getValue()))
            {
               return false;
            }
         }

         return true;
      }

      /**
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         if (m_nHashCode != 0)
         {
            return m_nHashCode;
         }

         int nHashCode = 0;

         for (Lookup.Iterator itr = m_schemaMap.iterator(); itr.hasNext(); )
         {
            nHashCode ^= itr.next().hashCode();

            Object value = itr.getValue();

            if (value != null)
            {
               nHashCode ^= value.hashCode();
            }
         }

         if (nHashCode == 0)
         {
            m_nHashCode = -1;
         }

         return m_nHashCode;
      }
   }

   /**
    * A resolver that translates arbitrary resource references from the document
    * to the actual URLs where the resources may be found.
    */
   public static class MappedResolver implements XMLEntityResolver
   {
      // attributes

      /**
       * True to resolve unmapped URLs; false to throw an exception.
       */
      protected boolean m_bResolveUnmappedEntities;

      // associations

      /**
       * The map of schema URL strings to the actual URLs to use.
       */
      protected Lookup m_schemaMap;  // of type URL[String]

      // constructors

      /**
       * Constructs a mapped resolver.
       * @param schemaMap The map of schema URL strings to the actual URLs to use.
       */
      public MappedResolver(Lookup schemaMap)
      {
         m_schemaMap = schemaMap;
      }

      /**
       * Constructs a mapped resolver.
       * @param schemaMap The map of schema URL strings to the actual URLs to use.
       * @param bResolveUnmappedEntities True to resolve unmapped URLs; false to throw an exception.
       */
      public MappedResolver(Lookup schemaMap, boolean bResolveUnmappedEntities)
      {
         m_schemaMap = schemaMap;
         m_bResolveUnmappedEntities = bResolveUnmappedEntities;
      }

      // operations

      /**
       * @see org.apache.xerces.xni.parser.XMLEntityResolver#resolveEntity(org.apache.xerces.xni.XMLResourceIdentifier)
       */
      public XMLInputSource resolveEntity(XMLResourceIdentifier res) throws XNIException, IOException
      {
         String sIdURL = res.getLiteralSystemId();

         if (sIdURL == null)
         {
            /* Fall-back to using the namespace itself as the schema location.
             * For example, the "xml:" namespace (http://www.w3.org/XML/1998/namespace) often
             * seems to be referenced without giving a schema location (as it is well-known).
             */
            sIdURL = res.getNamespace();
         }

         URL xsdURL = (m_schemaMap == null) ? null : (URL)m_schemaMap.get(sIdURL);

         if (xsdURL == null)
         {
            if (m_bResolveUnmappedEntities)
            {
               return null;
            }

            throw new LookupException("err.xml.missingResourceMapping", new Object[] {sIdURL});
         }

         XMLInputSource result = new XMLInputSource(res);

         result.setByteStream(URLUtil.openStream(xsdURL));

         return result;
      }
   }
}
