// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.xml;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.zip.CRC32;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataCompoundValidationException;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLoader;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataURLHandler;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.SystemResources;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Parser;
import nexj.core.scripting.ParserException;
import nexj.core.scripting.SchemeParser;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashDeque;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.HashTab2D;
import nexj.core.util.Holder;
import nexj.core.util.HolderDeque;
import nexj.core.util.IOUtil;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.LookupDeque;
import nexj.core.util.ObjUtil;
import nexj.core.util.ProgressListener;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.TextPositionReader;
import nexj.core.util.URLUtil;
import nexj.core.util.UncheckedException;
import nexj.core.util.XMLException;
import nexj.core.util.XMLUtil;
import nexj.core.util.cipher.CharacterStreamCipherDispatcher;

/**
 * Helper class for loading metadata using an XML serialization format.
 */
public final class XMLMetadataHelper
{
   // constants

   /**
    * Search the root URL first, and then the base URL.
    */
   public final static int SEARCH_ROOT_THEN_BASE = 0;

   /**
    * Search the root URL only.
    */
   public final static int SEARCH_ROOT_ONLY = 1;

   /**
    * Search the base URL only.
    */
   public final static int SEARCH_BASE_ONLY = 2;

   /**
    * Search the root URL first, then the base URL, then the mixin URLs.
    */
   public final static int SEARCH_ALL = 3;

   /**
    * Basic identifier starting with a letter or _ followed by letters, _ or digits.
    */
   public final static int NAME_ID = 0x00;

   /**
    * Periods are allowed in the name.
    */
   public final static int NAME_DOT = 0x01;

   /**
    * Special characters are allowed in the name: - ? !
    */
   public final static int NAME_SPEC = 0x02;

   /**
    * Colons are allowed the name for scoping.
    */
   public final static int NAME_SCOPE = 0x10;

   /**
    * The metadata XSD URL.
    */
   public final static URL XSD_URL = XMLMetadataHelper.class.getResource("metadata.xsd");

   /**
    * The base types XSD URL.
    */
   public final static URL BASE_TYPES_URL = XMLMetadataHelper.class.getResource("baseTypes.xsd");

   /**
    * The types XSD URL.
    */
   public final static URL TYPES_URL = XMLMetadataHelper.class.getResource("types.xsd");

   /**
    * The types XSD URL.
    */
   public final static URL BASE_METADATA_URL = XMLMetadataHelper.class.getResource("baseMetadata.xsd");

   /**
    * Invalid character matching algorithm, to be used to generate property and directory names from repository namespaces.
    */
   private final static Pattern INVALID_SUBSTRING_PATTERN = Pattern.compile("(^[^a-zA-Z]+)|([^a-zA-Z0-9]+)");

   /**
    * The default schema URL deque.
    */
   public final static LookupDeque DEFAULT_SCHEMA_URL_DEQUE = new LinkedHashTab(3);

   /**
    * The name of the repository descriptor.
    */
   public final static String DESCRIPTOR_NAME = ".metadata";

   /**
    * Old metadata node names.
    */
   protected final static Set OLD_METADATA_NODE_NAME_SET = new HashHolder(27);

   static
   {
      OLD_METADATA_NODE_NAME_SET.add("Actions");
      OLD_METADATA_NODE_NAME_SET.add("Channels");
      OLD_METADATA_NODE_NAME_SET.add("ChannelTypes");
      OLD_METADATA_NODE_NAME_SET.add("ClassDiagrams");
      OLD_METADATA_NODE_NAME_SET.add("Classes");
      OLD_METADATA_NODE_NAME_SET.add("Components");
      OLD_METADATA_NODE_NAME_SET.add("DataSources");
      OLD_METADATA_NODE_NAME_SET.add("DataSourceTypes");
      OLD_METADATA_NODE_NAME_SET.add("Dumps");
      OLD_METADATA_NODE_NAME_SET.add("Enumerations");
      OLD_METADATA_NODE_NAME_SET.add("ExternalLibraries");
      OLD_METADATA_NODE_NAME_SET.add("Formats");
      OLD_METADATA_NODE_NAME_SET.add("Interfaces");
      OLD_METADATA_NODE_NAME_SET.add("Libraries");
      OLD_METADATA_NODE_NAME_SET.add("Licenses");
      OLD_METADATA_NODE_NAME_SET.add("Locales");
      OLD_METADATA_NODE_NAME_SET.add("Messages");
      OLD_METADATA_NODE_NAME_SET.add("RuleSets");
      OLD_METADATA_NODE_NAME_SET.add("SecurityDescriptors");
      OLD_METADATA_NODE_NAME_SET.add("Services");
      OLD_METADATA_NODE_NAME_SET.add("Strings");
      OLD_METADATA_NODE_NAME_SET.add("Transformations");
      OLD_METADATA_NODE_NAME_SET.add("Upgrades");
      OLD_METADATA_NODE_NAME_SET.add("WorkflowQueues");
      OLD_METADATA_NODE_NAME_SET.add("Workflows");
      OLD_METADATA_NODE_NAME_SET.add("WSDLs");
      OLD_METADATA_NODE_NAME_SET.add("XSDs");
   }

   // attributes

   /**
    * Search mode, one of the SEARCH_* constants.
    */
   private int m_nSearchMode = SEARCH_ALL;

   /**
    * The relative name of the resource (XML file)
    * that is currently being processed.
    */
   private String m_sCurResourceName;

   /**
    * The cached base checksum. Null if not yet cached.
    */
   private String m_sBaseChecksum;

   /**
    * The .metadata file name.
    */
   private String m_sMetadataFileName = DESCRIPTOR_NAME;

   // associations

   /**
    * The root URL from which to get the data.
    */
   private URL m_rootURL;

   /**
    * The base URL, which serves as a fallback root URL.
    */
   private URL m_baseURL;

   /**
    * The resource listing.
    */
   protected XMLMetadataListing m_listing;

   /**
    * The properties for overriding various values.
    */
   private Properties m_properties;

   /**
    * The URL handler when loading from a dynamic metadata store.
    */
   protected URLStreamHandler m_handler;

   /**
    * The Scheme parser.
    */
   private SchemeParser m_parser;

   /**
    * The metadata upgrader.
    */
   private XMLMetadataUpgrader m_upgrader;

   /**
    * The metadata upgrader state.
    */
   protected Object m_upgraderState;

   /**
    * The cached base descriptor element. Null if not yet cached.
    */
   private Element m_baseDescriptorElement;

   /**
    * The cached root descriptor element. Null if not yet cached.
    */
   protected Element m_rootDescriptorElement;

   /**
    * The metadata validation exception container.
    */
   private MetadataCompoundValidationException m_exception = new MetadataCompoundValidationException();

   /**
    * The warning container. Null if warnings are disabled.
    */
   protected ExceptionHolder m_warnings;

   /**
    * The set of encryption schemes that were used for encrypting passwords
    * and connection and server files.
    */
   protected Set m_encryptionSchemeSet; // of type String

   /**
    * The validation exception context marker stack.
    */
   private List m_markerList = new ArrayList(10); // of type String[2*n], String[2*n+1]

   /**
    * The dynamically loaded class instance map (class name to class instance).
    */
   private Lookup m_instanceMap; // of type Object[String]

   /**
    * The logger.
    */
   private static Logger s_logger = Logger.getLogger(XMLMetadataHelper.class);

   static
   {
      try
      {
         DEFAULT_SCHEMA_URL_DEQUE.put(XSD_URL.toExternalForm(), XSD_URL);
         DEFAULT_SCHEMA_URL_DEQUE.put("baseTypes.xsd", BASE_TYPES_URL);
         DEFAULT_SCHEMA_URL_DEQUE.put("types.xsd", TYPES_URL);
         DEFAULT_SCHEMA_URL_DEQUE.put("baseMetadata.xsd", BASE_METADATA_URL);
      }
      catch (Throwable t)
      {
         ObjUtil.rethrow(t);
      }
   }

   // constructors

   public XMLMetadataHelper()
   {
   }

   /**
    * Creates a new metadata helper initialized with the repository root URL.
    * @param rootURL The repository root URL.
    * @param baseURL The fallback URL for resources that are not found under the root URL.
    * @param listing The resource listing.  If null, listing for base and root will be lazily constructed if required to perform an operation.
    * @param properties The metadata properties for overriding various values.
    */
   public XMLMetadataHelper(URL rootURL, URL baseURL, Properties properties, XMLMetadataListing listing)
   {
      m_rootURL = rootURL;
      m_baseURL = baseURL;
      m_properties = properties;
      m_listing = listing;
   }

   /**
    * Creates a new metadata helper initialized with the repository root URL.
    * @param rootURL The repository root URL.
    * @param baseURL The fallback URL for resources that are not found under the root URL.
    * @param properties The metadata properties for overriding various values.
    * @param listing The resource listing.  If null, listing for base and root will be lazily constructed if required to perform an operation.
    * @param upgrader The metadata upgrader.
    */
   public XMLMetadataHelper(URL rootURL, URL baseURL, Properties properties, XMLMetadataListing listing, XMLMetadataUpgrader upgrader)
   {
      m_upgrader = upgrader;
      m_rootURL = rootURL;
      m_baseURL = baseURL;
      m_properties = properties;
      m_listing = listing;
   }

   /**
    * Creates a new metadata helper initialized with the repository root URL.
    * @param upgrader The metadata upgrader.
    * @param rootURL The repository root URL.
    * @param baseURL The fallback URL for resources that are not found under the root URL.
    * @param properties The metadata properties for overriding various values.
    * @param listing The resource listing.  If null, listing for base and root will be lazily constructed if required to perform an operation.
    * @param handler The URL handler when loading from a dynamic metadata store.
    */
   public XMLMetadataHelper(XMLMetadataUpgrader upgrader, URL rootURL, URL baseURL, Properties properties, XMLMetadataListing listing, URLStreamHandler handler)
   {
      m_upgrader = upgrader;
      m_rootURL = rootURL;
      m_baseURL = baseURL;
      m_properties = properties;
      m_listing = listing;
      m_handler = handler;
   }

   // operations

   /**
    * @return The repository root URL.
    */
   public URL getRootURL()
   {
      return m_rootURL;
   }

   /**
    * @return The repository base URL (fallback URL for resources that are not found under the root URL).
    */
   public URL getBaseURL()
   {
      return m_baseURL;
   }

   /**
    * Sets the search mode.
    * @param nSearchMode One of the SEARCH_* constants.
    */
   public void setSearchMode(int nSearchMode)
   {
      m_nSearchMode = nSearchMode;
   }

   /**
    * @return The search mode, one of the SEARCH_* constants.
    */
   public int getSearchMode()
   {
      return m_nSearchMode;
   }

   /**
    * Sets the resource upgrader
    * @param upgrader The resource upgrader.
    */
   public void setUpgrader(XMLMetadataUpgrader upgrader)
   {
      m_upgrader = upgrader;
   }

   /**
    * @return The resource upgrader.
    */
   public XMLMetadataUpgrader getUpgrader()
   {
      return m_upgrader;
   }

   /**
    * Sets the metadata upgrader state.
    * @param upgraderState The metadata upgrader state to set.
    */
   public void setUpgraderState(Object upgraderState)
   {
      m_upgraderState = upgraderState;
   }

   /**
    * @return The metadata upgrader state.
    */
   public Object getUpgraderState()
   {
      return m_upgraderState;
   }

   /**
    * @return The name of the currently processed resource.
    */
   public String getCurResourceName()
   {
      return m_sCurResourceName;
   }

   /**
    * Pushes a validation exception context marker.
    * @param sName Marker property name.
    * @param sValue Marker property value.
    * @return A cookie for restoring the previous context.
    */
   public int pushMarker(String sName, String sValue)
   {
      m_markerList.add(sName);
      m_markerList.add(sValue);

      return m_markerList.size() - 2;
   }

   /**
    * Restores a validation exception context marker to a previous level.
    * @param nCookie  A cookie returned by pushMarker().
    */
   public void restoreMarker(int nCookie)
   {
      while (m_markerList.size() > nCookie)
      {
         m_markerList.remove(m_markerList.size() - 1);
      }
   }

   /**
    * Clears the context marker.
    */
   public void clearMarker()
   {
      m_markerList.clear();
   }

   /**
    * Sets the metadata validation exception marker to the current marker.
    * @param e The metadata validation exception to modify.
    */
   public void setMarker(MetadataValidationException e)
   {
      for (int i = 0; i < m_markerList.size(); i += 2)
      {
         e.setProperty((String)m_markerList.get(i), (String)m_markerList.get(i + 1));
      }
   }

   /**
    * Create a lookup of the current marker values.
    * @return A map containing the current marker values;
    */
   public Lookup saveMarkerState()
   {
      Lookup markerLookup = new HashTab(m_markerList.size() / 2);

      for (int i = 0; i < m_markerList.size(); i += 2)
      {
         markerLookup.put(m_markerList.get(i), m_markerList.get(i + 1));
      }

      return markerLookup;
   }

   /**
    * @return The compound validation exception.
    */
   public MetadataCompoundValidationException getException()
   {
      return m_exception;
   }

   /**
    * Adds a metadata validation exception to the compound exception.
    * Sets the resource name to the current resource, if not yet set.
    * @param e The exception to add.
    */
   public void addException(MetadataValidationException e)
   {
      if (e.getResourceName() == null)
      {
         e.setResourceName(getCurResourceName());
      }

      getException().addException(e);
   }

   /**
    * Wraps an unchecked exception as a metadata validation exception,
    * assigns the current context marker to it and adds it to the compound exception.
    * @param e The exception to add.
    */
   public void addException(UncheckedException e)
   {
      if (e instanceof MetadataCompoundValidationException &&
         ((MetadataCompoundValidationException)e).getExceptionCount() != 0)
      {
         if (e != m_exception)
         {
            MetadataCompoundValidationException x = (MetadataCompoundValidationException)e;

            for (Iterator itr = x.getExceptionIterator(); itr.hasNext();)
            {
               addException((UncheckedException)itr.next());
            }
         }
      }
      else if (e instanceof MetadataValidationException)
      {
         addException((MetadataValidationException)e);
      }
      else
      {
         MetadataValidationException x = new MetadataValidationException(e);

         setMarker(x);
         getException().addException(x);
      }
   }

   /**
    * @throws MetadataCompoundValidationException if any errors have been accumulated.
    */
   public void checkForError()
   {
      if (m_exception.getExceptionCount() > 0)
      {
         throw m_exception;
      }
   }

   /**
    * Sets the warning container.
    * @param warnings The warning container to set. Can be null.
    */
   public void setWarnings(ExceptionHolder warnings)
   {
      m_warnings = warnings;
   }

   /**
    * @return The warning container. Null if warnings are disabled.
    */
   public ExceptionHolder getWarnings()
   {
      return m_warnings;
   }

   /**
    * Gets an information object for the repository descriptor.
    * @param bRoot True to get the root repository descriptor,
    * false to get the base repository descriptor.
    * @return The resource object.
    * @throws MetadataException if the descriptor was not found.
    */
   public XMLResource getDescriptorResource(boolean bRoot)
   {
      if (!bRoot && getBaseURL() == null)
      {
         throw new MetadataException("err.meta.resourceOpen", new Object[]{m_sMetadataFileName});
      }

      InputStream istream = null;

      try
      {
         if (m_sMetadataFileName.length() == 0 || m_sMetadataFileName.charAt(0) == '/')
         {
            throw new MalformedURLException("Invalid resource name \"" + m_sMetadataFileName + "\"");
         }

         URL url = new URL((bRoot) ? m_rootURL : m_baseURL, m_sMetadataFileName);

         istream = URLUtil.openStream(url);

         if (s_logger.isDumpEnabled())
         {
            s_logger.dump("Found resource \"" + m_sMetadataFileName + "\" at URL=\"" + url + "\"");
         }

         return new XMLResource(m_sMetadataFileName, url, bRoot);
      }
      catch (Exception e)
      {
         throw new MetadataException("err.meta.resourceOpen", new Object[]{m_sMetadataFileName}, e);
      }
      finally
      {
         IOUtil.close(istream);
      }
   }

   /**
    * Returns the root document element of the repository descriptor.
    * @param bRoot True to get the root repository descriptor,
    * false to get the base repository descriptor.
    * @return The root document element of the descriptor.
    * @throws MetadataException if the loading failed.
    */
   public Element getDescriptorElement(boolean bRoot)
   {
      int nSearchModeSaved = getSearchMode();
      XMLMetadataListing savedListing = m_listing;

      if (bRoot)
      {
         if (m_rootDescriptorElement != null)
         {
            return m_rootDescriptorElement;
         }

         if (m_rootURL == null)
         {
            return null;
         }
      }
      else
      {
         if (m_baseDescriptorElement != null)
         {
            return m_baseDescriptorElement;
         }

         if (m_baseURL == null)
         {
            return null;
         }
      }

      int nCookie = pushMarker(MetadataValidationException.TYPE_NAME, "Metadata");

      try
      {
         final Element[] elementArray = new Element[1];
         Lookup resourceMap = new HashTab(1);

         m_listing = new XMLMetadataListing();

         if (bRoot)
         {
            setSearchMode(SEARCH_ROOT_ONLY);
            resourceMap.put(m_sMetadataFileName,
               new XMLResource(m_sMetadataFileName,  new URL(m_rootURL, m_sMetadataFileName), true));
            m_listing.setRootResourceMap(resourceMap);
         }
         else
         {
            setSearchMode(SEARCH_BASE_ONLY);
            resourceMap.put(m_sMetadataFileName,
               new XMLResource(m_sMetadataFileName, new URL(m_baseURL, m_sMetadataFileName), false));
            m_listing.setBaseResourceMap(resourceMap);
         }

         m_listing.setResourceMap(resourceMap);

         loadResource(m_sMetadataFileName, m_sMetadataFileName, new ResourceHandler()
         {
            public void handleResource(Element metadataElement, String sName)
            {
               if (!metadataElement.getNodeName().equals("Metadata"))
               {
                  throw new MetadataException("err.meta.docRoot",
                        new Object[]{metadataElement.getNodeName(), "Metadata"});
               }

               elementArray[0] = metadataElement;
            }
         });

         if (bRoot)
         {
            m_rootDescriptorElement = elementArray[0];
         }
         else
         {
            m_baseDescriptorElement = elementArray[0];
         }

         return elementArray[0];
      }
      catch (MalformedURLException e)
      {
         throw new MetadataException("err.meta.resourceOpen", new Object[]{m_sMetadataFileName}, e);
      }
      catch (UncheckedException e)
      {
         MetadataValidationException x = new MetadataValidationException(e);
         setMarker(x);

         throw x;
      }
      finally
      {
         restoreMarker(nCookie);
         setSearchMode(nSearchModeSaved);
         m_listing = savedListing;
      }
   }

   /**
    * Adds all resources with the specified extension to a map.
    * @param resourceMap The map where to add the resource names and paths (String[String]).
    * @param sExt The extension to chop off the resource name.
    * @param sMarkerTypeName The metadata marker type name for MetadataValidationException.
    * @param sMarkerPropertyName The metadata marker property name for MetadataValidationException.
    */
   public void addResources(Lookup resourceMap, String sExt, String sMarkerTypeName, String sMarkerPropertyName)
   {
      Lookup extensionMap = getListing().getExtensionMap();

      if (extensionMap == null)
      {
         return;
      }

      List resList = (List)extensionMap.get(sExt);

      if (resList != null)
      {
         for (Iterator itr = resList.iterator(); itr.hasNext(); )
         {
            String sResName = (String)itr.next();
            String sElName = getResourceName(sResName);
            Object oldResName = resourceMap.put(sElName, sResName);

            if (oldResName != null && !sResName.equals(oldResName))
            {
               MetadataValidationException e = new MetadataValidationException("err.meta.multiPathResource",
                  new Object[]{sResName, oldResName});

               e.setResourceName(sResName);
               e.setTypeName(sMarkerTypeName);
               e.setProperty(sMarkerPropertyName, sElName);

               addException(e);
            }
         }
      }

      // Process files added by the upgrades
      if (m_upgrader != null)
      {
         for (Iterator itr = m_upgrader.getNewResourceIterator(sExt); itr.hasNext(); )
         {
            String sResName = (String)itr.next();
            String sElName = getResourceName(sResName);
            Object oldResName = resourceMap.put(sElName, sResName);

            if (oldResName != null)
            {
               // Resources of the same (simple) name from the metadata take precedence
               resourceMap.put(sElName, oldResName);
            }
         }
      }

      checkForError();
   }

   /**
    * Loads all resources with the specified extension.
    * @param sExt The extension to chop off the resource name.
    * @param sMarkerTypeName The metadata marker type name for MetadataValidationException.
    * @param sMarkerPropertyName The metadata marker property name for MetadataValidationException.
    * @param handler The handler which processes the resource root DOM element.
    * @param progress The progress listener. Can be null.
    */
   public void loadResources(String sExt, String sMarkerTypeName, String sMarkerPropertyName,
      ResourceHandler handler, ProgressListener progress)
   {
      loadResources(
         sExt, sMarkerTypeName, sMarkerPropertyName,
         new UpgradingResourceCharacterStreamHandler(handler), progress);
   }

   /**
    * Loads all resources with the specified extension.
    * @param sExt The extension to chop off the resource name.
    * @param sMarkerTypeName The metadata marker type name for MetadataValidationException.
    * @param sMarkerPropertyName The metadata marker property name for MetadataValidationException.
    * @param handler The handler which processes the resource character stream.
    * @param progress The progress listener. Can be null.
    */
   public void loadResources(String sExt, String sMarkerTypeName, String sMarkerPropertyName,
      final CharacterStreamHandler streamHandler, ProgressListener progress)
   {
      loadResources(sExt, sMarkerTypeName, sMarkerPropertyName, new ResourceNameHandler()
      {
         public void handleResource(String sBaseName, String sFullName)
         {
            loadResource(sFullName, sBaseName, streamHandler);
         }
      }, progress);
   }

   /**
    * Loads all resources with the specified extension.
    * @param sExt The extension to chop off the resource name.
    * @param sMarkerTypeName The metadata marker type name for MetadataValidationException.
    * @param sMarkerPropertyName The metadata marker property name for MetadataValidationException.
    * @param handler The handler which processes the resource.
    * @param progress The progress listener. Can be null.
    */
   public void loadResources(String sExt, String sMarkerTypeName, String sMarkerPropertyName,
      ResourceNameHandler handler, ProgressListener progress)
   {
      Lookup resourceMap = new LinkedHashTab(256);

      addResources(resourceMap, sExt, sMarkerTypeName, sMarkerPropertyName);

      int nCount = 0;

      for (Lookup.Iterator itr = resourceMap.iterator(); itr.hasNext();)
      {
         String sBaseName = (String)itr.next();
         String sFullName = (String)itr.getValue();
         String sResourceNameSaved = m_sCurResourceName;
         int nCookie = pushMarker(MetadataValidationException.RESOURCE_NAME, sFullName);

         pushMarker(MetadataValidationException.TYPE_NAME, sMarkerTypeName);
         pushMarker(sMarkerPropertyName, sBaseName);
         m_sCurResourceName = sFullName;

         try
         {
            if (progress != null)
            {
               progress.progress("info.meta.loadingResource", new Object[]{sFullName},
                  (double)nCount++ / resourceMap.size());
            }

            handler.handleResource(sBaseName, sFullName);
         }
         catch (UncheckedException e)
         {
            addException(e);
         }
         finally
         {
            restoreMarker(nCookie);
            m_sCurResourceName = sResourceNameSaved;
         }
      }

      checkForError();
   }

   /**
    * Validates resources given a map of their name to path.
    * @param resourceMap The map containing key-value pairs of name key to path value.
    * @param sMarkerTypeName The metadata marker type name for MetadataValidationException.
    * @param sMarkerPropertyName The metadata marker property name for MetadataValidationException.
    * @param progress The progress listener. Can be null.
    */
   protected void validateResources(Lookup resourceMap, String sMarkerTypeName,
      String sMarkerPropertyName, ResourceNameHandler validationHandler,
      ProgressListener progress)
   {
      String sResourceNameSaved = m_sCurResourceName;

      try
      {
         Lookup.Iterator itr = resourceMap.iterator();
         int nCount = 0;

         while (itr.hasNext())
         {
            int nCookie = pushMarker(MetadataValidationException.TYPE_NAME, sMarkerTypeName);
            String sBaseName = (String)itr.next();
            String sFullName = (String)itr.getValue();
            pushMarker(MetadataValidationException.RESOURCE_NAME, sFullName);
            pushMarker(sMarkerPropertyName, sBaseName);

            try
            {
               if (progress != null)
               {
                  progress.progress("info.meta.validatingResource", new Object[]{sFullName},
                     (double)nCount++ / resourceMap.size());
               }

               m_sCurResourceName = sFullName;
               validationHandler.handleResource(sBaseName, sFullName);
            }
            catch (UncheckedException e)
            {
               addException(e);
            }
            finally
            {
               restoreMarker(nCookie);
            }
         }

         checkForError();
      }
      finally
      {
         m_sCurResourceName = sResourceNameSaved;
      }
   }

   /**
    * Loads the specified resource from the repository.
    * @param sResName The name of the resource relative to the repository root.
    * @param sName The name of the item corresponding to the resource.
    * @param handler The handler which processes the resource DOM root element.
    */
   public void loadResource(String sResName, String sName, ResourceHandler handler)
   {
      loadResource(sResName, sName, new UpgradingResourceCharacterStreamHandler(handler));
   }

   /**
    * Loads the specified resource from the repository.
    * @param sResName The name of the resource relative to the repository root.
    * @param sName The name of the item corresponding to the resource.
    * @param handler The handler which processes the resource DOM root element.
    * @param schemaURLDeque The mapping of schema URLs to other URLs that will
    * be used as the actual locations of those schemas, in the schema parse order.
    */
   public void loadResource(String sResName, String sName, ResourceHandler handler, LookupDeque schemaURLDeque)
   {
      loadResource(
         sResName, sName, new UpgradingResourceCharacterStreamHandler(handler, schemaURLDeque));
   }

   /**
    * Loads the specified resource from a given reader.
    * @param sResName The name of the resource, relative to the repository root.
    * @param sName The name of the item corresponding to the resource.
    * @param handler The handler which processes the resource DOM root element.
    * @param schemaURLDeque The mapping of schema URLs to other URLs that will
    * be used as the actual locations of those schemas, in the schema parse order.
    * @param reader Reader to provide the resource data.
    */
   public void loadResource(String sResName, String sName, ResourceHandler handler, LookupDeque schemaURLDeque, Reader reader)
   {
      loadResource(
         sResName, sName,
         new UpgradingResourceCharacterStreamHandler(handler, schemaURLDeque), reader);
   }

   /**
    * Loads the specified resource from a given reader.
    * @param sResName The name of the resource, relative to the repository root.
    * @param sName The name of the item corresponding to the resource.
    * @param handler The handler which processes the resource DOM root element.
    * @param reader Reader to provide the resource data.
    */
   public void loadResource(String sResName, String sName, CharacterStreamHandler handler, Reader reader)
   {
      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("Loading resource \"" + sResName + "\"");
      }

      String sResourceNameSaved = m_sCurResourceName;
      int nCookie = pushMarker(MetadataValidationException.RESOURCE_NAME, sResName);

      m_sCurResourceName = sResName;

      try
      {
         handler.handleCharacterStream(reader, sName);
      }
      catch (IOException e)
      {
         throw new MetadataException("err.meta.resourceOpen", new Object[]{sResName}, e);
      }
      finally
      {
         m_sCurResourceName = sResourceNameSaved;
         restoreMarker(nCookie);
      }
   }

   /**
    * Loads the specified resource from the repository.
    * @param sResName The name of the resource relative to the repository root.
    * @param sName The name of the item corresponding to the resource.
    * @param handler The handler which processes the resource character stream.
    */
   public void loadResource(String sResName, String sName, CharacterStreamHandler handler)
   {
      Reader reader = getResourceAsReader(sResName);

      try
      {
         loadResource(sResName, sName, handler, reader);
      }
      finally
      {
         IOUtil.close(reader);
      }
   }

   /**
    * Loads the specified system resource.
    * @param sResName The resource name, relative to nexj.core.meta.sys package.
    * @param sMarkerTypeName The metadata marker type name for MetadataValidationException.
    * @param sMarkerPropertyName The metadata marker property name for MetadataValidationException.
    * @param handler The handler which processes the resource root DOM element.
    */
   public void loadSystemResource(String sResName, String sMarkerTypeName, String sMarkerPropertyName,
      ResourceHandler handler)
   {
      loadSystemResource(sResName, sMarkerTypeName, sMarkerPropertyName, new ResourceCharacterStreamHandler(handler));
   }

   /**
    * Loads the specified system resource.
    * @param sResName The resource name, relative to nexj.core.meta.sys package.
    * @param sMarkerTypeName The metadata marker type name for MetadataValidationException.
    * @param sMarkerPropertyName The metadata marker property name for MetadataValidationException.
    * @param handler The handler which processes the resource character stream.
    */
   public void loadSystemResource(String sResName, String sMarkerTypeName,
      String sMarkerPropertyName, CharacterStreamHandler handler)
   {
      String sName = getSystemResourceName(sResName);

      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("Loading system resource \"" + sResName + "\"");
      }

      URL url;

      try
      {
         url = (URLUtil.isURL(sResName)) ? new URL(sResName) : SystemResources.find(sResName);
      }
      catch (IOException e)
      {
         throw new MetadataException("err.meta.resourceOpen", new Object[]{sResName}, e);
      }

      loadURL(url, sResName, sName, sMarkerTypeName, sMarkerPropertyName, handler);
   }

   /**
    * Loads an encrypted resource from the given URL.
    * @param resourceURL The URL of the resource to load.
    * @param sMarkerTypeName The metadata marker type name for MetadataValidationException.
    * @param sMarkerPropertyName The metadata marker property name for MetadataValidationException.
    * @param handler The handler which processes the resource character stream.
    * @param properties The properties to use for decryption.
    */
   public void loadEncryptedResourceURL(URL resourceURL, String sMarkerTypeName, String sMarkerPropertyName,
      ResourceHandler handler, Properties properties)
   {
      CharacterStreamHandler streamHandler = new UpgradingResourceCharacterStreamHandler(handler);
      EncryptedCharacterStreamHandler characterStreamHandler =
            new EncryptedCharacterStreamHandler(streamHandler, properties);

      characterStreamHandler.setEncryptionSchemeSet(m_encryptionSchemeSet);
      loadResourceURL(resourceURL, sMarkerTypeName, sMarkerPropertyName, characterStreamHandler);
   }

   /**
    * Loads a resource from the given URL.
    * @param resourceURL The URL of the resource to load.
    * @param sMarkerTypeName The metadata marker type name for MetadataValidationException.
    * @param sMarkerPropertyName The metadata marker property name for MetadataValidationException.
    * @param handler The handler which processes the resource character stream.
    */
   public void loadResourceURL(URL resourceURL, String sMarkerTypeName,
      String sMarkerPropertyName, CharacterStreamHandler handler)
   {
      String sName = getSystemResourceName(resourceURL.toString());

      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("Loading resource URL \"" + resourceURL + '"');
      }

      loadURL(resourceURL, resourceURL.toString(), sName, sMarkerTypeName, sMarkerPropertyName, handler);
   }

   /**
    * Loads a resource from the given URL.
    * @param url The URL of the resource to load.
    * @param sResName The resource name.
    * @param sBaseName The resource name without path or extension.
    * @param sMarkerTypeName The metadata marker type name for MetadataValidationException.
    * @param sMarkerPropertyName The metadata marker property name for MetadataValidationException.
    * @param handler The handler which processes the resource character stream.
    */
   protected void loadURL(URL url, String sResName, String sBaseName,
      String sMarkerTypeName, String sMarkerPropertyName, CharacterStreamHandler handler)
   {
      Reader reader = null;
      String sResourceNameSaved = m_sCurResourceName;
      int nCookie = pushMarker(MetadataValidationException.TYPE_NAME, sMarkerTypeName);

      pushMarker(sMarkerPropertyName, sBaseName);

      try
      {
         m_sCurResourceName = sResName;
         pushMarker(MetadataValidationException.RESOURCE_NAME, sResName);

         try
         {
            reader = new InputStreamReader(URLUtil.openStream(url), XMLUtil.ENCODING);
            reader = new BufferedReader(reader);
            handler.handleCharacterStream(reader, sBaseName);
         }
         catch (IOException e)
         {
            throw new MetadataException("err.meta.resourceOpen", new Object[]{sResName}, e);
         }
      }
      catch (UncheckedException e)
      {
         addException(e);
      }
      finally
      {
         IOUtil.close(reader);
         restoreMarker(nCookie);
         m_sCurResourceName = sResourceNameSaved;
      }

      checkForError();
   }

   /**
    * Finds a resource by name.
    * @param sName The resource name.
    * @param nSearchMode The search mode, one of the SEARCH_* constants.
    * @return The resource object, or null if not found.
    */
   public XMLResource findResource(String sName, int nSearchMode)
   {
      int nSearchModeSaved = m_nSearchMode;

      try
      {
         m_nSearchMode = nSearchMode;

         return findResource(sName);
      }
      finally
      {
         m_nSearchMode = nSearchModeSaved;
      }
   }

   /**
    * Finds a resource by name.
    * @param sName The resource name.
    * @return The resource object, or null if not found.
    */
   public XMLResource findResource(String sName)
   {
      XMLResource resource = null;
      XMLMetadataListing listing = getListing();

      if (sName.length() == 0 || sName.charAt(0) == '/')
      {
         return null;
      }

      if (m_nSearchMode == SEARCH_ALL)
      {
         Lookup resourceMap = listing.getResourceMap();

         resource = (resourceMap == null) ? null : (XMLResource)resourceMap.get(sName);
      }
      else
      {
         if (m_nSearchMode != SEARCH_BASE_ONLY)
         {
            Lookup rootMap = listing.getRootResourceMap();

            resource = (rootMap == null) ? null : (XMLResource)rootMap.get(sName);
         }

         if (resource == null && m_nSearchMode != SEARCH_ROOT_ONLY)
         {
            Lookup baseMap = listing.getBaseResourceMap();

            resource = (baseMap == null) ? null : (XMLResource)baseMap.get(sName);
         }
      }

      // check for a new resource from framework upgrades
      if (resource == null && m_upgrader != null)
      {
         URL url = m_upgrader.getNewResourceURL(sName);

         resource = (url == null) ? null : new XMLResource(sName, url, true);
      }

      if (s_logger.isDumpEnabled())
      {
         if (resource != null)
         {
            s_logger.dump("Found resource \"" + sName + "\" at URL=\"" + resource.getURL() + "\"");
         }
      }

      return resource;
   }

   /**
    * Gets an information object for the specified resource.
    * @param sName The resource name.
    * @return The resource object.
    * @throws MetadataException if the resource was not found.
    */
   public XMLResource getResource(String sName)
   {
      XMLResource resource = findResource(sName);

      if (resource == null)
      {
         throw new MetadataException("err.meta.resourceOpen", new Object[]{sName});
      }

      return resource;
   }

   /**
    * Gets an input stream for the specified resource.
    * @param sName The resource name.
    * @return The input stream.
    */
   public InputStream getResourceAsStream(String sName)
   {
      XMLResource resource = getResource(sName);

      try
      {
         return new BufferedInputStream(URLUtil.openStream(resource.getURL()), IOUtil.BUFFER_SIZE);
      }
      catch (Throwable t)
      {
         throw new MetadataException("err.meta.resourceOpen", new Object[]{sName}, t);
      }
   }

   /**
    * Opens a UTF-8 text input stream for the specified resource.
    * @param sName The resource name, relative to the root.
    * @return The binary stream. Closing it is a caller's responsibility.
    * @throws MetadataException if the resource was not found.
    */
   public Reader getResourceAsReader(String sName)
   {
      try
      {
         return IOUtil.openBufferedReader(getResourceAsStream(sName), XMLUtil.ENCODING);
      }
      catch (MetadataException e)
      {
         throw e;
      }
      catch (Exception e)
      {
         throw new MetadataException("err.meta.resourceOpen", new Object[]{sName}, e);
      }
   }

   /**
    * Fixes up all the references in a collection.
    * @param itr The collection iterator over Fixup instances.
    */
   public void fixup(Iterator itr)
   {
      while (itr.hasNext())
      {
         Fixup fixup = (Fixup)itr.next();

         try
         {
            fixup.fixup();
         }
         catch (MetadataCompoundValidationException e)
         {
            for (Iterator xitr = e.getExceptionIterator(); xitr.hasNext();)
            {
               UncheckedException ux = (UncheckedException)xitr.next();
               MetadataValidationException x;

               if (ux instanceof MetadataValidationException)
               {
                  x = (MetadataValidationException)ux;

                  if (x.getResourceName() == null)
                  {
                     fixup.setMarker(x);
                  }
               }
               else
               {
                  x = new MetadataValidationException(ux);
                  fixup.setMarker(x);
               }

               addException(x);
            }
         }
         catch (MetadataValidationException e)
         {
            if (e.getResourceName() == null)
            {
               fixup.setMarker(e);
            }

            addException(e);
         }
         catch (UncheckedException e)
         {
            MetadataValidationException x = new MetadataValidationException(e);
            fixup.setMarker(x);
            addException(x);
         }
      }

      checkForError();
   }

   /**
    * Calculate checksum for a specified resource.
    * @param sResName The name of the resource to calculate checksum for.
    * @param crc32 The checksum to update.
    * @throws MetadataException if resource was not found.
    */
   protected void updateResourceChecksum(CRC32 crc32, String sResName)
   {
      InputStream in = null;

      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("Calculating checksum for resource \"" + sResName + "\"");
      }

      try
      {
         in = getResourceAsStream(sResName);
         byte[] nBuf = new byte[2048];
         int nCount;

         while ((nCount = in.read(nBuf)) > 0)
         {
            crc32.update(nBuf, 0, nCount);
         }
      }
      catch (IOException e)
      {
         throw new MetadataException("err.meta.resourceOpen", new Object[]{sResName}, e);
      }
      finally
      {
         IOUtil.close(in);
      }
   }

   /**
    * Gets the repository namespace.
    * @param bRoot True to get the root repository namespace, false to get the base repository namespace.
    * @return The repository namespace.
    */
   public String getNamespace(boolean bRoot)
   {
      return XMLUtil.getReqStringAttr(getDescriptorElement(bRoot), "namespace");
   }

   /**
    * Gets the repository name.
    * @param bRoot True to get the root repository name, false to get the base repository name.
    * @return The repository name.
    */
   public String getName(boolean bRoot)
   {
      return XMLUtil.getReqStringAttr(getDescriptorElement(bRoot), "name");
   }

   /**
    * Gets the repository version.
    * @param bRoot True to get the root repository version, false to get the base repository version.
    * @return The repository version.
    */
   public String getVersion(boolean bRoot)
   {
      return XMLUtil.getReqStringAttr(getDescriptorElement(bRoot), "version");
   }

   /**
    * Gets the repository revision.
    * @param bRoot True to get the root repository revision, false to get the base repository revision.
    * @return The repository revision.
    */
   public String getRevision(boolean bRoot)
   {
      return XMLUtil.getReqStringAttr(getDescriptorElement(bRoot), "revision");
   }

   /**
    * Calculate checksum for the metadata descriptor and its resources in
    * the order they are list.
    * @param bRoot True to get the root repository checksum,
    *   false to get the base repository checksum.
    * @return Checksum as a hexadecimal string.
    */
   public String getChecksum(boolean bRoot)
   {
      int nSearchModeSaved = getSearchMode();
      String sChecksum = null;

      try
      {
         Element descriptorElement;

         if (bRoot)
         {
            setSearchMode(SEARCH_ROOT_THEN_BASE);
            descriptorElement = mergeDescriptorElements(null, null);
         }
         else
         {
            if (m_sBaseChecksum != null)
            {
               return m_sBaseChecksum;
            }

            setSearchMode(SEARCH_BASE_ONLY);
            descriptorElement = getDescriptorElement(false);

            if (descriptorElement == null)
            {
               m_sBaseChecksum = "";

               return m_sBaseChecksum;
            }

            normalizeDescriptorElement(descriptorElement);
         }

         final CRC32 crc32 = new CRC32();

         crc32.update(XMLUtil.formatXML(descriptorElement).getBytes(XMLUtil.ENCODING));

         // Ensure that metadata published by pre-6.2.0.20 framework still generates identical checksum.
         if (containsOldElements(descriptorElement))
         {
            // process resources in reference order
            XMLUtil.forEachChildElement(descriptorElement, null, new XMLUtil.ElementHandler()
            {
               public void handleElement(final Element containerElement)
               {
                  if (!containerElement.getNodeName().equals("Mixins") &&
                     !containerElement.getNodeName().equals("Resources"))
                  {
                     XMLUtil.forEachChildElement(containerElement, null, new XMLUtil.ElementHandler()
                     {
                        public void handleElement(Element element)
                        {
                           updateResourceChecksum(crc32, XMLUtil.getReqStringAttr(element, "resource"));
                        }
                     });
                  }
               }
            });
         }
         else
         {
            // if resource references are not supplied, process resources in directory listing order
            List resourceList = new ArrayList();
            Set resourceSet = new HashHolder();
            XMLMetadataListing listing = getListing();
            Lookup baseMap = listing.getBaseResourceMap();

            if (bRoot)
            {
               Lookup rootMap = listing.getRootResourceMap();

               for (Iterator itr = rootMap.iterator(); itr.hasNext(); )
               {
                  Object resource = itr.next();

                  resourceList.add(resource);
                  resourceSet.add(resource);
               }
            }

            if (baseMap != null)
            {
               for (Iterator itr = baseMap.iterator(); itr.hasNext(); )
               {
                  Object resource = itr.next();

                  if (!resourceSet.contains(resource))
                  {
                     resourceList.add(resource);
                  }
               }
            }

            Collections.sort(resourceList); // sort resources to ensure consistent checksum

            for (int i = 0; i < resourceList.size(); i++)
            {
               updateResourceChecksum(crc32, (String)resourceList.get(i));
            }
         }

         sChecksum = Long.toHexString(crc32.getValue());

         if (!bRoot)
         {
            m_sBaseChecksum = sChecksum;
         }

         if (s_logger.isDumpEnabled())
         {
            s_logger.dump(((bRoot) ? "Root" : "Base") + " checksum: " + sChecksum);
         }
      }
      catch (UnsupportedEncodingException e)
      {
         ObjUtil.rethrow(e);
      }
      finally
      {
         setSearchMode(nSearchModeSaved);
      }

      return sChecksum;
   }

   /**
    * Check whether element contains old metadata elements.
    * @param descriptorElement Metadata descriptor element.
    * @return Whether element contains old metadata elements.
    */
   private static boolean containsOldElements(Element descriptorElement)
   {
      NodeList nodeList = descriptorElement.getChildNodes();

      for (int i = 0; i < nodeList.getLength(); i++)
      {
         Node node = nodeList.item(i);

         if (node.getNodeType() == Node.ELEMENT_NODE &&
               OLD_METADATA_NODE_NAME_SET.contains(node.getNodeName()))
         {
            return true;
         }
      }

      return false;
   }

   /**
    * Checks if the element is enabled through its attributes op and cond.
    * @param element The element to check.
    * @return True if the element is enabled.
    * @throws MetadataException if the attributes are specified incorrectly.
    */
   public boolean isElementEnabled(Element element)
   {
      String sDir = XMLUtil.getStringAttr(element, "directive");
      String sCond = XMLUtil.getStringAttr(element, "condition");
      boolean bEnabled = XMLUtil.getBooleanAttr(element, "enabled", true);

      if ((sDir != null) != (sCond != null))
      {
         throw new MetadataException("err.meta.elementDirCondMismatch");
      }

      // directive and condition overrides "enabled" attribute, if all are present
      if (sDir == null)
      {
         return bEnabled;
      }

      if (sDir.equals("if") || sDir.equals("ifnot"))
      {
         boolean bValue = false;
         String sValue = getProperty(sCond);

         if (sValue != null)
         {
            sValue = sValue.trim();

            if (sValue.equals("1") || sValue.equalsIgnoreCase("true") || sValue.equalsIgnoreCase("yes"))
            {
               bValue = true;
            }
            else if (!sValue.equals("0") && !sValue.equals("false") && !sValue.equals("no") && !sValue.equals(""))
            {
               throw new MetadataException("err.meta.elementCondBoolean",
                  new Object[]{sValue, sCond});
            }
         }

         return bValue ^ sDir.equals("ifnot");
      }
      else if (sDir.equals("ifdef") || sDir.equals("ifndef"))
      {
         String sValue = getProperty(sCond);

         return (sValue != null && sValue.trim().length() > 0) ^ sDir.equals("ifndef");
      }
      else
      {
         throw new MetadataException("err.meta.elementDirective", new Object[]{sDir});
      }
   }

   /**
    * Returns an element value, possibly overridden by a property.
    * @param element The element which value to return.
    * @return The element value.
    */
   public String getElementValue(Element element)
   {
      String sOverride = XMLUtil.getStringAttr(element, "override");
      String sValue = null;

      if (sOverride != null)
      {
         sValue = m_properties.getProperty(sOverride);
      }

      if (sValue == null)
      {
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
      }

      return sValue;
   }

   /**
    * Returns an attribute value, possibly overridden by a property.
    * @param element The element to which the attribute belongs.
    * @param sName The attribute name.
    * @param sOverrideName The overriding attribute name.
    * @return The attribute value.
    */
   public String getAttrValue(Element element, String sName, String sOverrideName)
   {
      String sOverride = XMLUtil.getStringAttr(element, sOverrideName);
      String sValue = null;

      if (sOverride != null)
      {
         sValue = m_properties.getProperty(sOverride);
      }

      if (sValue == null)
      {
         sValue = XMLUtil.getStringAttr(element, sName);
      }

      return sValue;
   }

   /**
    * Parses a string into a Scheme S-expression. This shouldn't be used for
    * code that is to be debugged. That code should use
    * {@link #parse(String, boolean, String, Lookup, Object, GlobalEnvironment)}.
    * @param sText The string to parse.
    * @param bList True to parse the string as a list.
    * @param posMap The empty node position map. It will be populated by the parser. Can be null.
    * @param eof The object to return on EOF.
    * @param env The global environment where the symbols will be stored.
    * @return The resulting S-expression.
    * @see #parse(String, boolean, String, Lookup, Object, GlobalEnvironment)
    */
   public Object parse(String sText, boolean bList, Lookup posMap, Object eof, GlobalEnvironment env)
   {
      return parse(sText, bList, null, posMap, eof, env);
   }

   /**
    * Parses a string into a Scheme S-expression.
    * @param sText The string to parse.
    * @param bList True to parse the string as a list.
    * @param sURL The optional code URL to store.
    * @param posMap The empty node position map. It will be populated by the parser. Can be null.
    * @param eof The object to return on EOF.
    * @param env The global environment where the symbols will be stored.
    * @return The resulting S-expression.
    */
   public Object parse(String sText, boolean bList, String sURL, Lookup posMap, Object eof, GlobalEnvironment env)
   {
      if (sText == null)
      {
         return eof;
      }

      Reader reader = new TextPositionReader(new StringReader(sText), sURL);
      Pair first = null;
      Pair last = null;
      Object expr;

      if (m_parser == null)
      {
         m_parser = new SchemeParser(env);
      }
      else
      {
         m_parser.setGlobalEnvironment(env);
      }

      try
      {
         for (;;)
         {
            expr = m_parser.parse(reader, posMap);

            if (!bList || expr == Parser.EOF)
            {
               break;
            }

            if (first == null)
            {
               first = last = new Pair(expr);
            }
            else
            {
               Pair pair = new Pair(expr);

               last.setTail(pair);
               last = pair;
            }
         }
      }
      catch (ParserException e)
      {
         throw new MetadataException(e.getErrorCode(), e.getErrorArgs(), e);
      }
      catch (RuntimeException e)
      {
         throw new MetadataException("err.meta.sexprSyntax", new Object[]{sText}, e);
      }

      if (expr == Parser.EOF && first == null)
      {
         return eof;
      }

      if (!bList && m_parser.parse(reader, null) != Parser.EOF)
      {
         throw new MetadataException("err.meta.sexprExtra", new Object[]{sText});
      }

      return (bList) ? first : expr;
   }

   /**
    * Returns a class object by name.
    * @param The Java class name.
    * @return The class object.
    */
   public Class getClassObject(String sName)
   {
      try
      {
         return Class.forName(sName);
      }
      catch (Throwable e)
      {
         throw new MetadataException("err.meta.classLoad", new Object[]{sName}, e);
      }
   }

   /**
    * Adds descriptor mixin reference elements to a map.
    * @param mixinMap The destination map: repository namespace to mixin Element.
    * @param descriptorElement The descriptor element.
    */
   private static void addMixinElements(final Lookup mixinMap, Element descriptorElement)
   {
      verifyRootElement(descriptorElement, "Metadata");

      Element mixinsElement = XMLUtil.findChildElement(descriptorElement, "Mixins");

      if (mixinsElement != null)
      {
         XMLUtil.forEachChildElement(mixinsElement, null, new XMLUtil.ElementHandler()
         {
            public void handleElement(Element element)
            {
               mixinMap.put(XMLUtil.getReqStringAttr(element, "namespace"), element);
            }
         });
      }
   }

   /**
    * Adds descriptor resource reference elements to a map.
    * @param resourceMap The destination map: refElement[sContainerElementName, sResourceName].
    * @param refNameMap Reference element name map: sRefElementName[sContainerElementName].
    * @param descriptorElement The descriptor element.
    * @throws MetadataException if an invalid descriptor has been detected.
    */
   private static void addResourceElements(final Lookup2D resourceMap, final Lookup refNameMap,
      Element descriptorElement) throws MetadataException
   {
      verifyRootElement(descriptorElement, "Metadata");

      XMLUtil.forEachChildElement(descriptorElement, null, new XMLUtil.ElementHandler()
      {
         public void handleElement(final Element containerElement)
         {
            if (!"Mixins".equals(containerElement.getNodeName()))
            {
               XMLUtil.forEachChildElement(containerElement, null, new XMLUtil.ElementHandler()
               {
                  public void handleElement(Element refElement)
                  {
                     String sOldName = (String)refNameMap.put(
                        containerElement.getNodeName(), refElement.getNodeName());

                     if (sOldName != null && !sOldName.equals(refElement.getNodeName()))
                     {
                        throw new MetadataException("err.meta.refElement",
                           new Object[]{containerElement.getNodeName(), sOldName, refElement.getNodeName()});
                     }

                     resourceMap.put(containerElement.getNodeName(),
                        getResourceName(XMLUtil.getReqStringAttr(refElement, "resource")), refElement);
                  }
               });
            }
         }
      });
   }

   /**
    * Clears the cached resource listing.
    */
   public void resetListing()
   {
      m_listing = null;
   }

   /**
    * Gets the resource listing for the repository.  If not already initialized, initializes the listing to resources of the
    * base and root repositories.
    * @return The resource listing.
    */
   public XMLMetadataListing getListing()
   {
      if (m_listing == null)
      {
         if (m_rootURL != null)
         {
            getListing(null);
         }
         else
         {
            // only base resources are to be listed
            m_listing = getListing(false, true, null);
         }
      }

      return m_listing;
   }

   /**
    * @see XMLMetadataHelper#getListing(MixinHandler, Lookup)
    */
   public XMLMetadataListing getListing(MixinHandler handler)
   {
      return getListing(handler, new HashTab());
   }

   /**
    * Links all mix-in repositories.
    * @param handler The mix-in link event handler.  May be null
    * @param sourceMap Map of resource names to maps of XMLMixins to XMLResources.
    * @return The XMLMetadataListing.
    **/
   public XMLMetadataListing getListing(MixinHandler handler, Lookup sourceMap)
   {
      Lookup resourceMap = new HashTab();

      m_listing = linkMixinResources(null, sourceMap, new HashTab(), handler);

      // record full map of resources, check for conflicts
      for (Lookup.Iterator resItr = sourceMap.valueIterator(); resItr.hasNext(); )
      {
         Lookup srcMap = (Lookup)resItr.next();
         Iterator srcItr = srcMap.valueIterator();
         XMLResource mostDerivedSource = (XMLResource)srcItr.next();
         List identicalSourcesList = new ArrayList(srcMap.size());

         if (handler != null)
         {
            while (srcItr.hasNext())
            {
               XMLResource alternateSource = (XMLResource)srcItr.next();

               if (!mostDerivedSource.overrides(alternateSource))
               {
                  if (alternateSource.overrides(mostDerivedSource))
                  {
                     mostDerivedSource = alternateSource;
                     identicalSourcesList.clear();
                  }
                  else
                  {
                     handler.handleResourceConflict(mostDerivedSource, alternateSource);

                     break;
                  }
               }
               else
               {
                  if (alternateSource.overrides(mostDerivedSource))
                  {
                     identicalSourcesList.add(alternateSource);
                  }
               }
            }

            for (int i = 0; i < identicalSourcesList.size(); i++)
            {
               handler.handleAlternateResource(mostDerivedSource, (XMLResource)identicalSourcesList.get(i));
            }
         }

         resourceMap.put(mostDerivedSource.getName(), mostDerivedSource);
      }

      checkForError();
      m_listing.setResourceMap(resourceMap); // in root metadata, include the mixin resources.

      return m_listing;
   }

   /**
    * Recursively builds helpers for all mixin repositories, validates resource overrides and returns
    * a map of resource names to XMLResources.
    * @param The destination mixin. Null for the top-level model.
    * @param sourceMap Map of resource names to maps of XMLMixins to XMLResources.
    * @param mixinMap Map of namespaces to XMLMixins.
    * @param handler The mixin link event handler.
    * @return The metadata listing.
    */
   private XMLMetadataListing linkMixinResources(XMLMixin mixin, Lookup sourceMap, Lookup mixinMap, final MixinHandler handler)
   {
      if (mixin == null)
      {
         mixin = new XMLMixin(null);
         mixin.setHelper(this);
      }

      boolean bTop = (mixin.getNamespace() == null);
      Element rootDescriptorElement = getDescriptorElement(true);
      Element baseDescriptorElement = getDescriptorElement(false);
      final String sNamespace = XMLUtil.getReqStringAttr(rootDescriptorElement, "namespace");
      final String sBaseNamespace = (baseDescriptorElement == null) ? "" :
         XMLUtil.getReqStringAttr(baseDescriptorElement, "namespace");

      if (bTop)
      {
         mixin.setNamespace(sNamespace);
      }

      if (mixin.getName() == null)
      {
         mixin.setName(XMLUtil.getReqStringAttr(rootDescriptorElement, "name"));
      }

      XMLMetadataListing listing = mixin.getListing();

      if (listing == null)
      {
         if (!mixin.isRoot())
         {
            return null;
         }

         Object old = mixinMap.put(sNamespace, mixin);

         if (old != null)
         {
            mixinMap.put(sNamespace, old);

            if (handler != null)
            {
               handler.handleCircularReference(mixin);
            }

            return null;
         }

         listing = getListing(true, true, mixinMap);

         if (handler != null)
         {
            handler.handleRepository(mixin);
         }
      }

      Lookup refMap = new HashTab(); // XMLMixin[sNamespace]

      if (baseDescriptorElement != null)
      {
         loadMixinReferences(baseDescriptorElement, refMap, (XMLMixin)mixinMap.get(sBaseNamespace), handler);
      }

      loadMixinReferences(rootDescriptorElement, refMap, mixin, handler);

      // Process dynamic mix-ins
      if (bTop)
      {
         String sMixins = getProperty(MetadataLoader.METADATA_MIXINS_PROPERTY);

         if (!StringUtil.isEmpty(sMixins))
         {
            String[] sMixinArray = StringUtil.split(sMixins);

            for (int i = 0; i < sMixinArray.length; i++)
            {
               String sMixinNamespace = sMixinArray[i];

               if (!mixinMap.contains(sMixinNamespace))
               {
                  XMLMixin ref = new XMLMixin(sMixinNamespace);

                  if (handler != null)
                  {
                     handler.handleMixinReference(ref, mixin);
                  }

                  refMap.put(sMixinNamespace, ref);
               }
            }
         }
      }

      // set of XMLMixins having resources that can be overridden by the current mixin
      Set overridableSet = new HashHolder(1);

      for (Iterator itr = refMap.valueIterator(); itr.hasNext();)
      {
         XMLMixin ref = (XMLMixin)itr.next();
         boolean bOverride = ref.isOverridable();
         boolean bDynamic = (ref.getVersion() == null);
         String sRefNamespace = ref.getNamespace();
         XMLMixin old = (XMLMixin)mixinMap.get(sRefNamespace);

         try
         {
            if (old != null)
            {
               if (!ref.isRoot())
               {
                  old.setRoot(false);
               }

               ref = old;
            }
            else
            {
               String sMixinDefaultPath = getRepositoryPath(sRefNamespace);
               URL rootURL = null;

               if (m_handler == null)
               {
                  rootURL = getURL(getProperty(getRepositoryProperty(sRefNamespace), sMixinDefaultPath), true);
               }
               else
               {
                  String sURI = getProperty(getRepositoryProperty(sRefNamespace));

                  if (!StringUtil.isEmpty(sURI))
                  {
                     rootURL = findURL(sURI, true, null);
                  }

                  if (rootURL == null && m_handler instanceof MetadataURLHandler)
                  {
                     rootURL = ((MetadataURLHandler)m_handler).getMixinURL(m_rootURL, sRefNamespace);
                  }
               }

               ref.setHelper(new XMLMetadataHelper(rootURL, null, m_properties, null, m_upgrader));
            }

            ref.getHelper().linkMixinResources(ref, sourceMap, mixinMap, handler);

            if (bDynamic)
            {
               bOverride = ref.isOverridable();
            }

            if (bOverride)
            {
               if (!ref.isOverridable() && handler != null)
               {
                  handler.handleMixinOverrideConflict(ref, mixin);
               }

               overridableSet.add(ref);
            }
         }
         catch (Exception e)
         {
            if (handler != null)
            {
               // if mix-in cannot be linked, keep building listing, but notify handler
               handler.handleLinkFailure(ref, e);
            }
         }
      }

      Lookup baseMap = listing.getBaseResourceMap();

      if (baseMap != null)
      {
         linkMixinResources(mixin, sourceMap, baseMap, sBaseNamespace, overridableSet, handler);
      }

      overridableSet.add(mixin);
      linkMixinResources(mixin, sourceMap, listing.getRootResourceMap(), sNamespace, overridableSet, handler);

      if (mixin.getListing() == null)
      {
         mixin.setListing(listing);
      }

      return listing;
   }

   /**
    * Loads mixin references from a DOM element.
    * @param element The DOM element.
    * @param refMap Output map XMLMixin[sNamespace].
    * @param mixin The referrer.
    * @param handler The mixin handler. Can be null.
    */
   private static void loadMixinReferences(Element element, final Lookup refMap, final XMLMixin mixin, final MixinHandler handler)
   {
      XMLUtil.withFirstChildElement(element, "Mixins", false,
         new XMLUtil.ElementHandler()
         {
            public void handleElement(Element parent)
            {
               XMLUtil.forEachChildElement(parent, "Mixin", new XMLUtil.ElementHandler()
               {
                  public void handleElement(Element element)
                  {
                     XMLMixin ref = new XMLMixin(
                        XMLUtil.getReqStringAttr(element, "namespace"),
                        XMLUtil.getReqStringAttr(element, "version"),
                        XMLUtil.getReqStringAttr(element, "checksum"),
                        XMLUtil.getBooleanAttr(element, "override", false),
                        !XMLUtil.getBooleanAttr(element, "base", false));

                     if (handler != null)
                     {
                        handler.handleMixinReference(ref, mixin);
                     }

                     refMap.put(ref.getNamespace(), ref);
                  }
               });
            }
         });
   }

   /**
    * Updates a sourceMap with resources from resourceMap.
    * @param mixin The destination mixin.
    * @param sourceMap Map of resource names to maps of XMLMixins to XMLResources.
    * @param resourceMap A map of resource names to XMLResources.
    * @param sNamespace The namespace of the associated repository.
    * @param overrideSet The set of XMLMixins which can be overridden by resources from metadata.
    * @param handler The mix-in link event handler.
    */
   private static void linkMixinResources(XMLMixin mixin, Lookup sourceMap, Lookup resourceMap,
      String sNamespace, Set overrideSet, MixinHandler handler)
   {
      for (Lookup.Iterator itr = resourceMap.valueIterator(); itr.hasNext(); )
      {
         XMLResource resource = (XMLResource)itr.next();
         Lookup map = (Lookup)sourceMap.get(resource.getName());

         if (map == null)
         {
            map = new HashTab(1);
            sourceMap.put(resource.getName(), map);
         }
         else
         {
            for (Iterator overrideItr = overrideSet.iterator(); overrideItr.hasNext(); )
            {
               XMLResource base = (XMLResource)map.remove(overrideItr.next()); // if override is permitted, replace overridden source

               if (base != null && base != resource)
               {
                  resource.addBase(base);

                  if (handler != null)
                  {
                     handler.handleResourceOverride(resource, base);
                  }
               }
            }
         }

         map.put(mixin, resource);
      }
   }

   /**
    * Gets the root and base file listings for a repository.
    * @param bRoot Load the root listing.
    * @param bBase Load the base listing.
    * @param mixinMap Map of mixin namespace to XMLMixin. Can be null.
    * @return The listing, with excludes, baseMap and rootMap initialized.
    * ResourceMap is initialized, but does not include resources from any mix-ins.
    */
   private XMLMetadataListing getListing(boolean bRoot, boolean bBase, Lookup mixinMap)
   {
      XMLMetadataListing listing = new XMLMetadataListing();
      Lookup resourceMap = new HashTab();

      if (bBase)
      {
         Lookup map = findResources(false, mixinMap);

         if (map != null)
         {
            for (Lookup.Iterator itr = map.valueIterator(); itr.hasNext();)
            {
               XMLResource resource = (XMLResource)itr.next();

               resourceMap.put(resource.getName(), resource);
            }
         }

         listing.setBaseResourceMap(map);
      }

      if (bRoot)
      {
         Lookup map = findResources(true, mixinMap);

         if (map != null)
         {
            for (Lookup.Iterator itr = map.valueIterator(); itr.hasNext();)
            {
               XMLResource resource = (XMLResource)itr.next();

               resourceMap.put(resource.getName(), resource);
            }
         }

         listing.setRootResourceMap(map);
      }

      listing.setResourceMap(resourceMap);

      return listing;
   }

   /**
    * Load the resource listings for a repository.
    * @param bRoot True to load the root resource, false to load the base resources.
    * @param mixinMap Map of mixin namespace to XMLMixin. Can be null.
    * @return Map of resource names to XMLResource instances. Can be null.
    */
   private Lookup findResources(boolean bRoot, Lookup mixinMap)
   {
      Element element = getDescriptorElement(bRoot);

      if (element == null)
      {
         return null;
      }

      String sNamespace = XMLUtil.getReqStringAttr(element, "namespace");
      XMLMixin mixin = null;

      if (mixinMap != null)
      {
         mixin = (XMLMixin)mixinMap.get(sNamespace);
      }

      if (mixin == null)
      {
         mixin = new XMLMixin(sNamespace);

         if (mixinMap != null)
         {
            mixinMap.put(sNamespace, mixin);
         }
      }

      if (mixin.getName() == null)
      {
         mixin.setName(XMLUtil.getReqStringAttr(element, "name"));
      }

      mixin.setVersion(XMLUtil.getReqStringAttr(element, "version"));
      mixin.setOverridable(XMLUtil.getBooleanAttr(element, "override", true));

      String sModule = normalizeScope(XMLUtil.getStringAttr(element, "module"));

      mixin.setModule(sModule);

      if (mixin.getHelper() == null)
      {
         mixin.setHelper(this);
         mixin.setRoot(bRoot);
      }

      final Lookup map;

      try
      {
         List nameList = new ArrayList();
         URL url = (bRoot) ? m_rootURL : m_baseURL;

         URLUtil.addResourceNames(nameList, url, null, true);
         map = new HashTab(nameList.size());

         for (int i = 0, n = nameList.size(); i < n; ++i)
         {
            String sResource = (String)nameList.get(i);

            if (!sResource.equals(m_sMetadataFileName))
            {
               String sName = (sModule == null) ? sResource : sModule + Metadata.SCOPE_SEP + sResource;
               XMLResource resource = new XMLResource(sName, new URL(url, sResource), bRoot);

               resource.setMixin(mixin);
               map.put(sName, resource);
            }
         }
      }
      catch (Throwable t)
      {
         throw ObjUtil.rethrow(t);
      }

      XMLUtil.withFirstChildElement(element, "Resources", false, new XMLUtil.ElementHandler()
      {
         public void handleElement(Element parent)
         {
            XMLUtil.forEachChildElement(parent, "ResourceRef", new XMLUtil.ElementHandler()
            {
               public void handleElement(Element element)
               {
                  XMLResource resource = (XMLResource)map.get(XMLUtil.getReqStringAttr(element, "resource"));

                  if (resource != null)
                  {
                     resource.setEnabled(isElementEnabled(element));
                  }
               }
            });
         }
      });

      return map;
   }

   /**
    * Merges a base and a root metadata descriptor elements.
    * @param baseList The output list of base resource relative URLs. Can be null.
    * @param rootList The output list of root resource relative URLs. Can be null.
    */
   public Element mergeDescriptorElements(List baseList, List rootList) throws MetadataException
   {
      Document doc = XMLUtil.parse(new StringReader("<Metadata/>"));
      Element descriptorElement = doc.getDocumentElement();
      Lookup2D resourceMap = new HashTab2D();
      Lookup refNameMap = new LinkedHashTab();
      Lookup mixinMap = new LinkedHashTab();
      String sBaseNamespace = null;
      String sBaseVersion = null;
      String sBaseChecksum = null;
      String sBaseName = null;
      String sBaseRevision = null;
      String sBaseDescription = null;
      String sBaseCoreVersion = null;
      String sBaseModule = null;
      String sBaseOverride = null;
      String sRootNamespace = null;
      String sRootVersion = null;
      String sRootDescription = null;
      String sRootName = null;
      String sRootRevision = null;
      String sRootCoreVersion = null;
      String sRootModule = null;
      String sRootOverride = null;
      Element baseElement = getDescriptorElement(false);
      Element rootElement = getDescriptorElement(true);

      if (baseElement != null)
      {
         sBaseNamespace = XMLUtil.getReqStringAttr(baseElement, "namespace");
         sBaseVersion = XMLUtil.getReqStringAttr(baseElement, "version");
         sBaseName = XMLUtil.getReqStringAttr(baseElement, "name");
         sBaseRevision = XMLUtil.getReqStringAttr(baseElement, "revision");
         sBaseCoreVersion = XMLUtil.getStringAttr(baseElement, "coreVersion");
         sBaseModule = normalizeScope(XMLUtil.getStringAttr(baseElement, "module"));
         sBaseOverride = XMLUtil.getStringAttr(baseElement, "override");
         sBaseDescription = XMLUtil.getStringAttr(baseElement, "description");
         sBaseChecksum = getChecksum(false);
         addMixinElements(mixinMap, baseElement);
         addResourceElements(resourceMap, refNameMap, baseElement);
      }

      if (rootElement != null)
      {
         sRootNamespace = XMLUtil.getReqStringAttr(rootElement, "namespace");
         sRootVersion = XMLUtil.getReqStringAttr(rootElement, "version");
         sRootName = XMLUtil.getReqStringAttr(rootElement, "name");
         sRootRevision = XMLUtil.getReqStringAttr(rootElement, "revision");
         sRootCoreVersion = XMLUtil.getStringAttr(rootElement, "coreVersion");
         sRootModule = normalizeScope(XMLUtil.getStringAttr(rootElement, "module"));
         sRootOverride = XMLUtil.getStringAttr(rootElement, "override");
         sRootDescription = XMLUtil.getStringAttr(rootElement, "description");
         addMixinElements(mixinMap, rootElement);
         addResourceElements(resourceMap, refNameMap, rootElement);

         if (baseElement == null)
         {
            sBaseNamespace = XMLUtil.getStringAttr(rootElement, "baseNamespace");
            sBaseVersion = XMLUtil.getStringAttr(rootElement, "baseVersion");
            sBaseChecksum = XMLUtil.getStringAttr(rootElement, "baseChecksum");
         }
      }
      else
      {
         sRootNamespace = sBaseNamespace;
         sRootVersion = sBaseVersion;
         sRootName = sBaseName;
         sRootRevision = sBaseRevision;
         sRootModule = sBaseModule;
         sRootOverride = sBaseOverride;
         sRootDescription = sBaseDescription;
         sBaseNamespace = null;
         sBaseVersion = null;
         sBaseChecksum = null;
         sBaseModule = null;
         sBaseOverride = null;
         sBaseDescription = null;
      }

      if (sRootCoreVersion == null)
      {
         sRootCoreVersion = sBaseCoreVersion;
      }

      if (sRootNamespace != null)
      {
         descriptorElement.setAttribute("name", sRootName);
         descriptorElement.setAttribute("revision", sRootRevision);
         descriptorElement.setAttribute("namespace", sRootNamespace);
         descriptorElement.setAttribute("version", sRootVersion);

         if (sRootCoreVersion != null)
         {
            descriptorElement.setAttribute("coreVersion", sRootCoreVersion);
         }

         if (sRootModule != null)
         {
            descriptorElement.setAttribute("module", sRootModule);
         }

         if (sRootOverride != null)
         {
            descriptorElement.setAttribute("override", sRootOverride);
         }

         if (sRootDescription != null)
         {
            descriptorElement.setAttribute("description", sRootDescription);
         }
      }

      if (sBaseNamespace != null)
      {
         descriptorElement.setAttribute("baseNamespace", sBaseNamespace);
         descriptorElement.setAttribute("baseVersion", sBaseVersion);
         descriptorElement.setAttribute("baseChecksum", sBaseChecksum);
      }

      // Insert the mixin elements
      Element mixinsElement = doc.createElement("Mixins");

      descriptorElement.appendChild(mixinsElement);

      for (Iterator itr = mixinMap.valueIterator(); itr.hasNext(); )
      {
         Element mixinElement = XMLUtil.addChildElement(mixinsElement, null, "Mixin");
         NamedNodeMap map = ((Element)itr.next()).getAttributes();

         for (int i = 0, n = map.getLength(); i < n; ++i)
         {
            Attr attr = (Attr)map.item(i);

            mixinElement.setAttribute(attr.getName(), attr.getValue());
         }
      }

      // Instantiate all the container elements and place them in a map by name
      Lookup refMap = new HashTab(refNameMap.size());

      for (Lookup.Iterator itr = refNameMap.iterator(); itr.hasNext();)
      {
         Element element = doc.createElement((String)itr.next());

         descriptorElement.appendChild(element);
         refMap.put(itr.getKey(), element);
      }

      // Insert the reference elements
      for (Lookup2D.Iterator itr = resourceMap.valueIterator(); itr.hasNext();)
      {
         Element element = (Element)itr.next();
         Element refElement = doc.createElement((String)refNameMap.get(itr.getKey1()));

         ((Element)refMap.get(itr.getKey1())).appendChild(refElement);

         NamedNodeMap map = element.getAttributes();

         for (int i = 0, n = map.getLength(); i < n; ++i)
         {
            Attr attr = (Attr)map.item(i);

            refElement.setAttribute(attr.getName(), attr.getValue());
         }
      }

      XMLMetadataListing listing = getListing();

      if (rootList != null || baseList != null)
      {
         Set rootSet = new HashHolder();
         Lookup rootMap = listing.getRootResourceMap();

         if (rootMap != null)
         {
            for (Iterator iter = rootMap.iterator(); iter.hasNext(); )
            {
               String sName = (String)iter.next();

               sName = sName.substring(sName.lastIndexOf(Metadata.SCOPE_SEP) + 1);
               rootSet.add(sName);

               if (rootList != null)
               {
                  rootList.add(sName);
               }
            }
         }

         Lookup baseMap = listing.getBaseResourceMap();

         if (baseMap != null && baseList != null)
         {
            for (Iterator iter = baseMap.iterator(); iter.hasNext(); )
            {
               String sName = (String)iter.next();

               sName = sName.substring(sName.lastIndexOf(Metadata.SCOPE_SEP) + 1);

               if (!rootSet.contains(sName))
               {
                  baseList.add(sName);
               }
            }
         }
      }

      normalizeDescriptorElement(descriptorElement);

      return descriptorElement;
   }

   /**
    * Returns an instance of a dynamically loaded class.
    * @param sName The Java class name.
    * @return The instance of the dynamically loaded class.
    */
   public Object getClassInstance(String sName)
   {
      if (m_instanceMap == null)
      {
         m_instanceMap = new HashTab();
      }

      Object instance = m_instanceMap.get(sName);

      if (instance == null)
      {
         try
         {
            instance = Class.forName(sName).newInstance();
         }
         catch (Throwable e)
         {
            throw new MetadataException("err.meta.classLoad", new Object[]{sName}, e);
         }

         m_instanceMap.put(sName, instance);
      }

      return instance;
   }

   /**
    * Returns an instance of a class.
    * @param clazz The class object.
    * @return The instance.
    */
   public Object getClassInstance(Class clazz)
   {
      return getClassInstance(clazz.getName());
   }

   /**
    * Normalizes the descriptor element.
    * @param element The element to normalize.
    */
   public static void normalizeDescriptorElement(Element element)
   {
      XMLUtil.normalize(element);
      XMLUtil.sortNode(element, null);

      final List emptyElementList = new ArrayList(4);

      XMLUtil.forEachChildElement(element, null, new XMLUtil.ElementHandler()
      {
         public void handleElement(Element element)
         {
            if (element.getFirstChild() == null)
            {
               emptyElementList.add(element);
            }
            else
            {
               XMLUtil.forEachChildElement(element, null, new XMLUtil.ElementHandler()
               {
                  public void handleElement(Element element)
                  {
                     if (element.hasAttribute("merged") &&
                        !XMLUtil.getBooleanAttr(element, "merged"))
                     {
                        element.removeAttribute("merged");
                     }
                  }
               });

               XMLUtil.sortNode(element, "resource");
            }
         }
      });

      for (int i = 0, n = emptyElementList.size(); i < n; ++i)
      {
         element.removeChild((Element)emptyElementList.get(i));
      }
   }

   /**
    * Verifies that the root document element has the required name.
    * @param element The root element.
    * @param sName The required name.
    * @throws MetadataException if the element name does not match.
    */
   public static void verifyRootElement(Element element, String sName) throws MetadataException
   {
      if (!element.getNodeName().equals(sName))
      {
         throw new MetadataException("err.meta.docRoot", new Object[]{element.getNodeName(), sName});
      }
   }

   /**
    * Parses out a resource name from a URI.
    * @param sURI The resource URI.
    * @return The resource name.
    */
   public static String getResourceName(String sURI)
   {
      int nStart = sURI.lastIndexOf(Metadata.SCOPE_SEP) + 1;
      int nMid = sURI.indexOf('/', nStart) + 1;

      if (nMid < nStart)
      {
         nMid = nStart;
      }

      int nDir = sURI.lastIndexOf('/') + 1;

      if (nDir < nStart)
      {
         nDir = nStart;
      }

      int nEnd = sURI.lastIndexOf('.');

      if (nEnd <= nDir + 1)
      {
         nEnd = sURI.length();
      }

      return sURI.substring(0, nStart) + sURI.substring(nMid, nEnd).replace('/', Metadata.SCOPE_SEP);
   }

   /**
    * Parses out a system resource name from a URI.
    * @param sURI The resource URI.
    * @return The resource name.
    */
   public static String getSystemResourceName(String sURI)
   {
      int nStart = sURI.lastIndexOf('/') + 1;
      int nEnd = sURI.lastIndexOf('.');

      if (nEnd <= nStart + 1)
      {
         nEnd = sURI.length();
      }

      return sURI.substring(nStart, nEnd);
   }

   /**
    * Returns the name attribute of a given node.
    * @param node The DOM node containing the attribute.
    * @return The value of the attribute.
    * @throws XMLException if the attribute was not found or the value if empty.
    */
   public static String getNameAttr(Node node) throws XMLException
   {
      return XMLUtil.getReqStringAttr(node, "name");
   }

   /**
    * Returns an identifier attribute from a given node.
    * @param node The DOM node containing the attribute.
    * @param sName The attribute name.
    * @param nMode The validation mode. Mask of NAME_* constants.
    * @param bPeriodsAllowed True if periods are allowed within the name.
    * @return The value of the attribute.
    * @throws XMLException if the attribute was not found or the value if empty.
    * @throws MetadataException if the name is invalid.
    */
   public static String getNameAttr(Node node, String sName, int nMode) throws XMLException, MetadataException
   {
      String sValue = XMLUtil.getReqStringAttr(node, sName);

      validateName(sValue, nMode);

      return sValue;
   }

   /**
    * Determines if a name part is valid.
    * @param sName The name string to validate.
    * @param nStart The start offset in sName.
    * @param nEnd The end offset (past the last character) in sName.
    * @param nMode The validation mode. Mask of NAME_* constants.
    * @return True if the name part is valid.
    */
   protected static boolean isNamePartValid(String sName, int nStart, int nEnd, int nMode)
   {
      if (nStart >= nEnd)
      {
         return false;
      }

      char ch = sName.charAt(nStart);
      int i = nStart + 1;

      if (!Character.isLetter(ch) && ch != '_')
      {
         return false;
      }

      while (i < nEnd)
      {
         ch = sName.charAt(i);

         if (!Character.isLetterOrDigit(ch))
         {
            switch (ch)
            {
               case '_':
                  break;

               case '.':
                  if ((nMode & NAME_DOT) == 0)
                  {
                     return false;
                  }

                  break;

               case '-':
               case '?':
               case '!':
                  if ((nMode & NAME_SPEC) == 0)
                  {
                     return false;
                  }

                  break;

               default:
                  return false;
            }
         }

         ++i;
      }

      return true;
   }

   /**
    * Validates a name string - must begin with a letter or _
    * and contain only letters, digits or _.
    * @param sName The name to validate.
    * @param nMode The validation mode. Mask of NAME_* constants.
    * @throws MetadataException if the name is invalid.
    */
   public static void validateName(String sName, int nMode) throws MetadataException
   {
      boolean bValid;
      int nCount = sName.length();

      if ((nMode & NAME_SCOPE) != 0)
      {
         bValid = false;

         int nStart = 0;

         while (nStart < nCount)
         {
            int nEnd = sName.indexOf(Metadata.SCOPE_SEP, nStart);

            if (nEnd < 0)
            {
               nEnd = nCount;
            }

            if ((nEnd != 0 || nCount == 1) && !isNamePartValid(sName, nStart, nEnd, nMode))
            {
               bValid = false;

               break;
            }

            bValid = true;
            nStart = nEnd + 1;
         }
      }
      else
      {
         bValid = isNamePartValid(sName, 0, nCount, nMode);
      }

      if (!bValid)
      {
         throw new MetadataException("err.meta.name", new Object[]{sName});
      }
   }

   /**
    * Validates a fully scoped name string.
    * @param sName The name to validate.
    * @throws MetadataException if the name is invalid.
    */
   public static void validateName(String sName)
   {
      validateName(sName, NAME_SCOPE);
   }

   /**
    * Converts a string to an alias string.
    * @param sText The string to convert.
    * @return The converted string.
    */
   public static String makeAlias(String sText)
   {
      return makeName(sText, "-");
   }

   /**
    * Converts a string to a name string.
    * @param sText The string to convert.
    * @param sExtra The extra characters to allow. Can be null.
    * @return The converted string.
    */
   public static String makeName(String sText, String sExtra)
   {
      StringBuffer buf = new StringBuffer(sText.length());

      for (int i = 0; i < sText.length(); ++i)
      {
         char ch = sText.charAt(i);

         if (i == 0)
         {
            if (ch == '_' || Character.isLetter(ch))
            {
               buf.append(ch);
            }
            else if (Character.isDigit(ch))
            {
               buf.append('_');
               buf.append(ch);
            }
            else
            {
               buf.append('_');
            }
         }
         else
         {
            if (ch == '_' || Character.isLetterOrDigit(ch) ||
               sExtra != null && sExtra.indexOf(ch) >= 0)
            {
               buf.append(ch);
            }
            else
            {
               buf.append('_');
            }
         }
      }

      if (buf.length() == 0)
      {
         buf.append('_');
      }

      return buf.toString();
   }

   /**
    * Normalizes a fully scoped name by trimming scope separators.
    * @param sName The name to normalize. Can be null.
    * @return The normalized name. Can be null.
    */
   public static String normalizeScope(String sName)
   {
      if (sName == null)
      {
         return null;
      }

      int nStart = 0;
      int nEnd = sName.length();

      while (nStart < nEnd)
      {
         if (sName.charAt(nStart) == Metadata.SCOPE_SEP)
         {
            ++nStart;
         }
         else if (sName.charAt(nEnd - 1) == Metadata.SCOPE_SEP)
         {
            --nEnd;
         }
         else
         {
            break;
         }
      }

      if (nStart >= nEnd)
      {
         return null;
      }

      return sName.substring(nStart, nEnd);
   }

   /**
    * Normalizes the URI and converts it to a URL object.
    * @param sURI The URI to normalize.
    * @param bDir True if the URL indicates a directory.
    * @return The URL object.
    */
   public static URL getURL(String sURI, boolean bDir)
   {
      return getURL(sURI, bDir, null);
   }

   /**
    * Normalizes the URI and converts it to a URL object.
    * @param sURI The URI to normalize.
    * @param bDir True if the URL indicates a directory.
    * @param handler The URL stream handler if sURI uses a non-standard scheme.
    * @return The URL object.
    * @throws MetadataException If a URL could not be created.
    */
   public static URL getURL(String sURI, boolean bDir, URLStreamHandler handler)
   {
      URL url = findURL(sURI, bDir, handler);

      if (url == null)
      {
         throw new MetadataException("err.meta.resource", new Object[]{sURI});
      }

      return url;
   }

   /**
    * Normalizes the URI and converts it to a URL object.
    * @param sURI The URI to normalize.
    * @param bDir True if the URL indicates a directory.
    * @param handler The URL stream handler if sURI uses a non-standard scheme.
    * @return The URL object; null if URL could not be created.
    */
   public static URL findURL(String sURI, boolean bDir, URLStreamHandler handler)
   {
      URL url = null;

      if (bDir)
      {
         if (sURI.length() == 0 || sURI.charAt(sURI.length() - 1) != '/')
         {
            sURI += '/';
         }
      }

      if (sURI.length() == 0)
      {
         url = null;
      }
      else if (sURI.indexOf(':') > 1)  // avoid URL instantiation when sURI is clearly not a URL
      {
         try
         {
            url = new URL(null, sURI, handler);
         }
         catch (Exception e)
         {
            url = null;
         }
      }

      if (url == null && sURI.length() > 0)
      {
         if (!sURI.startsWith("/"))
         {
            sURI = '/' + sURI;
         }

         url = XMLMetadataHelper.class.getResource(sURI);
      }

      return url;
   }

   /**
    * Sets an alternate metadata file name.
    * @param sName Metadata file name.
    */
   public void setMetadataFileName(String sName)
   {
      m_sMetadataFileName = sName;
   }

   /**
    * Computes the identifier corresponding to a repository namespace.
    * @param sNamespace The repository namespace
    * @return The corresponding identifier.
    */
   public static String getRepositoryIdentifier(String sNamespace)
   {
      return INVALID_SUBSTRING_PATTERN.matcher(sNamespace).replaceAll("_");
   }

   /**
    * Computes the path name of a published repository, given its namespace.
    * @param sNamespace The repository namespace
    * @return The path name of the published repository
    */
   public static String getRepositoryPath(String sNamespace)
   {
      return SysUtil.NAMESPACE + "/meta/" + getRepositoryIdentifier(sNamespace);
   }

   /**
    * Computes the property name holding the path to a repository's published jar, given its namespace.
    * @param sNamespace The repository namespace.
    * @return The repository jar property name.
    */
   public static String getRepositoryProperty(String sNamespace)
   {
      return getRepositoryProperty(sNamespace, null);
   }

   /**
    * Computes the property name holding the path to a repository's published jar, given its namespace.
    * @param sNamespace The repository namespace.
    * @param sSuffix The property suffix.
    * @return The repository jar property name.
    */
   public static String getRepositoryProperty(String sNamespace, String sSuffix)
   {
      if (sSuffix == null)
      {
         sSuffix = "";
      }

      return "meta.mixin." + getRepositoryIdentifier(sNamespace) + sSuffix + ".url";
   }

   /**
    * @param sNamespace The repository namespace.
    * @return The unique environment name that can be use to deploy multiple repositories to the same server.
    *         This can come from a system property or if not specified, is derived from the metadata namespace.
    */
   public static String getEnvironmentName(String sNamespace)
   {
      return getRepositoryIdentifier(sNamespace.substring(sNamespace.lastIndexOf('/') + 1));
   }

   /**
    * Adds the mixins to the collection in topological order from root to base.
    * @param col The collection to add the mixin file paths.
    * @return Collection of mixin JAR file paths.
    * @throws MetadataException If a mixin cannot be loaded.
    */
   public Collection addMixinsTo(final Collection col) throws MetadataException
   {
      getListing(new XMLMetadataHelper.MixinHandler()
      {
         public void handleRepository(XMLMixin mixin)
         {
            try
            {
               URLConnection con = mixin.getHelper().getRootURL().openConnection();

               if (con instanceof JarURLConnection)
               {
                  col.add(((JarURLConnection)con).getJarFile().getName());
               }
            }
            catch (IOException e)
            {
               throw new MetadataException("err.meta.mixin", new Object[]{mixin.getNamespace()}, e);
            }
         }

         public void handleMixinReference(XMLMixin ref, XMLMixin parent)
         {
         }

         public void handleMixinOverrideConflict(XMLMixin ref, XMLMixin mixin)
         {
         }

         public void handleCircularReference(XMLMixin mixin)
         {
            throw new MetadataValidationException("err.meta.mixinCycle", new Object[]{mixin.getName()});
         }

         public void handleLinkFailure(XMLMixin ref, Exception e)
         {
            throw new MetadataException("err.meta.mixin", new Object[]{ref}, e);
         }

         public void handleAlternateResource(XMLResource first, XMLResource second)
         {
         }

         public void handleResourceOverride(XMLResource source, XMLResource overridden)
         {
         }

         public void handleResourceConflict(XMLResource first, XMLResource second)
         {
         }
      });

      return col;
   }

   /**
    * Iterates over all referenced mixins.
    * @param rootElement The root descriptor element.
    * @param baseMetaFolderURL Optional URL for the base repository meta folder.
    * @param handler The mixin handler.
    * @throws MetadataException if a mixin's .metadata file fails to parse.
    */
   public static void forEachReferencedMixin(Element rootElement, URL baseMetaFolderURL, MixinNamespaceHandler handler)
   {
      final HolderDeque mixinSet = new HashDeque(4);
      XMLUtil.ElementHandler mixinElementHandler = new XMLUtil.ElementHandler()
      {
         public void handleElement(Element element)
         {
            mixinSet.add(new Pair(XMLUtil.getStringAttr(element, "namespace", ""), new Pair(XMLUtil.getStringAttr(element,
               "version", ""), XMLUtil.getStringAttr(element, "checksum", ""))));
         }
      };

      forEachMixin(baseMetaFolderURL, mixinElementHandler);
      forEachMixin(rootElement, mixinElementHandler);

      Holder registeredList = new HashHolder(mixinSet.size());

      while (!mixinSet.isEmpty())
      {
         Pair p = (Pair)mixinSet.removeFirst();

         if (registeredList.add(p))
         {
            String sNamespace = (String)p.getHead();

            p = p.getNext();
            forEachMixin(handler.handle(sNamespace, (String)p.getHead(), (String)p.getTail()), mixinElementHandler);
         }
      }
   }

   /**
    * Iterates over mixin xml elements.
    * @param metaFolderURL The URL of a meta folder.
    * @param mixinHandler The mixin element handler.
    * @throws MetadataException if a mixin's .metadata file fails to parse.
    */
   public static void forEachMixin(URL metaFolderURL, XMLUtil.ElementHandler mixinHandler)
   {
      if (metaFolderURL != null)
      {
         Element root = parseElement(metaFolderURL);

         if (root != null)
         {
            forEachMixin(root, mixinHandler);
         }
      }
   }

   /**
    * @param metaFolderURL The URL of a meta folder.
    * @return The root descriptor element.
    * @throws MetadataException if a mixin's .metadata file fails to parse.
    */
   public static Element parseElement(URL metaFolderURL)
   {
      return new XMLMetadataHelper(metaFolderURL, null, null, null).getDescriptorElement(true).getOwnerDocument()
         .getDocumentElement();
   }

   /**
    * Iterates over mixin xml elements.
    * @param rootElement The root descriptor element.
    * @param mixinHandler The mixin element handler.
    */
   public static void forEachMixin(Element rootElement, final XMLUtil.ElementHandler mixinHandler)
   {
      XMLUtil.withFirstChildElement(rootElement, "Mixins", false, new XMLUtil.ElementHandler()
      {
         public void handleElement(Element element)
         {
            XMLUtil.forEachChildElement(element, "Mixin", mixinHandler);
         }
      });
   }

   /**
    * Gets the value of a property.
    * @param sName The name of the property to get.
    * @return The property value; null if the property doesn't exist.
    */
   public String getProperty(String sName)
   {
      return getProperty(sName, null);
   }

   /**
    * Gets the value of a property.
    * @param sName The name of the property to get.
    * @param sDefault The default value.
    * @return The property value; the default value if the property doesn't exist.
    */
   public String getProperty(String sName, String sDefault)
   {
      if (m_properties == null)
      {
         return sDefault;
      }

      return m_properties.getProperty(sName, sDefault);
   }

   // inner classes

   /**
    * Interface for processing a character stream.
    */
   public interface CharacterStreamHandler
   {
      /**
       * Process a character stream.
       * @param reader The character stream reader.
       * @param sName The resource base name (without path and extension).
       * @throws IOException If a stream input error occurs.
       */
      void handleCharacterStream(Reader reader, String sName) throws IOException;
   }

   /**
    * Character stream handler for XML resources.
    */
   public static class ResourceCharacterStreamHandler implements CharacterStreamHandler
   {
      // associations

      /**
       * The handler for the element.
       */
      protected ResourceHandler m_handler;

      /**
       * The mapping of schema URLs to other URLs that will be used as the actual locations
       * of those schemas.
       */
      protected LookupDeque m_schemaURLDeque;

      // constructors

      /**
       * Constructs the handler.
       * @param handler The resource handler.
       */
      public ResourceCharacterStreamHandler(ResourceHandler handler)
      {
         m_schemaURLDeque = DEFAULT_SCHEMA_URL_DEQUE;
         m_handler = handler;
      }

      /**
       * Constructs the handler.
       * @param handler The resource handler.
       * @param schemaURLDeque The mapping of schema URLs to other URLs that will
       * be used as the actual locations of those schemas, in the schema parse order.
       */
      public ResourceCharacterStreamHandler(ResourceHandler handler, LookupDeque schemaURLDeque)
      {
         m_handler = handler;
         m_schemaURLDeque = schemaURLDeque;
      }

      // operations

      /**
       * @see nexj.core.meta.xml.XMLMetadataHelper.CharacterStreamHandler#handleCharacterStream(java.io.Reader, java.lang.String)
       */
      public void handleCharacterStream(Reader reader, String sName) throws IOException
      {
         m_handler.handleResource(XMLUtil.parse(reader, m_schemaURLDeque).getDocumentElement(), sName);
      }
   }

   /**
    * Character stream handler for encrypted streams.
    */
   public static class EncryptedCharacterStreamHandler implements CharacterStreamHandler
   {
      // associations

      /**
       * The handler for the element.
       */
      protected CharacterStreamHandler m_handler;

      /**
       * The properties with which the cipher shall be initialized.
       */
      protected Properties m_properties;

      /**
       * Encryption scheme set to add encryptions to.
       */
      protected Set m_encryptionSchemeSet;

      // constructors

      /**
       * Constructs the handler.
       *
       * @param handler The resource handler.
       * @param properties The properties to initialize the cipher.
       */
      public EncryptedCharacterStreamHandler(CharacterStreamHandler handler, Properties properties)
      {
         m_handler = handler;
         m_properties = properties;
      }

      // operations

      /**
       * Set encryption scheme set.
       * @param encryptionSchemeSet Encryption scheme set.
       */
      public void setEncryptionSchemeSet(Set encryptionSchemeSet)
      {
         m_encryptionSchemeSet = encryptionSchemeSet;
      }

      /**
       * Get encryption scheme set.
       * @return Encryption scheme set.
       */
      public Set getEncryptionSchemeSet()
      {
         return m_encryptionSchemeSet;
      }

      /**
       * @see nexj.core.meta.xml.XMLMetadataHelper.CharacterStreamHandler#handleCharacterStream(java.io.Reader, java.lang.String)
       */
      public void handleCharacterStream(Reader reader, String sName) throws IOException
      {
         CharacterStreamCipherDispatcher dispatcher = new CharacterStreamCipherDispatcher();

         dispatcher.init(m_properties);
         dispatcher.setEncryptionSchemeSet(m_encryptionSchemeSet);
         m_handler.handleCharacterStream(dispatcher.createDecryptedReader(reader), sName);
      }
   }

   /**
    * Character stream handler for XML resources that will upgrade them prior to passong them on.
    */
   public class UpgradingResourceCharacterStreamHandler extends ResourceCharacterStreamHandler
   {
      /**
       * @see nexj.core.meta.xml.XMLMetadataHelper.ResourceCharacterStreamHandler#ResourceCharacterStreamHandler(nexj.core.meta.xml.XMLMetadataHelper.ResourceHandler)
       */
      public UpgradingResourceCharacterStreamHandler(ResourceHandler handler)
      {
         super(handler);
      }

      /**
       * @see nexj.core.meta.xml.XMLMetadataHelper.ResourceCharacterStreamHandler#ResourceCharacterStreamHandler(nexj.core.meta.xml.XMLMetadataHelper.ResourceHandler, nexj.core.util.LookupDeque)
       */
      public UpgradingResourceCharacterStreamHandler(
         ResourceHandler handler, LookupDeque schemaURLDeque)
      {
         super(handler, schemaURLDeque);
      }

      /**
       * @see nexj.core.meta.xml.XMLMetadataHelper.CharacterStreamHandler#handleCharacterStream(java.io.Reader, java.lang.String)
       */
      public void handleCharacterStream(Reader reader, String sName) throws IOException
      {
         Element element = (m_upgrader != null) ? m_upgrader.upgrade(reader, sName, XMLMetadataHelper.this) : null;

         if (element != null)
         {
            if (m_schemaURLDeque == null || m_schemaURLDeque.size() == 0)
            {
               m_handler.handleResource(element, sName);

               return;
            }

            reader = new StringReader(XMLUtil.formatXML(element));
         }

         super.handleCharacterStream(reader, sName);
      }
   }

   /**
    * Interface for processing a resource.
    */
   public interface ResourceHandler
   {
      /**
       * Process a resource.
       * @param rootElement The root DOM element.
       * @param sName The resource base name (without path and extension).
       */
      void handleResource(Element rootElement, String sName);
   }

   /**
    * Interface for handling a resource with a given base name.
    */
   public interface ResourceNameHandler
   {
      /**
       * Handles the resource.
       * @param sBaseName The resource base name (without path and extension).
       * @param sFullName The relative path to the resource from the repository root.
       */
      public void handleResource(String sBaseName, String sFullName);
   }

   /**
    * Element handler with exception handling.
    */
   public abstract class ElementHandler implements XMLUtil.ElementHandler
   {
      private String m_sProperty;

      /**
       * Constructs the element handler.
       * @param sProperty The property name.
       */
      public ElementHandler(String sProperty)
      {
         m_sProperty = sProperty;
      }

      /**
       * @see nexj.core.util.XMLUtil.ElementHandler#handleElement(org.w3c.dom.Element)
       */
      public final void handleElement(Element element)
      {
         String sName = getName(element);
         int nCookie = pushMarker(MetadataValidationException.TYPE_NAME, element.getNodeName());

         pushMarker(m_sProperty, sName);

         try
         {
            handleElement(element, sName);
         }
         catch (MetadataException e)
         {
            addException(e);
         }
         catch (XMLException e)
         {
            addException(e);
         }
         finally
         {
            restoreMarker(nCookie);
         }
      }

      /**
       * Gets the name attribute from the element.
       * @param element The element.
       * @return The name value.
       */
      protected String getName(Element element)
      {
         return getNameAttr(element);
      }

      /**
       * Template method to handle the element.
       */
      protected abstract void handleElement(Element element, String sName);
   }

   /**
    * Interface for Mixin handler.
    */
   public interface MixinNamespaceHandler
   {
      /**
       * @param sNamespace Model namespace.
       * @param sVersion Model version.
       * @param sChecksum Model checksum.
       * @return The URL of the meta folder for this mixin model.
       */
      URL handle(String sNamespace, String sVersion, String sChecksum);
   }

   /**
    * Interface for setting the validation exception context marker.
    */
   public interface Marker
   {
      /**
       * Sets the marker properties.
       * @param marker The metadata marker.
       */
      void setMarker(MetadataMarker e);
   }

   /**
    * Interface for fixing up references.
    */
   public interface Fixup extends Marker
   {
      /**
       * Fixes up the reference.
       */
      void fixup();
   }

   /**
    * Abstract fixup class for initializing the marker from the current context.
    */
   public static abstract class ContextFixup implements Fixup
   {
      private String[] m_markerArray;

      private final static String[] s_templateArray = new String[0];

      public ContextFixup(XMLMetadataHelper helper)
      {
         m_markerArray = (String[])helper.m_markerList.toArray(s_templateArray);
      }

      public final void setMarker(MetadataMarker e)
      {
         for (int i = 0; i < m_markerArray.length; i += 2)
         {
            e.setProperty(m_markerArray[i], m_markerArray[i + 1]);
         }
      }

      public final void pushMarker(XMLMetadataHelper helper)
      {
         for (int i = 0; i < m_markerArray.length; i += 2)
         {
            helper.pushMarker(m_markerArray[i], m_markerArray[i + 1]);
         }
      }
   }

   /**
    * Interface to handle events during Mixin linking.
    */
   public interface MixinHandler
   {
      /**
       * Called for each Mixin element encountered in each repository descriptor during linking.  Guaranteed to be called before the
       * corresponding handleRepository method is called.
       * @param ref The mixin reference.
       * @param mixin The referring mixin.
       */
      public void handleMixinReference(XMLMixin ref, XMLMixin mixin);

      /**
       * Called for a mixin that creates a reference loop.
       * @param mixin The mixin that creates a loop.
       */
      public void handleCircularReference(XMLMixin mixin);

      /**
       * Called when a non-overridable mixin is overridden.
       * @param ref The mixin reference.
       * @param mixin The referring mixin.
       */
      public void handleMixinOverrideConflict(XMLMixin ref, XMLMixin mixin);

      /**
       * Called for each repository linked, including the root, in pre-order.
       * @param mixin The linked mixin.
       */
      public void handleRepository(XMLMixin mixin);

      /**
       * Called once for each resource having multiple non-identical sources.
       * @param first One of the sources.
       * @param second Another source, which is not identical to first.
       */
      public void handleResourceConflict(XMLResource first, XMLResource second);

      /**
       * @param source The overriding source.
       * @param overridden The source being overridden.
       */
      public void handleResourceOverride(XMLResource source, XMLResource overridden);

      /**
       * @param first One of the sources.
       * @param second Another source, which is an identical alternate to first.
       */
      public void handleAlternateResource(XMLResource first, XMLResource second);

      /**
       * Called if a repository could not be linked.
       * @param ref The mixin which we failed to link.
       * @param e The exception raised during linking.
       */
      public void handleLinkFailure(XMLMixin ref, Exception e);
   }

   /**
    * Add encryption scheme.
    * @param sEncryptionScheme encryption scheme.
    */
   public void addEncryptionScheme(String sEncryptionScheme)
   {
      m_encryptionSchemeSet.add(sEncryptionScheme);
   }

   /**
    * Set encryption scheme set.
    * @param encryptionSchemeSet Encryption scheme set.
    */
   public void setEncryptionSchemeSet(Set encryptionSchemeSet)
   {
      m_encryptionSchemeSet = encryptionSchemeSet;
   }

   /**
    * Get encryption scheme set.
    * @return Encryption scheme set.
    */
   public Set getEncryptionSchemeSet()
   {
      return m_encryptionSchemeSet;
   }
}