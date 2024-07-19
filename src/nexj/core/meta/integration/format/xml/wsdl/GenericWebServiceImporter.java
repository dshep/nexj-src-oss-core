package nexj.core.meta.integration.format.xml.wsdl;

import java.net.URL;

import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import nexj.core.meta.integration.format.xml.XSDImportException;
import nexj.core.meta.integration.format.xml.schema.SchemaUniverse;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.TextPosition;

/**
 * Abstract base class for handlers for web service files.
 */
public abstract class GenericWebServiceImporter extends DefaultHandler
{
   //constants

   /**
    * Alias for the default namespace.
    */
   protected final static String DEFAULT_NS_ALIAS = "";

   // attributes

   /**
    * The target namespace.
    */
   protected String m_sTargetNamespace;

   /**
    * Whether or not to search the parent frame for namespace lookups.
    */
   protected boolean m_bSearchParent;

   // associations

   /**
    * The location of schema
    */
   protected URL m_url;

   /**
    * The parent importer.
    */
   protected GenericWebServiceImporter m_parent;

   /**
    * Document locator.
    */
   protected Locator m_locator;

   /**
    * The schema universe.
    */
   protected SchemaUniverse m_universe;

   /**
    * Map from namespace aliases to URIs.
    */
   protected Lookup m_aliasURIMap = new HashTab(); // String[String]

   /**
    * Map from namespace URIs to aliases.
    */
   protected Lookup m_uriAliasMap = new HashTab(); // String[String]

   // constructors

   /**
    * Constructor.
    * @param parent The parent.
    * @param bSearchParent True to query parent for namespace alias resolution.
    * @param universe The schema universe.
    * @param importedURLSet The set of previously imported URLs.
    * @param fixupList The list of fixups.
    */
   public GenericWebServiceImporter()
   {
      m_universe = new SchemaUniverse();
   }

   /**
    * Constructor.
    * @param parent The parent.
    */
   public GenericWebServiceImporter(GenericWebServiceImporter parent, boolean bSearchParent)
   {
      m_parent = parent;
      m_bSearchParent = bSearchParent;
      m_universe = parent.m_universe;
   }

   // operations

   /**
    * Maintains the map of namespace names to namespace URIs.
    * @see org.xml.sax.helpers.DefaultHandler#startPrefixMapping(java.lang.String, java.lang.String)
    */
   public void startPrefixMapping(String sPrefix, String sURI) throws SAXException
   {
      m_aliasURIMap.put(sPrefix, sURI);
      m_uriAliasMap.put(sURI, sPrefix);
   }

   /**
    * Maintains the map of namespace names to namespace URIs.
    * @see org.xml.sax.helpers.DefaultHandler#endPrefixMapping(java.lang.String)
    */
   public void endPrefixMapping(String sPrefix) throws SAXException
   {
      Object uri = m_aliasURIMap.remove(sPrefix);

      if (uri != null)
      {
         m_uriAliasMap.remove(uri);
      }
   }

   /**
    * @see org.xml.sax.helpers.DefaultHandler#setDocumentLocator(org.xml.sax.Locator)
    */
   public void setDocumentLocator(Locator locator)
   {
      m_locator = locator;
   }

   /**
    * Returns the URI mapped to the alias.
    * @param sAlias The alias.
    * @return The URI.
    */
   public String getURI(String sAlias)
   {
      if (m_aliasURIMap.get(sAlias) != null)
      {
         return (String)m_aliasURIMap.get(sAlias);
      }

      if (m_bSearchParent)
      {
         return m_parent.getURI(sAlias);
      }

      throw new XSDImportException("err.meta.import.unknownAlias", new Object[] { sAlias }, getTextPosition());
   }

   /**
    * @return The location.
    */
   public TextPosition getTextPosition()
   {
      return (m_locator == null) ? null : new TextPosition(m_locator.getLineNumber(), m_locator.getColumnNumber(), m_url.toExternalForm());
   }

   /**
    * Gets the namespace name from a qualified name.
    * @param sQName The qualified name.
    * @return The namespace name.
    */
   public static String getAlias(String sQName)
   {
      if (sQName != null)
      {
         int nIndex = sQName.indexOf(':');

         if (nIndex != -1)
         {
            return sQName.substring(0, nIndex);
         }
      }

      return DEFAULT_NS_ALIAS;
   }

   /**
    * Gets the local name from a qualified name.
    * @param sQName The qualified name.
    * @return The local name.
    */
   public static String getLocalName(String sQName)
   {
      return sQName.substring(sQName.lastIndexOf(':') + 1);
   }

   /**
    * Interface to resolve references after parsing is complete.
    */
   protected interface Fixup
   {
      public void fixup();
   }
}
