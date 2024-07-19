package nexj.core.meta.xml;

import nexj.core.meta.NamedMetadataObject;
import nexj.core.util.ObjUtil;

/**
 * Represents a grouped set of metadata resources with a namespace and a version.
 */
public class XMLMixin extends NamedMetadataObject
{
   // attributes

   /**
    * The mix-in namespace.
    */
   protected String m_sNamespace;

   /**
    * The mix-in version.
    */
   protected String m_sVersion;

   /**
    * The mix-in checksum.
    */
   protected String m_sChecksum;

   /**
    * The module name.
    */
   protected String m_sModule;

   /**
    * The resource overridability flag.
    */
   protected boolean m_bOverridable;

   /**
    * The root repository flag.
    */
   protected boolean m_bRoot;

   // associations

   /**
    * The metadata listing.
    */
   protected XMLMetadataListing m_listing;

   /**
    * The metadata helper.
    */
   protected XMLMetadataHelper m_helper;

   // constructors

   /**
    * Constructs the mix-in.
    */
   public XMLMixin(String sNamespace)
   {
      m_sNamespace = sNamespace;
      m_bRoot = true;
   }

   /**
    * Constructs the mix-in.
    */
   public XMLMixin(String sNamespace, String sVersion, String sChecksum, boolean bOverridable, boolean bRoot)
   {
      this(sNamespace);
      m_sVersion = sVersion;
      m_sChecksum = sChecksum;
      m_bOverridable = bOverridable;
      m_bRoot = bRoot;
   }

   // operations

   /**
    * Sets the mix-in namespace.
    * @param sNamespace The mix-in namespace to set.
    */
   public void setNamespace(String sNamespace)
   {
      verifyNotReadOnly();
      m_sNamespace = sNamespace;
   }

   /**
    * @return The mix-in namespace.
    */
   public String getNamespace()
   {
      return m_sNamespace;
   }

   /**
    * Sets the mix-in version.
    * @param sVersion The mix-in version to set.
    */
   public void setVersion(String sVersion)
   {
      verifyNotReadOnly();
      m_sVersion = sVersion;
   }

   /**
    * @return The mix-in version.
    */
   public String getVersion()
   {
      return m_sVersion;
   }

   /**
    * Sets the mix-in checksum.
    * @param sChecksum The mix-in checksum to set.
    */
   public void setChecksum(String sChecksum)
   {
      verifyNotReadOnly();
      m_sChecksum = sChecksum;
   }

   /**
    * @return The mix-in checksum.
    */
   public String getChecksum()
   {
      return m_sChecksum;
   }

   /**
    * Sets the module name.
    * @param sModule The module name to set.
    */
   public void setModule(String sModule)
   {
      verifyNotReadOnly();
      m_sModule = sModule;
   }

   /**
    * @return The module name.
    */
   public String getModule()
   {
      return m_sModule;
   }

   /**
    * Sets the resource overridability flag.
    * @param bOverridable The resource overridability flag to set.
    */
   public void setOverridable(boolean bOverridable)
   {
      verifyNotReadOnly();
      m_bOverridable = bOverridable;
   }

   /**
    * @return The resource overridability flag.
    */
   public boolean isOverridable()
   {
      return m_bOverridable;
   }

   /**
    * Sets the root repository flag.
    * @param bRoot The root repository flag to set.
    */
   public void setRoot(boolean bRoot)
   {
      verifyNotReadOnly();
      m_bRoot = bRoot;
   }

   /**
    * @return The root repository flag.
    */
   public boolean isRoot()
   {
      return m_bRoot;
   }

   /**
    * Sets the metadata listing.
    * @param listing The metadata listing to set.
    */
   public void setListing(XMLMetadataListing listing)
   {
      verifyNotReadOnly();
      m_listing = listing;
   }

   /**
    * @return The metadata listing.
    */
   public XMLMetadataListing getListing()
   {
      return m_listing;
   }

   /**
    * Sets the metadata helper.
    * @param helper The metadata helper to set.
    */
   public void setHelper(XMLMetadataHelper helper)
   {
      verifyNotReadOnly();
      m_helper = helper;
   }

   /**
    * @return The metadata helper.
    */
   public XMLMetadataHelper getHelper()
   {
      return m_helper;
   }

   /**
    * @see nexj.core.meta.NamedMetadataObject#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(64);

      buf.append(ObjUtil.getShortClassName(this));
      buf.append(" \"");
      buf.append(m_sNamespace);
      buf.append("\" ");
      buf.append(m_sVersion);

      return buf.toString();
   }
}
