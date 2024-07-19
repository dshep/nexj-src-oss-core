package nexj.core.meta.xml;

import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.zip.CRC32;

import nexj.core.meta.NamedMetadataObject;
import nexj.core.util.EmptyIterator;

/**
 * Class holding resource information.
 */
public class XMLResource extends NamedMetadataObject
{
   // attributes

   /**
    * The checksum.
    */
   protected long m_lChecksum;

   /**
    * True if the checksum has been computed.
    */
   protected boolean m_bChecksum;

   /**
    * True for a root resource.
    */
   protected boolean m_bRoot;

   /**
    * True if the resource is enabled.
    */
   protected boolean m_bEnabled = true;

   // associations

   /**
    * The resource URL.
    */
   protected URL m_url;

   /**
    * The sources which this source overrides, null if none.
    */
   protected List m_baseList;

   /**
    * The containing mix-in.
    */
   protected XMLMixin m_mixin;

   // constructors

   /**
    * Constructs the resource.
    */
   public XMLResource(String sName, URL url, boolean bRoot)
   {
      super(sName);
      m_url = url;
      m_bRoot = bRoot;
   }

   // operations

   /**
    * Sets the resource URL.
    * @param url The resource URL to set.
    */
   public void setURL(URL url)
   {
      verifyNotReadOnly();
      m_url = url;
   }

   /**
    * @return The resource URL.
    */
   public URL getURL()
   {
      return m_url;
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
    * Sets the resource enablement flag.
    * @param bEnabled True if the resource is enabled.
    */
   public void setEnabled(boolean bEnabled)
   {
      m_bEnabled = bEnabled;
   }

   /**
    * @return True if the resource is enabled.
    */
   public boolean isEnabled()
   {
      return m_bEnabled;
   }

   /**
    * Sets the containing mix-in.
    * @param mixin The containing mix-in to set.
    */
   public void setMixin(XMLMixin mixin)
   {
      verifyNotReadOnly();
      m_mixin = mixin;
   }

   /**
    * @return The containing mix-in.
    */
   public XMLMixin getMixin()
   {
      return m_mixin;
   }

   /**
    * @return The mix-in namespace.
    */
   public String getNamespace()
   {
      return m_mixin.getNamespace();
   }

   /**
    * @return The resource checksum.
    */
   public long getChecksum()
   {
      if (!m_bChecksum)
      {
         verifyNotReadOnly();

         CRC32 crc32 = new CRC32();
         XMLMetadataHelper helper = m_mixin.getHelper();
         int nSearchModeSaved = helper.getSearchMode();

         try
         {
            helper.setSearchMode((m_bRoot) ? XMLMetadataHelper.SEARCH_ROOT_ONLY : XMLMetadataHelper.SEARCH_BASE_ONLY);
            helper.updateResourceChecksum(crc32, m_sName);
         }
         finally
         {
            helper.setSearchMode(nSearchModeSaved);
         }

         m_lChecksum = crc32.getValue();
         m_bChecksum = true;
      }

      return m_lChecksum;
   }

   /**
    * Adds a resource that is overridden by this resource.
    * @param base The resource overridden by this resource.
    */
   public void addBase(XMLResource base)
   {
      verifyNotReadOnly();

      if (m_baseList == null)
      {
         m_baseList = new ArrayList();
      }
      
      m_baseList.add(base);
   }

   /**
    * @return An overridden resource iterator.
    */
   public Iterator getBaseIterator()
   {
      return (m_baseList == null) ? EmptyIterator.getInstance() : m_baseList.iterator();
   }

   /**
    * True if this resource overrides another resource.
    * @param resource The resource to compare.
    * @return True this resource overrides the resource specified in the argument.
    */
   public boolean overrides(XMLResource resource)
   {
      if (!m_sName.equals(resource.getName()))
      {
         return false;
      }

      if (getNamespace().equals(resource.getNamespace()))
      {
         return true;
      }

      if (m_baseList != null) // recursive check
      {
         for (int i = 0; i < m_baseList.size(); ++i)
         {
            if (((XMLResource)m_baseList.get(i)).overrides(resource))
            {
               return true;
            }
         }
      }

      // Fall back on checksum as a last resort, hopefully this can be avoided in most cases
      return getChecksum() == resource.getChecksum();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return (m_mixin == null) ? m_sName : m_sName + '[' + m_mixin + ']';
   }
}