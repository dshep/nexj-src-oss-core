// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.util.Named;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;

/**
 * Base class for all named metadata objects.
 */
public abstract class NamedMetadataObject extends MetadataObject implements Named, Comparable
{
   // attributes

   /**
    * The name of the object. It must be unique within its corresponding scope.
    */
   protected String m_sName;
   
   // constructors
   
   /**
    * Creates a named metadata object.
    * @param sName The name of the object.
    */
   protected NamedMetadataObject(String sName)
   {
      setName(sName);
   }

   /**
    * Creates a metadata object with null name.
    */
   protected NamedMetadataObject()
   {
      m_sName = null;
   }
   
   // operations

   /**
    * Sets the object name (if the object is not read-only).
    * @param sName The name to set.
    * @throws MetadataException if the metadata is read-only.
    */
   public void setName(String sName)
   {
      verifyNotReadOnly();
      m_sName = (sName == null) ? null : StringUtil.intern(sName);
   }

   /**
    * @return The object name.
    */
   public String getName()
   {
      return m_sName;
   }

   /**
    * @see nexj.core.meta.MetadataObject#createLookupException()
    */
   protected MetadataException createLookupException()
   {
      return new MetadataLookupException("err.meta.namedLookup",
         new Object[]{ObjUtil.getShortClassName(this), m_sName});
   }

   /**
    * @see java.lang.Comparable#compareTo(java.lang.Object)
    */
   public int compareTo(Object obj)
   {
      if (!(obj instanceof NamedMetadataObject))
      {
         return getClass().getName().compareTo(obj.getClass().getName());
      }

      String sName = ((NamedMetadataObject)obj).getName();
      int n = m_sName.compareToIgnoreCase(sName);

      if (n == 0)
      {
         n = m_sName.compareTo(sName);
      }

      return n;
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return (m_sName != null) ? m_sName.hashCode() : 0;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(64);

      buf.append(ObjUtil.getShortClassName(this));
      buf.append(' ');

      if (m_sName == null)
      {
         buf.append("<anonymous>");
      }
      else
      {
         buf.append(m_sName);
      }
      
      return buf.toString();
   }
}
