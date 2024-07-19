// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * String identifier used for representing localized strings.
 */
public class StringId implements java.io.Serializable
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 4710487296356320406L;

   // attributes
   
   private String m_sStringId;
   
   // constructors
   
   /**
    * Creates a new string identifier.
    * @param sStringId The string id to wrap.
    */
   public StringId(String sStringId)
   {
      m_sStringId = sStringId;
   }
   
   // operations
   
   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (!(obj instanceof StringId))
      {
         return false;
      }
      
      StringId id = (StringId)obj;
      
      if (m_sStringId == null)
      {
         return id.m_sStringId == null;
      }
      
      return m_sStringId.equals(id.m_sStringId);
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return 12345 ^ ((m_sStringId == null) ? 0 : m_sStringId.hashCode());
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return (m_sStringId == null) ? "" : m_sStringId;
   }
}
