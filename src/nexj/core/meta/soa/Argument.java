package nexj.core.meta.soa;

import nexj.core.meta.xml.XMLSOAMetadataLoader.SOAObject;
import nexj.core.scripting.Symbol;

/**
 * An argument to a method.
 */
public class Argument extends SOAObject
{
   // attributes

   /**
    * The type name.
    */
   protected Symbol m_type;

   /**
    * True if the argument is a collection; false otherwise.
    */
   protected boolean m_bCollection;

   // operations

   /**
    * Sets the type of data passed via this argument.
    * @param type The argument data type.
    */
   public void setType(Symbol type)
   {
      m_type = type;
   }

   /**
    * Gets the type of data passed via this argument.
    * @return The argument data type.
    */
   public Symbol getType()
   {
      return m_type;
   }

   /**
    * Sets whether this argument holds a collection.
    * @param bCollection True if the argument is a collection; false otherwise.
    */
   public void setCollection(boolean bCollection)
   {
      m_bCollection = bCollection;
   }

   /**
    * Gets whether this argument holds a collection.
    * @return True if the argument is a collection; false otherwise.
    */
   public boolean isCollection()
   {
      return m_bCollection;
   }

   /**
    * Ensures unique naming of an argument for its method.
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (!(obj instanceof Argument))
      {
         return false;
      }

      return m_sName.equals(((Argument)obj).m_sName);
   }

   /**
    * Ensures unique naming of an argument for its method.
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return m_sName.hashCode();
   }
}
