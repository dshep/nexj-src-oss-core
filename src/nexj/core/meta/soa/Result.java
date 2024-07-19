package nexj.core.meta.soa;

import nexj.core.scripting.Symbol;

/**
 * The result of a method.
 */
public class Result
{
   // attributes

   /**
    * The return value type.
    */
   public Symbol m_type;

   /**
    * The documentation for this result.
    */
   public String m_sDescription = "";

   /**
    * True if the result is a collection; false otherwise.
    */
   public boolean m_bCollection;

   // operations

   /**
    * Sets the return value type.
    * @param type The type name.
    */
   public void setType(Symbol type)
   {
      m_type = type;
   }

   /**
    * Gets the return value type.
    * @return The type name.
    */
   public Symbol getType()
   {
      return m_type;
   }

   /**
    * Sets the documentation on this result.
    * @param sDescription The result description.
    */
   public void setDescription(String sDescription)
   {
      m_sDescription = (sDescription == null) ? "" : sDescription.trim();
   }

   /**
    * Gets the documentation on this result.
    * @return The result description.
    */
   public String getDescription()
   {
      return m_sDescription;
   }

   /**
    * Sets whether the result returns a collection.
    * @param bCollection True if the result is a collection; false otherwise.
    */
   public void setCollection(boolean bCollection)
   {
      m_bCollection = bCollection;
   }

   /**
    * Gets whether the result returns a collection.
    * @return True if the result is a collection; false otherwise.
    */
   public boolean isCollection()
   {
      return m_bCollection;
   }
}
