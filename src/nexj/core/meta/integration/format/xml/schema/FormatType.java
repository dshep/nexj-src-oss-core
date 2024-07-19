// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;

/**
 * Definition of a primitive type that is governed by a format string.
 */
public class FormatType extends AtomicType
{
   // attributes

   /**
    * The format string.
    */
   protected String m_sFormat;

   // constructors

   /**
    * Constructs a new format string type definition.
    * @param sName The type name, if any.
    * @param base The base type that is restricted by this format.
    */
   public FormatType(String sName, AtomicType base)
   {
      super(sName);
      assert base != null;

      m_base = base;
   }

   // operations

   /**
    * Gets the format.
    * @return The format string.
    */
   public String getFormat()
   {
      return m_sFormat;
   }

   /**
    * Sets the format.
    * @param sFormat The format string to set.
    */
   public void setFormat(String sFormat)
   {
      m_sFormat = sFormat;
   }

   /**
    * @see nexj.core.meta.integration.format.xml.schema.AtomicType#getItemType()
    */
   public byte getItemType()
   {
      return FORMAT_TYPE;
   }

   /**
    * @see nexj.core.meta.integration.format.xml.schema.AtomicType#toString()
    */
   public String toString()
   {
      return "FormatType(" + ((m_sName == null) ? "ANONYMOUS" : "\"" + m_sName + "\"") + ", format=\"" + m_sFormat + "\")";
   }
}
