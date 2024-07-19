package nexj.core.util;

import java.io.FilterWriter;
import java.io.IOException;
import java.io.Writer;

/**
 * XML writer skipping unnecessary output.
 */
public class CompactXMLWriter extends XMLWriter
{
   // constructors

   /**
    * Constructs the writer.
    * @see FilterWriter
    */
   public CompactXMLWriter(Writer writer)
   {
      super(writer);
   }

   // operations

   /**
    * @see nexj.core.util.XMLWriter#writeAttribute(java.lang.String, java.lang.CharSequence)
    */
   public void writeAttribute(String sName, CharSequence sValue) throws IOException
   {
      if (sValue != null)
      {
         super.writeAttribute(sName, sValue);
      }
   }

   /**
    * @see nexj.core.util.XMLWriter#writeAttribute(java.lang.String, java.lang.CharSequence, java.lang.CharSequence)
    */
   public void writeAttribute(String sName, CharSequence s1, CharSequence s2) throws IOException
   {
      if (s1 != null || s2 != null)
      {
         super.writeAttribute(sName, s1, s2);
      }
   }

   /**
    * @see nexj.core.util.XMLWriter#writeAttribute(java.lang.String, java.lang.CharSequence, java.lang.CharSequence, java.lang.CharSequence)
    */
   public void writeAttribute(String sName, CharSequence s1,
      CharSequence s2, CharSequence s3) throws IOException
   {
      if (s1 != null || s2 != null || s3 != null)
      {
         super.writeAttribute(sName, s1, s2, s3);
      }
   }

   /**
    * @see nexj.core.util.XMLWriter#writeAttribute(java.lang.String, java.lang.CharSequence, java.lang.CharSequence, java.lang.CharSequence, java.lang.CharSequence)
    */
   public void writeAttribute(String sName, CharSequence s1, CharSequence s2,
      CharSequence s3, CharSequence s4) throws IOException
   {
      if (s1 != null || s2 != null || s3 != null || s4 != null)
      {
         super.writeAttribute(sName, s1, s2, s3, s4);
      }
   }

   /**
    * @see nexj.core.util.XMLWriter#writeAttribute(java.lang.String, java.lang.CharSequence, java.lang.CharSequence, java.lang.CharSequence, java.lang.CharSequence, java.lang.CharSequence)
    */
   public void writeAttribute(String sName, CharSequence s1, CharSequence s2,
      CharSequence s3, CharSequence s4, CharSequence s5) throws IOException
   {
      if (s1 != null || s2 != null || s3 != null || s4 != null || s5 != null)
      {
         super.writeAttribute(sName, s1, s2, s3, s4, s5);
      }
   }

   /**
    * @see nexj.core.util.XMLWriter#writeAttribute(java.lang.String, java.lang.CharSequence, java.lang.CharSequence, java.lang.CharSequence, java.lang.CharSequence, java.lang.CharSequence, java.lang.CharSequence)
    */
   public void writeAttribute(String sName, CharSequence s1, CharSequence s2,
      CharSequence s3, CharSequence s4, CharSequence s5, CharSequence s6) throws IOException
   {
      if (s1 != null || s2 != null || s3 != null ||
         s4 != null || s5 != null || s6 != null)
      {
         super.writeAttribute(sName, s1, s2, s3, s4, s5, s6);
      }
   }

   /**
    * @see nexj.core.util.XMLWriter#writeAttribute(java.lang.String, java.lang.CharSequence, java.lang.CharSequence, java.lang.CharSequence, java.lang.CharSequence, java.lang.CharSequence, java.lang.CharSequence, java.lang.CharSequence)
    */
   public void writeAttribute(String sName, CharSequence s1, CharSequence s2,
      CharSequence s3, CharSequence s4, CharSequence s5, CharSequence s6,
      CharSequence s7) throws IOException
   {
      if (s1 != null || s2 != null || s3 != null ||
         s4 != null || s5 != null || s6 != null ||
         s7 != null)
      {
         super.writeAttribute(sName, s1, s2, s3, s4, s5, s6, s7);
      }
   }
}
