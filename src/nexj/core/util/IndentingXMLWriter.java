// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.Writer;


/**
 * An XML writer that knows how to prettyprint via indentation and newlines.
 */
public class IndentingXMLWriter extends XMLWriter
{
   // attributes

   /**
    * Did a raw write event occur.
    * Use for determining if in-line content was written between tags.
    * Default value "true" will suppress the initial EOL
    * (initial indent is "" so it's not affected).
    */
   protected boolean m_bModified = true;

   /**
    * Indentation count.
    */
   protected int m_nIndent = 0;

   /**
    * The indentation string to use.
    */
   protected String m_sIndent;

   // constructors

   /**
    * Constructs the writer.
    * @param writer The writer to wrap.
    * @param sIndent The indent string to use. 
    */
   public IndentingXMLWriter(Writer writer, String sIndent)
   {
      super(writer);
      m_sIndent = (sIndent == null) ? "" : sIndent;
   }

   /**
    * Constructs the writer with the best-practice 3-space indentation.
    * @param writer The writer to wrap.
    */
   public IndentingXMLWriter(Writer writer)
   {
      this(writer, "   ");
   }

   // operations

   /**
    * @see nexj.core.util.XMLWriter#closeElement()
    */
   public void closeElement() throws IOException
   {
      super.closeElement();
      m_bModified = false;
      ++m_nIndent;
   }

   /**
    * @see nexj.core.util.XMLWriter#closeEmptyElement()
    */
   public void closeEmptyElement() throws IOException
   {
      super.closeEmptyElement();
      m_bModified = false;
   }

   /**
    * @see nexj.core.util.XMLWriter#endElement(java.lang.String)
    */
   public void endElement(String sName) throws IOException
   {
      --m_nIndent;
      prettyPrint();
      super.endElement(sName);
      m_bModified = false;
   }

   /**
    * @see nexj.core.util.XMLWriter#openElement(java.lang.String)
    */
   public void openElement(String sName) throws IOException
   {
      prettyPrint();
      super.openElement(sName);
      m_bModified = false;
   }

   /**
    * Write out a formatting string if prettyprinting is enabled for current level.
    * @throws IOException On output error.
    */
   protected void prettyPrint() throws IOException
   {   
      if (m_bModified)
      {
         return;
      }

      write(SysUtil.LINE_SEP);

      for (int i = 0; i < m_nIndent; ++i)
      {
         write(m_sIndent);
      }

      m_bModified = false; // modified with prettyprinting and not content
   }

   /**
    * Note that a raw write event occurred.
    * @see java.io.FilterWriter#write(char[], int, int)
    */
   public void write(char[] cbuf, int nOffset, int nLength) throws IOException
   {
      super.write(cbuf, nOffset, nLength);
      m_bModified = true;
   }

   /**
    * Note that a raw write event occurred.
    * @see java.io.FilterWriter#write(int)
    */
   public void write(int ch) throws IOException
   {
      super.write(ch);
      m_bModified = true;
   }

   /**
    * Note that a raw write event occurred.
    * @see java.io.FilterWriter#write(java.lang.String, int, int)
    */
   public void write(String sStr, int nOffset, int nLength) throws IOException
   {
      super.write(sStr, nOffset, nLength);
      m_bModified = true;
   }
}
