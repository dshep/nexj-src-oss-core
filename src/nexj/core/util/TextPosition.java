// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Represents the line:column position in a text file.
 */
public class TextPosition implements Comparable, Cloneable, java.io.Serializable
{
   // constants
   
   /**
    * The Java stream unique identifier for the class.
    */
   private final static long serialVersionUID = 2016978780886759938L;

   // attributes

   /**
    * The line number, zero-based.
    */
   private int m_nLine;

   /**
    * The column number, zero-based.
    */
   private int m_nColumn;

   /**
    * Optional URL.
    */
   private String m_sURL;

   // constructors

   public TextPosition(int nLine, int nColumn)
   {
      this(nLine, nColumn, null);
   }

   public TextPosition(int nLine, int nColumn, String sURL)
   {
      m_nLine = nLine;
      m_nColumn = nColumn;
      m_sURL = sURL;
   }

   // operations

   /**
    * @return The stored line number, zero-based.
    */
   public int getLine()
   {
      return m_nLine;
   }

   /**
    * Sets the line number.
    * @param nLine The line number, zero-based.
    */
   public void setLine(int nLine)
   {
      m_nLine = nLine;
   }

   /**
    * Sets the column number.
    * @param nColumn The column number, zero-based.
    */
   public void setColumn(int nColumn)
   {
      m_nColumn = nColumn;
   }

   /**
    * @return The stored column number, zero-based.
    */
   public int getColumn()
   {
      return m_nColumn;
   }

   /**
    * Sets the URL.
    * @param sURL The URL to set.
    */
   public void setURL(String sURL)
   {
      m_sURL = sURL;
   }

   /**
    * @return The URL.
    */
   public String getURL()
   {
      return m_sURL;
   }

   /**
    * @see java.lang.Comparable#compareTo(java.lang.Object)
    */
   public int compareTo(Object obj)
   {
      TextPosition pos = (TextPosition)obj;

      if (m_sURL == null)
      {
         return (pos.m_sURL == null) ? 0 : -1;
      }

      if (pos.m_sURL == null)
      {
         return 1;
      }

      int n = m_sURL.compareTo(pos.m_sURL);

      if (n != 0)
      {
         return n;
      }

      n = m_nLine - pos.m_nLine;
      
      if (n != 0)
      {
         return n;
      }
      
      return m_nColumn - pos.m_nColumn;
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         return super.clone();
      }
      catch (CloneNotSupportedException e)
      {
         return null;
      }
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (!(obj instanceof TextPosition))
      {
         return false;
      }

      TextPosition pos = (TextPosition)obj;

      return (m_nLine == pos.m_nLine &&
         m_nColumn == pos.m_nColumn &&
         ObjUtil.equal(m_sURL, pos.m_sURL));
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return m_nLine ^ m_nColumn ^ ((m_sURL == null) ? 0 : m_sURL.hashCode());
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(16);

      buf.append(m_nLine + 1);
      buf.append(':');
      buf.append(m_nColumn + 1);

      return buf.toString();
   }
}
