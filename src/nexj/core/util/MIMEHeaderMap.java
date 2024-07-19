// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Arrays;

/**
 * MIME header multimap: MIMEHeader[String].
 */
public class MIMEHeaderMap
{
   // attributes

   /**
    * The header count.
    */
   protected int m_nCount;
   
   // associations
   
   /**
    * The header array.
    */
   protected MIMEHeader[] m_headerArray = new MIMEHeader[8];

   // operations

   /**
    * Adds a header to the map without checking for duplicates.
    * @param header The header to add.
    */
   protected void addDup(MIMEHeader header)
   {
      if (m_nCount == m_headerArray.length)
      {
         MIMEHeader[] headerArray = new MIMEHeader[m_nCount << 1];
         
         System.arraycopy(m_headerArray, 0, headerArray, 0, m_nCount);
         m_headerArray = headerArray;
      }

      m_headerArray[m_nCount++] = header;
   }
   
   /**
    * Adds a header to the map.
    * @param header The header to add.
    * @return The resulting header.
    */
   public MIMEHeader add(MIMEHeader header)
   {
      MIMEHeader oldHeader = find(header.getName());

      if (oldHeader != null)
      {
         oldHeader.append(header);
         
         return oldHeader;
      }

      addDup(header);

      return header;
   }

   /**
    * Adds a header to the map.
    * @param sName The header name.
    * @param sValue The header value.
    * @return The resulting header.
    */
   public MIMEHeader add(String sName, String sValue)
   {
      return add(new MIMEHeader(sName, sValue));
   }

   /**
    * Adds/replaces a header.
    * @param The header to add/replace.
    */
   public void set(MIMEHeader header)
   {
      int i = indexOf(header.getName());

      if (i >= 0)
      {
         m_headerArray[i] = header;
      }
      else
      {
         addDup(header);
      }
   }

   /**
    * Adds/replaces a header.
    * @param sName The header name.
    * @param sValue The header value.
    * @return The resulting header.
    */
   public MIMEHeader set(String sName, String sValue)
   {
      MIMEHeader header = new MIMEHeader(sName, sValue);

      set(header);

      return header;
   }

   /**
    * Adds a header, if not already set.
    * @param sName The header name.
    * @param sValue The header value.
    * @return The resulting header.
    */
   public MIMEHeader setDefault(String sName, String sValue)
   {
      int i = indexOf(sName);

      if (i >= 0)
      {
         return m_headerArray[i];
      }

      MIMEHeader header = new MIMEHeader(sName, sValue);

      addDup(header);

      return header;
   }
   
   /**
    * Finds a header by name.
    * @param sName The header name.
    * @return The found header, or null if not found.
    */
   public MIMEHeader find(String sName)
   {
      for (int i = m_nCount - 1; i >= 0; --i)
      {
         MIMEHeader header = m_headerArray[i];

         if (header.getName().equalsIgnoreCase(sName))
         {
            return header;
         }
      }

      return null;
   }

   /**
    * Finds an ordinal number of a header.
    * @param sName The header name.
    * @return The header index, or -1 if not found.
    */
   public int indexOf(String sName)
   {
      for (int i = m_nCount - 1; i >= 0; --i)
      {
         if (m_headerArray[i].getName().equalsIgnoreCase(sName))
         {
            return i;
         }
      }

      return -1;
   }

   /**
    * Removes a header by ordinal number.
    * @param nOrdinal The header ordinal number.
    * @return The removed header.
    */
   public MIMEHeader remove(int nOrdinal)
   {
      if (nOrdinal >= m_nCount)
      {
         throw new ArrayIndexOutOfBoundsException(nOrdinal);
      }
      
      MIMEHeader header = m_headerArray[nOrdinal];
      
      System.arraycopy(m_headerArray, nOrdinal + 1, m_headerArray, nOrdinal, m_nCount - nOrdinal - 1);
      m_headerArray[--m_nCount] = null;

      return header;
   }

   /**
    * Removes a header by name.
    * @param sName The header name.
    * @return The remove header. Can be null.
    */
   public MIMEHeader remove(String sName)
   {
      int i = indexOf(sName);

      if (i >= 0)
      {
         return remove(i);
      }

      return null;
   }
   
   /**
    * Gets a header by ordinal number.
    * @param nOrdinal The header ordinal number.
    */
   public MIMEHeader get(int nOrdinal)
   {
      if (nOrdinal >= m_nCount)
      {
         throw new ArrayIndexOutOfBoundsException(nOrdinal);
      }
      
      return m_headerArray[nOrdinal];
   }

   /**
    * @return The header map size.
    */
   public int size()
   {
      return m_nCount;
   }

   /**
    * Removes all the headers from the map.
    */
   public void clear()
   {
      Arrays.fill(m_headerArray, 0, m_nCount, null);
      m_nCount = 0;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder((m_nCount + 1) << 4);

      for (int i = 0; i < m_nCount; ++i)
      {
         buf.append(m_headerArray[i]);
         buf.append(SysUtil.LINE_SEP);
      }

      return buf.toString();
   }
}
