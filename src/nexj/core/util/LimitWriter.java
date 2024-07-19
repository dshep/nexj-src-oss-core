// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.FilterWriter;
import java.io.IOException;
import java.io.Writer;

public class LimitWriter extends FilterWriter
{
   protected long m_lMaxCount;

   protected long m_lCount;

   public LimitWriter(Writer out, long lMaxCount)
   {
      super(out);
      m_lMaxCount = lMaxCount;
   }

   /**
    * @see java.io.Writer#write(int)
    */
   public void write(int c) throws IOException
   {
      if (m_lCount++ == m_lMaxCount)
      {
         out.write("...");
      }
      else if (m_lCount <= m_lMaxCount)
      {
         super.write(c);
      }
   }

   /**
    * @see java.io.Writer#write(char[], int, int)
    */
   public void write(char[] cbuf, int nOff, int nCount) throws IOException
   {
      boolean bLimit;
      
      if (nCount > 0)
      {
         int n = (int)Math.min(Math.max(m_lMaxCount - m_lCount, 0), nCount);

         bLimit = (n < nCount && m_lMaxCount >= m_lCount);
         m_lCount += nCount;
         nCount = n;
      }
      else
      {
         bLimit = false;
      }

      super.write(cbuf, nOff, nCount);

      if (bLimit)
      {
         out.write("...");
      }
   }

   /**
    * @see java.io.Writer#write(java.lang.String, int, int)
    */
   public void write(String s, int nOff, int nCount) throws IOException
   {
      boolean bLimit;
      
      if (nCount > 0)
      {
         int n = (int)Math.min(Math.max(m_lMaxCount - m_lCount, 0), nCount);

         bLimit = (n < nCount && m_lMaxCount >= m_lCount);
         m_lCount += nCount;
         nCount = n;
      }
      else
      {
         bLimit = false;
      }

      super.write(s, nOff, nCount);

      if (bLimit)
      {
         out.write("...");
      }
   }
}
