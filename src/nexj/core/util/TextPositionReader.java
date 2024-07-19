// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.Reader;

/**
 * Reader wrapper that tracks the text position.
 */
public class TextPositionReader extends Reader
{
   // attributes
   
   /**
    * Saved line for the marked position.
    */
   private int m_nLineSaved;
   
   /**
    * Saved column for the marked position.
    */
   private int m_nColumnSaved;
   
   /**
    * Saved flag for skipping the next new line char.
    */
   private boolean m_bSkipNLSaved;
   
   /**
    * Saved flag for increasing the line number.
    */
   private boolean m_bNewLineSaved;
   
   /**
    * True if the next new line char must be skipped.
    */
   private boolean m_bSkipNL = false;
   
   /**
    * True if the line number should be increased on the next character.
    */
   private boolean m_bNewLine = false;
   
   /**
    * The temporary buffer for skipping characters.
    */
   private char[] m_skipBuf = null;

   // associations

   /**
    * The wrapped reader.
    */
   private Reader m_reader;
   
   /**
    * The text position object.
    */
   private TextPosition m_textPos;

   // constructors

   /**
    * Creates a wrapper (decorator) for the supplied reader.
    * @param reader The reader to wrap (decorate).
    */
   public TextPositionReader(Reader reader)
   {
      this(reader, null);
   }

   /**
    * Creates a wrapper (decorator) for the supplied reader.
    * @param reader The reader to wrap (decorate).
    * @param sURL The URL that should be stored in the text positions
    */
   public TextPositionReader(Reader reader, String sURL)
   {
      super(reader);

      m_reader = reader;
      m_textPos = new TextPosition(0, -1, sURL); 
   }

   // operations

   /**
    * @return The text position object.
    */
   public TextPosition getTextPosition()
   {
      return m_textPos;
   }

   /**
    * @see java.io.Reader#read(char[], int, int)
    */
   public int read(char[] buf, int nOffset, int nCount) throws IOException
   {
      synchronized (this.lock)
      {
         int nReadCount = m_reader.read(buf, nOffset, nCount);
   
         for (int i = nOffset; i < nOffset + nReadCount; ++i)
         {
            int ch = buf[i];
            
            if (m_bSkipNL)
            {
               m_bSkipNL = false;
               
               if (ch == '\n')
               {
                  continue;
               }
            }
            
            if (m_bNewLine)
            {
               m_textPos.setLine(m_textPos.getLine() + 1);
               m_textPos.setColumn(-1);
               m_bNewLine = false;
            }

            if (ch == '\r')
            {
               m_bSkipNL = true;
               m_bNewLine = true;
            }
            else if (ch == '\n')
            {
               m_bNewLine = true;
            }

            if (ch != -1 || m_textPos.getColumn() == -1)
            {
               m_textPos.setColumn(m_textPos.getColumn() + 1);
            }
         }
   
         return nReadCount;
      }
   }

   /**
    * @see java.io.Reader#read()
    */
   public int read() throws IOException
   {
      synchronized (this.lock)
      {
         int ch = m_reader.read();
   
         if (m_bSkipNL)
         {
            if (ch == '\n')
            {
               ch = m_reader.read();
            }
   
            m_bSkipNL = false;
         }
         
         if (m_bNewLine)
         {
            m_textPos.setLine(m_textPos.getLine() + 1);
            m_textPos.setColumn(-1);
            m_bNewLine = false;
         }

         if (ch == '\r')
         {
            m_bSkipNL = true;
            m_bNewLine = true;
         }
         else if (ch == '\n')
         {
            m_bNewLine = true;
         }
         
         if (ch != -1 || m_textPos.getColumn() == -1)
         {
            m_textPos.setColumn(m_textPos.getColumn() + 1);
         }

         return ch;
      }
   }

   /**
    * @see java.io.Reader#close()
    */
   public void close() throws IOException
   {
      synchronized (this.lock)
      {
         if (m_reader != null)
         {
            m_reader.close();
            m_reader = null;
         }
      }
   }

   /**
    * @see java.io.Reader#mark(int)
    */
   public void mark(int nReadAheadLimit) throws IOException
   {
      synchronized (this.lock)
      {
         m_reader.mark(nReadAheadLimit);
         m_nLineSaved = m_textPos.getLine();
         m_nColumnSaved = m_textPos.getColumn();
         m_bSkipNLSaved = m_bSkipNL;
         m_bNewLineSaved = m_bNewLine;
      }
   }

   /**
    * @see java.io.Reader#reset()
    */
   public void reset() throws IOException
   {
      synchronized (this.lock)
      {
         m_reader.reset();
         m_textPos.setLine(m_nLineSaved);
         m_textPos.setColumn(m_nColumnSaved);
         m_bSkipNL = m_bSkipNLSaved;
         m_bNewLine = m_bNewLineSaved;
      }
   }

   /**
    * @see java.io.Reader#markSupported()
    */
   public boolean markSupported()
   {
      return m_reader.markSupported();
   }

   /**
    * @see java.io.Reader#ready()
    */
   public boolean ready() throws IOException
   {
      return m_reader.ready();
   }

   /**
    * @see java.io.Reader#skip(long)
    */
   public long skip(long lSkipCount) throws IOException
   {
      if (lSkipCount <= 0)
      {
         throw new IllegalArgumentException("Skip count is negative");
      }
      
      if (lSkipCount == 0)
      {
         return 0;
      }
      
      int nBufSize = (int)Math.max(16, Math.min(8192, lSkipCount));
      
      synchronized (this.lock)
      {
         if (m_skipBuf == null || m_skipBuf.length < nBufSize)
         {
            m_skipBuf = new char[nBufSize];
         }

         int nCount;
         long lReadCount;
         for (lReadCount = 0; lReadCount < lSkipCount; lReadCount += nCount)
         {
            nCount = m_reader.read(m_skipBuf, 0, (int)Math.min(lSkipCount - lReadCount, nBufSize));
            
            if (nCount < 0)
            {
               break;
            }
         }
         
         return lReadCount;
      }
   }
}
