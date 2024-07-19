package nexj.core.integration.io;

import java.io.FilterInputStream;
import java.io.FilterReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;

import nexj.core.integration.Input;
import nexj.core.integration.IntegrationException;
import nexj.core.util.Binary;
import nexj.core.util.XMLUtil;

/**
 * A wrapper implementation of Input for debugging purposes.
 */
public class DebugInput implements Input
{
   // constants

   /**
    * The default prefix size.
    */
   protected static int s_nDefaultPrefixSize = 1 << 14;  // 16KB

   // attributes

   /**
    * The size of the prefix in bytes (or characters).
    */
   protected int m_nPrefixSize;

   // associations

   /**
    * The underlying Input object.
    */
   protected Input m_input;

   /**
    * A buffer for keeping the first 16K characters.
    */
   protected StringBuffer m_strBuffer;

   /**
    * The Reader object, null if not used.
    */
   protected Reader m_reader;

   /**
    * A buffer for keeping the first 16KB.
    */
   protected ByteBuffer m_byteBuffer;

   /**
    * The InputStream object, null if not used.
    */
   protected InputStream m_inputStream;

   // constructors

   /**
    * Constructs the input.
    * @param input The Input to be wrapped.
    */
   public DebugInput(Input input)
   {
      m_input = input;
      m_nPrefixSize = s_nDefaultPrefixSize;
   }

   /**
    * Constructs the input with a non-default prefix size.
    * @param input The input to be wrapped.
    * @param nPrefixSize The prefix size.
    */
   public DebugInput(Input input, int nPrefixSize)
   {
      m_input = input;
      m_nPrefixSize = nPrefixSize;
   }

   // operations

   /**
    * @see nexj.core.integration.Input#getInputStream()
    */
   public InputStream getInputStream() throws IntegrationException
   {
      if (m_inputStream == null)
      {
         m_byteBuffer = ByteBuffer.allocate(m_nPrefixSize);
   
         m_inputStream = new FilterInputStream(m_input.getInputStream())
         {
            public int read(byte b[], int off, int len) throws IOException
            {
               int n = in.read(b, off, len);
               
               if (n > 0 && m_byteBuffer.position() < m_nPrefixSize)
               {
                  m_byteBuffer.put(b, off, Math.min(n, m_nPrefixSize - m_byteBuffer.position()));
               }
   
               return n;
            }
   
            public int read() throws IOException
            {
               int n = in.read();
   
               if (n > 0 && m_byteBuffer.position() < m_nPrefixSize)
               {
                  m_byteBuffer.put((byte)n);
               }
   
               return n;
            }

            public void close() throws IOException
            {
               while (read() > 0 && m_byteBuffer.position() < m_nPrefixSize);

               in.close();
            }
         };
      }

      return m_inputStream;
   }

   /**
    * @see nexj.core.integration.Input#getReader()
    */
   public Reader getReader() throws IntegrationException
   {
      if (m_reader == null)
      {
         m_strBuffer = new StringBuffer();
   
         m_reader = new FilterReader(m_input.getReader())
         {
            public int read(char[] cbuf, int off, int len) throws IOException
            {
               int n = in.read(cbuf, off, len);
   
               if (n > 0 && m_strBuffer.length() < m_nPrefixSize)
               {
                  m_strBuffer.append(cbuf, off, Math.min(n, m_nPrefixSize - m_strBuffer.length()));
               }
   
               return n;
            }
   
            public int read() throws IOException
            {
               int n = in.read();
   
               if (n > 0 && m_strBuffer.length() < m_nPrefixSize)
               {
                  m_strBuffer.append((char)n);
               }
   
               return n;
            }

            public void close() throws IOException
            {
               while (read() > 0 && m_strBuffer.length() < m_nPrefixSize);

               in.close();
            }
         };
      }

      return m_reader;
   }

   /**
    * @see nexj.core.integration.Input#getBinary()
    */
   public Binary getBinary() throws IntegrationException
   {
      return m_input.getBinary();
   }

   /**
    * @see nexj.core.integration.Input#getString()
    */
   public String getString() throws IntegrationException
   {
      return m_input.getString();
   }

   /**
    * @see nexj.core.integration.Input#getObject()
    */
   public Object getObject() throws IntegrationException
   {
      return m_input.getObject();
   }

   /**
    * @return The prefix to the Input.
    */
   public String getPrefix()
   {
      if (m_reader != null)
      {
         try
         {
            while (m_reader.read() > 0 && m_strBuffer.length() < m_nPrefixSize);
         }
         catch (IOException e)
         {
         }

         return m_strBuffer.toString();
      }
      else if (m_inputStream != null)
      {
         try
         {
            while (m_inputStream.read() > 0 && m_byteBuffer.position() < m_nPrefixSize);
         }
         catch (IOException e)
         {
         }

         byte[] bytes = new byte[m_byteBuffer.position()];
         String encoding = XMLUtil.ENCODING;

         m_byteBuffer.flip();
         m_byteBuffer.get(bytes);

         if (m_input instanceof ObjectInput)
         {
            encoding = ((ObjectInput)m_input).getEncoding();
         }
         else if (m_input instanceof StreamInput)
         {
            encoding = ((StreamInput)m_input).getEncoding();
         }

         try
         {
            return new String(bytes, encoding);
         }
         catch (UnsupportedEncodingException e)
         {
            return null;
         }
      }

      return null;
   }
}
