package nexj.core.rpc.tcp;

import java.io.IOException;
import java.io.InputStream;
import java.net.SocketTimeoutException;

import nexj.core.integration.MessageInputStream;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.tcp.ra.TCPTimeoutException;
import nexj.core.util.IOUtil;

/**
 * An ATNA input stream.
 */
public class ATNAMessageInputStream extends MessageInputStream
{
   // attributes

   /**
    * Array to buffer the message body.
    */
   protected byte[] m_nBuffer;

   /**
    * The current buffer position.
    * While reading: The next byte returned will be m_nBuffer[m_nBufReadPos].
    */
   protected int m_nBufReadPos;

   /**
    * The position in the buffer just past the last byte read from the socket.
    */
   protected int m_nLastIndex;

   /**
    * The number of bytes remaining to be returned for the current message.
    */
   protected int m_nBytesRemaining;

   // constructors

   /**
    * Construct an ATNAMessageInputStream.
    * @param istream The underlying input stream.
    */
   public ATNAMessageInputStream(InputStream istream)
   {
      super(istream);

      // Initialize buffer
      m_nBuffer = new byte[IOUtil.BUFFER_SIZE];
   }

   // operations

   /**
    * @see nexj.core.integration.MessageInputStream#next(nexj.core.rpc.TransferObject)
    */
   public boolean next(TransferObject raw) throws IOException
   {
      int n;
      StringBuffer sBuf = new StringBuffer();

      // Ignore remainder of previously obtained message, if applicable.
      for (; m_nBytesRemaining > 0; m_nBytesRemaining -= m_nLastIndex)
      {
         if (!nextBuffer())
         {
            return false;
         }

         m_nBufReadPos = m_nLastIndex;
      }

      assert m_nBytesRemaining == 0;

      // Read the message size integer.
      // Unexpected bytes trigger an exception.
      for (;;)
      {
         try
         {
            n = m_istream.read();
         }
         catch (SocketTimeoutException e)
         {
            m_nLastIndex = 0;

            throw new TCPTimeoutException(e);
         }

         if (n == -1)
         {
            return false;
         }

         // A string of digits (0-9), with first digit non-zero.
         // Maximum length: 9
         if (n >= (sBuf.length() == 0 ? 0x31 : 0x30) && n <= 0x39 && sBuf.length() <= 9)
         {
            sBuf.append((char)n);
         }
         else if (sBuf.length() > 0)
         {
            if (n != 0x20)
            {
               throw new RPCException("err.rpc.tcp.atna.invalidByteAfterMsgLength",
                  new Object[] { new Integer(0x20), new Integer(n) });
            }

            break;
         }
         else
         {
            throw new RPCException("err.rpc.tcp.atna.invalidMessageSize");
         }
      }

      m_nBytesRemaining = Integer.parseInt(sBuf.toString());

      return true;
   }

   /**
    * Read more data from the underlying InputStream if necessary.
    * @return False if we've reached the end of the stream, True otherwise.
    * @throws IOException If an I/O error occurs.
    * @throws TCPTimeoutException If a TCP timeout occurs.
    */
   protected boolean nextBuffer() throws TCPTimeoutException, IOException
   {
      try
      {
         if (m_nBufReadPos >= m_nLastIndex)
         {
            int nNewByteCount = m_istream.read(m_nBuffer, 0, Math.min(m_nBuffer.length, Math.max(1, m_nBytesRemaining)));

            if (nNewByteCount == -1)
            {
               return false;
            }

            m_nLastIndex = nNewByteCount;
            m_nBufReadPos = 0;
         }

         return true;
      }
      catch (SocketTimeoutException e)
      {
         m_nLastIndex = 0;

         throw new TCPTimeoutException(e);
      }
   }

   /**
    * @see java.io.InputStream#read()
    */
   public int read() throws IOException
   {
      // Have we read everything?
      if (m_nBytesRemaining < 1)
      {
         return -1;
      }

      if (nextBuffer())
      {
         --m_nBytesRemaining;

         return (int)m_nBuffer[m_nBufReadPos++] & 0xFF;
      }

      return -1;
   }

   /**
    * @see java.io.InputStream#read(byte[], int, int)
    */
   public int read(byte[] nArray, int nOffset, int nLength) throws IOException
   {
      // Have we read everything?
      if (m_nBytesRemaining < 1)
      {
         return -1;
      }

      if (nextBuffer())
      {
         int nNumToCopy = Math.min(nLength, Math.min(m_nBytesRemaining, m_nLastIndex - m_nBufReadPos));

         System.arraycopy(m_nBuffer, m_nBufReadPos, nArray, nOffset, nNumToCopy);
         m_nBufReadPos += nNumToCopy;
         m_nBytesRemaining -= nNumToCopy;

         return nNumToCopy;
      }

      return -1;
   }
}
