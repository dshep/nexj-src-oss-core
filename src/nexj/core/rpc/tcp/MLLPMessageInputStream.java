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
 * An MLLP input stream.
 */
public class MLLPMessageInputStream extends MessageInputStream
{
   // constants

   /**
    * The initial size of the buffer.
    * It will grow as needed.
    */
   protected final static int INITIAL_BUFFER_SIZE = IOUtil.BUFFER_SIZE;

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
    * The position in the buffer where the current message starts.
    */
   protected int m_nBufStartPos;

   /**
    * The position in the buffer that holds the last byte of the current message.
    */
   protected int m_nBufEndPos;

   /**
    * The position in the buffer just past the last byte read from the socket.
    */
   protected int m_nLastIndex;

   // associations

   /**
    * The MLLP message stream factory.
    */
   protected MLLPMessageStreamFactory m_factory;

   // constructors

   /**
    * @param istream The underlying input stream.
    * @param factory The message stream factory (caller).
    */
   public MLLPMessageInputStream(InputStream istream, MLLPMessageStreamFactory factory)
   {
      super(istream);

      m_factory = factory;

      // initialize buffer
      m_nBuffer = new byte[Math.min(INITIAL_BUFFER_SIZE, m_factory.getMaxReadBufferSize())];
   }

   // operations

   /**
    * @see nexj.core.integration.MessageInputStream#next(nexj.core.rpc.TransferObject)
    */
   public boolean next(TransferObject raw) throws IOException, TCPTimeoutException
   {
      // next byte will be read into m_nBuffer[nPos]
      int nPos = (m_nLastIndex == 0 ? 0 : m_nBufEndPos + 3);
      int n = 0;
      boolean bFoundEB = false;
      boolean bFoundSB = false;

      // resize buffer if necessary
      nPos = checkAndResizeBuffer(nPos, bFoundSB);

      for (;;)
      {
         if (nPos >= m_nLastIndex)
         {
            // read a chunk of data
            int nNewByteCount = readChunk(m_nBuffer, nPos, m_nBuffer.length - nPos);

            // end-of-file?
            if (nNewByteCount == -1)
            {
               if (bFoundSB)
               {
                  throw new RPCException("err.rpc.tcp.mllp.unexpectedEOF");
               }
               else
               {
                  return false;
               }
            }

            m_nLastIndex = nPos + nNewByteCount;
         }

         if (!bFoundSB)
         {
            for (; nPos < m_nLastIndex; ++nPos)
            {
               // read and discard until
               // we find the start byte
               n = m_nBuffer[nPos];

               // start byte?
               if (n == m_factory.getStartBlock())
               {
                  bFoundSB = true;

                  break;
               }
            }

            // resize buffer if necessary
            nPos = checkAndResizeBuffer(nPos, bFoundSB);

            if (!bFoundSB)
            {
               continue;
            }

            m_nBufStartPos = ++nPos;
         }

         // read until we get an end byte followed by message separator
         for (; nPos < m_nLastIndex; ++nPos)
         {
            n = m_nBuffer[nPos];

            // start byte?
            if (n == m_factory.getStartBlock())
            {
               // ignore all currently buffered bytes
               m_nBufStartPos = nPos + 1;

               continue;
            }

            // message separator?
            if (bFoundEB)
            {
               if (n == m_factory.getMessageSeparator())
               {
                  // We've read (and buffered) a complete message
                  m_nBufEndPos = nPos - 2;
                  m_nBufReadPos = m_nBufStartPos;

                  return true;
               }

               throw new RPCException("err.rpc.tcp.mllp.noCRAfterEB",
                  new Object[] { new Integer(m_factory.getMessageSeparator()), new Integer(n) });
            }

            // end byte?
            if (n == m_factory.getEndBlock())
            {
               bFoundEB = true;

               continue;
            }

            // invalid byte?
            if (!m_factory.isValidByte(n))
            {
               throw new RPCException("err.rpc.tcp.mllp.invalidByte",
                  new Object[] { new Integer(n), new Integer(m_factory.getMinByte()) });
            }

            // resize buffer if necessary
            nPos = checkAndResizeBuffer(nPos, bFoundSB);

            if (nPos == 0)
            {
               break;
            }
         }
      }
   }

   /**
    * The buffer will be re-sized or the existing bytes shifted to
    * the beginning if necessary.
    * @param nPos The current byte position in the buffer.
    * @param bFoundSB True iff we have found the start byte for the current message.
    * @return The new nPos value.
    */
   protected int checkAndResizeBuffer(int nPos, boolean bFoundSB)
   {
      // Do we need to increase the buffer size?
      // or shift array elements down?
      if (nPos >= m_nBuffer.length - 1)
      {
         if (!bFoundSB)
         {
            // we haven't found a start byte yet, so no need to increase
            // the buffer size, just reset it.
            m_nLastIndex = 0;
            m_nBufStartPos = 0;
            m_nBufEndPos = 0;
            nPos = 0;
         }
         else if (m_nBufStartPos >= m_nBuffer.length >> 1)
         {
            // shift bytes down
            System.arraycopy(m_nBuffer, m_nBufStartPos, m_nBuffer, 0, m_nLastIndex - m_nBufStartPos);
            nPos = nPos - m_nBufStartPos;
            m_nLastIndex = m_nLastIndex - m_nBufStartPos;
            m_nBufStartPos = 0;
         }
         else
         {
            // increase buffer size
            int nNewSize;

            // buffer length already greater than half the max?
            if (m_nBuffer.length > m_factory.getMaxReadBufferSize() >> 1)
            {
               // new size will be the maximum
               nNewSize = m_factory.getMaxReadBufferSize();
            }
            else
            {
               // double the current size
               nNewSize = m_nBuffer.length << 1;
            }

            if (nNewSize <= m_nBuffer.length)
            {
               // We can't continue, since we've reached the
               // maximum buffer size (according to the settings).
               throw new RPCException("err.rpc.tcp.mllp.noBufferSpace");
            }

            byte[] newBuffer = new byte[nNewSize];
            System.arraycopy(m_nBuffer, 0, newBuffer, 0, m_nBuffer.length);
            m_nBuffer = newBuffer;
         }
      }

      return nPos;
   }

   /**
    * @see java.io.InputStream#read()
    */
   public int read() throws IOException
   {
      assert m_nBufReadPos >= m_nBufStartPos;

      // Have we read everything?
      if (m_nBufReadPos > m_nBufEndPos)
      {
         return -1;
      }

      return (int)m_nBuffer[m_nBufReadPos++] & 0xFF;
   }

   /**
    * @see java.io.InputStream#read(byte[], int, int)
    */
   public int read(byte[] nArray, int nOffset, int nLength) throws IOException
   {
      assert m_nBufReadPos >= m_nBufStartPos;

      // Have we read everything?
      if (m_nBufReadPos > m_nBufEndPos)
      {
         return -1;
      }

      int nNumToCopy = Math.min(nLength, m_nBufEndPos - m_nBufReadPos + 1);

      System.arraycopy(m_nBuffer, m_nBufReadPos, nArray, nOffset, nNumToCopy);
      m_nBufReadPos += nNumToCopy;

      return nNumToCopy;
   }

   /**
    * Performs a read() call on the underlying input stream.
    * Catches SocketTimeoutException's and re-throws them as TCPTimeoutException's
    * after resetting the position variables.
    * @param nArray The destination buffer.
    * @param nOffset The start index into the destination buffer.
    * @param nLength The length of the destination buffer.
    * @return The number of bytes read into the buffer, or -1 if the end of the stream is reached.
    * @throws IOException, TCPTimeoutException
    */
   protected int readChunk(byte[] nArray, int nOffset, int nLength) throws IOException, TCPTimeoutException
   {
      try
      {
         return m_istream.read(nArray, nOffset, nLength);
      }
      catch (SocketTimeoutException e)
      {
         m_nLastIndex = 0;
         m_nBufStartPos = 0;
         m_nBufEndPos = 0;

         throw new TCPTimeoutException(e);
      }
   }
}
