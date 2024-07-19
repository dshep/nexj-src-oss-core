// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp;

import java.io.InputStream;
import java.io.OutputStream;

import nexj.core.integration.MessageInputStream;
import nexj.core.integration.MessageOutputStream;
import nexj.core.integration.MessageStreamFactory;

/**
 * Factory to create MLLP message input and output streams.
 * Also, keeps track of significant byte values (start block, end block, etc.)
 */
public class MLLPMessageStreamFactory implements MessageStreamFactory
{
   // attributes

   /**
    * The MLLP start byte value, which marks the
    * beginning of a new message.
    */
   protected int m_nStartBlock = 11;

   /**
    * The MLLP end byte value, which marks the
    * end of a message.
    */
   protected int m_nEndBlock = 28;

   /**
    * The MLLP message separator byte
    * (typically equal to the ASCII carriage return value).
    */
   protected int m_nMessageSeparator = 13;

   /**
    * The minimum valid byte value that can exist
    * in an MLLP message (except for a message separator).
    */
   protected int m_nMinByte = 31;
   
   /**
    * The byte value representing an acknowledgment.
    */
   protected int m_nACKByte = 6;
   
   /**
    * The byte value representing a negative acknowledgment.
    */
   protected int m_nNAKByte = 21;

   /**
    * The maximum size of the input buffer in
    * MLLPMessageInputStream.
    */
   protected int m_nMaxReadBufSize = 1 << 20;   // ~ 10MB

   /**
    * The MLLP version number.
    * Currently only 1 or 2 are valid.
    */
   protected int m_nVersion = 1;

   // operations

   /**
    * @param nStartBlock The byte value signifying the start of a block, e.g. 11.
    */
   public void setStartBlock(int nStartBlock)
   {
      m_nStartBlock = nStartBlock;
   }

   /**
    * @return The byte value signifying the start of a block.
    */
   public int getStartBlock()
   {
      return m_nStartBlock;
   }

   /**
    * @param nEndBlock The byte value signifying the end of a block, e.g. 28.
    */
   public void setEndBlock(int nEndBlock)
   {
      m_nEndBlock = nEndBlock;
   }

   /**
    * @return The byte value signifying the end of a block.
    */
   public int getEndBlock()
   {
      return m_nEndBlock;
   }

   /**
    * @param nMessageSeparator The message separator byte value, e.g. 13.
    */
   public void setMessageSeparator(int nMessageSeparator)
   {
      m_nMessageSeparator = nMessageSeparator;
   }

   /**
    * @return The message separator byte value.
    */
   public int getMessageSeparator()
   {
      return m_nMessageSeparator;
   }

   /**
    * @param nMinByte The minimum byte value that is valid in the stream, e.g. 31.
    */
   public void setMinByte(int nMinByte)
   {
      m_nMinByte = nMinByte;
   }

   /**
    * @return The minimum byte value that is valid in the stream.
    */
   public int getMinByte()
   {
      return m_nMinByte;
   }
   
   /**
    * @param nACKByte The byte value representing an acknowledgment, e.g. 6.
    */
   public void setAckByte(int nACKByte)
   {
      m_nACKByte = nACKByte;
   }

   /**
    * @return The byte value representing an acknowledgment.
    */
   public int getAckByte()
   {
      return m_nACKByte;
   }
   
   /**
    * @param nNAKByte The byte value representing a negative acknowledgment, e.g. 21.
    */
   public void setNakByte(int nNAKByte)
   {
      m_nNAKByte = nNAKByte;
   }

   /**
    * @return The byte value representing a negative acknowledgment.
    */
   public int getNakByte()
   {
      return m_nNAKByte;
   }

   /**
    * @param nMaxBuf The maximum size of the input buffer in MLLPMessageInputStream.
    */
   public void setMaxReadBufferSize(int nMaxBuf)
   {
      if (nMaxBuf <= 0)
      {
         return;
      }

      m_nMaxReadBufSize = nMaxBuf;
   }

   /**
    * @return The maximum size of the input buffer in MLLPMessageInputStream.
    */
   public int getMaxReadBufferSize()
   {
      return m_nMaxReadBufSize;
   }

   /**
    * @param nVersion The MLLP version number. Only 1 or 2 are valid.
    */
   public void setVersion(int nVersion)
   {
      if (nVersion < 1 || nVersion > 2)
      {
         throw new IllegalArgumentException("Invalid MLLP version: "
            + nVersion + ". Expected 1 or 2.");
      }
      
      m_nVersion = nVersion;
   }

   /**
    * @return The MLLP version number. Currently only 1 or 2 are valid.
    */
   public int getVersion()
   {
      return m_nVersion;
   }

   /**
    * @see nexj.core.integration.MessageStreamFactory#createMessageInputStream(java.io.InputStream)
    */
   public MessageInputStream createMessageInputStream(InputStream istream)
   {
      return new MLLPMessageInputStream(istream, this);
   }

   /**
    * @see nexj.core.integration.MessageStreamFactory#createMessageOutputStream(java.io.OutputStream)
    */
   public MessageOutputStream createMessageOutputStream(OutputStream ostream)
   {
      return new MLLPMessageOutputStream(ostream, this);
   }

   /**
    * @param n A byte value to test for validity
    * @return True iff n is greater than the minimum byte and not equal to a message separator
    */
   public boolean isValidByte(int n)
   {
      if (n > m_nMinByte || n == m_nMessageSeparator)
      {
         return true;
      }

      if (m_nVersion == 2 && (n == m_nACKByte || n == m_nNAKByte))
      {
         return true;
      }

      return false;
   }
}
