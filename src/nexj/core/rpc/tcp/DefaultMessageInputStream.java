// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp;

import java.io.IOException;
import java.io.InputStream;

import nexj.core.integration.MessageInputStream;
import nexj.core.rpc.TransferObject;

/**
 * A default message input stream that reads all bytes until
 * the underlying stream is closed.
 * Only one message will result.
 * Only the first call to next() will return true.
 */
public class DefaultMessageInputStream extends MessageInputStream
{
   // attributes

   /**
    * True iff next() has been called.
    */
   protected boolean m_bNextCalled;

   // constructors

   /**
    * @param istream The base input stream.
    */
   public DefaultMessageInputStream(InputStream istream)
   {
      super(istream);
   }

   // operations

   /**
    * @see nexj.core.integration.MessageInputStream#next(nexj.core.rpc.TransferObject)
    */
   public boolean next(TransferObject raw) throws IOException
   {
      if (m_bNextCalled)
      {
         while (read() != -1)
         {
         }

         return false;
      }

      m_bNextCalled = true;

      return true;
   }

   /**
    * @see java.io.InputStream#read()
    */
   public int read() throws IOException
   {
      try
      {
         return m_istream.read();
      }
      catch (IOException e)
      {
         return -1;
      }
   }
}
