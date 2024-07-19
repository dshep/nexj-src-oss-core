// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.cipher;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.Properties;

import nexj.core.util.cipher.StreamCipher;

/**
 * Performs no encryption/decryption of the stream data.
 */
public class IdentityStreamCipher implements StreamCipher
{
   // operations

   /**
    * @see nexj.core.util.cipher.StreamCipher#createDecryptedInputStream(java.io.InputStream)
    */
   public InputStream createDecryptedInputStream(InputStream istream)
   {
      return istream;
   }

   /**
    * @see nexj.core.util.cipher.StreamCipher#createEncryptedOutputStream(java.io.OutputStream)
    */
   public OutputStream createEncryptedOutputStream(OutputStream ostream)
   {
      return ostream;
   }

   /**
    * @see nexj.core.util.cipher.StreamCipher#init(java.util.Properties)
    */
   public void init(Properties properties)
   {
      //Do nothing.
   }
}
