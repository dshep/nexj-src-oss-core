// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.cipher;

import java.io.InputStream;
import java.util.Properties;
import java.io.OutputStream;

/**
 * Provides encryption and decryption of byte streams.
 */
public interface StreamCipher
{
   /**
    * Creates a byte stream to which plaintext data can be written, causing
    * encrypted data to be written to the argument.
    * 
    * @param ostream The stream on which the encrypted data should be written.
    * @return An OutputStream to which the plaintext data should be written.
    */
   public OutputStream createEncryptedOutputStream(OutputStream ostream);

   /**
    * Creates a byte stream from which plaintext data can be read.
    * 
    * @param istream The InputStream that connects to the encrypted data.
    * @return An InputStream from which the plaintext data can be read.
    */
   public InputStream createDecryptedInputStream(InputStream istream);

   /**
    * Initializes the cipher.
    * 
    * @param properties Cipher-specific properties that govern coding/decoding.
    */
   public void init(Properties properties);
}
