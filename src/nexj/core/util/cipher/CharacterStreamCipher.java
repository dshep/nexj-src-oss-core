// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.cipher;

import java.io.IOException;
import java.io.Reader;
import java.util.Properties;
import java.io.Writer;

/**
 * Provides encryption and decryption of character streams.
 */
public interface CharacterStreamCipher
{
   /**
    * Creates a character stream writer to which plaintext data can be written,
    * causing encrypted data to be written to the argument.
    * 
    * @param output The writer to which the encrypted data should be written.
    * @return A Writer to which the plaintext data should be written.
    * @throws IOException If an output error occurs.
    */
   public Writer createEncryptedWriter(Writer output) throws IOException;

   /**
    * Creates a character stream reader from which plaintext data can be
    * read.
    * 
    * @param input The reader of the encrypted data.
    * @return A Reader from which the plaintext data can be read.
    * @throws IOException If an input error occurs.
    */
   public Reader createDecryptedReader(Reader input) throws IOException;

   /**
    * Initializes the cipher.
    * 
    * @param properties Cipher-specific properties that govern coding/decoding.
    */
   public void init(Properties properties);
}
