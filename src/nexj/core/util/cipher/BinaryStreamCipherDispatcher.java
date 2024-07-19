// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.cipher;

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Properties;

import nexj.core.util.Binary;
import nexj.core.util.BinaryUtil;
import nexj.core.util.ObjUtil;

/**
 * Provides read/write access to streams with a plain text cipher.
 */
public class BinaryStreamCipherDispatcher implements StreamCipher
{
   /**
    * The "identity" scheme: passes through the data untouched.
    */
   public final static String SCHEME_IDENTITY = "identity";

   /**
    * The "base64" scheme: encodes the data using base64.
    */
   public final static String SCHEME_BASE64 = "base64";

   /**
    * The "base64" scheme: encodes the data using hexadecimal notation.
    */
   public final static String SCHEME_HEX = "hex";

   /**
    * The "master" scheme: encrypts the data with a master password.
    */
   public final static String SCHEME_MASTER = "master";

   /**
    * The "master" scheme: encrypts the data with a master password.
    */
   public final static String CIPHER_SCHEME_PROPERTY = "cipher.scheme";

   /**
    * The SCHEME_IDENTITY scheme.
    */
   protected final static byte IDENTITY = 0;

   /**
    * The SCHEME_BASE64 scheme.
    */
   protected final static byte BASE64 = 1;

   /**
    * The SCHEME_HEX scheme.
    */
   protected final static byte HEX = 2;

   /**
    * The SCHEME_MASTER scheme.
    */
   protected final static byte MASTER = 3;

   // attributes

   /**
    * The scheme to use for encryption.
    */
   protected byte m_nScheme = MASTER; // set default

   // associations

   /**
    * The properties used to initialize this dispatcher instance.
    */
   protected Properties m_properties;


   // operations

   /**
    * @see nexj.core.util.cipher.StreamCipher#createDecryptedInputStream(java.io.InputStream)
    */
   public InputStream createDecryptedInputStream(InputStream istream)
   {
      try
      {
         return createStreamCipher((byte)istream.read()).createDecryptedInputStream(istream);
      }
      catch (IOException e)
      {
         throw ObjUtil.rethrow(e);
      }
   }

   /**
    * @see nexj.core.util.cipher.StreamCipher#createEncryptedOutputStream(java.io.OutputStream)
    */
   public OutputStream createEncryptedOutputStream(OutputStream ostream)
   {
      try
      {
         ostream.write(m_nScheme);
      }
      catch (IOException e)
      {
         ObjUtil.rethrow(e);
      }

      return createStreamCipher(m_nScheme).createEncryptedOutputStream(ostream);
   }

   /**
    * Create an initialized StreamCipher for the specific scheme.
    * @param nScheme The StreamCipher scheme type to create.
    * @return The requested StreamCipher or IDENTITY if unknown.
    */
   protected StreamCipher createStreamCipher(byte nScheme)
   {
      String sClass = null;

      switch (nScheme)
      {
         case BASE64:
            sClass = m_properties.getProperty("cipher.class." + SCHEME_BASE64,
                                              "nexj.core.util.cipher.Base64StreamCipher");
               break;
         case HEX:
            sClass = m_properties.getProperty("cipher.class." + SCHEME_HEX,
                                              "nexj.core.util.cipher.HexStreamCipher");
            break;
         case MASTER:
            sClass = m_properties.getProperty("cipher.class." + SCHEME_MASTER,
                                              "nexj.core.util.cipher.MasterPasswordStreamCipher");
            break;
      }

      StreamCipher cipher = null;

      if (sClass != null)
      {
         try
         {
            cipher = (StreamCipher)Class.forName(sClass).newInstance();
         }
         catch (Exception e)
         {
            // NOOP, leave cipher = null
         }
      }

      if (cipher == null) // Requirement: Unknown protocol number is treated as plain text.
      {
         cipher = new IdentityStreamCipher();
      }

      cipher.init(m_properties);

      return cipher;
   }

   /**
    * Convenience method that returns the decrypted argument.
    * @param value The data to decrypt. Can be null.
    * @return The decrypted data.
    */
   public Object decrypt(Binary value)
   {
      if (value == null)
      {
         return null;
      }

      DataInputStream stream =
         new DataInputStream(createDecryptedInputStream(value.getInputStream()));

      try
      {
         return BinaryUtil.read(stream);
      }
      catch (IOException e)
      {
         throw ObjUtil.rethrow(e);
      }
      finally
      {
         try
         {
            stream.close();
         }
         catch (IOException e)
         {
            // NOOP
         }
      }
   }

   /**
    * Convenience method that returns the encrypted argument.
    * @param value The data to encrypt. Can be null.
    * @return The encrypted data.
    */
   public Binary encrypt(Object value)
   {
      if (value == null)
      {
         return null;
      }

      ByteArrayOutputStream out = new ByteArrayOutputStream();
      DataOutputStream stream = new DataOutputStream(createEncryptedOutputStream(out));

      try
      {
         BinaryUtil.write(stream, value);
      }
      catch (IOException e)
      {
         ObjUtil.rethrow(e);
      }
      finally
      {
         try
         {
            stream.close(); // need to push all the data through to the byte[]
         }
         catch (IOException e)
         {
            // NOOP
         }
      }

      return new Binary(out.toByteArray());
   }

   /**
    * @see nexj.core.util.cipher.StreamCipher#init(java.util.Properties)
    */
   public void init(Properties properties)
   {
      String sScheme = properties.getProperty("cipher.scheme", SCHEME_MASTER);

      if (SCHEME_BASE64.equals(sScheme))
      {
         m_nScheme = BASE64;
      }
      else if (SCHEME_HEX.equals(sScheme))
      {
         m_nScheme = HEX;
      }
      else if (SCHEME_MASTER.equals(sScheme))
      {
         m_nScheme = MASTER;
      }
      else
      {
         m_nScheme = IDENTITY;
      }

      m_properties = properties;
   }
}