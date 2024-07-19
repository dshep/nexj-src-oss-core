// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.security.KeyStore;

/**
 * A sub-class to simply store the base64 encoding.
 */
public class EncryptedKeyStore extends KeyStore
{
   // attributes

   /**
    * The base64 encoding for the KeyStore.
    */
   protected String m_sText;

   // constructors

   /**
    * Construct an EncryptedKeyStore object.
    */
   public EncryptedKeyStore(String sText)
   {
      super(null, null, null);

      m_sText = sText;
   }

   // operations

   /**
    * @return The base64 encoding for the KeyStore.
    */
   public String getText()
   {
      return m_sText;
   }
}
