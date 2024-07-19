// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.sys;

import java.util.Properties;

import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.runtime.ActionContext;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.Binary;
import nexj.core.util.cipher.BinaryStreamCipherDispatcher;

/**
 * Cipher API manager.
 */
public class SysCipher implements InvocationContextAware
{
   // associations

   /**
    * The decryption/encryption cipher used for the current context (lazy init).
    */
   protected BinaryStreamCipherDispatcher m_dispatcher;

   /**
    * Decrypt data using Metadata's dataPassword
    * @param metaclass The metaclass whose action has been invoked.
    * @param data The data to decrypt.
    * @param context The context for the action that has been invoked.
    * @return Data decrypted using Metadata's dataPassword.
    */
   public Object decrypt(Metaclass metaclass, Binary data, ActionContext context)
   {
      return m_dispatcher.decrypt(data);
   }

   /**
    * Encrypt data using Metadata's dataPassword.
    * @param metaclass The metaclass whose action has been invoked.
    * @param data The data to encrypt.
    * @param context The context for the action that has been invoked.
    * @return Data encrypted using Metadata's dataPassword.
    */
   public Binary encrypt(Metaclass metaclass, Object data, ActionContext context)
   {
      return m_dispatcher.encrypt(data);
   }

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      Metadata metadata = context.getMetadata();
      Properties properties = new Properties();

      setProperty(properties,
                  BinaryStreamCipherDispatcher.CIPHER_SCHEME_PROPERTY,
                  BinaryStreamCipherDispatcher.SCHEME_MASTER);

      // @see MasterPasswordStreamCipher.MASTER_PASSWORD_PROPERTY
      setProperty(properties, "cipher.master.password", metadata.getDataPassword());

      if (m_dispatcher == null)
      {
         m_dispatcher = new BinaryStreamCipherDispatcher();
      }

      m_dispatcher.init(properties);
   }

   /**
    * Set a property if both the key and value are not null.
    * @param properties The container where to set the property.
    * @param sKey The property key to modify.
    * @param sValue The property value to set (null == remove property).
    * @return The previous value of the property.
    */
   protected static String setProperty(Properties properties, String sKey, String sValue)
   {
      if (sKey != null)
      {
         return (String)((sValue == null)
                         ? properties.remove(sKey) : properties.setProperty(sKey, sValue));
      }

      return null;
   }
}