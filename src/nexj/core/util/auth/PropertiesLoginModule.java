// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.auth;

import java.io.IOException;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;
import java.util.Properties;

import javax.security.auth.Subject;
import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.UnsupportedCallbackException;
import javax.security.auth.login.LoginException;
import javax.security.auth.spi.LoginModule;

import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;
import nexj.core.util.SysUtil;

/**
 * A login module that posts its options to a callback handler.
 */
public class PropertiesLoginModule implements LoginModule
{
   // associations

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(PropertiesLoginModule.class);

   // operations

   /**
    * @see javax.security.auth.spi.LoginModule#abort()
    */
   public boolean abort() throws LoginException
   {
      return false;
   }

   /**
    * @see javax.security.auth.spi.LoginModule#commit()
    */
   public boolean commit() throws LoginException
   {
      return false;
   }

   /**
    * @see javax.security.auth.spi.LoginModule#initialize(javax.security.auth.Subject, javax.security.auth.callback.CallbackHandler, java.util.Map, java.util.Map)
    */
   public void initialize(Subject subject, CallbackHandler handler, Map sharedState, Map options)
   {
      PropertiesCallback callback = new PropertiesCallback();
      Properties properties = new Properties();

      properties.putAll(options);
      callback.setProperties(properties);

      try
      {
         handler.handle(new Callback[] {callback});
      }
      catch (IOException ex)
      {
         ObjUtil.rethrow(ex);
      }
      catch (UnsupportedCallbackException ex)
      {
         ObjUtil.rethrow(ex);
      }
   }

   /**
    * @see javax.security.auth.spi.LoginModule#login()
    */
   public boolean login() throws LoginException
   {
      return false;
   }

   /**
    * @see javax.security.auth.spi.LoginModule#logout()
    */
   public boolean logout() throws LoginException
   {
      return false;
   }

   // inner classes

   /**
    * A callback with properties to transfer from the login module
    * to the handler.
    */
   public static class PropertiesCallback implements Callback, Observer
   {
      // associations

      /**
       * The callback properties.
       */
      protected Properties m_properties;

      // operations

      /**
       * Sets the callback properties.
       * 
       * @param properties The properties to set.
       */
      public void setProperties(Properties properties)
      {
         m_properties = properties;
      }

      /**
       * Gets the callback properties.
       * 
       * @return The callback properties.
       */
      public Properties getProperties()
      {
         return m_properties;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return "PropertiesCallback(" + m_properties + ")";
      }

      /**
       * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
       */
      public void update(Observable o, Object arg)
      {
         if (arg instanceof Properties && m_properties != null)
         {
            ((Properties)arg).putAll(m_properties);
         }
      }
   }

   /**
    * A handler that collects the properties.
    */
   public static class PropertiesCallbackHandler implements CallbackHandler
   {
      // constants

      /**
       * The name of the properties callback class.
       */
      protected final static String CALLBACK_CLASS = PropertiesCallback.class.getCanonicalName();

      // associations

      /**
       * The properties collected by this handler.
       */
      protected Properties m_properties = new Properties(SysUtil.getConfigProperties());

      /**
       * Parent handler to which unrecognized callbacks are delegated. 
       */
      protected CallbackHandler m_parent;

      // constructors

      /**
       * Creates a new properties callback handler.
       */
      public PropertiesCallbackHandler()
      {
      }

      /**
       * Creates a new properties callback handler than delegates unrecognized
       * callbacks to a parent handler.
       * 
       * @param parent The parent handler to which unrecognized callbacks will
       * be delegated.
       */
      public PropertiesCallbackHandler(CallbackHandler parent)
      {
         m_parent = parent;
      }

      // operations

      /**
       * @see javax.security.auth.callback.CallbackHandler#handle(javax.security.auth.callback.Callback[])
       */
      public void handle(Callback[] callbacks) throws IOException, UnsupportedCallbackException
      {
         for (int i = 0; i < callbacks.length; i++)
         {
            Callback cb = callbacks[i];

            if (cb.getClass().getCanonicalName().equals(CALLBACK_CLASS))
            {
               ((Observer)cb).update(null, m_properties);
            }
         }

         if (m_parent != null)
         {
            m_parent.handle(callbacks);
         }
      }

      /**
       * Gets the properties collected by this handler.
       * 
       * @return The properties; null if no properties.
       */
      public Properties getProperties()
      {
         return m_properties;
      }
   }
}
