// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.auth;

import nexj.core.util.Logger;
import nexj.core.util.OS;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * Authenticator factory.
 */
public class AuthenticatorFactory
{
   // attributes
   
   /**
    * Flag indicating the use of a native authenticator.
    */
   private static boolean s_bNative;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(AuthenticatorFactory.class);

   static
   {
      try
      {
         if (OS.isWindows() &&
            StringUtil.parseBoolean(SysUtil.getConfigProperties().getProperty("auth.native.enabled", "true")))
         {
            new NativeAuthenticator().dispose();
            s_bNative = true;
         }
      }
      catch (NoClassDefFoundError e)
      {
         s_logger.info("Native authentication library wrapper not found", e); 
      }
      catch (UnsatisfiedLinkError e)
      {
         s_logger.info("Native authentication library not found", e); 
      }
      catch (Throwable e)
      {
         s_logger.info("Error loading the native authenticator", e);
      }
   }

   // constructors

   /**
    * Prevents construction.
    */
   protected AuthenticatorFactory()
   {
   }

   // operations

   /**
    * Creates a new authenticator.
    * 
    * @return A new authenticator implementation instance.
    */
   public static Authenticator create() 
   {
      if (s_bNative)
      {
         return new NativeAuthenticator();
      }
      else
      {
         try
         {
            Class clazz = Class.forName("nexj.core.util.auth.GSSAuthenticator");
            
            return (Authenticator)clazz.newInstance();
         }
         catch(Throwable t)
         {
            ObjUtil.rethrow(t);
            
            return null;
         }
      }
   }
}
