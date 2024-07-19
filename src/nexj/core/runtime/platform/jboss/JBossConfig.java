// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.platform.jboss;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Observable;
import java.util.Observer;

import nexj.core.runtime.platform.PlatformConfig;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * Platform configuration values when running under JBoss.
 */
public class JBossConfig extends PlatformConfig implements Observer
{
   // associations

   /**
    * ContainerInfo.getValue(String) method.
    */
   protected Method m_getValue;

   /**
    * ContainerInfo.getInstance() method.
    */
   protected Method m_getInstance;
   
   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JBossConfig.class);

   // constructors

   /**
    * Creates a new platform configuration instance for JBoss.
    * The data directory for JBoss is ${jboss.server.data.dir}\nexj
    */
   public JBossConfig()
   {
      assert J2EEUtil.CONTAINER == J2EEUtil.JBOSS;

      String sServerDataDir = System.getProperty("jboss.server.data.dir");

      if (!StringUtil.isEmpty(sServerDataDir))
      {
         m_dataDirectory = new File(sServerDataDir, SysUtil.NAMESPACE);
      }

      if (SysUtil.ENTERPRISE)
      {
         try
         {
            Class containerInfoClass = Class.forName(SysUtil.PACKAGE + ".core.container.platform.ContainerInfo");
   
            m_getValue = containerInfoClass.getDeclaredMethod("getValue", new Class[]{String.class});
            m_getInstance = containerInfoClass.getDeclaredMethod("getInstance", null);
            m_sHTTPNode = (String)m_getValue.invoke(m_getInstance.invoke(null, null), new Object[]{"httpNode"});
         }
         catch (Throwable t)
         {
            if (t instanceof InvocationTargetException)
            {
               t = ((InvocationTargetException)t).getTargetException();
            }
   
            s_logger.error("Unable to determine the HTTP node name. Verify that " +
               SysUtil.NAMESPACE + "-jboss.jar has been installed and is up to date.", t);
         }
      }
   }

   // operations

   /**
    * @see nexj.core.runtime.platform.PlatformConfig#addObserver(java.util.Observer)
    */
   public synchronized void addObserver(Observer observer)
   {
      super.addObserver(observer);

      if (countObservers() == 1 && m_getInstance != null)
      {
         try
         {
            ((Observable)m_getInstance.invoke(null, null)).addObserver(this);
         }
         catch (Throwable t)
         {
            if (t instanceof InvocationTargetException)
            {
               t = ((InvocationTargetException)t).getTargetException();
            }

            s_logger.error("Unexpected exception", t);
         }
      }
   }

   /**
    * @see java.util.Observable#deleteObserver(java.util.Observer)
    */
   public synchronized void deleteObserver(Observer o)
   {
      super.deleteObserver(o);
      cleanup();
   }

   /**
    * @see java.util.Observable#deleteObservers()
    */
   public synchronized void deleteObservers()
   {
      super.deleteObservers();
      cleanup();
   }

   /**
    * Prevent memory leakage by removing this instance from the Observable.
    * 
    * This is needed because the Observable is an instance of ContainerInfo, which is loaded in the server
    * class loader (it is in nexj-jboss.jar) and therefore persists across deployments.
    */
   protected void cleanup()
   {
      if (countObservers() == 0 && m_getInstance != null)
      {
         try
         {
            ((Observable)m_getInstance.invoke(null, null)).deleteObserver(this);
         }
         catch (Throwable t)
         {
            if (t instanceof InvocationTargetException)
            {
               t = ((InvocationTargetException)t).getTargetException();
            }

            s_logger.error("Unexpected exception", t);
         }
      }
   }

   /**
    * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
    */
   public void update(Observable info, Object property)
   {
      if ((property == null || property.equals("httpNode")) && m_getValue != null)
      {
         synchronized (this)
         {
            try
            {
               m_sHTTPNode = (String)m_getValue.invoke(info, new Object[]{"httpNode"});
               notifyObservers(property);
            }
            catch (Throwable t)
            {
               if (t instanceof InvocationTargetException)
               {
                  t = ((InvocationTargetException)t).getTargetException();
               }

               s_logger.error("Unexpected exception", t);
            }
         }
      }
   }
}
