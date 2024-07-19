package nexj.core.monitoring.jmx;

import java.util.ArrayList;
import java.util.List;

import nexj.core.meta.Component;
import nexj.core.meta.Metadata;
import nexj.core.util.HashTab;
import nexj.core.util.Lifecycle;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup.Iterator;

/**
 * Statistics manager.
 */
public class StatManager implements Lifecycle
{
   // constants

   /**
    * Default persistent statistic class.
    */
   public final static String DEFAULT_PERSIST_CLASS = "SysStat";

   /**
    * Current node suffix.
    */
   protected final static String CURRENT_NODE_SUFFIX = " (current)/";

   // associations

   /**
    * MBean map.
    */
   private final Lookup m_mBeanMap = new HashTab();

   /**
    * Statistics initializers.
    */
   private List m_initializerList = new ArrayList(4); // of type StatInitializer

   /**
    * Context component.
    */
   protected Component m_contextComponent;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(StatManager.class);

   // operations

   /**
    * Adds a statistics initializer.
    * @param initializer Initializer.
    */
   public void addInitializer(StatManagerAware initializer)
   {
      m_initializerList.add(initializer);
   }

   /**
    * Set context component.
    * @param contextComponent Context component.
    */
   public void setContextComponent(Component contextComponent)
   {
      m_contextComponent = contextComponent;
   }

   /**
    * @return The root metadata object.
    */
   public Metadata getMetadata()
   {
      return m_contextComponent.getMetadata();
   }

   /**
    * Define a statistic.
    * @param sPath Path, e.g. "A/B/C".
    * @param sName Name.
    * @param sType Type.
    * @param bReadOnly Whether statistic is read only, i.e. cannot be modified from JMX client.
    * @param sPersistClass Persistent statistics class name.
    * @return Statistic.
    */
   public StatValue defineStatistic(String sPath, String sName, String sType,
      boolean bReadOnly, String sPersistClass)
   {
      return defineStatistic(sPath, sName, sType, bReadOnly, sPersistClass, null);
   }

   /**
    * Define a statistic.
    * @param sPath Path, e.g. "A/B/C".
    * @param sName Name.
    * @param sType Type.
    * @param bReadOnly Whether statistic is read only, i.e. cannot be modified from JMX client.
    * @param sPersistClass Persistent statistics class name.
    * @return Statistic.
    */
   public StatValue defineStatistic(String sPath, String sName, String sType, boolean bReadOnly,
         String sPersistClass, StatMBeanCommandConsumer commandConsumer)
   {
      boolean bDistributed = getMetadata().isDistributed();

      return defineStatistic(sPath, sName, sType, !bDistributed, !bDistributed, sPersistClass,
            DefaultStatMBeanOperations.getInstance(), bReadOnly, commandConsumer);
   }

   /**
    * Define a statistic.
    * @param sPath Path, e.g. "A/B/C".
    * @param sName Name.
    * @param sType Type.
    * @param bSnapshot Whether snapshot.
    * @param bIsolated Whether to prepend isolated node name.
    * @param sPersistClass Persistent statistics class name.
    * @param operations MBean operations.
    * @param bReadOnly Whether statistic is read only, i.e. cannot be modified from JMX client.
    * @param bPersistable Whether persisting statistic is allowed.
    * @param commandConsumer Command consumer.
    * @return Statistic.
    */
   public StatValue defineStatistic(String sPath, String sName, String sType, boolean bSnapshot,
         boolean bIsolated, String sPersistClass, StatMBean.Operations operations, boolean bReadOnly,
         StatMBeanCommandConsumer commandConsumer)
   {
      StatMBean bean = getMBean(sPath, bSnapshot, bIsolated, operations, commandConsumer);

      return bean.addStat(sName, sType, bReadOnly, sPersistClass);
   }

   /**
    * Gets an MBean.
    * @param sPath Path, e.g. "A/B/C".
    * @param bSnapshot Whether snapshot.
    * @param bIsolated Whether to prepend isolated node name.
    * @param operations MBean operations.
    * @param commandConsumer Command consumer.
    * @return MBean.
    */
   private StatMBean getMBean(String sPath, boolean bSnapshot, boolean bIsolated, StatMBean.Operations operations,
      StatMBeanCommandConsumer commandConsumer)
   {
      StatMBean bean;

      synchronized (m_mBeanMap)
      {
         bean = (StatMBean)m_mBeanMap.get(sPath);

         if (bean == null)
         {
            bean = new StatMBean(m_contextComponent.getMetadata().getEnvironment(), sPath, bSnapshot, bIsolated, commandConsumer);

            m_mBeanMap.put(sPath, bean);
         }

         bean.setOperations(operations);
      }

      return bean;
   }

   /**
    * Adds a statistic.
    * @param sPath Path, e.g. "A/B/C".
    * @param sName Name.
    * @param statValue Statistic.
    * @param sPersistClass Persistent statistics class name.
    * @param bReadOnly Whether statistic is read only, i.e. cannot be modified from JMX client.
    */
   public void addStatistic(String sPath, String sName, StatValue statValue, String sPersistClass, boolean bReadOnly)
   {
      boolean bDistributed = getMetadata().isDistributed();
      StatMBean bean = getMBean(sPath, !bDistributed, !bDistributed, DefaultStatMBeanOperations.getInstance(), null);

      bean.addStat(sName, statValue, bReadOnly, sPersistClass);
   }

   /**
    * Undefine statistics.
    * @param sPath Path, e.g. "A/B/C".
    * @return Whether statistics existed.
    */
   public boolean undefine(String sPath)
   {
      if (sPath == null)
      {
         return false;
      }

      StatMBean bean;

      synchronized (m_mBeanMap)
      {
         bean = (StatMBean)m_mBeanMap.remove(sPath);

         if (bean == null)
         {
            return false;
         }
      }

      bean.clear();

      return true;
   }

   /**
    * Undefine all statistics starting with the given prefix.
    * @param sPrefix MBean path prefix.
    */
   public void undefineAll(String sPrefix)
   {
      forEachMBean(new MBeanHandler()
      {
         public void handle(StatMBean mBean)
         {
            ((StatMBean)m_mBeanMap.remove(mBean.getPath())).clear();
         }
      }, sPrefix);
   }

   /**
    * Check whether statistics is defined.
    * @param sPath Path, e.g. "A/B/C".
    * @return Whether statistics is defined.
    */
   public boolean isDefined(String sPath)
   {
      if (sPath == null)
      {
         return false;
      }

      synchronized (m_mBeanMap)
      {
         return m_mBeanMap.contains(sPath);
      }
   }

   /**
    * Find statistic.
    * @param sPath Path, e.g. "A/B/C".
    * @param sName Name.
    * @return Statistic or null.
    */
   public StatValue find(String sPath, String sName)
   {
      if (sPath == null)
      {
         return null;
      }

      StatMBean bean = null;

      synchronized (m_mBeanMap)
      {
         bean = (StatMBean)m_mBeanMap.get(sPath);
      }

      if (bean == null)
      {
         return null;
      }

      return bean.findStat(sName);
   }

   /**
    * Find MBean.
    * @param sPath Prefixed path, e.g. "Prefix/A/B/C".
    * @return MBean or null.
    */
   public StatMBean find(String sPath)
   {
      if (sPath == null)
      {
         return null;
      }

      synchronized (m_mBeanMap)
      {
         return (StatMBean)m_mBeanMap.get(sPath);
      }
   }

   /**
    * Add MBean.
    * @param mBean MBean to add.
    */
   public void add(StatMBean mBean)
   {
      synchronized (m_mBeanMap)
      {
         m_mBeanMap.put(mBean.getPath(), mBean);
      }
   }

   /**
    * Get the list with statistics.
    * @param statisticList Statistics list to fill.
    * @return Statistic list.
    */
   public List getStatistics()
   {
      final List statisticList = new ArrayList();

      forEachMBean(new MBeanHandler()
      {
         public void handle(StatMBean mBean)
         {
            if (!mBean.isSnapshot())
            {
               statisticList.add(mBean.clone());
            }
         }
      });

      return statisticList;
   }

   /**
    * Invokes the handler for every defined MBean with the given path prefix.
    * @param handler The handler to invoke.
    * @param sPrefix MBean path prefix to filter with. Can be null.
    */
   public void forEachMBean(MBeanHandler handler, String sPrefix)
   {
      if (sPrefix == null)
      {
         sPrefix = "";
      }

      synchronized (m_mBeanMap)
      {
         for (Iterator itr = m_mBeanMap.valueIterator(); itr.hasNext();)
         {
            StatMBean mBean = (StatMBean)itr.next();

            if (mBean.getPath().startsWith(sPrefix))
            {
               handler.handle(mBean);
            }
         }
      }
   }

   /**
    * Invokes the handler for every defined source (i.e. not snapshot) MBean with the given path prefix.
    * @param handler The handler to invoke.
    * @param sPrefix MBean path prefix to filter with. Can be null.
    */
   public void forEachSourceMBean(MBeanHandler handler, String sPrefix)
   {
      if (sPrefix == null)
      {
         sPrefix = "";
      }

      synchronized (m_mBeanMap)
      {
         for (Iterator itr = m_mBeanMap.valueIterator(); itr.hasNext();)
         {
            StatMBean mBean = (StatMBean)itr.next();

            if (!mBean.isSnapshot() && mBean.getPath().startsWith(sPrefix))
            {
               handler.handle(mBean);
            }
         }
      }
   }

   /**
    * Invokes the handler for every defined MBean.
    * @param handler The handler to invoke.
    */
   public void forEachMBean(MBeanHandler handler)
   {
      forEachMBean(handler, "");
   }

   /**
    * Invokes the handler for every defined source (i.e. not snapshot) MBean.
    * @param handler The handler to invoke.
    */
   public void forEachSourceMBean(MBeanHandler handler)
   {
      forEachSourceMBean(handler, "");
   }

   /**
    * Get node statistics prefix.
    * @param sNodeName Node name.
    * @param bLocal Whether local node.
    * @return Statistics prefix.
    */
   public static String getPrefix(String sNodeName, boolean bLocal)
   {
      if (sNodeName == null)
      {
         return "";
      }

      return sNodeName + ((bLocal) ? CURRENT_NODE_SUFFIX : "/");
   }

   // inner classes

   /**
    * Handles MBeans.
    */
   public interface MBeanHandler
   {
      /**
       * Handle an MBean.
       * @param mBean The MBean to handle.
       */
      public void handle(StatMBean mBean);
   }

   /**
    * @see nexj.core.util.Suspendable#suspend()
    */
   public void suspend() throws Exception
   {
   }

   /**
    * @see nexj.core.util.Suspendable#resume()
    */
   public void resume() throws Exception
   {
   }

   /**
    * @see nexj.core.util.Lifecycle#startup()
    */
   public void startup() throws Exception
   {
      for (int i = 0, nCount = m_initializerList.size(); i < nCount; ++i)
      {
         ((StatManagerAware)m_initializerList.get(i)).setStatManager(this);
      }
   }

   /**
    * @see nexj.core.util.Lifecycle#shutdown()
    */
   public void shutdown()
   {
      undefineAll(null);
   }
}
