package nexj.core.runtime.sys;

import nexj.core.meta.Accessor;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.TypeMismatchException;
import nexj.core.monitoring.jmx.StatManager;
import nexj.core.monitoring.Statistic;
import nexj.core.monitoring.jmx.SavedStatValue;
import nexj.core.monitoring.jmx.StatMBean;
import nexj.core.monitoring.jmx.StatUtil;
import nexj.core.monitoring.jmx.StatMBean.Attr;
import nexj.core.monitoring.jmx.StatMBean.AttrHandler;
import nexj.core.monitoring.jmx.StatValue;
import nexj.core.rpc.cluster.NodeManager;
import nexj.core.runtime.ActionContext;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.scripting.Symbol;

/**
 * MBean manager.
 */
public class SysStat implements InvocationContextAware
{
   // constants

   /**
    * Default persistent statistic class.
    */
   public final static String DEFAULT_PERSIST_CLASS = "SysStat";

   // associations

   /**
    * Invocation context.
    */
   private InvocationContext m_invocationContext;

   // operations

   /**
    * Define a statistic.
    * @param reader Reader.
    * @param sPath Path, e.g. "A/B/C".
    * @param name Name.
    * @param type Type.
    * @param bReadOnly Whether statistic is read only, i.e. cannot be modified from JMX client.
    * @param actx Action context.
    * @return Statistic.
    */
   public Statistic define(Accessor reader, String sPath, Object name, Object type, boolean bReadOnly,
         ActionContext actx)
   {
      if (!(name instanceof String) && !(name instanceof Symbol))
      {
         throw new TypeMismatchException(sPath);
      }

      if (!(type instanceof String) && !(type instanceof Symbol))
      {
         throw new TypeMismatchException(sPath);
      }

      StatManager statManager = StatUtil.findStatManager(m_invocationContext);

      if (statManager == null)
      {
         return null;
      }

      return statManager.defineStatistic(sPath, name.toString(), type.toString(), bReadOnly,
            DEFAULT_PERSIST_CLASS).getStatistic();
   }

   /**
    * Define a statistic.
    * @param reader Reader.
    * @param sPath Path, e.g. "A/B/C".
    * @param name Name.
    * @param type Type.
    * @param bReadOnly Whether statistic is read only, i.e. cannot be modified from JMX client.
    * @param sPersistClass Persistent statistics class name.
    * @param actx Action context.
    * @return Statistic.
    */
   public Statistic define(Accessor reader, String sPath, Object name, Object type, boolean bReadOnly,
         String sPersistClass, ActionContext actx)
   {
      if (!(name instanceof String) && !(name instanceof Symbol))
      {
         throw new TypeMismatchException(sPath);
      }

      if (!(type instanceof String) && !(type instanceof Symbol))
      {
         throw new TypeMismatchException(sPath);
      }

      StatManager statManager = StatUtil.findStatManager(m_invocationContext);

      if (statManager == null)
      {
         return null;
      }

      return statManager.defineStatistic(sPath, name.toString(), type.toString(), bReadOnly,
            sPersistClass).getStatistic();
   }

   /**
    * Define a statistic.
    * @param reader Reader.
    * @param sPath Path, e.g. "A/B/C".
    * @param name Name.
    * @param statValue Statistic. 
    * @param bReadOnly Whether statistic is read only, i.e. cannot be modified from JMX client.
    * @param actx Action context.
    */
   public void addStat(Accessor reader, String sPath, Object name, StatValue statValue, boolean bReadOnly,
      ActionContext actx)
   {
      if (!(name instanceof String) && !(name instanceof Symbol))
      {
         throw new TypeMismatchException(sPath);
      }

      StatManager statManager = StatUtil.findStatManager(m_invocationContext);

      if (statManager == null)
      {
         return;
      }

      statManager.addStatistic(sPath, name.toString(), statValue, DEFAULT_PERSIST_CLASS, bReadOnly);
   }

   /**
    * Undefine statistics.
    * @param reader Reader.
    * @param sPath Path, e.g. "A/B/C".
    * @param actx Action context.
    * @return Whether statistics existed.
    */
   public boolean undefine(Accessor reader, String sPath, ActionContext actx)
   {
      StatManager statManager = StatUtil.findStatManager(m_invocationContext);

      if (statManager == null)
      {
         return false;
      }

      return statManager.undefine(sPath);
   }

   /**
    * Check whether statistics is defined.
    * @param reader Reader.
    * @param sPath Path, e.g. "A/B/C".
    * @param actx Action context.
    * @return Whether statistics is defined.
    */
   public boolean isDefined(Accessor reader, String sPath, ActionContext actx)
   {
      StatManager statManager = StatUtil.findStatManager(m_invocationContext);

      if (statManager == null)
      {
         return false;
      }

      return statManager.isDefined(sPath);
   }

   /**
    * Find statistic.
    * @param reader Reader.
    * @param sPath Path, e.g. "A/B/C".
    * @param name Name.
    * @param actx Action context.
    * @return Statistic.
    */
   public Statistic find(Accessor reader, String sPath, Object name, ActionContext actx)
   {
      StatValue statValue = findValue(reader, sPath, name, actx);

      if (statValue == null)
      {
         return null;
      }

      return statValue.getStatistic();
   }

   /**
    * Find statistic value.
    * @param reader Reader.
    * @param sPath Path, e.g. "A/B/C".
    * @param name Name.
    * @param actx Action context.
    * @return Statistic value.
    */
   public StatValue findValue(Accessor reader, String sPath, Object name, ActionContext actx)
   {
      if (!(name instanceof String) && !(name instanceof Symbol))
      {
         throw new TypeMismatchException(sPath);
      }

      StatManager statManager = StatUtil.findStatManager(m_invocationContext);

      if (statManager == null)
      {
         return null;
      }

      return statManager.find(sPath, name.toString());
   }

   /**
    * Save statistics of the current node to obtain more recent statistics.
    * @param reader The command object.
    * @param actx action context.
    */
   public void save(Accessor reader, ActionContext actx)
   {
      StatManager statManager = StatUtil.findStatManager(m_invocationContext);

      if (statManager == null)
      {
         return;
      }

      StatManager.MBeanHandler mBeanSaveHandler = new StatManager.MBeanHandler()
      {
         public void handle(StatMBean mBean)
         {
            mBean.forEachAttr(new AttrHandler()
            {
               public void handle(Attr attr)
               {
                  StatValue stat = attr.getStatValue();

                  if (stat instanceof SavedStatValue)
                  {
                     ((SavedStatValue)stat).save();
                  }
               }
            });
         }
      };

      StatManager.MBeanHandler mBeanResetHandler = new StatManager.MBeanHandler()
      {
         public void handle(StatMBean mBean)
         {
            mBean.forEachAttr(new AttrHandler()
            {
               public void handle(Attr attr)
               {
                  StatValue stat = attr.getStatValue();

                  if (stat instanceof SavedStatValue)
                  {
                     ((SavedStatValue)stat).reset();
                  }
               }
            });
         }
      };

      if (m_invocationContext.getMetadata().isDistributed())
      {
         statManager.forEachSourceMBean(mBeanSaveHandler);
         statManager.forEachSourceMBean(mBeanResetHandler);
      }
      else
      {
         statManager.forEachMBean(mBeanSaveHandler);
         statManager.forEachMBean(mBeanResetHandler);
      }
   }

   /**
    * Persist statistics of the current node.
    * @param reader The command object.
    * @param actx action context.
    */
   public void persist(Accessor reader, ActionContext actx)
   {
      final Metadata metadata = m_invocationContext.getMetadata();
      NodeManager nodeManager = ((NodeManager)m_invocationContext.getComponentInstance(
            "System.ClusterManager"));

      synchronized (nodeManager)
      {
         StatManager statManager = StatUtil.findStatManager(m_invocationContext);

         if (statManager == null)
         {
            return;
         }

         statManager.forEachMBean(new StatManager.MBeanHandler()
         {
            public void handle(final StatMBean mBean)
            {
               mBean.forEachAttr(new StatMBean.AttrHandler()
               {
                  public void handle(Attr attr)
                  {
                     String sPersistClass = attr.getPersistClass();

                     if (sPersistClass == null)
                     {
                        return;
                     }

                     Metaclass persistClass = metadata.findMetaclass(sPersistClass);

                     if (persistClass == null)
                     {
                        return;
                     }

                     persistClass.invoke("create", new Object[]{mBean.getPath(), attr.getName()});
                  }
               });
            }
         }, (metadata.isDistributed()) ? StatManager.getPrefix(nodeManager.getHTTPNode(), true) : "");
      }
   }

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_invocationContext = context;
   }
}
