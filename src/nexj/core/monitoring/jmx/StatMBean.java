package nexj.core.monitoring.jmx;

import java.io.Serializable;
import java.lang.management.ManagementFactory;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.AttributeNotFoundException;
import javax.management.DynamicMBean;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.JMException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanOperationInfo;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

import nexj.core.monitoring.Average;
import nexj.core.monitoring.Counter;
import nexj.core.monitoring.Flag;
import nexj.core.monitoring.jmx.StatManager;
import nexj.core.monitoring.Value;
import nexj.core.util.HashTab;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * Dynamic MBean.
 */
public class StatMBean implements DynamicMBean, Serializable, Cloneable
{
   // constants

   /**
    * Serial version id.
    */
   private final static long serialVersionUID = -3676790448070145319L;

   /**
    * Counter statistic type.
    */
   public final static String STAT_COUNTER = "counter";

   /**
    * Average mean statistic type.
    */
   public final static String STAT_MEAN = "mean";

   /**
    * Value statistic type.
    */
   public final static String STAT_VALUE = "value";

   /**
    * Flag statistic type.
    */
   public final static String STAT_FLAG = "flag";

   /**
    * Reset statistic operation.
    */
   public final static String OP_NAME_RESET = "reset";

   /**
    * Remove stale statistics operation.
    */
   public final static String OP_NAME_REMOVE_STALE = "removeStale";

   /**
    * Set attribute operation.
    */
   public final static String OP_NAME_SET_ATTRIBUTE = "setAttribute";

   /**
    * Get statistic total operation.
    */
   public final static String OP_NAME_GET_TOTAL = "getTotal";

   /**
    * Get statistic average operation.
    */
   public final static String OP_NAME_GET_AVERAGE = "getAverage";

   /**
    * Statistic invalid path error.
    */
   public final static String ERR_INVALID_PATH = "err.monitoring.jmx.statInvalidPath";

   /**
    * Statistic invalid type error.
    */
   public final static String ERR_INVALID_TYPE = "err.monitoring.jmx.statInvalidType";

   /**
    * Statistic not found error.
    */
   public final static String ERR_NOT_FOUND = "err.monitoring.jmx.statNotFound";

   /**
    * MBean server.
    */
   private final static MBeanServer m_beanServer = ManagementFactory.getPlatformMBeanServer();

   // attributes

   /**
    * The environment name.
    */
   private String m_sEnvironmentName;

   /**
    * Path.
    */
   private String m_sPath;

   /**
    * Object name.
    */
   private transient ObjectName m_objectName;

   /**
    * Whether to prepend isolated node name.
    */
   private boolean m_bIsolated;

   /**
    * Whether registered.
    */
   private transient boolean m_bRegistered;

   /**
    * Whether snapshot MBean, i.e. visible to JMX.
    */
   private transient boolean m_bSnapshot;

   // associations

   /**
    * Attributes to expose over JMX.
    */
   private Lookup m_attributeMap = new HashTab(); // of type Attr[String]

   /**
    * JMX command consumer.
    */
   private transient StatMBeanCommandConsumer m_commandConsumer;

   /**
    * Operations.
    */
   private Operations m_operations;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(StatMBean.class);

   // constructors

   /**
    * Construct an MBean.
    * @param sPath Path, e.g. "Channel/FileChannel/filename"
    * @param bSnapshot Whether snapshot.
    * @param bIsolated Whether to prepend isolated node name.
    * @param commandConsumer Command consumer. Can be null.
    */
   public StatMBean(String sEnvironmentName, String sPath, boolean bSnapshot, boolean bIsolated,
         StatMBeanCommandConsumer commandConsumer)
   {
      m_sEnvironmentName = sEnvironmentName;
      m_sPath = sPath;
      m_bSnapshot = bSnapshot;
      m_bIsolated = bIsolated;
      buildName(sPath);
      m_commandConsumer = commandConsumer;
   }

   // operations

   /**
    * Set command consumer.
    * @param commandConsumer Command consumer to set.
    */
   public void setCommandConsumer(StatMBeanCommandConsumer commandConsumer)
   {
      m_commandConsumer = commandConsumer;
   }

   /**
    * Get command consumer.
    * @return Command consumer.
    */
   public StatMBeanCommandConsumer getCommandConsumer()
   {
      return m_commandConsumer;
   }

   /**
    * @return the operations
    */
   public synchronized Operations getOperations()
   {
      return m_operations;
   }

   /**
    * @param operations the operations to set
    */
   public synchronized void setOperations(Operations operations)
   {
      if (m_operations != operations)
      {
         m_operations = operations;
         update();
      }
   }

   /**
    * Add a statistic.
    * @param sName Name.
    * @param sType Type.
    * @param bReadOnly Whether read only, i.e. cannot be modified from JMX client.
    * @param sPersistClass Persistent statistics class name.
    * @return Statistic.
    */
   public synchronized StatValue addStat(String sName, String sType, boolean bReadOnly, String sPersistClass)
   {
      assert sName != null;

      Attr attr = (Attr)m_attributeMap.get(sName);

      if (attr != null && attr.getType().equals(sType))
      {
         return attr.getStatValue();
      }

      StatValue statValue;

      if (sType.equals(STAT_COUNTER))
      {
         statValue = new CounterStatValue(new Counter());
      }
      else if (sType.equals(STAT_MEAN))
      {
         statValue = new SavedStatValue(new AverageMeanStatValue(new Average()));
      }
      else if (sType.equals(STAT_VALUE))
      {
         statValue = new ValueStatValue(new Value());
      }
      else if (sType.equals(STAT_FLAG))
      {
         statValue = new FlagStatValue(new Flag());
      }
      else
      {
         throw new StatMBeanException(ERR_INVALID_TYPE, new Object[]{m_sPath, sName, sType});
      }

      addStat(sName, statValue, bReadOnly, sPersistClass);

      return statValue;
   }

   /**
    * Adds a statistic. If a statistic with the same name already exists, it is overwritten.
    * @param sName Name.
    * @param statValue Statistic.
    * @param bReadOnly Whether read only, i.e. cannot be modified from JMX client.
    * @param sPersistClass Persistent statistics class name.
    */
   public synchronized void addStat(String sName, StatValue statValue, boolean bReadOnly, String sPersistClass)
   {
      m_attributeMap.put(sName, new Attr(sName, statValue.getType(), statValue, bReadOnly, sPersistClass));
      update();
   }

   /**
    * Remove statistic.
    * @param sName Name.
    */
   public synchronized void removeStat(String sName)
   {
      assert sName != null;

      if (!m_attributeMap.contains(sName))
      {
         throw new StatMBeanException(ERR_NOT_FOUND, new Object[]{m_sPath, sName});
      }

      m_attributeMap.remove(sName);
      update();
   }

   /**
    * Clear statistics.
    */
   public synchronized void clear()
   {
      m_attributeMap.clear();
      update();
   }

   /**
    * Get statistic.
    * @param sName Name.
    */
   public synchronized StatValue getStat(String sName)
   {
      assert sName != null;

      Attr attr = (Attr)m_attributeMap.get(sName);

      if (attr == null)
      {
         throw new StatMBeanException(ERR_NOT_FOUND, new Object[]{m_sPath, sName});
      }

      return attr.getStatValue();
   }

   /**
    * Find statistic.
    * @param sName Name.
    */
   public synchronized StatValue findStat(String sName)
   {
      assert sName != null;

      Attr attr = (Attr)m_attributeMap.get(sName);

      if (attr == null)
      {
         return null;
      }

      return attr.getStatValue();
   }

   /**
    * @see javax.management.DynamicMBean#getAttribute(java.lang.String)
    */
   public synchronized Object getAttribute(String sAttribute) throws AttributeNotFoundException
   {
      Attr attr = (Attr)m_attributeMap.get(sAttribute);

      if (attr != null)
      {
         return attr.getStatValue().get();
      }

      throw new AttributeNotFoundException();
   }

   /**
    * @see javax.management.DynamicMBean#getAttributes(java.lang.String[])
    */
   public synchronized AttributeList getAttributes(String[] sAttributes)
   {
      AttributeList attributeList = new AttributeList(sAttributes.length);

      for (int i = 0; i < sAttributes.length; i++)
      {
         Attr attr = (Attr)m_attributeMap.get(sAttributes[i]);

         if (attr != null)
         {
            attributeList.add(new Attribute(sAttributes[i], attr.getStatValue().get()));
         }
      }

      return attributeList;
   }

   /**
    * @see javax.management.DynamicMBean#getMBeanInfo()
    */
   public synchronized MBeanInfo getMBeanInfo()
   {
      MBeanAttributeInfo[] attributesInfo = new MBeanAttributeInfo[m_attributeMap.size()];
      int i = 0;

      for (Lookup.Iterator itr = m_attributeMap.iterator(); itr.hasNext(); i++)
      {
         itr.next();
         String sName = (String)itr.getKey();
         Attr attr = (Attr)itr.getValue();
         String sType = attr.getStatValue().getType();

         attributesInfo[i] = new MBeanAttributeInfo(sName, sType, sName, true, !attr.isReadOnly(),
               sType.equals("java.lang.Boolean"));
      }

      Arrays.sort(attributesInfo, new Comparator()
      {
         public int compare(Object o1, Object o2)
         {
            return ((MBeanAttributeInfo)o1).getName().compareTo(((MBeanAttributeInfo)o2).getName());
         }
      });

      return new MBeanInfo(StatMBean.class.getName(), m_sPath, attributesInfo, null, (m_operations == null) ?
            null : m_operations.getInfo(), null);
   }

   /**
    * Invoke an action.
    * @param actionName Action name.
    * @param params Action parameters.
    * @param signature Action signature.
    * @param bConsume Whether to consume action if possible.
    */
   public Object invoke(String actionName, Object[] params, String[] signature, boolean bConsume)
   {
      if (bConsume && m_commandConsumer != null)
      {
         Object result = m_commandConsumer.consume(this, actionName, params, signature);

         if (result != null)
         {
            return result;
         }
      }

      if (actionName.equals(OP_NAME_RESET) && signature.length == 1
            && signature[0].equals("java.lang.String"))
      {
         reset((String)params[0]);

         return "Success";
      }
      else if (actionName.equals(OP_NAME_REMOVE_STALE) && signature.length == 0)
      {
         if (m_commandConsumer != null)
         {
            m_commandConsumer.removeStale(this);
         }

         return "Success";
      }

      throw new UnsupportedOperationException();
   }

   /**
    * @see javax.management.DynamicMBean#invoke(java.lang.String, java.lang.Object[], java.lang.String[])
    */
   public Object invoke(String actionName, Object[] params, String[] signature)
   {
      return invoke(actionName, params, signature, true);
   }

   /**
    * @see javax.management.DynamicMBean#setAttribute(javax.management.Attribute)
    */
   public synchronized void setAttribute(Attribute attribute)
   {
      setAttribute(attribute, true);
   }

   /**
    * Set attribute.
    * @param attribute Attribute to set.
    * @param bConsume Whether to consume this operation.
    */
   public synchronized void setAttribute(Attribute attribute, boolean bConsume)
   {
      String sAttribute = attribute.getName();
      Attr attr = (Attr)m_attributeMap.get(sAttribute);

      if (attr == null)
      {
         throw new IllegalArgumentException(sAttribute);
      }

      if (attr.isReadOnly())
      {
         throw new IllegalArgumentException(sAttribute);
      }

      try
      {
         attr.getStatValue().set(attribute.getValue());
      }
      catch (Exception e)
      {
         throw new IllegalArgumentException(sAttribute, e);
      }

      if (bConsume && m_commandConsumer != null)
      {
         m_commandConsumer.consume(this, OP_NAME_SET_ATTRIBUTE, new Object[]{attribute}, null);
      }
   }

   /**
    * @see javax.management.DynamicMBean#setAttributes(javax.management.AttributeList)
    */
   public synchronized AttributeList setAttributes(AttributeList attributes)
   {
      AttributeList newAttributes = new AttributeList();

      for (Iterator itr = attributes.iterator(); itr.hasNext();)
      {
         try
         {
            Attribute attr = (Attribute)itr.next();

            setAttribute(attr);
            newAttributes.add(attr);
         }
         catch (IllegalArgumentException e)
         {
            // Ignore.
         }
      }

      return newAttributes;
   }

   /**
    * Clear statistic.
    * @param sName Name.
    */
   private synchronized void reset(String sName)
   {
      Attr attr = (Attr)m_attributeMap.get(sName);

      if (attr == null)
      {
         throw new IllegalArgumentException(sName);
      }

      attr.getStatValue().getStatistic().reset();
   }

   /**
    * Update MBean, i.e. re-register.
    * @throws JMException MBean exception.
    */
   protected void update()
   {
      if (J2EEUtil.isContained())
      {
         if (m_bSnapshot)
         {
            try
            {
               if (m_objectName == null)
               {
                  buildName(m_sPath);
               }

               boolean bRegistered = m_bRegistered;

               if (m_bRegistered)
               {
                  try
                  {
                     m_beanServer.unregisterMBean(m_objectName);
                  }
                  catch (InstanceNotFoundException e)
                  {
                     // Ignore.
                  }

                  m_bRegistered = false;
               }

               if (m_attributeMap.size() > 0)
               {
                  for (;;)
                  {
                     try
                     {
                        m_beanServer.registerMBean(this, m_objectName);

                        break;
                     }
                     catch (InstanceAlreadyExistsException e)
                     {
                        if (bRegistered)
                        {
                           s_logger.warn("Statistic already exists", e);

                           break;
                        }

                        try
                        {
                           m_beanServer.unregisterMBean(m_objectName);
                        }
                        catch (InstanceNotFoundException e1)
                        {
                           // Ignore.
                        }
                     }
                  }

                  m_bRegistered = true;
               }
            }
            catch (Throwable e)
            {
               ObjUtil.rethrow(e);
            }
         }
      }
   }

   /**
    * Replace attributes with the attributes of the given MBean.
    * @param mBean MBean.
    */
   public synchronized void update(StatMBean mBean)
   {
      assert m_bSnapshot;

      if (mBean == null)
      {
         return;
      }

      boolean bUpdate = false;

      for (Iterator itr = m_attributeMap.iterator(); itr.hasNext();)
      {
         Object attribute = itr.next();

         if (!mBean.m_attributeMap.contains(attribute))
         {
            bUpdate = true;
            m_attributeMap.remove(attribute);
         }
      }

      for (Lookup.Iterator itr = mBean.m_attributeMap.iterator(); itr.hasNext();)
      {
         Object attribute = itr.next();

         bUpdate |= m_attributeMap.put(attribute, itr.getValue()) == null;
      }

      if (bUpdate)
      {
         update();
      }
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      StatMBean mBean;

      try
      {
         mBean = (StatMBean)super.clone();
      }
      catch (CloneNotSupportedException e)
      {
         mBean = null;
      }

      mBean.m_attributeMap = new HashTab(m_attributeMap.size());

      for (Lookup.Iterator itr = m_attributeMap.iterator(); itr.hasNext();)
      {
         itr.next();
         mBean.m_attributeMap.put(itr.getKey(), ((Attr)itr.getValue()).clone());
      }

      return mBean;
   }

   /**
    * Set snapshot.
    */
   public void setSnapshot()
   {
      if (!m_bSnapshot)
      {
         m_bSnapshot = true;
         update();
      }
   }

   /**
    * Get whether snapshot.
    * @return Whether snapshot.
    */
   public boolean isSnapshot()
   {
      return m_bSnapshot;
   }

   /**
    * Set path. For use in clustered mode only.
    * @param sPath New path.
    */
   public void setPath(String sPath)
   {
      buildName(sPath);
      m_sPath = sPath;
   }

   /**
    * Get path.
    * @return Path.
    */
   public String getPath()
   {
      return m_sPath;
   }

   /**
    * Build name from path.
    * @param sPath Path.
    * @param bIsolated Whether to prepend isolated node name.
    * @return Name.
    */
   private String buildName(String sPath)
   {
      if (StringUtil.isEmpty(sPath))
      {
         throw new StatMBeanException(ERR_INVALID_PATH, new Object[]{sPath});
      }

      sPath = ((m_bIsolated) ? StatManager.getPrefix(J2EEUtil.ISOLATED_NODE_NAME, true) : "") + sPath;

      String[] sParts = StringUtil.split(sPath, '/');
      StringBuilder builder = new StringBuilder(128);

      builder.append(SysUtil.PACKAGE).append('-').append(m_sEnvironmentName);
      builder.append(".core.monitoring.jmx:");

      for (int i = 0; i < sParts.length; i++)
      {
         builder.append('L');
         builder.append(i + 1);
         builder.append('=');
         builder.append(sParts[i]);

         if (i != sParts.length - 1)
         {
            builder.append(',');
         }
      }

      try
      {
         String sName = builder.toString();

         m_objectName = new ObjectName(sName);

         return sName;
      }
      catch (NullPointerException e)
      {
         throw new StatMBeanException(ERR_INVALID_PATH);
      }
      catch (MalformedObjectNameException e)
      {
         throw new StatMBeanException(ERR_INVALID_PATH);
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "MBean(name=" + m_sPath + ", attributes=" + m_attributeMap + ')';
   }

   /**
    * Invokes the handler for every defined statistic.
    * @param handler The handler to invoke.
    */
   public synchronized void forEachAttr(AttrHandler handler)
   {
      for (Iterator itr = m_attributeMap.valueIterator(); itr.hasNext();)
      {
         handler.handle((Attr)itr.next());
      }
   }

   // inner classes

   /**
    * Handles attributes.
    */
   public interface AttrHandler
   {
      /**
       * Handle an attribute.
       * @param attr The attribute to handle.
       */
      void handle(Attr attr);
   }

   /**
    * MBean attribute.
    */
   public static class Attr implements Serializable, Cloneable
   {
      // constants

      /**
       * Serial version id.
       */
      private final static long serialVersionUID = -7956320031931846273L;

      // attributes

      /**
       * Name.
       */
      private String m_sName;

      /**
       * Type.
       */
      private String m_sType;

      /**
       * Whether read only.
       */
      private boolean m_bReadOnly;

      /**
       * Persistence class.
       */
      private String m_sPersistClass;

      // associations

      /**
       * Statistic.
       */
      private StatValue m_stat;

      // constructors

      /**
       * Create an MBean attribute.
       * @param sName Name.
       * @param sType Type.
       * @param stat Statistic.
       * @param bReadOnly Whether read only, i.e. cannot be modified from JMX client.
       * @param bPersistable Whether persisting is allowed.
       */
      public Attr(String sName, String sType, StatValue stat, boolean bReadOnly, String sPersistClass)
      {
         m_sName = StringUtil.intern(sName);
         m_sType = StringUtil.intern(sType);
         m_stat = stat;
         m_bReadOnly = bReadOnly;
         m_sPersistClass = StringUtil.intern(sPersistClass);
      }

      /**
       * @return the name
       */
      public String getName()
      {
         return m_sName;
      }

      /**
       * @return the type
       */
      public String getType()
      {
         return m_sType;
      }

      /**
       * @return the bReadOnly
       */
      public boolean isReadOnly()
      {
         return m_bReadOnly;
      }

      /**
       * @return the persistClass
       */
      public String getPersistClass()
      {
         return m_sPersistClass;
      }

      /**
       * @return the stat
       */
      public StatValue getStatValue()
      {
         return m_stat;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return "Attr(" + m_sName + ')';
      }

      /**
       * @see java.lang.Object#clone()
       */
      public Object clone()
      {
         Attr attr;

         try
         {
            attr = (Attr)super.clone();
         }
         catch (CloneNotSupportedException e)
         {
            attr = null;
         }

         attr.m_stat = (StatValue)m_stat.clone();

         return attr;
      }
   }

   /**
    * MBean operations.
    */
   public interface Operations
   {
      /**
       * Get operations info.
       * @return Operations info.
       */
      MBeanOperationInfo[] getInfo();
   }
}
