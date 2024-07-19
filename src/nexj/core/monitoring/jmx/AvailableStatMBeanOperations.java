package nexj.core.monitoring.jmx;

import java.io.Serializable;

import javax.management.MBeanOperationInfo;

/**
 * MBean operations.
 */
public class AvailableStatMBeanOperations implements StatMBean.Operations, Serializable
{
   // constants

   /**
    * Serial version id.
    */
   private final static long serialVersionUID = -6143776549372248435L;

   /**
    * Singleton.
    */
   private final static StatMBean.Operations s_instance = new AvailableStatMBeanOperations();

   /**
    * Operation info.
    */
   private final static MBeanOperationInfo[] s_info = new MBeanOperationInfo[]
   {
      new MBeanOperationInfo(StatMBean.OP_NAME_REMOVE_STALE, "Remove stale statistics", null,
            "java.lang.String", MBeanOperationInfo.ACTION)
   };

   // constructors

   /**
    * Construct MBean operations.
    */
   private AvailableStatMBeanOperations()
   {
   }

   // operations

   public static StatMBean.Operations getInstance()
   {
      return s_instance;
   }

   /**
    * @see nexj.core.monitoring.jmx.StatMBean.Operations#getInfo()
    */
   public MBeanOperationInfo[] getInfo()
   {
      return (MBeanOperationInfo[])s_info.clone();
   }
}
