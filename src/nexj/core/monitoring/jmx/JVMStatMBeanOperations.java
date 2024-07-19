package nexj.core.monitoring.jmx;

import java.io.Serializable;

import javax.management.MBeanOperationInfo;
import javax.management.MBeanParameterInfo;

/**
 * MBean operations.
 */
public class JVMStatMBeanOperations implements StatMBean.Operations, Serializable
{
   // constants

   /**
    * Serial version id.
    */
   private final static long serialVersionUID = 22612214324944627L;

   /**
    * Singleton.
    */
   private final static StatMBean.Operations s_instance = new JVMStatMBeanOperations();

   /**
    * Operation info.
    */
   private final static MBeanOperationInfo[] s_info = new MBeanOperationInfo[]
   {
      new MBeanOperationInfo(StatMBean.OP_NAME_GET_TOTAL, "Get cluster-wide total.",
         new MBeanParameterInfo[]{new MBeanParameterInfo("sName", "java.lang.String",
            "Statistic's name.")}, "java.lang.Number", MBeanOperationInfo.ACTION),
      new MBeanOperationInfo(StatMBean.OP_NAME_GET_AVERAGE, "Get cluster-wide average.",
         new MBeanParameterInfo[]{new MBeanParameterInfo("sName", "java.lang.String",
            "Statistic's name.")}, "java.lang.Number", MBeanOperationInfo.ACTION)
   };

   // constructors

   /**
    * Construct MBean operations.
    */
   private JVMStatMBeanOperations()
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
