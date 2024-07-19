package nexj.core.monitoring.jmx;

/**
 * JMX command consumer.
 */
public interface StatMBeanCommandConsumer
{
   /**
    * Consume a JMX command.
    * @param mBean MBean whose command is to be consumed.
    * @param sCommandName Command name.
    * @param commandParamArray Command parameters.
    * @param sCommandSignatureArray Command signature.
    * @return Result or null if command is rejected.
    */
   Object consume(StatMBean mBean, String sCommandName, Object[] commandParamArray,
         String[] sCommandSignatureArray);

   /**
    * Remove stale statistics.
    * @param mBean MBean.
    */
   void removeStale(StatMBean mBean);
}
