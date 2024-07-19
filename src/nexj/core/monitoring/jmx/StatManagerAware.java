package nexj.core.monitoring.jmx;


/**
 * Statistics initializer interface.
 */
public interface StatManagerAware
{
   /**
    * Set statistics manager.
    * @param statManager Statistics manager.
    */
   void setStatManager(StatManager statManager);
}
