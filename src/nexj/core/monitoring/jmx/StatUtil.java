package nexj.core.monitoring.jmx;

import nexj.core.meta.Component;
import nexj.core.meta.Metadata;
import nexj.core.monitoring.Average;
import nexj.core.monitoring.Counter;
import nexj.core.monitoring.Statistic;
import nexj.core.monitoring.jmx.StatMBean;
import nexj.core.monitoring.jmx.StatManager;
import nexj.core.monitoring.jmx.StatValue;
import nexj.core.runtime.InvocationContext;

/**
 * Statistics utility.
 */
public class StatUtil
{
   // operations

   /**
    * Find statistics manager.
    * @param context Invocation context.
    * @return Statistics manager or null if not found.
    */
   public static StatManager findStatManager(InvocationContext context)
   {
      return findStatManager(context.getMetadata());
   }

   /**
    * Find statistics manager.
    * @param metadata Metadata.
    * @return Statistics manager or null if not found.
    */
   public static StatManager findStatManager(Metadata metadata)
   {
      Component statManagerComponent = metadata.findComponent("System.StatManager");

      if (statManagerComponent == null)
      {
         return null;
      }

      return (StatManager)statManagerComponent.getInstance(null);
   }

   /**
    * Find statistic.
    * @param context Invocation context.
    * @param sPath Statistic's path.
    * @return Statistic or null if not found.
    */
   public static StatMBean findStat(InvocationContext context, String sPath)
   {
      StatManager statManager = findStatManager(context);

      if (statManager == null)
      {
         return null;
      }

      return statManager.find(sPath);
   }

   /**
    * Find statistic.
    * @param context Invocation context.
    * @param sPath Statistic's path.
    * @param sName Statistic's name.
    * @return Statistic or null if not found.
    */
   public static Statistic findStat(InvocationContext context, String sPath, String sName)
   {
      StatManager statManager = findStatManager(context);

      if (statManager == null)
      {
         return null;
      }

      StatValue statValue = statManager.find(sPath, sName);

      if (statValue == null)
      {
         return null;
      }

      return statValue.getStatistic();
   }

   /**
    * Find statistic.
    * @param metadata Metadata.
    * @param sPath Statistic's path.
    * @return Statistic or null if not found.
    */
   public static StatMBean findStat(Metadata metadata, String sPath)
   {
      StatManager statManager = findStatManager(metadata);

      if (statManager == null)
      {
         return null;
      }

      return statManager.find(sPath);
   }

   /**
    * Find statistic.
    * @param metadata Metadata.
    * @param sPath Statistic's path.
    * @param sName Statistic's name.
    * @return Statistic or null if not found.
    */
   public static Statistic findStat(Metadata metadata, String sPath, String sName)
   {
      StatManager statManager = findStatManager(metadata);

      if (statManager == null)
      {
         return null;
      }

      StatValue statValue = statManager.find(sPath, sName);

      if (statValue == null)
      {
         return null;
      }

      return statValue.getStatistic();
   }

   /**
    * Increment counter if it exists.
    * @param context Invocation context.
    * @param sPath Statistic's path.
    * @param sName Statistic's name.
    * @param nIncr Increment.
    * @return Statistic or null if not found.
    */
   public static void incrCounter(InvocationContext context, String sPath, String sName, int nIncr)
   {
      if (context == null || sPath == null || sName == null)
      {
         return;
      }

      StatManager statManager = findStatManager(context);

      if (statManager == null)
      {
         return;
      }

      CounterStatValue counter = (CounterStatValue)statManager.find(sPath, sName);

      if (counter != null)
      {
         ((Counter)counter.getStatistic()).add(nIncr);
      }
   }

   /**
    * Update average if it exists.
    * @param context Invocation context.
    * @param sPath Statistic's path.
    * @param sName Statistic's name.
    * @param dValue Value.
    * @return Statistic or null if not found.
    */
   public static void updateAverage(InvocationContext context, String sPath, String sName, double dValue)
   {
      if (context == null || sPath == null || sName == null)
      {
         return;
      }

      StatManager statManager = findStatManager(context);

      if (statManager == null)
      {
         return;
      }

      SavedStatValue average = (SavedStatValue)statManager.find(sPath, sName);

      if (average != null)
      {
         ((Average)average.getStatistic()).add(dValue);
      }
   }

   /**
    * Increment counter if it exists.
    * @param context Invocation context.
    * @param sPath Statistic's path.
    * @param sName Statistic's name.
    * @param nIncr Increment.
    * @return Statistic or null if not found.
    */
   public static void incrCounter(Metadata metadata, String sPath, String sName, int nIncr)
   {
      if (metadata == null || sPath == null || sName == null)
      {
         return;
      }

      StatManager statManager = findStatManager(metadata);

      if (statManager == null)
      {
         return;
      }

      CounterStatValue counter = (CounterStatValue)statManager.find(sPath, sName);

      if (counter != null)
      {
         ((Counter)counter.getStatistic()).add(nIncr);
      }
   }

   /**
    * Update average if it exists.
    * @param context Invocation context.
    * @param sPath Statistic's path.
    * @param sName Statistic's name.
    * @param dValue Value.
    * @return Statistic or null if not found.
    */
   public static void updateAverage(Metadata metadata, String sPath, String sName, double dValue)
   {
      if (metadata == null || sPath == null || sName == null)
      {
         return;
      }

      StatManager statManager = findStatManager(metadata);

      if (statManager == null)
      {
         return;
      }

      SavedStatValue average = (SavedStatValue)statManager.find(sPath, sName);

      if (average != null)
      {
         ((Average)average.getStatistic()).add(dValue);
      }
   }
}
