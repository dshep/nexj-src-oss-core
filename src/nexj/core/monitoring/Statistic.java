// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.monitoring;

/**
 * Interface implemented by any statistic class.
 */
public interface Statistic
{
   /**
    * Accumulates the results from another statistic.
    * @param src The statistic to get the data from.
    */
   void accumulate(Statistic src);

   /**
    * Resets the statistic.
    */
   void reset();
}
