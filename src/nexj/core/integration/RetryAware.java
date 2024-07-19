// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

/**
 * A RetryAware object wants to know how many times it has been invoked and how many more times it will be invoked.
 */
public interface RetryAware
{
   /**
    * Sets the number of times this has previously been submitted.
    * @param nRetryCount The number of times this has previously been submitted (0 for first submission).
    */
   public void setRetryCount(int nRetryCount);
   
   /**
    * Sets the maximum number of times this will be submitted.
    * @param nMaxRetryCount The maximum number of times this will be submitted.
    */
   public void setMaxRetryCount(int nMaxRetryCount);
}
