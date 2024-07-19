// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Interface for handling progress notifications.
 */
public interface ProgressListener
{
   /**
    * This method is called back periodically for progress notifications.
    * @param sMessageId The string id of the message.
    * @param args The message arguments. Can be null.
    * @param dProgress A value between 0 and 1 indicating the relative part of the work done so far.
    */
   void progress(String sMessageId, Object[] args, double dProgress);
}
