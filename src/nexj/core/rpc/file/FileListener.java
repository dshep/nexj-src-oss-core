// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file;

/**
 * Interface implemented by File listeners.
 */
public interface FileListener
{
   /**
    * Receives a File message.
    * 
    * @param msg The message from which to read the data.
    */
   void onMessage(FileMessage msg);
}
