// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file;

import javax.resource.ResourceException;


/**
 * The client-side interface to the FileConnection factory.
 */
public interface FileConnectionFactory
{
   /**
    * Gets a FileConnection from the managed pool.
    * 
    * @return A clean FileConnection that may be used to perform incoming/outgoing
    *         file message operations.
    */
   public FileConnection getConnection() throws ResourceException;
}
