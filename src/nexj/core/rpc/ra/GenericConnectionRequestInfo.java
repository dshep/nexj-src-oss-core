// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra;

import javax.resource.spi.ConnectionRequestInfo;

/**
 * Generic connection request info.
 */
public abstract class GenericConnectionRequestInfo implements ConnectionRequestInfo
{
   // operations

   /**
    * @return Object used for partitioning the managed connections.
    */
   public Object getManagedConnectionPartition()
   {
      return this;
   }
}
