// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.sql.ra;


import javax.resource.ResourceException;
import javax.resource.spi.ActivationSpec;
import javax.resource.spi.ConnectionManager;
import javax.resource.spi.endpoint.MessageEndpointFactory;
import javax.transaction.xa.XAResource;

import nexj.core.rpc.ra.GenericConnectionManager;
import nexj.core.rpc.ra.GenericConsumerPool;
import nexj.core.rpc.ra.GenericResourceAdapter;
import nexj.core.util.Logger;

/**
 * SQL resource adapter.
 */
public class SQLResourceAdapter extends GenericResourceAdapter
{
   /**
    * The default connection manager.
    */
   private static GenericConnectionManager s_defaultConnectionManager;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(SQLResourceAdapter.class);


   /**
    * Constructor.
    */
   public SQLResourceAdapter()
   {
      super(s_logger);
   }

   /**
    * @see nexj.core.rpc.ra.GenericResourceAdapter#createConsumerPool(javax.resource.spi.endpoint.MessageEndpointFactory, javax.resource.spi.ActivationSpec)
    */
   protected GenericConsumerPool createConsumerPool(MessageEndpointFactory factory,
      ActivationSpec cfg) throws ResourceException
   {
      throw new UnsupportedOperationException(); // do not support incoming connections
   }

   /**
    * @return The default connection manager.
    */
   public static synchronized ConnectionManager getDefaultConnectionManager()
   {
      if (s_defaultConnectionManager == null)
      {
         s_defaultConnectionManager = new GenericConnectionManager(GenericConnectionManager.ASSOCIATED);
      }

      return s_defaultConnectionManager;
   }

   /**
    * @see javax.resource.spi.ResourceAdapter#getXAResources(javax.resource.spi.ActivationSpec[])
    */
   public XAResource[] getXAResources(ActivationSpec[] specs) throws ResourceException
   {
      return null;
   }
}