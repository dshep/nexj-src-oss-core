// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import java.io.InputStream;
import java.lang.reflect.Method;

import javax.resource.ResourceException;
import javax.resource.spi.endpoint.MessageEndpoint;

import nexj.core.rpc.file.FileListener;
import nexj.core.rpc.file.FileMessage;
import nexj.core.rpc.ra.TransactionalConsumer;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * File message consumer.
 */
public class FileConsumer extends TransactionalConsumer
{
   // associations

   /**
    * The FileListener.onMessage() method object.
    */
   protected final static Method FILELISTENER_ONMESSAGE_METHOD =
      getMethod(FileListener.class, "onMessage", new Class[]{FileMessage.class});

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(FileConsumer.class);

   /**
    * The managed connection to use over the lifetime of this consumer.
    */
   protected FileManagedConnection m_managedConnection;

   /**
    * The name of the file containing the message to be consumed.
    */
   protected String m_sFileName;


   // constructors

   /**
    * Constructs the consumer.
    * @param pool The consumer pool.
    */
   public FileConsumer(FileConsumerPool pool) throws Throwable
   {
      super(pool, s_logger);
   }


   // operations

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#close()
    */
   protected void close()
   {
      try
      {
         if (m_managedConnection != null)
         {
            m_managedConnection.destroy();
         }
      }
      catch (ResourceException ex)
      {
         ObjUtil.rethrow(ex);
      }
      
      m_managedConnection = null;
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#commit()
    */
   protected void commit() throws Throwable
   {
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#consume()
    */
   protected boolean consume() throws Throwable
   {
      FileConsumerPool pool = (FileConsumerPool)m_pool;
      final String sOriginalName = m_sFileName;
      final nexj.core.rpc.file.FileConnection fConn = (nexj.core.rpc.file.FileConnection)m_managedConnection.getConnection(null, null);

      try
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Using file connection: " + fConn);
         }

         if (fConn.attachToFile(sOriginalName))
         {
            consume(new FileMessage()
            {
               public InputStream getInputStream()
               {
                  return fConn.getInputStream();
               }

               public void setExpandedProcessedName(String sExpandedName)
               {
                  fConn.setExpandedProcessedName(sExpandedName);
               }

               public String getOriginalName()
               {
                  return sOriginalName;
               }
            });
         }

         pool.removeProblemFile(sOriginalName);
      }
      catch (FileConnectionException ex)
      {
         m_bFailed = true;

         if (s_logger.isWarnEnabled())
         {
            if (pool.addProblemFile(sOriginalName))
            {
               s_logger.warn("Error attaching to file \"" + sOriginalName + "\"", ex);
            }
         }
      }
      finally
      {
         fConn.close();
      }

      return false;
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#deliver(java.lang.Object)
    */
   protected void deliver(Object message) throws Throwable
   {
      ((FileListener)m_endpoint).onMessage((FileMessage)message);
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#getMethod()
    */
   protected Method getMethod()
   {
      return FILELISTENER_ONMESSAGE_METHOD;
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#init()
    */
   protected MessageEndpoint init() throws Throwable
   {
      FileConsumerPool pool = (FileConsumerPool)m_pool;
      
      m_managedConnection = (FileManagedConnection)pool.getManagedConnectionFactory().createManagedConnection(null, null);
      
      return m_pool.getFactory().createEndpoint(m_managedConnection.getXAResource());
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#reject(java.lang.Object)
    */
   protected void reject(Object message)
   {
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#rollback()
    */
   protected void rollback() throws Throwable
   {
   }

   /**
    * Starts the consumer.
    * 
    * @param sFileName The path and name of the file (relative to the incoming directory)
    *                  containing the message to consume.
    */
   public void start(String sFileName) throws Throwable
   {
      m_sFileName = sFileName;
      
      activate();
   }
}
