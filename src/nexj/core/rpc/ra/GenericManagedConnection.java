// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra;

import java.io.PrintWriter;
import java.util.Iterator;

import javax.resource.NotSupportedException;
import javax.resource.ResourceException;
import javax.resource.spi.ConnectionEvent;
import javax.resource.spi.ConnectionEventListener;
import javax.resource.spi.ConnectionRequestInfo;
import javax.resource.spi.LocalTransaction;
import javax.resource.spi.ManagedConnection;
import javax.resource.spi.ManagedConnectionMetaData;
import javax.security.auth.Subject;
import javax.transaction.xa.XAResource;

import nexj.core.util.HashDeque;
import nexj.core.util.HolderDeque;
import nexj.core.version.Version;

/**
 * Generic managed connection.
 */
public abstract class GenericManagedConnection implements ManagedConnection, ManagedConnectionMetaData
{
   // associations

   /**
    * The connection listener list. Can be null.
    */
   protected HolderDeque m_listenerDeque;

   /**
    * The log writer. Can be null.
    */
   protected PrintWriter m_logWriter;

   // operations

   /**
    * Creates a new connection.
    * @param subject The JAAS subject.
    * @param cri The connection request information.
    */
   protected abstract GenericConnection createConnection(Subject subject, ConnectionRequestInfo cri) throws ResourceException;

   /**
    * Determines if this managed connection corresponds to a given subject and connection request info.
    * @param subject The JAAS subject. Can be null.
    * @param cri The connectionrequest info. Can be null.
    * @return True if the managed connection matches this information.
    */
   public abstract boolean matches(Subject subject, GenericConnectionRequestInfo cri);

   /**
    * Adds a connection handle to the managed connection.
    * @param handle The connection handle.
    */
   protected abstract void addConnection(GenericConnection handle) throws ResourceException;

   /**
    * Removes a connection handle.
    * @param handle The connection handle.
    */
   protected abstract void removeConnection(GenericConnection handle);

   /**
    * Closes all the connection handles.
    */
   protected abstract void closeConnections() throws ResourceException;

   /**
    * @see javax.resource.spi.ManagedConnection#associateConnection(java.lang.Object)
    */
   public synchronized void associateConnection(Object handle) throws ResourceException
   {
      ((GenericConnection)handle).setManagedConnection(this);
   }

   /**
    * @see javax.resource.spi.ManagedConnection#cleanup()
    */
   public synchronized void cleanup() throws ResourceException
   {
      closeConnections();
   }

   /**
    * @see javax.resource.spi.ManagedConnection#destroy()
    */
   public synchronized void destroy() throws ResourceException
   {
      closeConnections();
   }

   /**
    * @see javax.resource.spi.ManagedConnection#getLocalTransaction()
    */
   public LocalTransaction getLocalTransaction() throws ResourceException
   {
      throw new NotSupportedException();
   }

   /**
    * @see javax.resource.spi.ManagedConnection#getXAResource()
    */
   public XAResource getXAResource() throws ResourceException
   {
      throw new NotSupportedException();
   }

   /**
    * @see javax.resource.spi.ManagedConnection#addConnectionEventListener(javax.resource.spi.ConnectionEventListener)
    */
   public synchronized void addConnectionEventListener(ConnectionEventListener listener)
   {
      if (listener == null)
      {
         throw new NullPointerException();
      }

      if (m_listenerDeque == null)
      {
         m_listenerDeque = new HashDeque(4);
      }

      m_listenerDeque.addLast(listener);
   }

   /**
    * @see javax.resource.spi.ManagedConnection#removeConnectionEventListener(javax.resource.spi.ConnectionEventListener)
    */
   public synchronized void removeConnectionEventListener(ConnectionEventListener listener)
   {
      if (listener == null)
      {
         throw new NullPointerException();
      }

      if (m_listenerDeque != null)
      {
         m_listenerDeque.remove(listener);
      }
   }

   /**
    * Fires a connection event.
    * @param nType The event type, one of the ConnectionEvent.* constants.
    * @param handle The connection handle. Can be null.
    * @param e The exception. Can be null.
    */
   public synchronized void notify(int nType, Object handle, Exception e)
   {
      if (m_listenerDeque != null)
      {
         ConnectionEvent evt = (e == null) ? new ConnectionEvent(this, nType) : new ConnectionEvent(this, nType, e);

         if (handle != null)
         {
            evt.setConnectionHandle(handle);
         }

         for (Iterator itr = m_listenerDeque.iterator(); itr.hasNext();)
         {
            ConnectionEventListener listener = (ConnectionEventListener)itr.next();

            switch (nType)
            {
               case ConnectionEvent.CONNECTION_CLOSED:
                  listener.connectionClosed(evt);
                  break;

               case ConnectionEvent.CONNECTION_ERROR_OCCURRED:
                  listener.connectionErrorOccurred(evt);
                  break;

               case ConnectionEvent.LOCAL_TRANSACTION_STARTED:
                  listener.localTransactionStarted(evt);
                  break;

               case ConnectionEvent.LOCAL_TRANSACTION_COMMITTED:
                  listener.localTransactionCommitted(evt);
                  break;

               case ConnectionEvent.LOCAL_TRANSACTION_ROLLEDBACK:
                  listener.localTransactionRolledback(evt);
                  break;
            }
         }
      }
   }

   /**
    * @see javax.resource.spi.ManagedConnection#setLogWriter(java.io.PrintWriter)
    */
   public synchronized void setLogWriter(PrintWriter logWriter) throws ResourceException
   {
      m_logWriter = logWriter;
   }

   /**
    * @see javax.resource.spi.ManagedConnection#getLogWriter()
    */
   public synchronized PrintWriter getLogWriter() throws ResourceException
   {
      return m_logWriter;
   }

   /**
    * @see javax.resource.spi.ManagedConnection#getMetaData()
    */
   public ManagedConnectionMetaData getMetaData() throws ResourceException
   {
      return this;
   }

   /**
    * @see javax.resource.spi.ManagedConnectionMetaData#getEISProductName()
    */
   public String getEISProductName() throws ResourceException
   {
      return "NexJ Resource Adapter";
   }

   /**
    * @see javax.resource.spi.ManagedConnectionMetaData#getEISProductVersion()
    */
   public String getEISProductVersion() throws ResourceException
   {
      return Version.RELEASE;
   }

   /**
    * @see javax.resource.spi.ManagedConnectionMetaData#getMaxConnections()
    */
   public int getMaxConnections() throws ResourceException
   {
      return 0;
   }

   /**
    * @see javax.resource.spi.ManagedConnectionMetaData#getUserName()
    */
   public String getUserName() throws ResourceException
   {
      return null;
   }
}
