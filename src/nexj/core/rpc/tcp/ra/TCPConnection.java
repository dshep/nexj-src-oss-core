// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp.ra;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;

import javax.resource.ResourceException;

import nexj.core.rpc.ra.GenericConnection;

/**
 * TCP connection.
 */
public class TCPConnection extends GenericConnection implements nexj.core.rpc.tcp.TCPConnection
{
   // constructors

   /**
    * Create a TCP connection.
    */
   public TCPConnection()
   {
   }

   // operations

   /**
    * @return The local socket address
    */
   public InetSocketAddress getLocalAddress()
   {
      return ((TCPManagedConnection)m_managedConnection).getLocalAddress();
   }

   /**
    * @return The remote socket address
    */
   public InetSocketAddress getRemoteAddress()
   {
      return ((TCPManagedConnection)m_managedConnection).getRemoteAddress();
   }

   /**
    * @return The socket's associated output stream
    */
   public OutputStream getOutputStream() throws IOException
   {
      return ((TCPManagedConnection)m_managedConnection).getOutputStream();
   }

   /**
    * @return The socket's associated input stream
    */
   public InputStream getInputStream() throws IOException
   {
      return ((TCPManagedConnection)m_managedConnection).getInputStream();
   }

   /**
    * @see nexj.core.rpc.tcp.TCPConnection#close()
    */
   public void close()
   {
      if (isEndOfStream())
      {
         invalidate();
      }

      if (!isValid())
      {
         return;
      }

      try
      {
         super.closeHandle();
      }
      catch (ResourceException e)
      {
      }
   }

   /**
    * @see nexj.core.rpc.tcp.TCPConnection#isValid()
    */
   public boolean isValid()
   {
      return m_managedConnection == null ||
         ((TCPManagedConnection)m_managedConnection).isValid();
   }

   /**
    * @see nexj.core.rpc.tcp.TCPConnection#isEndOfStream()
    */
   public boolean isEndOfStream()
   {
      return m_managedConnection != null &&
         ((TCPManagedConnection)m_managedConnection).isEndOfStream();
   }

   /**
    * @see nexj.core.rpc.tcp.TCPConnection#invalidate(boolean)
    */
   public void invalidate()
   {
      if (m_managedConnection != null)
      {
         ((TCPManagedConnection)m_managedConnection).invalidate();
      }
   }
   
   /**
    * @see nexj.core.rpc.tcp.TCPConnection#getReadTimeout()
    */
   public int getReadTimeout()
   {
      return ((TCPManagedConnection)m_managedConnection).getReadTimeout();
   }

   /**
    * @see nexj.core.rpc.tcp.TCPConnection#setReadTimeout(int)
    */
   public void setReadTimeout(int nTimeout)
   {
      if (m_managedConnection != null)
      {
         ((TCPManagedConnection)m_managedConnection).setReadTimeout(nTimeout);
      }
   }

   /**
    * @see nexj.core.rpc.tcp.TCPConnection#getTOS()
    */
   public int getTOS()
   {
      return ((TCPManagedConnection)m_managedConnection).getTOS();
   }

   /**
    * @see nexj.core.rpc.tcp.TCPConnection#setTOS(int)
    */
   public void setTOS(int nTOS)
   {
      if (m_managedConnection != null)
      {
         ((TCPManagedConnection)m_managedConnection).setTOS(nTOS);
      }
   }
}
