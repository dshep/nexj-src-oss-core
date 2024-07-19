// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.udp;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.MulticastSocket;
import java.net.SocketAddress;
import java.net.SocketException;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;

import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.pool.resource.GenericResource;

/**
 * UDP connection.
 */
public class UDPConnection extends GenericResource
{
   // associations

   /**
    * The UDP socket.
    */
   protected DatagramSocket m_socket;

   /**
    * Reusable lookup key.
    */
   protected static BindAddress s_address;

   /**
    * Map of a bind address and a socket to a socket ref. 
    */
   protected static Lookup s_socketMap = new HashTab();

   // constructors

   /**
    * Constructs the UDP connection.
    * @param bindAddress The local bind address. Can be null to use a random address.
    */
   protected UDPConnection(final SocketAddress bindAddress) throws Exception
   {
      try
      {
         m_socket = (DatagramSocket)AccessController.doPrivileged(
            new PrivilegedExceptionAction()
            {
               public Object run() throws Exception
               {
                  DatagramSocket socket;

                  if (!(bindAddress instanceof InetSocketAddress) ||
                     ((InetSocketAddress)bindAddress).getPort() <= 0)
                  {
                     socket = new MulticastSocket((SocketAddress)null);
                     boolean bDone = false;

                     try
                     {
                        socket.setReuseAddress(false);
                        socket.bind(bindAddress);
                        bDone = true;
                     }
                     finally
                     {
                        if (!bDone)
                        {
                           socket.close();
                        }
                     }
                  }
                  else
                  {
                     InetSocketAddress address = (InetSocketAddress)bindAddress;

                     socket = openSocket(address.getAddress(), address.getPort());
                  }

                  return socket;
               }
            });
      }
      catch (PrivilegedActionException e)
      {
         throw e.getException();
      }
   }

   // operations

   /**
    * Sends a datagram to a specified address.
    * @param address The destination address.
    * @param data The data to send.
    * @param nOfs The offset from the beginning of data.
    * @param nCount The count of bytes to send.
    * @param nTOS The RFC 1349 type-of-service.
    * @param nTTL Time-to-live in hops.
    * @throws IOException if an IO error occurs.
    */
   public synchronized void send(final SocketAddress address, final byte[] data,
      final int nOfs, final int nCount, final int nTOS, final int nTTL) throws IOException
   {
      try
      {
         AccessController.doPrivileged(
            new PrivilegedExceptionAction()
            {
               public Object run() throws Exception
               {
                  synchronized (m_socket)
                  {
                     if (nTOS != m_socket.getTrafficClass())
                     {
                        m_socket.setTrafficClass(nTOS);
                     }
   
                     if (nCount > m_socket.getSendBufferSize())
                     {
                        m_socket.setSendBufferSize(nCount);
                     }
   
                     if (m_socket instanceof MulticastSocket)
                     {
                        MulticastSocket socket = (MulticastSocket)m_socket;

                        if (nTTL != socket.getTimeToLive())
                        {
                           socket.setTimeToLive(nTTL);
                        }
                     }

                     if (m_socket.getChannel() != null)
                     {
                        m_socket.getChannel().send(ByteBuffer.wrap(data, nOfs, nCount), address);
                     }
                     else
                     {
                        m_socket.send(new DatagramPacket(data, nOfs, nCount, address));
                     }
                  }

                  return null;
               }
            });
      }
      catch (PrivilegedActionException e)
      {
         Exception x = e.getException();

         if (x instanceof IOException)
         {
            throw (SocketException)x;
         }

         ObjUtil.rethrow(x);
      }
   }

   /**
    * @see nexj.core.util.pool.resource.Resource#reset()
    */
   public void reset()
   {
   }

   /**
    * @see nexj.core.util.pool.resource.GenericResource#drop()
    */
   protected void drop() throws Throwable
   {
      if (m_socket != null)
      {
         closeSocket(m_socket);
         m_socket = null;
      }
   }

   /**
    * Gets a bound socket from the cache and lazy-creating one if needed.
    * @param address The bind address.
    * @param nPort The bind port.
    */
   public static DatagramSocket openSocket(InetAddress address, int nPort) throws IOException
   {
      synchronized (s_socketMap)
      {
         if (s_address == null)
         {
            s_address = new BindAddress(address, nPort);
         }
         else
         {
            s_address.setAddress(address, nPort);
         }

         SocketRef ref = (SocketRef)s_socketMap.get(s_address);

         if (ref == null)
         {
            DatagramChannel channel = DatagramChannel.open();
            DatagramSocket socket;
            boolean bDone = false;

            try
            {
               socket = channel.socket();
               socket.bind(new InetSocketAddress(address, nPort));
               bDone = true;
            }
            finally
            {
               if (!bDone)
               {
                  channel.close();
               }
            }

            ref = new SocketRef(socket, s_address);
            s_socketMap.put(s_address, ref);
            s_socketMap.put(socket, ref);
            s_address = null;
         }

         ref.incRef();

         return ref.getSocket();
      }
   }

   /**
    * Closes a socket and removes it from the cache when the reference count is 0.
    * @param socket The socket to remove from the cache.
    */
   public static void closeSocket(DatagramSocket socket)
   {
      synchronized (s_socketMap)
      {
         SocketRef ref = (SocketRef)s_socketMap.get(socket);

         if (ref != null)
         {
            if (ref.decRef() != 0)
            {
               return;
            }

            s_socketMap.remove(socket);
            s_socketMap.remove(ref.getAddress());
         }
      }

      if (socket.getChannel() != null)
      {
         try
         {
            socket.getChannel().close();
         }
         catch (IOException e)
         {
         }
      }

      socket.close();
   }

   // inner classes

   /**
    * The socket bind address.
    */
   protected static class BindAddress
   {
      /**
       * The socket IP address.
       */
      protected InetAddress m_address;

      /**
       * The port number.
       */
      protected int m_nPort;
      
      /**
       * Constructs the address.
       */
      public BindAddress(InetAddress address, int nPort)
      {
         m_address = address;
         m_nPort = nPort;
      }

      /**
       * Sets the bind address.
       * @param address The socket IP address.
       */
      public void setAddress(InetAddress address, int nPort)
      {
         m_address = address;
         m_nPort = nPort;
      }

      /**
       * @return The socket IP address.
       */
      public InetAddress getAddress()
      {
         return m_address;
      }

      /**
       * @return The port number.
       */
      public int getPort()
      {
         return m_nPort;
      }

      /**
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         if (obj instanceof BindAddress)
         {
            BindAddress addr = (BindAddress)obj;

            return m_nPort == addr.getPort() && m_address.equals(addr.getAddress());
         }

         return false;
      }

      /**
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         return m_address.hashCode() << 10 ^ m_nPort;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return "BindAddress(" + m_address + ':' + m_nPort + ')';
      }
   }

   /**
    * Tracks the reference count of a socket.
    */
   protected static class SocketRef
   {
      /**
       * The socket reference count.
       */
      protected int m_nRefCount;

      /**
       * The socket.
       */
      protected DatagramSocket m_socket;

      /**
       * The bind address.
       */
      protected BindAddress m_address;

      /**
       * Constructs the socket reference.
       */
      public SocketRef(DatagramSocket socket, BindAddress address)
      {
         m_socket = socket;
         m_address = address;
      }

      /**
       * @return The socket.
       */
      public DatagramSocket getSocket()
      {
         return m_socket;
      }

      /**
       * @return The bind address.
       */
      public BindAddress getAddress()
      {
         return m_address;
      }

      /**
       * Increments the reference count.
       */
      public void incRef()
      {
         ++m_nRefCount;
      }

      /**
       * Decrements the reference count.
       * @return The new reference count.
       */
      public int decRef()
      {
         return --m_nRefCount;
      }
   }
}
