// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.InetSocketAddress;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.cert.Certificate;
import java.util.ArrayList;
import java.util.Arrays;

import nexj.core.integration.ContextReceiver;
import nexj.core.integration.Input;
import nexj.core.integration.MessageInputStream;
import nexj.core.integration.io.ReaderInput;
import nexj.core.integration.io.StreamInput;
import nexj.core.meta.Component;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.channel.tcp.TCPChannel;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.NoCloseInputStream;
import nexj.core.util.UTF8BOMIgnoreInputStream;
import nexj.core.util.auth.CertificatePrincipalMapper;

/**
 * TCP receiver.
 */
public class TCPReceiver extends ContextReceiver implements TCPListener
{
   // associations

   /**
    * The channel metadata object.
    */
   protected TCPChannel m_channel;

   /**
    * Mapping of InputStream objects (from an open socket)
    * to the corresponding MessageInputStream object
    */
   protected Lookup m_msgInputStreamMap = new HashTab();

   // operations

   /**
    * Sets the channel metadata object.
    * @param channel The TCPChannel metadata object.
    */
   public void setChannel(TCPChannel channel)
   {
      m_channel = channel;
   }

   /**
    * @return The channel metadata object.
    */
   public TCPChannel getChannel()
   {
      return m_channel;
   }

   /**
    * @see nexj.core.rpc.tcp.TCPListener#onMessage(java.io.InputStream, java.net.InetSocketAddress, java.net.InetSocketAddress, java.security.cert.Certificate[])
    */
   public void onMessage(final InputStream in, final InetSocketAddress remoteAddress,
      final InetSocketAddress localAddress, final Certificate[] certificateArray)
   {
      run(new ContextRunnable()
      {
         /**
          * @see nexj.core.integration.ContextReceiver.ContextRunnable#run(nexj.core.runtime.InvocationContext)
          */
         public void run(InvocationContext context) throws Throwable
         {
            if (m_channel.getQueue() != null || isBound(m_channel, context))
            {
               MessageInputStream miStream;

               synchronized (m_msgInputStreamMap)
               {
                  miStream = (MessageInputStream)m_msgInputStreamMap.get(in);

                  if (miStream == null)
                  {
                     miStream = m_channel.getMessageStreamFactory(context).createMessageInputStream(in);
                     m_msgInputStreamMap.put(in, miStream);
                  }
               }

               final TransferObject tobj = new TransferObject("TCP", 7);

               if (m_channel.isResolvingEnabled())
               {
                  AccessController.doPrivileged(new PrivilegedAction()
                  {
                     public Object run()
                     {
                        // do hostname lookups
                        tobj.setValue(TCPSender.REMOTE_HOST, remoteAddress.getAddress().getHostName());
                        tobj.setValue(TCPSender.LOCAL_HOST, localAddress.getAddress().getHostName());

                        return null;
                     }
                  });
               }
               else
               {
                  tobj.setValue(TCPSender.REMOTE_HOST, remoteAddress.getAddress().getHostAddress());
                  tobj.setValue(TCPSender.LOCAL_HOST, localAddress.getAddress().getHostAddress());
               }

               tobj.setValue(TCPSender.REMOTE_PORT, Primitive.createInteger(remoteAddress.getPort()));
               tobj.setValue(TCPSender.LOCAL_PORT, Primitive.createInteger(localAddress.getPort()));
               tobj.setValue(TCPSender.CHANNEL, m_channel.getName());
               tobj.setValue(TCPSender.CLIENT_CERTIFICATES, (certificateArray == null) ? null : new ArrayList(Arrays.asList(certificateArray)));

               try
               {
                  if (miStream.next(tobj))
                  {
                     Object body;

                     // get body
                     InputStream bufStream = new BufferedInputStream(new NoCloseInputStream(miStream));

                     if (m_channel.getEncoding() != null)
                     {
                        String sEncoding = m_channel.getEncoding();
                        Reader ireader = new BufferedReader(new InputStreamReader(
                           UTF8BOMIgnoreInputStream.wrap(bufStream, sEncoding), sEncoding));

                        body = new ReaderInput(ireader);
                     }
                     else
                     {
                        body = new StreamInput(bufStream);
                     }

                     // Is this channel bound to a service
                     // AND a message queue?
                     if (m_channel.getQueue() != null
                           && isBound(m_channel, context))
                     {
                        // Read from the input stream and pass the actual
                        // data to the application
                        Input input = (Input)body;

                        if (m_channel.getEncoding() != null)
                        {
                           body = input.getString();
                        }
                        else
                        {
                           body = input.getBinary();
                        }
                     }

                     if (m_channel.getQueue() != null)
                     {
                        TransferObject msgObj = new TransferObject("MessageQueue", 1);

                        msgObj.setValue(TCPSender.BODY, body);
                        context.getUnitOfWork().addMessage(m_channel.getQueue(), msgObj);
                     }

                     if (isBound(m_channel, context))
                     {
                        tobj.setValue(TCPSender.BODY, body);
                        receive(tobj, m_channel, context);
                     }
                  }
                  else
                  {
                     miStream.close();

                     synchronized (m_msgInputStreamMap)
                     {
                        m_msgInputStreamMap.remove(in);
                     }
                  }
               }
               catch (Throwable t)
               {
                  miStream.close();

                  synchronized (m_msgInputStreamMap)
                  {
                     m_msgInputStreamMap.remove(in);
                  }

                  throw t;
               }
            }
         }

         /**
          * @see nexj.core.integration.ContextReceiver.ContextRunnable#isEnabled()
          */
         public boolean isEnabled() throws Throwable
         {
            return true;
         }

         /**
          * @see nexj.core.integration.ContextReceiver.ContextRunnable#getUser()
          */
         public String getUser() throws Throwable
         {
            String sDefaultUser = m_channel.getDefaultUser();

            if (!m_channel.isSecure())
            {
               return sDefaultUser;
            }

            Component mapper = m_channel.getMapper();

            if (mapper == null)
            {
               return sDefaultUser;
            }

            String sMappedUser = ((CertificatePrincipalMapper)mapper.getInstance(null)).getPrincipal(certificateArray);

            if (sMappedUser == null)
            {
               return sDefaultUser;
            }

            return sMappedUser;
         }

         /**
          * @see nexj.core.integration.ContextReceiver.ContextRunnable#getClientAddress()
          */
         public String getClientAddress() throws Throwable
         {
            InetSocketAddress iaddr = (InetSocketAddress)remoteAddress;

            return "tcp:" + iaddr.getAddress().getHostAddress() + ':' + iaddr.getPort();
         }

         /**
          * @see nexj.core.integration.ContextReceiver.ContextRunnable#err(java.lang.Throwable, nexj.core.runtime.InvocationContext)
          */
         public void err(Throwable t, InvocationContext context) throws Throwable
         {
         }
      }, m_channel, "TCP");
   }
}
