// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.udp;

import java.net.DatagramPacket;
import java.net.InetSocketAddress;

import nexj.core.integration.ContextReceiver;
import nexj.core.integration.Sender;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.channel.udp.UDPChannel;
import nexj.core.monitoring.jmx.StatManager;
import nexj.core.monitoring.jmx.StatManagerAware;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.pool.Processor;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.Binary;

/**
 * UDP receiver.
 */
public class UDPReceiver extends ContextReceiver implements Processor, StatManagerAware
{
   // associations

   /**
    * The channel metadata object.
    */
   protected UDPChannel m_channel;

   // operations

   /**
    * Sets the channel metadata object.
    * @param channel The channel metadata object to set.
    */
   public void setChannel(UDPChannel channel)
   {
      m_channel = channel;
   }

   /**
    * @return The channel metadata object.
    */
   public UDPChannel getChannel()
   {
      return m_channel;
   }

   /**
    * @see nexj.core.monitoring.jmx.StatManagerAware#setStatManager(nexj.core.monitoring.jmx.StatManager)
    */
   public void setStatManager(StatManager statManager)
   {
      initStat(m_channel, statManager);
   }

   /**
    * @see nexj.core.rpc.pool.Processor#process(java.lang.Object)
    */
   public void process(Object request) throws Throwable
   {
      final DatagramPacket packet = (DatagramPacket)request;

      run(new ContextRunnable()
      {
         public boolean isEnabled() throws Throwable
         {
            return true;
         }

         public String getClientAddress() throws Throwable
         {
            InetSocketAddress iaddr = (InetSocketAddress)packet.getSocketAddress();

            return "udp:" + iaddr.getAddress().getHostAddress() + ':' + iaddr.getPort();
         }

         public String getUser() throws Throwable
         {
            return m_channel.getDefaultUser();
         }

         public void run(InvocationContext context) throws Throwable
         {
            byte[] data = packet.getData();
            int nOfs = packet.getOffset();
            int nCount = packet.getLength();
            Object body;
            
            if (m_channel.getEncoding() != null)
            {
               body = new String(data, nOfs, nCount, m_channel.getEncoding());
            }
            else
            {
               if (nOfs == 0 && nCount == data.length) 
               {
                  body = new Binary(data);
               }
               else
               {
                  byte[] buf = new byte[nCount];

                  System.arraycopy(data, nOfs, buf, 0, nCount);
                  body = new Binary(buf);
               }
            }

            if (m_channel.getQueue() != null)
            {
               TransferObject tobj = new TransferObject(1);

               tobj.setClassName("MessageQueue");
               tobj.setValue(Sender.BODY, body);
               context.getUnitOfWork().addMessage(m_channel.getQueue(), tobj);
            }

            TransferObject tobj = new TransferObject(4);
            InetSocketAddress iaddr = (InetSocketAddress)packet.getSocketAddress();

            tobj.setClassName("UDP");
            tobj.setValue(UDPSender.HOST, iaddr.getAddress().getHostAddress());
            tobj.setValue(UDPSender.PORT, Primitive.createInteger(iaddr.getPort()));
            tobj.setValue(UDPSender.CHANNEL, m_channel.getName());
            tobj.setValue(UDPSender.BODY, body);

            receive(tobj, m_channel, context);
         }

         public void err(Throwable t, InvocationContext context) throws Throwable
         {
         }
      }, m_channel, "UDP");
   }
}
