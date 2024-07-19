// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file;

import java.io.IOException;
import java.util.Collection;
import java.util.Iterator;

import javax.resource.ResourceException;

import nexj.core.integration.Input;
import nexj.core.integration.IntegrationException;
import nexj.core.integration.Sender;
import nexj.core.integration.io.DelayedObjectInput;
import nexj.core.integration.io.DelayedObjectOutput;
import nexj.core.integration.io.ObjectInput;
import nexj.core.integration.io.ObjectOutput;
import nexj.core.meta.integration.Channel;
import nexj.core.integration.io.StreamOutput;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.channel.file.FileChannel;
import nexj.core.monitoring.Counter;
import nexj.core.monitoring.jmx.StatUtil;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.file.ra.FileConnectionException;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.Logger;

/**
 * The message sender for sending messages on a File Channel.
 */
public class FileSender implements Sender, InvocationContextAware
{
   // constants

   /**
    * The key in the transfer object to use for the filename override (when sending)
    * and the incoming filename (when receiving).
    */
   public final static String FILE = "file";

   // attributes

   /**
    * Counter of messages sent since the creation of this component
    */
   protected Counter m_sentCounter = new Counter();

   // associations

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(FileSender.class);

   /**
    * The file channel object on which messages will be sent.
    */
   protected FileChannel m_channel;

   /**
    * The factory for creating file connections for writing outgoing messages.
    * This is bound on initialization and kept throughout the lifetime of 
    * this sender.
    */
   protected FileConnectionFactory m_connectionFactory;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   // operations

   /**
    * @see nexj.core.integration.Sender#createOutput()
    */
   public ObjectOutput createOutput()
   {
      return new DelayedObjectOutput();
   }

   /**
    * @see nexj.core.integration.Sender#prepare(nexj.core.rpc.TransferObject, nexj.core.rpc.TransferObject, nexj.core.meta.integration.Message)
    */
   public void prepare(TransferObject raw, TransferObject tobj, Message message) throws IntegrationException
   {
   }

   /**
    * @see nexj.core.integration.Sender#send(nexj.core.rpc.TransferObject)
    */
   public void send(TransferObject tobj) throws IntegrationException
   {
      long lStartTime = System.nanoTime();
      String sSenderStatPath = m_channel.getSenderStatPath();
      FileConnection conn = null;
      String sOutgoingName = (String)tobj.findValue(FILE);

      if (sOutgoingName == null)
      {
         sOutgoingName = m_channel.getOutgoingName();
      }

      try
      {
         //Expand file name template
         sOutgoingName = FileNameExpander.expandString(sOutgoingName, m_context, null);

         Object body = tobj.findValue(BODY);

         if (body == null)
         {
            throw new IntegrationException("err.rpc.file.missingBody",
               new Object[] {m_channel.getName()});
         }

         conn = m_connectionFactory.getConnection();

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Using file connection: " + conn);
         }

         if (!conn.attachToFile(sOutgoingName))
         {
            throw new IntegrationException("err.rpc.file.cannotSend",
               new Object[] {m_channel.getName(), sOutgoingName});
         }

         m_sentCounter.add(1);
         m_context.addRPCCount(1);
         StatUtil.incrCounter(m_context, sSenderStatPath, Channel.STAT_TOTAL_COUNT, 1);

         if (body instanceof DelayedObjectInput)
         {
            StreamOutput output = new StreamOutput(conn.getOutputStream(), m_channel.getEncoding());

            ((DelayedObjectInput)body).output(output);
            output.close();
         }
         else if (body instanceof Input)
         {
            conn.write(((Input)body).getBinary());
         }
         else
         {
            ObjectInput conv = new ObjectInput(body);
   
            if (m_channel.getEncoding() != null)
            {
               conv.setEncoding(m_channel.getEncoding());
            }

            conn.write(conv.getBinary());
         }
      }
      catch (IOException ex)
      {
         throw new FileConnectionException("err.rpc.file.ioErr", ex);
      }
      catch (ResourceException ex)
      {
         throw new IntegrationException("err.rpc.file.cannotSend",
            new Object[] {m_channel.getName(), sOutgoingName}, ex);
      }
      finally
      {
         if (conn != null)
         {
            conn.close();
         }

         StatUtil.updateAverage(m_context, sSenderStatPath, Channel.STAT_AVERAGE_SEND_TIME,
               (double)(System.nanoTime() - lStartTime) / 1000000);
      }
   }

   /**
    * @see nexj.core.integration.Sender#send(java.util.Collection)
    */
   public void send(Collection col) throws IntegrationException
   {
      for (Iterator itr = col.iterator(); itr.hasNext();)
      {
         send((TransferObject)itr.next());
      }
   }

   /**
    * Sets the channel metadata object.
    * 
    * @param channel The channel metadata object to set.
    */
   public void setChannel(FileChannel channel)
   {
      m_channel = channel;
   }

   /**
    * @return The channel metadata object.
    */
   public FileChannel getChannel()
   {
      return m_channel;
   }

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * Sets the connection factory for this sender. Called automatically when
    * this component is invoked in a new invocation context.
    * 
    * @param factory The connection factory to use.
    */
   public void setConnectionFactory(FileConnectionFactory factory)
   {
      m_connectionFactory = factory;
   }

   /**
    * @see nexj.core.integration.Sender#getSentCount()
    */
   public long getSentCount()
   {
      return m_sentCounter.get();
   }
}
