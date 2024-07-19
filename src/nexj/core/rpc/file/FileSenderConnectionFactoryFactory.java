// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file;

import java.io.File;

import javax.naming.InitialContext;

import nexj.core.meta.integration.channel.file.FileChannel;
import nexj.core.rpc.file.ra.FileManagedConnectionFactory;
import nexj.core.runtime.Factory;
import nexj.core.runtime.platform.generic.GenericConfig;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;

/**
 * Factory for Connection Factory objects, to be used by the File Sender.
 */
public class FileSenderConnectionFactoryFactory implements Factory
{
   // associations

   /**
    * The file channel object on which messages will be sent.
    */
   protected FileChannel m_channel;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(FileSenderConnectionFactoryFactory.class);


   // operations

   /**
    * @see nexj.core.runtime.Factory#create()
    */
   public Object create() throws Exception
   {
      if (J2EEUtil.isContained())
      {
         String sFactory = J2EEUtil.JNDI_ENV_PREFIX + "file/" + m_channel.getName();

         if (s_logger.isInfoEnabled())
         {
            s_logger.info("Binding to connection factory \"" + sFactory + "\"");
         }

         return (FileConnectionFactory)new InitialContext().lookup(sFactory);
      }

      if (s_logger.isInfoEnabled())
      {
         s_logger.info("Creating J2EE-uncontained connection factory for channel \"" + m_channel.getName() + "\"");
      }
      
      FileManagedConnectionFactory mcf = new FileManagedConnectionFactory();
      
      //Configure it according to the settings on the channel.
      mcf.setInputConnection(false);
      mcf.setOutgoingTempDirectory(m_channel.getTemporaryDirectory());
      mcf.setOutgoingDirectory(m_channel.getOutgoingDirectory());
      
      File journalPath = new File(m_channel.getOutgoingJournalPath());
      
      if (!journalPath.isAbsolute())
      {
         GenericConfig platformConfig = new GenericConfig();
         
         journalPath = new File(platformConfig.createDataDirectory(),
            m_channel.getOutgoingJournalPath());
      }

      mcf.setJournalDirectory(journalPath.getAbsolutePath());

      return (FileConnectionFactory)mcf.createConnectionFactory();
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
}
