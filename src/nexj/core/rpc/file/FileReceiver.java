// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Locale;

import nexj.core.integration.ContextReceiver;
import nexj.core.integration.Sender;
import nexj.core.integration.io.ReaderInput;
import nexj.core.integration.io.StreamInput;
import nexj.core.meta.integration.channel.file.FileChannel;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.UTF8BOMIgnoreInputStream;

/**
 * The message receiver for receiving messages on a File Channel.
 */
public class FileReceiver extends ContextReceiver implements FileListener
{
   // associations

   /**
    * The File Channel. 
    */
   protected FileChannel m_channel;

   // operations

   /**
    * Sets the channel metadata object.
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
    * @see nexj.core.rpc.file.FileListener#onMessage(java.io.InputStream)
    */
   public void onMessage(final FileMessage message)
   {
      run(new ContextRunnable()
      {
         public boolean isEnabled() throws Throwable
         {
            return true;
         }

         public String getClientAddress() throws Throwable
         {
            return "file:" + message.getOriginalName();
         }

         public String getUser() throws Throwable
         {
            return m_channel.getDefaultUser();
         }

         public void run(InvocationContext context) throws Throwable
         {
            //Generate processed name.
            message.setExpandedProcessedName(
               FileNameExpander.expandString(
                  m_channel.getProcessedName(),
                  context,
                  message.getOriginalName()
                  )
               );
            
            if (isBound(m_channel, context))
            {
               TransferObject tobj = new TransferObject(2);

               tobj.setClassName("File");
               tobj.setValue(FileSender.FILE, message.getOriginalName());

               BufferedInputStream bufStream = new BufferedInputStream(message.getInputStream());

               //Decode channel data
               if (m_channel.getEncoding() != null)
               {
                  Reader ireader;
                  String sEncoding = m_channel.getEncoding().toLowerCase(Locale.ENGLISH);

                  ireader = new BufferedReader(new InputStreamReader(UTF8BOMIgnoreInputStream.wrap(bufStream, sEncoding), m_channel.getEncoding()));
                  
                  tobj.setValue(Sender.BODY, new ReaderInput(ireader));
               }
               else
               {
                  tobj.setValue(Sender.BODY, new StreamInput(bufStream));
               }
               
               receive(tobj, m_channel, context);
            }
         }

         public void err(Throwable t, InvocationContext context) throws Throwable
         {
         }
      }, m_channel, "File");
   }
}
