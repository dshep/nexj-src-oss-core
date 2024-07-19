// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file;

import nexj.core.meta.Repository;
import nexj.core.rpc.IntegrationMDB;
import nexj.core.rpc.ServerException;
import nexj.core.util.Logger;

/**
 * The File message server Message Driven Bean.
 */
public class FileServerMDB extends IntegrationMDB implements FileListener
{
   // constants

   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 1230109727207815113L;

   /**
    * The MDB logger.
    */
   protected final static Logger s_logger = Logger.getLogger(FileServerMDB.class);


   // operations

   /**
    * @see nexj.core.rpc.ServerMDB#getLogger()
    */
   protected Logger getLogger()
   {
      return s_logger;
   }


   /**
    * @see nexj.core.rpc.file.FileListener#onMessage(FileListener.Message)
    */
   public void onMessage(FileMessage msg)
   {
      FileListener receiver = (FileListener)Repository.getMetadata().getChannel(m_sChannelName)
         .getReceiver().getInstance(null);

      try
      {
         receiver.onMessage(msg);
      }
      catch (Throwable e)
      {
         if (!(e instanceof ServerException) && s_logger.isDebugEnabled())
         {
            s_logger.debug("Error in " + this, e);
         }
      }
   }
}
