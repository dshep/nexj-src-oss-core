// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file;

import java.io.InputStream;

/**
 * Interface to allow callback into the connection to set the
 * name for a file to be used after the file has been processed.
 */
public interface FileMessage
{
   /**
    * Gets the input stream from which to read the message file data.
    * 
    * @return Input stream connected to the incoming message file.
    */
   public InputStream getInputStream();

   /**
    * Callback to the FileConnection to allow the FileReceiver to set
    * the name to be used for the file after it has been processed. The
    * expanded processed name is a path and filename relative to the
    * processed message directory.
    * 
    * @param sExpandedName The name to use for the processed file, as a
    *                      relative path rooted in the processed
    *                      message directory.
    */
   public void setExpandedProcessedName(String sExpandedName);

   /**
    * Gets the original file name. Used in template expansion to construct
    * a processed file name that is based on the original file name. It is a
    * path that is relative to the incoming message directory.
    * 
    * @return The original name of the incoming message file, as a relative
    *         path rooted in the incoming message directory.
    */
   public String getOriginalName();
}
