// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import nexj.core.util.Binary;

/**
 * Interface used by file connections.
 */
public interface FileConnection
{
   /**
    * Opens the file of the given filename for transacted read or write, according
    * to the configuration of the associated managed connection.
    * 
    * @param sTargetFile The name of the file to open, relative to the directories for
    *                    this connection.
    * @return True if open was successful; false otherwise.
    */
   public boolean attachToFile(String sTargetFile);

   /**
    * Closes the connection to the file.
    * 
    * Also notifies the appserver that the associated managed connection is free
    * to be recycled.
    */
   public void close();

   /**
    * Gets an input stream to use for reading an incoming file connection.
    * 
    * @return An input stream.
    * @throws IllegalStateException if not an incoming message connection.
    */
   public InputStream getInputStream();

   /**
    * Gets an output stream to use for writing to an outgoing file connection.
    * 
    * @return An output stream.
    * @throws IllegalStateException if not an outgoing message connection.
    */
   public OutputStream getOutputStream();

   /**
    * Sets the file name to use for an incoming message file when it is moved
    * to the "processed" directory (after it has been successfully processed).
    * 
    * This routine is necessary because the processed message file name is
    * generated from a naming template, and the parameters that should be
    * inserted into the template fields are unknown to the file connection.
    * 
    * @param sExpandedName The file name for the processed message file, with
    *                      all template expansions already filled out.
    */
   public void setExpandedProcessedName(String sExpandedName);

   /**
    * Schedules the data file for deletion on commit.
    * 
    * @throws IOException
    * @throws IllegalStateException if not a persistence connection.
    */
   public void delete() throws IOException;

   /**
    * Writes data to the persistence file connection.
    * 
    * @param data The Binary data to write.
    * @throws IOException
    * @throws IllegalStateException if not a persistence or outgoing message connection.
    */
   public void write(Binary data) throws IOException;

   /**
    * Writes data to the persistence file connection.
    * 
    * @param sData The String to write. It will be UTF-8 encoded.
    * @throws IOException
    * @throws IllegalStateException if not a persistence or outgoing message connection.
    */
   public void write(String sData) throws IOException;

   /**
    * Reads data from the persistence file connection.
    * 
    * @return A Binary object containing the data in the file.
    * @throws IOException
    * @throws IllegalStateException if not a persistence connection.
    */
   public Binary readBinary() throws IOException;

   /**
    * Reads data from the persistence file connection.
    * 
    * @return A String object containing the data in the file,
    *         decoded using UTF-8.
    * @throws IOException
    * @throws IllegalStateException if not a persistence connection.
    */
   public String readString() throws IOException;

   /**
    * Gets the last modified time of the data file from the persistence
    * file connection.
    * 
    * @return The last modified time.
    * @throws IllegalStateException if not a persistence connection.
    */
   public long getLastModified();

   /**
    * Gets the last modified time of the temporary file from
    * the persistence file connection.
    * 
    * @return The last modified time.
    * @throws IllegalStateException if not a persistence connection.
    */
   public long getLastModifiedThisTxn();
}
