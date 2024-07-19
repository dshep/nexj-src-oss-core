// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.resource.ResourceException;

import nexj.core.rpc.ra.GenericConnection;
import nexj.core.util.Binary;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * This is a connection handle for working with a single file. It
 * may only be attached to one file over its lifetime.
 * 
 * This object is used in the integration framework for file channel
 * messages and in the persistence framework for the file persistence
 * adapter.
 */
public class FileConnection extends GenericConnection implements nexj.core.rpc.file.FileConnection
{
   // attributes

   /**
    * The key returned by the attachToFile() method of the managed connection. This
    * is passed back to the managed connection for all subsequent operations, to
    * identify to which file the operation applies.
    */
   protected FileManagedConnection.PhysicalConnectionParameters m_parameters;


   // associations

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(FileConnection.class);


   // constructors

   /**
    * Creates a new FileConnection and associates it with the given physical connection.
    * 
    * @param mConn The physical connection.
    */
   public FileConnection(FileManagedConnection mConn)
   {
      m_managedConnection = mConn;
   }


   // operations

   /**
    * @see nexj.core.rpc.file.FileConnection#attachToFile(java.lang.String)
    */
   public boolean attachToFile(String sTargetFile)
   {
      //Ensure not already attached to another file.
      if (m_parameters != null)
      {
         throw new FileConnectionException("err.rpc.file.duplicateAttach",
            new Object[] {sTargetFile});
      }

      m_parameters = ((FileManagedConnection)m_managedConnection).attachToFile(sTargetFile);

      return m_parameters != null;
   }

  /**
    * @see nexj.core.rpc.file.FileConnection#close()
    */
   public void close()
   {
      try
      {
         if (m_parameters != null)
         {
            ((FileManagedConnection)m_managedConnection).finish(m_parameters);
         }
      }
      catch (IOException ex)
      {
         ObjUtil.rethrow(ex);
      }
      finally
      {
         closeHandle();
      }
   }

   /**
    * @see nexj.core.rpc.file.FileConnection#closeHandle()
    */
   public void closeHandle()
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Closing connection: " + this + " (ManagedConnection=" + this.m_managedConnection + ")");
      }

      try
      {
         super.closeHandle();
      }
      catch (ResourceException ex)
      {
         throw new FileConnectionException("err.rpc.file.cannotClose", ex);
      }
   }


   /**
    * @see nexj.core.rpc.file.FileConnection#getInputStream()
    */
   public InputStream getInputStream()
   {
      return ((FileManagedConnection)m_managedConnection).getInputStream(m_parameters);
   }


   /**
    * @see nexj.core.rpc.file.FileConnection#getOutputStream()
    */
   public OutputStream getOutputStream()
   {
      return ((FileManagedConnection)m_managedConnection).getOutputStream(m_parameters);
   }


   /**
    * @see nexj.core.rpc.file.FileConnection#readBinary()
    */
   public synchronized Binary readBinary() throws IOException
   {
      return ((FileManagedConnection)m_managedConnection).readBinary(m_parameters);
   }


   /**
    * @see nexj.core.rpc.file.FileConnection#readString()
    */
   public synchronized String readString() throws IOException
   {
      return ((FileManagedConnection)m_managedConnection).readString(m_parameters);
   }


   /**
    * @see nexj.core.rpc.file.FileConnection#write(nexj.core.util.Binary)
    */
   public synchronized void write(Binary data) throws IOException
   {
      ((FileManagedConnection)m_managedConnection).write(m_parameters, data);
   }


   /**
    * @see nexj.core.rpc.file.FileConnection#write(java.lang.String)
    */
   public synchronized void write(String sData) throws IOException
   {
      ((FileManagedConnection)m_managedConnection).write(m_parameters, sData);
   }


   /**
    * @see nexj.core.rpc.file.FileConnection#delete()
    */
   public void delete() throws IOException
   {
      ((FileManagedConnection)m_managedConnection).delete(m_parameters);
   }


   /**
    * @see nexj.core.rpc.file.FileConnection#getLastModified()
    */
   public long getLastModified()
   {
      return ((FileManagedConnection)m_managedConnection).getLastModified(m_parameters);
   }


   /**
    * @see nexj.core.rpc.file.FileConnection#getLastModifiedThisTxn()
    */
   public long getLastModifiedThisTxn()
   {
      return ((FileManagedConnection)m_managedConnection).getLastModifiedThisTxn(m_parameters);
   }


   /**
    * @see nexj.core.rpc.file.FileConnection#setExpandedProcessedName(java.lang.String)
    */
   public void setExpandedProcessedName(String sExpandedName)
   {
      ((FileManagedConnection)m_managedConnection).setExpandedProcessedName(m_parameters, sExpandedName);
   }
}
