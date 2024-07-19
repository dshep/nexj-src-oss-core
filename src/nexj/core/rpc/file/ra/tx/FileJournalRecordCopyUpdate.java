// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra.tx;

import java.util.zip.Checksum;

import javax.transaction.xa.Xid;

import nexj.core.rpc.file.ra.LockableFile;
import nexj.core.rpc.ra.tx.GenericJournalRecord;

/**
 * A journal record for an operation that copies the contents of a source
 * file over top of an existing destination file.
 */
public final class FileJournalRecordCopyUpdate extends GenericJournalRecord
{
   // constants

   /**
    * The serialization version.
    * @see java.io.Serializable
    */
   private static final long serialVersionUID = 4094443672971410046L;

   /**
    * Operation: Copy temporary file over an existing persisted file.
    */
   public final static int OPCODE = 14;


   // attributes

   /**
    * The path to the file containing the data to be copied.
    */
   protected String m_sSourceFilePath;

   /**
    * The path to the destination file to where the data will be copied.
    */
   protected String m_sDestinationFilePath;


   // associations

   /**
    * The handle to the source file, if any.
    */
   protected transient LockableFile m_sourceFileHandle;

   /**
    * The handle to the destination file, if any.
    */
   protected transient LockableFile m_destinationFileHandle;


   // constructors

   /**
    * Creates a new COPY_UPDATE record.
    * 
    * @param xid          The id of the transaction for this operation.
    * @param sSrcPath     The path to the file containg the data to copy.
    * @param sDestPath    The path to the file where the data will be copied.
    * @param sourceHandle A handle to the source file.
    * @param destHandle   A handle to the destination file.
    */
   public FileJournalRecordCopyUpdate(Xid xid, String sSrcPath, String sDestPath, LockableFile sourceHandle, LockableFile destHandle)
   {
      setXid(xid);
      m_sSourceFilePath = sSrcPath;
      m_sDestinationFilePath = sDestPath;
      m_sourceFileHandle = sourceHandle;
      m_destinationFileHandle = destHandle;
   }


   // operations

   /**
    * @return The path to the file containing the data to be copied.
    */
   public String getSourceFilePath()
   {
      return m_sSourceFilePath;
   }


   /**
    * @return The path to the destination file to where the data will be copied.
    */
   public String getDestinationFilePath()
   {
      return m_sDestinationFilePath;
   }


   /**
    * @return The handle to the source file; null during recovery.
    */
   public LockableFile getSourceFileHandle()
   {
      return m_sourceFileHandle;
   }


   /**
    * @return The handle to the destination file; null during recovery.
    */
   public LockableFile getDestinationFileHandle()
   {
      return m_destinationFileHandle;
   }


   /**
    * @see nexj.core.rpc.ra.tx.GenericJournalRecord#getOpCode()
    */
   public int getOpCode()
   {
      return FileJournalRecordCopyUpdate.OPCODE;
   }

   /**
    * @see nexj.core.rpc.ra.tx.GenericJournalRecord#updateChecksum(java.util.zip.Checksum)
    */
   protected void updateChecksum(Checksum cksum)
   {
      super.updateChecksum(cksum);

      updateChecksum(cksum, m_sSourceFilePath);
      updateChecksum(cksum, m_sDestinationFilePath);
   }

   /**
    * For debugging purposes.
    * 
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder();
      
      buf.append("COPY_UPDATE(xid=");
      buf.append((getXid() != null) ? getXid().toString() : "null");
      buf.append(", src=\"");
      buf.append(m_sSourceFilePath);
      buf.append("\", dst=\"");
      buf.append(m_sDestinationFilePath);
      buf.append("\")");
      
      return buf.toString();
   }
}
