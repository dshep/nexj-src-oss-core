// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra.tx;

import java.util.zip.Checksum;

import javax.transaction.xa.Xid;

import nexj.core.rpc.file.ra.LockableFile;
import nexj.core.rpc.ra.tx.GenericJournalRecord;

/**
 * A journal record for an operation that deletes an incoming message file.
 */
public final class FileJournalRecordIncomingDelete extends GenericJournalRecord
{
   // constants

   /**
    * The serialization version.
    * @see java.io.Serializable
    */
   private static final long serialVersionUID = -1243181448178312733L;

   /**
    * Operation: Delete incoming message file.
    */
   public final static int OPCODE = 13;


   // attributes

   /**
    * The path to the incoming message file.
    */
   protected String m_sFilePath;


   // associations

   /**
    * The handle to the incoming message file, if any.
    */
   protected transient LockableFile m_fileHandle;


   // constructors

   /**
    * Creates a new INCOMING_DELETE record.
    * 
    * @param xid        The id of the transaction for this operation.
    * @param sFilePath  The path to the incoming file.
    * @param fileHandle The handle to the incoming file.
    */
   public FileJournalRecordIncomingDelete(Xid xid, String sFilePath, LockableFile fileHandle)
   {
      setXid(xid);
      m_sFilePath = sFilePath;
      m_fileHandle = fileHandle;
   }


   // operations

   /**
    * @return The path to the incoming message file.
    */
   public String getFilePath()
   {
      return m_sFilePath;
   }


   /**
    * @return The handle to the incoming message file; null during recovery.
    */
   public LockableFile getFileHandle()
   {
      return m_fileHandle;
   }


   /**
    * @see nexj.core.rpc.ra.tx.GenericJournalRecord#getOpCode()
    */
   public int getOpCode()
   {
      return FileJournalRecordIncomingDelete.OPCODE;
   }

   /**
    * @see nexj.core.rpc.ra.tx.GenericJournalRecord#updateChecksum(java.util.zip.Checksum)
    */
   protected void updateChecksum(Checksum cksum)
   {
      super.updateChecksum(cksum);

      updateChecksum(cksum, m_sFilePath);
   }

   /**
    * For debugging purposes.
    * 
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder();
      
      buf.append("INCOMING_DELETE(xid=");
      buf.append((getXid() != null) ? getXid().toString() : "null");
      buf.append(", file=\"");
      buf.append(m_sFilePath);
      buf.append("\")");
      
      return buf.toString();
   }
}
