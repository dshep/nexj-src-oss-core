// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra.tx;

import java.util.zip.Checksum;

import javax.transaction.xa.Xid;

import nexj.core.rpc.file.ra.LockableFile;
import nexj.core.rpc.ra.tx.GenericJournalRecord;

/**
 * A journal record for an operation that moves an incoming file from one
 * location to another.
 */
public final class FileJournalRecordIncomingMove extends GenericJournalRecord
{
   // constants

   /**
    * The serialization version.
    * @see java.io.Serializable
    */
   private static final long serialVersionUID = -8929160903312060416L;

   /**
    * Operation: Move incoming message file to processed directory.
    */
   public final static int OPCODE = 12;


   // attributes

   /**
    * The path to the incoming file.
    */
   protected String m_sSourceFilePath;

   /**
    * The path to where the incoming file will be moved.
    */
   protected String m_sDestinationFilePath;


   // associations

   /**
    * The handle to the incoming file, if any.
    */
   protected transient LockableFile m_sourceFileHandle;


   // constructors

   /**
    * Creates a new INCOMING_MOVE record.
    * 
    * @param xid          The id of the transaction for this operation.
    * @param sSrcPath     The path to the incoming file.
    * @param sDestPath    The path to where the incoming file will be moved.
    * @param sourceHandle A handle to the incoming file.
    */
   public FileJournalRecordIncomingMove(Xid xid, String sSrcPath, String sDestPath, LockableFile sourceHandle)
   {
      setXid(xid);
      m_sSourceFilePath = sSrcPath;
      m_sDestinationFilePath = sDestPath;
      m_sourceFileHandle = sourceHandle;
   }


   // operations

   /**
    * @return The path to the incoming file.
    */
   public String getSourceFilePath()
   {
      return m_sSourceFilePath;
   }


   /**
    * @return The path to where the incoming file will be moved.
    */
   public String getDestinationFilePath()
   {
      return m_sDestinationFilePath;
   }


   /**
    * @return The handle to the incoming file; null during recovery.
    */
   public LockableFile getSourceFileHandle()
   {
      return m_sourceFileHandle;
   }


   /**
    * @see nexj.core.rpc.ra.tx.GenericJournalRecord#getOpCode()
    */
   public int getOpCode()
   {
      return FileJournalRecordIncomingMove.OPCODE;
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
      
      buf.append("INCOMING_MOVE(xid=");
      buf.append((getXid() != null) ? getXid().toString() : "null");
      buf.append(", src=\"");
      buf.append(m_sSourceFilePath);
      buf.append("\", dst=\"");
      buf.append(m_sDestinationFilePath);
      buf.append("\")");
      
      return buf.toString();
   }
}
