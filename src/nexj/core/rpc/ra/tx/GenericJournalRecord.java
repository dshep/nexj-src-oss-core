// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra.tx;

import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.util.zip.CRC32;
import java.util.zip.Checksum;

import javax.transaction.xa.Xid;

import nexj.core.util.BinaryUtil;
import nexj.core.util.ObjUtil;


/**
 * A step in a transactional operation, consisting of an XID and an opcode.
 */
public abstract class GenericJournalRecord implements Serializable
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = 2279505013267983381L;

   // attributes

   /**
    * The checksum of this record.
    */
   protected long m_lChecksum;

   /**
    * Whether checksum is set.
    */
   protected boolean m_bChecksumSet;

   // associations

   /**
    * The transaction id in which this operation is being performed.
    */
   private RecoveredXid m_xid;

   // operations

   /**
    * Gets the code representing the operation performed by this
    * transactional step.
    * 
    * @return One of the OP_* constants.
    */
   public abstract int getOpCode();


   /**
    * Gets the transaction id in which this operation is being performed.
    * 
    * @return The XID of this operation.
    */
   public Xid getXid()
   {
      return m_xid;
   }


   /**
    * Sets the transaction id in which this operation is being performed.
    * 
    * @param xid The XID of this operation.
    */
   protected void setXid(Xid xid)
   {
      m_xid = (xid == null) ? null : new RecoveredXid(xid);
   }


   /**
    * For debugging purposes.
    * 
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder();
      
      buf.append("GenericJournalRecord(xid=");
      buf.append((m_xid != null) ? m_xid.toString() : "null");
      buf.append(")");
      
      return buf.toString();
   }


   /**
    * For testing purposes. (The unit test needs to be able to compare
    * GenericJournalRecord instances).
    * 
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (!(obj instanceof GenericJournalRecord))
      {
         return false;
      }
      
      GenericJournalRecord other = (GenericJournalRecord)obj;
      
      if (!m_xid.equals(other.m_xid))
      {
         return false;
      }
      
      return true;
   }

   /**
    * Get whether record is valid by verifying the checksum.
    * @return Whether record is valid.
    */
   public boolean isValid()
   {
      return !m_bChecksumSet || m_lChecksum == getChecksum();
   }

   /**
    * Initialize the checksum.
    */
   public void initChecksum()
   {
      m_lChecksum = getChecksum();
      m_bChecksumSet = true;
   }

   /**
    * Get checksum.
    * @return Checksum.
    */
   protected long getChecksum()
   {
      Checksum checksum = new CRC32();

      updateChecksum(checksum);

      return checksum.getValue();
   }

   /**
    * Update checksum.
    * @param checksum Checksum to update.
    */
   protected void updateChecksum(Checksum checksum)
   {
      byte[] nBuf;

      nBuf = m_xid.getBranchQualifier();
      checksum.update(nBuf, 0, nBuf.length);
      nBuf = m_xid.getGlobalTransactionId();
      checksum.update(nBuf, 0, nBuf.length);
      nBuf = new byte[4];
      BinaryUtil.setInt(nBuf, 0, m_xid.getFormatId());
      checksum.update(nBuf, 0, nBuf.length);
   }

   /**
    * Update checksum with the given string.
    * @param checksum Checksum to update.
    * @param s String.
    */
   protected final static void updateChecksum(Checksum checksum, String s)
   {
      try
      {
         byte[] nBuf = s.getBytes("UTF-8");

         checksum.update(nBuf, 0, nBuf.length);
      }
      catch (UnsupportedEncodingException ex)
      {
         ObjUtil.rethrow(ex);
      }
   }
}
