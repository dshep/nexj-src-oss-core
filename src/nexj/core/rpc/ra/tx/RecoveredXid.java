// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra.tx;

import java.io.Serializable;

import javax.transaction.xa.Xid;

import nexj.core.util.Binary;

/**
 * Xid implementation for XIDs that can be serialized to and deserialized from
 * a transactional journal.
 */
public final class RecoveredXid implements Xid, Comparable, Serializable
{
   // constants

   /**
    * The serialization version.
    */
   private static final long serialVersionUID = 7433733536689651720L;

   /**
    * The maximum space needed to store an Xid, in bytes.
    */
   public final static int MAX_SIZE = 4 + Xid.MAXBQUALSIZE + 4 + Xid.MAXGTRIDSIZE + 4;


   // attributes

   /**
    * The transaction id.
    */
   protected byte[] m_txId;

   /**
    * The branch qualifier.
    */
   protected byte[] m_brq;

   /**
    * The format id.
    */
   protected int m_nFormatId;

   /**
    * The hash code of the Xid.
    */
   protected int m_nHashCode;


   // constructors

   /**
    * Constructs a new Xid.
    * 
    * @param txId      The transaction Id.
    * @param brq       The branch qualifier.
    * @param nFormatId The format Id.
    */
   public RecoveredXid(byte[] txId, byte[] brq, int nFormatId)
   {
      m_txId = txId;
      m_brq = brq;
      m_nFormatId = nFormatId;
      
      computeHash();
   }


   /**
    * Copy constructor to create a RecoveredXid instance from any Xid.
    * 
    * @param src The Xid to copy.
    */
   public RecoveredXid(Xid src)
   {
      m_nFormatId = src.getFormatId();
      
      byte[] nSourceBytes;
      
      nSourceBytes = src.getBranchQualifier();
      m_brq = new byte[nSourceBytes.length];
      System.arraycopy(nSourceBytes, 0, m_brq, 0, nSourceBytes.length);
      
      nSourceBytes = src.getGlobalTransactionId();
      m_txId = new byte[nSourceBytes.length];
      System.arraycopy(nSourceBytes, 0, m_txId, 0, nSourceBytes.length);
      
      computeHash();
   }


   // operations

   /**
    * @see javax.transaction.xa.Xid#getFormatId()
    */
   public int getFormatId()
   {
      return m_nFormatId;
   }


   /**
    * @see javax.transaction.xa.Xid#getGlobalTransactionId()
    */
   public byte[] getGlobalTransactionId()
   {
      return m_txId;
   }


   /**
    * @see javax.transaction.xa.Xid#getBranchQualifier()
    */
   public byte[] getBranchQualifier()
   {
      return m_brq;
   }


   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (this == obj)
      {
         return true;
      }
      
      if (!(obj instanceof Xid))
      {
         return false;
      }
      
      Xid xid = (Xid)obj;
      
      return m_nFormatId == xid.getFormatId() &&
         Binary.compare(m_txId, xid.getGlobalTransactionId()) == 0 &&
         Binary.compare(m_brq, xid.getBranchQualifier()) == 0;
   }


   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return m_nHashCode;
   }


   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(128);
      
      buf.append("RecoveredXid(globalTx=");
      Binary.append(buf, m_txId, -1);
      buf.append(", brQualifier=");
      Binary.append(buf, m_brq, -1);
      buf.append(", formatId=0x");
      buf.append(Integer.toHexString(m_nFormatId));
      buf.append(")");
      
      return buf.toString();
   }


   /**
    * The compare method is used for testing and debugging.
    * 
    * @see java.lang.Comparable#compareTo(java.lang.Object)
    */
   public int compareTo(Object obj)
   {
      int nResult = 0;
      
      RecoveredXid other = (RecoveredXid)obj;
      
      if (this.m_nFormatId > other.m_nFormatId)
      {
         return 1;
      }
      else if(this.m_nFormatId < other.m_nFormatId)
      {
         return -1;
      }
      
      
      nResult = Binary.compare(this.m_txId, other.m_txId);
      
      if (nResult != 0)
      {
         return nResult;
      }
      
      return Binary.compare(this.m_brq, other.m_brq);
   }


   /**
    * Computes the hash code and caches it in this instance.
    */
   protected void computeHash()
   {
      m_nHashCode = Binary.hashCode(m_txId) ^ Binary.hashCode(m_brq) ^ m_nFormatId;
   }
}
