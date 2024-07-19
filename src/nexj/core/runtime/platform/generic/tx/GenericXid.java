// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.platform.generic.tx;

import javax.transaction.xa.Xid;

import nexj.core.util.Binary;

/**
 * Xid implementation.
 */
public class GenericXid implements Xid, java.io.Serializable
{
   // constants

   /**
    * The serialization UID.
    */
   private final static long serialVersionUID = 4655719520169481824L;

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
    * The hash code of the Xid.
    */
   protected int m_nHashCode;

   // constructors

   /**
    * Constructs a new Xid.
    * @param txId The transaction Id.
    * @param brq The branch qualifier.
    */
   public GenericXid(byte[] txId, byte[] brq)
   {
      m_txId = txId;
      m_brq = brq;
      m_nHashCode = Binary.hashCode(txId) ^ Binary.hashCode(brq);
   }

   // operations

   /**
    * @see javax.transaction.xa.Xid#getFormatId()
    */
   public int getFormatId()
   {
      return 'N' << 8 | 'J';
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
      
      if (!(obj instanceof GenericXid))
      {
         return false;
      }
      
      GenericXid xid = (GenericXid)obj;
      
      if (m_nHashCode != xid.m_nHashCode)
      {
         return false;
      }

      return Binary.compare(m_txId, xid.m_txId) == 0 &&
         Binary.compare(m_brq, xid.m_brq) == 0;
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
      
      buf.append("Xid(");
      Binary.append(buf, m_txId, -1);
      buf.append(", ");
      Binary.append(buf, m_brq, -1);
      buf.append(")");
      
      return buf.toString();
   }
}
