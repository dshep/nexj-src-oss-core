// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra.tx;

import javax.transaction.xa.Xid;

/**
 * The journal record used to indicate that a transaction has been prepared
 * and therefore requires a transactional outcome.
 */
public final class JournalRecordPrepared extends GenericJournalRecord
{
   // constants

   /**
    * The serialization version.
    * @see java.io.Serializable
    */
   private static final long serialVersionUID = 3511629123448872559L;
   /**
    * Operation: Marks this XID as successfully prepared.
    */
   public final static int OPCODE = 1;


   // constructors

   /**
    * Creates a new record used to indicate that the given transaction
    * id has entered a state where it requires a transactional outcome (it
    * is prepared).
    * 
    * @param xid The transaction id that has been prepared.
    */
   public JournalRecordPrepared(Xid xid)
   {
      setXid(xid);
   }


   // operations

   /**
    * @see nexj.core.rpc.ra.tx.GenericJournalRecord#getOpCode()
    */
   public int getOpCode()
   {
      return JournalRecordPrepared.OPCODE;
   }


   /**
    * For debugging purposes.
    * 
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder();
      
      buf.append("PREPARED(xid=");
      buf.append((getXid() != null) ? getXid().toString() : "null");
      buf.append(")");
      
      return buf.toString();
   }
}
