// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra.tx;

import javax.transaction.xa.Xid;

/**
 * The journal record used to indicate that a transaction has been completed.
 */
public final class JournalRecordCompleted extends GenericJournalRecord
{
   // constants

   /**
    * The serialization version.
    * @see java.io.Serializable
    */
   private static final long serialVersionUID = 9048066914495005913L;

   /**
    * Operation: Marks this XID as completed.
    */
   public final static int OPCODE = 2;


   // constructors

   /**
    * Creates a new record used to indicate that processing for the given
    * transaction id has been completed.
    * 
    * @param xid The transaction id that has been completed.
    */
   public JournalRecordCompleted(Xid xid)
   {
      setXid(xid);
   }


   // operations

   /**
    * @see nexj.core.rpc.ra.tx.GenericJournalRecord#getOpCode()
    */
   public int getOpCode()
   {
      return JournalRecordCompleted.OPCODE;
   }


   /**
    * For debugging purposes.
    * 
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder();
      
      buf.append("COMPLETED(xid=");
      buf.append((getXid() != null) ? getXid().toString() : "null");
      buf.append(")");
      
      return buf.toString();
   }
}
