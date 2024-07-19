// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import nexj.core.util.RandUtil;

/**
 * Exception storing a reference identifier for a system exception.
 * Used to report system errors to the end-user without revealing any details. 
 */
public class ReferenceException extends RPCException
{
   // constants

   /**
    * Serialization UID.
    */
   private static final long serialVersionUID = 6545435232122368146L;

   // attributes

   /**
    * The reference id.
    */
   protected long m_lRefId;

   // constructors

   /**
    * Constructs the exception with an automatically generated reference id.
    * @param lRefId The reference id. 
    */
   public ReferenceException()
   {
      this(RandUtil.getSecureRandom().nextLong() & Long.MAX_VALUE);
   }

   /**
    * Constructs the exception.
    * @param lRefId The reference id. 
    */
   public ReferenceException(long lRefId)
   {
      super("err.rpc.reference", new Object[]{String.valueOf(lRefId)});
      m_lRefId = lRefId;
   }

   // operations

   /**
    * @see nexj.core.util.UncheckedException#isSystem()
    */
   public boolean isSystem()
   {
      return false;
   }

   /**
    * Sets the reference id.
    * @param lRefId The reference id to set.
    */
   public void setId(long lRefId)
   {
      m_lRefId = lRefId;
   }

   /**
    * @return The reference id.
    */
   public long getId()
   {
      return m_lRefId;
   }
}
