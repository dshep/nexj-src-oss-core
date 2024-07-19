// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

/**
 * Query timeout exception.
 */
public class QueryTimeoutException extends PersistenceException
{
   // constants

   /**
    * The serialization UID.
    */
   private final static long serialVersionUID = -4226088232477791671L;

   // constructors

   public QueryTimeoutException()
   {
      super("err.persistence.queryTimeout");
   }

   public QueryTimeoutException(Throwable cause)
   {
      super("err.persistence.queryTimeout", cause);
   }

   // operations

   /**
    * @see nexj.core.util.ErrorCode#isSystem()
    */
   public boolean isSystem()
   {
      return false;
   }
}
