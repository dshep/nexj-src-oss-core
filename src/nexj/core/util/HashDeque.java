// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Fast hash set/deque implementation.
 * The focus is on efficiency. No error checking is provided.
 */
public class HashDeque extends GenericHashDeque
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -8746249559970914541L;

   // constructors

   /**
    * Creates a hash deque with an estimated number of values.
    * @param nCount The estimated value count.
    */
   public HashDeque(int nCount)
   {
      super(nCount);
   }

   /**
    * Creates a hash deque with an estimated value count of 8.
    */
   public HashDeque()
   {
      super();
   }

   // operations

   /**
    * @see nexj.core.util.GenericHashDeque#hash(java.lang.Object)
    */
   protected int hash(Object value)
   {
      return value.hashCode();
   }

   /**
    * @see nexj.core.util.GenericHashDeque#equal(java.lang.Object, java.lang.Object)
    */
   protected boolean equal(Object left, Object right)
   {
      return left.equals(right);
   }
}
