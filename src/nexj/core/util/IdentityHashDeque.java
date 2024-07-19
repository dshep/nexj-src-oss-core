// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Fast identity hash set/deque implementation.
 * The focus is on efficiency. No error checking is provided. 
 */
public class IdentityHashDeque extends GenericHashDeque
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 6876205465118684915L;

   // constructors

   /**
    * Creates a hash deque with an estimated number of values.
    * @param nCount The estimated value count.
    */
   public IdentityHashDeque(int nCount)
   {
      super(nCount);
   }

   /**
    * Creates a hash deque with an estimated value count of 8.
    */
   public IdentityHashDeque()
   {
      super();
   }

   // operations

   /**
    * @see nexj.core.util.GenericHashDeque#hash(java.lang.Object)
    */
   protected int hash(Object value)
   {
      return System.identityHashCode(value);
   }

   /**
    * @see nexj.core.util.GenericHashDeque#equal(java.lang.Object, java.lang.Object)
    */
   protected boolean equal(Object left, Object right)
   {
      return (left == right);
   }
}
