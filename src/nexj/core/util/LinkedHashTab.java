// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Fast linked hash table implementation.
 * The focus is on efficiency. No error checking is provided. 
 */
public class LinkedHashTab extends GenericLinkedHashTab
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 3314422571757377570L;

   // constructors

   /**
    * Creates a hash table with an estimated number of key-value pairs.
    * @param nCount The estimated key-value pair count.
    */
   public LinkedHashTab(int nCount)
   {
      super(nCount);
   }

   /**
    * Creates a hash table with an estimated key-value pair count of 8.
    */
   public LinkedHashTab()
   {
      super();
   }

   // operations

   /**
    * @see nexj.core.util.GenericLinkedHashTab#hash(java.lang.Object)
    */
   protected int hash(Object key)
   {
      return key.hashCode();
   }

   /**
    * @see nexj.core.util.GenericLinkedHashTab#equal(java.lang.Object, java.lang.Object)
    */
   protected boolean equal(Object left, Object right)
   {
      return left.equals(right);
   }
}
