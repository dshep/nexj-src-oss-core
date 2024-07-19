// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import nexj.core.util.NullableHashTab;

/**
 * HashTab implementation with Intrinsic.eq as equivalence function.
 */
public class EqHashTab extends NullableHashTab implements HashFunctionHolder
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = -2665964727831695997L;

   // constructors

   /**
    * Creates a hash table with an estimated number of key-value pairs.
    * @param nCount The estimated key-value pair count.
    */
   public EqHashTab(int nCount)
   {
      super(nCount);
   }

   /**
    * Creates a hash table with an estimated key-value pair count of 8.
    */
   public EqHashTab()
   {
      super();
   }

   // operations

   /**
    * @see nexj.core.util.GenericHashTab#equal(java.lang.Object, java.lang.Object)
    */
   protected boolean equal(Object left, Object right)
   {
      return Intrinsic.eq(left, right);
   }

   /**
    * @see nexj.core.util.GenericHashTab#hash(java.lang.Object)
    */
   protected int hash(Object key)
   {
      return (key == null) ? 0 : key.hashCode();
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getEquivalenceFunction()
    */
   public Object getEquivalenceFunction()
   {
      return Intrinsic.EQ_P;
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getHashFunction()
    */
   public Object getHashFunction()
   {
      return Boolean.FALSE;
   }
}
