// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.lang.reflect.Array;

import nexj.core.util.NullableHashTab;

/**
 * HashTab implementation with Intrinsic.equal as equivalence function.
 */
public class EqualHashTab extends NullableHashTab implements HashFunctionHolder
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = -858029964240811708L;

   // constructors

   /**
    * Creates a hash table with an estimated number of key-value pairs.
    * @param nCount The estimated key-value pair count.
    */
   public EqualHashTab(int nCount)
   {
      super(nCount);
   }

   /**
    * Creates a hash table with an estimated key-value pair count of 8.
    */
   public EqualHashTab()
   {
      super();
   }

   // operations

   /**
    * @see nexj.core.util.GenericHashTab#equal(java.lang.Object, java.lang.Object)
    */
   protected boolean equal(Object left, Object right)
   {
      return Intrinsic.equal(left, right);
   }

   /**
    * @see nexj.core.util.GenericHashTab#hash(java.lang.Object)
    */
   protected int hash(Object key)
   {
      return equalHash(key);
   }

   /**
    * Hash function compatible with equal.
    * @param key The object for which a hash code is generated.
    * @return The hash value of key.
    */
   public static int equalHash(Object key)
   {
      if (key == null)
      {
         return 0;
      }

      if (key instanceof Number)
      {
         return EqvHashTab.hash((Number)key);
      }

      if (key.getClass().isArray())
      {
         int nCount = Array.getLength(key);
         int nHashCode = 0;

         for (int i = 0; i < nCount; ++i)
         {
            nHashCode ^= equalHash(Array.get(key, i));
         }

         return nHashCode;
      }

      return key.hashCode();
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getEquivalenceFunction()
    */
   public Object getEquivalenceFunction()
   {
      return Intrinsic.EQUAL_P;
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getHashFunction()
    */
   public Object getHashFunction()
   {
      return Boolean.FALSE;
   }
}
