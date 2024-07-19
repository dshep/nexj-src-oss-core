// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.math.BigDecimal;

import nexj.core.util.NullableHashTab;

/**
 * HashTab implementation with Intrinsic.eqv as equivalence function.
 */
public class EqvHashTab extends NullableHashTab implements HashFunctionHolder
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = 2150922599061266175L;

   // constructors

   /**
    * Creates a hash table with an estimated number of key-value pairs.
    * @param nCount The estimated key-value pair count.
    */
   public EqvHashTab(int nCount)
   {
      super(nCount);
   }

   /**
    * Creates a hash table with an estimated key-value pair count of 8.
    */
   public EqvHashTab()
   {
      super();
   }

   // operations

   /**
    * @see nexj.core.util.GenericHashTab#equal(java.lang.Object, java.lang.Object)
    */
   protected boolean equal(Object left, Object right)
   {
      return Intrinsic.eqv(left, right);
   }

   /**
    * Computes an eqv?-compatible hash code of a number.
    * @param value The value to compute the hash on. 
    * @return The computed hash code.
    */
   public static int hash(Number value)
   {
      double dValue;

      if (value instanceof BigDecimal)
      {
         dValue = ((BigDecimal)value).doubleValue();

         if (dValue == Double.NEGATIVE_INFINITY || dValue == Double.POSITIVE_INFINITY)
         {
            // value doesn't fit into a double
            return ((BigDecimal)value).stripTrailingZeros().hashCode();
         }
      }
      else if (value instanceof Float || value instanceof Double)
      {
         dValue = ((Number)value).doubleValue(); 
      }
      else
      {
         return value.hashCode();
      }

      long lValue = (long)dValue;
      
      if (dValue != lValue)
      {
         lValue = Double.doubleToLongBits(dValue);
      }

      return (int)(lValue) ^ (int)(lValue >> 32);
   }
   
   /**
    * @see nexj.core.util.GenericHashTab#hash(java.lang.Object)
    */
   protected int hash(Object key)
   {
      if (key == null)
      {
         return 0;
      }

      if (key instanceof Number)
      {
         return hash((Number)key);
      }

      return key.hashCode();
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getEquivalenceFunction()
    */
   public Object getEquivalenceFunction()
   {
      return Intrinsic.EQV_P;
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getHashFunction()
    */
   public Object getHashFunction()
   {
      return Boolean.FALSE;
   }
}
