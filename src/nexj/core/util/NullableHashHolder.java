// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Iterator;

/**
 * Hash holder that accepts null keys.
 */
public abstract class NullableHashHolder extends GenericHashHolder
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = -2838507716810604835L;

   /**
    * Unique object representing the null value.
    */
   private final static Object NULL_VALUE = Null.VALUE;

   // constructors

   /**
    * Creates a hash holder with an estimated number of values.
    * @param nCount The estimated value count.
    */
   public NullableHashHolder(int nCount)
   {
      super(nCount);
   }

   /**
    * Creates a hash holder with an estimated value count of 8.
    */
   public NullableHashHolder()
   {
      super();
   }

   // operations

   /**
    * @see nexj.core.util.Holder#add(java.lang.Object)
    */
   public boolean add(Object value)
   {
      return super.add((value == null) ? NULL_VALUE : value);
   }

   /**
    * @see nexj.core.util.Holder#contains(java.lang.Object)
    */
   public boolean contains(Object value)
   {
      return super.contains((value == null) ? NULL_VALUE : value);
   }

   /**
    * @see nexj.core.util.Holder#get(java.lang.Object)
    */
   public Object get(Object value)
   {
      return super.get((value == null) ? NULL_VALUE : value);
   }

   /**
    * @see nexj.core.util.Holder#remove(java.lang.Object)
    */
   public boolean remove(Object value)
   {
      return super.remove((value == null) ? NULL_VALUE : value);
   }

   /**
    * @see nexj.core.util.GenericHashHolder#iterator()
    */
   public Iterator iterator()
   {
      return new NullableHashHolderIterator();
   }

   // inner classes

   protected class NullableHashHolderIterator extends GenericHashHolderIterator
   {
      /**
       * @see nexj.core.util.GenericHashHolder.GenericHashHolderIterator#next()
       */
      public Object next()
      {
         Object value = super.next();

         return (value == NULL_VALUE) ? null : value;
      }
   }
}
