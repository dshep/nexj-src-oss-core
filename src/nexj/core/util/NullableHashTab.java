// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Hash table that accepts null keys.
 */
public abstract class NullableHashTab extends GenericHashTab
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = 2178132441876130318L;

   /**
    * Unique object representing the null key.
    */
   private final static Object NULL_KEY = Null.VALUE;

   // constructors

   /**
    * Creates a hash table that accepts null keys with an estimated number of key-value pairs.
    * @param nCount The estimated key-value pair count.
    */
   protected NullableHashTab(int nCount)
   {
      super(nCount);
   }

   /**
    * Creates a hash table that accepts null keys with an estimated key-value pair count of 8.
    */
   protected NullableHashTab()
   {
      super();
   }

   // operations

   /**
    * @see nexj.core.util.GenericHashTab#contains(java.lang.Object)
    */
   public boolean contains(Object key)
   {
      return super.contains((key == null) ? NULL_KEY : key);
   }

   /**
    * @see nexj.core.util.GenericHashTab#put(java.lang.Object, java.lang.Object)
    */
   public Object put(Object key, Object value)
   {
      return super.put((key == null) ? NULL_KEY : key, value);
   }

   /**
    * @see nexj.core.util.GenericHashTab#get(java.lang.Object)
    */
   public Object get(Object key)
   {
      return super.get((key == null) ? NULL_KEY : key);
   }

   /**
    * @see nexj.core.util.GenericHashTab#remove(java.lang.Object)
    */
   public Object remove(Object key)
   {
      return super.remove((key == null) ? NULL_KEY : key);
   }

   /**
    * @see nexj.core.util.GenericHashTab#iterator()
    */
   public Iterator iterator()
   {
      return new NullableHashTabIterator();
   }

   /**
    * @see nexj.core.util.GenericHashTab#valueIterator()
    */
   public Iterator valueIterator()
   {
      return new NullableHashTabValueIterator();
   }

   // inner classes

   /**
    * The same as GenericHashTabIterator, but can handle null keys.
    */
   protected class NullableHashTabIterator extends GenericHashTabIterator
   {
      /**
       * @see nexj.core.util.GenericHashTab.GenericHashTabIterator#next()
       */
      public Object next()
      {
         Object key = super.next();

         return (key == NULL_KEY) ? null : key;
      }

      /**
       * @see nexj.core.util.GenericHashTab.GenericHashTabIterator#getKey()
       */
      public Object getKey()
      {
         Object key = super.getKey();

         return (key == NULL_KEY) ? null : key;
      }
   }

   /**
    * The same as the GenericNullKeyHashTabIterator, but next() returns the value.
    */
   protected class NullableHashTabValueIterator extends GenericHashTabValueIterator
   {
      /**
       * @see nexj.core.util.GenericHashTab.GenericHashTabIterator#next()
       */
      public Object next()
      {
         Object key = super.next();

         return (key == NULL_KEY) ? null : key;
      }

      /**
       * @see nexj.core.util.GenericHashTab.GenericHashTabIterator#getKey()
       */
      public Object getKey()
      {
         Object key = super.getKey();

         return (key == NULL_KEY) ? null : key;
      }
   }
}
