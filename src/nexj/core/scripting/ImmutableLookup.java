// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.io.IOException;
import java.io.Serializable;

import nexj.core.util.HashTab;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Lookup;
import nexj.core.util.PrintWriter;
import nexj.core.util.Printable;

/**
 * Immutable decorator for Lookup objects.
 */
public class ImmutableLookup implements Lookup, Cloneable, Serializable, Printable, HashFunctionHolder
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = 1694388794523837763L;

   // associations

   /**
    * The Lookup object.
    */
   protected Lookup m_map;

   // constructors

   /**
    * Constructs a new ImmutableLookup wrapping the given Lookup.
    * @param map The Lookup object.
    */
   public ImmutableLookup(Lookup map)
   {
      m_map = map;
   }

   // operations

   /**
    * @see nexj.core.util.Lookup#clear()
    */
   public void clear()
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * Clears the table and resets its capacity to the given number.
    * @param nSize The estimated key-value pairs count.
    */
   public void clear(int nSize)
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see nexj.core.util.Lookup#contains(java.lang.Object)
    */
   public boolean contains(Object key)
   {
      return m_map.contains(key);
   }

   /**
    * @see nexj.core.util.Lookup#get(java.lang.Object)
    */
   public Object get(Object key)
   {
      return m_map.get(key);
   }

   /**
    * @see nexj.core.util.Lookup#iterator()
    */
   public Iterator iterator()
   {
      return new ImmutableLookupIterator(m_map.iterator());
   }

   /**
    * @see nexj.core.util.Lookup#put(java.lang.Object, java.lang.Object)
    */
   public Object put(Object key, Object value)
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see nexj.core.util.Lookup#remove(java.lang.Object)
    */
   public Object remove(Object key)
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see nexj.core.util.Lookup#size()
    */
   public int size()
   {
      return m_map.size();
   }

   /**
    * @see nexj.core.util.Lookup#valueIterator()
    */
   public Iterator valueIterator()
   {
      return new ImmutableLookupIterator(m_map.valueIterator());
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.print(m_map);
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         return (ImmutableLookup)super.clone();
      }
      catch (CloneNotSupportedException e)
      {
         return null;
      }
   }

   /**
    * @return A mutable copy of this hash table.
    */
   public Object mutableClone()
   {
      return m_map.clone();
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getEquivalenceFunction()
    */
   public Object getEquivalenceFunction()
   {
      if (m_map instanceof HashFunctionHolder)
      {
         return ((HashFunctionHolder)m_map).getEquivalenceFunction();
      }

      if (m_map instanceof HashTab)
      {
         return Intrinsic.SYS_EQUAL_P;
      }

      if (m_map instanceof IdentityHashTab)
      {
         return Intrinsic.SYS_EQ_P;
      }

      return Boolean.FALSE;
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getHashFunction()
    */
   public Object getHashFunction()
   {
      if (m_map instanceof HashFunctionHolder)
      {
         return ((HashFunctionHolder)m_map).getHashFunction();
      }

      return Boolean.FALSE;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return PrintWriter.toString(this);
   }

   /**
    * ImmutableLookup iterator - can return the current key and the value
    * but cannot modify the underlying Lookup.
    * NOTE: This iterator does not check for errors for efficiency reasons.
    */
   protected static class ImmutableLookupIterator implements Iterator
   {
      // associations

      /**
       * The Iterator object of the underlying Lookup hash table.
       */
      protected Iterator m_itr;

      // constructors

      /**
       * Creates an iterator.
       */
      protected ImmutableLookupIterator(Iterator itr)
      {
         m_itr = itr;
      }

      // operations

      /**
       * @see java.util.Iterator#hasNext()
       */
      public boolean hasNext()
      {
         return m_itr.hasNext();
      }

      /**
       * @return The next available element key.
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         return m_itr.next();
      }

      /**
       * @see java.util.Iterator#remove()
       */
      public void remove()
      {
         throw new ScriptingException("err.scripting.readOnlyObject");
      }

      /**
       * @see nexj.core.util.Lookup.Iterator#getKey()
       */
      public Object getKey()
      {
         return m_itr.getKey();
      }

      /**
       * @see nexj.core.util.Lookup.Iterator#getValue()
       */
      public Object getValue()
      {
         return m_itr.getValue();
      }

      /**
       * @see nexj.core.util.Lookup.Iterator#setValue(java.lang.Object)
       */
      public void setValue(Object value)
      {
         throw new ScriptingException("err.scripting.readOnlyObject");
      }
   }
}
