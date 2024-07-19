// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.io.IOException;
import java.io.Serializable;
import java.util.Collection;
import java.util.Iterator;

import nexj.core.util.HashHolder;
import nexj.core.util.IdentityHashHolder;
import nexj.core.util.Holder;
import nexj.core.util.PrintWriter;
import nexj.core.util.Printable;

/**
 * Immutable decorator for Holder objects.
 */
public class ImmutableHolder implements Holder, Cloneable, Serializable, Printable, HashFunctionHolder
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = 1694388794523837763L;

   // associations

   /**
    * The Holder object.
    */
   protected Holder m_set;

   // constructors

   /**
    * Constructs a new ImmutableHolder wrapping the given Holder object.
    * @param set The Holder object.
    */
   public ImmutableHolder(Holder set)
   {
      m_set = set;
   }

   // operations

   /**
    * @see nexj.core.util.Holder#add(java.lang.Object)
    */
   public boolean add(Object value)
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see nexj.core.util.Holder#contains(java.lang.Object)
    */
   public boolean contains(Object value)
   {
      return m_set.contains(value);
   }

   /**
    * @see nexj.core.util.Holder#remove(java.lang.Object)
    */
   public boolean remove(Object key)
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see nexj.core.util.Holder#get(java.lang.Object)
    */
   public Object get(Object key)
   {
      return m_set.get(key);
   }

   /**
    * @see nexj.core.util.Holder#clear()
    */
   public void clear()
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see nexj.core.util.Holder#size()
    */
   public int size()
   {
      return m_set.size();
   }

   /**
    * @see nexj.core.util.Holder#iterator()
    */
   public Iterator iterator()
   {
      return new ImmutableHolderIterator(m_set.iterator());
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.print(m_set);
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         return (ImmutableHolder)super.clone();
      }
      catch (CloneNotSupportedException e)
      {
         return null;
      }
   }

   /**
    * Returns a mutable copy of this Holder.
    * @return A mutable copy of this Holder.
    */
   public Object mutableClone()
   {
      return m_set.clone();
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getEquivalenceFunction()
    */
   public Object getEquivalenceFunction()
   {
      if (m_set instanceof HashFunctionHolder)
      {
         return ((HashFunctionHolder)m_set).getEquivalenceFunction();
      }

      if (m_set instanceof HashHolder)
      {
         return Intrinsic.SYS_EQUAL_P;
      }

      if (m_set instanceof IdentityHashHolder)
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
      if (m_set instanceof HashFunctionHolder)
      {
         return ((HashFunctionHolder)m_set).getHashFunction();
      }

      return Boolean.FALSE;
   }

   /**
    * @see java.util.Set#addAll(java.util.Collection)
    */
   public boolean addAll(Collection arg0)
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see java.util.Set#containsAll(java.util.Collection)
    */
   public boolean containsAll(Collection collection)
   {
      return m_set.containsAll(collection);
   }

   /**
    * @see java.util.Set#isEmpty()
    */
   public boolean isEmpty()
   {
      return m_set.isEmpty();
   }

   /**
    * @see java.util.Set#removeAll(java.util.Collection)
    */
   public boolean removeAll(Collection arg0)
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see java.util.Set#retainAll(java.util.Collection)
    */
   public boolean retainAll(Collection collection)
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see java.util.Set#toArray()
    */
   public Object[] toArray()
   {
      return m_set.toArray();
   }

   /**
    * @see java.util.Set#toArray(java.lang.Object[])
    */
   public Object[] toArray(Object[] collection)
   {
      return m_set.toArray(collection);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return PrintWriter.toString(this);
   }

   /**
    * ImmutableHolder iterator - can return the current value but cannot modify the underlying Holder.
    * NOTE: This iterator does not check for errors for efficiency reasons.
    */
   protected static class ImmutableHolderIterator implements Iterator
   {
      // associations

      /**
       * The Iterator object of the underlying Holder hash set.
       */
      protected Iterator m_itr;

      // constructors

      /**
       * Creates an iterator.
       */
      protected ImmutableHolderIterator(Iterator itr)
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
   }
}
