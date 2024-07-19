// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.io.Serializable;
import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

/**
 * Immutable decorator for Set objects. 
 */
public class ImmutableSet implements Set, Cloneable, Serializable
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = 8660101645164328916L;

   // associations

   /**
    * The Set object.
    */
   protected Set m_set;

   // constructors

   /**
    * Constructs a new ImmutableSet wrapping the given Set.
    * @param set The Set object.
    */
   public ImmutableSet(Set set)
   {
      m_set = set;
   }

   // operations

   /**
    * @see java.util.Set#add(java.lang.Object)
    */
   public boolean add(Object obj)
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see java.util.Set#addAll(java.util.Collection)
    */
   public boolean addAll(Collection collection)
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see java.util.Set#clear()
    */
   public void clear()
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see java.util.Set#contains(java.lang.Object)
    */
   public boolean contains(Object obj)
   {
      return m_set.contains(obj);
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
    * @see java.util.Set#iterator()
    */
   public Iterator iterator()
   {
      return m_set.iterator();
   }

   /**
    * @see java.util.Set#remove(java.lang.Object)
    */
   public boolean remove(Object key)
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see java.util.Set#removeAll(java.util.Collection)
    */
   public boolean removeAll(Collection collection)
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
    * @see java.util.Set#size()
    */
   public int size()
   {
      return m_set.size();
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
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      return new ImmutableSet(m_set);
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      return m_set.equals(obj);
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return m_set.hashCode();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return m_set.toString();
   }
}
