// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.RandomAccess;

/**
 * A simple HolderList implementation.
 */
public class HashHolderList implements HolderList
{
   // associations

   /**
    * The list holding the collection elements, in order.
    */
   protected ArrayList m_list;

   /**
    * The set of collection elements.
    */
   protected HashHolder m_set;

   // constructors

   /**
    * Constructs a new instance using the default estimated value count of 8.
    */
   public HashHolderList()
   {
      m_list = new ArrayList(8);
      m_set = new HashHolder(8);
   }

   /**
    * Constructs a new instance with an estimated value count.
    * @param nCount The estimated value count.
    */
   public HashHolderList(int nCount)
   {
      m_list = new ArrayList(nCount);
      m_set = new HashHolder(nCount);
   }

   // operations

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         HashHolderList list = (HashHolderList)super.clone();

         list.m_set = (HashHolder)m_set.clone();
         list.m_list = (ArrayList)((ArrayList)m_list).clone();

         return list;
      }
      catch (CloneNotSupportedException r)
      {
         return null;
      }
   }

   /**
    * If the value is already in this collection, then it is replaced. Its position
    * does not change.
    * @see nexj.core.util.Holder#add(java.lang.Object)
    */
   public boolean add(Object value)
   {
      Object oldValue = m_set.get(value);

      if (oldValue == null)
      {
         m_set.add(value);
         m_list.add(value);

         return true;
      }
      else if (oldValue != value)
      {
         m_set.add(value);
         m_list.set(m_list.indexOf(oldValue), value);
      }

      return false;
   }

   /**
    * @see nexj.core.util.Holder#clear()
    */
   public void clear()
   {
      m_set.clear();
      m_list.clear();
   }

   /**
    * @see nexj.core.util.Holder#contains(java.lang.Object)
    */
   public boolean contains(Object value)
   {
      return m_set.contains(value);
   }

   /**
    * @see nexj.core.util.Holder#iterator()
    */
   public Iterator iterator()
   {
      return m_list.iterator();
   }

   /**
    * @see nexj.core.util.Holder#remove(java.lang.Object)
    */
   public boolean remove(Object value)
   {
      if (m_set.remove(value))
      {
         m_list.remove(value);

         return true;
      }

      return false;
   }

   /**
    * @see nexj.core.util.Holder#size()
    */
   public int size()
   {
      return m_set.size();
   }

   /**
    * @see java.util.Set#addAll(java.util.Collection)
    */
   public boolean addAll(Collection col)
   {
      boolean bModified = false;

      if (col instanceof RandomAccess)
      {
         List list = (List)col;

         for (int i = 0, nSize = col.size(); i < nSize; i++)
         {
            bModified |= add(list.get(i));
         }
      }
      else
      {
         for (Iterator itr = col.iterator(); itr.hasNext(); )
         {
            bModified |= add(itr.next());
         }
      }

      return bModified;
   }

   /**
    * @see java.util.Set#containsAll(java.util.Collection)
    */
   public boolean containsAll(Collection col)
   {
      return m_set.containsAll(col);
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
      throw new UnsupportedOperationException();
   }

   /**
    * @see java.util.Set#retainAll(java.util.Collection)
    */
   public boolean retainAll(Collection arg0)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see java.util.Set#toArray()
    */
   public Object[] toArray()
   {
      return m_list.toArray();
   }

   /**
    * @see java.util.Set#toArray(java.lang.Object[])
    */
   public Object[] toArray(Object[] array)
   {
      return m_list.toArray(array);
   }

   /**
    * @see java.util.List#add(int, java.lang.Object)
    */
   public void add(int nIndex, Object obj)
   {
      if (m_set.add(obj))
      {
         m_list.add(nIndex, obj);
      }
      else
      {
         int nOldIndex = m_list.indexOf(obj);

         m_list.add(nIndex, obj);

         if (nOldIndex < nIndex)
         {
            m_list.remove(nOldIndex);
         }
         else
         {
            m_list.remove(nOldIndex + 1);
         }
      }
   }

   /**
    * @see java.util.List#addAll(int, java.util.Collection)
    */
   public boolean addAll(int nIndex, Collection col)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see java.util.List#get(int)
    */
   public Object get(int nIndex)
   {
      return m_list.get(nIndex);
   }

   /**
    * @see nexj.core.util.Holder#get(java.lang.Object)
    */
   public Object get(Object obj)
   {
      return m_set.get(obj);
   }

   /**
    * @see java.util.List#indexOf(java.lang.Object)
    */
   public int indexOf(Object obj)
   {
      return m_list.indexOf(obj);
   }

   /**
    * @see java.util.List#lastIndexOf(java.lang.Object)
    */
   public int lastIndexOf(Object obj)
   {
      return m_list.lastIndexOf(obj);
   }

   /**
    * @see java.util.List#listIterator()
    */
   public ListIterator listIterator()
   {
      return m_list.listIterator();
   }

   /**
    * @see java.util.List#listIterator(int)
    */
   public ListIterator listIterator(int nIndex)
   {
      return m_list.listIterator(nIndex);
   }

   /**
    * @see java.util.List#remove(int)
    */
   public Object remove(int nIndex)
   {
      Object obj = m_list.remove(nIndex);

      m_set.remove(obj);

      return obj;
   }

   /**
    * @see java.util.List#set(int, java.lang.Object)
    */
   public Object set(int nIndex, Object obj)
   {
      Object oldObj = m_list.set(nIndex, obj);

      m_set.remove(oldObj);
      m_set.add(obj);

      return oldObj;
   }

   /**
    * @see java.util.List#subList(int, int)
    */
   public List subList(int nFromIndex, int nToIndex)
   {
      return m_list.subList(nFromIndex, nToIndex);
   }

   /**
    * @see nexj.core.util.HolderList#trimToSize()
    */
   public void trimToSize()
   {
      m_list.trimToSize();
   }
}
