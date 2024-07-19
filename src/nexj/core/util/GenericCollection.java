// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.lang.reflect.Array;
import java.util.Collection;
import java.util.Iterator;

/**
 * Generic collection implementation.
 */
public abstract class GenericCollection implements Collection
{
   /**
    * @see java.util.Collection#addAll(java.util.Collection)
    */
   public boolean addAll(Collection c)
   {
      boolean bModified = false;

      for (Iterator itr = c.iterator(); itr.hasNext();)
      {
         bModified |= add(itr.next());
      }

      return bModified;
   }

   /**
    * @see java.util.Collection#containsAll(java.util.Collection)
    */
   public boolean containsAll(Collection c)
   {
      for (Iterator itr = c.iterator(); itr.hasNext();)
      {
         if (!contains(itr.next()))
         {
            return false;
         }
      }
      
      return true;
   }

   /**
    * @see java.util.Collection#isEmpty()
    */
   public boolean isEmpty()
   {
      return size() == 0;
   }

   /**
    * @see java.util.Collection#removeAll(java.util.Collection)
    */
   public boolean removeAll(Collection c)
   {
      boolean bModified = false;

      for (Iterator itr = c.iterator(); itr.hasNext();)
      {
         bModified |= remove(itr.next());
      }

      return bModified;
   }

   /**
    * @see java.util.Collection#retainAll(java.util.Collection)
    */
   public boolean retainAll(Collection c)
   {
      boolean bModified = false;

      for (Iterator itr = iterator(); itr.hasNext();)
      {
         if (!c.contains(itr.next()))
         {
            itr.remove();
            bModified = true;
         }
      }

      return bModified;
   }

   /**
    * @see java.util.Collection#toArray()
    */
   public Object[] toArray()
   {
      Object[] a = new Object[size()];
      int i = 0;

      for (Iterator itr = iterator(); itr.hasNext();)
      {
         a[i++] = itr.next();
      }

      return a;
   }

   /**
    * @see java.util.Collection#toArray(java.lang.Object[])
    */
   public Object[] toArray(Object[] a)
   {
      int nCount = size();

      if (nCount > a.length)
      {
         a = (Object[])Array.newInstance(a.getClass().getComponentType(), nCount);
      }

      int i = 0;
      for (Iterator itr = iterator(); i != nCount && itr.hasNext();)
      {
         a[i++] = itr.next();
      }

      if (nCount < a.length)
      {
         a[nCount] = null;
      }

      return a;
   }
}
