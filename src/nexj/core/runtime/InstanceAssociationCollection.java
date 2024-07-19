package nexj.core.runtime;

import java.util.AbstractCollection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

import nexj.core.meta.Attribute;
import nexj.core.meta.Primitive;
import nexj.core.meta.TypeMismatchException;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.ScriptingException;
import nexj.core.scripting.Symbol;
import nexj.core.util.Ditto;

/**
 * Collection rolling up nested collections from associations.
 */
public class InstanceAssociationCollection extends AbstractCollection implements Cloneable, Ditto, Function
{
   // attributes

   /**
    * The property name.
    */
   protected String m_sName;

   // associations

   /**
    * The wrapped collection.
    */
   protected Collection m_col;

   // constructors

   /**
    * Constructs the accessor collection.
    * @param col The wrapped collection.
    * @param sName The property name.
    */
   public InstanceAssociationCollection(Collection col, String sName)
   {
      assert col != null;
      assert sName != null;

      m_col = col;
      m_sName = sName;
   }

   // operations

   /**
    * @see java.util.AbstractCollection#iterator()
    */
   public Iterator iterator()
   {
      if (m_sName == null)
      {
         return m_col.iterator();
      }

      return new AccessorIterator(m_col.iterator());
   }

   /**
    * @see java.util.AbstractCollection#size()
    */
   public int size()
   {
      if (m_col instanceof InstanceAssociationCollection)
      {
         int nCount = 0;

         for (Iterator itr = m_col.iterator(); itr.hasNext(); itr.next())
         {
            ++nCount;
         }

         return nCount;
      }

      return m_col.size();
   }

   /**
    * @see java.util.AbstractCollection#isEmpty()
    */
   public boolean isEmpty()
   {
      if (m_col instanceof InstanceAssociationCollection)
      {
         return !m_col.iterator().hasNext();
      }

      return m_col.isEmpty();
   }

   /**
    * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
    */
   public boolean invoke(int nArgCount, Machine machine)
   {
      if (nArgCount == 1)
      {
         Object arg = machine.getArg(0, nArgCount);

         if (!(arg instanceof Symbol))
         {
            throw new TypeMismatchException("#<association-collection>");
         }

         String sName = arg.toString();

         if (sName.length() != 0 && sName.charAt(0) == ':')
         {
            if (Symbol._SIZE.equals(arg) || Symbol._COUNT.equals(arg))
            {
               machine.returnValue(Primitive.createInteger(size()), nArgCount);

               return false;
            }

            if (Symbol._LIST.equals(arg))
            {
               machine.returnValue(list(), nArgCount);

               return false;
            }

            if (Symbol._ITERATOR.equals(arg))
            {
               machine.returnValue(iterator(), nArgCount);

               return false;
            }

            if (Symbol._EMPTY.equals(arg))
            {
               machine.returnValue(Boolean.valueOf(isEmpty()), nArgCount);

               return false;
            }

            if (Symbol._ASSOCIATION.equals(arg))
            {
               machine.returnValue(this, nArgCount);

               return false;
            }
         }

         machine.returnValue(new InstanceAssociationCollection((m_sName == null) ? m_col : this, arg.toString()), nArgCount);

         return false;
      }

      if (m_sName == null)
      {
         return machine.invokeJavaMethod(m_col, nArgCount);
      }

      throw new ScriptingException(
         (nArgCount < 1) ? "err.scripting.maxArgCount" : "err.scripting.maxArgCount",
         new Object[]{"#<association-collection>", Primitive.createInteger(1),
         Primitive.createInteger(nArgCount)});
   }

   /**
    * @return The items in the collection as a linked list.
    */
   public Pair list()
   {
      Pair pair = null;

      for (Iterator itr = iterator(); itr.hasNext();)
      {
         pair = new Pair(itr.next(), pair);
      }

      return Pair.nreverse(pair);
   }

   /**
    * @see nexj.core.util.Ditto#ditto()
    */
   public Object ditto()
   {
      return new InstanceAssociationCollection(new ArrayList(size()), null);
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         return super.clone();
      }
      catch (CloneNotSupportedException e)
      {
         return null;
      }
   }

   // inner classes

   /**
    * Iterator wrapper implementing the property access logic.
    */
   protected class AccessorIterator implements Iterator
   {
      // associations

      /**
       * The next available object. Points to this if empty. 
       */
      protected Object m_next;

      /**
       * The wrapped iterator.
       */
      protected Iterator m_itr;

      /**
       * Iterator for a possible inner collection.
       */
      protected Iterator m_innerItr;

      // constructors

      /**
       * Constructs the iterator.
       * @param itr The wrapped iterator.
       */
      protected AccessorIterator(Iterator itr)
      {
         m_next = this;
         m_itr = itr;
      }

      // operations

      /**
       * @return The next iterator value, or this if none.
       */
      protected Object findNext()
      {
         for (;;)
         {
            if (m_innerItr != null)
            {
               if (m_innerItr.hasNext())
               {
                  return m_innerItr.next();
               }

               m_innerItr = null;
            }

            if (m_itr.hasNext())
            {
               Object obj = m_itr.next();;

               if (obj == null)
               {
                  return null;
               }

               if (!(obj instanceof Instance))
               {
                  throw new TypeMismatchException("#<instance-collection select/1>");
               }

               Instance instance = (Instance)obj;

               Object value = instance.getValue(m_sName);
               Attribute attribute = instance.getMetaclass().findAttribute(m_sName);

               if (attribute != null && attribute.isCollection() ||
                  attribute == null && value instanceof Collection)
               {
                  if (value != null)
                  {
                     m_innerItr = ((Collection)value).iterator();
                  }

                  continue;
               }

               return value; 
            }

            return this;
         }
      }

      /**
       * @see java.util.Iterator#hasNext()
       */
      public boolean hasNext()
      {
         if (m_next == this)
         {
            m_next = findNext();

            return m_next != this;
         }

         return true;
      }

      /**
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         Object obj = m_next;

         if (obj == this)
         {
            obj = findNext();

            if (obj == this)
            {
               throw new NoSuchElementException();
            }
         }

         m_next = this;

         return obj;
      }

      /**
       * @see java.util.Iterator#remove()
       */
      public void remove()
      {
         throw new UnsupportedOperationException();
      }
   }
}
