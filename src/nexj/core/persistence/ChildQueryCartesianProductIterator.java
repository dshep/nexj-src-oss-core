package nexj.core.persistence;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Attribute;
import nexj.core.runtime.Instance;
import nexj.core.util.Lookup;

/**
 * An iterator designed to iterate the Cartesian Product of a Query with heterogeneous joins.
 */
public class ChildQueryCartesianProductIterator extends QueryCartesianProductIterator
{
   // associations

   /**
    * The parent iterator node.
    */
   protected QueryCartesianProductIterator m_parent;

   // constructors

   /**
    * Constructs an iterator tree rooted at the given Query node. Each Query node
    * has a corresponding iterator node.
    * @param query The Query node for this iterator node.
    * @param parent The parent iterator node.
    */
   protected ChildQueryCartesianProductIterator(Query query, QueryCartesianProductIterator parent)
   {
      super(query);
      m_parent = parent;

      for (Iterator itr = query.getAssocIterator(Query.ASSOC_QUERY); itr.hasNext(); )
      {
         Query child = (Query)itr.next();

         if (child.isOutput())
         {
            add(new ChildQueryCartesianProductIterator(child, this));
         }
      }
   }

   // operations

   /**
    * @see nexj.core.persistence.QueryCartesianProductIterator#hasNextDirect()
    */
   public boolean hasNextDirect()
   {
      if (m_list == null)
      {
         reset();

         if (m_list == null)
         {
            return false;
         }
      }

      return super.hasNextDirect();
   }

   /**
    * @see nexj.core.persistence.QueryCartesianProductIterator#nextDirect()
    */
   public Object nextDirect()
   {
      if (m_list == null)
      {
         reset();
      }

      return super.nextDirect();
   }

   /**
    * @see nexj.core.persistence.GenericCartesianProductIterator#reset()
    */
   public void reset()
   {
      super.reset();

      Object parentItem = m_parent.m_current;

      if (parentItem == null)
      {
         return;
      }

      Instance container;

      if (m_parent instanceof ChildQueryCartesianProductIterator)
      {
         container = (Instance)parentItem;
      }
      else
      {
         container = ((Instance[])parentItem)[m_query.getParent().getOrdinal()];
      }

      if (container == null)
      {
         m_list = Collections.emptyList();
      }
      else
      {
         Attribute attribute = m_query.getAttribute();

         if (container.getMetaclass().isUpcast(attribute.getMetaclass()))
         {
            Object value = container.getValue(attribute.getOrdinal());

            if (value == null)
            {
               m_list = Collections.emptyList();
            }
            else if (attribute.isCollection())
            {
               m_list = (List)value;
            }
            else
            {
               m_list = Collections.singletonList(value);
            }
         }
         else
         {
            // Polymorphic queries may add attributes that do not exist in all possible containers
            m_list = Collections.emptyList();
         }
      }
   }

   /**
    * @see nexj.core.persistence.QueryCartesianProductIterator#step(nexj.core.util.Lookup)
    */
   public void step(Lookup map)
   {
      map.put(m_query, m_current);

      for (int i = 0; i < m_nChildCount; i++)
      {
         ((ChildQueryCartesianProductIterator)m_childArray[i]).step(map);
      }
   }
}
