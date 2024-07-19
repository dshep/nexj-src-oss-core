package nexj.core.persistence;

import java.util.List;

import nexj.core.runtime.Instance;
import nexj.core.util.Lookup;

/**
 * An iterator designed to iterate the Cartesian Product of a Query with heterogeneous joins.
 */
public class QueryCartesianProductIterator extends GenericCartesianProductIterator
{
   // attributes

   /**
    * The index into the list of values for this iteration node.
    */
   protected int m_nIndex;

   // associations

   /**
    * The list of values for this iteration node.
    */
   protected List m_list;

   /**
    * The Query node.
    */
   protected final Query m_query;

   /**
    * Visitor for populating the output map.
    */
   protected final MapOutputVisitor m_visitor = new MapOutputVisitor();

   // constructors

   /**
    * Constructs an iterator node for the given Query.
    * @param query The Query.
    */
   protected QueryCartesianProductIterator(Query query)
   {
      m_query = query;
   }

   /**
    * Constructs an iterator tree for the given Query.
    * @param query The Query.
    * @param map A map with values for all Query nodes whose root is query.
    */
   public QueryCartesianProductIterator(final Query query, List stepList)
   {
      this(query);
      m_list = stepList;

      // Construct child iterators for all direct heterogeneous joins to query
      query.visit(new Query.Visitor()
      {
         public boolean isEligible(Query child)
         {
            return child.getRoot() == query || child.getParent().getRoot() == query;
         }

         public boolean visit(Query child)
         {
            if (child.isJoin() && child.isOutput())
            {
               add(new ChildQueryCartesianProductIterator(child, QueryCartesianProductIterator.this));
            }

            return true;
         }

         public boolean postVisit(Query child)
         {
            return true;
         }
      }, Query.VISIT_QUERY);
   }

   // operations

   /**
    * @see nexj.core.persistence.GenericCartesianProductIterator#hasNextDirect()
    */
   public boolean hasNextDirect()
   {
      return m_nIndex < m_list.size();
   }

   /**
    * @see nexj.core.persistence.GenericCartesianProductIterator#nextDirect()
    */
   public Object nextDirect()
   {
      return m_list.get(m_nIndex++);
   }

   /**
    * @see nexj.core.persistence.GenericCartesianProductIterator#reset()
    */
   public void reset()
   {
      m_nIndex = 0;
      m_list = null;
      m_current = null;
   }

   /**
    * Makes one step through the instances.
    * @param map The query to instance output map.
    */
   public void step(final Lookup map)
   {
      next();

      if (map != null)
      {
         m_visitor.output(map, (Instance[])m_current);

         // Add child instances to map
         for (int i = 0; i < m_nChildCount; i++)
         {
            ((ChildQueryCartesianProductIterator)m_childArray[i]).step(map);
         }
      }
   }

   // inner classes

   /**
    * Query visitor for putting the instances into the output map.
    */
   protected class MapOutputVisitor implements Query.Visitor
   {
      // associations

      /**
       * The output map.
       */
      protected Lookup m_map;

      /**
       * The instances, indexed by homogeneous Query ordinal.
       */
      protected Instance[] m_instanceArray;

      // operations

      /**
       * Populates the map with the instances from the array.
       * @param map The output map to populate.
       * @param instanceArray The instances, indexed by homogeneous Query ordinal.
       */
      public void output(Lookup map, Instance[] instanceArray)
      {
         m_map = map;
         m_instanceArray = instanceArray;
         m_query.visit(this, Query.VISIT_QUERY);
      }

      /**
       * @see nexj.core.persistence.Query.Visitor#isEligible(nexj.core.persistence.Query)
       */
      public boolean isEligible(Query query)
      {
         return query.getRoot() == m_query && query.isOutput();
      }

      /**
       * @see nexj.core.persistence.Query.Visitor#visit(nexj.core.persistence.Query)
       */
      public boolean visit(Query query)
      {
         m_map.put(query, m_instanceArray[query.getOrdinal()]);

         return true;
      }

      /**
       * @see nexj.core.persistence.Query.Visitor#postVisit(nexj.core.persistence.Query)
       */
      public boolean postVisit(Query query)
      {
         return true;
      }
   }
}
