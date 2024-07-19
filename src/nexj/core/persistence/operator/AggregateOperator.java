// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.meta.Primitive;
import nexj.core.persistence.InvalidQueryException;
import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.scripting.Symbol;

/**
 * Aggregate function operator.
 */
public abstract class AggregateOperator extends UnaryOperator implements Quantor
{
   // constants

   /**
    * The operator ordinal number.
    */
   public final static int ORDINAL = 23;

   /**
    * The operator priority.
    */
   public final static int PRIORITY = 8;

   // attributes

   /**
    * The unique operator flag.
    * If true, the function is applied to distinct unique values.
    */
   protected boolean m_bUnique;

   // associations

   /**
    * The operator symbol.
    */
   protected Symbol m_symbol;

   /**
    * The query, to which the operator applies.
    */
   protected Query m_query; 

   // constructors

   /**
    * Creates the aggregate function operator.
    * @param symbol The operator symbol.
    * @param type The function return type.
    * @param query The quantor query.
    */
   public AggregateOperator(Symbol symbol, Primitive type, Query query)
   {
      assert symbol != null;
      assert query != null;

      m_symbol = symbol;
      m_type = type;
      m_query = query;
   }

   // operations

   /**
    * @see nexj.core.persistence.Operator#getOrdinal()
    */
   public int getOrdinal()
   {
      return ORDINAL;
   }

   /**
    * @see nexj.core.persistence.Operator#getPriority()
    */
   public int getPriority()
   {
      return PRIORITY;
   }

   /**
    * @see nexj.core.persistence.operator.FunctionOperator#getSymbol()
    */
   public Symbol getSymbol()
   {
      return m_symbol;
   }

   /**
    * @see nexj.core.persistence.operator.Quantor#getQuantorQuery()
    */
   public Query getQuantorQuery()
   {
      return m_query;
   }

   /**
    * @return True if there is an associated quantor query. 
    */
   public boolean isQuantor()
   {
      return m_query != null && m_query.getAssocCount(this) != 0;
   }

   /**
    * Sets the unique operator flag.
    * @param bUnique The unique operator flag to set.
    */
   public void setUnique(boolean bUnique)
   {
      m_bUnique = bUnique;
   }

   /**
    * @return The unique operator flag.
    */
   public boolean isUnique()
   {
      return m_bUnique;
   }

   /**
    * @see nexj.core.persistence.operator.UnaryOperator#foldConstant()
    */
   protected void foldConstant()
   {
   }

   /**
    * Verifies that the operator is not nested.
    * @throws InvalidQueryException if the operator is nested.
    */
   protected void verifyNesting() throws InvalidQueryException
   {
      for (Operator parent = m_parent; parent != null; parent = parent.getParent())
      {
         if (parent instanceof AggregateOperator)
         {
            throw new InvalidQueryException("err.persistence.nestedAggregate", new Object[]{getSymbol().getName()});
         }
      }
   }

   /**
    * @see nexj.core.persistence.Operator#evaluate()
    */
   protected Object evaluate()
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.persistence.operator.MultiArgOperator#copy(nexj.core.persistence.Operator)
    */
   public void copy(Operator src)
   {
      super.copy(src);

      AggregateOperator op = (AggregateOperator)src;

      m_symbol = op.m_symbol;
   }
}
