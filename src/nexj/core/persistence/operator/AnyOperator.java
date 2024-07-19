// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.meta.TypeMismatchException;
import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.scripting.Symbol;

/**
 * ANY expression node. 
 */
public class AnyOperator extends UnaryOperator implements Quantor
{
   // constants
   
   /**
    * The operator ordinal number.
    */
   public final static int ORDINAL = 20;

   /**
    * The operator priority.
    */
   public final static int PRIORITY = 8;
   
   /**
    * The operator symbol.
    */
   public final static Symbol SYMBOL = Symbol.ANY;

   // associations

   /**
    * The query, to which the operator applies.
    */
   private Query m_query; 

   // constructors
   
   /**
    * Constructs the operator.
    * @param query The query, to which the operator applies.
    */
   public AnyOperator(Query query)
   {
      assert query != null;
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
    * @see nexj.core.persistence.Operator#getSymbol()
    */
   public Symbol getSymbol()
   {
      return SYMBOL;
   }

   /**
    * @see nexj.core.persistence.operator.Quantor#getQuantorQuery()
    */
   public Query getQuantorQuery()
   {
      return m_query;
   }

   /**
    * @see nexj.core.persistence.Operator#evaluate()
    */
   protected Object evaluate()
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.persistence.operator.UnaryOperator#normalize(int)
    */
   public Operator normalize(int nFlags)
   {
      if ((nFlags & NORMALIZE_NORECUR) == 0)
      {
         m_operand = m_operand.normalize(nFlags);
      }

      setSource(m_operand.getSource(), nFlags);
      m_type = Primitive.BOOLEAN;

      Type type = m_operand.getType();

      if (type == null)
      {
         setConstantValue(Boolean.FALSE);
      }
      else if (type.isPrimitive())
      {
         if (type != Primitive.BOOLEAN)
         {
            throw new TypeMismatchException(getSymbol());
         }

         if (m_operand.isConstant())
         {
            setConstantValue(m_operand.getValue());
         }
      }

      m_query = (Query)m_query.getSource();

      return this;
   }
}
