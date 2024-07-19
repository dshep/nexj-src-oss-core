// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import java.io.IOException;

import nexj.core.meta.Primitive;
import nexj.core.persistence.InvalidQueryException;
import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.runtime.Context;
import nexj.core.scripting.Pair;
import nexj.core.scripting.ScriptingException;
import nexj.core.scripting.Symbol;
import nexj.core.scripting.match.MatchNode;
import nexj.core.util.PrintWriter;

/**
 * MATCH expression node: (match? (@ x) (...))
 * The first argument is a attribute
 * The second argument is a Pair logical tree supporting AND/OR/NOT
 * The result of the operator is a float value in the range of 0.0 <= x <= 1.0
 *                                             or null for a non-matching record
 */
public final class MatchOperator extends Operator
{
   // constants

   /**
    * The operator ordinal number.
    * The last operator gets Operator.MAX_COUNT, and Operator.MAX_COUNT is incremented by 1.
    */
   public final static int ORDINAL = 21;

   /**
    * The operator priority.
    */
   public final static int PRIORITY = 8;

   /**
    * The operator symbol.
    */
   public final static Symbol SYMBOL = Symbol.MATCH;

   // associations

   /**
    * The operator holding the attribute for Full-Text matching.
    */
   protected AttributeOperator m_attribute;

   /**
    * The invocation context to query for runtime configuration.
    */
   protected Context m_context;

   /**
    * The expression used for Full-Text matching.
    */
   protected Pair m_expression;

   /**
    * The SQLJoin representation of the optional quasi-Full-Text join table for this operator.
    * (not used internally)
    */
   private Object m_mapping;

   /**
    * Reference to array used while computing Fuzzy scores using modified Levenshtein distance.
    */
   private MatchNode.IntArrayRef m_scoreArray; // lazy init

   // constructors

   /**
    * Constructor.
    * Can't add to query yet because don't know Attribute to map, add during normalization.
    * @param context The invocation context to query for runtime configuration.
    */
   public MatchOperator(Context context)
   {
      m_context = context;
      setType(Primitive.DOUBLE);
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
    * Store the operator containing the attribute used for Full-Text matching.
    * @param attribute The attribute operator to store (not null).
    */
   public void setAttribute(AttributeOperator attribute)
   {
      if (attribute == null)
      {
         throw new InvalidQueryException("err.persistence.invalidMatchArgument",
                                         new Object[] {attribute});
      }

      m_attribute = attribute;
   }

   /**
    * @return The attribute portion of this operator.
    */
   public AttributeOperator getAttribute()
   {
      return m_attribute;
   }

   /**
    * Store the operator containing the expression used for Full-text matching.
    * @param expression The expression operator to store (not null).
    */
   public void setExpression(Pair expression)
   {
      if (expression == null)
      {
         throw new InvalidQueryException("err.persistence.invalidMatchArgument",
                                         new Object[] {expression});
      }

      m_expression = expression;
   }

   /**
    * @return The expression portion of this operator.
    */
   public Pair getExpression()
   {
      return m_expression;
   }

   /**
    * Store the SQLJoin representation of the quasi-Full-Text join table for this operator.
    * @param join The alias of the quasi-Full-Text join table to store.
    */
   public void setMapping(Object mapping)
   {
      m_mapping = mapping;
   }

   /**
    * Return SQLJoin representation of the optional quasi-Full-Text join table for this operator.
    * @return The previously stored quasi-Full-Text join table for this operator.
    */
   public Object getMapping()
   {
      return m_mapping;
   }

   /**
    * @see nexj.core.persistence.Operator#visit(nexj.core.persistence.Operator.Visitor, int)
    */
   public boolean visit(Visitor visitor, int nFlags)
   {
      if ((nFlags & VISIT_PREORDER) != 0)
      {
         if (!visitor.visit(this))
         {
            return false;
         }
      }

      if (m_attribute != null)
      {
         if (visitor.isEligible(m_attribute))
         {
            if (!m_attribute.visit(visitor, nFlags))
            {
               return false;
            }
         }
      }

      if ((nFlags & VISIT_POSTORDER) != 0)
      {
         if (!visitor.visit(this))
         {
            return false;
         }
      }

      return true;
   }

   /**
    * @see nexj.core.persistence.Operator#normalize(int)
    */
   public Operator normalize(int nFlags)
   {
      if ((nFlags & NORMALIZE_NORECUR) == 0)
      {
         Operator attribute = m_attribute.normalize(nFlags);

         setAttribute((attribute instanceof AttributeOperator)
                      ? (AttributeOperator)attribute : null); // null check will throw error
      }

      return super.normalize(nFlags);
   }

   /**
    * @return Computed score in the range of [0..1] or null on non-match.
    * @see nexj.core.persistence.Operator#evaluate()
    */
   protected Object evaluate()
   {
      if (m_scoreArray == null)
      {
         m_scoreArray = new MatchNode.IntArrayRef();
      }

      String sValue = Primitive.toString(m_attribute.getValue());
      double nValue;

      try
      {
         nValue = MatchNode.evaluate(
            (sValue == null) ? null : sValue.toUpperCase(m_context.getLocale()),
            m_expression, m_scoreArray, m_context);
      }
      catch (ScriptingException e)
      {
         throw new InvalidQueryException("err.persistence.invalidMatchExpression", new Object[] {m_expression});
      }

      return (nValue > 0) ? Primitive.createDouble(nValue) : null;
   }

   /**
    * @see nexj.core.persistence.Operator#getExpression(nexj.core.persistence.Query)
    */
   public Object getExpression(Query root)
   {
      return Pair.list(SYMBOL, m_attribute.getExpression(root), m_expression);
   }

   /**
    * @see nexj.core.persistence.Operator#compareTo(java.lang.Object)
    */
   public int compareTo(Object obj)
   {
      int n = super.compareTo(obj);

      if (n != 0)
      {
         return n;
      }

      MatchOperator op = (MatchOperator)obj;

      n = m_attribute.compareTo(op.getAttribute());

      if (n != 0)
      {
         return n;
      }

      return (m_expression.hashCode() & Integer.MAX_VALUE) -
         (op.getExpression().hashCode() & Integer.MAX_VALUE);
   }

   /**
    * @see nexj.core.util.Printable#printOn(PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write('(');
      writer.print(getSymbol());
      writer.write(' ');
      writer.print(m_attribute);
      writer.write(' ');
      writer.print(m_expression);
      writer.write(')');
   }
}