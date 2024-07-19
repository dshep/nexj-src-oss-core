// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.persistence.operator.ConversionMapper;
import nexj.core.persistence.operator.DivisionOperator;
import nexj.core.persistence.operator.EqualsOperator;
import nexj.core.persistence.operator.GenericConversionMapper;
import nexj.core.persistence.operator.GreaterThanOperator;
import nexj.core.persistence.operator.GreaterThanOrEqualsOperator;
import nexj.core.persistence.operator.IfOperator;
import nexj.core.persistence.operator.LessThanOperator;
import nexj.core.persistence.operator.LessThanOrEqualsOperator;
import nexj.core.persistence.operator.LikeOperator;
import nexj.core.persistence.operator.MinusOperator;
import nexj.core.persistence.operator.MultiplicationOperator;
import nexj.core.persistence.operator.NegationOperator;
import nexj.core.persistence.operator.NotEqualsOperator;
import nexj.core.persistence.operator.NotOperator;
import nexj.core.persistence.operator.NumericAggregateOperator;
import nexj.core.persistence.operator.PlusOperator;
import nexj.core.scripting.Symbol;
import nexj.core.util.PrintWriter;
import nexj.core.util.Printable;

/**
 * Expression node representing an operator with its operands.
 */
public abstract class Operator implements Comparable, Cloneable, Printable 
{
   // constants

   /**
    * Preorder tree traversal: first the parent, then the children.
    */
   public final static int VISIT_PREORDER = 0x0001;

   /**
    * Postorder tree traversal: first the children, then the parent.
    */
   public final static int VISIT_POSTORDER = 0x0002;

   /**
    * Normalizes an expression used in a where conditional clause.
    */
   public final static int NORMALIZE_WHERE = 0x01;

   /**
    * Normalizes an expression used in a having conditional clause.
    */
   public final static int NORMALIZE_HAVING = 0x02;

   /**
    * Normalizes an expression used in a group by clause.
    */
   public final static int NORMALIZE_GROUPBY = 0x04;

   /**
    * Normalizes an expression used in an order by clause.
    */
   public final static int NORMALIZE_ORDERBY = 0x08;

   /**
    * Mask for normalizing an expression.
    */
   public final static int NORMALIZE_EXPRESSION = NORMALIZE_WHERE | NORMALIZE_HAVING | NORMALIZE_GROUPBY | NORMALIZE_ORDERBY;

   /**
    * Normalizes the persistence mappings.
    */
   public final static int NORMALIZE_PERSISTENCE = 0x40;

   /**
    * Normalizes only the current node. Does not descend recursively.
    */
   public final static int NORMALIZE_NORECUR = 0x80;

   /**
    * The number of supported operators.
    */
   public final static int MAX_COUNT = 24;

   // attributes

   /**
    * The operator evaluation result value.
    */
   protected Object m_value;

   /**
    * The constant value flag.
    */
   protected boolean m_bConstant;

   // associations

   /**
    * The result type.
    */
   protected Type m_type;

   /**
    * The parent node.
    */
   protected Operator m_parent;

   /**
    * The operator value source. Null if the expression is evaluated by the framework.
    */
   protected Source m_source;

   /**
    * The operator type conversion mapper.
    */
   private final static ConversionMapper s_conversionMapper;

   static
   {
      GenericConversionMapper dm = new GenericConversionMapper();

      s_conversionMapper = dm;

      dm.setUnaryDescriptor(NegationOperator.ORDINAL, new Primitive[]{Primitive.INTEGER,
         Primitive.LONG, Primitive.DECIMAL, Primitive.FLOAT, Primitive.DOUBLE});

      dm.setUnaryDescriptor(NumericAggregateOperator.ORDINAL, new Primitive[]{
         Primitive.DECIMAL, Primitive.FLOAT, Primitive.DOUBLE});
      dm.setUnaryDescriptor(NumericAggregateOperator.ORDINAL, Primitive.INTEGER, Primitive.DECIMAL);
      dm.setUnaryDescriptor(NumericAggregateOperator.ORDINAL, Primitive.LONG, Primitive.DECIMAL);

      dm.setUnaryDescriptor(NotOperator.ORDINAL, Primitive.BOOLEAN);

      dm.setBinaryDescriptor(new int[]{MultiplicationOperator.ORDINAL, DivisionOperator.ORDINAL,
         PlusOperator.ORDINAL, MinusOperator.ORDINAL, IfOperator.ORDINAL},
         new Primitive[]{Primitive.INTEGER, Primitive.LONG, Primitive.DECIMAL,
            Primitive.FLOAT, Primitive.DOUBLE}, true);

      dm.setBinaryDescriptor(IfOperator.ORDINAL,  Primitive.STRING, Primitive.STRING, true);
      dm.setBinaryDescriptor(IfOperator.ORDINAL,  Primitive.BINARY, Primitive.BINARY, true);
      dm.setBinaryDescriptor(IfOperator.ORDINAL,  Primitive.TIMESTAMP, Primitive.TIMESTAMP, true);
      dm.setBinaryDescriptor(IfOperator.ORDINAL,  Primitive.BOOLEAN, Primitive.BOOLEAN, true);

      dm.setBinaryDescriptor(DivisionOperator.ORDINAL, Primitive.INTEGER, Primitive.INTEGER,
         Primitive.DECIMAL, Primitive.DECIMAL, Primitive.DECIMAL, true);
      dm.setBinaryDescriptor(DivisionOperator.ORDINAL, Primitive.INTEGER, Primitive.LONG,
         Primitive.DECIMAL, Primitive.DECIMAL, Primitive.DECIMAL, true);
      dm.setBinaryDescriptor(DivisionOperator.ORDINAL, Primitive.LONG, Primitive.LONG,
         Primitive.DECIMAL, Primitive.DECIMAL, Primitive.DECIMAL, true);

      dm.setBinaryDescriptor(LikeOperator.ORDINAL, new Primitive[]{Primitive.STRING},
         Primitive.BOOLEAN, true);

      int [] comparisonOrdinalArray = new int[]{GreaterThanOperator.ORDINAL,
         GreaterThanOrEqualsOperator.ORDINAL, LessThanOperator.ORDINAL,
         LessThanOrEqualsOperator.ORDINAL, EqualsOperator.ORDINAL,
         NotEqualsOperator.ORDINAL};

      dm.setBinaryDescriptor(comparisonOrdinalArray,  new Primitive[]{Primitive.STRING}, Primitive.BOOLEAN, true);
      dm.setBinaryDescriptor(comparisonOrdinalArray,  new Primitive[]{Primitive.BINARY}, Primitive.BOOLEAN, true);
      dm.setBinaryDescriptor(comparisonOrdinalArray,  new Primitive[]{Primitive.INTEGER,
         Primitive.LONG, Primitive.DECIMAL, Primitive.FLOAT, Primitive.DOUBLE}, Primitive.BOOLEAN, true);
      dm.setBinaryDescriptor(comparisonOrdinalArray,  new Primitive[]{Primitive.TIMESTAMP}, Primitive.BOOLEAN, true);
      dm.setBinaryDescriptor(comparisonOrdinalArray,  new Primitive[]{Primitive.BOOLEAN}, Primitive.BOOLEAN, true);
   }

   // operations

   /**
    * @return The operator symbol. Non-unique. Can be null.
    */
   public abstract Symbol getSymbol();

   /**
    * @return The unique operator type ordinal number.
    */
   public abstract int getOrdinal();

   /**
    * @return The operator priority (logical are lower than arithmetic etc).
    */
   public abstract int getPriority();

   /**
    * Sets the operator evaluation result value.
    * @param value The operator evaluation result value to set.
    */
   public void setValue(Object value)
   {
      m_value = value;
   }

   /**
    * @return The operator evaluation result value.
    */
   public Object getValue()
   {
      if (!m_bConstant)
      {
         setValue(evaluate());
      }

      return m_value;
   }

   /**
    * Sets the constant value flag.
    * @param bConstant The constant value flag to set.
    */
   public void setConstant(boolean bConstant)
   {
      m_bConstant = bConstant;

      if (bConstant)
      {
         m_source = null;
      }
   }

   /**
    * @return The constant value flag.
    */
   public boolean isConstant()
   {
      return m_bConstant;
   }

   /**
    * Sets the value and the constant flag.
    * @param value The value to set.
    */
   public void setConstantValue(Object value)
   {
      setValue(value);
      setConstant(true);
   }

   /**
    * Sets the result type.
    * @param type The result type to set.
    */
   public void setType(Type type)
   {
      m_type = type;
   }

   /**
    * @return The result type.
    */
   public Type getType()
   {
      return m_type;
   }

   /**
    * Sets the parent node.
    * @param parent The parent node to set.
    */
   public void setParent(Operator parent)
   {
      m_parent = parent;
   }

   /**
    * @return The parent node.
    */
   public Operator getParent()
   {
      return m_parent;
   }

   /**
    * Updates the operator source based on a given operator.
    * @param op The operator.
    * @param bMixed The mixed source flag.
    * @return The new mixed source flag.
    */
   protected final boolean addSource(Operator op, boolean bMixed)
   {
      if (!op.isConstant() && !bMixed)
      {
         m_source = (m_source == null) ? op.getSource() : findCommonSource(m_source, op.getSource());
         bMixed = (m_source == null);
      }

      return bMixed;
   }

   /**
    * Sets the operator value source according to adapter capabilities.
    * @param source The operator source to set.
    * @param nFlags Normalization flags (combination of NORMALIZE_* constants).
    */
   protected final void setSource(Source source, int nFlags)
   {
      m_source = (source != null && source.getAdapter().isSupported(this, nFlags)) ? source : null;
   }

   /**
    * Sets the operator value source.
    * @param source The operator source to set.
    */
   public final void setSource(Source source)
   {
      m_source = source;
   }

   /**
    * @return The operator value source.
    */
   public final Source getSource()
   {
      return m_source;
   }

   /**
    * The source query object or null if there is no source.
    */
   public Query getQuery()
   {
      return (m_source == null) ? null : m_source.getQuery();
   }

   /**
    * Visits the operator tree in the specified order, starting with this node.
    * @param visitor The tree visitor.
    * @param nFlags Combination of the VISIT_* constant bits.
    */
   public abstract boolean visit(Visitor visitor, int nFlags);

   /**
    * Initializes the operator after its operands have been set up.
    */
   public void initialize()
   {
   }

   /**
    * Normalizes the operator. Can modify or replace the operator node or its
    * children, change its constant flag and values etc.
    * @param nFlags Normalization flags, Operator.NORMALIZE_*
    * @return The normalized operator. Can be a different operator or instance.
    */
   public Operator normalize(int nFlags)
   {
      return this;
   }

   /**
    * Evaluates the operator.
    * @return The result.
    */
   protected abstract Object evaluate();

   /**
    * Computes the common source.
    * @param left The first source.
    * @param right The second source.
    * @return The common source, or null if there is none.
    */
   public static Source findCommonSource(Source left, Source right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      if (right instanceof Field && !(left instanceof Field))
      {
         return right.findCommon(left);
      }

      return left.findCommon(right);
   }

   /**
    * Computes the common source.
    * @param left The first operator.
    * @param right The second operator.
    * @return The common source, or null if there is none.
    */
   public static Source findCommonSource(Operator left, Operator right)
   {
      if (left.isConstant())
      {
         return right.getSource();
      }

      if (right.isConstant())
      {
         return left.getSource();
      }

      return findCommonSource(left.getSource(), right.getSource());
   }

   /**
    * Computes the common source.
    * @param source The other source.
    * @return The common source, or null if there is none.
    */
   public Source findCommonSource(Source source)
   {
      if (isConstant())
      {
         return source;
      }

      return findCommonSource(getSource(), source);
   }

   /**
    * @return The system operator type conversion mapper.
    */
   public static ConversionMapper getConversionMapper()
   {
      return s_conversionMapper;
   }

   /**
    * Gets the S-expression for this operator and its children.
    * @param root The root query node.
    * @return The S-expression for this operator.
    */
   public abstract Object getExpression(Query root);

   /**
    * Copies the shallow state from the specified operator of the same type.
    * @param src The source operator.
    */
   public void copy(Operator src)
   {
      m_bConstant = src.m_bConstant;
      m_value = src.m_value;
      m_type = src.m_type;
      m_parent = src.m_parent;
      m_source = src.m_source;
   }

   /**
    * @see java.lang.Comparable#compareTo(java.lang.Object)
    */
   public int compareTo(Object obj)
   {
      if (!(obj instanceof Operator))
      {
         return getClass().getName().compareTo(obj.getClass().getName());
      }

      return getOrdinal() - ((Operator)obj).getOrdinal();
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

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return getOrdinal();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return PrintWriter.toString(this);
   }

   // inner classes

   /**
    * Operator visitor interface.
    */
   public interface Visitor
   {
      /**
       * Visits an operator.
       * @param op The operator to visit.
       * @return True if the iteration on the same level should continue.
       */
      boolean visit(Operator op);

      /**
       * Checks if an operator is eligible for visiting.
       * @param op The operator to check.
       * @return true if it is eligible for visiting.
       */
      boolean isEligible(Operator op);
   }
}
