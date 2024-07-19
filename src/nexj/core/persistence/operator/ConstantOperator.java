// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import java.io.IOException;

import nexj.core.meta.Primitive;
import nexj.core.persistence.OIDHolder;
import nexj.core.persistence.Query;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.Symbol;
import nexj.core.util.PrintWriter;

/**
 * Constant expression node.
 */
public final class ConstantOperator extends PrimitiveOperator
{
   // constants

   /**
    * The operator ordinal number.
    */
   public final static int ORDINAL = 0;

   /**
    * The operator priority.
    */
   public final static int PRIORITY = 9;

   // constructors

   /**
    * Creates a constant node representing a given value.
    * @param value The constant value.
    */
   public ConstantOperator(Object value)
   {
      m_value = value;
      m_type = Primitive.primitiveOf(value);
      m_bConstant = true;
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
      return null;
   }

   /**
    * @see nexj.core.persistence.Operator#evaluate()
    */
   protected Object evaluate()
   {
      return m_value;
   }

   /**
    * @see nexj.core.persistence.Operator#visit(nexj.core.persistence.Operator.Visitor, int)
    */
   public boolean visit(Visitor visitor, int nFlags)
   {
      return visitor.visit(this);
   }

   /**
    * @see nexj.core.persistence.Operator#getExpression(nexj.core.persistence.Query)
    */
   public Object getExpression(Query root)
   {
      if (m_value instanceof OIDHolder)
      {
         return ((OIDHolder)m_value).getOID();
      }

      return m_value;
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

      Object value = ((ConstantOperator)obj).getValue();

      return ((m_value == null) ? 0 : m_value.hashCode()) -
         ((value == null) ? 0 : value.hashCode());
   }

   /**
    * @see nexj.core.util.Printable#printOn(PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      Intrinsic.write(m_value, writer, false);
   }
}
