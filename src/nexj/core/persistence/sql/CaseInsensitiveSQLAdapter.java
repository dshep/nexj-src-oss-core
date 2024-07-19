// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.util.Iterator;

import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.persistence.Field;
import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.persistence.operator.AttributeOperator;
import nexj.core.persistence.operator.ComparisonOperator;
import nexj.core.persistence.operator.InOperator;

/**
 * Base for SQL persistence adapters implementing case-insensitive search.
 */
public abstract class CaseInsensitiveSQLAdapter extends SQLAdapter
{
   // constants

   /**
    * No case-insensitive prefix.
    */
   protected final static int CI_NONE = 0;

   /**
    * Attribute case-insensitive prefix.
    */
   protected final static int CI_ATTR = 1;

   /**
    * Expression case-insensitive prefix.
    */
   protected final static int CI_EXPR = 2;

   // attributes

   /**
    * The case insensitivity flag.
    */
   protected boolean m_bCaseInsensitive = true;

   // operations

   /**
    * Sets the case insensitivity flag.
    * @param bCaseInsensitive The case insensitivity flag to set.
    */
   public void setCaseInsensitive(boolean bCaseInsensitive)
   {
      m_bCaseInsensitive = bCaseInsensitive;
   }

   /**
    * @return The case insensitivity flag.
    */
   public boolean isCaseInsensitive()
   {
      return m_bCaseInsensitive;
   }

   /**
    * Determines the required case-insensitivity wrapping of an operand.
    * @return The operand.
    * @return One of the CI_* constants.
    */
   protected int getCaseInsensitivity(Operator op)
   {
      if (op.getOrdinal() != AttributeOperator.ORDINAL)
      {
         return CI_EXPR;
      }

      AttributeOperator aop = (AttributeOperator)op;

      if (aop.getConverter() != null)
      {
         if (aop.isNoConversion())
         {
            return CI_NONE;
         }

         return CI_EXPR;
      }

      Object item = aop.getSource().getItem();

      if (item instanceof Column && ((Column)item).isCaseInsensitive())
      {
         return CI_ATTR;
      }

      return CI_NONE;
   }

   /**
    * Appends a case conversion prefix to a string buffer.
    * @param buf The string buffer.
    * @param nCI One of the CI_* constants.
    * @return The case conversion suffix.
    */
   protected abstract String appendCaseConversion(StringBuffer buf, int nCI);

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendSortPrefix(java.lang.StringBuffer, nexj.core.persistence.Operator)
    */
   public String appendSortPrefix(StringBuffer buf, Operator op)
   {
      if (!isCaseConverted(op))
      {
         return null;
      }

      return appendCaseConversion(buf, getCaseInsensitivity(op));
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendGroupPrefix(java.lang.StringBuffer, nexj.core.persistence.Operator)
    */
   public String appendGroupPrefix(StringBuffer buf, Operator op)
   {
      if (!isCaseConverted(op))
      {
         return null;
      }

      return appendCaseConversion(buf, getCaseInsensitivity(op));
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendComparisonPrefix(java.lang.StringBuffer, nexj.core.persistence.Operator)
    */
   public String appendComparisonPrefix(StringBuffer buf, Operator op)
   {
      if (!m_bCaseInsensitive)
      {
         return null;
      }

      if (op.getParent() instanceof ComparisonOperator)
      {
         ComparisonOperator cmp = (ComparisonOperator)op.getParent();

         if (cmp.getLeft().getType() != Primitive.STRING)
         {
            return null;
         }

         int nLeftCI = getCaseInsensitivity(cmp.getLeft());

         if (nLeftCI == CI_NONE)
         {
            return null;
         }

         int nRightCI = getCaseInsensitivity(cmp.getRight());

         if (nRightCI == CI_NONE)
         {
            return null;
         }

         return appendCaseConversion(buf, (op == cmp.getLeft()) ? nLeftCI : nRightCI);
      }

      if (op.getParent() instanceof InOperator)
      {
         InOperator in = (InOperator)op.getParent();
         Operator first = in.getOperand(0);

         if (first.getType() != Primitive.STRING)
         {
            return null;
         }

         int nCI = getCaseInsensitivity(first);

         if (nCI == CI_NONE)
         {
            return null;
         }

         return appendCaseConversion(buf, (op == first) ? nCI : CI_EXPR);
      }

      return null;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendSuffix(java.lang.StringBuffer, java.lang.String)
    */
   public void appendSuffix(StringBuffer buf, String sSuffix)
   {
      if (sSuffix == null)
      {
         return;
      }

      // Comparing by reference, as the constant strings are interned
      if (sSuffix == "$")
      {
         int i = buf.length() - 1;
         char ch = buf.charAt(i);

         // Skip the (+) join suffix
         if (ch == ')')
         {
            i -= 3;
            ch = buf.charAt(i);
         }

         // Remove the quotes
         if (ch == '"')
         {
            buf.setCharAt(i, '$');
            buf.deleteCharAt(buf.lastIndexOf("\"", i));
         }
         else
         {
            buf.insert(i + 1, '$');
         }
      }
      else
      {
         buf.append(sSuffix);
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isCaseConverted(nexj.core.persistence.Operator)
    */
   public boolean isCaseConverted(Operator op)
   {
      return m_bCaseInsensitive && op.getType() == Primitive.STRING;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isCaseConverted(nexj.core.persistence.Field)
    */
   public boolean isCaseConverted(Field field)
   {
      Object item = field.getItem();

      return item instanceof Column && isCaseConverted((Column)item);
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isCaseConverted(nexj.core.meta.persistence.sql.Column)
    */
   public boolean isCaseConverted(Column column)
   {
      return m_bCaseInsensitive && column.isCaseInsensitive();
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendCaseConvertedColumn(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Column)
    */
   public void appendCaseConvertedColumn(StringBuffer buf, Column column)
   {
      buf.append(column.getName());
      buf.append('$');
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendExtraOutputFields(java.lang.StringBuffer, nexj.core.persistence.Query, nexj.core.persistence.sql.SQLGenerator)
    */
   public void appendExtraOutputFields(StringBuffer buf, Query query, SQLGenerator gen)
   {
      // Append the uppercase shadow columns
      if (m_bCaseInsensitive && gen.getSubquery() != null)
      {
         for (Iterator itr = query.getFieldIterator(); itr.hasNext();)
         {
            Field field = (Field)itr.next();
            Object mapping = field.getMapping();

            if (mapping instanceof SQLJoin)
            {
               SQLJoin join = (SQLJoin)mapping;
               Column column = (Column)field.getItem();

               if (column.isCaseInsensitive())
               {
                  buf.append(", ");
                  buf.append(join.alias);
                  buf.append('.');
                  buf.append(column.getName());
                  buf.append("$ ");
                  buf.append(join.alias);
                  buf.append('_');
                  buf.append(SQLGenerator.getAlias(column.getOrdinal()));
                  buf.append('$');
               }
            }
         }
      }
   }
}
