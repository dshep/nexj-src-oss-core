// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Attribute;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.IndexColumn;
import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalMapping;
import nexj.core.meta.persistence.sql.RelationalPrimitiveMapping;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.persistence.Converter;
import nexj.core.persistence.Field;
import nexj.core.persistence.InvalidQueryException;
import nexj.core.persistence.Operator;
import nexj.core.persistence.PersistenceAdapter;
import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.Query;
import nexj.core.persistence.Source;
import nexj.core.persistence.operator.AggregateOperator;
import nexj.core.persistence.operator.AndOperator;
import nexj.core.persistence.operator.AnyOperator;
import nexj.core.persistence.operator.AttributeOperator;
import nexj.core.persistence.operator.BinaryOperator;
import nexj.core.persistence.operator.ComparisonOperator;
import nexj.core.persistence.operator.ConstantOperator;
import nexj.core.persistence.operator.CountOperator;
import nexj.core.persistence.operator.DivisionOperator;
import nexj.core.persistence.operator.EqualsOperator;
import nexj.core.persistence.operator.FunctionOperator;
import nexj.core.persistence.operator.GreaterThanOperator;
import nexj.core.persistence.operator.GreaterThanOrEqualsOperator;
import nexj.core.persistence.operator.IfOperator;
import nexj.core.persistence.operator.InOperator;
import nexj.core.persistence.operator.LessThanOperator;
import nexj.core.persistence.operator.LessThanOrEqualsOperator;
import nexj.core.persistence.operator.LikeOperator;
import nexj.core.persistence.operator.Logical;
import nexj.core.persistence.operator.MatchOperator;
import nexj.core.persistence.operator.MinusOperator;
import nexj.core.persistence.operator.MultiArgOperator;
import nexj.core.persistence.operator.MultiplicationOperator;
import nexj.core.persistence.operator.NegationOperator;
import nexj.core.persistence.operator.NotEqualsOperator;
import nexj.core.persistence.operator.NotOperator;
import nexj.core.persistence.operator.OrOperator;
import nexj.core.persistence.operator.PlusOperator;
import nexj.core.persistence.operator.Quantor;
import nexj.core.persistence.operator.TypeConversionOperator;
import nexj.core.persistence.operator.UnaryOperator;
import nexj.core.persistence.sql.SQLAdapter.Bind;
import nexj.core.persistence.sql.SQLAdapter.OrderByPrefix;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Holder;
import nexj.core.util.Lookup;

/**
 * SQL statement generator.
 */
public class SQLGenerator
{
   // constants

   /**
    * ANSI joins.
    */
   public final static int JOIN_ANSI = 0;

   /**
    * Oracle joins.
    */
   public final static int JOIN_ORACLE = 1;

   /**
    * Generating the output fields.
    */
   public final static int GEN_OUTPUT = 0;

   /**
    * Generating a join condition.
    */
   public final static int GEN_JOIN = 1;

   /**
    * Generating a where clause.
    */
   public final static int GEN_WHERE = 2;

   /**
    * Generating a group by expression.
    */
   public final static int GEN_GROUPBY = 3;

   /**
    * Generating a having expression.
    */
   public final static int GEN_HAVING = 4;

   /**
    * Generating an order by expression.
    */
   public final static int GEN_ORDERBY = 5;

   /**
    * The main select.
    */
   protected final static int SEL_MAIN = 0;

   /**
    * Subquery select.
    */
   protected final static int SEL_SUB = 1;

   /**
    * Exist subquery.
    */
   protected final static int SEL_EXIST = 2;

   /**
    * The subquery alias prefix.
    */
   protected final static String SUBQUERY_PREFIX = "Q_";

   // attributes

   /**
    * The table count.
    */
   protected int m_nTableCount;

   /**
    * The output field count.
    */
   protected int m_nOutputFieldCount;

   /**
    * The bind variable count.
    */
   protected int m_nBindCount;

   /**
    * The substring offset array element count.
    */
   protected int m_nOffsetCount;

   /**
    * The join syntax, one of the JOIN_* constants.
    */
   protected int m_nJoin;

   /**
    * The SQL generation mode - one of the GEN_* constants.
    */
   protected int m_nGenMode;

   /**
    * The generated SQL.
    */
   protected String m_sSQL;

   /**
    * The default schema owner. 
    */
   protected String m_sOwner;

   // associations

   /**
    * The query.
    */
   protected Query m_query;

   /**
    * The currently generated subquery.
    */
   protected Query m_subquery;

   /**
    * The output field array.
    */
   protected Field[] m_outputFieldArray;

   /**
    * The output field map: Field[Field]
    */
   protected Lookup m_outputFieldMap;

   /**
    * The column set for subquery output field deduplication: [Column].
    */
   protected Holder m_columnSet;

   /**
    * The bind variable array: Column.Bind[2*n], Object[2*n+1].
    */
   protected Object[] m_bindArray;

   /**
    * The substring offset array: nOffset[2*n], nLength[2*n+1].
    */
   protected int[] m_offsetArray;

   /**
    * The associated SQL adapter.
    */
   protected SQLAdapter m_adapter;

   /**
    * The predefined alias array.
    */
   protected final static String[] s_aliasArray = new String[]
   {
      "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
      "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "AA", "AB", "AC", "AD",
      "AE", "AF", "AG", "AH", "AI", "AJ",  "AK", "AL", "AM", "AN", "A0", "AP", "AQ",
      "AR", "AU", "AV", "AW", "AX", "AY", "AZ", "BA", "BB", "BC", "BD", "BE", "BF",
      "BG", "BH", "BI", "BJ", "BK", "BL", "BM", "BN", "BO", "BP", "BQ", "BR", "BS",
      "BT", "BU", "BV", "BW", "BX", "BZ", "CA", "CB", "CC", "CD", "CE", "CF", "CG",
      "CH", "CI", "CJ", "CK", "CL", "CM", "CN", "CO", "CP", "CQ", "CR", "CS", "CT",
      "CU", "CV", "CW", "CX", "CY"
   };

   /**
    * The operator lookup array: OperatorAppender[nOperatorOrdinal].
    */
   protected final static OperatorAppender[] s_appenderArray = new OperatorAppender[Operator.MAX_COUNT];

   static
   {
      s_appenderArray[OrOperator.ORDINAL] = new MultiArgOperatorAppender(" or ");
      s_appenderArray[AndOperator.ORDINAL] = new MultiArgOperatorAppender(" and ");
      s_appenderArray[EqualsOperator.ORDINAL] = new EqualityComparisonOperatorAppender(" = ", " is null", " and ");
      s_appenderArray[NotEqualsOperator.ORDINAL] = new EqualityComparisonOperatorAppender(" <> ", " is not null", " or ");
      s_appenderArray[GreaterThanOperator.ORDINAL] = new ComparisonOperatorAppender(" > ");
      s_appenderArray[GreaterThanOrEqualsOperator.ORDINAL] = new ComparisonOperatorAppender(" >= ");
      s_appenderArray[LessThanOperator.ORDINAL] = new ComparisonOperatorAppender(" < ");
      s_appenderArray[LessThanOrEqualsOperator.ORDINAL] = new ComparisonOperatorAppender(" <= ");

      s_appenderArray[LikeOperator.ORDINAL] = new OperatorAppender()
      {
         public void appendOperator(StringBuffer buf, Operator op, SQLGenerator gen)
         {
            LikeOperator like = (LikeOperator)op;
            Operator left = like.getLeft();
            Operator right = like.getRight();

            gen.appendComparisonOperand(buf, left);
            gen.m_adapter.appendLikeStatement(buf);

            gen.appendComparisonOperand(buf, right);
            gen.m_adapter.appendLikeEscape(buf);

            if (left instanceof AttributeOperator &&
               ((AttributeOperator)left).getConverter() == null)
            {
               Column column = (Column)((Field)left.getSource()).getItem();
               String s = (String)right.getValue();

               if (!gen.m_adapter.isLiteral() && !column.isLiteral() ||
                  !gen.m_adapter.isLiteral(column.getType(), s))
               {
                  if (s == null)
                  {
                     buf.append(" /* null */");
                  }
                  else
                  {
                     int n = Math.min(s.length(), 4);
                     int i;

                     for (i = 0; i < n; ++i)
                     {
                        char ch = s.charAt(i);

                        if (ch == '*' || ch == '?')
                        {
                           break;
                        }
                     }

                     buf.append(" /* ");

                     if (n > 0)
                     {
                        buf.append(i);
                     }

                     buf.append(" */");
                  }
               }
            }
         }
      };

      s_appenderArray[MatchOperator.ORDINAL] = new OperatorAppender()
      {
         public void appendOperator(StringBuffer buf, Operator op, SQLGenerator gen)
         {
            MatchOperator match = (MatchOperator)op;
            Field field = (Field)match.getAttribute().getSource();

            gen.m_adapter.appendMatchStatement(buf, ((SQLJoin)field.getMapping()).alias,
               (Column)field.getItem(), (SQLJoin)match.getMapping(), match.getExpression());
         }
      };

      s_appenderArray[InOperator.ORDINAL] = new OperatorAppender()
      {
         public void appendOperator(StringBuffer buf, Operator op, SQLGenerator gen)
         {
            InOperator in = (InOperator)op;
            int nLastOperand = in.getOperandCount() - 1;
            int nCount = gen.m_adapter.roundUpListSize(nLastOperand, gen.m_nBindCount) + 1;
            int nMaxListSize = gen.m_adapter.isSupported(in) ? gen.m_adapter.getMaxListSize() : 1;
            int i = 1;

            if (nCount - 1 > nMaxListSize)
            {
               buf.append('(');
            }

            for (;;)
            {
               gen.appendComparisonOperand(buf, in.getOperand(0));

               int nStart = i;
               int nEnd = i + Math.min(nCount - i, nMaxListSize);

               if (nEnd - nStart == 1)
               {
                  buf.append(" = ");
                  gen.appendComparisonOperand(buf, in.getOperand(Math.min(i, nLastOperand)));
                  ++i;
               }
               else
               {
                  buf.append(" in (");

                  for (; i < nEnd; ++i)
                  {
                     if (i > nStart)
                     {
                        buf.append(", ");
                     }

                     gen.appendComparisonOperand(buf, in.getOperand(Math.min(i, nLastOperand)));
                  }

                  buf.append(')');
               }

               if (nEnd == nCount)
               {
                  break;
               }

               buf.append(" or ");
            }

            if (nCount - 1 > nMaxListSize)
            {
               buf.append(')');
            }
         }
      };

      s_appenderArray[PlusOperator.ORDINAL] = new BinaryOperatorAppender(" + ");
      s_appenderArray[MinusOperator.ORDINAL] = new BinaryOperatorAppender(" - ");
      s_appenderArray[MultiplicationOperator.ORDINAL] = new BinaryOperatorAppender(" * ");
      s_appenderArray[DivisionOperator.ORDINAL] = new BinaryOperatorAppender(" / ");
      s_appenderArray[NegationOperator.ORDINAL] = new UnaryOperatorAppender("-");

      s_appenderArray[NotOperator.ORDINAL] = new OperatorAppender()
      {
         public void appendOperator(StringBuffer buf, Operator op, SQLGenerator gen)
         {
            Operator operand = ((UnaryOperator)op).getOperand();

            if (operand.getOrdinal() != AttributeOperator.ORDINAL ||
               !gen.appendBooleanAttribute(buf, (AttributeOperator)operand, op.getParent(), false))
            {
               buf.append("not ");
               gen.appendOperator(buf, operand);
            }
         }
      };

      s_appenderArray[TypeConversionOperator.ORDINAL] = new OperatorAppender()
      {
         public void appendOperator(StringBuffer buf, Operator op, SQLGenerator gen)
         {
            Operator operand = ((TypeConversionOperator)op).getOperand();

            gen.m_adapter.appendTypeConversion(buf, operand,
               (Primitive)operand.getType(), (Primitive)op.getType(), gen);
         }
      };

      s_appenderArray[AttributeOperator.ORDINAL] = new OperatorAppender()
      {
         public void appendOperator(StringBuffer buf, Operator op, SQLGenerator gen)
         {
            Source source = op.getSource();

            if (!(source instanceof Field))
            {
               throw new PersistenceException("err.persistence.operandType",
                  new Object[]{(op.getParent() != null) ? op.getParent().getSymbol() : op.getSymbol(),
                  source.getQuery().getMetaclass().getName()});
            }

            AttributeOperator aop = (AttributeOperator)op;

            if (source.getAdapter() != gen.getAdapter())
            {
               Query query = source.getQuery().getRoot();
               Attribute attribute = query.getAttribute();

               if (attribute == null && gen.getQuery().getParent() != null)
               {
                  query = gen.getQuery().getParent().getRoot();
                  attribute = query.getAttribute();
               }

               throw new InvalidQueryException("err.persistence.heteroJoin",
                  (attribute == null) ? new Object[]{"", query.getMetaclass().getName()} :
                     new Object[]{attribute.getName(), attribute.getMetaclass().getName()});
            }

            if (!gen.appendBooleanAttribute(buf, aop, op.getParent(), true))
            {
               Converter converter = aop.getConverter();
               String sSuffix = gen.appendUngroupPrefix(buf, aop);

               if (converter == null)
               {
                  gen.appendField(buf, (Field)source);
               }
               else if (converter instanceof SQLConverter)
               {
                  ((SQLConverter)converter).appendConversion(buf, (Field)source, gen);
               }
               else
               {
                  throw new PersistenceException("err.persistence.sql.converter",
                     new Object[]{op.toString(), SQLGenerator.class.getName()});
               }

               gen.m_adapter.appendSuffix(buf, sSuffix);
            }
         }
      };

      s_appenderArray[AnyOperator.ORDINAL] = new OperatorAppender()
      {
         public void appendOperator(StringBuffer buf, Operator op, SQLGenerator gen)
         {
            buf.append("exists(select 1");

            AnyOperator any = (AnyOperator)op;
            gen.appendQuantor(buf, any, true); 
            buf.append(')');
         }
      };

      s_appenderArray[IfOperator.ORDINAL] = new OperatorAppender()
      {
         public void appendOperator(StringBuffer buf, Operator op, SQLGenerator gen)
         {
            IfOperator iop = (IfOperator)op;

            buf.append("(case when ");
            gen.appendOperator(buf, iop.getCondition());
            buf.append(" then ");
            gen.appendOperator(buf, iop.getThen());
            buf.append(" else ");
            gen.appendOperator(buf, iop.getElse());
            buf.append(" end)");
         }
      };

      s_appenderArray[FunctionOperator.ORDINAL] = new OperatorAppender()
      {
         public void appendOperator(StringBuffer buf, Operator op, SQLGenerator gen)
         {
            FunctionOperator fun = (FunctionOperator)op;
            Symbol sym = fun.getSymbol();
            SQLAdapter adapter = gen.getAdapter();
            String sSuffix;

            if (Symbol.STRING_LENGTH.equals(sym))
            {
               sSuffix = adapter.appendStringLengthPrefix(buf, fun);
               gen.appendOperator(buf, fun.getOperand(0));
            }
            else if (Symbol.SUBSTRING.equals(sym))
            {
               sSuffix = adapter.appendSubstringPrefix(buf, fun);
               gen.appendOperator(buf, fun.getOperand(0));
               buf.append(", ");

               try
               {
                  PlusOperator pos = new PlusOperator();

                  pos.setLeft(fun.getOperand(1));
                  pos.setRight(new ConstantOperator(Primitive.ONE_INTEGER));
                  pos.normalize(Operator.NORMALIZE_NORECUR);
                  gen.appendOperator(buf, pos);
                  buf.append(", ");

                  MinusOperator len = new MinusOperator();

                  len.setLeft(fun.getOperand(2));
                  len.setRight(fun.getOperand(1));
                  len.normalize(Operator.NORMALIZE_NORECUR);
                  gen.appendOperator(buf, len);
               }
               finally
               {
                  fun.setOperandParent();
               }
            }
            else
            {
               throw new IllegalStateException();
            }

            adapter.appendSuffix(buf, sSuffix);
         }
      };

      s_appenderArray[AggregateOperator.ORDINAL] = new OperatorAppender()
      {
         public void appendOperator(StringBuffer buf, Operator op, SQLGenerator gen)
         {
            AggregateOperator aop = (AggregateOperator)op;
            Symbol sym = aop.getSymbol();
            SQLAdapter adapter = gen.getAdapter();
            String sSuffix;

            boolean bQuantor = aop.isQuantor();

            if (bQuantor)
            {
               buf.append("(select ");
            }

            if (Symbol.COUNT.equals(sym))
            {
               sSuffix = adapter.appendCountPrefix(buf, aop);
            }
            else if (Symbol.SUM.equals(sym))
            {
               sSuffix = adapter.appendSumPrefix(buf, aop);
            }
            else if (Symbol.AVERAGE.equals(sym))
            {
               sSuffix = adapter.appendAveragePrefix(buf, aop);
            }
            else if (Symbol.MINIMUM.equals(sym))
            {
               sSuffix = adapter.appendMinimumPrefix(buf, aop);
            }
            else if (Symbol.MAXIMUM.equals(sym))
            {
               sSuffix = adapter.appendMaximumPrefix(buf, aop);
            }
            else
            {
               throw new IllegalStateException();
            }

            boolean bCount = (aop instanceof CountOperator && aop.getOperand().getType() == Primitive.BOOLEAN); 

            if (bCount)
            {
               buf.append("*");
            }
            else
            {
               if (aop.isUnique())
               {
                  buf.append("distinct ");
               }

               gen.appendOperator(buf, aop.getOperand());
            }

            adapter.appendSuffix(buf, sSuffix);

            if (bQuantor)
            {
               gen.appendQuantor(buf, aop, bCount);
               buf.append(')');
            }

            sSuffix = null;

            adapter.appendSuffix(buf, sSuffix);
         }
      };
   }

   // constructors

   /**
    * Creates an SQL generator for a given query.
    * @param query The query.
    */
   public SQLGenerator(Query query, SQLAdapter adapter)
   {
      m_query = query;
      m_adapter = adapter;
      m_nJoin = adapter.getJoinSyntax();
   }

   // operations

   /**
    * Sets the query.
    * @param query The query to set.
    */
   public void setQuery(Query query)
   {
      m_query = query;
   }

   /**
    * @return The query.
    */
   public Query getQuery()
   {
      return m_query;
   }

   /**
    * Sets the currently generated subquery.
    * @param subquery The currently generated subquery to set.
    */
   public void setSubquery(Query subquery)
   {
      m_subquery = subquery;
   }

   /**
    * @return The currently generated subquery.
    */
   public Query getSubquery()
   {
      return m_subquery;
   }

   /**
    * @return The generated SQL.
    */
   protected String getSQL()
   {
      return m_sSQL;
   }

   /**
    * @return The SQL adapter.
    */
   public SQLAdapter getAdapter()
   {
      return m_adapter;
   }

   /**
    * @return The SQL generation mode - one of the GEN_* constants.
    */
   public int getGenMode()
   {
      return m_nGenMode;
   }

   /**
    * @return The default owner of database objects (function/procedure/table/view).
    */
   public String getOwner()
   {
      if (m_sOwner == null)
      {
         m_sOwner = m_adapter.createSchemaManager((RelationalDatabase)m_query
            .getPersistenceMapping().getDataSource()).getOwner();
      }

      return m_sOwner;
   }

   /**
    * Adds a table to a given query.
    * @param queru The query to which to add the table.
    * @param table The table to add.
    * @return The SQLJoin corresponding to the table.
    */
   protected SQLJoin addTable(Query query, Table table)
   {
      SQLJoin join = (SQLJoin)query.getMapping();

      if (join == null)
      {
         join = new SQLJoin(query, null, table, getNewAlias());
         query.setMapping(join);

         return join;
      }

      SQLJoin primary = join;

      for (;;)
      {
         if (join.table == table)
         {
            return join;
         }

         if (join.next == null)
         {
            join.next = new SQLJoin(query, primary, table, getNewAlias());
            join.isEnabled = true;
            join = join.next;

            join.sourceKey = join.parent.table.getPrimaryKey();
            join.destinationKey = join.table.getPrimaryKey();

            return join;
         }

         join = join.next;
      }
   }

   /**
    * Adds an output field to the query and initializes its ordinal number.
    * @param field The output field to add and initialize.
    */
   protected final void addOutputField(Field field)
   {
      if (m_outputFieldMap == null)
      {
         m_outputFieldArray = new Field[16];
         m_outputFieldMap = new HashTab(16);
         m_outputFieldArray[m_nOutputFieldCount] = field;
         field.setOrdinal(m_nOutputFieldCount++);
         m_outputFieldMap.put(field, field);
      }
      else
      {
         Object obj = m_outputFieldMap.get(field);

         if (obj != null)
         {
            field.setOrdinal(((Field)obj).getOrdinal());
         }
         else
         {
            if (m_nOutputFieldCount == m_outputFieldArray.length)
            {
               Field[] outputFieldArray = new Field[m_nOutputFieldCount << 1];

               System.arraycopy(m_outputFieldArray, 0, outputFieldArray, 0, m_nOutputFieldCount);
               m_outputFieldArray = outputFieldArray;
            }

            m_outputFieldArray[m_nOutputFieldCount] = field;
            field.setOrdinal(m_nOutputFieldCount++);
            m_outputFieldMap.put(field, field);
         }
      }
   }

   /**
    * Gets an output field by ordinal number.
    * @param nOrdinal The output field ordinal number.
    * @return The output field.
    */
   public final Field getOutputField(int nOrdinal)
   {
      assert nOrdinal >= 0 && nOrdinal < m_nOutputFieldCount;

      return m_outputFieldArray[nOrdinal];
   }

   /**
    * @return The number of output fields.
    */
   public final int getOutputFieldCount()
   {
      return m_nOutputFieldCount;
   }

   /**
    * Adds the tables to the query.
    */
   protected void addMappings()
   {
      m_query.visit(new Query.Visitor()
      {
         public boolean visit(final Query query)
         {
            boolean bHomogeneous = (query.getRoot() == m_query);
            Query primarySource = query, fieldSource = null;
            SQLJoin primaryJoin, fieldJoin = null;
            Index primaryKey, fieldKey = null;

            if (query.isRoot() && bHomogeneous)
            {
               primaryJoin = addTable(query, ((RelationalMapping)query.getPersistenceMapping()).getPrimaryTable());
               primaryJoin.isInner = true;
               primaryJoin.isEnabled = true;
               primaryKey = primaryJoin.table.getPrimaryKey();

               if (query.isJoin())
               {
                  Index destinationKey = (Index)query.getKey(true);
                  int nDKColumnCount = destinationKey.getIndexColumnCount();
                  Field[] dkFieldArray = new Field[nDKColumnCount];
                  SQLJoin join = addTable(query, destinationKey.getTable());

                  join.isInner = true;
                  join.isEnabled = true;

                  for (int i = 0; i < nDKColumnCount; ++i)
                  {
                     Column column = destinationKey.getIndexColumn(i).getColumn();

                     addOutputField(dkFieldArray[i] = new Field(query, join, column,
                        column.getValueType(), m_adapter.getConverter(column.getValueType(), column),
                        m_adapter.getBind(column), true));
                  }

                  query.setChildItem(dkFieldArray);
               }
            }
            else
            {
               Query source = query;
               Index sourceKey;

               do
               {
                  sourceKey = (Index)source.getKey(false);
                  source = source.getParent();
               }
               while (
                  source.getWhere() == null &&
                  sourceKey.isObjectKey() &&
                  source.getParent() != null &&
                  source.getKey(true) == sourceKey &&
                  source.getParent().isSameRoot(query) &&
                  (source.isRequired() || !source.getKey(false).isObjectKey()));

               SQLJoin parent;

               if (sourceKey.isObjectKey())
               {
                  parent = (SQLJoin)source.getMapping();
                  sourceKey = parent.table.getPrimaryKey();
               }
               else
               {
                  parent = addTable(source, sourceKey.getTable());
               }

               parent.isEnabled = true;

               if (bHomogeneous)
               {
                  Index destinationKey = (Index)query.getKey(true);
                  SQLJoin join = addTable(query, destinationKey.getTable());

                  join.parent = parent;
                  join.sourceKey = sourceKey;
                  join.destinationKey = destinationKey;

                  // Decide whether we can use the left joined table
                  // source key instead of the right joined table primary key
                  // to reduce the number of indirections in the SQL query
                  if (query.getWhere() == null &&
                     (destinationKey.isObjectKey() &&
                        (!query.isOutput() ||
                           (!query.getKey(false).isObjectKey() || query.isRequired()) &&
                           query.getFieldCount() == 0 &&
                           query.getAssocCount(Query.ASSOC_QUERY) == 0) ||
                     destinationKey.isObjectKeyPart() &&
                        !query.isOutput() &&
                        query.getFieldCount() == 0 &&
                        query.getAssocCount(Query.ASSOC_QUERY) == 0))
                  {
                     primarySource = query.getParent();
                     primaryJoin = parent;
                     primaryKey = sourceKey;
                  }
                  else
                  {
                     primaryJoin = join;
                     primaryKey = join.table.getPrimaryKey();

                     if (destinationKey.isObjectKeyPart() &&
                        (!sourceKey.isObjectKey() || query.isRequired()))
                     {
                        fieldSource = query.getParent();
                        fieldJoin = parent;
                        fieldKey = sourceKey;
                     }
                  }

                  if (query.isRequired())
                  {
                     if (join.parent.isInner)
                     {
                        join.isInner = true;
                     }
                     else
                     {
                        SQLJoin primary = (SQLJoin)parent.query.getMapping();

                        if (parent != primary && primary.isInner && primary.parent != null)
                        {
                           join.isInner = parent.isInner = true;
                        }
                     }
                  }
               }
               else
               {
                  primarySource = query.getParent();
                  primaryJoin = parent;
                  primaryKey = sourceKey;
               }
            }

            primaryJoin.isEnabled = true;

            int nPKColumnCount = primaryKey.getIndexColumnCount();
            Field[] pkFieldArray = new Field[nPKColumnCount];

            for (int i = 0; i < nPKColumnCount; ++i)
            {
               Column column = primaryKey.getIndexColumn(i).getColumn();

               pkFieldArray[i] = new Field(primarySource, primaryJoin, column,
                  column.getValueType(), m_adapter.getConverter(column.getValueType(), column),
                  m_adapter.getBind(column), true);
            }

            if (query.getParent() != null && query.getParentItem() == null)
            {
               query.setParentItem(pkFieldArray);
            }

            if (bHomogeneous || !query.isOutput() || query.isLazy())
            {
               // The following conditional relies on the visit order
               // to avoid overwriting the PK in a hetero query
               if (query.getItem() == null)
               {
                  query.setItem(pkFieldArray);

                  if (fieldSource != null)
                  {
                     int nColumnCount = fieldKey.getIndexColumnCount();
                     Field[] fieldArray = new Field[nColumnCount];

                     for (int i = 0; i < nColumnCount; ++i)
                     {
                        Column column = fieldKey.getIndexColumn(i).getColumn();

                        fieldArray[i] = new Field(primarySource, fieldJoin, column,
                           column.getValueType(), m_adapter.getConverter(column.getValueType(), column),
                           m_adapter.getBind(column), true);
                     }

                     query.setFieldItem(fieldArray);
                  }
                  else
                  {
                     query.setFieldItem(pkFieldArray);
                  }
               }
            }

            if (query.isOutput())
            {
               if (query.isIdentity())
               {
                  // Output the primary key columns
                  for (int i = 0; i < nPKColumnCount; ++i)
                  {
                     addOutputField(pkFieldArray[i]);
                  }
               }

               m_query.addOutputQuery(query);
            }

            if (bHomogeneous)
            {
               // Bind the columns to the attributes
               for (Iterator itr = query.getFieldIterator(); itr.hasNext();)
               {
                  Field field = (Field)itr.next();

                  if (!field.isPersistent())
                  {
                     continue;
                  }

                  Operator op = field.getOperator();

                  if (op != null)
                  {
                     if (op.getType() instanceof Primitive)
                     {
                        field.setBind(m_adapter.getBind((Primitive)op.getType()));
                     }
                  }
                  else
                  {
                     Column column = ((RelationalPrimitiveMapping)field.getAttributeMapping()).getColumn();
                     SQLJoin join = addTable(query, column.getTable());

                     join.isEnabled = true;
                     field.setMapping(join);
                     field.setItem(column);
                     field.setConverter(m_adapter.getConverter((Primitive)field.getAttribute().getType(), column));
                     field.setBind(m_adapter.getBind(column));
                  }

                  // Add the output columns to the join
                  if (field.isOutput() && field.getBind() != null)
                  {
                     addOutputField(field);
                  }
               }

               assert query.getMapping() != null;
            }

            return true;
         }

         public boolean postVisit(final Query query)
         {
            if (query.isMatch()) // only postprocess operators if any require it
            {
               opVisit(query);
            }

            // Determine if a subquery should be generated
            Operator where = query.getWhere();

            if (where != null && !query.isRoot() && !query.isQuantorRoot())
            {
               for (Iterator itr = query.getAssocIterator(Query.ASSOC_WHERE); itr.hasNext();)
               {
                  Query assoc = (Query)itr.next();

                  if (assoc.isSameRoot(query))
                  {
                     SQLJoin join = (SQLJoin)assoc.getMapping();

                     do
                     {
                        if (join.isEnabled)
                        {
                           query.setSubquery(true);

                           break;
                        }

                        join = join.parent;
                     }
                     while (join != query.getMapping());
                  }
               }

               if (!query.isSubquery())
               {
                  where.visit(new Operator.Visitor()
                  {
                     public boolean visit(Operator op)
                     {
                        if (!m_adapter.isJoinSupported(query, op))
                        {
                           query.setSubquery(true);

                           return false;
                        }

                        return true;
                     }

                     public boolean isEligible(Operator op)
                     {
                        return !op.isConstant();
                     }
                  }, Operator.VISIT_POSTORDER);
               }

               if (query.isSubquery() && query.getFieldItem() != query.getItem())
               {
                  query.setFieldItem(query.getItem());
               }
            }

            return true;
         }

         public boolean isEligible(Query query)
         {
            return query.getRoot() == m_query || query.getParent().getRoot() == m_query;
         }

         /**
          * Visit all operators of a query.
          * @param query The query which operators to visit.
          */
         private void opVisit(final Query query)
         {
            // list of seen objects to corresponding SQLJoins, of max possible size,
            // seenObjectList{MatchOperator1, SQLJoin1, MatchOperator2, SQLJoin2, ...}
            // cannot do lazy init because Visitor requires object to be final
            final List seenObjectList = new ArrayList(query.getFieldCount());

            Operator.Visitor visitor = new Operator.Visitor()
            {
               public boolean isEligible(Operator op)
               {
                  return true; // visit all operators as they might contain MatchOperator in branch
               }

               public boolean visit(Operator op)
               {
                  if (!(op instanceof MatchOperator))
                  {
                     return true; // visit other branches and look for MatchOperators
                  }

                  MatchOperator matchOp = (MatchOperator)op;
                  SQLJoin join = null;

                  for (int i = 0, nCount = seenObjectList.size();
                       i < nCount && join == null;
                       i += 2)
                  {
                     MatchOperator seenOp = (MatchOperator)seenObjectList.get(i);

                     if (seenOp.getAttribute().getSource() == matchOp.getAttribute().getSource() &&
                         seenOp.getExpression().equals(matchOp.getExpression()))
                     {
                        join = (SQLJoin)seenObjectList.get(i + 1);
                     }
                  }

                  // optimize for the case where same join table is used multiple times
                  // by reusing it in any other operators of the same query
                  if (join != null)
                  {
                     matchOp.setMapping(join);
                  }
                  else // if null then not seen before or no join table created
                  {
                     Source source = matchOp.getAttribute().getSource();
                     Table table = m_adapter.getMatchJoin(
                        ((RelationalPrimitiveMapping)source.getAttributeMapping()).getColumn(),
                        matchOp.getExpression());

                     if (table != null) // if adapter needs a table join
                     {
                        join = addTable(source.getQuery(), table);
                        join.isEnabled = true; // enable and set operator value to join alias
                        matchOp.setMapping(join); // for s_appenderArray...appendOperator()
                        seenObjectList.add(matchOp); // store join to reuse later
                        seenObjectList.add(join);   // store join to reuse later
                     }
                  }

                  return true; // visit all branches on this level
               }
            };

            Operator whereOp = query.getWhere();

            if (whereOp != null) // only dereference if there is a "where" condition
            {
               whereOp.visit(visitor, Operator.VISIT_PREORDER);
            }

            for (int i = 0, nCount = query.getOrderByCount(); i < nCount; ++i)
            {
               query.getOrderByOperator(i).visit(visitor, Operator.VISIT_PREORDER);
            }
         }
      }, Query.VISIT_ALL);
   }

   /**
    * Adds a bind variable to the query.
    * @param bind The bind object.
    * @param value The variable value.
    * @return The bind variable ordinal number.
    */
   protected int addBind(Bind bind, Object value)
   {
      int i = m_nBindCount << 1;

      if (m_bindArray == null)
      {
         // Initial size must be a multiple of the structure element count
         m_bindArray = new Object[2 * 8];
      }
      else if (i == m_bindArray.length)
      {
         Object[] bindArray = new Object[i << 1];
         System.arraycopy(m_bindArray, 0, bindArray, 0, i);
         m_bindArray = bindArray;
      }

      m_bindArray[i] = bind;
      m_bindArray[i + 1] = value;

      return m_nBindCount++;
   }

   /**
    * Gets a bind by ordinal number.
    * @param nOrdinal The bind variable ordinal number.
    * @return The SQL bind.
    */
   protected Bind getBind(int nOrdinal)
   {
      assert nOrdinal >= 0 && nOrdinal < m_nBindCount;

      return (Bind)m_bindArray[nOrdinal << 1];
   }

   /**
    * Gets a bind value by ordinal number.
    * @param nOrdinal The bind value ordinal number.
    * @return The bind value.
    */
   protected Object getBindValue(int nOrdinal)
   {
      assert nOrdinal >= 0 && nOrdinal < m_nBindCount;

      return m_bindArray[(nOrdinal << 1) + 1];
   }

   /**
    * @return The bind variable count.
    */
   protected int getBindCount()
   {
      return m_nBindCount;
   }

   /**
    * Removes all the binds.
    */
   protected void clearBinds()
   {
      m_nBindCount = 0;
   }

   /**
    * Generates the SQL for a read query.
    */
   public void generateReadSQL()
   {
      StringBuffer buf = new StringBuffer(256);

      appendReadSQL(buf, m_query, SEL_MAIN, null);
      m_sSQL = buf.toString();
   }

   /**
    * Appends the SQL for a read query to a string buffer.
    * @param buf The string buffer where to append the SQL.
    * @param query The query to generate.
    * @param nSel The type of the select statement being generated, one of the SEL_* constants.
    * @param conj Where clause conjunction of SQLJoin and Operator. Can be null to lazy-allocate.
    * @return The where clause conjunction. Can be null.
    */
   protected Object[] appendReadSQL(final StringBuffer buf, final Query query, final int nSel, Object[] conj)
   {
      Query subquerySaved = m_subquery;
      boolean bGenWhere = (nSel != SEL_SUB ^ query.isSubquery());

      if (nSel == SEL_MAIN)
      {
         m_adapter.appendPrefixHint(buf, query);
      }

      if (nSel != SEL_EXIST)
      {
         int nGenModeSaved = m_nGenMode;

         m_nGenMode = GEN_OUTPUT;
         buf.append("select ");

         if (nSel != SEL_SUB)
         {
            m_adapter.appendInfixHint(buf, query);
            appendOutputFields(buf);
         }
         else
         {
            m_subquery = query;
            appendSubqueryOutputFields(buf, query);
         }

         m_adapter.appendExtraOutputFields(buf, query, this);
         m_nGenMode = nGenModeSaved;
         buf.append(" from ");
      }

      JoinVisitor visitor = new JoinVisitor((m_nJoin == JOIN_ANSI) ? buf : null, conj)
      {
         private boolean m_bFirst = true;

         public boolean visit(Query q)
         {
            if (q.isAlias())
            {
               return true;
            }

            if (!q.isSubquery() || q == query)
            {
               for (SQLJoin join = (SQLJoin)q.getMapping(); join != null; join = join.next)
               {
                  if (!join.isEnabled)
                  {
                     continue;
                  }

                  boolean bJoin = (nSel == SEL_EXIST) ?
                     join.parent.query.getQuantorRoot() == q.getQuantorRoot() :
                     join.parent != null && (q != query || join != q.getMapping());

                  if (m_bFirst)
                  {
                     m_bFirst = false;
                  }
                  else
                  {
                     appendJoinSeparator(buf, join, bJoin);
                  }

                  m_adapter.appendTable(buf, join.table);
                  buf.append(' ');
                  buf.append(join.alias);
                  m_adapter.appendTableHint(buf, join, q);

                  if (bJoin || nSel == SEL_EXIST)
                  {
                     append(join, m_nJoin != JOIN_ANSI, !bJoin);

                     if (q.getWhere() != null && (!q.isSubquery() || q == query) && join == q.getMapping())
                     {
                        append(q.getWhere(), !bJoin);
                     }
                  }
               }
            }
            else
            {
               SQLJoin join = (SQLJoin)q.getMapping();

               if (join.parent != null)
               {
                  appendJoinSeparator(buf, join, true);
               }

               buf.append('(');
               appendReadSQL(buf, q, SEL_SUB, null);
               buf.append(") ");
               buf.append(SUBQUERY_PREFIX);
               buf.append(join.alias);

               if (join.parent != null)
               {
                  append(join, true, false);
               }
            }

            return true;
         }
      };

      query.visit(visitor,
         (bGenWhere) ?
            ((query.isSubquery()) ? Query.VISIT_WHERE : Query.VISIT_WHERE | Query.VISIT_QUERY) :
               Query.VISIT_QUERY, Query.VISIT_QUERY);

      if (nSel == SEL_EXIST)
      {
         conj = visitor.getConjunction();
      }
      else
      {
         boolean bWhere = false;

         if (visitor.getConjunction() != null)
         {
            buf.append(" where ");
            appendJoinConjunction(buf, visitor.getConjunction());
            bWhere = true;
            conj = null;
         }

         if (bGenWhere && query.getWhere() != null)
         {
            buf.append((bWhere) ? " and " : " where ");
            appendWhereCondition(buf, query.getWhere(), GEN_WHERE, bWhere);
         }

         if (nSel == SEL_MAIN)
         {
            appendGroupBy(buf, query);

            if (query.getHaving() != null)
            {
               buf.append(" having ");
               appendWhereCondition(buf, query.getHaving(), GEN_HAVING, false);
            }

            appendOrderBy(buf, query);
            m_adapter.appendSuffixHint(buf, query);
         }
      }

      m_subquery = subquerySaved;

      return conj;
   }

   /**
    * Appends the query output fields to a select statement.
    * @param buf The buffer where to append the fields.
    */
   protected void appendOutputFields(StringBuffer buf)
   {
      boolean bAliased = m_adapter.isColumnAliased(m_query);

      if (m_nOutputFieldCount == 0)
      {
         buf.append((m_query.isAggregate()) ? "count(*)" : "1");
      }
      else
      {
         for (int i = 0; i < m_nOutputFieldCount; ++i)
         {
            if (i > 0)
            {
               buf.append(", ");
            }

            appendOutputField(buf, m_outputFieldArray[i]);

            if (bAliased)
            {
               buf.append(' ');
               buf.append(getAlias(i));
            }
         }
      }
   }

   /**
    * Appends the subquery output fields to a select statement.
    * @param buf The buffer where to append the fields.
    * @param query The subquery which fields to append.
    */
   protected void appendSubqueryOutputFields(StringBuffer buf, Query query)
   {
      SQLJoin join = (SQLJoin)query.getMapping();

      if (m_columnSet == null)
      {
         m_columnSet = new HashHolder(query.getFieldCount());
      }
      else
      {
         m_columnSet.clear();
      }

      // TODO: Figure out which fields to output in a non-output query

      Field[] pkFieldArray = (Field[])query.getItem();

      for (int i = 0, nCount = pkFieldArray.length; i < nCount; ++i)
      {
         Field field = pkFieldArray[i];
         SQLJoin mapping = (SQLJoin)field.getMapping();

         // Check whether the key is mapped to this
         // subquery or to the parent foreign key.
         // Do not add the key in the latter case.
         if (mapping.query != query)
         {
            break;
         }

         appendSubqueryOutputColumn(buf, mapping, (Column)field.getItem());
      }

      if (join.destinationKey != null)
      {
         appendSubqueryOutputKey(buf, join, join.destinationKey);
      }

      for (Iterator itr = query.getFieldIterator(); itr.hasNext();)
      {
         Field field = (Field)itr.next();
         Object mapping = field.getMapping();

         if (mapping instanceof SQLJoin)
         {
            appendSubqueryOutputColumn(buf, (SQLJoin)mapping, (Column)field.getItem());
         }
      }

      appendSubqueryOutputFields(buf, query, Query.ASSOC_QUERY);

      for (Iterator itr = query.getQuantorIterator(); itr.hasNext();)
      {
         appendSubqueryOutputFields(buf, query, itr.next());
      }
   }

   /**
    * Appends the subquery association output fields to a select statement.
    * @param buf The buffer where to append the fields.
    * @param query The subquery which fields to append.
    * @param key The association map key.
    */
   protected void appendSubqueryOutputFields(StringBuffer buf, Query query, Object key)
   {
      for (Iterator itr = query.getAssocIterator(key); itr.hasNext();)
      {
         Query assoc = (Query)itr.next();

         if (assoc.isSameRoot(query))
         {
            SQLJoin assocJoin = (SQLJoin)assoc.getMapping();

            appendSubqueryOutputKey(buf, assocJoin.parent, assocJoin.sourceKey);
         }
         else
         {
            Field[] pkFieldArray = (Field[])assoc.getParentItem();

            for (int i = 0, nCount = pkFieldArray.length; i < nCount; ++i)
            {
               Field field = pkFieldArray[i];
               SQLJoin mapping = (SQLJoin)field.getMapping();

               // Check whether the key is mapped to this
               // subquery or to the parent foreign key.
               // Do not add the key in the latter case.
               if (mapping.query != query)
               {
                  break;
               }

               appendSubqueryOutputColumn(buf, mapping, (Column)field.getItem());
            }
         }
      }
   }
   
   /**
    * Appends the columns of a given subquery output key to a string buffer.
    * @param buf The buffer where to append the key columns.
    * @param join The key join.
    * @param key The key to append.
    */
   protected void appendSubqueryOutputKey(StringBuffer buf, SQLJoin join, Index key)
   {
      int nCount = key.getIndexColumnCount();

      for (int i = 0; i < nCount; ++i)
      {
         appendSubqueryOutputColumn(buf, join, key.getIndexColumn(i).getColumn());
      }
   }

   /**
    * Appends a subquery output column to a string buffer.
    * @param buf The buffer where to append the column.
    * @param join The column join.
    * @param column The column to append.
    */
   protected void appendSubqueryOutputColumn(StringBuffer buf, SQLJoin join, Column column)
   {
      if (m_columnSet.add(column))
      {
         if (m_columnSet.size() > 1)
         {
            buf.append(", ");
         }

         buf.append(join.alias);
         buf.append('.');
         m_adapter.appendColumn(buf, column);
         buf.append(' ');
         buf.append(join.alias);
         buf.append('_');
         buf.append(getAlias(column.getOrdinal()));
      }
   }

   /**
    * Appends a join separator to a string buffer.
    * @param buf The string buffer where to append the separator.
    * @param join The join which separator to append.
    * @param bJoin True to append a join, false for a Cartesian product.
    */
   protected void appendJoinSeparator(StringBuffer buf, SQLJoin join, boolean bJoin)
   {
      if (m_nJoin == JOIN_ANSI && bJoin)
      {
         buf.append((join.isInner) ? " inner join " : " left join ");
      }
      else
      {
         buf.append(", ");
      }
   }

   /**
    * Appends a join conjunction to a string buffer.
    * @param buf The string buffer where to append the conjunction.
    * @param conj Null-terminated array of SQLJoin/Boolean and Operator.
    */
   protected void appendJoinConjunction(StringBuffer buf, Object[] conj)
   {
      for (int i = 0; i != conj.length; ++i)
      {
         Object obj = conj[i];

         if (obj == null)
         {
            break;
         }

         if (i != 0)
         {
            buf.append(" and ");
         }

         if (obj instanceof SQLJoin)
         {
            appendJoinCondition(buf, (SQLJoin)obj, ((Boolean)conj[++i]).booleanValue());
         }
         else
         {
            appendWhereCondition(buf, (Operator)obj, GEN_JOIN, true);
         }
      }
   }

   /**
    * Appends a join condition to a string buffer.
    * @param buf The buffer where to append the condition.
    * @param join The join to append.
    * @param bSubquery True if this is a subquery join.
    */
   protected void appendJoinCondition(StringBuffer buf, SQLJoin join, boolean bSubquery)
   {
      int nCount = join.destinationKey.getIndexColumnCount();

      for (int i = 0; i < nCount; ++i)
      {
         if (i > 0)
         {
            buf.append(" and ");
         }

         appendColumn(buf, join.parent, join.sourceKey.getIndexColumn(i).getColumn());
         buf.append(" = ");

         if (bSubquery)
         {
            appendColumn(buf, join, join.destinationKey.getIndexColumn(i).getColumn());
         }
         else
         {
            buf.append(join.alias);
            buf.append('.');
            m_adapter.appendColumn(buf, join.destinationKey.getIndexColumn(i).getColumn());
         }

         if (m_nJoin == JOIN_ORACLE && !join.isInner)
         {
            buf.append("(+)");
         }
      }
   }

   /**
    * Appends the where condition of a given query to a string buffer.
    * @param buf The string buffer where to append the condition.
    * @param where The condition to append.
    * @param nGenMode The generation mode, one of the GEN_* constants.
    * @param bConj True if the condition is part of a conjunction.
    */
   protected void appendWhereCondition(StringBuffer buf, Operator where, int nGenMode, boolean bConj)
   {
      if (where.isConstant())
      {
         buf.append((Boolean.FALSE.equals(where.getValue())) ? "0 = 1" :"1 = 1");
      }
      else
      {
         int nGenModeSaved = m_nGenMode;
         boolean bParen = (bConj && where.getOrdinal() == OrOperator.ORDINAL);

         if (bParen)
         {
            buf.append('(');
         }

         m_nGenMode = nGenMode;
         appendOperator(buf, where);
         m_nGenMode = nGenModeSaved;

         if (bParen)
         {
            buf.append(')');
         }
      }
   }

   /**
    * Appends the group by clause of a given query to a string buffer.
    * @param buf The string buffer where to append the clause.
    * @param query The query from which to take the clause.
    */
   protected void appendGroupBy(StringBuffer buf, Query query)
   {
      int nCount = query.getGroupByCount();

      if (nCount > 0)
      {
         int nGenModeSaved = m_nGenMode;
         int nOffset;

         m_nGenMode = GEN_GROUPBY;
         buf.append(" group by ");

         initOffsetArray(Math.max(nCount, query.getOrderByCount() + 4));

         for (int i = 0; i < nCount; ++i)
         {
            if (i > 0)
            {
               buf.append(", ");
            }

            nOffset = buf.length();

            Operator op = query.getGroupBy(i);
            String sSuffix = m_adapter.appendGroupPrefix(buf, op);

            appendOperator(buf, op);
            m_adapter.appendSuffix(buf, sSuffix);
            addSubstring(buf, nOffset, buf.length());
         }

         m_nGenMode = nGenModeSaved;
      }
   }

   /**
    * Appends the order by clause of a given query to a string buffer.
    * @param buf The string buffer where to append the clause.
    * @param query The query from which to take the clause.
    */
   protected void appendOrderBy(StringBuffer buf, Query query)
   {
      int nCount = query.getOrderByCount();

      if (nCount > 0)
      {
         int nGenModeSaved = m_nGenMode;

         m_nGenMode = GEN_ORDERBY;
         buf.append(" order by ");

         OrderByPrefix prefix = m_adapter.getOrderByPrefix(query);
         int nPrefixColumnCount = 0;
         Index index = null;
         Object mapping = null;
         boolean bSortDirectionReversed = false;

         if (prefix != null)
         {
            nPrefixColumnCount = prefix.getColumnCount();
            index = prefix.getIndex();
            mapping = prefix.getMapping();
            bSortDirectionReversed = prefix.isSortDirectionReversed();
            nCount += nPrefixColumnCount;
         }

         initOffsetArray(nCount);

         for (int i = 0; i < nCount; ++i)
         {
            if (i > 0)
            {
               buf.append(", ");
            }

            Operator op;
            boolean bAscending;

            if (i >= nPrefixColumnCount)
            {
               op = query.getOrderByOperator(i - nPrefixColumnCount);
               bAscending = query.isOrderByAscending(i - nPrefixColumnCount);
            }
            else
            {
               IndexColumn indexColumn = index.getIndexColumn(i);
               Column column = indexColumn.getColumn();
               Field field = new Field(query, mapping, column, column.getValueType(),
                  m_adapter.getConverter(column.getValueType(), column),
                  m_adapter.getBind(column), false);

               op = new AttributeOperator(field);
               bAscending = indexColumn.isAscending() ^ bSortDirectionReversed;
            }

            int nOffset = buf.length();
            String sSuffix = m_adapter.appendSortPrefix(buf, op);

            appendOperator(buf, op);
            m_adapter.appendSuffix(buf, sSuffix);

            if (addSubstring(buf, nOffset, buf.length()))
            {
               m_adapter.appendSortDirection(buf, op, bAscending);
            }
         }

         m_nGenMode = nGenModeSaved;
      }
   }

   /**
    * Appends an output field.
    * @param buf The buffer where to append the field.
    * @param field The field to append.
    */
   protected void appendOutputField(StringBuffer buf, Field field)
   {
      if (!m_query.isAggregate() ||
         field.getOperator() != null ||
         field.isGroupedBy() && !m_adapter.isCaseConverted(field))
      {
         appendField(buf, field);
      }
      else
      {
         String sSuffix = m_adapter.appendUngroupPrefix(buf, (Primitive)field.getType());

         appendField(buf, field);
         m_adapter.appendSuffix(buf, sSuffix);
      }
   }

   /**
    * Appends the mapped column of a given field to the SQL buffer.
    * @param buf The buffer where to append the column.
    * @param field The field specifying the mapped column.
    */
   protected void appendField(StringBuffer buf, Field field)
   {
      Operator op = field.getOperator();

      if (op != null)
      {
         appendOperator(buf, op);
      }
      else
      {
         SQLJoin join = (SQLJoin)field.getMapping();

         appendColumn(buf, join, (Column)field.getItem());

         if (m_nGenMode == GEN_JOIN && m_nJoin == JOIN_ORACLE && !join.isInner)
         {
            buf.append("(+)");
         }
      }
   }

   /**
    * Appends a mapped column to the SQL buffer.
    * @param buf The buffer where to append the column.
    * @param join The join providing the column.
    * @param column The column.
    */
   protected void appendColumn(StringBuffer buf, SQLJoin join, Column column)
   {
      Query query = join.query;

      if (!query.isSubquery() || query == m_subquery)
      {
         buf.append(join.alias);
         buf.append('.');
         m_adapter.appendColumn(buf, column);
      }
      else
      {
         buf.append(SUBQUERY_PREFIX);
         buf.append(((SQLJoin)query.getMapping()).alias);
         buf.append('.');
         buf.append(join.alias);
         buf.append('_');
         buf.append(getAlias(column.getOrdinal()));
      }
   }

   /**
    * Append Field/Operator operand to the String buffer.
    * @param buf The buffer to append the operand to.
    * @param op The operand to append (Filed or Source).
    */
   public void appendOperand(StringBuffer buf, Object op)
   {
      if (op instanceof Field)
      {
         appendField(buf, (Field)op);
      }
      else
      {
         appendOperator(buf, (Operator)op);
      }
   }

   /**
    * Appends an expression to the SQL clause.
    * @param buf The buffer where to append the operator.
    * @param op The operator to append.
    */
   protected void appendOperator(StringBuffer buf, Operator op)
   {
      if (op.isConstant())
      {
         Operator parent = op.getParent();
         Object value = op.getValue();
         Column column = null;
         Operator left = null;

         if (parent instanceof ComparisonOperator)
         {
            left = ((ComparisonOperator)op.getParent()).getLeft();

            if (parent.getOrdinal() == LikeOperator.ORDINAL && value != null)
            {
               value = m_adapter.getLikePattern((String)value);
            }
         }
         else if (parent instanceof InOperator)
         {
            left = ((InOperator)op.getParent()).getOperand(0);
         }

         if (left instanceof AttributeOperator)
         {
            AttributeOperator aop = (AttributeOperator)left;

            if (aop.getConverter() == null)
            {
               column = (Column)((Field)aop.getSource()).getItem();
            }
         }

         if (column != null)
         {
            if ((column.isLiteral() || m_adapter.isLiteral()) &&
               m_adapter.isLiteral(column.getType(), value) ||
               getBindCount() == m_adapter.getMaxBindCount())
            {
               m_adapter.appendLiteral(buf, column, value);
            }
            else
            {
               m_adapter.appendBind(buf, getBindCount(), column);
               addBind(m_adapter.getBind(column), m_adapter.toBind(column, value));
            }
         }
         else
         {
            if (value == null)
            {
               buf.append("null");
            }
            else
            {
               Primitive type = (Primitive)op.getType();

               if (m_adapter.isLiteral() && m_adapter.isLiteral(type, value) ||
                  getBindCount() == m_adapter.getMaxBindCount())
               {
                  m_adapter.appendLiteral(buf, type, type.convert(value));
               }
               else
               {
                  m_adapter.appendBind(buf, getBindCount(), type, value);
                  addBind(m_adapter.getBind(type), m_adapter.getInternal(type, value));
               }
            }
         }
      }
      else
      {
         OperatorAppender appender = m_adapter.findOperatorAppender(op);

         if (appender == null)
         {
            appender = s_appenderArray[op.getOrdinal()];
         }

         if (appender == null)
         {
            throw new PersistenceException("err.persistence.unsupportedOperator",
               new Object[]{op.getSymbol()});
         }

         Operator parent = op.getParent();
         String sSuffix = null;

         if (op.getType() == Primitive.BOOLEAN)
         {
            if (op instanceof IfOperator)
            {
               if (parent instanceof Logical ||
                  parent instanceof IfOperator && op == ((IfOperator)parent).getCondition() ||
                  parent == null && (m_nGenMode == GEN_JOIN || m_nGenMode == GEN_WHERE || m_nGenMode == GEN_HAVING))
               {
                  sSuffix = m_adapter.appendLogicalPrefix(buf, op);
               }
            }
            else if (!(op instanceof AttributeOperator))
            {
               if (op instanceof AggregateOperator)
               {
                  if (parent instanceof Logical)
                  {
                     sSuffix = m_adapter.appendLogicalPrefix(buf, op);
                  }
               }
               else if (parent == null &&
                  (m_nGenMode == GEN_OUTPUT || m_nGenMode == GEN_GROUPBY || m_nGenMode == GEN_ORDERBY) || 
                  parent instanceof IfOperator && op != ((IfOperator)parent).getCondition() ||
                  parent instanceof AggregateOperator && !(parent instanceof CountOperator))
               {
                  sSuffix = m_adapter.appendBooleanPrefix(buf, op);
               }
            }
         }

         if (sSuffix == null && parent != null)
         {
            int nPriority = op.getPriority();
            int nParentPriority = parent.getPriority();
            boolean bReprioritize = false;

            if (nPriority <= nParentPriority)
            {
               if (nPriority == nParentPriority)
               {
                  if (parent instanceof BinaryOperator)
                  {
                     bReprioritize = (((BinaryOperator)parent).getLeft() != op);
                  }
                  else if (!(parent instanceof FunctionOperator) && !(parent instanceof AggregateOperator))
                  {
                     bReprioritize = true;
                  }
               }
               else
               {
                  bReprioritize = !(parent instanceof Quantor) && !(parent instanceof FunctionOperator);
               }
            }

            if (bReprioritize)
            {
               buf.append('(');
               sSuffix = ")";
            }
         }

         appender.appendOperator(buf, op, this);
         m_adapter.appendSuffix(buf, sSuffix);
      }
   }

   /**
    * Appends an ungroup prefix for a given attribute operator, if necessary.
    * @param buf The buffer where to append the prefix.
    * @param aop The attribute operator.
    * @return The required suffix, or null if not needed.
    */
   protected String appendUngroupPrefix(StringBuffer buf, AttributeOperator aop)
   {
      if (m_query.isAggregate() && 
         (m_nGenMode == GEN_OUTPUT || m_nGenMode == GEN_HAVING || m_nGenMode == GEN_ORDERBY) &&
         !m_query.isGroupedByOperand(aop))
      {
         return m_adapter.appendUngroupPrefix(buf, (Primitive)aop.getType());
      }

      return null;
   }

   /**
    * Appends a comparison operand expression to the SQL clause.
    * @param buf The buffer where to append the operator.
    * @param op The operator to append.
    * @return True if a case-conversion has been added.
    */
   protected boolean appendComparisonOperand(StringBuffer buf, Operator op)
   {
      String sSuffix = m_adapter.appendComparisonPrefix(buf, op);

      appendOperator(buf, op);
      m_adapter.appendSuffix(buf, sSuffix);

      return sSuffix != null;
   }

   /**
    * Append a standalone boolean attribute by adding a comparison operator, if needed.
    * @param buf The string buffer where to append the operator.
    * @param op The operator to append.
    * @param parent The parent operator. May be different from op.getParent() due to not() optimization.
    * @param bTrue The constant to compare the attribute with.
    */
   protected boolean appendBooleanAttribute(StringBuffer buf, AttributeOperator op,
      Operator parent, boolean bTrue)
   {
      if (op.getType() == Primitive.BOOLEAN && 
         (parent == null && (m_nGenMode == GEN_JOIN || m_nGenMode == GEN_WHERE || m_nGenMode == GEN_HAVING) ||
          parent instanceof Logical || parent instanceof IfOperator && op == ((IfOperator)parent).getCondition()))
      {
         Field field = (Field)op.getSource();
         Column column = (Column)field.getItem();
         Boolean value = Boolean.valueOf(bTrue);
         boolean bParen = (parent != null && parent.getPriority() >= EqualsOperator.PRIORITY);

         if (bParen)
         {
            buf.append('(');
         }

         String sSuffix = appendUngroupPrefix(buf, op);

         appendField(buf, field);

         m_adapter.appendSuffix(buf, sSuffix);

         buf.append(" = ");

         if ((column.isLiteral() || m_adapter.isLiteral()) &&
            m_adapter.isLiteral(column.getType(), value) ||
            getBindCount() == m_adapter.getMaxBindCount())
         {
            Primitive type = column.getType();

            m_adapter.appendLiteral(buf, type, type.convert(value));
         }
         else
         {
            m_adapter.appendBind(buf, getBindCount());

            Converter converter = ((AttributeOperator)op).getConverter();

            if (converter == null)
            {
               addBind(m_adapter.getBind(Primitive.BOOLEAN),
                  m_adapter.getInternal(Primitive.BOOLEAN, value));
            }
            else
            {
               addBind(m_adapter.getBind(converter.getSourceType()),
                  converter.getInverseFunction().invoke(value));
            }
         }

         if (bParen)
         {
            buf.append(')');
         }

         return true;
      }

      return false;
   }

   /**
    * Appends a quantor source statement.
    * @param buf The string buffer where to append the statement.
    * @param quantor The quantor operator.
    * @param bWhere True to append a boolean expression to the where clause.
    */
   protected void appendQuantor(StringBuffer buf, Quantor quantor, boolean bWhere)
   {
      Object[] conj = null;
      boolean bFirst = true;

      buf.append(" from ");

      for (Iterator itr = quantor.getQuantorQuery().getAssocIterator(quantor); itr.hasNext();)
      {
         Query query = (Query)itr.next();

         if (bFirst)
         {
            bFirst = false;
         }
         else
         {
            buf.append(", ");
         }

         conj = appendReadSQL(buf, query, SEL_EXIST, conj);
      }

      if (conj == null)
      {
         bFirst = true;
      }
      else
      {
         buf.append(" where ");
         appendJoinConjunction(buf, conj);
         conj = null;
         bFirst = false;
      }

      if (bWhere && quantor.getOperand().getType() == Primitive.BOOLEAN)
      {
         buf.append((bFirst) ? " where " : " and ");
         appendWhereCondition(buf, quantor.getOperand(), GEN_WHERE, !bFirst);
      }
   }

   /**
    * Generates a new alias, unique within this object.
    */
   protected final String getNewAlias()
   {
      return getAlias(m_nTableCount++);
   }

   /**
    * Gets an SQL alias corresponding to an ordinal number.
    * @param nOrdinal The alias ordinal number.
    * @return The generated alias.
    */
   protected static String getAlias(int nOrdinal)
   {
      if (nOrdinal < s_aliasArray.length)
      {
         return s_aliasArray[nOrdinal];
      }

      StringBuffer buf = new StringBuffer("R");

      buf.append(nOrdinal - s_aliasArray.length);

      return buf.toString();
   }

   /**
    * @see PersistenceAdapter#isSupported(Operator, int)
    */
   public static boolean isSupported(Operator op)
   {
      return s_appenderArray[op.getOrdinal()] != null;
   }

   /**
    * Initializes the offset array for a given maximum offset count.
    * Must be called before addSubstring is used.
    * @param nCount The offset count.
    */
   protected void initOffsetArray(int nCount)
   {
      if (m_offsetArray == null || m_offsetArray.length < (nCount << 1))
      {
         m_offsetArray = new int[nCount << 1];
      }

      m_nOffsetCount = 0;
   }

   /**
    * Checks a buffer substring for uniqueness against the m_offsetArray and appends it to the array.
    * @param buf The buffer where the substrings are contained.
    * @param nStart Substring start offset.
    * @param nEnd Substring end offset.
    * @return True if the substring has been added, i.e. is unique.
    */
   protected boolean addSubstring(StringBuffer buf, int nStart, int nEnd)
   {
      int nLength = nEnd - nStart;

      for (int i = m_nOffsetCount; i > 0;)
      {
         int nCmpLength = m_offsetArray[--i];

         if (nLength == nCmpLength)
         {
            int nCmpOffset = m_offsetArray[--i];
            int nOffset = nStart;

            while (nCmpLength-- > 0)
            {
               if (buf.charAt(nOffset++) != buf.charAt(nCmpOffset++))
               {
                  break;
               }
            }

            if (nCmpLength < 0)
            {
               buf.setLength(nStart - 2);

               return false;
            }
         }
         else
         {
            --i;
         }
      }

      m_offsetArray[m_nOffsetCount++] = nStart;
      m_offsetArray[m_nOffsetCount++] = nLength;

      return true;
   }

   /**
    * Logs the SQL statement.
    */
   public void log()
   {
      if (m_adapter.isLogging())
      {
         m_adapter.log(m_sSQL);

         for (int i = 0; i < getBindCount(); ++i)
         {
            m_adapter.logBindValue(i, getBindValue(i));
         }
      }
   }

   // inner classes

   /**
    * Interface for appending operators.
    */
   protected interface OperatorAppender
   {
      /**
       * Appends an expression to the SQL clause.
       * @param buf The buffer where to append the operator.
       * @param op The operator to append.
       * @param gen The SQL generator.
       */
      void appendOperator(StringBuffer buf, Operator op, SQLGenerator gen);
   }

   /**
    * Unary operator appender.
    */
   protected final static class UnaryOperatorAppender implements OperatorAppender
   {
      /**
       * The string representation of the operator.
       */
      private String m_sOperator;

      /**
       * Creates the appender.
       * @param sOperator The string representation of the operator.
       */
      public UnaryOperatorAppender(String sOperator)
      {
         m_sOperator = sOperator;
      }

      /**
       * @see nexj.core.persistence.sql.SQLGenerator.OperatorAppender#appendOperator(java.lang.StringBuffer, nexj.core.persistence.Operator, nexj.core.persistence.sql.SQLGenerator)
       */
      public void appendOperator(StringBuffer buf, Operator op, SQLGenerator gen)
      {
         buf.append(m_sOperator);
         gen.appendOperator(buf, ((UnaryOperator)op).getOperand());
      }
   }

   /**
    * Binary operator appender.
    */
   protected final static class BinaryOperatorAppender implements OperatorAppender
   {
      /**
       * The string representation of the operator.
       */
      private String m_sOperator;

      /**
       * Creates the appender.
       * @param sOperator The string representation of the operator.
       */
      public BinaryOperatorAppender(String sOperator)
      {
         m_sOperator = sOperator;
      }

      /**
       * @see nexj.core.persistence.sql.SQLGenerator.OperatorAppender#appendOperator(java.lang.StringBuffer, nexj.core.persistence.Operator, nexj.core.persistence.sql.SQLGenerator)
       */
      public void appendOperator(StringBuffer buf, Operator op, SQLGenerator gen)
      {
         BinaryOperator binOp = (BinaryOperator)op;

         gen.appendOperator(buf, binOp.getLeft());
         buf.append(m_sOperator);
         gen.appendOperator(buf, binOp.getRight());
      }
   }

   protected final static class ComparisonOperatorAppender implements OperatorAppender
   {
      /**
       * The string representation of the operator.
       */
      private String m_sOperator;

      /**
       * Creates the appender.
       * @param sOperator The string representation of the operator.
       */
      public ComparisonOperatorAppender(String sOperator)
      {
         m_sOperator = sOperator;
      }

      /**
       * @see nexj.core.persistence.sql.SQLGenerator.OperatorAppender#appendOperator(java.lang.StringBuffer, nexj.core.persistence.Operator, nexj.core.persistence.sql.SQLGenerator)
       */
      public void appendOperator(StringBuffer buf, Operator op, SQLGenerator gen)
      {
         BinaryOperator binOp = (BinaryOperator)op;

         gen.appendComparisonOperand(buf, binOp.getLeft());
         buf.append(m_sOperator);
         gen.appendComparisonOperand(buf, binOp.getRight());
      }
   }

   /**
    * Multi-arg operator appender.
    */
   protected final static class MultiArgOperatorAppender implements OperatorAppender
   {
      /**
       * The string representation of the operator.
       */
      private String m_sOperator;

      /**
       * Creates the appender.
       * @param sOperator The string representation of the operator.
       */
      public MultiArgOperatorAppender(String sOperator)
      {
         m_sOperator = sOperator;
      }

      /**
       * @see nexj.core.persistence.sql.SQLGenerator.OperatorAppender#appendOperator(java.lang.StringBuffer, nexj.core.persistence.Operator, nexj.core.persistence.sql.SQLGenerator)
       */
      public void appendOperator(StringBuffer buf, Operator op, SQLGenerator gen)
      {
         MultiArgOperator multiOp = (MultiArgOperator)op;
         int nCount = multiOp.getOperandCount();

         for (int i = 0; i < nCount; ++i)
         {
            if (i > 0)
            {
               buf.append(m_sOperator);
            }

            gen.appendOperator(buf, multiOp.getOperand(i));
         }
      }
   }

   /**
    * Equality comparison operator appender (= or !=).
    */
   protected final static class EqualityComparisonOperatorAppender implements OperatorAppender
   {
      /**
       * The string representation of the operator.
       */
      private String m_sOperator;

      /**
       * The string representation of the null comparison (is null/is not null).
       */
      private String m_sNullComparison;

      /**
       * Creates the appender.
       * @param sOperator The string representation of the operator (= or !=)
       * @param sNullComparison The string representation of the null comparison (is null/is not null).
       */
      public EqualityComparisonOperatorAppender(String sOperator, String sNullComparison, String sCombinator)
      {
         m_sOperator = sOperator;
         m_sNullComparison = sNullComparison;
      }

      /**
       * @see nexj.core.persistence.sql.SQLGenerator.OperatorAppender#appendOperator(java.lang.StringBuffer, nexj.core.persistence.Operator, nexj.core.persistence.sql.SQLGenerator)
       */
      public void appendOperator(StringBuffer buf, Operator op, SQLGenerator gen)
      {
         ComparisonOperator compOp = (ComparisonOperator)op;
         Operator right = compOp.getRight();

         gen.appendComparisonOperand(buf, compOp.getLeft());

         if (right.isConstant() && right.getValue() == null)
         {
            buf.append(m_sNullComparison);
         }
         else
         {
            buf.append(m_sOperator);
            gen.appendComparisonOperand(buf, right);
         }
      }
   }

   /**
    * Query subtree visitor for generating joins.
    */
   protected abstract class JoinVisitor extends Query.SubtreeVisitor
   {
      /**
       * Conjunction array element count.
       */
      protected int m_nCount;

      /**
       * The conjunction array. Can be null to lazy-allocate.
       */
      protected Object[] m_conj;

      /**
       * The string buffer. If null, the joins are stored in the conjunction array.
       */
      protected StringBuffer m_buf;

      /**
       * Constructs the visitor.
       * @param buf The destination string buffer. Can be null to store
       * the joins in the conjunction array.
       * @param conj The conjunction array, can be null to allocate lazily.
       */
      public JoinVisitor(StringBuffer buf, Object[] conj)
      {
         m_buf = buf;
         m_conj = conj;

         if (conj != null)
         {
            int i;

            for (i = conj.length; i != 0; --i)
            {
               if (conj[i - 1] != null)
               {
                  break;
               }
            }

            m_nCount = i;
         }
      }

      /**
       * Appends an SQL join.
       * @param join The join to append.
       * @param bSubquery True if this is a subquery.
       * @param bDefer True to save the join in the conjunction array.
       */
      public void append(SQLJoin join, boolean bSubquery, boolean bDefer)
      {
         if (!bDefer && m_buf != null)
         {
            m_buf.append(" on ");
            appendJoinCondition(m_buf, join, bSubquery);
         }
         else
         {
            if (m_conj == null)
            {
               m_conj = new Object[4];
            }
            else if (m_nCount >= m_conj.length - 1)
            {
               grow();
            }

            m_conj[m_nCount++] = join;
            m_conj[m_nCount++] = Boolean.valueOf(bSubquery);
         }
      }

      /**
       * Appends an operator.
       * @param op The operator to append.
       * @param bDefer True to save the operator in the conjunction array.
       */
      public void append(Operator op, boolean bDefer)
      {
         if (!bDefer && m_buf != null)
         {
            m_buf.append(" and ");
            appendWhereCondition(m_buf, op, GEN_JOIN, true);
         }
         else
         {
            if (m_conj == null)
            {
               m_conj = new Object[4];
            }
            else if (m_nCount == m_conj.length)
            {
               grow();
            }

            m_conj[m_nCount++] = op;
         }
      }

      /**
       * Doubles the conjunction array size.
       */
      protected void grow()
      {
         Object[] conj = new Object[m_conj.length << 1];

         System.arraycopy(m_conj, 0, conj, 0, m_nCount);
         m_conj = conj;
      }

      /**
       * @return The conjunction array.
       */
      public Object[] getConjunction()
      {
         return m_conj;
      }
   }
}
