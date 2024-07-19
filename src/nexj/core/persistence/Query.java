// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.PrivilegeSet;
import nexj.core.meta.Type;
import nexj.core.meta.TypeMismatchException;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.ClassMapping;
import nexj.core.meta.persistence.DataSourceFragment;
import nexj.core.meta.persistence.Key;
import nexj.core.meta.persistence.PersistenceMapping;
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
import nexj.core.persistence.operator.IntrinsicFunctionOperator;
import nexj.core.persistence.operator.LessThanOperator;
import nexj.core.persistence.operator.LessThanOrEqualsOperator;
import nexj.core.persistence.operator.LikeOperator;
import nexj.core.persistence.operator.Logical;
import nexj.core.persistence.operator.LookupAggregateOperator;
import nexj.core.persistence.operator.MatchOperator;
import nexj.core.persistence.operator.MinusOperator;
import nexj.core.persistence.operator.MultiArgOperator;
import nexj.core.persistence.operator.MultiplicationOperator;
import nexj.core.persistence.operator.NegationOperator;
import nexj.core.persistence.operator.NotEqualsOperator;
import nexj.core.persistence.operator.NotOperator;
import nexj.core.persistence.operator.NumericAggregateOperator;
import nexj.core.persistence.operator.OrOperator;
import nexj.core.persistence.operator.PlusOperator;
import nexj.core.persistence.operator.Quantor;
import nexj.core.persistence.operator.SumOperator;
import nexj.core.persistence.operator.TypeConversionOperator;
import nexj.core.persistence.operator.UnaryOperator;
import nexj.core.persistence.operator.UniqueOperator;
import nexj.core.rpc.InstanceFactory;
import nexj.core.rpc.RPCUtil;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.DataVolumeException;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceArrayList;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.UnitOfWork;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.Function;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.IntrinsicFunction;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Macro;
import nexj.core.scripting.Pair;
import nexj.core.scripting.ScriptingException;
import nexj.core.scripting.Symbol;
import nexj.core.scripting.match.ExpressionParser;
import nexj.core.scripting.match.MatchNode;
import nexj.core.scripting.syntax.SyntaxFunction;
import nexj.core.util.ArrayIterator;
import nexj.core.util.EmptyIterator;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Named;
import nexj.core.util.ObjUtil;
import nexj.core.util.PrintWriter;
import nexj.core.util.Printable;
import nexj.core.util.PropertyMap;
import nexj.core.util.SysUtil;
import nexj.core.util.Undefined;

/**
 * Query node representing a set of class instances of the same base type.
 */
public class Query extends Source implements Comparator, Printable 
{
   // constants

   /**
    * Default query association key.
    */
   public final static Object ASSOC_QUERY = "QUERY";

   /**
    * Where clause association key.
    */
   public final static Object ASSOC_WHERE = "WHERE";

   /**
    * Visit the query associations.
    */
   public final static int VISIT_QUERY = 0x0001;

   /**
    * Visit the where associations.
    */
   public final static int VISIT_WHERE = 0x0002;

   /**
    * Visit the quantor query nodes.
    */
   public final static int VISIT_QUANTOR = 0x0004;

   /**
    * Visit all the associations.
    */
   public final static int VISIT_ALL = VISIT_QUERY | VISIT_WHERE | VISIT_QUANTOR;

   /**
    * Default security clause is applied.
    */
   public final static byte SEC_DEFAULT = -1;

   /**
    * No security clause applied.
    */
   public final static byte SEC_NONE = 0;

   /**
    * Security clause applied only to the current query node.
    */
   public final static byte SEC_NODE = 1;

   /**
    * Security clause applied to all the current query node and its sub-nodes.
    */
   public final static byte SEC_ALL = 2;

   /**
    * Unknown output mode.
    */
   public final static byte OUTPUT_UNKNOWN = -1;

   /**
    * The query node does not output any instances.
    */
   public final static byte OUTPUT_NONE = 0;

   /**
    * The query node outputs instances without querying for them
    * (e.g. only the OID is retrieved from another instance).
    */
   public final static byte OUTPUT_LAZY = 1;

   /**
    * The query node outputs instances with eager type code and locking loading.
    */
   public final static byte OUTPUT_EAGER = 2;

   /**
    * The query node is participating in a where clause.
    */
   public final static byte RESTRICTION_WHERE = 0x01;

   /**
    * The query node is participating in an order by clause.
    */
   public final static byte RESTRICTION_ORDERBY = 0x02;

   /**
    * The query node passes the restriction to the children.
    */
   public final static byte RESTRICTION_PARENT = 0x40;

   /**
    * The query node is tentatively required (used for required flag computation).
    */
   private final static byte RESTRICTION_REQUIRED = 0x04;

   /**
    * The query node is tentatively not required (used for required flag computation).
    */
   private final static byte RESTRICTION_NOT_REQUIRED = 0x08;

   /**
    * Unlimited timeout.
    */
   public final static int TIMEOUT_UNLIMITED = 0;

   /**
    * The timeout configured in the data source.
    */
   public final static int TIMEOUT_AUTO = -1;

   /**
    * Transfer object used to indicate a null cached instance.
    */
   protected final static TransferObject NULL_TO = new TransferObject(0);

   // attributes

   /**
    * The order by expression count.
    */
   private int m_nOrderByCount;

   /**
    * The group by expression count.
    */
   private int m_nGroupByCount;

   /**
    * The maximum instance count.
    */
   private int m_nMaxCount = -1;

   /**
    * The limit of the number of instances retrieved by a
    * single read() operation (negative for unlimited, 0 for default).
    */
   private int m_nLimit;

   /**
    * The query timeout in seconds (0 for unlimited, negative to use the default).
    */
   private int m_nTimeout = TIMEOUT_AUTO;

   /**
    * Number of retrieved instances to skip from the beginning.
    */
   private int m_nOffset;

   /**
    * Predecessor count for topological sorting.
    */
   private int m_nPredCount;

   /**
    * The security clause mode, one of the SEC_* constants.
    */
   private byte m_nSecurity;

   /**
    * The output mode, one of the OUTPUT_* constants.
    */
   private byte m_nOutput;

   /**
    * The restriction mode (combination of RESTRICTION_* flags).
    */
   private byte m_nRestriction;

   /**
    * 1 if the query selects a unique instance, 0 otherwise, -1 if unknown.
    */
   private byte m_nUnique = -1;

   /**
    * The caching flag (-1 means not set).
    */
   private byte m_nCached = -1;

   /**
    * True if an inverse attribute mapping is used.
    */
   private boolean m_bInverse;

   /**
    * True if the query node is required. This can be different from
    * the attribute required flag, as the persistence root is used.
    */
   private boolean m_bRequired;

   /**
    * True if the query where clause includes type code.
    */
   private boolean m_bTypeCodeFiltered;

   /**
    * True to lock the retrieved instances exclusively.
    */
   private boolean m_bLocking;

   /**
    * True if this is a subquery (for persistence mapping purposes).
    */
   private boolean m_bSubquery;

   /**
    * The flag indicating that the engine returns no more than the requested number of instances.
    */
   private boolean m_bLimited;

   /**
    * True if the query contains subcollections with the same root.
    */
   private boolean m_bPlural;

   /**
    * True if query has a match operator.
    */
   private boolean m_bMatch;

   /**
    * True if the query preserves the instance identity.
    */
   private boolean m_bIdentity = true;

   /**
    * The cursor retrieval flag.
    */
   private boolean m_bCursor;

   // associations

   /**
    * The query node metaclass.
    */
   private Metaclass m_metaclass;

   /**
    * The association where clause.
    * Null unless it has been specified on the association path: (@ attr1 filter ...)
    */
   private Object m_filter;

   /**
    * The persistence mapping. Cannot be null.
    */
   private PersistenceMapping m_persistenceMapping;

   /**
    * The data source fragment.
    */
   private DataSourceFragment m_fragment;

   /**
    * The field map: Field[Attribute|Operator|String].
    */
   private Lookup m_fieldMap = new HashTab();

   /**
    * The first output field.
    */
   private Field m_firstOutputField;

   /**
    * The last output field.
    */
   private Field m_lastOutputField;

   /**
    * The type code field. Can be null.
    */
   private Field m_typeCodeField;

   /**
    * The locking field. Can be null.
    */
   private Field m_lockingField;

   /**
    * The root node of the subtree.
    */
   private Query m_root;

   /**
    * The parent query.
    */
   private Query m_parent;

   /**
    * The output query collection.
    */
   private List m_outputQueryList; // of type Query

   /**
    * The quantor root node of the subtree.
    */
   private Query m_quantorRoot;

   /**
    * The associated query array.
    */
   private Query[] m_queryArray;

   /**
    * The where clause query array: Query[].
    */
   private Query[] m_whereArray;

   /**
    * Dependency array for topological sorting.
    */
   private Query[] m_depArray;

   /**
    * The quantor query map of arrays: Query[][Operator].
    */
   private Lookup m_quantorMap;

   /**
    * The where clause.
    */
   private Operator m_where;

   /**
    * The having clause.
    */
   private Operator m_having;

   /**
    * The group by expressions.
    */
   private Operator[] m_groupByArray;

   /**
    * The order by expressions: Operator[2*n], Boolean[2*n+1].
    */
   private Operator[] m_orderByArray;

   /**
    * The query constraint.
    */
   private Operator m_constraint;

   /**
    * The query instance selection OID.
    */
   private OID m_oid;

   /**
    * The field item.
    */
   private Object m_fieldItem;

   /**
    * The parent persistence mapping item.
    */
   private Object m_parentItem;

   /**
    * The child persistence mapping item.
    */
   private Object m_childItem;

   /**
    * The query generator.
    */
   private Object m_generator;

   /**
    * The persistence adapter.
    */
   private PersistenceAdapter m_adapter;

   /**
    * The invocation context.
    */
   private InvocationContext m_context;

   /**
    * List of all the root query nodes: Query[].
    */
   private List m_rootList;

   /**
    * The source key OID to parent instance set map: Instance[][OID].
    */
   private Lookup m_parentInstanceMap;

   /**
    * The currently evaluated instance.
    */
   private PropertyMap m_instance;

   /**
    * Cached visitor for determining if an operator is grouped by.
    */
   private Operator.Visitor m_groupedByVisitor;

   /**
    * Operator containing true.
    */
   private final static Operator TRUE_OPERATOR = new ConstantOperator(Boolean.TRUE);

   /**
    * Visitor for clearing the required restriction flags.
    */
   private final static Visitor REQUIRED_RESTRICTION_CLEANUP_VISITOR = new Visitor()
   {
      public boolean visit(Query query)
      {
         query.removeRestriction(RESTRICTION_REQUIRED | RESTRICTION_NOT_REQUIRED);

         return true;
      }
      
      public boolean postVisit(Query query)
      {
         return true;
      }
      
      public boolean isEligible(Query query)
      {
         return true;
      }
   };

   /**
    * Visitor for computing the required restrictions.
    */
   private final static Operator.Visitor REQUIRED_RESTRICTION_COMPUTING_VISITOR = new Operator.Visitor()
   {
      public boolean isEligible(Operator op)
      {
         return !(op instanceof IfOperator) && !(op instanceof Quantor);
      }

      public boolean visit(Operator op)
      {
         if (op.getOrdinal() == AttributeOperator.ORDINAL)
         {
            Source source = op.getSource();

            if (source != null)
            {
               Query query = source.getQuery();

               if (!query.isAssocRequired())
               {
                  Boolean required = getRequired(op.getParent());

                  if (required != null)
                  {
                     query.addRestriction((required.booleanValue()) ? RESTRICTION_REQUIRED : RESTRICTION_NOT_REQUIRED);
                  }
               }
            }
         }

         return true;
      }

      public Boolean getRequired(Operator parent)
      {
         if (parent == null)
         {
            return Boolean.TRUE;
         }

         Boolean required = null;

         if (parent instanceof NotEqualsOperator)
         {
            required = Boolean.TRUE;
         }
         else if (parent instanceof EqualsOperator)
         {
            Operator op = ((EqualsOperator)parent).getRight();

            if (op.isConstant())
            {
               required = Boolean.valueOf(op.getValue() != null);
            }
         }

         if (required != null)
         {
            parent = parent.getParent();
         }

         while (parent != null && !(parent instanceof Quantor))
         {
            if (parent instanceof InOperator)
            {
               if (required != null)
               {
                  return null;
               }

               required = Boolean.TRUE;
            }
            else if (parent instanceof ComparisonOperator)
            {
               if (required != null)
               {
                  return null;
               }

               Operator op = ((ComparisonOperator)parent).getRight();

               if (op.isConstant() && op.getValue() != null)
               {
                  required = Boolean.TRUE;
               }
            }
            else if (parent instanceof Logical)
            {
               if (parent instanceof AndOperator)
               {
                  if (required == null)
                  {
                     required = Boolean.TRUE;
                  }
               }
               else
               {
                  return null;
               }
            }

            parent = parent.getParent();
         }

         return required;
      }
   };

   /**
    * Visitor for normalizing the where clauses.
    */
   private final static Visitor NORMALIZATION_VISITOR = new Visitor()
   {
      public boolean visit(Query query)
      {
         query.normalizeWhere(false);
         query.normalizeRequired();

         return true;
      }

      public boolean postVisit(Query query)
      {
         return true;
      }

      public boolean isEligible(Query query)
      {
         return true;
      }
   };

   /**
    * Visitor for resolving caching and heterogeneous queries.
    */
   private final static Visitor PLANNING_VISITOR = new Visitor()
   {
      public boolean visit(Query query)
      {
         if (query.isRoot())
         {
            query.addRoot(query);
         }

         return true;
      }

      public boolean postVisit(Query query)
      {
         planCaching(query);

         // Heterogeneous joins must be output if the child node provides object identity
         query.setOutput(query.isJoin() && query.isLazy() &&
            !((ClassMapping)query.getAttributeMapping()).isInner());

         return true;
      }

      private void planCaching(Query query)
      {
         if (!query.isCachingSpecified() && !query.isLocking() && !query.getRoot().isAggregate() &&
            (query.getPersistenceMapping().getCaching() == PersistenceMapping.CACHING_CLASS ||
             query.getPersistenceMapping().getCaching() == PersistenceMapping.CACHING_INSTANCE &&
             (query.getAttribute() != null && !query.isCollection() || query.isUnique()) &&
             (query.getSecurity() == SEC_NONE || query.getMetaclass().getReadAccessAttribute() == null ||
              query.getOID() != null)) &&
            (query.getRestriction() & (RESTRICTION_WHERE | RESTRICTION_ORDERBY)) == 0 &&
            query.getQuantorCount() == 0)
         {
            for (Iterator itr = query.getAssocIterator(ASSOC_WHERE); itr.hasNext();)
            {
               Query assoc = (Query)itr.next();

               if (assoc.isInverse() ||
                  assoc.getAttribute().isLazy() ||
                  assoc.getAssocCount(ASSOC_QUERY) != 0 ||
                  assoc.getAssocCount(ASSOC_WHERE) != 0 ||
                  assoc.getQuantorCount() != 0 ||
                  assoc.getFieldCount() != 0)
               {
                  return;
               }
            }

            for (Iterator itr = query.getAssocIterator(ASSOC_QUERY); itr.hasNext();)
            {
               Query assoc = (Query)itr.next();

               if (!assoc.isRoot() && (!assoc.isLazy() || assoc.isInverse()) || assoc.getAttribute().isLazy())
               {
                  return;
               }
            }

            for (Field field = query.getFirstOutputField(); field != null; field = field.getNext())
            {
               if (field.getAnnotation() != null)
               {
                  return;
               }
            }

            query.setCached(true);

            if (!query.isRoot())
            {
               query.makeRoot(query.getRoot().getInvocationContext());
               query.addRoot(query);
            }

            Metaclass metaclass = query.getMetaclass();

            for (int i = 0, n = metaclass.getInstanceAttributeCount(); i < n; ++i)
            {
               Attribute attribute = metaclass.getInstanceAttribute(i);

               if (!attribute.isLazy())
               {
                  query.addAttribute(ASSOC_QUERY, attribute, null, false, OUTPUT_EAGER);
               }
            }
         }
      }

      public boolean isEligible(Query query)
      {
         return true;
      }
   };

   /**
    * Visitor for normalizing the fields.
    */
   private final static Visitor FIELD_VISITOR = new Visitor()
   {
      public boolean visit(Query query)
      {
         query.normalizeFields(false);
         query.sort();

         return true;
      }

      public boolean postVisit(Query query)
      {
         return true;
      }

      public boolean isEligible(Query query)
      {
         return true;
      }
   };

   /**
    * Visitor for computing the identity queries.
    */
   private final static Visitor IDENTITY_VISITOR = new Visitor()
   {
      public boolean visit(Query query)
      {
         if (!query.isGroupedBy())
         {
            Query parent = query.getParent();

            if (parent == null)
            {
               query.setIdentity(false);
            }
            else if (!parent.isIdentity() || query.isCollection())
            {
               if (query.isOutput())
               {
                  throw new InvalidQueryException("err.persistence.attributeGroupBy",
                     new Object[]{query.getAttribute().getName(), parent.getMetaclass().getName()});
               }

               query.setIdentity(false);
            }
         }

         return true;
      }

      public boolean postVisit(Query query)
      {
         if (!query.isIdentity())
         {
            for (Field field = query.getFirstOutputField(); field != null; field = field.getNext())
            {
               if (!query.isGroupedBy(field))
               {
                  throw new InvalidQueryException("err.persistence.attributeGroupBy",
                     new Object[]{
                        (field.getAttribute() != null) ? field.getAttribute().getName() :
                           (field.getAnnotation() != null) ? field.getAnnotation() : field.toString(),
                        query.getMetaclass().getName()});
               }
            }
         }

         return true;
      }

      public boolean isEligible(Query query)
      {
         return true;
      }
   };

   /**
    * Visitor for mapping the queries.
    */
   private final static Visitor MAPPING_VISITOR = new Visitor()
   {
      public boolean visit(Query query)
      {
         if (query.isRoot())
         {
            query.getAdapter().mapQuery(query);
         }

         return true;
      }

      public boolean postVisit(Query query)
      {
         query.normalizeFields(true);
         query.normalizeWhere(true);

         return true;
      }

      public boolean isEligible(Query query)
      {
         return true;
      }
   };

   /**
    * Visitor for making all fields in an operator output.
    */
   private final static Operator.Visitor OUTPUT_VISITOR = new Operator.Visitor()
   {
      public boolean visit(Operator op)
      {
         if (op instanceof AttributeOperator)
         {
            op.getSource().output(true);
         }

         return true;
      }

      public boolean isEligible(Operator op)
      {
         return !(op instanceof Quantor);
      }
   };

   /**
    * Comparator for sorting the field map keys.
    */
   private final static Comparator FIELD_KEY_COMPARATOR = new Comparator()
   {
      public int compare(Object left, Object right)
      {
         if (left instanceof String)
         {
            return -((Comparable)right).compareTo(left);
         }

         return ((Comparable)left).compareTo(right);
      }
   };

   /**
    * Map of operator symbol to operator factory: OperatorFactory[Symbol].
    */
   private final static Lookup s_operatorMap = new HashTab(35);

   static
   {
      s_operatorMap.put(OrOperator.SYMBOL, new MultiArgOperatorFactory()
      {
         public MultiArgOperator create(Query query)
         {
            return new OrOperator();
         }
      });

      s_operatorMap.put(AndOperator.SYMBOL, new MultiArgOperatorFactory()
      {
         public MultiArgOperator create(Query query)
         {
            return new AndOperator();
         }
      });

      s_operatorMap.put(EqualsOperator.SYMBOL, new ComparisonOperatorFactory()
      {
         public ComparisonOperator create(Query query)
         {
            return new EqualsOperator();
         }
      });

      s_operatorMap.put(NotEqualsOperator.SYMBOL, new ComparisonOperatorFactory()
      {
         public ComparisonOperator create(Query query)
         {
            return new NotEqualsOperator();
         }

         public MultiArgOperator createLogical(Query query)
         {
            return new OrOperator();
         }
      });

      s_operatorMap.put(GreaterThanOperator.SYMBOL, new ComparisonOperatorFactory()
      {
         public ComparisonOperator create(Query query)
         {
            return new GreaterThanOperator();
         }
      });

      s_operatorMap.put(GreaterThanOrEqualsOperator.SYMBOL, new ComparisonOperatorFactory()
      {
         public ComparisonOperator create(Query query)
         {
            return new GreaterThanOrEqualsOperator();
         }
      });

      s_operatorMap.put(LessThanOperator.SYMBOL, new ComparisonOperatorFactory()
      {
         public ComparisonOperator create(Query query)
         {
            return new LessThanOperator();
         }
      });

      s_operatorMap.put(LessThanOrEqualsOperator.SYMBOL, new ComparisonOperatorFactory()
      {
         public ComparisonOperator create(Query query)
         {
            return new LessThanOrEqualsOperator();
         }
      });

      s_operatorMap.put(LikeOperator.SYMBOL, new ComparisonOperatorFactory()
      {
         public ComparisonOperator create(Query query)
         {
            return new LikeOperator();
         }
      });

      s_operatorMap.put(MatchOperator.SYMBOL, new OperatorFactory()
      {
         public Operator create(Pair args, Query query, Object key, byte nOutput)
         {
            if (args == null || // ensure exactly 2 arguments
                !(args.getTail() instanceof Pair) ||
                args.getNext().getTail() != null)
            {
               throw new InvalidQueryException("err.persistence.matchArgCount",
                  new Object[]{Primitive.createInteger(Pair.length(args))});
            }

            Operator attributeOp = query.createOperator(key, args.getHead(), nOutput);

            if (!(attributeOp instanceof AttributeOperator))
            {
               if (attributeOp == null)
               {
                  return null;
               }

               throw new InvalidQueryException("err.persistence.invalidMatchArgument",
                                               new Object[] {attributeOp});
            }

            Operator expressionOp = query.createOperator(key, args.getNext().getHead(), nOutput); // unquote

            if (!(expressionOp instanceof ConstantOperator))
            {
               if (expressionOp == null)
               {
                  return null;
               }

               throw new InvalidQueryException("err.persistence.invalidMatchArgument",
                                               new Object[] {expressionOp});
            }

            Object expression = expressionOp.getValue(); // can only set after addOperator()

            if (expression instanceof String) // parse simplified infix notation
            {
               expression = new ExpressionParser().parse((String)expression);
            }

            MatchOperator matchOp = new MatchOperator(query.getInvocationContext());

            matchOp.setAttribute((AttributeOperator)attributeOp);
            matchOp.setExpression(MatchNode.parse(expression));
            query.m_bMatch = true;

            return matchOp;
         }
      });

      s_operatorMap.put(InOperator.SYMBOL, new OperatorFactory()
      {
         public Operator create(Pair args, Query query, Object key, byte nOutput)
         {
            if (args == null)
            {
               throw new InvalidQueryException("err.persistence.inOperatorArgCount");
            }

            MultiArgOperator op = new InOperator();

            for (; args != null; args = args.getNext())
            {
               Operator arg = query.createOperator(key, args.getHead(), nOutput);

               if (arg == null)
               {
                  return null;
               }

               if (arg.getOrdinal() == ConstantOperator.ORDINAL)
               {
                  Object value = arg.getValue();

                  if (value instanceof Pair)
                  {
                     for (Pair pair = (Pair)value; pair != null; pair = pair.getNext())
                     {
                        if (arg == null)
                        {
                           op.addOperand(new ConstantOperator(pair.getHead()));
                        }
                        else
                        {
                           arg.setValue(pair.getHead());
                           arg.setType(Primitive.typeOf(pair.getHead()));
                           op.addOperand(arg);
                           arg = null;
                        }
                     }
                  }
                  else if (value instanceof Collection)
                  {
                     for (Iterator itr = ((Collection)value).iterator(); itr.hasNext();)
                     {
                        if (arg == null)
                        {
                           op.addOperand(new ConstantOperator(itr.next()));
                        }
                        else
                        {
                           arg.setValue(itr.next());
                           arg.setType(Primitive.typeOf(arg.getValue()));
                           op.addOperand(arg);
                           arg = null;
                        }
                     }
                  }
                  else
                  {
                     op.addOperand(arg);
                  }
               }
               else
               {
                  op.addOperand(arg);
               }
            }

            return op;
         }
      });

      s_operatorMap.put(PlusOperator.SYMBOL, new BinaryOperatorFactory()
      {
         public BinaryOperator create(Query query)
         {
            return new PlusOperator();
         }
      });

      s_operatorMap.put(MinusOperator.SYMBOL, new BinaryOperatorFactory()
      {
         public BinaryOperator create(Query query)
         {
            return new MinusOperator();
         }

         public UnaryOperator create(Object arg, Query query, Object key, byte nOutput)
         {
            UnaryOperator op = new NegationOperator();

            op.setOperand(query.createOperator(key, arg, nOutput));

            return op;
         }
      });

      s_operatorMap.put(MultiplicationOperator.SYMBOL, new BinaryOperatorFactory()
      {
         public BinaryOperator create(Query query)
         {
            return new MultiplicationOperator();
         }
      });

      s_operatorMap.put(DivisionOperator.SYMBOL, new BinaryOperatorFactory()
      {
         public BinaryOperator create(Query query)
         {
            return new DivisionOperator();
         }
      });

      s_operatorMap.put(NotOperator.SYMBOL, new UnaryOperatorFactory()
      {
         public UnaryOperator create(Query query)
         {
            return new NotOperator();
         }
      });

      s_operatorMap.put(TypeConversionOperator.SYMBOL, new OperatorFactory()
      {
         public Operator create(Pair args, Query query, Object key, byte nOutput)
         {
            if (args == null || args.getTail() == null)
            {
               throw new InvalidQueryException("err.persistence.castOperatorArgCount");
            }

            Symbol symbol = (Symbol)args.getHead();

            args = args.getNext();

            if (args.getTail() != null)
            {
               throw new InvalidQueryException("err.persistence.castOperatorArgCount");
            }

            return new TypeConversionOperator(Primitive.parse(symbol.getName()),
               query.createOperator(key, args.getHead(), nOutput));
         }
      });

      s_operatorMap.put(AttributeOperator.SYMBOL, new OperatorFactory()
      {
         public Operator create(Pair args, Query query, Object key, byte nOutput)
         {
            if (args == null)
            {
               return new AttributeOperator(query);
            }

            Metaclass metaclass = query.getMetaclass();

            if (args.getHead() instanceof Pair)
            {
               Metaclass derived = metaclass.getCastMetaclass((Pair)args.getHead());

               if (derived == null)
               {
                  throw new InvalidQueryException("err.persistence.invalidTypeCastFilter");
               }

               if (!isCompatible(derived, metaclass))
               {
                  return new ConstantOperator(null);
               }

               metaclass = derived;
               args = args.getNext();
            }

            if (args == null)
            {
               return new AttributeOperator(query);
            }

            for (;;)
            {
               Pair next = args.getNext();
               Object filter = null;

               if (next != null)
               {
                  if (next.getHead() instanceof Pair)
                  {
                     filter = next.getHead();
                     next = next.getNext();
                  }
               }

               Metaclass derived = metaclass.getDerived(query.getFilter());

               if (!isCompatible(derived, metaclass))
               {
                  return new ConstantOperator(null);
               }

               String sName = ((Symbol)args.getHead()).getName();
               Attribute attribute;

               if (nOutput == OUTPUT_NONE)
               {
                  attribute = derived.getAttribute(sName);
               }
               else
               {
                  attribute = derived.findAttribute(sName);

                  if (attribute == null)
                  {
                     return null;
                  }

                  AttributeMapping mapping = attribute.findPersistenceMapping(query.getPersistenceMapping(), false);

                  if (mapping instanceof ClassMapping &&
                     (((ClassMapping)mapping).getPersistenceMapping().getCaching() != PersistenceMapping.CACHING_NONE ||
                        query.getPersistenceMapping().getCaching() != PersistenceMapping.CACHING_NONE))
                  {
                     return null;
                  }
               }

               if (next == null)
               {
                  if (attribute.getValue() != Undefined.VALUE && !attribute.isPersistent())
                  {
                     return query.createOperator(key, attribute.getDispatchedValue(), nOutput);
                  }

                  Source source = query.addAttribute(key, attribute, filter, false, nOutput); 

                  if (source == null)
                  {
                     return null;
                  }

                  return new AttributeOperator(source);
               }

               Source source = query.addAttribute(key, attribute, filter, false, nOutput);

               if (source instanceof Query)
               {
                  query = (Query)source;
                  metaclass = query.getMetaclass();
                  key = ASSOC_QUERY;
               }
               else if (source instanceof Field && ((Field)source).getOperator() == null)
               {
                  throw new InvalidQueryException("err.persistence.primitiveAssociation",
                     new Object[]{attribute.getName()});
               }
               else
               {
                  return null;
               }

               args = next;
            }
         }
      });

      s_operatorMap.put(Symbol.ATAT, new OperatorFactory()
      {
         public Operator create(Pair args, Query query, Object key, byte nOutput)
         {
            if (args == null)
            {
               throw new InvalidQueryException("err.persistence.missingReverseAssocClass");
            }

            if (nOutput != OUTPUT_NONE)
            {
               return null;
            }

            Metaclass metaclass = query.getMetaclass().getMetadata().getMetaclass(((Symbol)args.getHead()).getName());

            query = addAttribute(metaclass, null, args.getNext(), query, key, nOutput);

            if (query == null)
            {
               return new ConstantOperator(null);
            }

            return new AttributeOperator(query);
         }

         private Query addAttribute(Metaclass metaclass, Object where, Pair assoc, Query query, Object key, byte nOutput)
         {
            if (assoc == null)
            {
               if (!metaclass.isUpcast(query.getMetaclass()) && !query.getMetaclass().isUpcast(metaclass))
               {
                  throw new InvalidQueryException("err.persistence.reverseAssocClassMismatch",
                     new Object[]{metaclass.getName(), query.getMetaclass().getName()});
               }

               query.andWhere(where);

               return query;
            }

            Pair next = assoc.getNext();
            Object filter = null;

            if (next != null && next.getHead() instanceof Pair)
            {
               filter = next.getHead();
               next = next.getNext();
            }

            Attribute attribute = metaclass.getAttribute(((Symbol)assoc.getHead()).getName());

            if (attribute.getType().isPrimitive())
            {
               throw new InvalidQueryException("err.persistence.primitiveReverseAssoc",
                  new Object[]{attribute.getName(), metaclass.getName()});
            }

            Attribute reverse = attribute.getReverse();
            boolean bInverse = (reverse == null || !reverse.isPersistent() && attribute.isPersistent());
            Metaclass type = (Metaclass)attribute.getType();
            Metaclass derived = type.getDerived(filter);

            if (!isCompatible(derived, type))
            {
               return null;
            }

            query = addAttribute(derived, (bInverse) ? null : Pair.commutative(Symbol.AND, attribute.getWhere(), filter),
               next, query, key, nOutput);

            if (query != null)
            {
               query = (Query)query.addAttribute((next == null) ? key : ASSOC_QUERY,
                  (bInverse) ? attribute : reverse, where, bInverse, nOutput);

               if (bInverse && query != null)
               {
                  query.andWhere(filter);
               }
            }

            return query;
         }
      });

      s_operatorMap.put(IfOperator.SYMBOL, new OperatorFactory()
      {
         public Operator create(Pair args, Query query, Object key, byte nOutput)
         {
            if (args != null)
            {
               Object cond = args.getHead();
               args = args.getNext();

               if (args != null)
               {
                  Object tbranch = args.getHead();
                  args = args.getNext();

                  if (args == null || args.getTail() == null)
                  {
                     Object fbranch = (args == null) ? null : args.getHead();
                     Machine machine = query.getInvocationContext().getMachine();

                     if (machine.isEvalSupported(cond))
                     {
                        cond = machine.eval(cond);
                     }
                     else
                     {
                        Operator op = query.createOperator(key, cond, nOutput);

                        if (op == null)
                        {
                           return null;
                        }

                        op = op.normalize(0);

                        if (!op.isConstant())
                        {
                           IfOperator iop = new IfOperator();

                           iop.setCondition(op);

                           Metaclass metaclass = query.getMetaclass();

                           query.setMetaclass(metaclass.getDerived(cond));

                           try
                           {
                              if (!iop.setThen(query.createOperator(key, tbranch, nOutput)))
                              {
                                 return null;
                              }
                           }
                           finally
                           {
                              query.setMetaclass(metaclass);
                           }

                           if (!iop.setElse(query.createOperator(key, fbranch, nOutput)))
                           {
                              return null;
                           }

                           return iop;
                        }

                        cond = op.getValue();
                     }

                     return query.createOperator(key, (Intrinsic.isTrue(cond)) ? tbranch : fbranch, nOutput);
                  }
               }
            }

            throw new InvalidQueryException("err.persistence.ifOperatorArgCount");
         }
      });

      s_operatorMap.put(Symbol.NULL_P, new OperatorFactory()
      {
         public Operator create(Pair args, Query query, Object key, byte nOutput)
         {
            if (args == null || args.getTail() != null)
            {
               throw new InvalidQueryException("err.persistence.unaryOperatorArgCount",
                  new Object[]{Symbol.NULL_P});
            }

            BinaryOperator op = new EqualsOperator();

            if (!op.setLeft(query.createOperator(key, args.getHead(), nOutput)))
            {
               return null;
            }

            op.setRight(new ConstantOperator(null));

            return op;
         }
      });

      s_operatorMap.put(Symbol.INSTANCE_P, new OperatorFactory()
      {
         public Operator create(Pair args, Query query, Object key, byte nOutput)
         {
            if (args == null || args.getTail() == null)
            {
               throw new InvalidQueryException("err.persistence.instanceOfArgCount");
            }

            Operator op = query.createOperator(key, args.getHead(), nOutput);

            if (op == null)
            {
               return null;
            }

            args = args.getNext();

            if (args.getTail() != null)
            {
               throw new InvalidQueryException("err.persistence.instanceOfArgCount");
            }

            if (op.getOrdinal() != AttributeOperator.ORDINAL || !(args.getHead() instanceof Symbol))
            {
               throw new TypeMismatchException(Symbol.INSTANCE_P);
            }

            Metaclass metaclass = query.getMetaclass().getMetadata().getMetaclass(args.getHead().toString());
            Source source = ((AttributeOperator)op).getSource();

            if (!(source instanceof Query))
            {
               throw new TypeMismatchException(Symbol.INSTANCE_P);
            }

            Query target = (Query)source;

            if (metaclass.isUpcast(target.getMetaclass()))
            {
               return new ConstantOperator(Boolean.TRUE);
            }

            if (!target.getMetaclass().isUpcast(metaclass) || metaclass.getPersistenceMapping() == null)
            {
               return new ConstantOperator(Boolean.FALSE);
            }

            if (target.getPersistenceMapping().getTypeCodeAttribute() == null ||
               metaclass.getPersistenceMapping().getTypeCodeAttribute() == null)
            {
               throw new InvalidQueryException("err.persistence.instanceOfTypeCode");
            }

            return target.addTypeCodeComparison(metaclass.getPersistenceMapping(), null);
         }
      });

      s_operatorMap.put(Symbol.ANY, new OperatorFactory()
      {
         public Operator create(Pair args, Query query, Object key, byte nOutput)
         {
            if (args == null || args.getTail() != null)
            {
               throw new InvalidQueryException("err.persistence.anyArgCount");
            }

            AnyOperator any = new AnyOperator(query);
            Operator op = query.createOperator(any, args.getHead(),
               (nOutput == OUTPUT_UNKNOWN) ? OUTPUT_UNKNOWN : OUTPUT_NONE);

            if (op == null)
            {
               return null;
            }

            Query[] queryArray = query.findAssocs(any);

            any.setOperand(op);

            if (queryArray != null)
            {
               for (int i = 0; i < queryArray.length; ++i) 
               {
                  if (queryArray[i].isPlural())
                  {
                     return any;
                  }
               }
            }

            query.setAssocs(any, query.addAssocs(ASSOC_WHERE, queryArray, true, true));

            if (op.getType() != Primitive.BOOLEAN)
            {
               NotEqualsOperator ne = new NotEqualsOperator();

               ne.setLeft(op);
               ne.setRight(new ConstantOperator(null));

               return ne;
            }

            return op;
         }
      });

      s_operatorMap.put(Symbol.FOLD, new OperatorFactory()
      {
         public Operator create(Pair args, Query query, Object key, byte nOutput)
         {
            if (args != null)
            {
               Object aspect = args.getHead();

               if (!(aspect instanceof Symbol))
               {
                  throw new TypeMismatchException(Symbol.FOLD);
               }

               args = args.getNext();

               if (args != null && args.getTail() == null)
               {
                  if (!query.isRoot() &&
                     ((ClassMapping)query.getAttributeMapping()).isPure() &&
                     query.getParent().getMetaclass().hasAspect(
                        query.getInvocationContext().getMetadata().getClassAspect(aspect.toString())))
                  {
                     return new ConstantOperator(Boolean.TRUE);
                  }

                  return query.createOperator(key, args.getHead(), nOutput);
               }
            }

            throw new InvalidQueryException("err.persistence.foldOperatorArgCount");
         }
      });

      s_operatorMap.put(Symbol.VALUES, new OperatorFactory()
      {
         public Operator create(Pair args, Query query, Object key, byte nOutput)
         {
            if (key == ASSOC_QUERY && nOutput != OUTPUT_NONE || args == null || args.getTail() != null)
            {
               return null;
            }

            return query.createOperator(key, args.getHead(), nOutput);
         }
      });

      s_operatorMap.put(Symbol.define("query?"), new OperatorFactory()
      {
         public Operator create(Pair args, Query query, Object key, byte nOutput)
         {
            if (args != null)
            {
               throw new InvalidQueryException("err.persistence.operatorArgCount",
                  new Object[]{"query?", Primitive.ZERO_INTEGER});
            }

            return new ConstantOperator(Boolean.TRUE);
         }
      });

      s_operatorMap.put(Symbol.define("query-root?"), new OperatorFactory()
      {
         public Operator create(Pair args, Query query, Object key, byte nOutput)
         {
            if (args != null)
            {
               throw new InvalidQueryException("err.persistence.operatorArgCount",
                  new Object[]{"query-root?", Primitive.ZERO_INTEGER});
            }

            return new ConstantOperator(Boolean.valueOf(query.getParent() == null));
         }
      });

      s_operatorMap.put(Symbol.SUBSTRING, new IntrinsicFunctionOperatorFactory(Intrinsic.SUBSTRING,
         Primitive.STRING, new Primitive[]{Primitive.STRING, Primitive.INTEGER, Primitive.INTEGER}));

      s_operatorMap.put(Symbol.STRING_LENGTH, new IntrinsicFunctionOperatorFactory(Intrinsic.STRING_LENGTH,
         Primitive.INTEGER, new Primitive[]{Primitive.STRING}));

      s_operatorMap.put(Symbol.UNIQUE, new AggregateFunctionOperatorFactory()
      {
         public AggregateOperator create(Query query)
         {
            return new UniqueOperator(query);
         }
      });

      s_operatorMap.put(Symbol.COUNT, new AggregateFunctionOperatorFactory()
      {
         public AggregateOperator create(Query query)
         {
            return new CountOperator(query);
         }
      });

      s_operatorMap.put(Symbol.SUM, new AggregateFunctionOperatorFactory()
      {
         public AggregateOperator create(Query query)
         {
            return new SumOperator(query);
         }
      });

      s_operatorMap.put(Symbol.AVERAGE, new AggregateFunctionOperatorFactory()
      {
         public AggregateOperator create(Query query)
         {
            return new NumericAggregateOperator(Symbol.AVERAGE, query);
         }
      });

      s_operatorMap.put(Symbol.MINIMUM, new AggregateFunctionOperatorFactory()
      {
         public AggregateOperator create(Query query)
         {
            return new LookupAggregateOperator(Symbol.MINIMUM, query);
         }
      });

      s_operatorMap.put(Symbol.MAXIMUM, new AggregateFunctionOperatorFactory()
      {
         public AggregateOperator create(Query query)
         {
            return new LookupAggregateOperator(Symbol.MAXIMUM, query);
         }
      });
   }

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(Query.class);

   // constructors

   /**
    * Creates a root query.
    * @param metaclass The class associated with the query.
    * @param context The invocation context.
    */
   public Query(Metaclass metaclass, InvocationContext context)
   {
      assert metaclass != null;

      m_metaclass = metaclass;
      m_attribute = null;
      makeRoot(context);
      setPersistenceMapping(metaclass.getPersistenceMapping());

      if (m_persistenceMapping == null)
      {
         throw new InvalidQueryException("err.persistence.unmappedClass",
            new Object[]{metaclass.getName()});
      }

      makeRoot(context);
   }

   /**
    * Creates an associated query.
    * @param attribute The association attribute mapping.
    * @param filter The association filter.
    * @param bInverse True if the inverse attribute mapping is used.
    * @param bRequired True if the query node is required.
    */
   public Query(AttributeMapping mapping, Object filter, boolean bInverse, boolean bRequired)
   {
      m_bInverse = bInverse;
      m_bRequired = bRequired && filter == null;
      m_attribute = mapping.getAttribute();
      m_attributeMapping = mapping;
      m_filter = filter;

      assert !m_attribute.getType().isPrimitive();

      m_metaclass = (bInverse) ? m_attribute.getMetaclass() : (Metaclass)m_attribute.getType();
   }

   // operations

   /**
    * Sets the query node metaclass.
    * @param metaclass The query node metaclass to set.
    */
   public void setMetaclass(Metaclass metaclass)
   {
      m_metaclass = metaclass;
   }

   /**
    * @return The query node metaclass.
    */
   public Metaclass getMetaclass()
   {
      return m_metaclass;
   }

   /**
    * Sets the data source fragment.
    * @param sName The name of data source fragment to set.
    */
   public void setFragmentName(String sName)
   {
      if (m_persistenceMapping == null)
      {
         m_fragment = null;
      }
      else
      {
         if (sName == null)
         {
            sName = getInvocationContext().getUnitOfWork().getFragmentName(
               m_persistenceMapping.getDataSource().getFragmentCount() != 1);
         }

         m_fragment = m_persistenceMapping.getDataSource().getFragment(sName);
      }
   }

   /**
    * Sets the data source fragment.
    * @param fragment The data source fragment to set.
    */
   public void setFragment(DataSourceFragment fragment)
   {
      m_fragment = fragment;
   }

   /**
    * @return The data source fragment.
    */
   public DataSourceFragment getFragment()
   {
      return m_root.m_fragment;
   }

   /**
    * Sets the persistence mapping for this node.
    * @param mapping The mapping to set.
    */
   public void setPersistenceMapping(PersistenceMapping mapping)
   {
      m_persistenceMapping = mapping;

      if (m_persistenceMapping != null && m_persistenceMapping.isDynamic())
      {
         ((PersistenceResolver)m_persistenceMapping.getDataSource().getComponent()
            .getInstance(m_context)).resolve(this);
      }

      if (m_fragment == null)
      {
         setFragmentName((String)null);
      }
   }

   /**
    * @return The persistence mapping for this node.
    */
   public PersistenceMapping getPersistenceMapping()
   {
      return m_persistenceMapping;
   }

   /**
    * Get the parent (source) or child (destination) mapping key.
    * @see ClassMapping#getKey(boolean)
    */
   public Key getKey(boolean bChild)
   {
      assert m_parent != null;

      return ((ClassMapping)m_attributeMapping).getKey(bChild ^ m_bInverse);
   }

   /**
    * @see nexj.core.persistence.Source#getQuery()
    */
   public Query getQuery()
   {
      return this;
   }

   /**
    * @see nexj.core.persistence.Source#getType()
    */
   public Type getType()
   {
      return m_metaclass;
   }

   /**
    * @return The original query, of which this one is an alias.  
    */
   protected Query deref()
   {
      Query query = this;

      while (query.m_mapping instanceof Query)
      {
         query = (Query)query.m_mapping;
      }

      return query;
   }

   /**
    * Sets the field item.
    * @param fieldItem The field item to set.
    */
   public void setFieldItem(Object fieldItem)
   {
      deref().m_fieldItem = fieldItem;
   }

   /**
    * @return The field item.
    */
   public Object getFieldItem()
   {
      return deref().m_fieldItem;
   }

   /**
    * Sets the parent persistence mapping item.
    * 
    * In heterogeneous joins, used to determine the source key of the join. The
    * returned item can be used to retrieve the value of the key from the parent query.
    *
    * In homogeneous joins, used to determine the object key of the instance retrieved
    * by this query.
    * 
    * @param parentItem The parent persistence mapping item to set.
    */
   public void setParentItem(Object parentItem)
   {
      deref().m_parentItem = parentItem;
   }

   /**
    * Gets the parent persistence mapping item.
    * @return The parent persistence mapping item.
    * @see setParentItem(Object)
    */
   public Object getParentItem()
   {
      return deref().m_parentItem;
   }

   /**
    * Sets the child persistence mapping item.
    * 
    * In heterogeneous joins, used to determine the destination key of the join. The
    * returned item can be used to retrieve the value of the key from this query.
    *
    * The child persistence mapping item is not used in homogeneous joins.
    *
    * @param childItem The child persistence mapping item to set.
    */
   public void setChildItem(Object childItem)
   {
      deref().m_childItem = childItem;
   }

   /**
    * Gets the child persistence mapping item. Used only in heterogeneous joins.
    * @return The child persistence mapping item.
    * @see setChildItem(Object)
    */
   public Object getChildItem()
   {
      return deref().m_childItem;
   }

   /**
    * Sets the association attribute.
    * @param attribute The association attribute to set.
    */
   public void setAttribute(Attribute attribute)
   {
      super.setAttribute(attribute);

      if (attribute != null)
      {
         assert !attribute.getType().isPrimitive();

         m_metaclass = (m_bInverse) ? attribute.getMetaclass() : (Metaclass)attribute.getType();
      }
   }

   /**
    * The association filter. Can be null.
    */
   public Object getFilter()
   {
      return m_filter;
   }

   /**
    * @see Source#isInverse()
    */
   public boolean isInverse()
   {
      return m_bInverse;
   }

   /**
    * Sets the query node required flag.
    * @param bRequired The required flag to set.
    */
   public void setRequired(boolean bRequired)
   {
      m_bRequired = bRequired;
   }

   /**
    * @return True if the query node is required.
    */
   public boolean isRequired()
   {
      return m_bRequired;
   }

   /**
    * @return True if the query association path from the root to this node is required.
    */
   public boolean isAssocRequired()
   {
      for (Query query = this; query != null; query = query.getParent())
      {
         if (!query.isRequired())
         {
            return false;
         }
      }

      return true;
   }

   /**
    * Sets the security mode, one of the SEC_* constants.
    * @param nSecurity The security mode, one of the SEC_* constants to set.
    */
   public void setSecurity(byte nSecurity)
   {
      if (nSecurity < 0)
      {
         if (m_context.isSecure())
         {
            nSecurity = m_context.getQuerySecurity();

            if (nSecurity < 0)
            {
               nSecurity = SEC_NODE;
            }
         }
         else
         {
            nSecurity = SEC_NONE;
         }
      }

      m_nSecurity = nSecurity;
   }

   /**
    * @return The security mode, one of the SEC_* constants.
    */
   public byte getSecurity()
   {
      return m_nSecurity;
   }

   /**
    * @return True if the query instances are filtered with a security clause.
    */
   public boolean isSecure()
   {
      return m_nSecurity != SEC_NONE;
   }

   /**
    * @see nexj.core.persistence.Source#output(boolean)
    */
   public void output(boolean bParent)
   {
      setOutput(OUTPUT_LAZY, bParent);
   }

   /**
    * Sets the query eager output flag.
    * @param bOutput The query eager output flag to set.
    */
   public void setOutput(boolean bOutput)
   {
      if (bOutput)
      {
         setOutput(OUTPUT_EAGER);
      }
   }

   /**
    * Sets the output mode.
    * @param nOutput The output mode, one of the OUTPUT_* constants.
    */
   public void setOutput(byte nOutput)
   {
      setOutput(nOutput, true);
   }

   /**
    * Sets the output mode.
    * @param nOutput The output mode, one of the OUTPUT_* constants.
    * @param bParent True to set the parent output mode as well.
    */
   public void setOutput(byte nOutput, boolean bParent)
   {
      assert nOutput != OUTPUT_UNKNOWN;

      if (nOutput > m_nOutput)
      {
         if (bParent && m_parent != null)
         {
            m_parent.setOutput(OUTPUT_EAGER);
         }

         if (m_attribute == null || m_attribute.isCollection())
         {
            nOutput = OUTPUT_EAGER;
         }

         m_nOutput = nOutput;

         if (nOutput == OUTPUT_EAGER)
         {
            if (!isAggregate() || m_bGroupedBy)
            {
               Attribute attribute = m_persistenceMapping.getLockingAttribute();

               if (attribute != null)
               {
                  m_lockingField = (Field)addAttribute(ASSOC_QUERY, attribute, null, false, OUTPUT_EAGER);
               }

               if (m_persistenceMapping.isTypeCodeDispatched() ||
                  m_persistenceMapping.isTypeCodeFiltered() && !m_bTypeCodeFiltered)
               {
                  m_typeCodeField = (Field)addAttribute(ASSOC_QUERY, m_persistenceMapping.getTypeCodeAttribute(),
                     null, false, OUTPUT_EAGER);
               }
            }
         }
      }
   }

   /**
    * @return The output mode.
    */
   protected byte getOutput()
   {
      return m_nOutput;
   }  

   /**
    * @return The query output flag.
    */
   public final boolean isOutput()
   {
      return m_nOutput != OUTPUT_NONE;
   }

   /**
    * @see nexj.core.persistence.Source#getOperator()
    */
   public Operator getOperator()
   {
      return null;
   }

   /**
    * @return The lazy instance query flag.
    */
   public final boolean isLazy()
   {
      return m_nOutput == OUTPUT_LAZY;
   }

   /**
    * Sets the flag indicating that the engine returns no more than the requested number of instances.
    * @param bLimited The flag indicating that the engine returns no more than the requested number of instances to set.
    */
   public void setLimited(boolean bLimited)
   {
      m_bLimited = bLimited;
   }

   /**
    * @return The flag indicating that the engine returns no more than the requested number of instances.
    */
   public final boolean isLimited()
   {
      return m_bLimited;
   }

   /**
    * @return True if the query contains subcollections with the same root.
    */
   public final boolean isPlural()
   {
      return m_bPlural;
   }

   /**
    * @return True if the query has a match operator.
    */
   public final boolean isMatch()
   {
      return m_bMatch;
   }

   /**
    * Sets the identity preserving query flag.
    * @param bIdentity The identity preserving query flag to set.
    */
   protected void setIdentity(boolean bIdentity)
   {
      m_bIdentity = bIdentity;
   }

   /**
    * @return The identity preserving query flag.
    */
   public boolean isIdentity()
   {
      return m_bIdentity;
   }

   /**
    * Sets the aggregate query flag.
    * @param bAggregate The aggregate query flag to set.
    */
   public final void setAggregate(boolean bAggregate)
   {
      if (bAggregate)
      {
         if (m_groupByArray == null)
         {
            m_groupByArray = new Operator[8];
         }
      }
      else
      {
         m_groupByArray = null;
         m_nGroupByCount = 0;
         m_having = null;
      }
   }

   /**
    * @return The aggregate query flag.
    */
   public final boolean isAggregate()
   {
      return m_groupByArray != null;
   }

   /**
    * Adds an attribute to the query.
    * It must belong to the query class.
    * @param key The association map key.
    * @param attribute The attribute to add.
    * @param filter The association filter. Can be null.
    * @param bInverse True if the inverse attribute mapping is used.
    * @param nOutput The attribute output mode, one of the OUTPUT_* constants.
    * @return The attribute source, or null if the attribute is not mapped.
    */
   public Source addAttribute(Object key, Attribute attribute, Object filter, boolean bInverse, byte nOutput)
   {
      assert attribute != null;
      assert (bInverse) ?
         m_metaclass.isUpcast(attribute.getType()) ||
         attribute.getType().isUpcast(m_metaclass) :
         m_metaclass.isUpcast(attribute.getMetaclass()) ||
         attribute.getMetaclass().isUpcast(m_metaclass);
      assert filter == null || nOutput <= OUTPUT_NONE && !attribute.getType().isPrimitive();

      if (filter != null && attribute.getType().isPrimitive())
      {
         throw new InvalidQueryException("err.persistence.primitiveDynamicDerivedAssoc",
            new Object[]{attribute.getName()});
      }

      if (nOutput > OUTPUT_NONE)
      {
         Attribute accessAttribute = m_metaclass.getReadAccessAttribute();

         if (accessAttribute != null &&
            attribute.getOrdinal() == accessAttribute.getOrdinal() &&
            attribute.isStatic() == accessAttribute.isStatic() &&
            m_nSecurity != SEC_NONE)
         {
            return null;
         }
      }

      Attribute root = attribute.getPersistenceRoot();
      AttributeMapping mapping = root.findPersistenceMapping(m_persistenceMapping, bInverse);

      if (mapping == null)
      {
         Object value = attribute.getDispatchedValue();

         if (value != Undefined.VALUE && (nOutput == OUTPUT_NONE || attribute.isCached()))
         {
            Field field = (Field)m_fieldMap.get(attribute);

            if (field == null)
            {
               // TODO: Optimize by pre-computing operator calculability
               Operator op = createOperator(key, value, OUTPUT_UNKNOWN);

               if (op != null)
               {
                  op = op.normalize(0);

                  if (op.isConstant())
                  {
                     return null;
                  }

                  Source source = op.getSource();

                  if (op instanceof AttributeOperator)
                  {
                     if (nOutput > OUTPUT_NONE)
                     {
                        source.output(m_bIdentity);
                        addAnnotation(attribute, source);
                     }

                     return source;
                  }

                  if (Operator.findCommonSource(source, this) != null)
                  {
                     field = addOperator(op);

                     if (field.getAttribute() == null)
                     {
                        field.setAttribute(attribute);
                     }
                     else
                     {
                        if (nOutput > OUTPUT_NONE)
                        {
                           addOutputField(new Field(field, attribute));
                        }
                     }

                     m_fieldMap.put(attribute, field);
                  }
               }
            }

            if (field != null)
            {
               if (nOutput > OUTPUT_NONE)
               {
                  addOutputField(field);
                  addAnnotation(attribute, field);
               }

               return field;
            }
         }

         if (nOutput == OUTPUT_NONE)
         {
            throw new InvalidQueryException("err.persistence.nonPersistentAttribute",
               new Object[]{attribute.getName()});
         }

         if (nOutput > OUTPUT_NONE)
         {
            addDependency(key, attribute.getMetaclass(), attribute.getMetaclass(),
               attribute.getCumulativeDependency(), OUTPUT_LAZY);
         }

         return null;
      }

      if (attribute.getType().isPrimitive())
      {
         Field field = (Field)m_fieldMap.get(root);

         if (field == null)
         {
            field = new Field(this, root);
            field.setAttributeMapping(mapping);
            m_fieldMap.put(root, field);
         }

         if (nOutput > OUTPUT_NONE)
         {
            addOutputField(field);
            addAnnotation(attribute, field);
         }

         return field;
      }

      Query assoc;

      // Try to use the parent query with a reverse association

      if (m_attribute != null &&
         ((bInverse) ? m_bInverse && attribute.getOrdinal() == m_attribute.getOrdinal() :
         attribute.isReverseOf(m_attribute) && !attribute.isCollection()) &&
         (nOutput > OUTPUT_NONE || m_attribute.isRequired()))
      {
         assoc = m_parent;
         assoc.addRestriction(key, nOutput, 0, m_nRestriction);
      }
      else
      {
         Attribute req = (bInverse) ? attribute.getReverse() : attribute;
         boolean bRequired = (req != null && req.isRequired() &&
            req.getMetaclass().isUpcast(m_metaclass));

         assoc = findAssoc(key, root, filter, bInverse);

         if (assoc == null)
         {
            assoc = new Query(mapping, filter, bInverse, bRequired);

            if (m_nSecurity == SEC_ALL)
            {
               assoc.setSecurity(SEC_ALL);
            }

            assoc.addRestriction(key, nOutput, RESTRICTION_PARENT, m_nRestriction);
            addAssoc(key, assoc);

            if (bInverse)
            {
               andWhere(root.getWhere());
               assoc.setWhere(key, filter);
            }
            else
            {
               assoc.setWhere(key, Pair.commutative(Symbol.AND, root.getWhere(), filter));
            }
         }
         else
         {
            assoc.addRestriction(key, nOutput, RESTRICTION_PARENT, 0);

            if (!bRequired)
            {
               assoc.setRequired(false);
            }
         }
      }

      if (nOutput > OUTPUT_NONE)
      {
         assoc.setOutput(OUTPUT_LAZY);
         addAnnotation(attribute, assoc);
      }

      return assoc;
   }

   /**
    * Adds an attribute with a given name to the query.
    * @param key The association map key.
    * @param sName The attribute name.
    * @param bInverse True if the inverse attribute mapping is used.
    * @param nOutput The attribute output mode, one of the OUTPUT_* constants.
    * @return The attribute source, or null if the attribute is not mapped.
    */
   public Source addAttribute(Object key, String sName, boolean bInverse, byte nOutput)
   {
      assert sName != null;

      return addAttribute(key, m_metaclass.getAttribute(sName), null, bInverse, nOutput);
   }

   /**
    * Adds a dependency attribute list to this query: (a1 a2 (a3 a3.1 ... a3.N) ... aN).
    * @param key The association map key.
    * @param metaclass The class for the dependency attribute list.
    * @param base The base class for the dependency attribute list.
    * @param pair The dependency attribute symbol list.
    * @param nOutput Attribute output mode, one of the OUTPUT_* constants.
    */
   public void addDependency(Object key, Metaclass metaclass, Metaclass base, Pair pair, byte nOutput)
   {
      assert m_metaclass.isUpcast(metaclass);

      try
      {
         for (; pair != null; pair = pair.getNext())
         {
            assert pair.getHead() != null;

            if (pair.getHead() instanceof Pair)
            {
               Pair head = (Pair)pair.getHead();

               if (head == null || head.getHead() == null)
               {
                  throw new InvalidQueryException("err.persistence.queryDep");
               }

               Attribute attribute;
               Object operator = head.getHead();

               if (operator instanceof Symbol)
               {
                  if (Symbol.ATAT.equals(operator))
                  {
                     Pair next = head.getNext();

                     if (next == null || !(next.getHead() instanceof Symbol))
                     {
                        throw new InvalidQueryException("err.persistence.queryPoly");
                     }

                     Metaclass subclass = metaclass.getMetadata().getMetaclass(next.getHead().toString());

                     if (!metaclass.isUpcast(subclass))
                     {
                        if (!subclass.isUpcast(metaclass))
                        {
                           if (base != metaclass && base.isUpcast(subclass))
                           {
                              continue;
                           }

                           throw new InvalidQueryException("err.persistence.querySubclass",
                              new Object[]{subclass.getName(), metaclass.getName()});
                        }

                        subclass = metaclass;
                     }

                     addDependency(key, subclass, subclass, next.getNext(), nOutput);

                     continue;
                  }

                  if (Symbol.COLON.equals(operator))
                  {
                     Pair next = head.getNext();

                     if (next == null || !(next.getHead() instanceof Symbol))
                     {
                        throw new InvalidQueryException("err.persistence.queryAnnotation");
                     }

                     String sName = next.getHead().toString();

                     next = next.getNext();

                     if (next == null || next.getTail() != null)
                     {
                        throw new InvalidQueryException("err.persistence.queryAnnotation");
                     }

                     addAnnotation(sName, next.getHead());

                     continue;
                  }

                  attribute = metaclass.getAttribute(operator.toString());
               }
               else
               {
                  attribute = metaclass.getDerivedAttribute((Attribute)operator);
               }

               Source source = addAttribute(key, attribute, null, false, nOutput);

               if (source instanceof Query)
               {
                  Query query = (Query)source;
                  Metaclass type = (Metaclass)attribute.getType();

                  query.addDependency(key,
                     (query.getMetaclass().isUpcast(type)) ? type : query.getMetaclass(),
                     type, head.getNext(), nOutput);
               }
               else if (head.getTail() != null && attribute.getType().isPrimitive() &&
                  attribute.getType() != Primitive.ANY)
               {
                  throw new InvalidQueryException("err.persistence.queryDep");
               }
            }
            else
            {
               Attribute attribute;

               if (pair.getHead() instanceof Symbol)
               {
                  attribute = metaclass.getAttribute(pair.getHead().toString());
               }
               else
               {
                  attribute = metaclass.getDerivedAttribute((Attribute)pair.getHead());
               }

               addAttribute(key, attribute, null, false, nOutput);
            }
         }
      }
      catch (ClassCastException e)
      {
         throw new InvalidQueryException("err.persistence.queryDep", e);
      }
      catch (MetadataException e)
      {
         throw new InvalidQueryException("err.persistence.queryDep", e);
      }
   }

   /**
    * Adds an annotation to the query.
    * @param sName The annotation name.
    * @param value The annotation S-expression.
    * @return The annotation field.
    */
   public Field addAnnotation(String sName, Object value)
   {
      assert sName != null;

      Field field = (Field)m_fieldMap.get(sName);

      if (field != null)
      {
         throw new InvalidQueryException("err.persistence.queryAnnotationDup", new Object[]{sName});
      }

      Operator op = createOperator(ASSOC_QUERY, value, OUTPUT_UNKNOWN);

      if (op != null)
      {
         op = op.normalize(0);

         if (op.isConstant())
         {
            field = new Field(this, sName, op.getValue());
         }
         else
         {
            Source source = op.getSource();

            if (op instanceof AttributeOperator)
            {
               Query query = source.getQuery();

               if (source instanceof Field && query == this)
               {
                  field = (Field)source;
               }
               else if (!m_bIdentity)
               {
                  field = new Field(source);
               }

               if (field != null)
               {
                  source.output(false);
                  addAnnotation(sName, field);
               }
            }
            else if (Operator.findCommonSource(source, this) != null)
            {
               field = addOperator(op);
               addAnnotation(sName, field);
            }
         }
      }

      if (field == null)
      {
         if (op != null)
         {
            op.visit(OUTPUT_VISITOR, Operator.VISIT_PREORDER);
         }
         else
         {
            addDependency(ASSOC_QUERY, m_metaclass, m_metaclass,
               m_metaclass.dependency(value, false, getInvocationContext().getMachine()), OUTPUT_LAZY);
         }

         field = new Field(this, sName,
            new Compiler().compile(Pair.list(Symbol.LAMBDA, Pair.list(Symbol.THIS), value),
               null, m_context.getMachine(), false));
      }

      m_fieldMap.put(sName, field);
      addOutputField(field);

      return field;
   }

   /**
    * Adds a given source as an annotation.
    * @param sName The annotation name.
    * @param source The source.
    * @return The field corresponding to the annotation.
    */
   protected Field addAnnotation(String sName, Source source)
   {
      if (source instanceof Field)
      {
         Field field = (Field)source;

         if (field.getAnnotation() == null || field.getAnnotation().equals(sName))
         {
            field.setAnnotation(sName);
   
            return field;
         }
      }

      Field field = new Field(source, sName);

      if (m_fieldMap.put(sName, field) != null)
      {
         throw new InvalidQueryException("err.persistence.queryAnnotationDup", new Object[]{sName});
      }

      addOutputField(field);

      return field;
   }

   /**
    * Adds a given source as an annotation.
    * @param attribute The attribute.
    * @param source The source.
    * @return The field corresponding to the annotation.
    */
   protected Field addAnnotation(Attribute attribute, Source source)
   {
      return (!m_bIdentity) ? addAnnotation(attribute.getName(), source) : null;
   }

   /**
    * Adds an S-expression to this query: (op1 (op2 arg1 arg2 ... argN) ...)
    * @param key The association map key.
    * @param obj The object to add as an operator.
    * @param nOutput The attribute output mode, one of the OUTPUT_* constants.
    * @return The created operator node, or null if the operator is not supported.
    * The latter is possible only if nOutput != OUTPUT_NONE. 
    */
   public Operator createOperator(Object key, Object obj, byte nOutput)
   {
      try
      {
         while (obj instanceof Pair)
         {
            Pair pair = (Pair)obj;

            obj = pair.getHead();

            if (obj instanceof Pair)
            {
               Pair head = (Pair)obj;

               if (Symbol.GLOBAL.equals(head.getHead()))
               {
                  head = head.getNext();

                  if (head.getTail() == null && head.getHead() instanceof Symbol)
                  {
                     obj = head.getHead();
                  }
               }
            }

            if (obj instanceof Symbol)
            {
               Object factory = s_operatorMap.get(obj);

               if (factory != null)
               {
                  Operator op = ((OperatorFactory)factory).create(pair.getNext(), this, key, nOutput);

                  if (nOutput == OUTPUT_NONE && op == null)
                  {
                     throw new InvalidQueryException("err.persistence.unsupportedOperator",
                        new Object[]{obj.toString()});
                  }

                  return op;
               }
            }

            Machine machine = getInvocationContext().getMachine();

            if (obj instanceof Symbol)
            {
               Object value = machine.getGlobalEnvironment().findVariable((Symbol)obj);

               if (value instanceof Macro)
               {
                  obj = machine.invoke((Function)value, pair.getNext());

                  continue;
               }

               if (value instanceof SyntaxFunction)
               {
                  obj = machine.getTransformerContext().expandTransformer((SyntaxFunction)value, pair);

                  continue;
               }
            }

            if (nOutput == OUTPUT_NONE || machine.isEvalSupported(pair))
            {
               return new ConstantOperator(machine.eval(obj, pair.getNext()));
            }

            return null;
         }

         if (obj instanceof Symbol)
         {
            if (nOutput != OUTPUT_NONE)
            {
               return null;
            }

            Attribute attribute = m_metaclass.getAttribute(obj.toString());

            if (attribute.getValue() != Undefined.VALUE && !attribute.isPersistent())
            {
               return createOperator(key, attribute.getDispatchedValue(), nOutput);
            }

            return new AttributeOperator(addAttribute(key, attribute, null, false, nOutput));
         }

         return new ConstantOperator(obj);
      }
      catch (ClassCastException e)
      {
         throw new InvalidQueryException("err.persistence.queryOperator", e);
      }
      catch (MetadataException e)
      {
         throw new InvalidQueryException("err.persistence.queryOperator", e);
      }
      catch (ScriptingException e)
      {
         if (nOutput == OUTPUT_NONE)
         {
            throw new InvalidQueryException("err.persistence.unsupportedExpression", e);
         }

         return null;
      }
   }

   /**
    * Adds to the query an operator evaluated by the persistence engine.
    * @param op The operator to add.
    * @return The operator field.
    */
   public Field addOperator(Operator op)
   {
      assert op != null;

      Field field = (Field)m_fieldMap.get(op);

      if (field == null)
      {
         field = new Field(this, op);
         m_fieldMap.put(op, field);
      }

      return field;
   }

   /**
    * Adds a field to the output field list.
    * @param field The field to add.
    */
   protected void addOutputField(Field field)
   {
      addOutputField(field, m_bIdentity);
   }

   /**
    * Adds a field to the output field list.
    * @param field The field to add.
    * @param bQuery True to output the query as well.
    */
   protected void addOutputField(Field field, boolean bQuery)
   {
      if (!field.isOutput())
      {
         if (m_firstOutputField == null)
         {
            m_firstOutputField = m_lastOutputField = field;
         }
         else
         {
            m_lastOutputField.setNext(field);
            m_lastOutputField = field;
         }

         field.setOutput(true);
      }

      if (bQuery && field.getAttribute() != null)
      {
         setOutput(OUTPUT_EAGER);
      }
   }

   /**
    * @return The field count.
    */
   public int getFieldCount()
   {
      return m_fieldMap.size();
   }

   /**
    * @return The field iterator.
    */
   public Lookup.Iterator getFieldIterator()
   {
      return m_fieldMap.valueIterator();
   }

   /**
    * @return The first output field.
    */
   public Field getFirstOutputField()
   {
      return m_firstOutputField;
   }

   /**
    * @return The type code field. Can be null.
    */
   public Field getTypeCodeField()
   {
      return m_typeCodeField;
   }

   /**
    * @return True if the where clause includes a type code filter.
    */
   public boolean isTypeCodeFiltered()
   {
      return m_bTypeCodeFiltered;
   }

   /**
    * @return The locking field. Can be null.
    */
   public Field getLockingField()
   {
      return m_lockingField;
   }

   /**
    * Makes the query node a root node.
    * @param context The invocation context.
    */
   public void makeRoot(InvocationContext context)
   {
      assert context != null;

      setRoot(this);
      m_context = context;

      if (m_persistenceMapping != null)
      {
         m_adapter = (PersistenceAdapter)m_persistenceMapping.getDataSource()
            .getComponent().getInstance(m_context);
      }
   }

   /**
    * Sets the root node of the subtree, including on associated query nodes with the same root.
    * @param root The root node of the subtree to set.
    */
   public void setRoot(final Query root)
   {
      assert root != null;

      if (root != m_root)
      {
         if (m_queryArray != null || m_whereArray != null || m_quantorMap != null)
         {
            visit(new Visitor()
            {
               public boolean visit(Query query)
               {
                  query.m_root = root;

                  return true;
               }

               public boolean postVisit(Query query)
               {
                  return true;
               }

               public boolean isEligible(Query query)
               {
                  return !query.isRoot();
               }
            }, VISIT_ALL);
         }
         else
         {
            m_root = root;
         }
      }
   }

   /**
    * @return The root node of the subtree.
    */
   public Query getRoot()
   {
      return m_root;
   }

   /**
    * @return True if this is the root node of the subtree.
    */
   public boolean isRoot()
   {
      return m_root == this;
   }

   /**
    * @return True if this is the root node of a heterogeneous query.
    */
   public boolean isJoin()
   {
      return m_root == this && m_parent != null;
   }

   /**
    * Determines if a query belong to the same subtree as this one.
    * @param query The query to check.
    * @return True if it belongs to the same subtree.
    */
   public boolean isSameRoot(Query query)
   {
      assert query != null;
      assert query.getRoot() != null;
      assert getRoot() != null;

      return query.getRoot() == getRoot();
   }

   /**
    * @return The parent query, if this query has a non-lazy mapping; otherwise this query.
    */
   private Query getInner()
   {
      if (isJoin() && !m_bInverse && !m_attribute.isLazy())
      {
         return m_parent;
      }

      return this;
   }

   /**
    * @see nexj.core.persistence.Source#findCommon(nexj.core.persistence.Source)
    */
   public Source findCommon(Source source)
   {
      Query query = source.getQuery();

      if (isSameRoot(query))
      {
         return m_root;
      }

      Query inner = getInner();

      if (inner.isSameRoot(query))
      {
         return inner.getRoot();
      }

      if (source == query)
      {
         Query other = query.getInner();

         if (isSameRoot(other))
         {
            return m_root;
         }

         if (inner.isSameRoot(other))
         {
            return inner.getRoot();
         }
      }

      return null;
   }

   /**
    * Adds a query node to the query root list.
    * @param query The query to add.
    */
   public void addRoot(Query query)
   {
      if (m_rootList == null)
      {
         m_rootList = m_parent.m_root.m_rootList;
      }

      m_rootList.add(query);
   }

   /**
    * @return True if this query uses heterogeneous joins.
    */
   public boolean isHeterogeneous()
   {
      return m_rootList != null && m_rootList.size() > 1;
   }

   /**
    * Sets the parent query.
    * @param parent The parent query to set.
    */
   public void setParent(Query parent)
   {
      m_parent = parent;

      if (parent != null)
      {
         if (m_root != this)
         {
            m_root = parent.getRoot();

            if (m_nTimeout < 0)
            {
               m_nTimeout = m_root.getTimeout();
            }
         }

         if (m_quantorRoot != this)
         {
            m_quantorRoot = parent.getQuantorRoot();
         }
      }
   }

   /**
    * @return The parent query.
    */
   public Query getParent()
   {
      return m_parent;
   }

   /**
    * Adds a new output query to the root.
    * @param outputQuery The output query to add.
    */
   public void addOutputQuery(Query outputQuery)
   {
      if (m_outputQueryList == null)
      {
         m_outputQueryList = new ArrayList();
      }

      outputQuery.setOrdinal(m_outputQueryList.size());
      m_outputQueryList.add(outputQuery);
   }

   /**
    * Gets a output query by ordinal number.
    * @param nOrdinal The output query ordinal number (0-based).
    * @return The output query object.
    */
   public Query getOutputQuery(int nOrdinal)
   {
      return (Query)m_outputQueryList.get(nOrdinal);
   }

   /**
    * @return The output query count.
    */
   public int getOutputQueryCount()
   {
      if (m_outputQueryList == null)
      {
         return 0;
      }

      return m_outputQueryList.size();
   }

   /**
    * @return An iterator for the contained output query objects.
    */
   public Iterator getOutputQueryIterator()
   {
      if (m_outputQueryList == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_outputQueryList.iterator();
   }

   /**
    * Sets the quantor root node.
    */
   public void setQuantorRoot(Query query)
   {
      m_quantorRoot = query;
   }

   /**
    * @return The quantor root node (can be null).
    */
   public Query getQuantorRoot()
   {
      return m_quantorRoot;
   }

   /**
    * @return True if the query is a quantor root.
    */
   public boolean isQuantorRoot()
   {
      return this == m_quantorRoot;
   }

   /**
    * Returns the associations for a given key.
    * @param key The association key.
    * @return The corresponding association array, or null if not initialized yet.
    */
   protected Query[] findAssocs(Object key)
   {
      if (key == ASSOC_QUERY)
      {
         return m_queryArray;
      }

      if (key == ASSOC_WHERE)
      {
         return m_whereArray;
      }

      if (m_quantorMap != null)
      {
         return (Query[])m_quantorMap.get(key);
      }

      return null;
   }

   /**
    * Sets the associations for a given key.
    * @param key The association key.
    * @param queryArray The association array to set.
    */
   protected void setAssocs(Object key, Query[] queryArray)
   {
      if (key == ASSOC_QUERY)
      {
         m_queryArray = queryArray;
      }
      else if (key == ASSOC_WHERE)
      {
         m_whereArray = queryArray;
      }
      else if (queryArray != null)
      {
         if (m_quantorMap == null)
         {
            m_quantorMap = new HashTab(4);
         }

         m_quantorMap.put(key, queryArray);
      }
      else if (m_quantorMap != null)
      {
         m_quantorMap.remove(key);

         if (m_quantorMap.size() == 0)
         {
            m_quantorMap = null;
         }
      }
   }

   /**
    * Adds associations to the existing associations for a given key.
    * @param key The association key.
    * @param queryArray The association array to add. Can be null.
    * @param bCollection True to add the collections.
    * @param bMerge True to merge, false to alias the queries.
    * @return The new query array.
    */
   protected Query[] addAssocs(Object key, Query[] queryArray, boolean bCollection, boolean bMerge)
   {
      if (queryArray == null)
      {
         return null;
      }

      Query[] assocArray = findAssocs(key);

      if (assocArray == null)
      {
         setAssocs(key, queryArray);

         for (int i = 0; i < queryArray.length; ++i)
         {
            Query query = queryArray[i];

            if (!(key instanceof Quantor))
            {
               query.setQuantorRoot(null);
            }

            query.setParent(this);
         }

         queryArray = null;
      }
      else
      {
         int nMovedCount = 0;
         int nCount = queryArray.length;

         for (int i = 0; i < nCount; ++i)
         {
            Query query = queryArray[i];
            Query assoc = find(assocArray, query.getAttribute(), query.getFilter(), query.isInverse());

            if (assoc == null)
            {
               query.setParent(this);
               assocArray = add(assocArray, query);
               queryArray[i] = null;
               ++nMovedCount;
            }
            else if (assoc.isSameRoot(query) && (bCollection || !query.isCollection()))
            {
               assoc.addQuery(query, bMerge);
            }
         }

         if (nMovedCount != 0)
         {
            setAssocs(key, assocArray);
            nCount -= nMovedCount;

            if (nCount == 0)
            {
               queryArray = null;
            }
            else
            {
               assocArray = new Query[nCount];

               for (int i = 0, k = 0; k < nCount; ++i)
               {
                  Query query = queryArray[i];

                  if (query != null)
                  {
                     assocArray[k++] = query;
                  }
               }

               queryArray = assocArray;
            }
         }
      }

      return queryArray;
   }

   /**
    * Adds the fields and associations from a given query.
    * @param query The source query; modified by the method.
    * @param bMerge True to merge, false to alias.
    */
   protected void addQuery(Query query, boolean bMerge)
   {
      assert m_attribute == query.getAttribute() &&
         m_bInverse == query.isInverse() &&
         query.getFirstOutputField() == null;

      if (bMerge)
      {
         addRestriction(query.getRestriction() & ~RESTRICTION_PARENT);

         for (Iterator itr = query.getFieldIterator(); itr.hasNext();)
         {
            Field srcField = (Field)itr.next();
            Attribute attribute = srcField.getAttribute();
            Field dstField = (Field)m_fieldMap.get(attribute);

            if (dstField == null)
            {
               srcField.setQuery(this);
               m_fieldMap.put(attribute, srcField);
            }
            else
            {
               srcField.setSource(dstField);
            }
         }
      }

      query.m_queryArray = addAssocs(ASSOC_QUERY, query.m_queryArray, true, bMerge);
      query.m_whereArray = addAssocs(ASSOC_WHERE, query.m_whereArray, true, bMerge);

      if (query.m_quantorMap != null)
      {
         for (Lookup.Iterator itr = query.m_quantorMap.iterator(); itr.hasNext();)
         {
            itr.next();

            Query[] queryArray = addAssocs(itr.getKey(), (Query[])itr.getValue(), true, bMerge);

            if (queryArray == null)
            {
               itr.remove();
            }
            else
            {
               itr.setValue(queryArray);
            }
         }
      }

      query.setSource(this);

      if (bMerge)
      {
         if (m_constraint == null)
         {
            m_constraint = query.getConstraint();
         }
      }

      normalizeWhere(false);
   }

   /**
    * Computes the association map filter key.
    * Filters are where clauses used in dynamic derived associations.
    * @param filter The association filter. Can be null.
    * @param bInverse True for inverse attribute mapping.
    * @return The filter key.
    */
   protected static Object getFilterKey(Object filter, boolean bInverse)
   {
      if (filter == null)
      {
         return Boolean.valueOf(bInverse);
      }

      if (!bInverse && !(filter instanceof Boolean))
      {
         return filter;
      }

      return new Pair(filter, Boolean.valueOf(bInverse));
   }

   /**
    * Determines if a derived class belongs to the same inheritance branch as a base class.
    * @param derived The derived class object.
    * @param base The base class object.
    * @return True if derived belongs to the same inheritance branch as base. 
    */
   protected static boolean isCompatible(Metaclass derived, Metaclass base)
   {
      return base.isUpcast(derived) && derived.getPersistenceRoot() == base.getPersistenceRoot();
   }

   /**
    * Adds an associated query.
    * @param key The association key.
    * @param assoc The associated query to add.
    */
   public void addAssoc(Object key, Query assoc)
   {
      assert assoc != null;

      Attribute attribute = assoc.getAttribute();

      assert (assoc.isInverse()) ?
         m_metaclass.isUpcast(attribute.getType()) || attribute.getType().isUpcast(m_metaclass) :
         m_metaclass.isUpcast(attribute.getMetaclass()) || attribute.getMetaclass().isUpcast(m_metaclass);

      setAssocs(key, add(findAssocs(key), assoc));
      assoc.setParent(this);

      if (key instanceof Quantor)
      {
         assoc.setQuantorRoot(assoc);
      }

      AttributeMapping mapping = assoc.getAttributeMapping();

      if (mapping == null)
      {
         mapping = attribute.findPersistenceMapping(m_persistenceMapping, assoc.isInverse());
         assoc.setAttributeMapping(mapping);
      }

      if (mapping != null)
      {
         assoc.setPersistenceMapping((assoc.isInverse()) ?
            assoc.getMetaclass().getPersistenceMapping() :
            ((ClassMapping)mapping).getMapping());
      }

      if (assoc.getPersistenceMapping() == null)
      {
         if (key == ASSOC_WHERE)
         {
            throw new InvalidQueryException("err.persistence.calculatedWhereAssoc",
               new Object[]{assoc.getAttribute().getName(), assoc.getAttribute().getMetaclass().getName()});
         }

         assoc.makeRoot(getInvocationContext());
      }
      else if (assoc.getPersistenceMapping().getDataSource() != m_persistenceMapping.getDataSource())
      {
         assoc.makeRoot(getInvocationContext());
      }
      else if (assoc.isCollection())
      {
         Query root = assoc.getQuantorRoot();

         if (root == null)
         {
            root = m_root;
         }

         root.m_bPlural = true;
      }
   }

   /**
    * Adds a query to an array.
    * @param queryArray The original array.
    * @param query The query to add.
    * @return The new array.
    */
   protected static Query[] add(Query[] queryArray, Query query)
   {
      if (queryArray == null)
      {
         return new Query[]{query};
      }

      int nCount = queryArray.length;
      Query[] newArray = new Query[nCount + 1];

      System.arraycopy(queryArray, 0, newArray, 0, nCount); 
      newArray[nCount] = query;

      return newArray;
   }

   /**
    * Finds a query in an array.
    * @param queryArray The array to search. Can be null.
    * @param attribute The query attribute.
    * @param filter The query filter.
    * @param bInverse True to use inverse attribute mapping.
    * @return The found query, or null if not found. 
    */
   protected static Query find(Query[] queryArray, Attribute attribute, Object filter, boolean bInverse)
   {
      if (queryArray != null)
      {
         for (int i = 0; i < queryArray.length; ++i)
         {
            Query query = queryArray[i];

            if (query.getAttribute() == attribute &&
               query.isInverse() == bInverse &&
               ObjUtil.equal(query.getFilter(), filter))
            {
               return query;
            }
         }
      }

      return null;
   }

   /**
    * Finds an associated child query by association attribute.
    * @param key The association map key.
    * @param attribute The association attribute.
    * @param filter The association filter. Can be null.
    * @param bInverse True to use the inverse attribute mapping.
    * @return The found query, or null if not found.
    */
   public Query findAssoc(Object key, Attribute attribute, Object filter, boolean bInverse)
   {
      assert attribute != null;

      return find(findAssocs(key), attribute, filter, bInverse);
   }

   /**
    * Returns the associated query iterator.
    * @param key The association map key.
    * @return The associated query iterator.
    */
   public Iterator getAssocIterator(Object key)
   {
      Query[] queryArray = findAssocs(key);

      if (queryArray == null)
      {
         return EmptyIterator.getInstance();
      }

      return new ArrayIterator(queryArray);
   }

   /**
    * Returns the associated query count.
    * @param key The association map key.
    * @return The associated query count.
    */
   public int getAssocCount(Object key)
   {
      Query[] queryArray = findAssocs(key);

      if (queryArray == null)
      {
         return 0;
      }

      return queryArray.length;
   }

   /**
    * @return The quantor node count.
    */
   public int getQuantorCount()
   {
      if (m_quantorMap == null)
      {
         return 0;
      }

      return m_quantorMap.size();
   }

   /**
    * @return The quantor operator iterator.
    */
   public Iterator getQuantorIterator()
   {
      if (m_quantorMap == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_quantorMap.iterator();
   }

   /**
    * Adds a conjunction to the where clause.
    * @param where The conjunction where clause.
    */
   public void andWhere(Object where)
   {
      if (where != null)
      {
         andWhere(createOperator(ASSOC_WHERE, where, OUTPUT_NONE));
      }
   }

   /**
    * Adds a conjunction to the where clause.
    * @param op The conjunction where clause operator.
    */
   public void andWhere(Operator op)
   {
      if (op != null && (!op.isConstant() || Boolean.FALSE.equals(op.getValue())))
      {
         // addOperator() might modify m_where, hence it is invoked beforehand
         m_where = AndOperator.conjunction(m_where, op);

         if (m_nUnique == 0)
         {
            m_nUnique = -1;
         }
      }
   }

   /**
    * Collects the equality constraints in a logical expression.
    * @param op The logical expression.
    * @param sourceCollection The collection of sources. Can be null.
    * @return True if unique instance selection has been defined.
    */
   protected boolean addConstraints(Operator op, Collection sourceCollection)
   {
      Source source = null;
      Object value = null;

      switch (op.getOrdinal())
      {
         case AndOperator.ORDINAL:
            AndOperator and = (AndOperator)op;

            for (int i = 0, nCount = and.getOperandCount(); i < nCount; ++i)
            {
               if (addConstraints(and.getOperand(i), sourceCollection))
               {
                  return true;
               }
            }

            break;

         case EqualsOperator.ORDINAL:
            EqualsOperator eq = (EqualsOperator)op;

            if (eq.getLeft().getOrdinal() == AttributeOperator.ORDINAL)
            {
               if (eq.getRight().isConstant())
               {
                  source = ((AttributeOperator)eq.getLeft()).getSource();
                  value = eq.getRight().getValue();
               }
            }
            else if (eq.getRight().getOrdinal() == AttributeOperator.ORDINAL)
            {
               if (eq.getLeft().isConstant())
               {
                  source = ((AttributeOperator)eq.getRight()).getSource();
                  value = eq.getLeft().getValue();
               }
            }

            break;
      }

      if (source != null && source.getQuery() == this)
      {
         if (sourceCollection != null)
         {
            sourceCollection.add(source);
         }

         if (source == this)
         {
            if (value instanceof OIDHolder)
            {
               m_oid = ((OIDHolder)value).getOID();
            }

            return true;
         }
      }

      // TODO: Handle associated queries

      return false;
   }

   /**
    * @return True if the query retrieves a unique instance.
    */
   public boolean isUnique()
   {
      if (m_nUnique < 0)
      {
         // Stop recursion
         m_nUnique = 0;
         m_oid = null;

         if (m_where != null)
         {
            List sourceList = new ArrayList(4);

            if (addConstraints(m_where, sourceList) ||
               !sourceList.isEmpty() && getAdapter().isUnique(this, sourceList))
            {
               m_nUnique = 1;
            }
         }
      }

      return m_nUnique != 0;
   }

   /**
    * @return The query instance selection OID.
    */
   public OID getOID()
   {
      return m_oid;
   }

   /**
    * @return True if the query node retrieves a collection.
    */
   private boolean isCollection()
   {
      if (m_attribute != null)
      {
         if (m_bInverse)
         {
            if (!m_attribute.isCollection())
            {
               Attribute reverse = m_attribute.getReverse();

               if (reverse != null)
               {
                  if (reverse.isCollection())
                  {
                      return true;
                  }
               }
               else if (((ClassMapping)m_attributeMapping).isInner())
               {
                  return true;
               }
            }
         }
         else if (m_attribute.isCollection())
         {
             return true;
         }
      }

      return false;
   }

   /**
    * Adds a type code comparison operator for a given persistence mapping.
    * @param mapping The persistence mapping.
    * @param privilegeSet The privilege set, or null to add all the relevant type codes.
    */
   private Operator addTypeCodeComparison(PersistenceMapping mapping, PrivilegeSet privilegeSet)
   {
      Source typeCodeField = m_typeCodeField;

      if (typeCodeField == null)
      {
         typeCodeField = addAttribute(ASSOC_WHERE, m_persistenceMapping.getTypeCodeAttribute(), null, false, OUTPUT_NONE);
      }

      Iterator itr = mapping.getTypeCodeIterator(privilegeSet);

      if (itr.hasNext())
      {
         Object value = itr.next();

         if (itr.hasNext())
         {
            InOperator in = new InOperator();

            in.addOperand(new AttributeOperator(typeCodeField));
            in.addOperand(new ConstantOperator(value));

            do
            {
               in.addOperand(new ConstantOperator(itr.next()));
            }
            while (itr.hasNext());

            return in;
         }
         else
         {
            EqualsOperator eq = new EqualsOperator();

            eq.setLeft(new AttributeOperator(typeCodeField));
            eq.setRight(new ConstantOperator(value));

            return eq;
         }
      }
      else
      {
         return new ConstantOperator(Boolean.FALSE);
      }
   }

   /**
    * Sets the query where clause as an S-expression.
    * @param where The S-expression to set.
    */
   public void setWhere(Object where)
   {
      setWhere(ASSOC_QUERY, where);
   }

   /**
    * Sets the where clause as an S-expression.
    * @param key The association map key.
    * @param where The S-expression to set.
    */
   public void setWhere(Object key, Object where)
   {
      m_where = null;
      andWhere(where);

      if (m_attribute == null || !m_attribute.isRequired() ||
         !((ClassMapping)m_attributeMapping).isUnique())
      {
         andWhere(m_metaclass.getWhere());
      }

      if (m_nSecurity != SEC_NONE && key == ASSOC_QUERY)
      {
         Attribute attribute = m_metaclass.getReadAccessAttribute();

         if (attribute != null)
         {
            andWhere(attribute.getSymbol());
         }
      }

      if (m_persistenceMapping.isTypeCodeFiltered() &&
         (m_persistenceMapping.isTypeCodeForced() ||
            m_attribute == null ||
            m_attributeMapping.isMultiplexed()) &&
          !isUnique() ||
          m_nSecurity != SEC_NONE &&
          m_persistenceMapping.isTypeCodePrivileged(
             getInvocationContext().getPrivilegeSet()))
      {
         Operator op = addTypeCodeComparison(m_persistenceMapping,
            (m_nSecurity != SEC_NONE) ? getInvocationContext().getPrivilegeSet() : null);

         // addOperator() might modify m_where, hence it is invoked beforehand
         m_where = AndOperator.conjunction(m_where, op);
         m_bTypeCodeFiltered = true;

         if (m_nUnique == 0)
         {
            m_nUnique = -1;
         }
      }
   }

   /**
    * Sets the where clause.
    * @param where The where clause to set.
    */
   public void setWhere(Operator where)
   {
      m_where = where;
   }

   /**
    * @return The where clause.
    */
   public Operator getWhere()
   {
      return m_where;
   }

   /**
    * Sets the having clause as an S-expression.
    * @param where The S-expression to set.
    */
   public void setHaving(Object having)
   {
      setHaving((having == null) ? null : createOperator(ASSOC_QUERY, having, OUTPUT_NONE));
   }

   /**
    * Sets the having clause.
    * @param having The having clause to set.
    */
   public void setHaving(Operator having)
   {
      m_having = (having != null && (!having.isConstant() || Boolean.FALSE.equals(having.getValue()))) ? having : null;
      setAggregate(true);
   }

   /**
    * @return The having clause.
    */
   public Operator getHaving()
   {
      return m_having;
   }

   /**
    * Adds a group by clause to this query: (expr1 ... exprN).
    * @param groupBy The list of group by S-expressions.
    */
   public void addGroupBy(Pair groupBy)
   {
      try
      {
         for (; groupBy != null; groupBy = groupBy.getNext())
         {
            addGroupBy(createOperator(ASSOC_QUERY, groupBy.getHead(), OUTPUT_NONE));
         }

         setIdentity(isGroupedBy());
      }
      catch (ClassCastException e)
      {
         throw new InvalidQueryException("err.persistence.queryGroupBy", e);
      }
      catch (MetadataException e)
      {
         throw new InvalidQueryException("err.persistence.queryGroupBy", e);
      }
   }

   /**
    * Adds a group by expression.
    * @param nOrdinal The ordinal number of the expression.
    * @param operator The expression by which to group by.
    * @return The ordinal number of the added expression.
    */
   public int addGroupBy(int nOrdinal, Operator operator)
   {
      assert nOrdinal >= 0 && nOrdinal <= m_nGroupByCount;
      assert operator != null;

      if (m_groupByArray == null)
      {
         m_groupByArray = new Operator[8];
      }
      else
      {
         Operator[] groupByArray;

         if (m_nGroupByCount == m_groupByArray.length)
         {
            groupByArray = new Operator[m_nGroupByCount << 1];
            System.arraycopy(m_groupByArray, 0, groupByArray, 0, nOrdinal);
            System.arraycopy(m_groupByArray, nOrdinal, groupByArray, nOrdinal + 1, m_nGroupByCount - nOrdinal);
            m_orderByArray = groupByArray;
         }
         else
         {
            System.arraycopy(m_groupByArray, nOrdinal, m_groupByArray, nOrdinal + 1, m_nGroupByCount - nOrdinal);
         }
      }

      m_groupByArray[nOrdinal] = operator;
      m_nGroupByCount++;

      if (operator instanceof AttributeOperator)
      {
         operator.getSource().setGroupedBy(true);
      }

      return nOrdinal;
   }

   /**
    * Adds a group by expression.
    * @param operator The expression by which to group by.
    * @return The ordinal number of the added expression.
    */
   public int addGroupBy(Operator operator)
   {
      return addGroupBy(m_nGroupByCount, operator);
   }

   /**
    * Removes a group by expression.
    * @param nOrdinal The expression ordinal number.
    */
   public void removeGroupBy(int nOrdinal)
   {
      assert nOrdinal >= 0 && nOrdinal < m_nGroupByCount;

      System.arraycopy(m_groupByArray, nOrdinal + 1, m_orderByArray, nOrdinal, m_nGroupByCount-- - nOrdinal - 1);
   }

   /**
    * Replaces a group by expression.
    * @param nOrdinal The expression ordinal number.
    * @param operator The group by expression.
    */
   public void setGroupBy(int nOrdinal, Operator operator)
   {
      assert nOrdinal >= 0 && nOrdinal < m_nGroupByCount;

      m_groupByArray[nOrdinal] = operator;
   }

   /**
    * Gets a group by expression.
    * @param nOrdinal The expression ordinal number.
    * @return The group by expression.
    */
   public Operator getGroupBy(int nOrdinal)
   {
      assert nOrdinal >= 0 && nOrdinal < m_nGroupByCount;

      return m_groupByArray[nOrdinal];
   }

   /**
    * Finds a group by expression ordinal number.
    * @param op The expression to find.
    * @return The expression ordinal number, or -1 if not found.
    */
   public int findGroupBy(Operator op)
   {
      for (int i = 0; i < m_nGroupByCount; ++i)
      {
         if (op.compareTo(m_groupByArray[i]) == 0)
         {
            return i;
         }
      }

      return -1;
   }

   /**
    * @return The group by expression count.
    */
   public int getGroupByCount()
   {
      return m_nGroupByCount;
   }

   /**
    * Determines if a field is grouped by.
    * @param field The field to test.
    * @return True if it is grouped by.
    */
   public boolean isGroupedBy(Field field)
   {
      return !isAggregate() || field.isGroupedBy() ||
         field.getOperator() != null && isGroupedBy(field.getOperator());
   }

   /**
    * Determines if an operator is grouped by or
    * consists only of grouped by operands.
    * @param op The operator to test.
    * @return True if it is grouped by.
    */
   public boolean isGroupedBy(Operator op)
   {
      if (!isAggregate())
      {
         return true;
      }

      if (m_groupedByVisitor == null)
      {
         m_groupedByVisitor = new Operator.Visitor()
         {
            public boolean visit(Operator op)
            {
               if (op instanceof AttributeOperator)
               {
                  return op.getSource().isGroupedBy();
               }

               return true;
            }

            public boolean isEligible(Operator op)
            {
               if (op instanceof AggregateOperator || op.isConstant())
               {
                  return false;
               }

               return findGroupBy(op) < 0;
            }
         };
      }

      return !m_groupedByVisitor.isEligible(op) ||
         op.visit(m_groupedByVisitor, Operator.VISIT_PREORDER);
   }

   /**
    * Determines if an operator is grouped by or has a grouped by (indirect) parent.
    * @param aop The operator to test.
    * @return True if it is grouped by.
    */
   public boolean isGroupedByOperand(AttributeOperator aop)
   {
      if (aop.getSource().isGroupedBy())
      {
         return true;
      }

      for (Operator op = aop; op != null; op = op.getParent())
      {
         if (op instanceof AggregateOperator || findGroupBy(op) >= 0)
         {
            return true;
         }
      }

      return false;
   }

   /**
    * Adds an order by clause to this query: ((op1 . #t) (op2 . #t) ... (opN . #t)).
    * @param orderBy The list of order by S-expressions and ascending sort order flags.
    */
   public void addOrderBy(Pair orderBy)
   {
      try
      {
         for (; orderBy != null; orderBy = orderBy.getNext())
         {
            Pair pair = (Pair)orderBy.getHead();

            if (pair == null || pair.getTail() == null)
            {
               throw new InvalidQueryException("err.persistence.queryOrderBy");
            }

            addOrderBy(createOperator(ASSOC_QUERY, pair.getHead(), OUTPUT_NONE), ((Boolean)pair.getTail()).booleanValue());
         }
      }
      catch (ClassCastException e)
      {
         throw new InvalidQueryException("err.persistence.queryOrderBy", e);
      }
      catch (MetadataException e)
      {
         throw new InvalidQueryException("err.persistence.queryOrderBy", e);
      }
   }

   /**
    * Adds an order by expression.
    * @param nOrdinal The ordinal number of the expression.
    * @param operator The expression by which to order by.
    * @param bAscending True if ascending sort order should be applied.
    * @return The ordinal number of the added expression.
    */
   public int addOrderBy(int nOrdinal, Operator operator, boolean bAscending)
   {
      assert nOrdinal >= 0 && nOrdinal <= m_nOrderByCount;
      assert operator != null;

      nOrdinal <<= 1;

      int nCount = (m_nOrderByCount << 1);

      if (m_orderByArray == null)
      {
         m_orderByArray = new Operator[16];
      }
      else
      {
         if (nCount == m_orderByArray.length)
         {
            Operator[] orderByArray = new Operator[nCount << 1];
            System.arraycopy(m_orderByArray, 0, orderByArray, 0, nOrdinal);
            System.arraycopy(m_orderByArray, nOrdinal, orderByArray, nOrdinal + 2, nCount - nOrdinal);
            m_orderByArray = orderByArray;
         }
         else
         {
            System.arraycopy(m_orderByArray, nOrdinal, m_orderByArray, nOrdinal + 2, nCount - nOrdinal);
         }
      }

      m_orderByArray[nOrdinal] = operator;
      m_orderByArray[nOrdinal + 1] = (bAscending) ? TRUE_OPERATOR : null;
      m_nOrderByCount++;

      return nOrdinal >> 1;
   }

   /**
    * Adds an order by expression.
    * @param operator The expression by which to order by.
    * @param bAscending True if ascending sort order should be applied.
    * @return The ordinal number of the added expression.
    */
   public int addOrderBy(Operator operator, boolean bAscending)
   {
      return addOrderBy(m_nOrderByCount, operator, bAscending);
   }

   /**
    * Removes an order by expression.
    * @param nOrdinal The expression ordinal number.
    */
   public void removeOrderBy(int nOrdinal)
   {
      assert nOrdinal >= 0 && nOrdinal < m_nOrderByCount;

      nOrdinal <<= 1;

      int nCount = (m_nOrderByCount << 1);

      System.arraycopy(m_orderByArray, nOrdinal + 2, m_orderByArray, nOrdinal, nCount - nOrdinal - 2);
      --m_nOrderByCount;
   }

   /**
    * Replaces an order by expression.
    * @param nOrdinal The expression ordinal number.
    * @param operator The order by expression.
    */
   public void setOrderByOperator(int nOrdinal, Operator operator)
   {
      assert nOrdinal >= 0 && nOrdinal < m_nOrderByCount;

      m_orderByArray[nOrdinal << 1] = operator;
   }

   /**
    * Gets an order by expression.
    * @param nOrdinal The expression ordinal number.
    * @return The order by expression.
    */
   public Operator getOrderByOperator(int nOrdinal)
   {
      assert nOrdinal >= 0 && nOrdinal < m_nOrderByCount;

      return m_orderByArray[nOrdinal << 1];
   }

   /**
    * Gets an order by expression sort order.
    * @param nOrdinal The expression ordinal number.
    * @return True is the expression is sorted ascendingly.
    */
   public boolean isOrderByAscending(int nOrdinal)
   {
      assert nOrdinal >= 0 && nOrdinal < m_nOrderByCount;

      return m_orderByArray[(nOrdinal << 1) + 1] != null;
   }

   /**
    * @return The order by expression count.
    */
   public int getOrderByCount()
   {
      return m_nOrderByCount;
   }

   /**
    * Sets the maximum instance count.
    * @param nMaxCount The maximum instance count to set.
    */
   public void setMaxCount(int nMaxCount)
   {
      assert nMaxCount >= -1;

      m_nMaxCount = nMaxCount;
   }

   /**
    * @return The maximum instance count.
    */
   public int getMaxCount()
   {
      return m_nMaxCount;
   }

   /**
    * Sets the limit of the number of instances retrieved by a single read() operation.
    * @param nLimit The limit of the number of instances retrieved by a single read()
    * operation to set (negative for unlimited, 0 for default).
    */
   public void setLimit(int nLimit)
   {
      m_nLimit = nLimit;
   }

   /**
    * @return The limit of the number of instances retrieved by a single read()
    * operation (non-positive for unlimited).
    */
   public int getLimit()
   {
      if (m_nLimit == 0)
      {
         return m_persistenceMapping.getDataSource().getReadLimit();
      }

      return m_nLimit;
   }

   /**
    * Sets the instance offset.
    * @param nOffset The instance offset to set.
    */
   public void setOffset(int nOffset)
   {
      assert nOffset >= 0;

      m_nOffset = nOffset;
   }

   /**
    * @return The instance offset.
    */
   public int getOffset()
   {
      return m_nOffset;
   }

   /**
    * Sets the query timeout in seconds.
    * @param nTimeout The query timeout in seconds to set (0 for unlimited, negative to use the default).
    */
   public void setTimeout(int nTimeout)
   {
      m_nTimeout = nTimeout;
   }

   /**
    * @return The query timeout in seconds (0 for unlimited, negative to use the default).
    */
   public int getTimeout()
   {
      return m_nTimeout;
   }

   /**
    * Sets the locking flag.
    * @param bLocking The locking flag to set.
    */
   public void setLocking(boolean bLocking)
   {
      m_bLocking = bLocking;
   }

   /**
    * @return The locking flag.
    */
   public boolean isLocking()
   {
      return m_bLocking;
   }

   /**
    * Sets the query constraint.
    * @param constraint The query constraint to set.
    */
   public void setConstraint(Operator constraint)
   {
      m_constraint = constraint;
   }

   /**
    * @return The query constraint.
    */
   public Operator getConstraint()
   {
      return m_constraint;
   }

   /**
    * Sets the query generator.
    * @param generator The query generator to set.
    */
   public void setGenerator(Object generator)
   {
      m_generator = generator;
   }

   /**
    * @return The query generator.
    */
   public Object getGenerator()
   {
      return m_generator;
   }

   /**
    * Adds restriction flags.
    * @param nRestriction The restriction flags (combination of RESTRICTION_* constants).
    */
   public void addRestriction(int nRestriction)
   {
      m_nRestriction |= nRestriction;
   }

   /**
    * Adds restriction flags based on the association key and output mode.
    * @param key The association key (ASSOC_*).
    * @param nOutput The output mode (OUTPUT_*).
    * @param nRestriction The restriction mode.
    * @param nParentRestriction The parent restriction mode.
    */
   protected void addRestriction(Object key, byte nOutput, int nRestriction, int nParentRestriction)
   {
      if (nOutput == OUTPUT_NONE)
      {
         if (key == ASSOC_WHERE || key instanceof AnyOperator ||
            (nParentRestriction & RESTRICTION_PARENT) != 0 &&
            ((nParentRestriction | m_nRestriction) & RESTRICTION_WHERE) != 0)
         {
            nRestriction |= RESTRICTION_WHERE;
         }
         else
         {
            nRestriction |= RESTRICTION_ORDERBY;
         }

         addRestriction(nRestriction);
      }
      else if ((nParentRestriction & RESTRICTION_PARENT) != 0)
      {
         addRestriction(nParentRestriction & ~RESTRICTION_PARENT);
      }
   }

   /**
    * Removes restriction flags.
    * @param nRestriction The restriction flags (combination of RESTRICTION_* constants).
    */
   public void removeRestriction(int nRestriction)
   {
      m_nRestriction &= ~nRestriction;
   }

   /**
    * @return The restriction mode (combination of RESTRICTION_* constants).
    */
   public byte getRestriction()
   {
      return m_nRestriction;
   }

   /**
    * Sets the caching flag.
    * @param bCached The caching flag to set.
    */
   public void setCached(boolean bCached)
   {
      m_nCached = (bCached) ? (byte)1 : 0;
   }

   /**
    * @return The caching flag.
    */
   public boolean isCached()
   {
      return m_nCached > 0;
   }

   /**
    * @return True if the caching flag has been specified.
    */
   public boolean isCachingSpecified()
   {
      return m_nCached >= 0;
   }

   /**
    * Sets the subquery flag (for persistence mapping purposes).
    * @param bSubquery The subquery flag to set.
    */
   public void setSubquery(boolean bSubquery)
   {
      m_bSubquery = bSubquery;
   }

   /**
    * @return The subquery flag (for persistence mapping purposes).
    */
   public boolean isSubquery()
   {
      return m_bSubquery;
   }

   /**
    * Sets the cursor retrieval flag.
    * @param bCursor The cursor retrieval flag to set.
    */
   public void setCursor(boolean bCursor)
   {
      m_bCursor = bCursor;
   }

   /**
    * @return The cursor retrieval flag.
    */
   public boolean isCursor()
   {
      return m_bCursor;
   }

   /**
    * Sets the persistence adapter on this query.
    * @param adapter The persistence adapter to set.
    */
   public void setAdapter(PersistenceAdapter adapter)
   {
      m_adapter = adapter;
   }

   /**
    * Gets the persistence adapter from the query root.
    * @return The persistence adapter.
    */
   public PersistenceAdapter getAdapter()
   {
      return m_root.m_adapter;
   }

   /**
    * Sets the invocation context.
    * @param context The invocation context to set.
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * @return The invocation context.
    */
   public InvocationContext getInvocationContext()
   {
      return m_root.m_context;
   }

   /**
    * Reduces the number of involved where query nodes to eliminate
    * superfluous joins by aliasing them into the query map. 
    */
   public void optimizeJoins()
   {
      m_whereArray = addAssocs(ASSOC_QUERY, m_whereArray, false, false);
   }

   /**
    * Reduces the query multiplicity.
    * TODO: Use metadata and include in the query planning pass.
    * @return The log2 of current multiplicity.
    */
   public int reduce()
   {
      int nMultiplicity = 0;

      for (Iterator itr = getAssocIterator(ASSOC_QUERY); itr.hasNext();)
      {
         Query query = (Query)itr.next();
         int nAssocMultiplicity = query.reduce();

         if (nAssocMultiplicity >= 3 && nMultiplicity > 0)
         {
            if (query.isOutput() &&
               (query.getRestriction() & (RESTRICTION_WHERE | RESTRICTION_ORDERBY)) == 0 &&
               findAssoc(ASSOC_WHERE, query.getAttribute(), query.getFilter(), query.isInverse()) == null)
            {
               query.makeRoot(getRoot().getInvocationContext());
               nAssocMultiplicity = -1;
            }
         }

         if (nAssocMultiplicity >= 0)
         {
            nMultiplicity += nAssocMultiplicity;
         }
      }

      if (isRoot() || (getRestriction() & (RESTRICTION_WHERE | RESTRICTION_ORDERBY)) != 0)
      {
         return -1;
      }

      if (isCollection())
      {
         nMultiplicity += 3;
      }

      return nMultiplicity;
   }

   /**
    * @see nexj.core.persistence.Source#getConstrainedValue()
    */
   public Object getConstrainedValue()
   {
      if (m_parent != null)
      {
         return m_parent.getConstrainedValue(this);
      }

      return Undefined.VALUE;
   }

   /**
    * Gets the constant value of a given source inferred from constraints.
    * @param source The source.
    * @return The constant value, or Undefined.VALUE if not constant.
    */
   protected Object getConstrainedValue(Source source)
   {
      Operator op = m_constraint;

      if (op instanceof EqualsOperator)
      {
         EqualsOperator eq = (EqualsOperator)m_constraint;
         Object value = eq.getRight().getValue();

         if (value instanceof OIDHolder)
         {
            OID oid = ((OIDHolder)value).getOID();

            if (oid != null)
            {
               op = eq.getParent();

               while (op != null && op.getOrdinal() == AndOperator.ORDINAL)
               {
                  op = op.getParent();
               }

               if (op == null || op.getOrdinal() == AnyOperator.ORDINAL)
               {
                  return getAdapter().getValue(oid, source);
               }
            }
         }

         setConstraint(null);
      }

      return Undefined.VALUE;
   }

   /**
    * Sorts a query array.
    * @param queryArray The array to sort. Modified by this method. Can be null.
    */
   protected static void sort(Query[] queryArray)
   {
      if (queryArray != null)
      {
         // Sort the queries topologically

         if (queryArray.length > 1)
         {
            QueryDepVisitor visitor = new QueryDepVisitor(queryArray);
            boolean bDep = false;

            for (int i = 0; i < queryArray.length; ++i)
            {
               queryArray[i].m_nPredCount = 0;
            }

            for (int i = 0; i < queryArray.length; ++i)
            {
               Query query = queryArray[i];
               Query[] depArray = visitor.findDeps(query);

               if (depArray != null)
               {
                  for (int k = 0; k < depArray.length; ++k)
                  {
                     ++depArray[k].m_nPredCount;
                  }

                  bDep = true;
               }

               query.m_depArray = depArray;
            }

            if (bDep)
            {
               int nEnd = queryArray.length;
               int nStart = nEnd;
               Query[] topoArray = new Query[nEnd];

               for (int i = queryArray.length - 1; i >= 0; --i)
               {
                  Query query = queryArray[i];

                  if (query.m_nPredCount == 0)
                  {
                     topoArray[--nStart] = query;
                  }
               }

               while (nEnd > nStart)
               {
                  if (nEnd - nStart > 1)
                  {
                     Arrays.sort(topoArray, nStart, nEnd);
                  }

                  int i = nEnd;

                  nEnd = nStart;
                  
                  while (i > nEnd)
                  {
                     Query[] depArray = topoArray[--i].m_depArray;

                     if (depArray != null)
                     {
                        for (int k = 0; k < depArray.length; ++k)
                        {
                           Query query = depArray[k];

                           if (--query.m_nPredCount == 0)
                           {
                              topoArray[--nStart] = query;
                           }
                        }
                     }
                  }

               }

               if (nEnd > 0)
               {
                  for (int i = queryArray.length - 1; i >= 0; --i)
                  {
                     Query query = queryArray[i];

                     if (query.m_nPredCount != 0)
                     {
                        topoArray[--nStart] = query;
                     }
                  }
               }

               if (nEnd - nStart > 1)
               {
                  Arrays.sort(topoArray, nStart, nEnd);
               }

               System.arraycopy(topoArray, 0, queryArray, 0, topoArray.length);

               for (int i = 0; i < queryArray.length; ++i)
               {
                  queryArray[i].m_depArray = null;
               }
            }
            else
            {
               Arrays.sort(queryArray);
            }
         }
      }
   }

   /**
    * Sorts data to ensure consistent query plan caching in persistence
    * stores using generated query strings as cache keys.
    */
   public void sort()
   {
      // Sort fields

      if (m_fieldMap.size() > 1)
      {
         Object[] keyArray = new Object[m_fieldMap.size()];
         int i = 0;
         
         for (Lookup.Iterator itr = m_fieldMap.iterator(); itr.hasNext();)
         {
            keyArray[i++] = itr.next();
            ((Field)itr.getValue()).setNext(null);
         }

         Arrays.sort(keyArray, FIELD_KEY_COMPARATOR);

         Lookup fieldMap = new LinkedHashTab(keyArray.length);
         Field firstOutput = null;
         Field lastOutput = null;

         for (i = 0; i < keyArray.length; ++i)
         {
            Object key = keyArray[i];
            Field field = (Field)m_fieldMap.get(key);

            fieldMap.put(key, field);

            if (field.isOutput() && field.getNext() == null && field != lastOutput)
            {
               if (lastOutput == null)
               {
                  firstOutput = lastOutput = field;
               }
               else
               {
                  lastOutput.setNext(field);
                  lastOutput = field;
               }
            }
         }

         if (lastOutput != null)
         {
            lastOutput.setNext(null);
            m_firstOutputField = firstOutput;
            m_lastOutputField = lastOutput;
         }

         m_fieldMap = fieldMap;
      }

      // Sort associations

      sort(m_queryArray);
      sort(m_whereArray);

      if (m_quantorMap != null)
      {
         for (Iterator itr = m_quantorMap.valueIterator(); itr.hasNext();)
         {
            sort((Query[])itr.next());
         }
      }
   }

   /**
    * Normalizes the fields.
    * @param bPersistence True to normalize the persistence mappings.
    */
   public void normalizeFields(boolean bPersistence)
   {
      Field prev = null;

      for (Field field = m_firstOutputField; field != null; field = field.getNext())
      {
         Operator opSaved = field.getOperator();

         if (field.normalize((bPersistence) ? Operator.NORMALIZE_PERSISTENCE : 0))
         {
            prev = field;
         }
         else
         {
            Field next = field.getNext();

            if (prev == null)
            {
               m_firstOutputField = next;

               if (next == null)
               {
                  m_lastOutputField = null;
               }
            }
            else
            {
               prev.setNext(next);
            }

            if (opSaved != null)
            {
               m_fieldMap.remove(opSaved);
            }

            if (field.getAttribute() != null)
            {
               m_fieldMap.remove(field.getAttribute());
            }

            Operator op = field.getOperator();

            if (op != null)
            {
               op.visit(OUTPUT_VISITOR, Operator.VISIT_PREORDER);
            }
         }
      }
   }

   /**
    * Normalizes the where and having clauses.
    * @param bPersistence True to normalize the persistence mappings.
    */
   public void normalizeWhere(boolean bPersistence)
   {
      if (m_where != null)
      {
         m_where = m_where.normalize((bPersistence) ?
            Operator.NORMALIZE_WHERE | Operator.NORMALIZE_PERSISTENCE :
            Operator.NORMALIZE_WHERE);

         if (m_where.getType() != null && m_where.getType() != Primitive.BOOLEAN)
         {
            throw new InvalidQueryException("err.persistence.whereType");
         }

         if (m_where.isConstant() && !Boolean.FALSE.equals(m_where.getValue()))
         {
            m_where = null;
         }
      }

      if (bPersistence && m_having != null)
      {
         m_having = m_having.normalize(Operator.NORMALIZE_HAVING | Operator.NORMALIZE_PERSISTENCE);

         if (m_having.getType() != null && m_having.getType() != Primitive.BOOLEAN)
         {
            throw new InvalidQueryException("err.persistence.whereType");
         }

         if (m_having.isConstant() && !Boolean.FALSE.equals(m_having.getValue()))
         {
            m_having = null;
         }
      }

      if (m_where == null && m_having == null)
      {
         m_whereArray = null;

         if (m_firstOutputField == null && m_fieldMap.size() != 0 &&
            m_nOrderByCount == 0 && m_nGroupByCount == 0)
         {
            for (Lookup.Iterator itr = m_fieldMap.valueIterator(); itr.hasNext();)
            {
               Field field = (Field)itr.next();

               if (field.getConstrainedValue() != Undefined.VALUE)
               {
                  itr.remove();
               }
            }
         }
      }

      if (isJoin())
      {
         if (m_where != null && m_where.getOrdinal() != AndOperator.ORDINAL)
         {
            AndOperator and = new AndOperator();

            and.addOperand(m_where);
            m_where = and;
         }
      }
   }

   /**
    * Sets association query required flags, where possible.
    */
   private void normalizeRequired()
   {
      visit(REQUIRED_RESTRICTION_CLEANUP_VISITOR, VISIT_WHERE | VISIT_QUANTOR, VISIT_QUERY);

      if (m_where != null)
      {
         m_where.visit(REQUIRED_RESTRICTION_COMPUTING_VISITOR, Operator.VISIT_PREORDER);
      }

      if (m_quantorMap != null)
      {
         for (Iterator itr = m_quantorMap.iterator(); itr.hasNext();)
         {
            Operator op = ((Quantor)itr.next()).getOperand();

            if (op != null)
            {
               op.visit(REQUIRED_RESTRICTION_COMPUTING_VISITOR, Operator.VISIT_PREORDER);
            }
         }
      }

      visit(new Visitor()
      {
         public boolean visit(Query query)
         {
            byte nRestriction = (byte)(query.getRestriction() & (RESTRICTION_REQUIRED | RESTRICTION_NOT_REQUIRED));

            while (query != Query.this)
            {
               query = query.getParent();
               query.addRestriction(nRestriction);
            }

            return true;
         }

         public boolean postVisit(Query query)
         {
            if (query != Query.this &&
               (query.getRestriction() & (RESTRICTION_REQUIRED | RESTRICTION_NOT_REQUIRED)) == RESTRICTION_REQUIRED)
            {
               query.setRequired(true);
            }

            return true;
         }

         public boolean isEligible(Query query)
         {
            return true;
         }
      }, VISIT_WHERE | VISIT_QUANTOR, VISIT_QUERY);
   }

   /**
    * Normalizes the group by clause.
    */
   private void normalizeGroupBy()
   {
      for (int i = 0; i < m_nGroupByCount;)
      {
         Operator op = getGroupBy(i).normalize(Operator.NORMALIZE_GROUPBY);

         setGroupBy(i, op);

         if (op.isConstant())
         {
            removeGroupBy(i);
         }
         else
         {
            if (op.getOrdinal() == AttributeOperator.ORDINAL)
            {
               Source source = op.getSource();

               source.setGroupedBy(true);

               if (source.getType().isPrimitive())
               {
                  unconvert((AttributeOperator)op);
               }
               else
               {
                  Query query = source.getQuery();
                  Field[] fieldArray = query.getAdapter().getFields(query);

                  if (fieldArray != null)
                  {
                     Key key = query.getPersistenceMapping().getObjectKey();

                     for (int k = 0; k < fieldArray.length; ++k)
                     {
                        Field field = fieldArray[k];

                        field.setGroupedBy(true);

                        // No need to normalize
                        if (k == 0)
                        {
                           op.setSource(field);
                           op.setType(key.getPartType(k));
                           unconvert((AttributeOperator)op);
                        }
                        else
                        {
                           addGroupBy(++i, unconvert(new AttributeOperator(field)));
                        }
                     }
                  }
               }
            }

            ++i;
         }
      }

      if (m_having != null && !isGroupedBy(m_having))
      {
         throw new InvalidQueryException("err.persistence.ungroupedHaving");
      }
   }

   /**
    * Normalizes the order by clause.
    */
   private void normalizeOrderBy()
   {
      for (int i = 0; i < m_nOrderByCount;)
      {
         Operator op = getOrderByOperator(i).normalize(Operator.NORMALIZE_ORDERBY | Operator.NORMALIZE_PERSISTENCE);

         setOrderByOperator(i, op);

         if (op.isConstant())
         {
            removeOrderBy(i);
         }
         else
         {
            if (isAggregate() && !isGroupedBy(op))
            {
               throw new InvalidQueryException("err.persistence.ungroupedOrderBy");
            }

            if (op.getOrdinal() == AttributeOperator.ORDINAL)
            {
               if (op.getSource().getType().isPrimitive())
               {
                  unconvert((AttributeOperator)op);
               }
               else
               {
                  Query query = op.getSource().getQuery();
                  Field[] fieldArray = query.getAdapter().getFields(query);

                  if (fieldArray != null)
                  {
                     Key key = query.getPersistenceMapping().getObjectKey();
                     boolean bAscending = isOrderByAscending(i) ^ key.isPartAscending(0);

                     for (int k = 0; k < fieldArray.length; ++k)
                     {
                        // No need to normalize
                        if (k == 0)
                        {
                           op.setSource(fieldArray[k]);
                           op.setType(key.getPartType(k));
                           unconvert((AttributeOperator)op);
                        }
                        else
                        {
                           addOrderBy(++i, unconvert(new AttributeOperator(fieldArray[k])),
                              bAscending ^ key.isPartAscending(k));
                        }
                     }
                  }
               }
            }

            ++i;
         }
      }
   }

   /**
    * Removes the converter from an attribute operator if it preserves the sort order.
    * @param op The attribute operator.
    * @return op.
    */
   private AttributeOperator unconvert(AttributeOperator op)
   {
      Converter converter = op.getConverter();

      if (converter != null &&
         Primitive.isOrderPreserved(converter.getSourceType(), converter.getDestinationType()))
      {
         op.setType(converter.getSourceType());
         op.setNoConversion(true);
      }

      return op;
   }

   /**
    * Prepares the query for executing.
    * @param bPartial True if the query result will be retrieved partially.
    */
   public void prepare(boolean bPartial)
   {
      if (m_parent == null)
      {
         if (bPartial && m_bPlural && (!isAggregate() || m_bGroupedBy))
         {
            // For cursors and max count queries, forbid subcollections in the
            // order by clause and append the OID

            for (int i = 0; i < m_nOrderByCount; ++i)
            {
               Operator op = getOrderByOperator(i);

               if (op.getOrdinal() == AttributeOperator.ORDINAL)
               {
                  if (op.getSource() == this)
                  {
                     bPartial = false;
                     break;
                  }

                  for (Query query = op.getSource().getQuery(); query != this; query = query.getParent())
                  {
                     if (query.getAttribute().isCollection())
                     {
                        throw new InvalidQueryException("err.persistence.orderBySubcollection");
                     }
                  }
               }
            }

            if (bPartial)
            {
               addOrderBy(new AttributeOperator(this), true);
            }
         }

         if (m_outputQueryList != null)
         {
            m_outputQueryList.clear();
         }

         m_rootList = new ArrayList(4);
         visit(NORMALIZATION_VISITOR, VISIT_ALL);
         visit(PLANNING_VISITOR, VISIT_QUERY);
         visit(FIELD_VISITOR, VISIT_ALL);

         if (isAggregate())
         {
            visit(IDENTITY_VISITOR, VISIT_QUERY);
         }

         visit(MAPPING_VISITOR, VISIT_ALL);
         normalizeGroupBy();
         normalizeOrderBy();
      }
   }

   /**
    * @return List of cached attribute symbols for a read specification.
    */
   protected Pair getCachedAttributes()
   {
      Pair attributes = null;

      for (int i = m_metaclass.getInstanceAttributeCount() - 1; i >= 0; --i)
      {
         Attribute attribute = m_metaclass.getInstanceAttribute(i);

         if (!attribute.isLazy())
         {
            attributes = new Pair(attribute.getSymbol(), attributes);
         }
      }

      return attributes;
   }

   /**
    * Reads the class instances with disabled caching.
    * @param attributes The read attribute list.
    * @param where The where clause.
    * @return The instance list.
    * @see #createRead(Metaclass, Pair, Object, Pair, int, int, boolean, byte, InvocationContext)
    */
   protected InstanceList readUncached(Pair attributes, Object where)
   {
      Query query = Query.createRead(m_metaclass, attributes, where, null, -1, 0, false, SEC_NONE, getInvocationContext());

      query.setLimit(-1);
      query.setCached(false);

      if (m_parent != null)
      {
         for (Iterator itr = query.getAssocIterator(ASSOC_QUERY); itr.hasNext();)
         {
            Query assoc = (Query)itr.next();

            if (assoc.getMetaclass() == m_parent.getMetaclass())
            {
               assoc.setCached(false);
            }
         }
      }

      return query.read();
   }

   /**
    * Caches a query result.
    * @param cacheMap The map of items to cache.
    * @param result The query result for determining the policy.
    */
   protected void cache(Lookup cacheMap, Object result)
   {
      boolean bDirty = false;

      if (result instanceof TransferObject)
      {
         bDirty = (((TransferObject)result).getEventName() != null);
      }
      else if (result instanceof List)
      {
         List list = (List)result;

         for (int i = 0, n = list.size(); i < n; ++i)
         {
            TransferObject tobj = (TransferObject)list.get(i);

            bDirty = (tobj.getEventName() != null);

            if (bDirty)
            {
               break;
            }
         }
      }

      InvocationContext context = getInvocationContext();

      if (!bDirty)
      {
         context.getGlobalCache().update(null, cacheMap, context.getUnitOfWork().getTime());
      }

      for (Lookup.Iterator itr = cacheMap.iterator(); itr.hasNext();)
      {
         itr.next();
         context.getUnitOfWork().cacheTemporary(itr.getKey(), itr.getValue(), UnitOfWork.CACHE_UNPARTITIONED);
      }
   }

   /**
    * Gets a cached query result.
    * @param The query result key.
    * @return The query result, or null if not found.
    */
   protected Pair getCached(Object key)
   {
      return (Pair)getInvocationContext().getUnitOfWork().getCached(key, UnitOfWork.CACHE_UNPARTITIONED);
   }

   /**
    * Reads the class instances specified by the query from cache.
    * @param The class instance list.
    */
   protected InstanceList readCached()
   {
      assert isCached();

      InvocationContext context = getInvocationContext();
      Lookup identityMap = new HashTab(4);
      boolean bCached = true;

      for (;;)
      {
         boolean bInstanceCaching = true;
         boolean bFiltered = false;
         boolean bMissed = false;
         List tobjList = null;

         if (m_persistenceMapping.getCaching() == PersistenceMapping.CACHING_CLASS)
         {
            Object classKey = Instance.getClassKey(m_metaclass, null);
            Pair classResult = getCached(classKey);
            Object queryKey = classKey;
            Pair queryResult = classResult;

            if (m_metaclass.getWhere() != null)
            {
               Query query = Query.createRead(m_metaclass, null, null, null, -1, 0, false, SEC_NONE, context);

               query.setLimit(-1);
               query.setCached(false);
               query.setCursor(false);
               query.prepare(false);

               if (query.getWhere() != null)
               {
                  queryKey = Instance.getClassKey(m_metaclass, query.toString(query.getWhere()));
                  queryResult = getCached(queryKey);
               }
            }

            if (bCached && queryResult != null && classResult != null &&
               ObjUtil.equal(queryResult.getTail(), classResult.getTail()))
            {
               tobjList = (List)queryResult.getHead();
            }

            if (tobjList == null)
            {
               Pair attributes = getCachedAttributes();
               InstanceList instanceList = readUncached(attributes, null);
               int nCount = instanceList.size();

               tobjList = new ArrayList(nCount);

               for (int i = 0; i < nCount; ++i)
               {
                  tobjList.add(RPCUtil.transfer(instanceList.get(i), attributes, identityMap,
                     RPCUtil.TF_ALL | RPCUtil.TF_LAZY | RPCUtil.TF_OLD));
                  identityMap.clear();
               }

               instanceList = null;

               Lookup cacheMap = new HashTab(2);

               queryResult = new Pair(tobjList);

               if (classKey != queryKey)
               {
                  classResult = getCached(classKey);

                  if (classResult == null)
                  {
                     classResult = new Pair(null, new Object());
                     cacheMap.put(classKey, classResult);
                  }
               }

               if (classResult != null)
               {
                  queryResult.setTail(classResult.getTail());
               }
               else
               {
                  queryResult.setTail(new Object());
               }

               cacheMap.put(queryKey, queryResult);
               cache(cacheMap, tobjList);
               bMissed = true;
            }

            bInstanceCaching = tobjList.size() > 64 && isUnique();
         }

         if (bInstanceCaching)
         {
            TransferObject tobj = null;
            Object instanceKey = null;
            Pair instanceResult = null;
            Object oidKey = null;
            Pair oidResult = null;
            OID oid;

            if (m_oid != null)
            {
               oid = m_oid;
            }
            else
            {
               oidKey = Pair.list(Symbol._CLASS, m_metaclass.getSymbol(), toString(m_where));
               oidResult = getCached(oidKey);
               oid = (oidResult == null) ? null : (OID)oidResult.getHead();
            }

            if (oid != null)
            {
               instanceKey = Instance.getInstanceKey(m_metaclass, oid);
               instanceResult = getCached(instanceKey);

               if (bCached && instanceResult != null &&
                  (m_oid != null || ObjUtil.equal(instanceResult.getTail(), oidResult.getTail())))
               {
                  tobj = (TransferObject)instanceResult.getHead();
               }
            }

            if (tobj == null)
            {
               tobj = NULL_TO;

               if (tobjList == null)
               {
                  Pair attributes = getCachedAttributes();
                  InstanceList instanceList;

                  if (m_oid != null)
                  {
                     instanceList = readUncached(attributes, Pair.attribute("").eq(m_oid));
                  }
                  else
                  {
                     int nLimitSaved = m_nLimit;
                     int nOffsetSaved = m_nOffset;
                     int nMaxCountSaved = m_nMaxCount;
                     byte nCachedSaved = m_nCached;

                     m_nLimit = -1;
                     m_nOffset = 0;
                     setCached(false);

                     try
                     {
                        instanceList = m_adapter.read(this);
                     }
                     finally
                     {
                        m_nLimit = nLimitSaved;
                        m_nOffset = nOffsetSaved;
                        m_nMaxCount = nMaxCountSaved;
                        m_nCached = nCachedSaved;
                     }

                     bFiltered = true;

                     for (Iterator itr = getAssocIterator(ASSOC_QUERY); itr.hasNext();)
                     {
                        Query assoc = (Query)itr.next();

                        if (assoc.isJoin() && !assoc.isInverse() && !assoc.getAttribute().isLazy())
                        {
                           assoc.join();
                        }
                     }
                  }

                  if (!instanceList.isEmpty())
                  {
                     tobj = (TransferObject)RPCUtil.transfer(instanceList.get(0), attributes,
                        identityMap, RPCUtil.TF_ALL | RPCUtil.TF_LAZY | RPCUtil.TF_OLD);
                     identityMap.clear();
                     oid = tobj.getOID();
                  }

                  bMissed = true;
               }
               else
               {
                  if (m_oid != null)
                  {
                     for (int i = 0, n = tobjList.size(); i < n; ++i)
                     {
                        TransferObject cur = (TransferObject)tobjList.get(i);

                        if (m_oid.equals(cur.getOID()))
                        {
                           tobj = cur;

                           break;
                        }
                     }
                  }
                  else
                  {
                     for (int i = 0, n = tobjList.size(); i < n; ++i)
                     {
                        TransferObject cur = (TransferObject)tobjList.get(i);

                        m_instance = cur;

                        if (m_where == null || !Boolean.FALSE.equals(m_where.getValue()))
                        {
                           tobj = cur;

                           break;
                        }
                     }

                     bFiltered = true;
                  }
               }

               if (oid != null)
               {
                  if (instanceKey == null)
                  {
                     instanceKey = Instance.getInstanceKey(m_metaclass, oid);
                  }

                  Lookup cacheMap = new HashTab(2);

                  instanceResult = (m_oid != null) ? null : getCached(instanceKey);

                  if (instanceResult == null)
                  {
                     instanceResult = new Pair(tobj);

                     if (tobj != NULL_TO)
                     {
                        if (m_persistenceMapping.getLockingAttribute() != null)
                        {
                           instanceResult.setTail(tobj.findValue(m_persistenceMapping.getLockingAttribute().getName()));
                        }

                        if (instanceResult.getTail() == null)
                        {
                           instanceResult.setTail(new Object());
                        }
                     }

                     cacheMap.put(instanceKey, instanceResult);
                  }

                  if (m_oid == null)
                  {
                     cacheMap.put(oidKey, new Pair(oid, instanceResult.getTail()));
                  }

                  cache(cacheMap, tobj);
               }
            }
            else
            {
               bMissed = false;
            }

            if (tobj != NULL_TO)
            {
               tobjList = Collections.singletonList(tobj);
            }
            else
            {
               tobjList = Collections.EMPTY_LIST;
            }
         }

         int nOffset = 0;
         int nCount = tobjList.size();
         InstanceList instanceList = new InstanceArrayList(nCount);
         InstanceFactory factory = new InstanceFactory(identityMap, null, InstanceFactory.STATE, context);

         for (int i = 0; i < nCount && (m_nMaxCount < 0 || instanceList.size() < m_nMaxCount); ++i)
         {
            TransferObject tobj = (TransferObject)tobjList.get(i);

            m_instance = tobj;

            if (bFiltered || m_where == null || !Boolean.FALSE.equals(m_where.getValue()))
            {
               Instance instance = factory.instantiate(tobj);

               identityMap.clear();

               if (isJoin())
               {
                  join(instance, getAdapter().getOID(instance, m_childItem));
               }

               if (nOffset++ >= m_nOffset)
               {
                  instanceList.add(instance);
               }
            }
         }

         if (bCached && !bMissed && factory.isLockMismatch())
         {
            bCached = false;

            continue;
         }

         if (getOrderByCount() != 0 && instanceList.size() > 1)
         {
            instanceList.sort(this);
         }

         m_instance = null;

         return instanceList;
      }
   }

   /**
    * Reads the class instances specified by the query.
    * @return The class instance list.
    */
   public InstanceList read()
   {
      assert isRoot();

      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("Read: " + SysUtil.LINE_SEP + this);
      }

      int nLimit = getLimit();

      if (m_nMaxCount > nLimit && nLimit > 0)
      {
         throw new DataVolumeException("err.persistence.maxCount",
            new Object[]{Primitive.createInteger(m_nMaxCount), m_metaclass.getName(),
               Primitive.createInteger(nLimit)});
      }

      m_bCursor = false;
      prepare(m_nMaxCount >= 0);

      return m_adapter.read(this);
   }

   /**
    * Opens a cursor on the instances specified by the query.
    * @return The cursor.
    */
   public Cursor openCursor()
   {
      return openCursor(true);
   }

   /**
    * Opens a cursor on the instances specified by the query.
    * @param bComplete True if incompletely retrieved instances are prohibited.
    * @return The cursor.
    */
   public Cursor openCursor(boolean bComplete)
   {
      assert isRoot();

      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("OpenCursor: " + SysUtil.LINE_SEP + this);
      }

      m_bCursor = true;
      prepare(bComplete);

      return m_adapter.openCursor(this);
   }

   /**
    * @see nexj.core.persistence.Source#getValue()
    */
   public Object getValue()
   {
      return m_instance;
   }

   /**
    * Gets the source value from the evaluated instance.
    * @param source The source.
    * @return The source value, or Undefined.VALUE if not available.
    */
   protected Object getValue(Source source)
   {
      if (!isRoot())
      {
         Object value = m_parent.getValue(this);

         if (value == null)
         {
            return value;
         }

         if (value instanceof PropertyMap)
         {
            return getAdapter().getValue((PropertyMap)value, source);
         }

         if (value instanceof OIDHolder)
         {
            return getAdapter().getValue(((OIDHolder)value).getOID(), source);
         }

         return Undefined.VALUE;
      }

      if (source == this)
      {
         return getValue();
      }

      if (m_instance == null)
      {
         return null;
      }

      return m_adapter.getValue(m_instance, source);
   }

   /**
    * Associates a parent instance to a source key OID for a heterogeneous query.
    * @param instance The parent instance.
    * @param oid The source key OID.
    */
   public void addParentInstance(Instance instance, OID oid)
   {
      assert isRoot() && m_parent != null;
      assert instance != null;
      assert oid != null;

      Set instanceSet;

      if (m_parentInstanceMap == null)
      {
         m_parentInstanceMap = new HashTab();
         instanceSet = null;
      }
      else
      {
         instanceSet = (Set)m_parentInstanceMap.get(oid);
      }

      if (instanceSet == null)
      {
         instanceSet = new HashHolder(2);
         m_parentInstanceMap.put(oid, instanceSet);
      }

      instanceSet.add(instance);
   }

   /**
    * Gets parent instances corresponding with the given source key OID.
    * @param oid The source key OID.
    * @return The parent instance set.
    */
   public Set getParentInstances(OID oid)
   {
      return (m_parentInstanceMap == null) ? null : (Set)m_parentInstanceMap.get(oid);
   }

   /**
    * Clears the parent instance map.
    */
   public void clearParentInstances()
   {
      if (m_parentInstanceMap != null)
      {
         m_parentInstanceMap.clear();
      }
   }

   /**
    * Joins a child instance with a specified destination key OID
    * to a matching parent instance in a heterogeneous query.
    * @param instance The child instance.
    * @param oid The destination key OID.
    */
   public void join(Instance instance, OID oid)
   {
      assert isJoin();
      assert instance != null;

      if (oid != null && m_parentInstanceMap != null)
      {
         Set instanceSet = (Set)m_parentInstanceMap.get(oid);

         if (instanceSet != null)
         {
            Attribute reverse = m_attribute.getReverse();

            if (reverse != null && reverse.isCollection())
            {
               reverse = null;
            }

            for (Iterator itr = instanceSet.iterator(); itr.hasNext();)
            {
               Instance container = (Instance)itr.next();

               if (m_attribute.isCollection())
               {
                  Object value = container.getOldValueDirect(m_attribute.getOrdinal());
                  InstanceList list;

                  if (value instanceof Undefined)
                  {
                     if (value == Undefined.VALUE)
                     {
                        list = new InstanceArrayList();
                        list.setAssociation(container, m_attribute, true);
                        container.setOldValueDirect(m_attribute.getOrdinal(), list);
                     }
                     else
                     {
                        list = null;
                     }
                  }
                  else
                  {
                     list = (InstanceList)value;
                  }

                  if (list != null)
                  {
                     list.setLazy(false);
                     list.add(instance, InstanceList.DIRECT | InstanceList.REPLACE);
                  }
               }
               else
               {
                  container.setOldValueDirect(m_attribute.getOrdinal(), instance);
               }

               if (reverse != null && !instance.isLazy() && instance.getOldValueDirect(reverse.getOrdinal()) == Undefined.VALUE)
               {
                  instance.setOldValueDirect(reverse.getOrdinal(), container);
               }
            }
         }
      }
   }

   /**
    * Joins the class instances specified in the query to the parent instances.
    */
   private void join()
   {
      assert isJoin();

      if (m_parentInstanceMap != null && m_parentInstanceMap.size() != 0 && !isLazy())
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Joining " + m_parent.getMetaclass().getName() + '.' +
               m_attribute.getName() + '(' + m_metaclass.getName() + ')' +
               ((isCached()) ? " from cache" : ""));
         }

         Set oidSet = new HashHolder(m_parentInstanceMap.size());

         for (Iterator itr = m_parentInstanceMap.iterator(); itr.hasNext();)
         {
            oidSet.add(itr.next());
         }

         AttributeOperator aop = new AttributeOperator(new Field(this, m_mapping, m_childItem, m_metaclass, null, null, false));

         if (isCached() && m_persistenceMapping.getCaching() == PersistenceMapping.CACHING_INSTANCE)
         {
            EqualsOperator eq = new EqualsOperator();

            eq.setLeft(aop);
            eq.setRight(new ConstantOperator(null));

            for (Iterator itr = oidSet.iterator(); itr.hasNext();)
            {
               EqualsOperator clone = (oidSet.size() == 1) ? eq : (EqualsOperator)eq.clone();

               clone.getRight().setValue(itr.next());
               read(clone);
            }
         }
         else
         {
            InOperator in = new InOperator();

            in.addOperand(aop);

            for (Iterator itr = oidSet.iterator(); itr.hasNext();)
            {
               in.addOperand(new ConstantOperator(itr.next()));
            }

            read(in);
         }

         completeParentInstances();
         clearParentInstances();
      }
   }

   /**
    * Completes the parent instances for which this child Query returned no results.
    */
   public void completeParentInstances()
   {
      assert isJoin();

      if (m_parentInstanceMap != null && m_parentInstanceMap.size() != 0)
      {
         for (Iterator setItr = m_parentInstanceMap.valueIterator(); setItr.hasNext();)
         {
            for (Iterator itr = ((Set)setItr.next()).iterator(); itr.hasNext();)
            {
               Instance container = (Instance)itr.next();

               if (m_attribute.isCollection())
               {
                  Object value = container.getOldValueDirect(m_attribute.getOrdinal());
                  InstanceList list;

                  if (value instanceof Undefined)
                  {
                     if (value == Undefined.VALUE)
                     {
                        list = new InstanceArrayList();
                        list.setAssociation(container, m_attribute, true);
                        container.setOldValueDirect(m_attribute.getOrdinal(), list);
                     }
                     else
                     {
                        list = null;
                     }
                  }
                  else
                  {
                     list = (InstanceList)value;
                  }

                  if (list != null)
                  {
                     list.setLazy(false);
                  }
               }
               else if (container.getOldValueDirect(m_attribute.getOrdinal()) == Undefined.VALUE)
               {
                  container.setOldValueDirect(m_attribute.getOrdinal(), null);
               }
            }
         }
      }
   }

   /**
    * Reads an instance list with an additional conjunction.
    * @param conj The additional conjunction.
    * @return The read instance list.
    */
   private InstanceList read(Operator conj)
   {
      byte nUniquenessSaved = m_nUnique;
      AndOperator and;
      int nOperandCountSaved;

      if (m_where == null)
      {
         and = null;
         nOperandCountSaved = 0;
         m_where = conj;
      }
      else
      {
         and = (AndOperator)m_where; // and was setup during normalization
         nOperandCountSaved = and.getOperandCount();
         and.addOperand(conj);
      }

      if (m_nUnique == 0)
      {
         m_nUnique = -1;
      }

      try
      {
         conj = conj.normalize(Operator.NORMALIZE_WHERE | Operator.NORMALIZE_PERSISTENCE);

         if (and == null)
         {
            m_where = conj;
         }
         else
         {
            and.setOperand(nOperandCountSaved, conj);
         }

         return read();
      }
      finally
      {
         // remove the join filter
         if (and == null)
         {
            m_where = null;
         }
         else
         {
            while (and.getOperandCount() > nOperandCountSaved)
            {
               and.removeOperand(and.getOperandCount() - 1);
            }
         }

         m_nUnique = nUniquenessSaved;
      }
   }

   /**
    * Completes the query execution plan.
    */
   public void complete()
   {
      if (m_parent == null)
      {
         for (int i = 1, n = m_rootList.size(); i < n; ++i)
         {
            ((Query)m_rootList.get(i)).join();
         }
      }
   }

   /**
    * Compares two property maps according to the order by clause.
    * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
    */
   public int compare(Object left, Object right)
   {
      int nResult = 0;

      for (int i = 0, n = getOrderByCount(); i < n; ++i)
      {
         Operator op = getOrderByOperator(i);

         m_instance = (PropertyMap)left;

         Object leftValue = op.getValue();

         m_instance = (PropertyMap)right;

         nResult = Primitive.COMPARATOR.compare(leftValue, op.getValue());

         if (nResult != 0)
         {
            if (!isOrderByAscending(i))
            {
               nResult = -nResult;
            }

            break;
         }
      }

      return nResult;
   }

   /**
    * Visits the query tree starting with this node in the specified order.
    * The root node is always visited.
    * @param visitor The tree visitor.
    * @param nFlags Combination of the VISIT_* constant bits.
    */
   public boolean visit(Visitor visitor, int nFlags)
   {
      return visit(visitor, nFlags, nFlags);
   }

   /**
    * Visits the query tree in the specified order, starting with this node.
    * The root node is always visited.
    * @param visitor The tree visitor.
    * @param nFlags Combination of the VISIT_* constant bits.
    * @param nChildFlags Flags for child node iteration.
    */
   public boolean visit(Visitor visitor, int nFlags, int nChildFlags)
   {
      if (!visitor.visit(this))
      {
         return false;
      }

      if ((nFlags & VISIT_QUERY) != 0 && m_queryArray != null)
      {
         for (int i = 0; i < m_queryArray.length; ++i)
         {
            Query assoc = m_queryArray[i];

            if (visitor.isEligible(assoc) && !assoc.visit(visitor, nChildFlags, nChildFlags))
            {
               return false;
            }
         }
      }

      if ((nFlags & VISIT_WHERE) != 0 && m_whereArray != null)
      {
         for (int i = 0; i < m_whereArray.length; ++i)
         {
            Query assoc = m_whereArray[i];

            if (visitor.isEligible(assoc) && !assoc.visit(visitor, nChildFlags, nChildFlags))
            {
               return false;
            }
         }
      }

      if ((nFlags & VISIT_QUANTOR) != 0 && m_quantorMap != null)
      {
         for (Iterator itr = m_quantorMap.valueIterator(); itr.hasNext();)
         {
            Query[] queryArray = (Query[])itr.next();

            for (int i = 0; i < queryArray.length; ++i)
            {
               Query assoc = queryArray[i];

               if (visitor.isEligible(assoc) && !assoc.visit(visitor, nChildFlags, nChildFlags))
               {
                  return false;
               }
            }
         }
      }

      return visitor.postVisit(this);
   }

   /**
    * @see java.lang.Comparable#compareTo(java.lang.Object)
    */
   public int compareTo(Object obj)
   {
      if (!(obj instanceof Query))
      {
         return getClass().getName().compareTo(obj.getClass().getName());
      }

      Query query = (Query)obj;

      int n = m_metaclass.compareTo(query.m_metaclass);

      if (n != 0)
      {
         return n;
      }

      if (m_attribute != null && query.m_attribute != null)
      {
         n = m_attribute.compareTo(query.m_attribute);

         if (n != 0)
         {
            return n;
         }
      }

      return ((m_bInverse) ? 1 : 0) - ((query.m_bInverse) ? 1 : 0);
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      Query querySaved;

      if (writer.getWriter() instanceof QueryHolder)
      {
         QueryHolder holder = (QueryHolder)writer.getWriter();

         querySaved = holder.getQuery();
         holder.setQuery(this);
      }
      else
      {
         querySaved = null;
      }

      writer.write("Query(");

      if (m_bInverse)
      {
         writer.write("inverse, ");
      }

      if (m_attribute != null)
      {
         writer.write("attribute=");
         writer.write(m_attribute.getName());
         writer.write(", ");
      }

      if (m_filter != null)
      {
         writer.write("filter=");
         writer.print(m_filter);
         writer.write(", ");
      }

      if (m_metaclass != null)
      {
         writer.write("class=");
         writer.write(m_metaclass.getName());
      }

      if (m_fieldMap.size() != 0)
      {
         writer.write(", fields={");

         boolean bFirst = true;

         for (Lookup.Iterator itr = m_fieldMap.iterator(); itr.hasNext(); bFirst = false)
         {
            if (!bFirst)
            {
               writer.write(", ");
            }

            Object key = itr.next();

            if (key instanceof Named)
            {
               writer.write(((Named)key).getName());
            }
            else
            {
               writer.print(key);
            }

            writer.write('=');
            writer.print(itr.getValue());
         }

         writer.write('}');
      }

      if (m_where != null)
      {
         writer.write(", where=");
         writer.print(m_where);
      }

      if (m_nGroupByCount != 0)
      {
         writer.write(", groupBy=(");

         for (int i = 0; i < m_nGroupByCount; ++i)
         {
            if (i != 0)
            {
               writer.write(' ');
            }

            writer.print(m_groupByArray[i]);
         }

         writer.write(')');
      }

      if (m_having != null)
      {
         writer.write(", having=");
         writer.print(m_having);
      }

      if (m_nOrderByCount != 0)
      {
         writer.write(", orderBy=(");

         for (int i = 0; i < m_nOrderByCount; ++i)
         {
            if (i != 0)
            {
               writer.write(' ');
            }

            writer.write('(');
            writer.print(m_orderByArray[i << 1]);
            writer.print(m_orderByArray[(i << 1) + 1] != null);
            writer.write(')');
         }

         writer.write(')');
      }

      if (m_nMaxCount != -1)
      {
         writer.write(", maxCount=");
         writer.print(m_nMaxCount);
      }

      if (m_nOffset != 0)
      {
         writer.write(", offset=");
         writer.print(m_nOffset);
      }

      if (m_bLocking)
      {
         writer.write(", xlock=");
         writer.write(String.valueOf(m_bLocking));
      }

      if (m_nOutput != OUTPUT_NONE)
      {
         writer.write(", output=");
         writer.write((m_nOutput == OUTPUT_LAZY) ? "lazy" : "eager");
      }

      if (m_mapping != null)
      {
         writer.write(", mapping=");
         writer.print(m_mapping);
      }

      if (m_generator != null)
      {
         writer.write(", generator=");
         writer.print(m_generator);
      }

      writer.write(')');

      if (m_queryArray != null)
      {
         writer.indent();
         writer.write("query[]:");

         for (int i = 0; i < m_queryArray.length; ++i)
         {
            writer.addIndent(1);
            writer.indent();
            writer.print(m_queryArray[i]);
            writer.addIndent(-1);
         }
      }

      if (m_whereArray != null)
      {
         writer.indent();
         writer.write("where[]:");

         for (int i = 0; i < m_whereArray.length; ++i)
         {
            writer.addIndent(1);
            writer.indent();
            writer.print(m_whereArray[i]);
            writer.addIndent(-1);
         }
      }

      if (m_quantorMap != null)
      {
         writer.indent();
         writer.write("quantor[]:");

         for (Lookup.Iterator itr = m_quantorMap.iterator(); itr.hasNext();)
         {
            itr.next();
            writer.addIndent(1);
            writer.indent();
            writer.print(itr.getKey());
            writer.write(':');
            writer.indent();
            writer.print(itr.getValue());
            writer.addIndent(-1);
         }
      }

      if (writer.getWriter() instanceof QueryHolder)
      {
         ((QueryHolder)writer.getWriter()).setQuery(querySaved);
      }
   }

   /**
    * Converts an object to a string representation in the context of this query.
    * @param obj The object to convert. Can be null.
    */
   protected String toString(Printable obj)
   {
      QueryStringWriter writer = new QueryStringWriter(128);

      try
      {
         writer.setQuery(this);
         new PrintWriter(writer).print(obj);

         return writer.toString();
      }
      catch (IOException e)
      {
         throw ObjUtil.rethrow(e);
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      QueryStringWriter writer = new QueryStringWriter(128);

      try
      {
         new PrintWriter(writer).print(this);

         return writer.toString();
      }
      catch (IOException e)
      {
         return "#<I/O Error>";
      }
   }

   /**
    * Creates a read query object.
    * @param metaclass The primary class in the query.
    * @param attributes The attribute symbol list: (a1 a2 (a3 a3.1 ... a3.N) ... aN).
    * @param where The where clause as Scheme expression. Can be null to read everything.
    * @param orderBy The list of order by/asc flag expressions: ((e1 . #t) (e2 . #t) ... (eN . #t)).
    * @param nMaxCount The maximum number of primary class instances to retrieve.
    * @param nOffset The number of instances to skip.
    * @param bLock True to lock exclusively the retrieved primary class instances during the transaction.
    * @param nSecurity The security clause mode, one of the SEC_* constants.
    * @param context The invocation context.
    * @return The prepared query object.
    */
   public static Query createRead(Metaclass metaclass, Pair attributes, Object where, Pair orderBy,
      int nMaxCount, int nOffset, boolean bLock, byte nSecurity, InvocationContext context)
   {
      assert metaclass != null;

      Query root = new Query(metaclass, context);

      root.setSecurity(nSecurity);
      root.setWhere(where);
      root.setOutput(OUTPUT_EAGER);
      root.addDependency(ASSOC_QUERY, metaclass, metaclass, attributes, OUTPUT_EAGER);
      root.addOrderBy(orderBy);
      root.setMaxCount(nMaxCount);
      root.setOffset(nOffset);
      root.setLocking(bLock);
      root.setTimeout(context.getQueryTimeout());

      return root;
   }

   /**
    * Creates an aggregate query object.
    * @param metaclass The primary class in the query.
    * @param attributes The attribute symbol list: (a1 a2 (a3 a3.1 ... a3.N) ... aN).
    * @param where The where clause as Scheme expression. Can be null to aggregate everything.
    * @param groupBy The list of group by expressions: (e1 e2 ... eN).
    * @param having The having clause as Scheme expression. Can be null to read everything.
    * @param orderBy The list of order by/asc flag expressions: ((e1 . #t) (e2 . #t) ... (eN . #t)).
    * @param nMaxCount The maximum number of primary class instances to retrieve.
    * @param nOffset The number of instances to skip.
    * @param nSecurity The security clause mode, one of the SEC_* constants.
    * @param context The invocation context.
    * @return The prepared query object.
    */
   public static Query createAggregate(Metaclass metaclass, Pair attributes, Object where, Pair groupBy,
      Object having, Pair orderBy, int nMaxCount, int nOffset, byte nSecurity, InvocationContext context)
   {
      assert metaclass != null;

      Query root = new Query(metaclass, context);

      root.setAggregate(true);
      root.setSecurity(nSecurity);
      root.setWhere(where);
      root.addGroupBy(groupBy);
      root.setOutput(OUTPUT_EAGER);
      root.addDependency(ASSOC_QUERY, metaclass, metaclass, attributes, OUTPUT_EAGER);
      root.setHaving(having);
      root.addOrderBy(orderBy);
      root.setMaxCount(nMaxCount);
      root.setOffset(nOffset);
      root.setTimeout(context.getQueryTimeout());

      return root;
   }

   // inner classes

   /**
    * Interface for creating an operator from an S-expression.
    */
   protected interface OperatorFactory
   {
      /**
       * Creates an operator from an S-expression.
       * @param args The argument chain.
       * @param query The query to which to bind the operator.
       * @param key The association map key.
       * @param nOutput The attribute output mode, one of the OUTPUT_* constants.
       * @return The created operator, or null if not supported.
       */
      Operator create(Pair args, Query query, Object key, byte nOutput);
   }

   /**
    * Base class for creating a unary operator.
    */
   protected abstract static class UnaryOperatorFactory implements OperatorFactory
   {
      /**
       * @see nexj.core.persistence.Query.OperatorFactory#create(nexj.core.scripting.Pair, nexj.core.persistence.Query, Object, byte)
       */
      public Operator create(Pair args, Query query, Object key, byte nOutput)
      {
         UnaryOperator op = create(query);

         if (args == null || args.getTail() != null)
         {
            throw new InvalidQueryException("err.persistence.unaryOperatorArgCount",
               new Object[]{op.getSymbol()});
         }

         if (!op.setOperand(query.createOperator(key, args.getHead(), nOutput)))
         {
            return null;
         }

         return op;
      }

      /**
       * Creates the unary operator.
       * @param query The query which to bind to the operator.
       * @return The created unary operator.
       */
      public abstract UnaryOperator create(Query query);
   }

   /**
    * Base class for creating a binary operator.
    */
   protected abstract static class BinaryOperatorFactory implements OperatorFactory
   {
      /**
       * @see nexj.core.persistence.Query.OperatorFactory#create(nexj.core.scripting.Pair, nexj.core.persistence.Query, Object, byte)
       */
      public Operator create(Pair args, Query query, Object key, byte nOutput)
      {
         if (args == null)
         {
            throw new InvalidQueryException("err.persistence.binaryOperatorArgCount",
               new Object[]{create(query).getSymbol()});
         }

         if (args.getTail() == null)
         {
            return create(args.getHead(), query, key, nOutput);
         }

         BinaryOperator op = create(query);

         if (!op.setLeft(query.createOperator(key, args.getHead(), nOutput)))
         {
            return null;
         }

         args = (Pair)args.getTail();

         for (;;)
         {
            if (!op.setRight(query.createOperator(key, args.getHead(), nOutput)))
            {
               return null;
            }

            if (args.getTail() == null)
            {
               break;
            }

            BinaryOperator right = create(query);

            right.setLeft(op);
            op = right;
            args = args.getNext();
         }

         return op;
      }

      /**
       * Creates a unary operator from the same symbol, if only one argument was passed.
       * @param arg The argument for the unary operator.
       * @param query The query which to bind to the operator.
       * @param key The association map key.
       * @param nOutput The attribute output mode, one of the OUTPUT_* constants.
       * @return The created unary operator.
       */
      public UnaryOperator create(Object arg, Query query, Object key, byte nOutput)
      {
         throw new InvalidQueryException("err.persistence.binaryOperatorArgCount",
            new Object[]{create(query).getSymbol()});
      }

      /**
       * Creates the binary operator.
       * @param query The query which to bind to the operator.
       * @return The created binary operator.
       */
      public abstract BinaryOperator create(Query query);
   }

   /**
    * Base class for creating a comparison operator.
    */
   protected abstract static class ComparisonOperatorFactory implements OperatorFactory
   {
      /**
       * @see nexj.core.persistence.Query.OperatorFactory#create(nexj.core.scripting.Pair, nexj.core.persistence.Query, Object, byte)
       */
      public Operator create(Pair args, Query query, Object key, byte nOutput)
      {
         ComparisonOperator op = create(query);

         if (args == null || args.getTail() == null)
         {
            throw new InvalidQueryException("err.persistence.comparisonOperatorArgCount",
               new Object[]{op.getSymbol()});
         }

         if (!op.setLeft(query.createOperator(key, args.getHead(), nOutput)))
         {
            return null;
         }

         args = args.getNext();

         MultiArgOperator logical = null;

         for (;;)
         {
            if (!op.setRight(query.createOperator(key, args.getHead(), nOutput)))
            {
               return null;
            }

            op.initialize();

            if (args.getTail() == null)
            {
               break;
            }

            if (logical == null)
            {
               logical = createLogical(query);
               logical.addOperand(op);
            }

            ComparisonOperator right = create(query);

            if (!right.setLeft(query.createOperator(key, args.getHead(), nOutput)))
            {
               return null;
            }

            logical.addOperand(right);
            op = right;
            args = args.getNext();
         }

         if (logical != null)
         {
            return logical;
         }

         return op;
      }

      /**
       * Creates the comparison operator.
       * @param query The query which to bind to the operator.
       * @return The created comparison operator.
       */
      public abstract ComparisonOperator create(Query query);

      /**
       * Creates a logical operator for combining the comparisons.
       * @param query The query which to bind to the operator.
       * @return The logical operator.
       */
      public MultiArgOperator createLogical(Query query)
      {
         return new AndOperator();
      }
   }

   /**
    * Base class for creating a multi-arg operator.
    */
   protected abstract static class MultiArgOperatorFactory implements OperatorFactory
   {
      /**
       * @see nexj.core.persistence.Query.OperatorFactory#create(nexj.core.scripting.Pair, nexj.core.persistence.Query, Object, byte)
       */
      public Operator create(Pair args, Query query, Object key, byte nOutput)
      {
         MultiArgOperator op = create(query);

         for (; args != null; args = args.getNext())
         {
            if (op.addOperand(query.createOperator(key, args.getHead(), nOutput)) < 0)
            {
               return null;
            }
         }

         return op;
      }

      /**
       * Creates the multi-arg operator.
       * @param query The query which to bind to the operator.
       * @return The created multi-arg operator.
       */
      public abstract MultiArgOperator create(Query query);
   }

   /**
    * Factory for creating intrinsic function operators.
    */
   protected static class IntrinsicFunctionOperatorFactory implements OperatorFactory
   {
      // associations

      /**
       * The intrinsic function for evaluating the operator.
       */
      protected IntrinsicFunction m_fun;

      /**
       * The result type.
       */
      protected Primitive m_type;

      /**
       * Array of argument types, indexed by argument ordinal number.
       */
      protected Primitive[] m_argTypeArray;

      // constructor

      /**
       * Constructs the factory.
       * @param fun The function implementation.
       * @param type The function return type.
       * @param argTypeArray The argument type array, indexed by the argument ordinal number.
       */
      public IntrinsicFunctionOperatorFactory(IntrinsicFunction fun, Primitive type, Primitive[] argTypeArray)
      {
         m_fun = fun;
         m_type = type;
         m_argTypeArray = argTypeArray;
      }

      // operations

      /**
       * @see nexj.core.persistence.Query.OperatorFactory#create(nexj.core.scripting.Pair, nexj.core.persistence.Query, java.lang.Object, byte)
       */
      public Operator create(Pair args, Query query, Object key, byte nOutput)
      {
         if (Pair.length(args) != m_argTypeArray.length)
         {
            return null;
         }

         FunctionOperator op = new IntrinsicFunctionOperator(m_fun, m_type, m_argTypeArray);

         for (; args != null; args = args.getNext())
         {
            if (op.addOperand(query.createOperator(key, args.getHead(), nOutput)) < 0)
            {
               return null;
            }
         }

         return op;
      }
   }

   /**
    * Factory for creating aggregate function operators.
    */
   protected abstract static class AggregateFunctionOperatorFactory implements OperatorFactory
   {
      // operations

      /**
       * @see nexj.core.persistence.Query.OperatorFactory#create(nexj.core.scripting.Pair, nexj.core.persistence.Query, java.lang.Object, byte)
       */
      public Operator create(Pair args, Query query, Object key, byte nOutput)
      {
         if (args == null || args.getTail() != null)
         {
            return null;
         }

         AggregateOperator op = create(query);

         if (!op.setOperand(query.createOperator(
            (op instanceof UniqueOperator || query.isGroupedBy() || query.isAggregate()) ? key : op, args.getHead(),
            (nOutput == OUTPUT_UNKNOWN) ? OUTPUT_UNKNOWN : OUTPUT_NONE)))
         {
            return null;
         }

         return op;
      }

      /**
       * Template factory method.
       * @param query The operator query node.
       * @return A new instance of the operator.
       */
      protected abstract AggregateOperator create(Query query);
   }

   /**
    * Query visitor interface.
    * @see Query#visit(Visitor, int, int)
    */
   public interface Visitor
   {
      /**
       * Visits a query node in pre-order (parent first).
       * @param query The query node to visit.
       * @return True if the iteration on the same level should continue.
       */
      boolean visit(Query query);

      /**
       * Visits a query node in post-order (parent last).
       * @param query The query node to visit.
       * @return True if the iteration on the same level should continue.
       */
      boolean postVisit(Query query);

      /**
       * Checks if a query node is eligible for visiting.
       * @param query The query node to check.
       * @return True if it is eligible for visiting.
       */
      boolean isEligible(Query query);
   }

   /**
    * Subtree visitor - visits the nodes with the same subtree root.
    */
   public abstract static class SubtreeVisitor implements Visitor
   {
      public boolean postVisit(Query query)
      {
         return true;
      }

      /**
       * @see nexj.core.persistence.Query.Visitor#isEligible(nexj.core.persistence.Query)
       */
      public boolean isEligible(Query query)
      {
         return query.isSameRoot(query.getParent());
      }
   }

   /**
    * StringWriter providing the query context to the Printable objects.
    */
   protected static class QueryStringWriter extends StringWriter implements QueryHolder
   {
      // associations

      /**
       * The query.
       */
      protected Query m_query;

      // constructors

      /**
       * Constructs the writer.
       * @param nCount Preallocated character count.
       */
      public QueryStringWriter(int nCount)
      {
         super(nCount);
      }

      // operations

      /**
       * @see nexj.core.persistence.QueryHolder#setQuery(nexj.core.persistence.Query)
       */
      public void setQuery(Query query)
      {
         m_query = query;
      }

      /**
       * @see nexj.core.persistence.QueryHolder#getQuery()
       */
      public Query getQuery()
      {
         return m_query;
      }
   }

   /**
    * Visitor for computing query dependencies.
    */
   protected static class QueryDepVisitor implements Operator.Visitor
   {
      protected int m_nCount;
      protected Query[] m_depArray;
      protected Query[] m_queryArray;

      public QueryDepVisitor(Query[] queryArray)
      {
         m_depArray = new Query[queryArray.length - 1];
         m_queryArray = queryArray;
      }

      public Query[] findDeps(Query query)
      {
         if (query.m_where != null)
         {
            m_nCount = 0;

            query.m_where.visit(this, Operator.VISIT_PREORDER);

            if (m_nCount != 0)
            {
               Query[] depArray = new Query[m_nCount];

               System.arraycopy(m_depArray, 0, depArray, 0, m_nCount);

               return depArray;
            }
         }

         return null;
      }

      public boolean visit(Operator op)
      {
         Source source = op.getSource();

         if (source != null)
         {
            source = source.getQuery().getSource();
         }

      loop:
         for (int i = 0; i < m_queryArray.length; ++i)
         {
            Query query = m_queryArray[i];

            if (query.getSource() == source)
            {
               for (int k = 0; k < m_nCount; ++k)
               {
                  if (m_depArray[k] == query)
                  {
                     break loop;
                  }
               }

               m_depArray[m_nCount++] = query;

               break;
            }
         }

         return true;
      }

      public boolean isEligible(Operator op)
      {
         return true;
      }
   }
}
