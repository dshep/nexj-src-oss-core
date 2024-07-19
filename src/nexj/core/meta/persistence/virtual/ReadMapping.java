// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.virtual;

import java.util.Iterator;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.Primitive;
import nexj.core.persistence.Field;
import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashHolderList;
import nexj.core.util.HashTab;
import nexj.core.util.HolderList;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.TextPosition;

/**
 * A virtual persistence adapter read mapping. Holds a collection of cases, one for
 * each where clause pattern.
 */
public class ReadMapping extends MetadataObject
{
   // constants

   /**
    * The "class" argument symbol for read and openCursor.
    */
   public final static Symbol CLASS = Symbol.define("class");

   /**
    * The "attributes" argument symbol for read and openCursor.
    */
   public final static Symbol ATTRIBUTES = Symbol.define("attributes");

   /**
    * The "where" argument symbol for read and openCursor.
    */
   public final static Symbol WHERE = Symbol.define("where");

   /**
    * The "orderBy" argument symbol for read and openCursor.
    */
   public final static Symbol ORDER_BY = Symbol.define("orderBy");

   /**
    * The "count" argument symbol for read and openCursor.
    */
   public final static Symbol COUNT = Symbol.define("count");

   /**
    * The "offset" argument symbol for read and openCursor.
    */
   public final static Symbol OFFSET = Symbol.define("offset");

   /**
    * The "xlock" argument symbol for read and openCursor.
    */
   public final static Symbol XLOCK = Symbol.define("xlock");

   /**
    * The "associations" argument symbol for read and openCursor.
    * Holds the subset of "attributes" that is being read homogeneously.
    */
   public final static Symbol ASSOCIATIONS = Symbol.define("associations");

   /**
    * Symbol for the expr-case macro for performing where clause matching.
    */
   protected final static Symbol EXPR_CASE = Symbol.define("expr-case");

   // associations

   /**
    * The read mapping cases.
    */
   protected HolderList m_readMappingSet = new HashHolderList();

   /**
    * The compiled read function.
    */
   protected Function m_function;

   // operations

   /**
    * Adds a read case to this mapping.
    * @param read The read case to add.
    */
   public void addCase(ReadMappingCase read)
   {
      verifyNotReadOnly();
      m_readMappingSet.add(read);
   }

   /**
    * Gets an iterator over the read mapping cases.
    * @return An iterator over the mapping cases.
    */
   public Iterator getCaseIterator()
   {
      return m_readMappingSet.iterator();
   }

   /**
    * Gets a list of the read case where clauses and code for use in an
    * expr-case block. The read mapping with the "else" clause is put
    * at the end.
    * @return The code for use in an expr-case block.
    */
   private Pair getReadMappings()
   {
      Pair elseMapping = new Pair(Pair.list(Symbol.ELSE, Pair.list(Symbol.ERROR, "fail.persistence.virtual.unmappedWhere", WHERE, CLASS)));
      Pair mappings = null;

      for (Iterator itr = m_readMappingSet.iterator(); itr.hasNext(); )
      {
         ReadMappingCase read = (ReadMappingCase)itr.next();
         Object whereExpr = read.getWhere();

         if (Symbol.ELSE.equals(whereExpr))
         {
            elseMapping.setHead(Pair.list(Symbol.ELSE, read.getScript()));
         }
         else
         {
            mappings = new Pair(Pair.list(whereExpr, read.getScript()), mappings);
         }
      }

      elseMapping.setTail(mappings);

      return Pair.nreverse(elseMapping);
   }

   /**
    * Gets the read function.
    * @return The read function.
    */
   public Function getFunction()
   {
      return m_function;
   }

   /**
    * Gets the arguments for the read function.
    * @param query The query.
    * @param fragment The fragment being queried.
    * @return The arguments to the read function.
    */
   public Object[] getFunctionArgs(Query query, VirtualDataSourceFragment fragment)
   {
      Object where = (query.getWhere() == null) ? null : query.getWhere().getExpression(query);

      return new Object[]
      {
         query.getMetaclass(),
         getAttributes(query),
         where,
         getOrderBy(query),
         Primitive.createInteger(query.getMaxCount()),
         Primitive.createInteger(query.getOffset()),
         Boolean.valueOf(query.isLocking()),
         getCompositeAttributeList(query),
         fragment.getPropertyHolder()
      };
   }

   /**
    * Logs the read function arguments.
    * @param argArray The argument array.
    * @param logger The logger.
    * @param nLevel The log level.
    */
   public void logFunctionArgs(Object[] argArray, Logger logger, int nLevel)
   {
      if (logger.isLevelEnabled(nLevel))
      {
         logger.log(nLevel, CLASS + ": " + argArray[0]);
         logger.log(nLevel, ATTRIBUTES + ": " + argArray[1]);
         logger.log(nLevel, WHERE + ": " + argArray[2]);
         logger.log(nLevel, ORDER_BY + ": " + argArray[3]);
         logger.log(nLevel, COUNT + ": " + argArray[4]);
         logger.log(nLevel, OFFSET + ": " + argArray[5]);
         logger.log(nLevel, XLOCK + ": " + argArray[6]);
         logger.log(nLevel, ASSOCIATIONS + ": " + argArray[7]);
         logger.log(nLevel, VirtualMapping.PROPERTIES + ": " + argArray[8]);
      }
   }

   /**
    * Compiles the mapping.
    * @param machine The virtual machine for compilation.
    * @param sURLPrefix The code URL prefix. For example class:Name.persistence.read
    * @param textPosMap The text position map.
    */
   public void compile(Machine machine, String sURLPrefix, Lookup textPosMap)
   {
      /*
       * (lambda (class attributes where orderBy count offset xlock associations properties)
       *    (expr-case where
       *       (<where1>
       *          (
       *             (lambda (var1 var2 ... varN)
       *                (cons
       *                   (
       *                      (lambda (readResult)
       *                         (if (instance? readResult PCodeFunction)
       *                            (generator->iterator readResult)
       *                            readResult
       *                         )
       *                      )
       *                      (
       *                         (lambda ()  ; New block to allow define statements
       *                            <case1.read>
       *                         )
       *                      )
       *                   )
       *                   (lambda ()
       *                      <case1.close>
       *                   )
       *                )
       *             )
       *             '(() () ... ())
       *          )
       *       )
       *       (<where2>
       *          etc.
       *       )
       *       (else
       *          etc.
       *       )
       *    )
       * )
       */
      Pair code = Pair.list(
         Symbol.LAMBDA,
         Pair.list(CLASS, ATTRIBUTES, WHERE, ORDER_BY, COUNT, OFFSET, XLOCK, ASSOCIATIONS, VirtualMapping.PROPERTIES),
         new Pair(
            EXPR_CASE,
            new Pair(
               WHERE,
               getReadMappings()
            )
         )
      );

      textPosMap.put(code, new TextPosition(0, 0, sURLPrefix));

      m_function = new Compiler().compile(code, textPosMap, machine, false);
   }

   /**
    * Resolves inheritance for this read mapping. Read mapping cases in this mapping
    * override base mapping cases with the same where clause pattern.
    * @param base The base class read mapping.
    */
   public void resolveInheritance(ReadMapping base)
   {
      if (base == null)
      {
         return;
      }

      for (Iterator itr = base.m_readMappingSet.iterator(); itr.hasNext(); )
      {
         Object baseReadMapping = itr.next();

         if (!m_readMappingSet.contains(baseReadMapping))
         {
            m_readMappingSet.add(baseReadMapping);
         }
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();

      for (Iterator itr = m_readMappingSet.iterator(); itr.hasNext(); )
      {
         ((ReadMappingCase)itr.next()).makeReadOnly();
      }
   }

   /**
    * Gets a list of the attributes being read homogeneously.
    * @param query The query.
    * @return A list of the attributes being read homogeneously.
    */
   public static Pair getCompositeAttributeList(Query query)
   {
      Pair compAttrs = null;

      assert query.isRoot();

      for (int nQuery = 1; nQuery < query.getOutputQueryCount(); nQuery++)
      {
         Query subQuery = query.getOutputQuery(nQuery);

         if (!subQuery.isRoot())
         {
            Object attr = subQuery.getAttribute().getSymbol();

            if ((subQuery = subQuery.getParent()) != query)
            {
               attr = new Pair(attr);

               do
               {
                  attr = new Pair(subQuery.getAttribute().getSymbol(), attr);
                  subQuery = subQuery.getParent();
               }
               while (subQuery != query);
            }

            compAttrs = new Pair(attr, compAttrs);
         }
      }

      return compAttrs;
   }

   /**
    * Gets the order by expression from a query.
    * @param query The query.
    * @return The order by expression.
    */
   public static Pair getOrderBy(Query query)
   {
      Pair orderBy = null;

      for (int i = query.getOrderByCount() - 1; i >= 0; i--)
      {
         Operator op = query.getOrderByOperator(i);
         boolean bAscending = query.isOrderByAscending(i);

         orderBy = new Pair(
            new Pair(op.getExpression(query), Boolean.valueOf(bAscending)),
            orderBy
         );
      }

      return orderBy;
   }

   /**
    * Gets the attributes being queried. Traverses the query tree to find both
    * primitive attributes as well as associations.
    * @param query The query.
    * @return The attributes being queried, in Object'read attribute list form.
    */
   public static Pair getAttributes(Query query)
   {
      Pair attributes = null;
      Lookup polymorphicAttrMap = null;

      // Add primitive output attributes
      for (Field field = query.getFirstOutputField(); field != null; field = field.getNext())
      {
         Attribute attribute = field.getAttribute();
         Metaclass metaclass = attribute.getMetaclass();

         if (metaclass.isUpcast(query.getMetaclass()))
         {
            attributes = new Pair(attribute.getSymbol(), attributes);
         }
         else
         {
            // Attribute defined in subclass, add to polymorphic attribute list
            assert query.getMetaclass().isUpcast(metaclass);

            if (polymorphicAttrMap == null)
            {
               polymorphicAttrMap = new HashTab(2);
            }

            Pair subclassAttrs = (Pair)polymorphicAttrMap.get(metaclass);

            subclassAttrs = new Pair(attribute.getSymbol(), subclassAttrs);
            polymorphicAttrMap.put(metaclass, subclassAttrs);
         }
      }

      // Add non-primitive output attributes
      for (Iterator itr = query.getAssocIterator(Query.ASSOC_QUERY); itr.hasNext(); )
      {
         Query subQuery = (Query)itr.next();
         Attribute attribute = subQuery.getAttribute();
         Pair subQueryAttrs = getAttributes(subQuery);
         Metaclass metaclass = attribute.getMetaclass();
         Object itemToAdd = (subQueryAttrs == null) ? (Object)attribute.getSymbol() : new Pair(attribute.getSymbol(), subQueryAttrs);

         if (metaclass.isUpcast(query.getMetaclass()))
         {
            attributes = new Pair(itemToAdd, attributes);
         }
         else
         {
            // Attribute defined in subclass, add to polymorphic attribute list
            assert query.getMetaclass().isUpcast(metaclass);

            if (polymorphicAttrMap == null)
            {
               polymorphicAttrMap = new HashTab(2);
            }

            Pair subclassAttrs = (Pair)polymorphicAttrMap.get(metaclass);

            subclassAttrs = new Pair(itemToAdd, subclassAttrs);
            polymorphicAttrMap.put(metaclass, subclassAttrs);
         }
      }

      // Merge the polymorphic attributes
      if (polymorphicAttrMap != null)
      {
         for (Lookup.Iterator itr = (Lookup.Iterator)polymorphicAttrMap.iterator(); itr.hasNext(); )
         {
            Metaclass subClass = (Metaclass)itr.next();
            Pair subclassAttrs = (Pair)itr.getValue();

            attributes = new Pair(
               new Pair(Symbol.ATAT, new Pair(subClass.getSymbol(), subclassAttrs)), 
               attributes
            );
         }
      }

      return attributes;
   }
}
