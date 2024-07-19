// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import java.io.IOException;

import nexj.core.persistence.Converter;
import nexj.core.persistence.Field;
import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.persistence.QueryHolder;
import nexj.core.persistence.Source;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.PrintWriter;
import nexj.core.util.Undefined;

/**
 * Attribute expression node.
 */
public final class AttributeOperator extends PrimitiveOperator
{
   // constants

   /**
    * The operator ordinal number.
    */
   public final static int ORDINAL = 1;

   /**
    * The operator priority.
    */
   public final static int PRIORITY = 9;

   /**
    * The operator symbol.
    */
   public final static Symbol SYMBOL = Symbol.AT;

   // attributes

   /**
    * The flag to skip conversion.
    */
   private boolean m_bNoConversion;

   // constructors

   /**
    * Creates an attribute operator.
    * @param The operator value source.
    */
   public AttributeOperator(Source source)
   {
      m_source = source;
      m_type = source.getType();
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
    * Sets the flag to skip conversion.
    * @param bNoConversion The flag to skip conversion to set.
    */
   public void setNoConversion(boolean bNoConversion)
   {
      m_bNoConversion = bNoConversion;
   }

   /**
    * @return The flag to skip conversion.
    */
   public boolean isNoConversion()
   {
      return m_bNoConversion;
   }

   /**
    * @see nexj.core.persistence.Operator#visit(nexj.core.persistence.Operator.Visitor, int)
    */
   public boolean visit(Visitor visitor, int nFlags)
   {
      return visitor.visit(this);
   }

   /**
    * @see nexj.core.persistence.Operator#normalize(int)
    */
   public Operator normalize(int nFlags)
   {
      if (!m_bConstant)
      {
         setSource(m_source.getSource());

         Object value = m_source.getConstrainedValue();

         if (value != Undefined.VALUE)
         {
            setConstantValue(value);
         }
      }

      return this;
   }

   /**
    * @see nexj.core.persistence.Operator#evaluate()
    */
   protected Object evaluate()
   {
      return m_source.getValue();
   }

   /**
    * @return The custom type converter.
    */
   public Converter getConverter()
   {
      if (!m_bNoConversion && m_type.isPrimitive())
      {
         return ((Field)m_source).getConverter();
      }

      return null;
   }

   /**
    * @see nexj.core.persistence.Operator#copy(nexj.core.persistence.Operator)
    */
   public void copy(Operator src)
   {
      super.copy(src);

      AttributeOperator op = (AttributeOperator)src;

      m_bNoConversion = op.m_bNoConversion;
   }

   /**
    * @see nexj.core.util.Printable#printOn(PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write("(@");

      if (m_source == null)
      {
         writer.write(" ?");
      }
      else
      {
         Query query = m_source.getQuery();

         if (query == null)
         {
            writer.write(" ?"); 
         }
         else
         {
            if (query.isInverse())
            {
               writer.write("@ ");
               writer.write(query.getMetaclass().getName());
            }

            printAssoc(query, writer);
         }

         if (m_source instanceof Field)
         {
            writer.write(' ');

            Field field = (Field)m_source;

            if (field.getAttribute() != null)
            {
              writer.write(field.getAttribute().getName());
            }
            else if (field.getItem() != null)
            {
               writer.write('<');
               writer.print(field.getItem());
               writer.write('>');
            }
            else
            {
               writer.write('?');
            }
         }
      }

      writer.write(')');
   }

   /**
    * PrintOn helper. Prints the query association path, starting from the root query.
    * @param query The query to print.
    * @param writer The destination writer.
    * @throws IOException if the writer encounters an error.
    */
   private static void printAssoc(Query query, PrintWriter writer) throws IOException
   {
      Query root = (writer.getWriter() instanceof QueryHolder) ?
         ((QueryHolder)writer.getWriter()).getQuery() : null;

      if (query != root)
      {
         Query parent = query.getParent();

         if (parent == null)
         {
            writer.write("@ ");
            writer.write(query.getMetaclass().getName());
         }
         else
         {
            if (parent != root && !query.isInverse())
            {
               printAssoc(parent, writer);
            }

            writer.write(' ');
            writer.write(query.getAttribute().getName());

            if (parent != root && query.isInverse() &&
               (root != null || parent.getParent() != root))
            {
               printAssoc(parent, writer);
            }
         }
      }
   }

   /**
    * Gets the where-clause association path for the attribute represented by query.
    * @param root The query root node.
    * @param query The query.
    * @param path The list to which the association shall be added.
    */
   private static Pair getWhereAssoc(Query root, Query query, Pair path)
   {
      if (query != root)
      {
         Query parent = query.getParent();

         if (parent == null)
         {
            path.setHead(Symbol.ATAT);
            path = new Pair(query.getMetaclass().getSymbol(), path);
         }
         else
         {
            if (parent != root && !query.isInverse())
            {
               path = getWhereAssoc(root, parent, path);
            }

            path = new Pair(query.getAttribute().getSymbol(), path);

            if (parent != root && query.isInverse() &&
               (root != null || parent.getParent() != root))
            {
               getWhereAssoc(root, parent, path);
            }
         }
      }

      return path;
   }

   /**
    * @see nexj.core.persistence.Operator#getExpression(nexj.core.persistence.Query)
    */
   public Object getExpression(Query root)
   {
      Pair path = new Pair(Symbol.AT);
      Query query = m_source.getQuery();

      if (query.isInverse())
      {
         path.setHead(Symbol.ATAT);
         path = new Pair(query.getMetaclass().getSymbol(), path);
      }

      path = getWhereAssoc(root, query, path);

      if (m_source instanceof Field)
      {
         Field field = (Field)m_source;

         if (field.getAttribute() != null)
         {
            path = new Pair(field.getAttribute().getSymbol(), path);
         }
         else if (field.getItem() instanceof Field)
         {
            field = (Field)field.getItem();

            if (field.getAttribute() != null)
            {
               path = new Pair(field.getAttribute().getSymbol(), path);
            }
         }
      }

      return Pair.nreverse(path);
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

      return m_source.compareTo(((AttributeOperator)obj).getSource());
   }
}
