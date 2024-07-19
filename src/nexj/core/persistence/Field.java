// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import nexj.core.meta.Attribute;
import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.scripting.Function;
import nexj.core.util.Undefined;

/**
 * Object binding a query, an attribute and/or an operator or a key and a persistence mapping.
 */
public class Field extends Source
{
   // attributes

   /**
    * The field output flag.
    */
   protected boolean m_bOutput;

   /**
    * True if the field is persistent.
    */
   protected boolean m_bPersistent;

   /**
    * The annotation name.
    */
   protected String m_sAnnotation;

   // associations

   /**
    * This is the field source query.
    */
   protected Query m_query;

   /**
    * The field type.
    */
   protected Type m_type;

   /**
    * The type converter.
    */
   protected Converter m_converter;

   /**
    * The field mapping bind.
    */
   protected Object m_bind;

   /**
    * This is the next field.
    */
   protected Field m_next;

   // constructors

   /**
    * Creates a field with a query and attribute.
    * @param query The source query.
    * @param attribute The source attribute
    */
   public Field(Query query, Attribute attribute)
   {
      assert query != null;
      assert attribute != null;

      m_query = query;
      m_attribute = attribute;
      m_type = attribute.getType();
      m_bPersistent = true;
   }

   /**
    * Creates a field with a query and attribute.
    * @param query The source query.
    * @param attribute The source attribute
    * @param bOutput The output flag.
    */
   public Field(Query query, Attribute attribute, boolean bOutput)
   {
      this(query, attribute);
      m_bOutput = bOutput;
   }

   /**
    * Creates a field with a query and an operator.
    * @param query The source query.
    * @param operator The source operator.
    */
   public Field(Query query, Operator operator)
   {
      assert query != null;
      assert operator != null;

      m_query = query;
      m_mapping = operator;
      m_bPersistent = (operator.findCommonSource(query) != null);
   }

   /**
    * Creates an alias of a given source.
    * @param source The source.
    */
   public Field(Source source)
   {
      assert source != null;

      m_query = source.getQuery();
      m_mapping = source;
   }

   /**
    * Creates an alias of a given field corresponding to a given attribute.
    * @param field The field.
    * @param attribute The attribute.
    */
   public Field(Field field, Attribute attribute)
   {
      this(field);
      setAttribute(attribute);
   }

   /**
    * Creates an alias of a given source corresponding to a given annotation.
    * @param source The source.
    * @param sAnnotation The annotation name.
    */
   public Field(Source source, String sAnnotation)
   {
      this(source);

      assert sAnnotation != null;

      m_sAnnotation = sAnnotation;
   }

   /**
    * Creates a constant value field corresponding to a given annotation.
    * @param query The source query.
    * @param sAnnotation The annotation name.
    * @param value The constant value.
    */
   public Field(Query query, String sAnnotation, Object value)
   {
      assert query != null && sAnnotation != null;

      m_query = query;
      m_sAnnotation = sAnnotation;
      m_item = value;
      m_bGroupedBy = true;
   }

   /**
    * Creates a function field corresponding to a given annotation.
    * @param query The source query.
    * @param sAnnotation The annotation name.
    * @param fun The function.
    */
   public Field(Query query, String sAnnotation, Function fun)
   {
      assert query != null && sAnnotation != null && fun != null;

      m_query = query;
      m_sAnnotation = sAnnotation;
      m_mapping = fun;
   }

   /**
    * Creates a field with a query and persistence mappings.
    * @param query The source query.
    * @param mapping The persistence mapping.
    * @param item The mapping item.
    * @param type The field type.
    * @param converter The type converter. Can be null.
    * @param bind The mapping bind.
    * @param bOutput The output flag.
    */
   public Field(Query query, Object mapping, Object item, Type type, Converter converter, Object bind, boolean bOutput)
   {
      assert query != null && mapping != null && item != null;

      m_query = query;
      m_mapping = mapping;
      m_item = item;
      m_type = type;
      m_converter = converter;
      m_bind = bind;
      m_bOutput = bOutput;
      m_bPersistent = true;
   }

   // operations

   /**
    * Sets the field source query.
    * @param query The field source query to set.
    */
   public void setQuery(Query query)
   {
      m_query = query;
   }

   /**
    * @return The field source query.
    */
   public Query getQuery()
   {
      return m_query;
   }

   /**
    * @see nexj.core.persistence.Source#findCommon(nexj.core.persistence.Source)
    */
   public Source findCommon(Source source)
   {
      Operator op = getOperator();

      if (op != null)
      {
         return op.findCommonSource(source);
      }

      return (m_query.isSameRoot(source.getQuery())) ? m_query.getRoot() : null;
   }

   /**
    * @see nexj.core.persistence.Source#getAdapter()
    */
   public PersistenceAdapter getAdapter()
   {
      return m_query.getAdapter();
   }

   /**
    * Sets the field source attribute.
    * @param attribute The field source attribute to set.
    */
   public void setAttribute(Attribute attribute)
   {
      super.setAttribute(attribute);

      if (attribute != null)
      {
         m_type = attribute.getType();
      }
   }

   /**
    * Sets the annotation name.
    * @param sAnnotation The annotation name to set.
    */
   public void setAnnotation(String sAnnotation)
   {
      m_sAnnotation = sAnnotation;
   }

   /**
    * @return The annotation name.
    */
   public String getAnnotation()
   {
      return m_sAnnotation;
   }

   /**
    * Sets the associated operator.
    * @param operator The associated operator to set.
    */
   public void setOperator(Operator operator)
   {
      m_mapping = operator;
   }

   /**
    * @return The associated operator.
    */
   public Operator getOperator()
   {
      if (m_mapping instanceof Operator)
      {
         return (Operator)m_mapping;
      }

      return null;
   }

   /**
    * Sets the field type.
    * @param type The field type to set.
    */
   public void setType(Type type)
   {
      m_type = type;
   }

   /**
    * @see nexj.core.persistence.Source#getType()
    */
   public Type getType()
   {
      return m_type;
   }

   /**
    * @see nexj.core.persistence.Source#setMapping(java.lang.Object)
    */
   public void setMapping(Object mapping)
   {
      m_mapping = mapping;
   }

   /**
    * @see nexj.core.persistence.Source#getMapping()
    */
   public Object getMapping()
   {
      return m_mapping;
   }

   /**
    * @see nexj.core.persistence.Source#setItem(java.lang.Object)
    */
   public void setItem(Object item)
   {
      m_item = item;
   }

   /**
    * @see nexj.core.persistence.Source#getItem()
    */
   public Object getItem()
   {
      return m_item;
   }

   /**
    * @see nexj.core.persistence.Source#setOrdinal(int)
    */
   public void setOrdinal(int nOrdinal)
   {
      m_nOrdinal = nOrdinal;
   }

   /**
    * @see nexj.core.persistence.Source#getOrdinal()
    */
   public int getOrdinal()
   {
      return m_nOrdinal;
   }

   /**
    * @see nexj.core.persistence.Source#output(boolean)
    */
   public void output(boolean bParent)
   {
      m_query.addOutputField(this, bParent);
   }

   /**
    * @see nexj.core.persistence.Source#setOutput(boolean)
    */
   public void setOutput(boolean bOutput)
   {
      m_bOutput = bOutput;
   }

   /**
    * @see nexj.core.persistence.Source#isOutput()
    */
   public boolean isOutput()
   {
      return m_bOutput;
   }

   /**
    * @see nexj.core.persistence.Source#getConstrainedValue()
    */
   public Object getConstrainedValue()
   {
      if (m_attribute == null)
      {
         return Undefined.VALUE;
      }

      return m_query.getConstrainedValue(this);
   }

   /**
    * @see nexj.core.persistence.Source#getValue()
    */
   public Object getValue()
   {
      return m_query.getAdapter().getQuery(this).getValue(this);
   }

   /**
    * @return True if the field is evaluated by the persistence engine.
    */
   public boolean isPersistent()
   {
      return m_bPersistent;
   }

   /**
    * Sets the type converter.
    * @param converter The type converter to set.
    */
   public void setConverter(Converter converter)
   {
      m_converter = converter;
   }

   /**
    * @return The type converter.
    */
   public Converter getConverter()
   {
      return m_converter;
   }

   /**
    * Sets the field mapping bind.
    * @param bind The field mapping bind to set.
    */
   public void setBind(Object bind)
   {
      m_bind = bind;
   }

   /**
    * @return The field mapping bind.
    */
   public Object getBind()
   {
      return m_bind;
   }

   /**
    * Sets the next field.
    * @param next The next field to set.
    */
   public void setNext(Field next)
   {
      m_next = next;
   }

   /**
    * @return The next field.
    */
   public Field getNext()
   {
      return m_next;
   }

   /**
    * Normalizes the field.
    * @param nFlags Combination of Operator.NORMALIZE_* flags.
    * @return False if the field has to be removed.
    */
   public boolean normalize(int nFlags)
   {
      Operator op = getOperator();

      if (op != null)
      {
         setOperator(op = op.normalize(nFlags));

         Source source = op.getSource();

         if (source == null || m_query.findCommon(source) == null)
         {
            return false;
         }

         if (m_type != null && m_type.isPrimitive())
         {
            Type opType = op.getType();

            if (opType != null && opType.isPrimitive() && m_type != opType)
            {
               m_converter = new PrimitiveConverter((Primitive)opType, (Primitive)m_type);
            }
         }
      }

      return true;
   }

   /**
    * @see java.lang.Comparable#compareTo(java.lang.Object)
    */
   public int compareTo(Object obj)
   {
      if (obj == this)
      {
         return 0;
      }

      if (!(obj instanceof Field))
      {
         return getClass().getName().compareTo(obj.getClass().getName());
      }

      Field field = (Field)obj;

      if (m_attribute != null)
      {
         if (field.getAttribute() != null)
         {
            return m_attribute.compareTo(field.getAttribute());
         }

         return -1;
      }

      if (field.getAttribute() != null)
      {
         return 1;
      }

      if (m_mapping instanceof Comparable)
      {
         if (field.getMapping() instanceof Comparable)
         {
            return ((Comparable)m_mapping).compareTo(field.getMapping());
         }

         return -1;
      }

      if (field.getMapping() instanceof Comparable)
      {
         return 1;
      }

      if (m_item instanceof Comparable)
      {
         if (field.getItem() instanceof Comparable)
         {
            return ((Comparable)m_item).compareTo(field.getItem());
         }

         return -1;
      }

      if (field.getItem() instanceof Comparable)
      {
         return 1;
      }

      return hashCode() - field.hashCode();
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (!(obj instanceof Source))
      {
         return false;
      }

      Source source = (Source)obj;

      return m_mapping == source.getMapping() &&
         m_item == source.getItem();
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return ((m_mapping != null) ? m_mapping.hashCode() : 0) ^
         ((m_item != null) ? m_item.hashCode() : 0);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(128);

      buf.append("Field(");

      if (m_attribute != null)
      {
         buf.append("attribute=");
         buf.append(m_attribute.getName());
         buf.append(", ");
      }

      if (m_sAnnotation != null)
      {
         buf.append("annotation=");
         buf.append(m_sAnnotation);
         buf.append(", ");
      }

      if (m_mapping != null)
      {
         buf.append("mapping=");
         buf.append(m_mapping);
         buf.append(", ");
      }

      if (m_item != null)
      {
         buf.append("item=");
         buf.append(m_item);
         buf.append(", ");
      }

      if (m_nOrdinal >= 0)
      {
         buf.append("ordinal=");
         buf.append(m_nOrdinal);
         buf.append(", ");
      }

      if (m_bOutput)
      {
         buf.append("output=true, ");
      }

      if (buf.charAt(buf.length() - 1) == ' ')
      {
         buf.setLength(buf.length() - 2);
      }

      buf.append(')');

      return buf.toString();
   }
}
