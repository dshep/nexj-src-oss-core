// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import nexj.core.meta.Attribute;
import nexj.core.meta.Type;
import nexj.core.meta.persistence.AttributeMapping;

/**
 * Attribute or association source in the query.
 */
public abstract class Source implements Comparable
{
   // attributes

   /**
    * The field ordinal number.
    */
   protected int m_nOrdinal = -1;

   /**
    * The grouped by flag.
    */
   protected boolean m_bGroupedBy;

   // associations

   /**
    * The source attribute.
    */
   protected Attribute m_attribute;

   /**
    * The source attribute persistence mapping.
    */
   protected AttributeMapping m_attributeMapping;

   /**
    * The source mapping.
    */
   protected Object m_mapping;

   /**
    * The source mapping item.
    */
   protected Object m_item;

   // operations

   /**
    * Sets the source attribute.
    * @param attribute The source attribute to set.
    */
   public void setAttribute(Attribute attribute)
   {
      m_attribute = attribute;
   }

   /**
    * @return The source attribute.
    */
   public Attribute getAttribute()
   {
      return m_attribute;
   }

   /**
    * Sets the source attribute persistence mapping.
    * @param mapping The mapping to set.
    */
   public void setAttributeMapping(AttributeMapping mapping)
   {
      m_attributeMapping = mapping;
   }

   /**
    * @return The source attribute persistence mapping. 
    */
   public AttributeMapping getAttributeMapping()
   {
      return m_attributeMapping;
   }

   /**
    * @return True if the inverse attribute mapping is used.
    */
   public boolean isInverse()
   {
      return false;
   }

   /**
    * @return True if the source is an alias of another source
    * for persistence mapping purposes.
    */
   public boolean isAlias()
   {
      return m_mapping instanceof Source;
   }

   /**
    * Makes this source an alias of another one.
    * @param source The other source.
    */
   public void setSource(Source source)
   {
      if (source != null)
      {
         source = source.getSource();
      }

      m_mapping = source;
   }

   /**
    * @return The actual source - either this source or
    * the source of which this is an alias.
    */
   public Source getSource()
   {
      Source source = this;

      while (source.m_mapping instanceof Source)
      {
         source = (Source)source.m_mapping;
      }

      return source;
   }

   /**
    * Sets the source mapping.
    * @param mapping The mapping to set.
    */
   public void setMapping(Object mapping)
   {
      getSource().m_mapping = mapping;
   }

   /**
    * @return The source mapping.
    */
   public Object getMapping()
   {
      return getSource().m_mapping;
   }

   /**
    * Sets the source mapping item.
    * @param item The mapping item.
    */
   public void setItem(Object item)
   {
      getSource().m_item = item;
   }

   /**
    * @return The source mapping item.
    */
   public Object getItem()
   {
      return getSource().m_item;
   }

   /**
    * Sets the source ordinal number.
    * @param nOrdinal The ordinal number to set.
    */
   public void setOrdinal(int nOrdinal)
   {
      m_nOrdinal = nOrdinal;
   }

   /**
    * @return The source ordinal number.
    */
   public int getOrdinal()
   {
      return m_nOrdinal;
   }

   /**
    * Sets the grouped by flag.
    * @param bGroupedBy The grouped by flag to set.
    */
   public void setGroupedBy(boolean bGroupedBy)
   {
      getSource().m_bGroupedBy = bGroupedBy;
   }

   /**
    * @return The grouped by flag.
    */
   public boolean isGroupedBy()
   {
      return getSource().m_bGroupedBy;
   }

   /**
    * @return The source query.
    */
   public abstract Query getQuery();

   /**
    * Computes the common source.
    * @param source The other source.
    * @return The common source, or null if none.
    */
   public abstract Source findCommon(Source source);

   /**
    * @return The source adapter. Can be null.
    */
   public abstract PersistenceAdapter getAdapter();

   /**
    * @return The value type.
    */
   public abstract Type getType();

   /**
    * Sets the query output flag and and the source to the relevant query. 
    * @param bParent True to output the parent queries as well. 
    */
   public abstract void output(boolean bParent);

   /**
    * Sets the query output flag.
    * @param bOutput The output flag
    */
   public abstract void setOutput(boolean bOutput);

   /**
    * @return True if the value is returned from the query.
    */
   public abstract boolean isOutput();

   /**
    * @return The constant value inferred from constraints or Undefined.VALUE if not constant.
    */
   public abstract Object getConstrainedValue();

   /**
    * @return The current value from the evaluated instance, or Undefined.VALUE if not available.
    */
   public abstract Object getValue();
}
