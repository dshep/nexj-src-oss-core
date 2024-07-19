// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;

import java.util.ArrayList;
import java.util.List;

import nexj.core.util.HashTab2D;
import nexj.core.util.Lookup2D;

/**
 * Definition of a complex type.
 */
public class CompositeType extends Type
{
   // constants

   /**
    * Children aggregate in-order.
    */
   public final static byte AGGREGATION_SEQUENTIAL = 0;

   /**
    * Children aggregate in random order.
    */
   public final static byte AGGREGATION_RANDOM = 1;

   /**
    * One child must be chosen to the exclusion of all others.
    */
   public final static byte AGGREGATION_CHOICE = 2;

   // attributes

   /**
    * The child aggregation mode.
    */
   protected byte m_nAggregation;

   /**
    * True if this type is an abstract type.
    */
   protected boolean m_bAbstract;

   /**
    * True if this type may not be substituted by one of its sub-types.
    */
   protected boolean m_bBlocked;

   /**
    * True if this type allows content other than its children.
    */
   protected boolean m_bLax;

   // associations

   /**
    * The map of child name and namespace constant (Schema.NS_*) to child: Markup[String, Object].
    */
   protected Lookup2D m_childMap = new HashTab2D();

   /**
    * The ordered list of children: Markup[].
    */
   protected List m_childList = new ArrayList();

   /**
    * The type of the element's character content.
    */
   protected AtomicType m_valueType;

   // constructors

   /**
    * Constructs a new complex type definition.
    * @param sName The type name.
    */
   public CompositeType(String sName)
   {
      m_sName = sName;
   }

   // operations

   /**
    * Sets the child aggregation mode.
    * @param nAggregation One of the AGGREGATION_* constants.
    */
   public void setAggregation(byte nAggregation)
   {
      m_nAggregation = nAggregation;
   }

   /**
    * Gets the child aggregation mode.
    * @return One of the AGGREGATION_* constants.
    */
   public byte getAggregation()
   {
      return m_nAggregation;
   }

   /**
    * Sets the lax flag.
    * @param bLax True if this type allows content other than its children.
    */
   public void setLax(boolean bLax)
   {
      m_bLax = bLax;
   }

   /**
    * Gets the lax flag.
    * @return True if this type allows content other than its children.
    */
   public boolean isLax()
   {
      return m_bLax;
   }

   /**
    * Sets the type of the element's character content.
    * @param type The content type; null if only content is markup.
    */
   public void setValueType(AtomicType type)
   {
      m_valueType = type;
   }

   /**
    * Sets the type of the element's character content.
    * @return The content type; null if only content is markup.
    */
   public AtomicType getValueType()
   {
      return m_valueType;
   }

   /**
    * Sets the abstract type flag.
    * @param bAbastract True if this type is an abstract type.
    */
   public void setAbstract(boolean bAbastract)
   {
      m_bAbstract = bAbastract;
   }

   /**
    * Gets the abstract type flag.
    * @return True if this type is an abstract type.
    */
   public boolean isAbstract()
   {
      return m_bAbstract;
   }

   /**
    * Sets the blocked flag.
    * @param bBlocked True if this type may not be substituted by one of its sub-types.
    */
   public void setBlocked(boolean bBlocked)
   {
      m_bBlocked = bBlocked;
   }

   /**
    * Gets the blocked flag.
    * @return True if this type may not be substituted by one of its sub-types.
    */
   public boolean isBlocked()
   {
      return m_bBlocked;
   }

   /**
    * Sets the base type.
    * @param base The base type; null if none.
    */
   public void setBase(CompositeType base)
   {
      m_base = base;
   }

   /**
    * Adds a child to this type.
    * @param markup The child to add.
    */
   public void addChild(Markup markup)
   {
      if (m_childMap.put(markup.getName(), Schema.getTypeKey(markup.getItemType()), markup) != null)
      {
         throw new IllegalStateException("Redefinition of " + markup + " in " + this);
      }

      m_childList.add(markup);
   }

   /**
    * Gets the number of children.
    * @return The number of children.
    */
   public int getChildCount()
   {
      return m_childMap.size();
   }

   /**
    * Gets the child with the given ordinal number.
    * @param nOrdinal The child ordinal to get.
    * @return The child Markup definition.
    */
   public Markup getChild(int nOrdinal)
   {
      return (Markup)m_childList.get(nOrdinal);
   }

   /**
    * Gets the child with the given name and type.
    * @param sName The child name.
    * @param nType The type of the child to find; one of the SchemaItem.* constants.
    * @return The child Markup definition; null if not found.
    */
   public Markup getChild(String sName, byte nType)
   {
      return (Markup)m_childMap.get(sName, Schema.getTypeKey(nType));
   }

   /**
    * @see nexj.core.meta.integration.format.xml.schema.SchemaItem#getItemType()
    */
   public byte getItemType()
   {
      return COMPOSITE_TYPE;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "CompositeType(" + ((m_sName == null) ? "ANONYMOUS" : "\"" + m_sName + "\"") + ')';
   }
}
