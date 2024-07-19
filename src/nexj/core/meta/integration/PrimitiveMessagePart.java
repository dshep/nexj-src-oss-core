// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import java.util.Iterator;

import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.Primitive;
import nexj.core.rpc.TransferObject;
import nexj.core.util.EmptyIterator;
import nexj.core.util.HashHolder;
import nexj.core.util.Holder;

/**
 * Message part representing a primitive value.
 */
public class PrimitiveMessagePart extends MessagePart
{
   // attributes

   /**
    * Whether using lax enumerations, i.e. enumeration values are not enforced.
    */
   protected boolean m_bLax;

   // associations

   /**
    * The part type.
    */
   protected Primitive m_type;

   /**
    * The enumeration set: [Object].
    */
   protected Holder m_enumerationSet;

   // constructors

   /**
    * Constructs the message part.
    * @param sName The part name.
    */
   public PrimitiveMessagePart(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the message part.
    */
   public PrimitiveMessagePart()
   {
      super();
   }

   // operations

   /**
    * Sets the part type.
    * @param type The part type to set.
    */
   public void setType(Primitive type)
   {
      verifyNotReadOnly();
      m_type = type;
   }

   /**
    * @return The part type.
    */
   public Primitive getType()
   {
      return m_type;
   }

   /**
    * Get whether using lax enumerations.
    * @return Whether using lax enumerations.
    */
   public boolean isLax()
   {
      return m_bLax;
   }

   /**
    * Set whether using lax enumerations.
    * @param bLaxEnumerations Whether using lax enumerations.
    */
   public void setLax(boolean bLax)
   {
      verifyNotReadOnly();
      m_bLax = bLax;
   }

   /**
    * Adds a new enumeration to the message part.
    * @param enumeration The enumeration to add.
    * @throws MetadataException if a enumeration
    * with the same name already exists.
    */
   public void addEnumeration(Object enumeration)
   {
      verifyNotReadOnly();

      if (m_enumerationSet == null)
      {
         m_enumerationSet = new HashHolder();
      }
      
      enumeration = m_type.convert(enumeration);

      if (!m_enumerationSet.add(enumeration))
      {
         throw new MetadataException("err.meta.integration.enumerationDup",
            new Object[]{enumeration, getFullPath()});
      }
   }

   /**
    * @return The enumeration count.
    */
   public int getEnumerationCount()
   {
      if (m_enumerationSet == null)
      {
         return 0;
      }

      return m_enumerationSet.size();
   }

   /**
    * @return An iterator for the contained enumeration objects.
    */
   public Iterator getEnumerationIterator()
   {
      if (m_enumerationSet == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_enumerationSet.iterator();
   }

   /**
    * Converts and validates a value.
    * @param value The value to convert and validate.
    * @return The converted and validated value.
    */
   public Object convertValue(Object value)
   {
      return validateValue(m_type.convert(value));
   }
   
   /**
    * Validates a value against the enumeration set.
    * @param value The value to validate.
    * @return The validated value.
    */
   public Object validateValue(Object value)
   {
      if (value != null && !m_bLax && m_enumerationSet != null && !m_enumerationSet.contains(value))
      {
         throw new MetadataException("err.meta.integration.enumerationValue",
            new Object[]{value, getFullPath()});
      }

      return value;
   }

   /**
    * @see nexj.core.meta.integration.MessagePart#resolveInheritance(nexj.core.meta.integration.MessagePart)
    */
   public void resolveInheritance(MessagePart baseMessagePart)
   {
      if (baseMessagePart instanceof PrimitiveMessagePart)
      {
         if (m_mapping == null)
         {
            if (baseMessagePart.m_mapping != null)
            {
               m_mapping = (MessagePartMapping)baseMessagePart.m_mapping.clone();
               m_mapping.init(this);
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.integration.MessagePart#clone()
    */
   public Object clone()
   {
      PrimitiveMessagePart copy = (PrimitiveMessagePart)super.clone();

      if (m_enumerationSet != null)
      {
         copy.m_enumerationSet = (Holder)m_enumerationSet.clone();
      }

      return copy;
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#getChild(java.lang.String)
    */
   public EndpointPart getChild(String sName)
   {
      throw new MetadataLookupException("err.meta.namedLookup",
         new Object[]{EndpointPart.class.getName(), sName});
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#findChild(java.lang.String)
    */
   public EndpointPart findChild(String sName)
   {
      return null;
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#getChildIterator()
    */
   public Iterator getChildIterator()
   {
      return EmptyIterator.getInstance();
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#isPrimitive()
    */
   public boolean isPrimitive()
   {
      return true;
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#createObject()
    */
   public TransferObject createObject()
   {
      throw new IllegalStateException(getName());
   }
}
