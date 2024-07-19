// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import nexj.core.meta.ContextMetadata;
import nexj.core.meta.DocumentedNamedMetadataObject;
import nexj.core.meta.MetadataObject;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.PropertyMap;

/**
 * Base class for message parts.
 */
public abstract class MessagePart extends DocumentedNamedMetadataObject implements EndpointPart
{
   // attributes
   
   /**
    * The minimum occurrence count of the message part.
    */
   protected int m_nMinCount;

   /**
    * The maximum occurrence count of the message part.
    * 0 for unlimited.
    */
   protected int m_nMaxCount;
   
   // associations

   /**
    * The path of the ancestors.
    */
   protected CompositeMessagePartInstance m_parent;

   /**
    * The mapping object.
    */
   protected MessagePartMapping m_mapping;

   /**
    * The message in which this part is declared.
    */
   protected Message m_declarator;

   // constructors
   
   /**
    * Constructs the message part.
    * @param sName The message part name.
    */
   public MessagePart(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the message part.
    */
   public MessagePart()
   {
      super();
   }
   
   // operations

   /**
    * Sets the minimum occurrence count of the message part.
    * @param nMinCount The minimum occurrence count of the message part to set.
    */
   public void setMinCount(int nMinCount)
   {
      verifyNotReadOnly();
      m_nMinCount = nMinCount;
   }

   /**
    * @return The minimum occurrence count of the message part.
    */
   public int getMinCount()
   {
      return m_nMinCount;
   }
   
   /**
    * Sets the maximum occurrence count of the message part.
    * @param nMaxCount The maximum occurrence count of the message part to set.
    * 0 for unlimited.
    */
   public void setMaxCount(int nMaxCount)
   {
      verifyNotReadOnly();
      m_nMaxCount = (nMaxCount == 0) ? Integer.MAX_VALUE : nMaxCount;
   }

   /**
    * @return The maximum occurrence count of the message part. 0 for unlimited.
    */
   public int getMaxCount()
   {
      return m_nMaxCount;
   }

   /**
    * @return True if the message part is required.
    */
   public boolean isRequired()
   {
      return m_nMinCount != 0;
   }
   
   /**
    * @return True if the message part can be encountered multiple times.
    */
   public boolean isCollection()
   {
      return m_nMaxCount != 1;
   }
   
   /**
    * Sets the containing part.
    * @param composite The containing part to set.
    */
   public void setParent(CompositeMessagePartInstance parent)
   {
      verifyNotReadOnly();
      m_parent = parent;
   }

   /**
    * Gets the containing part.
    * @return The containing part.
    */
   public CompositeMessagePartInstance getParent()
   {
      return m_parent;
   }

   /**
    * Sets the part declarator.
    * @param declarator The message in which this part was declared.
    */
   public void setDeclarator(Message declarator)
   {
      verifyNotReadOnly();
      m_declarator = declarator;
   }

   /**
    * Gets the part declarator.
    * @return The message in which this part was declared.
    */
   public Message getDeclarator()
   {
      return m_declarator;
   }

   /**
    * @return The root container.
    */
   public CompositeMessagePart getRoot()
   {
      CompositeMessagePart root;
      
      if (this instanceof CompositeMessagePart)
      {
         root = (CompositeMessagePart)this;
      }
      else
      {
         root = m_parent;
         
         if (root == null)
         {
            return null;
         }
      }
      
      while (root.getParent() != null)
      {
         root = root.getParent();
      }
      
      return root;
   }

   /**
    * Sets the mapping object.
    * @param mapping The mapping object to set.
    */
   public void setMapping(MessagePartMapping mapping)
   {
      verifyNotReadOnly();
      m_mapping = mapping;
   }

   /**
    * @return The mapping object.
    */
   public MessagePartMapping getMapping()
   {
      return m_mapping;
   }

   /**
    * @return The full name of the message part, i.e. a space separated path.
    */
   public String getFullPath()
   {
      if (m_parent == null)
      {
         return getName();
      }

      return m_parent.getFullPath() + " " + getName();
   }
   
   public void copyAttributesFrom(MessagePart source)
   {
      verifyNotReadOnly();
      
      m_sName = source.m_sName;
      m_mapping = (source.m_mapping == null) ? null : (MessagePartMapping)source.m_mapping.clone();
      m_nMaxCount = source.m_nMaxCount;
      m_nMinCount = source.m_nMinCount;
      m_sDescription = source.m_sDescription;
      m_parent = source.m_parent;
      m_declarator = source.m_declarator;
   }

   /**
    * Performs a deep-copy of this message part.
    * @see nexj.core.meta.MetadataObject#clone()
    */
   public Object clone()
   {
      MessagePart copy = (MessagePart)super.clone();

      if (m_mapping != null)
      {
         copy.m_mapping = (MessagePartMapping)m_mapping.clone();
      }

      return copy;
   }

   /**
    * Copies this message part for inheritance resolution.
    * @param parent The parent part of the new message part.
    * @return A copy of this message part.
    */
   public MessagePart copy(CompositeMessagePartInstance parent)
   {
      MessagePart copy = (MessagePart)clone();

      copy.m_parent = parent;

      if (copy.m_mapping != null)
      {
         copy.m_mapping.init(copy);
      }

      return copy;
   }

   /**
    * Resolves inheritance for this message part.
    * @param baseMessagePart This part's parent in the inheritance hierarchy. It will be
    * the part from the base message with the same full path, root-excluded.
    */
   public abstract void resolveInheritance(MessagePart baseMessagePart);

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();

      if (m_mapping instanceof MetadataObject)
      {
         ((MetadataObject)m_mapping).makeReadOnly();
      }
   }

   /**
    * This is invoked on objects that have been fully loaded to check the object validity.
    * @param metadata The root metadata object.
    * @param warnings The exception holder where warnings should be appended. Can be null.
    * @throws MetadataException if the object is invalid, e.g. with broken referential integrity.
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      super.validate(metadata, warnings);

      if (m_mapping != null)
      {
         m_mapping.validate(metadata, warnings, this);
      }
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#getValue(nexj.core.util.PropertyMap, java.lang.Object)
    */
   public Object getValue(PropertyMap map, Object defValue)
   {
      return map.findValue(getName(), defValue);
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#setValue(nexj.core.util.PropertyMap, java.lang.Object)
    */
   public void setValue(PropertyMap map, Object value)
   {
      map.setValue(getName(), value);
   }
}
