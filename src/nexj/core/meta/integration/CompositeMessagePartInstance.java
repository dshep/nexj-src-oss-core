// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.ContextMetadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.rpc.TransferObject;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashTab;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.Lookup;
import nexj.core.util.LookupDeque;

public class CompositeMessagePartInstance extends CompositeMessagePart
{
   // attributes
   
   /**
    * The aggregation mode.
    */
   protected byte m_nAggregation;

   /**
    * The lax validation flag.
    */
   protected boolean m_bLax;

   // associations
   
   /**
    * The child part map: MessagePart[String].
    */
   protected Lookup m_partMap = new HashTab(4);
   
   /**
    * The child part list: MessagePart[].
    */
   protected List m_partList = new ArrayList(4);

   // constructors

   /**
    * Constructs the message part.
    * @param sName The part name.
    */
   public CompositeMessagePartInstance(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the message part.
    */
   public CompositeMessagePartInstance()
   {
      super();
   }

   // operations

   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#addPart(nexj.core.meta.integration.MessagePart)
    */
   public void addPart(MessagePart part)
   {
      verifyNotReadOnly();

      Object oldPart = m_partMap.put(part.getName(), part);

      if (oldPart != null)
      {
         m_partMap.put(part.getName(), oldPart);

         throw new MetadataException("err.meta.msgPartDup", new Object[]
         {
            part.getFullPath(),
            getFullPath()
         });
      }
      
      m_partList.add(part);
      part.setParent(this);
   }

   /**
    * @param child The child message ref to replace.
    * @param replace The message that replaces the child message ref.
    */
   public void replacePart(CompositeMessagePartRef child, CompositeMessagePart replace)
   {
      verifyNotReadOnly();

      m_partList.set(m_partList.indexOf(child), replace);
   }

   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#hasPart(java.lang.String)
    */
   public boolean hasPart(String sName)
   {
      return m_partMap.get(sName) != null;
   }

   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#getPart(java.lang.String)
    */
   public MessagePart getPart(String sName)
   {
      MessagePart part = (MessagePart) m_partMap.get(sName);

      if (part != null)
      {
         return part;
      }

      throw new MetadataLookupException("err.meta.msgPartLookup", sName, this);
   }

   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#findPart(java.lang.String)
    */
   public MessagePart findPart(String sName)
   {
      return (MessagePart)m_partMap.get(sName);
   }

   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#getPart(int)
    */
   public MessagePart getPart(int nOrdinal)
   {
      return (MessagePart)m_partList.get(nOrdinal);
   }

   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#getPartCount()
    */
   public int getPartCount()
   {
      return m_partMap.size();
   }

   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#getPartIterator()
    */
   public Iterator getPartIterator()
   {
      return m_partList.iterator();
   }

   /**
    * @see nexj.core.util.Sortable#sort(java.util.Comparator)
    */
   public void sort(Comparator cmp)
   {
      verifyNotReadOnly();
      Collections.sort(m_partList, cmp);
   }

   /**
    * Sets the aggregation mode.
    * @param nAggregation The aggregation mode to set.
    */
   public void setAggregation(byte nAggregation)
   {
      verifyNotReadOnly();
      m_nAggregation = nAggregation;
   }

   /**
    * @return The aggregation mode.
    */
   public byte getAggregation()
   {
      return m_nAggregation;
   }

   /**
    * Sets the lax validation flag.
    * @param bLax The lax validation flag to set.
    */
   public void setLax(boolean bLax)
   {
      verifyNotReadOnly();
      m_bLax = bLax;
   }

   /**
    * @return The lax validation flag.
    */
   public boolean isLax()
   {
      return m_bLax;
   }

   /**
    * @see nexj.core.meta.integration.MessagePart#resolveInheritance(nexj.core.meta.integration.MessagePart)
    */
   public void resolveInheritance(MessagePart baseMessagePart)
   {
      if (baseMessagePart instanceof CompositeMessagePartInstance)
      {
         CompositeMessagePartInstance baseMessagePartComp = (CompositeMessagePartInstance)baseMessagePart;
         LookupDeque orderedPartSet = new LinkedHashTab(m_partList.size());

         for (int i = 0; i < m_partList.size(); i++)
         {
            MessagePart part = (MessagePart)m_partList.get(i);

            orderedPartSet.put(part.getName(), part);
         }

         int nSize = baseMessagePartComp.m_partList.size();
         Lookup oldPartMap = m_partMap;

         m_partList = new ArrayList(nSize + m_partList.size());
         m_partMap = new HashTab(nSize);

         // Add parts defined in the base message.
         for (int i = 0; i < nSize; i++)
         {
            MessagePart baseMessageChildPart = (MessagePart)baseMessagePartComp.m_partList.get(i);
            MessagePart childPart = (MessagePart)oldPartMap.get(baseMessageChildPart.getName());

            if (childPart == null)
            {
               addPart(baseMessageChildPart.copy(this));
            }
            else
            {
               orderedPartSet.remove(childPart.getName());
               addPart(childPart);
               childPart.resolveInheritance(baseMessageChildPart);
            }
         }

         // Add parts defined in the derived message.
         for (Iterator itr = orderedPartSet.valueIterator(); itr.hasNext(); )
         {
            addPart((MessagePart)itr.next());
         }

         // Inherit the mapping
         if (m_mapping == null)
         {
            if (baseMessagePart.m_mapping != null)
            {
               m_mapping = (MessagePartMapping)baseMessagePart.m_mapping.clone();
               m_mapping.init(this);
            }
         }
         else
         {
            m_mapping.resolveInheritance(baseMessagePart.m_mapping);
         }
      }
   }

   /**
    * @see nexj.core.meta.integration.MessagePart#clone()
    */
   public Object clone()
   {
      CompositeMessagePartInstance copy = (CompositeMessagePartInstance)super.clone();
      int nCount = m_partList.size();

      copy.m_partList = new ArrayList(nCount);
      copy.m_partMap = new HashTab(nCount);

      for (int i = 0; i < nCount; i++)
      {
         MessagePart part = (MessagePart)m_partList.get(i);

         part = (MessagePart)part.clone();
         copy.addPart(part);

         if (part.getMapping() != null)
         {
            part.getMapping().init(part);
         }
      }

      return copy;
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

      for (Iterator iterator = getPartIterator(); iterator.hasNext();)
      {
         ((MessagePart)iterator.next()).validate(metadata, warnings);
      }
   }

   /**
    * @see nexj.core.meta.integration.MessagePart#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();

      for (Iterator itr = getPartIterator(); itr.hasNext(); )
      {
         ((MessagePart)itr.next()).makeReadOnly();
      }

      ((ArrayList)m_partList).trimToSize();
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#getChild(java.lang.String)
    */
   public EndpointPart getChild(String sName)
   {
      return getPart(sName);
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#findChild(java.lang.String)
    */
   public EndpointPart findChild(String sName)
   {
      return findPart(sName);
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#createObject()
    */
   public TransferObject createObject()
   {
      return new TransferObject();
   }
}