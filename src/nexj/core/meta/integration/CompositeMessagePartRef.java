// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.MetadataException;
import nexj.core.rpc.TransferObject;
import nexj.core.util.StringUtil;

public class CompositeMessagePartRef extends CompositeMessagePart
{
   // associations

   /**
    * The referenced composite message part.
    */
   private CompositeMessagePart m_refPart;

   // constructors

   public CompositeMessagePartRef(String name)
   {
      super(name);
   }
   
   // operations

   /**
    * Sets the referenced message part.
    * @param refPart The referenced message part to set.
    */
   public void setRefPart(CompositeMessagePart refPart)
   {
      verifyNotReadOnly();

      if (refPart == this)
      {
         throw new MetadataException("err.integration.invalidRef", new Object[]{getFullPath()});
      }

      m_refPart = refPart;

      if (refPart != null &&
         refPart.getDeclarator() != null &&
         m_declarator != null &&
         refPart.getDeclarator().getFormat() != m_declarator.getFormat() &&
         m_declarator.getFormat() != null)
      {
         throw new MetadataException("err.integration.incompatibleRefFormat", new Object[]{getFullPath()});
      }
   }

   /**
    * @return The referenced message part.
    */
   public CompositeMessagePart getRefPart()
   {
      return m_refPart;
   }
   
   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#addPart(nexj.core.meta.integration.MessagePart)
    */
   public void addPart(MessagePart part)
   {
      m_refPart.addPart(part);
   }

   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#getPart(java.lang.String)
    */
   public MessagePart getPart(String sName)
   {
      return m_refPart.getPart(sName);
   }

   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#findPart(java.lang.String)
    */
   public MessagePart findPart(String sName)
   {
      return m_refPart.findPart(sName);
   }

   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#getPart(int)
    */
   public MessagePart getPart(int nOrdinal)
   {
      return m_refPart.getPart(nOrdinal);
   }

   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#getPartCount()
    */
   public int getPartCount()
   {
      return m_refPart.getPartCount();
   }

   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#getPartIterator()
    */
   public Iterator getPartIterator()
   {
      return m_refPart.getPartIterator();
   }

   /**
    * @see nexj.core.util.Sortable#sort(java.util.Comparator)
    */
   public void sort(Comparator cmp)
   {
      m_refPart.sort(cmp);
   }

   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#hasPart(java.lang.String)
    */
   public boolean hasPart(String sName)
   {
      return m_refPart.hasPart(sName);
   }
   
   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#getAggregation()
    */
   public byte getAggregation()
   {
      return m_refPart.getAggregation();
   }

   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#isLax()
    */
   public boolean isLax()
   {
      return m_refPart.isLax();
   }

   /**
    * @see nexj.core.meta.integration.CompositeMessagePart#getReferencesToResolve()
    */
   public List getReferencesToResolve()
   {
      return Collections.singletonList(this);
   }

   /**
    * @see nexj.core.meta.integration.MessagePart#copy()
    */
   public MessagePart copy(CompositeMessagePartInstance parent)
   {
      CompositeMessagePartRef copy = (CompositeMessagePartRef)super.copy(parent);

      parent.getDeclarator().addRef(copy);

      return copy;
   }

   /**
    * @see nexj.core.meta.integration.MessagePart#resolveInheritance(nexj.core.meta.integration.MessagePart)
    */
   public void resolveInheritance(MessagePart baseMessagePart)
   {
      // Reference parts always get replaced by any other part type.
   }

   /**
    * @return whether or not m_refPart is an ancestor
    */
   public boolean isRefPartAncestor()
   {
      for (CompositeMessagePart parent = getParent(); parent != null; parent = parent.getParent())
      {
         if (parent == m_refPart)
         {
            return true;
         }
      }
      
      return false;
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#getChild(java.lang.String)
    */
   public EndpointPart getChild(String sName)
   {
      if (!StringUtil.isEmpty(sName) && sName.charAt(0) == ':')
      {
         if (sName.equals(":oid"))
         {
            return Transformation.OID;
         }

         if (sName.equals(":class"))
         {
            return Transformation.CLASS;
         }

         if (sName.equals(":event"))
         {
            return Transformation.EVENT;
         }
      }

      return getPart(sName);
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#findChild(java.lang.String)
    */
   public EndpointPart findChild(String sName)
   {
      if (!StringUtil.isEmpty(sName) && sName.charAt(0) == ':')
      {
         if (sName.equals(":oid"))
         {
            return Transformation.OID;
         }

         if (sName.equals(":class"))
         {
            return Transformation.CLASS;
         }

         if (sName.equals(":event"))
         {
            return Transformation.EVENT;
         }
      }

      return findPart(sName);
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#createObject()
    */
   public TransferObject createObject()
   {
      return new TransferObject(m_refPart.getName());
   }
}