// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import javax.xml.namespace.QName;

import nexj.core.util.EmptyIterator;
import nexj.core.util.Named;
import nexj.core.util.StringUtil;
import nexj.core.util.XMLWriter;

/**
 * Represents a complex type in an XML schema.
 */
public class XMLMetatype implements Named
{
   // attributes

   /**
    * QName comparator.
    */
   final static Comparator COMPARATOR = new Comparator()
   {
      public int compare(Object left, Object right)
      {
         QName leftType = ((XMLMetatype)left).getType();
         QName rightType = ((XMLMetatype)right).getType();
         int nComp = leftType.getNamespaceURI().compareTo(rightType.getNamespaceURI());

         return (nComp == 0) ? leftType.getLocalPart().compareTo(rightType.getLocalPart()) : nComp;
      }
   };

   /**
    * Is the type abstract.
    */
   protected boolean m_bAbstract = false;

   // associations

   /**
    * Qualified name of this object.
    */
   protected QName m_type;

   /**
    * Type constraint.
    */
   protected XMLMetatypeConstraint m_constraint;
   
   /**
    * Type attributes.
    */
   protected List/*<XMLMetatypeAttribute>*/ m_attributeList = null;

   /**
    * Type elements.
    */
   protected List/*<XMLMetatypeElement>*/ m_elementList = null;

   // constructors

   /**
    * Convenience constructor.
    * @param type The complexType type (not null).
    * @param constraint Additional constraints on the type
    *                   (e.g. restriction/extension) (default == null).
    * @param bAbstract The type should be declared abstract and not be directly instantiable
    *                  (default == false).
    */
   public XMLMetatype(QName type)
   {
      this(type, null);
   }

   /**
    * @see #XMLMetatype(QName)
    */
   public XMLMetatype(QName type, boolean bAbstract)
   {
      this(type, null, bAbstract);
   }

   /**
    * @see #XMLMetatype(QName)
    */
   public XMLMetatype(QName type, XMLMetatypeConstraint constraint)
   {
      assert type != null;

      m_type = type;
      m_constraint = constraint;
   }

   /**
    * @see #XMLMetatype(QName)
    */
   public XMLMetatype(QName type, XMLMetatypeConstraint constraint, boolean bAbstract)
   {
      this(type, constraint);

      m_bAbstract = bAbstract;
   }

   /**
    * Add a new element to the type.
    * @param attribute The attribute to add.
    * @return Self.
    */
   public XMLMetatype addAttribute(XMLMetatypeAttribute attribute)
   {
      if (attribute != null)
      {
         if (m_attributeList == null)
         {
            m_attributeList = new ArrayList/*<XMLMetatypeAttribute>*/();
         }

         m_attributeList.add(attribute);
      }
      
      return this;
   }

   /**
    * Add a new element to the type.
    * @param element The element to add.
    * @return Self.
    */
   public XMLMetatype addElement(XMLMetatypeElement element)
   {
      if (element != null)
      {
         if (m_elementList == null)
         {
            m_elementList = new ArrayList/*<XMLMetatypeElement>*/();
         }

         m_elementList.add(element);
      }
      
      return this;
   }
   
   /**
    * Get the name of the type.
    * @return Name of type.
    */
   public String getName()
   {
      return m_type.getLocalPart();
   }

   /**
    * Get the qualified type of this object.
    * @return Qualified type of this object.
    */
   public QName getType()
   {
      return m_type;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(128);
      
      if (m_bAbstract)
      {
         buf.append('/');
      }

         buf.append(getName());

      if (m_bAbstract)
      {
         buf.append('/');
      }

      if (m_constraint != null)
      {
         buf.append('[');
         buf.append(m_constraint);
         buf.append("] ");
      }

      buf.append('(');

         if (m_attributeList != null)
         {
            for (Iterator/*<XMLMetatypeAttribute>*/ attributeItr = m_attributeList.iterator();
                 attributeItr.hasNext();)
            {
               buf.append('<');
               buf.append(attributeItr.next());
               buf.append("> ");
            }
         }

      buf.append(") {");

         if (m_elementList != null)
         {
            for (Iterator/*<XMLMetatypeElement>*/ elementItr = m_elementList.iterator();
                 elementItr.hasNext();)
            {
               buf.append('<');
               buf.append(elementItr.next());
               buf.append("> ");
            }
         }

      buf.append('}');

      return buf.toString();
   }

   /**
    * Write out the XMLType to a writer as an XSD type.
    * @param writer The writer with which to create the document (if null then noop).
    * @throws IOException On IO error.
    */
   public void writeXSD(XMLWriter writer) throws IOException
   {
      writer.openElement("complexType");
         writer.writeAttribute("name", getName());

         if (m_bAbstract)
         {
            writer.writeAttribute("abstract", true);
         }

      writer.closeElement();

         if (m_constraint != null)
         {
            m_constraint.writeXSD(writer,
               (m_attributeList == null) ? EmptyIterator.getInstance() : m_attributeList.iterator(),
               (m_elementList == null) ? EmptyIterator.getInstance() : m_elementList.iterator());
         }
         else
         {
            if (m_attributeList != null)
            {
               for (Iterator/*<XMLMetatypeAttribute>*/ attributeItr = m_attributeList.iterator();
                    attributeItr.hasNext();)
               {
                  ((XMLMetatypeAttribute)attributeItr.next()).writeXSD(writer);
               }
            }

            if (m_elementList != null && !m_elementList.isEmpty())
            {
               writer.startElement("sequence");

                  for (Iterator/*<XMLMetatypeElement>*/ elementItr = m_elementList.iterator();
                       elementItr.hasNext();)
                  {
                      ((XMLMetatypeElement)elementItr.next()).writeXSD(writer);
                  }

               writer.endElement("sequence");
            }
         }
         
      writer.endElement("complexType");
   }

   /**
    * Class representing an attribute in an XMLMetatype.
    */
   public static class XMLMetatypeAttribute
   {
      /**
       * Is this attribute a reference (used only internally to identify constructor used).
       */
      protected boolean m_bRef = false;
      
      /**
       * Attribute name (or reference).
       */
      protected String m_sName;
      
      /**
       * Attribute type.
       */
      protected String m_sType; 

      /**
       * Convenience constructor.
       * @param sRef The name of the other attribute that this one refers to.
       * @param sRefNS The namespace of the other attribute that this one refers to (can be null).
       */
      public XMLMetatypeAttribute(String sRef, String sRefNS)
      {
         this(null, sRef, sRefNS);

         m_bRef = true;
      }

      /**
       * Convenience constructor.
       * @param sName The name of the attribute.
       * @param sType The type of the attribute.
       * @param sTypeNS The namespace of the type of the attribute (can be null).
       */
      public XMLMetatypeAttribute(String sName, String sType, String sTypeNS)
      {
         m_sName = sName;
         m_sType = sType;

         if (sTypeNS != null)
         {
            m_sType = sTypeNS + ":" + m_sType;
         }
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return (m_bRef) ? ("#" + m_sType) : (m_sName + "@" + m_sType);
      }

      /**
       * Write out the XMLMetatypeAttribute to a writer as an XSD type.
       * @param writer The writer with which to create the document.
       * @throws IOException On IO error.
       */
      public void writeXSD(XMLWriter writer) throws IOException
      {
         writer.openElement("attribute");

            if (m_bRef)
            {
               writer.writeAttribute("ref", m_sType);
            }
            else
            {
               writer.writeAttribute("name", m_sName);
               writer.writeAttribute("type", m_sType);
               writer.writeAttribute("use", "required");
            }

         writer.closeEmptyElement();
      }
   }

   /**
    * Class representing a constraint on XMLMetatype.
    */
   public static abstract class XMLMetatypeConstraint
   {
      /**
       * The base type to which the constraint applies.
       */
      protected String m_sBase;
      
      /**
       * The tag name to use for the element.
       */
      protected String m_sTag; 

      /**
       * Convenience constructor.
       * @param sBase The base attribute of the restriction. 
       * @param sBaseNS The namespace of the base  of the restriction (can be null).
       */
      protected XMLMetatypeConstraint(String sBase, String sBaseNS)
      {
         m_sBase = sBase;

         if (!StringUtil.isEmpty(sBaseNS))
         {
            m_sBase = sBaseNS + ":" + m_sBase;
         }
      }

      /**
       * Get the tag to use for constraint.
       * @return Tag to use for constraint.
       */
      protected abstract String getConstraintTag();

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return "base=" + m_sBase + " type=" + getConstraintTag();
      }

      /**
       * Write out the XMLMetatypeConstraint to a writer as an XSD type with requested
       * attributes/elements added.
       * @param writer The writer with which to create the document (if null then noop).
       * @param attributeItr An iterator over a collection of attributes to add.
       * @param elementItr An iterator over a collection of elements to add.
       * @throws IOException On IO error.
       */
      public void writeXSD(XMLWriter writer,
                           Iterator/*<XMLMetatypeAttribute>*/ attributeItr,
                           Iterator /*<XMLMetatypeElement>*/ elementItr) throws IOException
      {
         writer.startElement("complexContent");
            writer.openElement(getConstraintTag());
               writer.writeAttribute("base", m_sBase);
            writer.closeElement();
               
               if (attributeItr != null)
               {
                  while (attributeItr.hasNext())
                  {
                     ((XMLMetatypeAttribute)attributeItr.next()).writeXSD(writer);
                  }
               }

               if (elementItr != null && elementItr.hasNext())
               {
                  writer.startElement("sequence");

                     while (elementItr.hasNext())
                     {
                        ((XMLMetatypeElement)elementItr.next()).writeXSD(writer);
                     }

                  writer.endElement("sequence");
               }
               
            writer.endElement(getConstraintTag());
         writer.endElement("complexContent");
      }
   }

   /**
    * Class representing an element in an XMLType.
    */
   public static class XMLMetatypeElement
   {
      /**
       * Element name.
       */
      protected String m_sName;
      
      /**
       * Element type.
       */
      protected QName m_type;

      /**
       * Element is nullable.
       */
      protected boolean m_bNullable;
      
      /**
       * Element minOccurs.
       */
      protected int m_nMinOccurs;
      
      /**
       * Element maxOccurs.
       */
      protected int m_nMaxOccurs;

      /**
       * Constructor.
       * Note: maxOccurs!=1 + nullable => array
       *       maxOccurs!=1 + !nullable => set (null elements removed)
       * Note: MS.NET does not properly support an element with different repeating children having
       *       maxOccurs "unbounded"
       *       e.g. when one child is array of type X and another child is an array of type Y.
       *       For compatibility if a repeating element (array/list/set) is encountered then it
       *       should be placed in its own type (with only "item" children)
       *       or at least try generating the API and compiling the resulting code under MS.NET.
       * @param sName The name attribute of the element (not null).
       * @param type The element type (not null).
       * @param nMinOccurs Minimum number of allowed occurrences of the element
       *                   (1 == default => not output to XML).
       * @param nMaxOccurs Maximum number of allowed occurrences of the element
       *                   (1 == default => not output to XML) <0 == "unbounded".
       * @param bNullable If element is nullable (false == default => not output to XML)
       *                  true => minOccurs>0 (at least in .NET).
       */
      public XMLMetatypeElement(
         String sName, QName type, int nMinOccurs, int nMaxOccurs, boolean bNullable)
      {
         assert sName != null;
         assert type != null;

         m_sName = sName;
         m_type = type;
         m_bNullable = bNullable;
         m_nMinOccurs = nMinOccurs;
         m_nMaxOccurs = nMaxOccurs;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return m_sName + "@" + m_type + " " + m_nMinOccurs + "<=*=>" + m_nMaxOccurs +
                " NULLable=" + m_bNullable;
      }

      /**
       * Write out the XMLMetatypeElement to a writer as an XSD type.
       * @param writer The writer with which to create the document (if null then noop).
       * @throws IOException In IO error.
       */
      public void writeXSD(XMLWriter writer) throws IOException
      {
         writer.openElement("element");
            writer.writeAttribute("name", m_sName);

            if (m_type.getPrefix().length() > 0)
            {
               writer.writeAttribute("type", m_type.getPrefix(), ":", m_type.getLocalPart());
            }
            else
            {
               writer.writeAttribute("type", m_type.getLocalPart());
            }

            if (m_nMinOccurs != 1) // ignore default value to minimize XML output
            {
               writer.writeAttribute("minOccurs", m_nMinOccurs);
            }

            if (m_nMaxOccurs < 0)
            {
               writer.writeAttribute("maxOccurs", "unbounded");
            }
            else if (m_nMaxOccurs != 1) // ignore default value to minimize XML output
            {
               writer.writeAttribute("maxOccurs", m_nMaxOccurs);
            }

            if (m_bNullable) // ignore default value to minimize XML output
            {
               writer.writeAttribute("nillable", true);
            }
            
         writer.closeEmptyElement();
      }
   }

   /**
    * Class representing an extension of an XMLMetatype.
    */
   public static class XMLMetatypeExtension extends XMLMetatypeConstraint
   {

      /**
       * Constructor.
       * @param sBase The base attribute of the restriction. 
       * @param sBaseNS The namespace of the base  of the restriction (can be null).
       */
      protected XMLMetatypeExtension(String sBase, String sBaseNS)
      {
         super(sBase, sBaseNS);
      }

      /**
       * @see nexj.core.rpc.xml.XMLMetatype.XMLMetatypeConstraint#getConstraintTag()
       */
      protected String getConstraintTag()
      {
         return "extension";
      }
   }

   /**
    * Class representing a restriction on XMLMetatype.
    */
   public static class XMLMetatypeRestriction extends XMLMetatypeConstraint
   {
      /**
       * Constructor.
       * @param sBase The base attribute of the restriction. 
       * @param sBaseNS The namespace of the base  of the restriction (can be null).
       */
      public XMLMetatypeRestriction(String sBase, String sBaseNS)
      {
         super(sBase, sBaseNS);
      }

      /**
       * @see nexj.core.rpc.xml.XMLMetatype.XMLMetatypeConstraint#getConstraintTag()
       */
      protected String getConstraintTag()
      {
         return "restriction";
      }
   }
}