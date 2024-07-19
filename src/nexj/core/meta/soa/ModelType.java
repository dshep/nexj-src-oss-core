// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.soa;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.xml.XMLSOAMetadataLoader;
import nexj.core.meta.xml.XMLSOAMetadataLoader.GlobalObject;
import nexj.core.meta.xml.XMLSOAMetadataLoader.SOAObject;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

/**
 * An information model type.
 */
public class ModelType extends SOAObject implements GlobalObject
{
   /**
    * True if this is a Fault type; false if it is a regular information model type.
    */
   protected boolean m_bFault;

   /**
    * The definition where this information model type is defined.
    */
   protected Definition m_definition;

   /**
    * The attributes on this type.
    */
   protected Lookup m_attributeMap = new HashTab(); // Attribute[String]

   /**
    * The list of base types.
    */
   protected List m_baseList = new ArrayList(2); // ModelType[]

   // constructors

   /**
    * Creates a new information model type.
    * @param definition The definition where this type is defined.
    */
   public ModelType(Definition definition)
   {
      m_definition = definition;
   }

   // operations

   /**
    * Sets whether this type is a Fault.
    * @param bFault True if this is a Fault type; false otherwise.
    */
   public void setFault(boolean bFault)
   {
      m_bFault = bFault;
   }

   /**
    * Gets whether this type is a Fault.
    * @return True if this is a Fault type; false otherwise.
    */
   public boolean isFault()
   {
      return m_bFault;
   }

   /**
    * Gets the definition in which this information model type is defined.
    * @return The definition where this type is defined.
    */
   public Definition getDefinition()
   {
      return m_definition;
   }

   /**
    * Adds a base to this type.
    * @param base The base type to add.
    */
   public void addBase(ModelType base)
   {
      m_baseList.add(base);
   }

   /**
    * @return A space-separated list of the base types.
    */
   public String getBases()
   {
      StringBuilder sb = new StringBuilder();

      for (int i = 0 ; i < m_baseList.size(); i++)
      {
         if (i != 0)
         {
            sb.append(" ");
         }

         sb.append(((ModelType)m_baseList.get(i)).getName());
      }

      return sb.toString();
   }

   /**
    * Adds an attribute to this type.
    * @param attribute The attribute to add.
    * @return False if an attribute of the same name already exists, true otherwise
    */
   public boolean addAttribute(Attribute attribute)
   {
      return m_attributeMap.put(attribute.getName(), attribute) == null;
   }

   /**
    * Gets the attribute with the specified name or null if none.
    * @param sName The name
    * @return The attribute with this name or null if none.
    */
   public Attribute findAttribute(String sName)
   {
      return (Attribute)m_attributeMap.get(sName);
   }

   /**
    * Gets the number of attributes on this type.
    * @return The number of attributes.
    */
   public int getAttributeCount()
   {
      return m_attributeMap.size();
   }

   /**
    * Gets an iterator over the attributes on this type.
    * @return The Attribute iterator.
    */
   public Iterator getAttributeIterator()
   {
      return m_attributeMap.valueIterator();
   }

   /**
    * @see nexj.core.meta.xml.XMLSOAMetadataLoader.GlobalObject#getGlobalName()
    */
   public String getGlobalName()
   {
      StringBuilder buf = new StringBuilder(m_definition.getGlobalName().length() + 6 + m_sName.length());

      buf.append(m_definition.getGlobalName());
      buf.append(":type:");
      buf.append(m_sName);

      return buf.toString();
   }

   /**
    * Generates the following code:
    *
    *    (define-class <definition QName>:type:<name> (soa:InformationModelObject <bases>) "Description"
    *       (attribute ...)
    *       ...
    *    )
    *
    * @see nexj.core.meta.xml.XMLSOAMetadataLoader.SOAObject#getCode()
    */
   public Object getCode()
   {
      Pair attributes = null;

      for (Iterator itr = getAttributeIterator(); itr.hasNext(); )
      {
         attributes = new ConstPair(((Attribute)itr.next()).getCode(), attributes);
      }

      Pair bases = null;

      for (int i = m_baseList.size() - 1; i >= 0; i--)
      {
         bases = new ConstPair(Symbol.define(((ModelType)m_baseList.get(i)).getGlobalName()), bases);
      }

      bases = new ConstPair((m_bFault) ? XMLSOAMetadataLoader.FAULT_OBJECT : XMLSOAMetadataLoader.INFORMATION_MODEL_OBJECT, bases);

      return ConstPair.cons(
         XMLSOAMetadataLoader.DEFINE_CLASS,
         Symbol.define(getGlobalName()),
         bases,
         getDescription(),
         attributes
      );
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "ModelType(" + ((m_sName == null) ? "ANONYMOUS" : "\"" + m_sName + "\"") + ')';
   }
}
