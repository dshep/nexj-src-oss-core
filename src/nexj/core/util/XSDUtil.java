// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.CharArrayWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.StringWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.apache.xerces.impl.dv.InvalidDatatypeValueException;
import org.apache.xerces.impl.dv.ValidatedInfo;
import org.apache.xerces.impl.dv.ValidationContext;
import org.apache.xerces.impl.dv.XSSimpleType;
import org.apache.xerces.impl.validation.ValidationState;
import org.apache.xerces.parsers.XMLGrammarPreparser;
import org.apache.xerces.util.SymbolTable;
import org.apache.xerces.xni.grammars.Grammar;
import org.apache.xerces.xni.grammars.XMLGrammarDescription;
import org.apache.xerces.xni.grammars.XSGrammar;
import org.apache.xerces.xni.parser.XMLEntityResolver;
import org.apache.xerces.xni.parser.XMLInputSource;
import org.apache.xerces.xs.StringList;
import org.apache.xerces.xs.XSAnnotation;
import org.apache.xerces.xs.XSAttributeDeclaration;
import org.apache.xerces.xs.XSAttributeUse;
import org.apache.xerces.xs.XSComplexTypeDefinition;
import org.apache.xerces.xs.XSConstants;
import org.apache.xerces.xs.XSElementDeclaration;
import org.apache.xerces.xs.XSModel;
import org.apache.xerces.xs.XSModelGroup;
import org.apache.xerces.xs.XSObject;
import org.apache.xerces.xs.XSObjectList;
import org.apache.xerces.xs.XSParticle;
import org.apache.xerces.xs.XSSimpleTypeDefinition;
import org.apache.xerces.xs.XSTerm;
import org.apache.xerces.xs.XSTypeDefinition;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import nexj.core.util.XMLUtil.MappedResolver;

public class XSDUtil
{
   private final static String[] EMPTYSTRINGARRAY = new String[0];
   
   // constructors

   /**
    * Prevents instantiation.
    */
   protected XSDUtil()
   {
   }

   // operations

   public static XSParticle getParticle(XSElementDeclaration elemDecl)
   {
      XSComplexTypeDefinition complexDef = getComplexTypeDef(elemDecl);
      
      if (complexDef == null)
      { 
         return null;
      }
      
      return getParticle(complexDef);
   }
   
   public static XSParticle getParticle(XSComplexTypeDefinition complexTypeDef)
   {
      switch (complexTypeDef.getContentType())
      {
         case XSComplexTypeDefinition.CONTENTTYPE_EMPTY:
         case XSComplexTypeDefinition.CONTENTTYPE_SIMPLE:
            return null;
      }
      
      return complexTypeDef.getParticle();
   }
                                               
   public static XSElementDeclaration getChildElementDeclaration(XSElementDeclaration parentElemDecl, String nodeName)
   {
      return getElementDeclaration(getModelGroup(parentElemDecl),nodeName);
   }
   
   public static XSElementDeclaration getChildElementDeclaration(XSComplexTypeDefinition parentElemDecl, String nodeName)
   {
      return getElementDeclaration(getModelGroup(parentElemDecl),nodeName);
   }

   public static XSElementDeclaration getElementDeclaration(XSModelGroup modelGroup, String nodeName)
   {
      if (modelGroup == null)
      {
         return null;
      }
      
      XSElementDeclaration retVal = null;
      XSObjectList particles = modelGroup.getParticles();
      
      for (int i=0; i < particles.getLength(); ++i)
      {
         XSParticle particle = (XSParticle)particles.item(i);
         retVal = getElementDeclaration(particle.getTerm(), nodeName);
         
         if (retVal != null)
         {
            break;
         }
      }
      return retVal;
   }
   
   private static XSElementDeclaration getElementDeclaration(XSTerm term, String nodeName)
   {
      switch (term.getType())
      {
         case XSConstants.ELEMENT_DECLARATION:
         {
            return nodeName.equals(term.getName()) ? (XSElementDeclaration)term : null;
         }
         case XSConstants.MODEL_GROUP:
         {            
            return getElementDeclaration((XSModelGroup)term, nodeName);
         }
      }
      return null;
   }

   public static XSComplexTypeDefinition getComplexTypeDef(XSElementDeclaration elemDecl)
   {
      if (elemDecl == null)
      {
         return null;
      }
      
      XSTypeDefinition td = elemDecl.getTypeDefinition();
      
      if (td.getTypeCategory()!=XSTypeDefinition.COMPLEX_TYPE)
      {
         return null;
      }
      
      return (XSComplexTypeDefinition)td;
   }

   public static String getLabel(XSElementDeclaration elemDecl)
   {
      if (elemDecl == null)
      {
         return "";
      }
      
      XSAnnotation annotation = elemDecl.getAnnotation();
      
      if (annotation == null)
      {
         return "";      
      }
      
      return XSDUtil.getText(annotation, "label");
   }

   public static XSModelGroup getModelGroup(XSElementDeclaration decl)
   {
      return getModelGroup(getParticle(decl));
   }

   public static XSModelGroup getModelGroup(XSComplexTypeDefinition decl)
   {
      return getModelGroup(getParticle(decl));
   }

   public static XSModelGroup getModelGroup(XSParticle part)
   {
      if (part == null)
      {
         return null;
      }
      
      XSTerm term = part.getTerm();
      
      if (term.getType()!=XSConstants.MODEL_GROUP)
      {
         return null;
      }
      return (XSModelGroup)term;
   }

   /**
    * Gets the model from the schema at the given URL. For security and efficiency reasons, references to other
    * schemas will not be followed unless they are present in the schemaURLMap.
    * @param xsdURL The URL of the schema.
    * @param schemaMap The map of schema URL strings to the actual URLs to use.
    * @return The schema model
    * @throws IOException If an I/O error occurs.
    */
   public static XSModel getXSModel(URL xsdURL, Lookup schemaMap) throws IOException
   {
      return getXSModel(toXMLInputSource(xsdURL), schemaMap);
   }

   /**
    * Gets the schema model from a schema input source.
    * @param inputSource The schema input source.
    * @param schemaMap The map of schema URL strings to the actual URLs to use.
    * @return The schema model.
    * @throws IOException If an I/O error occurs.
    */
   public static XSModel getXSModel(XMLInputSource inputSource, Lookup schemaMap) throws IOException
   {
      return getXSModel(preloadGrammar(inputSource, schemaMap, false));
   }

   /**
    * Gets the model from the schema at the given URL.
    * @param xsdURL The URL of the schema.
    * @return The schema model.
    * @throws IOException If an I/O error occurs.
    */
   public static XSModel getXSModel(URL xsdURL) throws IOException
   {
      return getXSModel(toXMLInputSource(xsdURL), false);
   }

   public static XSModel getXSModel(XMLInputSource inputSource, boolean bAllowDownload, XMLEntityResolver resolver) throws IOException
   {
      return getXSModel(preloadGrammar(inputSource, null, bAllowDownload, resolver));
   }
   
   /**
    * Gets the schema model from a schema input source.
    * @param inputSource The schema input source.
    * @param bAllowDownload True to allow download of unmapped URLs; false to throw an exception.
    * @return The schema model.
    * @throws IOException If an I/O error occurs.
    */
   public static XSModel getXSModel(XMLInputSource inputSource, boolean bAllowDownload) throws IOException
   {
      return getXSModel(preloadGrammar(inputSource, null, bAllowDownload));
   }

   public static XSModel getXSModel(Grammar g)
   {
      if (g instanceof XSGrammar)
      {
         return ((XSGrammar)g).toXSModel();
      }

      return null;
   }

   private static Grammar preloadGrammar(XMLInputSource inputSource, Lookup schemaMap, boolean bResolveUnmappedEntities) throws IOException
   {
      return preloadGrammar(inputSource, schemaMap, bResolveUnmappedEntities, null);
   }
   
   /**
    * Loads the grammar from a schema input source.
    * @param inputSource The schema input source.
    * @param schemaMap
    * @param bResolveUnmappedEntities True to resolve unmapped URLs; false to throw an exception.
    * @return The schema grammar.
    * @throws IOException If an I/O error occurs.
    */
   private static Grammar preloadGrammar(XMLInputSource inputSource, Lookup schemaMap, boolean bResolveUnmappedEntities, XMLEntityResolver resolver) throws IOException
   {
      XMLGrammarPreparser preparser =
         new XMLGrammarPreparser(new SymbolTable(XMLUtil.SYMBOL_TABLE_SIZE));

      preparser.registerPreparser(XMLGrammarDescription.XML_SCHEMA, null);
      preparser.setFeature(XMLUtil.NAMESPACES_FEATURE, true);
      preparser.setFeature(XMLUtil.VALIDATION_FEATURE, true);
      preparser.setFeature(XMLUtil.SCHEMA_VALIDATION_FEATURE, true);
      preparser.setFeature(XMLUtil.NODE_DEFERRAL_FEATURE, false);
      preparser.setFeature(XMLUtil.HONOR_ALL_SCHEMALOCATIONS_FEATURE, true);
      
      if (resolver == null)
      {
         resolver = new MappedResolver(schemaMap, bResolveUnmappedEntities);
      }
      
      preparser.setEntityResolver(resolver);

      return preparser.preparseGrammar(XMLGrammarDescription.XML_SCHEMA, inputSource);
   }

   /**
    * 
    * @param elemDeclaration
    * @param choiceElement hint for a choice, can be null
    * @return
    */
   public static String getTemplateXML(final XSElementDeclaration elemDeclaration, String choiceElement)
   {     
      return "<xml>" + internalGetTemplateXML(elemDeclaration, choiceElement) + "</xml>"; 
   }
   
   public static String internalGetTemplateXML(final XSElementDeclaration elemDeclaration, String choiceElement)
   {
      String begin = "<" + elemDeclaration.getName();
      XSParticle particle = getParticle(elemDeclaration);

      if (particle == null)
      {
         return begin + "/>"; 
      }

      return begin + ">" + getTemplateXML(particle, choiceElement) + "</" + elemDeclaration.getName() + ">";
   }

   private static String getTemplateXML(XSParticle particle, String choiceElement)
   {
      String retVal = "";
      if (particle.getMinOccurs() > 0)
      {
         String innerXML = getTemplateXML(particle.getTerm(), choiceElement);
         for (int j=0; j < particle.getMinOccurs(); j++)
         {
            retVal += innerXML;
         }
      }
      return retVal;
   }

   private static String getTemplateXML(XSTerm term, String choiceElement)
   {
      switch (term.getType())
      {
         case XSConstants.MODEL_GROUP:
         {
            return getTemplateXML((XSModelGroup)term, choiceElement);
         }
         case XSConstants.ELEMENT_DECLARATION:
         {
            return internalGetTemplateXML((XSElementDeclaration)term, choiceElement);
         }
      }
      return "<" + term.getName() + "/>";
   }

   private static String getTemplateXML(XSModelGroup modelGroup, String choiceElement)
   {
      XSObjectList particles = modelGroup.getParticles();
      String retVal = "";
      switch (modelGroup.getCompositor())
      {
         case XSModelGroup.COMPOSITOR_ALL:
         case XSModelGroup.COMPOSITOR_SEQUENCE:
         {
            for (int i=0; i < particles.getLength(); ++i)
            {
               XSParticle particle = (XSParticle)particles.item(i);
               retVal += getTemplateXML(particle, null);
            }
         }
         break;
         case XSModelGroup.COMPOSITOR_CHOICE:
         {
            if (choiceElement != null)
            {
               for (int i=0; i < particles.getLength(); ++i)
               {
                  XSParticle particle = (XSParticle)particles.item(i);
                  if (choiceElement.equals(particle.getTerm().getName()) && (particle.getMaxOccursUnbounded() || particle.getMaxOccurs() > 0))
                  {
                     retVal += getTemplateXML(particle, null);
                     break;
                  }
               }
            }
         }
         break;
      }
      return retVal;
   }

   public static String getText(XSAnnotation annotation, final String fTagName)
   {
      final CharArrayWriter retVal = new CharArrayWriter();
      annotation.writeAnnotation(new DefaultHandler()
      {
         boolean m_insideTag;
         public void startElement(java.lang.String uri, java.lang.String localName, java.lang.String qName, org.xml.sax.Attributes attributes)
         {
            if (fTagName.equals(localName))
            {
               m_insideTag = true; 
            }
            else
            {
               m_insideTag = false;
            }
         }
         public void characters (char ch[], int start, int length)
        throws SAXException
         {
            if (m_insideTag)
            {
               retVal.write(ch,start,length);
            }
         }
      }, XSAnnotation.SAX_CONTENTHANDLER);
      return retVal.toString().trim();
   }

   public static XSAttributeUse searchForAttributeUse(XSElementDeclaration descriptor, String attrName)
   {
      XSComplexTypeDefinition def = getComplexTypeDef(descriptor);
      if (def == null)
      {
         return null;
      }
      XSObjectList l = def.getAttributeUses();
      for (int i=0; i < l.getLength(); ++i)
      {
         XSAttributeUse use = (XSAttributeUse)l.item(i);
         if (attrName.equals(use.getAttrDeclaration().getName()))
         {
            return use;
         }
      } 
      return null;
   }

   public static List toList(XSObjectList srcList)
   {
      List newList = new ArrayList(srcList.getLength());
      for (int i=0; i < srcList.getLength(); ++i)
      {
         newList.add(srcList.item(i));
      }
      return newList;
   }

   /**
    * @return A boolean value indicating whether attrUse requires validation against a pattern.
    */
   public static boolean hasPattern(XSAttributeDeclaration attrDecl)
   {
      return containsFacet(attrDecl, XSSimpleTypeDefinition.FACET_PATTERN);
   }

   
   /**
    * @param elemDecl
    * @param attribute Cannot be null.
    * @return
    */
   public static XSAttributeUse getAttributeUse(XSElementDeclaration elemDecl, String attrName)
   {
       return getAttributeUse(XSDUtil.getComplexTypeDef(elemDecl), attrName);
   }

   public static XSAttributeUse getAttributeUse(XSComplexTypeDefinition def, String attrName)
   {
      if (def == null)
      {
         return null;
      }
      
      XSObjectList attributeUses = def.getAttributeUses();
      
      if (attributeUses == null || attributeUses.getLength() == 0)
      {
         return null;
      }
      
      for (int i=0; i<attributeUses.getLength(); ++i)
      {
         XSAttributeUse attr = (XSAttributeUse)attributeUses.item(i);

         if (attrName.equals(attr.getAttrDeclaration().getName()))
         {
            return attr;
         }
      }
      
      return null;
   }

   public static String[] getEnumeration(XSSimpleTypeDefinition simpleType)
   {
      if (!containsFacet(simpleType, XSSimpleTypeDefinition.FACET_ENUMERATION))
      {
         return EMPTYSTRINGARRAY;
      }
      
      StringList sList = simpleType.getLexicalEnumeration();
      
      if (sList == null || sList.getLength() == 0)
      {
         return EMPTYSTRINGARRAY;
      }
      
      String[] retVal = new String[sList.getLength()];
      
      for (int i = 0; i < sList.getLength(); ++i)
      {
         retVal[i] = sList.item(i);
      }

      return retVal;
   }
   
   /**
    * @return The array of enumerated string values or the empty array.
    */
   public static String[] getEnumeration(XSAttributeDeclaration attrDecl)
   {
      return getEnumeration(attrDecl.getTypeDefinition());
   }

   private static boolean containsFacet(XSAttributeDeclaration attrDecl, short facet)
   {
      return containsFacet(attrDecl.getTypeDefinition(), facet); 
   }

   private static boolean containsFacet(XSSimpleTypeDefinition def, short facet)
   {
      return ((def.getDefinedFacets() & facet) != 0); 
   }

   /**
    * @return true if attrDecl is type boolean.
    */
   public static boolean isBooleanAttribute(XSAttributeDeclaration attrDecl)
   {
      XSSimpleTypeDefinition def = attrDecl.getTypeDefinition();
      def = def.getPrimitiveType();
      
      if (def == null)
      {
         return false;      
      }
      
      return (def.getName().equals("boolean"));
   }

   /**
    * Validates a string against an attribute type with a pattern.
    * @return Error message or null value if successful. 
    */   
   public static String validate(String value, XSAttributeDeclaration attrDecl)
   {
      XSSimpleTypeDefinition def = attrDecl.getTypeDefinition();
      
      if (!(def instanceof XSSimpleType))
      {
         return null;      
      }
      
      XSSimpleType simpleType = (XSSimpleType)def;
      ValidatedInfo validatedInfo = new ValidatedInfo();
      ValidationContext validationState = new ValidationState();

      try
      {
          simpleType.validate(value, validationState, validatedInfo);
      }
      catch(InvalidDatatypeValueException ex)
      {
          return ObjUtil.getMessage(ex);
      }
      
      return null;
   }

   public static String getDefaultAttributeValue(XSElementDeclaration elemDecl, String attrName)
   {
      return getDefaultAttributeValue(getAttributeUse(elemDecl, attrName));
   }

   public static String getDefaultAttributeValue(XSComplexTypeDefinition elemDecl, String attrName)
   {
      return getDefaultAttributeValue(getAttributeUse(elemDecl, attrName));
   }
   
   public static String getDefaultAttributeValue(XSAttributeUse attrUse)
   {
      if (attrUse != null && attrUse.getConstraintType() == XSConstants.VC_DEFAULT)
      {
         String sDefaultVal = attrUse.getValueConstraintValue().getNormalizedValue();

         return sDefaultVal == null ? "" : sDefaultVal;
      }
      
      return "";
   }
   
   public static boolean hasWildCard(XSObject obj)
   {
      switch (obj.getType())
      {
         case XSConstants.PARTICLE:
         {
            return hasWildCard(((XSParticle)obj).getTerm());
         }
         case XSConstants.MODEL_GROUP:
         {
            return hasWildCard((XSModelGroup)obj);
         }
         case XSConstants.WILDCARD:
         {
            return true;
         }
      }

      return false;
   }
   
   public static boolean hasWildCard(XSModelGroup group)
   {
      XSObjectList l = group.getParticles();
      
      for (int i = 0; i < l.getLength(); ++i)
      {
         XSObject obj = l.item(i);
         
         if (hasWildCard(obj))
         {
            return true;
         }
      }
      
      return false;
   }
   
   public static XSObjectList getAttributeUses(XSElementDeclaration elemDecl)
   {
      XSComplexTypeDefinition def = XSDUtil.getComplexTypeDef(elemDecl);
      
      return def != null ? def.getAttributeUses() : null; 
   }
   
   public static void stripAttributes(Element element, XSElementDeclaration elemDecl)
   {
      XSObjectList attributeUses = getAttributeUses(elemDecl);
      
      if (attributeUses == null)
      {
         return;
      }
      
      Set nameLookup = new HashSet(attributeUses.getLength());
      
      for (int i=0; i < attributeUses.getLength(); ++i)
      {
         XSAttributeUse attr = (XSAttributeUse)attributeUses.item(i);
         XSAttributeDeclaration attrDecl = attr.getAttrDeclaration();
         nameLookup.add(attrDecl.getName());
      }
      
      stripAttributes(element, nameLookup);
   }

   public static void stripAttributes(Element element, Set nameLookup)
   {
      NamedNodeMap attributes = element.getAttributes();
      
      if (attributes == null || attributes.getLength() == 0)
      {
         return;
      }
      
      for (int i = 0; i < attributes.getLength(); ++i)
      {
         Node node = attributes.item(i);
         
         if (!nameLookup.contains(node.getNodeName()))
         {
            element.removeAttributeNode((Attr)node);
         }
      }
   }

   public static String transform(Transformer transformer, Reader reader) throws TransformerException
   {
      return transform(transformer, new StreamSource(reader));
   }
   
   public static String transform(Transformer transformer, Source xmlSource) throws TransformerException
   {
      transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "3");     
      StringWriter writer = new StringWriter(8192);
      transformer.transform(xmlSource, new StreamResult(writer));
      return writer.toString();
   }
   
   public static XMLInputSource toXMLInputSource(URL xsdURL)
   {
      return new XMLInputSource(null, xsdURL.toExternalForm(), null);
   }
}
