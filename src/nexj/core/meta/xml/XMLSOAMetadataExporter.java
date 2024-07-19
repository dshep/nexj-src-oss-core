// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.xml;

import java.io.IOException;
import java.util.Iterator;

import nexj.core.meta.soa.Argument;
import nexj.core.meta.soa.Attribute;
import nexj.core.meta.soa.Definition;
import nexj.core.meta.soa.EnumType;
import nexj.core.meta.soa.Interface;
import nexj.core.meta.soa.Method;
import nexj.core.meta.soa.ModelType;
import nexj.core.meta.soa.Result;
import nexj.core.scripting.Symbol;
import nexj.core.util.StringUtil;
import nexj.core.util.XMLWriter;

/**
 * Exports Definition metadata and related constructs to an output stream.
 */
public class XMLSOAMetadataExporter
{
   // associations

   /**
    * The output stream.
    */
   protected XMLWriter m_writer;

   // constructors

   /**
    * Creates the exporter with an XML print stream.
    * @param writer The output character stream.
    */
   public XMLSOAMetadataExporter(XMLWriter writer)
   {
      m_writer = writer;
   }

   // operations

   /**
    * Writes a definition to an XML print stream.
    * @param def The definition.
    * @param writer The XML print stream.
    * @throws IOException If an error occurs.
    */
   public static void export(Definition def, XMLWriter writer) throws IOException
   {
      XMLSOAMetadataExporter exporter = new XMLSOAMetadataExporter(writer);

      exporter.exportDefinition(def);
   }

   /**
    * Exports the specified Definition.
    * @param def The Definition to export.
    * @throws IOException if an error occurs.
    */
   public void exportDefinition(Definition def) throws IOException
   {
      m_writer.openElement("SOADefinition");

      if (!StringUtil.isEmpty(def.getName()))
      {
         m_writer.writeAttribute("name", def.getName());
      }

      if (!StringUtil.isEmpty(def.getVersion()))
      {
         m_writer.writeAttribute("version", def.getVersion());
      }

      if (!StringUtil.isEmpty(def.getDescription()))
      {
         m_writer.writeAttribute("description", def.getDescription());
      }

      m_writer.closeElement();

      m_writer.startElement("Service");

      if (def.getServiceInterfaceCount() > 0)
      {
         m_writer.startElement("Interfaces");

         boolean bFirst = true;

         for (Iterator it = def.getServiceInterfaceIterator(); it.hasNext();)
         {
            exportInterfaceRef((Interface)it.next(), bFirst);

            bFirst = false;
         }

         m_writer.endElement("Interfaces");
      }

      m_writer.endElement("Service");

      //interfaces
      if (def.getDefinedInterfaceCount() > 0)
      {
         m_writer.startElement("Interfaces");

         for (Iterator it = def.getDefinedInterfaceIterator(); it.hasNext();)
         {
            exportInterface((Interface)it.next(), def);
         }

         m_writer.endElement("Interfaces");
      }

      //enumerations
      if (def.getEnumCount() > 0)
      {
         m_writer.startElement("Enumerations");

         for (Iterator it = def.getEnumIterator(); it.hasNext();)
         {
            exportEnumeration((EnumType)it.next());
         }

         m_writer.endElement("Enumerations");
      }

      //types
      if (def.getTypeCount() > 0)
      {
         m_writer.startElement("Types");

         for (Iterator it = def.getTypeIterator(); it.hasNext();)
         {
            ModelType type = (ModelType)it.next();

            if (!type.isFault())
            {
               exportModelType(type, def);
            }
         }

         m_writer.endElement("Types");
         m_writer.startElement("Faults");

         for (Iterator it = def.getTypeIterator(); it.hasNext();)
         {
            ModelType type = (ModelType)it.next();

            if (type.isFault())
            {
               exportModelType(type, def);
            }
         }

         m_writer.endElement("Faults");
      }

      //diagrams {0, 1}
      //bindings {0, 1}
      //channels {0, 1}
      //integration interfaces {0, 1}
      //libraries {0, 1}
      //messages {0, 1}
      //integration services {0, 1}
      //transformations {0, 1}

      m_writer.endElement("SOADefinition");
   }

   // operations

   /**
    * @param enumType The enumeration to export.
    * @throws IOException if an error occurs.
    */
   private void exportEnumeration(EnumType enumType) throws IOException
   {
      m_writer.openElement("Enumeration");

      if (!StringUtil.isEmpty(enumType.getName()))
      {
         m_writer.writeAttribute("name", enumType.getName());
      }

      if (!StringUtil.isEmpty(enumType.getDescription()))
      {
         m_writer.writeAttribute("description", enumType.getDescription());
      }

      m_writer.closeElement();

      for (Iterator it = enumType.getItemIterator(); it.hasNext();)
      {
         Symbol item = (Symbol)it.next();

         m_writer.openElement("Item");
         m_writer.writeAttribute("name", item.toString());
         m_writer.closeEmptyElement();
      }

      m_writer.endElement("Enumeration");
   }

   /**
    * Exports the specified Argument.
    * @param arg The Argument to export.
    * @param def The enclosing definition.
    * @throws IOException if an error occurs.
    */
   private void exportArgument(Argument arg, Definition def) throws IOException
   {
      m_writer.openElement("Argument");

      if (!StringUtil.isEmpty(arg.getName()))
      {
         m_writer.writeAttribute("name", arg.getName());
      }

      if (arg.getType() != null)
      {
         m_writer.writeAttribute("type", def.getLocalName(arg.getType().getName()));
      }

      if (arg.isCollection())
      {
         m_writer.writeAttribute("collection", arg.isCollection());
      }

      if (!StringUtil.isEmpty(arg.getDescription()))
      {
         m_writer.writeAttribute("description", arg.getDescription());
      }

      m_writer.closeEmptyElement();
   }

   /**
    * Exports the specified ModelType.
    * @param type The ModelType to export.
    * @param def The enclosing definition.
    * @throws IOException if an error occurs.
    */
   private void exportModelType(ModelType type, Definition def) throws IOException
   {
      m_writer.openElement((type.isFault()) ? "Fault" : "Type");

      if (!StringUtil.isEmpty(type.getName()))
      {
         m_writer.writeAttribute("name", type.getName());
      }

      if (!StringUtil.isEmpty(type.getDescription()))
      {
         m_writer.writeAttribute("description", type.getDescription());
      }

      if (!StringUtil.isEmpty(type.getBases()))
      {
         m_writer.writeAttribute("bases", type.getBases());
      }

      if (type.getAttributeCount() > 0)
      {
         m_writer.closeElement();
         m_writer.startElement("Attributes");

         for (Iterator it = type.getAttributeIterator(); it.hasNext();)
         {
            exportAttribute((Attribute)it.next(), def);
         }

         m_writer.endElement("Attributes");
         m_writer.endElement((type.isFault()) ? "Fault" : "Type");
      }
      else
      {
         m_writer.closeEmptyElement();
      }
   }

   /**
    * Exports the specified Attribute.
    * @param  The Attribute to export.
    * @param def The enclosing definition.
    * @throws IOException if an error occurs.
    */
   private void exportAttribute(Attribute attr, Definition def) throws IOException
   {
      m_writer.openElement("Attribute");

      if (!StringUtil.isEmpty(attr.getName()))
      {
         m_writer.writeAttribute("name", attr.getName());
      }

      if (attr.getType() != null)
      {
         m_writer.writeAttribute("type", def.getLocalName(attr.getType().getName()));
      }

      if (attr.isRequired())
      {
         m_writer.writeAttribute("required", true);
      }

      if (attr.isCollection())
      {
         m_writer.writeAttribute("collection", true);
      }

      if (!StringUtil.isEmpty(attr.getDescription()))
      {
         m_writer.writeAttribute("description", attr.getDescription());
      }

      m_writer.closeEmptyElement();
   }

   /**
    * Exports the specified Interface.
    * @param iface The Interface to export.
    * @param def The enclosing definition.
    * @throws IOException if an error occurs.
    */
   private void exportInterface(Interface iface, Definition def) throws IOException
   {
      m_writer.openElement("Interface");

      if (!StringUtil.isEmpty(iface.getName()))
      {
         m_writer.writeAttribute("name", iface.getName());
      }

      if (!StringUtil.isEmpty(iface.getDescription()))
      {
         m_writer.writeAttribute("description", iface.getDescription());
      }

      m_writer.closeElement();

      for (Iterator it = iface.getMethodIterator(); it.hasNext();)
      {
         exportMethod((Method)it.next(), def);
      }

      m_writer.endElement("Interface");
   }

   /**
    * Exports the specified interface reference.
    * @param iface The interface referenced.
    * @param bDefault True if this interface is the default interface.
    * @throws IOException if an error occurs.
    */
   private void exportInterfaceRef(Interface iface, boolean bDefault) throws IOException
   {
      m_writer.openElement("Interface");

      if (!StringUtil.isEmpty(iface.getName()))
      {
         m_writer.writeAttribute("ref", iface.getName());
      }

      if (bDefault)
      {
         m_writer.writeAttribute("default", true);
      }

      m_writer.closeEmptyElement();
   }

   /**
    * Exports the specified Method.
    * @param method The Method to export.
    * @param def The enclosing definition.
    * @throws IOException if an error occurs.
    */
   private void exportMethod(Method method, Definition def) throws IOException
   {
      m_writer.openElement("Method");

      if (!StringUtil.isEmpty(method.getName()))
      {
         m_writer.writeAttribute("name", method.getName());
      }

      if (!StringUtil.isEmpty(method.getDescription()))
      {
         m_writer.writeAttribute("description", method.getDescription());
      }

      m_writer.closeElement();

      if (method.getArgCount() > 0)
      {
         m_writer.startElement("Arguments");

         for (Iterator it = method.getArgIterator(); it.hasNext();)
         {
            exportArgument((Argument)it.next(), def);
         }

         m_writer.endElement("Arguments");
      }

      if (method.getResult() != null)
      {
         exportResult(method.getResult(), def);
      }

      if (method.getFaultCount() > 0)
      {
         m_writer.startElement("Faults");

         for (Iterator it = method.getFaultIterator(); it.hasNext();)
         {
            // export fault refs
            m_writer.openElement("Fault");
            m_writer.writeAttribute("type", ((Symbol)it.next()).getName());
            m_writer.closeEmptyElement();
         }

         m_writer.endElement("Faults");
      }

      m_writer.endElement("Method");
   }

   /**
    * Exports the specified Result.
    * @param result The Result to export.
    * @param def The enclosing definition.
    * @throws IOException if an error occurs.
    */
   private void exportResult(Result result, Definition def) throws IOException
   {
      m_writer.openElement("Result");

      m_writer.writeAttribute("type", def.getLocalName(result.getType().getName()));

      if (result.isCollection())
      {
         m_writer.writeAttribute("collection", result.isCollection());
      }

      if (!StringUtil.isEmpty(result.getDescription()))
      {
         m_writer.writeAttribute("description", result.getDescription());
      }

      m_writer.closeEmptyElement();
   }

   /**
    * @return The XML writer.
    */
   public XMLWriter getWriter()
   {
      return m_writer;
   }
}
