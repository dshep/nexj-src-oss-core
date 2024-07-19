// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.wsdl;

import java.util.Iterator;

import nexj.core.meta.Primitive;
import nexj.core.meta.integration.format.xml.NameResolver;
import nexj.core.meta.integration.format.xml.schema.Attribute;
import nexj.core.meta.integration.format.xml.schema.CompositeType;
import nexj.core.meta.integration.format.xml.schema.Element;
import nexj.core.meta.integration.format.xml.schema.Markup;
import nexj.core.meta.integration.format.xml.schema.PrimitiveType;
import nexj.core.meta.integration.format.xml.schema.Schema;
import nexj.core.meta.integration.format.xml.schema.SchemaItem;
import nexj.core.meta.integration.format.xml.schema.Type;
import nexj.core.meta.soa.Argument;
import nexj.core.meta.soa.Definition;
import nexj.core.meta.soa.EnumType;
import nexj.core.meta.soa.Interface;
import nexj.core.meta.soa.Method;
import nexj.core.meta.soa.ModelType;
import nexj.core.meta.soa.Result;
import nexj.core.scripting.Symbol;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Lookup;
import nexj.core.util.StringUtil;

/**
 * Converts web service definition to an SOA definition.
 */
public class ServiceSOAConverter
{
   // constants

   /**
    * Map of common type names to their type symbols.
    */
   protected final static Lookup s_commonTypeSymbolMap = new IdentityHashTab(Primitive.MAX_COUNT + 1); //Symbol[String]

   // associations

   /**
    * A map of types that have been imported
    */
   protected Lookup m_typeTypeMap = new IdentityHashTab(); // SOAObject[xml.schema.Type]

   static
   {
      s_commonTypeSymbolMap.put(PrimitiveType.ANY, Symbol.ANY);
      s_commonTypeSymbolMap.put(PrimitiveType.HEX, Symbol.define(Primitive.BINARY.getName()));
      s_commonTypeSymbolMap.put(PrimitiveType.BASE64, Symbol.define(Primitive.BINARY.getName()));
      s_commonTypeSymbolMap.put(PrimitiveType.DATETIME, Symbol.define(Primitive.TIMESTAMP.getName()));
      s_commonTypeSymbolMap.put(PrimitiveType.TIME, Symbol.define(Primitive.TIMESTAMP.getName()));
      s_commonTypeSymbolMap.put(PrimitiveType.DATE, Symbol.define(Primitive.TIMESTAMP.getName()));
      s_commonTypeSymbolMap.put(PrimitiveType.STRING, Symbol.define(Primitive.STRING.getName()));
      s_commonTypeSymbolMap.put(PrimitiveType.LONG, Symbol.define(Primitive.LONG.getName()));
      s_commonTypeSymbolMap.put(PrimitiveType.INTEGER, Symbol.define(Primitive.INTEGER.getName()));
      s_commonTypeSymbolMap.put(PrimitiveType.FLOAT, Symbol.define(Primitive.FLOAT.getName()));
      s_commonTypeSymbolMap.put(PrimitiveType.DOUBLE, Symbol.define(Primitive.DOUBLE.getName()));
      s_commonTypeSymbolMap.put(PrimitiveType.DECIMAL, Symbol.define(Primitive.DECIMAL.getName()));
      s_commonTypeSymbolMap.put(PrimitiveType.BOOLEAN, Symbol.define(Primitive.BOOLEAN.getName()));
   }

   // operations

   /**
    * Converts a web service definition to an SOA definition.
    * @param soapService The web service to convert.
    * @return An SOA definition for the web service.
    */
   public static Definition convert(SOAPService soapService)
   {
      return new ServiceSOAConverter().createDefinition(soapService);
   }

   /**
    * Creates an SOA definition from a web service definition.
    * @param soapService The web service definition.
    * @return An SOA definition representing the web service definition.
    */
   protected Definition createDefinition(SOAPService soapService)
   {
      Definition def = new Definition();

      def.setName(new NameResolver().generateName(soapService.getName()));

      Interface iface = def.defineInterface(def.getName());

      iface.setName(def.getName());

      // import each operation (and each input/output message associated with them)
      for (Iterator it = soapService.getOperationIterator(); it.hasNext();)
      {
         Method method = createMethod((Operation)it.next(), iface, def);

         iface.addMethod(method);
      }

      // import child types of types already imported
      for (Iterator it = soapService.getUniverse().getSchemaIterator(); it.hasNext();)
      {
         Schema schema = (Schema)it.next();

         for (Iterator jt = schema.getItemIterator(); jt.hasNext();)
         {
            SchemaItem item = (SchemaItem)jt.next();
            Type type;

            switch (item.getItemType())
            {
               case SchemaItem.COMPOSITE_TYPE:
               case SchemaItem.ENUM_TYPE:
               case SchemaItem.PRIMITIVE_TYPE:
               case SchemaItem.FORMAT_TYPE:
                  type = (Type)item;

                  if (type.getBase() != null)
                  {
                     if (m_typeTypeMap.get(type.getBase()) != null)
                     {
                        getSymbol(type, def);
                     }
                  }

               default:
                  break;
            }
         }
      }

      def.addServiceInterface(iface);

      return def;
   }

   /**
    * Creates an SOA method from a web services operation.
    * @param op The web services operation.
    * @param iface The SOA interface this method belongs to.
    * @param def The enclosing web service.
    * @return An SOA method representing the operation.
    */
   protected Method createMethod(final Operation op, final Interface iface, final Definition def)
   {
      final Method method = new Method(iface);
      StringBuilder description = new StringBuilder();

      // handle input message
      if (op.getInput() != null)
      {
         addArguments(method, op.getInput(), def);

         if (op.getInput().getRoot() != null)
         {
            if (!StringUtil.isEmpty(op.getInput().getRoot().getDescription()))
            {
               description.append(op.getInput().getRoot().getDescription());
            }
         }
      }

      // handle output message
      if (op.getOutput() != null)
      {
         method.setResult(createResult(op.getOutput(), def));

         if (op.getOutput().getRoot() != null)
         {
            if (!StringUtil.isEmpty(op.getOutput().getRoot().getDescription()))
            {
               if (description.length() > 0)
               {
                  description.append("\r\n");
               }

               description.append(op.getOutput().getRoot().getDescription());
            }
         }
      }

      method.setName(new NameResolver()
      {
         public boolean isValid(String sName)
         {
            return iface.getMethod(sName, method.getArgCount()) == null;
         }
      }.generateName(op.getName()));

      method.setDescription(description.toString());

      return method;
   }

   /**
    * Adds the arguments to an SOA method according to the structure of a web services message.
    *
    * If the root element is a composite type, arguments are added for each child of the
    * composite type. Otherwise, one argument is added to represent the entire root element.
    * @param method The SOA method.
    * @param msg The web services message.
    * @param def The enclosing web service.
    */
   protected void addArguments(Method method, Message msg, Definition def)
   {
      Element rootElement = msg.getRoot();

      // unwrap the message
      if (rootElement != null)
      {
         switch (rootElement.getType().getItemType())
         {
            case SchemaItem.COMPOSITE_TYPE:
               CompositeType rootType = (CompositeType)rootElement.getType();

               // for each element, create an argument
               for (int i = 0; i < rootType.getChildCount(); i++)
               {
                  addArgument(method, rootType.getChild(i), def);
               }
               break;

            case SchemaItem.PRIMITIVE_TYPE:
            case SchemaItem.FORMAT_TYPE:
            case SchemaItem.ENUM_TYPE:
               addArgument(method, rootElement, def);
               break;
         }
      }
   }

   /**
    * Creates an argument for a SOA method based on a schema element or attribute
    * and adds it to the method.
    * @param method The method.
    * @param child The element or attribute.
    * @param def The enclosing web service.
    */
   protected void addArgument(final Method method, Markup child, Definition def)
   {
      Argument arg = new Argument();

      arg.setName(new NameResolver()
      {
         public boolean isValid(String sName)
         {
            return method.findArgument(sName) == null;
         }
      }.generateName(child.getName()));

      arg.setDescription(child.getDescription());
      arg.setType(getSymbol(child.getType(), def));

      switch (child.getItemType())
      {
         case SchemaItem.ELEMENT:
         case SchemaItem.ELEMENT_REF:
            if (((Element)child).getMaxCount() != 1)
            {
               arg.setCollection(true);
            }
      }

      method.addArgument(arg);
   }

   /**
    * Creates a result for an SOA method from a web service message.
    *
    * If any of the following are true, the returned result will be null to indicate a void return type.
    *    1. The root element of the message is null
    *    2. The root element has no type
    *    3. The root element is a composite type with no children
    *
    * If the root element is a composite type with one child, the result will represent returning
    * the type of the child as the return type.
    *
    * If the root element is a composite type with multiple children, the result will represent
    * returning the type of the root element as the return type.
    *
    * If the root element is not a composite type, the result will represent returning the type of
    * the root element as the return type.
    *
    * @param msg The message.
    * @param def The enclosing web service.
    * @return A result created from the message or null indicating a void return type.
    */
   protected Result createResult(Message msg, Definition def)
   {
      Element rootElement = msg.getRoot();

      // unwrap the message
      if (rootElement == null || rootElement.getType() == null)
      {
         return null;
      }
      else if (rootElement.getType().getItemType() == SchemaItem.COMPOSITE_TYPE)
      {
         CompositeType rootType = (CompositeType)rootElement.getType();

         if (rootType.getChildCount() == 0)
         {
            return null;
         }
         else if (rootType.getChildCount() == 1)
         {
            Markup child = rootType.getChild(0);
            Result result = new Result();

            result.setDescription(child.getDescription());
            result.setType(getSymbol(child.getType(), def));

            if (child.getItemType() == SchemaItem.ELEMENT || child.getItemType() == SchemaItem.ELEMENT_REF)
            {
               result.setCollection(((Element)child).getMaxCount() != 1);
            }

            return result;
         }
         else
         {
            Result result = new Result();

            result.setDescription(rootElement.getDescription());
            result.setType(getSymbol(rootElement.getType(), def));

            return result;
         }
      }
      else // type PrimitiveType, EnumType, or FormatType
      {
         Result result = new Result();

         result.setDescription(rootElement.getType().getDescription());
         result.setType(getSymbol(rootElement.getType(), def));
         result.setCollection(rootElement.getMaxCount() != 1);

         return result;
      }
   }

   /**
    * Create an SOA attribute for an information model type from a schema attribute.
    * @param attr The schema attribute.
    * @param soaType The information model type this attribute applies to.
    * @param def The enclosing definition.
    * @return An attribute.
    */
   protected nexj.core.meta.soa.Attribute createAttribute(final Attribute attr, final ModelType soaType, final Definition def)
   {
      final nexj.core.meta.soa.Attribute soaAttr = new nexj.core.meta.soa.Attribute(soaType);

      soaAttr.setName(new NameResolver()
      {
         public boolean isValid(String sName)
         {
            return soaType.findAttribute(sName) == null;
         }
      }.generateName(attr.getName()));

      soaAttr.setDescription(attr.getDescription());
      soaAttr.setRequired(attr.isRequired());
      soaAttr.setType(getSymbol(attr.getAttributeType(), def));

      return soaAttr;
   }

   /**
    * Creates an attribute for an information model type referencing a type based on a schema element.
    * @param element The element.
    * @param def The enclosing definition.
    * @return An attribute.
    */
   protected nexj.core.meta.soa.Attribute createElementAtribute(Element element, final ModelType soaType, Definition def)
   {
      // add attribute that references this element
      nexj.core.meta.soa.Attribute attr = new nexj.core.meta.soa.Attribute(soaType);

      attr.setName(new NameResolver()
      {
         public boolean isValid(String sName)
         {
            return soaType.findAttribute(sName) == null;
         }
      }.generateName(element.getName()));

      attr.setType(getSymbol(element.getType(), def));
      attr.setDescription(element.getDescription());
      attr.setCollection(element.getMaxCount() != 1);
      attr.setRequired(element.getMinCount() > 0 && !element.isNillable());

      return attr;
   }

   /**
    * Creates an SOA enumeration from a schema enumeration.
    * @param type The schema enumeration.
    * @param def The enclosing web service.
    * @return An enumeration.
    */
   protected EnumType importEnum(nexj.core.meta.integration.format.xml.schema.EnumType type, final Definition def)
   {
      final EnumType soaEnum = new EnumType(def);

      soaEnum.setDescription(type.getDescription());
      soaEnum.setName(new NameResolver()
      {
         public boolean isValid(String sName)
         {
            return def.findEnum(sName) == null;
         }
      }.generateName(type.getName()));

      for (Iterator it = type.getValueIterator(); it.hasNext();)
      {
         soaEnum.addItem(new NameResolver()
         {
            public boolean isValid(String sName)
            {
               return soaEnum.getItem(sName) == null;
            }
         }.generateName(it.next().toString()));
      }

      def.addEnum(soaEnum);
      m_typeTypeMap.put(type, soaEnum);

      return soaEnum;
   }

   /**
    * Creates an information model type based on a schema type.
    * @param type The type.
    * @param def The enclosing web service.
    * @return The information model type created from the schema type.
    */
   protected ModelType importType(final CompositeType type, final Definition def)
   {
      final ModelType soaType = new ModelType(def);

      soaType.setName(new NameResolver()
      {
         public boolean isValid(String sName)
         {
            return def.findType(sName) == null;
         }
      }.generateName(type.getName()));

      soaType.setDescription(type.getDescription());

      Type base = type.getBase();

      if (base != null)
      {
         ModelType modelType = (ModelType)m_typeTypeMap.get(base);

         if (modelType == null)
         {
            modelType = importType((CompositeType)base, def);
         }

         soaType.addBase(modelType);
      }

      def.addType(soaType);
      m_typeTypeMap.put(type, soaType);

      for (int i = 0 ; i < type.getChildCount(); i++)
      {
         Markup child = type.getChild(i);

         switch (child.getItemType())
         {
            case SchemaItem.ATTRIBUTE:
            case SchemaItem.ATTRIBUTE_REF:
               soaType.addAttribute(createAttribute((Attribute)child, soaType, def));
               break;

            case SchemaItem.ELEMENT:
            case SchemaItem.ELEMENT_REF:
               soaType.addAttribute(createElementAtribute((Element)child, soaType, def));
               break;

            default:
               throw new IllegalStateException();
         }
      }

      return soaType;
   }

   /**
    * Returns a Symbol referring to the information model type created from a Schema Type.
    * @param type The schema type.
    * @return A Symbol.
    */
   protected Symbol getSymbol(Type type, Definition def)
   {
      switch (type.getItemType())
      {
         case SchemaItem.COMPOSITE_TYPE:
            ModelType modelType = (ModelType)m_typeTypeMap.get(type);

            if (modelType == null)
            {
               modelType = importType((CompositeType)type, def);
            }

            return Symbol.define(modelType.getName());

         case SchemaItem.ENUM_TYPE:
            EnumType enumType = (EnumType)m_typeTypeMap.get(type);

            if (enumType == null)
            {
               enumType = importEnum((nexj.core.meta.integration.format.xml.schema.EnumType)type, def);
            }

            return Symbol.define(enumType.getName());

         case SchemaItem.PRIMITIVE_TYPE:
            return getSymbol((PrimitiveType)type);

         case SchemaItem.FORMAT_TYPE:
            return (Symbol)s_commonTypeSymbolMap.get(type.getBase());

         default:
            throw new IllegalStateException();
      }
   }

   /**
    * Returns the symbol for the primitive type.
    * @param type The primitive type.
    * @return A symbol for an appropriate built-in type.
    */
   protected Symbol getSymbol(PrimitiveType type)
   {
      Type base = type;

      while (base.getBase() != null)
      {
         base = base.getBase();
      }

      return (Symbol)s_commonTypeSymbolMap.get(base);
   }
}
