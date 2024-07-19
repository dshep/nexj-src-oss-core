// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import nexj.core.rpc.xml.XML;
import nexj.core.util.HashTab2D;
import nexj.core.util.ObjUtil;

/**
 * A singleton representation of the XML Schema namespace.
 */
public final class XSDSchema extends Schema
{
   // constants

   /**
    * xsd:anyType
    */
   public final static PrimitiveType ANY = new PrimitiveType("anyType");

   /**
    * xsd:base64Binary
    */
   public final static PrimitiveType BASE64_BINARY = new PrimitiveType("base64Binary");

   /**
    * xsd:boolean
    */
   public final static PrimitiveType BOOLEAN = new PrimitiveType("boolean");

   /**
    * xsd:date
    */
   public final static PrimitiveType DATE = new PrimitiveType("date");

   /**
    * xsd:dateTime
    */
   public final static PrimitiveType DATETIME = new PrimitiveType("dateTime");

   /**
    * xsd:decimal
    */
   public final static PrimitiveType DECIMAL = new PrimitiveType("decimal");

   /**
    * xsd:double
    */
   public final static PrimitiveType DOUBLE = new PrimitiveType("double");

   /**
    * xsd:float
    */
   public final static PrimitiveType FLOAT = new PrimitiveType("float");

   /**
    * xsd:hexBinary
    */
   public final static PrimitiveType HEX = new PrimitiveType("hexBinary");

   /**
    * xsd:int
    */
   public final static PrimitiveType INTEGER = new PrimitiveType("int");

   /**
    * xsd:long
    */
   public final static PrimitiveType LONG = new PrimitiveType("long");

   /**
    * xsd:string
    */
   public final static PrimitiveType STRING = new PrimitiveType("string");

   /**
    * xsd:time
    */
   public final static PrimitiveType TIME = new PrimitiveType("time");

   /**
    * The singleton instance.
    */
   public final static XSDSchema XSD = new XSDSchema();

   // constructors

   /**
    * Constructs a new representation of XML Schema.
    */
   private XSDSchema()
   {
      super(XML.XSD_URI);
      m_sPreferredPrefix = "xsd";

      Field[] fieldArray = XSDSchema.class.getDeclaredFields();
      final int PUBLIC_FINAL_STATIC = Modifier.PUBLIC | Modifier.FINAL | Modifier.STATIC;

      try
      {
         m_itemMap = new HashTab2D(fieldArray.length);

         for (int i = 0; i < fieldArray.length; i++)
         {
            Field field = fieldArray[i];

            if (field.getType() == PrimitiveType.class && (field.getModifiers() ^ PUBLIC_FINAL_STATIC) == 0)
            {
               super.addItem((SchemaItem)field.get(null));
            }
         }
      }
      catch (IllegalAccessException ex)
      {
         ObjUtil.rethrow(ex);
      }
   }

   /**
    * @see nexj.core.meta.integration.format.xml.schema.Schema#addItem(nexj.core.meta.integration.format.xml.schema.SchemaItem)
    */
   public void addItem(SchemaItem item)
   {
      throw new UnsupportedOperationException("Immutable");
   }

   /**
    * @see nexj.core.meta.integration.format.xml.schema.Schema#setPreferredPrefix(java.lang.String)
    */
   public void setPreferredPrefix(String sPrefix)
   {
      throw new UnsupportedOperationException("Immutable");
   }
}
