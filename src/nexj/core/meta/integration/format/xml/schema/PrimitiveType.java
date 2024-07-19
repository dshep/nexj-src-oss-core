// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;

import nexj.core.meta.Primitive;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Lookup;

/**
 * Definition of a primitive type.
 */
public final class PrimitiveType extends AtomicType
{
   // constants

   /**
    * Binary represented as a hexadecmial string.
    */
   public final static PrimitiveType HEX = new PrimitiveType(Primitive.BINARY.getName() + ".hex");

   /**
    * Binary represented as a Base64-encoded string.
    */
   public final static PrimitiveType BASE64 = new PrimitiveType(Primitive.BINARY.getName() + ".base64");

   /**
    * Timestamp with both date and time parts.
    */
   public final static PrimitiveType DATETIME = new PrimitiveType(Primitive.TIMESTAMP.getName() + ".dateTime");

   /**
    * Timestamp with time part only.
    */
   public final static PrimitiveType TIME = new PrimitiveType(Primitive.TIMESTAMP.getName() + ".time");

   /**
    * Timestamp with date part only.
    */
   public final static PrimitiveType DATE = new PrimitiveType(Primitive.TIMESTAMP.getName() + ".date");

   /**
    * String type.
    */
   public final static PrimitiveType STRING = new PrimitiveType(Primitive.STRING.getName());

   /**
    * Long integer type.
    */
   public final static PrimitiveType LONG = new PrimitiveType(Primitive.LONG.getName());

   /**
    * Short integer type.
    */
   public final static PrimitiveType INTEGER = new PrimitiveType(Primitive.INTEGER.getName());

   /**
    * Floating point type.
    */
   public final static PrimitiveType FLOAT = new PrimitiveType(Primitive.FLOAT.getName());

   /**
    * Double-width floating point type.
    */
   public final static PrimitiveType DOUBLE = new PrimitiveType(Primitive.DOUBLE.getName());

   /**
    * Arbitrary precision decimal number type.
    */
   public final static PrimitiveType DECIMAL = new PrimitiveType(Primitive.DECIMAL.getName());

   /**
    * Boolean type.
    */
   public final static PrimitiveType BOOLEAN = new PrimitiveType(Primitive.BOOLEAN.getName());

   /**
    * Any type.
    */
   public final static PrimitiveType ANY = new PrimitiveType(Primitive.ANY.getName());

   // associations

   /**
    * Map of primitive types to their primitives: Primitive[PrimitiveType].
    */
   protected final static Lookup s_typePrimitiveMap = new IdentityHashTab(13);

   static
   {
      s_typePrimitiveMap.put(HEX, Primitive.BINARY);
      s_typePrimitiveMap.put(BASE64, Primitive.BINARY);
      s_typePrimitiveMap.put(DATETIME, Primitive.TIMESTAMP);
      s_typePrimitiveMap.put(TIME, Primitive.TIMESTAMP);
      s_typePrimitiveMap.put(DATE, Primitive.TIMESTAMP);
      s_typePrimitiveMap.put(STRING, Primitive.STRING);
      s_typePrimitiveMap.put(LONG, Primitive.LONG);
      s_typePrimitiveMap.put(INTEGER, Primitive.INTEGER);
      s_typePrimitiveMap.put(FLOAT, Primitive.FLOAT);
      s_typePrimitiveMap.put(DOUBLE, Primitive.DOUBLE);
      s_typePrimitiveMap.put(DECIMAL, Primitive.DECIMAL);
      s_typePrimitiveMap.put(BOOLEAN, Primitive.BOOLEAN);
      s_typePrimitiveMap.put(ANY, Primitive.ANY);
   }

   // constructors

   /**
    * Constructs a new primitive type definition.
    * @param sName The type name.
    */
   public PrimitiveType(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * @see nexj.core.meta.integration.format.xml.schema.SchemaItem#getItemType()
    */
   public byte getItemType()
   {
      return PRIMITIVE_TYPE;
   }

   /**
    * Gets the primitive of one of the pre-defined primitive types.
    * @param type The pre-defined primitive type.
    * @return The primitive.
    */
   public static Primitive getPrimitive(PrimitiveType type)
   {
      return (Primitive)s_typePrimitiveMap.get(type);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "PrimitiveType(" + ((m_sName == null) ? "ANONYMOUS" : "\"" + m_sName + "\"") + ')';
   }
}
