// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;


/**
 * A type definition.
 */
public abstract class GenericType extends NamedMetadataObject implements Type
{
   // constructors

   /**
    * Creates a type with a given name.
    * @param sName The type name.
    */
   public GenericType(String sName)
   {
      super(sName);
   }
   
   /**
    * Creates a type with a null name.
    */
   protected GenericType()
   {
      super();
   }

   // operations

   /**
    * @see nexj.core.meta.Type#isPrimitive()
    */
   public abstract boolean isPrimitive();

   /**
    * @see nexj.core.meta.Type#getBaseType()
    */
   public abstract Type getBaseType();

   /**
    * @see nexj.core.meta.Type#convert(java.lang.Object)
    */
   public Object convert(Object value) throws TypeConversionException
   {
      if (value == null)
      {
         return null;
      }

      if (value instanceof Typed)
      {
         Type type = ((Typed)value).getType();

         if (type.isUpcast(this) || this.isUpcast(type))
         {
            return value;
         }
      }

      throw new TypeConversionException(this);
   }

   /**
    * @see nexj.core.meta.Type#isUpcast(nexj.core.meta.Type)
    */
   public boolean isUpcast(Type type)
   {
      while (type != null)
      {
         if (getName().equals(type.getName()))
         {
            return isPrimitive() == type.isPrimitive();
         }

         type = type.getBaseType();
      }
      
      return false;
   }

   /**
    * Determines the type of a given value.
    * @param value The value of which to determine the type.
    * @return The value type (null if value is null).
    * @throws MetadataException if the type has not been recognized.
    */
   public static Type typeOf(Object value)
   {
      if (value instanceof Typed)
      {
         return ((Typed)value).getType();
      }

      return Primitive.primitiveOf(value);
   }
}
