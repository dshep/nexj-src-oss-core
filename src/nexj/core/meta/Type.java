//Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.util.Named;

/**
 * A representation of a data type.
 */
public interface Type extends Named
{
   /**
    * Determines if the type is primitive.
    * @return true if primitive.
    */
   public boolean isPrimitive();

   /**
    * @return The base type, or null if this is the root of a hierarchy.
    */
   public Type getBaseType();

   /**
    * Determines if a type can be upcast to obtain this type.
    * @param metaclass The type to upcast.
    * @return True if this class can be obtained by upcasting type.
    */
   public boolean isUpcast(Type type);

   /**
    * Converts a value to this type.
    * @param value The value to convert.
    * @return The converted value.
    * @throws TypeConversionException if the conversion fails.
    */
   public Object convert(Object value) throws TypeConversionException;
}
