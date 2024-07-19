// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import nexj.core.util.Binary;
import nexj.core.util.PropertyMap;

/**
 * Property map with serialization interface.
 */
public interface SerializablePropertyMap extends PropertyMap
{
   /**
    * Serializes the values to a string.
    * @param context The invocation context.
    * @return The serialized value.
    */
   String serializeValues(InvocationContext context);

   /**
    * Serializes the values to a binary
    * @param writer The writer.
    * @param context The invocation context.
    */
   Binary serializeValuesToBinary(InvocationContext context);

   /**
    * Deserializes the values from a sequence.
    * @param serializedValues the string or binary sequence.
    * @param context The invocation context.
    */
   void deserializeValues(Object serializedValues, InvocationContext context);
}
