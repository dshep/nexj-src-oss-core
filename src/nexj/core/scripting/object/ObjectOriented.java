// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;

/**
 * Interface implemented by any object instance.
 */
public interface ObjectOriented extends Function
{
   /**
    * @return The class object.
    */
   ClassObject getClassObject();

   /**
    * Initializes the object after creation.
    * @param machine The VM for initialization.
    */
   void initialize(Machine machine);

   /**
    * Sets an attribute value at the specified offset.
    * @param nOffset The value offset.
    * @param value The attribute value.
    * @param machine The VM.
    */
   void setValue(int nOffset, Object value, Machine machine);

   /**
    * Gets an attribute value at the specified offset.
    * @param nOffset The value offset.
    * @param machine The VM.
    */
   Object getValue(int nOffset, Machine machine);
}
