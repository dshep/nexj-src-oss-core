// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;

/**
 * Set-accessor.
 */
public class Setter implements Function
{
   // attributes

   /**
    * The value offset.
    */
   protected final int m_nOffset;

   // constructors

   /**
    * Constructs the setter.
    * @param nOffset The value offset.
    */
   public Setter(int nOffset)
   {
      m_nOffset = nOffset;
   }

   // operations

   /**
    * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
    */
   public boolean invoke(int nArgCount, Machine machine)
   {
      Object value = machine.getArg(1, nArgCount);

      ((ObjectOriented)machine.getArg(0, nArgCount)).setValue(m_nOffset, value, machine);

      machine.returnValue(value, nArgCount);

      return false;
   }
}
