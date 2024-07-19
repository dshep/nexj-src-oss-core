// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;

/**
 * Get-accessor.
 */
public class Getter implements Function
{
   // attributes

   /**
    * The value offset.
    */
   protected final int m_nOffset;

   // constructors

   /**
    * Constructs the getter.
    * @param nOffset The value offset.
    */
   public Getter(int nOffset)
   {
      m_nOffset = nOffset;
   }

   // operations

   /**
    * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
    */
   public boolean invoke(int nArgCount, Machine machine)
   {
      machine.returnValue(((ObjectOriented)machine.getArg(0, nArgCount)).getValue(m_nOffset, machine), nArgCount);

      return false;
   }
}
