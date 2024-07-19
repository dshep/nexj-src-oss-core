package nexj.core.scripting.object;

import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;

/**
 * Set-accessor with interceptor function invocation.
 */
public class InterceptingSetter extends Setter
{
   // associations

   /**
    * The set interceptor.
    */
   protected Function m_interceptor;

   // constructors

   /**
    * Constructs the setter.
    * @param nOffset The value offset.
    * @param interceptor The set interceptor.
    */
   public InterceptingSetter(int nOffset, Function interceptor)
   {
      super(nOffset);
      m_interceptor = interceptor;
   }

   // operations

   /**
    * @see nexj.core.scripting.object.Setter#invoke(int, nexj.core.scripting.Machine)
    */
   public boolean invoke(int nArgCount, Machine machine)
   {
      ObjectOriented obj = (ObjectOriented)machine.getArg(0, nArgCount);
      Object value = machine.getArg(1, nArgCount);

      value = machine.invoke(m_interceptor, obj, value, null);
      obj.setValue(m_nOffset, value, machine);

      machine.returnValue(value, nArgCount);

      return false;
   }
}
