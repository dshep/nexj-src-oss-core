// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

/**
 * Interface for invoking a function.
 */
public interface Function
{
   /**
    * Invokes a function from the virtual machine. The function should either
    * compute its value immediately and push it on the VM stack, or setup the
    * VM for executing its code, if it is a byte code function.
    * 
    * @param nArgCount The number of arguments pushed on the VM stack.
    * @param machine The virtual machine instance.
    * @return True if the virtual machine registers should be reset.
    */
   public boolean invoke(int nArgCount, Machine machine);
}
