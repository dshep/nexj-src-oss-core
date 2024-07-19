// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

/**
 * Byte code function implementing the additional Macro interface.
 */
public class PCodeMacro extends PCodeFunction implements Macro
{
   // constants

   /**
    * The Java stream unique identifier for the class.
    */
   private final static long serialVersionUID = 796030935269562523L;

   // constructors
   
   /**
    * Constructs the p-code macro.
    */
   public PCodeMacro()
   {
   }
   
   /**
    * Constructs the p-code macro.
    * @param code The p-code array.
    * @param constants The constant array.
    * @param frame The closure frame.
    */
   public PCodeMacro(char[] code, Object[] constants, Object[] frame)
   {
      super(code, constants, frame);
   }

   // operations

   /**
    * @see nexj.core.scripting.PCodeFunction#bind(java.lang.Object[])
    */
   public PCodeFunction bind(Object[] frame)
   {
      return new PCodeMacro(this.code, this.constants, frame);
   }
   
   /**
    * @see nexj.core.scripting.PCodeFunction#create(char[], Object[], Object[])
    */
   public PCodeFunction create(char[] code, Object[] constants, Object[] frame)
   {
      return new PCodeMacro(code, constants, frame);
   }
}
