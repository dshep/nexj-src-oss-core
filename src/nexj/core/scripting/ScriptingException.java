// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import nexj.core.util.UncheckedException;

/**
 * Base class for all scripting exceptions.
 */
public class ScriptingException extends UncheckedException
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = -6781596246942788091L;

   // constructors

   public ScriptingException(String sErrCode)
   {
      super(sErrCode);
   }

   public ScriptingException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public ScriptingException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public ScriptingException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
