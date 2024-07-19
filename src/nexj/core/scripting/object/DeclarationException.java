// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.scripting.ScriptingException;

/**
 * Exception indicating incorrect class declaration.
 */
public class DeclarationException extends ScriptingException
{
   // constants

   /**
    * Serialization UID.
    */
   private static final long serialVersionUID = -5159609003044563375L;

   // constructors

   public DeclarationException(String sErrCode)
   {
      super(sErrCode);
   }

   public DeclarationException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public DeclarationException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public DeclarationException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
