// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.scripting.ScriptingException;

/**
 * Exception indicating an incorrect method invocation.
 */
public class InvocationException extends ScriptingException
{
   // constants

   /**
    * Serialization UID.
    */
   private static final long serialVersionUID = -540647756931727748L;

   // constructors

   public InvocationException(String sErrCode)
   {
      super(sErrCode);
   }

   public InvocationException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public InvocationException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public InvocationException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
