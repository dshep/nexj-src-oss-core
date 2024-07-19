// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

/**
 * Exception thrown by the (error ...) function.
 */
public class ScriptingError extends ScriptingException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 1806343032481072987L;

   // constructors

   public ScriptingError(String errCode)
   {
      super(errCode);
   }

   public ScriptingError(String errCode, Object[] argArray)
   {
      super(errCode, argArray);
   }

   public ScriptingError(String errCode, Throwable cause)
   {
      super(errCode, cause);
   }

   public ScriptingError(String errCode, Object[] argArray, Throwable cause)
   {
      super(errCode, argArray, cause);
   }

   // operations

   /**
    * @see nexj.core.util.ErrorCode#isSystem()
    */
   public boolean isSystem()
   {
      return m_sErrCode == null || !m_sErrCode.startsWith("err.");
   }
}
