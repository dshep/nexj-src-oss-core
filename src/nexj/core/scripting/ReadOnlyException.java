// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;


/**
 * Exception indicating that an attempt to modify read-only state has been made.
 */
public class ReadOnlyException extends ScriptingException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -7360355828668863684L;

   // constructors

   public ReadOnlyException()
   {
      super("err.scripting.readOnlyEnv");
   }

   public ReadOnlyException(String sErrCode)
   {
      super(sErrCode);
   }

   public ReadOnlyException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }
}
