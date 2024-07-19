// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import nexj.core.util.ErrorCode;
import nexj.core.util.ObjUtil;

/**
 * Context utility functions.
 */
public class ContextUtil
{
   // constructors

   /**
    * Prevents construction.
    */
   protected ContextUtil()
   {
   }

   // operations

   /**
    * Extracts a message from a throwable.
    * @param t The throwable.
    * @param context The context. Can be null.
    * @return The extracted message.
    */
   public static String getMessage(Throwable t, Context context)
   {
      if (context != null && t instanceof ErrorCode)
      {
         ErrorCode ec = (ErrorCode)t;

         return context.formatString(ec.getErrorCode(), ec.getErrorArgs());
      }

      return ObjUtil.getMessage(t);
   }
}
