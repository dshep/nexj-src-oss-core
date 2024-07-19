// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;

/**
 * Exception indicating an error during serialization.
 */
public class SerializationException extends IOException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 8581434542128415368L;

   // constructors
   
   public SerializationException(String sErrCode)
   {
      super(sErrCode);
   }

   public SerializationException(String sErrCode, Throwable cause)
   {
      super(sErrCode);
      initCause(cause);
   }
}
