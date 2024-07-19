// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Exception indicating a mutipart data decoding failure.
 */
public class MultipartDataException extends MIMEDataException
{
   // constants
   
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 1727875334815310071L;

   // constructors
   
   public MultipartDataException()
   {
      super();
   }

   public MultipartDataException(String s)
   {
      super(s);
   }
}
