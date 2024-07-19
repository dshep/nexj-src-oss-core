// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;

/**
 * Exception thrown when an IO size limit has been exceeded.
 */
public class LimitIOException extends IOException
{
   // constants
   
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 2565970811483356700L;
   
   // constructors

   public LimitIOException()
   {
      super();
   }

   public LimitIOException(String s)
   {
      super(s);
   }
}
