// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;

/**
 * Exception indicating MIME data decoding failure.
 */
public class MIMEDataException extends IOException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -4787997494922502400L;

   // constructors

   public MIMEDataException()
   {
      super();
   }

   public MIMEDataException(String s)
   {
      super(s);
   }
}
