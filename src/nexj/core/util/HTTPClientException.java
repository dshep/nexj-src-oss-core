// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;

/**
 * Exception thrown when an HTTP client-side error occurs. 
 */
public class HTTPClientException extends IOException
{
   // constants
   
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 5295814227791280294L;

   // constructors

   public HTTPClientException()
   {
      super();
   }

   public HTTPClientException(String s)
   {
      super(s);
   }
   
   public HTTPClientException(String s, Throwable t)
   {
      super(s);
      initCause(t);
   }
}
