// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;

/**
 * Exception indicating a Base64 decoding failure.
 */
public class Base64Exception extends IOException
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = -2590731699799917597L;

   // constructors
   
   public Base64Exception(String s)
   {
      super(s);
   }
}
