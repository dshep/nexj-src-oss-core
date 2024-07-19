// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.security.SecureRandom;

/**
 * Utilities for random number generation.
 */
public class RandUtil
{
   // associations
   
   /**
    * The shared secure random generator instance.
    */
   private final static SecureRandom s_secureRandom = new SecureRandom();
   
   static
   {
      s_secureRandom.nextInt();
   };
   
   // constructors
   
   /**
    * Prevents construction.
    */
   protected RandUtil()
   {
   }
   
   // operations
   
   /**
    * @return An initialized secure random generator instance.
    */
   public static SecureRandom getSecureRandom()
   {
      return s_secureRandom;
   }
}
