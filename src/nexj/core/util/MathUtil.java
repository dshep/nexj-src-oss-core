// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Numerical helper functions.
 */
public class MathUtil
{
   // constructors

   /**
    * Prevents construction.
    */
   protected MathUtil()
   {
   }

   // operations

   /**
    * Rounds up an integer to the next higher power of 2.
    * @param n The integer to round up.
    * @return The next higher power of 2.
    */
   public static int ceil2(int n)
   {
      --n;
      n |= n >> 1;
      n |= n >> 2;
      n |= n >> 4;
      n |= n >> 8;
      n |= n >> 16;

      return n + 1; 
   }
}
