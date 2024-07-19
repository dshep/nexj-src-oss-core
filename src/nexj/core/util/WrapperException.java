// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Runtime exception wrapping another exception (usually checked). 
 */
public class WrapperException extends RuntimeException
{
   // constants

   /**
    * The serialization UID.
    */
   private final static long serialVersionUID = -811047060622542853L;

   // constructors

   /**
    * Constructs the exception.
    * @param t The original exception.
    */
   public WrapperException(Throwable t)
   {
      super(ObjUtil.getMessage(t), t);
   }
}
