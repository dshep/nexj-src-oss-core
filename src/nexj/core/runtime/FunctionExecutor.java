// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.io.Serializable;

/**
 * A command that executes a Scheme function.
 * @deprecated
 * FunctionExecutor must not be used. It is here for backward compatibility.
 */
public class FunctionExecutor extends TrustedExecutable implements Serializable
{
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -6540349061564467099L;

   public FunctionExecutor()
   {
      throw new UnsupportedOperationException("FunctionExecutor must not be used. It is here for backward compatibility.");
   }

   /**
    * @see nexj.core.runtime.TrustedExecutable#executeTrusted()
    */
   protected void executeTrusted()
   {
      throw new UnsupportedOperationException("FunctionExecutor must not be used. It is here for backward compatibility.");
   }
}
