// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.service;

import nexj.core.meta.workflow.Code;
import nexj.core.meta.workflow.Redirector;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Step for jumping to another step.
 */
public class Jump extends Code implements Redirector
{
   // constants

   /**
    * The state management function body.
    */
   public final static Pair BODY = new ConstPair(Symbol.THIS);

   // constructors

   /**
    * Constructs the step.
    * @param sName The step name.
    */
   public Jump(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the step.
    */
   public Jump()
   {
      super();
   }

   // operations
   
   /**
    * @see nexj.core.meta.workflow.Step#isDependent()
    */
   public boolean isDependent()
   {
      return true;
   }

   /**
    * @see nexj.core.meta.workflow.Code#getBody()
    */
   protected Pair getBody()
   {
      return BODY;
   }
}
