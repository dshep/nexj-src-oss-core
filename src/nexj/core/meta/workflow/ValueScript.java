// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.scripting.Pair;

/**
 * Step argument represented by a Scheme script.
 */
public class ValueScript extends ValueExpression
{
   // constructors

   /**
    * @param script The script.
    */
   public ValueScript(Pair script)
   {
      super(script);
   }

   // operations

   /**
    * @see nexj.core.meta.workflow.ValueExpression#getBody()
    */
   protected Pair getBody()
   {
      return (Pair)m_value;
   }
}
