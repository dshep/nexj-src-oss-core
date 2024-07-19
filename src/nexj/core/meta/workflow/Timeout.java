// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.meta.Primitive;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Flow timeout setup.
 */
public class Timeout extends Code
{
   // associations
   
   /**
    * The timeout value expression.
    */
   protected Object m_value;

   // constructors
   
   /**
    * Constructs the timeout.
    * @param sName The step name.
    */
   public Timeout(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the timeout. 
    */
   public Timeout()
   {
      super();
   }

   // operations

   /**
    * Sets the timeout value expression.
    * @param value The timeout value expression to set.
    */
   public void setValue(Object value)
   {
      verifyNotReadOnly();
      m_value = value;
   }

   /**
    * @return The timeout value expression.
    */
   public Object getValue()
   {
      return m_value;
   }

   /**
    * @see nexj.core.meta.workflow.Code#getBody()
    */
   protected Pair getBody()
   {
      // (lambda (this) (:flow'addTimer <nOrdinal> <value>))
      return Pair.list(Pair.list(Symbol._FLOW, Pair.quote(Symbol.ADDTIMER),
         Primitive.createInteger(m_nOrdinal), m_value));
   }
}
