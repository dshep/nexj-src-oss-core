// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.service;

import nexj.core.integration.Transformer;
import nexj.core.meta.Metadata;
import nexj.core.meta.workflow.FunctionStep;
import nexj.core.meta.workflow.State;
import nexj.core.meta.workflow.ValueExpression;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Message transformation step.
 */
public class Transform extends FunctionStep
{
   // constants

   /**
    * The send step arguments.
    */
   protected final static Pair ARGUMENTS = new ConstPair(Symbol.THIS, new ConstPair(Symbol._STATE));

   /**
    * Arguments required to evaluate the transformation name and argument expression.
    */
   private final static Pair TRANSFORM_ARGUMENTS = new ConstPair(Symbol.THIS);

   // associations

   /**
    * The transformation expression.
    */
   private ValueExpression m_transformation;

   /**
    * The transformation arguments expression.
    */
   protected ValueExpression m_arguments;

   // constructors

   /**
    * Constructs the step.
    * @param sName The step name.
    */
   public Transform(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the step.
    */
   public Transform()
   {
      super();
   }

   // operations

   /**
    * Sets the expression that evaluates to the transformation name.
    * @param expression The expression that evaluates to the transformation name to set.
    */
   public void setTransformationExpression(Object expression)
   {
      verifyNotReadOnly();
      m_transformation = new ValueExpression(expression);
   }

   /**
    * Gets the expression that evaluates to the transformation name.
    * @return The expression that evaluates to the transformation name.
    */
   public Object getTransformationExpression()
   {
      return (m_transformation != null) ? m_transformation.getValue() : null;
   }

   /**
    * Sets the transformation arguments.
    * @param expression A list of expressions that evaluate to the transformation arguments;
    * may be null.
    */
   public void setTransformationArguments(Object expressions)
   {
      verifyNotReadOnly();
      m_arguments = new ValueExpression(new Pair(Symbol.LIST, expressions));
   }

   /**
    * Gets the transformation arguments.
    * @return The list of expressions that evaluate to the transformation arguments; may be null.
    */
   public Object getTransformationArguments()
   {
      return (m_arguments == null) ? null : ((Pair)m_arguments.getValue()).getTail();
   }

   /**
    * @see nexj.core.meta.workflow.Step#generate(nexj.core.scripting.Machine)
    */
   public void generate(Machine machine)
   {
      super.generate(machine);
      m_transformation.compile(this, machine, TRANSFORM_ARGUMENTS);
      m_arguments.compile(this, machine, TRANSFORM_ARGUMENTS);
   }

   /**
    * @see nexj.core.meta.workflow.FunctionStep#getFunction()
    */
   protected Function getFunction()
   {
      return new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            assert nArgCount == 2;

            TransferObject tobj = (TransferObject)machine.getArg(0, nArgCount);
            State state = (State)machine.getArg(1, nArgCount);

            machine.returnValue(new Transformer((InvocationContext)machine.getContext())
               .transform(tobj, ((Metadata)machine.getContext().getContextMetadata())
                  .getTransformation((String)m_transformation.invoke(machine, state, new Object[] {tobj})),
                  (Pair)m_arguments.invoke(machine, state, new Object[] {tobj})), nArgCount);

            return false;
         }
      };
   }

   /**
    * @return The function arguments.
    */
   protected Pair getArguments()
   {
      return ARGUMENTS;
   }
}
