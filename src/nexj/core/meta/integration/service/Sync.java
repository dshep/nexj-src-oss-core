// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.service;

import nexj.core.integration.MessageFormatter;
import nexj.core.integration.format.object.ObjectMessageFormatter;
import nexj.core.meta.workflow.ValueExpression;
import nexj.core.meta.workflow.State;
import nexj.core.meta.workflow.ValueScript;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Object persistence step.
 */
public class Sync extends Persist
{
   // constants

   /**
    * The persist step arguments.
    */
   protected final static Pair ARGUMENTS = new ConstPair(Symbol.THIS, new ConstPair(Symbol._STATE));

   /**
    * The conflict resolution algorithm arguments.
    */
   protected final static Pair SYNC_SCRIPT_ARGUMENTS = Pair.list(Symbol.THIS, Symbol.OLD,
      Symbol.INSTANCE, Symbol.define("syncObject"), Symbol.define("conflicts"));

   /**
    * Arguments required to evaluate the synchronization link
    */
   private final static Pair SYNC_LINK_ARGUMENTS = new ConstPair(Symbol.THIS);

   /**
    * Symbol for the function that evaluates to SysSyncLink instance
    */
   private final static Symbol SYNC_LINK = Symbol.define("sync-link");

   // attributes

   /**
    * The Scheme expression that evaluates to the sync link instance.
    */
   protected ValueExpression m_syncLinkExpr;

   /**
    * The Scheme script executed by the Sync step (hook)
    */
   protected ValueExpression m_syncScriptExpr;

   // constructors

   /**
    * Constructs the step.
    * @param sName The step name.
    * @param syncLink The Scheme expression that evaluates to the synchronization link.
    * @param syncScript The Scheme script executed by the Sync step (hook).
    * @param nOnError The exception handling strategy.
    */
   public Sync(String sName, Object syncLink, Pair syncScript, byte nOnError)
   {
      super(sName, nOnError);

      m_syncLinkExpr = new ValueExpression(syncLink);
      m_syncScriptExpr = new ValueScript(syncScript);
   }

   // operations

   /**
    * Sets the Scheme expression that evaluates to the synchronization link.
    * @param expression The Scheme expression that evaluates to the sync link to set.
    */
   public void setSyncLink(Object expression)
   {
      verifyNotReadOnly();
      m_syncLinkExpr = new ValueExpression(expression);
   }

   /**
    * @return The Scheme expression that evaluates to the synchronization link.
    */
   public Object getSyncLink()
   {
      return (m_syncLinkExpr != null) ? m_syncLinkExpr.getValue() : null;
   }

   /**
    * Sets the Scheme script executed by the Sync step (hook)
    * @param expression The Scheme script executed by the Sync step (hook)
    */
   public void setSyncScript(Pair expression)
   {
      verifyNotReadOnly();
      m_syncScriptExpr = new ValueScript(expression);
   }

   /**
    * @return The Scheme script executed by the Sync step (hook)
    */
   public Object getSyncScript()
   {
      return (m_syncScriptExpr != null) ? m_syncScriptExpr.getValue() : null;
   }

   /**
    * @see nexj.core.meta.workflow.Step#generate(nexj.core.scripting.Machine)
    */
   public void generate(Machine machine)
   {
      super.generate(machine);
      m_syncLinkExpr.compile(this, machine, SYNC_LINK_ARGUMENTS);
      m_syncScriptExpr.compile(this, machine, SYNC_SCRIPT_ARGUMENTS);
   }

   /**
    * @see nexj.core.meta.workflow.FunctionStep#getArguments()
    */
   protected Pair getArguments()
   {
      return ARGUMENTS;
   }

   /**
    * @see nexj.core.meta.workflow.Persist#configureFormatter()
    */
   protected Object configureFormatter(MessageFormatter messageFormatter, int nArgCount, Machine machine)
   {
      TransferObject tobj = (TransferObject)machine.getArg(0, nArgCount);
      State state = (State) machine.getArg(1, nArgCount);
      final Instance syncLink = (Instance)m_syncLinkExpr.invoke(machine, state, new Object[] {tobj});

      ((ObjectMessageFormatter)messageFormatter).initFormatterStrategy(new Link(syncLink, m_syncScriptExpr, nArgCount,
         machine));

      Object oldValue = machine.getGlobalEnvironment().findVariable(SYNC_LINK);

      machine.getGlobalEnvironment().defineVariable(SYNC_LINK,
         new Function()
         {
            public boolean invoke(int nArgCount, Machine machine)
            {
               assert nArgCount == 0;

               machine.returnValue(syncLink, nArgCount);

               return false;
            }
         });

      return oldValue;
   }

   /**
    * @see nexj.core.meta.workflow.Persist#cleanUp()
    */
   protected void cleanUp(Machine machine, Object oldValue)
   {
      if (oldValue == null)
      {
         machine.getGlobalEnvironment().removeVariable(SYNC_LINK);
      }
      else
      {
         machine.getGlobalEnvironment().defineVariable(SYNC_LINK, oldValue);
      }
   }

   /**
    * @see nexj.core.meta.workflow.Code#getPCode()
    */
   public PCodeFunction getPCode()
   {
      return m_syncScriptExpr.getPCode();
   }

   // inner classes

   public static class Link
   {
      /**
       * The script actual argument count.
       */
      private final int m_nArgCount;

      /**
       * The scripting VM.
       */
      private final Machine m_machine;

      /**
       * The synchronization link.
       */
      private final Instance m_link;

      /**
       * The hook script executed by the Sync step.
       */
      private final ValueExpression m_script;

      /**
       * Constructs the link.
       */
      private Link(Instance link, ValueExpression script, int nArgCount, Machine machine)
      {
         m_nArgCount = nArgCount;
         m_machine = machine;
         m_link = link;
         m_script = script;
      }

      /**
       * @return The synchronization link.
       */
      public Instance getLink()
      {
         return m_link;
      }

      /**
       * @param newTobj The new image received from spoke.
       * @param oldTobj The image received from spoke last time (could be null, if this is the first time we receive change for this instance from the spoke).
       * @param instance  The instance as it is currently in NexJ hub.
       * @param syncObject The SysSyncObject instance for this synchronization request.
       * @param conflicts The list of conflicts detected for this request.
       * @return conflict resolution strategy
       */
      public Object invokeScript(TransferObject newTobj, TransferObject oldTobj,
         Instance instance, Instance syncObject, Pair conflicts)
      {
         return m_script.invoke(m_machine, (State)m_machine.getArg(1, m_nArgCount),
            new Object[] {newTobj, oldTobj, instance, syncObject, conflicts});
      }
   }
}
