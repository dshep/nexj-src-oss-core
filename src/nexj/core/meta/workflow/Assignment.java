// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import java.util.ArrayList;
import java.util.List;

import nexj.core.meta.Metadata;
import nexj.core.meta.Primitive;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Undefined;

/**
 * Flow assignment step. Adds an item to a
 * work queue by creating an instance of SysWorkflowAssignment.
 */
public class Assignment extends Step
{
   // constants
   
   /**
    * Modal UI.
    */
   public final static int MODAL = 0;
   
   /**
    * Modeless UI.
    */
   public final static int MODELESS = 1;
   
   /**
    * SDI UI.
    */
   public final static int SDI = 2;

   /**
    * Target function arguments.
    */
   public final static Pair TARGET_ARGUMENTS = new ConstPair(Symbol._ASSIGNMENT, ARGUMENTS);

   /**
    * "semaphore" attribute symbol for SysWorkflowAssignment.
    */
   protected final static Symbol SEMAPHORE = Symbol.define("semaphore");

   /**
    * The default workflow assignment class symbol; SysWorkflowAssignment.
    */
   protected final static Symbol DEFAULT_ASSIGNMENT_CLASS = Symbol.define(Metadata.WORKFLOW_ASSIGNMENT_CLASS_NAME);

   /**
    * The default factory code for creating assignments; (SysWorkflowAssignment 'new)
    */
   protected final static Pair DEFAULT_FACTORY = new ConstPair(
      DEFAULT_ASSIGNMENT_CLASS,
      new ConstPair(
         new ConstPair(Symbol.QUOTE, new ConstPair(Symbol.NEW))
      )
   );

   // attributes

   /**
    * Semaphore flag; true if the assignment instance will be marked as created
    * for a Semaphore.
    */
   protected boolean m_bSemaphore;

   // associations

   /**
    * The assignment item caption expression.
    */
   protected Object m_caption = Undefined.VALUE;

   /**
    * The priority expression.
    */
   protected Object m_priority = Undefined.VALUE;

   /**
    * The assignee expression.
    */
   protected Object m_assignee = Undefined.VALUE;

   /**
    * The owner expression.
    */
   protected Object m_owner = Undefined.VALUE;

   /**
    * The optional queue name. An expression
    * that evaluates to the queue name at run time.
    */
   protected Object m_queue;

   /**
    * The factory code. A list of the form "AssignmentClass ['event [arg1 arg2 ... argN]]" where
    * square brackets indicate optional items.
    */
   protected Pair m_factory = DEFAULT_FACTORY;

   /**
    * The state management function.
    */
   protected PCodeFunction m_function;

   /**
    * The target collection: Object condition[5*n], String sTarget[5*n+1],
    * String sForm[5*n+2], String nUIMode[5*n+3], String sBranch[5*n+4]. 
    */
   protected List m_targetList;

   /**
    * The target selection function.
    */
   protected Function m_targetFunction;

   // constructors

   /**
    * Constructs the step.
    * @param sName The step name.
    * @param activity The containing activity.
    * @param queue The queue name expression.
    * @param caption The assignment item caption expression.
    */
   public Assignment(String sName, Activity activity, Object queue, Object caption)
   {
      super(sName);

      m_queue = queue;
      m_caption = caption;
      setActivity(activity);
   }

   // operations

   /**
    * The optional manual completion step.
    */
   protected ManualCompletion m_manualCompletion;

   /**
    * Sets the optional manual completion step.
    * @param manualCompletion The optional manual completion step to set.
    */
   public void setManualCompletion(ManualCompletion manualCompletion)
   {
      verifyNotReadOnly();
      m_manualCompletion = manualCompletion;
   }

   /**
    * @return The optional manual completion step.
    */
   public ManualCompletion getManualCompletion()
   {
      return m_manualCompletion;
   }
   
   /**
    * Sets the assignment item caption expression.
    * @param caption The assignment item expression to set.
    */
   public void setCaption(Object caption)
   {
      verifyNotReadOnly();
      m_caption = caption;
   }

   /**
    * @return The assignment item caption expression.
    */
   public Object getCaption()
   {
      return m_caption;
   }

   /**
    * Sets the priority expression.
    * @param priority The priority expression to set.
    */
   public void setPriority(Object priority)
   {
      verifyNotReadOnly();
      m_priority = priority;
   }

   /**
    * @return The priority expression.
    */
   public Object getPriority()
   {
      return m_priority;
   }

   /**
    * Sets the assignee expression.
    * @param assignee The assignee expression to set.
    */
   public void setAssignee(Object assignee)
   {
      verifyNotReadOnly();
      m_assignee = assignee;
   }

   /**
    * @return The assignee expression.
    */
   public Object getAssignee()
   {
      return m_assignee;
   }

   /**
    * Sets the owner expression.
    * @param owner The owner expression to set.
    */
   public void setOwner(Object owner)
   {
      verifyNotReadOnly();
      m_owner = owner;
   }

   /**
    * @return The owner expression.
    */
   public Object getOwner()
   {
      return m_owner;
   }

   /**
    * Sets the optional queue name.
    * @param queue The optional queue name to set. An expression
    * that is evaluated to get the queue name at run time.
    */
   public void setQueue(Object queue)
   {
      verifyNotReadOnly();
      m_queue = queue;
   }

   /**
    * @return The optional queue name.
    */
   public Object getQueue()
   {
      return m_queue;
   }

   /**
    * Sets the assignment class factory code. It is a list of the form
    * "AssignmentClass ['event [arg1 arg2 ... argN]]". If "'event" is omitted,
    * then "'new" is assumed.
    * 
    * @param factory The factory code to set.
    */
   public void setFactory(Pair factory)
   {
      verifyNotReadOnly();

      if (factory == null)
      {
         factory = DEFAULT_FACTORY;
      }
      else if (factory.getNext() == null)
      {
         factory = Pair.list(factory.getHead(), Pair.quote(Symbol.NEW));
      }

      m_factory = factory;
   }

   /**
    * Gets the assignment class factory code.
    * 
    * @return  The factory code of the form "AssignmentClass ['event [arg1 arg2 ... argN]]".
    */
   public Pair getFactory()
   {
      return m_factory;
   }

   /**
    * Sets the Semaphore flag.
    * 
    * @param bSemaphore True if the assignment instances created by this
    * step will be marked as created for a Semaphore.
    */
   public void setSemaphore(boolean bSemaphore)
   {
      verifyNotReadOnly();
      m_bSemaphore = bSemaphore;
   }

   /**
    * Gets the Semaphore flag.
    * 
    * @return True if the assignment instances created by this step will be
    * marked as created for a Semaphore.
    */
   public boolean isSemaphore()
   {
      return m_bSemaphore;
   }

   /**
    * Adds a conditional target/form.
    * @param condition The condition used to select the target.
    * @param sTargetAssociation The target association path.
    * @param sFormName The target form name.
    * @param nUIMode The UI mode (one of the UI mode constants).
    * @param sBranchName The branch name.
    */
   public void addTarget(Object condition, String sTargetAssociation, String sFormName, int nUIMode, String sBranchName)
   {
      if (m_targetList == null)
      {
         m_targetList = new ArrayList(10);
      }
      
      m_targetList.add(condition);
      m_targetList.add(sTargetAssociation);
      m_targetList.add(sFormName);
      m_targetList.add(Primitive.createInteger(nUIMode));
      m_targetList.add(sBranchName);
   }

   /**
    * @return True if the target must be calculated at run time.
    */
   public boolean isVarTarget()
   {
      return m_targetList != null && (m_targetList.size() > 1 || m_targetList.get(0) != Boolean.TRUE);
   }

   /**
    * @return The target selection function.
    */
   public Function getTargetFunction()
   {
      return m_targetFunction;
   }

   /**
    * Adds a new named argument to an argument list.
    * @param code The code, to which to prepend the new code.
    * @param sym The argument symbol.
    * @param arg The argument expression.
    */
   protected static Pair addArg(Pair code, Symbol sym, Object arg)
   {
      return new Pair(Pair.list(Symbol.CONS, Pair.quote(sym), arg), code);
   }
   
   /**
    * @see nexj.core.meta.workflow.Step#generate(nexj.core.scripting.Machine)
    */
   public void generate(Machine machine)
   {
      assert m_next != null;
      assert m_next.getOrdinal() >= 0;

      verifyNotReadOnly();
      
      Pair code = addArg(null, Symbol.WORKFLOW, Symbol._FLOW);
      Object caption = (m_caption != Undefined.VALUE) ? m_caption : getName();

      code = addArg(code, Symbol.CAPTION, (caption == null) ? " " : caption);

      if (m_priority != Undefined.VALUE)
      {
         code = addArg(code, Symbol.PRIORITY, m_priority);
      }
      
      if (m_assignee != Undefined.VALUE)
      {
         code = addArg(code, Symbol.ASSIGNEE, m_assignee);
      }

      if (m_owner != Undefined.VALUE)
      {
         code = addArg(code, Symbol.OWNER, m_owner);
      }

      if (m_queue != null)
      {
         code = addArg(code, Symbol.QUEUE,
            Pair.list(Symbol.SYSWORKFLOWQUEUE, Pair.quote(Symbol.GETQUEUE), m_queue)
            );
      }

      code = addArg(code, Symbol.ORDINAL, Primitive.createInteger(m_nOrdinal));
      code = addArg(code, SEMAPHORE, Boolean.valueOf(m_bSemaphore));

      // (lambda (...) (begin-privileged (<m_factory> (cons 'workflow :flow) (cons ...))))
      // m_factory defaults to produce the expression (SysWorkflowAssignment 'new (cons 'workflow :flow) (cons ...))
      m_function = compile(ARGUMENTS,
         new Pair(
            Pair.list(Symbol.BEGIN_PRIVILEGED,
               Pair.append(m_factory, code)
            ),
            m_activity.getFlow().getDefaultReturnCode()
         ), machine);

      // (lambda (...)
      //    (cond
      //       (cond1
      //          (:assignment'target "target1")
      //          (:assignment'form "form1")
      //          (:assignment'uimode nMode)
      //          "branch1"
      //       )
      //       ...
      //    )
      // )
      if (m_targetList != null)
      {
         Pair pair = null;

         for (int i = m_targetList.size() - 5; i >= 0; i -= 5)
         {
            Pair body = Pair.list(
               Pair.list(Symbol._ASSIGNMENT, Pair.quote(Symbol.TARGET), m_targetList.get(i + 1)),
               Pair.list(Symbol._ASSIGNMENT, Pair.quote(Symbol.FORM), m_targetList.get(i + 2)),
               Pair.list(Symbol._ASSIGNMENT, Pair.quote(Symbol.UIMODE), m_targetList.get(i + 3)),
               m_targetList.get(i + 4)
               );

            if (!m_activity.getFlow().isPrivileged())
            {
               body = Pair.list(new Pair(Symbol.BEGIN_PRIVILEGED, body));
            }

            pair = new Pair(new Pair(m_targetList.get(i), body), pair);
         }

         pair = new Pair(Symbol.COND, pair);

         if (m_activity.getFlow().isPrivileged())
         {
            pair = Pair.list(Symbol.BEGIN_PRIVILEGED, pair);
         }

         m_targetFunction = compile(TARGET_ARGUMENTS, Pair.list(pair), machine);
      }
   }

   /**
    * @see nexj.core.meta.workflow.Step#step(nexj.core.meta.workflow.State)
    */
   public PCodeFunction step(State state)
   {
      state.remove(this);
      state.add(m_next);

      return m_function;
   }
}
