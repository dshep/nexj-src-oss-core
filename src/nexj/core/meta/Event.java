// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.workflow.Handler;
import nexj.core.runtime.Context;
import nexj.core.runtime.SecurityViolationException;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.CompilerException;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Function;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.JavaAction;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.EmptyIterator;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashTab;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Named;
import nexj.core.util.TextPosition;
import nexj.core.util.UncheckedException;

/**
 * Event handler or method metadata. When an event is triggered or method
 * is invoked, a series of actions associated with it is executed.
 * 
 * The following events are generated automatically by the framework:
 * 
 * new(class, values) - invoked to check the creation
 *    privileges and initialize the instance with default values.
 * 
 * create(obj) - invoked after the object has been created.
 * 
 * read(class, attributes, where, orderBy, maxCount, offset, xlock) -
 *    invoked to read an object collection.
 * 
 * load(obj, attributes) - invoked to read or compute a field
 *    on-demand.
 * 
 * update(obj) - invoked to update an instance.
 * 
 * delete(obj) - invoked to delete an instance.
 * 
 * lock(obj) - invoked to lock an instance in the persistent storage.
 */
public final class Event extends Member
{
   // constants

   // transaction constants

   /**
    * Use the default or inherited transaction attribute. 
    */
   public final static byte TX_DEFAULT = 0;

   /**
    * Join a transaction, if available, otherwise run transactionless.
    */
   public final static byte TX_SUPPORTED = 1;

   /**
    * Join a transaction, if available, otherwise start a new transaction.
    * Do not auto-commit on return.
    */
   public final static byte TX_REQUIRED = 2;

   /**
    * Always suspend the current transaction, if any, and start a new one.
    * Auto-commit on return.
    */
   public final static byte TX_NEW = 3;

   /**
    * Always suspend the current transaction, if any.
    * Restore on return.
    */
   public final static byte TX_NONE = 4;
   
   /**
    * Fail if there is no transaction.
    */
   public final static byte TX_MANDATORY = 5;
   
   /**
    * Fail if there is a transaction.
    */
   public final static byte TX_UNSUPPORTED = 6;

   // other constants

   /**
    * Empty action body, returns null.
    */
   public final static Object EMPTY_BODY = Pair.quote(null); 

   /**
    * Closure that returns null.
    */
   protected final static Object EMPTY_CLOSURE = Pair.list(Symbol.LAMBDA, null, EMPTY_BODY);

   /**
    * Empty Symbol[].
    */
   protected final static Symbol[] EMPTY_SYMBOL_ARRAY = new Symbol[0];

   /**
    * The flow action name.
    */
   public final static String FLOW_ACTION_NAME = "workflow";

   /**
    * The create flow action name.
    */
   public final static String CREATE_FLOW_ACTION_NAME = "create-workflow-";

   /**
    * The trigger flow action name.
    */
   public final static String TRIGGER_FLOW_ACTION_NAME = "trigger-workflow";

   /**
    * The delete flow action name.
    */
   public final static String DELETE_FLOW_ACTION_NAME = "delete-workflow";

   /**
    * The add dependency change action name.
    */
   public final static String SYNC_DEPENDENCY_ACTION_NAME = "add-dependency-change";
   
   /**
    * The symbol that names the addDependencyChange event.
    */
   public final static Symbol SYNC_DEPENDENCY_EVENT_SYMBOL = Symbol.define("addDependencyChange");

   /**
    * The symbol that names the DependencySynchronizationCommand class.
    */
   public final static Symbol DEPENDENCY_SYNC_COMMAND = Symbol.define("nexj.core.integration.sync.DependencySynchronizationCommand");

   // attributes

   /**
    * The variable argument number flag.
    */
   protected boolean m_bVarArg;

   /**
    * True if this event should be invoked in a transient context.
    */
   protected boolean m_bTransient;

   /**
    * The auditing flag.
    */
   protected byte m_nAudited = -1;

   /**
    * The transaction mode, one of Metaclass.TX_* constants.
    */
   protected byte m_nTransactionMode;

   // associations

   /**
    * The action map.
    */
   protected Lookup m_actionMap = new HashTab(4); // of type Action[String]

   /**
    * The list of actions associated with the event.
    */
   protected List m_actionList = new ArrayList(4); // of type Action
   
   /**
    * The event argument map.
    */
   protected Lookup m_argMap = new HashTab(4); // of type Argument[String]
   
   /**
    * The event argument list.
    */
   protected List m_argList = new ArrayList(4); // of type Argument

   /**
    * The event result type. (null == unknown result type)
    * The name of the object is ignored and should be null.
    */
   protected Argument m_result;

   /**
    * The selector corresponding to the event.
    */
   protected Selector m_selector;

   /**
    * The local variable map.
    * Maps a variable symbol to a Scheme expression: Object[Symbol].
    */
   protected Lookup m_varMap; // of type Object[Symbol]

   /**
    * The flow association symbol path list: Pair[].
    * These associations indicate the instance to query and
    * invoke a flow handler when this event is triggered.
    */
   protected List m_flowAssocList;

   /**
    * The flow starter collection: Handler[].
    */
   protected List m_flowStarterList;

   /**
    * The invocation privilege.
    */
   protected PrimitivePrivilege m_privilege;

   /**
    * The event function.
    */
   protected Function m_function;

   // constructor

   /**
    * Constructs an event with a given name.
    * @param sName The name of the event.
    */
   public Event(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * @see nexj.core.meta.Member#isAttribute()
    */
   public boolean isAttribute()
   {
      return false;
   }

   /**
    * Sets the variable argument number flag.
    * @param bVarArg The variable argument number flag to set.
    */
   public void setVarArg(boolean bVarArg)
   {
      verifyNotReadOnly();
      m_bVarArg = bVarArg;
   }

   /**
    * @return The variable argument number flag.
    */
   public boolean isVarArg()
   {
      return m_bVarArg;
   }
   
   /**
    * Adds a new event action to the event.
    * @param action The event action to add.
    * @throws MetadataException if an action with the same name already exists.
    */
   public void addAction(Action action)
   {
      verifyNotReadOnly();

      Object oldAction = m_actionMap.put(action.getName(), action);

      if (oldAction != null)
      {
         m_actionMap.put(action.getName(), oldAction);

         throw new MetadataException("err.meta.actionDup",
            new Object[] {action.getName(), getName(), getMetaclass().getName()});
      }

      m_actionList.add(action);
      action.setEvent(this);
   }

   /**
    * Adds an action preceding a specified action (if it exists).
    * @param action The action to add.
    * @param sNextName The name of the next action.
    */
   public void addAction(Action action, String sNextName)
   {
      action.setNextAction(findAction(sNextName));
      addAction(action);
   }
   
   /**
    * Gets an event action by name.
    * @param sName The action name.
    * @return The action object.
    * @throws MetadataLookupException if the action does not exist.
    */
   public Action getAction(String sName)
   {
      Action action = (Action)m_actionMap.get(sName);

      if (action != null)
      {
         return action;
      }

      throw new MetadataLookupException("err.meta.actionLookup", sName, this);
   }

   /**
    * Finds an event action by name.
    * @param sName The action name.
    * @return The action object, or null if not found.
    */
   public Action findAction(String sName)
   {
      return (Action)m_actionMap.get(sName);
   }

   /**
    * Gets an event action by ordinal number.
    * @param nOrdinal The event action ordinal number (0-based).
    * @return The event action object.
    */
   public Action getAction(int nOrdinal)
   {
      return (Action)m_actionList.get(nOrdinal);
   }

   /**
    * @return The event action count.
    */
   public int getActionCount()
   {
      return m_actionList.size();
   }

   /**
    * @return An iterator for the contained event action objects.
    */
   public Iterator getActionIterator()
   {
      return m_actionList.iterator();
   }

   /**
    * Rotates forward the action list by a given number of elements.
    * @param nCount The action count (distance) by which to rotate.
    */
   protected void rotateActions(int nCount)
   {
      Collections.rotate(m_actionList, nCount);
   }

   /**
    * Adds a new event argument to the event.
    * @param arg The event argument to add.
    * @throws MetadataException if an argument with the same name already exists.
    */
   public void addArgument(Argument arg)
   {
      verifyNotReadOnly();

      Object oldArg = m_argMap.put(arg.getName(), arg);

      if (oldArg != null)
      {
         m_argMap.put(arg.getName(), oldArg);

         throw new MetadataException("err.meta.argDup",
            new Object[] {arg.getName(), getName(), m_metaclass.getName()});
      }

      arg.setOrdinal(m_argList.size());
      arg.setEvent(this);
      m_argList.add(arg);
   }

   /**
    * Finds an argument by name.
    * @param sName The argument name.
    * @return The argument object, or null if not found.
    */
   public Argument findArgument(String sName)
   {
      return (Argument)m_argMap.get(sName);
   }

   /**
    * Gets an argument by name.
    * @param sName The argument name.
    * @return The argument object.
    * @throws MetadataLookupException if the argument does not exist.
    */
   public Argument getArgument(String sName)
   {
      Argument arg = (Argument)m_argMap.get(sName);

      if (arg != null)
      {
         return arg;
      }

      throw new MetadataLookupException("err.meta.argLookup", sName, this);
   }

   /**
    * Gets a event argument by ordinal number.
    * @param nOrdinal The event argument ordinal number (0-based).
    * @return The event argument object.
    */
   public Argument getArgument(int nOrdinal)
   {
      return (Argument)m_argList.get(nOrdinal);
   }

   /**
    * @return The event argument count.
    */
   public int getArgumentCount()
   {
      return m_argList.size();
   }

   /**
    * @return An iterator for the contained event argument objects.
    */
   public Iterator getArgumentIterator()
   {
      return m_argList.iterator();
   }

   /**
    * Adds a local variable to the event.
    * @param symbol The variable symbol.
    * @param init The initializer.
    * @throws MetadataException if a variable with the same name already exists and bSstrict is true.
    */
   public void addVariable(Symbol symbol, Object init)
   {
      verifyNotReadOnly();
      
      if (m_varMap == null)
      {
         m_varMap = new HashTab();
      }

      if (init == null)
      {
         init = EMPTY_BODY;
      }

      Object oldInit = m_varMap.put(symbol, init);

      if (oldInit != null || findArgument(symbol.getName()) != null)
      {
         m_varMap.put(symbol, oldInit);

         throw new MetadataException("err.meta.varDup",
            new Object[] {symbol, getName(), m_metaclass.getName()});
      }
   }

   /**
    * Adds the local variables specified in a list to the event.
    * The list should contain variable symbols and/or (symbol expr) pairs.
    * @param list The list of local variables.
    * @throws MetadataException if the syntax is incorrect. 
    */
   public void addVariables(Pair list)
   {
      for (; list != null; list = list.getNext())
      {
         if (list.getHead() instanceof Symbol)
         {
            addVariable((Symbol)list.getHead(), null);
         }
         else if (list.getHead() instanceof Pair)
         {
            Pair pair = (Pair)list.getHead();
            
            if (pair.getHead() instanceof Symbol &&
                  pair.getTail() instanceof Pair &&
                  ((Pair)pair.getTail()).getTail() == null)
            {
               addVariable((Symbol)pair.getHead(), ((Pair)pair.getTail()).getHead());
            }
            else
            {
               throw new MetadataException("err.meta.invalidVarList", new Object[]{getName(), m_metaclass.getName()});
            }
         }
         else
         {
            throw new MetadataException("err.meta.invalidVarList", new Object[]{getName(), m_metaclass.getName()});
         }
      }
   }

   /**
    * Inherits the attribute and result type definitions from the specified event.
    * The most restrictive definition is chosen.
    * @param event The event from which to inherit the arguments.
    */
   protected void inheritEventTypes(Event event)
   {
      Argument result = getResult();
      Argument template = event.getResult();

      if (result == null && template != null)
      {
         setResult((Argument)template.clone());
      }
      else if (template != null)
      {
         result.inherit(template);
      }

      for (int i = 0, nCount = getArgumentCount(); i < nCount; ++i)
      {
         Argument arg = getArgument(i);

         template = event.findArgument(arg.getName());

         if (template != null)
         {
            arg.inherit(template);
         }
      }
   }

   /**
    * Inherits the local variables from the specified event.
    * @param event The event from which to inherit the variables.
    */
   public void inheritVariables(Event event)
   {
      if (event.m_varMap != null)
      {
         if (m_varMap == null)
         {
            m_varMap = new HashTab(event.m_varMap.size());
         }

         for (Lookup.Iterator itr = event.m_varMap.iterator(); itr.hasNext();)
         {
            Object symbol = itr.next();

            if (!m_varMap.contains(symbol))
            {
               m_varMap.put(symbol, itr.getValue());
            }
         }
      }
   }

   /**
    * Sets the event selector.
    * @param selector The event selector to set.
    */
   public void setSelector(Selector selector)
   {
      verifyNotReadOnly();
      m_selector = selector;
   }

   /**
    * @return The event selector.
    */
   public Selector getSelector()
   {
      return m_selector;
   }

   /**
    * Sets the invocation privilege.
    * @param privilege The invocation privilege to set.
    */
   public void setPrivilege(PrimitivePrivilege privilege)
   {
      verifyNotReadOnly();
      m_privilege = privilege;
   }

   /**
    * @return The invocation privilege.
    */
   public PrimitivePrivilege getPrivilege()
   {
      return m_privilege;
   }

   /**
    * @see Event#m_result
    */
   public Argument getResult()
   {
      return m_result;
   }

   /**
    * @type The event result type.
    * @see Event#m_result
    */
   public void setResult(Argument result)
   {
      verifyNotReadOnly();
      m_result = result;
   }

   /**
    * Sets the transaction mode.
    * @param nTransactionMode The transaction mode to set, one of the TX_* constants.
    */
   public void setTransactionMode(byte nTransactionMode)
   {
      verifyNotReadOnly();
      m_nTransactionMode = nTransactionMode;
   }

   /**
    * @return The transaction mode, one of the TX_* constants.
    */
   public byte getTransactionMode()
   {
      return m_nTransactionMode;
   }

   /**
    * Gets whether the event should be invoked in a transient context.
    * @param bTransient The transient flag determined from the Request.
    * @return True if the event should be invoked in a transient context; false otherwise.
    */
   public boolean isTransient(boolean bTransient)
   {
      return bTransient && m_bTransient;
   }

   /**
    * Sets the auditing flag.
    * @param audited The auditing flag to set.
    */
   public void setAudited(Boolean audited)
   {
      verifyNotReadOnly();

      if (audited == null)
      {
         m_nAudited = -1;
      }
      else
      {
         setAudited(audited.booleanValue());
      }
   }

   /**
    * Sets the auditing flag.
    * @param bAudited The auditing flag to set.
    */
   public void setAudited(boolean bAudited)
   {
      verifyNotReadOnly();
      m_nAudited = (bAudited) ? (byte)1 : (byte)0;
   }

   /**
    * @return The auditing flag.
    */
   public boolean isAudited()
   {
      return m_nAudited > 0;
   }

   /**
    * @return The auditing flag. 
    */
   public Boolean getAudited()
   {
      switch (m_nAudited)
      {
         case 0:
            return Boolean.FALSE;

         case 1:
            return Boolean.TRUE;

         default:
            return null;
      }
   }

   /**
    * Sets the event function.
    * @param function The event function to set.
    */
   public void setFunction(Function function)
   {
      verifyNotReadOnly();
      m_function = function;
   }

   /**
    * @return The event function.
    */
   public Function getFunction()
   {
      return m_function;
   }

   /**
    * Adds a new flow starter to the event.
    * @param flowStarter The flow starter to add.
    */
   public void addFlowStarter(Handler flowStarter)
   {
      verifyNotReadOnly();
      
      if (m_flowStarterList == null)
      {
         m_flowStarterList = new ArrayList(4);
      }

      m_flowStarterList.add(flowStarter);
   }

   /**
    * Gets a flow starter by ordinal number.
    * @param nOrdinal The flow starter ordinal number (0-based).
    * @return The flow starter object.
    */
   public Handler getFlowStarter(int nOrdinal)
   {
      return (Handler)m_flowStarterList.get(nOrdinal);
   }

   /**
    * @return The flow starter count.
    */
   public int getFlowStarterCount()
   {
      if (m_flowStarterList == null)
      {
         return 0;
      }
      
      return m_flowStarterList.size();
   }

   /**
    * @return An iterator for the contained flow starter objects.
    */
   public Iterator getFlowStarterIterator()
   {
      if (m_flowStarterList == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_flowStarterList.iterator();
   }

   /**
    * Adds a flow association.
    * @param assoc The association patch to add. Can be null to indicate the event instance.
    */
   public void addFlowAssoc(Pair assoc)
   {
      if (m_flowAssocList == null)
      {
         m_flowAssocList = new ArrayList(4);
      }

      if (!m_flowAssocList.contains(assoc))
      {
         m_flowAssocList.add(assoc);
      }
   }
   
   /**
    * Gets a flow association by ordinal number.
    * @param nOrdinal The flow association ordinal number.
    * @return The association path.
    */
   public Pair getFlowAssoc(int nOrdinal)
   {
      return (Pair)m_flowAssocList.get(nOrdinal);
   }

   /**
    * @return The flow association count.
    */
   public int getFlowAssocCount()
   {
      if (m_flowAssocList == null)
      {
         return 0;
      }
      
      return m_flowAssocList.size();
   }

   /**
    * @return The flow association iterator.
    */
   public Iterator getFlowAssocIterator()
   {
      if (m_flowAssocList == null)
      {
         return EmptyIterator.getInstance();
      }
      
      return m_flowAssocList.iterator();
   }

   /**
    * Generates an action for creating a flow.
    * Must be called first on the base event, then on its subclasses and so on.
    */
   public void generateCreateFlowAction()
   {
      verifyNotReadOnly();

      if (m_metaclass.getBase() != null)
      {
         Event baseEvent = m_metaclass.getBase().findEvent(getName(), getArgumentCount());

         if (baseEvent != null)
         {
            for (int i = 0, nCount = baseEvent.getFlowStarterCount(); i < nCount; ++i)
            {
               addFlowStarter(baseEvent.getFlowStarter(i));
            }
         }
      }

      for (int nFlow = 0, nFlowCount = getFlowStarterCount(); nFlow < nFlowCount; ++nFlow)
      {
         Handler flow = getFlowStarter(nFlow);
         Action action = new Action(CREATE_FLOW_ACTION_NAME + ((Named)flow).getName());

         action.setType(Action.BEFORE);
         action.setCondition(flow.getCondition());
         
         Pair assoc = null;

         for (int i = flow.getAssociationCount() - 1; i >= 0; --i)
         {
            assoc = new Pair(flow.getAssociation(i).getReverse().getSymbol(), assoc);
         }

         action.setBody(
            Pair.list(
               Pair.list(
                  Pair.quote(m_metaclass.getMetadata().getMetaclass(Metadata.WORKFLOW_CLASS_NAME)),
                  Pair.quote(Symbol.START),
                  new Pair(Symbol.AT, assoc),
                  ((Named)flow).getName(),
                  Boolean.TRUE
               )
            )
         );
         
         addAction(action, FLOW_ACTION_NAME);
         m_declarator = m_metaclass;
      }
   }

   /**
    * Generates an action for triggering a flow state change.
    * Must be called first on the base event, then on its subclasses and so on.
    */
   public void generateTriggerFlowAction()
   {
      verifyNotReadOnly();

      if (m_metaclass.getBase() != null)
      {
         Event baseEvent = m_metaclass.getBase().findEvent(getName(), getArgumentCount());
         
         if (baseEvent != null)
         {
            Action action = baseEvent.findAction(TRIGGER_FLOW_ACTION_NAME);
            
            if (action != null)
            {
               if (getFlowAssocCount() == 0)
               {
                  m_flowAssocList = baseEvent.m_flowAssocList;
                  action = (Action)action.clone();
                  addAction(action, FLOW_ACTION_NAME);
                  
                  return;
               }
               else
               {
                  for (int i = 0, nCount = baseEvent.getFlowAssocCount(); i < nCount; ++i)
                  {
                     addFlowAssoc(baseEvent.getFlowAssoc(i));
                  }
               }
            }
         }
      }

      if (getFlowAssocCount() == 0)
      {
         return;
      }

      Action action = new Action(TRIGGER_FLOW_ACTION_NAME);

      action.setType(Action.BEFORE);
      action.setCondition(Boolean.TRUE);
      
      Pair body = null;
      
      for (int i = getFlowAssocCount() - 1; i >= 0; --i)
      {
         Pair assoc = getFlowAssoc(i);

         // (SysWorkflow'trigger (@ <reverse-assoc>) this '<event> <args>)
         body = new Pair(
            new Pair(
               Pair.quote(m_metaclass.getMetadata().getMetaclass(Metadata.WORKFLOW_CLASS_NAME)),
               new Pair(
                  Pair.quote(Symbol.TRIGGER),
                  new Pair(
                     new Pair(Symbol.AT, assoc),
                     new Pair(
                        Symbol.THIS,
                        new Pair(Pair.quote(assoc),
                           new Pair(
                              Pair.quote(this),
                              getArguments(false)
                              )
                           )
                        )
                     )
                  )
               ),
            body);
      }

      action.setBody(body);
      addAction(action, FLOW_ACTION_NAME);
      m_declarator = m_metaclass;
   }

   /**
    * Generates an action for deleting all the flows associated with the instance.
    * @return True if the action has been generated, false if already exists.
    */
   public boolean generateDeleteFlowAction()
   {
      Action action = findAction(DELETE_FLOW_ACTION_NAME);
      
      if (action != null)
      {
         return false;
      }
      
      action = new Action(DELETE_FLOW_ACTION_NAME);
      action.setType(Action.AFTER);
      action.setCondition(Boolean.TRUE);
      action.setBody(
         Pair.list(
            Pair.list(
               Pair.quote(m_metaclass.getMetadata().getMetaclass(Metadata.WORKFLOW_CLASS_NAME)),
               Pair.quote(Symbol.DELETE),
               Symbol.THIS
               )
            )
         );

      addAction(action, FLOW_ACTION_NAME);
      m_declarator = m_metaclass;

      return true;
   }

   /**
    * Generates an action for adding changes that affects synchronization to the unit of work transient cache, to be
    * processed by the synchronization engine.
    * @param pair A Scheme list of Pairs, in which head is a name of the metaclass that affects synchronization, and
    * tail is a list of attributes of this metaclass, affecting synchronization.
    */
   public void generateSyncDependencyAction(Pair attributes)
   {
      verifyNotReadOnly();

      Action action = findAction(SYNC_DEPENDENCY_ACTION_NAME);

      if (action != null)
      {
         return;
      }

      action = new Action(SYNC_DEPENDENCY_ACTION_NAME);
      action.setType(Action.BEFORE);
      action.setCondition(Boolean.TRUE);

      // (DEPENDENCY_SYNC_COMMAND'addDependencyChange this '<attributes>)
      Pair body = Pair.list(Pair.list(DEPENDENCY_SYNC_COMMAND, Pair.quote(SYNC_DEPENDENCY_EVENT_SYMBOL), Symbol.THIS,
         Pair.quote(attributes)));

      action.setBody(body);
      addAction(action);
      m_declarator = m_metaclass;
   }

   /**
    * Determines if this event is derivation compatible with a base event.
    * @param base The base event.
    * @return True if it is compatible.
    */
   public boolean isCompatibleWith(Event base)
   {
      int nCount = getArgumentCount();
      
      if (m_bStatic != base.m_bStatic ||
         m_bVarArg != base.m_bVarArg ||
         nCount != base.getArgumentCount())
      {
         return false;
      }

      if (getResult() != null && !getResult().isCompatibleWith(base.getResult()))
      {
         return false; // results not compatible
      }

      for (int i = 0; i < nCount; ++i)
      {
         Argument derivedArg = getArgument(i);
         Argument baseArg = base.getArgument(i);

         if (!derivedArg.getSymbol().equals(baseArg.getSymbol()) || !derivedArg.isCompatibleWith(baseArg))
         {
            return false;
         }
      }

      return true;
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      Event event = (Event)super.clone();

      if (event.getResult() != null)
      {
         event.setResult((Argument)event.getResult().clone());
      }

      event.m_argList = new ArrayList(getArgumentCount());
      event.m_argMap = new HashTab(getArgumentCount());

      for (int i = 0; i < getArgumentCount(); ++i)
      {
         event.addArgument((Argument)getArgument(i).clone());
      }

      event.m_actionList = new ArrayList(getActionCount());
      event.m_actionMap = new HashTab(getActionCount());

      for (int i = 0; i < getActionCount(); ++i)
      {
         event.addAction((Action)getAction(i).clone());
      }

      if (m_varMap != null)
      {
         event.m_varMap = (Lookup)((HashTab)m_varMap).clone();
      }

      return event;
   }

   /**
    * Resolves the action references against the aspects and the base class.
    */
   public void resolveActions()
   {
      verifyNotReadOnly();

      for (int nAction = 0, nActionCount = m_actionList.size();
         nAction < nActionCount; ++nAction)
      {
         Action action = (Action)m_actionList.get(nAction);

         if (action.getNextAction() == null || action.getNextAction().getEvent() != this)
         {
            Action baseAction = null;

            for (int nAspect = 0, nAspectCount = m_metaclass.getDirectAspectCount();
               nAspect < nAspectCount; ++nAspect)
            {
               Event baseEvent = ((ClassAspect)m_metaclass.getAspect(nAspect))
                  .findEvent(m_sName, getArgumentCount());

               if (baseEvent != null)
               {
                  baseAction = baseEvent.findAction(action.getName());

                  if (baseAction != null)
                  {
                     break;
                  }
               }
            }

            if (baseAction == null && m_rootDeclarator != m_metaclass)
            {
               if (m_metaclass.getBase() != null)
               {
                  Event baseEvent = m_metaclass.getBase().findEvent(m_sName, getArgumentCount());

                  if (baseEvent != null)
                  {
                     baseAction = baseEvent.findAction(action.getName());
                  }
               }
            }

            if (baseAction != null && baseAction.getNextAction() != null)
            {
               Action nextAction = findAction(baseAction.getNextAction().getName());

               if (nextAction != null)
               {
                  action.setNextAction(nextAction);
               }
            }
         }
      }
   }

   /**
    * Sorts the actions based on their precedence established by
    * getNextAction() and their declaration order.
    * @throws MetadataException if a cycle has been discovered in the action graph.
    */
   public void sortActions()
   {
      verifyNotReadOnly();
      
      try
      {
         int nActionCount = m_actionList.size();

         if (nActionCount == 0)
         {
            return;
         }

         // Assign ordinal numbers
         for (int i = 0; i < nActionCount; ++i)
         {
            ((Action)m_actionList.get(i)).m_nOrdinal = i;
         }

         Action main = findAction(Action.MAIN_ACTION_NAME);

         for (int nSortCount = 0; nSortCount < nActionCount; ++nSortCount)
         {
            boolean bModified = false;

            for (int i = 0; i < nActionCount; ++i)
            {
               Action action = (Action)m_actionList.get(i);

               if (action.getNextAction() != null && action.getNextAction().m_nOrdinal <= i)
               {
                  for (int k = action.getNextAction().m_nOrdinal; k < i; ++k)
                  {
                     Action tmp = (Action)m_actionList.get(k + 1);

                     tmp.m_nOrdinal = k;
                     m_actionList.set(k, tmp);
                  }

                  action.getNextAction().m_nOrdinal = i;
                  m_actionList.set(i, action.getNextAction());

                  bModified = true;
               }
            }

            if (!bModified)
            {
               if (main != null)
               {
                  for (int k = main.m_nOrdinal; k < nActionCount - 1; ++k)
                  {
                     m_actionList.set(k, m_actionList.get(k + 1));
                  }

                  m_actionList.set(nActionCount - 1, main);
               }

               for (int i = 0; i < nActionCount - 1; ++i)
               {
                  Action action = (Action)m_actionList.get(i);

                  if (action.getNextAction() == null)
                  {
                     action.setNextAction((Action)m_actionList.get(i + 1));
                  }

                  // Verify that there are no gaps between the grouped actions
                  if (action.getGroupName() != null)
                  {
                     boolean bGap = false;

                     for (int k = i - 1; k >= 0; --k)
                     {
                        Action prev = (Action)m_actionList.get(k);

                        if (action.getGroupName().equals(prev.getGroupName()))
                        {
                           if (prev.getType() != action.getType())
                           {
                              MetadataValidationException e = new MetadataValidationException("err.meta.actionGroupTypeMismatch",
                                 new Object[]{action.getName(), prev.getName(), action.getGroupName()});

                              action.setProperties(e);

                              throw e;
                           }

                           if (bGap)
                           {
                              MetadataValidationException e = new MetadataValidationException("err.meta.actionGroupGap",
                                 new Object[]{action.getName(), action.getGroupName()});

                              action.setProperties(e);

                              throw e;
                           }

                           break;
                        }
                        else
                        {
                           if (prev.getType() != ((action.getType() == Action.BEFORE) ? Action.AFTER : Action.BEFORE))
                           {
                              bGap = true;
                           }
                        }
                     }
                  }
               }

               return;
            }
         }

         throw new MetadataException("err.meta.actionCycle",
            new Object[]{getName(), m_metaclass.getName()});
      }
      catch (MetadataException e)
      {
         MetadataValidationException x = new MetadataValidationException(e);

         setProperties(x);

         throw x;
      }
   }

   /**
    * @return The base (declared) event. Never returns null.
    */
   public Event getBase()
   {
      if (m_declarator == m_metaclass)
      {
         return this;
      }

      return (Event)m_declarator.getSelector(getName()).getMember(getArgumentCount());
   }

   /**
    * @return The root declared event.
    */
   public Event getRoot()
   {
      if (m_rootDeclarator == m_metaclass)
      {
         return this;
      }
      
      return (Event)m_rootDeclarator.getSelector(getName()).getMember(getArgumentCount());
   }
   
   /**
    * Compiles the ECA code.
    * @param machine The VM for the compilation.
    */
   public void compile(Machine machine)
   {
      assert m_function == null;

      if (m_declarator != m_metaclass)
      {
         m_function = getBase().getFunction();

         assert m_function != null;
      }
      else
      {
         int nActionCount = getActionCount();
         final int nArgOffset = 7; // number of non-vararg arguments
         Object[] args = new Object[Math.max(nActionCount, 1) + nArgOffset];
         Lookup posMap = new IdentityHashTab(64);
         Lookup urlMap = new IdentityHashTab(64);
         String sEventURL = getURL(m_metaclass);
         String sURLPrefix = sEventURL + "$";

         // When updating the event generator invocation code,
         // also update the plugin action compilation validation
         args[0] = this;
         args[1] = getArguments(true);
         args[2] = getVariables();
         args[3] = getPrivilege();

         if (m_accessAttribute == null)
         {
            args[4] = null;
         }
         else if (m_bStatic)
         {
            if (m_accessAttribute.isStatic())
            {
               args[4] = Primitive.createInteger(m_accessAttribute.getOrdinal());
            }
            else
            {
               args[4] = null;
            }
         }
         else
         {
            args[4] = Primitive.createInteger(
               (m_accessAttribute.isStatic()) ?
                  -1 -m_accessAttribute.getOrdinal() :
                  m_accessAttribute.getOrdinal());
         }

         args[5] = Boolean.valueOf(getArgumentCount() == 0 && "delete".equals(m_sName));

         byte nTransactionMode = ((Symbol.CREATE.equals(getSymbol()) || Symbol.UPDATE.equals(getSymbol())) &&
            getArgumentCount() == 0) ? TX_SUPPORTED : getTransactionMode();

         args[6] = Primitive.createInteger(nTransactionMode);
         m_bTransient = (nTransactionMode != TX_SUPPORTED);

         for (int i = 0; i < nActionCount; ++i)
         {
            Action action = getAction(i);
            Object condition = action.getCondition();
            String sURL;
            Object body;

            if (action.getDeclarator() != null)
            {
               sURL = getURL(action.getDeclarator()) + "$" + action.getFullName();
            }
            else
            {
               sURL = sURLPrefix + action.getFullName();
            }

            if (action.getMethod() == null)
            {
               body = action.getBody();

               if (body == null)
               {
                  body = EMPTY_BODY;
               }
            }
            else
            {
               body = Pair.list(Pair.list(Pair.quote(new JavaAction(action)),
                  (action.getType() == Action.AROUND) ? Symbol.CALL_NEXT : EMPTY_CLOSURE));
            }

            Lookup map = action.getTextPositionMap();

            if (map != null)
            {
               for (Lookup.Iterator itr = map.iterator(); itr.hasNext();)
               {
                  itr.next();
                  posMap.put(itr.getKey(), itr.getValue());
                  urlMap.put(itr.getValue(), sURL);
               } 
            }

            if (condition != null)
            {
               Compiler.setPosURLs(condition, sURL + "$condition", posMap, urlMap);
            }

            Boolean grouped = Boolean.FALSE;

            // Set grouped for all but the first action in a group
            if (action.getGroupName() != null)
            {
               if (action.getType() == Action.BEFORE)
               {
                  for (int k = i - 1; k >= 0; --k)
                  {
                     if (action.getGroupName().equals(getAction(k).getGroupName()))
                     {
                        grouped = Boolean.TRUE;
                        break;
                     }
                  }
               }
               else
               {
                  for (int k = i + 1; k < nActionCount; ++k)
                  {
                     if (action.getGroupName().equals(getAction(k).getGroupName()))
                     {
                        grouped = Boolean.TRUE;
                        break;
                     }
                  }
               }
            }
            
            args[i + nArgOffset] = new Pair(action.getTypeSymbol(), new Pair(grouped, new Pair(condition, body)));

            action.setBody(null);
            action.setTextPositionMap(null);
         }

         // Ensure there is a main action
         if (nActionCount == 0 || !Symbol.MAIN.equals(((Pair)args[args.length - 1]).getHead()))
         {
            if (nActionCount > 0)
            {
               Object[] argsSaved = args;

               args = new Object[args.length + 1];
               System.arraycopy(argsSaved, 0, args, 0, argsSaved.length);
            }

            args[args.length - 1] =  new Pair(Symbol.MAIN, new Pair(Boolean.FALSE, new Pair(Boolean.TRUE, EMPTY_BODY)));
         }

         try
         {
            // Invoke the sys:generate-event macro that generates the ECA code and compile the result
            Pair code = (Pair)machine.invoke((Function)machine.getGlobalEnvironment().getVariable(Symbol.SYS_GENERATE_EVENT), args);
            TextPosition pos = new TextPosition(0, 0);

            posMap.put(code, pos);
            urlMap.put(pos, sEventURL);

            for (Pair pair = code; pair != null; pair = pair.getNext())
            {
               if (pair.getHead() instanceof Pair && !posMap.contains(pair.getHead()))
               {
                  pos = new TextPosition(0, 0);
                  posMap.put(pair.getHead(), pos);
                  urlMap.put(pos, sEventURL);
               }
            }
            
            m_function = new Compiler().compile(code, posMap, urlMap, machine, false);
         }
         catch (Exception e)
         {
            MetadataValidationException x;

            if (e instanceof UncheckedException)
            {
               x = new MetadataValidationException((UncheckedException)e);
            }
            else
            {
               x = new MetadataValidationException("err.meta.eventCompilation", e);
            }

            setProperties(x);

            if (e instanceof CompilerException)
            {
               String sURL = ((CompilerException)e).getURL();

               if (sURL != null)
               {
                  int i = sURL.indexOf('$');

                  if (i > 0)
                  {
                     int k = sURL.indexOf('$', i + 1);
                     
                     if (k < 0)
                     {
                        x.setProperty("action", sURL.substring(i + 1));
                     }
                     else
                     {
                        x.setProperty("action", sURL.subSequence(i + 1, k));
                        x.setProperty("condition", Boolean.TRUE);
                     }
                  }
               }
            }

            throw x;
         }
      }
   }

   /**
    * Creates an argument symbol list S-expression.
    * @param bThis True to include the this keyword. 
    * @return The argument list S-expression.
    */
   public Pair getArguments(boolean bThis)
   {
      Object obj = null;
      int i = getArgumentCount() - 1;
      
      if (m_bVarArg)
      {
         obj = getArgument(i--).getSymbol();
      }

      while (i >= 0)
      {
         obj = new ConstPair(getArgument(i--).getSymbol(), obj);
      }

      if (bThis)
      {
         return new ConstPair(Symbol.THIS, obj);
      }
      
      return (Pair)obj;
   }
   
   /**
    * @return The variable initializer in let format S-expression.
    */
   private Pair getVariables()
   {
      Pair pair = null;

      if (m_varMap != null)
      {
         for (Lookup.Iterator itr = m_varMap.iterator(); itr.hasNext();)
         {
            itr.next();
            pair = new ConstPair(new ConstPair(itr.getKey(), new ConstPair(itr.getValue())), pair);
         }
      }

      return pair;
   }

   /**
    * Checks the event visibility and access against a privilege set.
    * @param privilegeSet The privilege set containing the allowed privileges.
    * @throws SecurityViolationException if the visibility is not public or the access is denied.
    */
   public void checkAccess(PrivilegeSet privilegeSet) throws SecurityViolationException
   {
      if (m_nVisibility != Metaclass.PUBLIC)
      {
         throw new SecurityViolationException("err.rpc.eventVisibility",
            new Object[]{getName(), m_metaclass.getName()});
      }

      if (m_privilege != null && !privilegeSet.contains(m_privilege))
      {
         throw new SecurityViolationException("err.rpc.eventPrivilege",
            new Object[]{getName(), m_metaclass.getName(), m_privilege.getName()});
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      assert m_nTransactionMode != TX_DEFAULT || m_metaclass instanceof Aspect;

      for (Iterator itr = getArgumentIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getActionIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      super.makeReadOnly();

      ((ArrayList)m_actionList).trimToSize(); // free memory not used after compile()
      ((ArrayList)m_argList).trimToSize(); // free memory not used after compile()
   }

   /**
    * Invokes the event in a given context.
    * @param args The event arguments. The first one is the object instance.
    * @param machine The VM.
    * @return The event return value.
    */
   public Object invoke(Object[] args, Machine machine)
   {
      if (m_metaclass.getLogger().isDebugEnabled())
      {
         dump(EMPTY_CLOSURE, args, machine.getContext());
      }

      return machine.invoke(m_function, args);
   }

   /**
    * Invokes the event in a given context.
    * @param accessor The accessor.
    * @param args The event arguments.
    * @param machine The VM.
    * @return The event return value.
    */
   public Object invoke(Accessor accessor, Object[] args, Machine machine)
   {
      if (m_metaclass.getLogger().isDebugEnabled())
      {
         dump(accessor, args, machine.getContext());
      }

      return machine.invoke(m_function, accessor, args);
   }

   /**
    * Invokes the event in a given context.
    * @param args The event arguments.
    * @param machine The VM.
    * @return The event return value.
    */
   public Object invoke(Accessor accessor, Pair args, Machine machine)
   {
      if (m_metaclass.getLogger().isDebugEnabled())
      {
         dump(accessor, Pair.toArray(args), machine.getContext());
      }

      return machine.invoke(m_function, accessor, args);
   }

   /**
    * Dumps the event to the logger.
    * @param nArgCount The argument count.
    * @param machine The VM on which stack are the arguments.
    */
   public void dump(int nArgCount, Machine machine)
   {
      Object[] args = new Object[nArgCount];

      machine.getArgs(args);
      dump(EMPTY_CLOSURE, args, machine.getContext());
   }

   /**
    * Dumps the event to the logger.
    * @param obj The event object.
    * @param args The event argument values.
    * @param ctx The context
    */
   public void dump(Object obj, Object[] args, Context ctx)
   {
      Logger logger = m_metaclass.getLogger();

      logger.debug("Invoking " + this);

      if (logger.isDumpEnabled())
      {
         int nOfs = (obj == EMPTY_CLOSURE) ? 1 : 0;
         
         if (args == null)
         {
            args = EMPTY_SYMBOL_ARRAY;
         }

         logger.dump("this = " + Intrinsic.toString((nOfs == 0) ? obj : args[0]));

         for (int i = 0, n = getArgumentCount() - ((m_bVarArg) ? 1 : 0); i < n; ++i)
         {
            if (i + nOfs >= args.length)
            {
               break;
            }

            logger.dump(getArgument(i).getName() + " = " + Intrinsic.toString(args[i + nOfs]));
         }

         if (m_bVarArg)
         {
            StringBuffer buf = new StringBuffer(128);

            buf.append(getArgument(getArgumentCount() - 1).getName());
            buf.append(" = (");

            for (int i = getArgumentCount() - 1, n = args.length - nOfs; i < n; ++i)
            {
               if (i >= getArgumentCount())
               {
                  buf.append(' ');
               }

               buf.append(Intrinsic.toString(args[i + nOfs]));
            }

            buf.append(')');

            logger.dump(buf.toString());
         }
      }
   }

   /**
    * Append this event arguments to the given buffer in the format
    * (arg1,arg2,..,argN)
    * 
    * @param buf The buffer to which the arguments should be appended
    * @param sSeparator String to separate arguments
    * @param bTyped Include argument/result types
    */
   protected void appendArguments(StringBuilder buf, String sSeparator, boolean bTyped)
   {
      buf.append('(');

      for (int i = 0; i < getArgumentCount(); ++i)
      {
         if (i != 0)
         {
            buf.append(sSeparator);
         }

         if (bTyped)
         {
            getArgument(i).appendTo(buf);
         }
         else
         {
            buf.append(getArgument(i).getName());
         }
      }

      buf.append(')');
   }

   /**
    * Format the URL with the given metaclass in the second field. For example,
    * class:ClassName.eventName(arg1,arg2,..,argN).
    * @param metaclass The class this URL should refer to
    * @return The scripting URL for this event
    */
   protected String getURL(Metaclass metaclass)
   {
      StringBuilder buf = new StringBuilder(64);

      buf.append("class:");
      buf.append(metaclass.getName());
      buf.append(".");
      buf.append(getName());
      appendArguments(buf, ",", false);

      return buf.toString();
   }

   /**
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      if (warnings != null &&
         m_nVisibility == Metaclass.PUBLIC &&
         m_metaclass.getVisibility() == Metaclass.PUBLIC &&
         m_privilege == null &&
         m_accessAttribute == null)
      {
         StringBuilder buf = new StringBuilder(64);

         buf.append(getName());
         appendArguments(buf, ", ", false);

         MetadataValidationException e = new MetadataValidationException(
            "err.meta.missingEventSecurity", new Object[]{buf.toString(), m_metaclass.getName()});

         setProperties(e);
         warnings.addException(e);
      }
   }
   
   /**
    * Creates an incompatible event exception.
    * @param metaclass The metaclass.
    * @param sErrCode The error string identifier.
    * @param argArray The error string arguments.
    */
   private MetadataValidationException createCompatibilityException(Metaclass metaclass, String sErrCode, Object[] argArray)
   {
      MetadataValidationException mve = metaclass.createCompatibilityException(sErrCode, argArray);
      
      mve.setTypeName("Event");
      mve.setProperty("event", getName());
      
      return mve;
   }
   
   /**
    * @see nexj.core.meta.Member#checkCompatibility(nexj.core.meta.Metaclass, nexj.core.util.ExceptionHolder)
    */
   public void checkCompatibility(Metaclass metaclass, ExceptionHolder eh)
   {
      Event event = metaclass.findEvent(getName(), getArgumentCount());
      
      if (event == null)
      {
         eh.addException(createCompatibilityException(metaclass, "err.meta.missingCompatibleEvent",
            new Object[] {getName(), metaclass.getName()}));

         return;
      }

      if (!event.isCompatible())
      {
         eh.addException(createCompatibilityException(metaclass, "err.meta.incompatibleEventProperty",
            new Object[] {"compatible", event.getName(), metaclass.getName()}));
      }
      else if (getVisibility() != event.getVisibility() && event.getVisibility() != Metaclass.PUBLIC)
      {
         eh.addException(createCompatibilityException(metaclass, "err.meta.incompatibleEventProperty",
            new Object[] {"visibility", event.getName(), metaclass.getName()}));
      }     
      else if (event.isVarArg() != isVarArg())
      {
         eh.addException(createCompatibilityException(metaclass, "err.meta.incompatibleEventProperty",
            new Object[] {"vararg", event.getName(), metaclass.getName()}));
      }
      else if (event.isStatic() != isStatic())
      {
         eh.addException(createCompatibilityException(metaclass, "err.meta.incompatibleEventProperty",
            new Object[] {"static", event.getName(), metaclass.getName()}));
      }

      for (int i = 0; i < event.getArgumentCount(); ++i)
      {
         if (!event.getArgument(i).getName().equals(getArgument(i).getName()))
         {
            eh.addException(createCompatibilityException(metaclass, "err.meta.incompatibleEventArgName",
               new Object[] {event.getArgument(i).getName(), event.getName(), metaclass.getName()}));
            
            break;
         }
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      m_metaclass.setProperties(marker);
      marker.setTypeName("Event");
      marker.setProperty("event", m_sName);
      marker.setProperty("args", Primitive.createInteger(getArgumentCount()));
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder(64);

      buf.append("Event ");

      if (getMetaclass() != null)
      {
         buf.append(getMetaclass().getName());
         buf.append('.');
      }

      buf.append(getName());
      appendArguments(buf, ", ", true);

      Argument result = getResult();

      if (result != null)
      {
         buf.append(" :");
         result.appendTo(buf);
      }

      return buf.toString();
   }
}