// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Attribute;
import nexj.core.meta.Event;
import nexj.core.meta.Member;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.CompilerException;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.EmptyIterator;
import nexj.core.util.HashTab2D;
import nexj.core.util.Lookup2D;

/**
 * Workflow metadata.
 */
public final class Workflow extends Flow implements Handler
{
   // constants

   /**
    * The empty association.
    */
   protected final static Pair EMPTY_ASSOC = new Pair(null); 

   // associations

   /**
    * The containing metaclass.
    */
   protected Metaclass m_metaclass;

   /**
    * The attribute list for primary object retrieval.
    */
   protected Pair m_attributes;

   /**
    * The association collection.
    */
   protected List m_associationList; // of type Attribute

   /**
    * The action event.
    */
   protected Event m_event;

   /**
    * The condition.
    */
   protected Object m_condition;

   /**
    * Map of event objects to scripting functions implementing the state management: Function[Event][Association].
    * On the first pass it maps the event objects to cond code: Object[Event][Association].
    */
   protected Lookup2D m_functionMap = new HashTab2D();

   // constructors

   /**
    * Constructs the workflow.
    * @param sName The workflow name.
    * @param sResourceName The metadata resource name.
    * @param nVersion The flow version.
    * @param metaclass The containing metaclass.
    * @param attributes The attribute list for primary object retrieval.
    * @param sCaption The flow caption.
    */
   public Workflow(String sName, String sResourceName, int nVersion, Metaclass metaclass, Pair attributes, String sCaption)
   {
      super(sName, nVersion);

      m_metaclass = metaclass;
      m_attributes = attributes;
      setCaption(sCaption);
      setResourceName(sResourceName);
   }
   
   // operations

   /**
    * @see nexj.core.meta.workflow.Flow#getMetadata()
    */
   public Metadata getMetadata()
   {
      return m_metaclass.getMetadata();
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getPropName()
    */
   public String getPropName()
   {
      return "workflow";
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getTypeName()
    */
   protected String getTypeName()
   {
      return "Workflow";
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getURLPrefix()
    */
   protected String getURLPrefix()
   {
      return "workflow:";
   }

   /**
    * Sets the containing metaclass.
    * @param metaclass The containing metaclass to set.
    */
   public void setMetaclass(Metaclass metaclass)
   {
      verifyNotReadOnly();
      m_metaclass = metaclass;
   }

   /**
    * @return The containing metaclass.
    */
   public Metaclass getMetaclass()
   {
      return m_metaclass;
   }

   /**
    * Sets the attribute list for primary object retrieval.
    * @param attributes The attribute list for primary object retrieval to set.
    */
   public void setAttributes(Pair attributes)
   {
      verifyNotReadOnly();
      m_attributes = attributes;
   }

   /**
    * @return The attribute list for primary object retrieval.
    */
   public Pair getAttributes()
   {
      return m_attributes;
   }
   
   /**
    * @see Flow#isCurrentVersion()
    */
   public boolean isCurrentVersion()
   {
      return m_metaclass.getMetadata().getWorkflow(getName()).getVersion() == m_nVersion;
   }

   /**
    * Adds a new association to the action.
    * @param association The association to add.
    */
   public void addAssociation(Attribute association)
   {
      verifyNotReadOnly();

      if (association.getReverse() == null)
      {
         throw new MetadataException("err.meta.workflow.association",
            new Object[]{association.getName(), association.getMetaclass().getName(), getFullName()});
      }
      
      if (association.getReverse().isCollection())
      {
         throw new MetadataException("err.meta.workflow.associationMultiplicity",
            new Object[]{association.getName(), association.getMetaclass().getName(), getFullName()});
      }

      if (m_associationList == null)
      {
         m_associationList = new ArrayList(4);
      }
      
      m_associationList.add(association);
   }

   /**
    * Gets a association by ordinal number.
    * @param nOrdinal The association ordinal number (0-based).
    * @return The association object.
    */
   public Attribute getAssociation(int nOrdinal)
   {
      return (Attribute)m_associationList.get(nOrdinal);
   }

   /**
    * @return The association count.
    */
   public int getAssociationCount()
   {
      if (m_associationList == null)
      {
         return 0;
      }
      
      return m_associationList.size();
   }

   /**
    * @return An iterator for the contained association objects.
    */
   public Iterator getAssociationIterator()
   {
      if (m_associationList == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_associationList.iterator();
   }

   /**
    * @return The target class of the event.
    */
   public Metaclass getTarget()
   {
      if (m_associationList != null)
      {
         return (Metaclass)((Attribute)m_associationList.get(m_associationList.size() - 1)).getType();
      }

      return m_metaclass;
   }

   /**
    * Sets the action event.
    * @param sName The event name.
    * @param nArgCount The event argument count.
    */
   public void setEvent(String sName, int nArgCount)
   {
      if (nArgCount < 0)
      {
         throw new MetadataException("err.meta.workflow.negativeEventArgCount",
            new Object[]{sName, getFullName()});
      }

      Member member = getTarget().getSelector(sName).getMember(nArgCount);

      if (!(member instanceof Event))
      {
         throw new MetadataException("err.meta.workflow.invalidEvent",
            new Object[]{sName, member.getMetaclass().getName(), getFullName()});
      }

      setEvent((Event)member);
   }

   /**
    * Sets the action event.
    * @param event The action event to set.
    */
   public void setEvent(Event event)
   {
      verifyNotReadOnly();

      if (event != null)
      {
         if (event.isStatic())
         {
            throw new MetadataException("err.meta.workflow.staticActionEvent",
               new Object[]{event.getName(), event.getMetaclass().getName(),
                  getFullName(), m_metaclass.getName()});
         }
      }
      
      m_event = event;
   }

   /**
    * @return The action event.
    */
   public Event getEvent()
   {
      return m_event;
   }
   
   /**
    * Sets the condition.
    * @param condition The condition to set.
    */
   public void setCondition(Object condition)
   {
      verifyNotReadOnly();
      m_condition = condition;
   }

   /**
    * @return The condition.
    */
   public Object getCondition()
   {
      return m_condition;
   }

   /**
    * Adds a state management function code to the flow.
    * @param step The step that provides the code.
    * @param assoc The association path to the instance handling the event.
    * @param event The event, which is handled by the function.
    * @param code The function code.
    */
   public void addFunction(Step step, Pair assoc, Event event, Object code)
   {
      verifyNotReadOnly();

      if (assoc == null)
      {
         assoc = EMPTY_ASSOC;
      }

      m_functionMap.put(event, assoc, new Pair(code, m_functionMap.get(event, assoc)));
      setPosURLs(code, step.getName());
   }

   /**
    * Gets a state management function list for a given event.
    * @param assoc The association path to the instance handling the event.
    * @param event The event for which to get the function.
    * @return The state management function list.
    */
   public Pair getFunctions(Pair assoc, Event event)
   {
      if (assoc == null)
      {
         assoc = EMPTY_ASSOC;
      }

      return (Pair)m_functionMap.get(event, assoc);
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getEmptyFunction()
    */
   public PCodeFunction getEmptyFunction()
   {
      return Machine.EMPTY_FUNCTION;
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getDefaultReturnCode()
    */
   public Pair getDefaultReturnCode()
   {
      return null;
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getArgument(int)
    */
   public Variable getArgument(int nOrdinal)
   {
      throw new ArrayIndexOutOfBoundsException();
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getArgument(java.lang.String)
    */
   public Variable getArgument(String sName) throws MetadataLookupException
   {
      throw new MetadataLookupException("err.meta." + getPropName() + ".argLookup", sName, this);
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getArgumentCount()
    */
   public int getArgumentCount()
   {
      return 0;
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getArgumentIterator()
    */
   public Iterator getArgumentIterator()
   {
      return EmptyIterator.getInstance();
   }

   /**
    * @see nexj.core.meta.workflow.Flow#generate(nexj.core.scripting.Machine)
    */
   protected void generate(Machine machine)
   {
      // Generate the flow startup action for the latest version of the flow 
      if (m_event != null && isCurrentVersion())
      {
         m_event.addFlowStarter(this);
      }

      super.generate(machine);

      generateDeleteFlowAction(m_metaclass.findEvent("delete", 0));
   }
   
   /**
    * @see nexj.core.meta.workflow.Flow#compile(nexj.core.scripting.Machine)
    */
   protected void compile(Machine machine) throws CompilerException
   {
      super.compile(machine);

      Lookup2D map = new HashTab2D(m_functionMap.size());

      // Compile the flow functions
      for (Lookup2D.Iterator itr = m_functionMap.valueIterator(); itr.hasNext();)
      {
         itr.next();

         Event event = (Event)itr.getKey1();
         Pair body = (Pair)machine.invoke((Function)machine.getGlobalEnvironment()
            .getVariable(Symbol.SYS_GENERATE_FLOW_FUNCTION), itr.getValue(), (Object[])null);

         itr.setValue(new Pair(new Compiler().compile(m_variables,
            event.getArguments(true), body, m_posMap, m_urlMap, machine)));
         map.put(event.getRoot(), itr.getKey2(), null);
      }

      for (Lookup2D.Iterator itr = map.valueIterator(); itr.hasNext();)
      {
         itr.next();

         addDerivedFlowFunctions((Pair)itr.getKey2(), (Event)itr.getKey1(),
            (Pair)m_functionMap.get(itr.getKey1(), itr.getKey2()));
      }
   }

   /**
    * Adds the flow functions from an event to its derived events.
    * @param assoc The association path to the instance handling the event.
    * @param event The event.
    * @param pair The flow function list.
    */
   protected void addDerivedFlowFunctions(Pair assoc, Event event, Pair pair)
   {
      Metaclass metaclass = event.getMetaclass();
      
      for (int i = 0, nCount = metaclass.getDerivedCount(); i < nCount; ++i)
      {
         Event derived = metaclass.getDerived(i).findEvent(event.getName(), event.getArgumentCount());
         Pair dpair = (Pair)m_functionMap.get(derived, assoc);
         
         if (pair != null)
         {
            dpair = Pair.append(pair, dpair);
            m_functionMap.put(derived, assoc, dpair);
         }

         addDerivedFlowFunctions(assoc, derived, dpair);
      }
   }

   /**
    * Generates a delete flow action in the delete event
    * of the associated class and its subclasses.
    * @param event The delete event.
    */
   protected static void generateDeleteFlowAction(Event event)
   {
      if (event == null)
      {
         return;
      }
      
      if (event.generateDeleteFlowAction())
      {
         Metaclass metaclass = event.getMetaclass();
         
         for (int i = 0, nCount = metaclass.getDerivedCount(); i < nCount; ++i)
         {
            generateDeleteFlowAction(metaclass.getDerived(i).findEvent(event.getName(), event.getArgumentCount()));
         }
      }
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getSysMetaclass()
    */
   public String getSysMetaclass()
   {
      return "SysWorkflow";
   }
}
