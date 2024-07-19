// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Attribute;
import nexj.core.meta.Event;
import nexj.core.meta.Member;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataException;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.util.EmptyIterator;

/**
 * Base class for steps triggered by events.
 */
public abstract class HandlerStep extends Step implements Handler
{
   // associations
   
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

   // constructors

   /**
    * Constructs the step.
    * @param sName The step name.
    * @param activity The containing activity.
    */
   public HandlerStep(String sName, Activity activity)
   {
      super(sName);

      setActivity(activity);
   }

   // operations
   
   /**
    * Adds a new association to the step.
    * @param association The association to add.
    */
   public void addAssociation(Attribute association)
   {
      verifyNotReadOnly();

      if (association.getReverse() == null)
      {
         throw new MetadataException("err.meta.workflow.actionAssociation",
            new Object[]{association.getName(), association.getMetaclass().getName(),
            getActivity().getFlow().getFullName()});
      }
      
      if (association.getReverse().isCollection())
      {
         throw new MetadataException("err.meta.workflow.actionAssociationMultiplicity",
            new Object[]{association.getName(), association.getMetaclass().getName(),
            getActivity().getFlow().getFullName()});
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
      
      return getActivity().getFlow().getMetaclass();
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
            new Object[]{sName, getActivity().getFlow().getFullName()});
      }

      Member member = getTarget().getSelector(sName).getMember(nArgCount);

      if (!(member instanceof Event))
      {
         throw new MetadataException("err.meta.workflow.invalidEvent",
            new Object[]{sName, member.getMetaclass().getName(),
            getActivity().getFlow().getFullName()});
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
               getActivity().getFlow().getName(), getActivity().getFlow().getMetaclass().getName()});
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
    * @see nexj.core.meta.workflow.Step#isPersistent()
    */
   public boolean isPersistent()
   {
      return true;
   }

   /**
    * Sets the executable code.
    */
   protected abstract void setBody(Pair body);
   
   /**
    * @return The executable code.
    */
   protected abstract Pair getBody();

   /**
    * @see nexj.core.meta.workflow.Step#generate(nexj.core.scripting.Machine)
    */
   public void generate(Machine machine)
   {
      verifyNotReadOnly();

      Pair assoc = null;

      for (int i = 0, n = getAssociationCount(); i < n; ++i)
      {
         assoc = new ConstPair(getAssociation(i).getReverse().getSymbol(), assoc);
      }

      m_event.addFlowAssoc(assoc);

      ((Workflow)m_activity.getFlow()).addFunction(this, assoc,
         m_event, new Pair(this, new Pair(m_condition, getBody())));

      setBody(null);
   }

   /**
    * @see nexj.core.meta.workflow.Step#step(nexj.core.meta.workflow.State)
    */
   public PCodeFunction step(State state)
   {
      state.remove(this);
      state.add(m_next);

      return null;
   }
}
