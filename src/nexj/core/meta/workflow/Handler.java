// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import java.util.Iterator;

import nexj.core.meta.Attribute;
import nexj.core.meta.Event;
import nexj.core.meta.Metaclass;

/**
 * Interface implemented by event handling steps.
 */
public interface Handler
{
   /**
    * Adds a new association to the handler.
    * @param association The association to add.
    */
   void addAssociation(Attribute association);
   
   /**
    * Gets a association by ordinal number.
    * @param nOrdinal The association ordinal number (0-based).
    * @return The association object.
    */
   Attribute getAssociation(int nOrdinal);
   
   /**
    * @return The association count.
    */
   int getAssociationCount();
   
   /**
    * @return An iterator for the contained association objects.
    */
   Iterator getAssociationIterator();
   
   /**
    * @return The target class of the event.
    */
   Metaclass getTarget();
   
   /**
    * Sets the handler event.
    * @param sName The event name.
    * @param nArgCount The event argument count.
    */
   void setEvent(String sName, int nArgCount);

   /**
    * Sets the action event.
    * @param event The action event to set.
    */
   void setEvent(Event event);

   /**
    * @return The action event.
    */
   Event getEvent();
   
   /**
    * Sets the event condition.
    * @param condition The condition to set.
    */
   void setCondition(Object condition);
   
   /**
    * @return The event condition.
    */
   public Object getCondition();
}
