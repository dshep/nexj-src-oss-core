// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.util.Collection;
import java.util.List;

import nexj.core.meta.Attribute;
import nexj.core.scripting.Pair;
import nexj.core.util.Sortable;

/**
 * Interface for representing object instance lists, esp. association collections.
 * Supports efficient membership checks and does not allow duplicate objects. 
 */
public interface InstanceList extends List, Sortable
{
   // constants

   /**
    * Default behavior.
    */
   final static int DEFAULT = 0x00;

   /**
    * Replace an already existing instance, if any.
    */
   final static int REPLACE = 0x01;

   /**
    * Do not execute any additional logic.
    */
   final static int DIRECT = 0x02;

   /**
    * Create a weak reference.
    */
   final static int WEAK = 0x04;

   /**
    * Track the changes in the Unit-of-Work.
    */
   final static int TRACK = 0x08;
   
   // operations

   /**
    * Checks the update access of an attribute for all the instances of the list.
    * @param attribute The attribute, which update access to check.
    * @param instance The instance which is going to be assigned.
    * @throws SecurityViolationException if the access is denied.
    */
   void checkUpdateAccess(Attribute attribute, Instance instance) throws SecurityViolationException;

   /**
    * Sets the association which represents this instance list.
    * @param container The association container.
    * @param attribute The association attribute.
    * @param bDirect True to use direct assignment.
    */
   void setAssociation(Instance container, Attribute attribute, boolean bDirect);

   /**
    * @return The instance list container.
    */
   Instance getContainer();

   /**
    * @return The association attribute.
    */
   Attribute getAttribute();

   /**
    * @return The current instance count. Unlike size(), does not cause loading.
    */
   int getCount();
   
   /**
    * Sets the weak reference flag. Expensive operation.
    * @param bWeak The flag to set.
    */
   void setWeak(boolean bWeak);

   /**
    * @return true if the instance list has weak references.
    */
   boolean isWeak();

   /**
    * Sets the lazy flag. Potentially expensive operation.
    * @param bLazy The flag to set.
    */
   void setLazy(boolean bLazy);

   /**
    * @return True if the instance list is not fully loaded.
    */
   boolean isLazy();

   /**
    * Sets the list in a loading state.
    */
   void setLoading();

   /**
    * Loads the specified number of instances.
    * @param attributes The list of attributes to load in the instances.
    * @param where The where clause.
    * @param nCount The number of instances to load.
    * -1 to load the whole list. 0 is equivalent to setWeak(false).
    */
   void load(Pair attributes, Object where, int nCount);

   /**
    * Loads the instance list if it is not loaded.
    */
   void load();

   /**
    * Adds an instance to the list.
    * @param nOrdinal The position into which to add the instance.
    * @param instance The instance to add.
    * @param nFlags Combination of REPLACE, DIRECT, WEAK and TRACK, or DEFAULT. 
    * @return True if the item has been added (not replaced).
    * @throws nexj.core.util.DuplicateItemException if bReplace is false and the instance is already in the list.
    */
   boolean add(int nOrdinal, Instance instance, int nFlags);

   /**
    * Adds an instance to the end of the list.
    * @param instance The instance to add.
    * @param nFlags Combination of REPLACE, DIRECT and WEAK, or DEFAULT. 
    * @return True if the item has been added (not replaced).
    * @throws nexj.core.util.DuplicateItemException if bReplace is false and the instance is already in the list.
    */
   boolean add(Instance instance, int nFlags);

   /**
    * Gets an object instance by ordinal number.
    * @param nOrdinal The object ordinal number in the collection.
    * @return The object instance. Can be null, if the list has weak references.
    */
   Instance getInstance(int nOrdinal);

   /**
    * Removes an instance from the list.
    * @param nOrdinal The position from which to remove the instance.
    * @param nFlags DIRECT or DEFAULT.
    * @return The instance which was removed.
    */
   Instance remove(int nOrdinal, int nFlags);

   /**
    * Removes an instance from the list.
    * @param instance The instance to remove.
    * @param nFlags Combination of DIRECT and WEAK, or DEFAULT.
    * @return True if the instance was removed from the list, false if it was not in the list.
    */
   boolean remove(Instance instance, int nFlags);

   /**
    * @return True if there have been changes in the list.
    * The changes are tracked only when the list is an association.
    * The list becomes clean again after all the Units-of-Work have invoked
    * complete() on the list.
    * @see InstanceList#complete(UnitOfWork, boolean, boolean)
    * @see InstanceList#finish()
    */
   boolean isDirty();

   /**
    * @return True if there have been unfinished changes on the list. 
    * @see InstanceList#complete(UnitOfWork, boolean, boolean)
    */
   boolean isUpdated();

   /**
    * Completes all the tracked changes for a given Unit-of-Work.
    * This method is for INTERNAL USE ONLY.
    * @param uow The Unit-of-Work, for which to complete the changes, or null for all.
    * @param bCommit True to commit, false to roll back.
    * @param bDelta True to skip the unfinished changes.
    * @see InstanceList#finish()
    */
   void complete(UnitOfWork uow, boolean bCommit, boolean bDelta);

   /**
    * Marks all the tracked changes as finished.
    */
   void finish();

   /**
    * Reverses the object instance order in the array.
    */
   void reverse();

   /**
    * @return A linked list containing the list elements.
    */
   Pair list();

   /**
    * Selects the values of the attribute from the association path as a new collection.
    * @param path The association path. Can be null.
    * @return The selected collection.
    */
   Collection select(Pair path); 

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone();
}
