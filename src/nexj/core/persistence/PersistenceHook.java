// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import nexj.core.runtime.Instance;
import nexj.core.runtime.UnitOfWork;

/**
 * Interface for modifying the persistence operations.
 */
public interface PersistenceHook
{
   /**
    * Adds work items or dependencies between them to the UOW.
    * @param uow The unit of work.
    * @param instance The instance to process.
    */
   void addWork(UnitOfWork uow, Instance instance);
   
   /**
    * Invoked before a work item is prepared for execution.
    * @param work The work item to prepare.
    */
   void prepare(Work work);
}
