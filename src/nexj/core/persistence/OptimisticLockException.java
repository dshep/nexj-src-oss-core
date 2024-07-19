// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import nexj.core.util.PropertyMap;
import nexj.core.util.StringId;

/**
 * Exception thrown when an optimistic lock has failed.
 */
public class OptimisticLockException extends LocatorPersistenceException
{
   // constants
   
   /**
    * The serialization UID.
    */
   private final static long serialVersionUID = 6500756318147824797L;

   // constructors

   /**
    * Constructs the exception.
    * @param location The location where the lock has failed.
    */
   public OptimisticLockException(LazyLocation location)
   {
      super("err.persistence.optimisticLock",
         new Object[]
         {
            new StringId((location == null) ? "Object" : location.getLazyCaption())
         });

      setLocation(location);
   }

   /**
    * Constructs the exception.
    * @param holder The holder containing the OID of the instance where the lock
    * has been violated.
    */
   public OptimisticLockException(OIDHolder holder)
   {
      super("err.persistence.optimisticLock",
         new Object[]
         {
            (holder instanceof PropertyMap && ((PropertyMap)holder).getClassName() != null) ?
               ((PropertyMap)holder).getClassName() : "Object"
         }
      );

      setClassName((holder instanceof PropertyMap) ? ((PropertyMap)holder).getClassName() : null); 
      setOIDHolder(holder);
   }

   // operations

   /**
    * @see nexj.core.util.ErrorCode#isSystem()
    */
   public boolean isSystem()
   {
      return false;
   }
}
