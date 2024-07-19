// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

/**
 * Interface for associated query holders.
 */
public interface QueryHolder
{
   /**
    * @return The associated query.
    */
   Query getQuery();

   /**
    * Sets the associated query.
    * @param query The query to set.
    */
   void setQuery(Query query);
}
