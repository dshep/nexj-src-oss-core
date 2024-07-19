package nexj.core.persistence;

import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.runtime.Instance;

/**
 * Interface for dynamically resolving persistence objects.
 */
public interface PersistenceResolver
{
   /**
    * Computes the persistence mapping of an instance.
    * @param instance The instance.
    * @return The persistence mapping. Can be null.
    */
   PersistenceMapping getMapping(Instance instance);

   /**
    * Computes the data source fragment name of an instance.
    * @param instance The instance.
    * @return The fragment name. Can be null.
    */
   String getFragmentName(Instance instance);

   /**
    * Modifies a query node persistence mapping and related settings.
    * @param query The query node to modify.
    */
   void resolve(Query query);
}
