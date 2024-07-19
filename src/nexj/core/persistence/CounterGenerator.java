// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import nexj.core.meta.Metaclass;
import nexj.core.runtime.Instance;

/**
 * OID generator using a persistent counter.
 */
public class CounterGenerator implements OIDGenerator
{
   // constants

   /**
    * Value count for metadata validation.
    * Used by the metadata loader.
    */
   public final static int VALUE_COUNT = 1;

   // operations

   /**
    * @see nexj.core.persistence.OIDGenerator#generateOID(nexj.core.runtime.Instance, nexj.core.persistence.PersistenceAdapter)
    */
   public OID generateOID(Instance instance, PersistenceAdapter adapter)
   {
      Metaclass metaclass = instance.getMetaclass().getPersistenceRoot();

      return new OID(new Object[]{instance.getInvocationContext().getMetadata()
         .getMetaclass("SysCounter").invoke("next", new Object[]{"class." + metaclass.getName()})});
   }
}
