// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import nexj.core.runtime.Instance;


/**
 * Interface implemented by OID generators.
 */
public interface OIDGenerator
{
   /**
    * Generates a new OID.
    * @param instance The instance for which to generate the OID.
    * @param adapter The instance persistence adapter.
    * @return The generated OID.
    */
   OID generateOID(Instance instance, PersistenceAdapter adapter);
}
