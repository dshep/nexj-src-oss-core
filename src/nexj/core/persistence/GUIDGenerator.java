// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import nexj.core.runtime.Instance;
import nexj.core.util.GUIDUtil;

/**
 * GUID OID generator.
 */
public class GUIDGenerator implements OIDGenerator
{
   // constants

   /**
    * Value count for metadata validation.
    * Used by the metadata loader.
    */
   public final static int VALUE_COUNT = 1;

   // operations

   /**
    * @see nexj.core.persistence.OIDGenerator#generateOID(Instance, nexj.core.persistence.PersistenceAdapter)
    */
   public OID generateOID(Instance instance, PersistenceAdapter adapter)
   {
      return new OID(new Object[]{GUIDUtil.generateGUID()});
   }
}
