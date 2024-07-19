package nexj.core.persistence;

import nexj.core.runtime.Instance;
import nexj.core.util.RandUtil;

/**
 * Secure random long value generator. 
 */
public class UIDGenerator implements OIDGenerator
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
      return new OID(new Object[]{new Long(RandUtil.getSecureRandom().nextLong())});
   }
}
