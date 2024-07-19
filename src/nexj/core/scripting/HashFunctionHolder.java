// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

/**
 * Interface providing access to the hash function and
 * the equivalence function of a hash table.
 */
public interface HashFunctionHolder
{
   /**
    * @return The hash function.
    */
   Object getHashFunction();

   /**
    * @return The equivalence function.
    */
   Object getEquivalenceFunction();
}
