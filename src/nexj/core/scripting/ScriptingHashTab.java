// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import nexj.core.runtime.ThreadContextHolder;
import nexj.core.util.NullableHashTab;

/**
 * Hash table implementation with arbitrary hash and equivalence functions.
 * The focus is on efficiency. No error checking is provided.
 */
public class ScriptingHashTab extends NullableHashTab implements HashFunctionHolder
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = -6480328189385502724L;

   // associations

   /**
    * The equivalence function.
    */
   protected Function m_equivFunction;

   /**
    * The hash function.
    */
   protected Function m_hashFunction;

   // constructors

   /**
    * Creates a hash table with an estimated number of key-value pairs.
    * @param hash The hash function.
    * @param equiv The equivalence function.
    * @param nCount The estimated key-value pair count.
    */
   public ScriptingHashTab(Function hash, Function equiv, int nCount)
   {
      super(nCount);
      m_equivFunction = equiv;
      m_hashFunction = hash;
   }

   /**
    * Creates a hash table with an estimated key-value pair count of 8.
    * @param hash The hash function.
    * @param equiv The equivalence function.
    */
   public ScriptingHashTab(Function hash, Function equiv)
   {
      super();
      m_hashFunction = hash;
      m_equivFunction = equiv;
   }

   // operations

   /**
    * @see nexj.core.util.GenericHashTab#equal(java.lang.Object, java.lang.Object)
    */
   protected boolean equal(Object left, Object right)
   {
      return Intrinsic.isTrue(
         ThreadContextHolder.getContext().getMachine().invoke(m_equivFunction, left, right, null));
   }

   /**
    * @see nexj.core.util.GenericHashTab#hash(java.lang.Object)
    */
   protected int hash(Object key)
   {
      Object hashCode = ThreadContextHolder.getContext().getMachine().invoke(m_hashFunction, key, (Pair)null);

      if (!(hashCode instanceof Number))
      {
         throw new ScriptingException("err.scripting.hashFunctionResult");
      }

      return ((Number)hashCode).intValue();
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getEquivalenceFunction()
    */
   public Object getEquivalenceFunction()
   {
      return m_equivFunction;
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getHashFunction()
    */
   public Object getHashFunction()
   {
      return m_hashFunction;
   }
}
