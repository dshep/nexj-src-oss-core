// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import nexj.core.runtime.ThreadContextHolder;
import nexj.core.util.NullableHashHolder;

/**
 * Hash holder implementation with arbitrary hash and equivalence functions.
 * The focus is on efficiency. No error checking is provided.
 */
public class ScriptingHashHolder extends NullableHashHolder implements HashFunctionHolder
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = 7266371505962521638L;

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
    * Creates a hash holder with an estimated number of values.
    * @param hash The hash function.
    * @param equiv The equivalence function.
    * @param nCount The estimated value count.
    */
   public ScriptingHashHolder(Function hash, Function equiv, int nCount)
   {
      super(nCount);
      m_equivFunction = equiv;
      m_hashFunction = hash;
   }

   /**
    * Creates a hash holder with an estimated value count of 8.
    * @param hash The hash function.
    * @param equiv The equivalence function.
    */
   public ScriptingHashHolder(Function hash, Function equiv)
   {
      super();
      m_hashFunction = hash;
      m_equivFunction = equiv;
   }

   // operations

   /**
    * @see nexj.core.util.GenericHashHolder#equal(java.lang.Object, java.lang.Object)
    */
   protected boolean equal(Object left, Object right)
   {
      return Intrinsic.isTrue(
         ThreadContextHolder.getContext().getMachine().invoke(m_equivFunction, left, right, null));
   }

   /**
    * @see nexj.core.util.GenericHashHolder#hash(java.lang.Object)
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
