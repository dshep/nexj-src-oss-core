package nexj.core.scripting;

import nexj.core.util.NullableHashHolder;

public class EqualHashHolder extends NullableHashHolder implements HashFunctionHolder
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = 2136052437815446959L;

   // constructors

   /**
    * Creates a hash holder with an estimated number of values.
    * @param nCount The estimated value count.
    */
   public EqualHashHolder(int nCount)
   {
      super(nCount);
   }

   /**
    * Creates a hash holder with an estimated value count of 8.
    */
   public EqualHashHolder()
   {
      super();
   }

   // operations

   /**
    * @see nexj.core.util.NullableHashHolder#equal(java.lang.Object, java.lang.Object)
    */
   protected boolean equal(Object left, Object right)
   {
      return Intrinsic.equal(left, right);
   }

   /**
    * @see nexj.core.util.NullableHashHolder#hash(java.lang.Object)
    */
   protected int hash(Object key)
   {
      return EqualHashTab.equalHash(key);
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getEquivalenceFunction()
    */
   public Object getEquivalenceFunction()
   {
      return Intrinsic.EQUAL_P;
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getHashFunction()
    */
   public Object getHashFunction()
   {
      return Boolean.FALSE;
   }
}
