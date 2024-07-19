package nexj.core.scripting;

import nexj.core.util.NullableHashHolder;

public class EqvHashHolder extends NullableHashHolder implements HashFunctionHolder
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = -6152463208184163989L;

   // constructors

   /**
    * Creates a hash holder with an estimated number of values.
    * @param nCount The estimated value count.
    */
   public EqvHashHolder(int nCount)
   {
      super(nCount);
   }

   /**
    * Creates a hash holder with an estimated value count of 8.
    */
   public EqvHashHolder()
   {
      super();
   }

   // operations

   /**
    * @see nexj.core.util.NullableHashHolder#equal(java.lang.Object, java.lang.Object)
    */
   protected boolean equal(Object left, Object right)
   {
      return Intrinsic.eqv(left, right);
   }

   /**
    * @see nexj.core.util.NullableHashHolder#hash(java.lang.Object)
    */
   protected int hash(Object key)
   {
      if (key == null)
      {
         return 0;
      }

      if (key instanceof Number)
      {
         return EqvHashTab.hash((Number)key);
      }

      return key.hashCode();
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getEquivalenceFunction()
    */
   public Object getEquivalenceFunction()
   {
      return Intrinsic.EQV_P;
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getHashFunction()
    */
   public Object getHashFunction()
   {
      return Boolean.FALSE;
   }
}
