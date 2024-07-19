package nexj.core.scripting;

import nexj.core.util.NullableHashHolder;

public class EqHashHolder extends NullableHashHolder implements HashFunctionHolder
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = 3818493491771553623L;

   // constructors

   /**
    * Creates a hash holder with an estimated number of values.
    * @param nCount The estimated value count.
    */
   public EqHashHolder(int nCount)
   {
      super(nCount);
   }

   /**
    * Creates a hash holder with an estimated value count of 8.
    */
   public EqHashHolder()
   {
      super();
   }

   // operations

   /**
    * @see nexj.core.util.NullableHashHolder#equal(java.lang.Object, java.lang.Object)
    */
   protected boolean equal(Object left, Object right)
   {
      return Intrinsic.eq(left, right);
   }

   /**
    * @see nexj.core.util.NullableHashHolder#hash(java.lang.Object)
    */
   protected int hash(Object key)
   {
      return (key == null) ? 0 : key.hashCode();
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getEquivalenceFunction()
    */
   public Object getEquivalenceFunction()
   {
      return Intrinsic.EQ_P;
   }

   /**
    * @see nexj.core.scripting.HashFunctionHolder#getHashFunction()
    */
   public Object getHashFunction()
   {
      return Boolean.FALSE;
   }
   
}
