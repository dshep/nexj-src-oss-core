package nexj.core.util;

import java.util.Collection;


/**
 * Case-insensitive keyword table.
 */
public class KeywordTab extends GenericHashHolder
{
   // constants

   /**
    * Serialization UID.
    */
   private static final long serialVersionUID = -8729930626849137145L;

   // constructors

   /**
    * Constructs the table.
    * @param nCount The estimated keyword count.
    */
   public KeywordTab(int nCount)
   {
      super(nCount << 1);
   }

   /**
    * Constructs the table from an array.
    * @param keywordArray The keyword array.
    */
   public KeywordTab(String[] keywordArray)
   {
      this(keywordArray, null);
   }

   /**
    * Constructs the table from an array and a collection.
    * @param keywordArray The keyword array.
    * @param base The base collection. Can be null.
    */
   public KeywordTab(String[] keywordArray, Collection base)
   {
      this(keywordArray.length + ((base == null) ? 0 : base.size()));

      for (int i = 0; i < keywordArray.length; ++i)
      {
         add(keywordArray[i]);
      }

      if (base != null)
      {
         addAll(base);
      }
   }

   // operations

   /**
    * @see nexj.core.util.GenericHashHolder#equal(java.lang.Object, java.lang.Object)
    */
   protected boolean equal(Object left, Object right)
   {
      return ((String)left).equalsIgnoreCase((String)right);
   }

   /**
    * @see nexj.core.util.GenericHashHolder#hash(java.lang.Object)
    */
   protected int hash(Object key)
   {
      return StringUtil.hashIgnoreCase((String)key);
   }
}