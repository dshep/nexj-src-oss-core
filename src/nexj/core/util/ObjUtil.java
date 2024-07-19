// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Iterator;

/**
 * Common object utilities.
 */
public class ObjUtil
{
   // constants

   /**
    * Empty stack trace.
    */
   public final static StackTraceElement[] EMPTY_STACK_TRACE = new StackTraceElement[0];

   // constructors

   /**
    * Prevents construction.
    */
   protected ObjUtil()
   {
   }

   // operations

   /**
    * Compares two objects using Object.equals() and
    * taking into account null references.
    * @param left The left instance. Can be null.
    * @param right The right instance. Can be null.
    * @return True if the objects are equal (or both are null references).
    */
   public static boolean equal(Object left, Object right)
   {
      if (left == null)
      {
         return right == null;
      }
      
      return left.equals(right);
   }
   
   /**
    * Compares two objects using obj.compareTo() and 
    * taking into account null references.
    * Non-null reference is always greater than a null reference.
    * @param left The left instance. Can be null.
    * @param right The right instance. Can be null.
    * @return The return value of compareTo().
    */
   public static int compare(Comparable left, Object right)
   {
      if (left == null)
      {
         return (right == null) ? 0 : -1;
      }
      
      if (right == null)
      {
         return 1;
      }
      
      return left.compareTo(right);
   }

   /**
    * Extracts a message from a throwable.
    * @param t The throwable.
    * @return The extracted message.
    */
   public static String getMessage(Throwable t)
   {
      String sMsg = t.getLocalizedMessage();
      
      if (sMsg == null || sMsg.length() == 0)
      {
         sMsg = t.getClass().getName();
      }

      return sMsg;
   }

   /**
    * Gets a short class name (without packages) of an object.
    * @param obj The object. Can be null.
    * @return The short class name.
    */
   public static String getShortClassName(Object obj)
   {
      if (obj == null)
      {
         return "null";
      }

      String sName = obj.getClass().getName();

      return sName.substring(sName.lastIndexOf('.') + 1);
   }

   /**
    * Rethrows an exception as a runtime exception.
    * @param t The exception to rethrow.
    * @return Does not return anything. Throw the return value to suppress compiler warnings.
    */
   public static RuntimeException rethrow(Throwable t) throws RuntimeException, Error
   {
      if (t instanceof RuntimeException)
      {
         throw (RuntimeException)t;
      }

      if (t instanceof Error)
      {
         throw (Error)t;
      }

      throw new WrapperException(t);
   }

   /**
    * Determines whether a given exception is a potentially critical error.
    * @param t The exception to test.
    * @return True if the exception is an error.
    */
   public static boolean isError(Throwable t)
   {
      return !(t instanceof ErrorCode); 
   }

   /**
    * Determines whether a given exception is a system error,
    * which should not be shown directly to end-users. 
    * @param t The exception to test.
    * @return True if the exception is an error.
    */
   public static boolean isSystem(Throwable t)
   {
      return !(t instanceof ErrorCode) || ((ErrorCode)t).isSystem(); 
   }

   /**
    * Fills the array with items from the iterator.
    * @param destArray The array to fill.
    * @param sourceItr The iterator from which to retrieve the items.
    * @return The actual number of items copied.
    */
   public static int copy(Object[] destArray, Iterator sourceItr)
   {
      return copy(destArray, 0, destArray.length, sourceItr);
   }

   /**
    * Copies items from the iterator to the array. Stops when the iterator provides no more
    * items or nLength items have been copied.
    * @param destArray The array to fill.
    * @param nStart The starting index in the array.
    * @param nLength The count of items to copy.
    * @param sourceItr The iterator from which to retrieve the items.
    * @return The actual number of items copied.
    */
   public static int copy(Object[] destArray, int nStart, int nLength, Iterator sourceItr)
   {
      int nEnd = nStart + nLength;
      int nCount = 0;

      for (int i = nStart; sourceItr.hasNext() && i < nEnd; i++)
      {
         destArray[i] = sourceItr.next();
         nCount++;
      }

      return nCount;
   }
}
