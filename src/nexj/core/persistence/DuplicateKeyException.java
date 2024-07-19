// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import nexj.core.runtime.Context;
import nexj.core.runtime.ContextHolder;
import nexj.core.scripting.Pair;
import nexj.core.util.Named;
import nexj.core.util.StringId;

/**
 * Exception caused by a duplicate key in the persistent store
 */
public class DuplicateKeyException extends ConstraintViolationException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 1131020240514934252L;

   // constructors

   /**
    * Constructs the exception for a given location.
    * @param location The location of the duplicate key.
    * @param key The duplicate key specification: list of symbols.
    * @param cause The original exception that has caused this exception.
    */
   public DuplicateKeyException(LazyLocation location, Pair keys, Throwable cause)
   {
      super((keys == null) ? "err.persistence.duplicateKeyClass" : "err.persistence.duplicateKey",
         new Object[]
         {
            new StringId((location == null) ? "Object" : location.getLazyCaption()),
            join(keys, ", "),
            null // This can be assigned later on for custom error codes
         }, cause);

      if (location instanceof ContextHolder)
      {
         Context context = ((ContextHolder)location).getContext();

         if (context != null)
         {
            StringBuffer buf = new StringBuffer(128);

            buf.append(m_sErrCode);
            buf.append('.');
            buf.append((location == null) ? "Object" : location.getLazyClassName());

            if (keys != null)
            {
               buf.append('.');
               append(buf, keys, "-");
            }

            String sFullId = buf.toString();

            if (context.getString(sFullId) != sFullId)
            {
               m_sErrCode = sFullId;

               if (location instanceof Named)
               {
                  m_argArray[2] = ((Named)location).getName();
               }
            }
         }
      }

      setLocation(location);

      for (; keys != null; keys = keys.getNext())
      {
         addException(keys.getHead().toString(), this);
      }
   }

   // operations

   /**
    * Appends a pair list to a string buffer.
    * @param buf The destination buffer.
    * @param pair The list to append.
    * @param sDelim The delimiter.
    */
   protected static void append(StringBuffer buf, Pair pair, String sDelim)
   {
      while (pair != null)
      {
         buf.append(pair.getHead());
         pair = pair.getNext();

         if (pair != null)
         {
            buf.append(sDelim);
         }
      }
   }

   /**
    * Joins the strings in a list. 
    * @param pair The list to join.
    * @param sDelim The delimiter.
    * @return The joined string.
    */
   protected static String join(Pair pair, String sDelim)
   {
      StringBuffer buf = new StringBuffer(64);

      append(buf, pair, sDelim);

      return buf.toString();
   }
}
