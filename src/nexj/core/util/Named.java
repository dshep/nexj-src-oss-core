// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Comparator;

/**
 * Interface implemented by objects exposing a name.
 */
public interface Named
{
   // constants
   
   /**
    * Name comparator.
    */
   final static Comparator COMPARATOR = new Comparator()
   {
      public int compare(Object left, Object right)
      {
         return ((Named)left).getName().compareToIgnoreCase(((Named)right).getName());
      }
   };

   // operations

   /**
    * Returns the name of the object.
    * @return The name.
    */
   String getName();
}
