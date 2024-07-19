// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Comparator;

/**
 * Serializable comparator relying on Comparable interface.
 */
public class ComparableComparator implements Comparator, java.io.Serializable
{
   // constants
   
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -5293157416993935262L;

   /**
    * Comparator singleton instance.
    */
   public final static Comparator INSTANCE = new ComparableComparator();
   
   // constructors
   
   /**
    * Prevents construction.
    */
   protected ComparableComparator()
   {
   }
   
   // operations
   
   /**
    * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
    */
   public int compare(Object left, Object right)
   {
      return ((Comparable)left).compareTo(right);
   }
   
   /**
    * java.io serialization API method.
    * Returns a singleton reference.
    */
   private Object readResolve() throws java.io.ObjectStreamException
   {
      return INSTANCE;
   }
}
