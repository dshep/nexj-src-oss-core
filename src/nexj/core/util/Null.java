// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Represents a null value.
 * Used instead of null to avoid conditional logic.
 */
public class Null implements java.io.Serializable
{
   // constants

   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = -5255195852393780754L;

   /**
    * The global undefined value.
    */
   public final static Null VALUE = new Null();

   // constructors
   
   /**
    * Prevents construction.
    */
   protected Null()
   {
   }
   
   // operations

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      return obj == VALUE;
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return 0;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
     return "()";
   }

   /**
    * java.io serialization API method.
    * Returns a singleton reference.
    */
   private Object readResolve() throws java.io.ObjectStreamException
   {
      return VALUE;
   }
}
