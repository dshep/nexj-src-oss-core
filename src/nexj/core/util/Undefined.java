// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Represents an undefined value.
 */
public class Undefined implements java.io.Serializable
{
   // constants

   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 6817558543281891451L;

   /**
    * The global undefined value.
    */
   public final static Undefined VALUE = new Undefined();

   // constructors
   
   /**
    * Prevents construction.
    */
   protected Undefined()
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
      return 314;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
     return "UNDEFINED";
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
