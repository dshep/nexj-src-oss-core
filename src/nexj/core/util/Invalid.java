// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;


/**
 * Represents an invalid value. 
 */
public class Invalid extends Undefined
{
   // constants

   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = -8280150270478866016L;

   /**
    * The global invalid value.
    */
   public final static Invalid VALUE = new Invalid();

   // constructors
   
   /**
    * Prevents construction.
    */
   protected Invalid()
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
      return 317;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
     return "INVALID";
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
