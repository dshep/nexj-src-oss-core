// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.jni;

/**
 * Base class for objects implemented in C++.
 */
public abstract class JNIObject
{
   // attributes
   
   /**
    * Pointer to the C++ object, managed by the C++ class.
    */
   private transient long m_handle;
   
   // operations
   
   /**
    * Releases the object.
    */
   public abstract void dispose();

   /**
    * @see java.lang.Object#finalize()
    */
   protected void finalize() throws Throwable
   {
      if (m_handle != 0)
      {
         dispose();
      }
      
      super.finalize();
   }
}
