// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Iterator;

/**
 * Interface implemented by objects containing exceptions,
 * e.g. composite exceptions.
 */
public interface ExceptionHolder
{
   /**
    * Adds a new contained exception to the holder.
    * @param e The exception to add.
    */
   void addException(Throwable e);
   
   /**
    * @return The contained exception count.
    */
   int getExceptionCount();
   
   /**
    * @return An iterator over the contained exceptions.
    */
   Iterator getExceptionIterator();
}
