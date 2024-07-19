// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import java.util.Iterator;

import nexj.core.persistence.OIDHolder;

/**
 * Interface implemented by exceptions providing an error location within an object.
 */
public interface ErrorLocationHolder
{
   /**
    * Sets the class name of the object, in which the error has occurred.
    * @param sName The class name.
    */
   void setClassName(String sName);

   /**
    * @return The class name of the object, in which the error has occurred.
    */
   String getClassName();

   /**
    * Sets the object, in which the error has occurred.
    * @param The OID holder object. Its OID can be null.
    */
   void setOIDHolder(OIDHolder holder);

   /**
    * @return The object, in which the error has occurred. Can be null.
    */
   OIDHolder getOIDHolder();

   /**
    * Sets the request argument ordinal number.
    * @param nOrdinal The request argument ordinal number.
    */
   void setOrdinal(int nOrdinal);

   /**
    * @return The request argument ordinal number, or -1 if not available.
    */
   int getOrdinal();

   /**
    * Associates an exception with a given object attribute.
    * @param sAttribute The name of an attribute, for which an exception has occurred.
    * @param t The exception.
    */
   void addException(String sAttribute, Throwable t);

   /**
    * Finds an exception for a given attribute.
    * @param sAttribute The attribute name.
    * @return The exception, or null if not found.
    */
   Throwable findException(String sAttribute);

   /**
    * @return An iterator over the attributes with associated exceptions.
    */
   Iterator getAttributeIterator();

   /**
    * @return The count of attributes with associated exceptions.
    */
   int getAttributeCount();
}
