// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.scripting.Pair;

/**
 * An interface to access a generic object.
 */
public interface Accessor
{
   // operations

   /**
    * @return The object class.
    */
   Metaclass getMetaclass();
   
   /**
    * @return The tentative object class (i.e. before lazy-loading,
    *    which is not triggered by this method).
    */
   Metaclass getLazyMetaclass();

   /**
    * @return True if the object has a deferred loading.
    */
   boolean isLazy();
   
   /**
    * Sets an object attribute value by ordinal number.
    * @param nOrdinal The attribute ordinal number.
    * @param value The value to set.
    */
   void setValue(int nOrdinal, Object value);

   /**
    * Gets an object attribute value by ordinal number.
    * @param nOrdinal The attribute ordinal number.
    * @return The attribute value.
    */
   Object getValue(int nOrdinal);

   /**
    * Sets an object attribute value by name.
    * @param sName The name of the attribute.
    * @param value The attribute value.
    */
   void setValue(String sName, Object value);

   /**
    * Gets an object attribute value by name.
    * @param sName The attribute name.
    * @return The attribute value.
    */
   Object getValue(String sName);

   /**
    * Gets an object attribute value by name.
    * Falls back to another name if the first one is not found.
    * This method is for preserving metadata compatibility.
    * @param sName The attribute name.
    * @param sFallbackName The fall back attribute name.
    * Can be null to return null if sName does not exist.
    * @return The attribute value.
    */
   Object getValue(String sName, String sFallbackName);

   /**
    * Sets a value without causing any side effects
    * like event triggering or dependency checks.
    * @param nOrdinal The attribute ordinal.
    * @param value The value to set.
    */
   void setValueDirect(int nOrdinal, Object value);

   /**
    * Gets a value without causing any side effects
    * like automatic data retrieval.
    * @param nOrdinal The attribute ordinal.
    * @return The value.
    */
   Object getValueDirect(int nOrdinal);

   /**
    * Invalidates a given attribute value.
    * @param nOrdinal The attribute ordinal.
    * @param invalid The invalid value to assign.
    */
   void invalidate(int nOrdinal, Object invalid);
   
   /**
    * Invokes a method on the object.
    * @param sName The method name.
    * @param argArray The method arguments.
    * @return The method return value.
    */
   Object invoke(String sName, Object[] argArray);
   
   /**
    * Invokes a method on the object.
    * @param sName The method name.
    * @param args The method arguments.
    * @return The method return value.
    */
   Object invoke(String sName, Pair args);
   
   /**
    * Invokes an method on the object.
    * @param sName The method name.
    * @return The method return value.
    */
   Object invoke(String sName);
}
