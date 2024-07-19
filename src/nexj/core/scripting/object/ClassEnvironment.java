// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.scripting.Function;
import nexj.core.scripting.ReadOnlyException;
import nexj.core.scripting.Symbol;

/**
 * Class resolution cache.
 */
public interface ClassEnvironment
{
   /**
    * Sets the deferred resolution mode (saves CPU).
    * @param bDeferred True to enable deferred resolution.
    */
   void setDeferred(boolean bDeferred);

   /**
    * @return The deferred resolution mode flag.
    */
   boolean isDeferred();
   
   /**
    * Prepares the environment for a change of a class object.
    * @param classObject The class object.
    * @throws ReadOnlyException if the environment is read-only.
    */
   void change(ClassObject classObject) throws ReadOnlyException;

   /**
    * Completes the environment change.
    * @return True if completed, false if deferred.
    */
   boolean complete();

   /**
    * Finds a class object by symbol.
    * @param symbol The class symbol.
    * @return The class object, or null if not found.
    */
   ClassObject findClass(Symbol symbol);

   /**
    * Defines a class object.
    * @param classObject The class object.
    */
   void defineClass(ClassObject classObject);

   /**
    * Finds a cached function given a class object, member symbol and an argument count.
    * @param classObject The class object.
    * @param symbol The member symbol.
    * @param nArgCount The argument count.
    * @return The function, or null if not found.
    */
   Function findFunction(ClassObject classObject, Symbol symbol, int nArgCount);

   /**
    * Adds a function to the cache.
    * @param classObject The class object.
    * @param symbol The member symbol.
    * @param nArgCount The argument count.
    * @param function The function to add.
    */
   void addFunction(ClassObject classObject, Symbol symbol, int nArgCount, Function function);


   /**
    * Sets a state value.
    * @param key The state key. Cannot be null.
    * @param value The state value.
    */
   void setState(Object key, Object value);

   /**
    * Gets a state value.
    * @param key The state key. Cannot be null.
    * @return The state value, or null if not found.
    */
   Object getState(Object key);
}
