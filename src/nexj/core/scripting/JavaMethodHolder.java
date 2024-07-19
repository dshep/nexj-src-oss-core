// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

public interface JavaMethodHolder
{
   /**
    * Adds a Java method to the map.
    * @param clazz The Java class object.
    * @param symbol The method symbol.
    * @param fun The method implementation function.
    */
   public void addJavaMethod(Class clazz, Symbol symbol, Function fun);

   /**
    * Gets a java method wrapper for a given class and method symbol.
    * @param clazz The java class.
    * @param symbol The method symbol.
    * @return The java method wrapper.
    * @throws ScriptingException if the method was not found.
    */
   public Function getJavaMethod(Class clazz, Symbol symbol);
}
