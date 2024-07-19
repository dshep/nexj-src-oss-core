// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.util.Arrays;
import java.util.EventListener;
import java.util.Iterator;
import java.util.Set;

import nexj.core.meta.Primitive;
import nexj.core.scripting.object.ClassEnvironment;
import nexj.core.scripting.object.ClassObject;
import nexj.core.util.HashDeque;
import nexj.core.util.HashTab;
import nexj.core.util.HashTab2D;
import nexj.core.util.IdentityHashHolder;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.Null;

/**
 * A global scripting environment. Holds an interned symbol map and a
 * variable map.
 */
public final class GlobalEnvironment implements JavaMethodHolder, ClassEnvironment
{
   // constants

   /**
    * Flag to convert the symbols to lower case.
    */
   public final static int OPTION_CONVERT_SYMBOLS = 0x0001;

   /**
    * Flag to allow intrinsics redefinition.
    */
   public final static int OPTION_REDEFINE_INTRINSICS = 0x0002;

   /**
    * The class logger.
    */
   public final static Logger LOGGER = Logger.getLogger(GlobalEnvironment.class);

   /**
    * Not found value.
    */
   private final static Object NULL_VALUE = Null.VALUE;

   /**
    * The Java metaclass object.
    */
   private final static Class JAVA_METACLASS = Class.class;

   // attributes

   /**
    * Count of occupied entries in the function table,
    * including the load factor (8*nFunctionCount - 1). 
    */
   private int m_nFunctionTableLevel = -1;

   /**
    * The option flags.
    */
   private int m_nOptions;

   /**
    * True is the environment is read-only.
    */
   private boolean m_bReadOnly;

   /**
    * True if deferred resolution mode is enabled.
    */
   private boolean m_bDeferred;

   // associations

   /**
    * The map of a symbol to a variable value: Object[Symbol].
    */
   private Lookup m_variableMap = new HashTab(16);

   /**
    * Map for storing class state etc: Object[Object].
    */
   private final Lookup m_stateMap = new HashTab();

   /**
    * Map of class objects and method symbols to wrapper functions: Function[Class][Symbol].
    */
   private final Lookup2D m_javaMethodMap = new HashTab2D(16);

   /**
    * The function hash table: [ClassObject, Symbol, argCount, Function].
    */
   private Object[] m_functionTable = new Object[16];

   /**
    * The parent global environment. If a variable is not found in the current
    * environment, then the parent environment is checked.
    */
   private GlobalEnvironment m_parent;

   /**
    * Set of class objects to update.
    */
   private Set m_classSet;

   /**
    * Optional text position map for the currently executed code: TextPosition[Object].
    * It is used for tracking text positions in transformed code that is compiled e.g. through (eval ...).
    */
   /**
    * The text position map.
    */
   private Lookup m_textPosMap;

   // constructors
   
   /**
    * Constructs a shared global environment.
    */
   public GlobalEnvironment()
   {
      this(null);
   }
   
   /**
    * Constructs a context-specific global environment.
    * @param parent The parent environment.
    */
   public GlobalEnvironment(GlobalEnvironment parent)
   {
      m_parent = parent;
      
      if (parent != null)
      {
         m_nOptions = parent.m_nOptions;
      }
      else
      {
         // Add the intrinsic function symbols

         for (Iterator itr = Intrinsic.getFunctionIterator(); itr.hasNext();)
         {
            IntrinsicFunction fun = (IntrinsicFunction)itr.next();

            defineVariable(fun.getSymbol(), fun);
         }

         // Add the primitive type symbols

         for (int i = 0; i < Primitive.MAX_COUNT; ++i)
         {
            Primitive type = Primitive.get(i);

            defineVariable("sys:" + type.getName(), type);
         }
      }
   }

   // operations

   /**
    * @return The read-only flag.
    */
   public boolean isReadOnly()
   {
      return m_bReadOnly;
   }

   /**
    * Adds an environment listener.
    * @param listener The listener to add.
    */
   public void addListener(Listener listener)
   {
      if (m_bReadOnly)
      {
         throw new ReadOnlyException();
      }

      if (!(m_variableMap instanceof NotificationLookup))
      {
         m_variableMap = new NotificationLookup(m_variableMap);
      }

      ((NotificationLookup)m_variableMap).addListener(listener);
   }

   /**
    * Removes an environment listener.
    * @param listener The listener to remove.
    */
   public void removeListener(Listener listener)
   {
      if (m_bReadOnly)
      {
         throw new ReadOnlyException();
      }

      if (m_variableMap instanceof NotificationLookup)
      {
         NotificationLookup map = (NotificationLookup)m_variableMap;

         map.removeListener(listener);

         if (map.getListenerCount() == 0)
         {
            m_variableMap = map.getLookup();
         }
      }
   }

   /**
    * Sets the scope symbol.
    * @param The scope symbol.
    */
   public void setScope(Symbol scope)
   {
      if (m_bReadOnly)
      {
         throw new ReadOnlyException();
      }

      if (m_variableMap instanceof NotificationLookup)
      {
         ((NotificationLookup)m_variableMap).notifyScope(scope);
      }
   }

   /**
    * Sets the option flag set.
    * @param nOptions The option flag set to set.
    */
   public void setOptions(int nOptions)
   {
      if (m_bReadOnly)
      {
         throw new ReadOnlyException();
      }

      m_nOptions = nOptions;
   }

   /**
    * @return The option flag set.
    */
   public int getOptions()
   {
      return m_nOptions;
   }

   /**
    * Sets an option value.
    * @param nOption The option to set, one of the OPTION_* constants.
    * @param bValue The option value.
    */
   public void setOption(int nOption, boolean bValue)
   {
      if (bValue)
      {
         m_nOptions |= nOption;
      }
      else
      {
         m_nOptions &= ~nOption;
      }
   }
   
   /**
    * Returns an option value.
    * @param nOption The option, which value to return.
    * @return The option value.
    */
   public boolean isOptionSet(int nOption)
   {
      return (m_nOptions & nOption) != 0;
   }

   /**
    * Defines a variable.
    * @param sym The variable symbol.
    * @param value The variable value.
    */
   public void defineVariable(Symbol sym, Object value)
   {
      m_variableMap.put(sym, (value == null) ? NULL_VALUE : value);
   }
   
   /**
    * Defines a variable.
    * @param sName The variable symbol name.
    * @param value The variable value.
    */
   public void defineVariable(String sName, Object value)
   {
      defineVariable(Symbol.define(sName), value);
   }

   /**
    * Sets the value of a variable.
    * @param sym The variable symbol.
    * @param value The variable value to set.
    */
   public void setVariable(Symbol sym, Object value)
   {
      if (m_variableMap.put(sym, (value == null) ? NULL_VALUE : value) == null)
      {
         for (GlobalEnvironment parent = m_parent; parent != null; parent = parent.m_parent)
         {
            if (parent.m_variableMap.get(sym) != null)
            {
               return;
            }
         }

         m_variableMap.remove(sym);

         throw new ScriptingException("err.scripting.undefVar", new Object[]{sym.getName()});
      }
   }

   /**
    * Gets the value of a variable.
    * @param sym The variable symbol.
    * @return The variable value.
    */
   public Object getVariable(Symbol sym)
   {
      Object value = m_variableMap.get(sym);
      
      if (value != null)
      {
         return (value == NULL_VALUE) ? null : value;
      }

      if (m_parent != null)
      {
         return m_parent.getVariable(sym);
      }

      throw new ScriptingException("err.scripting.undefVar", new Object[]{sym.getName()});
   }

   /**
    * Determines if getVariable() will succeed.
    * NOTE: Use findVariable() is you need the value to avoid a second lookup.
    * @param sym The variable symbol.
    * @return True if the variable is defined.
    */
   public boolean isDefined(Symbol sym)
   {
      return m_variableMap.contains(sym) || m_parent != null && m_parent.isDefined(sym);
   }

   /**
    * Finds the value of a variable.
    * @param sym The variable symbol.
    * @return The variable value, or null if not found.
    */
   public Object findVariable(Symbol sym)
   {
      Object value = m_variableMap.get(sym);
      
      if (value != null)
      {
         return (value == NULL_VALUE) ? null : value;
      }

      if (m_parent != null)
      {
         return m_parent.findVariable(sym);
      }

      return null;
   }

   /**
    * Finds the value of a variable.
    * @param sym The variable symbol.
    * @param defaultValue The value if not found.
    * @return The variable value, or default if not found.
    */
   public Object findVariable(Symbol sym, Object defaultValue)
   {
      Object value = m_variableMap.get(sym);
      
      if (value != null)
      {
         return (value == NULL_VALUE) ? null : value;
      }

      if (m_parent != null)
      {
         return m_parent.findVariable(sym, defaultValue);
      }

      return defaultValue;
   }

   /**
    * Removes a variable from the current global environment only (not from the parent).
    * @param sym The variable symbol.
    */
   public void removeVariable(Symbol sym)
   {
      m_variableMap.remove(sym);
   }

   /**
    * @return The variable iterator.
    */
   public Lookup.Iterator getVariableIterator()
   {
      return m_variableMap.iterator();
   }

   /**
    * Creates method wrappers for the given class.
    * @param clazz The Java class for which to create the wrappers.
    */
   public void importJavaClass(Class clazz)
   {
      if (m_bReadOnly)
      {
         throw new ReadOnlyException();
      }

      if (LOGGER.isDebugEnabled())
      {
         LOGGER.debug("Importing class " + clazz.getName());
      }

      JavaConstructor.importJavaConstructors(clazz, this);
      JavaMethod.importJavaMethods(clazz, this);
      defineVariable(clazz.getName(), clazz);
   }

   /**
    * Finds a java method wrapper using a class object and a method symbol,
    * searching first in the parent environment.
    * @param clazz The class object.
    * @param symbol The method symbol.
    * @return The java method wrapper, or null if not found.
    */
   protected Function findJavaMethod(Class clazz, Symbol symbol)
   {
      if (m_parent != null)
      {
         Function meth = m_parent.findJavaMethod(clazz, symbol);
         
         if (meth != null)
         {
            return meth;
         }
      }

      return (Function)m_javaMethodMap.get(clazz, symbol);
   }

   /**
    * Adds a Java method to the map.
    * @param clazz The Java class object.
    * @param symbol The method symbol.
    * @param fun The method implementation function.
    */
   public void addJavaMethod(Class clazz, Symbol symbol, Function fun)
   {
      m_javaMethodMap.put(clazz, symbol, fun);
   }

   /**
    * Gets a java method wrapper for a given class and method symbol.
    * @param clazz The java class.
    * @param symbol The method symbol.
    * @return The java method wrapper.
    * @throws ScriptingException if the method was not found.
    */
   public Function getJavaMethod(Class clazz, Symbol symbol)
   {
      Function meth = findJavaMethod(clazz, symbol);

      if (meth != null)
      {
         return meth;
      }

      if (findVariable(Symbol.define(clazz.getName())) != clazz)
      {
         importJavaClass(clazz);
         meth = findJavaMethod(clazz, symbol);

         if (meth != null)
         {
            return meth;
         }
      }

      meth = findJavaMethod(JAVA_METACLASS, symbol);

      if (meth != null)
      {
         return meth;
      }

      throw new ScriptingException("err.scripting.unknownMethod",
         new Object[] {symbol, clazz.getName()});
   }

   /**
    * Sets a state value.
    * @param key The state key. Cannot be null.
    * @param value The state value.
    */
   public void setState(Object key, Object value)
   {
      m_stateMap.put(key, value);
   }

   /**
    * Gets a state value.
    * @param key The state key. Cannot be null.
    * @return The state value, or null if not found.
    */
   public Object getState(Object key)
   {
      return m_stateMap.get(key);
   }

   /**
    * Clears the state. 
    */
   public void clearState()
   {
      if (m_bReadOnly)
      {
         throw new ReadOnlyException();
      }

      m_stateMap.clear();
   }

   /**
    * @see nexj.core.scripting.object.ClassEnvironment#setDeferred(boolean)
    */
   public void setDeferred(boolean bDeferred)
   {
      if (m_bReadOnly)
      {
         throw new ReadOnlyException();
      }

      m_bDeferred = bDeferred;
   }

   /**
    * @see nexj.core.scripting.object.ClassEnvironment#isDeferred()
    */
   public boolean isDeferred()
   {
      return m_bDeferred;
   }

   /**
    * @see nexj.core.scripting.object.ClassEnvironment#defineClass(nexj.core.scripting.object.ClassObject)
    */
   public void defineClass(ClassObject classObject)
   {
      defineVariable(classObject.getSymbol(), classObject);
   }

   /**
    * @see nexj.core.scripting.object.ClassEnvironment#findClass(nexj.core.scripting.Symbol)
    */
   public ClassObject findClass(Symbol symbol)
   {
      Object obj = findVariable(symbol);

      if (obj instanceof ClassObject)
      {
         return (ClassObject)obj;
      }

      return null;
   }

   /**
    * Computes the function hash table code.
    */
   protected static int hashFunctionKey(Object classObject, Object symbol, int nCount)
   {
      return classObject.hashCode() ^ symbol.hashCode() ^ nCount;
   }

   /**
    * Rehashes the function table.
    * @param nSize2 The new table size. 
    */
   protected void rehashFunctionTable(int nSize2)
   {
      int nMask2 = nSize2 - 1;
      Object[] table2 = new Object[nSize2];
      int nSize = m_functionTable.length;

      for (int k = 0; k < nSize; k += 4)
      {
         Object classObject = m_functionTable[k];

         if (classObject != null)
         {
            Object symbol = m_functionTable[k + 1];
            Integer count = (Integer)m_functionTable[k + 2];
            int i = (hashFunctionKey(classObject, symbol, count.intValue()) << 2) & nMask2;

            for (;;)
            {
               if (table2[i] == null)
               {
                  table2[i] = classObject;
                  table2[i + 1] = symbol;
                  table2[i + 2] = count;
                  table2[i + 3] = m_functionTable[k + 3];

                  break;
               }

               i = (i + 4) & nMask2;
            }
         }
      }

      m_functionTable = table2;
   }
   
   /**
    * @see nexj.core.scripting.object.ClassEnvironment#addFunction(nexj.core.scripting.object.ClassObject, nexj.core.scripting.Symbol, int, nexj.core.scripting.Function)
    * NOTE: Only adding new values is supported. Overwriting existing keys is not.
    */
   public void addFunction(ClassObject classObject, Symbol symbol, int nArgCount, Function function)
   {
      int nMask = m_functionTable.length - 1;
      int i = (hashFunctionKey(classObject, symbol, nArgCount) << 2) & nMask;

      for (;;)
      {
         Object classObject2 = m_functionTable[i];

         if (classObject2 == null)
         {
            m_functionTable[i] = classObject;
            m_functionTable[i + 1] = symbol;
            m_functionTable[i + 2] = Primitive.createInteger(nArgCount);
            m_functionTable[i + 3] = function;

            if ((m_nFunctionTableLevel += 8) > nMask)
            {
               rehashFunctionTable((nMask + 1) << 1);
            }

            return;
         }

         i = (i + 4) & nMask;
      } 
   }

   /**
    * @see nexj.core.scripting.object.ClassEnvironment#findFunction(nexj.core.scripting.object.ClassObject, nexj.core.scripting.Symbol, int)
    */
   public Function findFunction(ClassObject classObject, Symbol symbol, int nArgCount)
   {
      int nMask = m_functionTable.length - 1;
      int i = (hashFunctionKey(classObject, symbol, nArgCount) << 2) & nMask;

      for (;;)
      {
         Object classObject2 = m_functionTable[i];

         if (classObject2 == null)
         {
            return null;
         }

         if (classObject2 == classObject &&
            symbol.equals(m_functionTable[i + 1]) &&
            ((Integer)m_functionTable[i + 2]).intValue() == nArgCount)
         {
            return (Function)m_functionTable[i + 3];
         }

         i = (i + 4) & nMask;
      } 
   }

   /**
    * Clears the function table.
    */
   protected void clearFunctions()
   {
      Arrays.fill(m_functionTable, null);
   }

   /**
    * @see nexj.core.scripting.object.ClassEnvironment#change(ClassObject)
    */
   public void change(ClassObject classObject) throws ReadOnlyException
   {
      if (m_bReadOnly)
      {
         throw new ReadOnlyException();
      }

      if (m_parent != null &&
         m_parent.isReadOnly() &&
         classObject.getSymbol() != null &&
         m_parent.findClass(classObject.getSymbol()) == classObject)
      {
         throw new ReadOnlyException("err.scripting.readOnlyClass",
            new Object[]{classObject.getName()});
      }

      if (m_classSet == null)
      {
         m_classSet = new HashDeque();
      }

      m_classSet.add(classObject);
   }

   /**
    * @see nexj.core.scripting.object.ClassEnvironment#complete()
    */
   public boolean complete()
   {
      if (m_bReadOnly)
      {
         throw new ReadOnlyException();
      }

      if (m_bDeferred)
      {
         return false;
      }

      try
      {
         m_bDeferred = true;

         if (m_classSet != null)
         {
            Set identitySet = new IdentityHashHolder();
   
            for (Iterator itr = m_classSet.iterator(); itr.hasNext();)
            {
               ClassObject clazz = (ClassObject)itr.next();
   
               clazz.complete(identitySet);
               m_stateMap.remove(clazz);
            }
   
            m_classSet = null;
            clearFunctions();
         }
      }
      finally
      {
         m_bDeferred = false;
      }

      return true;
   }

   /**
    * Sets the text position map.
    * @param textPosMap The text position map to set.
    */
   public void setTextPositionMap(Lookup textPosMap)
   {
      m_textPosMap = textPosMap;
   }

   /**
    * @return The text position map.
    */
   public Lookup getTextPositionMap()
   {
      return m_textPosMap;
   }

   /**
    * Makes the object read-only.
    */
   public void makeReadOnly()
   {
      m_bReadOnly = true;
   }
   
   // inner classes

   /**
    * Environment event listener.
    */
   public interface Listener extends EventListener
   {
      /**
       * Invoked before the scope is set.
       * @param env The global environment.
       * @param scope The scope to set.
       */
      void setScope(GlobalEnvironment env, Symbol scope);

      /**
       * Invoked before the variable is set.
       * @param env The global environment.
       * @param name The variable name.
       * @param value The variable value.
       */
      void setVariable(GlobalEnvironment env, Symbol name, Object value);
   }

   /**
    * Notification wrapper for lookups.
    */
   private class NotificationLookup implements Lookup
   {
      // attributes

      /**
       * The listener count.
       */
      private int m_nListenerCount;

      // associations

      /**
       * The wrapped lookup.
       */
      private Lookup m_lookup;

      /**
       * The listener array.
       */
      private Listener[] m_listenerArray = new Listener[4];
      
      // constructors

      /**
       * Constructs the lookup.
       * @param lookup The wrapped lookup.
       */
      public NotificationLookup(Lookup lookup)
      {
         m_lookup = lookup;
      }

      // operations

      /**
       * @return The wrapped object.
       */
      public Lookup getLookup()
      {
         return m_lookup;
      }

      /**
       * Adds a listener.
       * @param listener The listener to add.
       */
      public void addListener(Listener listener)
      {
         assert listener != null;

         if (m_nListenerCount == m_listenerArray.length)
         {
            Listener[] listenerArray = new Listener[m_nListenerCount << 1];

            System.arraycopy(m_listenerArray, 0, listenerArray, 0, m_nListenerCount);
            m_listenerArray = listenerArray;
         }

         m_listenerArray[m_nListenerCount++] = listener;
      }

      /**
       * Removes a listener.
       * @param listener The listener to remove.
       */
      public void removeListener(Listener listener)
      {
         assert listener != null;

         for (int i = m_nListenerCount - 1; i >= 0; --i)
         {
            if (m_listenerArray[i] == listener)
            {
               System.arraycopy(m_listenerArray, i + 1, m_listenerArray, i, m_nListenerCount - i - 1);
               m_listenerArray[--m_nListenerCount] = null;

               break;
            }
         }
      }

      /**
       * @return The listener count.
       */
      public int getListenerCount()
      {
         return m_nListenerCount;
      }

      /**
       * Notifies the listener about scope change.
       * @param scope The new scope.
       */
      public void notifyScope(Symbol scope)
      {
         for (int i = 0; i < m_nListenerCount; ++i)
         {
            m_listenerArray[i].setScope(GlobalEnvironment.this, scope);
         }
      }

      /**
       * @see nexj.core.util.Lookup#put(java.lang.Object, java.lang.Object)
       */
      public Object put(Object key, Object value)
      {
         for (int i = 0; i < m_nListenerCount; ++i)
         {
            m_listenerArray[i].setVariable(GlobalEnvironment.this,
               (Symbol)key, (value == NULL_VALUE) ? null : value);
         }

         return m_lookup.put(key, value);
      }

      /**
       * @see nexj.core.util.Lookup#contains(java.lang.Object)
       */
      public boolean contains(Object key)
      {
         return m_lookup.contains(key);
      }

      /**
       * @see nexj.core.util.Lookup#get(java.lang.Object)
       */
      public Object get(Object key)
      {
         return m_lookup.get(key);
      }

      /**
       * @see nexj.core.util.Lookup#remove(java.lang.Object)
       */
      public Object remove(Object key)
      {
         return m_lookup.remove(key);
      }

      /**
       * @see nexj.core.util.Lookup#size()
       */
      public int size()
      {
         return m_lookup.size();
      }

      /**
       * @see nexj.core.util.Lookup#iterator()
       */
      public nexj.core.util.Lookup.Iterator iterator()
      {
         return m_lookup.iterator();
      }

      /**
       * @see nexj.core.util.Lookup#valueIterator()
       */
      public nexj.core.util.Lookup.Iterator valueIterator()
      {
         return m_lookup.valueIterator();
      }

      /**
       * @see nexj.core.util.Lookup#clear()
       */
      public void clear()
      {
         m_lookup.clear();
      }

      /**
       * @see nexj.core.util.Lookup#clone()
       */
      public Object clone()
      {
         return m_lookup.clone();
      }
   }
}
