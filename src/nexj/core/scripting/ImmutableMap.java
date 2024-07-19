// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

/**
 * Immutable decorator for Map objects. 
 */
public class ImmutableMap implements Map, Cloneable, Serializable
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = -6257715978941405078L;

   // associations

   /**
    * The Map object.
    */
   protected Map m_map;

   /**
    * Unmodifiable view of the key set.
    */
   protected Set m_keySet;

   /**
    * Unmodifiable view of the entry set.
    */
   protected Set m_entrySet;

   /**
    * Unmodifiable view of the values.
    */
   protected Collection m_values;

   // constructors

   /**
    * Constructs a new ImmutableMap wrapping the given Map.
    * @param map The Map object.
    */
   public ImmutableMap(Map map)
   {
      m_map = map;
   }

   // operations

   /**
    * @see java.util.Map#clear()
    */
   public void clear()
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see java.util.Map#containsKey(java.lang.Object)
    */
   public boolean containsKey(Object key)
   {
      return m_map.containsKey(key);
   }

   /**
    * @see java.util.Map#containsValue(java.lang.Object)
    */
   public boolean containsValue(Object value)
   {
      return m_map.containsKey(value);
   }

   /**
    * @see java.util.Map#entrySet()
    */
   public Set entrySet()
   {
      if (m_entrySet == null)
      {
         m_entrySet = Collections.unmodifiableSet(m_map.entrySet());
      }

      return m_entrySet;
   }

   /**
    * @see java.util.Map#get(java.lang.Object)
    */
   public Object get(Object key)
   {
      return m_map.get(key);
   }

   /**
    * @see java.util.Map#isEmpty()
    */
   public boolean isEmpty()
   {
      return m_map.isEmpty();
   }

   /**
    * @see java.util.Map#keySet()
    */
   public Set keySet()
   {
      if (m_keySet == null)
      {
         m_keySet = Collections.unmodifiableSet(m_map.keySet());
      }

      return m_keySet;
   }

   /**
    * @see java.util.Map#put(K, V)
    */
   public Object put(Object key, Object value)
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see java.util.Map#putAll(java.util.Map)
    */
   public void putAll(Map map)
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see java.util.Map#remove(java.lang.Object)
    */
   public Object remove(Object key)
   {
      throw new ScriptingException("err.scripting.readOnlyObject");
   }

   /**
    * @see java.util.Map#size()
    */
   public int size()
   {
      return m_map.size();
   }

   /**
    * @see java.util.Map#values()
    */
   public Collection values()
   {
      if (m_values == null)
      {
         m_values = Collections.unmodifiableCollection(m_map.values());
      }

      return m_values;
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      return new ImmutableMap(m_map);
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      return m_map.equals(obj);
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return m_map.hashCode();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return m_map.toString();
   }
}
