// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.sys;

import nexj.core.meta.Metaclass;
import nexj.core.runtime.ActionContext;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.runtime.UnitOfWork;
import nexj.core.scripting.Pair;

/**
 * Data cache wrapper.
 */
public class SysCache implements InvocationContextAware
{
   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * Gets an item from the cache.
    * @param key The item key.
    * @return The cached object, or null if not found.
    */
   public Object get(Metaclass metaclass, Object key, ActionContext actx)
   {
      return m_context.getUnitOfWork().getCached(key);
   }

   /**
    * @see nexj.core.runtime.UnitOfWork#getCachedLocal(Object)
    * @param key The item key.
    * @return The cached object from the local cache.
    */
   public Object getLocal(Metaclass metaclass, Object key, ActionContext actx)
   {
      return m_context.getUnitOfWork().getCachedLocal(key);
   }

   /**
    * @see nexj.core.runtime.UnitOfWork#getCachedTransient(Object)
    * @param key The item key.
    * @return The cached object from the transient cache.
    */
   public Object getTransient(Metaclass metaclass, Object key, ActionContext actx)
   {
      return m_context.getUnitOfWork().getCachedTransient(key);
   }

   /**
    * Puts a reference to an immutable object into the cache.
    * @param key The item key.
    * @param ref The object reference.
    */
   public void putReference(Metaclass metaclass, Object key, Object ref, ActionContext actx)
   {
      m_context.getUnitOfWork().cacheReference(key, ref);
   }
   
   /**
    * Puts a serialized copy of an object into the cache.
    * @param key The item key.
    * @param obj The serializable object.
    */
   public void putCopy(Metaclass metaclass, Object key, Object obj, ActionContext actx)
   {
      m_context.getUnitOfWork().cacheCopy(key, obj);
   }
   
   /**
    * Puts an instance or an instance collection into the cache.
    * @param key The item key.
    * @param instance The instance or the collection.
    * @param attributes The list of attributes to cache.
    */
   public void putInstance(Metaclass metaclass, Object key, Object instance, Pair attributes, ActionContext actx)
   {
      m_context.getUnitOfWork().cacheInstance(key, instance, attributes);
   }

   /**
    * @see nexj.core.runtime.UnitOfWork#cacheLocal(Object, Object)
    * @param key The item key.
    * @param obj The object to cache in the local UOW cache.
    */
   public void putLocal(Metaclass metaclass, Object key, Object obj, ActionContext actx)
   {
      m_context.getUnitOfWork().cacheLocal(key, obj);
   }

   /**
    * @see nexj.core.runtime.UnitOfWork#cacheTemporary(Object, Object, byte)
    * @param key The item key.
    * @param obj The object to cache temporarily without commit.
    */
   public void putTemporary(Metaclass metaclass, Object key, Object obj, ActionContext actx)
   {
      m_context.getUnitOfWork().cacheTemporary(key, obj, UnitOfWork.CACHE_DEFAULT);
   }

   /**
    * @see nexj.core.runtime.UnitOfWork#cacheTransient(Object, Object)
    * @param key The item key.
    * @param obj The object to cache in the transient cache.
    */
   public void putTransient(Metaclass metaclass, Object key, Object obj, ActionContext actx)
   {
      m_context.getUnitOfWork().cacheTransient(key, obj);
   }

   /**
    * Removes an item from the cache.
    * @param key The item key.
    */
   public void remove(Metaclass metaclass, Object key, ActionContext actx)
   {
      m_context.getUnitOfWork().uncache(key);
   }

   /**
    * @see nexj.core.runtime.UnitOfWork#uncacheLocal(Object)
    * @param key The item key.
    */
   public void removeLocal(Metaclass metaclass, Object key, ActionContext actx)
   {
      m_context.getUnitOfWork().uncacheLocal(key);
   }

   /**
    * @see nexj.core.runtime.UnitOfWork#uncacheTransient(Object)
    * @param key The item key.
    */
   public void removeTransient(Metaclass metaclass, Object key, ActionContext actx)
   {
      m_context.getUnitOfWork().uncacheTransient(key);
   }
}