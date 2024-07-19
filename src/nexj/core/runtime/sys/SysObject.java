// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.sys;

import nexj.core.meta.Event;
import nexj.core.meta.Metaclass;
import nexj.core.persistence.Cursor;
import nexj.core.persistence.OID;
import nexj.core.persistence.Query;
import nexj.core.runtime.ActionContext;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.scripting.Function;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;

/**
 * Implements the system object methods.
 */
public class SysObject implements InvocationContextAware
{
   // associations

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   // operations

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * The system read event main action.
    * read(class, attributes, where, orderBy, count, offset, xlock)
    */
   public InstanceList read(Metaclass metaclass, Pair attributes, Object where, Pair orderBy,
      Number count, Number offset, Boolean xlock, ActionContext actx)
   {
      return Query.createRead(metaclass, attributes, where, orderBy,
         (count == null) ? -1 : count.intValue(),
         (offset == null) ? 0 : offset.intValue(),
         (xlock == null) ? false : xlock.booleanValue(),
         Query.SEC_DEFAULT, m_context).read();
   }

   /**
    * The system aggregate event main action.
    * aggregate(class, attributes, where, groupBy, having, orderBy, count, offset)
    */
   public InstanceList aggregate(Metaclass metaclass, Pair attributes, Object where, Pair groupBy,
      Object having, Pair orderBy, Number count, Number offset, ActionContext actx)
   {
      return Query.createAggregate(metaclass, attributes, where, groupBy, having, orderBy,
         (count == null) ? -1 : count.intValue(),
         (offset == null) ? 0 : offset.intValue(),
         Query.SEC_DEFAULT, m_context).read();
   }

   /**
    * The system openCursor event main action.
    * openCursor(class, attributes, where, orderBy, count, offset, xlock)
    */
   public Cursor openCursor(Metaclass metaclass, Pair attributes, Object where, Pair orderBy,
      Number count, Number offset, Boolean xlock, ActionContext actx)
   {
      return Query.createRead(metaclass, attributes, where, orderBy,
         (count == null) ? -1 : count.intValue(),
         (offset == null) ? 0 : offset.intValue(),
         (xlock == null) ? false : xlock.booleanValue(),
         Query.SEC_DEFAULT, m_context).openCursor();
   }

   /**
    * The system openAggregateCursor event main action.
    * openAggregateCursor(class, attributes, where, groupBy, having, orderBy, count, offset)
    */
   public Cursor openAggregateCursor(Metaclass metaclass, Pair attributes, Object where, Pair groupBy,
      Object having, Pair orderBy, Number count, Number offset, ActionContext actx)
   {
      return Query.createAggregate(metaclass, attributes, where, groupBy, having, orderBy,
         (count == null) ? -1 : count.intValue(),
         (offset == null) ? 0 : offset.intValue(),
         Query.SEC_DEFAULT, m_context).openCursor();
   }

   /**
    * The system load event aroundLoad action.
    */
   public void aroundLoad(Instance instance, Pair attributes, ActionContext actx)
   {
      try
      {
         instance.setLoading(true);

         // If the load event has been overridden, provide a simple form of the associations as well
         if (actx.getAction().getEvent().getDeclarator().getBase() != null)
         {
            for (Pair pair = attributes, prev = null; pair != null; pair = pair.getNext())
            {
               if (pair.getHead() instanceof Pair)
               {
                  Pair assoc = (Pair)pair.getHead();

                  if (Symbol.ATAT.equals(assoc.getHead()))
                  {
                     assoc = assoc.getNext();

                     if (instance.getMetaclass().getMetadata().getMetaclass
                        (((Symbol)assoc.getHead()).getName()).isUpcast(instance.getMetaclass()))
                     {
                        pair.setHead(null);
                        
                        for (assoc = assoc.getNext(); assoc != null; assoc = assoc.getNext())
                        {
                           if (pair.getHead() == null)
                           {
                              pair.setHead(assoc.getHead());
                           }
                           else
                           {
                              Pair copy = new Pair(assoc.getHead(), pair.getTail());
                              
                              pair.setTail(copy);
                              pair = copy;
                           }
                        }

                        if (pair.getHead() != null)
                        {
                           prev = pair;
                           continue;
                        }
                     }

                     if (prev == null)
                     {
                        attributes = pair.getNext();
                     }
                     else
                     {
                        prev.setTail(pair.getTail());
                     }

                     continue;
                  }

                  attributes = new Pair(assoc.getHead(), attributes);
               }

               prev = pair;
            }

            actx.setArg(0, attributes);
         }

         actx.callNext();
      }
      finally
      {
         instance.setLoading(false);
      }
   }

   /**
    * The system load event main action.
    * load(this, attributes)
    */
   public void load(Instance instance, Pair attributes, ActionContext actx)
   {
      actx.setArg(0, instance.load(attributes));
   }

   /**
    * The system new event main action.
    * new(class, values)
    */
   public Instance newInstance(Metaclass metaclass, Pair values, ActionContext actx)
   {
      Instance instance = new Instance(metaclass, m_context);

      instance.setNew();

      for (; values != null; values = values.getNext())
      {
         Pair pair = (Pair)values.getHead();
         Symbol sym = (Symbol)pair.getHead();

         if (Symbol._OID.equals(sym))
         {
            instance.setOID((OID)pair.getTail());
         }
         else
         {
            instance.setValue(sym.getName(), pair.getTail());
         }
      }

      ((Event)metaclass.getSelector("create").getMember(0)).invoke(instance, (Object[])null, m_context.getMachine());

      return instance;
   }

   /**
    * The system create event main action.
    * isNew(obj)
    */
   public boolean isNew(Instance instance, ActionContext actx)
   {
      return instance.getState() == Instance.NEW;
   }

   /**
    * The system create event main action.
    * create(obj)
    */
   public void create(Instance instance, ActionContext actx)
   {
      instance.setEventInvoked(true);
   }

   /**
    * The system update event main action.
    * update(obj)
    */
   public void update(Instance instance, ActionContext actx)
   {
      switch (instance.getState())
      {
         case Instance.NEW:
            if (!instance.isEventInvoked())
            {
               break;
            }

         case Instance.INIT:
         case Instance.CLEAN:
         case Instance.DIRTY:
            instance.setDirty();
            instance.setEventInvoked(true);
            break;
      }
   }

   /**
    * The system create event main action.
    * delete(obj)
    */
   public void delete(Instance instance, ActionContext actx)
   {
      instance.delete();
      instance.replicate();
   }

   /**
    * Invokes cascade delete logic on related objects.
    */
   public void deleteRelated(Instance instance, ActionContext actx)
   {
      instance.deleteRelated();
   }

   /**
    * The system commit event main action.
    * commit(obj)
    */
   public void commit(Instance instance, ActionContext actx)
   {
      instance.setCommitPending(false);
      instance.replicate();
   }

   /**
    * The system lock event main action.
    * lock(obj)
    */
   public void lock(Instance instance, ActionContext actx)
   {
      instance.lock();
   }

   /**
    * Old event main action.
    * old(obj, attribute)
    * @see nexj.core.runtime.Instance#getOldValue(String)
    */
   public Object getOldValue(Instance instance, Symbol sym, ActionContext actx)
   {
      return instance.getOldValue(sym.getName());
   }

   /**
    * Pre event main action.
    * pre(obj, attribute)
    * @see nexj.core.runtime.Instance#getPreValue(String)
    */
   public Object getPreValue(Instance instance, Symbol sym, ActionContext actx)
   {
      return instance.getPreValue(sym.getName());
   }

   /**
    * isDirty event main action.
    * isDirty(obj, attribute)
    * Invokes Instance.isDirty(String).
    * @see nexj.core.runtime.Instance#isDirty(String)
    */
   public boolean isDirty(Instance instance, Symbol sym, ActionContext actx)
   {
      return instance.isDirty(sym.getName());
   }

   /**
    * isUpdated event main action.
    * isUpdated(obj, attribute)
    * Invokes Instance.isUpdated(String).
    * @see nexj.core.runtime.Instance#isUpdated(String)
    */
   public boolean isUpdated(Instance instance, Symbol sym, ActionContext actx)
   {
      return instance.isUpdated(sym.getName());
   }

   /**
    * annotation event main action.
    * Invokes Instance.setAnnotation(String, Object).
    * @see nexj.core.runtime.Instance#setAnnotation(String, Object)
    */
   public void setAnnotation(Instance instance, Symbol sym, Object value, ActionContext actx)
   {
      instance.setAnnotation(sym.getName(), value);
   }

   /**
    * annotation event main action.
    * Invokes Instance.findAnnotation(String, Object).
    * @see nexj.core.runtime.Instance#findAnnotation(String)
    */
   public Object getAnnotation(Instance instance, Symbol sym, ActionContext actx)
   {
      return instance.findAnnotation(sym.getName());
   }

   /**
    * Class event main action.
    * :class()
    * @return The metaclass.
    */
   public Metaclass getClass(Metaclass metaclass, ActionContext actx)
   {
      return metaclass;
   }

   /**
    * The OID event main action.
    * :oid()
    * @return The instance OID.
    */
   public OID getOID(Instance instance, ActionContext actx)
   {
      return instance.getOID();
   }
   
   /**
    * Invokes the rules engine.
    * invoke(instance, rulesetName, attribute, attributes)
    */
   public Object invokeRulesEngine(Metaclass metaclass, Instance instance, String sName, Symbol attribute, Pair attributes, ActionContext actx)
   {
      Lookup2D objMap = (Lookup2D)metaclass.getValue("objMap");
      Instance engine = (Instance)objMap.get(instance, sName);
      boolean bCreate = (engine == null);
      
      if (bCreate)
      {
         engine = getRulesEngine(metaclass, instance, sName);
         objMap.put(instance, sName, engine);
      }

      try
      {
         Object[] args = new Object[]{attribute};
         Object obj = engine.invoke("compute", args);
         
         for (; attributes != null; attributes = attributes.getNext())
         {
            args[0] = attributes.getHead();
            engine.invoke("compute", args);
         }

         return obj;
      }
      finally
      {
         if (bCreate)
         {
            objMap.remove(instance, sName);
         }
      }
   }

   /**
    * Creates a rule set cache key.
    * @param metaclass The rules engine metaclass.
    * @param sName The rule set name.  
    */
   protected Object createRuleSetKey(Metaclass metaclass, String sName)
   {
      return new Pair(metaclass.getSymbol(), sName);
   }

   /**
    * Gets the rules engine constructor from the cache or loads and compiles the rules.
    * @param metaclass The rules engine metaclass.
    * @param instance The instance for which to construct the rules engine.
    * @param sName The rule set name.
    * @return The rules engine instance.
    */
   protected Instance getRulesEngine(Metaclass metaclass, Instance instance, String sName)
   {
      Object key = m_context.getPartitionedKey(createRuleSetKey(metaclass, sName)); 
      Function constructor = (Function)m_context.getGlobalCache().get(key);

      if (constructor == null)
      {
         boolean bSecureSaved = m_context.isSecure();

         m_context.setSecure(false);

         try
         {
            Object rules = metaclass.invoke("loadRules", new Object[]{instance.getMetaclass(), sName});

            constructor = (Function)metaclass.invoke("compile", new Object[]{instance.getMetaclass(), sName, rules});

            Lookup map = new HashTab(1);

            map.put(key, constructor);
            m_context.getGlobalCache().update(null, map, m_context.getUnitOfWork().getTime());
         }
         finally
         {
            m_context.setSecure(bSecureSaved);
         }
      }

      return (Instance)m_context.getMachine().invoke(constructor, instance, (Object[])null);
   }

   /**
    * Invalidates the cached rules for a given rule set.
    * clearCache(rulesetName)
    * @param metaclass The rules engine metaclass.
    * @param sName The rule set name.  
    * @param actx The action context.
    */
   public void clearRulesCache(Metaclass metaclass, String sName, ActionContext actx)
   {
      m_context.getUnitOfWork().uncache(createRuleSetKey(metaclass, sName));
   }
}
