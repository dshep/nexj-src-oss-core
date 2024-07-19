// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.sys;

import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.persistence.Query;
import nexj.core.runtime.ActionContext;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.runtime.UnitOfWork;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashTab;
import nexj.core.util.Lifecycle;
import nexj.core.util.Lookup;
import nexj.core.util.UncheckedException;

/**
 * Counter implementation.
 */
public class SysCounter implements InvocationContextAware, Lifecycle
{
   // constants

   /**
    * The attributes to read.
    */
   protected final static Pair ATTRIBUTES = Pair.list(Symbol.NAME, Symbol.VALUE,
      Symbol.define("increment"), Symbol.define("cache"));

   // attributes

   /**
    * True if this counter's "next" event is being called.
    */
   protected boolean m_bBusy;

   // associations

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The counter name to object map: Counter[String].
    */
   protected final static Lookup s_counterMap = new HashTab();

   // operations

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * Increments the named counter and returns the new value.
    * next(name)
    */
   public Long next(Metaclass metaclass, String sName, ActionContext actx)
   {
      Counter counter;

      synchronized (s_counterMap)
      {
         counter = (Counter)s_counterMap.get(sName);

         if (counter != null && !counter.isStale())
         {
            return Primitive.createLong(counter.next());
         }
      }

      UnitOfWork uowOld = m_context.beginTransaction(false);
      boolean bCommitted = false;

      try
      {
         InstanceList list = Query.createRead(metaclass, ATTRIBUTES,
            Pair.binary(Symbol.EQ, Symbol.NAME, sName),
            null, -1, 0, true, Query.SEC_NONE, m_context).read();

         if (list.isEmpty())
         {
            throw new UncheckedException("err.runtime.unknownCounter", new Object[]{sName});
         }

         long lValue;

         m_bBusy = true;

         synchronized (s_counterMap)
         {
            counter = (Counter)s_counterMap.get(sName);

            if (counter != null && !counter.isStale())
            {
               return Primitive.createLong(counter.next());
            }

            if (counter == null)
            {
               counter = new Counter();
               s_counterMap.put(sName, counter);
            }

            Instance instance = list.getInstance(0);

            counter.initialize(((Number)instance.getValue("value")).longValue(),
               ((Number)instance.getValue("increment")).longValue(),
               ((Number)instance.getValue("cache")).intValue());

            instance.setValue("value", Primitive.createLong(counter.getLimit()));
            lValue = counter.next();
         }

         m_context.commitAndResume(uowOld);
         bCommitted = true;

         return Primitive.createLong(lValue);
      }
      finally
      {
         if (!bCommitted)
         {
            m_context.rollbackAndResume(uowOld);
         }

         m_bBusy = false;
      }
   }

   /**
    * Clears the cached value of the counter iff the developer has explicitly updated the value attribute.
    * commit()
    */
   public void commit(Instance instance, ActionContext actx)
   {
      if (!m_bBusy)
      {
         s_counterMap.remove((String)instance.getValue("name"));
      }
   }

   /**
    * @see nexj.core.util.Lifecycle#shutdown()
    */
   public void shutdown()
   {
   }

   /**
    * @see nexj.core.util.Lifecycle#startup()
    */
   public void startup() throws Exception
   {
      synchronized (s_counterMap)
      {
         s_counterMap.clear();
      }
   }

   /**
    * @see nexj.core.util.Suspendable#resume()
    */
   public void resume() throws Exception
   {
      synchronized (s_counterMap)
      {
         s_counterMap.clear();
      }
   }

   /**
    * @see nexj.core.util.Suspendable#suspend()
    */
   public void suspend() throws Exception
   {
   }

   // inner classes

   /**
    * The counter state.
    */
   protected static class Counter
   {
      // attributes

      /**
       * Current value.
       */
      protected long m_lValue;

      /**
       * Upper limit.
       */
      protected long m_lLimit;

      /**
       * The value by which to increment the counter.
       */
      protected long m_lIncrement;

      // constructors

      /**
       * Constructs the counter.
       */
      public Counter()
      {
      }

      /**
       * Constructs the counter.
       * @param lValue The counter value.
       * @param lIncrement The counter increment.
       * @param nCache The number of values to cache.
       */
      public Counter(long lValue, long lIncrement, int nCache)
      {
         initialize(lValue, lIncrement, nCache);
      }

      // operations

      /**
       * Initializes the counter.
       * @param lValue The counter value.
       * @param lIncrement The counter increment.
       * @param nCache The number of values to cache.
       */
      public void initialize(long lValue, long lIncrement, int nCache)
      {
         if (lIncrement == 0)
         {
            lIncrement = 1;
         }

         if (nCache <= 0)
         {
            nCache = 1;
         }

         m_lValue = lValue;
         m_lIncrement = lIncrement;
         m_lLimit = m_lValue + lIncrement * nCache;

         if (m_lIncrement > 0 && m_lLimit < m_lIncrement ||
            m_lIncrement < 0 && m_lLimit > m_lIncrement)
         {
            throw new ArithmeticException("Counter overflow");
         }
      }

      /**
       * Increments the counter.
       * @return The new counter value.
       */
      public long next()
      {
         long lValue = m_lValue;

         m_lValue += m_lIncrement;

         return lValue;
      }

      /**
       * @return The counter limit.
       */
      public long getLimit()
      {
         return m_lLimit;
      }

      /**
       * @return True if the counter must be reinitialized.
       */
      public boolean isStale()
      {
         return m_lValue == m_lLimit;
      }
   }
}
