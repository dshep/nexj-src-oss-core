// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.sys;

import java.sql.Timestamp;
import java.util.List;

import nexj.core.meta.Accessor;
import nexj.core.meta.Argument;
import nexj.core.meta.Attribute;
import nexj.core.meta.Event;
import nexj.core.meta.Metaclass;
import nexj.core.persistence.InvalidQueryException;
import nexj.core.runtime.ActionContext;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceArrayList;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.SoftHashTab;
import nexj.core.util.Undefined;

/**
 * Read assembler implementation.
 */
public class SysReader implements InvocationContextAware
{
   // constants

   /**
    * The attributes to load automatically.
    */
   private final static Object[] LOAD_ATTRIBUTES = new Object[]
   {
      Symbol.define("event"),
      Symbol.define("key"),
      Symbol.define("expiration"),
      Symbol.define("dispatchParent"),
      Symbol.define("dispatchAttribute"),
      Symbol.define("dispatchValues"),
      Symbol.define("dispatchGroup"),
      Symbol.define("dispatched"),
      Symbol.define("offset"),
      Symbol.define("count"),
      Symbol.define("xlock"),
      Symbol.define("bookmark"),
      Symbol.define("next"),
      Symbol.define("inclusive"),
      Symbol.define("parents"),
      Symbol.define("results"),
      Symbol.define("where"),
      Symbol.define("orderBy")
   };

   // associations

   /**
    * The invocation context.
    */
   private InvocationContext m_context;

   // operations

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * Collects the instances specified by a given association path in a list.
    * @param assoc The association path.
    * @param instance The instance relative to which the association is.
    * @param results The output list.
    */
   protected static void collectAssociatedInstances(Pair assoc, Instance instance, InstanceList results)
   {
      if (assoc == null)
      {
         results.add(instance, InstanceList.REPLACE | InstanceList.DIRECT);
      }
      else
      {
         Object value = instance.getValue(((Symbol)assoc.getHead()).getName());

         if (value != null)
         {
            if (value instanceof InstanceList)
            {
               assoc = assoc.getNext();

               InstanceList list = (InstanceList)value;

               for (int i = 0, nCount = list.size(); i < nCount; ++i)
               {
                  collectAssociatedInstances(assoc, list.getInstance(i), results);
               }
            }
            else if (value instanceof Instance)
            {
               collectAssociatedInstances(assoc.getNext(), (Instance)value, results);
            }
            else
            {
               throw new InvalidQueryException("err.persistence.primitiveAssocQuery",
                  new Object[]{assoc.getHead(), instance.getMetaclass().getName()});
            }
         }
      }
   }

   /**
    * Checks if the dispatch conditions are satisfied.
    * @param reader The reader object.
    * @return True if the query should be executed.
    */
   protected boolean isEnabled(Accessor reader)
   {
      Accessor dispGroup = (Accessor)reader.getValue("dispatchGroup");

      if (dispGroup != null)
      {
         boolean bDispatched = ((Boolean)dispGroup.getValue("dispatched")).booleanValue();

         if (dispGroup == reader)
         {
            if (bDispatched)
            {
               return false;
            }
         }
         else if (((Boolean)reader.getValue("dispatched")).booleanValue() != bDispatched)
         {
            return false;
         }
      }

      Accessor dispParent = (Accessor)reader.getValue("dispatchParent");

      if (dispParent == null)
      {
         if (dispGroup != null && reader != dispGroup)
         {
            reader.setValue("dispatched", Boolean.TRUE);
         }

         return true;
      }

      InstanceList resultList = (InstanceList)dispParent.getValue("results");

      if (resultList == null || resultList.isEmpty())
      {
         return false;
      }

      dispParent = resultList.getInstance(0);

      Pair dispAssoc = (Pair)reader.getValue("dispatchAttribute");
      Pair dispValues = (Pair)reader.getValue("dispatchValues");

      if (dispAssoc == null)
      {
         Metaclass metaclass = dispParent.getMetaclass();

         for (; dispValues != null; dispValues = dispValues.getNext())
         {
            if (m_context.getMetadata().getMetaclass(((Symbol)dispValues.getHead()).getName()).isUpcast(metaclass))
            {
               if (dispGroup != null)
               {
                  dispGroup.setValue("dispatched", Boolean.TRUE);

                  if (reader != dispGroup)
                  {
                     reader.setValue("dispatched", Boolean.TRUE);
                  }
               }

               return true;
            }
         }

         return false;
      }

      Object value;

      for (value = dispParent; value != null && dispAssoc != null; dispAssoc = dispAssoc.getNext())
      {
         dispParent = (Accessor)value;

         Attribute attribute = dispParent.getMetaclass().getAttribute(((Symbol)dispAssoc.getHead()).getName());

         attribute.checkReadAccess(m_context.getPrivilegeSet());
         value = dispParent.getValue(attribute.getOrdinal());
      }

      for (; dispValues != null; dispValues = dispValues.getNext())
      {
         if (Intrinsic.equal(value, dispValues.getHead()))
         {
            if (dispGroup != null)
            {
               dispGroup.setValue("dispatched", Boolean.TRUE);

               if (reader != dispGroup)
               {
                  reader.setValue("dispatched", Boolean.TRUE);
               }
            }

            return true;
         }
      }

      return false;
   }

   /**
    * Creates a cached query key.
    * @param sKey The cache key. Can be null.
    * @param user The user. Can be null.
    * @return The cached query key. Can be null, for null sKey.
    */
   protected static Pair getQueryKey(String sKey, Instance user)
   {
      if (sKey == null)
      {
         return null;
      }

      return Pair.list("read", (user != null) ? user.getOID() : null, sKey);
   }

   /**
    * Checks if a session has expired and a query should be performed.
    * @param sessionMap The session map. Can be null.
    * @param session Session identifier.
    * @param context The invocation context.
    * @return True if the session has expired.
    */
   protected static boolean isExpired(Lookup sessionMap, Object session, InvocationContext context)
   {
      if (sessionMap != null)
      {
         synchronized (sessionMap)
         {
            Object expiration = sessionMap.get(session);

            if (expiration == null)
            {
               return true;
            }

            if (!(expiration instanceof Timestamp) ||
               ((Timestamp)expiration).getTime() > context.getUnitOfWork().getTime())
            {
               return false;
            }
         }
      }

      return true;
   }

   /**
    * Checks if a session has expired and a query should be performed.
    * @param sKey The cache key. Can be null.
    * @param session Session identifier.
    * @param context The invocation context.
    * @return True if the session has expired.
    */
   public static boolean isExpired(String sKey, Object session, InvocationContext context)
   {
      Object key = getQueryKey(sKey, context.getUser());

      return key == null || isExpired((Lookup)context.getGlobalCache().get(key), session, context);
   }

   /**
    * Sets an empty result list on the reader.
    * @param reader The reader.
    * @return The result list.
    */
   protected static InstanceList setEmptyResult(Accessor reader)
   {
      reader.setValue("results", null);

      return null;
   }

   /**
    * Finds the value of a named argument in a list.
    * @param event The event object.
    * @param sName The argument name.
    * @param args The event argument value list.
    * @return The argument value, or null if not found.
    */
   public static Object findValue(Event event, String sName, Pair args)
   {
      Argument arg = event.findArgument(sName);

      if (arg == null)
      {
         return null;
      }

      for (int i = arg.getOrdinal(); i != 0; --i)
      {
         args = args.getNext();
      }

      return args.getHead();
   }

   /**
    * Sets the value of a named argument in an array.
    * @param event The event object.
    * @param sName The argument name.
    * @param args The event argument value array.
    * @param The value to set.
    */
   public static void setValue(Event event, String sName, Object[] args, Object value)
   {
      Argument arg = event.findArgument(sName);

      if (arg != null)
      {
         args[arg.getOrdinal() + 1] = value;
      }
   }

   /**
    * Updates the attributes if a custom event is specified.
    * @param reader The command object containing the query.
    */
   public void updateAttributes(Accessor reader, ActionContext actx)
   {
      Object value = reader.getValue("event");

      if (value != null)
      {
         Pair args;

         if (value instanceof Pair)
         {
            args = (Pair)value;
         }
         else
         {
            args = Pair.list(Symbol.define((String)value), reader.getValue("attributes"),
               reader.getValue("where"), reader.getValue("orderBy"), reader.getValue("count"),
               reader.getValue("offset"), reader.getValue("xlock"));

            reader.setValue("event", args);
         }

         Symbol symbol = (Symbol)args.getHead();

         args = args.getNext();

         Event event = (Event)m_context.getMetadata()
            .getMetaclass(((Symbol)reader.getValue("class")).getName())
            .getSelector(symbol).getMember(Pair.length(args));

         reader.setValue("attributes", findValue(event, "attributes", args));
         reader.setValue("where", findValue(event, "where", args));
         reader.setValue("orderBy", findValue(event, "orderBy", args));
         reader.setValue("count", findValue(event, "count", args));
         reader.setValue("offset", findValue(event, "offset", args));
         reader.setValue("xlock", findValue(event, "xlock", args));
      }
   }

   /**
    * Reads the instances specified in the reader command object query.
    * @param reader The command object containing the query.
    */
   public InstanceList read(Accessor reader, ActionContext actx)
   {
      boolean bUOWGlobalSaved = m_context.isUnitOfWorkGlobal();

      try
      {
         m_context.setUnitOfWorkGlobal(false);

         reader.invoke("load", LOAD_ATTRIBUTES);

         // Check if the data has changed
         Lookup sessionMap = null;
         Pair key = getQueryKey((String)reader.getValue("key"), m_context.getUser());

         if (key != null)
         {
            sessionMap = (Lookup)m_context.getGlobalCache().get(key);

            if (!isExpired(sessionMap, reader.getValue("session"), m_context))
            {
               return setEmptyResult(reader);
            }
         }

         if (!isEnabled(reader))
         {
            return setEmptyResult(reader);
         }

         Symbol classSym = (Symbol)reader.getValue("class");
         Pair attributes = (Pair)reader.getValue("attributes");
         Object where = reader.getValue("where");
         Pair orderBy = (Pair)reader.getValue("orderBy");
         Number count = (Number)reader.getValue("count");
         Number offset = (Number)reader.getValue("offset");
         Boolean xlock = (Boolean)reader.getValue("xlock");
         Pair bookmark = (Pair)reader.getValue("bookmark");
         Boolean next = (Boolean)reader.getValue("next");
         Boolean inclusive = (Boolean)reader.getValue("inclusive");
         List parentList = (List)reader.getValue("parents");
         Object args = reader.getValue("event");
         Metaclass metaclass = m_context.getMetadata().getMetaclass(classSym.getName());
         Object[] argArray = null;
         Event event = null;

         if (args instanceof Pair)
         {
            argArray = Pair.toArray((Pair)args);
            event = (Event)metaclass.getSelector((Symbol)argArray[0]).getMember(argArray.length - 1);
            argArray[0] = metaclass;

            Argument result = event.getResult();

            if (result != null && !result.getType().isPrimitive())
            {
               metaclass = (Metaclass)result.getType();
            }
         }
         else if (args instanceof String)
         {
            event = (Event)metaclass.getSelector((String)args).getMember(6);
            argArray = new Object[]{metaclass, attributes, where, orderBy, count, offset, xlock};
         }

         metaclass.checkReadAccess(m_context.getPrivilegeSet());

         Pair security = metaclass.checkReadAccess(attributes, m_context.getPrivilegeSet());

         metaclass.checkExpressionAccess(where,  m_context.getPrivilegeSet());

         if (parentList != null)
         {
            List assocList = (List)reader.getValue("associations");
            boolean bConj = false;

            for (int i = parentList.size() - 1; i >= 0; --i)
            {
               Accessor parent = (Accessor)parentList.get(i);
               InstanceList resultList = (InstanceList)parent.getValue("results");

               if (resultList == null || resultList.size() == 0)
               {
                  return setEmptyResult(reader);
               }

               Object assoc = assocList.get(i);

               if (assoc !=  null && !(assoc instanceof Pair))
               {
                  assoc = new Pair(assoc);
               }

               Instance parentInstance = resultList.getInstance(0);
               Metaclass parentMetaclass = parentInstance.getMetaclass();

               // Check for attributes without persistence mappings
               if (i == 0)
               {
                  Metaclass containerMetaclass = parentMetaclass;

                  for (Pair pair = (Pair)assoc; pair != null; pair = pair.getNext())
                  {
                     Attribute attribute = containerMetaclass.findAttribute((Symbol)pair.getHead());

                     if (attribute == null)
                     {
                        break;
                     }

                     attribute.checkReadAccess(m_context.getPrivilegeSet());

                     if (!attribute.isPersistent() &&
                        !attribute.getType().isPrimitive() &&
                        event == null &&
                        ((Metaclass)attribute.getType()).findEvent("read", 6)
                           .findAction("main").getDeclarator().getBase() == null)
                     {
                        if (parentList.size() > 1)
                        {
                           throw new InvalidQueryException("err.persistence.multipleParentsWithoutPersistence",
                              new Object[]{classSym, assoc, parentMetaclass.getName()});
                        }

                        if (where != null || bookmark != null || orderBy != null)
                        {
                           throw new InvalidQueryException("err.persistence.whereWithoutPersistence",
                              new Object[]{classSym, assoc, parentMetaclass.getName()});
                        }

                        resultList = new InstanceArrayList();
                        collectAssociatedInstances((Pair)assoc, parentInstance, resultList);

                        int nOffset = (offset == null) ? 0 : offset.intValue();
                        int nCount = (count == null) ? -1 : count.intValue();

                        if (nOffset < 0)
                        {
                           nOffset = 0;
                        }

                        if (nCount < 0 || nCount > resultList.size() - nOffset)
                        {
                           nCount = resultList.size() - nOffset;

                           if (nCount < 0)
                           {
                              nCount = 0;
                           }
                        }

                        if (nOffset > 0 || resultList.size() > nCount)
                        {
                           InstanceList list = new InstanceArrayList(nCount);

                           for (int k = 0; k < nCount; ++k)
                           {
                              list.add(resultList.getInstance(k + nOffset), InstanceList.REPLACE | InstanceList.DIRECT);
                           }

                           resultList = list;
                        }

                        if (next != null && !next.booleanValue())
                        {
                           resultList.reverse();
                        }

                        for (int k = 0; k < nCount; ++k)
                        {
                           resultList.getInstance(k).invoke("load", attributes);
                        }

                        reader.setValue("results", resultList);

                        return resultList;
                     }

                     if (attribute.getType().isPrimitive())
                     {
                        break;
                     }

                     containerMetaclass = (Metaclass)attribute.getType();
                  }
               }

               assoc = new Pair(Symbol.ATAT, new Pair(parentMetaclass.getSymbol(), assoc));

               Pair cond = Pair.binary(Symbol.EQ, assoc, parentInstance.getOID());

               if (bConj)
               {
                  where = new Pair(cond, where);
               }
               else if (where != null)
               {
                  where = new Pair(cond, new Pair(where));
                  bConj = true;
               }
               else
               {
                  where = cond;
               }
            }

            if (bConj)
            {
               where = new Pair(Symbol.AND, where);
            }
         }

         // Handle the bookmark
         boolean bNext = true;

         if (next != null && orderBy != null)
         {
            bNext = next.booleanValue();

            if (!bNext)
            {
               // Flip sort order
               Pair pair = orderBy;
               Pair last = orderBy = null;

               for (; pair != null; pair = pair.getNext())
               {
                  Pair head = (Pair)pair.getHead();
                  Pair item = new Pair(new Pair(head.getHead(),
                     Boolean.valueOf(!((Boolean)head.getTail()).booleanValue())));

                  if (last == null)
                  {
                     orderBy = item;
                  }
                  else
                  {
                     last.setTail(item);
                  }

                  last = item;
               }
            }

            // The bookmark has the following format: (value1 ... valueN)
            // If values are missing, then the start or the end of the recordset is
            // retrieved, depending on the value of next (inclusive is ignored in this case)

            if (bookmark != null)
            {
               where = addBookmarkToWhere(where, orderBy, bookmark, inclusive);
            }
         }

         // This takes care of the bookmark attributes access rights
         metaclass.checkOrderByAccess(orderBy, m_context.getPrivilegeSet());
         attributes = Pair.nconc(security, attributes); 

         InstanceList resultList;
         
         if (event != null)
         {
            setValue(event, "attributes", argArray, attributes);
            setValue(event, "where", argArray, where);
            setValue(event, "orderBy", argArray, orderBy);

            resultList = (InstanceList)event.invoke(argArray, m_context.getMachine());
         }
         else
         {
            resultList = (InstanceList)metaclass.invoke("read",
               new Object[]{attributes, where, orderBy, count, offset, xlock});
         }

         if (!bNext)
         {
            resultList.reverse();
         }

         reader.setValue("results", resultList);

         if (key != null)
         {
            Lookup cacheMap = null;

            if (sessionMap == null)
            {
               sessionMap = new SoftHashTab(1);
               cacheMap = new HashTab(1);
            }

            synchronized (sessionMap)
            {
               Number expiration = (Number)reader.getValue("expiration");
               long lExpiration = (expiration == null) ? -1 : expiration.longValue();

               sessionMap.put(reader.getValue("session"),
                  (lExpiration >= 0) ? new Timestamp(m_context.getUnitOfWork().getTime() + lExpiration) :
                     (Object)Undefined.VALUE);
            }

            if (cacheMap != null)
            {
               cacheMap.put(key, sessionMap);
               m_context.getGlobalCache().update(null, cacheMap, m_context.getUnitOfWork().getTime());
            }
         }

         return resultList;
      }
      finally
      {
         m_context.setUnitOfWorkGlobal(bUOWGlobalSaved);
      }
   }

   /**
    * Add bookmark to where.
    * @param where Where.
    * @param orderBy Order by.
    * @param bookmark Bookmark: (value1 ... valueN).
    * @param inclusive Whether inclusive.
    * @return New where containing bookmark condition.
    */
   public static Object addBookmarkToWhere(Object where, Pair orderBy, Pair bookmark, Boolean inclusive)
   {
      // Add conditions to the where clause
      boolean bInclusive = inclusive.booleanValue();
      Pair lastOrderBy = null;
      boolean bDisj = false;
      Pair disj = null;

      do
      {
         boolean bConj = false;
         Pair conj = null;
         Pair lastConj = null;

         for (Pair pair = orderBy, mark = bookmark;;
            pair = pair.getNext(), mark = mark.getNext())
         {
            Symbol op;

            if (pair.getTail() == lastOrderBy || mark.getTail() == null)
            {
               lastOrderBy = pair;

               if (!((Boolean)((Pair)pair.getHead()).getTail()).booleanValue())
               {
                  op = (bInclusive && mark.getTail() == null) ? Symbol.LE : Symbol.LT;
               }
               else
               {
                  op = (bInclusive && mark.getTail() == null) ? Symbol.GE : Symbol.GT;
               }
            }
            else
            {
               op = Symbol.EQ;
            }

            Pair cmp = Pair.binary(op, ((Pair)pair.getHead()).getHead(), mark.getHead());

            if (conj == null)
            {
               conj = lastConj = cmp;
            }
            else if (bConj)
            {
               Pair newConj = new Pair(cmp);

               lastConj.setTail(newConj);
               lastConj = newConj;
            }
            else
            {
               lastConj = new Pair(cmp);
               conj = new Pair(conj, lastConj);
               bConj = true;
            }

            if (pair == lastOrderBy)
            {
               if (bConj)
               {
                  conj = new Pair(Symbol.AND, conj);
               }

               break;
            }
         }

         if (disj == null)
         {
            disj = conj;
         }
         else if (bDisj)
         {
            disj = new Pair(conj, disj);
         }
         else
         {
            disj = new Pair(conj, new Pair(disj));
            bDisj = true;
         }
      }
      while (lastOrderBy != orderBy);

      if (bDisj)
      {
         disj = new Pair(Symbol.OR, disj);
      }

      where = Pair.commutative(Symbol.AND, where, disj);

      if (bookmark.getTail() != null)
      {
         where = Pair.commutative(Symbol.AND, where,
            Pair.binary((!((Boolean)((Pair)orderBy.getHead()).getTail()).booleanValue())
               ? Symbol.LE : Symbol.GE, ((Pair)orderBy.getHead()).getHead(), bookmark.getHead()));
      }

      return where;
   }
}
