package nexj.core.rpc.msg;

import java.util.Set;

import nexj.core.meta.Component;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.persistence.OID;
import nexj.core.runtime.ActionContext;
import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.Pair;
import nexj.core.util.Lifecycle;

/**
 * Concurrency control component to support the SysMessageDispatcher class.
 * @deprecated
 */
public class SysMessageDispatcher implements Lifecycle
{
   // operations

   
   /**
    * @deprecated
    * Acquire a semaphore.  Note: not thread-safe.  Assumes caller is the single dispatcher thread.
    * @param semaphoreDef The (resource . maxCount) pair defining the semaphore.
    * @return true if the semaphore is acquired, false otherwise.
    */
   public static Boolean acquireSemaphore(Metaclass metaclass, InvocationContext context, SysMessageDispatcher comp, Pair semaphoreDef, ActionContext actx)
   {
      return Boolean.FALSE;
   }
   
   /**
    * @deprecated
    * Release a semaphore on commit.  Note: not thread-safe.  Assumes caller is the single dispatcher thread.
    * @param semaphoreDef The (resource . maxCount) pair defining the semaphore.
    */
   public static void releaseSemaphore(Metaclass metaclass, InvocationContext context, SysMessageDispatcher comp, Pair semaphoreDef, ActionContext actx)
   {
   }
   
   /**
    * @deprecated
    * Complete a message delivery.
    * @param OID oid the oid of the message that has been delivered.
    */
   public static void complete(Metaclass metaclass, InvocationContext context, final SysMessageDispatcher comp, final OID oid, ActionContext actx)
   {
   }

   /**
    * @deprecated
    * @return Set of resources that have saturated semaphores.
    */
   public Set getBlockingSet()
   {
      return null;
   }

   /**
    * @deprecated
    * Sets the invocation context component.
    * @param contextComponent The invocation context component to set.
    */
   public void setContextComponent(Component contextComponent)
   {
   }
   
   /**
    * @deprecated
    * Sets the metadata.
    * @param metadata The metadata.
    */
   public void setMetadata(Metadata metadata)
   {
   }

   /**
    * @see nexj.core.util.Lifecycle#shutdown()
    */
   public synchronized void shutdown()
   {
   }

   /**
    * @see nexj.core.util.Lifecycle#startup()
    */
   public synchronized void startup() throws Exception
   {
   }

   /**
    * @see nexj.core.util.Suspendable#resume()
    */
   public void resume() throws Exception
   {
      startup();
   }

   /**
    * @see nexj.core.util.Suspendable#suspend()
    */
   public void suspend() throws Exception
   {
      shutdown();
   }
}