package nexj.core.runtime;

import java.util.Iterator;

import nexj.core.meta.ClassAspect;
import nexj.core.meta.Component;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.Repository;
import nexj.core.util.Lifecycle;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * Initializer for classes that have RUNTIME_INITIALIZER aspect.
 */
public class Initializer implements Initializable, Lifecycle
{
   // associations

   /**
    * The invocation context component.
    */
   protected Component m_contextComponent;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(Initializer.class);

   // operations

   /**
    * Sets the invocation context component.
    * @param contextComponent The invocation context component to set.
    */
   public void setContextComponent(Component contextComponent)
   {
      m_contextComponent = contextComponent;
   }

   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      if (m_contextComponent == null)
      {
         m_contextComponent = Repository.getMetadata().getComponent("System.InvocationContext");
      }
   }

   /**
    * Invokes an event on classes having a given aspect.
    * @param sAspect The aspect name.
    * @param sEvent The event name.
    * @param sDescription The operation description.
    * @param bPreCommit True to pre-commit after the event invocation.
    * @param context The invocation context. 
    */
   public static void invoke(String sAspect, String sEvent, String sDescription,
      boolean bPreCommit, InvocationContext context)
   {
      Metadata metadata = context.getMetadata();
      ClassAspect aspect = metadata.findClassAspect(sAspect);

      if (aspect != null)
      {
         boolean bLogged = false;

         for (Iterator itr = metadata.getMetaclassIterator(); itr.hasNext();)
         {
            Metaclass metaclass = (Metaclass)itr.next();

            if (metaclass.hasAspect(aspect))
            {
               if (!bLogged)
               {
                  if (s_logger.isDebugEnabled())
                  {
                     s_logger.debug("Started " + sDescription);
                  }

                  bLogged = true;
               }

               metaclass.invoke(sEvent);

               if (bPreCommit)
               {
                  context.getUnitOfWork().commit(false);
               }
            }
         }

         if (bLogged && s_logger.isDebugEnabled())
         {
            s_logger.debug("Completed " + sDescription);
         }
      }
   }

   /**
    * @see nexj.core.util.Lifecycle#startup()
    */
   public void startup() throws Exception
   {
      Context contextSaved = ThreadContextHolder.getContext();
      InvocationContext context = (InvocationContext)m_contextComponent.getInstance(null);

      try
      {
         context.initialize(null);
         context.setSecure(false);
         invoke("RUNTIME_INITIALIZER", "initializeRuntime", "initializing runtime", false, context);
         context.complete(true);
      }
      catch (Throwable t)
      {
         context.complete(false);

         if (t instanceof Exception)
         {
            throw (Exception)t;
         }

         ObjUtil.rethrow(t);
      }
      finally
      {
         ThreadContextHolder.setContext(contextSaved);
      }
   }

   /**
    * @see nexj.core.util.Lifecycle#shutdown()
    */
   public void shutdown()
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
