package nexj.core.tools;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;

import nexj.core.meta.MetadataObject;
import nexj.core.meta.NamedMetadataObject;
import nexj.core.meta.Primitive;
import nexj.core.runtime.Initializable;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Lifecycle;
import nexj.core.util.Logger;
import nexj.core.util.Named;
import nexj.core.util.StringUtil;
import nexj.core.util.Suspendable;

/**
 * Generic class to control and execute a tool on metadata objects.
 */
public abstract class GenericMetadataObjectTool extends GenericTool
{
   // constants

   /**
    * Toggle regular expression support argument.
    */
   protected final static String REGEXP_PROPERTY = "regexp";
   
   /**
    * Toggle test sorting argument.
    */
   protected final static String SORT_PROPERTY = "sort";
   
   // associations

   /**
    * List of metadata objects this tool operates on.
    */
   protected List m_objectList = new ArrayList();

   /**
    * The J2EE container for the tests.
    */
   protected DelayedStartWrapper m_container;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(GenericMetadataObjectTool.class);

   // operations

   /**
    * @see nexj.core.tools.GenericTool#execute(java.lang.String)
    * Aggregates metadata objects to be operated on sequentially.
    */
   protected void execute(String sCommand) throws Exception
   {
      Pattern pattern = null;

      boolean bExclude = sCommand.startsWith("!");

      if (bExclude)
      {
         sCommand = sCommand.substring(1);
      }

      if (StringUtil.parseBoolean(getProperty(REGEXP_PROPERTY, "false")))
      {
         pattern = Pattern.compile(sCommand);
      }
      else if (sCommand.indexOf('*') >= 0 || sCommand.indexOf('?') >= 0)
      {
         sCommand = sCommand.replace("\\", "");
         pattern = Primitive.likePattern(sCommand, 0);
      }

      if (pattern != null)
      {
         if (bExclude)
         {
            for (Iterator itr = m_objectList.iterator(); itr.hasNext();)
            {
               if (pattern.matcher(((NamedMetadataObject)itr.next()).getName()).matches())
               {
                  itr.remove();
               }
            }
         }
         else
         {
            boolean bMatch = false;

            for (Iterator itr = getObjectNameIterator(); itr.hasNext();)
            {
               String sObjectName = (String)itr.next();

               if (pattern.matcher(sObjectName).matches())
               {
                  m_objectList.add(getObject(sObjectName));
                  bMatch = true;
               }
            }

            if (s_logger.isInfoEnabled() && !bMatch)
            {
               s_logger.info("No " + getObjectTypeName() + " matches \"" + sCommand + "\"");
            }
         }
      }
      else
      {
         MetadataObject obj = getObject(sCommand);

         if (bExclude)
         {
            m_objectList.remove(obj);
         }
         else
         {
            m_objectList.add(obj);
         }
      }
   }

   /**
    * @see nexj.core.tools.GenericTool#begin()
    */
   protected void begin() throws Exception
   {
      super.begin();

      if (isContained() && J2EEUtil.CONTAINER == J2EEUtil.TEEE)
      {
         Lifecycle container = (Lifecycle)Class.forName("nexj.core.container.platform.teee.GenericContainerContainer")
            .newInstance();

         if (container instanceof Initializable)
         {
            ((Initializable)container).initialize();
         }

         m_container = new DelayedStartWrapper(container);
      }
   }

   /**
    * @see nexj.core.tools.GenericTool#end()
    */
   protected void end() throws Exception
   {
      super.end();

      if (StringUtil.parseBoolean(getProperty(SORT_PROPERTY, "true")))
      {
         Collections.sort(m_objectList, Named.COMPARATOR);
      }

      if (m_objectList.isEmpty())
      {
         if (s_logger.isInfoEnabled())
         {
            s_logger.info("No " + getObjectTypeName() + " to run");
         }
      }
   }

   /**
    * @see nexj.core.tools.GenericTool#getOptionUsage()
    */
   protected String[] getOptionUsage()
   {
      return new String[]
      {
         "-D" + REGEXP_PROPERTY + "=true|false",
         "-D" + SORT_PROPERTY + "=true|false"
      };
   }

   /**
    * @return True to initialize a container, if available.
    */
   protected boolean isContained()
   {
      return true;
   }

   /**
    * @param sName the name of the metadata object
    * @return the MetadataObject
    */
   protected abstract NamedMetadataObject getObject(String sName);

   /**
    * @return an iterator for the names of the metadata objects
    */
   protected abstract Iterator getObjectNameIterator();

   /**
    * @return the name of the metadata object type this tool operates on
    */
   protected abstract String getObjectTypeName();

   // inner classes

   /**
    * A wrapper that starts the container the first time resume is called.
    */
   protected static class DelayedStartWrapper implements Suspendable
   {
      // attributes

      /**
       * Whether or not the wrapped container has been started.
       */
      protected boolean m_bStarted;

      // associations

      /**
       * The wrapped container.
       */
      protected Lifecycle m_wrapped;

      /**
       * Flag to determine if the container is suspended.
       */
      protected boolean m_bSuspended;

      // constructors

      /**
       * Creates a new wrapper.
       * @param wrapped The container to wrap.
       */
      public DelayedStartWrapper(Lifecycle wrapped)
      {
         m_wrapped = wrapped;
      }

      /**
       * Creates a new wrapper.
       * @param wrapped The container to wrap.
       * @param bStarted The initial state of the container.
       */
      public DelayedStartWrapper(Lifecycle wrapped, boolean bStarted)
      {
         m_wrapped = wrapped;
         m_bStarted = bStarted;
      }

      // operations

      /**
       * @see nexj.core.util.Suspendable#resume()
       */
      public void resume() throws Exception
      {
         if (m_bStarted)
         {
            if (m_bSuspended)
            {
               m_wrapped.resume();
               m_bSuspended = false;
            }
         }
         else
         {
            m_wrapped.startup();
            m_bStarted = true;
         }
      }

      /**
       * @see nexj.core.util.Suspendable#suspend()
       */
      public void suspend() throws Exception
      {
         if (m_bStarted)
         {
            m_wrapped.suspend();
            m_bSuspended = true;
         }
      }

      /**
       * Shutdown wrapped container.
       */
      public void shutdown() throws Exception
      {
         if (m_bStarted)
         {
            m_wrapped.shutdown();
         }
      }
   }
}
