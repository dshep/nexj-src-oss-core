// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.sys;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import nexj.core.integration.MessageFormatter;
import nexj.core.integration.MessageParser;
import nexj.core.integration.Transformer;
import nexj.core.integration.io.ObjectOutput;
import nexj.core.integration.io.ReaderInput;
import nexj.core.meta.Accessor;
import nexj.core.meta.Metadata;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.Transformation;
import nexj.core.meta.xml.XMLMetadata;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataHelper.CharacterStreamHandler;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.ActionContext;
import nexj.core.runtime.Initializer;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.runtime.UnitOfWork;
import nexj.core.scripting.Function;
import nexj.core.scripting.Intrinsic;
import nexj.core.util.HashHolder;
import nexj.core.util.Logger;
import nexj.core.util.UncheckedException;

/**
 * System upgrade manager.
 */
public class SysUpgrade implements InvocationContextAware
{
   // associations

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(SysUpgrade.class);

   // operations

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * Seeds data taken from any resource.
    * @deprecated
    */
   public void seed(Accessor metaclass, String sContainerName, String sName, String sExt,
      String sMarkerTypeName, String sMarkerPropertyName, String sTransformation,
      final Function enabled, final Function delete, ActionContext actx)
   {
      seed(metaclass, sExt, sMarkerTypeName, sMarkerPropertyName, sTransformation, enabled, delete, actx);
   }

   /**
    * Seeds data taken from a collection of resources.
    * @param resourceNameList the list of resources from which to seed.
    * @param sExt the extension of the resources from which to seed.
    * @param sMarkerTypeName the marker type name.
    * @param sMarkerPropertyName the marker property name.
    * @param sTransformation the name of the transformation to map the resources to objects.
    * @param enabled the function to determine a the resource seed update can be performed for a given resource.
    * @param delete the function to determine a the resource seed delete can be performed for a given resource.
    */
   public void seed(Accessor metaclass, final List resourceNameList, final String sExt,
      final String sMarkerTypeName, final String sMarkerPropertyName, String sTransformation,
      Function enabled, Function delete, ActionContext actx)
   {
      final Set nameSet = new HashHolder(resourceNameList.size());

      nameSet.addAll(resourceNameList);

      seed(sTransformation, enabled, delete, new ResourceLoader()
      {
         public void seedResources(XMLMetadataHelper helper, final CharacterStreamHandler handler)
         {
            helper.loadResources(sExt, sMarkerTypeName, sMarkerPropertyName, new CharacterStreamHandler()
            {
               public void handleCharacterStream(Reader reader, String sName) throws IOException
               {
                  if (nameSet.contains(sName))
                  {
                     handler.handleCharacterStream(reader, sName);
                  }
               }
            }, null);
         }
      });
   }

   /**
    * Seeds data taken from a collection of resources.
    * @param sExt the extension of the resources from which to seed.
    * @param sMarkerTypeName the marker type name.
    * @param sMarkerPropertyName the marker property name.
    * @param sTransformation the name of the transformation to map the resources to objects.
    * @param enabled the function to determine a the resource seed update can be performed for a given resource.
    * @param delete the function to determine a the resource seed delete can be performed for a given resource.
    */
   public void seed(Accessor metaclass, final String sExt,
      final String sMarkerTypeName, final String sMarkerPropertyName, String sTransformation,
      Function enabled, Function delete, ActionContext actx)
   {
      seed(sTransformation, enabled, delete, new ResourceLoader()
      {
         public void seedResources(XMLMetadataHelper helper, CharacterStreamHandler handler)
         {
            helper.loadResources(sExt, sMarkerTypeName, sMarkerPropertyName, handler, null);
         }
      });
   }

   /**
    * Seeds data taken from a collection of resources.
    * @param sMarkerTypeName the marker type name.
    * @param sMarkerPropertyName the marker property name.
    * @param sTransformation the name of the transformation to map the resources to objects.
    * @param enabled the function to determine a the resource seed update can be performed for a given resource.
    * @param delete the function to determine a the resource seed delete can be performed for a given resource.
    * @param loader the loader of resources to seed.
    */
   protected void seed(String sTransformation, final Function enabled, final Function delete, ResourceLoader loader)
   {
      final Metadata metadata = m_context.getMetadata();
      final MessageParser parser = (MessageParser)metadata.getFormat("XML").getParser().getInstance(m_context);
      final MessageFormatter formatter = (MessageFormatter)metadata.getFormat("Object").getFormatter().getInstance(m_context);
      final Transformation transformation = metadata.getTransformation(sTransformation);
      final Transformer transformer = new Transformer(m_context);
      final UnitOfWork uow = m_context.getUnitOfWork();
      final boolean bRawSaved = uow.isRaw();

      try
      {
         uow.setRaw(true);

         final List tobjList = new ArrayList();

         loader.seedResources(((XMLMetadata)metadata).getHelper(), new CharacterStreamHandler()
         {
            public void handleCharacterStream(Reader reader, String sName) throws IOException
            {
               try
               {
                  TransferObject tobj = parser.parse(new ReaderInput(reader), (Message)transformation.getSource());

                  tobj.setValue("name", sName);

                  if (enabled != null && !Intrinsic.isTrue(m_context.getMachine().invoke(enabled, tobj, (Object[])null)))
                  {
                     return;
                  }

                  if (delete != null)
                  {
                     m_context.getMachine().invoke(delete, tobj, (Object[])null);
                  }

                  tobj = transformer.transform(tobj, transformation);
                  tobjList.add(tobj);
               }
               catch (Exception e)
               {
                  s_logger.error("Unable to seed resource \"" + sName + "\"", e);

                  if (e instanceof RuntimeException)
                  {
                     throw (RuntimeException)e;
                  }

                  throw new UncheckedException("err.runtime.seed", new Object[]{sName}, e);
               }
            }
         });

         // Commit the deletes
         uow.commit(false);

         for (Iterator itr = tobjList.iterator(); itr.hasNext();)
         {
            formatter.format((TransferObject)itr.next(), (Message)transformation.getDestination(), new ObjectOutput());
         }

         // Commit the inserts/updates
         uow.commit(false);
      }
      finally
      {
         uow.setRaw(bRawSaved);
      }
   }

   /**
    * Delegates seeding to classes with PERSISTENCE_INITIALIZER aspect.
    */
   public void initializePersistence(Accessor metaclass, ActionContext actx)
   {
      Initializer.invoke("PERSISTENCE_INITIALIZER", "initializePersistence", "initializing persistence", true, m_context);
   }

   // inner classes

   /**
    * Interface to encapsulate seeding a collection of resources.
    */
   protected interface ResourceLoader
   {
      /**
       * Seeds a collection of resources.
       * @param handler the character stream handler for each resource.
       */
      public void seedResources(XMLMetadataHelper helper, CharacterStreamHandler handler);
   }
}
