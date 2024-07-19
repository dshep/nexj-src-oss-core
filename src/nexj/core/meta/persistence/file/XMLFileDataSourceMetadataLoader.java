// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.file;

import java.util.Iterator;

import org.w3c.dom.Element;

import nexj.core.meta.Aspect;
import nexj.core.meta.Component;
import nexj.core.meta.ComponentPropertyInitializer;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.DataSourceType;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.meta.persistence.SchemaUpgrade;
import nexj.core.meta.persistence.XMLPersistenceMetadataLoader;
import nexj.core.meta.xml.XMLMetadata;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataHelper.ContextFixup;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.util.J2EEUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.XMLUtil;
import nexj.core.util.XMLUtil.ElementHandler;

/**
 * Load metadata for the File Persistence Adapter from XML.
 */
public class XMLFileDataSourceMetadataLoader implements XMLPersistenceMetadataLoader
{
   // constants

   /**
    * The name of the attribute that holds the path to the root of the
    * directory tree used for storing the data files.
    */
   protected final static String CONNECTION_DATA_DIR = "dataDirectory";

   /**
    * The name of the attribute that holds the path to the temporary directory
    * used for preparing data files for persistence.
    */
   protected final static String CONNECTION_TEMP_DIR = "temporaryDirectory";

   /**
    * The name of the attribute that holds the path to the directory in which
    * to store the transactional journal for the adapter.
    */
   protected final static String CONNECTION_JOURNAL_DIR = "journalDirectory";

   /**
    * The name of the attribute that holds the count of the maximum number
    * of levels of subdirectories to create inside the data directory.
    */
   protected final static String CONNECTION_SUBDIR_MAX_LEVELS = "subdirLevels";

   /**
    * The name of the attribute that holds the number of characters to use
    * in the names of the subdirectories created inside the data directory.
    */
   protected final static String CONNECTION_SUBDIR_NAME_LEN = "subdirNameLength";

   /**
    * The name of the attribute that holds the maximum connection pool size.
    */
   protected final static String CONNECTION_MAX_POOL_SIZE = "maxPoolSize";

   /**
    * The name of the tag that defines how attributes on the metaclass are
    * mapped to fields in the file persistence adapter.
    */
   protected final static String MAPPING_TAG_NAME = "FileStorageMapping";

   /**
    * The name of the attribute that holds the metaclass attribute name
    * that is mapped to the file name in the file persistence adapter.
    */
   protected final static String MAPPING_ID_ATTRIBUTE = "name";

   /**
    * The name of the attribute that holds the metaclass attribute name
    * that is mapped to the contents of the files retrieved by the
    * file persistence adapter.
    */
   protected final static String MAPPING_DATA_ATTRIBUTE = "data";

   /**
    * The name of the attribute that holds the name of the key generator
    * component to use for generating names for files as they are persisted.
    */
   protected final static String MAPPING_KEY_GENERATOR_ATTRIBUTE = "keyGenerator";


   // associations

   /**
    * The metadata loader.
    */
   private XMLMetadataLoader m_loader;


   // operations

   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataLoader#loadConnection(org.w3c.dom.Element, nexj.core.meta.persistence.DataSource, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public void loadConnection(Element element, final DataSource source, final XMLMetadataLoader loader)
   {
      final FileDataSource fileDS = (FileDataSource)source;
      final FileDataSourceFragment defaultFragment = (FileDataSourceFragment)fileDS.getDefaultFragment();

      final Component component = new Component(source.getName(), source.getAdapter().getClassObject(), Component.CONTEXT);

      fileDS.setComponent(component);
      component.setMetadata(loader.getMetadata());

      if (element != null)
      {
         //This loads the default fragment properties off the main FileStorageConnection node
         loadFragment(element, defaultFragment, false);

         //Load property overrides for each fragment off the fragment nodes
         XMLUtil.withFirstChildElement(element, "Fragments", false, new ElementHandler()
         {
            public void handleElement(Element fragmentsElement)
            {
               XMLUtil.forEachChildElement(fragmentsElement, "Fragment", getHelper().new ElementHandler("fragment")
               {
                  protected void handleElement(Element fragmentElement, String sName)
                  {
                     FileDataSourceFragment fragment = (FileDataSourceFragment)defaultFragment.clone();

                     fragment.setName(sName);
                     loadFragment(fragmentElement, fragment, true);
                     fileDS.addFragment(fragment);
                  }
               });
            }
         });

         if (!loader.isEnvironmentOnly())
         {
            loader.addEnvironmentFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
            {
               public void fixup()
               {
                  Component cf = null;

                  for (Iterator itr = fileDS.getFragmentIterator(); itr.hasNext(); )
                  {
                     FileDataSourceFragment fragment = (FileDataSourceFragment)itr.next();

                     if (J2EEUtil.isContained())
                     {
                        cf = new Component(source.getName() + ".ConnectionFactory" + fragment.getSuffix(),
                           loader.getHelper().getClassObject(SysUtil.PACKAGE + ".core.persistence.file.JNDIFileStorageConnectionFactory"),
                           Component.SINGLETON);

                        cf.addPrimitivePropertyInitializer("factoryName", J2EEUtil.JNDI_ENV_PREFIX + "fileStorage/" + fileDS.getName() + fragment.getSuffix());
                        cf.addPrimitivePropertyInitializer("dataSource", fileDS);
                     }
                     else
                     {
                        cf = new Component(source.getName() + ".ConnectionFactory" + fragment.getSuffix(),
                           loader.getHelper().getClassObject(SysUtil.PACKAGE + ".core.persistence.file.XAFileStorageConnectionFactory"),
                           Component.SINGLETON);

                        cf.addPrimitivePropertyInitializer("fragment", fragment);

                        final ComponentPropertyInitializer initializer = cf.addComponentPropertyInitializer("transactionManager", null);

                        loader.addComponentFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
                        {
                           public void fixup()
                           {
                              initializer.setInstanceComponent(loader.getMetadata().getComponent("System.TransactionManager"));
                           }
                        });
                     }

                     cf.setMetadata(loader.getMetadata());
                     fragment.setConnectionFactory(cf);
                     loader.addSingletonFixup(cf);
                  }

                  if (fileDS.getFragmentCount() > 1)
                  {
                     component.addPrimitivePropertyInitializer("fragmented", Boolean.TRUE);
                  }
               }
            });
         }

         loader.addSingletonFixup(component);
      }
      else
      {
         //No connection element, so the data source is disabled.
         fileDS.setCreatable(false);
         fileDS.setReadable(false);
         fileDS.setUpdatable(false);
         fileDS.setDeletable(false);
         fileDS.setExecutable(false);
      }
   }


   /**
    * Loads a fragment from a DOM element.
    *
    * For the default fragment, call it with element as the FileStorageConnection tag,
    * and bDefault as false.
    *
    * For the non-default fragments, element should be the Fragment tag itself and
    * bDefault should be true. The fragment must have been clone()'d from the default
    * fragment for configuration property inheritance to work properly.
    *
    *
    * @param element The DOM element.
    * @param fragment The fragment to load.
    * @param bDefault True to to inherit unspecified properties from the default fragment
    *                 (from which the non-default fragments must be clone()'d); false to
    *                 to use the global defaults for unspecified properties, throwing
    *                 an error if there is no global default.
    */
   protected void loadFragment(Element element, FileDataSourceFragment fragment, boolean bDefault)
   {
      /*
       * The following fragment properties must be specified on all fragments.
       */
      fragment.setDataDirectory(XMLUtil.getReqStringAttr(element, CONNECTION_DATA_DIR));
      fragment.setTemporaryDirectory(XMLUtil.getReqStringAttr(element, CONNECTION_TEMP_DIR));
      fragment.setJournalPath(XMLUtil.getStringAttr(element, CONNECTION_JOURNAL_DIR));

      /*
       * The following fragment properties have a global default value, which is overridden
       * by the default fragment, which is overridden in each non-default fragment.
       */
      fragment.setMaxNameSplits(
         XMLUtil.getIntAttr(element, CONNECTION_SUBDIR_MAX_LEVELS, fragment.getMaxNameSplits())
         );
      fragment.setNameSplitSize(
         XMLUtil.getIntAttr(element, CONNECTION_SUBDIR_NAME_LEN, fragment.getNameSplitSize())
         );
      fragment.setMaxPoolSize(
         XMLUtil.getIntAttr(element, CONNECTION_MAX_POOL_SIZE, fragment.getMaxPoolSize()));
   }


   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataLoader#loadDataSource(org.w3c.dom.Element, java.lang.String, nexj.core.meta.persistence.DataSourceType, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public DataSource loadDataSource(Element element, String sName, DataSourceType type, XMLMetadataLoader loader)
   {
      FileDataSource ds = new FileDataSource(sName);

      m_loader = loader;

      ds.setType(type);
      loader.loadDataSource(element, ds);

      return ds;
   }


   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataLoader#loadMapping(org.w3c.dom.Element, nexj.core.meta.Metaclass, nexj.core.meta.persistence.DataSource, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public PersistenceMapping loadMapping(Element persistenceMappingElement, final Metaclass metaclass, DataSource dataSource, final XMLMetadataLoader loader)
   {
      final FileMapping mapping = new FileMapping();
      int nCookie = getHelper().pushMarker(MetadataValidationException.TYPE_NAME, MAPPING_TAG_NAME);
      getHelper().pushMarker("class", metaclass.getName());

      m_loader = loader;

      try
      {
         mapping.setMetaclass(metaclass);
         mapping.setDataSource(dataSource);

         XMLUtil.withFirstChildElement(persistenceMappingElement, MAPPING_TAG_NAME,
            true, new XMLUtil.ElementHandler()
         {
            public void handleElement(Element mappingElement)
            {
               loadAttributeMapping(mappingElement, MAPPING_ID_ATTRIBUTE, FilePrimitiveMapping.SYSID_ID, false, mapping);
               loadAttributeMapping(mappingElement, MAPPING_DATA_ATTRIBUTE, FilePrimitiveMapping.SYSID_DATA, false, mapping);

               /*
                * Key Generator
                */
               if (!(metaclass instanceof Aspect))
               {
                  final String sKeyGeneratorName = XMLUtil.getReqStringAttr(mappingElement, MAPPING_KEY_GENERATOR_ATTRIBUTE);

                  if (sKeyGeneratorName != null)
                  {
                     loader.addComponentFixup(new ContextFixup(getHelper())
                     {
                        public void fixup()
                        {
                           mapping.setKeyGenerator(
                              metaclass.getMetadata().getComponent(sKeyGeneratorName)
                           );
                        }
                     });
                  }
               }
            }
         });
      }
      finally
      {
         getHelper().restoreMarker(nCookie);
      }

      return mapping;
   }


   /**
    * Configures a primitive mapping between an attribute and a persistence field
    * in the file persistence adapter.
    *
    * @param element The DOM element.
    * @param sAttribute The DOM attribute name.
    * @param nSysId One of the MAPPING_* constants.
    * @param bRequired True if the mapping is required.
    * @param fileMapping The destination mapping.
    */
   protected void loadAttributeMapping(Element element, String sAttribute,
      byte nSysId, boolean bRequired, FileMapping fileMapping)
   {
      String sName = (bRequired) ? XMLUtil.getReqStringAttr(element, sAttribute) : XMLUtil.getStringAttr(element, sAttribute);

      if (sName != null)
      {
         FilePrimitiveMapping mapping = new FilePrimitiveMapping();

         mapping.setAttribute(fileMapping.getMetaclass().getAttribute(sName));
         mapping.setSysId(nSysId);
         fileMapping.addAttributeMapping(mapping);

         mapping.validate(getMetadata(), m_loader.getHelper().getWarnings());
      }
   }


   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataLoader#loadUpgrade(org.w3c.dom.Element, java.lang.String, nexj.core.meta.persistence.DataSource, nexj.core.meta.xml.XMLMetadataHelper)
    */
   public SchemaUpgrade loadUpgrade(Element element, String sName, DataSource source, XMLMetadataHelper helper)
   {
      throw new MetadataException("err.meta.upgrade.unsupported", new Object[]{source.getType().getName()});
   }


   /**
    * @return The current metadata object.
    */
   protected XMLMetadata getMetadata()
   {
      return m_loader.getMetadata();
   }


   /**
    * @return The metadata helper object.
    */
   protected XMLMetadataHelper getHelper()
   {
      return m_loader.getHelper();
   }
}
