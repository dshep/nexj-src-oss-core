// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.Properties;

import nexj.core.util.Logger;
import nexj.core.util.ProgressListener;
import nexj.core.util.SysUtil;

/**
 * Metadata loader proxy capable of determining the correct loader and arguments to use based on
 * supplied and system arguments.
 */
public class MetadataLoaderDispatcher implements MetadataLoader
{
   // constants

   /**
    * Metadata loader property name.
    */
   public final static String METADATA_LOADER_PROPERTY = "meta.loader";

   /**
    * Default metadata loader class name.
    */
   public final static String DEFAULT_METADATA_LOADER = SysUtil.PACKAGE + ".core.meta.xml.DefaultXMLMetadataLoader";

   // associations

   /**
    * The metadata loader.
    */
   protected MetadataLoader m_loader;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(MetadataLoaderDispatcher.class);

   // operations

   /**
    * @param sURI Determine sURI value from system properties.
    * @param properties Use SysUtil.getConfigProperties() as properties value.
    * @param nFlags Use DEFAULT as nFlags value.
    * @param progress Use null as progress value.
    * @see nexj.core.meta.MetadataLoader#load(java.lang.String, java.util.Properties, int, nexj.core.util.ProgressListener)
    */
   public Metadata load() throws MetadataException
   {
      return load(null, null, DEFAULT, null);
   }

   /**
    * @param sURI (null == determine sURI value from system properties).
    * @param properties (null == use SysUtil.getConfigProperties()).
    * @see nexj.core.meta.MetadataLoader#load(java.lang.String, java.util.Properties, int, nexj.core.util.ProgressListener)
    */
   public Metadata load(String sURI, Properties properties, int nFlags, ProgressListener progress)
      throws MetadataException
   {
      if (properties == null) // use system properties
      {
         properties = SysUtil.getConfigProperties();
      }

      String sLoaderClassName = properties.getProperty(METADATA_LOADER_PROPERTY, DEFAULT_METADATA_LOADER);

      try
      {
         m_loader = (MetadataLoader)Class.forName(sLoaderClassName).newInstance();
      }
      catch (Exception e)
      {
         s_logger.fatal("Cannot start the metadata loader \"" + sLoaderClassName + '"', e);

         throw new MetadataException("err.meta.loaderClass",
            new Object[]{sLoaderClassName}, e);
      }

      if (s_logger.isInfoEnabled())
      {
         s_logger.info("Using metadata loader \"" + m_loader.getClass().getName() + '"');
      }

      if (sURI == null) // retrieve URI from supplied properties
      {
         sURI = properties.getProperty(METADATA_URL_PROPERTY, DEFAULT_METADATA_URL);
      }

      try
      {
         Metadata metadata = m_loader.load(sURI, properties, nFlags, progress);

         if (s_logger.isInfoEnabled())
         {
            s_logger.info("Completed loading metadata from \"" + sURI + '"');
         }

         return metadata;
      }
      catch (Exception e)
      {
         s_logger.fatal("Cannot load metadata from \"" + sURI + '"', e);

         throw new MetadataException("err.meta.load", new Object[]{sURI}, e);
      }
   }

   /**
    * Get the most recently used MetadataLoader.
    * @return the metadata loader.
    */
   public MetadataLoader getLoader()
   {
      return m_loader;
   }
}
