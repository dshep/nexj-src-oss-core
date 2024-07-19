// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.util.StringTable;
import nexj.core.util.TZ;

/**
 * Singleton providing access to the shared metadata.
 * The following configuration file properties
 * affect the repository initialization:
 * meta.loader=[Metadata loader class name (nexj.core.meta.xml.XMLMetadataLoader)]
 * meta.url=[Metadata location URL (/nexj/meta)]
 */
public final class Repository
{
   // attributes

   /**
    * True if the repository has been initialized; false otherwise.
    */
   private static boolean s_bInitialized;

   /**
    * True if the shared metadata are loaded; false otherwise.
    */
   private volatile static boolean s_bLoaded;

   // associations

   /**
    * The shared metadata instance.
    */
   private static Metadata s_metadata;

   // operations

   /**
    * Sets the shared metadata instance. FOR INTERNAL USE ONLY!
    * @param metadata The shared metadata instance.
    */
   public synchronized final static void setMetadata(Metadata metadata)
   {
      s_metadata = metadata;

      if (!s_bInitialized)
      {
         StringTable.setInstance(s_metadata.getStringTable(Metadata.DEFAULT_LOCALE));
         StringTable.setTimeZone(TZ.UTC);
      }

      s_bInitialized = true;
      s_bLoaded = s_metadata != null;
   }

   /**
    * Returns the shared metadata instance.
    * @return The shared metadata instance.
    */
   public synchronized final static Metadata getMetadata()
   {
      if (!s_bInitialized)
      {
         setMetadata(new MetadataLoaderDispatcher().load());
      }
      else if (s_metadata == null)
      {
         throw new IllegalStateException("Metadata not loaded");
      }

      return s_metadata;
   }

   /**
    * Returns whether or not the repository has been loaded. Does not block.
    * @return True if the repository is loaded; false otherwise.
    */
   public final static boolean isLoaded()
   {
      return s_bLoaded;
   }

   /**
    * Releases the metadata to avoid memory leaks in buggy J2EE class loaders.
    * May be called only once during application shutdown.
    */
   public final static void unload()
   {
      setMetadata(null);
   }
}
