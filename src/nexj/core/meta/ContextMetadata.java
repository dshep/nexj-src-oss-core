// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.Iterator;
import java.util.Locale;

import nexj.core.scripting.GlobalEnvironment;
import nexj.core.util.StringTable;

/**
 * Basic context metadata. 
 */
public interface ContextMetadata
{
   // constants

   /**
    * The default locale.
    */
   public final static String DEFAULT_LOCALE = "en";
   
   // operations

   /**
    * @return The repository name.
    */
   String getName();

   /**
    * @return The repository revision.
    */
   String getRevision();

   /**
    * @return The metadata namespace.
    */
   String getNamespace();

   /**
    * @return The metadata version.
    */
   String getVersion();

   /**
    * @return The metadata checksum.
    */
   String getChecksum();

   /**
    * @return The base metadata namespace. Can be null.
    */
   String getBaseNamespace();

   /**
    * @return The base metadata version.
    */
   String getBaseVersion();

   /**
    * @return The base metadata checksum.
    */
   String getBaseChecksum();

   /**
    * @return The read-only global scripting environment.
    */
   GlobalEnvironment getGlobalEnvironment();

   /**
    * Determines if an exact locale variant is supported.
    * @param sLocale The locale variant name.
    * @return True if the locale variant is supported.
    * It might return false, even though the variant base is supported,
    * e.g. false for "en_US", true for "en".  
    */
   boolean isLocaleSupported(String sLocale);

   /**
    * Gets a supported locale name from a locale variant.
    * @param sLocale The locale variant name.
    * @return The supported locale name (might be a folded version of sLocale).
    */
   String getLocaleName(String sLocale);

   /**
    * Gets a supported locale from a locale variant.
    * @param sLocale The locale variant name.
    * @return The supported locale.
    */
   Locale getLocale(String sLocale);
   
   /**
    * @return An iterator over the supported locales.
    */
   Iterator getLocaleIterator();

   /**
    * @return The supported locale count.
    */
   int getLocaleCount();

   /**
    * Gets the string table for the specified locale.
    * @param sLocale The locale name.
    * @return The string table.
    * @throws MetadataException if the default string resource is not found.
    */
   StringTable getStringTable(String sLocale);
}
