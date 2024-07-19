// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Locale;

/**
 * Locale-related utility functions.
 */
public class LocaleUtil
{
   /**
    * The variant separation character.
    */
   public final static char SEPARATOR = '_';

   // constructors

   /**
    * Prevents construction
    */
   protected LocaleUtil()
   {
   }

   // operations

   /**
    * Parses a string and creates a locale from it.
    * @param sLocale The string to parse, the variants are sperated with _.
    * @return The locale object.
    */
   public static Locale parse(String sLocale)
   {
      int i = sLocale.indexOf(SEPARATOR);
      int k = sLocale.indexOf(SEPARATOR, i + 1);

      if (i < 0)
      {
         return new Locale(sLocale);
      }
      else if (k < 0)
      {
         return new Locale(sLocale.substring(0, i), sLocale.substring(i + 1));
      }
      else
      {
         return new Locale(sLocale.substring(0, i), sLocale.substring(i + 1, k), sLocale.substring(k + 1));
      }
   }

   /**
    * Gets the base locale name of a given locale name.
    * @param sLocale The locale name, the variants are separated with _.
    * @param sDefault The default locale name, if the locale name has no variants.
    */
   public static String getBase(String sLocale, String sDefault)
   {
      if (sLocale != null)
      {
         int i = sLocale.lastIndexOf(SEPARATOR);

         if (i >= 0)
         {
            return sLocale.substring(0, i);
         }
      }

      return sDefault;
   }

   /**
    * Determines if a locale is a base of another locale.
    * @param sLocale The locale name to test.
    * @param sVariant The variant (derived) locale.
    * @return True if sLocale is a base of sVariant.
    */
   public static boolean isBaseOf(String sLocale, String sVariant)
   {
      return sVariant.startsWith(sLocale) &&
         (sLocale.length() == sVariant.length() ||
            sVariant.charAt(sLocale.length()) == SEPARATOR);
   }
}
