// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.Format;
import java.text.MessageFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;
import java.util.regex.Pattern;


/**
 * String table with hierarchical lookup.
 */
public class StringTable
{
   // associations

   /**
    * The hash map containing the strings: String[String].
    */
   protected Lookup m_stringMap;

   /**
    * The default string table to use if the string is not found.
    */
   protected StringTable m_defaultTable;

   /**
    * The singleton string table for message formatting, may be null.
    */
   private static StringTable s_instance;

   /**
    * The default time zone for message formatting.
    */
   private static TimeZone s_timeZone;
      
   /**
    * The table locale.
    */
   protected Locale m_locale;
   
   /**
    * The hash map containing localized format patterns: String[Format].
    */
   protected Lookup m_patternMap;

   /**
    * Empty object array.
    */
   protected final static Object[] EMPTY_ARRAY = new Object[0];

   /**
    * Used to replace single quote with double single quotes to handle French.
    */
   private final static Pattern s_singleQuotePattern = Pattern.compile("'");
   
   /**
    * Used to escape non-arguments in the format strings with single quotes.
    */
   private final static Pattern s_argumentPattern = Pattern.compile("\\{([^\\{\\d]+)\\}");
   

   // constructors

   /**
    * Creates a string table.
    * @param defaultTable The default table to use if the string is not found. Can be null.
    * @param locale The locale of the strings contained in the string table.
    */
   public StringTable(StringTable defaultTable, Locale locale)
   {
      m_stringMap = new HashTab();
      m_defaultTable = defaultTable;
      m_locale = locale;
      
      initPatternMap();
   }
   
   /**
    * Creates a string table.
    * @param initMap The map to copy into this string table.
    * @param defaultTable The default table to use if the string is not found. Can be null.
    * @param locale The locale of the strings contained in the string table.
    */
   public StringTable(Map initMap, StringTable defaultTable, Locale locale)
   {
      m_stringMap = new HashTab(initMap.size());
      
      for (Iterator itr = initMap.entrySet().iterator(); itr.hasNext();)
      {
         Map.Entry entry = (Map.Entry)itr.next();
         
         m_stringMap.put(entry.getKey(), entry.getValue());
      }

      m_defaultTable = defaultTable;
      m_locale = locale;
      
      initPatternMap();
   }
      
   // operations
   
   /**
    * Sets the string table for message formatting.
    * @param stringTable The string table for message formatting to set, may be null.
    */
   public synchronized static void setInstance(StringTable instance)
   {
      s_instance = instance;
   }

   /**
    * @return The string table for message fomatting, may be null.
    */
   public synchronized static StringTable getInstance()
   {
      return s_instance;
   }

   /**
    * Sets the time zone for message formatting.
    * @param timeZone The time zone for message formatting to set, may be null.
    */
   public synchronized static void setTimeZone(TimeZone timeZone)
   {
      s_timeZone = timeZone;
   }

   /**
    * @return The time zone for message formatting, may be null.
    */
   public synchronized static TimeZone getTimeZone()
   {
      return s_timeZone;
   }

   /**
    * @return The default table.
    */
   public StringTable getDefaultTable()
   {
      return m_defaultTable;
   }
   
   /**
    * @return The table locale.
    */
   public Locale getLocale()
   {
      return m_locale;
   }

   /**
    * @return String map iterator.
    */
   public Lookup.Iterator getIterator()
   {
      return m_stringMap.iterator();
   }

   /**
    * Looks up the string first in this table, then in the default one.
    * @param sName The string name.
    * @return The found string, or sName if not found.
    */
   public String get(String sName)
   {
      if (sName == null)
      {
         return null;
      }

      Object s = m_stringMap.get(sName);

      if (s == null && m_defaultTable != null)
      {
         s = m_defaultTable.get(sName);
      }

      if (s == null)
      {
         return sName;
      }

      return (String)s;
   }

   /**
    * Formats a string according to the invocation context locale and a given time zone.
    * @param sName The string name. It must correspond to a string table
    * entry specifying a java.text.MessageFormat pattern. 
    * @param args The argument array. Can be null.
    * @param timeZone The time zone to be used when formatting timestamp args.
    * Null to use the system default.
    * @return The formatted string.
    */
   public String format(String sName, Object[] args, TimeZone timeZone)
   {
      String sValue = get(sName);
      
      if (sValue != null && sValue.indexOf("'") >= 0)
      {
         sValue = s_singleQuotePattern.matcher(sValue).replaceAll("''");
         sValue = s_argumentPattern.matcher(sValue).replaceAll("'{$1}'");
      }
      
      MessageFormat msgFormat = new MessageFormat(sValue, m_locale);

      if (args == null)
      {
         args = EMPTY_ARRAY;
      }
      else
      {
         boolean bDuplicated = false;
         Format[] formatArray = null;

         for (int i = 0; i < args.length; ++i)
         {
            Object arg = args[i];
            
            if (arg instanceof StringId ||
               arg instanceof Captioned ||
               arg instanceof Named)
            {
               if (!bDuplicated)
               {
                  Object[] dupArgs = new Object[args.length];
   
                  System.arraycopy(args, 0, dupArgs, 0, args.length);
                  args = dupArgs;
                  bDuplicated = true;
               }
               
               args[i] = toString(arg);
            }
            else if (timeZone != null && arg instanceof Date)
            {
               if (formatArray == null)
               {
                  formatArray = msgFormat.getFormatsByArgumentIndex();
               }

               if (i < formatArray.length && formatArray[i] == null)
               {
                  DateFormat fmt = DateFormat.getDateTimeInstance(
                     DateFormat.SHORT, DateFormat.SHORT, m_locale);

                  fmt.setTimeZone(timeZone);

                  // This will work only if an argument is used not more than once.
                  // Unfortunately, MessageFormat does not expose
                  // the argument indexes in any useful way.
                  msgFormat.setFormatByArgumentIndex(i, fmt);
               }
            }
         }

         formatArray = msgFormat.getFormats();

         for (int i = 0; i < formatArray.length; i++)
         {
            Format fmt = formatArray[i];

            applyLocalePattern(fmt);
            
            if (timeZone != null)
            {
               if (fmt instanceof DateFormat)
               {
                  ((DateFormat)fmt).setTimeZone(timeZone);
               }
            }
         }
      }

      return msgFormat.format(args, new StringBuffer(128), null).toString();
   }
   
   /**
    * The format is mapped to a corresponding locale pattern.  If a pattern exists, it is applied to the format.
    * @param format The format obtained from MessageFormat.
    */
   private void applyLocalePattern(Format format)
   {
      if (format != null && m_patternMap != null)
      {
         String sPattern = (String)m_patternMap.get(format);

         if (sPattern != null)
         {
            if (format instanceof DecimalFormat)
            {
               ((DecimalFormat)format).applyPattern(sPattern);
            }
            else if (format instanceof SimpleDateFormat)
            {
               ((SimpleDateFormat)format).applyPattern(sPattern);
            }
         }
      }
   }
   
   /**
    * Initialize the pattern map with customized format patterns.
    */
   protected void initPatternMap()
   {
      addPattern(Localization.INTEGER_FORMATID, NumberFormat.getIntegerInstance(m_locale));
      addPattern(Localization.PERCENT_FORMATID, NumberFormat.getPercentInstance(m_locale));
      addPattern(Localization.FULLTIME_FORMATID, DateFormat.getTimeInstance(DateFormat.FULL, m_locale));
      addPattern(Localization.LONGTIME_FORMATID, DateFormat.getTimeInstance(DateFormat.LONG, m_locale));
      addPattern(Localization.MEDIUMTIME_FORMATID, DateFormat.getTimeInstance(DateFormat.DEFAULT, m_locale));
      addPattern(Localization.SHORTTIME_FORMATID, DateFormat.getTimeInstance(DateFormat.SHORT, m_locale));
      addPattern(Localization.FULLDATE_FORMATID, DateFormat.getDateInstance(DateFormat.FULL, m_locale));
      addPattern(Localization.LONGDATE_FORMATID, DateFormat.getDateInstance(DateFormat.LONG, m_locale));
      addPattern(Localization.MEDIUMDATE_FORMATID, DateFormat.getDateInstance(DateFormat.DEFAULT, m_locale));
      addPattern(Localization.SHORTDATE_FORMATID, DateFormat.getDateInstance(DateFormat.SHORT, m_locale));
   }

   /**
    * Stores a customized format pattern for a default format.
    * @param sId The locale string id.
    * @param format The default format.
    */
   protected void addPattern(String sId, Format format) 
   {
      Object pattern = m_stringMap.get(sId);

      if (pattern == null && m_defaultTable != null)
      {
         pattern = m_defaultTable.get(sId);
         
         if (pattern == sId)
         {
            return;
         }
      }
      
      if (pattern != null)
      {
         if (m_patternMap == null)
         {
            m_patternMap = new HashTab(10);
         }

         m_patternMap.put(format, pattern);
      }
   }
   
   /**
    * Converts an object to string, taking into account its localization capabilities.
    * @param obj The object to convert.
    */
   public String toString(Object obj)
   {
      if (obj == null)
      {
         return "";
      }
      
      if (obj instanceof StringId)
      {
         return get(obj.toString());
      }
      
      if (obj instanceof Captioned)
      {
         return get(((Captioned)obj).getCaption());
      }
      
      if (obj instanceof Named)
      {
         return ((Named)obj).getName();
      }
      
      return obj.toString();
   }
}
