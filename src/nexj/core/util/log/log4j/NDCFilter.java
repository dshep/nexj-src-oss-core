// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.log.log4j;

import java.util.regex.Pattern;

import org.apache.log4j.Level;
import org.apache.log4j.spi.Filter;
import org.apache.log4j.spi.LoggingEvent;

/**
 * Custom log4j filter that, for a specific category, accepts only those messages of a certain
 * level (or higher) or containing a fixed string in their nested diagnostic context.
 */
public class NDCFilter extends Filter
{
   /**
    * The category to filter (this filter is neutral to all other categories).
    */
   protected String m_sCategory;

   /**
    * The level at which to accept any event in the filter category.
    */
   protected Level m_level;
   
   /**
    * The string regular expression to search for in NDCs of events that are too detailed to be accepted based on level.
    */
   protected Pattern m_pattern;
   
   /**
    * The deny unaccepted events from filter category flag.
    */
   protected boolean m_bRequired;

   /**
    * Sets the category to filter (this filter is neutral to all other categories).
    * @param sCategory The category to filter (this filter is neutral to all other categories) to set.
    */
   public void setCategory(String sCategory)
   {
      m_sCategory = sCategory;
   }

   /**
    * @return The category to filter (this filter is neutral to all other categories).
    */
   public String getCategory()
   {
      return m_sCategory;
   }

   /**
    * Sets the level at which to accept any message in the filter category.
    * @param sLevel The level at which to accept any message in the filter category to set.
    */
   public void setLevel(String sLevel)
   {
      m_level = Level.toLevel(sLevel);
   }

   /**
    * @return The level at which to accept any message in the filter category.
    */
   public String getLevel()
   {
      return m_level.toString();
   }
   
   /**
    * Sets the string to search for in NDCs of events that are too detailed to be accepted based on level.
    * @param sPattern The string to search for in NDCs of events that are too detailed to be accepted based on level to set.
    */
   public void setPattern(String sPattern)
   {
      m_pattern = Pattern.compile(sPattern);
   }

   /**
    * @return The string to search for in NDCs of events that are too detailed to be accepted based on level.
    */
   public String getPattern()
   {
      return m_pattern.pattern();
   }

   /**
    * Sets the deny unaccepted events from filter category flag.
    * @param bRequired The deny unaccepted events from filter category flag to set.
    */
   public void setRequired(boolean bRequired)
   {
      m_bRequired = bRequired;
   }

   /**
    * @return The deny unaccepted events from filter category flag.
    */
   public boolean isRequired()
   {
      return m_bRequired;
   }
   
   /**
    * @see org.apache.log4j.spi.Filter#decide(org.apache.log4j.spi.LoggingEvent)
    */
   public int decide(LoggingEvent event)
   {
      String sLoggerName = event.getLoggerName();
      
      if (sLoggerName.startsWith(m_sCategory)
         && (sLoggerName.length() == m_sCategory.length()
            || sLoggerName.charAt(m_sCategory.length()) == '.'))
      {
         if (event.getLevel().isGreaterOrEqual(m_level))
         {
            return ACCEPT;
         }
         else
         {
            if (event.getNDC() != null && m_pattern.matcher(event.getNDC()).find())
            {
               return ACCEPT;
            }
            else
            {
               return (m_bRequired) ? DENY : NEUTRAL;
            }
         }
      }
      else
      {
         return NEUTRAL;
      }
   }
}
