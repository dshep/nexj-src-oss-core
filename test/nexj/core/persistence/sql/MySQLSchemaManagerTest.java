// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.Timestamp;

import nexj.core.meta.persistence.sql.SQLSubstReader;

/**
 * Base test case for SQLSchemaManager with MySQL customizations.
 */
public class MySQLSchemaManagerTest extends SQLSchemaManagerTest
{
   public MySQLSchemaManagerTest(String sName)
   {
      super(sName, new MySQLAdapterTest(null));
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManagerTest#formatTimestamp(java.sql.Timestamp)
    */
   protected String formatTimestamp(Timestamp ts)
   {
      // MySQL requires strings to be in a specific format for conversion to work
      return MySQLAdapter.TIMESTAMP_FORMAT.format(ts);
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManagerTest#generatePrimaryKeyName(java.lang.String)
    */
   protected String getPrimaryKeyName(String sIndex)
   {
      // MySQL doesn't store index name for Primary Key, it's just called "PRIMARY"
      return "PRIMARY";
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManagerTest#testAppendConcatenate()
    */
   public void testAppendConcatenate()
   {
      StringBuffer buf = new StringBuffer();

      m_manager.appendConcatenate(buf, null);
      assertEquals("", buf.toString());

      buf.setLength(0);
      m_manager.appendConcatenate(buf, new String[0]);
      assertEquals("", buf.toString());

      buf.setLength(0);
      m_manager.appendConcatenate(buf, new String[]{"abc"});
      assertEquals("abc", buf.toString());

      buf.setLength(0);
      m_manager.appendConcatenate(buf, new String[]{"abc","def"});
      assertEquals("concat(abc, def)", buf.toString());

      buf.setLength(0);
      m_manager.appendConcatenate(buf, new String[]{"abc","def","ghi"});
      assertEquals("concat(abc, def, ghi)", buf.toString());
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManagerTest#testAppendTSExtract()
    */
   public void testAppendTSExtract()
   {
      StringBuffer buf = new StringBuffer();

      m_manager.appendTSExtract(buf, "X", SQLSubstReader.TS_YEAR);
      assertEquals("extract(year from X)", buf.toString());

      buf.setLength(0);
      m_manager.appendTSExtract(buf, "X", SQLSubstReader.TS_QUARTER);
      assertEquals("extract(quarter from X)", buf.toString());

      buf.setLength(0);
      m_manager.appendTSExtract(buf, "X", SQLSubstReader.TS_MONTH);
      assertEquals("extract(month from X)", buf.toString());

      buf.setLength(0);
      m_manager.appendTSExtract(buf, "X", SQLSubstReader.TS_WEEK);
      assertEquals("extract(week from X)", buf.toString());

      buf.setLength(0);
      m_manager.appendTSExtract(buf, "X", SQLSubstReader.TS_DAY);
      assertEquals("extract(day from X)", buf.toString());

      buf.setLength(0);
      m_manager.appendTSExtract(buf, "X", SQLSubstReader.TS_HOUR);
      assertEquals("extract(hour from X)", buf.toString());

      buf.setLength(0);
      m_manager.appendTSExtract(buf, "X", SQLSubstReader.TS_MIN);
      assertEquals("extract(minute from X)", buf.toString());

      buf.setLength(0);
      m_manager.appendTSExtract(buf, "X", SQLSubstReader.TS_SEC);
      assertEquals("extract(second from X)", buf.toString());

      buf.setLength(0);
      m_manager.appendTSExtract(buf, "X", SQLSubstReader.TS_USEC);
      assertEquals("extract(microsecond from X)", buf.toString());
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManagerTest#testAppendTSIncrement()
    */
   public void testAppendTSIncrement()
   {
      StringBuffer buf = new StringBuffer();

      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_YEAR);
      assertEquals("timestampadd(year, Y, X)", buf.toString());

      buf.setLength(0);
      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_QUARTER);
      assertEquals("timestampadd(quarter, Y, X)", buf.toString());

      buf.setLength(0);
      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_MONTH);
      assertEquals("timestampadd(month, Y, X)", buf.toString());

      buf.setLength(0);
      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_WEEK);
      assertEquals("timestampadd(week, Y, X)", buf.toString());

      buf.setLength(0);
      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_DAY);
      assertEquals("timestampadd(day, Y, X)", buf.toString());

      buf.setLength(0);
      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_HOUR);
      assertEquals("timestampadd(hour, Y, X)", buf.toString());

      buf.setLength(0);
      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_MIN);
      assertEquals("timestampadd(minute, Y, X)", buf.toString());

      buf.setLength(0);
      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_SEC);
      assertEquals("timestampadd(second, Y, X)", buf.toString());

      buf.setLength(0);
      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_USEC);
      assertEquals("timestampadd(microsecond, Y, X)", buf.toString());
   }
}