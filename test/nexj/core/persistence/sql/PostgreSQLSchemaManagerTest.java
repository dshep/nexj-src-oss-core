// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import nexj.core.meta.persistence.sql.SQLSubstReader;

public class PostgreSQLSchemaManagerTest extends SQLSchemaManagerTest
{

   public PostgreSQLSchemaManagerTest(String sName)
   {
      super(sName, new PostgreSQLAdapterTest(null));
   }

   public void testAppendConcatenate()
   {
      StringBuffer buf = new StringBuffer();

      m_manager.appendConcatenate(buf, null);
      assertEquals("", buf.toString());

      buf.setLength(0);
      m_manager.appendConcatenate(buf, new String[0]);
      assertEquals("", buf.toString());

      buf.setLength(0);
      m_manager.appendConcatenate(buf, new String[] { "abc" });
      assertEquals("abc", buf.toString());

      buf.setLength(0);
      m_manager.appendConcatenate(buf, new String[] { "abc", "def" });
      assertEquals("(abc || def)", buf.toString());

      buf.setLength(0);
      m_manager.appendConcatenate(buf, new String[] { "abc", "def", "ghi" });
      assertEquals("(abc || def || ghi)", buf.toString());
   }

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
      assertEquals("extract(microseconds from X)", buf.toString());

   }

   public void testAppendTSIncrement()
   {
      StringBuffer buf = new StringBuffer();

      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_YEAR);
      assertEquals("X + interval 'Y year'", buf.toString());

      buf.setLength(0);
      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_QUARTER);
      assertEquals("X + (Y * interval '3 month')", buf.toString());

      buf.setLength(0);
      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_MONTH);
      assertEquals("X + interval 'Y month'", buf.toString());

      buf.setLength(0);
      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_WEEK);
      assertEquals("X + interval 'Y week'", buf.toString());

      buf.setLength(0);
      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_DAY);
      assertEquals("X + interval 'Y day'", buf.toString());

      buf.setLength(0);
      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_HOUR);
      assertEquals("X + interval 'Y hour'", buf.toString());

      buf.setLength(0);
      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_MIN);
      assertEquals("X + interval 'Y minute'", buf.toString());

      buf.setLength(0);
      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_SEC);
      assertEquals("X + interval 'Y second'", buf.toString());

      buf.setLength(0);
      m_manager.appendTSIncrement(buf, "X", "Y", SQLSubstReader.TS_USEC);
      assertEquals("X + interval 'Y microseconds'", buf.toString());
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManagerTest#testAlterTableStep()
    */
   public void testAlterTableStep()
   {
      if (!((PostgreSQLAdapter)m_adapter).isClouded())
      {
         super.testAlterTableStep();
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManagerTest#testApplyIndexAspectStep()
    */
   public void testApplyIndexAspectStep()
   {
      if (!((PostgreSQLAdapter)m_adapter).isClouded())
      {
         super.testApplyIndexAspectStep();
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManagerTest#testCreateIndexStep()
    */
   public void testCreateIndexStep()
   {
      if (!((PostgreSQLAdapter)m_adapter).isClouded())
      {
         super.testCreateIndexStep();
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManagerTest#testDropIndexStep()
    */
   public void testDropIndexStep()
   {
      if (!((PostgreSQLAdapter)m_adapter).isClouded())
      {
         super.testDropIndexStep();
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManagerTest#testRemoveIndexAspectStep()
    */
   public void testRemoveIndexAspectStep()
   {
      if (!((PostgreSQLAdapter)m_adapter).isClouded())
      {
         super.testRemoveIndexAspectStep();
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManagerTest#testRenameIndexStep()
    */
   public void testRenameIndexStep()
   {
      if (!((PostgreSQLAdapter)m_adapter).isClouded())
      {
         super.testRenameIndexStep();
      }
   }
}