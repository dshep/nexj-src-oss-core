// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.net.URL;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.Iterator;

import nexj.core.admin.etl.sql.SQLUtil;
import nexj.core.meta.Component;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.DataSourceAdapter;
import nexj.core.meta.persistence.DataSourceType;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.IndexColumn;
import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalDatabaseFragment;
import nexj.core.meta.persistence.sql.RelationalObject;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.RelationalSchemaTest;
import nexj.core.meta.persistence.sql.SQLScript;
import nexj.core.meta.persistence.sql.SQLStatement;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.meta.persistence.sql.upgrade.AlterColumnStep;
import nexj.core.meta.persistence.sql.upgrade.AlterTableStep;
import nexj.core.meta.persistence.sql.upgrade.ApplyIndexAspectStep;
import nexj.core.meta.persistence.sql.upgrade.ApplyTableAspectStep;
import nexj.core.meta.persistence.sql.upgrade.ColumnOutline;
import nexj.core.meta.persistence.sql.upgrade.CreateColumnStep;
import nexj.core.meta.persistence.sql.upgrade.CreateIndexStep;
import nexj.core.meta.persistence.sql.upgrade.CreateTableStep;
import nexj.core.meta.persistence.sql.upgrade.DropColumnStep;
import nexj.core.meta.persistence.sql.upgrade.DropIndexStep;
import nexj.core.meta.persistence.sql.upgrade.DropTableStep;
import nexj.core.meta.persistence.sql.upgrade.ExecStep;
import nexj.core.meta.persistence.sql.upgrade.IndexOutline;
import nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgrade;
import nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState;
import nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep;
import nexj.core.meta.persistence.sql.upgrade.RemoveIndexAspectStep;
import nexj.core.meta.persistence.sql.upgrade.RemoveTableAspectStep;
import nexj.core.meta.persistence.sql.upgrade.RenameColumnStep;
import nexj.core.meta.persistence.sql.upgrade.RenameIndexStep;
import nexj.core.meta.persistence.sql.upgrade.RenameTableStep;
import nexj.core.meta.persistence.sql.upgrade.SupportAdapterStep;
import nexj.core.meta.upgrade.LoadUpgrade;
import nexj.core.meta.upgrade.Upgrade;
import nexj.core.meta.upgrade.VersionUpgrade;
import nexj.core.meta.xml.XMLMetadata;
import nexj.core.persistence.SchemaVersion;
import nexj.core.persistence.sql.SQLSchemaManager.SQLAppender;
import nexj.core.rpc.TransferObject;
import nexj.core.util.Binary;
import nexj.core.util.IOUtil;
import nexj.core.util.LookupException;
import nexj.core.util.PropertyMap;
import nexj.core.util.UncheckedException;
import nexj.test.util.AssertUtil;

/**
 * Base test case for SQLSchemaManager.
 */
public abstract class SQLSchemaManagerTest extends SQLDataTest
{
   /**
    * The adapter test suite that this schema manager test suite gets its meta information from.
    */
   protected SQLAdapterTest m_adapterTest;

   /**
    * Active DB connection to be used by tests.
    */
   protected SQLConnection m_connection;

   /**
    * The schema manager for the persistence adapter.
    */
   protected SQLSchemaManager m_manager;

   /**
    * Step for creating a single column table used in some tests.
    */
   protected CreateTableStep m_doubleColTableStep;

   /**
    * Step for creating a single column table used in some tests.
    */
   protected CreateTableStep m_singleColTableStep;

   /**
    * Step for creating a single column table with an Index used in some tests.
    */
   protected CreateTableStep m_singleIdxColTableStep;

   /**
    * Step for creating a single column table with Primary Key used in some tests.
    */
   protected CreateTableStep m_singlePKColTableStep;

   /**
    * Constructor.
    * @param sName The name of the test.
    * @param adapterTest The adapter test object containing the relevant metadata.
    */
   public SQLSchemaManagerTest(String sName, SQLAdapterTest adapterTest)
   {
      super(sName);
      m_adapterTest = adapterTest;
   }

   /**
    * @see nexj.core.persistence.sql.SQLDataTest#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      m_manager = m_adapter.createSchemaManager(m_database);
      m_connection = m_adapter.getConnection();
      m_manager.setOwner("test"); // same as in lock() during schema creation
      m_manager.setConnection(m_connection.getConnection());

      SQLUtil.execute(m_connection.getConnection(), getSQLScript(ST_DROP));
      m_doubleColTableStep = new CreateTableStep();
      m_singleColTableStep = new CreateTableStep();
      m_singleIdxColTableStep = new CreateTableStep();
      m_singlePKColTableStep = new CreateTableStep();
      m_doubleColTableStep.setName(((RelationalSchema)m_database.getSchema()).getPrefix() + "a");
      m_singleColTableStep.setName(m_doubleColTableStep.getName());   // readSchema() returns
      m_singleIdxColTableStep.setName(m_doubleColTableStep.getName()); // prefixed table names
      m_singlePKColTableStep.setName(m_doubleColTableStep.getName());

      ColumnOutline column = new ColumnOutline("id");
      column.setType(Primitive.INTEGER);
      column.setNullable(Boolean.FALSE);

      m_doubleColTableStep.addColumnOutline(column);
      m_singleColTableStep.addColumnOutline(column);
      m_singleIdxColTableStep.addColumnOutline(column);
      m_singlePKColTableStep.addColumnOutline(column);

      column = new ColumnOutline("value");
      column.setType(Primitive.STRING);
      column.setPrecision(256);
      m_doubleColTableStep.addColumnOutline(column);

      IndexOutline index = new IndexOutline();

      index.setName(m_manager.generateIndexName("a", "ind0", null));
      index.addColumn("id", true);
      m_singleIdxColTableStep.addIndexOutline(index);

      index = new IndexOutline();
      index.setName(m_manager.generateIndexName("a", getPrimaryKeyName("pk"), null));
      index.addColumn("id", true);
      m_singlePKColTableStep.addIndexOutline(index);
      m_singlePKColTableStep.setPrimaryKeyName(index.getName());
   }

   /**
    * @see nexj.core.persistence.sql.SQLDataTest#tearDown()
    */
   protected void tearDown() throws Exception
   {
      m_manager.setConnection(null);

      try
      {
         if (m_connection != null)
         {
            m_connection.decRef();
         }
      }
      finally
      {
         super.tearDown();
      }
   }

   /**
    * Assert actual type (read from DB) is equivalent to the target type (expected to be in DB).
    * @param target The expected column type in DB.
    * @param actual The actual column type read from DB.
    */
   protected void assertType(Primitive target, Primitive actual)
   {
      assertEquals(target, actual);
   }

   /**
    * Convert a column 'from' a specific type 'to' a specific type.
    * @param from The type of initial column.
    * @param bFromLOB The allocation of the initial column is LOB (if possible).
    * @param to The type of new column.
    * @param bToLOB The allocation of the new column is LOB (if possible).
    * @param seed The seed value to insert prior to conversion.
    * @return The converted seed value.
    */
   protected Object convertColumnType(
      Primitive from, boolean bFromLOB, Primitive to, boolean bToLOB, Object seed) throws Exception
   {
      m_doubleColTableStep.getColumnOutline("value").setType(from);
      m_doubleColTableStep.getColumnOutline("value")
         .setAllocation((bFromLOB) ? Column.LOCATOR : Column.VARYING);
      m_doubleColTableStep.getColumnOutline("value").setNullable(Boolean.FALSE);

      if (from == Primitive.BINARY || from == Primitive.STRING)
      {
         m_doubleColTableStep.getColumnOutline("value").setPrecision(100);
         m_doubleColTableStep.getColumnOutline("value").setCaseInsensitive(Boolean.FALSE);
      }

      RelationalSchema schema = upgrade(m_doubleColTableStep, null, null);
      Table table = schema.getTable(m_doubleColTableStep.getName());

      assertNotNull(table);
      assertEquals(2, table.getColumnCount());
      table.setType(Table.MANAGED); // comes in as Table.EXTERNAL from DB

      Column column0 = table.getColumn(0);
      Column column1 = table.getColumn(1);

      assertType(Primitive.INTEGER, column0.getType());
      assertFalse(column0.isNullable());
      assertType(from, column1.getType());
      assertFalse(column1.isNullable());
      column1.setType(from); // reset to proper type since this will be required further in test

      StringBuffer buf = new StringBuffer(128);
      ExecStep exec = new ExecStep();
      SQLScript script = new SQLScript();
      SQLStatement insert = new SQLStatement();

      buf.append("insert into ");
      m_adapter.appendTable(buf, table);
      buf.append(" (");
      m_adapter.appendColumn(buf, column0);
      buf.append(", ");
      m_adapter.appendColumn(buf, column1);
      buf.append(") values (1, ");
      m_adapter.appendLiteral(buf, column1, seed);
      buf.append(")");
      insert.setSQL(buf.toString()); // ensure at least one row in table
      script.addStatement(insert);
      exec.getScriptHolder().addScript(script);
      upgrade(exec, null, null); // insert a record into the table to test altering an actual value

      AlterColumnStep step = new AlterColumnStep();
      ColumnOutline outline = new ColumnOutline("value");

      outline.setType(to);

      if (bToLOB)
      {
         outline.setAllocation(Column.LOCATOR);
      }
      else
      {
         outline.setAllocation( // set Column.FIXED/Column.VARYING as per original column
            (column1.getAllocation() == Column.LOCATOR) ? Column.VARYING : column1.getAllocation());
      }

      outline.setNullable(Boolean.FALSE);

      if (to == Primitive.BINARY || to == Primitive.STRING)
      {
         outline.setPrecision(100);
         outline.setCaseInsensitive(Boolean.FALSE);
      }

      step.setOutline(outline);
      step.setTableName(m_doubleColTableStep.getName());

      try
      {
         schema = upgrade(step, schema, null);
      }
      catch (RuntimeException e)
      {
         throw new RuntimeException("Error: alter column from: " + from + " to: " + to, e);
      }

      table = schema.getTable(m_doubleColTableStep.getName());
      assertNotNull(table);
      assertEquals(2, table.getColumnCount());
      column1 = table.getColumn(1);
      assertType(to, column1.getType());
      assertFalse(column1.isNullable());

      buf.setLength(0);
      buf.append("select ");
      m_adapter.appendColumn(buf, column1);
      buf.append(" from ");
      m_adapter.appendTable(buf, table);
      buf.append(" where ");
      m_adapter.appendColumn(buf, column0);
      buf.append(" = 1"); // the value from ExecStep from above
      m_adapter.log(buf);

      PreparedStatement stmt = m_connection.getConnection().prepareStatement(buf.toString());
      ResultSet rs = stmt.executeQuery();

      assertTrue(rs.next());
      seed = m_adapter.getBind(column1).getValue(rs, 0, m_adapter);
      assertFalse(rs.next());
      m_adapter.close(rs);
      m_adapter.cancel(stmt);

      DropTableStep drop = new DropTableStep();

      table.setType(Table.MANAGED); // comes in as Table.EXTERNAL from DB
      drop.setName(m_doubleColTableStep.getName());
      upgrade(drop, schema, null);

      return seed;
   }

   /**
    * Format a timestamp according to the DB literal timestamp representation.
    * @param ts The timestamp to format.
    * @return The unquoted String DB formated representation of the timestamp.
    */
   protected String formatTimestamp(Timestamp ts)
   {
      return Primitive.toString(ts);
   }

   /**
    * @see nexj.core.persistence.sql.SQLDataTest#getAdapterName()
    */
   protected String getAdapterName()
   {
      return (m_adapterTest == null) ? super.getAdapterName() : m_adapterTest.getAdapterName();
   }

   /**
    * @see nexj.core.persistence.sql.SQLDataTest#getMetadata()
    */
   protected Metadata getMetadata()
   {
      return (m_adapterTest == null) ? super.getMetadata() : m_adapterTest.getMetadata();
   }

   /**
    * Generate a valid index name for a primary key.
    * @param sIndex The index name to use as primary key.
    */
   protected String getPrimaryKeyName(String sIndex)
   {
      return sIndex;
   }

   /**
    * @see nexj.core.persistence.sql.SQLDataTest#getSQLScript(java.lang.String)
    */
   protected URL getSQLScript(String sType)
   {
      return m_adapterTest.getSQLScript(sType);
   }

   /**
    * Read the RDBMS schema into a new RelationalSchema object.
    * @param template The schema to get configuration from (not modified).
    * @return The new schema state as read from RDBMS.
    */
   protected RelationalSchema readSchema(RelationalSchema template)
   {
      RelationalSchema schema = new RelationalSchema(); // need empty schema for readSchema()
      String sSchema = template.getPrefix(); // determine schema to avoid random tables

      if (sSchema != null)
      {
         int nSchemaSep = sSchema.indexOf('.');
         sSchema = (nSchemaSep < 0) ? null : sSchema.substring(0, nSchemaSep);
      }

      schema.setDataSource(template.getDataSource());
      schema.setIndexFill(template.getIndexFill());
      schema.setIndexspaceName(template.getIndexspaceName());
      schema.setLongspaceName(template.getLongspaceName());
      schema.setPrefix(template.getPrefix());
      schema.setRoleName(template.getRoleName());
      schema.setTablespaceName(template.getTablespaceName());
      m_manager.readSchema(schema, null, sSchema, "_", null, null); //limit to single char table

      return schema;
   }

   public void testAlterColumnStep()
   {
      RelationalSchema schema = upgrade(m_singleColTableStep, null, null);
      Table table = schema.getTable(m_singleColTableStep.getName());

      assertNotNull(table);
      assertEquals(1, table.getColumnCount());
      table.setType(Table.MANAGED); // comes in as Table.EXTERNAL from DB

      Column column = table.getColumn(0);

      assertEquals(Primitive.INTEGER, column.getType());
      assertFalse(column.isNullable());

      AlterColumnStep step = new AlterColumnStep();
      ColumnOutline outline = new ColumnOutline("id");

      outline.setType(Primitive.STRING);
      outline.setNullable(Boolean.TRUE);
      step.setOutline(outline);
      step.setTableName(m_singleColTableStep.getName());
      schema = upgrade(step, schema, null);
      table = schema.getTable(m_singleColTableStep.getName());
      assertNotNull(table);
      assertEquals(1, table.getColumnCount());
      column = table.getColumn(0);
      assertEquals(Primitive.STRING, column.getType());
      assertTrue(column.isNullable());
   }

   public void testAlterTableStep()
   {
      RelationalSchema schema = upgrade(m_singleIdxColTableStep, null, null);
      Table table = schema.getTable(m_singleIdxColTableStep.getName());

      assertNotNull(table);
      assertNull(table.getPrimaryKey());
      table.setType(Table.MANAGED); // comes in as Table.EXTERNAL from DB

      AlterTableStep step = new AlterTableStep();

      step.setName(m_singleIdxColTableStep.getName());
      step.setPrimaryKeyName(table.getIndex(0).getName());
      schema = upgrade(step, schema, null);
      table = schema.getTable(m_singleIdxColTableStep.getName());

      assertNotNull(table);

      Index index = table.getPrimaryKey();

      assertNotNull(index);
      assertEquals(
         m_manager.generateIndexName(table.getTableName(),
                                     getPrimaryKeyName(step.getPrimaryKeyName()).replace('.', '_'),
                                     null),
         index.getName());
      assertEquals(1, index.getIndexColumnCount());

      Column column = index.getIndexColumn(0).getColumn();

      assertEquals("id", column.getName());
      assertEquals(Primitive.INTEGER, column.getType());
   }

   public abstract void testAppendConcatenate();

   public abstract void testAppendTSExtract();

   public abstract void testAppendTSIncrement();

   public void testApplyIndexAspectStep()
   {
      IndexOutline outline = new IndexOutline();

      outline.setName("ind0");
      outline.addColumn("id", true);
      m_doubleColTableStep.addIndexOutline(outline);

      RelationalSchema schema = upgrade(m_doubleColTableStep, null, null);
      Table table = schema.getTable(m_doubleColTableStep.getName());

      assertNotNull(table);
      assertEquals(1, table.getIndexCount());
      assertEquals(1, table.getIndex(0).getIndexColumnCount());
      table.setType(Table.MANAGED); // comes in as Table.EXTERNAL from DB

      Index aspect = new Index("aspect", Index.ASPECT, null);

      aspect.addIndexColumn(new IndexColumn(table.getColumn("value"), true));
      schema.addIndex(aspect);

      ApplyIndexAspectStep step = new ApplyIndexAspectStep();

      step.setAspectName(aspect.getName());
      step.addPointcutPattern(table.getIndex(0).getName(), true);
      schema = upgrade(step, schema, null);
      table = schema.getTable(m_doubleColTableStep.getName());
      assertNotNull(table);
      assertEquals(1, table.getIndexCount());

      Index index = table.getIndex(0);

      assertEquals(2, index.getIndexColumnCount()); // aspect columns always before index columns
      assertEquals("value", index.getIndexColumn(0).getColumn().getName());
   }

   public void testApplyTableAspectStep()
   {
      RelationalSchema schema = upgrade(m_singleColTableStep, null, null);
      Table table = schema.getTable(m_singleColTableStep.getName());

      assertNotNull(table);
      assertEquals(1, table.getColumnCount());
      table.setType(Table.MANAGED); // comes in as Table.EXTERNAL from DB

      Table aspect = new Table(schema);
      Column column = new Column("value", aspect);

      aspect.setName(m_manager.getFullTableName(null, "aspect"));
      aspect.setType(Table.ASPECT);
      column.setType(Primitive.STRING);
      aspect.addColumn(column);

      ApplyTableAspectStep step = new ApplyTableAspectStep();

      step.setAspectName(aspect.getName());
      step.addPointcutPattern(table.getName(), true);
      schema.addTable(aspect);
      schema = upgrade(step, schema, null);
      table = schema.getTable(m_singleColTableStep.getName());
      assertNotNull(table);
      assertEquals(2, table.getColumnCount());
      assertEquals("id", table.getColumn(0).getName());
      assertEquals("value", table.getColumn(1).getName());
   }

   public void testConvertColumnType() throws Exception
   {
      Object[] array = new Object[]
      {
         Primitive.BOOLEAN, Boolean.TRUE,
         Primitive.DECIMAL, new BigDecimal("1.101"),
         Primitive.DOUBLE, new Double(1.101d),
         Primitive.FLOAT, new Float(1.101f),
         Primitive.INTEGER, new Integer(12345),
         Primitive.LONG, new Long(12345),
         Primitive.STRING, new String("12345"),
      };

      for (int i = 0; i < array.length; i += 2)
      {
         for (int j = 0; j < array.length; j += 2)
         {
            convertColumnType((Primitive)array[i], false, (Primitive)array[j], false, array[i + 1]);
         }
      }

      // Binary conversion only supported to/from String
      // at least MSSQL sometimes truncates 0x20 chars from binaries
      Binary bin = new Binary(new byte[]{1,2,3,0x20});

      assertEquals(
         "01020320", convertColumnType(Primitive.BINARY, false, Primitive.STRING, false, bin));
      assertEquals(
         "01020320", convertColumnType(Primitive.BINARY, true, Primitive.STRING, true, bin));
      convertColumnType(Primitive.STRING, false, Primitive.BINARY, false, "abcd");
      convertColumnType(Primitive.STRING, true, Primitive.BINARY, true, "abcd");

      // Timestamp conversion not supported to/from binary/boolean
      Timestamp ts = new Timestamp(Integer.MAX_VALUE); // value that fits into a 32 bit signed int

      array = new Object[]
      {
         Primitive.DECIMAL, new BigDecimal("123"),
         Primitive.DOUBLE, new Double(123d),
         Primitive.FLOAT, new Float(123f),
         Primitive.INTEGER, new Integer(123),
         Primitive.LONG, new Long(123),
         Primitive.STRING, formatTimestamp(ts),
      };

      for (int i = 0; i < array.length; i += 2)
      {
         convertColumnType((Primitive)array[i], false, Primitive.TIMESTAMP, false, array[i + 1]);
         convertColumnType(Primitive.TIMESTAMP, false, (Primitive)array[i], false, ts);
      }
   }

   public void testCreateColumnStep()
   {
      RelationalSchema schema = upgrade(m_singleColTableStep, null, null);
      Table table = schema.getTable(m_singleColTableStep.getName());

      assertNotNull(table);
      assertEquals(1, table.getColumnCount());
      table.setType(Table.MANAGED); // comes in as Table.EXTERNAL from DB

      CreateColumnStep step = new CreateColumnStep();
      ColumnOutline column = new ColumnOutline("value");

      column.setType(Primitive.STRING);
      step.setTableName(m_singleColTableStep.getName());
      step.setOutline(column);
      schema = upgrade(step, schema, null);
      table = schema.getTable(m_singleColTableStep.getName());
      assertNotNull(table);
      assertEquals(2, table.getColumnCount());
      assertEquals("value", table.getColumn(1).getName());
   }

   public void testCreateIndexStep()
   {
      RelationalSchema schema = upgrade(m_singleColTableStep, null, null);
      Table table = schema.getTable(m_singleColTableStep.getName());

      assertNotNull(table);
      assertEquals(0, table.getIndexCount());
      table.setType(Table.MANAGED); // comes in as Table.EXTERNAL from DB

      CreateIndexStep step = new CreateIndexStep();
      IndexOutline outline = new IndexOutline();

      outline.setName("idx01");
      outline.addColumn("id", true);
      step.setTableName(table.getName());
      step.setOutline(outline);
      schema = upgrade(step, schema, null);
      table = schema.getTable(m_singleColTableStep.getName());
      assertNotNull(table);
      assertEquals(1, table.getIndexCount());

      Index index = table.getIndex(0);

      assertEquals(1, index.getIndexColumnCount());
      assertEquals("id", index.getIndexColumn(0).getColumn().getName());
   }

   public void testCreateTableStep()
   {
      RelationalSchema schema = upgrade(m_singlePKColTableStep, null, null);
      Table table = schema.getTable(m_singlePKColTableStep.getName());

      assertNotNull(table);
      assertEquals(1, table.getColumnCount());

      Column column = table.getColumn(0);

      assertEquals("id", column.getName());
      assertEquals(Primitive.INTEGER, column.getType());

      // validate tablespace propagation, cannot use DB since tablespaces not defined there
      schema = new RelationalSchema();
      schema.setDataSource(m_database);
      m_singlePKColTableStep.setIndexspaceName("iSpace");
      m_singlePKColTableStep.setLongspaceName("lSpace");
      m_singlePKColTableStep.setTablespaceName("tSpace");
      m_singlePKColTableStep.apply(new RelationalSchemaUpgradeState(schema, null, null));
      table = schema.getTable(m_singlePKColTableStep.getName());
      assertNotNull(table);
      assertEquals(m_singlePKColTableStep.getIndexspaceName(), table.getIndexspaceName());
      assertEquals(m_singlePKColTableStep.getLongspaceName(), table.getLongspaceName());
      assertEquals(m_singlePKColTableStep.getTablespaceName(), table.getTablespaceName());
   }

   public void testDropColumnStep()
   {
      RelationalSchema schema = upgrade(m_doubleColTableStep, null, null);
      Table table = schema.getTable(m_doubleColTableStep.getName());

      assertNotNull(table);
      assertEquals(2, table.getColumnCount());
      table.setType(Table.MANAGED); // comes in as Table.EXTERNAL from DB

      DropColumnStep step = new DropColumnStep();

      step.setName("value");
      step.setTableName(table.getName());
      schema = upgrade(step, schema, null);
      table = schema.getTable(m_doubleColTableStep.getName());
      assertNotNull(table);
      assertEquals(1, table.getColumnCount());
      assertEquals("id", table.getColumn(0).getName());
   }

   public void testDropIndexStep()
   {
      RelationalSchema schema = upgrade(m_singleIdxColTableStep, null, null);
      Table table = schema.getTable(m_singleIdxColTableStep.getName());

      assertNotNull(table);
      assertEquals(1, table.getIndexCount());
      table.setType(Table.MANAGED); // comes in as Table.EXTERNAL from DB

      DropIndexStep step = new DropIndexStep();

      step.setName(table.getIndex(0).getName());
      schema = upgrade(step, schema, null);
      table = schema.getTable(m_singleIdxColTableStep.getName());
      assertNotNull(table);
      assertEquals(0, table.getIndexCount());
   }

   public void testDropTableStep()
   {
      RelationalSchema schema = upgrade(m_singlePKColTableStep, null, null);
      Table table = schema.getTable(m_singlePKColTableStep.getName());

      assertNotNull(table);
      table.setType(Table.MANAGED); // comes in as Table.EXTERNAL from DB

      DropTableStep step = new DropTableStep();

      step.setName(m_singlePKColTableStep.getName());
      schema = upgrade(step, schema, null);
      assertNull(schema.findTable(m_singlePKColTableStep.getName()));
   }

   public void testDropUpgradeSchema()
   {
      SQLAppender origAppender = m_manager.getSQLAppender();
      RelationalSchema schema = new RelationalSchema(); // schema used for dropTable step
      Table table = new Table(schema);
      StringWriter sql = new StringWriter();
      DropTableStep dropStep = new DropTableStep();
      String sDropSQL;

      table.setName("test.A"); // this table exists in Main.upgrade
      schema.addTable(table);
      schema.setDataSource(m_database);
      dropStep.setName(table.getName());
      m_manager.m_state = new RelationalSchemaUpgradeState(schema, null, null); // required for drop
      m_manager.setSQLAppender(m_manager.new SQLWriterAppender(sql));

      try
      {
         m_manager.dropTable(dropStep);
         sDropSQL = sql.getBuffer().toString();
         sql.getBuffer().setLength(0);
         m_manager.dropSchema((RelationalSchema)m_database.getSchema(), true);
      }
      finally
      {
         m_manager.setSQLAppender(origAppender);
      }

      int nPos = sql.getBuffer().indexOf(sDropSQL);

      assertTrue(nPos >= 0); // at least one occurrence
      assertTrue(sql.getBuffer().indexOf(sDropSQL, nPos + 1) < 0); // no more than one occurrence
   }

   public void testTruncateSchema() throws SQLException
   {
      RelationalSchema schema = (RelationalSchema)m_database.getSchema();
      SQLAppender origAppender = m_manager.getSQLAppender();
      Table queryView = new Table(schema);
      SQLScript viewScript = new SQLScript();
      SQLStatement sqlStatement = new SQLStatement();
      Column column = new Column("name", queryView);

      column.setType(Primitive.STRING);
      column.setNullable(false);
      column.setAllocation(Column.VARYING);
      column.setPrecision(256);
      sqlStatement.setSQL("select name ${ifci:test.Usr.name:, name$} from ${table:test.Usr}");
      viewScript.addStatement(sqlStatement);
      queryView.addColumn(column);
      queryView.setName("test.UsrView");
      queryView.setType(Table.QUERY);
      queryView.setViewScript(viewScript);

      Index index = new Index("OK1", Index.CLUSTER, queryView);

      index.setUnique(true);
      index.addIndexColumn(new IndexColumn(column, false));
      queryView.addIndex(index);
      schema.addTable(queryView);
      m_manager.setSQLAppender(new SQLSchemaManager.SQLConnectionAppender(m_connection.getConnection(), false));

      try
      {
         m_manager.createTable(queryView);
         m_manager.truncateSchema(schema);
      }
      finally
      {
         try
         {
            m_manager.dropTable(queryView, false);
         }
         finally
         {
            schema.removeTable(queryView);
            m_manager.setSQLAppender(origAppender);
         }
      }
   }

   public void testExecStep()
   {
      StringWriter sql = new StringWriter();

      upgrade(m_singleColTableStep, null, sql);

      ExecStep step = new ExecStep();
      SQLScript script = new SQLScript();
      SQLStatement stmt = new SQLStatement();
      String sSQL = sql.toString();
      int nSQLStmt = sSQL.indexOf(m_manager.getSeparator());

      stmt.setSQL((nSQLStmt < 0) ? sSQL : sSQL.substring(0, nSQLStmt)); // only first statement
      script.addStatement(stmt);
      step.getScriptHolder().addScript(script);

      RelationalSchema schema = upgrade(step, null, null);
      Table table = schema.getTable(m_singleColTableStep.getName());

      assertNotNull(table);
      assertEquals(Table.EXTERNAL, table.getType()); // comes in as Table.EXTERNAL from DB

      DataSource ds = schema.getDataSource();

      for (Iterator/*<DataSourceAdapter>*/ itr = ds.getType().getAdapterIterator(); itr.hasNext();)
      {
         String sName = ((DataSourceAdapter)itr.next()).getName();

         if (!ds.getAdapter().getName().equals(sName)) // adapter name not matching current adapter
         {
            stmt.addAdapter(sName, ds.getType());
            break;
         }
      }

      if (stmt.getAdapterCount() == 0)
      {
         return; // no way to test undefined adapter if only one adapter defined in system.dstypes
      }

      try
      {
         upgrade(step, null, null); // test ExecStep which does not match current adapter
         fail(); // nexj.core.meta.MetadataException expected
      }
      catch (MetadataException e)
      {
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertEquals("err.meta.sql.statementAdapter", e.getErrorCode());
      }
   }

   public void testHint()
   {
      RelationalSchemaUpgrade version = // version testing hints upgrade
         (RelationalSchemaUpgrade)getMetadata().getUpgrade("Main").getVersion("15");
      RelationalSchemaUpgradeState state =
         (RelationalSchemaUpgradeState)Upgrade.getState(Upgrade.getInitialState(version), version);

      assertNull(state.getSchema().findTable("A"));
      version.getStep(0).apply(state); // CreateTable

      Table table = state.getSchema().getTable("A");
      Iterator/*<String>*/ itr = table.getHintIterator();

      itr.next(); // 1st hint
      itr.next(); // 2nd hint
      assertFalse(itr.hasNext());
      assertTrue(table.isHintEnabled("test1"));
      assertTrue(table.isHintEnabled("test2"));
      version.getStep(1).apply(state); // AlterTable

      itr = table.getHintIterator();
      itr.next(); // 1st hint
      itr.next(); // 2nd hint
      assertFalse(itr.hasNext());
      assertTrue(table.isHintEnabled("test1"));
      assertTrue(table.isHintEnabled("test3"));
   }

   public void testIndexDiscardState()
   {
      SQLAppender origAppender = m_manager.getSQLAppender();
      RelationalSchema schema = new RelationalSchema();
      Table table = new Table(schema);
      Column column = new Column("col", table);
      Index index = new Index("indx", Index.CLUSTER, table);

      table.setName("test"); // table name required to add table to schema, to add index to schema
      schema.addTable(table);
      column.setType(Primitive.INTEGER);
      table.addColumn(column);
      index.addIndexColumn(new IndexColumn(column, true));
      table.addIndex(index);
      table.setPrimaryKey(index);
      schema.setDataSource(m_database);

      // validate initial schema state
      assertEquals(1, schema.getIndexCount());
      assertEquals(index, schema.findIndex("indx"));

      try
      {
         m_manager.setSQLAppender(m_manager.new SQLWriterAppender(new StringWriter()));
         m_manager.discardIndex(index, true);
      }
      finally
      {
         m_manager.setSQLAppender(origAppender);
      }

      // schema definition is untouched
      assertEquals(1, schema.getIndexCount());
      assertEquals(index, schema.findIndex("indx"));

      // index either removed or renamed
      assertTrue(table.getIndexCount() == 0 || !index.getName().equals("indx"));
   }

   public void testLoad()
   {
      Metadata metadata = getMetadata();
      RelationalSchema schema = (RelationalSchema)m_database.getSchema();
      Table origVersionTable = schema.getVersionTable(); // note original
      VersionUpgrade loadUpgrade = LoadUpgrade.create(metadata.getVersion(), metadata, null, null);
      String sSQL;

      try
      {
         schema.setVersionTable(schema.getTable("Version"));
         sSQL = upgrade(loadUpgrade, schema);
      }
      finally
      {
         schema.setVersionTable(origVersionTable); // reset so testUpgrade() will not fail
      }

      AssertUtil.assertContained("update test.Version set loaded = 0", sSQL);
   }

   public void testRemoveIndexAspectStep()
   {
      IndexOutline outline = new IndexOutline();

      outline.setName("idx01");
      outline.addColumn("id", true);
      outline.addColumn("value", true);
      m_doubleColTableStep.addIndexOutline(outline);

      RelationalSchema schema = upgrade(m_doubleColTableStep, null, null);
      Table table = schema.getTable(m_doubleColTableStep.getName());

      assertNotNull(table);
      assertEquals(1, table.getIndexCount());
      assertEquals(2, table.getIndex(0).getIndexColumnCount());
      table.setType(Table.MANAGED); // comes in as Table.EXTERNAL from DB

      Index aspect = new Index("aspect", Index.ASPECT, null);

      aspect.addIndexColumn(new IndexColumn(table.getColumn("value"), true));
      table.getIndex(0).addAspect(aspect);
      schema.addIndex(aspect);

      RemoveIndexAspectStep step = new RemoveIndexAspectStep();

      step.setAspectName("aspect");
      step.addPointcutPattern(table.getIndex(0).getName(), true);
      schema = upgrade(step, schema, null);
      table = schema.getTable(m_doubleColTableStep.getName());
      assertNotNull(table);
      assertEquals(1, table.getIndexCount());

      Index index = table.getIndex(0);

      assertEquals(1, index.getIndexColumnCount());
      assertEquals("id", index.getIndexColumn(0).getColumn().getName());
   }

   public void testRemoveTableAspectStep()
   {
      RelationalSchema schema = upgrade(m_doubleColTableStep, null, null);
      Table table = schema.getTable(m_doubleColTableStep.getName());

      assertNotNull(table);
      assertEquals(2, table.getColumnCount());
      table.setType(Table.MANAGED); // comes in as Table.EXTERNAL from DB

      Table aspect = new Table(schema);

      aspect.setName(m_manager.getFullTableName(null, "aspect"));
      aspect.setType(Table.ASPECT);
      aspect.addColumn(new Column("value", aspect));
      table.addAspect(aspect);
      schema.addTable(aspect);

      RemoveTableAspectStep step = new RemoveTableAspectStep();

      step.setAspectName(aspect.getName());
      step.addPointcutPattern(table.getName(), true);
      schema = upgrade(step, schema, null);
      table = schema.getTable(m_doubleColTableStep.getName());
      assertNotNull(table);
      assertEquals(1, table.getColumnCount());
      assertEquals("id", table.getColumn(0).getName());
   }

   public void testRenameColumnStep() throws SQLException
   {
      RelationalSchema schema = upgrade(m_singleColTableStep, null, null);
      Table table = schema.getTable(m_singleColTableStep.getName());

      assertNotNull(table);
      assertEquals(1, table.getColumnCount());
      assertEquals("id", table.getColumn(0).getName());
      table.setType(Table.MANAGED); // comes in as Table.EXTERNAL from DB

      SQLStatement statement = new SQLStatement();
      SQLScript script = new SQLScript();
      ExecStep populate = new ExecStep();

      statement.setSQL(
         "insert into " + m_adapter.getTableName(table, null) +
         " (" + table.getColumn(0).getName() + ") values (1)");
      script.addStatement(statement);
      populate.getScriptHolder().addScript(script);
      upgrade(populate, schema, null);

      RenameColumnStep step = new RenameColumnStep();

      step.setTableName(table.getName());
      step.setOldName("id");
      step.setNewName("id1");
      schema = upgrade(step, schema, null);
      table = schema.getTable(m_singleColTableStep.getName());
      assertNotNull(table);
      assertEquals(1, table.getColumnCount());
      assertEquals("id1", table.getColumn(0).getName());

      // ensure no data loss
      Statement stmt = null;
      ResultSet rs = null;

      try
      {
         stmt = m_connection.getConnection().createStatement();
         rs = stmt.executeQuery("select count(*) from " + m_manager.getTableName(table));
         assertNotNull(rs);
         assertTrue(rs.next());
         assertEquals(1, rs.getInt(1));
      }
      finally
      {
         if (rs != null)
         {
            rs.close();
         }

         if (stmt != null)
         {
            stmt.close();
         }
      }
   }

   public void testRenameIndexStep()
   {
      RelationalSchema schema = upgrade(m_singleIdxColTableStep, null, null);
      Table table = schema.getTable(m_singleIdxColTableStep.getName());

      assertNotNull(table);
      assertEquals(1, table.getIndexCount());
      assertEquals(m_manager.generateIndexName(table.getTableName(), "ind0", null),
                   table.getIndex(0).getName());
      table.setType(Table.MANAGED); // comes in as Table.EXTERNAL from DB

      RenameIndexStep step = new RenameIndexStep();

      step.setOldName(table.getIndex(0).getName());
      step.setNewName("ind1");
      schema = upgrade(step, schema, null);
      table = schema.getTable(m_singleIdxColTableStep.getName());
      assertNotNull(table);
      assertEquals(1, table.getIndexCount());
      assertEquals(m_manager.generateIndexName(table.getTableName(), "ind1", null),
                   table.getIndex(0).getName());
   }

   public void testRenameTableStep() throws SQLException
   {
      RelationalSchema schema = upgrade(m_singlePKColTableStep, null, null);
      Table table = schema.getTable(m_singlePKColTableStep.getName());

      assertNotNull(table);
      table.setType(Table.MANAGED); // comes in as Table.EXTERNAL from DB

      SQLStatement statement = new SQLStatement();
      SQLScript script = new SQLScript();
      ExecStep populate = new ExecStep();

      statement.setSQL(
         "insert into " + m_adapter.getTableName(table, null) +
         " (" + table.getColumn(0).getName() + ") values (1)");
      script.addStatement(statement);
      populate.getScriptHolder().addScript(script);
      upgrade(populate, schema, null);

      RenameTableStep step = new RenameTableStep();
      step.setOldName(table.getName());
      step.setNewName("B");
      schema = upgrade(step, schema, null);

      assertNull(schema.findTable(m_singlePKColTableStep.getName()));

      table = schema.getTable(schema.getPrefix() + "b");

      assertNotNull(table);
      assertEquals(1, table.getColumnCount());

      Column column = table.getColumn(0);

      assertEquals("id", column.getName());
      assertEquals(Primitive.INTEGER, column.getType());

      // ensure no data loss
      Statement stmt = null;
      ResultSet rs = null;

      try
      {
         stmt = m_connection.getConnection().createStatement();
         rs = stmt.executeQuery("select count(*) from " + m_manager.getTableName(table));
         assertNotNull(rs);
         assertTrue(rs.next());
         assertEquals(1, rs.getInt(1));
      }
      finally
      {
         if (rs != null)
         {
            rs.close();
         }

         if (stmt != null)
         {
            stmt.close();
         }
      }
   }

   public void testSchemaVersion()
   {
      RelationalSchema schema = (RelationalSchema)m_database.getSchema();
      Metadata metadata = schema.getMetadata();
      StringWriter writer = new StringWriter();
      Table origVersionTable = schema.getVersionTable(); // note original
      SQLAppender origAppender = m_manager.getSQLAppender();

      try
      {
         schema.setVersionTable(schema.getTable("Version"));
         m_manager.setSQLAppender(m_manager.new SQLWriterAppender(writer));
         m_manager.createSchema(schema);
      }
      finally
      {
         m_manager.setSQLAppender(origAppender);
         schema.setVersionTable(origVersionTable); // reset so testUpgrade() will not fail
      }

      StringBuffer buf =
         new StringBuffer(".Version(namespace, version, step, upgradable, test, loaded) values (");

      m_adapter.appendLiteral(buf, Primitive.STRING, metadata.getNamespace());
      buf.append(", ");
      m_adapter.appendLiteral(buf, Primitive.STRING, metadata.getVersion());
      buf.append(", ");
      m_adapter.appendLiteral(buf, Primitive.INTEGER, Primitive.createInteger(SQLSchemaManager.UPGRADE_END_STEP));
      buf.append(", ");
      m_adapter.appendLiteral(buf, Primitive.BOOLEAN, Boolean.FALSE);
      buf.append(", ");
      m_adapter.appendLiteral(buf, Primitive.BOOLEAN, Boolean.FALSE);
      buf.append(", ");
      m_adapter.appendLiteral(buf, Primitive.BOOLEAN, Boolean.FALSE);
      buf.append(')');

      AssertUtil.assertContained(buf, writer.toString());
   }

   public void testSetupDefaultProperties()
   {
      RelationalSchema schema = (RelationalSchema)m_database.getSchema();
      PropertyMap defaults = m_manager.getDefaultDatabaseProperties(schema, null);
      PropertyMap custom = m_manager.getDefaultDatabaseProperties(schema, defaults); // reinterpret

      assertEquals(defaults.getValueCount(), custom.getValueCount());

      // ensure all keys exist in both sets and non-null values are set for same keys
      for (Iterator itr = defaults.getIterator(); itr.hasNext();)
      {
         String sKey = (String)itr.next();

         assertTrue(defaults.getValue(sKey) == null || custom.getValue(sKey) != null);
      }
   }

   public void testSetupDefaultsExtractor() throws IOException
   {
      PropertyMap propertyMap = new TransferObject();
      PropertyMap referenceMap = new TransferObject();
      String input = "${validKey}${emptyKey}${multiKey:multiValue}${multiKey:multiValue2}"
                   + "${for-each:tablespace:${recurKey:recurValue1}}"
                   + "${for-each:tablespace:${recurKey:recurValue2}}"
                   + "${emptyKey:emptyValue}";

      referenceMap.setValue("validKey", null);
      referenceMap.setValue("multiKey", null);
      referenceMap.setValue("recurKey", null);
      referenceMap.setValue("emptyKey", null);
      propertyMap.setValue("validKey", "validValue");
      propertyMap.setValue("multiKey", "multiValue");
      propertyMap.setValue("recurKey", null);
      propertyMap.setValue("emptyKey", null);
      m_manager.setTemplateDefaults(propertyMap, referenceMap, new StringReader(input), true);

      assertEquals(4, propertyMap.getValueCount());
      assertEquals("validValue", propertyMap.getValue("validKey"));
      assertEquals("multiValue", propertyMap.getValue("multiKey"));
      assertEquals("recurValue1", propertyMap.getValue("recurKey"));
      assertEquals("emptyValue", propertyMap.getValue("emptyKey"));

      // test ${iftest:...} with test environment enabled
      input = "${iftest:${emptyKey:emptyValue}}";
      propertyMap = new TransferObject();
      propertyMap.setValue("emptyKey", null);
      m_manager.setTemplateDefaults(propertyMap, referenceMap, new StringReader(input), true);
      assertEquals(1, propertyMap.getValueCount());
      assertEquals("emptyValue", propertyMap.getValue("emptyKey"));

      // test ${iftest:...} with test environment disabled
      propertyMap = new TransferObject();
      propertyMap.setValue("emptyKey", null);
      m_manager.setTemplateDefaults(propertyMap, referenceMap, new StringReader(input), false);
      assertEquals(1, propertyMap.getValueCount());
      assertNull(propertyMap.getValue("emptyKey"));
   }

   public void testSetupReader() throws IOException
   {
      StringWriter output = new StringWriter();
      RelationalSchema schema = new RelationalSchema();
      TransferObject valueMap = new TransferObject();
      TransferObject defaultsMap = new TransferObject();
      Table tmpTable;

      tmpTable = new Table(schema);
      tmpTable.setName("table1"); // required for addTable() to work
      tmpTable.setLongspaceName("longspace1");
      schema.addTable(tmpTable);
      tmpTable = new Table(schema);
      tmpTable.setName("table2"); // required for addTable() to work
      tmpTable.setLongspaceName("longspace2");
      schema.addTable(tmpTable);
      tmpTable = new Table(schema);
      tmpTable.setName("table3"); // required for addTable() to work
      tmpTable.setLongspaceName("longspace3");
      schema.addTable(tmpTable);
      valueMap.setValue("valid-key", "valid-value");
      valueMap.setValue("missing-key", null);
      valueMap.setValue("default-key", null);
      defaultsMap.setValue("default-key", "default-value");
      valueMap.setValue("tablespace", null);
      valueMap.setValue("indexspace", "indexspace-value");
      valueMap.setValue("longspace", null);

      // existing value
      IOUtil.copy(output,
                  new SQLSchemaManager.DatabaseTemplateSubstReader(
                     new StringReader("[${valid-key}]"),
                     schema,
                     valueMap,
                     defaultsMap,
                     '/'));
      assertEquals("[valid-value]", output.toString());

      // missing value
      output.getBuffer().setLength(0);
      try
      {
         IOUtil.copy(output,
                     new SQLSchemaManager.DatabaseTemplateSubstReader(
                        new StringReader("[${missing-key}]"),
                        schema,
                        valueMap,
                        defaultsMap,
                        '/'));
         fail(); // exception expected
      }
      catch (LookupException e)
      {
         assertEquals("err.meta.persistence.sql.undefined", e.getErrorCode());
      }

      // missing value with default
      output.getBuffer().setLength(0);
      IOUtil.copy(output,
                  new SQLSchemaManager.DatabaseTemplateSubstReader(
                     new StringReader("[${default-key}]"),
                     schema,
                     valueMap,
                     defaultsMap,
                     '/'));
      assertEquals("[default-value]", output.toString());

      // unknown value
      output.getBuffer().setLength(0);
      try
      {
         IOUtil.copy(output,
                     new SQLSchemaManager.DatabaseTemplateSubstReader(
                        new StringReader("[${unknown-key}]"),
                        schema,
                        valueMap,
                        defaultsMap,
                        '/'));
         fail(); // exception expected
      }
      catch (LookupException e)
      {
         assertEquals("err.meta.persistence.sql.variable", e.getErrorCode());
      }

      // foreach with no tables, no default tablespace
      output.getBuffer().setLength(0);
      IOUtil.copy(output,
                  new SQLSchemaManager.DatabaseTemplateSubstReader(
                     new StringReader("[${for-each:tablespace:${tablespace}}]"),
                     schema,
                     valueMap,
                     defaultsMap,
                     '/'));
      assertEquals("[]", output.toString());

      // foreach with no tables, only default tablespace
      output.getBuffer().setLength(0);
      IOUtil.copy(output,
                  new SQLSchemaManager.DatabaseTemplateSubstReader(
                     new StringReader("[${for-each:indexspace:${indexspace}}]"),
                     schema,
                     valueMap,
                     defaultsMap,
                     '/'));
      assertEquals("[indexspace-value]", output.toString());

      // foreach with several tables
      output.getBuffer().setLength(0);
      IOUtil.copy(output,
                  new SQLSchemaManager.DatabaseTemplateSubstReader(
                     new StringReader("[${for-each:longspace:${longspace}}]"),
                     schema,
                     valueMap,
                     defaultsMap,
                     '/'));
      assertEquals("[longspace1longspace2longspace3]", output.toString());

      // foreach not collection
      output.getBuffer().setLength(0);
      IOUtil.copy(output,
                  new SQLSchemaManager.DatabaseTemplateSubstReader(
                     new StringReader("[${for-each:non-collection:default-value}]"),
                     schema,
                     valueMap,
                     defaultsMap,
                     '/'));
      assertEquals("[non-collection:default-value]", output.toString());

      // set schema.getMetadata().isTestEnvironment() return value for test
      XMLMetadata metadata = new XMLMetadata(null, null, null, null, null);
      DataSourceType dsType = new DataSourceType(null);
      DataSource ds = new DataSource(null) {};

      dsType.setMetadata(metadata);
      ds.setType(dsType);
      schema.setDataSource(ds);

      // iftest enabled
      metadata.setTestEnvironment(true);
      output.getBuffer().setLength(0);
      IOUtil.copy(output,
                  new SQLSchemaManager.DatabaseTemplateSubstReader(
                     new StringReader("[${iftest:${valid-key}}]"),
                     schema,
                     valueMap,
                     defaultsMap,
                     '/'));
      assertEquals("[valid-value]", output.toString());

      // iftest disabled
      metadata.setTestEnvironment(false);
      output.getBuffer().setLength(0);
      IOUtil.copy(output,
                  new SQLSchemaManager.DatabaseTemplateSubstReader(
                     new StringReader("[${iftest:${valid-key}}]"),
                     schema,
                     valueMap,
                     defaultsMap,
                     '/'));
      assertEquals("[]", output.toString());

      // path MSFT compatible
      valueMap.setValue("datapath", "C:\\test\\path");
      output.getBuffer().setLength(0);
      IOUtil.copy(output,
                  new SQLSchemaManager.DatabaseTemplateSubstReader(
                     new StringReader("[${path:${datapath:/abc/def} /ghi/jkl}]"),
                     schema,
                     valueMap,
                     defaultsMap,
                     '\\'));
      assertEquals("[C:\\test\\path \\ghi\\jkl]", output.toString());

      output.getBuffer().setLength(0);
      IOUtil.copy(output,
                  new SQLSchemaManager.DatabaseTemplateSubstReader(
                     new StringReader("[${path:${datapath:C:\\abc\\def} D:\\ghi\\jkl ab:\\xy}]"),
                     schema,
                     valueMap,
                     defaultsMap,
                     '\\'));
      assertEquals("[C:\\test\\path D:\\ghi\\jkl ab:\\xy]", output.toString());

      // path generic
      valueMap.setValue("datapath", "/test/path");
      output.getBuffer().setLength(0);
      IOUtil.copy(output,
                  new SQLSchemaManager.DatabaseTemplateSubstReader(
                     new StringReader("[${path:${datapath:/abc/def} /ghi/jkl}]"),
                     schema,
                     valueMap,
                     defaultsMap,
                     '/'));
      assertEquals("[/test/path /ghi/jkl]", output.toString());

      output.getBuffer().setLength(0);
      IOUtil.copy(output,
                  new SQLSchemaManager.DatabaseTemplateSubstReader(
                     new StringReader("[${path:${datapath:c:\\abc\\def} d:\\ghi\\jkl ab:\\xy}]"),
                     schema,
                     valueMap,
                     defaultsMap,
                     '/'));
      assertEquals("[/test/path d:/ghi/jkl ab:/xy]", output.toString());
   }

   public void testSortExistancePrerequisites()
   {
      RelationalSchema schema = new RelationalSchema();
      Table z = new Table(schema);
      Table y = new Table(schema);
      Table x = new Table(schema);
      Table w = new Table(schema);
      Table v = new Table(schema);
      Table u = new Table(schema);
      RelationalObject[] array;

      // set names in reverse order to actual dependency order
      z.setName("z");
      y.setName("y");
      x.setName("x");
      w.setName("w");
      v.setName("v");
      u.setName("u");

      schema.addTable(z);
      y.addPrerequisite(z);
      schema.addTable(y);
      x.addPrerequisite(y);
      schema.addTable(x);
      array = SQLSchemaManager.getSortedRelationalObjects(schema);

      assertEquals(3, array.length);
      assertEquals(z, array[0]);
      assertEquals(y, array[1]);
      assertEquals(x, array[2]);

      w.addPrerequisite(z);
      w.addPrerequisite(x);
      schema.addTable(w);
      array = SQLSchemaManager.getSortedRelationalObjects(schema);

      assertEquals(4, array.length);
      assertEquals(z, array[0]);
      assertEquals(y, array[1]);
      assertEquals(x, array[2]);
      assertEquals(w, array[3]);

      v.addPrerequisite(w);
      v.addPrerequisite(u);
      schema.addTable(v);
      schema.addTable(u);
      array = SQLSchemaManager.getSortedRelationalObjects(schema);

      assertEquals(6, array.length);
      assertEquals(u, array[0]);
      assertEquals(z, array[1]);
      assertEquals(y, array[2]);
      assertEquals(x, array[3]);
      assertEquals(w, array[4]);
      assertEquals(v, array[5]);

      try
      {
         v.addPrerequisite(w);
         fail(); // MetadataException expected
      }
      catch (MetadataException e)
      {
         assertEquals("err.meta.sql.prerequisiteDup", e.getErrorCode());
      }

      u.addPrerequisite(v);
      array = SQLSchemaManager.getSortedRelationalObjects(schema); // ensure u->v->u loop terminates

      try
      {
         v.validate(m_context.getMetadata(), null);
         fail(); // MetadataException expected
      }
      catch (MetadataException e)
      {
         assertEquals("err.meta.sql.prerequisiteLoop", e.getErrorCode());
      }
   }

   public void testViewScriptUpgradeMismtch()
   {
      Upgrade upgrade = new Upgrade(null);
      RelationalSchemaUpgrade version = new RelationalSchemaUpgrade("test");
      XMLMetadata metadata = new XMLMetadata(null, null, null, null, null);
      RelationalDatabase ds = new RelationalDatabase(null);
      RelationalSchema schema = new RelationalSchema();
      CreateTableStep step = new CreateTableStep(); // only step that can populate ViewScript
      Table table = new Table(schema);
      SQLScript finalScript = new SQLScript();
      SQLScript upgradeScript = new SQLScript();
      SQLStatement finalStmt = new SQLStatement();
      SQLStatement upgradeStmt = new SQLStatement();

      upgradeStmt.addAdapter("*", m_database.getSchema().getDataSource().getType());
      finalStmt.addAdapter("*", m_database.getSchema().getDataSource().getType());
      upgradeStmt.setSQL("upgrade view SQL");
      finalStmt.setSQL("final view SQL");
      upgradeScript.addStatement(upgradeStmt);
      finalScript.addStatement(finalStmt);
      table.setName("testTable");
      table.setType(Table.VIEW);
      step.setName(table.getName());
      step.setType(table.getType());
      step.setViewScript(upgradeScript);
      schema.addTable(table);
      schema.setDataSource(ds);
      ds.setAdapter(m_database.getSchema().getDataSource().getAdapter());
      ds.setSchema(schema);
      ds.setType(new DataSourceType(null));
      ds.getType().setMetadata(metadata);
      version.setDataSource(ds);
      version.addStep(step);
      metadata.setVersion(version.getName());
      upgrade.setMetadata(metadata);
      upgrade.addVersion(version);
      table.setViewScript(finalScript);

      try
      {
         upgrade.validate(metadata, null);
         fail(); // exception expected
      }
      catch (MetadataException e)
      {
         assertEquals("err.meta.upgrade.sql.viewTableMismatch",
                      ((UncheckedException)e.getCause()).getErrorCode());
      }

      upgradeStmt.setSQL(finalStmt.getSQL()); // different object with same value
      upgrade.validate(metadata, null); // this must pass without exception
   }

   public void testUpgradeValidation()
   {
      StringWriter buf = new StringWriter();
      DataSourceAdapter invalid = new DataSourceAdapter("InvalidAdapter");
      DataSourceAdapter valid = new DataSourceAdapter("ValidAdapter");
      RelationalDatabase ds = new RelationalDatabase("DataSource");
      RelationalSchema schema = new RelationalSchema();
      final Upgrade upgrade = new Upgrade(null);
      XMLMetadata metadata = new XMLMetadata(null, null, null, null, null)
      {
         public Upgrade getUpgrade(String sName) { return upgrade; }
      };
      SQLSchemaManager manager = m_adapter.createSchemaManager();
      RelationalSchemaUpgrade upgradeVersion;

      ds.setAdapter(valid);
      ds.setComponent(new Component("Component"));
      ds.setSchema(schema);
      ds.setType(new DataSourceType("DataSourceType"));
      ds.getType().setMetadata(metadata);
      ds.getType().addAdapter(ds.getAdapter());
      ds.getType().addAdapter(invalid);
      ((RelationalDatabaseFragment)ds.getDefaultFragment()).setDatabase("Database");
      metadata.addDataSource(ds);
      manager.setSQLAppender(manager.new SQLWriterAppender(buf));
      schema.setVersionTable(new Table(schema));
      schema.getVersionTable().setName("VersionTable");
      schema.addTable(schema.getVersionTable());
      upgrade.setMetadata(metadata);

      // setup for first upgrade version
      ExecStep execStep = new ExecStep(); // step lacking match for current adapter
      SQLScript stepScript = new SQLScript();
      SQLStatement stepStmt = new SQLStatement();

      stepStmt.addAdapter(invalid.getName(), ds.getType());
      stepStmt.setSQL("SQLStatement SQL");
      stepScript.addStatement(stepStmt);
      execStep.getScriptHolder().addScript(stepScript);

      // single incompatible step
      metadata.setVersion("1-step");
      upgradeVersion = new RelationalSchemaUpgrade(metadata.getVersion());
      upgradeVersion.addStep(execStep);
      upgradeVersion.setDataSource(ds);
      upgrade.addVersion(upgradeVersion);

      try
      {
         upgrade.validate(null, null); // must fail since an incompatible step "1-step" exists
         fail(); // exception expected
      }
      catch (MetadataException e)
      {
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertEquals("err.meta.sql.statementAdapter", e.getErrorCode());
      }

      try
      {
         manager.upgrade(schema, null); //must fail since no compatible versions found to start with
         fail(); // exception expected
      }
      catch (MetadataException e)
      {
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertEquals("err.meta.sql.statementAdapter", e.getErrorCode());
      }

      try
      {
         manager.upgrade(schema, "1-step"); // must fail since step "1-step" is incompatible
         fail(); // exception expected
      }
      catch (MetadataException e)
      {
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertEquals("err.meta.sql.statementAdapter", e.getErrorCode());
      }

      // setup for second upgrade version
      SupportAdapterStep adapterStep = new SupportAdapterStep();
      CreateTableStep createTableStep = new CreateTableStep(); // step has match for current adapter
      Table table = new Table(schema);
      SQLScript tableScript = new SQLScript();
      SQLStatement tableStmtInvalid = new SQLStatement();
      SQLStatement tableStmtValid = new SQLStatement();

      tableStmtInvalid.addAdapter(invalid.getName(), ds.getType());
      tableStmtInvalid.setSQL("SQLStatement Table SQL Invalid");
      tableStmtValid.addAdapter(ds.getAdapter().getName(), ds.getType());
      tableStmtValid.setSQL("SQLStatement Table SQL Valid");
      tableScript.addStatement(tableStmtValid);
      tableScript.addStatement(tableStmtInvalid);
      tableScript.addStatement(stepStmt);
      table.setName("Table");
      table.setType(Table.VIEW);
      table.setViewScript(tableScript);
      schema.addTable(table);
      createTableStep.setName(table.getName());
      createTableStep.setType(table.getType());
      createTableStep.setViewScript(tableScript);
      adapterStep.setAdapter(invalid);

      // incompatible step -> compatible step
      metadata.setVersion("2-step");
      upgradeVersion = new RelationalSchemaUpgrade(metadata.getVersion());
      upgradeVersion.addStep(adapterStep);
      upgradeVersion.addStep(createTableStep);
      upgradeVersion.setDataSource(ds);
      upgrade.addVersion(upgradeVersion);

      try
      {
         upgrade.validate(null, null); // must fail since an incompatible step "1-step" exists
         fail(); // exception expected
      }
      catch (MetadataException e)
      {
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertEquals("err.meta.sql.statementAdapter", e.getErrorCode());
      }

      try
      {
         manager.upgrade(schema, null); // must fail since an incompatible step "1-step" exists
         fail(); // exception expected
      }
      catch (MetadataException e)
      {
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertEquals("err.meta.sql.statementAdapter", e.getErrorCode());
      }

      buf.getBuffer().setLength(0);
      ds.setAdapter(invalid);
      manager.upgrade(schema, "2-step"); // validation of invalid adapter should not be prevented
      AssertUtil.assertContained(tableStmtInvalid.getSQL(), buf.toString());
      ds.setAdapter(valid);

      try
      {
         manager.upgrade(schema, "2-step"); // must fail since validating current adapter from start
         fail(); // exception expected
      }
      catch (MetadataException e)
      {
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertEquals("err.meta.sql.statementAdapter", e.getErrorCode());
      }

      adapterStep.setAdapter(ds.getAdapter());
      upgrade.validate(null, null); // must pass since adapter validation from "2-step"
      buf.getBuffer().setLength(0);
      manager.upgrade(schema, null); // must upgrade starting from "2-step"
      AssertUtil.assertContained(tableStmtValid.getSQL(), buf.toString());

      // setup for third upgrade version
      CreateColumnStep createColumnStep = new CreateColumnStep(); // step lacking match for adapter
      Column column = new Column("Column", table);
      ColumnOutline outline = new ColumnOutline(column.getName());

      column.setType(Primitive.INTEGER);
      table.addColumn(column);
      outline.setType(column.getType());
      createColumnStep.setOutline(outline);
      createColumnStep.setTableName(table.getName());
      createColumnStep.getScriptHolder().addScript(stepScript);

      // incompatible step -> compatible step -> incompatible step
      metadata.setVersion("3-step");
      upgradeVersion = new RelationalSchemaUpgrade(metadata.getVersion());
      upgradeVersion.addStep(createColumnStep);
      upgradeVersion.setDataSource(ds);
      upgrade.addVersion(upgradeVersion);

      try
      {
         upgrade.validate(null, null);//must fail since an incompatible step exists after compatible
         fail(); // exception expected
      }
         catch (MetadataException e)
      {
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertEquals("err.meta.sql.statementAdapter", e.getErrorCode());
      }

      try
      {
         manager.upgrade(schema, null); //must fail since no compatible versions found to start with
         fail(); // exception expected
      }
         catch (MetadataException e)
      {
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertEquals("err.meta.sql.statementAdapter", e.getErrorCode());
      }

      try
      {
         manager.upgrade(schema, "1-step"); // must fail since step "1-step" is incompatible
         fail(); // exception expected
      }
         catch (MetadataException e)
      {
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertEquals("err.meta.sql.statementAdapter", e.getErrorCode());
      }

      try
      {
         manager.upgrade(schema, "2-step"); // must fail since step "3-step" is incompatible
         fail(); // exception expected
      }
      catch (MetadataException e)
      {
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertTrue(e.getCause() instanceof MetadataException);
         e = (MetadataException)e.getCause();
         assertEquals("err.meta.sql.statementAdapter", e.getErrorCode());
      }
   }

   /**
    * Perform a RDBMS upgrade step.
    * @param step The step to perform.
    * @param schema The modifiable current schema state (null == use empty schema).
    * @param writer The writer to receive the generated SQL (null == use DB connection).
    * @return The new schema state as read from RDBMS.
    */
   protected RelationalSchema upgrade(
      RelationalSchemaUpgradeStep step, RelationalSchema schema, Writer writer)
   {
      if (schema == null)
      {
         RelationalSchema templateSchema = (RelationalSchema)m_database.getSchema();

         schema = new RelationalSchema();
         schema.setDataSource(templateSchema.getDataSource());
         schema.setIndexFill(templateSchema.getIndexFill());
         schema.setIndexspaceName(templateSchema.getIndexspaceName());
         schema.setLongspaceName(templateSchema.getLongspaceName());
         schema.setPrefix(templateSchema.getPrefix());
         schema.setRoleName(templateSchema.getRoleName());
         schema.setTablespaceName(templateSchema.getTablespaceName());

         // hints declared/used by core/test/nexj/base/upgrades/Main.upgrade, need for validation
         RelationalSchemaTest.addHint(schema, "test1");
         RelationalSchemaTest.addHint(schema, "test2");
         RelationalSchemaTest.addHint(schema, "test3");
      }

      RelationalSchemaUpgradeState state = new RelationalSchemaUpgradeState(schema, null, null);
      SchemaVersion version = new SchemaVersion();
      RelationalSchemaUpgrade upgrade = new RelationalSchemaUpgrade();
      SQLAppender origAppender = m_manager.getSQLAppender();

      upgrade.addStep(step);
      upgrade.setUpgrade(new Upgrade(null));
      m_manager.setSQLAppender(
         (writer == null) ? new SQLSchemaManager.SQLConnectionAppender(m_connection.getConnection())
                          : (SQLAppender)m_manager.new SQLWriterAppender(writer));

      try
      {
         m_manager.upgrade(upgrade, state, version);
      }
      finally
      {
         m_manager.setSQLAppender(origAppender);
      }

      return readSchema(schema);
   }

   /**
    * Perform a simulated RDBMS upgrade step with output as return value.
    * @param version The VersionUpgrade to simulate.
    * @param schema The modifiable current schema state (not null and getVersionTable() not null).
    * @return The SQL generated during the execution of the upgrade.
    */
   protected String upgrade(VersionUpgrade version, RelationalSchema schema)
   {
      Upgrade upgrade = new Upgrade(null);
      SQLAppender origAppender = m_manager.getSQLAppender();
      StringWriter sql = new StringWriter();

      upgrade.setMetadata(getMetadata());
      upgrade.addVersion(version);
      m_manager.setSQLAppender(m_manager.new SQLWriterAppender(sql));

      try
      {
         m_manager.upgrade(schema, upgrade, upgrade.getFirstVersion().getName());
      }
      finally
      {
         m_manager.setSQLAppender(origAppender);
      }

      return sql.getBuffer().toString();
   }
}