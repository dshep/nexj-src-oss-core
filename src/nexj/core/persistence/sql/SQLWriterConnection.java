// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.math.BigDecimal;
import java.net.URL;
import java.sql.Array;
import java.sql.Blob;
import java.sql.CallableStatement;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.Date;
import java.sql.ParameterMetaData;
import java.sql.PreparedStatement;
import java.sql.Ref;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.Savepoint;
import java.sql.Statement;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Map;
import java.util.TimeZone;

import nexj.core.meta.Primitive;
import nexj.core.util.Binary;
import nexj.core.util.MathUtil;

/**
 * A Connection that dumps plain SQL to a writer.
 * Reads from this connections will never return any data.
 * Access to the writer is _not_ synchronized.
 */
public class SQLWriterConnection implements Connection
{
   /**
    * Write statements that would produce a ResultSet.
    */
   protected boolean m_bQueryWrittenThough;

   /**
    * The SQL adapter to use for creating value literals.
    */
   protected SQLAdapter m_adapter;

   /**
    * The SchemaManager for the adapter.
    * Used to get statement separator token.
    */
   protected SQLSchemaManager m_schemaManager;

   /**
    * The Writer object receiving all SQL requests.
    */
   protected Writer m_writer;

   /**
    * Constructor.
    * @param adapter The SQL adapter to use for creating value literals.
    * @param writer The object to output writes to.
    */
   public SQLWriterConnection(SQLAdapter adapter, Writer writer)
   {
      this(adapter, writer, true);
   }

   /**
    * Constructor.
    * @param adapter The SQL adapter to use for creating value literals.
    * @param writer The object to output writes to.
    * @param bQueryWrittenThough Write statements that would produce a ResultSet.
    */
   public SQLWriterConnection(SQLAdapter adapter, Writer writer, boolean bQueryWrittenThough)
   {
      assert adapter != null;

      m_adapter = adapter;
      m_writer = writer;
      m_schemaManager = m_adapter.createSchemaManager();
      m_bQueryWrittenThough = bQueryWrittenThough;

      assert m_schemaManager != null;
   }

   /**
    * Append contents of buffer to the writer.
    * @param buf The contents to append to the writer.
    * @throws SQLException Conform to API's use of SQLException by converting from IOException
    */
   protected void append(CharSequence buf) throws SQLException
   {
      validateOpen();

      try
      {
         m_writer.append(buf);
      }
      catch (IOException e)
      {
         throw new SQLException(e.getMessage()); // conform to API's use of SQLException
      }
   }

   /**
    * @see java.sql.Connection#clearWarnings()
    */
   public void clearWarnings() throws SQLException
   {
      // NOOP
   }

   /**
    * @see java.sql.Connection#close()
    */
   public void close() throws SQLException
   {
      m_writer = null; // mark as closed
   }

   /**
    * @see java.sql.Connection#commit()
    */
   public void commit() throws SQLException
   {
      append("commit"); // part of SQL standard
      append(m_schemaManager.getSeparator());

      try
      {
         m_writer.flush();
      }
      catch (IOException e)
      {
         throw new SQLException(e.getMessage()); // conform to API's use of SQLException
      }
   }

   /**
    * @see java.sql.Connection#createStatement()
    */
   public Statement createStatement() throws SQLException
   {
      return new SQLWriterStatement(null);
   }

   /**
    * @see java.sql.Connection#createStatement(int, int)
    */
   public Statement createStatement(int resultSetType, int resultSetConcurrency)
      throws SQLException
   {
      return createStatement();
   }

   /**
    * @see java.sql.Connection#createStatement(int, int, int)
    */
   public Statement createStatement(int resultSetType, int resultSetConcurrency,
      int resultSetHoldability) throws SQLException
   {
      return createStatement();
   }

   /**
    * @see java.sql.Connection#getAutoCommit()
    */
   public boolean getAutoCommit() throws SQLException
   {
      return true; // Always commits to writer (realistically it depends on default DB client behaviour)
   }

   /**
    * @see java.sql.Connection#getCatalog()
    */
   public String getCatalog() throws SQLException
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see java.sql.Connection#getHoldability()
    */
   public int getHoldability() throws SQLException
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see java.sql.Connection#getMetaData()
    */
   public DatabaseMetaData getMetaData() throws SQLException
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see java.sql.Connection#getTransactionIsolation()
    */
   public int getTransactionIsolation() throws SQLException
   {
      return Connection.TRANSACTION_NONE;
   }

   /**
    * @see java.sql.Connection#getTypeMap()
    */
   public Map getTypeMap() throws SQLException
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see java.sql.Connection#getWarnings()
    */
   public SQLWarning getWarnings() throws SQLException
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see java.sql.Connection#isClosed()
    */
   public boolean isClosed() throws SQLException
   {
      return m_writer == null;
   }

   /**
    * @see java.sql.Connection#isReadOnly()
    */
   public boolean isReadOnly() throws SQLException
   {
      return false;
   }

   /**
    * @see java.sql.Connection#nativeSQL(java.lang.String)
    */
   public String nativeSQL(String sSQL) throws SQLException
   {
      return sSQL;
   }

   /**
    * @see java.sql.Connection#prepareCall(java.lang.String)
    */
   public CallableStatement prepareCall(String sSQL) throws SQLException
   {
      return new SQLWriterStatement(sSQL);
   }

   /**
    * @see java.sql.Connection#prepareCall(java.lang.String, int, int)
    */
   public CallableStatement prepareCall(String sSQL, int resultSetType, int resultSetConcurrency)
      throws SQLException
   {
      return prepareCall(sSQL);
   }

   /**
    * @see java.sql.Connection#prepareCall(java.lang.String, int, int, int)
    */
   public CallableStatement prepareCall(String sSQL, int resultSetType, int resultSetConcurrency,
      int resultSetHoldability) throws SQLException
   {
      return prepareCall(sSQL);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String)
    */
   public PreparedStatement prepareStatement(String sSQL) throws SQLException
   {
      return new SQLWriterStatement(sSQL);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, int)
    */
   public PreparedStatement prepareStatement(String sSQL, int autoGeneratedKeys)
      throws SQLException
   {
      return prepareStatement(sSQL);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, int[])
    */
   public PreparedStatement prepareStatement(String sSQL, int[] columnIndexes)
      throws SQLException
   {
      return prepareStatement(sSQL);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, java.lang.String[])
    */
   public PreparedStatement prepareStatement(String sSQL, String[] columnNames)
      throws SQLException
   {
      return prepareStatement(sSQL);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, int, int)
    */
   public PreparedStatement prepareStatement(String sSQL, int resultSetType,
      int resultSetConcurrency) throws SQLException
   {
      return prepareStatement(sSQL);
   }

   /**
    * @see java.sql.Connection#prepareStatement(java.lang.String, int, int, int)
    */
   public PreparedStatement prepareStatement(String sSQL, int resultSetType,
      int resultSetConcurrency, int resultSetHoldability) throws SQLException
   {
      return prepareStatement(sSQL);
   }

   /**
    * @see java.sql.Connection#releaseSavepoint(java.sql.Savepoint)
    */
   public void releaseSavepoint(Savepoint savepoint) throws SQLException
   {
      // NOOP
   }

   /**
    * @see java.sql.Connection#rollback()
    */
   public void rollback() throws SQLException
   {
      append("rollback"); // part of SQL standard
      append(m_schemaManager.getSeparator());

      try
      {
         m_writer.flush();
      }
      catch (IOException e)
      {
         throw new SQLException(e.getMessage()); // conform to API's use of SQLException
      }

   }

   /**
    * @see java.sql.Connection#rollback(java.sql.Savepoint)
    */
   public void rollback(Savepoint savepoint) throws SQLException
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see java.sql.Connection#setAutoCommit(boolean)
    */
   public void setAutoCommit(boolean autoCommit) throws SQLException
   {
      // NOOP
   }

   /**
    * @see java.sql.Connection#setCatalog(java.lang.String)
    */
   public void setCatalog(String catalog) throws SQLException
   {
      // NOOP
   }

   /**
    * @see java.sql.Connection#setHoldability(int)
    */
   public void setHoldability(int holdability) throws SQLException
   {
      // NOOP
   }

   /**
    * @see java.sql.Connection#setReadOnly(boolean)
    */
   public void setReadOnly(boolean readOnly) throws SQLException
   {
      // NOOP
   }

   /**
    * @see java.sql.Connection#setSavepoint()
    */
   public Savepoint setSavepoint() throws SQLException
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see java.sql.Connection#setSavepoint(java.lang.String)
    */
   public Savepoint setSavepoint(String name) throws SQLException
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see java.sql.Connection#setTransactionIsolation(int)
    */
   public void setTransactionIsolation(int level) throws SQLException
   {
      // NOOP
   }

   /**
    * @see java.sql.Connection#setTypeMap(java.util.Map)
    */
   public void setTypeMap(Map arg0) throws SQLException
   {
      // NOOP
   }

   /**
    * Validate that m_writer is open.
    * @throws SQLException If m_writer is not open.
    */
   protected void validateOpen() throws SQLException
   {
      if (isClosed())
      {
         throw new SQLException(this.getClass().getSimpleName() + '@' +
                                System.identityHashCode(this) +
                                " has been previously closed.");
      }
   }

   /**
    * CallableStatement that sends all SQL requests to a writer.
    */
   protected class SQLWriterStatement implements CallableStatement
   {
      /**
       * ID offset into m_markArray metadata group.
       */
      protected final static int MARKER_ID = 0;

      /**
       * Length offset into m_markArray metadata group.
       */
      protected final static int MARKER_LENGTH = 2;

      /**
       * Position in m_SQL offset into m_markArray metadata group.
       */
      protected final static int MARKER_OFFSET = 1;

      /**
       * Length of each individual meta group in m_markArray array.
       */
      protected final static int MARKER_META_SIZE = 3;

      /**
       * Type offset into m_paramArray metadata group.
       */
      protected final static int PARAM_TYPE = 0;

      /**
       * Value offset into m_paramArray metadata group.
       */
      protected final static int PARAM_VALUE = 1;

      /**
       * Length of each individual meta group in m_paramArray array.
       */
      protected final static int PARAM_META_SIZE = 2;

      /**
       * Sequential list of batched m_paramArray objects, valid up to m_nBatchCount, in the form of:
       * {<paramArray>,<paramArray>,...}
       */
      Object[][] m_batchArray; // lazy init

      /**
       * The SQL of this statement.
       */
      protected String m_sSQL;

      /**
       * Ordered array of positions in m_sSQL where each of the parameters is supposed to go,
       * terminated by -1. The array is in the form of:
       * {..., <marker id>, <marker start offset>, <marker length>, -1, ...}
       * e.g. for 2 tokens of size 2,3 and at positions 3,7 respectively:
       * {1, 3, 2,  2, 7, 3,  -1, ...}
       */
      int[] m_markArray = new int[] {-1};

      /**
       * Number of set batch groups.
       */
      int m_nBatchCount = 0;

      /**
       * Number of declared parameters, used for validation.
       */
      int m_nParamCount = 0;

      /**
       * Array of parameter objects and their types ordered by parameter ID in the form of:
       * {<Primitive type>, <value>, ...}
       */
      Object[] m_paramArray = new Object[0];

      /**
       * Constructor.
       * @param sSQL The SQL of this statement.
       * @throws SQLException if parameter parsing error occurs.
       */
      public SQLWriterStatement(String sSQL) throws SQLException
      {
         m_sSQL = sSQL;

         if (m_sSQL == null) // not a prepared/callable statement
         {
            return;
         }

         StringBuffer buf = new StringBuffer();

         m_adapter.appendBind(buf, m_nParamCount);

         String sToken = buf.toString();
         int nLength = m_sSQL.length();
         int[] markArray = new int[nLength << 1]; // double size, worst case have token per every char
         Arrays.fill(markArray, 0, markArray.length, -1); // reset

         // use a linear marker array so that out-of-order markers would not require inserting
         // values in the middle of m_markArray
         if ("?".equals(sToken)) // standard SQL placeholder
         {
            for (int nCount = markTokens(m_sSQL, sToken, 0, true, markArray);
                 m_nParamCount < nCount;
                 ++m_nParamCount)
            {
               markArray[nLength + m_nParamCount] = sToken.length(); // note token length
            }
         }
         else // some other SQL placeholder, assume unique/distinct (e.g. Oracle)
         {
            // terminate loop on first missed token
            while (markTokens(m_sSQL, sToken, m_nParamCount, false, markArray) > 0)
            {
               markArray[nLength + m_nParamCount] = sToken.length(); // note token length
               buf.setLength(0);
               m_adapter.appendBind(buf, ++m_nParamCount); // +1 for next
               sToken = buf.toString(); // update next placeholder token
            }
         }

         int nOffset = 0;

         for (int i = 0; i < nLength; ++i)
         {
            if (markArray[i] < 0)
            {
               continue; // not a parameter placeholder
            }

            if (m_markArray.length <= nOffset + MARKER_META_SIZE) // array too small for param
            {
               int[] tmpArray = new int[(m_markArray.length << 1) + MARKER_META_SIZE];

               System.arraycopy(m_markArray, 0, tmpArray, 0, m_markArray.length);
               m_markArray = tmpArray;
            }

            int nTokenLength = markArray[nLength + markArray[i]];

            m_markArray[nOffset + MARKER_ID] = markArray[i]; // <marker id>
            m_markArray[nOffset + MARKER_OFFSET] = i; // <marker start offset>
            m_markArray[nOffset + MARKER_LENGTH] = nTokenLength; // <marker length>
            m_markArray[nOffset + MARKER_META_SIZE] = -1;
            nOffset += MARKER_META_SIZE; // increment by size of metadata block
            i += Math.max(1, nTokenLength) - 1; // -1 because for-loop increments
         }
      }

      /**
       * @see java.sql.CallableStatement#getArray(int)
       */
      public Array getArray(int i) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getArray(java.lang.String)
       */
      public Array getArray(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getBigDecimal(int)
       */
      public BigDecimal getBigDecimal(int parameterIndex) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getBigDecimal(java.lang.String)
       */
      public BigDecimal getBigDecimal(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getBigDecimal(int, int)
       * @deprecated To remove Eclipse warning.
       */
      public BigDecimal getBigDecimal(int parameterIndex, int scale) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getBlob(int)
       */
      public Blob getBlob(int i) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getBlob(java.lang.String)
       */
      public Blob getBlob(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getBoolean(int)
       */
      public boolean getBoolean(int parameterIndex) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getBoolean(java.lang.String)
       */
      public boolean getBoolean(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getByte(int)
       */
      public byte getByte(int parameterIndex) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getByte(java.lang.String)
       */
      public byte getByte(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getBytes(int)
       */
      public byte[] getBytes(int parameterIndex) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getBytes(java.lang.String)
       */
      public byte[] getBytes(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getClob(int)
       */
      public Clob getClob(int i) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getClob(java.lang.String)
       */
      public Clob getClob(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getDate(int)
       */
      public Date getDate(int parameterIndex) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getDate(java.lang.String)
       */
      public Date getDate(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getDate(int, java.util.Calendar)
       */
      public Date getDate(int parameterIndex, Calendar cal) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getDate(java.lang.String, java.util.Calendar)
       */
      public Date getDate(String parameterName, Calendar cal) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getDouble(int)
       */
      public double getDouble(int parameterIndex) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getDouble(java.lang.String)
       */
      public double getDouble(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getFloat(int)
       */
      public float getFloat(int parameterIndex) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getFloat(java.lang.String)
       */
      public float getFloat(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getInt(int)
       */
      public int getInt(int parameterIndex) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getInt(java.lang.String)
       */
      public int getInt(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getLong(int)
       */
      public long getLong(int parameterIndex) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getLong(java.lang.String)
       */
      public long getLong(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getObject(int)
       */
      public Object getObject(int parameterIndex) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getObject(java.lang.String)
       */
      public Object getObject(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getObject(int, java.util.Map)
       */
      public Object getObject(int arg0, Map arg1) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getObject(java.lang.String, java.util.Map)
       */
      public Object getObject(String arg0, Map arg1) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getRef(int)
       */
      public Ref getRef(int i) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getRef(java.lang.String)
       */
      public Ref getRef(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getShort(int)
       */
      public short getShort(int parameterIndex) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getShort(java.lang.String)
       */
      public short getShort(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getString(int)
       */
      public String getString(int parameterIndex) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getString(java.lang.String)
       */
      public String getString(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getTime(int)
       */
      public Time getTime(int parameterIndex) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getTime(java.lang.String)
       */
      public Time getTime(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getTime(int, java.util.Calendar)
       */
      public Time getTime(int parameterIndex, Calendar cal) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getTime(java.lang.String, java.util.Calendar)
       */
      public Time getTime(String parameterName, Calendar cal) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getTimestamp(int)
       */
      public Timestamp getTimestamp(int parameterIndex) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getTimestamp(java.lang.String)
       */
      public Timestamp getTimestamp(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getTimestamp(int, java.util.Calendar)
       */
      public Timestamp getTimestamp(int parameterIndex, Calendar cal) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getTimestamp(java.lang.String, java.util.Calendar)
       */
      public Timestamp getTimestamp(String parameterName, Calendar cal) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getURL(int)
       */
      public URL getURL(int parameterIndex) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#getURL(java.lang.String)
       */
      public URL getURL(String parameterName) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * Initialize the object pair representing the parameter at nIndex.
       * @param nPraramId The parameter buffer to initialize (1-based).
       * @return the offset of the parameter metadata group.
       * @throws SQLException On invalid nPraramId.
       */
      protected int initParam(int nPraramId) throws SQLException
      {
         if (nPraramId < 1 || nPraramId > m_nParamCount)
         {
            throw new SQLException("Undefined parameter " + nPraramId + " in " + m_sSQL + ".");
         }

         int nParamOffset = (nPraramId - 1) * PARAM_META_SIZE;

         if (m_paramArray.length < nPraramId * PARAM_META_SIZE)
         {
            Object[] paramArray = new Object[MathUtil.ceil2(nParamOffset + PARAM_META_SIZE)];

            System.arraycopy(m_paramArray, 0, paramArray, 0, m_paramArray.length);
            m_paramArray = paramArray;
         }

         m_paramArray[nParamOffset + PARAM_TYPE] = null;
         m_paramArray[nParamOffset + PARAM_VALUE] = null;

         return nParamOffset;
      }

      /**
       * Mark the positions of 'sNeedle' within 'sHaystack' to markArray.
       * Checks that existing value at index of sNeedle is negative.
       * Places the desired mark at index of sNeedle and through remaining sNeedle length.
       * @param sHaystack The String where to search for sNeedle.
       * @param sNeedle The string to search for in sHaystack.
       * @param nMark The initial mark to use.
       * @param bIncrement If nMark should be incremented after every use.
       * @param markArray The array to mark.
       * @return Number of sNeedle found.
       * @throws SQLException if a position has already been previously marked with a
       *                      non-negative value at any point for the length of sNeedle.
       */
      protected int markTokens(
         String sHaystack, String sNeedle, int nMark, boolean bIncrement, int[] markArray)
         throws SQLException
      {
         int nLength = Math.max(1, sNeedle.length()); // avoid infinite loop for empty token
         int nCount = 0;

         for (int nPos = -1; (nPos = nextUnquotedToken(sHaystack, sNeedle, nPos)) >= 0;)
         {
            // isJavaIdentifierPart() seems to be generic enough for finding token boundaries
            if ((nPos > 0 && Character.isJavaIdentifierPart(sHaystack.charAt(nPos - 1))) ||
                (nPos + nLength < sHaystack.length() &&
                 Character.isJavaIdentifierPart(sHaystack.charAt(nPos + nLength))))
            {
               nPos += nLength; // advance past token

               continue; // sNeedle is a substring of token at nPos e.g. :2 and :20 or 2 and 22
            }

            for (int nEnd = nPos + nLength; nPos < nEnd; ++nPos)
            {
               if (markArray[nPos] >= 0) // check if another parameter already reserved offset
               {
                  throw new SQLException("Parameter " + nMark + " with token " +  sNeedle +
                                         " overlap at offset " + nPos + ".");
               }

               markArray[nPos] = nMark; // mark as used
            }

            ++nCount;

            if (bIncrement)
            {
               ++nMark; // increment mark for next use if requested
            }
         }

         return nCount;
      }

      /**
       * Find next occurrence of sNeedle in sHaystack starting from nStart.
       * The algorithm assumes character at nStart is not part of a quoted literal.
       * @param sHaystack The string where to search for the substring.
       * @param sNeedle The substring to search for.
       * @param nStart The starting position of the search.
       * @return Starting position of sNeedle that is not quoted, or -1 if not found. 
       */
      protected int nextUnquotedToken(String sHaystack, String sNeedle, int nStart)
      {
         boolean bQuoted = false;

         for (int i = Math.max(0, nStart), nCount = sHaystack.length(); i < nCount; ++i)
         {
            char ch = sHaystack.charAt(i);

            if (bQuoted && ch == '\'')
            {
               bQuoted = false;
            }
            else if (!bQuoted && ch == sNeedle.charAt(0) && sHaystack.startsWith(sNeedle, i))
            {
               return i;
            }
            else if (!bQuoted && ch == '\'')
            {
               bQuoted = true;
            }
         }

         return -1;
      }

      /**
       * @see java.sql.CallableStatement#registerOutParameter(int, int)
       */
      public void registerOutParameter(int parameterIndex, int sqlType) throws SQLException
      {
         // NOOP
      }

      /**
       * @see java.sql.CallableStatement#registerOutParameter(java.lang.String, int)
       */
      public void registerOutParameter(String parameterName, int sqlType) throws SQLException
      {
         // NOOP
      }

      /**
       * @see java.sql.CallableStatement#registerOutParameter(int, int, int)
       */
      public void registerOutParameter(int parameterIndex, int sqlType, int scale)
         throws SQLException
      {
         // NOOP
      }

      /**
       * @see java.sql.CallableStatement#registerOutParameter(int, int, java.lang.String)
       */
      public void registerOutParameter(int paramIndex, int sqlType, String typeName)
         throws SQLException
      {
         // NOOP
      }

      /**
       * @see java.sql.CallableStatement#registerOutParameter(java.lang.String, int, int)
       */
      public void registerOutParameter(String parameterName, int sqlType, int scale)
         throws SQLException
      {
         // NOOP
      }

      /**
       * @see java.sql.CallableStatement#registerOutParameter(java.lang.String, int, java.lang.String)
       */
      public void registerOutParameter(String parameterName, int sqlType, String typeName)
         throws SQLException
      {
         // NOOP
      }

      /**
       * @see java.sql.CallableStatement#setAsciiStream(java.lang.String, java.io.InputStream, int)
       */
      public void setAsciiStream(String parameterName, InputStream x, int nLength)
         throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setBigDecimal(java.lang.String, java.math.BigDecimal)
       */
      public void setBigDecimal(String parameterName, BigDecimal x) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setBinaryStream(java.lang.String, java.io.InputStream, int)
       */
      public void setBinaryStream(String parameterName, InputStream x, int nLength)
         throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setBoolean(java.lang.String, boolean)
       */
      public void setBoolean(String parameterName, boolean x) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setByte(java.lang.String, byte)
       */
      public void setByte(String parameterName, byte x) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setBytes(java.lang.String, byte[])
       */
      public void setBytes(String parameterName, byte[] x) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setCharacterStream(java.lang.String, java.io.Reader, int)
       */
      public void setCharacterStream(String parameterName, Reader reader, int nLength)
         throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setDate(java.lang.String, java.sql.Date)
       */
      public void setDate(String parameterName, Date x) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setDate(java.lang.String, java.sql.Date, java.util.Calendar)
       */
      public void setDate(String parameterName, Date x, Calendar cal) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setDouble(java.lang.String, double)
       */
      public void setDouble(String parameterName, double x) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setFloat(java.lang.String, float)
       */
      public void setFloat(String parameterName, float x) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setInt(java.lang.String, int)
       */
      public void setInt(String parameterName, int x) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setLong(java.lang.String, long)
       */
      public void setLong(String parameterName, long x) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setNull(java.lang.String, int)
       */
      public void setNull(String parameterName, int sqlType) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setNull(java.lang.String, int, java.lang.String)
       */
      public void setNull(String parameterName, int sqlType, String typeName)
         throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setObject(java.lang.String, java.lang.Object)
       */
      public void setObject(String parameterName, Object x) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setObject(java.lang.String, java.lang.Object, int)
       */
      public void setObject(String parameterName, Object x, int targetSqlType)
         throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setObject(java.lang.String, java.lang.Object, int, int)
       */
      public void setObject(String parameterName, Object x, int targetSqlType, int scale)
         throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setShort(java.lang.String, short)
       */
      public void setShort(String parameterName, short x) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setString(java.lang.String, java.lang.String)
       */
      public void setString(String parameterName, String x) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setTime(java.lang.String, java.sql.Time)
       */
      public void setTime(String parameterName, Time x) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setTime(java.lang.String, java.sql.Time, java.util.Calendar)
       */
      public void setTime(String parameterName, Time x, Calendar cal) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setTimestamp(java.lang.String, java.sql.Timestamp)
       */
      public void setTimestamp(String parameterName, Timestamp x) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setTimestamp(java.lang.String, java.sql.Timestamp, java.util.Calendar)
       */
      public void setTimestamp(String parameterName, Timestamp x, Calendar cal)
         throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#setURL(java.lang.String, java.net.URL)
       */
      public void setURL(String parameterName, URL val) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.CallableStatement#wasNull()
       */
      public boolean wasNull() throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.PreparedStatement#addBatch()
       */
      public void addBatch() throws SQLException
      {
         ++m_nBatchCount; // new incoming batch group

         if (m_batchArray == null)
         {
            m_batchArray = new Object[2][]; // create new array
         }

         if (m_batchArray.length < m_nBatchCount)
         {
            Object[][] batchArray = new Object[m_batchArray.length << 1][];

            System.arraycopy(m_batchArray, 0, batchArray, 0, m_batchArray.length); // grow the array
            m_batchArray = batchArray;
         }

         Object[] paramArray = m_batchArray[m_nBatchCount - 1]; // reuse previously allocated array

         m_batchArray[m_nBatchCount - 1] = m_paramArray; // store values for batch group
         m_paramArray = (paramArray == null) ? new Object[0] : paramArray;
      }

      /**
       * @see java.sql.PreparedStatement#clearParameters()
       */
      public void clearParameters() throws SQLException
      {
         // NOOP
      }

      /**
       * @see java.sql.PreparedStatement#execute()
       */
      public boolean execute() throws SQLException
      {
         StringBuffer buf = new StringBuffer(); 
         int nStart = 0;

         for (int i = 0; m_markArray[i] > -1; i += MARKER_META_SIZE)
         {
            int nParamID = m_markArray[i + MARKER_ID];
            int nMarkerOffset = m_markArray[i + MARKER_OFFSET];
            int nArgOffset = nParamID * PARAM_META_SIZE;
            Primitive type;

            if (m_paramArray.length < nArgOffset + PARAM_META_SIZE ||
                (type = (Primitive)m_paramArray[nArgOffset + PARAM_TYPE]) == null)
            {
               throw new SQLException("Parameter " + (nParamID + 1) + " not set.");
            }

            buf.append(m_sSQL.substring(nStart, nMarkerOffset));
            m_adapter.appendLiteral(buf, type, type.convert(m_paramArray[nArgOffset + PARAM_VALUE]));
            nStart = nMarkerOffset + m_markArray[i + MARKER_LENGTH];
         }

         buf.append(m_sSQL.substring(nStart)); // remainder of query
         buf.append(m_schemaManager.getSeparator()); // end of statement token
         append(buf);
         Arrays.fill(m_paramArray, null); // free memory pointers

         return false;
      }

      /**
       * @see java.sql.PreparedStatement#executeQuery()
       */
      public ResultSet executeQuery() throws SQLException
      {
         if (m_bQueryWrittenThough)
         {
            execute();
         }

         return new EmptyResultSet(); // Java API requires non-null return value
      }

      /**
       * @see java.sql.PreparedStatement#executeUpdate()
       */
      public int executeUpdate() throws SQLException
      {
         execute();

         return 0;
      }

      /**
       * @see java.sql.PreparedStatement#getMetaData()
       */
      public ResultSetMetaData getMetaData() throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.PreparedStatement#getParameterMetaData()
       */
      public ParameterMetaData getParameterMetaData() throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.PreparedStatement#setArray(int, java.sql.Array)
       */
      public void setArray(int i, Array x) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.PreparedStatement#setAsciiStream(int, java.io.InputStream, int)
       */
      public void setAsciiStream(int nParameterIndex, InputStream istream, int nLength)
         throws SQLException
      {
         setBinaryStream(nParameterIndex, istream, nLength);
      }

      /**
       * @see java.sql.PreparedStatement#setBigDecimal(int, java.math.BigDecimal)
       */
      public void setBigDecimal(int nParameterIndex, BigDecimal dec) throws SQLException
      {
         int nOffset = initParam(nParameterIndex);

         m_paramArray[nOffset + PARAM_TYPE] = Primitive.DECIMAL;
         m_paramArray[nOffset + PARAM_VALUE] = dec;
      }

      /**
       * @see java.sql.PreparedStatement#setBinaryStream(int, java.io.InputStream, int)
       */
      public void setBinaryStream(int nParameterIndex, InputStream istream, int nLength)
         throws SQLException
      {
         byte[] buf = null;

         if (istream != null)
         {
            int nRead = 0;

            buf = new byte[nLength];

            try
            {
               for (int i;
                    (i = istream.read(buf, nRead, nLength - nRead)) >= 0 &&
                    (nRead += i) < nLength;);
            }
            catch (IOException e)
            {
               throw new SQLException(e.getMessage()); // conform to API's use of SQLException
            }

            if (nRead != nLength)
            {
               throw new SQLException("Truncated stream.");
            }
         }

         setBytes(nParameterIndex, buf);
      }

      /**
       * @see java.sql.PreparedStatement#setBlob(int, java.sql.Blob)
       */
      public void setBlob(int nParameterIndex, Blob blob) throws SQLException
      {
         setBinaryStream(nParameterIndex, // Binary can handle at most arrays of int size
                         (blob == null) ? null : blob.getBinaryStream(),
                         (blob == null) ? 0 : (int)blob.length());
      }

      /**
       * @see java.sql.PreparedStatement#setBoolean(int, boolean)
       */
      public void setBoolean(int nParameterIndex, boolean b) throws SQLException
      {
         int nOffset = initParam(nParameterIndex);

         m_paramArray[nOffset + PARAM_TYPE] = Primitive.BOOLEAN;
         m_paramArray[nOffset + PARAM_VALUE] = Boolean.valueOf(b);
      }

      /**
       * @see java.sql.PreparedStatement#setByte(int, byte)
       */
      public void setByte(int nParameterIndex, byte n) throws SQLException
      {
         setBytes(nParameterIndex, new byte[]{n});
      }

      /**
       * @see java.sql.PreparedStatement#setBytes(int, byte[])
       */
      public void setBytes(int nParameterIndex, byte[] data) throws SQLException
      {
         int nOffset = initParam(nParameterIndex);

         m_paramArray[nOffset + PARAM_TYPE] = Primitive.BINARY;
         m_paramArray[nOffset + PARAM_VALUE] = (data == null) ? null : new Binary(data);
      }

      /**
       * @see java.sql.PreparedStatement#setCharacterStream(int, java.io.Reader, int)
       */
      public void setCharacterStream(int nParameterIndex, Reader reader, int nLength)
         throws SQLException
      {
         char[] buf = null;

         if (reader != null)
         {
            int nRead = 0;

            buf = new char[nLength];

            try
            {
               for (int i;
                    (i = reader.read(buf, nRead, nLength - nRead)) >= 0 &&
                    (nRead += i) < nLength;);
            }
            catch (IOException e)
            {
               throw new SQLException(e.getMessage()); // conform to API's use of SQLException
            }

            if (nRead != nLength)
            {
               throw new SQLException("Truncated stream.");
            }
         }

         setString(nParameterIndex, (buf == null) ? null : String.valueOf(buf));
      }

      /**
       * @see java.sql.PreparedStatement#setClob(int, java.sql.Clob)
       */
      public void setClob(int nParameterIndex, Clob clob) throws SQLException
      {
         setCharacterStream(nParameterIndex, // String can handle at most arrays of int size
                            (clob == null) ? null : clob.getCharacterStream(),
                            (clob == null) ? 0 : (int)clob.length());
      }

      /**
       * @see java.sql.PreparedStatement#setDate(int, java.sql.Date)
       */
      public void setDate(int nParameterIndex, Date dt) throws SQLException
      {
         setTimestamp(nParameterIndex, (dt == null) ? null : new Timestamp(dt.getTime()));
      }

      /**
       * @see java.sql.PreparedStatement#setDate(int, java.sql.Date, java.util.Calendar)
       */
      public void setDate(int nParameterIndex, Date dt, Calendar cal) throws SQLException
      {
         setTimestamp(
            nParameterIndex,
            (dt == null) ? null
                        : new Timestamp(dt.getTime() +
                                        ((cal == null)
                                        ? TimeZone.getDefault()
                                        : cal.getTimeZone()).getOffset(dt.getTime())));
      }

      /**
       * @see java.sql.PreparedStatement#setDouble(int, double)
       */
      public void setDouble(int nParameterIndex, double d) throws SQLException
      {
         int nOffset = initParam(nParameterIndex);

         m_paramArray[nOffset + PARAM_TYPE] = Primitive.DOUBLE;
         m_paramArray[nOffset + PARAM_VALUE] = Primitive.createDouble(d);
      }

      /**
       * @see java.sql.PreparedStatement#setFloat(int, float)
       */
      public void setFloat(int nParameterIndex, float f) throws SQLException
      {
         int nOffset = initParam(nParameterIndex);

         m_paramArray[nOffset + PARAM_TYPE] = Primitive.FLOAT;
         m_paramArray[nOffset + PARAM_VALUE] = Primitive.createFloat(f);
      }

      /**
       * @see java.sql.PreparedStatement#setInt(int, int)
       */
      public void setInt(int nParameterIndex, int n) throws SQLException
      {
         int nOffset = initParam(nParameterIndex);

         m_paramArray[nOffset + PARAM_TYPE] = Primitive.INTEGER;
         m_paramArray[nOffset + PARAM_VALUE] = Primitive.createInteger(n);
      }

      /**
       * @see java.sql.PreparedStatement#setLong(int, long)
       */
      public void setLong(int nParameterIndex, long l) throws SQLException
      {
         int nOffset = initParam(nParameterIndex);

         m_paramArray[nOffset + PARAM_TYPE] = Primitive.LONG;
         m_paramArray[nOffset + PARAM_VALUE] = Primitive.createLong(l);
      }

      /**
       * @see java.sql.PreparedStatement#setNull(int, int)
       */
      public void setNull(int nParameterIndex, int nSQLType) throws SQLException
      {
         int nOffset = initParam(nParameterIndex);

         m_paramArray[nOffset + PARAM_TYPE] = Primitive.ANY; // hopefully type doesn't matter
         m_paramArray[nOffset + PARAM_VALUE] = null;
      }

      /**
       * @see java.sql.PreparedStatement#setNull(int, int, java.lang.String)
       */
      public void setNull(int nParameterIndex, int nSQLType, String sTypeName) throws SQLException
      {
         setNull(nParameterIndex, nSQLType);
      }

      /**
       * @see java.sql.PreparedStatement#setObject(int, java.lang.Object)
       */
      public void setObject(int nParameterIndex, Object obj) throws SQLException
      {
         int nOffset = initParam(nParameterIndex);
         Primitive type = Primitive.primitiveOf(obj);

         // hopefully type doesn't matter for null
         m_paramArray[nOffset + PARAM_TYPE] = (type == null) ? Primitive.ANY : type;
         m_paramArray[nOffset + PARAM_VALUE] = obj;
      }

      /**
       * @see java.sql.PreparedStatement#setObject(int, java.lang.Object, int)
       */
      public void setObject(int nParameterIndex, Object obj, int nTargetSQLType) throws SQLException
      {
         setObject(nParameterIndex, obj);
      }

      /**
       * @see java.sql.PreparedStatement#setObject(int, java.lang.Object, int, int)
       */
      public void setObject(int nParameterIndex, Object obj, int nTargetSQLType, int nScale)
         throws SQLException
      {
         setObject(nParameterIndex, obj);
      }

      /**
       * @see java.sql.PreparedStatement#setRef(int, java.sql.Ref)
       */
      public void setRef(int i, Ref ref) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.PreparedStatement#setShort(int, short)
       */
      public void setShort(int nParameterIndex, short n) throws SQLException
      {
         setInt(nParameterIndex, n);
      }

      /**
       * @see java.sql.PreparedStatement#setString(int, java.lang.String)
       */
      public void setString(int nParameterIndex, String s) throws SQLException
      {
         int nOffset = initParam(nParameterIndex);

         m_paramArray[nOffset + PARAM_TYPE] = Primitive.STRING;
         m_paramArray[nOffset + PARAM_VALUE] = s;
      }

      /**
       * @see java.sql.PreparedStatement#setTime(int, java.sql.Time)
       */
      public void setTime(int nParameterIndex, Time tm) throws SQLException
      {
         setTimestamp(nParameterIndex, (tm == null) ? null : new Timestamp(tm.getTime()));
      }

      /**
       * @see java.sql.PreparedStatement#setTime(int, java.sql.Time, java.util.Calendar)
       */
      public void setTime(int nParameterIndex, Time tm, Calendar cal) throws SQLException
      {
         setTimestamp(
            nParameterIndex,
            (tm == null) ? null
                        : new Timestamp(tm.getTime() +
                                        ((cal == null)
                                        ? TimeZone.getDefault()
                                        : cal.getTimeZone()).getOffset(tm.getTime())));
      }

      /**
       * @see java.sql.PreparedStatement#setTimestamp(int, java.sql.Timestamp)
       */
      public void setTimestamp(int nParameterIndex, Timestamp ts) throws SQLException
      {
         int nOffset = initParam(nParameterIndex);

         m_paramArray[nOffset + PARAM_TYPE] = Primitive.TIMESTAMP;
         m_paramArray[nOffset + PARAM_VALUE] = ts;
      }

      /**
       * @see java.sql.PreparedStatement#setTimestamp(int, java.sql.Timestamp, java.util.Calendar)
       */
      public void setTimestamp(int nParameterIndex, Timestamp ts, Calendar cal)
         throws SQLException
      {
         setTimestamp(
            nParameterIndex,
            (ts == null) ? null
                        : new Timestamp(ts.getTime() +
                                        ((cal == null)
                                        ? TimeZone.getDefault()
                                        : cal.getTimeZone()).getOffset(ts.getTime())));
      }

      /**
       * @see java.sql.PreparedStatement#setURL(int, java.net.URL)
       */
      public void setURL(int nParameterIndex, URL url) throws SQLException
      {
         setString(nParameterIndex, (url == null) ? null : url.toString());
      }

      /**
       * @see java.sql.PreparedStatement#setUnicodeStream(int, java.io.InputStream, int)
       * @deprecated To remove Eclipse warning.
       */
      public void setUnicodeStream(int nParameterIndex, InputStream istream, int nLength)
         throws SQLException
      {
         try
         {
            // UTF-16 stream is 1/2 size because every char == 2 bytes
            setCharacterStream(null, new InputStreamReader(istream, "UTF-16"), nLength >> 1);
         }
         catch (UnsupportedEncodingException e)
         {
            throw new SQLException(e.getMessage()); // conform to API's use of SQLException
         }
      }

      /**
       * @see java.sql.Statement#addBatch(java.lang.String)
       */
      public void addBatch(String sSQL) throws SQLException
      {
         execute(sSQL);
      }

      /**
       * @see java.sql.Statement#cancel()
       */
      public void cancel() throws SQLException
      {
         // NOOP
      }

      /**
       * @see java.sql.Statement#clearBatch()
       */
      public void clearBatch() throws SQLException
      {
         // NOOP
      }

      /**
       * @see java.sql.Statement#clearWarnings()
       */
      public void clearWarnings() throws SQLException
      {
         // NOOP
      }

      /**
       * @see java.sql.Statement#close()
       */
      public void close() throws SQLException
      {
         Arrays.fill(m_paramArray, null); // free memory pointers
      }

      /**
       * @see java.sql.Statement#execute(java.lang.String)
       */
      public boolean execute(String sSQL) throws SQLException
      {
         append(sSQL);
         append(m_schemaManager.getSeparator());

         return false;
      }

      /**
       * @see java.sql.Statement#execute(java.lang.String, int)
       */
      public boolean execute(String sSQL, int nAutoGeneratedKeys) throws SQLException
      {
         return execute(sSQL);
      }

      /**
       * @see java.sql.Statement#execute(java.lang.String, int[])
       */
      public boolean execute(String sSQL, int[] columnIndexes) throws SQLException
      {
         return execute(sSQL);
      }

      /**
       * @see java.sql.Statement#execute(java.lang.String, java.lang.String[])
       */
      public boolean execute(String sSQL, String[] columnNames) throws SQLException
      {
         return execute(sSQL);
      }

      /**
       * @see java.sql.Statement#executeBatch()
       */
      public int[] executeBatch() throws SQLException
      {
         if (m_nBatchCount > 0)
         {
            Object[] paramArray = m_paramArray; // non-batched array of values

            for (int i = 0; i < m_nBatchCount; ++i)
            {
               m_paramArray = m_batchArray[i]; // set paramArray for next execution
               execute();
            }

            m_paramArray = paramArray; // restore array
         }

         m_nBatchCount = 0;

         return null;
      }

      /**
       * @see java.sql.Statement#executeQuery(java.lang.String)
       */
      public ResultSet executeQuery(String sSQL) throws SQLException
      {
         if (m_bQueryWrittenThough)
         {
            execute(sSQL);
         }

         return new EmptyResultSet(); // Java API requires non-null return value
      }

      /**
       * @see java.sql.Statement#executeUpdate(java.lang.String)
       */
      public int executeUpdate(String sSQL) throws SQLException
      {
         execute(sSQL);

         return 0;
      }

      /**
       * @see java.sql.Statement#executeUpdate(java.lang.String, int)
       */
      public int executeUpdate(String sSQL, int autoGeneratedKeys) throws SQLException
      {
         return executeUpdate(sSQL);
      }

      /**
       * @see java.sql.Statement#executeUpdate(java.lang.String, int[])
       */
      public int executeUpdate(String sSQL, int[] columnIndexes) throws SQLException
      {
         return executeUpdate(sSQL);
      }

      /**
       * @see java.sql.Statement#executeUpdate(java.lang.String, java.lang.String[])
       */
      public int executeUpdate(String sSQL, String[] columnNames) throws SQLException
      {
         return executeUpdate(sSQL);
      }

      /**
       * @see java.sql.Statement#getConnection()
       */
      public Connection getConnection() throws SQLException
      {
         return SQLWriterConnection.this;
      }

      /**
       * @see java.sql.Statement#getFetchDirection()
       */
      public int getFetchDirection() throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.Statement#getFetchSize()
       */
      public int getFetchSize() throws SQLException
      {
         return 0;
      }

      /**
       * @see java.sql.Statement#getGeneratedKeys()
       */
      public ResultSet getGeneratedKeys() throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.Statement#getMaxFieldSize()
       */
      public int getMaxFieldSize() throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.Statement#getMaxRows()
       */
      public int getMaxRows() throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.Statement#getMoreResults()
       */
      public boolean getMoreResults() throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.Statement#getMoreResults(int)
       */
      public boolean getMoreResults(int current) throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.Statement#getQueryTimeout()
       */
      public int getQueryTimeout() throws SQLException
      {
         return 0;
      }

      /**
       * @see java.sql.Statement#getResultSet()
       */
      public ResultSet getResultSet() throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.Statement#getResultSetConcurrency()
       */
      public int getResultSetConcurrency() throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.Statement#getResultSetHoldability()
       */
      public int getResultSetHoldability() throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.Statement#getResultSetType()
       */
      public int getResultSetType() throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.Statement#getUpdateCount()
       */
      public int getUpdateCount() throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.Statement#getWarnings()
       */
      public SQLWarning getWarnings() throws SQLException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.sql.Statement#setCursorName(java.lang.String)
       */
      public void setCursorName(String name) throws SQLException
      {
         // NOOP
      }

      /**
       * @see java.sql.Statement#setEscapeProcessing(boolean)
       */
      public void setEscapeProcessing(boolean enable) throws SQLException
      {
         // NOOP
      }

      /**
       * @see java.sql.Statement#setFetchDirection(int)
       */
      public void setFetchDirection(int direction) throws SQLException
      {
         // NOOP
      }

      /**
       * @see java.sql.Statement#setFetchSize(int)
       */
      public void setFetchSize(int rows) throws SQLException
      {
         // NOOP
      }

      /**
       * @see java.sql.Statement#setMaxFieldSize(int)
       */
      public void setMaxFieldSize(int max) throws SQLException
      {
         // NOOP
      }

      /**
       * @see java.sql.Statement#setMaxRows(int)
       */
      public void setMaxRows(int max) throws SQLException
      {
         // NOOP
      }

      /**
       * @see java.sql.Statement#setQueryTimeout(int)
       */
      public void setQueryTimeout(int seconds) throws SQLException
      {
         // NOOP
      }

      /**
       * ResultSet that is always empty.
       */
      protected class EmptyResultSet implements ResultSet
      {
         /**
          * @see java.sql.ResultSet#absolute(int)
          */
         public boolean absolute(int nRow) throws SQLException
         {
            return false;
         }

         /**
          * @see java.sql.ResultSet#afterLast()
          */
         public void afterLast() throws SQLException
         {
            // NOOP
         }

         /**
          * @see java.sql.ResultSet#beforeFirst()
          */
         public void beforeFirst() throws SQLException
         {
            // NOOP
         }

         /**
          * @see java.sql.ResultSet#cancelRowUpdates()
          */
         public void cancelRowUpdates() throws SQLException
         {
            // NOOP
         }

         /**
          * @see java.sql.ResultSet#clearWarnings()
          */
         public void clearWarnings() throws SQLException
         {
            // NOOP
         }

         /**
          * @see java.sql.ResultSet#close()
          */
         public void close() throws SQLException
         {
            // NOOP
         }

         /**
          * @see java.sql.ResultSet#deleteRow()
          */
         public void deleteRow() throws SQLException
         {
            // NOOP
         }

         /**
          * @see java.sql.ResultSet#findColumn(java.lang.String)
          */
         public int findColumn(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#first()
          */
         public boolean first() throws SQLException
         {
            return false;
         }

         /**
          * @see java.sql.ResultSet#getArray(int)
          */
         public Array getArray(int n) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getArray(java.lang.String)
          */
         public Array getArray(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getAsciiStream(int)
          */
         public InputStream getAsciiStream(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getAsciiStream(java.lang.String)
          */
         public InputStream getAsciiStream(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getBigDecimal(int)
          */
         public BigDecimal getBigDecimal(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getBigDecimal(java.lang.String)
          */
         public BigDecimal getBigDecimal(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getBigDecimal(int, int)
          * @deprecated
          */
         public BigDecimal getBigDecimal(int nColumnIndex, int nScale) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getBigDecimal(java.lang.String, int)
          * @deprecated
          */
         public BigDecimal getBigDecimal(String sColumnName, int nScale) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getBinaryStream(int)
          */
         public InputStream getBinaryStream(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getBinaryStream(java.lang.String)
          */
         public InputStream getBinaryStream(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getBlob(int)
          */
         public Blob getBlob(int n) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getBlob(java.lang.String)
          */
         public Blob getBlob(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getBoolean(int)
          */
         public boolean getBoolean(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getBoolean(java.lang.String)
          */
         public boolean getBoolean(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getByte(int)
          */
         public byte getByte(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getByte(java.lang.String)
          */
         public byte getByte(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getBytes(int)
          */
         public byte[] getBytes(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getBytes(java.lang.String)
          */
         public byte[] getBytes(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getCharacterStream(int)
          */
         public Reader getCharacterStream(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getCharacterStream(java.lang.String)
          */
         public Reader getCharacterStream(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getClob(int)
          */
         public Clob getClob(int n) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getClob(java.lang.String)
          */
         public Clob getClob(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getConcurrency()
          */
         public int getConcurrency() throws SQLException
         {
            return ResultSet.CONCUR_READ_ONLY;
         }

         /**
          * @see java.sql.ResultSet#getCursorName()
          */
         public String getCursorName() throws SQLException
         {
            return null;
         }

         /**
          * @see java.sql.ResultSet#getDate(int)
          */
         public Date getDate(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getDate(java.lang.String)
          */
         public Date getDate(String nColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getDate(int, java.util.Calendar)
          */
         public Date getDate(int nColumnIndex, Calendar cal) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getDate(java.lang.String, java.util.Calendar)
          */
         public Date getDate(String sColumnName, Calendar cal) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getDouble(int)
          */
         public double getDouble(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getDouble(java.lang.String)
          */
         public double getDouble(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getFetchDirection()
          */
         public int getFetchDirection() throws SQLException
         {
            return ResultSet.FETCH_UNKNOWN;
         }

         /**
          * @see java.sql.ResultSet#getFetchSize()
          */
         public int getFetchSize() throws SQLException
         {
            return 0;
         }

         /**
          * @see java.sql.ResultSet#getFloat(int)
          */
         public float getFloat(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getFloat(java.lang.String)
          */
         public float getFloat(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getInt(int)
          */
         public int getInt(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getInt(java.lang.String)
          */
         public int getInt(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getLong(int)
          */
         public long getLong(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getLong(java.lang.String)
          */
         public long getLong(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getMetaData()
          */
         public ResultSetMetaData getMetaData() throws SQLException
         {
            return null;
         }

         /**
          * @see java.sql.ResultSet#getObject(int)
          */
         public Object getObject(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getObject(java.lang.String)
          */
         public Object getObject(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getObject(int, java.util.Map)
          */
         public Object getObject(int nArg0, Map arg1) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getObject(java.lang.String, java.util.Map)
          */
         public Object getObject(String sArg0, Map arg1) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getRef(int)
          */
         public Ref getRef(int n) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getRef(java.lang.String)
          */
         public Ref getRef(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getRow()
          */
         public int getRow() throws SQLException
         {
            return 0;
         }

         /**
          * @see java.sql.ResultSet#getShort(int)
          */
         public short getShort(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getShort(java.lang.String)
          */
         public short getShort(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getStatement()
          */
         public Statement getStatement() throws SQLException
         {
            return SQLWriterStatement.this;
         }

         /**
          * @see java.sql.ResultSet#getString(int)
          */
         public String getString(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getString(java.lang.String)
          */
         public String getString(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getTime(int)
          */
         public Time getTime(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getTime(java.lang.String)
          */
         public Time getTime(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getTime(int, java.util.Calendar)
          */
         public Time getTime(int nColumnIndex, Calendar cal) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getTime(java.lang.String, java.util.Calendar)
          */
         public Time getTime(String sColumnName, Calendar cal) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getTimestamp(int)
          */
         public Timestamp getTimestamp(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getTimestamp(java.lang.String)
          */
         public Timestamp getTimestamp(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getTimestamp(int, java.util.Calendar)
          */
         public Timestamp getTimestamp(int nColumnIndex, Calendar cal) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getTimestamp(java.lang.String, java.util.Calendar)
          */
         public Timestamp getTimestamp(String sColumnName, Calendar cal) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getType()
          */
         public int getType() throws SQLException
         {
            return ResultSet.TYPE_FORWARD_ONLY;
         }

         /**
          * @see java.sql.ResultSet#getURL(int)
          */
         public URL getURL(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getURL(java.lang.String)
          */
         public URL getURL(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getUnicodeStream(int)
          * @deprecated
          */
         public InputStream getUnicodeStream(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getUnicodeStream(java.lang.String)
          * @deprecated
          */
         public InputStream getUnicodeStream(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#getWarnings()
          */
         public SQLWarning getWarnings() throws SQLException
         {
            return null;
         }

         /**
          * @see java.sql.ResultSet#insertRow()
          */
         public void insertRow() throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#isAfterLast()
          */
         public boolean isAfterLast() throws SQLException
         {
            return false;
         }

         /**
          * @see java.sql.ResultSet#isBeforeFirst()
          */
         public boolean isBeforeFirst() throws SQLException
         {
            return false;
         }

         /**
          * @see java.sql.ResultSet#isFirst()
          */
         public boolean isFirst() throws SQLException
         {
            return false;
         }

         /**
          * @see java.sql.ResultSet#isLast()
          */
         public boolean isLast() throws SQLException
         {
            return false;
         }

         /**
          * @see java.sql.ResultSet#last()
          */
         public boolean last() throws SQLException
         {
            return false;
         }

         /**
          * @see java.sql.ResultSet#moveToCurrentRow()
          */
         public void moveToCurrentRow() throws SQLException
         {
            // NOOP
         }

         /**
          * @see java.sql.ResultSet#moveToInsertRow()
          */
         public void moveToInsertRow() throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#next()
          */
         public boolean next() throws SQLException
         {
            return false;
         }

         /**
          * @see java.sql.ResultSet#previous()
          */
         public boolean previous() throws SQLException
         {
            return false;
         }

         /**
          * @see java.sql.ResultSet#refreshRow()
          */
         public void refreshRow() throws SQLException
         {
            // NOOP
         }

         /**
          * @see java.sql.ResultSet#relative(int)
          */
         public boolean relative(int nRows) throws SQLException
         {
            return false;
         }

         /**
          * @see java.sql.ResultSet#rowDeleted()
          */
         public boolean rowDeleted() throws SQLException
         {
            return false;
         }

         /**
          * @see java.sql.ResultSet#rowInserted()
          */
         public boolean rowInserted() throws SQLException
         {
            return false;
         }

         /**
          * @see java.sql.ResultSet#rowUpdated()
          */
         public boolean rowUpdated() throws SQLException
         {
            return false;
         }

         /**
          * @see java.sql.ResultSet#setFetchDirection(int)
          */
         public void setFetchDirection(int nDirection) throws SQLException
         {
            // NOOP
         }

         /**
          * @see java.sql.ResultSet#setFetchSize(int)
          */
         public void setFetchSize(int nRows) throws SQLException
         {
            // NOOP
         }

         /**
          * @see java.sql.ResultSet#updateArray(int, java.sql.Array)
          */
         public void updateArray(int nColumnIndex, Array x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateArray(java.lang.String, java.sql.Array)
          */
         public void updateArray(String sColumnName, Array x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateAsciiStream(int, java.io.InputStream, int)
          */
         public void updateAsciiStream(int nColumnIndex, InputStream x, int nLength) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateAsciiStream(java.lang.String, java.io.InputStream, int)
          */
         public void updateAsciiStream(String sColumnName, InputStream x, int nLength) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateBigDecimal(int, java.math.BigDecimal)
          */
         public void updateBigDecimal(int nColumnIndex, BigDecimal x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateBigDecimal(java.lang.String, java.math.BigDecimal)
          */
         public void updateBigDecimal(String sColumnName, BigDecimal x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateBinaryStream(int, java.io.InputStream, int)
          */
         public void updateBinaryStream(int nColumnIndex, InputStream x, int nLength) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateBinaryStream(java.lang.String, java.io.InputStream, int)
          */
         public void updateBinaryStream(String sColumnName, InputStream x, int nLength) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateBlob(int, java.sql.Blob)
          */
         public void updateBlob(int nColumnIndex, Blob x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateBlob(java.lang.String, java.sql.Blob)
          */
         public void updateBlob(String sColumnName, Blob x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateBoolean(int, boolean)
          */
         public void updateBoolean(int nColumnIndex, boolean b) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateBoolean(java.lang.String, boolean)
          */
         public void updateBoolean(String sColumnName, boolean b) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateByte(int, byte)
          */
         public void updateByte(int nColumnIndex, byte n) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateByte(java.lang.String, byte)
          */
         public void updateByte(String sColumnName, byte n) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateBytes(int, byte[])
          */
         public void updateBytes(int nColumnIndex, byte[] nArray) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateBytes(java.lang.String, byte[])
          */
         public void updateBytes(String sColumnName, byte[] nArray) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateCharacterStream(int, java.io.Reader, int)
          */
         public void updateCharacterStream(int nColumnIndex, Reader x, int nLength) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateCharacterStream(java.lang.String, java.io.Reader, int)
          */
         public void updateCharacterStream(String nColumnName, Reader reader, int nLength) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateClob(int, java.sql.Clob)
          */
         public void updateClob(int nColumnIndex, Clob x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateClob(java.lang.String, java.sql.Clob)
          */
         public void updateClob(String sColumnName, Clob x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateDate(int, java.sql.Date)
          */
         public void updateDate(int nColumnIndex, Date x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateDate(java.lang.String, java.sql.Date)
          */
         public void updateDate(String sColumnName, Date x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateDouble(int, double)
          */
         public void updateDouble(int nColumnIndex, double d) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateDouble(java.lang.String, double)
          */
         public void updateDouble(String sColumnName, double d) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateFloat(int, float)
          */
         public void updateFloat(int nColumnIndex, float f) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateFloat(java.lang.String, float)
          */
         public void updateFloat(String sColumnName, float f) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateInt(int, int)
          */
         public void updateInt(int nColumnIndex, int n) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateInt(java.lang.String, int)
          */
         public void updateInt(String sColumnName, int n) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateLong(int, long)
          */
         public void updateLong(int nColumnIndex, long n) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateLong(java.lang.String, long)
          */
         public void updateLong(String sColumnName, long n) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateNull(int)
          */
         public void updateNull(int nColumnIndex) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateNull(java.lang.String)
          */
         public void updateNull(String sColumnName) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateObject(int, java.lang.Object)
          */
         public void updateObject(int nColumnIndex, Object x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateObject(java.lang.String, java.lang.Object)
          */
         public void updateObject(String sColumnName, Object x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateObject(int, java.lang.Object, int)
          */
         public void updateObject(int nColumnIndex, Object x, int nScale) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateObject(java.lang.String, java.lang.Object, int)
          */
         public void updateObject(String sColumnName, Object x, int nScale) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateRef(int, java.sql.Ref)
          */
         public void updateRef(int nColumnIndex, Ref x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateRef(java.lang.String, java.sql.Ref)
          */
         public void updateRef(String sColumnName, Ref x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateRow()
          */
         public void updateRow() throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateShort(int, short)
          */
         public void updateShort(int nColumnIndex, short n) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateShort(java.lang.String, short)
          */
         public void updateShort(String sColumnName, short n) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateString(int, java.lang.String)
          */
         public void updateString(int nColumnIndex, String s) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateString(java.lang.String, java.lang.String)
          */
         public void updateString(String sColumnName, String s) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateTime(int, java.sql.Time)
          */
         public void updateTime(int nColumnIndex, Time x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateTime(java.lang.String, java.sql.Time)
          */
         public void updateTime(String sColumnName, Time x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateTimestamp(int, java.sql.Timestamp)
          */
         public void updateTimestamp(int nColumnIndex, Timestamp x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#updateTimestamp(java.lang.String, java.sql.Timestamp)
          */
         public void updateTimestamp(String sColumnName, Timestamp x) throws SQLException
         {
            throw new SQLException();
         }

         /**
          * @see java.sql.ResultSet#wasNull()
          */
         public boolean wasNull() throws SQLException
         {
            throw new SQLException();
         }
      }
   }
}