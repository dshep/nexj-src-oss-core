// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.net.URL;
import java.sql.Array;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.Date;
import java.sql.ParameterMetaData;
import java.sql.PreparedStatement;
import java.sql.Ref;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.Calendar;

import nexj.core.util.sql.StatementWrapper;

public abstract class PreparedStatementWrapper extends StatementWrapper implements PreparedStatement
{
   /**
    * Constructor.
    * @param stmt The statement to wrap (not null).
    */
   public PreparedStatementWrapper(PreparedStatement stmt)
   {
      super(stmt);
   }

   /**
    * @see java.sql.PreparedStatement#addBatch()
    */
   public void addBatch() throws SQLException
   {
      getStatement().addBatch();
   }

   /**
    * @see java.sql.PreparedStatement#clearParameters()
    */
   public void clearParameters() throws SQLException
   {
      getStatement().clearParameters();
   }

   /**
    * @see nexj.core.rpc.sql.ra.SQLConnection.SQLStatement#close()
    */
   public void close() throws SQLException
   {
     getStatement().close();
   }

   /**
    * @see java.sql.PreparedStatement#execute()
    */
   public boolean execute() throws SQLException
   {
      return getStatement().execute();
   }
   /**
    * @see java.sql.PreparedStatement#executeQuery()
    */
   public ResultSet executeQuery() throws SQLException
   {
      return getStatement().executeQuery();
   }

   /**
    * @see java.sql.PreparedStatement#executeUpdate()
    */
   public int executeUpdate() throws SQLException
   {
      return getStatement().executeUpdate();
   }

   /**
    * @see java.sql.PreparedStatement#getMetaData()
    */
   public ResultSetMetaData getMetaData() throws SQLException
   {
      return getStatement().getMetaData();
   }

   /**
    * @see java.sql.PreparedStatement#getParameterMetaData()
    */
   public ParameterMetaData getParameterMetaData() throws SQLException
   {
      return getStatement().getParameterMetaData();
   }

   /**
    * @return The wrapped prepared statement.
    */
   public abstract PreparedStatement getStatement();

   /**
    * @see java.sql.PreparedStatement#setArray(int, java.sql.Array)
    */
   public void setArray(int nParameterIndex, Array x) throws SQLException
   {
      getStatement().setArray(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setAsciiStream(int, java.io.InputStream, int)
    */
   public void setAsciiStream(int nParameterIndex, InputStream x, int nLength) throws SQLException
   {
      getStatement().setAsciiStream(nParameterIndex, x, nLength);
   }

   /**
    * @see java.sql.PreparedStatement#setBigDecimal(int, java.math.BigDecimal)
    */
   public void setBigDecimal(int nParameterIndex, BigDecimal x) throws SQLException
   {
      getStatement().setBigDecimal(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setBinaryStream(int, java.io.InputStream, int)
    */
   public void setBinaryStream(int nParameterIndex, InputStream x, int nLength) throws SQLException
   {
      getStatement().setBinaryStream(nParameterIndex, x, nLength);
   }

   /**
    * @see java.sql.PreparedStatement#setBlob(int, java.sql.Blob)
    */
   public void setBlob(int nParameterIndex, Blob x) throws SQLException
   {
      getStatement().setBlob(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setBoolean(int, boolean)
    */
   public void setBoolean(int nParameterIndex, boolean x) throws SQLException
   {
      getStatement().setBoolean(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setByte(int, byte)
    */
   public void setByte(int nParameterIndex, byte x) throws SQLException
   {
      getStatement().setByte(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setBytes(int, byte[])
    */
   public void setBytes(int nParameterIndex, byte[] x) throws SQLException
   {
      getStatement().setBytes(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setCharacterStream(int, java.io.Reader, int)
    */
   public void setCharacterStream(int nParameterIndex, Reader reader, int nLength) throws SQLException
   {
      getStatement().setCharacterStream(nParameterIndex, reader, nLength);
   }

   /**
    * @see java.sql.PreparedStatement#setClob(int, java.sql.Clob)
    */
   public void setClob(int nParameterIndex, Clob x) throws SQLException
   {
      getStatement().setClob(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setDate(int, java.sql.Date)
    */
   public void setDate(int nParameterIndex, Date x) throws SQLException
   {
      getStatement().setDate(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setDate(int, java.sql.Date, java.util.Calendar)
    */
   public void setDate(int nParameterIndex, Date x, Calendar cal) throws SQLException
   {
      getStatement().setDate(nParameterIndex, x, cal);
   }

   /**
    * @see java.sql.PreparedStatement#setDouble(int, double)
    */
   public void setDouble(int nParameterIndex, double x) throws SQLException
   {
      getStatement().setDouble(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setFloat(int, float)
    */
   public void setFloat(int nParameterIndex, float x) throws SQLException
   {
      getStatement().setFloat(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setInt(int, int)
    */
   public void setInt(int nParameterIndex, int x) throws SQLException
   {
      getStatement().setInt(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setLong(int, long)
    */
   public void setLong(int nParameterIndex, long x) throws SQLException
   {
      getStatement().setLong(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setNull(int, int)
    */
   public void setNull(int nParameterIndex, int nSQLType) throws SQLException
   {
      getStatement().setNull(nParameterIndex, nSQLType);
   }

   /**
    * @see java.sql.PreparedStatement#setNull(int, int, java.lang.String)
    */
   public void setNull(int nParameterIndex, int nSQLType, String sTypeName) throws SQLException
   {
      getStatement().setNull(nParameterIndex, nSQLType,sTypeName);
   }

   /**
    * @see java.sql.PreparedStatement#setObject(int, java.lang.Object)
    */
   public void setObject(int nParameterIndex, Object x) throws SQLException
   {
      getStatement().setObject(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setObject(int, java.lang.Object, int)
    */
   public void setObject(int nParameterIndex, Object x, int nTargetSQLType) throws SQLException
   {
      getStatement().setObject(nParameterIndex, x, nTargetSQLType);
   }

   /**
    * @see java.sql.PreparedStatement#setObject(int, java.lang.Object, int, int)
    */
   public void setObject(int nParameterIndex, Object x, int nTargetSQLType, int nScale) throws SQLException
   {
      getStatement().setObject(nParameterIndex, x, nTargetSQLType, nScale);
   }

   /**
    * @see java.sql.PreparedStatement#setRef(int, java.sql.Ref)
    */
   public void setRef(int nParameterIndex, Ref x) throws SQLException
   {
      getStatement().setRef(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setShort(int, short)
    */
   public void setShort(int nParameterIndex, short x) throws SQLException
   {
      getStatement().setShort(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setString(int, java.lang.String)
    */
   public void setString(int nParameterIndex, String x) throws SQLException
   {
      getStatement().setString(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setTime(int, java.sql.Time)
    */
   public void setTime(int nParameterIndex, Time x) throws SQLException
   {
      getStatement().setTime(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setTime(int, java.sql.Time, java.util.Calendar)
    */
   public void setTime(int nParameterIndex, Time x, Calendar cal) throws SQLException
   {
      getStatement().setTime(nParameterIndex, x, cal);
   }

   /**
    * @see java.sql.PreparedStatement#setTimestamp(int, java.sql.Timestamp)
    */
   public void setTimestamp(int nParameterIndex, Timestamp x) throws SQLException
   {
      getStatement().setTimestamp(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setTimestamp(int, java.sql.Timestamp, java.util.Calendar)
    */
   public void setTimestamp(int nParameterIndex, Timestamp x, Calendar cal) throws SQLException
   {
      getStatement().setTimestamp(nParameterIndex, x, cal);
   }

   /**
    * @see java.sql.PreparedStatement#setURL(int, java.net.URL)
    */
   public void setURL(int nParameterIndex, URL x) throws SQLException
   {
      getStatement().setURL(nParameterIndex, x);
   }

   /**
    * @see java.sql.PreparedStatement#setUnicodeStream(int, java.io.InputStream, int)
    * @deprecated
    */
   public void setUnicodeStream(int nParameterIndex, InputStream x, int nLength) throws SQLException
   {
      getStatement().setUnicodeStream(nParameterIndex, x, nLength);
   }
}