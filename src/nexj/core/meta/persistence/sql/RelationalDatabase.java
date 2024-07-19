// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.util.Iterator;

import nexj.core.meta.ContextMetadata;
import nexj.core.meta.persistence.DataSource;
import nexj.core.util.ExceptionHolder;

/**
 * Relational database data source.
 */
public class RelationalDatabase extends DataSource
{
   // attributes

   /**
    * The JNDI alias.
    */
   protected String m_sAlias;

   /**
    * The JDBC driver path.
    */
   protected String m_sPath;

   /**
    * The JDBC driver class name.
    */
   protected String m_sDriver;

   /**
    * The default indexspace name to override the value in schema.
    */
   protected String m_sIndexspaceName;

   /**
    * The default LOB space name to override the value in schema.
    */
   protected String m_sLongspaceName;

   /**
    * The default tablespace name to override the value in schema.
    */
   protected String m_sTablespaceName;

   /**
    * The page size of the database, in bytes. Defaults to 0 (use persistence adapter default).
    */
   protected int m_nPageSize;

   /**
    * The Unicode flag.
    */
   protected boolean m_bUnicode = true;

   /**
    * The literal binding flag.
    */
   protected boolean m_bLiteral;

   // constructors

   /**
    * Constructs the relational database metadata object.
    * @param sName The data source name.
    */
   public RelationalDatabase(String sName)
   {
      super(sName);
      addFragment(new RelationalDatabaseFragment());
   }

   // operations

   /**
    * Sets the JNDI alias.
    * @param sAlias The JNDI alias to set.
    */
   public void setAlias(String sAlias)
   {
      verifyNotReadOnly();
      m_sAlias = sAlias;
   }

   /**
    * @return The JNDI alias.
    */
   public String getAlias()
   {
      return m_sAlias;
   }

   /**
    * Sets the Unicode flag.
    * @param bUnicode The Unicode flag to set.
    */
   public void setUnicode(boolean bUnicode)
   {
      verifyNotReadOnly();
      m_bUnicode = bUnicode;
   }

   /**
    * @return The Unicode flag.
    */
   public boolean isUnicode()
   {
      return m_bUnicode;
   }

   /**
    * Sets the JDBC driver path.
    * @param sPath The JDBC driver path to set.
    */
   public void setPath(String sPath)
   {
      verifyNotReadOnly();
      m_sPath = sPath;
   }

   /**
    * @return The JDBC driver path.
    */
   public String getPath()
   {
      return m_sPath;
   }

   /**
    * Sets the JDBC driver class name.
    * @param sDriver The JDBC driver class name to set.
    */
   public void setDriver(String sDriver)
   {
      verifyNotReadOnly();
      m_sDriver = sDriver;
   }

   /**
    * @return The JDBC driver class name.
    */
   public String getDriver()
   {
      return m_sDriver;
   }

   /**
    * Sets the database page size.
    *
    * @param nPageSize The page size in bytes; 0 to make persistence adapter use default page size.
    */
   public void setPageSize(int nPageSize)
   {
      verifyNotReadOnly();
      m_nPageSize = nPageSize;
   }

   /**
    * Gets the database page size.
    * @return The database page size in bytes; 0 to use persistence adapter default.
    */
   public int getPageSize()
   {
      return m_nPageSize;
   }

   /**
    * Sets the literal binding flag.
    * @param bLiteral The literal binding flag to set.
    */
   public void setLiteral(boolean bLiteral)
   {
      verifyNotReadOnly();
      m_bLiteral = bLiteral;
   }

   /**
    * @return The literal binding flag.
    */
   public boolean isLiteral()
   {
      return m_bLiteral;
   }

   /**
    * Sets the default Indexspace name.
    * @param sIndexspace The default indexspace name (null == no override).
    */
   public void setIndexspaceName(String sIndexspaceName)
   {
      verifyNotReadOnly();
      m_sIndexspaceName = sIndexspaceName;
   }

   /**
    * Gets the indexspace override value.
    * @return The indexspace name override value.
    */
   public String getIndexspaceName()
   {
      return m_sIndexspaceName;
   }

   /**
    * Sets the default LOB space name.
    * @param sIndexspaceName The default longspace name (null == no override).
    */
   public void setLongspaceName(String sLongspaceName)
   {
      verifyNotReadOnly();
      m_sLongspaceName = sLongspaceName;
   }

   /**
    * Gets the LOB space override value.
    * @return The longspace name override value.
    */
   public String getLongspaceName()
   {
      return m_sLongspaceName;
   }

   /**
    * Sets the default Tablespace name.
    * @param sIndexspaceName The default tablespace name (null == no override).
    */
   public void setTablespaceName(String sTablespaceName)
   {
      verifyNotReadOnly();
      m_sTablespaceName = sTablespaceName;
   }

   /**
    * Gets the tablespace override value.
    * @return The tablespace name override value.
    */
   public String getTablespaceName()
   {
      return m_sTablespaceName;
   }

   /**
    * Sets the default properties for the supported drivers.
    * @param nContainer The container type, one of the J2EEUtil constants.
    */
   public void setDefaultProperties(int nContainer)
   {
      verifyNotReadOnly();

      if (m_sDriver == null && m_adapter != null)
      {
         if (m_adapter.getName().equals("MSSQL"))
         {
            m_sDriver = "net.sourceforge.jtds.jdbcx.JtdsDataSource";
         }
         else if (m_adapter.getName().startsWith("Oracle"))
         {
            m_sDriver = "oracle.jdbc.xa.client.OracleXADataSource";
         }
         else if (m_adapter.getName().equals("Sybase"))
         {
            m_sDriver = "com.sybase.jdbc3.jdbc.SybXADataSource";
         }
         else if (m_adapter.getName().equals("MySQL"))
         {
            m_sDriver = "com.mysql.jdbc.jdbc2.optional.MysqlXADataSource";
         }
         else if (m_adapter.getName().startsWith("DB2"))
         {
            m_sDriver = "com.ibm.db2.jcc.DB2XADataSource";
         }
         else if (m_adapter.getName().equals("Teradata"))
         {
            m_sDriver = "nexj.core.persistence.sql.TeradataDataSourceWrapper";
         }
         else if (m_adapter.getName().equals("PostgreSQL"))
         {
            m_sDriver = "org.postgresql.xa.PGXADataSource";
         }
      }

      for (Iterator itr = getFragmentIterator(); itr.hasNext();)
      {
         RelationalDatabaseFragment fragment = (RelationalDatabaseFragment)itr.next();

         if (!fragment.isDefault())
         {
            fragment.getPropertyHolder().addDefaultProperties(
               ((RelationalDatabaseFragment)getDefaultFragment()).getPropertyHolder());
         }
      }

      for (Iterator itr = getFragmentIterator(); itr.hasNext();)
      {
         ((RelationalDatabaseFragment)itr.next()).setDefaultProperties(nContainer);
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, nexj.core.util.ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      super.validate(metadata, warnings);

      // optional DataSources are never used for persistence, hence can have invalid schema
      if (isEnabled())
      {
         m_schema.validate(metadata, warnings);
      }
   }
}