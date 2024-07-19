// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.RelationalDatabaseFragment;
import nexj.core.meta.persistence.sql.RelationalMapping;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.persistence.OID;
import nexj.core.persistence.OIDGenerator;
import nexj.core.persistence.PersistenceAdapter;
import nexj.core.runtime.Instance;
import nexj.core.runtime.UnitOfWork;
import nexj.core.scripting.Pair;

/**
 * GUID OID generator.
 */
public class MaxPlus1Generator implements OIDGenerator
{
   // constants

   /**
    * Value count for metadata validation.
    * Used by the metadata loader.
    */
   public final static int VALUE_COUNT = 1;

   /**
    * Local map key part.
    */
   protected final static Object KEY = new Object(); 

   // operations

   /**
    * @see nexj.core.persistence.OIDGenerator#generateOID(Instance, nexj.core.persistence.PersistenceAdapter)
    */
   public OID generateOID(Instance instance, PersistenceAdapter adapter)
   {
      Table table = ((RelationalMapping)instance.getPersistenceMapping()).getPrimaryTable();
      UnitOfWork uow = instance.getUnitOfWork();
      Object key = new Pair(table, KEY);
      Object count = uow.getCachedLocal(key);

      if (count == null)
      {
         Column column = table.getPrimaryKey().getIndexColumn(0).getColumn();
         SQLAdapter sqlAdapter = (SQLAdapter)adapter;
         StringBuffer buf = new StringBuffer(128);

         buf.append("select max(");
         sqlAdapter.appendColumn(buf, column);
         buf.append(") + 1 from ");
         sqlAdapter.appendTable(buf, table);

         SQLConnection connection = null;
         PreparedStatement stmt = null;
         ResultSet rs = null;

         try
         {
            connection = sqlAdapter.getConnection((RelationalDatabaseFragment)instance.getFragment(), true);
            stmt = connection.getConnection().prepareStatement(buf.toString());
            rs = sqlAdapter.executeQuery(stmt);
            rs.next();

            count = rs.getObject(1);

            if (count == null)
            {
               count = Primitive.ONE_INTEGER;
            }

            count = column.getType().convert(count);
         }
         catch (SQLException e)
         {
            throw sqlAdapter.getException(e, null, 0, 0);
         }
         finally
         {
            if (rs != null)
            {
               try
               {
                  rs.close();
               }
               catch (SQLException e)
               {
               }
            }

            if (stmt != null)
            {
               try
               {
                  stmt.close();
               }
               catch (SQLException e)
               {
               }
            }

            if (connection != null)
            {
               connection.decRef();
            }
         }
      }
      else
      {
         count = Primitive.add(count, Primitive.ONE_INTEGER);
      }

      uow.cacheLocal(key, count);

      return new OID(new Object[]{count});
   }
}
