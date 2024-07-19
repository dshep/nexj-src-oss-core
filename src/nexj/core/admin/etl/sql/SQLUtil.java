// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.admin.etl.sql;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.net.URL;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;

import nexj.core.util.IOUtil;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;
import nexj.core.util.URLUtil;
import nexj.core.util.XMLUtil;

/**
 * SQL script utilities.
 */
public class SQLUtil
{
   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(SQLUtil.class);
   
   /**
    * Prevents construction.
    */
   protected SQLUtil()
   {
   }
   
   /**
    * Executes an SQL script.
    * @param con The connection on which to execute the script.
    * @param url The URL containing the script.
    */
   public static void execute(Connection con, URL url) throws SQLException
   {
      Reader reader = null;
      String sSQL;

      try
      {
         StringWriter writer = new StringWriter(4096);

         reader = new InputStreamReader(URLUtil.openStream(url), XMLUtil.ENCODING);
         IOUtil.copy(writer, reader);
         sSQL = writer.toString();
      }
      catch (IOException e)
      {
         SQLException x = new SQLException(ObjUtil.getMessage(e));

         x.initCause(e);

         throw x;
      } 
      finally
      {
         IOUtil.close(reader);
      }

      execute(con, sSQL);
   }
   
   /**
    * Executes an SQL script.
    * @param con The connection on which to execute the script.
    * @param sSQL The SQL script.
    */
   public static void execute(Connection con, String sSQL) throws SQLException
   {
      boolean bOracle = con.getMetaData().getDatabaseProductName().equals("Oracle");
      String sCmd = "";
      int nStart = 0;
      int nEnd = 0;

      while (nStart < sSQL.length())
      {
         nEnd = sSQL.indexOf('\n', nStart);

         if (nEnd < 0)
         {
            nEnd = sSQL.length();
         }
         else
         {
            ++nEnd;
         }

         String sLine = sSQL.substring(nStart, nEnd);

         nStart = nEnd;

         while (sLine.length() > 0 && sLine.charAt(sLine.length() - 1) <= ' ')
         {
            sLine = sLine.substring(0, sLine.length()- 1);
         }

         boolean bTerm = false;

         sLine = sLine.trim();

         if (sLine.startsWith("--", 0))
         {
         }
         else if (sLine.endsWith(";") && (!bOracle || !sCmd.startsWith("create or replace trigger ")))
         {
            if (sCmd.length() > 0)
            {
               sCmd += " ";
            }

            sCmd += sLine.substring(0, sLine.length() - 1);
            bTerm = true;
         }
         else if (sLine.equalsIgnoreCase("go") || sLine.equals("/"))
         {
            bTerm = true;
         }
         else
         {
            if (sCmd.length() > 0)
            {
               sCmd += " ";
            }

            sCmd += sLine;
         }

         if (bTerm)
         {
            sCmd = sCmd.trim();

            if (sCmd.length() > 0)
            {
               executeStatement(con, sCmd);
               sCmd = "";
            }
         }
      }

      sCmd = sCmd.trim();

      if (sCmd.length() > 0)
      {
         executeStatement(con, sCmd);
      }
   }

   /**
    * Executes a single SQL statement.
    * @param con The connection on which to execute the SQL.
    * @param sSQL The SQL statement.
    */
   private static void executeStatement(Connection con, String sSQL) throws SQLException
   {
      Statement stmt = null;

      try
      {
         s_logger.dump(sSQL);
         stmt = con.createStatement();
         stmt.execute(sSQL);
      }
      catch (SQLException e)
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug(ObjUtil.getMessage(e));
         }

         if (!sSQL.substring(0, 5).equalsIgnoreCase("drop ") &&
            !sSQL.substring(0, 14).equalsIgnoreCase("create schema ") &&
            !sSQL.substring(0, 5).equalsIgnoreCase("exec "))
         {
            throw e;
         }
      }
      finally
      {
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
      }
   }
}
