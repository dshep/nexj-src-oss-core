// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.util.Calendar;

/**
 * Interface implemented by SQL interceptors.
 */
public interface SQLHook
{
   /**
    * Inspects an SQL statement.
    * @param sSQL The statement to inspect.
    * @return The new SQL statement to execute (in which case modify() will not be invoked),
    * or null to collect the bind parameters and invoke modify().
    */
   String inspect(String sSQL);

   /**
    * Modifies an SQL batch.
    * Invoked only if inspect() has returned
    * previously null for a given SQL statement.
    * @param batch The batch to modify.
    */
   void modify(Batch batch);

   // inner classes

   /**
    * Interface passed to the SQL hook implementation. 
    */
   interface Batch
   {
      /**
       * Sets the SQL statement.
       * @param sSQL The statement to set.
       */
      void setSQL(String sSQL);

      /**
       * @return The SQL statement.
       */
      String getSQL();

      /**
       * Sets a parameter type.
       * @param nOrdinal The parameter ordinal number (0-based).
       * @param nType The parameter type, one of java.sql.Types constants.
       */
      void setParamType(int nOrdinal, int nType);

      /**
       * Gets a parameter type.
       * @param nOrdinal The parameter ordinal number (0-based).
       * @return The parameter type, one of java.sql.Types constants.
       */
      int getParamType(int nOrdinal); 

      /**
       * Sets a parameter value.
       * @param nRow The batch row number (0-based).
       * @param nOrdinal The parameter ordinal number (0-based).
       * @param value The parameter value to set.
       */
      void setParamValue(int nRow, int nOrdinal, Object value);

      /**
       * Gets a parameter value.
       * @param nRow The batch row number (0-based).
       * @param nOrdinal The parameter ordinal number (0-based).
       * @return The parameter value.
       */
      Object getParamValue(int nRow, int nOrdinal);

      /**
       * Sets a parameter length or scale.
       * @param nRow The batch row number (0-based).
       * @param nOrdinal The parameter ordinal number (0-based).
       * @param nSize The parameter size.
       */
      void setParamSize(int nRow, int nOrdinal, int nSize);

      /**
       * Gets a parameter length or scale.
       * @param nRow The batch row number (0-based).
       * @param nOrdinal The parameter ordinal number (0-based).
       * @return The parameter length or scale, if relevant, otherwise Integer.MIN_VALUE.
       */
      int getParamSize(int nRow, int nOrdinal);

      /**
       * Sets a parameter calendar.
       * @param nRow The batch row number (0-based).
       * @param nOrdinal The parameter ordinal number (0-based).
       * @param calendar The calendar to set.
       */
      void setParamCalendar(int nRow, int nOrdinal, Calendar calendar);

      /**
       * Gets a parameter calendar.
       * @param nRow The batch row number (0-based).
       * @param nOrdinal The parameter ordinal number (0-based).
       * @return The parameter Calendar, if relevant, otherwise 0.
       */
      Calendar getParamCalendar(int nRow, int nOrdinal);

      /**
       * Sets the bind parameter count.
       * @param nCount The count to set.
       */
      void setParamCount(int nCount);

      /**
       * @return The bind parameter count.
       */
      int getParamCount();

      /**
       * Sets the batch size.
       * @param nSize The batch size to set.
       */
      void setBatchSize(int nSize);

      /**
       * @return The number of statements in the batch.
       */
      int getBatchSize();
   }
}
