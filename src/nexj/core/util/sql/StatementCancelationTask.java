// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.sql;

import java.sql.SQLException;
import java.sql.Statement;
import java.util.Timer;
import java.util.TimerTask;

/**
 * Timer task to cancel a statement.
 * Cancellation performed inside a synchronized block on the StatementCancelationTask.this object
 */
public class StatementCancelationTask extends TimerTask
{
   // associations

   /**
    * Timer object used mainly to launch statement cancellation tasks.
    */
   protected static Timer s_timer;

   /**
    * The statement to cancel (null == abort cancellation).
    */
   protected Statement m_stmt;

   //constructors

   /**
    * Constructor
    * @param stmt The statement to cancel.
    */
   protected StatementCancelationTask(Statement stmt)
   {
      m_stmt = stmt;
   }

   // operations

   /**
    * Cancels a statement timeout task.
    * @param task The task to cancel.
    * @return True if the task has not yet run.
    */
   public static boolean cancel(TimerTask task)
   {
      // abort task and ensure that the task does not get run a second time on timeout
      return (task == null) ? true : task.cancel();
   }

   /**
    * Schedules a statement timeout task.
    * @param stmt The statement to request the task for.
    * @return The task for the requested statement or null if statement requires no timeout.
    */
   public static TimerTask schedule(Statement stmt) throws SQLException
   {
      long nDelay = (long)stmt.getQueryTimeout();

      if (nDelay <= 0)
      {
         return null; // timeout not required
      }

      TimerTask task = new StatementCancelationTask(stmt);

      synchronized (StatementCancelationTask.class)
      {
         if (s_timer == null)
         {
            s_timer = new Timer(StatementCancelationTask.class.getName() + " timer", true);
         }
      }

      stmt.setQueryTimeout(0);
      s_timer.schedule(task, nDelay * 1000);

      return task;
   }

   /**
    * @see java.util.TimerTask#cancel()
    */
   public synchronized boolean cancel()
   {
      boolean bPending = (m_stmt != null);

      m_stmt = null;
      super.cancel();

      return bPending;
   }

   /**
    * @see java.util.TimerTask#run()
    */
   public synchronized void run()
   {
      if (m_stmt == null)
      {
         return; // already aborted
      }

      Statement stmt = m_stmt;

      m_stmt = null; // cancellation in progress/completed

      try
      {
         stmt.cancel();
      }
      catch (Throwable t)
      {
      }
   }
}