package nexj.core.persistence.sql;

/**
 * Counts the number of select statements issued in an adapter.
 */
public class ReadCountHook implements SQLHook
{
   // attributes

   /**
    * The select statement count.
    */
   protected int m_nReadCount;

   // operations

   /**
    * @return The number of select statements issued.
    */
   public synchronized int getReadCount()
   {
      return m_nReadCount;
   }

   /**
    * @see nexj.core.persistence.sql.SQLHook#inspect(java.lang.String)
    */
   public String inspect(String sSQL)
   {
      if (sSQL.startsWith("select"))
      {
         synchronized (this)
         {
            m_nReadCount++;
         }
      }

      return sSQL;
   }

   /**
    * @see nexj.core.persistence.sql.SQLHook#modify(nexj.core.persistence.sql.SQLHook.Batch)
    */
   public void modify(Batch batch)
   {
   }
}
