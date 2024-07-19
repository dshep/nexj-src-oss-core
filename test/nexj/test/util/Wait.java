package nexj.test.util;

/**
 * Encapsulates waiting logic.
 */
public abstract class Wait
{
   /**
    * @return True if the wait is over.
    */
   protected abstract boolean isOver();

   /**
    * Starts waiting.
    * @param nTimeout Timeout in milliseconds.
    * @return True if the wait is over, false if a timeout has occurred.
    */
   public boolean proceed(int nTimeout)
   {
      long lTimeout = nTimeout * 1000000L;
      long lStart = System.nanoTime();
      int nDelay = 10;

      while (!isOver())
      {
         long lTime = System.nanoTime() - lStart;

         if (lTime >= lTimeout)
         {
            return false;
         }

         try
         {
            Thread.sleep(Math.max(1, Math.min((lTimeout - lTime) / 1000000, nDelay)));
         }
         catch (InterruptedException e)
         {
         }

         nDelay = Math.min(nDelay << 1, 500);
      }

      return true;
   }
}
