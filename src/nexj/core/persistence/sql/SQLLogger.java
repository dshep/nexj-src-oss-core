package nexj.core.persistence.sql;

/**
 * Interface implemented by SQL loggers.
 */
public interface SQLLogger
{
   /**
    * @return True if logging is enabled.
    */
   boolean isLogging();

   /**
    * Logs a message.
    * @param message The message to log.
    */
   void log(Object message);

   /**
    * Logs a bind value.
    * @param nOrdinal The bind value ordinal number.
    * @param value The bind value.
    */
   void logBindValue(int nOrdinal, Object value);
   
   /**
    * Logs a batch start.
    */
   void logBatch(int nOrdinal);
}
