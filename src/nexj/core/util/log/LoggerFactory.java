// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.log;

import nexj.core.util.Logger;

/**
 * Interface to create logger implementations.
 */
public interface LoggerFactory
{
   /**
    * Creates a logger with a given name.
    * @param sName The logger name.
    * @return The logger instance.
    */
   Logger createLogger(String sName);
   
   /**
    * Pushes a thread diagnostic context token.
    * The logger outputs the entire context stack.
    * @param sToken The token to push and print in the log messages.
    * @return A cookie for resetting the context to its previous level.
    */
   int pushContext(String sToken);
   
   /**
    * Resets the thread diagnostic context to a previous level.
    * @param nCookie A cookie returned previously by pushContext.
    */
   void resetContext(int nCookie);
}
