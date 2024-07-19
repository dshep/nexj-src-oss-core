// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

/**
 * Interface for intercepting web requests.
 */
public interface WebInterceptor
{
   /**
    * Invoked before the request processing starts.
    * @param server The web server.
    * @return True if the request handling has been overridden, false to continue.
    */
   boolean beginWebRequest(WebServer server);

   /**
    * Invoked after the request processing completes, if beginWebRequest() has returned false.
    * @param server The web server.
    */
   void endWebRequest(WebServer server);
}
