// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

/**
 * The entry point to the server.
 */
public interface Server
{
   // operations

   /**
    * Invokes the server with a specified request object
    * and returns a response object.
    * @param request The request object.
    * @return The response object.
    */
   public Response invoke(Request request);
}
