// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import nexj.core.runtime.InvocationContext;

/**
 * Server interceptor.
 */
public interface Interceptor
{
   /**
    * Determines if the interceptor is enabled in this invocation context.
    * @param context The invocation context.
    */
   boolean isEnabled(InvocationContext context);

   /**
    * Intercepts the request.
    * @param request The request to intercept.
    */
   void interceptRequest(Request request);

   /**
    * Intercepts the response.
    * @param response The response to intercept.
    */
   void interceptResponse(Response response);
}
