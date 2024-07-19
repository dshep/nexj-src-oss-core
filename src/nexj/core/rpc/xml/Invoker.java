// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.util.Locale;

import nexj.core.rpc.Request;
import nexj.core.rpc.Server;

/**
 * Server invocation adapter.
 */
public abstract class Invoker
{
   /**
    * The request object.
    */
   protected Request m_request = new Request();

   // operations

   /**
    * Sets the request locale.
    * @param locale The request locale to set.
    */
   public void setLocale(Locale locale)
   {
      m_request.setLocale(locale);
   }

   /**
    * @return The request locale.
    */
   public Locale getLocale()
   {
      return m_request.getLocale();
   }

   /**
    * Invokes the server.
    * @param server The server.
    * @return The invocation result.
    */
   public abstract Object invoke(Server server);
}
