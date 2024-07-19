// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Interface implemented by HTTP servers.
 */
public interface HTTPServer
{
   /**
    * Invokes the HTTP server.
    * @param servlet The HTTP servlet.
    * @param request The HTTP request.
    * @param response The HTTP response.
    */
   void invoke(HttpServlet servlet, HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException;
}
