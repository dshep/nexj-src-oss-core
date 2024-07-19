// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.PasswordAuthentication;
import java.net.URI;

import nexj.core.rpc.Request;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.text.TextMarshaller;
import nexj.core.rpc.text.TextUnmarshaller;
import nexj.core.tools.GenericTool;
import nexj.core.util.HTTP;
import nexj.core.util.HTTPClient;
import nexj.core.util.Logger;
import nexj.core.util.StringUtil;
import nexj.core.util.XMLUtil;
import nexj.core.util.auth.LoginException;
import nexj.core.util.auth.PasswordAuthenticationProvider;

/**
 * HTTP Test Client
 */
public class HTTPTestClient extends GenericTool
{
   // associations

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(HTTPTestClient.class);
   
   // operations

   /**
    * @see nexj.core.tools.GenericTool#execute(java.lang.String)
    */
   protected void execute(String sCommand) throws Exception
   {
      HTTPClient client = new HTTPClient();

      client.setPasswordProvider(new PasswordAuthenticationProvider()
      {
         public PasswordAuthentication getPasswordAuthentication()
         {
            return new PasswordAuthentication("nexjsa", "nexj".toCharArray());
         }

         public boolean isAuthenticationDeterministic()
         {
            return true;
         }
      });

      int nReqCount = Integer.parseInt(getProperty("req.count", "1"));

      final Request request = new Request();
      TransferObject tobj = new TransferObject();
      
      tobj.setClassName("TestMessage");
      tobj.setEventName("runTest");
      
      request.addInvocation(tobj);
      request.setAsync(StringUtil.parseBoolean(getProperty("async", "0")));

      for (int nReq = 0; nReq < nReqCount; ++ nReq)
      {
         Object response = client.invoke(new URI(getProperty("url", "http://localhost:8080/nexj/text")), HTTP.METHOD_POST,
            new HTTPClient.RequestHandler()
            {
               public void handleRequest(HTTPClient client, OutputStream ostream) throws IOException
               {
                  Writer writer = new OutputStreamWriter(ostream, XMLUtil.ENCODING);
   
                  new TextMarshaller(null).serialize(request, writer);
                  writer.close();
               }
            },
            new HTTPClient.ResponseHandler()
            {
               public Object handleResponse(HTTPClient client, InputStream istream) throws IOException
               {
                  int nResult = client.getResponseStatus();
   
                  if (nResult != HTTP.STATUS_OK)
                  {
                     RuntimeException e = null;
   
                     if (nResult == HTTP.STATUS_UNAUTHORIZED ||
                        nResult == HTTP.STATUS_FORBIDDEN)
                     {
                        e = new LoginException("err.auth.login");
                     }
   
                     if (e == null || e.getCause() == null)
                     {
                        RuntimeException x = new HTTPException(nResult, client.getResponseMessage());
   
                        if (e == null)
                        {
                           e = x;
                        }
                        else
                        {
                           e.initCause(x);
                        }
                     }
   
                     throw e;
                  }
   
                  return new TextUnmarshaller(null).deserialize(new InputStreamReader(istream, XMLUtil.ENCODING));
               }
            });

         s_logger.debug(response);
      }
   }

   /**
    * @see nexj.core.tools.GenericTool#getCommandUsage()
    */
   protected String[] getCommandUsage()
   {
      return null;
   }

   /**
    * @see nexj.core.tools.GenericTool#getOptionUsage()
    */
   protected String[] getOptionUsage()
   {
      return new String[]
      {
         "-Durl=<server url>",
         "-Duser=<user name>",
         "-Dpassword=<password>",
         "-Dasync=<true|false>",
         "-Dreq.count=<duplicate request count>",
      };
   }

   public static void main(String[] args)
   {
      new HTTPTestClient().run(args);
   }
}
