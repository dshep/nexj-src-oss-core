package nexj.core.rpc;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * Server for processing character streams that contain generic RPC requests.
 */
public abstract class GenericCharacterStreamServer implements CharacterStreamServer, InvocationContextAware
{
   // associations

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The request server.
    */
   protected Server m_server;

   /**
    * The server logger.
    */
   protected Logger m_logger;

   // operations

   /**
    * @see nexj.core.rpc.CharacterStreamServer#invoke(java.io.Reader, java.io.Writer, nexj.core.rpc.Preprocessor)
    */
   public void invoke(Reader reader, Writer writer, Preprocessor preprocessor) throws RPCException
   {
      Request request;

      try
      {
         request = getRequest(reader);

         if (preprocessor != null)
         {
            preprocessor.preprocess(request);
         }
      }
      catch (RPCException e)
      {
         throw e;
      }
      catch (Throwable t)
      {
         throw new RequestException("err.rpc." + getType() + ".request", t);
      }

      CharacterStreamMarshaller marshaller = getResponseMarshaller(request);
      Response response;

      try
      {
         response = m_server.invoke(request);
      }
      catch (Throwable t)
      {
         try
         {
            marshaller.serialize(RPCUtil.handleException(t, ObjUtil.isSystem(t), getType() , getLogger()), writer);
         }
         catch (RPCException e)
         {
            throw e;
         }
         catch (Throwable x)
         {
            throw new ServerException("err.rpc." + getType() + ".serialize", x);
         }

         return;
      }

      try
      {
         marshaller.serialize(response, writer);
      }
      catch (RPCException e)
      {
         throw e;
      }
      catch (Throwable t)
      {
         throw new ServerException("err.rpc." + getType() + ".serialize", t);
      }
   }

   /**
    * Sets the invocation context.
    * @param context The invocation context to set.
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * @return The invocation context.
    */
   public InvocationContext getInvocationContext()
   {
      return m_context;
   }

   /**
    * Sets the generic server.
    * @param server The generic server to set.
    */
   public void setServer(Server server)
   {
      m_server = server;
   }

   /**
    * @return The generic server.
    */
   public Server getServer()
   {
      return m_server;
   }

   /**
    * @return The Logger for the CharacterStream Server
    */
   protected Logger getLogger()
   {
      return (m_logger == null) ? m_logger = Logger.getLogger(getClass()) : m_logger;
   }

   /**
    * Gets the Request from the character stream.
    * @param reader The reader to read the serialized request.
    * @return The deserialized request.
    * @throws IOException If an I/O error occurs.
    */
   protected Request getRequest(Reader reader) throws IOException
   {
      return (Request)getRequestUnmarshaller().deserialize(reader);
   }

   /**
    * Gets the marshaller to create the Response.
    * @return The Response marshaller.
    */
   protected abstract CharacterStreamMarshaller getResponseMarshaller(Request request);

   /**
    * Gets the unmarshaller to deserialize the Request.
    * @return The Request unmarshaller.
    */
   protected abstract CharacterStreamUnmarshaller getRequestUnmarshaller();

   /**
    * Gets the name of the request type.
    * @return The request type name.
    */
   protected abstract String getType();
}
