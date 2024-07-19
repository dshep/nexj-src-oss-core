// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.runtime.InvocationContextHolder;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * Local (in-process) server wrapper, implementing system error handling.
 */
public class LocalServer implements Server, InvocationContextAware, InvocationContextHolder
{
   // associations

   /**
    * The wrapped server.
    */
   protected Server m_server;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(LocalServer.class);

   // constructors

   /**
    * Constructs the wrapper.
    * @param server The server to wrap.
    */
   public LocalServer(Server server)
   {
      assert server != null;

      m_server = server;
   }

   /**
    * Constructs the wrapper.
    * setServer() has to be invoked afterwards.
    */
   public LocalServer()
   {
   }

   // operations

   /**
    * Sets the wrapped server.
    * @param server The wrapped server to set.
    */
   public void setServer(Server server)
   {
      m_server = server;
   }

   /**
    * @return The wrapped server.
    */
   public Server getServer()
   {
      return m_server;
   }

   /**
    * @see nexj.core.rpc.Server#invoke(nexj.core.rpc.Request)
    */
   public Response invoke(Request request)
   {
      try
      {
         return m_server.invoke(request);
      }
      catch (Throwable t)
      {
         throw ObjUtil.rethrow(RPCUtil.handleException(t, ObjUtil.isSystem(t), "local", s_logger));
      }
   }

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      if (m_server instanceof InvocationContextAware)
      {
         ((InvocationContextAware)m_server).setInvocationContext(context);
      }
   }

   /**
    * @see nexj.core.runtime.InvocationContextHolder#getInvocationContext()
    */
   public InvocationContext getInvocationContext()
   {
      if (m_server instanceof InvocationContextHolder)
      {
         return ((InvocationContextHolder)m_server).getInvocationContext();
      }

      return null;
   }
}
