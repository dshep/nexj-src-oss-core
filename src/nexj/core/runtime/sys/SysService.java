// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.sys;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.Sender;
import nexj.core.integration.io.ObjectOutput;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessageTable;
import nexj.core.meta.integration.service.Send;
import nexj.core.meta.integration.service.Service;
import nexj.core.meta.workflow.Flow;
import nexj.core.meta.workflow.State;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.ActionContext;
import nexj.core.runtime.Instance;
import nexj.core.runtime.SecurityViolationException;
import nexj.core.scripting.Pair;
import nexj.core.util.GUIDUtil;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;

/**
 * Messaging service implementation.
 */
public class SysService extends SysWorkflow
{
   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(SysService.class);
   
   /**
    * @see nexj.core.runtime.sys.SysWorkflow#getFlow(java.lang.String, int)
    */
   protected Flow getFlow(String sName, int nVersion)
   {
      return m_context.getMetadata().getService(sName, nVersion);
   }

   /**
    * @see nexj.core.runtime.sys.SysWorkflow#getFlow(java.lang.String)
    */
   protected Flow getFlow(String sName)
   {
      return m_context.getMetadata().getService(sName);
   }

   /**
    * @see nexj.core.runtime.sys.SysWorkflow#findFlow(nexj.core.meta.Metaclass, java.lang.String, java.lang.Number, nexj.core.runtime.ActionContext)
    */
   public Flow findFlow(Metaclass metaclass, String sName, Number version, ActionContext actx)
   {
      return m_context.getMetadata().findService(sName, version.intValue());
   }

   /**
    * Starts the service flow.
    */
   public Instance invoke(Metaclass metaclass, String sName, Object message, String sOutput, Pair args, ActionContext actx) throws IntegrationException
   {
      Service service = (Service)getFlow(sName);
      boolean bSecure = m_context.isSecure();

      if (bSecure && service.getPrivilege() != null &&
         !m_context.getPrivilegeSet().contains(service.getPrivilege()))
      {
         throw new SecurityViolationException("err.integration.service.unauthorized",
            new Object[]{service.getFullName()});
      }

      if (service.getInterface() != null)
      {
         MessageTable table = service.getInterface().getRequestTable();
         
         if (!(message instanceof TransferObject) ||
            table.getMessageCount() != 0 && table.findMessage(((TransferObject)message).getClassName()) == null)
         {
            throw new IntegrationException("err.integration.service.inputMessage", new Object[]{service.getFullName()});
         }
      }

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Invoking " + service);
      }

      Instance instance = null;

      try
      {
         m_context.setSecure(false);
         instance = new Instance(metaclass, m_context);

         State state = new State(service, true);

         state.setReservedValue(0, instance);
         state.setReservedValue(1, state);
         state.setValue(Service.OUTPUT, sOutput);
         state.setToken(service, message);

         int i = 0;

         for (Pair pair = args; pair != null; pair = pair.getNext())
         {
            if (i >= service.getArgumentCount())
            {
               i = -1;
               break;
            }
            
            state.setValue(service.getArgument(i++), pair.getHead());
         }

         if (i != service.getArgumentCount())
         {
            throw new IntegrationException("err.integration.service.argCount",
               new Object[]{Primitive.createInteger(Pair.length(args)),
                  Primitive.createInteger(service.getArgumentCount())});
         }

         instance.setNew();
         instance.setValue("name", service.getName());
         instance.setValue("version", Primitive.createInteger(service.getVersion()));
         instance.setValue("oid", EMPTY_BINARY);
         instance.setValue("class", "");
         instance.setValue("local", GUIDUtil.generateGUID());
         instance.setValue("object", null);
         instance.setValue("state", state);
         instance.setValue("serializedState", null);
         instance.setValue("serializedVariables", null);
         instance.invoke("create");
         m_context.setSecure(bSecure);
         instance.invoke("run");
      }
      catch (Throwable e)
      {
         if (instance != null && instance.getState() != Instance.INIT)
         {
            m_context.setSecure(false);
            instance.invoke("delete");
         }

         ObjUtil.rethrow(e);
      }
      finally
      {
         m_context.setSecure(bSecure);
      }

      return instance;
   }

   /**
    * @see nexj.core.runtime.sys.SysWorkflow#complete(nexj.core.runtime.Instance, nexj.core.runtime.ActionContext)
    */
   public void complete(Instance instance, ActionContext actx)
   {
      State state = (State)instance.getValue("state");
      Service service = (Service)state.getFlow();
      String sChannel = (String)state.getValue(Service.OUTPUT);
      Channel channel = (StringUtil.isEmpty(sChannel)) ? null : m_context.getMetadata().getChannel(sChannel);
      Object value = state.getToken(null);

      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("Output message:");
         s_logger.dump(value);
      }
      
      if (service.getInterface() != null)
      {
         MessageTable responseTable = service.getInterface().getResponseTable();
         Message message = null;

         if (responseTable.getMessageCount() != 0)
         {
            if (!(value instanceof TransferObject) ||
               (message = responseTable.findMessage(((TransferObject)value).getClassName())) == null)
            {
               throw new IntegrationException("err.integration.service.outputMessage", new Object[]{service.getFullName()});
            }

            if (message.getFormat() == null)
            {
               message = null;
            }
         }
         else if (responseTable.getFormat() != null)
         {
            channel = null;
         }

         if (channel != null && message != null)
         {
            if (channel.getSender() == null)
            {
               throw new RPCException("err.rpc.notSender", new Object[]{channel.getName()});
            }

            Sender sender = (Sender)channel.getSender().getInstance(m_context);
            ObjectOutput output = sender.createOutput();
            TransferObject tobj = new TransferObject(1);

            Send.formatToOutput((TransferObject)value, message, m_context, output);
            tobj.setValue(Sender.BODY, output.getObject());
            value = tobj;
         }
      }

      if (channel != null)
      {
         if (!(value instanceof TransferObject))
         {
            throw new IntegrationException("err.integration.service.outputMessage", new Object[]{service.getFullName()});
         }

         m_context.getUnitOfWork().addMessage(channel, (TransferObject)value);
      }

      boolean bSecure = m_context.isSecure();
      
      try
      {
         m_context.setSecure(false);
         instance.invoke("delete");
      }
      finally
      {
         m_context.setSecure(bSecure);
      }
   }
}
