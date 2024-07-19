// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms;

import nexj.core.meta.Component;
import nexj.core.meta.Metaclass;
import nexj.core.rpc.Request;
import nexj.core.rpc.Response;
import nexj.core.rpc.Server;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;

/**
 * Base class for JMS message to request dispatching.
 */
public abstract class JMSMessageDispatcher
{
   // attributes

   /**
    * The receiver event name.
    */
   protected String m_sEvent = "receive";

   /**
    * The message argument name.
    */
   protected String m_sMessage = "message";

   /**
    * The properties argument name.
    */
   protected String m_sProperties = "properties";

   // associations

   /**
    * The class to use for receiving the messages.
    */
   protected Metaclass m_metaclass;

   /**
    * The server component.
    */
   protected Component m_server;

   // operations

   /**
    * Sets the server component.
    * @param server The server component to set.
    */
   public void setServer(Component server)
   {
      m_server = server;
   }

   /**
    * @return The server component.
    */
   public Component getServer()
   {
      return m_server;
   }

   /**
    * Sets the class to use for receiving the messages.
    * @param metaclass The class to use for receiving the messages to set.
    */
   public void setMetaclass(Metaclass metaclass)
   {
      m_metaclass = metaclass;
   }

   /**
    * Sets the receiver event name.
    * @param sEvent The receiver event name to set.
    */
   public void setEvent(String sEvent)
   {
      m_sEvent = sEvent;
   }

   /**
    * Sets the message argument name.
    * @param sMessage The message argument name to set.
    */
   public void setMessage(String sMessage)
   {
      m_sMessage = sMessage;
   }

   /**
    * Sets the properties argument name.
    * @param sProperties The properties argument name to set.
    */
   public void setProperties(String sProperties)
   {
      m_sProperties = sProperties;
   }

   /**
    * Invokes the generic server.
    * @param sClass The class name to invoke.
    * @param sEvent The event name to invoke.
    * @param message The message to pass.
    * @param properties The message properties.
    * @param context The invocation context.
    * @return The method invocation return value.
    */
   protected Object invoke(String sClass, String sEvent, Object message, TransferObject properties, InvocationContext context)
   {
      TransferObject tobj = new TransferObject();

      tobj.setClassName(sClass);
      tobj.setEventName(sEvent);
      tobj.setValue(m_sMessage, message);

      if (m_sProperties != null)
      {
         tobj.setValue(m_sProperties, properties);
      }

      Request request = new Request();

      request.addInvocation(tobj);

      Response response = ((Server)m_server.getInstance(context)).invoke(request);

      return response.getResult(0);
   }
}
