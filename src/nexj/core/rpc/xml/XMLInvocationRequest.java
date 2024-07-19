// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import nexj.core.meta.Argument;
import nexj.core.meta.Event;
import nexj.core.meta.Type;
import nexj.core.rpc.Request;
import nexj.core.rpc.Server;
import nexj.core.rpc.TransferObject;

/**
 * Event invocation request.
 */
public class XMLInvocationRequest extends Invoker
{
   // attributes

   /**
    * The result tag should represent a list.
    */
   protected boolean m_bList;

   // associations

   /**
    * The argument list for this event call (lazy init).
    */
   protected List/*<Object>*/ m_argList;

   /**
    * The event to invoke.
    */
   protected Event m_event;

   /**
    * The instance for this event (null for static events).
    */
   protected TransferObject m_instance;

   /**
    * The tag name for the result.
    */
   protected QName m_resultElement;

   /**
    * @param event The event to invoke (not null).
    * @param instance The instance to invoke event on (null for static events).
    * @param resultElement The result tag type (null == undefined result element).
    * @param bList The result should be represented as a list.
    */
   public XMLInvocationRequest(
         Event event, TransferObject instance, QName resultElement, boolean bList)
   {
      assert event != null;
      assert event.isStatic() || instance != null;

      m_bList = bList;
      m_event = event;
      m_instance = instance;
      m_resultElement = resultElement;
   }

   /**
    * Add an argument for the event invocation.
    * @param arg The next argument to add.
    */
   public void addArgument(Object arg)
   {
      if (m_argList == null)
      {
         m_argList = new ArrayList/*<Object>*/(m_event.getArgumentCount());
      }

      m_argList.add(arg);
   }

   /**
    * @see nexj.core.rpc.xml.Invoker#invoke(nexj.core.rpc.Server)
    */
   public Object invoke(Server server)
   {
      Request request = new Request();

      if (m_instance == null)
      {
         request.addInvocation(
            m_event.getMetaclass().getName(), m_event.getName(),
            (m_argList == null) ? null : m_argList.toArray(), null);
      }
      else
      {
         request.addInvocation(
            m_instance, m_event.getName(), (m_argList == null) ? null : m_argList.toArray(), null);
      }

      Argument result = m_event.getResult();
      Type resultType = (result == null) ? null : result.getType();
      Object resultObj = server.invoke(request).getResult(0);

      if (m_resultElement == null)
      {
         return resultObj; // no defined result element tag
      }

      return (m_bList) ? new XMLElementList(resultObj, m_resultElement, resultType)
                       : new XMLElement(resultObj, m_resultElement, resultType);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "XMLInvocationRequest(event=" + m_event +
                                 " instance=" + m_instance +
                                 " args=" + m_argList + ")";
   }
}