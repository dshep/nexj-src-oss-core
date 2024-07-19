package nexj.core.rpc.queueing.ra;

import java.io.Serializable;

import nexj.core.rpc.queueing.DispatcherMessage;

/**
 * A serializable SysDispatcherMessage.
 */
public class SerializableMessage implements Serializable, DispatcherMessage
{
   /**
    * The serial version UID.
    */
   private final static long serialVersionUID = -6914001932494184946L;
   
   /**
    * The message type.
    */
   private int m_nType;
   
   /**
    * The message arguments.
    */
   private Object[] m_argArray;
   
   /**
    * The message dispatcher.
    */
   private String m_sDispatcherNodeName;

   /**
    * @param nType the message type.
    * @param argArray the message arguments.
    * @param sDispatcherNodeName the expected dispatcher node id.
    */
   public SerializableMessage(int nType, Object[] argArray, String sDispatcherNodeName)
   {
      m_nType = nType;
      m_argArray = argArray;
      m_sDispatcherNodeName = sDispatcherNodeName;
   }
   
   /**
    * @param message the message from which to extract type, arguments and dispatcher.
    */
   public SerializableMessage(DispatcherMessage message)
   {
      m_nType = message.getType();
      m_argArray = message.getArgArray();
      m_sDispatcherNodeName = message.getDispatcherName();
   }

   /**
    * @see nexj.core.rpc.queueing.DispatcherMessage#error(java.lang.Throwable)
    */
   public void error(int nCause, Throwable t)
   {
   }

   /**
    * @see nexj.core.rpc.queueing.DispatcherMessage#getArgArray()
    */
   public Object[] getArgArray()
   {
      return m_argArray;
   }

   /**
    * @see nexj.core.rpc.queueing.DispatcherMessage#getType()
    */
   public int getType()
   {
      return m_nType;
   }

   /**
    * @see nexj.core.rpc.queueing.DispatcherMessage#respond(java.lang.Object)
    */
   public void respond(Object result)
   {
   }

   /**
    * @see nexj.core.rpc.queueing.DispatcherMessage#getDispatcherName()
    */
   public String getDispatcherName()
   {
      return m_sDispatcherNodeName;
   }
}
