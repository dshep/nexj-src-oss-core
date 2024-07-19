// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms;

import java.util.Enumeration;

import javax.jms.DeliveryMode;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;

import nexj.core.meta.Primitive;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextHolder;
import nexj.core.util.EmptyIterator;
import nexj.core.util.IteratorEnumeration;

/**
 * JMS message containing a transfer object.
 * Used for simulating a JMS implementation.
 */
public class TransferObjectMessage implements Message, InvocationContextHolder
{
   // associations

   /**
    * The wrapped transfer object.
    */
   protected TransferObject m_tobj;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   // constructors

   /**
    * Constructs the message.
    * @param tobj The transfer object to wrap.
    */
   public TransferObjectMessage(TransferObject tobj)
   {
      m_tobj = tobj;
   }

   /**
    * Constructs the message.
    * @param tobj The transfer object to wrap.
    * @param context The invocation context.
    */
   public TransferObjectMessage(TransferObject tobj, InvocationContext context)
   {
      m_tobj = tobj;
      m_context = context;
   }

   // operations

   /**
    * @return The wrapped transfer object.
    */
   public TransferObject getTransferObject()
   {
      return m_tobj;
   }

   /**
    * @see javax.jms.Message#setJMSCorrelationID(java.lang.String)
    */
   public void setJMSCorrelationID(String sId) throws JMSException
   {
   }

   /**
    * @see javax.jms.Message#setJMSCorrelationIDAsBytes(byte[])
    */
   public void setJMSCorrelationIDAsBytes(byte[] id) throws JMSException
   {
   }

   /**
    * @see javax.jms.Message#getJMSCorrelationID()
    */
   public String getJMSCorrelationID() throws JMSException
   {
      return null;
   }

   /**
    * @see javax.jms.Message#getJMSCorrelationIDAsBytes()
    */
   public byte[] getJMSCorrelationIDAsBytes() throws JMSException
   {
      return null;
   }

   /**
    * @see javax.jms.Message#setJMSDeliveryMode(int)
    */
   public void setJMSDeliveryMode(int nMode) throws JMSException
   {
   }

   /**
    * @see javax.jms.Message#getJMSDeliveryMode()
    */
   public int getJMSDeliveryMode() throws JMSException
   {
      return DeliveryMode.NON_PERSISTENT;
   }

   /**
    * @see javax.jms.Message#setJMSDestination(javax.jms.Destination)
    */
   public void setJMSDestination(Destination destination) throws JMSException
   {
   }

   /**
    * @see javax.jms.Message#getJMSDestination()
    */
   public Destination getJMSDestination() throws JMSException
   {
      return null;
   }

   /**
    * @see javax.jms.Message#setJMSExpiration(long)
    */
   public void setJMSExpiration(long lTime) throws JMSException
   {
   }

   /**
    * @see javax.jms.Message#getJMSExpiration()
    */
   public long getJMSExpiration() throws JMSException
   {
      return 0;
   }

   /**
    * @see javax.jms.Message#setJMSMessageID(java.lang.String)
    */
   public void setJMSMessageID(String sId) throws JMSException
   {
      m_tobj.setValue(JMSReceiver.MESSAGE_ID, sId);
   }

   /**
    * @see javax.jms.Message#getJMSMessageID()
    */
   public String getJMSMessageID() throws JMSException
   {
      return (String)m_tobj.findValue(JMSReceiver.MESSAGE_ID);
   }

   /**
    * @see javax.jms.Message#setJMSPriority(int)
    */
   public void setJMSPriority(int nPriority) throws JMSException
   {
   }

   /**
    * @see javax.jms.Message#getJMSPriority()
    */
   public int getJMSPriority() throws JMSException
   {
      return 4;
   }

   /**
    * @see javax.jms.Message#setJMSRedelivered(boolean)
    */
   public void setJMSRedelivered(boolean bRedelivered) throws JMSException
   {
   }

   /**
    * @see javax.jms.Message#getJMSRedelivered()
    */
   public boolean getJMSRedelivered() throws JMSException
   {
      return false;
   }

   /**
    * @see javax.jms.Message#setJMSReplyTo(javax.jms.Destination)
    */
   public void setJMSReplyTo(Destination destination) throws JMSException
   {
   }

   /**
    * @see javax.jms.Message#getJMSReplyTo()
    */
   public Destination getJMSReplyTo() throws JMSException
   {
      return null;
   }

   /**
    * @see javax.jms.Message#setJMSTimestamp(long)
    */
   public void setJMSTimestamp(long lTime) throws JMSException
   {
   }

   /**
    * @see javax.jms.Message#getJMSTimestamp()
    */
   public long getJMSTimestamp() throws JMSException
   {
      return 0;
   }

   /**
    * @see javax.jms.Message#setJMSType(java.lang.String)
    */
   public void setJMSType(String sType) throws JMSException
   {
      m_tobj.setValue(JMSSender.TYPE, sType);
   }

   /**
    * @see javax.jms.Message#getJMSType()
    */
   public String getJMSType() throws JMSException
   {
      return (String)m_tobj.findValue(JMSSender.TYPE);
   }

   /**
    * @see javax.jms.Message#setBooleanProperty(java.lang.String, boolean)
    */
   public void setBooleanProperty(String sName, boolean bValue) throws JMSException
   {
      getProperties().setValue(sName, Boolean.valueOf(bValue));
   }

   /**
    * @see javax.jms.Message#getBooleanProperty(java.lang.String)
    */
   public boolean getBooleanProperty(String sName) throws JMSException
   {
      Boolean value = (Boolean)getObjectProperty(sName);

      return (value == null) ? false : value.booleanValue();
   }

   /**
    * @see javax.jms.Message#setByteProperty(java.lang.String, byte)
    */
   public void setByteProperty(String sName, byte nValue) throws JMSException
   {
      getProperties().setValue(sName, Primitive.createInteger(nValue));
   }

   /**
    * @see javax.jms.Message#getByteProperty(java.lang.String)
    */
   public byte getByteProperty(String sName) throws JMSException
   {
      Number value = (Number)getObjectProperty(sName);

      return (value == null) ? 0 : value.byteValue();
   }

   /**
    * @see javax.jms.Message#setDoubleProperty(java.lang.String, double)
    */
   public void setDoubleProperty(String sName, double dValue) throws JMSException
   {
      getProperties().setValue(sName, Primitive.createDouble(dValue));
   }

   /**
    * @see javax.jms.Message#getDoubleProperty(java.lang.String)
    */
   public double getDoubleProperty(String sName) throws JMSException
   {
      Number value = (Number)getObjectProperty(sName);

      return (value == null) ? 0 : value.doubleValue();
   }

   /**
    * @see javax.jms.Message#setFloatProperty(java.lang.String, float)
    */
   public void setFloatProperty(String sName, float fValue) throws JMSException
   {
      getProperties().setValue(sName, Primitive.createDouble(fValue));
   }

   /**
    * @see javax.jms.Message#getFloatProperty(java.lang.String)
    */
   public float getFloatProperty(String sName) throws JMSException
   {
      Number value = (Number)getObjectProperty(sName);

      return (value == null) ? 0 : value.floatValue();
   }

   /**
    * @see javax.jms.Message#setIntProperty(java.lang.String, int)
    */
   public void setIntProperty(String sName, int nValue) throws JMSException
   {
      getProperties().setValue(sName, Primitive.createInteger(nValue));
   }

   /**
    * @see javax.jms.Message#getIntProperty(java.lang.String)
    */
   public int getIntProperty(String sName) throws JMSException
   {
      Number value = (Number)getObjectProperty(sName);

      return (value == null) ? 0 : value.intValue();
   }

   /**
    * @see javax.jms.Message#setLongProperty(java.lang.String, long)
    */
   public void setLongProperty(String sName, long lValue) throws JMSException
   {
      getProperties().setValue(sName, Primitive.createLong(lValue));
   }

   /**
    * @see javax.jms.Message#getLongProperty(java.lang.String)
    */
   public long getLongProperty(String sName) throws JMSException
   {
      Number value = (Number)getObjectProperty(sName);

      return (value == null) ? 0 : value.longValue();
   }

   /**
    * @see javax.jms.Message#setObjectProperty(java.lang.String, java.lang.Object)
    */
   public void setObjectProperty(String sName, Object value) throws JMSException
   {
      getProperties().setValue(sName, value);
   }

   /**
    * @see javax.jms.Message#getObjectProperty(java.lang.String)
    */
   public Object getObjectProperty(String sName) throws JMSException
   {
      TransferObject tobj = findProperties();

      return (tobj == null) ? null : tobj.findValue(sName); 
   }

   /**
    * @see javax.jms.Message#setShortProperty(java.lang.String, short)
    */
   public void setShortProperty(String sName, short nValue) throws JMSException
   {
      getProperties().setValue(sName, Primitive.createInteger(nValue));
   }

   /**
    * @see javax.jms.Message#getShortProperty(java.lang.String)
    */
   public short getShortProperty(String sName) throws JMSException
   {
      Number value = (Number)getObjectProperty(sName);

      return (value == null) ? 0 : value.shortValue();
   }

   /**
    * @see javax.jms.Message#setStringProperty(java.lang.String, java.lang.String)
    */
   public void setStringProperty(String sName, String sValue) throws JMSException
   {
      getProperties().setValue(sName, sValue);
   }

   /**
    * @see javax.jms.Message#getStringProperty(java.lang.String)
    */
   public String getStringProperty(String sName) throws JMSException
   {
      return (String)getObjectProperty(sName);
   }

   /**
    * @see javax.jms.Message#getPropertyNames()
    */
   public Enumeration getPropertyNames() throws JMSException
   {
      TransferObject tobj = findProperties();

      return new IteratorEnumeration((tobj == null) ?
         EmptyIterator.getInstance() :
         tobj.getIterator());
   }

   /**
    * @see javax.jms.Message#propertyExists(java.lang.String)
    */
   public boolean propertyExists(String sName) throws JMSException
   {
      TransferObject tobj = findProperties();

      return (tobj == null) ? false : tobj.hasValue(sName);
   }

   /**
    * @see javax.jms.Message#clearProperties()
    */
   public void clearProperties() throws JMSException
   {
      m_tobj.removeValue(JMSSender.PROPERTIES);
   }

   /**
    * @see javax.jms.Message#clearBody()
    */
   public void clearBody() throws JMSException
   {
      m_tobj.removeValue(JMSSender.BODY);
   }

   /**
    * @see javax.jms.Message#acknowledge()
    */
   public void acknowledge() throws JMSException
   {
   }

   /**
    * @return The property map. Creates one of not found.
    */
   protected TransferObject getProperties()
   {
      TransferObject tobj = (TransferObject)m_tobj.findValue(JMSSender.PROPERTIES);

      if (tobj == null)
      {
         tobj = new TransferObject();
         m_tobj.setValue(JMSSender.PROPERTIES, tobj);
      }

      return tobj;
   }

   /**
    * @return The property map, or null if not found.
    */
   protected TransferObject findProperties()
   {
      return (TransferObject)m_tobj.findValue(JMSSender.PROPERTIES);
   }

   /**
    * @see nexj.core.runtime.InvocationContextHolder#getInvocationContext()
    */
   public InvocationContext getInvocationContext()
   {
      return m_context;
   }
}
