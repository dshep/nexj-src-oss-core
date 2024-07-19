// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import nexj.core.rpc.Server;
import nexj.core.rpc.TransferObject;
import nexj.core.scripting.Pair;

/**
 * Generic change request for a single instance.
 */
public class XMLChange1Request extends Invoker
{
   // associations

   /**
    * The transfer object.
    */
   protected TransferObject m_tobj;

   /**
    * The response attributes.
    */
   protected Pair m_attributes;

   // constructors

   /**
    * Constructs the request.
    */
   public XMLChange1Request()
   {
   }

   /**
    * Constructs the request.
    * @param tobj The transfer object.
    * @param attributes The response attributes.
    */
   public XMLChange1Request(TransferObject tobj, Pair attributes)
   {
      m_tobj = tobj;
      m_attributes = attributes;
   }

   // operations

   /**
    * Sets the transfer object.
    * @param tobj The transfer object to set.
    */
   public void setTransferObject(TransferObject tobj)
   {
      m_tobj = tobj;
   }

   /**
    * @return The transfer object.
    */
   public TransferObject getTransferObject()
   {
      return m_tobj;
   }

   /**
    * Sets the response attributes.
    * @param attributes The response attributes to set.
    */
   public void setAttributes(Pair attributes)
   {
      m_attributes = attributes;
   }

   /**
    * @return The response attributes.
    */
   public Pair getAttributes()
   {
      return m_attributes;
   }

   /**
    * @see nexj.core.rpc.xml.Invoker#invoke(nexj.core.rpc.Server)
    */
   public Object invoke(Server server)
   {
      m_request.addInvocation(m_tobj, m_attributes);

      return server.invoke(m_request).getResult(0);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "XMLChange1Request(locale=" + m_request.getLocale() + ", object=" + m_tobj +
         ", attributes=" + m_attributes + ")";
   }
}
