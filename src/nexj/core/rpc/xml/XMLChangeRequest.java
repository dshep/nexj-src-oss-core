// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Metaclass;
import nexj.core.rpc.Response;
import nexj.core.rpc.Server;
import nexj.core.rpc.TransferObject;
import nexj.core.scripting.Pair;
import nexj.core.util.IteratorEnumeration;

/**
 * Generic change request.
 */
public class XMLChangeRequest extends Invoker
{
   // attributes

   /**
    * The response type name.
    */
   protected String m_sResponseType;

   // associations

   /**
    * The metaclass.
    */
   protected Metaclass m_metaclass;

   /**
    * The response attributes.
    */
   protected Pair m_attributes;

   // operations

   /**
    * Sets the response type name.
    * @param sResponseType The response type name to set.
    */
   public void setResponseType(String sResponseType)
   {
      m_sResponseType = sResponseType;
   }

   /**
    * @return The response type name.
    */
   public String getResponseType()
   {
      return m_sResponseType;
   }

   /**
    * Sets the metaclass.
    * @param metaclass The metaclass to set.
    */
   public void setMetaclass(Metaclass metaclass)
   {
      m_metaclass = metaclass;
   }

   /**
    * @return The metaclass.
    */
   public Metaclass getMetaclass()
   {
      return m_metaclass;
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
    * Adds a transfer object.
    * @param tobj The object to add.
    */
   public void addTransferObject(TransferObject tobj)
   {
      m_request.addInvocation(tobj, m_attributes);
   }

   /**
    * Adds a transfer object collection.
    * @param col The collection to add. Can be null.
    */
   public void addTransferObjects(Collection col)
   {
      if (col != null)
      {
         for (Iterator itr = col.iterator(); itr.hasNext();)
         {
            addTransferObject((TransferObject)itr.next());
         }
      }
   }

   /**
    * @see nexj.core.rpc.xml.Invoker#invoke(nexj.core.rpc.Server)
    */
   public Object invoke(Server server)
   {
      Response response = server.invoke(m_request);
      List resultList = new ArrayList(response.getResultCount());

      for (int i = 0; i < response.getResultCount(); ++i)
      {
         resultList.add(response.getResult(i));
      }

      return new XMLCollection(m_metaclass, resultList, m_sResponseType);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "XMLChangeRequest(locale=" + m_request.getLocale() + ", class=" + m_metaclass +
         ", objects=" + Collections.list(new IteratorEnumeration(m_request.getInvocationIterator())) +
         ", attributes=" + m_attributes + ", responseType=" + m_sResponseType + ")";
   }
}
