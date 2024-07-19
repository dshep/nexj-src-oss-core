// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.util.Collection;

import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.rpc.Server;
import nexj.core.scripting.Pair;

/**
 * Generic read request.
 */
public class XMLReadRequest extends Invoker
{
   // attributes

   /**
    * The response type name.
    */
   protected String m_sResponseType;

   /**
    * The class name.
    */
   protected String m_sClassName;

   /**
    * The maximum instance count.
    */
   protected int m_nCount = -1;

   /**
    * The instance offset.
    */
   protected int m_nOffset;

   // associations

   /**
    * The metaclass.
    */
   protected Metaclass m_metaclass;

   /**
    * The read attribute list.
    */
   protected Pair m_attributes;

   /**
    * The where clause.
    */
   protected Object m_where;

   /**
    * The order by specification.
    */
   protected Pair m_orderBy;

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

      if (metaclass != null)
      {
         m_sClassName = metaclass.getName();
      }
   }

   /**
    * @return The metaclass.
    */
   public Metaclass getMetaclass()
   {
      return m_metaclass;
   }

   /**
    * Sets the class name.
    * @param sClassName The class name to set.
    */
   public void setClassName(String sClassName)
   {
      m_sClassName = sClassName;
   }

   /**
    * @return The class name.
    */
   public String getClassName()
   {
      return m_sClassName;
   }

   /**
    * Sets the read attribute list.
    * @param attributes The read attribute list to set.
    */
   public void setAttributes(Pair attributes)
   {
      m_attributes = attributes;
   }

   /**
    * @return The read attribute list.
    */
   public Pair getAttributes()
   {
      return m_attributes;
   }

   /**
    * Sets the where clause.
    * @param where The where clause to set.
    */
   public void setWhere(Object where)
   {
      m_where = where;
   }

   /**
    * @return The where clause.
    */
   public Object getWhere()
   {
      return m_where;
   }

   /**
    * Sets the order by specification.
    * @param orderBy The order by specification to set.
    */
   public void setOrderBy(Pair orderBy)
   {
      m_orderBy = orderBy;
   }

   /**
    * @return The order by specification.
    */
   public Pair getOrderBy()
   {
      return m_orderBy;
   }

   /**
    * Sets the maximum instance count.
    * @param nCount The maximum instance count to set. Can be null for default.
    */
   public void setCount(Number count)
   {
      m_nCount = (count == null) ? -1 : count.intValue();
   }

   /**
    * Sets the maximum instance count.
    * @param nCount The maximum instance count to set.
    */
   public void setCount(int nCount)
   {
      m_nCount = nCount;
   }

   /**
    * @return The maximum instance count.
    */
   public int getCount()
   {
      return m_nCount;
   }

   /**
    * Sets the instance offset.
    * @param offset The instance offset to set. Can be null for default.
    */
   public void setOffset(Number offset)
   {
      m_nOffset = (offset == null) ? 0 : offset.intValue();
   }
   
   /**
    * Sets the instance offset.
    * @param nOffset The instance offset to set.
    */
   public void setOffset(int nOffset)
   {
      m_nOffset = nOffset;
   }

   /**
    * @return The instance offset.
    */
   public int getOffset()
   {
      return m_nOffset;
   }

   /**
    * @see nexj.core.rpc.xml.Invoker#invoke(nexj.core.rpc.Server)
    */
   public Object invoke(Server server)
   {
      m_request.addInvocation(
         m_sClassName,
         "read",
         new Object[]{m_attributes, m_where, m_orderBy, Primitive.createInteger(m_nCount),
            Primitive.createInteger(m_nOffset), Boolean.FALSE},
         null);

      return new XMLCollection(m_metaclass,
         (Collection)server.invoke(m_request).getResult(0),
         m_sResponseType);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "XMLReadRequest(locale=" + m_request.getLocale() + ", class=" + m_metaclass +
      ", className=" + m_sClassName + ", attributes=" + m_attributes + ", where=" + m_where +
      ", orderBy=" + m_orderBy + ", count=" + m_nCount + ", offset=" + m_nOffset + 
      ", responseType=" + m_sResponseType + ")";
   }
}
