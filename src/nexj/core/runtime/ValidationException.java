// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Iterator;

import nexj.core.persistence.OIDHolder;
import nexj.core.rpc.ErrorLocationHolder;
import nexj.core.util.EmptyIterator;
import nexj.core.util.GenericException;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

/**
 * Generic validation exception.
 */
public class ValidationException extends GenericException implements ErrorLocationHolder
{
   // attributes
   
   /**
    * Serial version UID.
    */
   private final static long serialVersionUID = 2019957307731160546L;

   /**
    * The target object class name.
    */
   protected String m_sClassName;
   
   /**
    * The request argument ordinal number, corresponding to the target object.
    */
   protected int m_nOrdinal = -1;
   
   // associations
   
   /**
    * Attribute name to exception map: Throwable[String].
    */
   protected Lookup m_exceptionMap;
   
   /**
    * The target object OID holder.
    */
   protected transient OIDHolder m_oidHolder;
   
   // constructors
   
   public ValidationException(String sErrCode)
   {
      super(sErrCode);
   }

   public ValidationException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public ValidationException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public ValidationException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }

   // operations

   /**
    * @see nexj.core.rpc.ErrorLocationHolder#setClassName(java.lang.String)
    */
   public void setClassName(String sName)
   {
      m_sClassName = sName;
   }

   /**
    * @see nexj.core.rpc.ErrorLocationHolder#getClassName()
    */
   public String getClassName()
   {
      return m_sClassName;
   }

   /**
    * @see nexj.core.rpc.ErrorLocationHolder#setOIDHolder(nexj.core.persistence.OIDHolder)
    */
   public void setOIDHolder(OIDHolder holder)
   {
      m_oidHolder = holder;
   }

   /**
    * @see nexj.core.rpc.ErrorLocationHolder#getOIDHolder()
    */
   public OIDHolder getOIDHolder()
   {
      return m_oidHolder;
   }

   /**
    * @see nexj.core.rpc.ErrorLocationHolder#setOrdinal(int)
    */
   public void setOrdinal(int nOrdinal)
   {
      m_nOrdinal = nOrdinal;
   }

   /**
    * @see nexj.core.rpc.ErrorLocationHolder#getOrdinal()
    */
   public int getOrdinal()
   {
      return m_nOrdinal;
   }

   /**
    * @see nexj.core.rpc.ErrorLocationHolder#addException(java.lang.String, java.lang.Throwable)
    */
   public void addException(String sAttribute, Throwable t)
   {
      assert sAttribute != null;
      assert t != null;

      if (m_exceptionMap == null)
      {
         m_exceptionMap = new HashTab(4);
      }

      m_exceptionMap.put(sAttribute, t);

      if (getCause() == null && t != this)
      {
         initCause(t);
      }
   }

   /**
    * @see nexj.core.rpc.ErrorLocationHolder#findException(java.lang.String)
    */
   public Throwable findException(String sAttribute)
   {
      if (m_exceptionMap == null)
      {
         return null;
      }
      
      return (Throwable)m_exceptionMap.get(sAttribute);
   }

   /**
    * @see nexj.core.rpc.ErrorLocationHolder#getAttributeIterator()
    */
   public Iterator getAttributeIterator()
   {
      if (m_exceptionMap == null)
      {
         return EmptyIterator.getInstance();
      }
      
      return m_exceptionMap.iterator();
   }

   /**
    * @see nexj.core.rpc.ErrorLocationHolder#getAttributeCount()
    */
   public int getAttributeCount()
   {
      if (m_exceptionMap == null)
      {
         return 0;
      }

      return m_exceptionMap.size();
   }

   /**
    * Serializes the object.
    */
   private void writeObject(ObjectOutputStream out) throws IOException
   {
      out.defaultWriteObject();
      out.writeObject((m_oidHolder != null) ? m_oidHolder.getOID() : null);
   }

   /**
    * Deserializes the object.
    */
   private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException
   {
      in.defaultReadObject();
      m_oidHolder = (OIDHolder)in.readObject();
   }
}
