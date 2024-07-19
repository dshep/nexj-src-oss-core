// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.io;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;

import nexj.core.integration.Input;
import nexj.core.integration.IntegrationException;
import nexj.core.util.Binary;
import nexj.core.util.XMLUtil;

/**
 * Input implementation with an object.
 */
public class ObjectInput implements Input
{
   // attributes

   /**
    * The encoding.
    */
   protected String m_sEncoding = XMLUtil.ENCODING;

   // associations
   
   protected Object m_obj;

   // constructors
   
   /**
    * Constructs the input.
    * @param obj The input object.
    */
   public ObjectInput(Object obj)
   {
      m_obj = obj;
   }
   
   // operations

   /**
    * Sets the encoding.
    * @param sEncoding The encoding to set.
    */
   public void setEncoding(String sEncoding)
   {
      m_sEncoding = sEncoding;
   }

   /**
    * @return The encoding.
    */
   public String getEncoding()
   {
      return m_sEncoding;
   }

   /**
    * @see nexj.core.integration.Input#getInputStream()
    */
   public InputStream getInputStream() throws IntegrationException
   {
      if (m_obj == null)
      {
         return new ByteArrayInputStream(new byte[0]);
      }

      if (m_obj instanceof Binary)
      {
         return ((Binary)m_obj).getInputStream();
      }

      throw new IntegrationException("err.integration.inputStream");
   }

   /**
    * @see nexj.core.integration.Input#getReader()
    */
   public Reader getReader() throws IntegrationException
   {
      if (m_obj == null)
      {
         return new StringReader("");
      }
      
      if (m_obj instanceof String)
      {
         return new StringReader((String)m_obj);
      }
      
      if (m_obj instanceof Binary)
      {
         try
         {
            return new InputStreamReader(((Binary)m_obj).getInputStream(), m_sEncoding);
         }
         catch (UnsupportedEncodingException e)
         {
            throw new IntegrationException("err.integration.io", e);
         }
      }
      
      throw new IntegrationException("err.integration.reader");
   }

   /**
    * @see nexj.core.integration.Input#getBinary()
    */
   public Binary getBinary() throws IntegrationException
   {
      if (m_obj == null)
      {
         return null;
      }
      
      if (m_obj instanceof Binary)
      {
         return (Binary)m_obj;
      }
      
      if (m_obj instanceof String)
      {
         try
         {
            return new Binary(((String)m_obj).getBytes(m_sEncoding));
         }
         catch (UnsupportedEncodingException e)
         {
            throw new IntegrationException("err.integration.io", e);
         }
      }
      
      throw new IntegrationException("err.integration.binary");
   }
   
   /**
    * @see nexj.core.integration.Input#getString()
    */
   public String getString() throws IntegrationException
   {
      if (m_obj == null)
      {
         return null;
      }
      
      if (m_obj instanceof String)
      {
         return (String)m_obj;
      }
      
      if (m_obj instanceof Binary)
      {
         try
         {
            return new String(((Binary)m_obj).getData(), m_sEncoding);
         }
         catch (UnsupportedEncodingException e)
         {
            throw new IntegrationException("err.integration.io", e);
         }
      }
      
      throw new IntegrationException("err.integration.string");
   }

   /**
    * @see nexj.core.integration.Input#getObject()
    */
   public Object getObject() throws IntegrationException
   {
      return m_obj;
   }

   /**
    * @return True if the input stream will be empty.
    */
   public boolean isEmpty()
   {
      if (m_obj == null)
      {
         return true;
      }

      if (m_obj instanceof String)
      {
         return ((String)m_obj).length() == 0;
      }

      if (m_obj instanceof Binary)
      {
         return ((Binary)m_obj).getSize() == 0;
      }

      return false;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return String.valueOf(m_obj);
   }
}
