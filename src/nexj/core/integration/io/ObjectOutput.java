// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.io;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.Output;
import nexj.core.util.Binary;
import nexj.core.util.XMLUtil;

/**
 * Output implementation for retrieving an object.
 */
public class ObjectOutput implements Output
{
   // attributes

   /**
    * The encoding.
    */
   protected String m_sEncoding = XMLUtil.ENCODING;

   /**
    * True if the writer was instantiated first.
    */
   protected boolean m_bWriter;

   // associations

   /**
    * The output object.
    */
   protected Object m_obj;

   /**
    * The output stream.
    */
   protected OutputStream m_ostream;
   
   /**
    * The character stream writer.
    */
   protected Writer m_writer;

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
    * @see nexj.core.integration.Output#getOutputStream()
    */
   public OutputStream getOutputStream() throws IntegrationException
   {
      if (m_ostream == null)
      {
         if (m_writer != null)
         {
            throw new IntegrationException("err.integration.outputStream");
         }
         
         m_ostream = new ByteArrayOutputStream(256);
      }

      return m_ostream;
   }

   /**
    * @see nexj.core.integration.Output#getWriter()
    */
   public Writer getWriter() throws IntegrationException
   {
      if (m_writer == null)
      {
         if (m_ostream != null)
         {
            try
            {
               m_writer = new OutputStreamWriter(m_ostream, m_sEncoding);
            }
            catch (UnsupportedEncodingException e)
            {
               throw new IntegrationException("err.integration.io", e);
            }
         }
         else
         {
            m_writer = new StringWriter(256);
            m_bWriter = true;
         }
      }
      
      return m_writer;
   }

   /**
    * @see nexj.core.integration.Output#setBinary(nexj.core.util.Binary)
    */
   public void setBinary(Binary msg) throws IntegrationException
   {
      m_obj = msg;
   }

   /**
    * @see nexj.core.integration.Output#setString(java.lang.String)
    */
   public void setString(String sMsg) throws IntegrationException
   {
      m_obj = sMsg;
   }

   /**
    * @see nexj.core.integration.Output#setObject(java.lang.Object)
    */
   public void setObject(Object obj) throws IntegrationException
   {
      m_obj = obj;
   }

   /**
    * @return The object that was previously set.
    */
   public Object getObject()
   {
      if (m_obj == null)
      {
         if (m_ostream != null && !m_bWriter)
         {
            m_obj = new Binary(((ByteArrayOutputStream)m_ostream).toByteArray());
         }
         else if (m_writer != null)
         {
            m_obj = m_writer.toString();
         }
      }

      return m_obj;
   }
}
