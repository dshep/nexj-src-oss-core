// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.io;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;

import nexj.core.integration.Input;
import nexj.core.integration.IntegrationException;
import nexj.core.util.Binary;
import nexj.core.util.IOUtil;
import nexj.core.util.XMLUtil;

/**
 * Input stream input.
 */
public class StreamInput implements Input
{
   // associations

   /**
    * The encoding.
    */
   protected String m_sEncoding;

   /**
    * The input stream.
    */
   protected InputStream m_istream;

   // constructors

   /**
    * Constructs the input.
    * @param istream The input stream.
    */
   public StreamInput(InputStream istream)
   {
      m_istream = istream;
      m_sEncoding = XMLUtil.ENCODING;
   }

   /**
    * Constructs the input.
    * @param istream The input stream.
    * @param istream The input stream encoding.
    */
   public StreamInput(InputStream istream, String sEncoding)
   {
      m_istream = istream;
      m_sEncoding = sEncoding;
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
      return m_istream;
   }

   /**
    * @see nexj.core.integration.Input#getReader()
    */
   public Reader getReader() throws IntegrationException
   {
      try
      {
         return new BufferedReader(new InputStreamReader(m_istream, m_sEncoding));
      }
      catch (UnsupportedEncodingException e)
      {
         throw new IntegrationException("err.integration.io", e);
      }
   }

   /**
    * @see nexj.core.integration.Input#getBinary()
    */
   public Binary getBinary() throws IntegrationException
   {
      ByteArrayOutputStream ostream = new ByteArrayOutputStream(256);

      try
      {
         IOUtil.copy(ostream, m_istream);
      }
      catch (IOException e)
      {
         throw new IntegrationException("err.integration.io", e);
      }

      return new Binary(ostream.toByteArray());
   }

   /**
    * @see nexj.core.integration.Input#getString()
    */
   public String getString() throws IntegrationException
   {
      StringWriter writer = new StringWriter(128);

      try
      {
         IOUtil.copy(writer, new InputStreamReader(m_istream, m_sEncoding));
      }
      catch (IOException e)
      {
         throw new IntegrationException("err.integration.io", e);
      }

      return writer.toString();
   }

   /**
    * @see nexj.core.integration.Input#getObject()
    */
   public Object getObject() throws IntegrationException
   {
      return getBinary();
   }
}
