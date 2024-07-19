// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.io;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.Output;
import nexj.core.util.Binary;

/**
 * Provides integration framework output functionality to an OutputStream.
 */
public class StreamOutput implements Output
{
   // associations

   /**
    * The encoding.
    */
   protected String m_sEncoding;

   /**
    * The output stream.
    */
   protected OutputStream m_ostream;

   /**
    * The Writer object.
    */
   protected Writer m_writer;


   // constructors

   /**
    * Constructs the output.
    * @param ostream The output stream.
    */
   public StreamOutput(OutputStream ostream)
   {
      m_ostream = ostream;
      m_sEncoding = "UTF-8";
   }

   /**
    * Constructs the output.
    * @param ostream The output stream.
    * @param sEncoding The encoding when writing characters.
    */
   public StreamOutput(OutputStream ostream, String sEncoding)
   {
      m_ostream = ostream;
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
    * @see nexj.core.integration.Output#getOutputStream()
    */
   public OutputStream getOutputStream() throws IntegrationException
   {
      return m_ostream;
   }

   /**
    * @see nexj.core.integration.Output#getWriter()
    */
   public Writer getWriter() throws IntegrationException
   {
      if (m_writer == null)
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

      return m_writer;
   }

   /**
    * @see nexj.core.integration.Output#setBinary(nexj.core.util.Binary)
    */
   public void setBinary(Binary msg) throws IntegrationException
   {
      try
      {
         m_ostream.write(msg.getData());
      }
      catch (IOException ex)
      {
         throw new IntegrationException("err.integration.io", ex);
      }
   }

   /**
    * @see nexj.core.integration.Output#setObject(java.lang.Object)
    */
   public void setObject(Object obj) throws IntegrationException
   {
      throw new IntegrationException("err.integration.outputObject");
   }

   /**
    * @see nexj.core.integration.Output#setString(java.lang.String)
    */
   public void setString(String sMsg) throws IntegrationException
   {
      throw new IntegrationException("err.integration.outputString");
   }

   /**
    * Flush the Writer's stream.
    * @throws IOException If an I/O error occurs.
    */
   public void flush() throws IOException
   {
      if (m_writer != null)
      {
         m_writer.flush();
      }
   }

   /**
    * Close the Writer's stream.
    * @throws IOException If an I/O error occurs.
    */
   public void close() throws IOException
   {
      if (m_writer != null)
      {
         m_writer.close();
      }
   }
}
