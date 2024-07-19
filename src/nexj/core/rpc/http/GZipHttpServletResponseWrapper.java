// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;

/**
 * The servlet response wrapper used by the GZip filter.
 */
public class GZipHttpServletResponseWrapper extends HttpServletResponseWrapper
{
   // associations

   /**
    * The servlet output stream.
    */
   protected ServletOutputStream m_outputStream;
   
   /**
    * The print writer.
    */
   protected PrintWriter m_writer;
   
   /**
    * The request object.
    */
   protected HttpServletRequest m_request;
   
   // constructors
   /**
    * Creates the response wrapper.
    * @param request The request object.
    * @param response The response to wrap.
    */
   public GZipHttpServletResponseWrapper(HttpServletRequest request, HttpServletResponse response)
   {
      super(response);
      m_request = request;
   }
   
   // operations
   
   /**
    * @see javax.servlet.ServletResponse#flushBuffer()
    */
   public void flushBuffer() throws IOException
   {
      if (m_writer != null)
      {
         m_writer.flush();
      }
      else
      {
         m_outputStream.flush();
      }
   }
   /**
    * @see javax.servlet.ServletResponse#getOutputStream()
    */
   public ServletOutputStream getOutputStream() throws IOException
   {
      if (m_writer != null)
      {
         throw new IllegalStateException();
      }
      
      if (m_outputStream == null)
      {
         m_outputStream = new GZipServletOutputStream(m_request, (HttpServletResponse)getResponse());
      }
      
      return m_outputStream;
   }
   /**
    * @see javax.servlet.ServletResponse#getWriter()
    */
   public PrintWriter getWriter() throws IOException
   {
      if (m_writer == null)
      {
         if (m_outputStream != null)
         {
            throw new IllegalStateException();
         }
         
         m_outputStream = new GZipServletOutputStream(m_request, (HttpServletResponse)getResponse());
         m_writer = new PrintWriter(new OutputStreamWriter(m_outputStream, getCharacterEncoding()));
      }
      
      return m_writer;
   }

   /**
    * @see javax.servlet.ServletResponse#setContentLength(int)
    */
   public void setContentLength(int nLength)
   {
   }
   
   /**
    * Flushes the buffer and closes the output stream.
    */
   public void finish() throws IOException
   {
      if (m_writer != null)
      {
         m_writer.close();
      }
      else if (m_outputStream != null)
      {
         m_outputStream.close();
      }
   }
}
