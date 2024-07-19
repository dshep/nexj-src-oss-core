// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.zip.GZIPOutputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import nexj.core.util.Logger;

/**
 * Servlet output stream providing GZip compression and content length calculation.
 */
public class GZipServletOutputStream extends ServletOutputStream
{
   // associations
   
   /**
    * The servlet request object.
    */
   protected HttpServletRequest m_request;
   
   /**
    * The servlet response object.
    */
   protected HttpServletResponse m_response;
   
   /**
    * The stream where the compressed data is accumulated.
    */
   protected ByteArrayOutputStream m_outputStream;
   
   /**
    * The compression filter stream.
    */
   protected GZIPOutputStream m_gzipStream;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(GZipServletOutputStream.class);
   
   // constructors
   
   /**
    * Creates the output stream.
    * @param request The servlet request object.
    * @param response The servlet response object. 
    */
   public GZipServletOutputStream(HttpServletRequest request, HttpServletResponse response) throws IOException
   {
      m_request = request;
      m_response = response;
      m_outputStream = new ByteArrayOutputStream(response.getBufferSize());
      m_gzipStream = new GZIPOutputStream(m_outputStream);
   }

   // operations
   
   /**
    * @see java.io.OutputStream#write(int)
    */
   public void write(int b) throws IOException
   {
      m_gzipStream.write(b);
   }
   
   /**
    * @see java.io.OutputStream#write(byte[], int, int)
    */
   public void write(byte[] buf, int nOff, int nLen) throws IOException
   {
      m_gzipStream.write(buf, nOff, nLen);
   }

   /**
    * @see java.io.OutputStream#flush()
    */
   public void flush() throws IOException
   {
      m_gzipStream.flush();
   }

   /**
    * @see java.io.OutputStream#close()
    */
   public void close() throws IOException
   {
      if (m_gzipStream != null)
      {
         m_gzipStream.finish();
         m_response.setContentLength(m_outputStream.size());
         m_response.addHeader("Content-Encoding", "gzip");

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("GZipped " + m_request.getRequestURL() + ", Content-Length=" + m_outputStream.size());
         }

         OutputStream os = m_response.getOutputStream();

         m_outputStream.writeTo(os);
         os.close();
         m_gzipStream = null;
         m_outputStream = null;
      }
   }
}
