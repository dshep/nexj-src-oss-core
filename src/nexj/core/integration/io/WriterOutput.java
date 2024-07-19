// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.io;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.Output;
import nexj.core.util.Binary;

/**
 * Output implementation initialized with a character stream writer.
 */
public class WriterOutput implements Output
{
   // associations

   /**
    * The character stream writer.
    */
   protected Writer m_writer;

   // constructors
   
   /**
    * Constructs the output.
    * @param writer The output writer.
    */
   public WriterOutput(Writer writer)
   {
      m_writer = writer;
   }
   
   // operations
   
   /**
    * @see nexj.core.integration.Output#getOutputStream()
    */
   public OutputStream getOutputStream() throws IntegrationException
   {
      throw new IntegrationException("err.integration.outputStream");
   }

   /**
    * @see nexj.core.integration.Output#getWriter()
    */
   public Writer getWriter() throws IntegrationException
   {
      return m_writer;
   }

   /**
    * @see nexj.core.integration.Output#setBinary(nexj.core.util.Binary)
    */
   public void setBinary(Binary msg) throws IntegrationException
   {
      throw new IntegrationException("err.integration.outputBinary");
   }

   /**
    * @see nexj.core.integration.Output#setString(java.lang.String)
    */
   public void setString(String sMsg) throws IntegrationException
   {
      try
      {
         m_writer.write(sMsg);
      }
      catch (IOException e)
      {
         throw new IntegrationException("err.integration.io", e);
      }
   }

   /**
    * @see nexj.core.integration.Output#setObject(java.lang.Object)
    */
   public void setObject(Object obj) throws IntegrationException
   {
      throw new IntegrationException("err.integration.outputObject");
   }
}
