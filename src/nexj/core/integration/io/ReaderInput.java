// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.io;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringWriter;

import nexj.core.integration.Input;
import nexj.core.integration.IntegrationException;
import nexj.core.util.Binary;
import nexj.core.util.IOUtil;

/**
 * Input implementation with a character input stream.
 */
public class ReaderInput implements Input
{
   // associations
   
   protected Reader m_reader;
   
   // constructors
   
   /**
    * Constructs the input.
    * @param reader The character stream reader.
    */
   public ReaderInput(Reader reader)
   {
      m_reader = reader;
   }

   // operations
   
   /**
    * @see nexj.core.integration.Input#getInputStream()
    */
   public InputStream getInputStream() throws IntegrationException
   {
      throw new IntegrationException("err.integration.inputStream");
   }

   /**
    * @see nexj.core.integration.Input#getReader()
    */
   public Reader getReader() throws IntegrationException
   {
      return m_reader;
   }

   /**
    * @see nexj.core.integration.Input#getBinary()
    */
   public Binary getBinary() throws IntegrationException
   {
      throw new IntegrationException("err.integration.binary");
   }

   /**
    * @see nexj.core.integration.Input#getString()
    */
   public String getString() throws IntegrationException
   {
      StringWriter writer = new StringWriter(128);

      try
      {
         IOUtil.copy(writer, m_reader);
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
      return getString();
   }
}
