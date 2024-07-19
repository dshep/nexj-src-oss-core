// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.io;

import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import nexj.core.integration.Input;
import nexj.core.integration.IntegrationException;
import nexj.core.util.Binary;

/**
 * Input implementation with a string.
 */
public class StringInput implements Input
{
   // attributes
   
   protected String m_sText;

   // constants
   
   /**
    * Constructs the input.
    * @param sText The input string.
    */
   public StringInput(String sText)
   {
      m_sText = sText;
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
      return new StringReader(m_sText);
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
      return m_sText;
   }

   /**
    * @see nexj.core.integration.Input#getObject()
    */
   public Object getObject() throws IntegrationException
   {
      return m_sText;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return m_sText;
   }
}
