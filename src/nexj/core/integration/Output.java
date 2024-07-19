// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import java.io.OutputStream;
import java.io.Writer;

import nexj.core.util.Binary;

/**
 * Message output interface.
 */
public interface Output
{
   /**
    * @return The binary output stream.
    */
   OutputStream getOutputStream() throws IntegrationException;
   
   /**
    * @return The character stream writer.
    */
   Writer getWriter() throws IntegrationException;
   
   /**
    * Sets the binary serialized representation of the message.
    * @param msg The binary value to set.
    */
   void setBinary(Binary msg) throws IntegrationException;
   
   /**
    * Sets the string representation of the message.
    * @param sMsg The string value to set.
    */
   void setString(String sMsg) throws IntegrationException;
   
   /**
    * Sets the object representation of the message.
    */
   void setObject(Object obj) throws IntegrationException;
}
