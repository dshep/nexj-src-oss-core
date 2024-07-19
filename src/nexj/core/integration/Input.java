// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import java.io.InputStream;
import java.io.Reader;

import nexj.core.util.Binary;

/**
 * Message input interface.
 */
public interface Input
{
   /**
    * @return The binary input stream.
    */
   InputStream getInputStream() throws IntegrationException;
   
   /**
    * @return The character stream reader.
    */
   Reader getReader() throws IntegrationException;
   
   /**
    * @return The binary serialized representation of the message.
    */
   Binary getBinary() throws IntegrationException;
   
   /**
    * @return The string representation of the message.
    */
   String getString() throws IntegrationException;
   
   /**
    * @return The object representation of the message.
    */
   Object getObject() throws IntegrationException;
}
