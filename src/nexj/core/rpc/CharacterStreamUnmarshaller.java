// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import java.io.IOException;
import java.io.Reader;

/**
 * Interface implemented by unmarshallers deserializing from a character stream.
 */
public interface CharacterStreamUnmarshaller
{
   /**
    * Deserializes an object from a character stream containing a message.
    * @param reader The character stream reader.
    * @return The deserialized object.
    * @throws IOException if an IO error occurs
    * @throws UnmarshallerException if an unmarshalling error occurs.
    */
   public Object deserialize(Reader reader) throws IOException, UnmarshallerException;
}
