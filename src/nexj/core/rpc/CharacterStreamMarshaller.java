// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import java.io.IOException;
import java.io.Writer;

/**
 * Interface implemented by marshallers serializing to a character stream.
 */
public interface CharacterStreamMarshaller
{
   /**
    * Serializes an object to a character stream.
    * @param obj The object to serialize.
    * @param writer The character stream writer.
    * @throws IOException if an IO error occurs
    * @throws MarshallerException if a marshalling error occurs.
    */
   public void serialize(Object obj, Writer writer) throws IOException, MarshallerException;
}
