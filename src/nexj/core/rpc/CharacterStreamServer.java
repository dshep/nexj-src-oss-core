// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import java.io.Reader;
import java.io.Writer;

/**
 * Interface implemented by servers that
 * produce a character stream response to
 * a character stream request.
 */
public interface CharacterStreamServer
{
   /**
    * Invokes the server with input and output streams.
    * @param reader The input stream reader, containing the request.
    * @param writer The output stream writer, which will return the response.
    * @param preprocessor The request preprocessor. Can be null.
    * @throws RPCException When an invalid request is encountered.
    */
   void invoke(Reader reader, Writer writer, Preprocessor preprocessor) throws RPCException;
}
