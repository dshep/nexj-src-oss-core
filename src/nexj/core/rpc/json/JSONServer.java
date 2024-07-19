// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.json;

import nexj.core.rpc.CharacterStreamMarshaller;
import nexj.core.rpc.CharacterStreamUnmarshaller;
import nexj.core.rpc.GenericCharacterStreamServer;
import nexj.core.rpc.Request;

/**
 * The JSON request server.
 */
public class JSONServer extends GenericCharacterStreamServer
{
   // operations

   /**
    * @see nexj.core.rpc.GenericCharacterStreamServer#getResponseMarshaller(nexj.core.rpc.Request)
    */
   protected CharacterStreamMarshaller getResponseMarshaller(Request request)
   {
      return new JSONMarshaller(m_context);
   }

   /**
    * @see nexj.core.rpc.GenericCharacterStreamServer#getRequestUnmarshaller()
    */
   protected CharacterStreamUnmarshaller getRequestUnmarshaller()
   {
      return new JSONUnmarshaller(m_context);
   }

   /**
    * @see nexj.core.rpc.GenericCharacterStreamServer#getType()
    */
   protected String getType()
   {
      return "json";
   }
}