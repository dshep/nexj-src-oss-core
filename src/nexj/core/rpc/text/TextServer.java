// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.text;

import java.io.IOException;
import java.io.Reader;

import nexj.core.rpc.CharacterStreamMarshaller;
import nexj.core.rpc.CharacterStreamUnmarshaller;
import nexj.core.rpc.GenericCharacterStreamServer;
import nexj.core.rpc.Request;

/**
 * The text request server.
 */
public class TextServer extends GenericCharacterStreamServer
{
   // attributes

   /**
    * The request serialization format version.
    */
   protected int m_nVersion;

   // operations

   /**
    * @see nexj.core.rpc.GenericCharacterStreamServer#getResponseMarshaller(nexj.core.rpc.Request)
    */
   protected CharacterStreamMarshaller getResponseMarshaller(Request request)
   {
      TextMarshaller msh = (request.getInvocationCount() != 0 &&
         "SysMetadata".equals(request.getObject(0).getClassName()))
         ? new RefTextMarshaller(m_context) : new TextMarshaller(m_context);

      msh.setVersion(m_nVersion);

      return msh;
   }

   /**
    * @see nexj.core.rpc.GenericCharacterStreamServer#getRequestUnmarshaller()
    */
   protected CharacterStreamUnmarshaller getRequestUnmarshaller()
   {
      return new TextUnmarshaller(m_context);
   }

   /**
    * @see nexj.core.rpc.GenericCharacterStreamServer#getRequest(java.io.Reader)
    */
   protected Request getRequest(Reader reader) throws IOException
   {
      TextUnmarshaller unmsh = (TextUnmarshaller)getRequestUnmarshaller();
      Request request = (Request)unmsh.deserialize(reader);

      m_nVersion = unmsh.getVersion();

      return request;
   }

   /**
    * @see nexj.core.rpc.GenericCharacterStreamServer#getType()
    */
   protected String getType()
   {
      return "text";
   }
}
