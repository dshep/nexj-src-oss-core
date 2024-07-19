// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.text;

import java.io.IOException;
import java.io.StringReader;

import nexj.core.meta.Repository;
import nexj.core.rpc.CharacterStreamMarshallerTest;
import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.Pair;

public class TextMarshallerTest extends CharacterStreamMarshallerTest
{
   public TextMarshallerTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      m_marshaller = new TextMarshaller(new InvocationContext(Repository.getMetadata()));
      m_unmarshaller = new TextUnmarshaller(new InvocationContext(Repository.getMetadata()));
   }

   public void testDeserializeExpression() throws IOException
   {
      Pair pair = (Pair)m_unmarshaller.deserialize(new StringReader("3vP3X(1)1#"));

      assertEquals(new Integer(1), ((Pair)pair.getHead()).getHead());
      assertNull(((Pair)pair.getHead()).getTail());
      assertSame(pair.getHead(), pair.getTail());
   }
}
