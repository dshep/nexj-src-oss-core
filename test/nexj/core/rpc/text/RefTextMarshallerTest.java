// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.text;

import nexj.core.meta.Repository;
import nexj.core.runtime.InvocationContext;

public class RefTextMarshallerTest extends TextMarshallerTest
{
   public RefTextMarshallerTest(String sName)
   {
      super(sName);
   }

   /**
    * @see nexj.core.rpc.text.TextMarshallerTest#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      m_marshaller = new RefTextMarshaller(new InvocationContext(Repository.getMetadata()));
   }
}
