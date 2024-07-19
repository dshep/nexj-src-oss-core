// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import junit.framework.TestCase;

public class ArgumentTest extends TestCase
{
   private Argument m_x = null;

   public ArgumentTest(String name)
   {
      super(name);
   }

   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_x = ((Event)Repository.getMetadata().getMetaclass("Principal").getSelector("test").getMember(2)).getArgument("x");
   }

   public void testGetEvent()
   {
      assertEquals("test", m_x.getEvent().getName());
   }

   public void testGetName()
   {
      assertEquals("x", m_x.getName());
   }

   public void testGetOrdinal()
   {
      assertEquals(0, m_x.getOrdinal());
   }

   public void testIsCollection()
   {
      Argument arg = new Argument(null);

      assertFalse(arg.isCollection());
      arg.setCollection(true);
      assertTrue(arg.isCollection());
      arg.setCollection(false);
      assertFalse(arg.isCollection());
   }

   public void testIsCompatibleWith()
   {
      Argument arg = new Argument(null);
      Argument base = new Argument(null);

      assertTrue(arg.isCompatibleWith(base));

      arg.setType(Repository.getMetadata().getMetaclass("User"));
      assertTrue(arg.isCompatibleWith(base));
      assertTrue(base.isCompatibleWith(arg));

      base.setType(Repository.getMetadata().getMetaclass("Object"));
      assertTrue(arg.isCompatibleWith(base));
      assertFalse(base.isCompatibleWith(arg));

      base.setType(Repository.getMetadata().getMetaclass("User"));
      assertTrue(arg.isCompatibleWith(base));
      assertTrue(base.isCompatibleWith(arg));

      arg.setCollection(true);
      assertFalse(arg.isCompatibleWith(base));
      assertFalse(base.isCompatibleWith(arg));

      base.setCollection(true);
      assertTrue(arg.isCompatibleWith(base));
      assertTrue(base.isCompatibleWith(arg));

      arg.setRequired(true); // isRequired() not validated
      assertTrue(arg.isCompatibleWith(base));
      assertTrue(base.isCompatibleWith(arg));

      base.setRequired(true); // isRequired() not validated
      assertTrue(arg.isCompatibleWith(base));
      assertTrue(base.isCompatibleWith(arg));
   }

   public void testIsRequired()
   {
      Argument arg = new Argument(null);

      assertFalse(arg.isRequired());
      arg.setRequired(true);
      assertTrue(arg.isRequired());
      arg.setRequired(false);
      assertFalse(arg.isRequired());
   }

   public void testToString()
   {
      assertEquals("Argument x", m_x.toString());
   }
}
