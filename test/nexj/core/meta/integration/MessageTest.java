// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import nexj.core.meta.Metadata;
import nexj.core.meta.Repository;

import junit.framework.TestCase;

/**
 * Tests the Message metadata.
 */
public class MessageTest extends TestCase
{
   /**
    * Tests the isUpcast() method.
    */
   public void testIsUpcast()
   {
      Metadata metadata = Repository.getMetadata();
      Message parent = metadata.getMessage("XML_Inherit_Parent");
      Message child1 = metadata.getMessage("XML_Inherit_Child1");
      Message child1Child = metadata.getMessage("XML_Inherit_Child1_Child");
      Message other = metadata.getMessage("XMLAnyTest");

      assertTrue(parent.isUpcast(child1));
      assertFalse(child1.isUpcast(parent));

      assertTrue(child1.isUpcast(child1Child));
      assertFalse(child1Child.isUpcast(child1));

      assertTrue(parent.isUpcast(child1Child));
      assertFalse(child1Child.isUpcast(parent));

      assertTrue(parent.isUpcast(parent));
      assertTrue(child1.isUpcast(child1));
      assertTrue(child1Child.isUpcast(child1Child));


      assertTrue(other.isUpcast(other));
      assertFalse(other.isUpcast(parent));
      assertFalse(parent.isUpcast(other));
      assertFalse(other.isUpcast(child1Child));
      assertFalse(child1Child.isUpcast(other));
   }
}
