// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import junit.framework.TestCase;

public class RepositoryTest extends TestCase
{
   public RepositoryTest(String name)
   {
      super(name);
   }

   public void testGetMetadata()
   {
      assertNotNull(Repository.getMetadata());
   }
}
