package nexj.core.meta.testing.unit;

import nexj.core.meta.NamedMetadataObject;
import nexj.core.scripting.Pair;

/**
 * A test case from a unit test
 */
public class UnitTestCase extends NamedMetadataObject
{
   /**
    * The test case script.
    */
   protected Pair m_body;

   /**
    * Sets the test case script.
    * @param body The test case script to set.
    */
   public void setBody(Pair body)
   {
      verifyNotReadOnly();
      m_body = body;
   }

   /**
    * @return The test case script.
    */
   public Pair getBody()
   {
      return m_body;
   }
}
