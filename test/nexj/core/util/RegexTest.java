// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.regex.Pattern;

import junit.framework.TestCase;


/**
 * Verifies the correctness of some regular expressions.
 */
public class RegexTest extends TestCase
{
   // operations

   /**
    * The naive pattern: ([\p{L}_][\p{L}\p{N}_]*\s*)* will blow up exponentially
    * when trying to match the non-matching input: "aaaaaaaaaabbbbbbbbbbcccccccccc-"
    * 
    * This tests optimized pattern: ([\p{L}_][\p{L}\p{N}_]*\s+)*([\p{L}_][\p{L}\p{N}_]*)?
    * 
    * This pattern is from metadata.xsd.
    */
   public void testIdentifierList()
   {
      Pattern pat = Pattern.compile("([\\p{L}_][\\p{L}\\p{N}_]*\\s+)*([\\p{L}_][\\p{L}\\p{N}_]*)?");

      assertTrue(pat.matcher("").matches());
      assertTrue(pat.matcher("a").matches());
      assertTrue(pat.matcher("a b").matches());
      assertTrue(pat.matcher("a_b").matches());
      assertTrue(pat.matcher("a bc").matches());
      assertFalse(pat.matcher("pre-commit xyz").matches());
      assertTrue(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc").matches());
      assertFalse(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc-").matches());
      assertTrue(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc abc def").matches());
      assertTrue(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc abc def ").matches());
      assertFalse(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc- abc def").matches());
      assertFalse(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc-zyx abc def").matches());
   }

   /**
    * The naive pattern: (\*|([\p{L}_][\p{L}\p{N}_]*\s*)*(\s+\*)?) will blow up exponentially
    * when it cannot match certain inputs.
    * 
    * This tests the optimized pattern:
    *    (\*|([\p{L}_][\p{L}\p{N}_]*\s+)*([\p{L}_][\p{L}\p{N}_]*)?(\s+\*)?)
    * 
    * This pattern is from metadata.xsd.
    */
   public void testIdentifierListStar()
   {
      Pattern pat = Pattern.compile("(\\*|([\\p{L}_][\\p{L}\\p{N}_]*\\s+)*([\\p{L}_][\\p{L}\\p{N}_]*)?(\\s+\\*)?)");

      assertTrue(pat.matcher("").matches());
      assertTrue(pat.matcher("a").matches());
      assertTrue(pat.matcher("*").matches());
      assertFalse(pat.matcher("a*").matches());
      assertTrue(pat.matcher("a *").matches());
      assertTrue(pat.matcher("a b").matches());
      assertTrue(pat.matcher("a_b").matches());
      assertTrue(pat.matcher("a bc").matches());
      assertTrue(pat.matcher("a b *").matches());
      assertTrue(pat.matcher("a_b *").matches());
      assertTrue(pat.matcher("a bc *").matches());
      assertFalse(pat.matcher("pre-commit xyz").matches());
      assertTrue(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc").matches());
      assertFalse(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc-").matches());
      assertTrue(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc abc def *").matches());
      assertTrue(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc abc def ").matches());
      assertFalse(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc abc * def").matches());
      assertFalse(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc- abc def *").matches());
      assertFalse(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc-zyx abc def").matches());
   }

   /**
    * The naive pattern: (!?[\p{L}_][\p{L}\p{N}_]*\s*)* will blow up exponentially
    * when it cannot match certain inputs.
    * 
    * This tests the optimized pattern:
    *    (!?[\p{L}_][\p{L}\p{N}_]*\s+)*(!?[\p{L}_][\p{L}\p{N}_]*)?
    * 
    * This pattern is from metadata.xsd.
    */
   public void testAspectList()
   {
      Pattern pat = Pattern.compile("(!?[\\p{L}_][\\p{L}\\p{N}_]*\\s+)*(!?[\\p{L}_][\\p{L}\\p{N}_]*)?");

      assertTrue(pat.matcher("").matches());
      assertTrue(pat.matcher("a").matches());
      assertTrue(pat.matcher("a b").matches());
      assertTrue(pat.matcher("!a b").matches());
      assertTrue(pat.matcher("a !b").matches());
      assertTrue(pat.matcher("!a !b").matches());
      assertTrue(pat.matcher("a_b").matches());
      assertTrue(pat.matcher("a bc").matches());
      assertFalse(pat.matcher("pre-commit xyz").matches());
      assertTrue(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc").matches());
      assertFalse(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc-").matches());
      assertTrue(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc !abc def").matches());
      assertTrue(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc !abc def ").matches());
      assertFalse(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc- abc !def").matches());
      assertFalse(pat.matcher("aaaaaaaaaabbbbbbbbbbcccccccccc-zyx !abc def").matches());
   }
}
