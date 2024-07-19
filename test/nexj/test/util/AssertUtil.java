// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.test.util;

import java.net.URL;

import junit.framework.Assert;

import nexj.core.rpc.TransferObject;
import nexj.core.scripting.Pair;
import nexj.core.util.HashTab2D;
import nexj.core.util.Lookup2D;
import nexj.core.util.PropertyIterator;

/**
 * JUnit assertion utilities.
 */
public class AssertUtil
{
   /**
    * Prevents construction.
    */
   protected AssertUtil()
   {
   }

   /**
    * Asserts that a substring is present in another string.
    * @param needle The substring to search for.
    * @param sHaystack The string to search in.
    */
   public static void assertContained(CharSequence needle, String sHaystack)
   {
      if (!sHaystack.contains(needle))
      {
         Assert.fail("Test substring (" + needle + ") not found in value (" + sHaystack + ").");
      }
   }

   /**
    * Asserts that an object is equal to its serialized representation.
    * @param expected The base URL of the expected serialized representation.
    * @param actual The actual object.
    */
   public static void assertEquals(URL expected, Object actual)
   {
      if (!CmpUtil.equal(expected, actual))
      {
         Assert.fail("Test object does not match the serialized representation at \"" + expected + "\"");
      }
   }

   /**
    * Asserts that an object is equal to its serialized representation.
    * @param expected The URL of the expected serialized representation.
    * @param actual The actual object.
    * @param attributes The list of attributes to use in the comparison.
    */
   public static void assertEquals(URL expected, Object actual, Pair attributes)
   {
      if (!CmpUtil.equal(expected, actual, attributes))
      {
         Assert.fail("Test object does not match the serialized representation at \"" + expected + "\"");
      }
   }

   /**
    * Asserts that an object is equal to its serialized representation, ignoring the OIDs.
    * @param expected The URL of the expected serialized representation.
    * @param actual The actual object.
    * @param attributes The list of attributes to use in the comparison.
    */
   public static void assertEqualsIgnoreOIDs(URL expected, Object actual, Pair attributes)
   {
      if (!CmpUtil.equalIgnoreOIDs(expected, actual, attributes))
      {
         Assert.fail("Test object does not match the serialized representation at \"" + expected + "\"");
      }
   }

   /**
    * Asserts that two transfer object are equal.
    * @param left The left TO.
    * @param right The right TO.
    * @param identityMap Map for tracking circular references.
    */
   protected static void assertEquals(TransferObject left, TransferObject right, Lookup2D identityMap)
   {
      if (identityMap.put(left, right, Boolean.TRUE) != null)
      {
         return;
      }

      Assert.assertEquals(left.getClassName(), right.getClassName());
      Assert.assertEquals(left.getEventName(), right.getEventName());
      Assert.assertEquals(left.getOID(), right.getOID());
      Assert.assertEquals(left.getVersion(), right.getVersion());
      Assert.assertEquals(left.getValueCount(), right.getValueCount());

      for (PropertyIterator itr = left.getIterator(); itr.hasNext();)
      {
         itr.next();

         Object leftValue = itr.getValue();
         Object rightValue = right.getValue(itr.getName());

         if (leftValue instanceof TransferObject &&
            rightValue instanceof TransferObject)
         {
            assertEquals((TransferObject)leftValue, (TransferObject)rightValue, identityMap); 
         }
         else
         {
            Assert.assertEquals(leftValue, rightValue);
         }
      }
   }

   /**
    * Asserts that two transfer object are equal.
    * @param left The left TO.
    * @param right The right TO.
    */
   public static void assertEquals(TransferObject left, TransferObject right)
   {
      assertEquals(left, right, new HashTab2D());
   }
}
