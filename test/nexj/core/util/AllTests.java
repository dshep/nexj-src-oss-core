// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;


import nexj.test.junit.TestSuite;

import junit.framework.Test;

public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.util");
      
      suite.addTest("nexj.core.util.auth.AllTests");
      suite.addTest("nexj.core.util.cipher.AllTests");
      suite.addTestSuite("nexj.core.util.ArrayTabTest");
      suite.addTestSuite("nexj.core.util.TimeZoneUtilTest");
      suite.addTestSuite("nexj.core.util.UIMetaUtilTest");
      suite.addTestSuite("nexj.core.util.WMFParserTest");
      suite.addTest(nexj.core.util.pool.resource.AllTests.suite());
      suite.addTest(nexj.core.util.pool.consumer.AllTests.suite());

      //$JUnit-BEGIN$
      suite.addTestSuite(BeanAccessorTest.class);
      suite.addTestSuite(HashTab2DTest.class);
      suite.addTestSuite(WeakHashHolderTest.class);
      suite.addTestSuite(SoftHashTabTest.class);
      suite.addTestSuite(MIMEUtilTest.class);
      suite.addTestSuite(MultipartInputStreamTest.class);
      suite.addTestSuite(GUIDUtilTest.class);
      suite.addTestSuite(SubstReaderTest.class);
      suite.addTestSuite(HashTabTest.class);
      suite.addTestSuite(PagedArrayListTest.class);
      suite.addTestSuite(MIMEHeaderTest.class);
      suite.addTestSuite(StringUtilTest.class);
      suite.addTestSuite(SoftHashTab2DTest.class);
      suite.addTestSuite(LocaleUtilTest.class);
      suite.addTestSuite(URIUtilTest.class);
      suite.addTestSuite(Base64UtilTest.class);
      suite.addTestSuite(BinaryTest.class);
      suite.addTestSuite(WeakHashTabTest.class);
      suite.addTestSuite(MathUtilTest.class);
      suite.addTestSuite(HTTPTest.class);
      suite.addTestSuite(LimitInputStreamTest.class);
      suite.addTestSuite(HashHolderTest.class);
      suite.addTestSuite(LinkedHashTabTest.class);
      suite.addTestSuite(WeakKeyHashTabTest.class);
      suite.addTestSuite(URLEncodeTest.class);
      suite.addTestSuite(IdentityHashTabTest.class);
      suite.addTestSuite(RegexTest.class);
      suite.addTestSuite(WeakHashTab2DTest.class);
      suite.addTestSuite(HashDequeTest.class);
      suite.addTestSuite(XSDUtilTest.class);
      suite.addTestSuite(URLWriterTest.class);
      suite.addTestSuite(BinaryHeapTest.class);
      suite.addTestSuite(TextPositionReaderTest.class);
      suite.addTestSuite(SOAPUtilTest.class);
      suite.addTestSuite(IndentingXMLWriterTest.class);
      suite.addTestSuite(XMLUtilTest.class);
      suite.addTestSuite(UnlimitedMarkReaderTest.class);
      suite.addTestSuite(NetworkAddressTest.class);
      suite.addTestSuite(Base64InputStreamTest.class);
      suite.addTestSuite(Base64OutputStreamTest.class);
      suite.addTestSuite(HexInputStreamTest.class);
      suite.addTestSuite(HexOutputStreamTest.class);
      suite.addTestSuite(MultiMapTest.class);
      suite.addTestSuite(NetUtilTest.class);
      suite.addTestSuite(ResettableInputStreamTest.class);
      suite.addTestSuite(UTF8BOMIgnoreInputStreamTest.class);
      //$JUnit-END$
      return suite;
   }
}