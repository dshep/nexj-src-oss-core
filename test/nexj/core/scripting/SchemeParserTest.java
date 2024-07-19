// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.io.Reader;
import java.io.StringReader;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import junit.framework.TestCase;

import nexj.core.meta.Primitive;
import nexj.core.util.Binary;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Lookup;
import nexj.core.util.TextPosition;
import nexj.core.util.TextPositionReader;

/**
 * Test case for nexj.core.scripting.SchemeParser.
 */
public class SchemeParserTest extends TestCase
{
   private GlobalEnvironment m_globalEnv = null;
   private SchemeParser m_parser = null;
   private Lookup m_posMap = new IdentityHashTab();

   /**
    * Constructor for SchemeParserTest.
    * @param name
    */
   public SchemeParserTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_globalEnv = new GlobalEnvironment();
      m_parser = new SchemeParser(m_globalEnv);
   }

   /**
    * @see junit.framework.TestCase#tearDown()
    */
   protected void tearDown() throws Exception
   {
      super.tearDown();

      m_globalEnv = null;
      m_parser = null;
   }

   public void testParse()
   {
      Object[] v = (Object[])m_parser.parse(new TextPositionReader(new StringReader(
         "#(1 1.3 -1.23 5.456N 0xabL;hehe\n" +
         " #b10100101 #o377 a 'a'b'c #\\J\r\n" +
         "\"abcdef\\u0041\"123456789012\r" + 
         "2.3e-4 2e-4 2e-4f 2e-4n\n" +
         "(if a b (c '(e . f-1-2-3=t) #(1 2)))" +
         "#z123456abc #m2003-11-11T10:00:00.000 #m2003-11-11 #m10:00:00" +
         "#vu8(0 1 2 127 128 254 255) #(+inf.0 -inf.0 +nan.0 -nan.0))"
         )), m_posMap);

      assertEquals(25, v.length);
      assertTrue(new Integer(1).equals(v[0]));
      assertTrue(new Double(1.3).equals(v[1]));
      assertTrue(new Double(-1.23).equals(v[2]));
      assertTrue(new BigDecimal("5.456").equals(v[3]));
      assertTrue(new Long(0xAB).equals(v[4]));
      assertTrue(new Integer(0xA5).equals(v[5]));
      assertTrue(new Integer(0xFF).equals(v[6]));
      assertTrue(((Symbol)v[7]).toString().equals("a"));
      assertTrue(v[7].equals(((Pair)v[8]).getNext().getHead()));
      assertTrue(new Character('J').equals(v[11]));
      assertTrue("abcdefA".equals(v[12]));
      assertTrue(new Long(123456789012L).equals(v[13]));
      assertTrue(new Double(2.3e-4).equals(v[14]));
      assertTrue(new Double(2e-4).equals(v[15]));
      assertTrue(new Float(2e-4f).equals(v[16]));
      assertTrue(new BigDecimal("2e-4").equals(v[17]));
      
      assertEquals(new TextPosition(0, 0), m_posMap.get(v));
      assertEquals(new TextPosition(1, 20), m_posMap.get(v[8]));
      assertEquals(new TextPosition(1, 24), m_posMap.get(v[10]));
      assertEquals(new TextPosition(4, 12), m_posMap.get(
         ((Pair)((Pair)((Pair)v[18]).getNext().getNext().getNext().getHead()).getNext().getHead()).getNext().getHead()));

      Pair p = (Pair)m_parser.parse(new StringReader("((firstName . #t)(lastName . #t)((@) . #t))"), null);

      assertNotNull(p);
      assertNotNull(p.getTail());
      assertNotNull(p.getNext().getTail());
      assertNull(p.getNext().getNext().getTail());

      assertEquals("+", ((Pair)m_parser.parse(new StringReader("(+ 1)"), null)).getHead().toString());
      assertEquals("+a", ((Pair)m_parser.parse(new StringReader("(+a 1 2 3)"), null)).getHead().toString());
      assertEquals(new Binary(new byte[]{0x01, 0x23 , 0x45, 0x6a, (byte)0xbc}), v[19]);
      assertEquals(new Timestamp(1068544800000L), v[20]);
      assertEquals(new Timestamp(1068508800000L), v[21]);
      assertEquals(new Timestamp(36000000), v[22]);

      assertTrue(v[23] instanceof byte[]);
      assertTrue(Arrays.equals(new byte[]{0, 1, 2, 127, -128, -2, -1}, (byte[])v[23]));

      assertTrue(v[24] instanceof Object[]);
      assertTrue(Arrays.equals(
         new Object[]{
            Primitive.POSITIVE_INF_DOUBLE, Primitive.NEGATIVE_INF_DOUBLE, Primitive.NAN_DOUBLE, Primitive.NAN_DOUBLE},
         (Object[])v[24]));
   }

   public void testSafeParse()
   {
      List errList = new ArrayList();

      m_parser.setErrorList(errList);
      m_parser.setCommenting(true);

      Pair pair = (Pair)m_parser.parse(new TextPositionReader(new StringReader(
         "  ; Comment 1\n\r\n\r\r      ;;  Comment 2\r;Comment 3\n" +
         "  (define (map fun lst) (a . b c) (#grr)"
         )), m_posMap);

      assertEquals(" Comment 2 Comment 3\n", m_parser.getComment());
      assertEquals(3, errList.size());
      assertEquals("(define (map fun lst) (a b c) (0 rr))", Intrinsic.toString(pair));
   }

   public void testComments()
   {
      doTestComments("\n");
      doTestComments("\r\n");
   }

   private void doTestComments(String sLineSep)
   {
      StringBuilder buffer = new StringBuilder();

      buffer.append("; a");
      buffer.append(sLineSep);
      buffer.append(sLineSep);
      buffer.append("; b");
      buffer.append(sLineSep);
      buffer.append("; c");
      buffer.append(sLineSep);
      buffer.append("(define (p a b) (+ a b))");
      buffer.append(sLineSep);

      // Again not starting at beginning of file, and no leading spaces
      buffer.append("; a");
      buffer.append(sLineSep);
      buffer.append(sLineSep);
      buffer.append(";d");
      buffer.append(sLineSep);
      buffer.append(";e");
      buffer.append(sLineSep);
      buffer.append("(define (m a b) (- a b))");
      buffer.append(sLineSep);

      // With paragraph separators
      buffer.append("; a");
      buffer.append(sLineSep);
      buffer.append(sLineSep);
      buffer.append("; f");
      buffer.append(sLineSep);
      buffer.append("; g");
      buffer.append(sLineSep);
      buffer.append("; ");
      buffer.append(sLineSep);
      buffer.append("; h ");
      buffer.append(sLineSep);
      buffer.append(";");
      buffer.append(sLineSep);
      buffer.append(";");
      buffer.append(sLineSep);
      buffer.append("; i");
      buffer.append(sLineSep);
      buffer.append("(define (m a b) (- a b))");
      buffer.append(sLineSep);

      m_parser.setCommenting(true);

      Reader reader = new TextPositionReader(new StringReader(buffer.substring(0)));
      String sComment;

      assertNotNull(m_parser.parse(reader, m_posMap));
      sComment = m_parser.getComment();
      assertEquals("b c\n", sComment);

      assertNotNull(m_parser.parse(reader, m_posMap));
      sComment = m_parser.getComment();
      assertEquals("d e\n", sComment);

      // Paragraph detection
      assertNotNull(m_parser.parse(reader, m_posMap));
      sComment = m_parser.getComment();
      // Extra spaces inserted
      assertEquals("f g \nh  \n i\n", sComment);
   }
   
   public void testExampleComment()
   {
      doTestExampleComment("\n");
      doTestExampleComment("\r\n");
   }
   
   private void doTestExampleComment(String sLineSep)
   {
      StringBuilder b = new StringBuilder();
      
      b.append("; @example");
      b.append(sLineSep);
      b.append("; line1");
      b.append(sLineSep);
      b.append("; line2");
      b.append(sLineSep);
      b.append("(define (m a b) (- a b))");
      b.append(sLineSep);
      
      m_parser.setCommenting(true);

      assertNotNull(m_parser.parse(new TextPositionReader(new StringReader(b.substring(0))), m_posMap));
      assertEquals("@example\nline1\nline2\n", m_parser.getComment()); 
   }
}
