// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.util.Arrays;

import junit.framework.TestCase;

import nexj.core.meta.Primitive;

public class ExpressionTest extends TestCase
{
   private Pair m_add;
   private Pair m_attr;

   public ExpressionTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_add = Pair.binary(Symbol.PLUS,
         Primitive.createInteger(1),
         Primitive.createInteger(2));
      m_attr = Pair.attribute("a");
   }

   public void testGetOperator()
   {

      assertSame(Symbol.PLUS, m_add.getOperator());
   }

   public void testFrom()
   {
      assertEquals("(@ addresses city)", Pair.attribute("city").from("addresses").toString());
      assertEquals("(@ addresses city name)", Pair.attribute("name").from("addresses city").toString());
      assertEquals("(@ addresses city name)", Pair.attribute("name").from("addresses  city").toString());
   }

   public void testAfter()
   {
      assertEquals("(+ 3 1 2)", m_add.after(Primitive.createInteger(3)).toString());
   }

   /*
    * Test for Expression create(Symbol, Object)
    */
   public void testCreateSymbolObject()
   {
      assertEquals("(* (+ 1 2) 3)", m_add.binary(Symbol.MUL, Primitive.createInteger(3)).toString());
   }

   /*
    * Test for Expression create(Symbol)
    */
   public void testCreateSymbol()
   {
      assertEquals("(- (+ 1 2))", m_add.unary(Symbol.MINUS).toString());
   }

   public void testOr()
   {
      assertEquals("(or (@ a) #f)", m_attr.or(Boolean.FALSE).toString());
   }

   public void testAnd()
   {
      assertEquals("(and (@ a) #t)", m_attr.and(Boolean.TRUE).toString());
   }

   public void testEq()
   {
      assertEquals("(= (+ 1 2) 3)", m_add.eq(Primitive.createInteger(3)).toString());
   }

   public void testNe()
   {
      assertEquals("(!= (+ 1 2) 4)", m_add.ne(Primitive.createInteger(4)).toString());
   }

   public void testGt()
   {
      assertEquals("(> (+ 1 2) 0)", m_add.gt(Primitive.createInteger(0)).toString());
   }

   public void testGe()
   {
      assertEquals("(>= (+ 1 2) 0)", m_add.ge(Primitive.createInteger(0)).toString());
   }

   public void testLt()
   {
      assertEquals("(< (+ 1 2) 4)", m_add.lt(Primitive.createInteger(4)).toString());
   }

   public void testLe()
   {
      assertEquals("(<= (+ 1 2) 4)", m_add.le(Primitive.createInteger(4)).toString());
   }

   public void testLike()
   {
      assertEquals("(like? (@ a) \"A*\")", m_attr.like("A*").toString());
   }

   public void testIn()
   {
      assertEquals("(in? (@ a) \"A\" \"B\" \"C\")",
         m_attr.in(Pair.fromIterator(Arrays.asList(new Object[]{"A", "B", "C"}).iterator())).toString());
   }

   public void testPlus()
   {
      assertEquals("(+ (+ 1 2) 3)", m_add.plus(Primitive.createInteger(3)).toString());
   }

   public void testMinus()
   {
      assertEquals("(- (+ 1 2) 3)", m_add.minus(Primitive.createInteger(3)).toString());
   }

   public void testMul()
   {
      assertEquals("(* (+ 1 2) 3)", m_add.mul(Primitive.createInteger(3)).toString());
   }

   public void testDiv()
   {
      assertEquals("(/ (+ 1 2) 3)", m_add.div(Primitive.createInteger(3)).toString());
   }

   public void testNeg()
   {
      assertEquals("(- (+ 1 2))", m_add.neg().toString());
   }

   public void testNot()
   {
      assertEquals("(not (@ a))", m_attr.not().toString());
   }
   
   public void testExpr()
   {
      assertEquals("(and (like? (@ address city) \"A*\") (>= (@ birthDate) 1980-01-01))",
         Pair.attribute("city").from("address").like("A*")
         .and(Pair.attribute("birthDate").ge(java.sql.Date.valueOf("1980-01-01"))).toString());
   }
}
