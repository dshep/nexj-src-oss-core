// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import java.util.Locale;

import junit.framework.TestCase;

import nexj.core.meta.Metaclass;
import nexj.core.meta.Repository;
import nexj.core.persistence.Field;
import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.Pair;
import nexj.core.scripting.ParserException;
import nexj.core.scripting.match.ExpressionParser;
import nexj.core.scripting.match.MatchNode;

public class MatchOperatorTest extends TestCase
{
   private ExpressionParser m_parser = new ExpressionParser();

   /**
    * Parse an expression in simplified infix notation.
    * @param expression The expression to parse.
    */
   private Pair parse(String expression)
   {
      return MatchNode.parse(m_parser.parse(expression));
   }

   public void testEvaluation()
   {
      InvocationContext context = new InvocationContext(Repository.getMetadata());
      MatchOperator op = new MatchOperator(context);
      Metaclass metaclass = context.getMetadata().getMetaclass("Contact");
      AttributeOperator attribOp = new AttributeOperator(
         new Field(new Query(metaclass, context), metaclass.getAttribute("firstName")));

      context.setLocale(Locale.ENGLISH);
      op.setAttribute(attribOp);
      attribOp.setConstant(true);
      attribOp.setValue("a abc def ghi");

      op.setExpression(parse("a and b"));
      op.normalize(Operator.NORMALIZE_NORECUR); // force expression parsing
      assertEquals(null, op.evaluate()); // min(1/7, 0/7) && > 0

      op.setExpression(parse("a or b"));
      op.normalize(Operator.NORMALIZE_NORECUR); // force expression parsing
      assertEquals(new Double(1/7d), op.evaluate()); // max(1/7, 0/7)

      op.setExpression(parse("a or b and c or abc"));
      op.normalize(Operator.NORMALIZE_NORECUR); // force expression parsing
      assertEquals(new Double(1/3d), op.evaluate()); // max(1/7, min(0/7, 0/7), 1/3)

      op.setExpression(parse("not a"));
      op.normalize(Operator.NORMALIZE_NORECUR); // force expression parsing
      assertEquals(null, op.evaluate());

      op.setExpression(parse("a*0.2 or abc * 0.0"));
      op.normalize(Operator.NORMALIZE_NORECUR); // force expression parsing
      assertEquals(new Double(53/105d), op.evaluate()); // (1.2) * 1/7 + 1/3 = 53/105

      op.setExpression(parse("a not b"));
      op.normalize(Operator.NORMALIZE_NORECUR); // force expression parsing
      assertEquals(new Double(1/7d), op.evaluate()); // 1/7

      op.setExpression(parse("like? b"));
      op.normalize(Operator.NORMALIZE_NORECUR); // force expression parsing
      assertEquals(new Double(1/13d), op.evaluate()); // 1/13

      op.setExpression(parse("def and a and ghi"));
      op.normalize(Operator.NORMALIZE_NORECUR); // force expression parsing
      assertEquals(new Double(1/7d), op.evaluate()); // min(1/3, 1/7, 1/3)

      op.setExpression(parse("def and a not abc and ghi"));
      op.normalize(Operator.NORMALIZE_NORECUR); // force expression parsing
      assertEquals(null, op.evaluate());

      op.setExpression(parse("(def or a) and abc"));
      op.normalize(Operator.NORMALIZE_NORECUR); // force expression parsing
      assertEquals(new Double(1/3d), op.evaluate()); // min(max(1/3, 1/7), 1/3)

      op.setExpression(parse("A or a"));
      op.normalize(Operator.NORMALIZE_NORECUR); // force expression parsing
      assertEquals(new Double(1/7d), op.evaluate()); // max(1/7, 1/7)

      attribOp.setValue("defgabchijkl");
      op.setExpression(parse("like? abc"));
      op.normalize(Operator.NORMALIZE_NORECUR); // force expression parsing
      assertEquals(new Double(3/12d), op.evaluate());

      attribOp.setValue("abcdefghijkl");
      op.setExpression(parse("like? dfh"));
      op.normalize(Operator.NORMALIZE_NORECUR); // force expression parsing
      assertEquals(new Double(1/12d), op.evaluate());

      attribOp.setValue("abcaadbecd");
      op.setExpression(parse("like? abc"));
      op.normalize(Operator.NORMALIZE_NORECUR); // force expression parsing
      assertEquals(new Double(3/10d), op.evaluate());

      attribOp.setValue("abcaadbecd");
      op.setExpression(parse("like? c"));
      op.normalize(Operator.NORMALIZE_NORECUR); // force expression parsing
      assertEquals(new Double(2/10d), op.evaluate());
   }

   public void testParser()
   {
      assertEquals("(\"a\")", m_parser.parse("a").toString());
      assertEquals("(or \"a\" \"b\")", m_parser.parse("a or b").toString());
      assertEquals("(and \"a\" \"b\")", m_parser.parse("a and b").toString());
      assertEquals("(and \"a\" (not \"b\"))", m_parser.parse("a not b").toString());
      assertEquals("(not \"a\")", m_parser.parse("not a").toString());
      assertEquals("(like? \"a\")", m_parser.parse("like? a").toString());
      assertEquals("(and (* (5.0 \"a\")) (* (6.0 \"b\")))",
                   m_parser.parse("a * 5 and b*6").toString());
      assertEquals("(* (5.0 \"a\") (6.0 \"b\"))", m_parser.parse("a * 5 or b*6").toString());
      assertEquals("(or (not \"a\") \"b\")", m_parser.parse("not a or b").toString());
      assertEquals("(and (not \"a\") \"b\")", m_parser.parse("not a and b").toString());
      assertEquals("(and \"a\" (not \"b\"))", m_parser.parse("a not b").toString());
      assertEquals("(or \"a\" (not \"b\"))", m_parser.parse("a or not b").toString());
      assertEquals("(and \"a\" (not \"b\"))", m_parser.parse("a and not b").toString());
      assertEquals("(not (and \"a\" \"b\"))", m_parser.parse("not (a and b)").toString());
      assertEquals("(\"a\")", m_parser.parse("(a)").toString());
      assertEquals("(or \"a\" \"b\")", m_parser.parse("(a or b)").toString());
      assertEquals("(and \"a\" \"b\")", m_parser.parse("(a and b)").toString());
      assertEquals("(and (or \"a\" \"b\") \"c\")", m_parser.parse("(a or b) and c").toString());
      assertEquals("(or (and \"a\" \"b\") \"c\")", m_parser.parse("(a and b) or c").toString());
      assertEquals("(\"a\")", m_parser.parse("(((a)))").toString());
      assertEquals("(or \"a\" \"b\")", m_parser.parse("(((a or b)))").toString());
      assertEquals("(or \"a b c\" \"d e f\")",
                   m_parser.parse("\"a b c\" or \"d e f\"").toString());
      assertEquals("(or (* (5.0 \"abc\") (6.0 \"def\")) \"7\")",
                   m_parser.parse("abc * 5 or def * 6 or 7").toString());
      assertEquals("(and (* (5.0 \"abc\") (6.0 \"def\")) \"7\")",
                   m_parser.parse("(abc * 5 or def * 6) and 7").toString());

      // Error validation
      try
      {
         m_parser.parse(")");
         fail("Exception expected.");
      }
      catch (ParserException e)
      {
         assertEquals(")", e.getErrorArgs()[0]);
         assertEquals(0, e.getTextPosition().getColumn());
      }

      try
      {
         m_parser.parse("(a or b");
         fail("Exception expected.");
      }
      catch (ParserException e)
      {
         assertEquals("(a or b", e.getErrorArgs()[0]);
         assertEquals(0, e.getTextPosition().getColumn());
      }

      try
      {
         m_parser.parse("a b");
         fail("Exception expected.");
      }
      catch (ParserException e)
      {
         assertEquals("a b", e.getErrorArgs()[0]);
         assertEquals(2, e.getTextPosition().getColumn());
      }

      try
      {
         m_parser.parse("a (b)");
         fail("Exception expected.");
      }
      catch (ParserException e)
      {
         assertEquals("a (b)", e.getErrorArgs()[0]);
         assertEquals(2, e.getTextPosition().getColumn());
      }

      try
      {
         m_parser.parse("abc * 5 or def * 6 * 7");
         fail("Exception expected.");
      }
      catch (ParserException e)
      {
         assertEquals("abc * 5 or def * 6 * 7", e.getErrorArgs()[0]);
         assertEquals(19, e.getTextPosition().getColumn());
      }

      try
      {
         m_parser.parse("a or");
         fail("Exception expected.");
      }
      catch (ParserException e)
      {
         assertEquals("a or", e.getErrorArgs()[0]);
         assertEquals(4, e.getTextPosition().getColumn());
      }

      try
      {
         m_parser.parse("a and");
         fail("Exception expected.");
      }
      catch (ParserException e)
      {
         assertEquals("a and", e.getErrorArgs()[0]);
         assertEquals(5, e.getTextPosition().getColumn());
      }

      try
      {
         m_parser.parse("a not");
         fail("Exception expected.");
      }
      catch (ParserException e)
      {
         assertEquals("a not", e.getErrorArgs()[0]);
         assertEquals(5, e.getTextPosition().getColumn());
      }

      try
      {
         m_parser.parse("and a");
         fail("Exception expected.");
      }
      catch (ParserException e)
      {
         assertEquals("and a", e.getErrorArgs()[0]);
         assertEquals(4, e.getTextPosition().getColumn());
      }

      try
      {
         m_parser.parse("or a");
         fail("Exception expected.");
      }
      catch (ParserException e)
      {
         assertEquals("or a", e.getErrorArgs()[0]);
         assertEquals(3, e.getTextPosition().getColumn());
      }

      try
      {
         m_parser.parse("* a");
         fail("Exception expected.");
      }
      catch (ParserException e)
      {
         assertEquals("* a", e.getErrorArgs()[0]);
         assertEquals(2, e.getTextPosition().getColumn());
      }

      try
      {
         m_parser.parse("a *");
         fail("Exception expected.");
      }
      catch (ParserException e)
      {
         assertEquals("a *", e.getErrorArgs()[0]);
         assertEquals(3, e.getTextPosition().getColumn());
      }
   }
}
