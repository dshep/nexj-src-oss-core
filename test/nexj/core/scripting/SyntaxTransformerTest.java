package nexj.core.scripting;

import java.io.StringReader;

import junit.framework.TestCase;

import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.runtime.InvocationContext;

/**
 * Tests transformer function compilation and expansion.
 */
public class SyntaxTransformerTest extends TestCase
{
   private SchemeParser m_parser;
   private Compiler m_compiler;
   private Machine m_machine;

   /**
    * Constructor for TransformerTest.
    * @param name
    */
   public SyntaxTransformerTest(String name)
   {
      super(name);
   }

   /**
    * @see junit.framework.TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      m_parser = new SchemeParser(new GlobalEnvironment());
      m_compiler = new Compiler();
      m_machine = new InvocationContext(Repository.getMetadata()).getMachine();
   }

   /**
    * @see junit.framework.TestCase#tearDown()
    */
   protected void tearDown() throws Exception
   {
      super.tearDown();

      m_parser = null;
      m_compiler = null;
      m_machine = null;
   }
   
   /**
    * Convenience function. Parses the given string.
    */
   protected Object parse(String sValue)
   {
      return m_parser.parse(new StringReader(sValue), null);
   }

   /**
    * Convenience function. Parses the given string, compiles into p-code and invokes the p-code function.
    */
   protected Object invoke(String sValue)
   {
      PCodeFunction func = m_compiler.compile(parse(sValue), null, m_machine, true);

      return m_machine.invoke(func, (Pair)null);
   }

   /**
    * Convenience function. Parses and evaluates the given string.
    */
   protected Object eval(String sValue)
   {
      Object expr = parse(sValue);

      assertTrue(m_machine.isEvalSupported(expr));

      return m_machine.eval(expr);
   }

   /**
    * Tests transformer expansion in Machine.eval.
    */
   public void testEval()
   {
      invoke("(define p (list 0 2))");
      invoke("(define-syntax p.car (identifier-syntax (y (car p)) ((set! _ e) (set-car! p e))))");
      invoke("(define-syntax contains? (syntax-rules () ((_ () val) #f) ((_ (e) val) (= val e)) ((_ (e1 e2 ...) val) (if (= val e1) #t (contains? (e2 ...) val)))))");

      assertTrue(m_machine.isEvalSupported(Symbol.CAR));
      assertEquals(Primitive.ZERO_INTEGER, eval("p.car"));

      invoke("(set! p.car 1)");
      assertEquals(Primitive.ZERO_INTEGER, eval("(- p.car 1)"));
      assertEquals(Pair.list(Primitive.ONE_INTEGER, Primitive.createInteger(2)), eval("p"));

      assertSame(Boolean.FALSE, eval("(contains? () 1)"));
      assertSame(Boolean.TRUE, eval("(contains? (1) 1)"));
      assertSame(Boolean.FALSE, eval("(contains? (2) 1)"));
      assertSame(Boolean.TRUE, eval("(contains? (1 2 3 4) 1)"));
      assertSame(Boolean.TRUE, eval("(contains? (2 3 1 4) 1)"));
      assertSame(Boolean.TRUE, eval("(contains? (2 3 4 1) 1)"));
      assertSame(Boolean.FALSE, eval("(contains? (2 3 4 5) 1)"));
   }

   /**
    * Tests transformer definition with raw symbols.
    */
   public void testInvalidRawSymbols()
   {
      try
      {
         invoke("(define-syntax transformer (lambda (x) (syntax-case x () ((_ a) a))))");
      }
      catch (CompilerException e)
      {
         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());

         Throwable t = e.getCause();

         assertTrue(t instanceof CompilerException);

         e = (CompilerException)t;

         Object[] args = e.getErrorArgs();

         assertEquals("err.compiler.syntax.nonsyntaxPatternVariable", e.getErrorCode());
         assertEquals(1, args.length);
         assertEquals("a", args[0].toString());

         return;
      }

      assertTrue(false); // shouldn't get here
   }

   /**
    * Tests transformer definition with raw symbols defined locally.
    */
   public void testInvalidLocalRawSymbols()
   {
      try
      {
         invoke("(let ((a 123)) (define-syntax transformer (lambda (x) (syntax-case x () ((_ a) a)))))");
      }
      catch (CompilerException e)
      {
         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());

         Throwable t = e.getCause();

         assertTrue(t instanceof CompilerException);

         e = (CompilerException)t;

         Object[] args = e.getErrorArgs();

         assertEquals("err.compiler.syntax.nonsyntaxPatternVariable", e.getErrorCode());
         assertEquals(1, args.length);
         assertEquals("a", args[0].toString());

         return;
      }

      assertTrue(false); // shouldn't get here
   }

   /**
    * Tests external local variable referenced from inside a transformer.
    */
   public void testInvalidLocalRef()
   {
      try
      {
         invoke("(let ((local 123)) (define-syntax transformer (lambda (x) (syntax-case x () (_ local)))))");
      }
      catch (CompilerException e)
      {
         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());

         Throwable t = e.getCause();

         assertTrue(t instanceof CompilerException);

         e = (CompilerException)t;

         Object[] args = e.getErrorArgs();

         assertEquals("err.compiler.syntax.identifierRef", e.getErrorCode());
         assertEquals(1, args.length);
         assertEquals("local", args[0].toString());

         return;
      }

      assertTrue(false); // shouldn't get here
   }

   /**
    * Tests global variable referenced from inside a transformer.
    */
   public void testInvalidGlobalRef()
   {
      try
      {
         invoke("(let ((global 123)) (define-syntax transformer (lambda (x) (syntax-case x () (_ global)))))");
      }
      catch (CompilerException e)
      {
         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());

         Throwable t = e.getCause();

         assertTrue(t instanceof CompilerException);

         e = (CompilerException)t;

         Object[] args = e.getErrorArgs();

         assertEquals("err.compiler.syntax.identifierRef", e.getErrorCode());
         assertEquals(1, args.length);
         assertEquals("global", args[0].toString());

         return;
      }

      assertTrue(false); // shouldn't get here
   }

   /**
    * Tests invalid syntax-rules clause.
    */
   public void testInvalidSyntaxRulesClause()
   {
      int nError = 0;

      try
      {
         invoke("(define-syntax transformer (syntax-rules () ((_ pattern) 'template 'extra)))");
      }
      catch (CompilerException e)
      {
         Object[] args = e.getErrorArgs();

         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());
         assertEquals(1, args.length);
         assertEquals(Symbol.SYNTAX_RULES, args[0]);

         nError++;
      }

      try
      {
         invoke("(define-syntax transformer (syntax-rules () ((_ pattern))))");
      }
      catch (CompilerException e)
      {
         Object[] args = e.getErrorArgs();

         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());
         assertEquals(1, args.length);
         assertEquals(Symbol.SYNTAX_RULES, args[0]);

         nError++;
      }

      try
      {
         invoke("(define-syntax transformer (syntax-rules () ((_ pattern) . 'template)))");
      }
      catch (CompilerException e)
      {
         Object[] args = e.getErrorArgs();

         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());
         assertEquals(1, args.length);
         assertEquals(Symbol.SYNTAX_RULES, args[0]);

         nError++;
      }

      assertTrue(nError == 3);
   }

   /**
    * Tests invalid syntax-case clause.
    */
   public void testInvalidSyntaxCaseClause()
   {
      int nError = 0;

      try
      {
         invoke("(define-syntax transformer (syntax-case () ((_ pattern) 'fender 'template)))");
      }
      catch (CompilerException e)
      {
         Object[] args = e.getErrorArgs();

         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());
         assertEquals(1, args.length);
         assertEquals(Symbol.SYNTAX_CASE, args[0]);

         nError++;
      }

      try
      {
         invoke("(define-syntax transformer (lambda (x) (syntax-case x () ((_ pattern) 'fender 'template 'extra))))");
      }
      catch (CompilerException e)
      {
         Object[] args = e.getErrorArgs();

         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());
         assertEquals(1, args.length);
         assertEquals(Symbol.SYNTAX_CASE, args[0]);

         nError++;
      }

      try
      {
         invoke("(define-syntax transformer (lambda (x) (syntax-case x () ((_ pattern)))))");
      }
      catch (CompilerException e)
      {
         Object[] args = e.getErrorArgs();

         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());
         assertEquals(1, args.length);
         assertEquals(Symbol.SYNTAX_CASE, args[0]);

         nError++;
      }

      try
      {
         invoke("(define-syntax transformer (lambda (x) (syntax-case x () ((_ pattern) . 'template))))");
      }
      catch (CompilerException e)
      {
         Object[] args = e.getErrorArgs();

         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());
         assertEquals(1, args.length);
         assertEquals(Symbol.SYNTAX_CASE, args[0]);

         nError++;
      }

      assertTrue(nError == 4);
   }

   /**
    * Tests transformer definition with invalid values as literals.
    */
   public void testInvalidLiteral()
   {
      try
      {
         invoke("(define-syntax transformer (lambda (x) (syntax-case x (1) (_ 1))))");
      }
      catch (CompilerException e)
      {
         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());

         Throwable t = e.getCause();

         assertTrue(t instanceof ScriptingException);

         ScriptingException se = (ScriptingException)t;
         Object[] args = se.getErrorArgs();

         assertEquals("err.scripting.syntax.invalidLiteral", se.getErrorCode());
         assertEquals(1, args.length);
         assertEquals("1", args[0].toString());

         return;
      }

      assertTrue(false); // shouldn't get here
   }

   /**
    * Tests syntax definition with ellipses.
    */
   public void testEllipsisCount()
   {
      int nError = 0;

      try
      {
         invoke("(syntax-rules () ((_ a) (a ...)))");
      }
      catch (CompilerException e)
      {
         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());

         Throwable t = e.getCause();

         assertTrue(t instanceof ScriptingException);
         assertEquals("err.scripting.syntax.maxTemplateEllipsisCount.detailed", ((ScriptingException)t).getErrorCode());

         Object[] args = ((ScriptingException)t).getErrorArgs();

         assertEquals(3, args.length);
         assertEquals("a", args[0].toString());
         assertEquals(Primitive.ZERO_INTEGER, args[1]);
         assertEquals(Primitive.ONE_INTEGER, args[2]);

         nError++;
      }

      try
      {
         invoke("(lambda (x) (syntax-case x () ((_ a) #'((a) ...))))");
      }
      catch (CompilerException e)
      {
         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());

         Throwable t = e.getCause();

         assertTrue(t instanceof ScriptingException);
         assertEquals("err.scripting.syntax.maxTemplateEllipsisCount", ((ScriptingException)t).getErrorCode());

         nError++;
      }

      invoke("(syntax-rules () ((_ a b ...) ((a b) ...)))");
      invoke("(syntax-rules () ((_ a b ...) ((((((a)))) b) ...)))");

      invoke("(lambda (x) (syntax-case x () ((_ a b ...) #'((a b) ...))))");
      invoke("(lambda (x) (syntax-case x () ((_ a b ...) #'((((((a)))) b) ...))))");

      assertTrue(nError == 2);
   }

   /**
    * Tests transformer definition with invalid number of ellipses in templates.
    */
   public void testInvalidTemplateEllipsisCount()
   {
      int nError = 0;

      try
      {
         invoke("(define-syntax transformer (syntax-rules () ((_ a) (list a ... 123))))");
      }
      catch (CompilerException e)
      {
         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());

         Throwable t = e.getCause();

         assertTrue(t instanceof ScriptingException);
         assertEquals("err.scripting.syntax.maxTemplateEllipsisCount.detailed", ((ScriptingException)t).getErrorCode());

         nError++;
      }

      try
      {
         invoke("(define-syntax transformer (syntax-rules () ((_ a) (list '(a) ... 123))))");
      }
      catch (CompilerException e)
      {
         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());

         Throwable t = e.getCause();

         assertTrue(t instanceof ScriptingException);
         assertEquals("err.scripting.syntax.maxTemplateEllipsisCount", ((ScriptingException)t).getErrorCode());

         nError++;
      }

      try
      {
         invoke("(define-syntax transformer (lambda (x) (syntax-case x () ((_ a b ...) #'(list a ... 123)))))");
      }
      catch (CompilerException e)
      {
         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());

         Throwable t = e.getCause();

         assertTrue(t instanceof ScriptingException);
         assertEquals("err.scripting.syntax.maxTemplateEllipsisCount.detailed", ((ScriptingException)t).getErrorCode());

         nError++;
      }

      try
      {
         invoke("(define-syntax transformer (lambda (x) (syntax-case x () ((_ a b ...) #'(list #(a) ... 123)))))");
      }
      catch (CompilerException e)
      {
         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());

         Throwable t = e.getCause();

         assertTrue(t instanceof ScriptingException);
         assertEquals("err.scripting.syntax.maxTemplateEllipsisCount", ((ScriptingException)t).getErrorCode());

         nError++;
      }

      invoke("(define-syntax transformer (lambda (x) (syntax-case x () ((_ a b ...) #'(list '(a b) ... 123)))))");

      assertTrue(nError == 4);
   }

   public void testInvalidPatternEllipsisCount()
   {
      int nError = 0;

      try
      {
         invoke("(define-syntax transformer (syntax-rules () ((_ a ... b c ... d) c)))");
      }
      catch (CompilerException e)
      {
         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());

         Throwable t = e.getCause();

         assertTrue(t instanceof ScriptingException);
         assertEquals("err.scripting.syntax.maxPatternEllipsisCount", ((ScriptingException)t).getErrorCode());

         nError++;
      }

      try
      {
         invoke("(define-syntax transformer (syntax-rules () ((_ (a ... b c ... d)) c)))");
      }
      catch (CompilerException e)
      {
         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());

         Throwable t = e.getCause();

         assertTrue(t instanceof ScriptingException);
         assertEquals("err.scripting.syntax.maxPatternEllipsisCount", ((ScriptingException)t).getErrorCode());

         nError++;
      }

      try
      {
         invoke("(define-syntax transformer (syntax-rules () ((_ #(a ... b c ... d)) c)))");
      }
      catch (CompilerException e)
      {
         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());

         Throwable t = e.getCause();

         assertTrue(t instanceof ScriptingException);
         assertEquals("err.scripting.syntax.maxPatternEllipsisCount", ((ScriptingException)t).getErrorCode());

         nError++;
      }

      try
      {
         invoke("(define-syntax transformer (syntax-rules () ((_ (#(((#(a ... b c ... d)))))) c)))");
      }
      catch (CompilerException e)
      {
         assertEquals("err.compiler.syntax.invalidTransformerSyntax", e.getErrorCode());

         Throwable t = e.getCause();

         assertTrue(t instanceof ScriptingException);
         assertEquals("err.scripting.syntax.maxPatternEllipsisCount", ((ScriptingException)t).getErrorCode());

         nError++;
      }

      assertTrue(nError == 4);
   }

   /**
    * Tests non-looping variable value misalignment.
    */
   public void testMisalignment1()
   {
      int nError = 0;

      // A and B don't loop
      invoke("(define-syntax transformer (syntax-rules () ((_ (A ... (B ...))) '((A B) ...))))");

      try
      {
         invoke("(transformer (a1 a2 a3 (b1 b2)))");
      }
      catch (CompilerException e)
      {
         Object[] args = e.getErrorArgs();

         assertEquals("err.compiler.syntax.transformerExpansion", e.getErrorCode());
         assertEquals(2, args.length);
         assertEquals("transformer", args[0].toString());
         assertEquals("(transformer (a1 a2 a3 (b1 b2)))", args[1].toString());

         assert e.getCause() instanceof ScriptingException;

         ScriptingException se = (ScriptingException)e.getCause();
         
         args = se.getErrorArgs();

         assertEquals("err.scripting.syntax.incompatibleEllipsisExpansion", se.getErrorCode());
         assertEquals(2, args.length);
         assertEquals("A", args[0].toString());
         assertEquals("B", args[1].toString());

         nError++;
      }

      // a loops, B and C don't loop
      invoke("(define-syntax transformer (syntax-rules () ((_ (a (B ...) C ...) ...) '((a B C) ... ...))))");

      try
      {
         invoke("(transformer (a1 (b11 b12) c11) (a2 (b21 b22) c21))");
      }
      catch (CompilerException e)
      {
         assert e.getCause() instanceof ScriptingException;

         ScriptingException se = (ScriptingException)e.getCause();
         Object[] args = se.getErrorArgs();

         assertEquals("err.compiler.syntax.transformerExpansion", e.getErrorCode());
         assertEquals("err.scripting.syntax.incompatibleEllipsisExpansion", se.getErrorCode());
         assertEquals(2, args.length);
         assertEquals("B", args[0].toString());
         assertEquals("C", args[1].toString());

         nError++;
      }

      try
      {
         invoke("(transformer (a1 (b11) c11 c12) (a2 (b21 b22) c21 c22))");
      }
      catch (CompilerException e)
      {
         assert e.getCause() instanceof ScriptingException;

         ScriptingException se = (ScriptingException)e.getCause();
         Object[] args = se.getErrorArgs();

         assertEquals("err.compiler.syntax.transformerExpansion", e.getErrorCode());
         assertEquals("err.scripting.syntax.incompatibleEllipsisExpansion", se.getErrorCode());
         assertEquals(2, args.length);
         assertEquals("B", args[0].toString());
         assertEquals("C", args[1].toString());

         nError++;
      }

      assertTrue(nError == 3);
   }

   /**
    * Tests looping variable value misalignment.
    */
   public void testMisalignment2()
   {
      int nError = 0;

      // a and c loop, B doesn't loop
      invoke("(define-syntax transformer (syntax-rules () ((_ (a (B ...)) ... (c ...)) '((a B c) ... ...))))");

      try
      {
         invoke("(transformer (a1 (b11)) (a2 (b21)) (a3 (b31)) (c1))");
      }
      catch (CompilerException e)
      {
         assert e.getCause() instanceof ScriptingException;

         ScriptingException se = (ScriptingException)e.getCause();
         Object[] args = se.getErrorArgs();

         assertEquals("err.compiler.syntax.transformerExpansion", e.getErrorCode());
         assertEquals("err.scripting.syntax.incompatibleEllipsisExpansion", se.getErrorCode());
         assertEquals(2, args.length);
         assertEquals("a", args[0].toString());
         assertEquals("c", args[1].toString());

         nError++;
      }

      try
      {
         invoke("(transformer (a1 (b11 b12 b13)) (a2 (b21 b22 b23)) (a3 (b31 b32 b33)) (c1 c2))");
      }
      catch (CompilerException e)
      {
         assert e.getCause() instanceof ScriptingException;

         ScriptingException se = (ScriptingException)e.getCause();
         Object[] args = se.getErrorArgs();

         assertEquals("err.compiler.syntax.transformerExpansion", e.getErrorCode());
         assertEquals("err.scripting.syntax.incompatibleEllipsisExpansion", se.getErrorCode());
         assertEquals(2, args.length);
         assertEquals("a", args[0].toString());
         assertEquals("c", args[1].toString());

         nError++;
      }

      try
      {
         invoke("(transformer (a1 (b11 b12 b13)) (a2 (b21 b22 b23)) (a3 (b31 b32 b33)) (c1 c2 c3 c4))");
      }
      catch (CompilerException e)
      {
         assert e.getCause() instanceof ScriptingException;

         ScriptingException se = (ScriptingException)e.getCause();
         Object[] args = se.getErrorArgs();

         assertEquals("err.compiler.syntax.transformerExpansion", e.getErrorCode());
         assertEquals("err.scripting.syntax.incompatibleEllipsisExpansion", se.getErrorCode());
         assertEquals(2, args.length);
         assertEquals("a", args[0].toString());
         assertEquals("c", args[1].toString());

         nError++;
      }

      assertTrue(nError == 3);
   }

   /**
    * Tests non-looping and looping variable value misalignment.
    */
   public void testMisalignment3()
   {
      int nError = 0;

      // a and c loop, B doesn't loop
      invoke("(define-syntax transformer (syntax-rules () ((_ (a (B ...)) ... (c ...)) '((B c) ... ...))))");

      try
      {
         invoke("(transformer (a1 (b11 b12)) (a2 (b21 b22)) (c1 c2 c3))");
      }
      catch (CompilerException e)
      {
         assert e.getCause() instanceof ScriptingException;

         ScriptingException se = (ScriptingException)e.getCause();
         Object[] args = se.getErrorArgs();

         assertEquals("err.compiler.syntax.transformerExpansion", e.getErrorCode());
         assertEquals("err.scripting.syntax.incompatibleEllipsisExpansion", se.getErrorCode());
         assertEquals(2, args.length);
         assertEquals("B", args[0].toString());
         assertEquals("c", args[1].toString());

         nError++;
      }

      // a and c loop, B doesn't loop
      invoke("(define-syntax transformer (syntax-rules () ((_ (a (B ...)) ... (c ...)) '((c B) ... ...))))");

      try
      {
         invoke("(transformer (a1 (b11 b12)) (a2 (b21 b22)) (a3 (b31 b32)) (c1 c2 c3 c4))");
      }
      catch (CompilerException e)
      {
         assert e.getCause() instanceof ScriptingException;

         ScriptingException se = (ScriptingException)e.getCause();
         Object[] args = se.getErrorArgs();

         assertEquals("err.compiler.syntax.transformerExpansion", e.getErrorCode());
         assertEquals("err.scripting.syntax.incompatibleEllipsisExpansion", se.getErrorCode());
         assertEquals(2, args.length);
         assertEquals("B", args[0].toString());
         assertEquals("c", args[1].toString());

         nError++;
      }

      assertTrue(nError == 2);
   }

   /**
    * Tests value misalignment with nested structures that require alignment checks.
    */
   public void testMisalignmentNested()
   {
      int nError = 0;

      invoke("(define-syntax transformer (syntax-rules () ((_ (a ...) (b ...) (c ...) ...) '((a b (a c) ...) ...))))");

      try
      {
         invoke("(transformer (a1 a2) (b1 b2) (c11 c12))");
      }
      catch (CompilerException e)
      {
         assert e.getCause() instanceof ScriptingException;

         ScriptingException se = (ScriptingException)e.getCause();
         Object[] args = se.getErrorArgs();

         assertEquals("err.compiler.syntax.transformerExpansion", e.getErrorCode());
         assertEquals("err.scripting.syntax.incompatibleEllipsisExpansion", se.getErrorCode());
         assertEquals(2, args.length);
         assertEquals("b", args[0].toString());
         assertEquals("c", args[1].toString());

         nError++;
      }

      try
      {
         invoke("(transformer (a1 a2) (b1 b2) (c11 c12) (c21 c22) (c31 c32))");
      }
      catch (CompilerException e)
      {
         assert e.getCause() instanceof ScriptingException;

         ScriptingException se = (ScriptingException)e.getCause();
         Object[] args = se.getErrorArgs();

         assertEquals("err.compiler.syntax.transformerExpansion", e.getErrorCode());
         assertEquals("err.scripting.syntax.incompatibleEllipsisExpansion", se.getErrorCode());
         assertEquals(2, args.length);
         assertEquals("b", args[0].toString());
         assertEquals("c", args[1].toString());

         nError++;
      }

      assertTrue(nError == 2);
   }

   /**
    * Tests no matching pattern.
    */
   public void testNoMatchingPattern()
   {
      int nError = 0;

      invoke("(define-syntax transformer (syntax-rules () ((_ a b c ...) '(a b c ...))))");

      try
      {
         invoke("(transformer 1)");
      }
      catch (CompilerException e)
      {
         assert e.getCause() instanceof ScriptingException;

         ScriptingException se = (ScriptingException)e.getCause();

         assertEquals("err.compiler.syntax.transformerExpansion", e.getErrorCode());
         assertEquals("err.compiler.syntax.noMatch", se.getErrorCode());
         nError++;
      }

      try
      {
         invoke("(transformer)");
      }
      catch (CompilerException e)
      {
         assert e.getCause() instanceof ScriptingException;

         ScriptingException se = (ScriptingException)e.getCause();

         assertEquals("err.compiler.syntax.transformerExpansion", e.getErrorCode());
         assertEquals("err.compiler.syntax.noMatch", se.getErrorCode());
         nError++;
      }

      try
      {
         invoke("transformer");
      }
      catch (CompilerException e)
      {
         assert e.getCause() instanceof ScriptingException;

         ScriptingException se = (ScriptingException)e.getCause();

         assertEquals("err.compiler.syntax.transformerExpansion", e.getErrorCode());
         assertEquals("err.compiler.syntax.noMatch", se.getErrorCode());
         nError++;
      }

      try
      {
         invoke("(+ transformer 1)");
      }
      catch (CompilerException e)
      {
         assert e.getCause() instanceof ScriptingException;

         ScriptingException se = (ScriptingException)e.getCause();

         assertEquals("err.compiler.syntax.transformerExpansion", e.getErrorCode());
         assertEquals("err.compiler.syntax.noMatch", se.getErrorCode());
         nError++;
      }

      try
      {
         invoke("(set! transformer 1)");
      }
      catch (CompilerException e)
      {
         Object[] args = e.getErrorArgs();

         assertEquals("err.compiler.syntax.invariableTransformer", e.getErrorCode());
         assertEquals(1, args.length);
         assertEquals("transformer", args[0].toString());

         nError++;
      }

      assertTrue(nError == 5);
   }
}
