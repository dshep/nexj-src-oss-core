package nexj.test.junit;

import java.io.Reader;
import java.io.StringReader;

import nexj.core.meta.Metadata;
import nexj.core.meta.Repository;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Parser;
import nexj.core.scripting.SchemeParser;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.StackTrace;
import nexj.core.util.TextPositionReader;

import junit.framework.TestCase;

/**
 * Tests the dynamic object system.
 */
public abstract class EvalTestCase extends TestCase
{
   // associations
   protected Metadata m_metadata;
   protected InvocationContext m_context;
   protected Compiler m_compiler;
   protected Parser m_parser;

   // operations

   /**
    * @see junit.framework.TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      m_metadata = Repository.getMetadata();
      m_context = new InvocationContext(m_metadata);
      m_parser = new SchemeParser(m_context.getMachine().getGlobalEnvironment());
      m_compiler = new Compiler();

      try
      {
         m_context.initialize(null);
      }
      catch (Throwable t)
      {
         ThreadContextHolder.setContext(null);
         ObjUtil.rethrow(t);
      }
   }

   /**
    * Parses a Scheme S-expression.
    * @param sExpr The string to parse.
    * @return The parse tree.
    */
   protected Object parse(String sExpr, Lookup posMap)
   {
      Reader reader = new TextPositionReader(new StringReader(sExpr), "eval");
      Object expr;

      expr = m_parser.parse(reader, posMap);

      if (expr == Parser.EOF)
      {
         return null;
      }

      return expr;
   }

   /**
    * Evaluates the given expression.
    * @param sExpr The expression to evaluate.
    * @return The return value of the expression.
    * @throws Exception If an error occurs. The current stack trace is added to the exception so that the
    * JUnit test line with the error can be located.
    */
   protected Object eval(String sExpr) throws Exception
   {
      try
      {
         Lookup posMap = new HashTab();
         Object code = parse(sExpr, posMap);
         PCodeFunction fun = m_compiler.compile(code, posMap, m_context.getMachine(), true);

         return m_context.getMachine().invoke(fun, (Pair)null);
      }
      catch (Exception e)
      {
         m_context.getMachine().updateStackTrace(e);

         StackTrace trace = new StackTrace();
         StackTraceElement[] traceArray = e.getStackTrace();
         StackTraceElement[] currentArray = trace.getStackTrace();
         StackTraceElement[] amendedTraceArray = new StackTraceElement[traceArray.length + currentArray.length];

         System.arraycopy(traceArray, 0, amendedTraceArray, 0, traceArray.length);
         System.arraycopy(currentArray, 0, amendedTraceArray, traceArray.length, currentArray.length);

         e.setStackTrace(amendedTraceArray);

         throw e;
      }
   }
}
