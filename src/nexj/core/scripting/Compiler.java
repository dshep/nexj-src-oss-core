// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Primitive;
import nexj.core.scripting.PCodeFunction.DebugInfo;
import nexj.core.scripting.syntax.SyntaxTransformer;
import nexj.core.scripting.syntax.SyntaxExpander;
import nexj.core.scripting.syntax.SyntaxMatcher;
import nexj.core.scripting.syntax.SyntaxFunction;
import nexj.core.scripting.syntax.SyntaxTransformerContext;
import nexj.core.util.EmptyIterator;
import nexj.core.util.ErrorCode;
import nexj.core.util.HashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.TextPosition;

/**
 * Compiler for s-expressions to byte code.
 */
public class Compiler
{
   // attributes

   /**
    * The URL holder object. The URL for code can be set using one of these modes:
    * 1) A constant string (m_urlHolder instanceof String)
    * 2) A lookup map (m_urlHolder is String[TextPosition])
    * 3) From the text positions if available (m_urlHolder is null)
    * 
    * Modes 1 and 2 are deprecated. 
    */
   protected Object m_urlHolder;

   /**
    * Flag for additional local variables declared through define.
    */
   protected boolean m_bDefine;

   // associations

   /**
    * The innermost (top) frame.
    */
   protected Frame m_topFrame;

   /**
    * The text position element map.
    */
   protected Lookup m_posMap;

   /**
    * The VM for macro lookup and execution.
    */
   protected Machine m_machine;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(Compiler.class);

   /**
    * The symbol of the transformer function being created. May be null.
    */
   protected Symbol m_defineSyntaxSymbol;

   /**
    * The transformer pattern of the transformer clause currently being compiled. May be null.
    */
   protected SyntaxMatcher m_matcher;

   // operations

   /**
    * Compiles an s-expression to a byte code function. URL information is
    * fetched from {@link TextPosition}s.
    * 
    * @param expr The s-expression to compile.
    * @param posMap The text position element map. Can be null.
    * @param machine The VM for macro lookup and execution.
    * @param bEval True to generate code to push a top level lambda as a
    *           closure.
    * @return The compiled function.
    */
   public PCodeFunction compile(Object expr, Lookup posMap, Machine machine, boolean bEval)
   {
      return compile(expr, posMap, null, machine, bEval);
   }

   /**
    * Compiles an s-expression to a byte code function.
    * 
    * Future: This method should be deprecated.
    * 
    * @param expr The s-expression to compile.
    * @param posMap The text position element map. Can be null.
    * @param urlHolder The source code URL holder object - a string or a Lookup map String[TextPosition]. Can be null.
    * @param machine The VM for macro lookup and execution.
    * @param bEval True to generate code to push a top level lambda as a closure.
    * @return The compiled function.
    */
   public PCodeFunction compile(Object expr, Lookup posMap, Object urlHolder, Machine machine, boolean bEval)
   {
      m_posMap = posMap;
      m_urlHolder = urlHolder;
      m_machine = machine;

      return compile(expr, bEval);
   }

   /**
    * Compiles and returns a transformer function.
    * @param symbol The transformer symbol.
    * @param expr The transformer code.
    * @param machine The machine used to expand macros and transformers.
    * @return The new transformer function.
    */
   public SyntaxFunction compileSyntaxFunction(Symbol symbol, Object expr, Machine machine)
   {
      m_machine = machine;

      return createSyntaxFunction(symbol, false, expr, null);
   }

   /**
    * Compiles an s-expression to a byte code function.
    * 
    * @param expr The s-expression to compile.
    * @param bEval True to generate code to push a top level lambda as a closure.
    * @return The compiled function.
    */
   protected PCodeFunction compile(Object expr, boolean bEval)
   {
      Instruction instr = compile(expr, null, null);

      if (instr == null)
      {
         instr = new GetConstantInstruction(null);
      }

      instr.setFlagsAll(false, true);
      instr = instr.optimizeAll(this);

      PCodeFunction fun;

      if (bEval)
      {
         PCodeBuffer buf = new PCodeBuffer(this);

         instr.generateAll(buf);
         fun = new PCodeFunction();
         buf.copyTo(fun);
      }
      else
      {
        fun = instr.generate(this);
      }

      if (s_logger.isDumpEnabled())
      {
         s_logger.dump(fun.disasm());
      }

      return fun;
   }

   /**
    * Compiles a closure as if returned by the following expression, but unbound to the outer frame:
    * ((lambda (var1 ... varN) (lambda (arg1 ... argM) body)) arg1 ... argN)
    * @param vars The outer function arguments.
    * @param args The inner function arguments.
    * @param body The inner function body.
    * @param posMap The text position element map. Can be null.
    * @param urlHolder The source code URL holder object - a string or a Lookup map String[TextPosition]. Can be null.
    * @param machine The VM for macro lookup and execution.
    * @return The compiled inner function.
    */
   public PCodeFunction compile(Pair vars, Pair args, Pair body, Lookup posMap, Object urlHolder, Machine machine)
   {
      Pair lambda = new Pair(Symbol.LAMBDA, new Pair(args, body));

      // Ensure there is a URL for the compiled function
      if (posMap != null && body != null && posMap.contains(body))
      {
         posMap.put(lambda, posMap.get(body));
      }

      PCodeFunction parent = (PCodeFunction)compile(Pair.list(Symbol.LAMBDA, vars, lambda),
         posMap, urlHolder, machine, false);

      Machine.getDebugger().setAnonymousParent((PCodeFunction)parent.constants[0], parent);

      return (PCodeFunction)parent.constants[0];
   }

   /**
    * Compiles an s-expression to intermediate language instructions.
    * @param expr The expression to compile.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compile(Object expr, Instruction parent, TextPosition pos)
   {
      if (m_posMap != null && expr != null)
      {
         Object obj = m_posMap.get(expr);

         if (obj != null)
         {
            pos = (TextPosition)obj;
         }
      }

      if (expr instanceof Symbol)
      {
         return compileVariable((Symbol)expr, parent, pos);
      }

      if (expr instanceof Pair)
      {
         Pair pair = (Pair)expr;
         Object head = pair.getHead();

         if (head instanceof Symbol)
         {
            return compileSpecialForm((Symbol)head, pair, m_topFrame, parent, pos, false);
         }

         if (head instanceof Pair)
         {
            Pair fun = (Pair)head;

            if (Symbol.GLOBAL.equals(fun.getHead()) && fun.getTail() instanceof Pair)
            {
               fun = (Pair)fun.getTail();

               if (fun.getHead() instanceof Symbol && fun.getTail() == null)
               {
                  return compileSpecialForm((Symbol)fun.getHead(), pair, null, parent, pos, true);
               }
            }
         }
         else if (head instanceof Macro)
         {
            if (s_logger.isDumpEnabled())
            {
               s_logger.dump("Expanding macro \"" + head + "\"");
            }

            return compileMacroExpansion((Function)head, pair.getTail(), parent, pos);
         }

         return compileCall(pair, null, parent, pos);
      }

      return compileConstant(expr, parent, pos);
   }

   /**
    * Compiles a primitive or a macro.
    * @param symbol The special form symbol.
    * @param expr The whole expression.
    * @param frame The frame to search for local variables matching the symbol. Can be null.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @param bGlobal Whether the symbol is defined globally.
    * @return The first compiled intermediate language instruction,
    * or null if no special form has been recognized.
    */
   protected Instruction compileSpecialForm(Symbol symbol, Pair expr, Frame frame, Instruction parent, TextPosition pos, boolean bGlobal)
   {
      if (Symbol.QUOTE.equals(symbol))
      {
         return compileQuote(expr.getTail(), parent, pos);
      }

      if (Symbol.BEGIN.equals(symbol))
      {
         return compileBegin(expr.getTail(), parent, pos);
      }

      if (Symbol.SET.equals(symbol))
      {
         return compileSet(expr, parent, pos);
      }

      if (Symbol.IF.equals(symbol))
      {
         return compileIf(expr.getTail(), parent, pos);
      }

      if (Symbol.LAMBDA.equals(symbol))
      {
         return compileLambda(expr.getTail(), false, parent, pos);
      }

      if (Symbol.MACRO.equals(symbol))
      {
         return compileLambda(expr.getTail(), true, parent, pos);
      }

      if (Symbol.DEFINE.equals(symbol))
      {
         return compileDefine(expr.getTail(), parent, pos);
      }

      if (Symbol.GLOBAL.equals(symbol))
      {
         return compileGlobal(expr.getTail(), parent, pos);
      }

      if (Symbol.DECLARE.equals(symbol))
      {
         return compileDeclare(expr.getTail(), parent, pos);
      }

      if (Symbol.SYNTAX.equals(symbol))
      {
         return compileSyntax(expr.getTail(), parent, pos);
      }

      if (Symbol.SYNTAX_RULES.equals(symbol))
      {
         return compileSyntaxRules(expr.getTail(), parent, pos);
      }

      if (Symbol.SYNTAX_CASE.equals(symbol))
      {
         return compileSyntaxCase(expr.getTail(), parent, pos);
      }

      if (Symbol.DEFINE_SYNTAX.equals(symbol))
      {
         return compileDefineSyntax(expr.getTail(), false, parent, pos);
      }

      if (Symbol.SYS_DEFINE_LET_SYNTAX.equals(symbol))
      {
         return compileDefineSyntax(expr.getTail(), true, parent, pos);
      }

      if (!bGlobal)
      {
         Local var = getLocalDefinition(frame, symbol);

         if (var != null)
         {
            if (var.isTransformer())
            {
               return compileTransformerExpansion(var.getTransformer(), expr, false, parent, pos);
            }

            return compileCall(expr, compileVariable(var, null, pos), parent, pos);
         }
      }

      Object value = m_machine.getGlobalEnvironment().findVariable(symbol);

      if (value instanceof Macro)
      {
         if (s_logger.isDumpEnabled())
         {
            s_logger.dump("Expanding macro \"" + symbol + "\"");
         }

         return compileMacroExpansion((Function)value, expr.getTail(), parent, pos);
      }

      if (value instanceof SyntaxFunction)
      {
         return compileTransformerExpansion((SyntaxFunction)value, expr, false, parent, pos);
      }

      return compileCall(expr, null, parent, pos);
   }

   /**
    * Compiles a reference to a local variable.
    * @param var The local variable.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileVariable(Local var, Instruction parent, TextPosition pos)
   {
      if (var.isTransformer())
      {
         return compileTransformerExpansion(var.getTransformer(), var.getSymbol(), false, parent, pos);
      }

      if (m_defineSyntaxSymbol != var.getFrame().getTransformerSymbol())
      {
         Symbol symbol = var.getSymbol();

         if (m_matcher != null && m_matcher.isVariable(symbol)) // template symbol not wrapped in syntax block
         {
            throw setURL(new CompilerException("err.compiler.syntax.nonsyntaxPatternVariable", new Object[]{symbol}, null, pos));
         }

         // cannot reference variables outside of the transformer definition
         throw setURL(new CompilerException("err.compiler.syntax.identifierRef", new Object[]{symbol}, null, pos));
      }

      Instruction instr = new GetLocalInstruction(var, m_topFrame);

      instr.setParent(parent);
      instr.setTextPos(pos);

      return instr;
   }

   /**
    * Compiles a reference to a variable.
    * @param symbol The variable symbol.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileVariable(Symbol symbol, Instruction parent, TextPosition pos)
   {
      Local var = getLocalDefinition(m_topFrame, symbol);

      if (var != null)
      {
         return compileVariable(var, parent, pos);
      }

      if (m_matcher != null && m_matcher.isVariable(symbol))
      {
         throw setURL(new CompilerException("err.compiler.syntax.nonsyntaxPatternVariable", new Object[]{symbol}, null, pos));
      }

      symbol = m_machine.getGlobalVarSymbol(symbol);

      Object value = m_machine.getGlobalEnvironment().findVariable(symbol);

      if (value instanceof SyntaxFunction)
      {
         return compileTransformerExpansion((SyntaxFunction)value, symbol, false, parent, pos);
      }

      if (m_defineSyntaxSymbol != null && !(value instanceof Function))
      {
         // cannot reference a non-function global variable outside of the transformer definition 
         throw setURL(new CompilerException("err.compiler.syntax.identifierRef", new Object[]{symbol}, null, pos));
      }

      Instruction instr = new GetGlobalInstruction(symbol);

      instr.setParent(parent);
      instr.setTextPos(pos);

      return instr;
   }

   /**
    * Compiles a quoted expression.
    * @param expr The pair containing the quoted expression.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileQuote(Object expr, Instruction parent, TextPosition pos)
   {
      verifyArgCount(expr, Symbol.QUOTE, 1, 1, pos);

      return compileConstant(((Pair)expr).getHead(), parent, pos);
   }

   /**
    * Compiles a syntax expression. Behaves like quote if not compiling transformer clause.
    * @param expr The pair containing the syntax expression.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileSyntax(Object expr, Instruction parent, TextPosition pos)
   {
      verifyArgCount(expr, Symbol.SYNTAX, 1, 1, pos);

      expr = ((Pair)expr).getHead();

      if (m_matcher == null)
      {
         return compileConstant(expr, parent, pos);
      }

      CallInstruction call = new CallInstruction(
         m_machine.getGlobalEnvironment().isOptionSet(GlobalEnvironment.OPTION_REDEFINE_INTRINSICS));
      Instruction func = compile(SyntaxExpander.createExpander(expr, m_matcher), call, pos);

      call.setFunction(func);
      call.setParent(parent);
      call.setTextPos(pos);

      return call; // (<expander>)
   }

   /**
    * Compiles a begin sequence.
    * @param expr The pair containing the first expression in the sequence.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileBegin(Object expr, Instruction parent, TextPosition pos)
   {
      verifyArgCount(expr, Symbol.BEGIN, 0, -1, pos);

      Instruction instr = compileSequence(expr, parent, pos);
      
      if (instr == null && parent != null && !(parent instanceof GetFunctionInstruction))
      {
         throw setURL(new CompilerException("err.compiler.emptySequence", null, null, pos));
      }

      return instr;
   }

   /**
    * Compiles an expression sequence.
    * @param expr The pair containing the first expression in the sequence.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileSequence(Object expr, Instruction parent, TextPosition pos)
   {
      Instruction first = null;
      Instruction last = null;

      while (expr != null)
      {
         Pair pair = (Pair)expr;
         Object head = pair.getHead();
         Instruction instr = compile(head, parent, pos);

         if (instr != null)
         {
            if (last != null)
            {
               instr.setPrevious(last);
               last.setNext(instr);
            }
            else
            {
               first = instr;
            }
   
            // Find the end of the subsequence, if any
            for (last = instr; last.getNext() != null; last = last.getNext());
         }

         expr = pair.getTail();
      }

      return first;
   }

   /**
    * Compiles a set! assignment.
    * @param expr The pair containing the set! symbol.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileSet(Pair expr, Instruction parent, TextPosition pos)
   {
      Pair pair = expr.getNext();

      verifyArgCount(pair, Symbol.SET, 2, 2, pos);

      Object head = pair.getHead();

      if (!(head instanceof Symbol))
      {
         throw setURL(new CompilerException("err.compiler.setSymbol", null, null, pos));
      }

      Symbol symbol = (Symbol)head;
      Instruction instr = null;
      Local var = getLocalDefinition(m_topFrame, symbol);

      if (var != null)
      {
         if (var.isTransformer()) // local variable transformer
         {
            return compileTransformerExpansion(var.getTransformer(), expr, true, parent, pos);
         }

         instr = new SetLocalInstruction(var, m_topFrame);
      }
      else
      {
         symbol = m_machine.getGlobalVarSymbol(symbol);

         Object value = m_machine.getGlobalEnvironment().findVariable(symbol);

         if (value instanceof SyntaxFunction) // global variable transformer
         {
            return compileTransformerExpansion((SyntaxFunction)value, expr, true, parent, pos);
         }

         validateGlobalAssignment(symbol, pos);
         instr = new SetGlobalInstruction(symbol);
      }

      instr.setParent(parent);
      instr.setTextPos(pos);

      Instruction value = compile(pair.getNext().getHead(), instr, pos);

      if (value instanceof GetFunctionInstruction)
      {
         ((GetFunctionInstruction)value).setName(symbol.getName());
      }

      ((Unary)instr).setValue(value);

      return instr;
   }

   /**
    * Compiles a conditional expression.
    * @param expr The pair containing the first expression after the if symbol.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileIf(Object expr, Instruction parent, TextPosition pos)
   {
      verifyArgCount(expr, Symbol.IF, 2, 3, pos);

      BranchInstruction instr = new BranchInstruction();

      instr.setParent(parent);
      instr.setTextPos(pos);

      Pair pair = (Pair)expr;

      instr.setCondition(compile(pair.getHead(), instr, pos));

      pair = pair.getNext();
      instr.setTrueValue(compile(pair.getHead(), instr, pos));

      if (pair.getTail() != null)
      {
         pair = pair.getNext();
         instr.setFalseValue(compile(pair.getHead(), instr, pos));
      }
      
      return instr;
   }

   /**
    * Compiles a function or macro declaration.
    * @param expr The pair containing the first expression after the lambda symbol.
    * @param bMacro True to compile a macro declaration.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileLambda(Object expr, boolean bMacro, Instruction parent, TextPosition pos)
   {
      verifyArgCount(expr, Symbol.LAMBDA, 2, -1, pos);

      GetFunctionInstruction func = (bMacro) ? new GetMacroInstruction() : new GetFunctionInstruction();
      Frame frame = pushFrame();

      func.setParent(parent);
      func.setTextPos(pos);
      func.setFrame(frame);

      Pair pair = (Pair)expr;
      Object head = pair.getHead();

      // Compile the argument declaration
      if (head instanceof Symbol)
      {
         // (lambda x ...)

         frame.addVar(new Local((Symbol)head));
         func.incArgCount();
         func.setVarArg(true);
      }
      else if (head instanceof Pair)
      {
         // (lambda (...) ...)

         Pair arg = (Pair)head;

         for (;;)
         {
            head = arg.getHead();

            if (!(head instanceof Symbol))
            {
               throw setURL(new CompilerException("err.compiler.lambdaArg", null, null, pos));
            }

            Symbol symbol = verifyArgSymbol((Symbol)head, frame, pos);

            frame.addVar(new Local(symbol));
            func.incArgCount();

            head = arg.getTail();

            if (head == null)
            {
               break;
            }

            if (head instanceof Symbol)
            {
               // (lambda (x y . z) ...)

               symbol = verifyArgSymbol((Symbol)head, frame, pos);

               frame.addVar(new Local(symbol));
               func.incArgCount();
               func.setVarArg(true);

               break;
            }

            if (!(head instanceof Pair))
            {
               throw setURL(new CompilerException("err.compiler.lambdaArg", null, null, pos));
            }

            arg = (Pair)head;
         }
      }
      else if (head != null)
      {
         throw setURL(new CompilerException("err.compiler.lambdaArg", null, null, pos));
      }

      compileLambdaBody(func, pair.getTail());

      return func;
   }

   /**
    * Declares a new local variable through define.
    * @param symbol The variable symbol.
    * @param pair The pair containing the define.
    * @param pos The text position of the function.
    */
   protected void declareLocal(Symbol symbol, Pair pair, TextPosition pos)
   {
      if (!m_bDefine)
      {
         pushFrame().addVar(new Local(symbol));
         m_bDefine = true;
      }
      else
      {
         if (m_topFrame.findVar(symbol) != null)
         {
            if (m_posMap != null)
            {
               Object obj = m_posMap.get(pair);

               if (obj != null)
               {
                  pos = (TextPosition)obj;
               }
            }

            throw setURL(new CompilerException("err.compiler.dupVar", new Object[]{symbol}, null, pos));
         }

         m_topFrame.addVar(new Local(symbol));
      }
   }

   /**
    * Declares variables found in local define statements.
    * @param expr The list containing the expressions to process.
    * @param pos The text position of the parent expression.
    */
   protected boolean declareLocals(Object expr, TextPosition pos)
   {
      while (expr instanceof Pair)
      {
         Pair pair = (Pair)expr;

         expr = pair.getHead();

         if (expr instanceof Pair)
         {
            Pair head = (Pair)expr;

            expr = head.getHead();

            // Take into account ((global <sym>) ...)
            if (expr instanceof Pair)
            {
               Pair fun = (Pair)expr;

               if (Symbol.GLOBAL.equals(fun.getHead()) && fun.getTail() instanceof Pair)
               {
                  fun = (Pair)fun.getTail();
                  expr = fun.getHead();

                  if (fun.getTail() != null)
                  {
                     expr = null;
                  }
               }
            }

            if (Symbol.DEFINE.equals(expr) || Symbol.DEFINE_SYNTAX.equals(expr) || Symbol.SYS_DEFINE_LET_SYNTAX.equals(expr))
            {
               if (head.getTail() instanceof Pair)
               {
                  Pair tail = head.getNext();

                  if (tail.getHead() instanceof Symbol)
                  {
                     declareLocal((Symbol)tail.getHead(), head, pos);
                  }
                  else if (tail.getHead() instanceof Pair)
                  {
                     head = (Pair)tail.getHead();

                     if (head.getHead() instanceof Symbol)
                     {
                        declareLocal((Symbol)head.getHead(), head, pos);
                     }
                  }
               }
            }
            else if (Symbol.BEGIN.equals(expr))
            {
               if (!declareLocals(head.getTail(), pos))
               {
                  return false;
               }
            }
            else
            {
               return false;
            }
         }

         expr = pair.getTail();
      }

      return true;
   }

   /**
    * Compiles a function body.
    * @param lambda The function instruction for which to compile the body.
    * @param expr The first pair of the function body.
    */
   protected void compileLambdaBody(GetFunctionInstruction lambda, Object expr)
   {
      boolean bDefineSaved = m_bDefine;
      int nOptionsSaved = m_machine.getGlobalEnvironment().getOptions();

      m_bDefine = false;

      try
      {
         declareLocals(expr, lambda.getTextPos());

         Instruction body = compileSequence(expr, lambda, lambda.getTextPos());

         setGetFunctionInstructionBody(lambda, body);
      }
      finally
      {
         m_machine.getGlobalEnvironment().setOptions(nOptionsSaved);

         if (m_bDefine)
         {
            popFrame();
         }

         m_bDefine = bDefineSaved;
         popFrame();
      }
   }

   /**
    * Sets the body of the GetFunctionInstruction instruction.
    * @param lambda The GetFunctionInstruction instruction to set.
    * @param body The instruction body.
    */
   protected void setGetFunctionInstructionBody(GetFunctionInstruction lambda, Instruction body)
   {
      if (body == null)
      {
         throw setURL(new CompilerException("err.compiler.emptySequence", null, null, lambda.getTextPos()));
      }

      if (m_bDefine)
      {
         GetFunctionInstruction func = new GetFunctionInstruction(m_topFrame, body);
         CallInstruction call = new CallInstruction(func,
            m_machine.getGlobalEnvironment().isOptionSet(GlobalEnvironment.OPTION_REDEFINE_INTRINSICS));

         call.setTextPos(lambda.getTextPos());
         call.setParent(lambda);
         func.setTextPos(lambda.getTextPos());
         func.setParent(call);

         for (Instruction instr = body; instr != null; instr = instr.getNext())
         {
            instr.setParent(func);
         }

         for (Local var = m_topFrame.getLastVar(); var != null; var = var.getNext())
         {
            call.addArg(new GetConstantInstruction(null));
            func.incArgCount();
         }

         lambda.setBody(call);
      }
      else
      {
         lambda.setBody(body);
      }
   }

   /**
    * Compiles a syntax-rules transformer declaration.
    * @param expr The pair containing the first expression after the syntax-rules symbol.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileSyntaxRules(Object expr, Instruction parent, TextPosition pos)
   {
      try
      {
         // (lambda (<symbol>) <transformerBody>)
         return compileTransformerBody(new Symbol(""), (Pair)expr, true, parent, pos);
      }
      catch (Exception e)
      {
         throw setURL(new CompilerException("err.compiler.syntax.invalidTransformerSyntax", new Object[]{Symbol.SYNTAX_RULES}, e, pos));
      }
   }

   /**
    * Compiles a syntax-case transformer declaration.
    * @param expr The pair containing the first expression after the syntax-case symbol.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileSyntaxCase(Object expr, Instruction parent, TextPosition pos)
   {
      try
      {
         Symbol symbol = new Symbol("");
         Pair pair = (Pair)expr;
         CallInstruction call = new CallInstruction(
            m_machine.getGlobalEnvironment().isOptionSet(GlobalEnvironment.OPTION_REDEFINE_INTRINSICS));
         Instruction instr = compile(pair.getHead(), call, pos);
         GetFunctionInstruction lambda = compileTransformerBody(symbol, pair.getNext(), false, parent, pos);

         call.setFunction(lambda);
         call.addArg(instr);
         call.setParent(parent);
         call.setTextPos(pos);

         return call; // ((lambda (<symbol>) <transformerBody>) <arg>)
      }
      catch (Exception e)
      {
         throw setURL(new CompilerException("err.compiler.syntax.invalidTransformerSyntax", new Object[]{Symbol.SYNTAX_CASE}, e, pos));
      }
   }

   /**
    * Compiles the body of a transformer declaration
    * @param symbol The symbol representing the input argument to the transformer.
    * @param expr The pair containing the first expression after the syntax-rules or syntax-case symbol.
    * @param bSyntaxRules Flag indicating a syntax-rules or syntax-case transformer.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    * @throws ClassCastException
    */
   protected GetFunctionInstruction compileTransformerBody(Symbol symbol, Pair expr, boolean bSyntaxRules,
      Instruction parent, TextPosition pos) throws ClassCastException
   {
      boolean bDefineSaved = m_bDefine;
      int nOptionsSaved = m_machine.getGlobalEnvironment().getOptions();

      m_bDefine = false;

      try
      {
         GetFunctionInstruction lambda = new GetFunctionInstruction();
         Frame frame = pushFrame();

         lambda.setParent(parent);
         lambda.setTextPos(pos);
         lambda.setFrame(frame);
         frame.addVar(new Local(symbol));
         lambda.incArgCount();

         Lookup literalMap = SyntaxMatcher.createLiteralMap(expr.getHead());
         Instruction transformerBody =
            compileTransformerClauses(symbol, literalMap, expr.getTail(), bSyntaxRules, parent, pos, m_matcher);

         setGetFunctionInstructionBody(lambda, transformerBody);

         return lambda; // (lambda (<symbol>) <transformerBody>)
      }
      finally
      {
         m_machine.getGlobalEnvironment().setOptions(nOptionsSaved);

         if (m_bDefine)
         {
            popFrame();
         }

         m_bDefine = bDefineSaved;
         popFrame();
      }
   }

   /**
    * Compiles the transformer clauses.
    * @param arg The symbol representing the input argument.
    * @param literalMap Mappings from each literal to its corresponding SyntaxPattern object.
    * @param clauses Scheme list of transformer clauses.
    * @param bSyntaxRules Flag indicating a syntax-rules or syntax-case transformer.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @param parentMatcher The parent matcher of the transformer clauses to be compiled. May be null.
    * @return The first compiled intermediate language instruction.
    * @throws ClassCastException
    */
   protected Instruction compileTransformerClauses(Symbol arg, Lookup literalMap, Object clauses,
      boolean bSyntaxRules, Instruction parent, TextPosition pos, SyntaxMatcher parentMatcher) throws ClassCastException
   {
      if (clauses == null)
      {
         return compile(Pair.list(Symbol.ERROR, "err.compiler.syntax.noMatch"), parent, pos);
      }

      Pair clausePair = (Pair)clauses;
      Pair clause = (Pair)clausePair.getHead();
      Object cur = clause.getHead();

      m_matcher = new SyntaxMatcher(cur, literalMap, parentMatcher);

      Pair condition = Pair.list(m_matcher, arg); // (<matcher> <arg>)

      clause = clause.getNext();
      cur = clause.getHead();
      clause = clause.getNext();

      Object template;

      if (bSyntaxRules)
      {
         if (clause != null)
         {
            throw setURL(new CompilerException("err.compiler.syntax.invalidTransformerSyntax", new Object[]{Symbol.SYNTAX_RULES}, null, pos));
         }

         // Note: for syntax-rules, the (syntax ...) wrap around template is implicit
         template = new Pair(SyntaxExpander.createExpander(cur, m_matcher));
      }
      else
      {
         if (clause != null)
         {
            // (and (<matcher> <arg>) <fender>)
            condition = Pair.list(Symbol.AND, condition, cur);
            cur = clause.getHead();

            if (clause.getTail() != null)
            {
               throw setURL(new CompilerException("err.compiler.syntax.invalidTransformerSyntax", new Object[]{Symbol.SYNTAX_CASE}, null, pos));
            }
         }

         template = cur;
      }

      BranchInstruction instr = new BranchInstruction();

      instr.setParent(parent);
      instr.setTextPos(pos);
      instr.setCondition(compile(condition, instr, pos));
      instr.setTrueValue(compile(template, instr, pos));

      m_matcher = parentMatcher;
      instr.setFalseValue(compileTransformerClauses(arg, literalMap, clausePair.getTail(), bSyntaxRules, instr, pos, parentMatcher));

      return instr;
   }

   /**
    * Compiles a global definition.
    * @param expr The pair containing the first expression after the define symbol.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileDefine(Object expr, Instruction parent, TextPosition pos)
   {
      Instruction instr = createDefineInstruction(parent, pos);
      Instruction value = compileDefinition(expr, instr, pos);

      setDefineInstructionValue(instr, value);

      return instr;
   }

   /**
    * Compiles a transformer function definition.
    * @param expr The pair containing the first expression after the define-syntax or
    * sys:define-let-syntax symbol.
    * @param bLet Flag indicating a let-syntax transformer.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileDefineSyntax(Object expr, boolean bLet, Instruction parent, TextPosition pos)
   {
      verifyArgCount(expr, Symbol.DEFINE_SYNTAX, 2, 2, pos);

      Pair pair = (Pair)expr;
      Object head = pair.getHead();

      if (!(head instanceof Symbol))
      {
         throw setURL(new CompilerException("err.compiler.syntax.defineSyntaxVar", new Object[]{head}, null, pos));
      }

      Symbol symbol = (Symbol)head;
      SyntaxFunction transformer = createSyntaxFunction(symbol, bLet, pair.getNext().getHead(), pos);
      Instruction instr = createDefineInstruction(parent, pos);

      defineVariable(instr, symbol, pos, transformer);
      setDefineInstructionValue(instr,
         new GetConstantInstruction((m_topFrame == null) ? transformer : null));

      return instr;
   }

   /**
    * Creates a transformer function.
    * @param symbol The transformer symbol.
    * @param bLet Flag indicating a let-syntax transformer.
    * @param expr The transformer code.
    * @param pos The text position. Can be null.
    * @return The new transformer function.
    */
   protected SyntaxFunction createSyntaxFunction(Symbol symbol, boolean bLet, Object expr, TextPosition pos)
   {
      Symbol definedTransformerSymbolSaved = m_defineSyntaxSymbol;

      m_defineSyntaxSymbol = symbol;

      try
      {
         PCodeFunction func = compile(expr, true);
         Object value = m_machine.invoke(func, (Pair)null);
         SyntaxFunction transformer;

         if (value instanceof SyntaxFunction)
         {
            transformer = (SyntaxFunction)value;
            transformer.setSymbol(symbol);
         }
         else if (value instanceof Function)
         {
            transformer = new SyntaxTransformer(symbol, (Function)value, false);
         }
         else
         {
            throw setURL(new CompilerException("err.compiler.syntax.defineSyntax", new Object[]{value}, null, pos));
         }

         if (m_topFrame != null)
         {
            transformer = new LocalSyntaxTransformer(transformer, m_topFrame, bLet);
         }

         return transformer;
      }
      finally
      {
         m_defineSyntaxSymbol = definedTransformerSymbolSaved;
      }
   }

   /**
    * Creates a define instruction with the given parent instruction and text position.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction createDefineInstruction(Instruction parent, TextPosition pos)
   {
      Instruction instr;

      if (m_topFrame == null)
      {
         if (parent != null)
         {
            throw setURL(new CompilerException("err.compiler.define", null, null, pos));
         }

         instr = new DefGlobalInstruction(null);
      }
      else
      {
         if (!(parent instanceof GetFunctionInstruction))
         {
            throw setURL(new CompilerException("err.compiler.define", null, null, pos));
         }

         instr = new SetLocalInstruction(null, m_topFrame);
      }

      instr.setParent(parent);
      instr.setTextPos(pos);

      return instr;
   }

   /**
    * Compiles a variable definition.
    * @param expr The pair containing the first expression after the define symbol.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileDefinition(Object expr, Instruction parent, TextPosition pos)
   {
      int nArgCount = verifyArgCount(expr, Symbol.DEFINE, 2, -1, pos);
      Pair pair = (Pair)expr;
      Object obj = pair.getHead();

      if (obj instanceof Symbol)
      {
         // (define x ...)

         if (nArgCount > 2)
         {
            throw setURL(new CompilerException("err.compiler.maxArgCount",
               new Object[]{Symbol.DEFINE, Primitive.createInteger(2)}, null, pos));
         }

         defineVariable(parent, (Symbol)obj, pos, null);

         return compile(pair.getNext().getHead(), parent, pos);
      }
      else if (obj instanceof Pair)
      {
         // (define (f ...) ...) 

         Pair arg = (Pair)obj;

         obj = arg.getHead();

         if (!(obj instanceof Symbol))
         {
            throw setURL(new CompilerException("err.compiler.lambdaArg", null, null, pos)); 
         }

         defineVariable(parent, (Symbol)obj, pos, null);

         GetFunctionInstruction func = new GetFunctionInstruction();
         Frame frame = pushFrame();

         func.setParent(parent);
         func.setTextPos(pos);
         func.setFrame(frame);

         for (obj = arg.getTail(); obj != null; obj = arg.getTail())
         {
            if (obj instanceof Symbol)
            {
               // (define (f x y . z) ...)

               Symbol symbol = verifyArgSymbol((Symbol)obj, frame, pos);

               frame.addVar(new Local(symbol));
               func.incArgCount();
               func.setVarArg(true);

               break;
            }
            else if (obj instanceof Pair)
            {
               // (define (f x y z) ...)
               
               arg = (Pair)obj;
               obj = arg.getHead();

               if (!(obj instanceof Symbol))
               {
                  throw setURL(new CompilerException("err.compiler.lambdaArg", null, null, pos)); 
               }

               Symbol symbol = verifyArgSymbol((Symbol)obj, frame, pos);

               frame.addVar(new Local(symbol));
               func.incArgCount();
            }
            else
            {
               throw setURL(new CompilerException("err.compiler.lambdaArg", null, null, pos)); 
            }
         }

         compileLambdaBody(func, pair.getTail());

         return func;
      }
      else
      {
         throw setURL(new CompilerException("err.compiler.defineVar", null, null, pos));
      }
   }

   /**
    * Sets the value of the define instruction.
    * @param instr The define instruction.
    * @param value The value of the instruction.
    */
   protected void setDefineInstructionValue(Instruction instr, Instruction value)
   {
      ((Unary)instr).setValue(value);

      if (value instanceof GetFunctionInstruction)
      {
         String sName;

         if (instr instanceof DefGlobalInstruction)
         {
            sName = ((DefGlobalInstruction)instr).getSymbol().getName();
         }
         else
         {
            sName = ((SetLocalInstruction)instr).getVar().getSymbol().getName();
         }

         ((GetFunctionInstruction)value).setName(sName);
      }
   }

   /**
    * Validates a global assignment.
    * @param symbol The variable symbol.
    * @param pos The text position.
    */
   protected void validateGlobalAssignment(Symbol symbol, TextPosition pos)
   {
      if (Intrinsic.findFunction(symbol) != null &&
         !m_machine.getGlobalEnvironment().isOptionSet(GlobalEnvironment.OPTION_REDEFINE_INTRINSICS))
      {
         throw setURL(new CompilerException("err.compiler.intrinsicRedefinition", new Object[]{symbol}, null, pos));
      }
   }

   /**
    * Defines the variable symbol.
    * @param instr LocalVarInstruction or GlobalVarInstruction.
    * @param symbol The variable symbol.
    * @param pos The text position.
    * @param transformer A transformer function if the variable is bound to it; null otherwise.
    */
   protected void defineVariable(Instruction instr, Symbol symbol, TextPosition pos, SyntaxFunction transformer)
   {
      if (instr instanceof LocalVarInstruction)
      {
         Local var;

         if (!m_bDefine || (var = m_topFrame.findVar(symbol)) == null)
         {
            throw setURL(new CompilerException("err.compiler.define", null, null, pos));
         }

         ((LocalVarInstruction)instr).setVar(var);
         var.setTransformer(transformer);
      }
      else
      {
         // if compiling a transformer expansion, rename global variables introduced by transformer
         // expansions so that they do not change the values of existing like-named global variables
         symbol = m_machine.generateGlobalVarSymbol(symbol);

         validateGlobalAssignment(symbol, pos);
         ((GlobalVarInstruction)instr).setSymbol(symbol);
      }
   }

   /**
    * Compiles a global namespace escape.
    * @param expr The pair containing the first expression after the global symbol.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileGlobal(Object expr, Instruction parent, TextPosition pos)
   {
      verifyArgCount(expr, Symbol.GLOBAL, 1, 1, pos);

      Pair pair = (Pair)expr;

      if (!(pair.getHead() instanceof Symbol))
      {
         throw setURL(new CompilerException("err.compiler.globalSymbol", null, null, pos));
      }

      Instruction instr = new GetGlobalInstruction((Symbol)pair.getHead());

      instr.setParent(parent);
      instr.setTextPos(pos);

      return instr;
   }
   
   /**
    * Compiles a declare directive.
    * @param expr The pair containing the first expression after the declare symbol.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileDeclare(Object expr, Instruction parent, TextPosition pos)
   {
      verifyArgCount(expr, Symbol.DECLARE, 1, -1, pos);

      Pair pair = (Pair)expr;
      Object symbol = pair.getHead();

      if (Symbol.OPTION.equals(symbol))
      {
         for (pair = pair.getNext(); pair != null; pair = pair.getNext())
         {
            if (!(pair.getHead() instanceof Pair))
            {
               throw setURL(new CompilerException("err.compiler.declareSpec", null, null, pos));
            }
            
            Pair head = (Pair)pair.getHead();
            
            if (!(head.getTail() instanceof Pair))
            {
               throw setURL(new CompilerException("err.compiler.declareSpec", null, null, pos));
            }
            
            Pair tail = head.getNext();

            symbol = head.getHead();
            
            if (tail.getTail() != null || !(symbol instanceof Symbol))
            {
               throw setURL(new CompilerException("err.compiler.declareSpec", null, null, pos));
            }

            int nOption;
            
            if (Symbol.CONVERT_SYMBOLS.equals(symbol))
            {
               nOption = GlobalEnvironment.OPTION_CONVERT_SYMBOLS;
            }
            else if (Symbol.REDEFINE_INTRINSICS.equals(symbol))
            {
               nOption = GlobalEnvironment.OPTION_REDEFINE_INTRINSICS;
            }
            else
            {
               throw setURL(new CompilerException("err.compiler.declareOption", new Object[]{symbol}, null, pos));
            }
   
            if (!(tail.getHead() instanceof Boolean))
            {
               throw setURL(new CompilerException("err.compiler.declareValue", null, null, pos));
            }
   
            m_machine.getGlobalEnvironment().setOption(nOption, ((Boolean)tail.getHead()).booleanValue());
         }
      }
      else if (Symbol.SCOPE.equals(symbol))
      {
         if (!(pair.getTail() instanceof Pair) || pair.getNext().getTail() != null ||
            !((symbol = pair.getNext().getHead()) instanceof Symbol))
         {
            throw setURL(new CompilerException("err.compiler.declareScope", null, null, pos));
         }

         m_machine.getGlobalEnvironment().setScope((Symbol)symbol);
      }
      else
      {
         throw setURL(new CompilerException("err.compiler.declareSymbol", null, null, pos));
      }

      Instruction instr = new GetConstantInstruction(null);

      instr.setParent(parent);
      instr.setTextPos(pos);

      return instr;
   }

   /**
    * Expands a macro and compiles the resulting code.
    * @param func The macro function.
    * @param expr The pair containing the first expression after the macro variable.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileMacroExpansion(Function func, Object expr, Instruction parent, TextPosition pos)
   {
      if (expr != null && !(expr instanceof Pair))
      {
         throw setURL(new CompilerException("err.compiler.callArgs", null, null, pos));
      }
      
      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("args = " + Intrinsic.toString(expr));
      }

      Object expansion;

      try
      {
         expansion = m_machine.invoke(func, (Pair)expr);

         if (s_logger.isDumpEnabled())
         {
            s_logger.dump("expansion = " + Intrinsic.toString(expansion));
         }

         return compile(expansion, parent, pos);
      }
      catch (Throwable e)
      {
         if (e instanceof ErrorCode)
         {
            ErrorCode ec = (ErrorCode)e;

            throw setURL(new CompilerException(ec.getErrorCode(), ec.getErrorArgs(), e, pos));
         }

         throw setURL(new CompilerException("err.compiler.macroExpansion",
            new Object[]{ObjUtil.getMessage(e)}, e, pos));
      }
   }

   /**
    * Expands a transformer and compiles the resulting code.
    * @param transformer The transformer function.
    * @param expr The pair containing the entire transformer call.
    * @param bSetExpr Flag indicating if this is a set! transformer call.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileTransformerExpansion(SyntaxFunction transformer, Object expr,
      boolean bSetExpr, Instruction parent, TextPosition pos)
   {
      if (bSetExpr && !transformer.isVariable())
      {
         throw setURL(new CompilerException("err.compiler.syntax.invariableTransformer",
            new Object[]{transformer.getSymbol()}, null, pos));
      }

      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("args = " + Intrinsic.toString(expr));
      }

      SyntaxTransformerContext transformerContext = m_machine.getTransformerContext();

      transformerContext.initCompilation();

      try
      {
         Object expansion = transformerContext.expandTransformer(transformer, expr);

         if (s_logger.isDumpEnabled())
         {
            s_logger.dump("expansion = " + Intrinsic.toString(expansion));
         }

         return compile(expansion, parent, pos);
      }
      catch (Throwable e)
      {
         throw setURL(new CompilerException("err.compiler.syntax.transformerExpansion",
            new Object[]{transformer.getSymbol(), expr}, e, pos));
      }
      finally
      {
         transformerContext.finishCompilation();
      }
   }

   /**
    * Compiles a function call.
    * @param expr The pair containing the function call.
    * @param func The function instruction. Null to determine from expr.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileCall(Pair expr, Instruction func, Instruction parent, TextPosition pos)
   {
      Object obj = expr.getHead();
      CallInstruction call = new CallInstruction(
         m_machine.getGlobalEnvironment().isOptionSet(GlobalEnvironment.OPTION_REDEFINE_INTRINSICS));

      call.setParent(parent);
      call.setTextPos(pos);

      if (func == null)
      {
         func = compile(obj, call, pos);
      }
      else
      {
         func.setParent(call);
      }

      call.setFunction(func);

      for (obj = expr.getTail(); obj != null; obj = expr.getTail())
      {
         if (!(obj instanceof Pair))
         {
            throw setURL(new CompilerException("err.compiler.callArgs", null, null, pos));
         }

         expr = (Pair)obj;
         call.addArg(compile(expr.getHead(), call, pos));
      }

      if (func instanceof GetFunctionInstruction)
      {
         GetFunctionInstruction lambda = (GetFunctionInstruction)func;
         Frame frame = lambda.getFrame();
         Local var = frame.getLastVar();
         int nArgCount = lambda.getArgCount();

         if (lambda.isVarArg())
         {
            --nArgCount;
            var = var.getNext();
         }

         if (nArgCount <= call.getArgCount())
         {
            for (int i = nArgCount - 1; i >= 0; --i, var = var.getNext())
            {
               Instruction instr = call.getArg(i);

               if (instr instanceof GetFunctionInstruction)
               {
                  ((GetFunctionInstruction)instr).setName(var.getSymbol().getName());
               }
            }
         }
      }

      return call;
   }

   /**
    * Compiles a reference to a constant.
    * @param value The constant value.
    * @param parent The parent instruction.
    * @param pos The text position. Can be null.
    * @return The first compiled intermediate language instruction.
    */
   protected Instruction compileConstant(Object value, Instruction parent, TextPosition pos)
   {
      Instruction instr = new GetConstantInstruction(value);

      instr.setParent(parent);
      instr.setTextPos(pos);

      return instr;
   }

   /**
    * Finds a variable by symbol in the given frame or its parent frames,
    * skipping over the ignored frame, if one is set.
    * @param frame The frame to start from.
    * @param symbol The symbol to look up.
    * @return The variable, or null if not found.
    */
   protected Local getLocalDefinition(Frame frame, Symbol symbol)
   {
      if (frame == null)
      {
         return null;
      }

      if (m_machine.getExpansionCount() > 0)
      {
         return frame.findSymbolRec(symbol, m_machine.getTransformerContext());
      }

      return frame.findVarRec(symbol);
   }

   /**
    * Verifies that the given symbol is valid as a variable name in the given frame.
    * @param symbol The original symbol. May be replaced during compilation if necessary.
    * @param frame The frame to check. May not be null.
    * @param pos The text position.
    * @return Either the given symbol, or a newly generated temporary symbol if renaming is necessary.
    */
   protected Symbol verifyArgSymbol(Symbol symbol, Frame frame, TextPosition pos)
   {
      assert frame != null;

      if (frame.findVar(symbol) != null)
      {
         throw setURL(new CompilerException("err.compiler.dupArg", new Object[]{symbol}, null, pos));
      }

      return symbol;
   }

   /**
    * Verifies the argument count in a function call.
    * @param firstPair The pair holding the first argument.
    * @param func The object which provides the function name string through toString().
    * @param nMinCount The minimum valid argument count.
    * @param nMaxCount The maximum valid argument count. -1 is unlimited.
    * @param pos The text position for the error message.
    * @return The found argument count.
    * @throws CompilerException if not a pair list is passed or
    * if the argument count is outside the allowed range.  
    */
   protected int verifyArgCount(Object firstPair, Object func,
      int nMinCount, int nMaxCount, TextPosition pos) throws CompilerException
   {
      int nCount = 0;
      
      while (firstPair != null)
      {
         if (!(firstPair instanceof Pair))
         {
            throw setURL(new CompilerException("err.compiler.callArgs", null, null, pos));
         }
         
         firstPair = ((Pair)firstPair).getTail();
         ++nCount;
      }
      
      if (nCount < nMinCount)
      {
         throw setURL(new CompilerException("err.compiler.minArgCount",
            new Object[]{func, Primitive.createInteger(nMinCount)}, null, pos));
      }
      
      if (nMaxCount >= 0 && nCount > nMaxCount)
      {
         throw setURL(new CompilerException("err.compiler.maxArgCount",
            new Object[]{func, Primitive.createInteger(nMaxCount)}, null, pos));
      }
      
      return nCount;
   } 

   /**
    * Creates and pushes a new frame on the top of frame stack.
    * @return The new frame.
    */
   protected Frame pushFrame()
   {
      Frame frame = new Frame(m_defineSyntaxSymbol, m_machine.getExpansionCount());

      frame.setParent(m_topFrame);
      m_topFrame = frame;

      return frame;
   }

   /**
    * Pops the innermost frame from the frame stack.
    * @return The removed frame.
    */
   protected Frame popFrame()
   {
      Frame frame = m_topFrame;

      m_topFrame = frame.getParent();

      return frame;
   }

   /**
    * Removes the specified frame for the frame stack.
    * @param frame The frame to remove.
    */
   protected void removeFrame(Frame frame)
   {
      if (m_topFrame == frame)
      {
         m_topFrame = frame.getParent();
      }
      else
      {
         for (Frame f = m_topFrame; ; f = f.getParent())
         {
            if (f.getParent() == frame)
            {
               f.setParent(frame.getParent());

               break;
            }
         }
      }

      frame.setParent(null);
   }

   /**
    * Sets the source code URL on a compiler exception.
    * @param e The compiler exception on which to set the URL.
    * @return e
    */
   protected CompilerException setURL(CompilerException e)
   {
      if (m_urlHolder != null)
      {
         if (m_urlHolder instanceof String)
         {
            e.setURL((String)m_urlHolder);
         }
         else if (e.getTextPosition() != null)
         {
            e.setURL((String)((Lookup)m_urlHolder).get(e.getTextPosition()));
         }
      }

      return e;
   }

   /**
    * Sets a given URL for the text positions corresponding to the specified expression.
    * @param expr The specified expression to parse.
    * @param sURL The URL to set.
    * @param posMap The text position map.
    * @param urlMap The URL map.
    */
   public static void setPosURLs(Object expr, String sURL, Lookup posMap, Lookup urlMap)
   {
      while (expr != null)
      {
         Object pos = posMap.get(expr);

         if (pos != null)
         {
            urlMap.put(pos, sURL);
         }

         if (expr instanceof Pair)
         {
            setPosURLs(((Pair)expr).getHead(), sURL, posMap, urlMap);
            expr = ((Pair)expr).getTail();
         }
         else
         {
            break;
         }
      }
   }

   /**
    * Sets a given URL for the text positions corresponding to the specified expression.
    * @param expr The specified expression to parse.
    * @param sURL The URL to set.
    * @param posMap The text position map.
    */
   public static void setPosURLs(Object expr, String sURL, Lookup posMap)
   {
      while (expr != null)
      {
         TextPosition pos = (TextPosition)posMap.get(expr);

         if (pos != null)
         {
            pos.setURL(sURL);
         }

         if (expr instanceof Pair)
         {
            setPosURLs(((Pair)expr).getHead(), sURL, posMap);
            expr = ((Pair)expr).getTail();
         }
         else
         {
            break;
         }
      }
   }

   /**
    * Finds a text position URL corresponding to the specified expression or one of its sub-expressions.
    * @param expr The expression to find the URL.
    * @param posMap The text position map.
    * @return The text position URL; null if not found.
    */
   public static String findPosURL(Object expr, Lookup posMap)
   {
      while (expr != null)
      {
         TextPosition pos = (TextPosition)posMap.get(expr);

         if (pos != null && !StringUtil.isEmpty(pos.getURL()))
         {
            return pos.getURL();
         }

         if (expr instanceof Pair)
         {
            String sURL = findPosURL(((Pair)expr).getHead(), posMap);

            if (!StringUtil.isEmpty(sURL))
            {
               return sURL;
            }

            expr = ((Pair)expr).getTail();
         }
         else
         {
            break;
         }
      }

      return null;
   }

   // inner classes

   /**
    * Represents a local variable frame.
    */
   protected final static class Frame
   {
      // attributes

      /**
       * The usage flag.
       */
      protected boolean m_bUsed;

      /**
       * The transformer expansion level at which this frame was created.
       */
      protected int m_nExpLevel;

      // associations

      /**
       * The last variable in this frame.
       */
      private Local m_lastVar;

      /**
       * The parent frame. Can be null.
       */
      private Frame m_parent;

      /**
       * The symbol of the transformer function being created, if any.
       */
      protected final Symbol m_transformerSymbol;

      // operations

      /**
       * Creates a new frame.
       * @param transformerSymbol The symbol of the transformer function being created, if any.
       * @param nExpLevel The transformer expansion level at which this frame was created.
       */
      protected Frame(Symbol transformerSymbol, int nExpLevel)
      {
         m_transformerSymbol = transformerSymbol;
         m_nExpLevel = nExpLevel;
      }

      /**
       * Sets the usage flag.
       * @param bUsed The usage flag to set.
       */
      public void setUsed(boolean bUsed)
      {
         m_bUsed = bUsed;
      }

      /**
       * @return The usage flag.
       */
      public boolean isUsed()
      {
         return m_bUsed;
      }

      /**
       * @return The symbol of the transformer definition this frame belongs to,
       * or null if not a transformer definition frame.
       */
      protected Symbol getTransformerSymbol()
      {
         return m_transformerSymbol;
      }

      /**
       * Sets the parent frame. Can be null.
       * @param parent The parent frame.
       */
      public void setParent(Frame parent)
      {
         m_parent = parent;
      }

      /**
       * @return The parent frame. Can be null.
       */
      public Frame getParent()
      {
         return m_parent;
      }
      
      /**
       * @return The last added variable. Can be null.
       * The added variables form a linked list.
       */
      public Local getLastVar()
      {
         return m_lastVar;
      }

      /**
       * Adds a variable to the frame.
       * @param var The variable to add.
       */
      public void addVar(Local var)
      {
         var.setNext(m_lastVar);
         var.setFrame(this);
         m_lastVar = var;
      }

      /**
       * Removes a variable from the frame.
       * @param var The variable to remove.
       */
      public void removeVar(Local var)
      {
         assert var.getFrame() == this;

         if (m_lastVar == var)
         {
            m_lastVar = var.getNext();
         }
         else
         {
            for (Local last = m_lastVar; ; last = last.getNext())
            {
               if (last.getNext() == var)
               {
                  last.setNext(var.getNext());

                  break;
               }
            }
         }

         var.setFrame(null);
         var.setNext(null);
      }
      
      /**
       * Finds a variable by symbol in this frame. Compares symbols by identity.
       * @param symbol The variable symbol.
       * @return The variable, or null if not found.
       */
      public Local findVar(Symbol symbol)
      {
         for (Local var = m_lastVar; var != null; var = var.getNext())
         {
            if (var.getSymbol() == symbol)
            {
               return var;
            }
         }

         return null;
      }

      /**
       * Finds a variable by symbol in this or any parent frame.
       * @param symbol The variable symbol.
       * @return The variable, or null if not found.
       */
      public Local findVarRec(Symbol symbol)
      {
         Frame frame = this;
         
         do
         {
            Local var = frame.findVar(symbol);
            
            if (var != null)
            {
               return var;
            }

            frame = frame.getParent();
         }
         while (frame != null);
         
         return null;
      }

      /**
       * Finds a variable by identifier equivalence comparison in this frame.
       * @param identifier The variable identifier.
       * @param transformerContext The transformer context.
       * @return The variable, or null if not found.
       */
      protected Local findIdentifier(Symbol identifier, SyntaxTransformerContext transformerContext)
      {
         for (Local var = m_lastVar; var != null; var = var.getNext())
         {
            if (transformerContext.compareSymbols(identifier, var.getSymbol()))
            {
               return var;
            }
         }

         return null;
      }

      /**
       * Finds a variable in this frame or any parent frame by symbol identity comparison,
       * identifier equivalence comparison or identifier resolution, depending on whether the
       * symbol is an identifier and the transformer expansion level of the frame relative
       * to the current expansion count.
       * @param symbol The variable symbol.
       * @param transformerContext The current expansion context.
       * @return The variable, or null if not found.
       */
      protected Local findSymbolRec(Symbol symbol, SyntaxTransformerContext transformerContext)
      {
         Frame frame = this;
         int nExpLevel = transformerContext.getExpansionCount();

         if (transformerContext.isIdentifier(symbol)) // transformer variable symbol
         {
            do
            {
               if (nExpLevel != frame.m_nExpLevel)
               {
                  return frame.resolveIdentifierRec(symbol, transformerContext);
               }

               Local var = frame.findIdentifier(symbol, transformerContext);

               if (var != null)
               {
                  return var;
               }

               frame = frame.getParent();
            }
            while (frame != null);
         }
         else // transformer template symbol
         {
            do
            {
               if (nExpLevel != frame.m_nExpLevel)
               {
                  // Find local definition from the transformer frame.
                  SyntaxFunction transformer = transformerContext.getTransformer();

                  if (transformer instanceof LocalSyntaxTransformer)
                  {
                     return ((LocalSyntaxTransformer)transformer).getFrame().findVarRec(symbol);
                  }

                  return null;
               }

               Local var = frame.findVar(symbol);

               if (var != null)
               {
                  return var;
               }

               frame = frame.getParent();
            }
            while (frame != null);
         }

         return null;
      }

      /**
       * Finds a variable by identifier resolution in this frame.
       * @param identifier The variable identifier.
       * @param transformerContext The expansion context.
       * @return The variable, or null if not found.
       */
      protected Local resolveIdentifier(Symbol identifier, SyntaxTransformerContext transformerContext)
      {
         for (Local var = m_lastVar; var != null; var = var.getNext())
         {
            int nVal = transformerContext.resolveIdentifier(identifier, var.getSymbol());

            if (nVal == 0)
            {
               SyntaxFunction transformer = transformerContext.getTransformer();

               if (var.getFrame().getTransformerSymbol() == transformer.getSymbol()
                  || transformer.isContained(var.getFrame()))
               {
                  return var;
               }

               break;
            }
            else if (nVal > 0)
            {
               return var;
            }
         }

         return null;
      }

      /**
       * Finds a variable by identifier resolution in this frame or any parent frame.
       * @param identifier The variable identifier.
       * @param transformerContext The expansion context.
       * @return The variable, or null if not found.
       */
      protected Local resolveIdentifierRec(Symbol identifier, SyntaxTransformerContext transformerContext)
      {
         Frame frame = this;

         do
         {
            Local var = frame.resolveIdentifier(identifier, transformerContext);

            if (var != null)
            {
               return var;
            }

            frame = frame.getParent();
         }
         while (frame != null);

         return null;
      }

      /**
       * Computes the offset of this frame relative to another frame.
       * @param frame The frame relative to which to compute the offset.
       * @return The offset of this frame.
       */
      public int getOffset(Frame frame)
      {
         int nOffset = 0;
         
         while (frame != this)
         {
            if (frame.m_bUsed)
            {
               ++nOffset;
            }

            frame = frame.m_parent;
         }

         return nOffset;
      }
   }

   /**
    * Represents a local variable.
    */
   protected final static class Local
   {
      // associations

      /**
       * The variable symbol.
       */
      private Symbol m_symbol;

      /**
       * The variable frame.
       */
      private Frame m_frame;

      /**
       * The next variable in the frame.
       */
      private Local m_next;

      /**
       * The transformer bound to this variable, if any.
       */
      private LocalSyntaxTransformer m_transformer;

      // constructors

      /**
       * Constructs a local variable.
       * @param symbol The variable symbol or identifier.
       */
      public Local(Symbol symbol)
      {
         m_symbol = symbol;
      }

      // operations

      /**
       * Sets the variable symbol.
       * @param symbol The variable symbol or identifier to set.
       */
      public void setSymbol(Symbol symbol)
      {
         m_symbol = symbol;
      }

      /**
       * @return The variable symbol or identifier.
       */
      public Symbol getSymbol()
      {
         return m_symbol;
      }
      
      /**
       * Sets the variable frame.
       * @param frame The variable frame to set.
       */
      public void setFrame(Frame frame)
      {
         m_frame = frame;
      }

      /**
       * @return The variable frame.
       */
      public Frame getFrame()
      {
         return m_frame;
      }

      /**
       * Sets the next variable in the frame.
       * @param next The next variable in the frame to set.
       */
      public void setNext(Local next)
      {
         m_next = next;
      }

      /**
       * @return The next variable in the frame.
       */
      public Local getNext()
      {
         return m_next;
      }
      
      /**
       * Removes the variable from its frame.
       */
      public void remove()
      {
         m_frame.removeVar(this);
      }

      /**
       * @return The offset of the variable in the local frame.
       */
      public int getOffset()
      {
         int nOffset = 1;

         for (Local local = this.m_next; local != null; local = local.m_next)
         {
            ++nOffset;
         }

         return nOffset;
      }

      /**
       * Flags the variable as used.
       */
      public void setUsed()
      {
         m_frame.setUsed(true);
      }

      /**
       * @return True if this variable is bound to a transformer.
       */
      protected boolean isTransformer()
      {
         return m_transformer != null;
      }

      /**
       * @return The transformer bound to this variable, or null if it is not bound to a transformer.
       */
      protected SyntaxFunction getTransformer()
      {
         return m_transformer;
      }

      /**
       * Sets the transformer bound to this variable.
       * @param transformer The transformer function object.
       */
      protected void setTransformer(SyntaxFunction transformer)
      {
         m_transformer = (LocalSyntaxTransformer)transformer;
      }
   }

   /**
    * Base class for VM instructions.
    */
   protected abstract static class Instruction
   {
      // constants

      public final static int GET_LOCAL = 0;
      public final static int SET_LOCAL = 1;
      public final static int GET_GLOBAL = 2;
      public final static int SET_GLOBAL = 3;
      public final static int DEF_GLOBAL = 4;
      public final static int GET_CONST = 5;
      public final static int GET_FUNCTION = 6;
      public final static int BRANCH = 7;
      public final static int CALL = 8;
      
      // attributes

      /**
       * True if the value created by the instruction is not needed.
       */
      protected boolean m_bVoid;

      /**
       * True if the instruction is executed last in the function.
       */
      protected boolean m_bLast;

      // associations

      /**
       * The previous instruction.
       */
      protected Instruction m_previous;

      /**
       * The next instruction.
       */
      protected Instruction m_next;

      /**
       * The parent instruction.
       */
      protected Instruction m_parent;

      /**
       * The source code text position.
       */
      protected TextPosition m_textPos;

      // operations

      /**
       * @return The instruction code.
       */
      public abstract int getCode();
      
      /**
       * Optimizes this instruction.
       * Do NOT call directly from optimize, call optimizeAll instead. 
       * @param compiler The compiler object.
       * @return The resulting instruction.
       */
      protected Instruction optimize(Compiler compiler)
      {
         return this;
      }
      
      /**
       * Optimizes this and the following sibling instructions.
       * @param compiler The compiler object.
       * @return The resulting instruction.
       */
      public Instruction optimizeAll(Compiler compiler)
      {
         Instruction first = null;
         
         for (Instruction instr = this; instr != null; instr = instr.m_next)
         {
            Instruction opt = instr.optimize(compiler);

            instr.replaceBy(opt);

            if (first == null)
            {
               first = opt;
            }
         }

         return first;
      }

      /**
       * Generates the p-code for the instruction.
       * @param buf The p-code output buffer.
       */
      protected abstract void generate(PCodeBuffer buf);

      /**
       * Generates the p-code for this and the following sibling instructions.
       * @param buf The p-code output buffer.
       */
      protected final void generateAll(PCodeBuffer buf)
      {
         for (Instruction instr = this; instr != null; instr = instr.m_next)
         {
            instr.generate(buf);
         }
      }

      /**
       * Generates a p-code function from the instruction.
       * @param compiler The compiler object.
       * @return The generated p-code function.
       */
      public PCodeFunction generate(Compiler compiler)
      {
         PCodeBuffer buf = new PCodeBuffer(compiler);

         generateAll(buf);

         PCodeFunction fun = createPCodeFunction();

         buf.copyTo(fun);

         return fun;
      } 
      
      /**
       * Template method to create the p-code function for this instruction.
       * @return A new empty instance of a p-code function.
       */
      protected PCodeFunction createPCodeFunction()
      {
         return new PCodeFunction();
      }

      /**
       * Generates return if the instruction is last.
       * @param buf The p-code output buffer.
       */
      protected void returnIfLast(PCodeBuffer buf)
      {
         if (m_bLast)
         {
            buf.addCode(Machine.RETURN);
         }
      }
      
      /**
       * Generates pop if the instruction is void.
       * @param buf The p-code output buffer.
       */
      protected void popIfVoid(PCodeBuffer buf)
      {
         if (m_bVoid)
         {
            buf.addCode(Machine.POP);
         }
      }

      /**
       * Sets the previous instruction.
       * @param previous The previous instruction to set.
       */
      public void setPrevious(Instruction previous)
      {
         m_previous = previous;
      }

      /**
       * @return The previous instruction.
       */
      public Instruction getPrevious()
      {
         return m_previous;
      }
      
      /**
       * Sets the next instruction.
       * @param next The next instruction to set.
       */
      public void setNext(Instruction next)
      {
         m_next = next;
      }

      /**
       * @return The next instruction.
       */
      public Instruction getNext()
      {
         return m_next;
      }

      /**
       * @return The last instruction in the list. 
       */
      public Instruction getLast()
      {
         Instruction instr;
         
         for (instr = this; instr.m_next != null; instr = instr.m_next);
         
         return instr;
      }
      
      /**
       * Sets the parent instruction.
       * @param parent The parent instruction to set.
       */
      public void setParent(Instruction parent)
      {
         m_parent = parent;
      }

      /**
       * @return The parent instruction.
       */
      public Instruction getParent()
      {
         return m_parent;
      }

      /**
       * Sets the source code text position.
       * @param textPos The source code text position to set.
       */
      public void setTextPos(TextPosition textPos)
      {
         m_textPos = textPos;
      }

      /**
       * @return The source code text position.
       */
      public TextPosition getTextPos()
      {
         return m_textPos;
      }

      /**
       * Appends an instruction after this one.
       * @param instr The instruction to append.
       */
      public void append(Instruction instr)
      {
         instr.setNext(m_next);
         instr.setPrevious(this);
         
         if (m_next != null)
         {
            m_next.setPrevious(instr);
         }
         
         setNext(instr);
         instr.setParent(m_parent);
      }
      
      /**
       * Prepends an instruction before this one.
       * @param instr The instruction to prepend.
       */
      public void prepend(Instruction instr)
      {
         instr.setPrevious(m_previous);
         instr.setNext(this);
         
         if (m_previous != null)
         {
            m_previous.setNext(instr);
         }
         
         setPrevious(instr);
         instr.setParent(m_parent);
      }

      /**
       * Replaces this instruction by another one.
       * @param instr The instruction by which to replace this instruction.
       */
      public void replaceBy(Instruction instr)
      {
         if (instr != this)
         {
            instr.setPrevious(m_previous);

            if (m_previous != null)
            {
               m_previous.setNext(instr);
            }

            for (;;)
            {
               instr.setParent(m_parent);

               if (instr.getNext() == null)
               {
                  break;
               }

               instr = instr.getNext();
            }

            instr.setNext(m_next);

            if (m_next != null)
            {
               m_next.setPrevious(instr);
            }

            instr.setFlags(m_bVoid, m_bLast);
         }
      }

      /**
       * Removes the current instruction.
       */
      public void remove()
      {
         if (m_previous != null)
         {
            m_previous.setNext(m_next);
         }

         if (m_next != null)
         {
            m_next.setPrevious(m_previous);
         }

         setPrevious(null);
         setNext(null);
         setParent(null);
      }

      /**
       * Sets the void flag on the instruction.
       * @param bVoid The void flag.
       */
      public void setVoid(boolean bVoid)
      {
         m_bVoid = bVoid;
      }

      /**
       * @return True if the value created by the instruction is not needed.
       */
      public boolean isVoid()
      {
         return m_bVoid;  
      }

      /**
       * Sets the last flag on the instruction.
       * @param bLast The last flag.
       */
      public void setLast(boolean bLast)
      {
         m_bLast = bLast;
      }

      /**
       * @return True if the instruction is executed last in the function.
       */
      public boolean isLast()
      {
         return m_bLast;
      }
      
      /**
       * Sets the evaluation flags on this instruction.
       * @param bVoid The void flag.
       * @param bLast The last flag.
       */
      protected void setFlags(boolean bVoid, boolean bLast)
      {
         m_bVoid = bVoid || m_next != null;
         m_bLast = bLast && m_next == null;
      }
      
      /**
       * Sets the evaluation flags on this and the following sibling instructions.
       * @param bVoid The void flag.
       * @param bLast The last flag.
       */
      public void setFlagsAll(boolean bVoid, boolean bLast)
      {
         for (Instruction instr = this; instr != null; instr = instr.m_next)
         {
            instr.setFlags(bVoid, bLast);
         }
      }
   }

   /**
    * Interface for unary instructions.
    */
   protected interface Unary
   {
      /**
       * Sets the operand value.
       * @param value The operand value to set.
       */
      public void setValue(Instruction value);

      /**
       * @return The operand value.
       */
      public Instruction getValue();
   }

   /**
    * Base class for local variable instructions.
    */
   protected abstract static class LocalVarInstruction extends Instruction
   {
      // associations
      
      /**
       * The local variable.
       */
      protected Local m_var;
      
      /**
       * The current frame.
       */
      private Frame m_frame;

      // constructors

      /**
       * Constructs the instruction.
       * @param var The local variable.
       * @param frame The current frame.
       */
      public LocalVarInstruction(Local var, Frame frame)
      {
         m_var = var;
         m_frame = frame;
      }

      // operations

      /**
       * Sets the local variable.
       * @param var The local variable to set.
       */
      public void setVar(Local var)
      {
         m_var = var;
      }

      /**
       * @return The local variable.
       */
      public Local getVar()
      {
         return m_var;
      }

      /**
       * Sets the current frame.
       * @param frame The current frame to set.
       */
      public void setFrame(Frame frame)
      {
         m_frame = frame;
      }

      /**
       * @return The current frame.
       */
      public Frame getFrame()
      {
         return m_frame;
      }
   }

   /**
    * Instruction to get a local variable value.
    */
   protected final static class GetLocalInstruction extends LocalVarInstruction
   {
      // constructors

      /**
       * Constructs the instruction.
       * @param var The local variable to get.
       * @param frame The current frame.
       */
      public GetLocalInstruction(Local var, Frame frame)
      {
         super(var, frame);
      }

      // operations

      /**
       * @see nexj.core.scripting.Compiler.Instruction#getCode()
       */
      public int getCode()
      {
         return GET_LOCAL;
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#optimize(nexj.core.scripting.Compiler)
       */
      protected Instruction optimize(Compiler compiler)
      {
         if (!m_bVoid)
         {
            m_var.setUsed();
         }

         return this;
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#generate(nexj.core.scripting.Compiler.PCodeBuffer)
       */
      protected void generate(PCodeBuffer buf)
      {
         if (!m_bVoid)
         {
            int nOffset = m_var.getFrame().getOffset(getFrame());

            if (nOffset <= 4)
            {
               buf.addCode(Machine.PUSH_LOCAL_0 + nOffset, m_var.getOffset());
            }
            else
            {
               buf.addCode(Machine.PUSH_LOCAL, nOffset, m_var.getOffset());
            }
         }

         returnIfLast(buf);
      }
   } 

   /**
    * Instruction to set a local variable value.
    */
   protected final static class SetLocalInstruction extends LocalVarInstruction implements Unary
   {
      // associations

      /**
       * The value to set.
       */
      private Instruction m_value;

      // constructors

      /**
       * Constructs the instruction.
       * @param var The local variable to set.
       * @param frame The current frame. 
       */
      public SetLocalInstruction(Local var, Frame frame)
      {
         super(var, frame);
      }

      // operations

      /**
       * @see nexj.core.scripting.Compiler.Instruction#getCode()
       */
      public int getCode()
      {
         return SET_LOCAL;
      }

      /**
       * Sets the value to set.
       * @param value The value to set to set.
       */
      public void setValue(Instruction value)
      {
         m_value = value;
      }

      /**
       * @return The value to set.
       */
      public Instruction getValue()
      {
         return m_value;
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#setFlags(boolean, boolean)
       */
      protected void setFlags(boolean bVoid, boolean bLast)
      {
         super.setFlags(bVoid, bLast);
         m_value.setFlagsAll(false, false);
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#optimize(nexj.core.scripting.Compiler)
       */
      protected Instruction optimize(Compiler compiler)
      {
         m_value = m_value.optimizeAll(compiler);
         m_var.setUsed();

         return this;
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#generate(nexj.core.scripting.Compiler.PCodeBuffer)
       */
      protected void generate(PCodeBuffer buf)
      {
         m_value.generateAll(buf);

         int nOffset = m_var.getFrame().getOffset(getFrame());

         buf.addTextPosition(m_textPos);

         if (nOffset <= 4)
         {
            buf.addCode(Machine.SET_LOCAL_0 + nOffset, m_var.getOffset());
         }
         else
         {
            buf.addCode(Machine.SET_LOCAL, nOffset, m_var.getOffset());
         }

         popIfVoid(buf);
         returnIfLast(buf);
      }
   }

   /**
    * Base class for global variable instructions.
    */
   protected abstract static class GlobalVarInstruction extends Instruction
   {
      // associations
      
      /**
       * The global variable symbol.
       */
      private Symbol m_symbol;

      // constructors
      
      /**
       * Constructs the instruction.
       * @param symbol The global variable symbol.
       */
      public GlobalVarInstruction(Symbol symbol)
      {
         m_symbol = symbol;
      }
      
      // operations
      
      /**
       * Sets the global variable symbol.
       * @param symbol The global variable symbol to set.
       */
      public void setSymbol(Symbol symbol)
      {
         m_symbol = symbol;
      }

      /**
       * @return The global variable symbol.
       */
      public Symbol getSymbol()
      {
         return m_symbol;
      }
   }
   
   /**
    * Instruction to get a global variable.
    */
   protected final static class GetGlobalInstruction extends GlobalVarInstruction
   {
      // constructors

      /**
       * Constructs the instruction.
       * @param symbol The global variable symbol.
       */
      public GetGlobalInstruction(Symbol symbol)
      {
         super(symbol);
      }

      // operations

      /**
       * @see nexj.core.scripting.Compiler.Instruction#getCode()
       */
      public int getCode()
      {
         return GET_GLOBAL;
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#generate(nexj.core.scripting.Compiler.PCodeBuffer)
       */
      protected void generate(PCodeBuffer buf)
      {
         if (!m_bVoid)
         {
            buf.addCode(Machine.PUSH_GLOBAL, buf.addConstant(getSymbol()));
         }

         returnIfLast(buf);
      }
   }

   protected abstract static class GlobalAssignmentInstruction extends GlobalVarInstruction implements Unary
   {
      // associations

      /**
       * The value to set.
       */
      private Instruction m_value;

      // constructors

      /**
       * Constructs the instruction.
       * @param symbol The global variable symbol. 
       */
      public GlobalAssignmentInstruction(Symbol symbol)
      {
         super(symbol);
      }
      
      // operations
      
      /**
       * Sets the value to set.
       * @param value The value to set to set.
       */
      public void setValue(Instruction value)
      {
         m_value = value;
      }

      /**
       * @return The value to set.
       */
      public Instruction getValue()
      {
         return m_value;
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#setFlags(boolean, boolean)
       */
      protected void setFlags(boolean bVoid, boolean bLast)
      {
         super.setFlags(bVoid, bLast);
         m_value.setFlagsAll(false, false);
      }
      
      /**
       * @see nexj.core.scripting.Compiler.Instruction#optimize(nexj.core.scripting.Compiler)
       */
      protected Instruction optimize(Compiler compiler)
      {
         m_value = m_value.optimizeAll(compiler);
         
         return this;
      }
   }

   /**
    * Instruction to set a global variable.
    */
   protected final static class SetGlobalInstruction extends GlobalAssignmentInstruction
   {
      // constructors

      /**
       * Constructs the instruction.
       * @param symbol The global variable symbol.
       */
      public SetGlobalInstruction(Symbol symbol)
      {
         super(symbol);
      }

      // operations

      /**
       * @see nexj.core.scripting.Compiler.Instruction#getCode()
       */
      public int getCode()
      {
         return SET_GLOBAL;
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#generate(nexj.core.scripting.Compiler.PCodeBuffer)
       */
      protected void generate(PCodeBuffer buf)
      {
         getValue().generateAll(buf);
         buf.addTextPosition(m_textPos);
         buf.addCode(Machine.SET_GLOBAL, buf.addConstant(getSymbol()));
         popIfVoid(buf);
         returnIfLast(buf);
      }
   }
   
   /**
    * Instruction to define a global variable.
    */
   protected final static class DefGlobalInstruction extends GlobalAssignmentInstruction
   {
      // constructors

      /**
       * Constructs the instruction.
       * @param symbol The global variable symbol.
       */
      public DefGlobalInstruction(Symbol symbol)
      {
         super(symbol);
      }

      // operations

      /**
       * @see nexj.core.scripting.Compiler.Instruction#getCode()
       */
      public int getCode()
      {
         return DEF_GLOBAL;
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#generate(nexj.core.scripting.Compiler.PCodeBuffer)
       */
      protected void generate(PCodeBuffer buf)
      {
         getValue().generateAll(buf);
         buf.addTextPosition(m_textPos);
         buf.addCode(Machine.DEF_GLOBAL, buf.addConstant(getSymbol()));
         popIfVoid(buf);
         returnIfLast(buf);
      }
   }

   /**
    * Instruction to get a constant value.
    */
   protected final static class GetConstantInstruction extends Instruction
   {
      // attributes

      /**
       * The constant value.
       */
      private Object m_value;

      // constructors

      /**
       * Constructs the instruction.
       * @param value The constant value.
       */
      public GetConstantInstruction(Object value)
      {
         m_value = value;
      }

      // operations

      /**
       * @see nexj.core.scripting.Compiler.Instruction#getCode()
       */
      public int getCode()
      {
         return GET_CONST;
      }

      /**
       * Sets the constant value.
       * @param value The constant value to set.
       */
      public void setValue(Object value)
      {
         m_value = value;
      }

      /**
       * @return The constant value.
       */
      public Object getValue()
      {
         return m_value;
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#generate(nexj.core.scripting.Compiler.PCodeBuffer)
       */
      protected void generate(PCodeBuffer buf)
      {
         if (!m_bVoid)
         {
            if (m_value == null)
            {
               buf.addCode(Machine.PUSH_NULL);
            }
            else if (m_value.equals(Primitive.ZERO_INTEGER))
            {
               buf.addCode(Machine.PUSH_ZERO);
            }
            else if (m_value.equals(Primitive.ONE_INTEGER))
            {
               buf.addCode(Machine.PUSH_ONE);
            }
            else if (m_value.equals(Boolean.TRUE))
            {
               buf.addCode(Machine.PUSH_TRUE);
            }
            else if (m_value.equals(Boolean.FALSE))
            {
               buf.addCode(Machine.PUSH_FALSE);
            }
            else
            {
               buf.addCode(Machine.PUSH_CONST, buf.addConstant(m_value));
            }
         }

         returnIfLast(buf);
      }
   }

   /**
    * Instruction to get a function reference (closure).
    */
   protected static class GetFunctionInstruction extends Instruction
   {
      // attributes

      /**
       * The formal argument count (including the last variable argument, if any).
       */
      private int m_nArgCount;

      /**
       * The variable argument flag.
       */
      private boolean m_bVarArg;

      // associations

      /**
       * The function frame.
       */
      private Frame m_frame;

      /**
       * The function body.
       */
      private Instruction m_body;

      /**
       * The function name. Can be null.
       */
      private String m_sName;

      // constructors
      
      /**
       * Constructs the instruction.
       */
      public GetFunctionInstruction()
      {
      }

      /**
       * Constructs the instruction.
       * @param frame The function frame.
       * @param body The function body.
       */
      public GetFunctionInstruction(Frame frame, Instruction body)
      {
         m_frame = frame;
         m_body = body;
      }

      // operations

      /**
       * @see nexj.core.scripting.Compiler.Instruction#getCode()
       */
      public int getCode()
      {
         return GET_FUNCTION;
      }

      /**
       * @return True if the function is a macro.
       */
      public boolean isMacro()
      {
         return false;
      }

      /**
       * Sets the function frame.
       * @param frame The function frame to set.
       */
      public void setFrame(Frame frame)
      {
         m_frame = frame;
      }

      /**
       * @return The function frame.
       */
      public Frame getFrame()
      {
         return m_frame;
      }
      
      /**
       * Sets the function body.
       * @param body The function body to set.
       */
      public void setBody(Instruction body)
      {
         m_body = body;
      }

      /**
       * @return The function body.
       */
      public Instruction getBody()
      {
         return m_body;
      }

      /**
       * Sets the formal argument count.
       * @param nArgCount The formal argument count to set.
       */
      public void setArgCount(int nArgCount)
      {
         m_nArgCount = nArgCount;
      }

      /**
       * @return The formal argument count.
       */
      public int getArgCount()
      {
         return m_nArgCount;
      }

      /**
       * Increments the argument count by one.
       * @return The new argument count.
       */
      public int incArgCount()
      {
         return ++m_nArgCount;
      }

      /**
       * Sets the variable argument flag.
       * @param bVarArg The variable argument flag to set.
       */
      public void setVarArg(boolean bVarArg)
      {
         m_bVarArg = bVarArg;
      }

      /**
       * @return The variable argument flag.
       */
      public boolean isVarArg()
      {
         return m_bVarArg;
      }

      /**
       * Sets the function name.
       * @param sName The function name to set.
       */
      public void setName(String sName)
      {
         m_sName = sName;
      }

      /**
       * @return The function name.
       */
      public String getName()
      {
         return m_sName;
      }
      
      /**
       * @see nexj.core.scripting.Compiler.Instruction#setFlags(boolean, boolean)
       */
      protected void setFlags(boolean bVoid, boolean bLast)
      {
         super.setFlags(bVoid, bLast);
         m_body.setFlagsAll(false, true);
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#optimize(nexj.core.scripting.Compiler)
       */
      protected Instruction optimize(Compiler compiler)
      {
         m_body = m_body.optimizeAll(compiler);
         
         return this;
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#generate(nexj.core.scripting.Compiler.PCodeBuffer)
       */
      protected void generate(PCodeBuffer buf)
      {
         if (!m_bVoid)
         {
            buf.addTextPosition(m_textPos);
            buf.addCode(Machine.PUSH_CLOSURE, buf.addConstant(generate(buf.getCompiler())));
         }

         returnIfLast(buf);
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#generate(Compiler)
       */
      public PCodeFunction generate(Compiler compiler)
      {
         PCodeBuffer buf = new PCodeBuffer(compiler);

         buf.addTextPosition(m_textPos);

         if (m_bVarArg)
         {
            buf.addCode((m_frame.isUsed()) ?
               Machine.SETUP_VARARG_FRAME :
               Machine.CHECK_VARARG_FRAME,
               m_nArgCount - 1);
         }
         else
         {
            buf.addCode((m_frame.isUsed()) ?
               Machine.SETUP_FRAME :
               Machine.CHECK_FRAME,
               m_nArgCount);
         }

         m_body.generateAll(buf);

         PCodeFunction fun = createPCodeFunction();

         buf.setFunction(this);
         buf.copyTo(fun);

         return fun;
      }
   }

   /**
    * Instruction to get a function reference (closure).
    */
   protected final static class GetMacroInstruction extends GetFunctionInstruction
   {
      /**
       * @see nexj.core.scripting.Compiler.GetFunctionInstruction#isMacro()
       */
      public boolean isMacro()
      {
         return true;
      }
      
      /**
       * @see nexj.core.scripting.Compiler.Instruction#createPCodeFunction()
       */
      protected PCodeFunction createPCodeFunction()
      {
         return new PCodeMacro();
      }
   }

   /**
    * Instruction to branch the execution path based on a condition.
    */
   protected final static class BranchInstruction extends Instruction
   {
      // associations

      /**
       * The condition.
       */
      private Instruction m_condition;

      /**
       * The true value.
       */
      private Instruction m_trueValue;

      /**
       * The false value.
       */
      private Instruction m_falseValue;

      // constructors

      /**
       * Constructs the instruction.
       */
      public BranchInstruction()
      {
      }

      /**
       * Constructs the instruction.
       * @param condition The condition instruction.
       * @param trueValue The instruction executing when the condition is true.
       * @param falseValue The instruction executing when the condition is false.
       */
      public BranchInstruction(Instruction condition, Instruction trueValue, Instruction falseValue)
      {
         m_condition = condition;
         m_trueValue = trueValue;
         m_falseValue = falseValue;
      }

      // operations

      /**
       * @see nexj.core.scripting.Compiler.Instruction#getCode()
       */
      public int getCode()
      {
         return BRANCH;
      }
      
      /**
       * Sets the condition.
       * @param condition The condition to set.
       */
      public void setCondition(Instruction condition)
      {
         m_condition = condition;
      }

      /**
       * @return The condition.
       */
      public Instruction getCondition()
      {
         return m_condition;
      }

      /**
       * Sets the true value.
       * @param trueValue The true value to set.
       */
      public void setTrueValue(Instruction trueValue)
      {
         m_trueValue = trueValue;
      }

      /**
       * @return The true value.
       */
      public Instruction getTrueValue()
      {
         return m_trueValue;
      }

      /**
       * Sets the false value.
       * @param falseValue The false value to set.
       */
      public void setFalseValue(Instruction falseValue)
      {
         m_falseValue = falseValue;
      }

      /**
       * @return The false value.
       */
      public Instruction getFalseValue()
      {
         return m_falseValue;
      }
      
      /**
       * @see nexj.core.scripting.Compiler.Instruction#setFlags(boolean, boolean)
       */
      protected void setFlags(boolean bVoid, boolean bLast)
      {
         super.setFlags(bVoid, bLast);
         m_condition.setFlagsAll(false, false);

         if (m_trueValue != null)
         {
            m_trueValue.setFlagsAll(m_bVoid, m_bLast);
         }

         if (m_falseValue != null)
         {
            m_falseValue.setFlagsAll(m_bVoid, m_bLast);
         }
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#optimize(nexj.core.scripting.Compiler)
       */
      protected Instruction optimize(Compiler compiler)
      {
         m_condition = m_condition.optimizeAll(compiler);

         // (if (not cond) expr_t expr_f) -> (if cond expr_f expr_t) 
         Instruction cond = m_condition.getLast();
         Instruction prev = cond.getPrevious();
         Instruction branch = this;

         if (prev != null)
         {
            cond.remove();
            cond.setParent(this);
            prev.append(this);
            branch = m_condition;
            m_condition = cond;
         }

         while (cond.getCode() == CALL)
         {
            CallInstruction call = (CallInstruction)cond;

            if (call.getArgCount() != 1 || call.getFunction().getCode() != GET_GLOBAL)
            {
               break;
            }

            if (!Symbol.NOT.equals(((GetGlobalInstruction)call.getFunction()).getSymbol()))
            {
               break;
            }

            Instruction instr = call.getArg(0);

            cond.replaceBy(instr);

            if (cond == m_condition)
            {
               m_condition = instr;
            }

            cond = instr;
            instr = m_trueValue;
            m_trueValue = m_falseValue;
            m_falseValue = instr;
         }

         if (m_trueValue != null)
         {
            m_trueValue = m_trueValue.optimizeAll(compiler);
         }

         if (m_falseValue != null)
         {
            m_falseValue = m_falseValue.optimizeAll(compiler);
         }

         if (m_condition.getCode() == GET_CONST)
         {
            Instruction instr = (Intrinsic.isTrue(((GetConstantInstruction)m_condition).getValue())) ? m_trueValue : m_falseValue;

            if (instr == null)
            {
               instr = new GetConstantInstruction(null);
            }

            if (prev == null)
            {
               return instr;
            }

            remove();
            prev.append(instr);
         }

         return branch;
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#generate(nexj.core.scripting.Compiler.PCodeBuffer)
       */
      protected void generate(PCodeBuffer buf)
      {
         m_condition.generateAll(buf);

         buf.addTextPosition(m_textPos);
         buf.addCode((m_trueValue == null && m_bVoid) ?
            Machine.JUMP_TRUE :
            Machine.JUMP_FALSE, 0);

         int nCondOffset = buf.getOffset() - 1;
         int nJumpOffset = 0;

         if (m_trueValue != null)
         {
            m_trueValue.generateAll(buf);
            
            if (!m_bLast && (m_falseValue != null || !m_bVoid))
            {
               buf.addCode(Machine.JUMP, 0);
               nJumpOffset = buf.getOffset() - 1;
            }
         }
         else if (!m_bVoid)
         {
            buf.addCode(Machine.PUSH_NULL);
            
            if (m_bLast)
            {
               buf.addCode(Machine.RETURN);
            }
            else
            {
               buf.addCode(Machine.JUMP, 0);
               nJumpOffset = buf.getOffset() - 1;
            }
         }

         if (m_falseValue != null)
         {
            if (m_trueValue != null || !m_bVoid)
            {
               buf.setCode(nCondOffset, buf.getOffset());
            }

            buf.addTextPosition(m_falseValue.m_textPos);
            m_falseValue.generateAll(buf);
            
            if (m_trueValue == null && m_bVoid)
            {
               buf.setCode(nCondOffset, buf.getOffset());
            }
         }
         else if (m_bVoid)
         {
            buf.setCode(nCondOffset, (m_bLast) ? buf.getOffset() - 1 : buf.getOffset());
         }
         else
         {
            buf.setCode(nCondOffset, buf.getOffset());
            buf.addTextPosition(m_textPos);
            buf.addCode(Machine.PUSH_NULL);
            returnIfLast(buf);
         }

         if (nJumpOffset != 0)
         {
            buf.setCode(nJumpOffset, buf.getOffset());
         }

         if (m_next != null)
         {
            buf.addTextPosition(m_next.m_textPos);
         }
      }
   }

   /**
    * Instruction to call a function.
    */
   protected final static class CallInstruction extends Instruction
   {
      // attributes
      
      private boolean m_bRedefineIntrinsics;
      
      // associations

      /**
       * The function to call.
       */
      private Instruction m_function;
      
      /**
       * The argument list: Instruction[n].
       */
      private List m_argList = null;

      // constructors

      /**
       * Constructs the instruction.
       * @param bRedefineIntrinsincs True if intrinsics redefinition is allowed.
       */
      public CallInstruction(boolean bRedefineIntrinsics)
      {
         m_bRedefineIntrinsics = bRedefineIntrinsics;
      }      

      /**
       * Constructs the instruction.
       * @param function The function to call.
       * @param bRedefineIntrinsincs True if intrinsics redefinition is allowed.
       */
      public CallInstruction(Instruction function, boolean bRedefineIntrinsincs)
      {
         m_function = function;
         m_bRedefineIntrinsics = bRedefineIntrinsincs;
      }
      
      // operations
      
      /**
       * @see nexj.core.scripting.Compiler.Instruction#getCode()
       */
      public int getCode()
      {
         return CALL;
      }
      
      /**
       * Sets the function to call.
       * @param function The function to call to set.
       */
      public void setFunction(Instruction function)
      {
         m_function = function;
      }

      /**
       * @return The function to call.
       */
      public Instruction getFunction()
      {
         return m_function;
      }

      /**
       * Adds a new argument to the function.
       * @param arg The argument to add.
       */
      public void addArg(Instruction arg)
      {
         if (m_argList == null)
         {
            m_argList = new ArrayList(8);
         }

         m_argList.add(arg);
      }

      /**
       * Replaces an argument with a given ordinal number.
       * @param nOrdinal The argument ordinal number.
       * @param arg The new argument.
       */
      public void setArg(int nOrdinal, Instruction arg)
      {
         m_argList.set(nOrdinal, arg);
      }

      /**
       * Gets a argument by ordinal number.
       * @param nOrdinal The argument ordinal number (0-based).
       * @return The argument object.
       */
      public Instruction getArg(int nOrdinal)
      {
         return (Instruction)m_argList.get(nOrdinal);
      }

      /**
       * @return The argument count.
       */
      public int getArgCount()
      {
         if (m_argList == null)
         {
            return 0;
         }
         
         return m_argList.size();
      }

      /**
       * @return An iterator for the contained argument objects.
       */
      public Iterator getArgIterator()
      {
         if (m_argList == null)
         {
            return EmptyIterator.getInstance();
         }
         
         return m_argList.iterator();
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#setFlags(boolean, boolean)
       */
      protected void setFlags(boolean bVoid, boolean bLast)
      {
         super.setFlags(bVoid, bLast);
         m_function.setFlags(false, false);
         
         if (m_argList != null)
         {
            int nCount = m_argList.size();
            
            for (int i = 0; i < nCount; ++i)
            {
               ((Instruction)m_argList.get(i)).setFlagsAll(false, false);
            }
         }
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#optimize(nexj.core.scripting.Compiler)
       */
      protected Instruction optimize(Compiler compiler)
      {
         m_function = m_function.optimizeAll(compiler);

         int nArgCount = getArgCount();
         boolean bConstArgs = true;
         Symbol symbol = null;

         if (m_function.getCode() == GET_GLOBAL)
         {
            symbol = ((GetGlobalInstruction)m_function).getSymbol();
         }

         for (int i = 0; i < nArgCount; ++i)
         {
            Instruction instr = getArg(i).optimizeAll(compiler);

            bConstArgs &= instr instanceof GetConstantInstruction;
            setArg(i, instr);
         }

         switch (m_function.getCode())
         {
            case GET_FUNCTION:
               // ((lambda () expr1 ... exprN)) -> (begin expr1 ... exprN)
               if (nArgCount == 0)
               {
                  GetFunctionInstruction fun = (GetFunctionInstruction)m_function;

                  if (fun.getArgCount() == 0 && !fun.isMacro())
                  {
                     return fun.getBody();
                  }
               }

               break;

            case GET_CONST:
               if (((GetConstantInstruction)m_function).getValue() instanceof FrameAware)
               {
                  for (Instruction instr = m_parent; instr != null; instr = instr.m_parent)
                  {
                     if (instr.getCode() == GET_FUNCTION)
                     {
                        Frame frame = ((GetFunctionInstruction)instr).getFrame();
                        
                        for (;;)
                        {
                           if (frame.getParent() == null)
                           {
                              frame.setUsed(true);

                              break;
                           }

                           frame = frame.getParent();
                        }

                        break;
                     }
                  }
               }

               break;

            case GET_GLOBAL:
               if (bConstArgs && !m_bRedefineIntrinsics)
               {
                  IntrinsicFunction fun = Intrinsic.findFunction(symbol);

                  if (fun != null && fun.isDeterministic())
                  {
                     Object[] argArray = null;

                     if (m_argList != null)
                     {
                        argArray = new Object[m_argList.size()];

                        for (int i = 0; i < nArgCount; ++i)
                        {
                           argArray[i] = ((GetConstantInstruction)getArg(i)).getValue();
                        }
                     }

                     try
                     {
                        return new GetConstantInstruction(compiler.m_machine.invoke(fun, argArray));
                     }
                     catch (Throwable t)
                     {
                        // do not fold
                     }
                  }
               }

               break;
         }

         return this;
      }

      /**
       * @see nexj.core.scripting.Compiler.Instruction#generate(nexj.core.scripting.Compiler.PCodeBuffer)
       */
      protected void generate(PCodeBuffer buf)
      {
         switch (m_function.getCode())
         {
            case GET_GLOBAL:
               if (!m_bRedefineIntrinsics)
               {
                  Symbol symbol = ((GetGlobalInstruction)m_function).getSymbol();

                  if (generateIntrinsicCall(buf, Intrinsic.findFunction(symbol)))
                  {
                     return;
                  }
               }

               break;

            case GET_CONST:
               if (generateIntrinsicCall(buf, ((GetConstantInstruction)m_function).getValue()))
               {
                  return;
               }

               break;
         }

         int nArgCount;

         if (m_bLast)
         {
            nArgCount = generateArgs(buf);
            m_function.generateAll(buf);
            buf.addTextPosition(m_textPos);
            buf.addCode(Machine.CALL, nArgCount);
         }
         else
         {
            buf.addCode(Machine.PUSH_PC, 0);

            int nOffset = buf.getOffset() - 1;

            nArgCount = generateArgs(buf);
            m_function.generateAll(buf);
            buf.addTextPosition(m_textPos);
            buf.addCode(Machine.CALL, nArgCount);
            buf.setCode(nOffset, buf.getOffset());
            popIfVoid(buf);
         }
      }

      /**
       * Generates the p-code for the call instruction if the given function is an intrinsic function.
       * @param buf The p-code output buffer.
       * @param fun The function to call.
       * @return True if the p-code was generated and written to the buffer.
       */
      protected boolean generateIntrinsicCall(PCodeBuffer buf, Object fun)
      {
         if (!(fun instanceof IntrinsicFunction))
         {
            return false;
         }

         IntrinsicFunction intrinsicFun = (IntrinsicFunction)fun;
         int nOffset = 0;

         if (!m_bLast && intrinsicFun.isPCode())
         {
            buf.addCode(Machine.PUSH_PC, 0);
            nOffset = buf.getOffset() - 1;
         }

         int nArgCount = generateArgs(buf);

         buf.addTextPosition(m_textPos);

         if (!intrinsicFun.generate(buf, this))
         {
            buf.addCode(Machine.CALL_INTRINSIC + Intrinsic.getOrdinal(intrinsicFun), nArgCount);
         }

         if (nOffset != 0)
         {
            buf.setCode(nOffset, buf.getOffset());
            popIfVoid(buf);
            returnIfLast(buf);
         }
         else
         {
            popIfVoid(buf);

            if (!intrinsicFun.isPCode())
            {
               returnIfLast(buf);
            }
         }

         return true;
      }
      
      /**
       * Generates the argument code.
       * @param buf The p-code output buffer.
       * @return The argument count.
       */
      private int generateArgs(PCodeBuffer buf)
      {
         if (m_argList == null)
         {
            return 0;
         }

         int nCount = m_argList.size();

         for (int i = 0; i < nCount; ++i)
         {
            ((Instruction)m_argList.get(i)).generateAll(buf);
         }

         return nCount;
      }
   }
   
   /**
    * Buffer for generating p-code. 
    */
   protected final static class PCodeBuffer
   {
      // attributes

      /**
       * The source code URL index in the URL array.
       */
      private int m_nURLIndex = -1;

      /**
       * The number of elements in the text position array.
       */
      private int m_nTextPosCount;

      /**
       * The text position array: Offset[3*n], Line[3*n+1], Column[3*n+2]
       */
      private char[] m_textPosArray;

      // associations

      /**
       * Code buffer.
       */
      private StringBuilder m_buf = new StringBuilder(128);

      /**
       * Map of constant objects to their constant table offsets: Integer[Object].
       */
      private Lookup m_constMap = new HashTab();
      
      /**
       * Map of URLs to array indexes: Integer[String].
       */
      private Lookup m_urlMap;

      /**
       * The function instruction.
       */
      private GetFunctionInstruction m_func;

      /**
       * The compiler object.
       */
      private Compiler m_compiler;

      // constructors

      /**
       * Constructs the code buffer.
       * @param compiler The compiler object.
       */
      public PCodeBuffer(Compiler compiler)
      {
         m_compiler = compiler;
         
         if (compiler.m_urlHolder instanceof String)
         {
            m_urlMap = new HashTab(1);
            m_urlMap.put(compiler.m_urlHolder, Primitive.ZERO_INTEGER);
            m_nURLIndex = 0;
         }
      }

      // operations

      /**
       * @return The compiler object.
       */
      public Compiler getCompiler()
      {
         return m_compiler;
      }

      /**
       * @return The current code offset.
       */
      public int getOffset()
      {
         return m_buf.length();
      }

      /**
       * Adds a single code instruction (or offset).
       * @param nPCode The instruction p-code.
       */
      public void addCode(int nPCode)
      {
         m_buf.append((char)nPCode);
      }

      /**
       * Adds a code instruction with one argument.
       * @param nPCode The instruction p-code.
       * @param nArg The instruction argument.
       */
      public void addCode(int nPCode, int nArg)
      {
         addCode(nPCode);
         addCode(nArg);
      }

      /**
       * Adds a code instruction with two arguments.
       * @param nPCode The instruction p-code.
       * @param nArg1 The first instruction argument.
       * @param nArg2 The second instruction argument.
       */
      public void addCode(int nPCode, int nArg1, int nArg2)
      {
         addCode(nPCode);
         addCode(nArg1);
         addCode(nArg2);
      }
      
      /**
       * Sets a code instruction at a specified offset.
       * @param nOffset The offset at which to set the instruction.
       * @param nPCode The instruction p-code.
       */
      public void setCode(int nOffset, int nPCode)
      {
         m_buf.setCharAt(nOffset, (char)nPCode);
      }
      
      /**
       * Adds a constant to the buffer.
       * @param value The constant value to add.
       * @return The offset of the added constant.
       */
      public int addConstant(Object value)
      {
         Object obj = m_constMap.get(value);

         if (obj != null)
         {
            return ((Integer)obj).intValue();
         }

         int nOffset = m_constMap.size();

         m_constMap.put(value, Primitive.createInteger(nOffset));

         return nOffset;
      }

      /**
       * Adds a text position for the current code offset.
       * @param pos The text position to add.
       */
      public void addTextPosition(TextPosition pos)
      {
         if (pos != null)
         {
            Object sURL = pos.getURL();

            if (sURL == null && (m_compiler.m_urlHolder instanceof Lookup))
            {
               sURL = ((Lookup)m_compiler.m_urlHolder).get(pos);
            }

            if (sURL != null)
            {
               if (m_urlMap == null)
               {
                  m_urlMap = new HashTab();
               }

               Object index = m_urlMap.get(sURL);

               if (index == null)
               {
                  index = Primitive.createInteger(m_urlMap.size());
                  m_urlMap.put(sURL, index);
               }

               m_nURLIndex = ((Integer)index).intValue();
            }

            if (m_nURLIndex >= 0)
            {
               char nOffset = (char)m_buf.length();
               char nLine = (char)pos.getLine();
               char nCol = (char)pos.getColumn();

               if (m_nTextPosCount >= DebugInfo.POS_SIZE)
               {
                  char nLastOffset = m_textPosArray[m_nTextPosCount - DebugInfo.POS_SIZE + DebugInfo.POS_OFFSET];
                  char nLastLine = m_textPosArray[m_nTextPosCount - DebugInfo.POS_SIZE + DebugInfo.POS_LINE];
                  char nLastCol = m_textPosArray[m_nTextPosCount - DebugInfo.POS_SIZE + DebugInfo.POS_COLUMN];
                  char nLastURL = m_textPosArray[m_nTextPosCount - DebugInfo.POS_SIZE + DebugInfo.POS_URL];

                  if(nLastOffset == nOffset)
                  {
                     // Let URLs added later at the same offset take precedence 
                     m_nTextPosCount -= DebugInfo.POS_SIZE;
                  }
                  else if(nLine == nLastLine && nCol == nLastCol && m_nURLIndex == nLastURL)
                  {
                     // Don't add redundant position items
                     return;
                  }
               }

               if (m_textPosArray == null)
               {
                  m_textPosArray = new char[DebugInfo.POS_SIZE * 64];
               }
               else if (m_nTextPosCount == m_textPosArray.length)
               {
                  char[] textPosArray = new char[m_nTextPosCount << 1];

                  System.arraycopy(m_textPosArray, 0, textPosArray, 0, m_nTextPosCount);
                  m_textPosArray = textPosArray;
               }

               m_textPosArray[m_nTextPosCount++] = nOffset;
               m_textPosArray[m_nTextPosCount++] = nLine;
               m_textPosArray[m_nTextPosCount++] = nCol;
               m_textPosArray[m_nTextPosCount++] = (char)m_nURLIndex;
            }
         }
      }

      /**
       * Sets the function instruction.
       * @param func The GetFunctionInstruction instance. Can be null.
       */
      public void setFunction(GetFunctionInstruction func)
      {
         m_func = func;
      }

      /**
       * Copies the generated p-code and constants to a given function.
       * @param fun The function where to copy the code.
       */
      public void copyTo(PCodeFunction fun)
      {
         if (m_buf.length() > Character.MAX_VALUE)
         {
            throw new CompilerException("err.compiler.codeOverflow", null, null, null);
         }

         fun.code = new char[m_buf.length()];
         m_buf.getChars(0, m_buf.length(), fun.code, 0);

         boolean bFuncInfo = (m_func != null && (m_func.getName() != null ||
            m_func.getArgCount() != 0 && m_func.getFrame().isUsed()));
         boolean bDebugInfo = (bFuncInfo || m_urlMap != null && m_nTextPosCount > 0);
         int nConstCount = m_constMap.size() + ((bDebugInfo) ? PCodeFunction.DEBUG_CONSTANT_COUNT : 0);

         if (nConstCount == 0)
         {
            fun.constants = null;
         }
         else
         {
            fun.constants = new Object[nConstCount];

            for (Lookup.Iterator itr = m_constMap.iterator(); itr.hasNext();)
            {
               itr.next();
               fun.constants[((Integer)itr.getValue()).intValue()] = itr.getKey();
            }

            if (bDebugInfo)
            {
               char[] textPosArray = new char[m_nTextPosCount];

               if (m_textPosArray != null)
               {
                  System.arraycopy(m_textPosArray, 0, textPosArray, 0, m_nTextPosCount);
               }

               int nURLCount = 0;

               if (m_urlMap != null)
               {
                  nURLCount += m_urlMap.size();
               }

               if (bFuncInfo)
               {
                  ++nURLCount;

                  if (m_func.getFrame().isUsed())
                  {
                     nURLCount +=  m_func.getArgCount();
                  }
               }

               String[] urlArray = new String[nURLCount];

               if (m_urlMap != null)
               {
                  for (Lookup.Iterator itr = m_urlMap.iterator(); itr.hasNext();)
                  {
                     String sURL = (String)itr.next();
                     urlArray[((Integer)itr.getValue()).intValue()] = sURL;
                  }
               }

               if (bFuncInfo)
               {
                  urlArray[--nURLCount] = m_func.getName();

                  if (m_func.getFrame().isUsed())
                  {
                     for (Local var = m_func.getFrame().getLastVar(); var != null; var = var.getNext())
                     {
                        urlArray[--nURLCount] = var.getSymbol().getName();
                     }
                  }
               }

               fun.constants[nConstCount - 2] = textPosArray;
               fun.constants[nConstCount - 1] = urlArray;
            }
         }
      }
   }

   /**
    * The local syntax transformer function Decorator implementation.
    */
   protected static class LocalSyntaxTransformer implements SyntaxFunction
   {
      // associations

      /**
       * The frame in which this function is defined.
       */
      protected final Frame m_frame;

      /**
       * The syntax transformer function.
       */
      protected final SyntaxFunction m_transformer;

      // constructors

      /**
       * Creates a new local transformer function.
       * @param transformer The syntax transformer function.
       * @param frame The frame in which this function is defined.
       * @param bLet Flag indicating a let-syntax transformer.
       */
      public LocalSyntaxTransformer(SyntaxFunction transformer, Frame frame, boolean bLet)
      {
         assert frame != null;

         m_transformer = transformer;
         m_frame = (bLet) ? frame.getParent() : frame;
      }

      // operations

      /**
       * @return The frame in which this function is defined.
       */
      protected Frame getFrame()
      {
         return m_frame;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxFunction#isVariable()
       */
      public boolean isVariable()
      {
         return m_transformer.isVariable();
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxFunction#getSymbol()
       */
      public Symbol getSymbol()
      {
         return m_transformer.getSymbol();
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxFunction#setSymbol(nexj.core.scripting.Symbol)
       */
      public void setSymbol(Symbol symbol)
      {
         m_transformer.setSymbol(symbol);
      }

      /**
       * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
       */
      public boolean invoke(int nArgCount, Machine machine)
      {
         return m_transformer.invoke(nArgCount, machine);
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxFunction#isContained(java.lang.Object)
       */
      public boolean isContained(Object frame)
      {
         assert frame != null;

         Frame fr = m_frame;

         do
         {
            if (fr == frame)
            {
               return true;
            }

            fr = fr.getParent();
         }
         while (fr != null);

         return false;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return m_transformer.toString();
      }
   }
}
