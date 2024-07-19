// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.io.Reader;
import java.io.Writer;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import nexj.core.meta.Primitive;
import nexj.core.meta.TypeMismatchException;
import nexj.core.runtime.Context;
import nexj.core.scripting.Intrinsic.Continuation;
import nexj.core.scripting.PCodeFunction.DebugInfo;
import nexj.core.scripting.syntax.SyntaxFunction;
import nexj.core.scripting.syntax.SyntaxTransformerContext;
import nexj.core.util.HashHolder;
import nexj.core.util.ObjUtil;
import nexj.core.util.StackTrace;
import nexj.core.util.SysUtil;
import nexj.core.util.UncheckedException;
import nexj.core.util.Undefined;

/**
 * The scripting engine virtual machine.
 */
public class Machine
{
   // constants
   /**
    * VM p-codes.
    */
   public final static char PUSH_LOCAL = 0; // frameOffset, varOffset
   public final static char PUSH_LOCAL_0 = 1; // varOffset
   public final static char PUSH_LOCAL_1 = 2; // varOffset
   public final static char PUSH_LOCAL_2 = 3; // varOffset
   public final static char PUSH_LOCAL_3 = 4; // varOffset
   public final static char PUSH_LOCAL_4 = 5; // varOffset
   public final static char SET_LOCAL = 6; // frameOffset, varOffset
   public final static char SET_LOCAL_0 = 7; // varOffset
   public final static char SET_LOCAL_1 = 8; // varOffset
   public final static char SET_LOCAL_2 = 9; // varOffset
   public final static char SET_LOCAL_3 = 10; // varOffset
   public final static char SET_LOCAL_4 = 11; // varOffset
   public final static char PUSH_GLOBAL = 12; // constOffset
   public final static char SET_GLOBAL = 13; // constOffset
   public final static char DEF_GLOBAL = 14; // constOffset
   public final static char PUSH_CONST = 15; // constOffset
   public final static char PUSH_ZERO = 16;
   public final static char PUSH_ONE = 17;
   public final static char PUSH_NULL = 18;
   public final static char PUSH_TRUE = 19;
   public final static char PUSH_FALSE = 20;
   public final static char PUSH_CLOSURE = 21; // constOffset
   public final static char PUSH_PC = 22; // codeOffset
   public final static char RETURN = 23;
   public final static char JUMP_TRUE = 24; // codeOffset
   public final static char JUMP_FALSE = 25; // codeOffset
   public final static char JUMP = 26; // codeOffset
   public final static char CALL = 27; // argCount
   public final static char CHECK_FRAME = 28; // argCount
   public final static char SETUP_FRAME = 29; // argCount
   public final static char CHECK_VARARG_FRAME = 30; // argCount
   public final static char SETUP_VARARG_FRAME = 31; // argCount
   public final static char POP = 32;
   public final static char EQ_P = 33;
   public final static char EQ = 34;
   public final static char NE = 35;
   public final static char GT = 36;
   public final static char GE = 37;
   public final static char LT = 38;
   public final static char LE = 39;
   public final static char LIKE = 40;
   public final static char ADD = 41;
   public final static char SUB = 42;
   public final static char MUL = 43;
   public final static char DIV = 44;
   public final static char CAR = 45;
   public final static char CDR = 46;
   public final static char CONS = 47;
   public final static char LIST_1 = 48;
   public final static char LIST_2 = 49;
   public final static char LIST_3 = 50;
   public final static char LIST_4 = 51;
   public final static char DEBUG = 52;
   public final static char CALL_INTRINSIC = 53; // argCount

   // system functions

   public final static PCodeFunction EMPTY_FUNCTION = new PCodeFunction(
      new char[]{RETURN});

   protected final static PCodeFunction TRY_FUNCTION = new PCodeFunction(
      new char[]
      {  // (lambda (expr) ...)
         SETUP_FRAME, 1,
         PUSH_PC, 8,
         PUSH_LOCAL_0, 1,
         CALL, 0, // (expr)
         PUSH_PC, 12,
         CALL_INTRINSIC + 0, 0, // (sys:finalize)
         POP,
         RETURN,
      });

   protected final static PCodeFunction CATCH_FUNCTION = new PCodeFunction(
      new char[]
      {  // (lambda (handler e) ...)
         SETUP_FRAME, 2,
         PUSH_PC, 10,
         PUSH_LOCAL_0, 2,
         PUSH_LOCAL_0, 1,
         CALL, 1, // (handler e)
         PUSH_PC, 14,
         CALL_INTRINSIC + 0, 0, // (sys:finalize)
         POP,
         RETURN,
      });

   protected final static PCodeFunction RETHROW_FUNCTION = new PCodeFunction(
      new char[]
      {  // (lambda (e) (sys:finalize) (throw e))
         SETUP_FRAME, 1,
         PUSH_PC, 6,
         CALL_INTRINSIC + 0, 0, // (sys:finalize)
         POP,
         PUSH_LOCAL_0, 1,
         CALL_INTRINSIC + 1, 1, // (throw e)
         RETURN,
      });

   protected final static PCodeFunction CALL1_FUNCTION = new PCodeFunction(
      new char[]
      {
         // 1 skipped marker instruction that cannot be generated by the compiler
         RETURN,
         // (lambda (consumer value) (apply consumer value '()))
         PUSH_NULL,
         CALL_INTRINSIC + 2, 3, // (apply consumer value '())
      });

   protected final static PCodeFunction CALL_FUNCTION = new PCodeFunction(
      new char[]
      {
         CALL, 0,
      });

   protected final static PCodeFunction POP_CALL_FUNCTION = new PCodeFunction(
      new char[]
      {
         POP,
         CALL, 0,
      });

   protected final static PCodeFunction VALUES_FUNCTION = new PCodeFunction(
      new char[]
      {
         CALL_INTRINSIC + 3, 1, // (sys:return-values nArgCount)
         RETURN,
      });

   /**
    * Minimum stack length.
    */
   protected final static int MIN_STACK_LENGTH = 64;

   /**
    * Minimum exception handler count.
    */
   protected final static int MIN_HANDLER_COUNT = 16;

   /**
    * The debugger singleton instance
    */
   private final static Debugger s_debugger;

   /**
    * If there is or has been a breakpoint installed then this is true. This may
    * only be set to false if it is guaranteed that there is not a DEBUG
    * instruction in any bytecode.
    */
   public static boolean debuggerEnabled;

   static
   {
      Debugger debugger;

      try
      {
         String sDebuggerImpl = SysUtil.getConfigProperties().getProperty("debugger.class",
            "nexj.core.scripting.debugger.Debugger");

         debugger = (Debugger)Class.forName(sDebuggerImpl).newInstance();
      }
      catch (ClassNotFoundException e)
      {
         debugger = new Debugger()
         {
            public void setAnonymousParent(PCodeFunction pCodeFunction, PCodeFunction parent)
            {
            }

            public void hitException(MachineState machineState, Set steppingBreakpointSet)
            {
            }

            public BreakpointSite hitBreakpoint(MachineState state)
            {
               return null;
            }

            public BreakpointSite findInstallationSite(char[] code, int nOffset)
            {
               return null;
            }
         };
      }
      catch (Throwable t)
      {
         throw ObjUtil.rethrow(t);
      }

      s_debugger = debugger;
   }

   // attributes

   /**
    * The argument count passed during the last function call.
    */
   protected int m_nArgCount;

   /**
    * The stack top offset.
    */
   protected int m_nTop;

   /**
    * The stack return offset.
    */
   protected int m_nReturn;

   /**
    * The stack length.
    */
   protected int m_nStackLength = MIN_STACK_LENGTH;

   /**
    * The stack continuation barrier offset.
    */
   protected int m_nStackBarrier;

   /**
    * The VM stack. Grows upwards.
    */
   protected Object[] m_stack = new Object[m_nStackLength];

   /**
    * The exception stack top offset.
    */
   protected int m_nExceptionTop;

   /**
    * The exception stack return offset.
    */
   protected int m_nExceptionReturn;

   /**
    * The exception stack continuation barrier.
    */
   protected int m_nExceptionBarrier;

   /**
    * The exception handler stack. Grows upwards.
    */
   protected Object[] m_exceptionHandlerStack = new Object[MIN_HANDLER_COUNT];

   // associations

   /**
    * The current p-code function.
    */
   protected PCodeFunction m_fun;

   /**
    * The global environment.
    */
   protected GlobalEnvironment m_globalEnv;

   /**
    * The invocation context.
    */
   protected Context m_context;

   /**
    * The default character stream reader.
    */
   private Reader m_reader;

   /**
    * The default character stream writer.
    */
   private Writer m_writer;

   /**
    * When single stepping and we encounter an exception we need to invalidate
    * the breakpoints. This is used to store those breakpoints.
    */
   private Set m_steppingBreakpointSet;

   /**
    * The current transformer context.
    */
   protected SyntaxTransformerContext m_transformerContext;

   /**
    * StackTraceElement fields.
    */
   protected static Field s_declaringClassField;
   protected static Field s_methodNameField;
   protected static Field s_fileNameField;
   protected static Field s_lineNumberField;

   static
   {
      try
      {
         Class clazz = StackTraceElement.class;

         s_declaringClassField = clazz.getDeclaredField("declaringClass");
         s_declaringClassField.setAccessible(true);
         s_methodNameField = clazz.getDeclaredField("methodName");
         s_methodNameField.setAccessible(true);
         s_fileNameField = clazz.getDeclaredField("fileName");
         s_fileNameField.setAccessible(true);
         s_lineNumberField = clazz.getDeclaredField("lineNumber");
         s_lineNumberField.setAccessible(true);
      }
      catch (Exception e)
      {
         s_declaringClassField = null;
         s_methodNameField = null;
         s_fileNameField = null;
         s_lineNumberField = null;
      }
   }

   // constructors

   /**
    * Creates a virtual machine with given
    * global environment and invocation context.
    * @param globalEnv The global environment.
    * @param context The runtime context.
    */
   public Machine(GlobalEnvironment globalEnv, Context context)
   {
      m_globalEnv = globalEnv;
      m_context = context;
   }

   /**
    * Creates a virtual machine with the same global environment,
    * invocation context and i/o ports as a given machine.
    * @param machine The machine from which to take the environment.
    */
   public Machine(Machine machine)
   {
      this(machine.m_globalEnv, machine);
   }

   /**
    * Creates a virtual machine with a given global environment and
    * with invocation context and i/o ports from another machine.
    * @param globalEnv The global environment.
    * @param machine The machine from which to take the context.
    */
   public Machine(GlobalEnvironment globalEnv, Machine machine)
   {
      m_globalEnv = globalEnv;
      m_context = machine.m_context;
      m_reader = machine.m_reader;
      m_writer = machine.m_writer;
   }

   // operations

   /**
    * @return The global environment.
    */
   public GlobalEnvironment getGlobalEnvironment()
   {
      return m_globalEnv;
   }

   /**
    * @return The runtime context.
    */
   public Context getContext()
   {
      return m_context;
   }

   /**
    * @return The current syntax transformer expansion context.
    */
   public SyntaxTransformerContext getTransformerContext()
   {
      if (m_transformerContext == null)
      {
         m_transformerContext = new SyntaxTransformerContext(this);
      }

      return m_transformerContext;
   }

   /**
    * @return The number of transformer expansion and compilation in progress.
    */
   public int getExpansionCount()
   {
      return (m_transformerContext == null) ? 0 : m_transformerContext.getExpansionCount();
   }

   /**
    * Returns the symbol corresponding to the given symbol that can be used for global
    * definition without affecting hygiene. A new symbol is generated if necessary;
    * otherwise, the argument itself is returned.
    * @param symbol The original symbol.
    * @return The symbol that can be used for global definition.
    */
   public Symbol generateGlobalVarSymbol(Symbol symbol)
   {
      return (m_transformerContext == null) ? symbol : m_transformerContext.generateGlobalVarName(symbol);
   }

   /**
    * Returns the symbol corresponding to the given symbol that can be used at the
    * global scope without affecting hygiene, if such a symbol was previously generated.
    * Otherwise, returns the argument itself.
    * @param symbol The original symbol.
    * @return The symbol that can be used for global definition.
    */
   public Symbol getGlobalVarSymbol(Symbol symbol)
   {
      return (m_transformerContext == null) ? symbol : m_transformerContext.getGlobalVarName(symbol);
   }

   /**
    * Sets the default character stream reader.
    * @param reader The default character stream reader to set.
    */
   public void setReader(Reader reader)
   {
      m_reader = reader;
   }

   /**
    * @return The default character stream reader.
    */
   public Reader getReader()
   {
      return m_reader;
   }

   /**
    * Sets the default character stream writer.
    * @param writer The default character stream writer to set.
    */
   public void setWriter(Writer writer)
   {
      m_writer = writer;
   }

   /**
    * @return The default character stream writer.
    */
   public Writer getWriter()
   {
      return m_writer;
   }

   /**
    * Invokes a function using the VM.
    * @param fun The function to invoke.
    * @param args The function argument values.
    * @return The function return value.
    */
   public Object invoke(Function fun, Object[] args)
   {
      int nReturnSaved = m_nReturn;
      int nExceptionReturnSaved = m_nExceptionReturn;

      try
      {
         m_nReturn = m_nTop;
         m_nExceptionReturn = m_nExceptionTop;
         m_fun = null;

         if (args != null)
         {
            int nCount = args.length;

            for (int i = 0; i < nCount; ++i)
            {
               push(args[i]);
            }
         }

         if (fun.invoke(m_nTop - m_nReturn, this))
         {
            run();
         }

         return pop();
      }
      finally
      {
         m_nTop = m_nReturn;
         m_nReturn = nReturnSaved;
         m_nExceptionTop = m_nExceptionReturn;
         m_nExceptionReturn = nExceptionReturnSaved;
      }
   }

   /**
    * Invokes a function using the VM.
    * @param fun The function to invoke.
    * @param obj The first argument.
    * @param args The function argument values.
    * @return The function return value.
    */
   public Object invoke(Function fun, Object obj, Object[] args)
   {
      int nReturnSaved = m_nReturn;
      int nExceptionReturnSaved = m_nExceptionReturn;

      try
      {
         m_nReturn = m_nTop;
         m_nExceptionReturn = m_nExceptionTop;
         m_fun = null;

         push(obj);

         if (args != null)
         {
            int nCount = args.length;

            for (int i = 0; i < nCount; ++i)
            {
               push(args[i]);
            }
         }

         if (fun.invoke(m_nTop - m_nReturn, this))
         {
            run();
         }

         return pop();
      }
      finally
      {
         m_nTop = m_nReturn;
         m_nReturn = nReturnSaved;
         m_nExceptionTop = m_nExceptionReturn;
         m_nExceptionReturn = nExceptionReturnSaved;
      }
   }

   /**
    * Invokes a function using the VM.
    * @param fun The function to invoke.
    * @param arg1 The first argument.
    * @param arg2 The second argument.
    * @param args The remaining function argument values.
    * @return The function return value.
    */
   public Object invoke(Function fun, Object arg1, Object arg2, Object[] args)
   {
      int nReturnSaved = m_nReturn;
      int nExceptionReturnSaved = m_nExceptionReturn;

      try
      {
         m_nReturn = m_nTop;
         m_nExceptionReturn = m_nExceptionTop;
         m_fun = null;

         push(arg1);
         push(arg2);

         if (args != null)
         {
            int nCount = args.length;

            for (int i = 0; i < nCount; ++i)
            {
               push(args[i]);
            }
         }

         if (fun.invoke(m_nTop - m_nReturn, this))
         {
            run();
         }

         return pop();
      }
      finally
      {
         m_nTop = m_nReturn;
         m_nReturn = nReturnSaved;
         m_nExceptionTop = m_nExceptionReturn;
         m_nExceptionReturn = nExceptionReturnSaved;
      }
   }

   /**
    * Invokes a function using the VM.
    * @param fun The function to invoke.
    * @param arg1 The first argument.
    * @param arg2 The second argument.
    * @param args The remaining function argument values.
    * @param consumer The function to invoke with the multiple return values of fun as arguments.
    * @return The value returned by the consumer function.
    */
   public Object invokeValuesConsumer(Function fun, Object arg1, Object arg2, Object[] args, Function consumer)
   {
      pushValuesConsumer(consumer);
      push(arg1);
      push(arg2);

      int nCount = (args == null) ? 0 : args.length;

      for (int i = 0; i < nCount; ++i)
      {
         push(args[i]);
      }

      apply(fun, 2 + nCount);
      run();

      return pop();
   }

   /**
    * Invokes a function using the VM.
    * @param fun The function to invoke.
    * @param argList The function argument value list.
    * @return The function return value.
    */
   public Object invoke(Function fun, List argList)
   {
      int nReturnSaved = m_nReturn;
      int nExceptionReturnSaved = m_nExceptionReturn;

      try
      {
         m_nReturn = m_nTop;
         m_nExceptionReturn = m_nExceptionTop;
         m_fun = null;

         if (argList != null)
         {
            int nCount = argList.size();

            for (int i = 0; i < nCount; ++i)
            {
               push(argList.get(i));
            }
         }

         if (fun.invoke(m_nTop - m_nReturn, this))
         {
            run();
         }

         return pop();
      }
      finally
      {
         m_nTop = m_nReturn;
         m_nReturn = nReturnSaved;
         m_nExceptionTop = m_nExceptionReturn;
         m_nExceptionReturn = nExceptionReturnSaved;
      }
   }

   /**
    * Invokes a function using the VM.
    * @param fun The function to invoke.
    * @param args The function argument values.
    * @return The function return value.
    */
   public Object invoke(Function fun, Pair args)
   {
      int nReturnSaved = m_nReturn;
      int nExceptionReturnSaved = m_nExceptionReturn;

      try
      {
         m_nReturn = m_nTop;
         m_nExceptionReturn = m_nExceptionTop;
         m_fun = null;

         if (args != null)
         {
            for (;;)
            {
               push(args.getHead());

               if (!(args.getTail() instanceof Pair))
               {
                  if (args.getTail() == null)
                  {
                     break;
                  }

                  throw new ScriptingException("err.scripting.callArgs");
               }

               args = args.getNext();
            }
         }

         if (fun.invoke(m_nTop - m_nReturn, this))
         {
            run();
         }

         return pop();
      }
      finally
      {
         m_nTop = m_nReturn;
         m_nReturn = nReturnSaved;
         m_nExceptionTop = m_nExceptionReturn;
         m_nExceptionReturn = nExceptionReturnSaved;
      }
   }

   /**
    * Invokes a function using the VM.
    * @param fun The function to invoke.
    * @param obj The first argument.
    * @param args The function argument values.
    * @return The function return value.
    */
   public Object invoke(Function fun, Object obj, Pair args)
   {
      int nReturnSaved = m_nReturn;
      int nExceptionReturnSaved = m_nExceptionReturn;

      try
      {
         m_nReturn = m_nTop;
         m_nExceptionReturn = m_nExceptionTop;
         m_fun = null;

         push(obj);

         if (args != null)
         {
            for (;;)
            {
               push(args.getHead());

               if (!(args.getTail() instanceof Pair))
               {
                  if (args.getTail() == null)
                  {
                     break;
                  }

                  throw new ScriptingException("err.scripting.callArgs");
               }

               args = args.getNext();
            }
         }

         if (fun.invoke(m_nTop - m_nReturn, this))
         {
            run();
         }

         return pop();
      }
      finally
      {
         m_nTop = m_nReturn;
         m_nReturn = nReturnSaved;
         m_nExceptionTop = m_nExceptionReturn;
         m_nExceptionReturn = nExceptionReturnSaved;
      }
   }

   /**
    * Applies a function.
    * @param fun The function to apply.
    * @param nArgCount The argument count.
    */
   public void apply(Function fun, int nArgCount)
   {
      if (!fun.invoke(nArgCount, this))
      {
         setFunction(EMPTY_FUNCTION, 0);
      }
   }

   /**
    * Invokes a Java method using the arguments on the stack.
    * @param obj The java object, on which to invoke the method.
    * @param nArgCount The number of arguments on the top of the stack.
    * @return The Function.invoke() return value.
    * @throws ScriptingException if a method invocation error occurs.
    */
   public boolean invokeJavaMethod(Object obj, int nArgCount) throws ScriptingException
   {
      if (nArgCount != 0)
      {
         Object sym = m_stack[m_nTop - nArgCount];

         if (sym instanceof Symbol)
         {
            if (obj == null)
            {
               returnValue(null, nArgCount);

               return false;
            }

            Class clazz = (obj instanceof Class) ? (Class)obj : obj.getClass();

            m_stack[m_nTop - nArgCount] = obj;

            return m_globalEnv.getJavaMethod(clazz, (Symbol)sym).invoke(nArgCount, this);
         }

         if (sym instanceof Number)
         {
            int i = ((Number)sym).intValue();

            if (obj != null)
            {
               try
               {
                  if (obj.getClass().isArray())
                  {
                     if (nArgCount == 1)
                     {
                        if (obj instanceof byte[])
                        {
                           returnValue(Primitive.createInteger(((byte[])obj)[i] & 0xFF), nArgCount);
                        }
                        else
                        {
                           returnValue(Array.get(obj, i), nArgCount);
                        }
                     }
                     else if (nArgCount == 2)
                     {
                        Object value = getArg(1, nArgCount);

                        if (obj instanceof byte[])
                        {
                           if (!(value instanceof Number))
                           {
                              throw new TypeMismatchException(Symbol.BYTEVECTOR_U8_SET);
                           }

                           byte nValue = ((Number)value).byteValue();

                           ((byte[])obj)[i] = nValue;
                           returnValue(Primitive.createInteger(nValue & 0xFF), nArgCount);
                        }
                        else
                        {
                           Array.set(obj, i, value);
                           returnValue(value, nArgCount);
                        }
                     }
                     else
                     {
                        throw new ScriptingException("err.scripting.maxArgCount",
                           new Object[]{"#<vector-index>", Primitive.createInteger(2),
                              Primitive.createInteger(nArgCount)});
                     }
                     
                     return false;
                  }
                  else if (obj instanceof List)
                  {
                     if (nArgCount == 1)
                     {
                        returnValue(((List)obj).get(i), nArgCount);
                     }
                     else if (nArgCount == 2)
                     {
                        Object value = getArg(1, nArgCount);

                        ((List)obj).set(i, value);
                        returnValue(value, nArgCount);
                     }
                     else
                     {
                        throw new ScriptingException("err.scripting.maxArgCount",
                           new Object[]{"#<vector-index>", Primitive.createInteger(2),
                              Primitive.createInteger(nArgCount)});
                     }

                     return false;
                  }
               }
               catch (IndexOutOfBoundsException e)
               {
                  throw new ScriptingException("err.scripting.badIndex", new Object[]{sym}, e);
               }
            }
         }
      }

      throw new ScriptingException("err.scripting.funCall");
   }

   /**
    * Determines if the stack is empty.
    * @param nArgCount The number of arguments currently pushed on the stack.
    * @return True if the stack is empty.
    */
   public boolean isStackEmpty(int nArgCount)
   {
      return m_nTop <= nArgCount;
   }

   /**
    * Determines if a given S-expression is supported by eval().
    * @param obj The S-expression.
    * @return True if the S-expression is supported.
    */
   public boolean isEvalSupported(Object obj)
   {
      if (obj instanceof Pair)
      {
         Pair pair = (Pair)obj;

         return isEvalSupported(pair.getHead(), pair.getNext());
      }

      if (obj instanceof Symbol)
      {
         return m_globalEnv.isDefined((Symbol)obj);
      }

      return true;
   }

   /**
    * Determines if a given function or special form is supported by eval().
    * @param obj The object to invoke.
    * @param args The function argument S-expressions.
    * @return True if the S-expression is supported.
    */
   protected boolean isEvalSupported(Object obj, Pair args)
   {
      if (obj instanceof Symbol)
      {
         if (Symbol.IF.equals(obj))
         {
            if (!isEvalSupported(args.getHead()))
            {
               return false;
            }

            args = args.getNext();

            if (args == null)
            {
               return false;
            }

            if (Boolean.FALSE.equals(obj))
            {
               args = args.getNext();

               if (args == null)
               {
                  return true;
               }
            }

            return isEvalSupported(args.getHead());
         }

         if (Symbol.QUOTE.equals(obj))
         {
            return true;
         }

         if (Symbol.GLOBAL.equals(obj))
         {
            Symbol symbol = (Symbol)args.getHead();

            return m_globalEnv.isDefined(symbol);
         }

         Symbol sym = (Symbol)obj;

         obj = m_globalEnv.findVariable(sym, Undefined.VALUE);

         if (obj == Undefined.VALUE)
         {
            return false;
         }

         if (obj instanceof Macro)
         {
            return isEvalSupported(invoke((Function)obj, args));
         }

         if (obj instanceof SyntaxFunction)
         {
            return isEvalSupported(getTransformerContext().expandTransformer((SyntaxFunction)obj, new Pair(sym, args)));
         }
      }
      else if (obj instanceof Pair)
      {
         Pair pair = (Pair)obj;

         if (!isEvalSupported(pair.getHead(), pair.getNext()))
         {
            return false;
         }
      }
      else
      {
         return false;
      }

      for (; args != null; args = args.getNext())
      {
         if (!isEvalSupported(args.getHead()))
         {
            return false;
         }
      }

      return true;
   }

   /**
    * Evaluates an S-expression in interpreted mode.
    * Not all the functionality is supported.
    * This is different from the eval function,
    * where the expression is precompiled.
    * @param obj The S-expression.
    * @return The expression value.
    */
   public Object eval(Object obj)
   {
      if (obj instanceof Pair)
      {
         Pair pair = (Pair)obj;

         return eval(pair.getHead(), pair.getNext());
      }

      if (obj instanceof Symbol)
      {
         Object value = m_globalEnv.getVariable((Symbol)obj);

         if (value instanceof SyntaxFunction)
         {
            return eval(getTransformerContext().expandTransformer((SyntaxFunction)value, obj));
         }

         return value;
      }

      return obj;
   }

   /**
    * Evaluates a function or special form.
    * Not all the functionality is supported.
    * This is different from the eval function,
    * where the expression is precompiled.
    * @param obj The object to invoke.
    * @param args The function argument S-expressions.
    * @return The function return value.
    */
   public Object eval(Object obj, Pair args)
   {
      int nReturnSaved = m_nReturn;
      int nExceptionReturnSaved = m_nExceptionReturn;

      try
      {
         m_nReturn = m_nTop;
         m_nExceptionReturn = m_nExceptionTop;
         m_fun = null;

         if (obj instanceof Symbol)
         {
            if (Symbol.IF.equals(obj))
            {
               obj = eval(args.getHead());
               args = args.getNext();

               if (Boolean.FALSE.equals(obj))
               {
                  args = args.getNext();

                  if (args == null)
                  {
                     return null;
                  }
               }

               return eval(args.getHead());
            }

            if (Symbol.QUOTE.equals(obj))
            {
               return args.getHead();
            }

            if (Symbol.GLOBAL.equals(obj))
            {
               return m_globalEnv.getVariable((Symbol)args.getHead());
            }

            Symbol sym = (Symbol)obj;

            obj = m_globalEnv.getVariable((Symbol)obj);

            if (obj instanceof Macro)
            {
               return eval(invoke((Function)obj, args));
            }

            if (obj instanceof SyntaxFunction)
            {
               return eval(getTransformerContext().expandTransformer((SyntaxFunction)obj, new Pair(sym, args)));
            }
         }
         else if (obj instanceof Pair)
         {
            Pair pair = (Pair)obj;

            if (Symbol.GLOBAL.equals(pair.getHead()))
            {
               return eval(pair.getNext().getHead(), args);
            }

            obj = eval(pair.getHead(), pair.getNext());
         }
         else
         {
            obj = null;
         }

         for (; args != null; args = args.getNext())
         {
            push(eval(args.getHead()));
         }

         if (invoke(obj, m_nTop - m_nReturn))
         {
            run();
         }

         return pop();
      }
      finally
      {
         m_nTop = m_nReturn;
         m_nReturn = nReturnSaved;
         m_nExceptionTop = m_nExceptionReturn;
         m_nExceptionReturn = nExceptionReturnSaved;
      }
   }

   /**
    * Invokes a function or a Java method using the arguments on the stack.
    * @param obj The function of Java object.
    * @param nArgCount The number of arguments on the top of the stack.
    * @return The Function.invoke() return value.
    * @throws ScriptingException if a method invocation error occurs.
    */
   protected boolean invoke(Object obj, int nArgCount) throws ScriptingException
   {
      if (obj instanceof Function)
      {
         return ((Function)obj).invoke(nArgCount, this);
      }

      return invokeJavaMethod(obj, nArgCount);
   }

   /**
    * Applies a function.
    * @param fun The function to apply.
    * @param nArgCount The argument count.
    */
   public void apply(Object fun, int nArgCount)
   {
      if (!invoke(fun, nArgCount))
      {
         setFunction(EMPTY_FUNCTION, 0);
      }
   }

   /**
    * Sets the current p-code function.
    * @param fun The p-code function to execute.
    * @param nArgCount The argument count.
    */
   public void setFunction(PCodeFunction fun, int nArgCount)
   {
      m_fun = fun;
      m_nArgCount = nArgCount;
   }

   /**
    * Replaces an argument at a given position on the stack.
    * @param nOrdinal The argument ordinal number, 0-based.
    * @param nArgCount The total argument count.
    * @param value The value to set.
    */
   public void setArg(int nOrdinal, int nArgCount, Object value)
   {
      m_stack[m_nTop - nArgCount + nOrdinal] = value;
   }

   /**
    * Gets an argument from the stack by position.
    * @param nOrdinal The argument ordinal number, 0-based.
    * @param nArgCount The total argument count.
    * @return The argument value.
    */
   public Object getArg(int nOrdinal, int nArgCount)
   {
      return m_stack[m_nTop - nArgCount + nOrdinal];
   }

   /**
    * Gets the last several arguments from the stack.
    * @param args The array where to copy the arguments.
    */
   public void getArgs(Object[] args)
   {
      System.arraycopy(m_stack, m_nTop - args.length, args, 0, args.length);
   }

   /**
    * Shifts the arguments on the top of the stack by removing the first arguments.
    * @param nOffset the number of arguments to remove.
    * @param nArgCount The total number of arguments.
    */
   public void shiftArgs(int nOffset, int nArgCount)
   {
      System.arraycopy(m_stack, m_nTop - nArgCount + nOffset, m_stack, m_nTop - nArgCount, nArgCount - nOffset);
      m_nTop -= nOffset;
   }

   /**
    * Cleans up a stack frame (not a local environment frame) and pushes the
    * return value on top of the stack.
    * @param value The value to return to the caller.
    * @param nArgCount The number of arguments to clean up from the stack frame.
    */
   public void returnValue(Object value, int nArgCount)
   {
      m_nTop -= nArgCount;
      push(value);
   }

   /**
    * Pushes a value on the stack.
    * @param value The value to push, it will be at the stack top.
    */
   public void push(Object value)
   {
      if (m_nTop == m_nStackLength)
      {
         m_nStackLength <<= 1;

         Object[] stack = new Object[m_nStackLength];

         System.arraycopy(m_stack, 0, stack, 0, m_stack.length);
         m_stack = stack;
      }

      m_stack[m_nTop++] = value;
   }

   /**
    * Pops a value from the stack.
    * @return The value that has been on the top of the stack.
    */
   public Object pop()
   {
      return m_stack[--m_nTop];
   }

   /**
    * Pops several values from the stack.
    * @param nCount The number of values to pop.
    */
   public void pop(int nCount)
   {
      m_nTop -= nCount;
   }

   /**
    * Pops all the values from the stack and
    * all the exceptions from the exeption stack.
    */
   public void popAll()
   {
      m_nTop = m_nReturn;
      m_nExceptionTop = m_nExceptionReturn;
   }

   /**
    * Creates and pushes a function pair, either as an exception handler
    * or a dynamic-wind function pair.
    * @param handler The exception handling code or the dynamic-wind pre-function.
    * @param finalizer The finalizer code or the dynamic-wind post-function.
    * @param marker Marker indicating the type of the function pair.
    */
   public void pushExceptionHandler(PCodeFunction handler, PCodeFunction finalizer, Object marker)
   {
      if (m_nExceptionTop == m_exceptionHandlerStack.length)
      {
         Object[] stack = new Object[m_exceptionHandlerStack.length << 1];

         System.arraycopy(m_exceptionHandlerStack, 0, stack, 0, m_exceptionHandlerStack.length);
         m_exceptionHandlerStack = stack;
      }

      m_exceptionHandlerStack[m_nExceptionTop++] = Primitive.createInteger(m_nTop);
      m_exceptionHandlerStack[m_nExceptionTop++] = handler;
      m_exceptionHandlerStack[m_nExceptionTop++] = finalizer;
      m_exceptionHandlerStack[m_nExceptionTop++] = marker;
   }

   /**
    * Pops the top exception handler and returns its finalizer.
    * @return The top exception handler finalizer.
    */
   public PCodeFunction popExceptionHandler()
   {
      m_nExceptionTop -= 4;

      return (PCodeFunction)m_exceptionHandlerStack[m_nExceptionTop + 2];
   }

   /**
    * Sets up a try-catch block.
    * @param fun The function to run.
    * @param handler The exception handler.
    * @param finalizer The finalizer.
    * @param bDynamicWind Flag for dynamic-wind function pair.
    */
   public void setTry(PCodeFunction fun, PCodeFunction handler, PCodeFunction finalizer, boolean bDynamicWind)
   {
      pushExceptionHandler((PCodeFunction)handler, (PCodeFunction)finalizer,
         (bDynamicWind) ? null : Boolean.FALSE);
      push(fun);
      setFunction(TRY_FUNCTION, 1);
   }

   /**
    * Pushes a multiple value consumer and a call function used by returnValues() as a marker.
    * @param consumer The multiple value consumer.
    */
   public void pushValuesConsumer(Object consumer)
   {
      push(consumer);
      push(Primitive.ONE_INTEGER); // Skip the first instruction using return offset of 1
      push(CALL1_FUNCTION);
      push(null);
   }

   /**
    * Returns the current top values from the stack.
    * @param nCount The number of values to return from the top of the stack.
    * @return True if the VM registers should be reset.
    * @see #pushValuesConsumer(Object)
    */
   public boolean returnValues(int nCount)
   {
      int nArgCount = nCount + 4;

      if (m_nTop - m_nReturn >= nArgCount)
      {
         Object obj = getArg(2, nArgCount);

         if (obj instanceof PCodeFunction &&
            Primitive.ONE_INTEGER.equals(getArg(1, nArgCount)) &&
            Arrays.equals(((PCodeFunction)obj).code, CALL1_FUNCTION.code))
         {
            Object fun = getArg(0, nArgCount);

            shiftArgs(4, nArgCount);

            return invoke(fun, nCount);
         }
      }

      if (nCount == 0)
      {
         throw new ScriptingException("err.scripting.minRetCount",
            new Object[]{Primitive.ONE_INTEGER, Primitive.ZERO_INTEGER});
      }

      pop(nCount - 1);

      return false;
   }

   /**
    * Cleans up a stack frame (not a local environment frame) and pushes the
    * return values on top of the stack.
    * @param values The values to return to the caller.
    * @param nArgCount The number of arguments to clean up from the stack frame.
    * @return True if the VM registers should be reset.
    * @see #pushValuesConsumer(Object)
    */
   public boolean returnValues(Object[] values, int nArgCount)
   {
      m_nTop -= nArgCount;

      int nCount = values.length;

      for (int i = 0; i < nCount; ++i)
      {
         push(values[i]);
      }

      return returnValues(nCount);
   }

   /**
    * Cleans up a stack frame (not a local environment frame) and pushes the
    * return values on top of the stack.
    * @param value1 The first value to return to the caller.
    * @param value2 The second value to return to the caller.
    * @param values The remaining values to return to the caller.
    * @param nArgCount The number of arguments to clean up from the stack frame.
    * @return True if the VM registers should be reset.
    * @see #pushValuesConsumer(Object)
    */
   public boolean returnValues(Object value1, Object value2, Object[] values, int nArgCount)
   {
      m_nTop -= nArgCount;
      push(value1);
      push(value2);

      int nCount = 0;

      if (values != null)
      {
         nCount = values.length;

         for (int i = 0; i < nCount; ++i)
         {
            push(values[i]);
         }
      }

      return returnValues(nCount + 2);
   }

   /**
    * Creates a continuation barrier.
    * @return A cookie for restoring the old barrier.
    */
   public Object createBarrier()
   {
      Pair cookie = new Pair(Primitive.createInteger(m_nStackBarrier),
         Primitive.createInteger(m_nExceptionBarrier));

      m_nStackBarrier = m_nTop;
      m_nExceptionBarrier = m_nExceptionTop;

      return cookie;
   }

   /**
    * Restores a continuation barrier.
    * @param cookie The cookie of the barrier to be restored.
    */
   public void restoreBarrier(Object cookie)
   {
      Pair pair = (Pair)cookie;

      m_nStackBarrier = ((Number)pair.getHead()).intValue();
      m_nExceptionBarrier = ((Number)pair.getTail()).intValue();
   }

   /**
    * @return A copy of the current stack.
    */
   public Object[] cloneStack()
   {
      if (m_nReturn > m_nStackBarrier || m_nTop < m_nStackBarrier)
      {
         throw new ScriptingException("err.scripting.callcc");
      }

      int nStackCount = m_nTop - m_nStackBarrier;
      int nExceptionCount = m_nExceptionTop - m_nExceptionBarrier;
      Object[] stack = new Object[nStackCount + nExceptionCount + 1];

      System.arraycopy(m_stack, m_nStackBarrier, stack, 0, nStackCount);
      System.arraycopy(m_exceptionHandlerStack, m_nExceptionBarrier, stack, nStackCount, nExceptionCount);
      stack[nStackCount + nExceptionCount] = Primitive.createInteger(nExceptionCount);

      return stack;
   }

   /**
    * Sets the stack contents and the current p-code function, when switching
    * to a continuation.
    * @param stack The stack contents to set.
    * @param nArgCount The number of arguments at the top of the current stack
    * to be shifted to the top of the resulting stack.
    * @return True if the virtual machine registers should be reset.
    */
   public boolean setContinuation(Object[] stack, int nArgCount)
   {
      if (m_nReturn > m_nStackBarrier || m_nTop < m_nStackBarrier)
      {
         throw new ScriptingException("err.scripting.callcc");
      }

      int nLength = stack.length;
      int nExceptionCount = ((Integer)stack[nLength - 1]).intValue();
      int nStackCount = nLength - nExceptionCount - 1;
      int nEstimatedFuncCount = (m_nExceptionTop - m_nExceptionBarrier + nExceptionCount) >> 2;
      int nActualFuncCount = 0;
      int nIndex = m_nStackBarrier + nStackCount;
      int nMinStackSize = nIndex + nArgCount + 2 + nEstimatedFuncCount * 4;

      if (nMinStackSize > m_nStackLength)
      {
         m_nStackLength = nMinStackSize + 32;

         Object[] biggerStack = new Object[m_nStackLength];

         System.arraycopy(m_stack, 0, biggerStack, 0, m_nTop);
         m_stack = biggerStack;
      }

      nMinStackSize = m_nExceptionBarrier + nExceptionCount;

      if (nMinStackSize > m_exceptionHandlerStack.length)
      {
         Object[] biggerStack = new Object[nMinStackSize + 16];

         System.arraycopy(m_exceptionHandlerStack, 0, biggerStack, 0, m_nExceptionTop);
         m_exceptionHandlerStack = biggerStack;
      }

      System.arraycopy(m_stack, m_nTop - nArgCount, m_stack, nIndex, nArgCount);
      System.arraycopy(stack, 0, m_stack, m_nStackBarrier, nStackCount);

      nIndex += nArgCount;
      m_nTop = nIndex + 2;

      // Push the pre-functions in reversed order.
      for (int i = nLength - 2; i > nStackCount; i -= 4)
      {
         if (stack[i] == null) // dynamic-wind case
         {
            m_stack[m_nTop++] = Primitive.ZERO_INTEGER;
            m_stack[m_nTop++] = POP_CALL_FUNCTION;
            m_stack[m_nTop++] = null;
            m_stack[m_nTop++] = stack[i - 2];
            nActualFuncCount++;
         }
      }

      // Push the post-functions in order. 
      for (int i = m_nExceptionBarrier + 3; i < m_nExceptionTop; i += 4)
      {
         if (m_exceptionHandlerStack[i] == null) // dynamic-wind case
         {
            m_stack[m_nTop++] = Primitive.ZERO_INTEGER;
            m_stack[m_nTop++] = POP_CALL_FUNCTION;
            m_stack[m_nTop++] = null;
            m_stack[m_nTop++] = m_exceptionHandlerStack[i - 1];
            nActualFuncCount++;
         }
      }

      System.arraycopy(stack, nStackCount, m_exceptionHandlerStack, m_nExceptionBarrier, nExceptionCount);
      m_nExceptionTop = nMinStackSize;

      PCodeFunction func;

      if (nActualFuncCount == 0)
      {
         m_nTop -= 2;

         if (nArgCount != 1) 
         {
            return returnValues(nArgCount);
         }

         func = Machine.EMPTY_FUNCTION;
      }
      else
      {
         m_stack[nIndex++] = Primitive.createInteger(nArgCount);
         m_stack[nIndex++] = VALUES_FUNCTION;
         func = CALL_FUNCTION;
      }

      setFunction(func, nArgCount);

      return true;
   }

   /**
    * Cleans up redundant resources (e.g. by nullifying extra stack space).
    * Should be called when no VM activity is expected in the following ~100 ms.
    */
   public void cleanup()
   {
      if ((m_nStackLength >> 2) > m_nTop && m_nStackLength > (MIN_STACK_LENGTH << 1))
      {
         m_nStackLength = Math.max(m_nTop << 2, MIN_STACK_LENGTH);

         Object[] stack = new Object[m_nStackLength];

         System.arraycopy(m_stack, 0, stack, 0, m_nTop);
         m_stack = stack;
      }
      else
      {
         Arrays.fill(m_stack, m_nTop, m_nStackLength, null);
      }

      int nCount = m_exceptionHandlerStack.length;

      if ((nCount >> 2) > m_nExceptionTop && nCount > (MIN_HANDLER_COUNT << 1))
      {
         Object[] stack = new Object[Math.max(m_nExceptionTop << 2, MIN_HANDLER_COUNT)];

         System.arraycopy(m_exceptionHandlerStack, 0, stack, 0, m_nExceptionTop);
         m_exceptionHandlerStack = stack;
      }
      else
      {
         Arrays.fill(m_exceptionHandlerStack, m_nExceptionTop, nCount, null);
      }

      m_fun = null;
   }

   /**
    * Runs the VM on the current code.
    */
   protected void run()
   {
      int nPC = 0;
      PCodeFunction fun = m_fun;
      char[] code = fun.code;
      char[] altCode = null;
      char[] origCode = null;
      Object[] constants = fun.constants;
      Object[] frame = fun.frame;
      Object[] vars;
      Object value;
      Pair pair;
      int i, n;

      m_steppingBreakpointSet = null;

      for (;;)
      {
         try
         {
            for (;;)
            {
               switch (code[nPC++])
               {
                  case PUSH_LOCAL: // frameOffset, varOffset
                     vars = frame;

                     for (n = code[nPC++]; n != 0; --n)
                     {
                        vars = (Object[])vars[0];
                     }

                     push(vars[code[nPC++]]);

                     break;

                  case PUSH_LOCAL_0: // varOffset
                     push(frame[code[nPC++]]);
                     break;

                  case PUSH_LOCAL_1: // varOffset
                     push(((Object[])frame[0])[code[nPC++]]);
                     break;

                  case PUSH_LOCAL_2: // varOffset
                     push(((Object[])((Object[])frame[0])[0])[code[nPC++]]);
                     break;

                  case PUSH_LOCAL_3: // varOffset
                     push(((Object[])((Object[])((Object[])frame[0])[0])[0])[code[nPC++]]);
                     break;

                  case PUSH_LOCAL_4: // varOffset
                     push(((Object[])((Object[])((Object[])((Object[])frame[0])[0])[0])[0])[code[nPC++]]);
                     break;

                  case SET_LOCAL: // frameOffset, varOffset
                     vars = frame;

                     for (n = code[nPC++]; n != 0; --n)
                     {
                        vars = (Object[])vars[0];
                     }

                     vars[code[nPC++]] = m_stack[m_nTop - 1];

                     break;

                  case SET_LOCAL_0: // varOffset
                     frame[code[nPC++]] = m_stack[m_nTop - 1];
                     break;

                  case SET_LOCAL_1: // varOffset
                     ((Object[])frame[0])[code[nPC++]] = m_stack[m_nTop - 1];
                     break;

                  case SET_LOCAL_2: // varOffset
                     ((Object[])((Object[])frame[0])[0])[code[nPC++]] = m_stack[m_nTop - 1];
                     break;

                  case SET_LOCAL_3: // varOffset
                     ((Object[])((Object[])((Object[])frame[0])[0])[0])[code[nPC++]] = m_stack[m_nTop - 1];
                     break;

                  case SET_LOCAL_4: // varOffset
                     ((Object[])((Object[])((Object[])((Object[])frame[0])[0])[0])[0])[code[nPC++]] = m_stack[m_nTop - 1];
                     break;

                  case PUSH_GLOBAL: // constOffset
                     push(m_globalEnv.getVariable((Symbol)constants[code[nPC++]]));
                     break;

                  case SET_GLOBAL: // constOffset
                     m_globalEnv.setVariable((Symbol)constants[code[nPC++]], m_stack[m_nTop - 1]);
                     break;

                  case DEF_GLOBAL: // constOffset
                     m_globalEnv.defineVariable((Symbol)constants[code[nPC++]], m_stack[m_nTop - 1]);
                     break;

                  case PUSH_CONST: // constOffset
                     push(constants[code[nPC++]]);
                     break;

                  case PUSH_ZERO:
                     push(Primitive.ZERO_INTEGER);
                     break;

                  case PUSH_ONE:
                     push(Primitive.ONE_INTEGER);
                     break;

                  case PUSH_NULL:
                     push(null);
                     break;

                  case PUSH_TRUE:
                     push(Boolean.TRUE);
                     break;

                  case PUSH_FALSE:
                     push(Boolean.FALSE);
                     break;

                  case PUSH_CLOSURE: // constOffset
                     push(((PCodeFunction)constants[code[nPC++]]).bind(frame));
                     break;

                  case PUSH_PC: // codeOffset
                     if (m_nTop + 3 >= m_nStackLength)
                     {
                        m_nStackLength = (m_nStackLength + 3) << 1;
                        vars = new Object[m_nStackLength];
                        System.arraycopy(m_stack, 0, vars, 0, m_stack.length);
                        m_stack = vars;
                     }

                     m_stack[m_nTop++] = Primitive.createInteger(code[nPC++]);
                     m_stack[m_nTop++] = fun;
                     m_stack[m_nTop++] = frame;

                     break;

                  case RETURN:
                     if ((n = m_nTop - 3) <= m_nReturn)
                     {
                        return;
                     }

                     m_nTop = n;
                     frame = (Object[])m_stack[n + 1];
                     fun = (PCodeFunction)m_stack[n];
                     constants = fun.constants;
                     code = fun.code;
                     nPC = ((Integer)m_stack[n - 1]).intValue();
                     m_stack[n - 1] = m_stack[n + 2];

                     break;

                  case JUMP_TRUE: // codeOffset
                     n = code[nPC++];
                     value = m_stack[--m_nTop];

                     if (!(value instanceof Boolean) || ((Boolean)value).booleanValue())
                     {
                        nPC = n;
                     }

                     break;

                  case JUMP_FALSE: // codeOffset
                     n = code[nPC++];
                     value = m_stack[--m_nTop];

                     if (value instanceof Boolean && !((Boolean)value).booleanValue())
                     {
                        nPC = n;
                     }

                     break;

                  case JUMP: // codeOffset
                     nPC = code[nPC++];
                     break;

                  case CALL: // argCount
                     m_nArgCount = code[nPC++];
                     value = m_stack[--m_nTop];

                     if (value instanceof Function)
                     {
                        if (((Function)value).invoke(m_nArgCount, this))
                        {
                           nPC = 0;
                           fun = m_fun;
                           code = fun.code;
                           constants = fun.constants;
                           frame = fun.frame;

                           break;
                        }
                     }
                     else
                     {
                        invokeJavaMethod(value, m_nArgCount);
                     }

                     if ((n = m_nTop - 3) <= m_nReturn)
                     {
                        return;
                     }

                     m_nTop = n;
                     frame = (Object[])m_stack[n + 1];
                     fun = (PCodeFunction)m_stack[n];
                     constants = fun.constants;
                     code = fun.code;
                     nPC = ((Integer)m_stack[n - 1]).intValue();
                     m_stack[n - 1] = m_stack[n + 2];

                     break;

                  case CHECK_FRAME: // argCount
                     n = code[nPC++];

                     if (m_nArgCount != n)
                     {
                        throw new ScriptingException(
                           (m_nArgCount < n) ? "err.scripting.minArgCount" : "err.scripting.maxArgCount",
                              new Object[]{fun.getName(),
                                 Primitive.createInteger(n),
                                 Primitive.createInteger(m_nArgCount)});
                     }

                     m_nTop -= n;

                     break;

                  case SETUP_FRAME: // argCount
                     n = code[nPC++];

                     if (m_nArgCount != n)
                     {
                        throw new ScriptingException(
                           (m_nArgCount < n) ? "err.scripting.minArgCount" : "err.scripting.maxArgCount",
                              new Object[]{fun.getName(),
                                 Primitive.createInteger(n),
                                 Primitive.createInteger(m_nArgCount)});
                     }

                     vars = new Object[n + 1];

                     while (n != 0)
                     {
                        vars[n--] = m_stack[--m_nTop];
                     }

                     vars[0] = frame;
                     frame = vars;

                     break;

                  case CHECK_VARARG_FRAME: // argCount
                     n = code[nPC++];

                     if (m_nArgCount < n)
                     {
                        throw new ScriptingException("err.scripting.minArgCount",
                           new Object[]{fun.getName(),
                              Primitive.createInteger(n),
                              Primitive.createInteger(m_nArgCount)});
                     }

                     m_nTop -= m_nArgCount;

                     break;

                  case SETUP_VARARG_FRAME: // argCount
                     n = code[nPC++];

                     if (m_nArgCount < n)
                     {
                        throw new ScriptingException("err.scripting.minArgCount",
                           new Object[]{fun.getName(),
                              Primitive.createInteger(n),
                              Primitive.createInteger(m_nArgCount)});
                     }

                     vars = new Object[n + 2];
                     pair = null;

                     for (i = n; i != m_nArgCount; ++i)
                     {
                        pair = new Pair(m_stack[--m_nTop], pair);
                     }

                     vars[n + 1] = pair;

                     while (n != 0)
                     {
                        vars[n--] = m_stack[--m_nTop];
                     }

                     vars[0] = frame;
                     frame = vars;

                     break;

                  case POP:
                     --m_nTop;
                     break;

                  case EQ_P:
                     value = m_stack[--m_nTop];
                     m_stack[m_nTop - 1] = Boolean.valueOf(Intrinsic.eq(m_stack[m_nTop - 1], value));
                     break;

                  case EQ:
                     value = m_stack[--m_nTop];
                     m_stack[m_nTop - 1] = Primitive.eq(m_stack[m_nTop - 1], value);
                     break;

                  case NE:
                     value = m_stack[--m_nTop];
                     m_stack[m_nTop - 1] = Primitive.ne(m_stack[m_nTop - 1], value);
                     break;

                  case GT:
                     value = m_stack[--m_nTop];
                     m_stack[m_nTop - 1] = Primitive.gt(m_stack[m_nTop - 1], value);
                     break;

                  case GE:
                     value = m_stack[--m_nTop];
                     m_stack[m_nTop - 1] = Primitive.ge(m_stack[m_nTop - 1], value);
                     break;

                  case LT:
                     value = m_stack[--m_nTop];
                     m_stack[m_nTop - 1] = Primitive.lt(m_stack[m_nTop - 1], value);
                     break;

                  case LE:
                     value = m_stack[--m_nTop];
                     m_stack[m_nTop - 1] = Primitive.le(m_stack[m_nTop - 1], value);
                     break;

                  case LIKE:
                     value = m_stack[--m_nTop];
                     m_stack[m_nTop - 1] = Primitive.like(m_stack[m_nTop - 1], value);
                     break;

                  case ADD:
                     value = m_stack[--m_nTop];
                     m_stack[m_nTop - 1] = Primitive.add(m_stack[m_nTop - 1], value);
                     break;

                  case SUB:
                     value = m_stack[--m_nTop];
                     m_stack[m_nTop - 1] = Primitive.subtract(m_stack[m_nTop - 1], value);
                     break;

                  case MUL:
                     value = m_stack[--m_nTop];
                     m_stack[m_nTop - 1] = Primitive.multiply(m_stack[m_nTop - 1], value);
                     break;

                  case DIV:
                     value = m_stack[--m_nTop];
                     m_stack[m_nTop - 1] = Primitive.divide(m_stack[m_nTop - 1], value);
                     break;

                  case CAR:
                     value = m_stack[m_nTop - 1];

                     if (!(value instanceof Pair))
                     {
                        throw new TypeMismatchException(Symbol.CAR);
                     }

                     m_stack[m_nTop - 1] = ((Pair)value).getHead();

                     break;

                  case CDR:
                     value = m_stack[m_nTop - 1];

                     if (!(value instanceof Pair))
                     {
                        throw new TypeMismatchException(Symbol.CDR);
                     }

                     m_stack[m_nTop - 1] = ((Pair)value).getTail();

                     break;

                  case CONS:
                     value = m_stack[--m_nTop];
                     m_stack[m_nTop - 1] = new Pair(m_stack[m_nTop - 1], value);
                     break;

                  case LIST_1:
                     m_stack[m_nTop - 1] = new Pair(m_stack[m_nTop - 1]);
                     break;

                  case LIST_2:
                     value = m_stack[--m_nTop];
                     m_stack[m_nTop - 1] = new Pair(m_stack[m_nTop - 1], new Pair(value));
                     break;

                  case LIST_3:
                     m_nTop -= 2;
                     m_stack[m_nTop - 1] = new Pair(m_stack[m_nTop - 1], new Pair(m_stack[m_nTop], new Pair(m_stack[m_nTop + 1])));
                     break;

                  case LIST_4:
                     m_nTop -= 3;
                     m_stack[m_nTop - 1] = new Pair(m_stack[m_nTop - 1], new Pair(m_stack[m_nTop],
                        new Pair(m_stack[m_nTop + 1], new Pair(m_stack[m_nTop + 2]))));
                     break;

                  case DEBUG:
                     // Re execute at the same offset
                     --nPC;

                     if (altCode == code)
                     {
                        // Switch back from the alternative code
                        code = origCode;
                        altCode = origCode = null;
                     }
                     else
                     {
                        assert code == fun.code;

                        MachineState state = new MachineState(nPC, fun, frame, null);
                        BreakpointSite site = s_debugger.hitBreakpoint(state);

                        if (site == null)
                        {
                           // A DEBUG instruction could be uninstalled between
                           // the time the DEBUG is hit and the time we lookup
                           // the installation side. Ideally we would always
                           // re-execute at the current PC if lookup fails,
                           // however on the blackberry we cannot guarantee that
                           // breakpoints are not persisted and must throw an
                           // exception.
                           if (code[nPC] == DEBUG)
                           {
                              throw new InvalidDebugBytecodeException();
                           }

                           // re-execute the instruction
                           break;
                        }

                        if (site.isInstalled())
                        {
                           origCode = code;
                           altCode = code = state.createAlternativeCode(site.getInstruction());
                        }
                     }

                     break;

                  default: // argCount
                     n = code[nPC - 1] - CALL_INTRINSIC;
                     m_nArgCount = code[nPC++];

                     if (Intrinsic.getFunction(n).invoke(m_nArgCount, this))
                     {
                        nPC = 0;
                        fun = m_fun;
                        code = fun.code;
                        constants = fun.constants;
                        frame = fun.frame;
                     }

                     break;
               }
            }
         }
         catch (Throwable e)
         {
            if (debuggerEnabled)
            {
               // nPC will not necessarily be at an instruction boundary
               s_debugger.hitException(new MachineState(nPC - 1, fun, frame, e), m_steppingBreakpointSet);

               // m_steppingBreakpointSet is not necessarily empty if the user stepped from the exception.
            }

            if (m_nExceptionTop != m_nExceptionReturn)
            {
               updateStackTrace(e, fun, nPC);
               m_nTop = ((Integer)m_exceptionHandlerStack[m_nExceptionTop - 4]).intValue();

               if (!Boolean.FALSE.equals(m_exceptionHandlerStack[m_nExceptionTop - 1]))
               {
                  push(e);
                  m_nArgCount = 1;
                  fun = RETHROW_FUNCTION;
               }
               else
               {
                  m_exceptionHandlerStack[m_nExceptionTop - 1] = Boolean.TRUE;
                  push(m_exceptionHandlerStack[m_nExceptionTop - 3]);
                  push(e);
                  m_nArgCount = 2;
                  fun = CATCH_FUNCTION;
               }

               nPC = 0;
               code = fun.code;
               constants = fun.constants;
               frame = fun.frame;

               continue;
            }

            updateStackTrace(e, fun, nPC);
            ObjUtil.rethrow(e);
         }
      }
   }

   /**
    * Updates the stack trace in a given exception with p-code trace information.
    * @param t The exception to update.
    * @param fun The p-code function.
    * @param nOffset The program counter offset.
    */
   protected void updateStackTrace(Throwable t, PCodeFunction fun, int nOffset)
   {
      if (s_declaringClassField == null)
      {
         return;
      }

      StackTraceElement[] traceArray = t.getStackTrace();
      int nTraceIndex;

      for (nTraceIndex = 0; nTraceIndex < traceArray.length; ++nTraceIndex)
      {
         StackTraceElement element = traceArray[nTraceIndex];
         String sName = element.getClassName();

         if (sName == null || sName.equals("nexj.core.scripting.Machine"))
         {
            break;
         }

         sName = element.getFileName();

         if (sName != null && !sName.endsWith(".java"))
         {
            return;
         }
      }

      List traceList = new ArrayList(16);

      for (int i = 0; i < nTraceIndex; ++i)
      {
         traceList.add(traceArray[i]);
      }

      if (nTraceIndex == traceArray.length)
      {
         traceArray = new Throwable().getStackTrace();

         if (traceArray.length == 0)
         {
            return;
         }

         nTraceIndex = 0;
      }

      if (updateTrace(traceArray[nTraceIndex], fun, nOffset))
      {
         traceList.add(traceArray[nTraceIndex++]);
      }

      addStackTrace(traceList, traceArray, nTraceIndex);

      if (traceList.isEmpty())
      {
         return;
      }

      traceArray = new StackTraceElement[traceList.size()];
      traceList.toArray(traceArray);
      t.setStackTrace(traceArray);
   }

   /**
    * Updates the stack trace in a given exception with p-code trace information.
    * @param t The exception to update.
    */
   public void updateStackTrace(Throwable t)
   {
      if (s_declaringClassField == null)
      {
         return;
      }

      StackTraceElement[] traceArray = t.getStackTrace();
      List traceList = new ArrayList(16);

      addStackTrace(traceList, traceArray, 0);

      if (traceList.isEmpty())
      {
         return;
      }

      traceArray = new StackTraceElement[traceList.size()];
      traceList.toArray(traceArray);
      t.setStackTrace(traceArray);
   }

   /**
    * Visits all frames in m_stack in the order of m_top to 0 (closest to
    * execution to furthest).
    *
    * Note that frames are guessed by checking the type of three adjacent
    * elements in m_stack. In particular the types are: Integer, PCodeFunction,
    * Object[] or null. That is in some cases may incorrectly identify stack
    * frames.
    *
    * Also removes extra frames that are pushed onto the stack prior to CALLs:
    * If there are frames with duplicate stacks then all but the first are
    * ignored.
    *
    * @param visitor The visitor. visitor.visit() will be called with all
    *           identified stack frames.
    */
   public void visitStackTrace(StackFrameVisitor visitor)
   {
      visitStackTrace(visitor, null);
   }

   /**
    * Visits the stack trace like {@link #visitStackTrace(nexj.core.scripting.Machine.StackFrameVisitor)}
    * but allows the caller to provide the most recent frame to aid in filtering.
    *
    * @param visitor The visitor. visitor.visit() will be called with all
    *           identified stack frames.
    * @param lastFrame The current stack frame. Frames will not be visited that have this frame[].
    */
   public void visitStackTrace(StackFrameVisitor visitor, Object[] lastFrame)
   {
      visitStackTrace(visitor, m_stack, 0, m_nTop, lastFrame);
   }

   /**
    * Visits the stack trace allowing the caller to provide the stack and most
    * recent frame to aid in filtering.
    * 
    * @param visitor The visitor. visitor.visit() will be called with all
    *           identified stack frames.
    * @param stack The stack to visit
    * @param nBottom The bottom of the stack
    * @param nTop The top of the stack
    * @param lastFrame The current stack frame. Frames will not be visited that
    *           have this frame[].
    */
   protected static void visitStackTrace(StackFrameVisitor visitor, Object[] stack, int nBottom, int nTop, Object[] lastFrame)
   {
      for (int i = nTop - 3; i >= nBottom; --i)
      {
         if (stack[i + 1] instanceof PCodeFunction && stack[i] instanceof Integer &&
            (stack[i + 2] == null || stack[i + 2] instanceof Object[]))
         {
            PCodeFunction fun = (PCodeFunction)stack[i + 1];
            int nOffset = ((Integer)stack[i]).intValue();
            Object[] frame = (Object[])stack[i + 2];

            if (nOffset >= 0 && nOffset < fun.code.length &&
               (frame != lastFrame || frame == null))
            {
               if (!visitor.visit(fun, nOffset, frame))
               {
                  break;
               }

               lastFrame = frame;
            }

            i -= 2;
         }
      }
   }

   /**
    * Adds stack trace elements to a trace list.
    *
    * @param traceList The trace list, where to add the elements.
    * @param traceArray The trace array to reuse, can be null.
    * @param nTraceIndex The index of the first reusable element in traceArray.
    */
   protected void addStackTrace(final List traceList, final StackTraceElement[] traceArrayIn, final int nTraceIndexIn)
   {
      visitStackTrace(new StackFrameVisitor()
      {
         int nTraceIndex = nTraceIndexIn;

         StackTraceElement[] traceArray = traceArrayIn;

         public boolean visit(PCodeFunction fun, int nOffset, Object[] frame)
         {
            if (nOffset >= 0 && nOffset < fun.code.length)
            {
               if (traceArray == null || nTraceIndex == traceArray.length)
               {
                  traceArray = new Throwable().getStackTrace();
                  nTraceIndex = 0;
               }

               StackTraceElement element = traceArray[nTraceIndex];

               if (updateTrace(element, fun, nOffset))
               {
                  if (traceList.isEmpty() || !traceList.get(traceList.size() - 1).equals(element))
                  {
                     traceList.add(element);
                     ++nTraceIndex;
                  }
               }
            }

            return true;
         }
      });
   }

   /**
    * Updates a stack trace element with p-code trace information.
    * @param trace The element to update.
    * @param fun The p-code function.
    * @param nOffset The program counter offset.
    * @return True if the update succeeded.
    */
   protected static boolean updateTrace(StackTraceElement trace, PCodeFunction fun, int nOffset)
   {
      PCodeFunction.DebugInfo info = fun.getDebugInfo();

      if (info == null)
      {
         return false;
      }

      int nPos = info.getPos(nOffset);
      String sURL = info.getURL(nPos);

      if (sURL == null)
      {
         sURL = "<input>";
      }

      try
      {
         if (sURL.startsWith("class:"))
         {
            int k = sURL.indexOf('.', 6);

            if (k < 0)
            {
               // Validation URLs don't have method part
               k = sURL.indexOf('$', 6);
            }

            String sClassName = sURL.substring(6, k);
            String sMethod = sURL.substring(k + 1);

            s_declaringClassField.set(trace, sClassName);
            s_methodNameField.set(trace, sMethod);
         }
         else
         {
            int nFragIndex = sURL.lastIndexOf('#');

            if (nFragIndex >= 0)
            {
               int i = sURL.indexOf(' ', nFragIndex);

               if (i < 0)
               {
                  i = sURL.length();
               }

               s_declaringClassField.set(trace, sURL.substring(nFragIndex + 1, i));
               s_methodNameField.set(trace, (i == sURL.length()) ? "<unknown>" : sURL.substring(i + 1, sURL.length()));
               sURL = sURL.substring(0, nFragIndex);
            }
            else
            {
               int i = sURL.indexOf(':');
               int k = sURL.lastIndexOf('.');

               if (i < 0 || k <= i)
               {
                  k = sURL.length();
               }

               String sName = info.getName();

               if (sName == null)
               {
                  if (k < sURL.length())
                  {
                     sName = sURL.substring(k + 1);
                  }
                  else
                  {
                     sName = "lambda";
                  }
               }

               s_methodNameField.set(trace, sName);

               if (i > 0)
               {
                  if (sURL.startsWith("library:"))
                  {
                     String sClassName = sURL.substring(i + 1);

                     s_declaringClassField.set(trace, sClassName);
                  }
                  else
                  {
                     s_declaringClassField.set(trace, sURL.substring(i + 1, k));
                  }
               }
               else
               {
                  s_declaringClassField.set(trace, "<environment>");
               }
            }
         }

         s_fileNameField.set(trace, sURL);
         s_lineNumberField.set(trace, Primitive.createInteger(info.getLine(nPos) + 1));
      }
      catch (Exception e)
      {
         return false;
      }

      return true;
   }

   /**
    * Logs a stack trace.
    * @param nLevel The log level - one of the Logger.* constants.
    */
   public void logTrace(int nLevel)
   {
      if (GlobalEnvironment.LOGGER.isLevelEnabled(nLevel))
      {
         StackTrace t = new StackTrace();

         updateStackTrace(t);
         GlobalEnvironment.LOGGER.log(nLevel, "Stack trace", t);
      }
   }

   /**
    * Adds a stepping breakpoint. If an exception is encountered while this
    * Machine is executing then this breakpoint's owner will be notified.
    *
    * @param spec The breakpoint to be registered
    */
   public void addSteppingBreakpoint(Object spec)
   {
      if (m_steppingBreakpointSet == null)
      {
         m_steppingBreakpointSet = new HashHolder(2);
      }

      m_steppingBreakpointSet.add(spec);
   }

   /**
    * Removes a breakpoint from the list of stepping breakpoints. Must be called from the thread that "owns"
    * this Machine.
    *
    * @param spec The breakpoint to be unregistered
    */
   public void removeSteppingBreakpoint(Object spec)
   {
      if (m_steppingBreakpointSet != null)
      {
         m_steppingBreakpointSet.remove(spec);
      }
   }

   /**
    * Determines the total length of a p-code instruction with operands.
    *
    * Note that a DEBUG instruction has undefined length and the caller must
    * ensure that this is not called with a DEBUG instruction.
    *
    * @param code The instruction p-code, one of the Machine.* p-code constants.
    * @return The p-code instruction length (in chars).
    */
   public static int getPCodeLength(char code)
   {
      switch (code)
      {
         case PUSH_LOCAL:
            return 3;

         case PUSH_LOCAL_0:
         case PUSH_LOCAL_1:
         case PUSH_LOCAL_2:
         case PUSH_LOCAL_3:
         case PUSH_LOCAL_4:
            return 2;

         case SET_LOCAL:
            return 3;

         case SET_LOCAL_0:
         case SET_LOCAL_1:
         case SET_LOCAL_2:
         case SET_LOCAL_3:
         case SET_LOCAL_4:
         case PUSH_GLOBAL:
         case SET_GLOBAL:
         case DEF_GLOBAL:
         case PUSH_CONST:
            return 2;

         case PUSH_ZERO:
         case PUSH_ONE:
         case PUSH_NULL:
         case PUSH_TRUE:
         case PUSH_FALSE:
            return 1;

         case PUSH_CLOSURE:
         case PUSH_PC:
            return 2;

         case RETURN:
            return 1;

         case JUMP_TRUE:
         case JUMP_FALSE:
         case JUMP:
         case CALL:
         case CHECK_FRAME:
         case SETUP_FRAME:
         case CHECK_VARARG_FRAME:
         case SETUP_VARARG_FRAME:
            return 2;

         case POP:
         case EQ_P:
         case EQ:
         case NE:
         case GT:
         case GE:
         case LT:
         case LE:
         case LIKE:
         case ADD:
         case SUB:
         case MUL:
         case DIV:
         case CAR:
         case CDR:
         case CONS:
         case LIST_1:
         case LIST_2:
         case LIST_3:
         case LIST_4:
            return 1;

         case DEBUG:
            throw new IllegalStateException();

         default:
            return 2;
      }
   }

   /**
    * @return The debugger singleton
    */
   public static Debugger getDebugger()
   {
      return s_debugger;
   }

   // inner classes

   /**
    * A helper class to encapsulate a Machine state while it is suspended at a
    * breakpoint.
    */
   public class MachineState
   {
      /**
       * The current program counter
       */
      public final int pc;

      /**
       * The currently executing function
       */
      public final PCodeFunction function;

      /**
       * The current frame
       */
      public final Object[] frame;

      /**
       * The exception if applicable, otherwise null.
       */
      public final Throwable exception;

      // constructors

      /**
       * Initializes the machine state.
       *
       * @param nPC The current program counter
       * @param fun The current function
       * @param frame The current frame
       * @param exception The exception that was active at this machine state. May be null.
       */
      public MachineState(int nPC, PCodeFunction fun, Object[] frame, Throwable exception)
      {
         this.pc = nPC;
         this.frame = frame;
         this.function = fun;
         this.exception = exception;

         assert this.function != null;
         assert this.pc >= 0 && this.pc < this.function.code.length;
      }

      // operations

      /**
       * @return The suspended machine
       */
      public Machine getMachine()
      {
         return Machine.this;
      }

      /**
       * @return A copy of the stack
       */
      public List getRawStack()
      {
         List ret = new ArrayList();

         for (int i = 0; i <= m_nTop; i++)
         {
            ret.add(m_stack[i]);
         }

         return ret;
      }

      /**
       * @return The Context instance associated with the machine
       */
      public Context getContext()
      {
         return m_context;
      }

      /**
       * Makes a copy of m_code that has instruction replacing m_code[nPC]. Also sets
       * a DEBUG instruction in the returned code at the location the instruction pointer
       * would be after executing m_code[nPC] based on the machine's state.
       *
       * @param instruction The instruction to replace m_code[nPC]
       */
      public char[] createAlternativeCode(char instruction)
      {
         char[] altCode = new char[function.code.length];
         PCodeLocation target = findNextInstructionLocation(instruction, null, true);

         // Copy the code to altCode
         System.arraycopy(function.code, 0, altCode, 0, function.code.length);

         // remove the debug instruction in altCode
         altCode[pc] = instruction;

         if (target != null && target.function.code == function.code)
         {
            // Need to set switchback breakpoint in altCode otherwise the
            // non-authoritative code will keep executing
            altCode[target.offset] = DEBUG;
         }

         return altCode;
      }

      /**
       * Get the location that the program counter would be after executing
       * site.instruction.
       *
       * NOTE: result is best guess. In particular it might not be possible to
       * always predict the next instruction of CALLs and perhaps some intrinsic
       * functions.
       *
       * @param site The site to base the calculation on
       * @return The location of the program counter after the current
       *         instruction is executed. May be null if cannot be determined.
       */
      public PCodeLocation findNextInstructionLocation(BreakpointSite site, Predictor stepper, boolean bStepIn)
      {
         return findNextInstructionLocation(site.getInstruction(), stepper, bStepIn);
      }

      /**
       * @see this{@link #findNextInstructionLocation(BreakpointSite, Predictor, boolean)}
       *
       * @param instruction The instruction to test execution of. Assumed to be at nPC in
       *    this.fun.code.
       * @return The location of the program counter after the current
       *    instruction is executed. May be null if cannot be determined.
       */
      public PCodeLocation findNextInstructionLocation(char instruction, Predictor stepper, boolean bStepIn)
      {
         TopStackFrameVisitor visitor;
         int nNextPC = this.pc;
         PCodeFunction nextFun = this.function;
         boolean bStepReturnPossible = true;

         switch (instruction)
         {
            case JUMP_FALSE:

               if (Boolean.FALSE.equals(m_stack[m_nTop - 1]))
               {
                  nNextPC = nextFun.code[pc + 1];
               }
               else
               {
                  nNextPC += getPCodeLength(instruction);
               }

               break;

            case JUMP_TRUE:

               if (!Boolean.FALSE.equals(m_stack[m_nTop - 1]))
               {
                  nNextPC = nextFun.code[pc + 1];
               }
               else
               {
                  nNextPC += getPCodeLength(instruction);
               }

               break;

            case JUMP:

               nNextPC = nextFun.code[pc + 1];
               break;

            case CALL:
               Object value = m_stack[m_nTop - 1];

               // We cannot use bStepIn because we can't distinguish tail recursion
               // from actual calls.
               if (value instanceof PCodeFunction)
               {
                  nNextPC = 0;
                  nextFun = (PCodeFunction)value;

                  break;
               }

               if (bStepIn && stepper != null)
               {
                  int nArgCount = nextFun.code[pc + 1];
                  PCodeLocation loc = null;

                  try
                  {
                     m_nTop--;
                     loc = stepper.findStepInLocation(nArgCount, Machine.this, value);
                  }
                  finally
                  {
                     m_nTop++;
                  }

                  if (loc != null)
                  {
                     return loc;
                  }
               }

               visitor = new TopStackFrameVisitor();

               if (value instanceof Intrinsic.Continuation)
               {
                  visitContinuation(visitor, (Intrinsic.Continuation)value);
               }
               else
               {
                  // Fall back:
                  // If we can't step in then get the return address on the
                  // stack and set a breakpoint there.
                  visitStackTrace(visitor);
               }

               nextFun = visitor.getFunction();
               nNextPC = visitor.getOffset();
               bStepReturnPossible = false;

               break;

            case RETURN:

               int n;

               if ((n = m_nTop - 3) <= m_nReturn)
               {
                  return null;
               }

               nextFun = (PCodeFunction)m_stack[n];
               nNextPC = ((Integer)m_stack[n - 1]).intValue();

               break;

            default:

               // Handle intrinsic functions
               if (instruction >= CALL_INTRINSIC)
               {
                  IntrinsicFunction ifun = Intrinsic.getFunction(instruction - CALL_INTRINSIC);

                  if (ifun == Intrinsic.SYS_TRY)
                  {
                     nextFun = TRY_FUNCTION;
                     nNextPC = 0;

                     break;
                  }

                  if (ifun == Intrinsic.SYS_FINALIZE)
                  {
                     nextFun = (PCodeFunction)m_exceptionHandlerStack[m_nExceptionTop - 2];
                     nNextPC = 0;

                     break;
                  }

                  if (ifun == Intrinsic.CALL_CC || ifun == Intrinsic.CALL_WITH_CURRENT_CONTINUATION)
                  {
                     int nArgCount = nextFun.code[pc + 1];

                     if (nArgCount == 1)
                     { 
                        value = getArg(0, nArgCount);

                        if (value instanceof PCodeFunction)
                        {
                           nNextPC = 0;
                           nextFun = (PCodeFunction)value;

                           break;
                        }

                        if (value instanceof Intrinsic.Continuation)
                        {
                           visitor = new TopStackFrameVisitor();
                           visitContinuation(visitor, (Intrinsic.Continuation)value);
                           nextFun = visitor.getFunction();
                           nNextPC = visitor.getOffset();

                           break;
                        }
                     }
                  }
                  else if (ifun == Intrinsic.APPLY)
                  {
                     int nArgCount = nextFun.code[pc + 1];

                     if (nArgCount > 0)
                     {
                        value = getArg(0, nArgCount);

                        if (value instanceof PCodeFunction)
                        {
                           nNextPC = 0;
                           nextFun = (PCodeFunction)value;

                           if (function == CALL1_FUNCTION)
                           {
                              bStepReturnPossible = false;
                           }

                           break;
                        }

                        if (value instanceof Intrinsic.Continuation)
                        {
                           visitor = new TopStackFrameVisitor();
                           visitContinuation(visitor, (Intrinsic.Continuation)value);
                           nextFun = visitor.getFunction();
                           nNextPC = visitor.getOffset();

                           if (function == CALL1_FUNCTION)
                           {
                              bStepReturnPossible = false;
                           }

                           break;
                        }
                     }
                  }
                  else if (ifun == Intrinsic.CALL_WITH_VALUES)
                  {
                     int nArgCount = nextFun.code[pc + 1];

                     if (nArgCount == 2)
                     {
                        value = getArg(0, nArgCount);

                        if (value instanceof PCodeFunction ||
                           (!(value instanceof Intrinsic.Continuation) &&
                              (value = getArg(1, nArgCount)) instanceof PCodeFunction))
                        {
                           nNextPC = 0;
                           nextFun = (PCodeFunction)value;

                           break;
                        }

                        if (value instanceof Intrinsic.Continuation)
                        {
                           visitor = new TopStackFrameVisitor();
                           visitContinuation(visitor, (Intrinsic.Continuation)value);
                           nextFun = visitor.getFunction();
                           nNextPC = visitor.getOffset();

                           break;
                        }
                     }
                  }
                  else // detect call-with-values pattern from the producer function
                  {
                     int nArgCount = nextFun.code[pc + 1] + 4;

                     if (m_nTop - m_nReturn >= nArgCount)
                     {
                        value = getArg(2, nArgCount);

                        if (value instanceof PCodeFunction &&
                           Primitive.ONE_INTEGER.equals(getArg(1, nArgCount)) &&
                           Arrays.equals(((PCodeFunction)value).code, CALL1_FUNCTION.code))
                        {
                           value = getArg(0, nArgCount);

                           if (value instanceof PCodeFunction)
                           {
                              nNextPC = 0;
                              nextFun = (PCodeFunction)value;
                              bStepReturnPossible = false;

                              break;
                           }

                           if (value instanceof Intrinsic.Continuation)
                           {
                              visitor = new TopStackFrameVisitor();
                              visitContinuation(visitor, (Intrinsic.Continuation)value);
                              nextFun = visitor.getFunction();
                              nNextPC = visitor.getOffset();
                              bStepReturnPossible = false;

                              break;
                           }
                        }
                     }
                  }
               }

               nNextPC += getPCodeLength(instruction);

               break;
         }

         if (nextFun == null)
         {
            return null;
         }

         int nLength = nextFun.code.length;

         while (nNextPC < nLength && nextFun.code[nNextPC] == JUMP)
         {
            nNextPC = nextFun.code[nNextPC + 1];
         }

         // Some functions may return true, and this PCodeFunction may not
         // contain an instruction after this one.
         if (nNextPC >= nLength)
         {
            visitor = new TopStackFrameVisitor();
            visitStackTrace(visitor);

            nextFun = visitor.getFunction();
            nNextPC = visitor.getOffset();
         }
         else if (nNextPC == 0)
         {
            char c = nextFun.code[nNextPC];

            switch (c)
            {
               case CHECK_FRAME:
               case SETUP_FRAME:
               case CHECK_VARARG_FRAME:
               case SETUP_VARARG_FRAME:
                  nNextPC += getPCodeLength(c);
                  break;
            }
         }

         return new PCodeLocation(nextFun, nNextPC, bStepReturnPossible);
      }

      /**
       * Use the visitor to find the next code that will be executed once the
       * continuation is executed.
       * 
       * @param visitor The visitor to use
       * @param value The continuation
       */
      protected void visitContinuation(TopStackFrameVisitor visitor, Continuation value)
      {
         // See Machine.setContinuation()
         Object[] stack = ((Intrinsic.Continuation)value).getStack();
         int nExceptionCount = ((Integer)stack[stack.length - 1]).intValue();

         visitStackTrace(visitor, stack, 0, stack.length - nExceptionCount - 1, null);

         if (!visitor.isVisited())
         {
            visitStackTrace(visitor, m_stack, 0, m_nExceptionBarrier, null);
         }
      }

      /**
       * Find the closest exception handler.
       *
       * @return The next exception handler that will be executed or null if none.
       */
      public PCodeFunction findNextExceptionHandler()
      {
         PCodeFunction fun;

         if (m_nExceptionTop == m_nExceptionReturn)
         {
            fun = null;
         }
         else if (!Boolean.FALSE.equals(m_exceptionHandlerStack[m_nExceptionTop - 1]))
         {
            // Thrown from catch - get the finalizer
            fun = (PCodeFunction)m_exceptionHandlerStack[m_nExceptionTop - 2];

            assert fun != null;
         }
         else
         {
            // Get the catch
            fun = (PCodeFunction)m_exceptionHandlerStack[m_nExceptionTop - 3];

            assert fun != null;
         }

         return fun;
      }
   }

   /**
    * Encapsulates a particular point in this PCodeFunction
    */
   public static class PCodeLocation
   {
      /**
       * Offset in code[]
       */
      public final int offset;

      /**
       * Function this location applies to
       */
      public final PCodeFunction function;

      /**
       * Indicates whether the action to perform at this location may be step return.
       */
      public final boolean stepReturnPossible;

      /**
       * Initializes the code location.
       *
       * @param fun Function to use
       * @param nOffset Offset to use
       */
      public PCodeLocation(PCodeFunction fun, int nOffset)
      {
         this(fun, nOffset, false);
      }

      /**
       * Initializes the code location.
       *
       * @param fun Function to use
       * @param nOffset Offset to use
       * @param bStepReturnPossible True if the action to perform at this location may be step return.
       */
      public PCodeLocation(PCodeFunction fun, int nOffset, boolean bStepReturnPossible)
      {
         this.offset = nOffset;
         this.function = fun;
         this.stepReturnPossible = bStepReturnPossible;

         if (nOffset < 0 || nOffset >= function.code.length)
         {
            throw new ArrayIndexOutOfBoundsException(nOffset);
         }
      }

      /**
       * @return The instruction this location refers to
       */
      public char getInstruction()
      {
         return function.code[offset];
      }

      /**
       * @return Gets the line number if debug information available, or -1
       *         otherwise
       */
      public int getLine()
      {
         DebugInfo di = function.getDebugInfo();

         return (di == null) ? -1 : di.getLine(di.getPos(offset + 1));
      }

      /**
       * @return Gets the URL if debug information available, or -1 otherwise
       */
      public String getURL()
      {
         DebugInfo di = function.getDebugInfo();

         return (di == null) ? null : di.getURL(di.getPos(offset + 1));
      }

      /**
       * Determine if this location refers to the same line and URL as other.
       *
       * Note: even if this returns true other.getFun() is not necessarily the
       * same as getFun(). For example when a PCodeFunction is bind()ed
       *
       * @param other To be compared against this
       * @return True if same line and URL or false if debug info not available
       */
      public boolean isSameLine(PCodeLocation other)
      {
         if (function.getDebugInfo() == null)
         {
            return false;
         }

         return (getLine() == other.getLine()) && ObjUtil.equal(getURL(), other.getURL());
      }

      /**
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object o)
      {
         if (o instanceof PCodeLocation)
         {
            PCodeLocation other = (PCodeLocation)o;

            return other.function.code == function.code && other.offset == offset;
         }

         return false;
      }

      /**
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         return (((function.code == null) ? 0 : function.code.hashCode())) * 31 + offset;
      }
   }

   /**
    * Used by
    * {@link Machine#visitStackTrace(nexj.core.scripting.Machine.StackFrameVisitor)}
    * to visit stack traces.
    */
   public interface StackFrameVisitor
   {
      /**
       * Called once for each frame in the stack
       *
       * @param fun The function in this stack frame
       * @param nOffset Offset of the program counter in fun.code
       * @param frame The current frame, may be null.
       * @return True to continue the iteration, false to stop it.
       */
      boolean visit(PCodeFunction fun, int nOffset, Object[] frame);
   }

   /**
    * Stack frame visitor that peeks the top frame information.
    */
   public static class TopStackFrameVisitor implements StackFrameVisitor
   {
      /**
       * The stack frame code offset.
       */
      protected int m_nOffset;

      /**
       * The stack frame function.
       */
      protected PCodeFunction m_function;

      /**
       * @see nexj.core.scripting.Machine.StackFrameVisitor#visit(nexj.core.scripting.PCodeFunction, int, java.lang.Object[])
       */
      public boolean visit(PCodeFunction fun, int nOffset, Object[] frame)
      {
         m_nOffset = nOffset;
         m_function = fun;

         return false;
      }

      /**
       * @return The stack frame code offset.
       */
      public int getOffset()
      {
         return m_nOffset;
      }

      /**
       * @return The stack frame function.
       */
      public PCodeFunction getFunction()
      {
         return m_function;
      }

      /**
       * @return Whether this visitor successfully visited the top frame
       */
      public boolean isVisited()
      {
         return m_function != null;
      }
   }

   public static class InvalidDebugBytecodeException extends UncheckedException
   {
      private final static long serialVersionUID = 6220079379960381845L;

      /**
       * Constructor
       */
      public InvalidDebugBytecodeException()
      {
         super("err.debugger.invalidDebugBytecodeInstruction");
      }
   }

   /**
    * Encapsulates platform specific functionality for stepping in. For example the
    * Java client does not ship with the Metaclass class.
    */
   public interface Predictor
   {
      /**
       * Step into something
       * @param nArgCount The argument count
       * @param machine The machine
       * @param value The thing we are to step into
       *
       * @return Step in location, or null on error
       */
      PCodeLocation findStepInLocation(int nArgCount, Machine machine, Object value);
   }

   /**
    * Encapsulates global state of the debugger
    */
   public interface Debugger
   {
      /**
       * Handle a breakpoint hit.
       *
       * @param state Machine state when the DEBUG instruction was encountered.
       * @return The site corresponding to the location at which the breakpoint
       *         was hit. Could be null if not installed.
       */
      public BreakpointSite hitBreakpoint(MachineState state);

      /**
       * Called when an exception is caught in the scripting virtual machine.
       *
       * @param state The state at the time the exception was thrown. The
       *           exception field must be set.
       * @param steppingBreakpointSet Set of stepping breakpoints currently
       *           active in state. May be null. May be live.
       */
      public void hitException(MachineState machineState, Set steppingBreakpointSet);

      /**
       * In some cases a top level function is a closure bound at runtime to
       * another frame. The parent frame is discarded at compile-time so we save
       * it here. This can be used for variable name information.
       *
       * @param child A "top level function".
       * @param parent The parent of child.
       */
      public void setAnonymousParent(PCodeFunction pCodeFunction, PCodeFunction parent);

      /**
       * Find an installation site directly by its keys
       *
       * @param code Code[] of the installation site
       * @param nOffset Offset in code
       * @return May be null
       */
      public BreakpointSite findInstallationSite(char[] code, int nOffset);
   }

   /**
    * A location at which breakpoints can be installed
    */
   public interface BreakpointSite
   {
      /**
       * @return Whether the DEBUG instruction is currently installed in
       *         bytecode
       */
      boolean isInstalled();

      /**
       * @return The original instruction at the installation site
       */
      char getInstruction();
   }
}
