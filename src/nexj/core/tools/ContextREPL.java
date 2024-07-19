// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.tools;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

import nexj.core.runtime.Context;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Parser;
import nexj.core.scripting.SchemeParser;
import nexj.core.scripting.Symbol;
import nexj.core.util.ErrorCode;
import nexj.core.util.LimitWriter;
import nexj.core.util.ObjUtil;

/**
 * A REPL that has a context.
 */
public abstract class ContextREPL extends GenericREPL
{
   // constants

   /**
    * The return value of the previous evaluation.
    */
   private final static Symbol LAST = Symbol.define("*last*");

   /**
    * The REPL instance.
    */
   private final static Symbol REPL = Symbol.define("*repl*");

   // associations

   /**
    * The context for this REPL
    */
   protected Context m_context;

   /**
    * The machine associated with the context
    */
   protected Machine m_machine;

   // operations

   /**
    * Resets the REPL.
    */
   public void reset() throws Exception
   {
      begin();
   }

   protected abstract Context initializeContext();

   /**
    * @see nexj.core.tools.GenericREPL#begin()
    */
   protected void begin() throws Exception
   {
      super.begin();

      m_context = initializeContext();
      m_machine = m_context.getMachine();
      m_machine.setWriter(m_writer);
      m_machine.getGlobalEnvironment().defineVariable(REPL, this);
   }

   /**
    * @see nexj.core.tools.GenericTool#dispose()
    */
   protected void dispose()
   {
      super.dispose();

      if (m_machine != null)
      {
         m_machine.cleanup();
      }
   }

   /**
    * @see nexj.core.tools.GenericREPL#evaluate(java.lang.Object)
    */
   protected Object evaluate(Object expr)
   {
      PCodeFunction fun = new Compiler().compile(expr, null, m_machine, true);
      Object value = m_machine.invoke(fun, (Pair)null);

      m_machine.getGlobalEnvironment().defineVariable(LAST, value);

      return value;
   }

   /**
    * @see nexj.core.tools.GenericREPL#getParser()
    */
   protected Parser getParser()
   {
      return new SchemeParser(m_machine.getGlobalEnvironment());
   }

   /**
    * Uses Intrinsic.write with a limit writer
    * 
    * @param writer Writer to write the object to
    * @param obj Object to write
    * @throws IOException
    */
   protected void write(Writer writer, Object obj) throws IOException
   {
      Intrinsic.write(obj, (m_lMaxCount > 0) ? new LimitWriter(writer, m_lMaxCount) : writer, false);
   }

   /**
    * @see nexj.core.tools.GenericREPL#getMessage(java.lang.Throwable)
    */
   protected String getMessage(Throwable t)
   {
      if (t instanceof ErrorCode)
      {
         ErrorCode ec = (ErrorCode)t;

         return m_context.formatString(ec.getErrorCode(), ec.getErrorArgs());
      }
      else
      {
         return ObjUtil.getMessage(t);
      }
   }

   /**
    * @see nexj.core.tools.GenericREPL#setReader(java.io.Reader)
    */
   protected void setReader(Reader reader)
   {
      m_machine.setReader(reader);
   }
}
