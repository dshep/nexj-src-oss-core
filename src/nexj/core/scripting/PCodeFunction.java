// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.io.IOException;
import java.io.ObjectStreamException;
import java.io.Serializable;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import nexj.core.util.ObjUtil;

/**
 * A function that is executed by the virtual machine.
 */
public class PCodeFunction implements Function, Serializable
{
   // constants

   /**
    * The last two constants of every PCodeFunction contain debugging data
    */
   public static final int DEBUG_CONSTANT_COUNT = 2;
   
   /**
    * The Java stream unique identifier for the class.
    */
   private final static long serialVersionUID = -5783938240409793911L;

   // attributes

   /**
    * The compiled p-code of the function.
    */
   public char[] code;

   /**
    * An array storing all the constants referenced by the function.
    */
   public Object[] constants;

   /**
    * Represents the local variable frame of the function: Object[offset].
    * Element [0] is a pointer to the parent frame.
    */
   public Object[] frame;

   // constructors

   /**
    * Constructs the p-code function.
    */
   public PCodeFunction()
   {
   }

   /**
    * Constructs the p-code function.
    * @param code The p-code array.
    */
   public PCodeFunction(char[] code)
   {
      this.code = code;
   }

   /**
    * Constructs the p-code function.
    * @param code The p-code array.
    * @param constants The constant array.
    */
   public PCodeFunction(char[] code, Object[] constants)
   {
      this.code = code;
      this.constants = constants;
   }

   /**
    * Constructs the p-code function.
    * @param code The p-code array.
    * @param constants The constant array.
    * @param frame The closure frame.
    */
   public PCodeFunction(char[] code, Object[] constants, Object[] frame)
   {
      this.code = code;
      this.constants = constants;
      this.frame = frame;
   }

   // operations

   /**
    * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
    */
   public final boolean invoke(int nArgCount, Machine machine)
   {
      machine.setFunction(this, nArgCount);

      return true;
   }

   /**
    * Binds the function to a new local environment, thus creating a closure.
    * @param frame The new local frame.
    * @return The new closure.
    */
   public PCodeFunction bind(Object[] frame)
   {
      return new PCodeFunction(this.code, this.constants, frame);
   }

   /**
    * Used by writeReplace() to get a new instance of the current class.
    */
   public PCodeFunction create(char[] code, Object[] constants, Object[] frame)
   {
      return new PCodeFunction(code, constants, frame);
   }

   /**
    * @return The formal argument count.
    */
   public int getArgCount()
   {
      switch (code[0])
      {
         case Machine.CHECK_FRAME:
         case Machine.SETUP_FRAME:
            return code[1];

         case Machine.CHECK_VARARG_FRAME:
         case Machine.SETUP_VARARG_FRAME:
            return code[1] + 1;

         default:
            return 0;
      }
   }

   /**
    * @return The formal argument count available to the debugger.
    */
   public int getDebugArgCount()
   {
      switch (code[0])
      {
         case Machine.SETUP_FRAME:
            return code[1];

         case Machine.SETUP_VARARG_FRAME:
            return code[1] + 1;

         default:
            return 0;
      }
   }

   /**
    * @return True if the function accepts a variable number of arguments.
    */
   public boolean isVarArg()
   {
      switch (code[0])
      {
         case Machine.CHECK_VARARG_FRAME:
         case Machine.SETUP_VARARG_FRAME:
            return true;
      }

      return false;
   }

   /**
    * @return The debug information, or null if not available.
    */
   public DebugInfo getDebugInfo()
   {
      if (constants == null || constants.length < DEBUG_CONSTANT_COUNT)
      {
         return null;
      }

      Object obj = constants[constants.length - 2];

      if (!(obj instanceof char[]))
      {
         return null;
      }

      char[] posArray = (char[])obj;

      if (posArray.length % DebugInfo.POS_SIZE != 0)
      {
         return null;
      }

      obj = constants[constants.length - 1];

      if (!(obj instanceof String[]))
      {
         return null;
      }

      return new DebugInfo(posArray, (String[])obj);
   }

   /**
    * Computes the global symbols used by this function and its dependencies.
    * @param symbolSet The set of global symbols: Symbol[]. Updated by this method.
    * @param env The global environment containing the dependency symbols. Can be null.
    */
   public void computeGlobals(Set symbolSet, GlobalEnvironment env)
   {
      for (int i = 0; i < code.length;)
      {
         char c = code[i];

         for (;;)
         {
            switch (c)
            {
               case Machine.PUSH_GLOBAL:
               case Machine.SET_GLOBAL:
                  Symbol sym = (Symbol)constants[code[i + 1]];
   
                  if (symbolSet.add(sym) && env != null)
                  {
                     Object value = env.findVariable(sym);
   
                     if (value instanceof PCodeFunction)
                     {
                        ((PCodeFunction)value).computeGlobals(symbolSet, env);
                     }
                  }
   
                  break;
   
               case Machine.PUSH_CLOSURE:
                  ((PCodeFunction)constants[code[i + 1]]).computeGlobals(symbolSet, env);
                  break;
   
               case Machine.DEBUG:
                  c = Machine.getDebugger().findInstallationSite(code, i).getInstruction();
                  continue;
            }

            break;
         }

         i += Machine.getPCodeLength(c);
      }
   }

   /**
    * Disassembles the function into a string buffer.
    * @param buf The output string buffer.
    */
   public void disasm(StringBuffer buf)
   {
      List referencedFunctionsList = null;

      buf.append("; ");
      buf.append(toString());
      buf.append('\n');

      if (code == null)
      {
         return;
      }

      for (int i = 0; i < code.length; ++i)
      {
         buf.append(i);
         buf.append('\t');

         char c = code[i];

         for (;;)
         {
            switch (c)
            {
               case Machine.PUSH_LOCAL:
                  buf.append("pushl\t[");
                  buf.append((int)code[++i]);
                  buf.append(", ");
                  buf.append((int)code[++i]);
                  buf.append(']');
                  break;

               case Machine.PUSH_LOCAL_0:
               case Machine.PUSH_LOCAL_1:
               case Machine.PUSH_LOCAL_2:
               case Machine.PUSH_LOCAL_3:
               case Machine.PUSH_LOCAL_4:
                  buf.append("pushl");
                  buf.append((int)code[i] - Machine.PUSH_LOCAL_0);
                  buf.append("\t[");
                  buf.append((int)code[++i]);
                  buf.append(']');
                  break;

               case Machine.SET_LOCAL:
                  buf.append("setl\t[");
                  buf.append((int)code[++i]);
                  buf.append(", ");
                  buf.append((int)code[++i]);
                  buf.append(']');
                  break;

               case Machine.SET_LOCAL_0:
               case Machine.SET_LOCAL_1:
               case Machine.SET_LOCAL_2:
               case Machine.SET_LOCAL_3:
               case Machine.SET_LOCAL_4:
                  buf.append("setl");
                  buf.append((int)code[i] - Machine.SET_LOCAL_0);
                  buf.append("\t[");
                  buf.append((int)code[++i]);
                  buf.append(']');
                  break;

               case Machine.PUSH_GLOBAL:
                  buf.append("pushg\t[");
                  buf.append((int)code[++i]);
                  buf.append("]\t; '");
                  buf.append(constants[code[i]]);
                  break;

               case Machine.SET_GLOBAL:
                  buf.append("setg\t[");
                  buf.append((int)code[++i]);
                  buf.append("]\t; '");
                  buf.append(constants[code[i]]);
                  break;

               case Machine.DEF_GLOBAL:
                  buf.append("defg\t[");
                  buf.append((int)code[++i]);
                  buf.append("]\t; '");
                  buf.append(constants[code[i]]);
                  break;

               case Machine.PUSH_CONST:
                  buf.append("pushc\t[");
                  buf.append((int)code[++i]);
                  buf.append("]\t; ");
                  appendConstant(buf, constants[code[i]]);
                  break;

               case Machine.PUSH_ZERO:
                  buf.append("push0");
                  break;

               case Machine.PUSH_ONE:
                  buf.append("push1");
                  break;

               case Machine.PUSH_NULL:
                  buf.append("pushn");
                  break;

               case Machine.PUSH_TRUE:
                  buf.append("push#t");
                  break;

               case Machine.PUSH_FALSE:
                  buf.append("push#f");
                  break;

               case Machine.PUSH_CLOSURE:
                  buf.append("pushf\t[");
                  buf.append((int)code[++i]);
                  buf.append("]\t; ");
                  appendConstant(buf, constants[code[i]]);
                  break;

               case Machine.PUSH_PC:
                  buf.append("pushpc\t");
                  buf.append((int)code[++i]);
                  break;

               case Machine.RETURN:
                  buf.append("ret");
                  break;

               case Machine.JUMP_TRUE:
                  buf.append("jmpt\t");
                  buf.append((int)code[++i]);
                  break;

               case Machine.JUMP_FALSE:
                  buf.append("jmpf\t");
                  buf.append((int)code[++i]);
                  break;

               case Machine.JUMP:
                  buf.append("jmp\t");
                  buf.append((int)code[++i]);
                  break;

               case Machine.CALL:
                  buf.append("call\t");
                  buf.append((int)code[++i]);
                  break;

               case Machine.CHECK_FRAME:
                  buf.append("chkf\t");
                  buf.append((int)code[++i]);
                  break;

               case Machine.SETUP_FRAME:
                  buf.append("setf\t");
                  buf.append((int)code[++i]);
                  break;

               case Machine.CHECK_VARARG_FRAME:
                  buf.append("chkv\t");
                  buf.append((int)code[++i]);
                  break;

               case Machine.SETUP_VARARG_FRAME:
                  buf.append("setv\t");
                  buf.append((int)code[++i]);
                  break;

               case Machine.POP:
                  buf.append("pop");
                  break;

               case Machine.EQ_P:
                  buf.append("eq?");
                  break;

               case Machine.EQ:
                  buf.append("eq");
                  break;

               case Machine.NE:
                  buf.append("ne");
                  break;

               case Machine.GT:
                  buf.append("gt");
                  break;

               case Machine.GE:
                  buf.append("ge");
                  break;

               case Machine.LT:
                  buf.append("lt");
                  break;

               case Machine.LE:
                  buf.append("le");
                  break;

               case Machine.LIKE:
                  buf.append("like");
                  break;

               case Machine.ADD:
                  buf.append("add");
                  break;

               case Machine.SUB:
                  buf.append("sub");
                  break;

               case Machine.MUL:
                  buf.append("mul");
                  break;

               case Machine.DIV:
                  buf.append("div");
                  break;

               case Machine.CAR:
                  buf.append("car");
                  break;

               case Machine.CDR:
                  buf.append("cdr");
                  break;

               case Machine.CONS:
                  buf.append("cons");
                  break;

               case Machine.LIST_1:
                  buf.append("list1");
                  break;

               case Machine.LIST_2:
                  buf.append("list2");
                  break;

               case Machine.LIST_3:
                  buf.append("list3");
                  break;

               case Machine.LIST_4:
                  buf.append("list4");
                  break;

               case Machine.DEBUG:
                  c = Machine.getDebugger().findInstallationSite(code, i).getInstruction();
                  continue; // Jump to top, show the replaced instruction

               default:
                  buf.append("call:");
                  buf.append(Intrinsic.getFunction(code[i] - Machine.CALL_INTRINSIC).getSymbol());
                  buf.append('\t');
                  buf.append((int)code[++i]);
                  break;
            }

            break;
         }

         buf.append('\n');
      }

      if (constants != null)
      {
         buf.append('\n');

         for (int i = 0; i < constants.length; ++i)
         {
            buf.append(i);
            buf.append("\tdefc\t");

            Object value = constants[i];

            if (value instanceof PCodeFunction)
            {
               buf.append("lambda ");
               buf.append(value.toString());

               if (referencedFunctionsList == null)
               {
                  referencedFunctionsList = new ArrayList(4);
               }

               referencedFunctionsList.add(value);
            }
            else if (value instanceof char[] &&
               ((char[])value).length % DebugInfo.POS_SIZE == 0 &&
               i == constants.length - 2)
            {
               buf.append("#(");

               char[] posArray = (char[])value;

               for (int k = 0; k < posArray.length; ++k)
               {
                  if (k != 0)
                  {
                     buf.append(' ');

                     if (k % DebugInfo.POS_SIZE == 0)
                     {
                        buf.append(' ');
                     }
                  }

                  buf.append((int)posArray[k]);
               }

               buf.append(") ; debug info");
            }
            else
            {
               appendConstant(buf, value);
            }

            buf.append('\n');
         }
      }

      if (referencedFunctionsList != null)
      {
         for (Iterator it = referencedFunctionsList.iterator(); it.hasNext();)
         {
            buf.append('\n');

            ((PCodeFunction)it.next()).disasm(buf);
         }
      }
   }

   /**
    * @return The disassembly of this function.
    */
   public String disasm()
   {
      StringBuffer buf = new StringBuffer(256);

      disasm(buf);

      return buf.toString();
   }

   /**
    * Appends a formatted constant to a string buffer.
    * @param buf The output string buffer.
    * @param value The value to append.
    */
   private static void appendConstant(StringBuffer buf, Object value)
   {
      if (value instanceof Symbol)
      {
         buf.append('\'');
      }

      StringWriter writer = new StringWriter(32);

      try
      {
         Intrinsic.write(value, writer, false);
      }
      catch (IOException e)
      {
      }

      buf.append(writer.getBuffer());
   }

   /**
    * @return The function name.
    */
   public String getName()
   {
      DebugInfo info = getDebugInfo();

      if (info != null)
      {
         String sName = info.getName();

         if (sName != null)
         {
            return sName;
         }
      }

      return super.toString();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      DebugInfo info = getDebugInfo();

      if (info != null)
      {
         String sName = info.getName();

         if (sName != null)
         {
            return ObjUtil.getShortClassName(this) + ':' + sName;
         }
      }

      return super.toString();
   }

   /**
    * Called by Java serialization
    * @return An identical object with all breakpoints removed
    * @throws ObjectStreamException
    */
   protected Object writeReplace()
   {
      return getCleanCopy();
   }

   /**
    * If the current code[] has DEBUG instructions then create a new instance of this 
    * with a copy of code[] that has no DEBUG instructions. If there are no DEBUG 
    * instructions then this is returned. 
    * 
    * {@link #create(char[], Object[], Object[])} is used to instantiate a new instance.   
    * 
    * @return An instance of the current PCodeFunction with no DEBUG instructions, may 
    *   be this.
    */
   public PCodeFunction getCleanCopy()
   {
      if (Machine.debuggerEnabled)
      {
         final char[] code = this.code;
         char[] curCode = code;

         if (curCode == null)
         {
            return this;
         }

         for (int i = 0; i < curCode.length; i += Machine.getPCodeLength(curCode[i]))
         {
            if (curCode[i] == Machine.DEBUG)
            {
               Machine.BreakpointSite site = Machine.getDebugger().findInstallationSite(code, i);

               if (site == null)
               {
                  throw new IllegalStateException();
               }

               // Create a copy of the code
               if (curCode == code)
               {
                  curCode = new char[curCode.length];
                  System.arraycopy(code, 0, curCode, 0, curCode.length);
               }

               curCode[i] = site.getInstruction();
            }
         }

         // Was the code copied?
         if (curCode != code)
         {
            return create(curCode, this.constants, this.frame);
         }
         else
         {
            return this;
         }
      }

      return this;
   }

   // inner classes

   /**
    * The debug information.
    */
   public static class DebugInfo
   {
      // constants

      /**
       * Code offset.
       */
      protected final static int POS_OFFSET = 0;

      /**
       * Line number.
       */
      protected final static int POS_LINE = 1;

      /**
       * Column number.
       */
      protected final static int POS_COLUMN = 2;

      /**
       * URL index.
       */
      protected final static int POS_URL = 3;

      /**
       * Position element size.
       */
      protected final static int POS_SIZE = 4;

      // attributes

      /**
       * The URL count.
       */
      protected int m_nURLCount;

      // associations

      /**
       * The text position array.
       */
      protected char[] m_posArray;

      /**
       * The URL array.
       */
      protected String[] m_urlArray;

      // constructors

      /**
       * Constructs the debug info.
       * @param posArray The text position array.
       * @param urlArray The URL array.
       */
      protected DebugInfo(char[] posArray, String[] urlArray)
      {
         m_posArray = posArray;
         m_urlArray = urlArray;
      }

      // operations

      /**
       * @return The text position count.
       */
      public int getPosCount()
      {
         return m_posArray.length / POS_SIZE;
      }

      /**
       * Finds a position based on the code offset.
       * @param nOffset The code offset.
       * @return The position ordinal number.
       */
      public int getPos(int nOffset)
      {
         int i;

         for (i = m_posArray.length - POS_SIZE; i >= POS_SIZE; i -= POS_SIZE)
         {
            if (m_posArray[i + POS_OFFSET] < nOffset)
            {
               break;
            }
         }

         return i / POS_SIZE;
      }
      
      /**
       * Gets the code offset of a given position item.
       * @param nPos The position item ordinal number.
       * @return The code offset.
       */
      public int getOffset(int nPos)
      {
         if (nPos < 0)
         {
            return 0;
         }

         return m_posArray[nPos * POS_SIZE + POS_OFFSET]; 
      }

      /**
       * Gets the line number of a given position item.
       * @param nPos The position item ordinal number.
       * @return The line number (0-based).
       */
      public int getLine(int nPos)
      {
         if (nPos < 0)
         {
            return 0;
         }

         return m_posArray[nPos * POS_SIZE + POS_LINE]; 
      }

      /**
       * Gets the column number of a given position item.
       * @param nPos The position item ordinal number.
       * @return The column number (0-based).
       */
      public int getColumn(int nPos)
      {
         if (nPos < 0)
         {
            return 0;
         }

         return m_posArray[nPos * POS_SIZE + POS_COLUMN]; 
      }

      /**
       * Gets the URL of a given position item.
       * @param nPos The position item ordinal number.
       * @return The URL, or null if there is no URL info.
       */
      public String getURL(int nPos)
      {
         if (nPos < 0)
         {
            return null;
         }

         return m_urlArray[m_posArray[nPos * POS_SIZE + POS_URL]];
      }

      /**
       * @return The URL count.
       */
      public int getURLCount()
      {
         if (m_nURLCount == 0)
         {
            for (int i = POS_URL, n = m_posArray.length; i < n; i += POS_SIZE)
            {
               int nURL = m_posArray[i];

               if (nURL >= m_nURLCount)
               {
                  m_nURLCount = nURL + 1;
               }
            }
         }

         return m_nURLCount;
      }

      /**
       * @return The function name. Can be null.
       */
      public String getName()
      {
         int nURLCount = getURLCount();

         if (nURLCount != m_urlArray.length)
         {
            return m_urlArray[m_urlArray.length - 1];
         }

         return null;
      }

      /**
       * Gets an argument name.
       * @param nArg The argument ordinal number (0-based).
       * @return The argument name.
       */
      public String getArgName(int nArg)
      {
         int nURLCount = getURLCount();

         if (nURLCount != m_urlArray.length)
         {
            return m_urlArray[nURLCount + nArg];
         }

         return "arg" + (nArg + 1);
      }
   }
}
