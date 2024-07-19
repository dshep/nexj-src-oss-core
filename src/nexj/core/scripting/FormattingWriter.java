package nexj.core.scripting;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.sql.Timestamp;

import nexj.core.util.Binary;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * The FormattingWriter class implementation.
 */
public class FormattingWriter extends Writer
{
   // constants

   /**
    * Mappings from special macro symbols to their abbreviated forms as strings.
    */
   protected final static Lookup MACRO_SHORTCUT_MAP = new HashTab(9);

   static
   {
      MACRO_SHORTCUT_MAP.put(Symbol.GLOBAL, "##");
      MACRO_SHORTCUT_MAP.put(Symbol.QUOTE, "'");
      MACRO_SHORTCUT_MAP.put(Symbol.QUASIQUOTE, "`");
      MACRO_SHORTCUT_MAP.put(Symbol.QUASISYNTAX, "#`");
      MACRO_SHORTCUT_MAP.put(Symbol.SYNTAX, "#'");
      MACRO_SHORTCUT_MAP.put(Symbol.UNQUOTE, ",");
      MACRO_SHORTCUT_MAP.put(Symbol.UNQUOTE_SPLICING, ",@");
      MACRO_SHORTCUT_MAP.put(Symbol.UNSYNTAX, "#,");
      MACRO_SHORTCUT_MAP.put(Symbol.UNSYNTAX_SPLICING, "#,@");
   }

   // attributes

   /**
    * The line separator string.
    */
   protected String m_sLineSep;

   /**
    * The unit separator string.
    */
   protected String m_sUnitSep;

   /**
    * True to automatically add a unit separator between code units.
    */
   protected final boolean m_bUnitSepEnabled;

   /**
    * True indicates that a unit separator is needed before writing the next code unit.
    */
   protected boolean m_bUnitSepRequired;

   // associations

   /**
    * The writer to output to.
    */
   protected Writer m_outputWriter;

   // constructors

   /**
    * Creates a new FormattingWriter.
    * @param writer The underlying Writer object.
    * @param bUnitSepEnabled True to automatically add a unit separator between code units.
    */
   public FormattingWriter(Writer writer, boolean bUnitSepEnabled)
   {
      super(writer);

      m_outputWriter = writer;
      m_bUnitSepEnabled = bUnitSepEnabled;
      m_sUnitSep = " ";
      m_sLineSep = SysUtil.LINE_SEP;
   }

   // operations

   /**
    * @return The underlying Writer object.
    */
   public Writer getWriter()
   {
      return m_outputWriter;
   }

   /**
    * Returns the accumulated string of all written characters.
    */
   public String getOutputString()
   {
      if (!(m_outputWriter instanceof StringWriter))
      {
         return "";
      }

      try
      {
         flush();
      }
      catch (IOException e)
      {
         throw new ScriptingException("err.scripting.io", e);
      }

      return m_outputWriter.toString();
   }

   /**
    * @return The line length limit (in characters).
    */
   public int getLineLength()
   {
      return -1;
   }

   /**
    * @return The line break threshold (in characters).
    */
   public int getLineBreakThreshold()
   {
      return -1;
   }

   /**
    * @return The number of characters per indentation level.
    */
   public int getIndentation()
   {
      return 0;
   }

   /**
    * @return The line separator string.
    */
   public String getLineSeparator()
   {
      return m_sLineSep;
   }

   /**
    * Sets the line length limit, including indentation.
    * @param nLineLength The new line length limit (in characters).
    */
   public void setLineLength(int nLineLength)
   {
   }

   /**
    * Sets the character count threshold, excluding indentation, above which line breaks are allowed.
    * @param nThreshold The new line break threshold (in characters).
    */
   public void setLineBreakThreshold(int nThreshold)
   {
   }

   /**
    * Sets the number of characters per indentation level.
    * @param nCount The new number of characters per indentation level. Must be a positive number.
    */
   public void setIndentation(int nCount)
   {
   }

   /**
    * Adds indentation if necessary.
    * @param expr The expression to format.
    * @param nCurIndent The current indentation of the expression (in spaces).
    */
   protected void indent(Object expr, int nCurIndent) throws IOException
   {
   }

   /**
    * Sets the line separator string.
    * @param sLineSep The new line separator string.
    */
   public void setLineSeparator(String sLineSep)
   {
      m_sLineSep = sLineSep;
   }

   /**
    * Writes the given open token.
    * @param ch The open token.
    */
   public void writeOpenToken(char ch) throws IOException
   {
      write(ch);
   }

   /**
    * Writes the given open token.
    * @param sToken The open token.
    */
   public void writeOpenToken(String sToken) throws IOException
   {
      write(sToken);
   }

   /**
    * Writes a close token. 
    * @param ch The close token.
    */
   public void writeCloseToken(char ch) throws IOException
   {
      write(ch);
   }

   /**
    * Writes a separator, which is either a line break character or a whitespace character,
    * at the appropriate indentation level.
    * @param bSpaceRequired True indicates that a whitespace character is required if
    * a line break is determined to be unnecessary. False indicates that no character
    * is needed if a line break is determined to be unnecessary.
    */
   public void writeSeparator(boolean bSpaceRequired) throws IOException
   {
      if (bSpaceRequired)
      {
         writeSpace();
      }
   }

   /**
    * Writes a single space.
    */
   public void writeSpace() throws IOException
   {
      write(' ');
   }

   /**
    * Writes a line separator without handling indentation.
    */
   public void writeLineSeparator() throws IOException
   {
      write(m_sLineSep);

      // a line separator is considered a unit separator, although they may be different strings
      m_bUnitSepRequired = false;
   }

   /**
    * Writes the given special token.
    * @param sToken The special token.
    */
   public void writeSpecialToken(String sToken) throws IOException
   {
      write(sToken);
   }

   /**
    * Flush the buffered special tokens, if any.
    */
   public void flushSpecialTokens() throws IOException
   {
   }

   /**
    * Writes a comment.
    * @param comment The comment to be written.
    */
   protected void writeComment(Comment comment) throws IOException
   {
   }

   /**
    * @see java.io.Writer#write(char[], int, int)
    */
   public void write(char cbuf[], int off, int len) throws IOException
   {
      m_outputWriter.write(cbuf, off, len);
   }

   /**
    * @see java.io.Writer#flush()
    */
   public void flush() throws IOException
   {
      m_outputWriter.flush();
   }

   /**
    * Clear the output character stream.
    */
   public void clear()
   {
      if (m_outputWriter instanceof StringWriter)
      {
         ((StringWriter)m_outputWriter).getBuffer().setLength(0);
      }

      m_bUnitSepRequired = false;
   }

   /**
    * @see java.io.Writer#close()
    */
   public void close() throws IOException
   {
      m_outputWriter.close();
   }

   /**
    * Pretty-formats the given S-expression. If this writer is configured to automatically
    * add unit separators, a unit separator is inserted between calls to this function;
    * otherwise this function behaves like
    * {@link nexj.core.scripting.FormattingWriter#format(java.lang.Object, boolean)}.
    * @param expr The S-expression to write. Considered a single code unit for the purpose
    * of adding unit separators.
    * @param bDisplay True to display strings without quotes and number without suffixes.
    * @throws IOException if an I/O error occurs.
    */
   public void formatUnit(Object expr, boolean bDisplay) throws IOException
   {
      formatUnit(expr, bDisplay, 0);
   }

   /**
    * Pretty-formats the given S-expression, taking into account its current indentation.
    * If this writer is configured to automatically add unit separators, a unit separator
    * is inserted between calls to this function.
    * @param expr The S-expression to write. Considered a single unit for the purpose of
    * adding unit separators.
    * @param bDisplay True to display strings without quotes and number without suffixes.
    * @param nCurIndent The current indentation of the expression (in spaces).
    * @throws IOException if an I/O error occurs.
    */
   public void formatUnit(Object expr, boolean bDisplay, int nCurIndent) throws IOException
   {
      if (m_bUnitSepEnabled)
      {
         if (m_bUnitSepRequired)
         {
            write(m_sUnitSep);
         }

         indent(expr, nCurIndent);
         m_bUnitSepRequired = true;
      }

      format(expr, bDisplay);
   }

   /**
    * Pretty-formats the given S-expression.
    * @param expr The S-expression to write.
    * @param bDisplay True to display strings without quotes and number without suffixes.
    * @throws IOException if an I/O error occurs.
    */
   public void format(Object expr, boolean bDisplay) throws IOException
   {
      if (expr instanceof String)
      {
         flushSpecialTokens();

         String s = (String)expr;

         if (bDisplay)
         {
            write(s);
         }
         else
         {
            write('"');

            int nCount = s.length();

            for (int i = 0; i < nCount; ++i)
            {
               char ch = s.charAt(i);

               if (ch >= ' ' && ch <= '~')
               {
                  if (ch == '\\' || ch == '"')
                  {
                     write('\\');
                  }

                  write(ch);
               }
               else
               {
                  switch (ch)
                  {
                     case '\b':
                        write("\\b");
                        break;

                     case '\t':
                        write("\\t");
                        break;

                     case '\n':
                        write("\\n");
                        break;

                     case '\r':
                        write("\\r");
                        break;

                     default:
                        write("\\u");
                        StringUtil.writeHex(this, ch);
                        break;
                  }
               }
            }

            write('"');
         }
      }
      else if (expr instanceof Symbol)
      {
         flushSpecialTokens();
         write(((Symbol)expr).getName());
      }
      else if (expr instanceof Pair)
      {
         Pair pair = (Pair)expr;

         if (handleSpecialPair(pair, bDisplay))
         {
            writeOpenToken('(');

            Object head = pair.getHead();

            if (!(head instanceof Symbol) && !(head instanceof Pair))
            {
               // Not a function call. Line break may be added before the head.
               writeSeparator(false);
            }

            writePair(pair, bDisplay);
            writeCloseToken(')');
         }
      }
      else if (expr instanceof Number)
      {
         flushSpecialTokens();
         write(expr.toString());

         if (!bDisplay)
         {
            if (expr instanceof Long)
            {
               long l = ((Long)expr).longValue();

               if (l >= Integer.MIN_VALUE && l <= Integer.MAX_VALUE)
               {
                  write('L');
               }
            }
            else if (expr instanceof Float)
            {
               write('F');
            }
            else if (expr instanceof BigDecimal)
            {
               BigDecimal dec = (BigDecimal)expr;

               if (dec.compareTo(Intrinsic.MIN_LONG_DECIMAL) >= 0 && dec.compareTo(Intrinsic.MAX_LONG_DECIMAL) <= 0)
               {
                  write('N');
               }
            }
         }
      }
      else if (expr instanceof Boolean)
      {
         flushSpecialTokens();
         write((((Boolean)expr).booleanValue()) ? "#t" : "#f");
      }
      else if (expr instanceof Binary)
      {
         flushSpecialTokens();

         if (!bDisplay)
         {
            write("#z");
         }

         byte[] data = ((Binary)expr).getData();

         if (data != null)
         {
            Binary.write(this, data, 0, data.length);
         }
      }
      else if (expr instanceof Timestamp)
      {
         flushSpecialTokens();

         if (!bDisplay)
         {
            write("#m");
         }

         StringBuffer buf = new StringBuffer(26);

         StringUtil.appendUTC(buf, (Timestamp)expr, !bDisplay);

         for (int i = 0, n = buf.length(); i < n; ++i)
         {
            write(buf.charAt(i));
         }
      }
      else if (expr instanceof Character)
      {
         flushSpecialTokens();

         char ch = ((Character)expr).charValue();

         if (bDisplay)
         {
            write(ch);
         }
         else
         {
            write("#\\");

            if (ch > ' ' && ch <= '~')
            {
               write(ch);

               if (ch == '\\')
               {
                  write(ch);
               }
            }
            else
            {
               switch (ch)
               {
                  case '\b':
                     write("\\b");
                     break;

                  case '\t':
                     write("\\t");
                     break;

                  case '\r':
                     write("\\r");
                     break;

                  case '\n':
                     write("newline");
                     break;

                  case ' ':
                     write("space");
                     break;

                  default:
                     write("\\u");
                     StringUtil.writeHex(this, ch);
                     break;
               }
            }
         }
      }
      else if (expr instanceof Comment)
      {
         writeComment((Comment)expr);
      }
      else if (expr == null)
      {
         flushSpecialTokens();
         write("()");
      }
      else if (expr.getClass().isArray())
      {
         writeOpenToken((expr instanceof byte[]) ? "#vu8(" : "#(");

         for (int i = 0, nCount = Array.getLength(expr); i < nCount; ++i)
         {
            writeSeparator(i > 0);
            format(Array.get(expr, i), bDisplay);
         }

         writeCloseToken(')');
      }
      else
      {
         flushSpecialTokens();

         String sExpr = expr.toString();

         if (sExpr == null)
         {
            write("#<>");
         }
         else
         {
            write("#<");
            write(sExpr);
            write('>');
         }
      }
   }

   /**
    * Detects and handles special cases.
    * @param pair The pair to format.
    * @return False if the pair is a special case that has been handled successfully.  
    */
   protected boolean handleSpecialPair(Pair pair, boolean bDisplay) throws IOException
   {
      Object head = pair.getHead();
      Object tail = pair.getTail();

      if (head instanceof Symbol && tail instanceof Pair)
      {
         Pair tailPair = (Pair)tail;

         if (tailPair.getTail() == null)
         {
            String sAbbr = abbreviateMacro((Symbol)head);

            if (sAbbr != null)
            {
               writeSpecialToken(sAbbr);
               format(tailPair.getHead(), bDisplay);

               return false;
            }
         }
      }

      return true;
   }

   /**
    * Writes a pair without parentheses.
    * @param pair The pair to write.
    * @param bDisplay True to display strings without quotes and number without suffixes.
    */
   protected void writePair(Pair pair, boolean bDisplay) throws IOException
   {
      for (;;)
      {
         format(pair.getHead(), bDisplay);

         Object tail = preprocessPair(pair.getTail(), false);

         if (tail instanceof Pair)
         {
            writeSeparator(true);
            pair = (Pair)tail;
         }
         else
         {
            writeImproperTail(tail, bDisplay);
            break;
         }
      }
   }

   /**
    * Writes the improper tail of a pair, without parentheses.
    * @param tail The tail to write.
    * @param bDisplay True to display strings without quotes and number without suffixes.
    */
   protected void writeImproperTail(Object tail, boolean bDisplay) throws IOException
   {
      if (tail != null)
      {
         writeSeparator(true);
         write('.');
         writeSeparator(true);
         format(tail, bDisplay);
      }
   }

   /**
    * Preprocesses the expression, if it is a pair.
    * @param expr The expression.
    * @param bHead Whether the expression is the head of a list.
    * @return The preprocessed expression if it is a pair;
    * the argument itself otherwise.
    */
   protected Object preprocessPair(Object expr, boolean bHead) throws IOException
   {
      return expr;
   }

   /**
    * @return The abbreviated form of the given symbol, or null if no abbreviation exists.
    */
   protected String abbreviateMacro(Symbol sym)
   {
      return (String)MACRO_SHORTCUT_MAP.get(sym);
   }
}
