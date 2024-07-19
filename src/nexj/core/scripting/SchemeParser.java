// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.util.ArrayList;
import java.util.List;

import nexj.core.meta.Primitive;
import nexj.core.util.TextPosition;

/**
 * Parser for streams representing scheme S-expressions.
 */
public class SchemeParser extends GenericParser
{
   // constants

   /**
    * Open paren (
    */
   protected final static int TOKEN_OPAREN = 1;

   /**
    * Closing paren )
    */
   protected final static int TOKEN_CPAREN = 2;

   /**
    * Period .
    */
   protected final static int TOKEN_PERIOD = 3;

   /**
    * Quote '
    */
   protected final static int TOKEN_QUOTE = 4;

   /**
    * Quasiquote `
    */
   protected final static int TOKEN_QUASIQUOTE = 5;

   /**
    * Unquote ,
    */
   protected final static int TOKEN_UNQUOTE = 6;

   /**
    * Unquote-splicing ,@
    */
   protected final static int TOKEN_UNQUOTESPLICING = 7;

   /**
    * Global ## 
    */
   protected final static int TOKEN_GLOBAL = 8;

   /**
    * Sharp paren #(
    */
   protected final static int TOKEN_SHARPPAREN = 9;

   /**
    * Atom (symbol, boolean, number, char, string).
    */
   protected final static int TOKEN_ATOM = 10;

   /**
    * Bytevector #vu8(
    */
   protected final static int TOKEN_BYTEVECTOR = 11;

   /**
    * Syntax #'
    */
   protected final static int TOKEN_SYNTAX = 12;

   /**
    * Quasisyntax #`
    */
   protected final static int TOKEN_QUASISYNTAX = 13;

   /**
    * Unsyntax #,
    */
   protected final static int TOKEN_UNSYNTAX = 14;

   /**
    * Unsyntax-splicing #,@
    */
   protected final static int TOKEN_UNSYNTAXSPLICING = 15;

   // constructors

   /**
    * Create a parser with a given global environment for interning symbols.
    * @param globalEnv The global environment, into which to store the symbols.
    */
   public SchemeParser(GlobalEnvironment globalEnv)
   {
      super(globalEnv);
   }

   // operations

   /**
    * Parses a single Scheme element - atom, list or vector.
    * @return The internal representation of the element.
    * @throws ParserException if a syntax error has been encountered.
    */
   protected Object parseElement()
   {
      TextPosition pos = null;
      Object obj;

      switch (getCurToken())
      {
         case TOKEN_OPAREN:
            if (m_posMap != null)
            {
               pos = getCurTokenPos();
            }

            forgetToken();
            obj = parseList();

            if (m_posMap != null && obj != null)
            {
               m_posMap.put(obj, pos);
            }

            return obj;

         case TOKEN_CPAREN:
            fail("err.parser.unexpectedToken", new Object[]{")"}, getCurTokenPos());
            forgetToken();
            return null;

         case TOKEN_PERIOD:
            fail("err.parser.unexpectedToken", new Object[]{"."}, getCurTokenPos());
            forgetToken();
            return null;

         case TOKEN_QUOTE:
            if (m_posMap != null)
            {
               pos = getCurTokenPos();
            }
            
            obj = parsePrefix(Symbol.QUOTE);

            if (m_posMap != null)
            {
               m_posMap.put(obj, pos);
            }

            return obj;

         case TOKEN_QUASIQUOTE:
            if (m_posMap != null)
            {
               pos = getCurTokenPos();
            }

            obj = parsePrefix(Symbol.QUASIQUOTE);

            if (m_posMap != null)
            {
               m_posMap.put(obj, pos);
            }

            return obj;

         case TOKEN_UNQUOTE:
            if (m_posMap != null)
            {
               pos = getCurTokenPos();
            }

            obj = parsePrefix(Symbol.UNQUOTE);

            if (m_posMap != null)
            {
               m_posMap.put(obj, pos);
            }

            return obj;

         case TOKEN_UNQUOTESPLICING:
            if (m_posMap != null)
            {
               pos = getCurTokenPos();
            }

            obj = parsePrefix(Symbol.UNQUOTE_SPLICING);

            if (m_posMap != null)
            {
               m_posMap.put(obj, pos);
            }

            return obj;

         case TOKEN_GLOBAL:
            if (m_posMap != null)
            {
               pos = getCurTokenPos();
            }

            obj = parsePrefix(Symbol.GLOBAL);

            if (m_posMap != null)
            {
               m_posMap.put(obj, pos);
            }

            return obj;

         case TOKEN_SHARPPAREN:
            if (m_posMap != null)
            {
               pos = getCurTokenPos();
            }

            forgetToken();
            obj = parseVector();

            if (m_posMap != null)
            {
               m_posMap.put(obj, pos);
            }

            return obj;

         case TOKEN_ATOM:
            if (m_posMap != null && m_nListDepth == 0)
            {
               pos = getCurTokenPos();
            }

            forgetToken();

            if (m_posMap != null && m_nListDepth == 0)
            {
               m_posMap.put(m_tokenValue, pos);
            }

            return m_tokenValue;

         case TOKEN_BYTEVECTOR:
            if (m_posMap != null)
            {
               pos = getCurTokenPos();
            }

            forgetToken();
            obj = parseByteVector();

            if (m_posMap != null)
            {
               m_posMap.put(obj, pos);
            }

            return obj;

         case TOKEN_SYNTAX:
            if (m_posMap != null)
            {
               pos = getCurTokenPos();
            }
            
            obj = parsePrefix(Symbol.SYNTAX);

            if (m_posMap != null)
            {
               m_posMap.put(obj, pos);
            }

            return obj;

         case TOKEN_QUASISYNTAX:
            if (m_posMap != null)
            {
               pos = getCurTokenPos();
            }

            obj = parsePrefix(Symbol.QUASISYNTAX);

            if (m_posMap != null)
            {
               m_posMap.put(obj, pos);
            }

            return obj;

         case TOKEN_UNSYNTAX:
            if (m_posMap != null)
            {
               pos = getCurTokenPos();
            }

            obj = parsePrefix(Symbol.UNSYNTAX);

            if (m_posMap != null)
            {
               m_posMap.put(obj, pos);
            }

            return obj;

         case TOKEN_UNSYNTAXSPLICING:
            if (m_posMap != null)
            {
               pos = getCurTokenPos();
            }

            obj = parsePrefix(Symbol.UNSYNTAX_SPLICING);

            if (m_posMap != null)
            {
               m_posMap.put(obj, pos);
            }

            return obj;

         default:
            return EOF;
      }
   }

   /**
    * Parses a Scheme list.
    * @return The first list pair.
    * @throws ParserException if a syntax error has been encountered.
    */
   protected Pair parseList()
   {
      Pair head = null;
      Pair tail = null;

      ++m_nListDepth;

      for (;;)
      {
         switch (getCurToken())
         {
            case TOKEN_EOF:
               fail("err.parser.listEOF", null, getCurTokenPos());

            case TOKEN_CPAREN:
               forgetToken();
               --m_nListDepth;
               return head;

            case TOKEN_PERIOD:
               if (head == null)
               {
                  fail("err.parser.unexpectedPeriod", null, getCurTokenPos());
                  forgetToken();
                  break;
               }

               forgetToken();
               tail.m_tail = parseElement();

               if (getCurToken() != TOKEN_CPAREN)
               {
                  fail("err.parser.missingCParen", null, getCurTokenPos());
                  tail.m_tail = new ConstPair(tail.m_tail);
                  tail = (Pair)tail.m_tail;
                  break;
               }

               forgetToken();
               --m_nListDepth;

               return head;

            default:
               if (head == null)
               {
                  head = tail = new ConstPair(parseElement());
               }
               else
               {
                  tail.m_tail = new ConstPair(parseElement());
                  tail = (Pair)tail.m_tail;
               }

               break;
         }
      }
   }

   /**
    * Parses a Scheme vector.
    * @return The vector object.
    * @throws ParserException if a syntax error has been encountered.
    */
   protected Object[] parseVector()
   {
      List elementList = new ArrayList();

      ++m_nListDepth;

      for (;;)
      {
         switch (getCurToken())
         {
            case TOKEN_EOF:
               fail("err.parser.vectorEOF", null, getCurTokenPos());

            case TOKEN_CPAREN:
               forgetToken();
               --m_nListDepth;
               return elementList.toArray();

            default:
               elementList.add(parseElement());
               break;
         }
      }
   }

   /**
    * Parses an expression with a prefix token.
    * @return The first list pair.
    * @throws ParserException if a syntax error has been encountered.
    */
   protected Pair parsePrefix(Symbol symbol)
   {
      if (getNextToken() == TOKEN_EOF)
      {
         fail("err.parser.prefixEOF", new Object[]{symbol.getName()}, getCurTokenPos());

         return null;
      }

      return new ConstPair(symbol, new ConstPair(parseElement()));
   }

   /**
    * Parses a Scheme bytevector.
    * @return The bytevector object.
    * @throws ParserException if a syntax error has been encountered.
    */
   protected byte[] parseByteVector()
   {
      List elementList = new ArrayList();

      ++m_nListDepth;

      for (;;)
      {
         switch (getCurToken())
         {
            case TOKEN_EOF:
               fail("err.parser.vectorEOF", null, getCurTokenPos());

            case TOKEN_CPAREN:
               forgetToken();
               --m_nListDepth;

               int nLength = elementList.size();
               byte[] byteArray = new byte[nLength];

               for (int i = 0; i < nLength; i++)
               {
                  byteArray[i] = ((Integer)elementList.get(i)).byteValue();
               }

               return byteArray;

            default:
               Object element = parseElement();

               if (!(element instanceof Integer))
               {
                  fail("err.parser.invalidNumber", null, getCurTokenPos());
               }

               elementList.add(element);
         }
      }
   }

   /**
    * Parses the next token out of the stream without assigning it to m_nToken.
    * @return One of the TOKEN_* constants.
    */
   protected int parseToken()
   {
      for (;;)
      {
         skipToToken();

         Symbol sym;
         String sName;

         switch (m_ch)
         {
            case CHAR_EOF:
               return TOKEN_EOF;

            case ';':
               int nToken = parseSemicolon();

               if (nToken == TOKEN_NONE)
               {
                  continue;
               }

               return nToken;

            case '(':
               forgetChar();
               return TOKEN_OPAREN;

            case ')':
               forgetChar();
               return TOKEN_CPAREN;

            case '\'':
               forgetChar();
               return TOKEN_QUOTE;

            case '`':
               forgetChar();
               return TOKEN_QUASIQUOTE;

            case ',':
               if (getNextChar() == '@')
               {
                  forgetChar();
                  return TOKEN_UNQUOTESPLICING;
               }

               return TOKEN_UNQUOTE;

            case '"':
               m_tokenValue = parseString();
               return TOKEN_ATOM;

            case '#':
               switch (getNextChar())
               {
                  case '#':
                     return parseSharpGlobal();

                  case '(':
                     return parseSharpParen();

                  case 'v':
                  case 'V':
                     return parseSharpV();

                  case 't':
                  case 'T':
                     forgetChar();
                     m_tokenValue = Boolean.TRUE;
                     return TOKEN_ATOM;

                  case 'f':
                  case 'F':
                     forgetChar();
                     m_tokenValue = Boolean.FALSE;
                     return TOKEN_ATOM;

                  case '\\':
                     forgetChar();
                     m_tokenValue = parseChar();
                     return TOKEN_ATOM;

                  case 'b':
                  case 'B':
                     forgetChar();
                     m_tokenValue = parseNumber(2, 0, parseExactness(), false);
                     return TOKEN_ATOM;

                  case 'o':
                  case 'O':
                     forgetChar();
                     m_tokenValue = parseNumber(8, 0, parseExactness(), false);
                     return TOKEN_ATOM;

                  case 'd':
                  case 'D':
                     forgetChar();
                     m_tokenValue = parseNumber(10, 0,  parseExactness(), false);
                     return TOKEN_ATOM;

                  case 'x':
                  case 'X':
                     forgetChar();
                     m_tokenValue = parseNumber(16, 0, parseExactness(), false);
                     return TOKEN_ATOM;

                  case 'e':
                  case 'E':
                     forgetChar();
                     m_tokenValue = parseNumber(parseRadix(0), 0, 1, false);
                     return TOKEN_ATOM;

                  case 'i':
                  case 'I':
                     forgetChar();
                     m_tokenValue = parseNumber(parseRadix(0), 0, 2, false);
                     return TOKEN_ATOM;

                  case 'm':
                  case 'M':
                     forgetChar();
                     m_tokenValue = parseTimestamp();
                     return TOKEN_ATOM;

                  case 'z':
                  case 'Z':
                     forgetChar();
                     m_tokenValue = parseBinary();
                     return TOKEN_ATOM;

                  case '\'':
                     forgetChar();
                     return TOKEN_SYNTAX;

                  case '`':
                     forgetChar();
                     return TOKEN_QUASISYNTAX;

                  case ',':
                     if (getNextChar() == '@')
                     {
                        forgetChar();
                        return TOKEN_UNSYNTAXSPLICING;
                     }

                     return TOKEN_UNSYNTAX;

                  default:
                     return parseSharpChar();
               }

            case '+':
               switch (getNextChar())
               {
                  case '0':
                  case '1':
                  case '2':
                  case '3':
                  case '4':
                  case '5':
                  case '6':
                  case '7':
                  case '8':
                  case '9':
                     m_tokenValue = parseNumber(0, 1, 0, false);
                     return TOKEN_ATOM;
               }

               m_tokenBuf.setLength(0);
               m_tokenBuf.append('+');
               sym = parseSymbol(false);
               sName = sym.getName();

               if (sName.equalsIgnoreCase("+inf.0"))
               {
                  m_tokenValue = Primitive.POSITIVE_INF_DOUBLE;
               }
               else if (sName.equalsIgnoreCase("+nan.0"))
               {
                  m_tokenValue = Primitive.NAN_DOUBLE;
               }
               else
               {
                  m_tokenValue = sym;
               }

               return TOKEN_ATOM;

            case '-':
               switch (getNextChar())
               {
                  case '0':
                  case '1':
                  case '2':
                  case '3':
                  case '4':
                  case '5':
                  case '6':
                  case '7':
                  case '8':
                  case '9':
                     m_tokenValue = parseNumber(0, -1, 0, false);
                     return TOKEN_ATOM;
               }

            	m_tokenBuf.setLength(0);
            	m_tokenBuf.append('-');
               sym = parseSymbol(false);
               sName = sym.getName();

               if (sName.equalsIgnoreCase("-inf.0"))
               {
                  m_tokenValue = Primitive.NEGATIVE_INF_DOUBLE;
               }
               else if (sName.equalsIgnoreCase("-nan.0"))
               {
                  m_tokenValue = Primitive.NAN_DOUBLE;
               }
               else
               {
                  m_tokenValue = sym;
               }

            return TOKEN_ATOM;

            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
               m_tokenValue = parseNumber(0, 1, 0, false);
               return TOKEN_ATOM;

            case '.':
               switch (getNextChar())
               {
                  case '0':
                  case '1':
                  case '2':
                  case '3':
                  case '4':
                  case '5':
                  case '6':
                  case '7':
                  case '8':
                  case '9':
                     m_tokenValue = parseNumber(10, 1, 2, true);
                     return TOKEN_ATOM;

                  case '+':
                  case '-':
                  case '.':
                  case '*':
                  case '/':
                  case '<':
                  case '>':
                  case '=':
                  case '!':
                  case '?':
                  case ':':
                  case '$':
                  case '%':
                  case '_':
                  case '~':
                  case '&':
                  case '|':
                  case '^':
                  case '@':
                     m_tokenBuf.setLength(0);
                     m_tokenBuf.append('.');
                     m_tokenValue = parseSymbol(false);
                     return TOKEN_ATOM;

                  default:
                     if (Character.isLetter((char)m_ch))
                     {
                        m_tokenBuf.setLength(0);
                        m_tokenBuf.append('.');
                        m_tokenValue = parseSymbol(false);
                        return TOKEN_ATOM;
                     }

                     break;
               }

               return TOKEN_PERIOD;

            case '*':
            case '/':
            case '<':
            case '>':
            case '=':
            case '!':
            case '?':
            case ':':
            case '$':
            case '%':
            case '_':
            case '~':
            case '&':
            case '|':
            case '^':
            case '@':
               m_tokenValue = parseSymbol(true);
               return TOKEN_ATOM;

            default:
               if (Character.isLetter((char)m_ch))
               {
                  m_tokenValue = parseSymbol(true);
                  return TOKEN_ATOM;
               }

               if (m_ch == 0xFEFF || m_ch == 0xFFFE)
               {
                  forgetChar();
                  continue;
               }

               return parseOtherChar();
         }
      }
   }

   /**
    * Invoked before a token is parsed.
    */
   protected void skipToToken()
   {
      while (Character.isWhitespace((char)getCurChar()))
      {
         forgetChar();
      }

      if (m_textPosReader != null)
      {
         m_nTokenLine = m_textPosReader.getTextPosition().getLine();
         m_nTokenColumn = m_textPosReader.getTextPosition().getColumn();
      }
   }

   /**
    * Invoked when ; has been parsed
    * @return One of the TOKEN_* constants, including TOKEN_NONE.
    */
   protected int parseSemicolon()
   {
      if (m_bCommenting && m_nListDepth == 0)
      {
         m_sComment = parseComment();
      }
      else
      {
         skipToEOL();
      }

      return TOKEN_NONE;
   }

   /**
    * Invoked when ## has been parsed.
    * @return One of the TOKEN_* constants.
    */
   protected int parseSharpGlobal()
   {
      forgetChar();

      return TOKEN_GLOBAL;
   }
   
   /**
    * Invoked when #( has been parsed.
    * @return One of the TOKEN_* constants.
    */
   protected int parseSharpParen()
   {
      forgetChar();

      return TOKEN_SHARPPAREN;
   }

   /**
    * Called with #vu8( has been parsed.
    * @return A TOKEN_ code
    */
   protected int parseSharpVU8()
   {
      forgetChar();

      return TOKEN_BYTEVECTOR;
   }

   /**
    * Called when #v has been parsed.
    * @return A TOKEN_ code
    */
   protected int parseSharpV()
   {
      String sValue = "#v";

      switch (getNextChar())
      {
         case 'u':
         case 'U':
            if (getNextChar() == '8')
            {
               if (getNextChar() == '(')
               {
                  return parseSharpVU8();
               }

               sValue = "#vu8";
            }
            else
            {
               sValue = "#vu";
            }

         default:
            return invalidSharpVU8(sValue);
      }
   }

   /**
    * Called from the default case of the sharpV character token parsing.
    * @param sValue 
    * @return A TOKEN_ code
    */
   protected int invalidSharpVU8(String sValue)
   {
      fail("err.parser.invalidCharacter", new Object[]{sValue}, getCurTokenPos());
      forgetChar();
      m_tokenValue = Primitive.ZERO_INTEGER;

      return TOKEN_ATOM;
   }

   /**
    * Invoked from the default case of the sharp character token parsing.
    * Note: the current character is the one after the sharp character.
    * @return One of the TOKEN_* constants.
    */
   protected int parseSharpChar()
   {
      fail("err.parser.invalidSharpChar", null, getCurTokenPos());
      forgetChar();
      m_tokenValue = Primitive.ZERO_INTEGER;

      return TOKEN_ATOM;
   }

   /**
    * Invoked from the default case of the top level token parsing.
    * @return One of the TOKEN_* constants.
    */
   protected int parseOtherChar()
   {
      fail("err.parser.invalidChar", null, getCurTextPosition());
      forgetChar();
      m_tokenValue = Primitive.ZERO_INTEGER;

      return TOKEN_ATOM;
   }

   /**
    * Parse a multiline comment.
    * @return The parsed out comment.
    */
   protected String parseComment()
   {
      boolean bStart = true;

      m_tokenBuf.setLength(0);

      for (;;)
      {
         getNextChar();

         if (m_ch == CHAR_EOF)
         {
            break;
         }

         if (bStart)
         {
            if (m_ch == ';')
            {
               continue;
            }

            bStart = false;

            if (m_ch == ' ' || m_ch == '\t')
            {
               continue;
            }
         }

         if (m_ch == '\r' || m_ch == '\n')
         {
            m_tokenBuf.append('\n');

            if (m_ch == '\r')
            {
               getNextChar();
            }

            if (m_ch == '\n')
            {
               getNextChar();
            }

            while (m_ch != '\n' && m_ch != '\r' && Character.isWhitespace(m_ch))
            {
               getNextChar();
            }

            if (m_ch == ';')
            {
               bStart = true;
               continue;
            }

            break;
         }

         m_tokenBuf.append((char)m_ch);
      }

      int chPrev = CHAR_EOF;

      for (int i = 0, n = m_tokenBuf.length(); i != n; ++i)
      {
         char ch = m_tokenBuf.charAt(i);

         if (chPrev == '\n' || chPrev == CHAR_EOF)
         {
            if (ch == '@')
            {
               if (n - i >= 7 && m_tokenBuf.charAt(i + 1) == 'e')
               {
                  break;
               }
            }
            else if (chPrev != CHAR_EOF)
            {
               m_tokenBuf.setCharAt(i - 1, ' ');

               if (ch == '\n')
               {
                  chPrev = ' ';
                  continue;
               }
            }
         }

         chPrev = ch;
      }

      return m_tokenBuf.substring(0);
   }

   /**
    * Parses a character code #\c.
    * @return The parsed out character.
    */
   protected Character parseChar()
   {
      m_tokenBuf.setLength(0);

      if (getCurChar() == '\\')
      {
         if (Character.isWhitespace((char)getNextChar()) || m_ch == CHAR_EOF ||
            m_ch == '(' || m_ch == ')' || m_ch == ';' || m_ch == '"')
         {
            return Primitive.createCharacter('\\');
         }

         return Primitive.createCharacter(parseCharEscape());
      }

      if (Character.isLetter((char)m_ch))
      {
         m_tokenBuf.append((char)m_ch);

         for (;;)
         {
            markReader1();

            if (Character.isLetter((char)getNextChar()))
            {
               m_tokenBuf.append((char)m_ch);
            }
            else
            {
               resetReader();
               break;
            }
         }

         if (m_tokenBuf.length() == 1)
         {
            return Primitive.createCharacter(m_tokenBuf.charAt(0));
         }

         String s = m_tokenBuf.substring(0);

         if (s.compareToIgnoreCase("space") == 0)
         {
            return Primitive.createCharacter(' ');
         }
         else if (s.compareToIgnoreCase("newline") == 0)
         {
            return Primitive.createCharacter('\n');
         }
         else
         {
            fail("err.parser.invalidSharpChar", null, getCurTokenPos());
            return Primitive.createCharacter(' ');
         }
      }
      else if (m_ch == CHAR_EOF)
      {
         fail("err.parser.invalidSharpChar", null, getCurTokenPos());
         return Primitive.createCharacter(' ');
      }
      else
      {
         Character ch = Primitive.createCharacter(m_ch);

         forgetChar();

         return ch;
      }
   }
}
