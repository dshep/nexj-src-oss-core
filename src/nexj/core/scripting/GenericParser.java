// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.io.IOException;
import java.io.Reader;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;
import java.util.Locale;

import nexj.core.meta.Primitive;
import nexj.core.util.Binary;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.TextPosition;
import nexj.core.util.TextPositionReader;

/**
 * Base for parser implementations.
 */
public abstract class GenericParser implements Parser
{
   // constants

   // ... character constants

   /**
    * Special value to indicate that no current char is stored.
    */
   protected final static int CHAR_NONE = -2;

   /**
    * The special End-Of-File char.
    */
   protected final static int CHAR_EOF = -1;

   // ... token constants

   /**
    * Special value to indicate that no token is stored.
    */
   protected final static int TOKEN_NONE = -2;

   /**
    * End-Of-File token.
    */
   protected final static int TOKEN_EOF = -1;

   // attributes

   /**
    * The current char.
    */
   protected int m_ch = CHAR_NONE;

   /**
    * The current token.
    */
   protected int m_nToken = TOKEN_NONE;
   
   /**
    * The token value.
    */
   protected Object m_tokenValue;
   
   /**
    * The token start line.
    */
   protected int m_nTokenLine;
   
   /**
    * The token start column.
    */
   protected int m_nTokenColumn;
   
   /**
    * Depth of the currently parsed list.
    */
   protected int m_nListDepth;
   
   /**
    * Comment in front of the last top-level S-expression.
    */
   protected String m_sComment;

   /**
    * The top-level comment parsing flag.
    */
   protected boolean m_bCommenting;

   // associations

   /**
    * The token buffer.
    */
   protected StringBuffer m_tokenBuf = new StringBuffer(128);
   
   /**
    * The global environment for storing symbols.
    */
   protected GlobalEnvironment m_globalEnv;

   /**
    * The input text stream to parse.
    */
   protected Reader m_reader;
   
   /**
    * The reader cast to TextPositionReader, or null if the cast failed.
    */
   protected TextPositionReader m_textPosReader;

   /**
    * The node to text position map: TextPosition[Object]
    */
   protected Lookup m_posMap;
   
   /**
    * The error list: ParserError[].
    * If present, parser errors are collected here instead of being
    * thrown as exceptions.
    */
   protected List m_errorList;

   // constructors

   /**
    * Create a parser with a given global environment for interning symbols.
    * @param globalEnv The global environment, into which to store the symbols.
    */ 
   public GenericParser(GlobalEnvironment globalEnv)
   {
      m_globalEnv = globalEnv;
   }

   // operations

   /**
    * Sets the global environment.
    * @param globalEnv The global environment to set.
    */
   public void setGlobalEnvironment(GlobalEnvironment globalEnv)
   {
      m_globalEnv = globalEnv;
   }

   /**
    * @return The global environment.
    */
   public GlobalEnvironment getGlobalEnvironment()
   {
      return m_globalEnv;
   }

   /**
    * Sets the parser error list.
    * @param errorList The parser error list to set.
    */
   public void setErrorList(List errorList)
   {
      m_errorList = errorList;
   }

   /**
    * @return The parser error list.
    */
   public List getErrorList()
   {
      return m_errorList;
   }

   /**
    * Sets the top-level comment parsing flag.
    * @param bCommenting The top-level comment parsing flag to set.
    */
   public void setCommenting(boolean bCommenting)
   {
      m_bCommenting = bCommenting;
   }

   /**
    * @return The top-level comment parsing flag.
    */
   public boolean isCommenting()
   {
      return m_bCommenting;
   }
   
   /**
    * @return The last top-level comment (in commenting mode only).
    */
   public String getComment()
   {
      return m_sComment;
   }

   /**
    * @see nexj.core.scripting.Parser#parse(java.io.Reader, nexj.core.util.Lookup)
    */
   public Object parse(Reader reader, Lookup posMap)
   {
      m_reader = reader;
      m_posMap = posMap;

      if (reader instanceof TextPositionReader)
      {
         m_textPosReader = (TextPositionReader)reader;
      }
      else
      {
         m_posMap = null;
         m_textPosReader = null;
      }

      m_ch = CHAR_NONE;
      m_nToken = TOKEN_NONE;
      m_nTokenLine = 0;
      m_nTokenColumn = 0;
      m_nListDepth = 0;
      m_sComment = null;

      return parseElement();
   }

   /**
    * Parses a single syntactic element.
    * @return The internal representation of the element.
    * @throws ParserException if a syntax error has been encountered.
    */
   protected abstract Object parseElement();

   /**
    * @return Current token text position.
    */
   protected TextPosition getCurTokenPos()
   {
      return new TextPosition(m_nTokenLine, m_nTokenColumn, 
         (m_textPosReader != null) ? m_textPosReader.getTextPosition().getURL() : null);
   }

   /**
    * @return A clone of the current reader text position.
    */
   protected TextPosition getCurTextPosition()
   {
      if (m_textPosReader != null)
      {
         return (TextPosition)m_textPosReader.getTextPosition().clone();
      }

      return null;
   }

   /**
    * Saves the current reader position with read-ahead buffer of 1 character,
    * if no list is being parsed.
    */
   protected void markReader1()
   {
      if (m_nListDepth == 0)
      {
         try
         {
            m_reader.mark(1);
         }
         catch (IOException e)
         {
            throw new ParserException("err.parser.ioError",
               new Object[]{ObjUtil.getMessage(e)}, e, getCurTextPosition());
         }
      }
   }

   /**
    * Restores the current reader position with read-ahead buffer of 1 character,
    * if no list is being parsed.
    */
   protected void resetReader()
   {
      if (m_nListDepth == 0)
      {
         try
         {
            m_reader.reset();
         }
         catch (IOException e)
         {
            throw new ParserException("err.parser.ioError",
               new Object[]{ObjUtil.getMessage(e)}, e, getCurTextPosition());
         }

         forgetChar();
      }
   }

   /**
    * Returns the current character. If there is no
    * current character, gets one from the stream.
    * @return Character code, or one of the CHAR_* constants.
    */
   protected int getCurChar()
   {
      if (m_ch == CHAR_NONE)
      {
         try
         {
            m_ch = m_reader.read();
         }
         catch (IOException e)
         {
            throw new ParserException("err.parser.ioError",
               new Object[]{ObjUtil.getMessage(e)}, e, getCurTextPosition());
         }
      }

      return m_ch;
   }

   /**
    * Sets the current character to NONE, so that the next getCurChar
    * reads a new character form the stream.
    */
   protected void forgetChar()
   {
      if (m_ch != CHAR_EOF)
      {
         m_ch = CHAR_NONE;
      }
   }

   /**
    * Gets the next character from the stream.
    * @return The next character from the stream.
    */ 
   protected int getNextChar()
   {
      forgetChar();
      return getCurChar();
   }

   /**
    * Returns the current token. If there is no current token,
    * parses the stream until one is found.
    * @return One of the TOKEN_* constants.
    */
   protected int getCurToken()
   {
      if (m_nToken == TOKEN_NONE)
      {
         m_nToken = parseToken();
      }

      return m_nToken;
   }

   /**
    * Parses the next token out of the stream without assigning it to m_nToken.
    * @return One of the TOKEN_* constants.
    */ 
   protected abstract int parseToken();

   /**
    * Sets the current token to NONE, so that the next getCurToken
    * parses a new token.
    */
   protected void forgetToken()
   {
      if (m_nToken != TOKEN_EOF)
      {
         m_nToken = TOKEN_NONE;
      }
   }

   /**
    * Parses the next token out of the stream.
    * @return One of the TOKEN_* constants.
    */
   protected int getNextToken()
   {
      forgetToken();
      return getCurToken();
   }

   /**
    * Skips characters until end-of-line is reached.
    */
   protected void skipToEOL()
   {
      for (;;)
      {
         getNextChar();

         if (m_ch == '\r' ||
            m_ch == '\n' ||
            m_ch == CHAR_EOF)
         {
            break;
         }
      }
   }
   
   /**
    * Parses a string with character escapes.
    * @return The parsed out string.
    */
   protected String parseString()
   {
      forgetChar();
      m_tokenBuf.setLength(0);

      while (getCurChar() != '"')
      {
         if (m_ch == CHAR_EOF)
         {
            fail("err.parser.unterminatedString", null, getCurTextPosition()); 
         }
         else if (m_ch == '\\')
         {
            forgetChar();
            m_tokenBuf.append(parseCharEscape());
         }
         else
         {
            m_tokenBuf.append((char)m_ch);
            forgetChar();
         }
      }
               
      forgetChar();
      return m_tokenBuf.substring(0);
   }

   /**
    * Parses a character escape sequence (characters following a backslash \).
    * @return The parsed out character.
    */
   protected char parseCharEscape()
   {
      switch (getCurChar())
      {
         case '\\':
            forgetChar();
            return '\\'; 
         
         case '\'':
            forgetChar();
            return '\''; 
         
         case '\"':
            forgetChar();
            return '\"'; 

         case 'b':
            forgetChar();
            return '\b';

         case 't':
            forgetChar();
            return '\t';

         case 'n':
            forgetChar();
            return '\n';

         case 'r':
            forgetChar();
            return '\r';
                           
         case 'u':
            forgetChar();
            return parseUnicodeEscape();

         case '0':
         case '1':
         case '2':
         case '3':
         case '4':
         case '5':
         case '6':
         case '7':
            return parseOctalEscape();

         default:
            fail("err.parser.invalidCharEscape", null, getCurTextPosition());
            forgetChar();
            return ' ';
      }
   }
   
   /**
    * Parses an octal character escape \\###.
    * @return The parsed out character.
    */
   protected char parseOctalEscape()
   {
      int i, n;
               
      for (i = 0; i < 3; ++i)
      {
         m_tokenBuf.append((char)m_ch);
                  
         if (i < 2)
         {
            markReader1();
            getNextChar();
                     
            if (m_ch < '0' || m_ch > '7')
            {
               resetReader();
               break;
            }
         }
      }
               
      try
      {
         n = Integer.parseInt(m_tokenBuf.substring(m_tokenBuf.length() - i), 8);
      }
      catch (Exception e)
      {
         fail("err.parser.invalidCharEscape", null, getCurTextPosition());
         n = ' ';
      }
               
      if (n > 0377)
      {
         fail("err.parser.invalidCharEscape", null, getCurTextPosition()); 
      }
               
      m_tokenBuf.setLength(m_tokenBuf.length() - i);

      return (char)n;
   }
   
   /**
    * Parses a unicode character escape \\u####.
    * @return The parsed out character.
    */
   protected char parseUnicodeEscape()
   {
      int i, n;
               
      for (i = 0; i < 4; ++i)
      {
         if (Character.digit((char)getNextChar(), 16) < 0)
         {
            fail("err.parser.invalidCharEscape", null, getCurTextPosition());
            m_ch = '0'; 
         }

         m_tokenBuf.append((char)m_ch);
      }

      forgetChar();
               
      try
      {
         n = Integer.parseInt(m_tokenBuf.substring(m_tokenBuf.length() - i), 16);
      }
      catch (Exception e)
      {
         fail("err.parser.invalidCharEscape", null, e, getCurTextPosition());
         n = ' ';
      }

      m_tokenBuf.setLength(m_tokenBuf.length() - i);

      return (char)n;
   }

   
   /**
    * Parses an identifier.
    * @param bResetBuf True to reset the token buffer and get the next character. 
    * @return The parsed out identifier.
    */
   protected String parseIdentifier(boolean bResetBuf)
   {
      if (bResetBuf)
      {
         m_tokenBuf.setLength(0);
         m_tokenBuf.append((char)getCurChar());
      }

      for (;;)
      {
         markReader1();

         if (bResetBuf)
         {
            getNextChar();
         }
         else
         {
            bResetBuf = true;
         }

         switch (m_ch)
         {
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
               m_tokenBuf.append((char)m_ch);
               continue;

            default:
               if (Character.isLetterOrDigit((char)m_ch))
               {
                  m_tokenBuf.append((char)m_ch);
                  continue;
               }

               resetReader();
               break;
         }

         return m_tokenBuf.substring(0);
      }
   }

   /**
    * Parses a symbol.
    * @param bResetBuf True to reset the token buffer and get the next character. 
    * @return The parsed out symbol.
    */
   protected Symbol parseSymbol(boolean bResetBuf)
   {
      String sIdentifier = parseIdentifier(bResetBuf);

      if (m_globalEnv.isOptionSet(GlobalEnvironment.OPTION_CONVERT_SYMBOLS))
      {
         sIdentifier = sIdentifier.toLowerCase(Locale.ENGLISH);
      }

      return Symbol.define(sIdentifier);
   }

   /**
    * Parses a number radix.
    * @param nDefaultRadix The default radix to be returned.
    * @return The parsed out radix value, or nDefaultRadix if none.
    */
   protected int parseRadix(int nDefaultRadix)
   {
      if (getCurChar() == '#')
      {
         forgetChar();

         switch (getCurChar())
         {
            case 'b':
            case 'B':
               forgetChar();
               return 2;

            case 'o':
            case 'O':
               forgetChar();
               return 8;

            case 'd':
            case 'D':
               forgetChar();
               return 10;

            case 'x':
            case 'X':
               forgetChar();
               return 16;

            default:
               invalidNumber();
         }
      }

      return nDefaultRadix;
   }
   
   /**
    * Parses a number exactness.
    * @return The parsed out exactness value, 0, 1 or 2 (@see parseNumber).
    */
   protected int parseExactness()
   {
      if (getCurChar() == '#')
      {
         forgetChar();

         switch (getCurChar())
         {
            case 'e':
            case 'E':
               forgetChar();
               return 1;

            case 'i':
            case 'I':
               forgetChar();
               return 2;

            default:
               invalidNumber();
         }
      }
      
      return 0;
   }
   
   /**
    * Parses a number.
    * @param nRadix The number radix, 0 means that it has to be determined from prefix.
    * @param nSign The sign: -1, 1; 0 means that it has to be determined.
    * @param nExact The exactness: 1 = exact, 2 = inexact; 0 means that it has to be determined.
    * @param bDecPoint True if a decimal point has been encountered as a first char.
    * @return The parsed out number.
    */
   protected Number parseNumber(int nRadix, int nSign, int nExact, boolean bDecPoint)
   {
      int nHash = 0;
      
      m_tokenBuf.setLength(0);

      if (nSign == 0)
      {
         switch (getCurChar())
         {
            case '+':
               forgetChar();
               nSign = 1;
               break;
            
            case '-':
               forgetChar();
               nSign = -1;
               break;
         }
      }
      
      if (nSign < 0)
      {
         m_tokenBuf.append('-');
      }

      if (bDecPoint)
      {
         m_tokenBuf.append('.');
         nHash = 1;
      }
      else
      {
         if (nRadix == 0)
         {
            if (getCurChar() == '0')
            {
               markReader1();
               
               switch (getNextChar())
               {
                  case 'x':
                  case 'X':
                     forgetChar();
                     nRadix = 16;
                     break;
                  
                  default:
                     if (m_ch >= '0' && m_ch <= '9')
                     {
                        nRadix = 8;
                     }
                     else
                     {
                        nRadix = 10;
                        m_tokenBuf.append('0');
                     }
                     
                     break;
               }
            }
            else
            {
               nRadix = 10;
            }
         }

         while (Character.digit((char)getCurChar(), nRadix) >= 0 || m_ch == '#')
         {
            if (m_ch == '#')
            {
               if (nHash == 0 || nRadix != 10 || nExact == 1)
               {
                  invalidNumber();
               }

               m_tokenBuf.append('0');
               nExact = 2;
               nHash = 2;
            }
            else
            {
               if (nHash == 2)
               {
                  invalidNumber();
               }
               
               m_tokenBuf.append((char)m_ch);
               nHash = 1;
            }

            forgetChar();
            markReader1();
         }

         if (getCurChar() == '.')
         {
            if (nRadix != 10)
            {
               invalidNumber();
            }
            
            if (nHash == 0)
            {
               nHash = 1;
            }

            m_tokenBuf.append('.');
            forgetChar();
            markReader1();
            bDecPoint = true;
         }
      }
      
      if (bDecPoint)
      {
         while (getCurChar() >= '0' && m_ch <= '9' || m_ch == '#')
         {
            if (m_ch == '#')
            {
               if (nHash == 0 || nExact == 1)
               {
                  invalidNumber();
               }

               m_tokenBuf.append('0');
               nExact = 2;
               nHash = 2;
            }
            else
            {
               if (nHash == 2)
               {
                  invalidNumber();
               }
               
               m_tokenBuf.append((char)m_ch);
               nHash = 1;
            }

            forgetChar();
            markReader1();
         }
      }
      
      // parse the exponent
      
      char ch = ' ';

      switch (getCurChar())
      {
         case 'e':
         case 'E':
         case 's':
         case 'S':
         case 'f':
         case 'F':
         case 'd':
         case 'D':
         case 'l':
         case 'L':
         case 'n':
         case 'N':
            ch = (char)m_ch;
            markReader1();
            getNextChar();
            
            if (m_ch == '+' || m_ch == '-' || m_ch >= '0' && m_ch <= '9')
            {
               bDecPoint = true;
               m_tokenBuf.append('e');
               
               if (m_ch == '+' || m_ch == '-')
               {
                  m_tokenBuf.append((char)m_ch);
                  getNextChar();
               }

               if (m_ch < '0' || m_ch > '9')
               {
                  invalidNumber();
               }

               do
               {
                  m_tokenBuf.append((char)m_ch);
                  markReader1();
                  getNextChar();
               }
               while (m_ch >= '0' && m_ch <= '9');
               
               switch (m_ch)
               {
                  case 'n':
                  case 'N':
                  case 'l':
                  case 'L':
                  case 'd':
                  case 'D':
                  case 'f':
                  case 'F':
                     if (ch != ' ' && ch != 'e' && ch != 'E')
                     {
                        invalidNumber();
                     }

                     ch = (char)m_ch;
                     markReader1();
                     forgetChar();
                     break;

                  default:
                     if (Character.isLetter((char)m_ch))
                     {
                        invalidNumber();
                     }
                     
                     break;
               }
            }

            break;
      }
      
      if (ch == ' ')
      {
         switch (getCurChar())
         {
            case 'n':
            case 'N':
            case 'l':
            case 'L':
            case 'd':
            case 'D':
            case 'f':
            case 'F':
               ch = (char)m_ch;
               markReader1();
               forgetChar();
               break;
            
            default:
               if (Character.isLetter((char)m_ch))
               {
                  invalidNumber();
               }
               
               break;
         }
      }

      resetReader();
      
      try
      {
         String s = m_tokenBuf.substring(0);
         
         if (bDecPoint)
         {
            if (nRadix != 10)
            {
               invalidNumber();
            }
   
            switch (ch)
            {
               case 's':
               case 'S':
               case 'f':
               case 'F':
                  if (nExact == 1)
                  {
                     invalidNumber();
                  }
               
                  return Primitive.createFloat(Float.parseFloat(s));
   
               case 'n':
               case 'N':
                  if (nExact == 2)
                  {
                     BigDecimal dec = new java.math.BigDecimal(s);
                     
                     if (dec.scale() < Primitive.MAX_SCALE)
                     {
                        dec = dec.setScale(Primitive.MAX_SCALE);
                     }
                     
                     return dec;
                  }

                  return new java.math.BigDecimal(s);
               
               default:
                  if (nExact == 1)
                  {
                     return new java.math.BigDecimal(s);
                  }

                  return Primitive.createDouble(Double.parseDouble(s));
            }
         }
         else
         {
            switch (ch)
            {
               case 'e':
               case 'E':
                  invalidNumber();

               case 's':
               case 'S':
               case 'f':
               case 'F':
                  if (nRadix != 10 || nExact == 1)
                  {
                     invalidNumber();
                  }
                  
                  return Primitive.createFloat(Float.parseFloat(s));

               case 'd':
               case 'D':
                  if (nRadix != 10 || nExact == 1)
                  {
                     invalidNumber();
                  }

                  return Primitive.createDouble(Double.parseDouble(s));

               case 'l':
               case 'L':
                  if (nExact == 2)
                  {
                     invalidNumber();
                  }

                  return Primitive.createLong(Long.parseLong(s, nRadix));

               case 'n':
               case 'N':
                  if (nRadix != 10)
                  {
                     invalidNumber();
                  }

                  if (nExact == 2)
                  {
                     BigDecimal dec = new java.math.BigDecimal(s);
                        
                     if (dec.scale() < Primitive.MAX_SCALE)
                     {
                        dec = dec.setScale(Primitive.MAX_SCALE);
                     }
                        
                     return dec;
                  }

                  return new BigDecimal(s);

               default:
                  try
                  {
                     long l = Long.parseLong(s, nRadix);
                     
                     if (nExact == 2)
                     {
                        return Primitive.createDouble(l);
                     }
                     
                     if (l >= Integer.MIN_VALUE && l <= Integer.MAX_VALUE)
                     {
                        return Primitive.createInteger((int)l);
                     }
                     
                     return Primitive.createLong(l);
                  }
                  catch (NumberFormatException e)
                  {
                     if (nRadix != 10)
                     {
                        throw e;
                     }
                     
                     BigDecimal dec = new BigDecimal(s);
                     
                     if (nExact == 2)
                     {
                        return Primitive.createDouble(dec.doubleValue());
                     }
                     
                     return dec;
                  }
            }
         }
      }
      catch (ParserException e)
      {
         throw e;
      }
      catch (Exception e)
      {
         fail("err.parser.invalidNumber", null, e, getCurTokenPos());
         return Primitive.createInteger(0);
      }
   }

   /**
    * @throws ParserException err.parser.invalidNumber
    */
   protected void invalidNumber() throws ParserException
   {
      fail("err.parser.invalidNumber", null, getCurTokenPos()); 
   } 

   /**
    * Parses out a number.
    * @param nRadix The default number radix, 0 to determine automatically.
    * @return The parsed out number.
    * @throws ParserException if the number is invalid.
    */
   protected Number parseNumber(int nRadix) throws ParserException
   {
      while (Character.isWhitespace((char)getCurChar()))
      {
         forgetChar();
      }

      if (getCurChar() == '#')
      {
         switch (getNextChar())
         {
            case 'b':
            case 'B':
               forgetChar();
               return parseNumber(2, 0, parseExactness(), false);

            case 'o':
            case 'O':
               forgetChar();
               return parseNumber(8, 0, parseExactness(), false);

            case 'd':
            case 'D':
               forgetChar();
               return parseNumber(10, 0, parseExactness(), false);

            case 'x':
            case 'X':
               forgetChar();
               return parseNumber(16, 0, parseExactness(), false);

            case 'e':
            case 'E':
               forgetChar();
               return parseNumber(parseRadix(nRadix), 0, 1, false);

            case 'i':
            case 'I':
               forgetChar();
               return parseNumber(parseRadix(nRadix), 0, 2, false);
               
            default:
               invalidNumber();
         }
      }

      return parseNumber(nRadix, 0, 0, false);
   }

   /**
    * Parses a number out of a character stream.
    * @param reader The character stream reader.
    * @param nRadix The default number radix, 0 to determine automatically.
    * @return The parsed out number.
    * @throws ParserException if the number is invalid.
    */
   public Number parseNumber(Reader reader, int nRadix) throws ParserException
   {
      m_reader = reader;
      m_posMap = null;
      m_textPosReader = null;
      m_ch = CHAR_NONE;
      m_nToken = TOKEN_NONE;
      m_nTokenLine = 0;
      m_nTokenColumn = 0;
      m_nListDepth = 0;
      
      Number num = parseNumber(nRadix);
      
      while (Character.isWhitespace((char)getCurChar()))
      {
         forgetChar();
      }
      
      if (getCurChar() != CHAR_EOF)
      {
         invalidNumber();
      } 
      
      return num;
   }

   /**
    * Parses a timestamp out of a character stream.
    * @return The parsed out timestamp.
    * @throws ParserException if the timestamp is invalid.
    */
   protected Timestamp parseTimestamp() throws ParserException
   {
      m_tokenBuf.setLength(0);

   loop:
      for (;;)
      {
         markReader1();

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
            case 't':
            case 'T':
            case '.':
            case '-':
            case ':':
               m_tokenBuf.append((char)m_ch);
               break;

            case CHAR_EOF:
               break loop;

            default:
               if (Character.isLetter((char)m_ch))
               {
                  fail("err.parser.invalidTimestamp", null, getCurTokenPos());
                  return new Timestamp(0);
               }

               resetReader();
               break loop;
         }
      }

      try
      {
         return Primitive.toTimestamp(m_tokenBuf);
      }
      catch (Exception e)
      {
         fail("err.parser.invalidTimestamp", null, getCurTokenPos());
         return new Timestamp(0);
      }
   }
   
   /**
    * Parses a binary in hexadecimal representation out of a character stream.
    * @return The parsed out binary.
    * @throws ParserException if the binary is invalid.
    */
   protected Binary parseBinary() throws ParserException
   {
      m_tokenBuf.setLength(0);

      for (;;)
      {
         markReader1();

         if (getNextChar() == CHAR_EOF)
         {
            break;
         }

         if (Character.digit((char)m_ch, 16) < 0)
         {
            if (Character.isLetter((char)m_ch))
            {
               fail("err.parser.invalidBinary", null, getCurTokenPos());
               return new Binary(new byte[]{0});
            }

            resetReader();
            break;
         }

         m_tokenBuf.append((char)m_ch);
      }

      return Binary.parse(m_tokenBuf);
   }

   /**
    * Throws or collects an error, depending on the error list availability.
    * @param sErrCode The error code.
    * @param errArgs The error arguments.
    * @param cause The error cause.
    * @param pos The text position.
    * @throws ParserException if error list is null.
    */
   protected void fail(String sErrCode, Object[] errArgs, Throwable cause, TextPosition pos) throws ParserException
   {
      if (m_errorList == null)
      {
         throw new ParserException(sErrCode, errArgs, cause, pos);
      }

      m_errorList.add(new ParserError(sErrCode, errArgs, pos));
   }
   
   /**
    * Throws or collects an error, depending on the error list availability.
    * @param sErrCode The error code.
    * @param errArgs The error arguments.
    * @param pos The text position.
    * @throws ParserException if error list is null.
    */
   protected void fail(String sErrCode, Object[] errArgs, TextPosition pos) throws ParserException
   {
      fail(sErrCode, errArgs, null, pos);
   }
}
