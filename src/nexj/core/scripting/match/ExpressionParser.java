// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.match;

import java.io.StringReader;

import nexj.core.scripting.GenericParser;
import nexj.core.scripting.Pair;
import nexj.core.scripting.ParserException;
import nexj.core.scripting.Symbol;
import nexj.core.util.TextPosition;
import nexj.core.util.TextPositionHolder;

/**
 * Class for parsing "match" operator string expressions into Pair trees.
 */
public class ExpressionParser extends GenericParser
{
   /**
    * Token denoting an AND operator.
    */
   private static final int TOKEN_AND = 1;

   /**
    * Token denoting an FUZZY operator.
    */
   private static final int TOKEN_FUZZY = 2;

   /**
    * Token denoting an NOT operator.
    */
   private static final int TOKEN_NOT = 3;

   /**
    * Token denoting an OR operator.
    */
   private static final int TOKEN_OR = 4;

   /**
    * Token denoting a value.
    */
   private static final int TOKEN_VALUE = 5;

   /**
    * Token denoting an WEIGHT operator.
    */
   private static final int TOKEN_WEIGHT = 6;

   /**
    * Is parser currently inside a subexpression, used for exception generation only.
    */
   private boolean m_bSubexpression;

   /**
    * Current read offset, used for exception generation.
    */
   private int m_nOffset;

   /**
    * Constructor.
    */
   public ExpressionParser()
   {
      super(null); // do not use global environment
   }

   /**
    * Parse a string into a Pair expression tree (not thread safe).
    * @param sExpression The expression to parse.
    * @return Parsed expression tree.
    */
   public Pair parse(String sExpression) throws ParserException
   {
      if (sExpression == null)
      {
         return null;
      }

      m_tokenBuf.setLength(0); // reset for new string
      m_bSubexpression = false; // reset for new string
      m_nOffset = 0; // reset for new string

      Object value;

      try
      {
         value = parse(new StringReader(sExpression), null);
      }
      catch (RuntimeException e) // common parent of NumberFormatException & ParserException
      {
         TextPosition pos = (e instanceof TextPositionHolder) ? ((TextPositionHolder)e).getTextPosition() : getCurTokenPos();

         throw new ParserException("err.scripting.invalidMatchExpression", new Object[]
         {
            sExpression
         }, e, pos);
      }
      finally
      {
         m_tokenBuf.setLength(0); // free memory
      }

      return (value instanceof Pair || value == null) ? (Pair)value : new Pair(value);
   }

   /**
    * @see nexj.core.scripting.GenericParser#parseElement()
    */
   protected Object parseElement()
   {
      Object value = parseOr();

      if (getCurToken() != TOKEN_EOF) // some sort of parse exception (usually missing operator)
      {
         fail(true);
      }

      return value;
   }

   /**
    * Parse a single atom.
    * @return an atomic value.
    */
   protected Object parseAtom()
   {
      int nValueType = getCurToken();

      if (nValueType == TOKEN_EOF)
      {
         return null;
      }

      Object value = m_tokenValue; // either a String or subexpression e.g. '("a")'

      // only triggered if it's the first exclusion of the expression
      if (nValueType == TOKEN_NOT) // have a unary NOT
      {
         if (getNextToken() == TOKEN_EOF) // get token after done with current buffer
         {
            fail(true); // invalid to not have RHS value
         }

         value = Pair.list(Symbol.NOT, m_tokenValue);
      }
      else if (nValueType == TOKEN_FUZZY) // have a unary FUZZY
      {
         // get token after done with current buffer (a next value is required)
         if (getNextToken() == TOKEN_EOF || !(m_tokenValue instanceof String))
         {
            fail(true); // invalid to not have RHS value
         }

         value = Pair.list(Symbol.LIKE_P, m_tokenValue);
      } // else have literal value

      forgetToken(); // token consumed

      return value;
   }

   /**
    * Parse atoms ANDed together.
    * @param value A value that should be consumed before the next input (null if none).
    * @return an atomic value.
    */
   protected Object parseAnd(Object value)
   {
      if (value == null) // only consume next value if none provided
      {
         value = parseWeight();
      }

      while(getCurToken() == TOKEN_AND || getCurToken() == TOKEN_NOT)
      {
         if (getCurToken() == TOKEN_AND)
         {
            forgetToken(); // AND token consumed
         }

         Object next = parseWeight();

         if (next == null) // invalid to end expression with "and"
         {
            fail(true);
         }

         if (!(value instanceof Pair) || !Symbol.AND.equals(((Pair)value).getHead()))
         {
            value = Pair.list(Symbol.AND, value); // do not have an AND on left
         }

         // have an AND list or some other sort of expression/value
         value = Pair.append((Pair)value,
                             (next instanceof Pair && Symbol.AND.equals(((Pair)next).getHead()))
                             ? ((Pair)next).getNext() : new Pair(next));
      }

      return value;
   }

   /**
    * Parse atoms ORed together (possibly join multiple WEIGHT atoms).
    * @return an atomic value.
    */
   protected Object parseOr()
   {
      Object value = parseAnd(null);

      while(getCurToken() == TOKEN_OR)
      {
         forgetToken(); // OR token consumed

         Object next = parseAnd(null); // NOTE: curToken now set to next operator by parseAnd()

         if (next == null) // invalid to end expression with "or"
         {
            fail(true);
         }

         // join sequential WEIGHT statements separated by OR into a single statement
         // used in jUnit tests for testing WEIGHT evaluation
         if (next instanceof Pair && Symbol.MUL.equals(((Pair)next).getHead()) && value instanceof Pair)
         {
            if (Symbol.MUL.equals(((Pair)value).getHead()))
            {
               //have a WEIGHT list to append to, push-back concatenated and get actual first value
               value = parseAnd(Pair.append((Pair)value, ((Pair)next).getNext()));

               continue;
            }

            if (Symbol.OR.equals(((Pair)value).getHead())) // check if last element of OR list is WEIGHT
            {
               Pair last = (Pair)value;

               while (last.getTail() != null)
               {
                  last = last.getNext();
               }

               if (last.getHead() instanceof Pair && // have a WEIGHT list to append to
                   Symbol.MUL.equals(((Pair)last.getHead()).getHead()))
               {
                  last.setHead(parseAnd(Pair.append((Pair)last.getHead(), // push-back concatenated
                                                    ((Pair)next).getNext()))); // then set as head

                  continue;
               }
            }
         }

         // else regular OR expression
         if (!(value instanceof Pair) || !Symbol.OR.equals(((Pair)value).getHead()))
         {
            value = Pair.list(Symbol.OR, value); // do not have an OR on left
         }

         // have an OR list or some other sort of expression/value
         value = Pair.append((Pair)value,
                             (next instanceof Pair && Symbol.OR.equals(((Pair)next).getHead()))
                             ? ((Pair)next).getNext() : new Pair(next));
      }

      return value;
   }

   /**
    * Parse atom with a weight.
    * @return an atomic value.
    */
   protected Object parseWeight()
   {
      Object value = parseAtom();

      if (getCurToken() == TOKEN_WEIGHT)
      {
         // get token after done with current buffer (a next value is required)
         if (getNextToken() == TOKEN_EOF || !(m_tokenValue instanceof String))
         {
            fail(true); // invalid to not have a RHS value
         }

         Double weight = new Double(m_tokenValue.toString());

         // do not have a WEIGHT on left
         if (!(value instanceof Pair) || !Symbol.MUL.equals(((Pair)value).getHead()))
         {
            value = Pair.list(Symbol.MUL, Pair.list(weight, value));
         }

         forgetToken(); // token consumed
      }

      return value;
   }

   /**
    * @see nexj.core.scripting.GenericParser#parseToken()
    */
   protected int parseToken()
   {
      boolean bQuoted = false;

      m_tokenBuf.setLength(0);

      while(getCurChar() != CHAR_EOF && Character.isWhitespace(getCurChar()))
      {
         forgetChar();
         ++m_nOffset;
      }

      m_nTokenColumn = m_nOffset; // mark current token start position

      while(getCurChar() != CHAR_EOF && (bQuoted || !Character.isWhitespace(getCurChar())))
      {
         switch (getCurChar())
         {
            case '"':
               if (!bQuoted)
               {
                  if (m_tokenBuf.length() > 0) // if have something before quoted literal
                  {
                     m_tokenValue = m_tokenBuf.toString();

                     return valueType((String)m_tokenValue); // will pick up '"' char next time
                  }

                  bQuoted = true;
                  forgetChar();
                  ++m_nOffset;

                  continue;
               }

               forgetChar();
               ++m_nOffset;
               m_tokenValue = m_tokenBuf.toString();

               return TOKEN_VALUE; // finished literal processing (can be empty)

            case ')':
               if (!m_bSubexpression)
               {
                  fail(false); // unmatched closing parenthesis
               }

               m_tokenValue = m_tokenBuf.toString();

               // generate subexpression EOF on end of subexpression
               return (m_tokenBuf.length() > 0) ? valueType((String)m_tokenValue) : TOKEN_EOF;

            case '(':
               int nOffset = m_nTokenColumn; // note starting position of atom
               boolean bSubexpression = m_bSubexpression; //remember current state

               forgetChar();
               ++m_nOffset;
               m_bSubexpression = true; // expect a closing parenthesis
               m_tokenValue = parseElement();
               m_nTokenColumn = nOffset; // note starting position for exception (if any)

               if (getCurChar() != ')')
               {
                  fail(false); // unmatched opening parenthesis
               }

               m_bSubexpression = bSubexpression; // clear modified state
               m_nToken = 0; // reset for getCurToken() to work
               forgetChar();
               ++m_nOffset;

               return TOKEN_VALUE;

            case '!': // fall through
            case '&': // fall through
            case '*': // fall through
            case '-': // fall through
            case '|': // fall through
            case '~':
               if (m_tokenBuf.length() == 0)
               {
                  m_tokenBuf.append((char)getCurChar());
                  forgetChar();
                  ++m_nOffset;
               }

               m_tokenValue = m_tokenBuf.toString();

               return valueType((String)m_tokenValue);
         }

         m_tokenBuf.append((char)getCurChar());
         forgetChar();
         ++m_nOffset;
      }

      if (bQuoted)
      {
         fail(false);  // quoted literals should have been closed
      }

      m_tokenValue = m_tokenBuf.toString();

      return (m_tokenBuf.length() > 0) ? valueType((String)m_tokenValue) : TOKEN_EOF;
   }

   /**
    * Determine the type of the value stored in the buffer.
    * @param sVlaue The value to examine.
    * @return One of the TOKEN_* constants.
    */
   protected static int valueType(String sValue)
   {
      if (("|".length() == sValue.length() && "|".equals(sValue)) ||
          (Symbol.OR.getName().length() == sValue.length() && // MatchNode.OR
           Symbol.OR.getName().equals(sValue)))
      {
         return TOKEN_OR;
      }

      if (("&".length() == sValue.length() && "&".equals(sValue)) ||
          (Symbol.AND.getName().length() == sValue.length() && // MatchNode.AND
           Symbol.AND.getName().equals(sValue)))
      {
         return TOKEN_AND;
      }

      if (("!".length() == sValue.length() && "!".equals(sValue)) ||
          ("-".length() == sValue.length() && "-".equals(sValue)) ||
          (Symbol.NOT.getName().length() == sValue.length() && // MatchNode.NOT
           Symbol.NOT.getName().equals(sValue)))
      {
         return TOKEN_NOT;
      }

      if (("*".length() == sValue.length() && "*".equals(sValue)) ||
          (Symbol.MUL.getName().length() == sValue.length() && // MatchNode.WEIGHT
           Symbol.MUL.getName().equals(sValue)))
      {
         return TOKEN_WEIGHT;
      }

      if (("~".length() == sValue.length() && "~".equals(sValue)) ||
          (Symbol.LIKE_P.getName().length() == sValue.length() && // MatchNode.FUZZY
           Symbol.LIKE_P.getName().equals(sValue)))
      {
         return TOKEN_FUZZY;
      }

      return TOKEN_VALUE;
   }

   /**
    * Convenience method to abort parsing.
    * 
    * @param bToken Whether the position should be reported as token or text
    *           position.
    * @throws ParserException
    */
   protected final void fail(boolean bToken) throws ParserException
   {
      TextPosition pos = (bToken) ? null : getCurTextPosition();

      if (pos == null)
      {
         // Never returns null.
         pos = getCurTokenPos();
      }

      throw new ParserException("err.scripting.matchExpressionParsing", null, null, pos);
   }
}
