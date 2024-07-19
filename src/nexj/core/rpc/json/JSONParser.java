package nexj.core.rpc.json;

import java.util.ArrayList;
import java.util.Collection;

import nexj.core.scripting.GenericParser;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.ParserException;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.TextPosition;

/**
 * Parses a JSON message into a map.
 */
public class JSONParser extends GenericParser
{
   // constants

   /**
    * Open Container {
    */
   protected final static int TOKEN_OBRACE = 1;

   /**
    * Closing Container }
    */
   protected final static int TOKEN_CBRACE = 2;

   /**
    * Open [
    */
   protected final static int TOKEN_OBRACKET = 3;

   /**
    * Closing ]
    */
   protected final static int TOKEN_CBRACKET = 4;

   /**
    * Comma ,
    */
   protected final static int TOKEN_COMMA = 5;

   /**
    * Colon :
    */
   protected final static int TOKEN_COLON = 6;

   /**
    * Atom (null, boolean, number, string).
    */
   protected final static int TOKEN_ATOM = 7;

   // constructors

   /**
    * Constructs the JSON parser with a given global environment for interning symbols.
    * @param globalEnv The global environment, into which to store the symbols.
    */
   public JSONParser(GlobalEnvironment env)
   {
      super(env);
   }

   // operations

   /**
    * Parses a JSON Array.
    * @return The parsed out array as a collection.
    * @throws ParserException if a syntax error has been encountered.
    */
   protected Object parseArray()
   {
      Object array = createArray();
      boolean bHasValue = false;
      int nIndex = 0;

      ++m_nListDepth;

      if (getCurToken() != TOKEN_CBRACKET)
      {
         for (;;)
         {
            addArrayValue(array, nIndex, parseIndexValue(nIndex));
            bHasValue = true;

            if (getCurToken() == TOKEN_CBRACKET)
            {
               forgetToken();
               --m_nListDepth;

               break;
            }
            else if (getCurToken() == TOKEN_COMMA)
            {
               if (!bHasValue)
               {
                  fail("err.parser.json.array.unexpectedComma", null, getCurTokenPos());
               }
               else
               {
                  forgetToken();

                  bHasValue = false;
               }
            }
            else
            {
               fail("err.parser.json.array.unexpectedToken", new Object[]{m_tokenValue}, getCurTokenPos());
            }

            ++nIndex;
         }
      }
      else
      {
         forgetToken();
      }

      return array;
   }

   /**
    * @return The array object.
    */
   protected Object createArray()
   {
      return new ArrayList(4);
   }

   /**
    * Parses an array element for the given index.
    * @param nIndex The index.
    * @return The array element.
    */
   protected Object parseIndexValue(int nIndex)
   {
      return parseElement();
   }

   /**
    * Adds a value to the provided array object.
    * @param array The array object.
    * @param nIndex The array index.
    * @param value The value.
    */
   protected void addArrayValue(Object array, int nIndex, Object value)
   {
      ((Collection)array).add(value);
   }

   /**
    * Parses a JSON value - array, object or primitive.
    * @return The internal representation of the value.
    * @throws ParserException if a syntax error has been encountered.
    */
   protected Object parseValue()
   {
      Object value = null;

      for (;;)
      {
         switch (getCurToken())
         {
            case TOKEN_EOF:
               fail("err.parser.listEOF", null, getCurTokenPos());

            case TOKEN_OBRACE:
            case TOKEN_OBRACKET:
               value = parseElement();

               return value;

            case TOKEN_ATOM:
               value = m_tokenValue;
               forgetToken();

               return value;

            default:
               fail("err.parser.json.value.unexpectedToken", new Object[]{m_tokenValue}, getCurTokenPos());
         }
      }
   }

   /**
    * Parses a JSON object.
    * @return The internal representation of the object.
    * @throws ParserException if a syntax error has been encountered.
    */
   protected Object parseObject()
   {
      String sKey = null;
      Object obj = createObject();

      ++m_nListDepth;

      if (getCurToken() != TOKEN_CBRACE)
      {
         for (;;)
         {
            sKey = parseObjectKey(obj);

            // value
            if (getCurToken() != TOKEN_COLON)
            {
               fail("err.parser.unexpectedToken", new Object[]{m_tokenValue}, getCurTokenPos());
            }
            else
            {
               forgetToken();
               addObjectValue(obj, sKey, parseKeyValue(sKey));

               if (getCurToken() == TOKEN_CBRACE)
               {
                  forgetToken();
                  --m_nListDepth;

                  break;
               }
               else if (getCurToken() == TOKEN_COMMA)
               {
                  forgetToken();
                  sKey = null;
               }
               else
               {
                  fail("err.parser.unexpectedToken", new Object[]{m_tokenValue}, getCurTokenPos());
               }
            }
         }
      }
      else
      {
         forgetToken();
      }

      return obj;
   }

   /**
    * @return The object object.
    */
   protected Object createObject()
   {
      return new HashTab(4);
   }

   /**
    * Parses an object key.
    * @param obj The object to which the key belongs.
    * @return The key.
    */
   protected String parseObjectKey(Object obj)
   {
      Object value = parseValue();

      if (!(value instanceof String))
      {
         fail("err.parser.json.object.unexpectedKeyValue", new Object[]{value}, getCurTokenPos());
      }

      return (String)value;
   }

   /**
    * Parses the value corresponding to a given object key.
    * @param sKey The key which value to parse.
    * @return The value.
    */
   protected Object parseKeyValue(String sKey)
   {
      return parseValue();
   }

   /**
    * Adds a key-value pair to the provided object.
    * @param obj The object.
    * @param sKey The key.
    * @param value The value.
    */
   protected void addObjectValue(Object obj, String sKey, Object value)
   {
      ((Lookup)obj).put(sKey, value);
   }

   /**
    * Parses a single JSON element - object, array or primitive.
    * @return The internal representation of the element.
    * @throws ParserException if a syntax error has been encountered.
    */
   protected Object parseElement()
   {
      TextPosition pos = null;
      Object obj;

      switch (getCurToken())
      {
         // must start with an open brace, open array, or primitive
         case TOKEN_ATOM:
            obj = m_tokenValue;
            forgetToken();

            return obj;

         case TOKEN_OBRACKET:
            if (m_posMap != null)
            {
               pos = getCurTokenPos();
            }

            forgetToken();
            obj = parseArray();

            if (m_posMap != null && obj != null)
            {
               m_posMap.put(obj, pos);
            }

            return obj;

         case TOKEN_OBRACE:
            if (m_posMap != null)
            {
               pos = getCurTokenPos();
            }

            forgetToken();
            obj = parseObject();

            if (m_posMap != null && obj != null)
            {
               m_posMap.put(obj, pos);
            }

            return obj;

         case TOKEN_CBRACE:
         case TOKEN_CBRACKET:
         case TOKEN_COMMA:
         case TOKEN_COLON:
            fail("err.parser.json.value.unexpectedToken", new Object[]{m_tokenValue}, getCurTokenPos());

            return null;

         default:

            return EOF;
      }
   }

   /**
    * @see nexj.core.scripting.GenericParser#parseToken()
    */
   protected int parseToken()
   {
      for (;;)
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

         m_tokenValue = Character.toString((char)m_ch);

         switch (m_ch)
         {
            case CHAR_EOF:

               return TOKEN_EOF;

            case '"':
               m_tokenValue = parseString();

               return TOKEN_ATOM;

            case ':':
               forgetChar();

               return TOKEN_COLON;

            case '{':
               forgetChar();

               return TOKEN_OBRACE;

            case '}':
               forgetChar();

               return TOKEN_CBRACE;

            case '[':
               forgetChar();

               return TOKEN_OBRACKET;

            case ']':
               forgetChar();

               return TOKEN_CBRACKET;

            case ',':
               forgetChar();

               return TOKEN_COMMA;

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

               fail("err.parser.unexpectedToken", new Object[]{Character.toString((char)m_ch)}, getCurTextPosition());
               forgetChar();
               m_tokenValue = "";

               return TOKEN_NONE;

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
               }

               fail("err.parser.unexpectedToken", new Object[]{Character.toString((char)m_ch)}, getCurTextPosition());
               forgetChar();
               m_tokenValue = "";

               return TOKEN_NONE;

            case 'n':
               m_tokenValue = parseNull();

               return TOKEN_ATOM;

            case 't':
            case 'f':
               m_tokenValue = parseBoolean();

               return TOKEN_ATOM;

            default:
               fail("err.parser.unexpectedToken", new Object[]{m_tokenValue}, getCurTextPosition());
               forgetChar();
               m_tokenValue = "";

               return TOKEN_NONE;
         }
      }
   }

   /**
    * Parses a JSON null.
    * @return The internal representation of null.
    * @throws ParserException if a syntax error has been encountered.
    */
   protected Object parseNull()
   {
      m_tokenBuf.setLength(0);
      m_tokenBuf.append((char)getCurChar());

      for (;;)
      {
         markReader1();
         getNextChar();

         if (Character.isLetterOrDigit((char)m_ch))
         {
            m_tokenBuf.append((char)m_ch);

            continue;
         }

         resetReader();

         break;
      }

      String sValue = m_tokenBuf.toString();

      if (!sValue.equals("null"))
      {
         fail("err.parser.unexpectedToken", new Object[]{sValue}, getCurTextPosition());
      }

      return null;
   }

   /**
    * Parses buffer for a JSON boolean.
    * @return The internal representation of the boolean value.
    * @throws ParserException if a syntax error has been encountered.
    */
   protected Boolean parseBoolean()
   {
      m_tokenBuf.setLength(0);
      m_tokenBuf.append((char)getCurChar());

      for (;;)
      {
         markReader1();
         getNextChar();

         if (Character.isLetterOrDigit((char)m_ch))
         {
            m_tokenBuf.append((char)m_ch);

            continue;
         }

         resetReader();

         break;
      }

      String sValue = m_tokenBuf.toString();

      if (sValue.equals("true"))
      {
         return Boolean.TRUE;
      }

      if (sValue.equals("false"))
      {
         return Boolean.FALSE;
      }

      fail("err.parser.unexpectedToken", new Object[]{sValue}, getCurTextPosition());

      return null;
   }
}
