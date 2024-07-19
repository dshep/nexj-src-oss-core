// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

/**
 * This parser interprets backslash escape sequences in strings, converting them
 * to their proper characters.
 *
 * @see nexj.core.scripting.GenericParser
 */
public class StringParser extends GenericParser
{
   public StringParser(GlobalEnvironment globalEnv)
   {
      super(globalEnv);
   }

   /**
    * @see nexj.core.scripting.GenericParser#parseElement()
    */
   protected Object parseElement()
   {
      //Assume anything that gets parsed will be a string.
      return parseString();
   }

   /**
    * @see nexj.core.scripting.GenericParser#parseToken()
    */
   protected int parseToken()
   {
      return 0;
   }

   /**
    * @see nexj.core.scripting.GenericParser#parseString()
    */
   protected String parseString()
   {
      forgetChar();
      m_tokenBuf.setLength(0);

      while (getCurChar() != CHAR_EOF)
      {
         if (m_ch == '\\')
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
}
