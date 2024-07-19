// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Locale;


/**
 * Represents a MIME header name and values.
 */
public class MIMEHeader
{
   // attributes
   
   /**
    * The header name.
    */
   protected String m_sName;

   /**
    * The header value string.
    */
   protected String m_sValue;

   /**
    * The value count.
    */
   protected int m_nValueCount;

   // associations

   /**
    * The parsed value array.
    */
   protected Value[] m_valueArray;

   // constructors

   /**
    * Constructs an empty header.
    * @param sName The header name.
    */
   public MIMEHeader(String sName)
   {
      assert sName.equals(sName.toLowerCase(Locale.ENGLISH));
      
      m_sName = sName;
   }

   /**
    * Constructs the header.
    * @param sName The header name.
    * @param sValue The header value string.
    */
   public MIMEHeader(String sName, String sValue)
   {
      if (sValue == null)
      {
         sValue = "";
      }

      m_sName = sName;
      m_sValue = sValue;
   }

   // operations

   /**
    * @return The header name.
    */
   public String getName()
   {
      return m_sName;
   }

   /**
    * @return The header value string.
    */
   public String getValue()
   {
      if (m_sValue == null)
      {
         if (m_nValueCount == 0)
         {
            return m_sValue = "";
         }

         StringBuffer buf = new StringBuffer(64);

         appendValue(buf);
         m_sValue = buf.toString();
      }

      return m_sValue;
   }

   /**
    * @return The first value object.
    */
   public Value getFirstValue()
   {
      parse();

      return (m_nValueCount == 0) ? null : m_valueArray[0];
   }

   /**
    * Adds a value to the header.
    * @param value The value to add.
    */
   public void addValue(Value value)
   {
      parse();
      
      m_sValue = null;

      if (m_nValueCount == m_valueArray.length)
      {
         Value[] valueArray = new Value[m_nValueCount << 1];
         
         System.arraycopy(m_valueArray, 0, valueArray, 0, m_nValueCount);
         m_valueArray = valueArray;
      }
      
      m_valueArray[m_nValueCount++] = value;
   }

   /**
    * Finds a value by name.
    * @param sName The value name.
    * @return The value object, or null if not found.
    */
   public Value findValue(String sName)
   {
      parse();

      for (int i = 0; i < m_nValueCount; ++i)
      {
         Value value = m_valueArray[i];
         
         if (value.getName().equalsIgnoreCase(sName))
         {
            return value;
         }
      }

      return null;
   }

   /**
    * Gets a value by ordinal number.
    * @param nOrdinal The value ordinal number.
    * @return The value. 
    */
   public Value getValue(int nOrdinal)
   {
      parse();

      if (nOrdinal >= m_nValueCount)
      {
         throw new ArrayIndexOutOfBoundsException(nOrdinal);
      }
      
      return m_valueArray[nOrdinal];
   }

   /**
    * Removes a value by ordinal number.
    * @param nOrdinal The value ordinal number.
    * @return The removed value.
    */
   public Value removeValue(int nOrdinal)
   {
      parse();
      
      if (nOrdinal >= m_nValueCount)
      {
         throw new ArrayIndexOutOfBoundsException(nOrdinal);
      }
      
      Value value = m_valueArray[nOrdinal];

      System.arraycopy(m_valueArray, nOrdinal + 1, m_valueArray, nOrdinal, m_nValueCount - nOrdinal - 1);
      m_valueArray[--m_nValueCount] = null;

      return value;
   }

   /**
    * @return The value count.
    */
   public int getValueCount()
   {
      parse();
      
      return m_nValueCount;
   }

   /**
    * Parses the header values into a map.
    */
   protected void parse()
   {
      if (m_valueArray != null)
      {
         return;
      }

      m_valueArray = new Value[4];

      if (m_sValue == null)
      {
         return;
      }

      String s = m_sValue;
      
      try
      {
         StringBuffer buf = new StringBuffer(32);
         int nLen = s.length();
         int nParen = 0;
         int i = 0;
         char ch;
   
         for (;;)
         {
            while (i != nLen && Character.isWhitespace(s.charAt(i)))
            {
               ++i;
            }
            
            while (i != nLen)
            {
               ch = s.charAt(i);
               
               if (ch == ';' || ch == ',')
               {
                  if (nParen == 0)
                  {
                     break;
                  }
               }
               else if (ch == '(')
               {
                  ++nParen;
               }
               else if (ch == ')')
               {
                  if (nParen != 0)
                  {
                     --nParen;
                  }
               }

               buf.append(ch);
               ++i;
            }
            
            if (buf.length() == 0)
            {
               break;
            }

            Value value = new Value(buf.toString());

            while (i != nLen && s.charAt(i) == ';')
            {
               ++i;

               while (i != nLen && Character.isWhitespace(s.charAt(i)))
               {
                  ++i;
               }

               buf.setLength(0);

               while (i != nLen)
               {
                  ch = s.charAt(i);

                  if (ch == '=')
                  {
                     String sName = buf.toString();

                     buf.setLength(0);
                     ++i;

                     boolean bQuote = (i != nLen && s.charAt(i) == '"');

                     if (bQuote)
                     {
                        ++i;
                     }

                     while (i != nLen)
                     {
                        ch = s.charAt(i);

                        if (bQuote)
                        {
                           if (ch == '\\')
                           {
                              ++i;

                              if (i != nLen)
                              {
                                 ch = s.charAt(i);

                                 if (isSeparator(ch))
                                 {
                                    buf.append(ch);
                                    ++i;
                                 }
                                 else
                                 {
                                    buf.append('\\');
                                 }
                              }
                              else
                              {
                                 buf.append(ch);
                              }

                              continue;
                           }

                           if (ch == '"')
                           {
                              ++i;
                              
                              break;
                           }
                        }
                        else if (ch == ',')
                        {
                           if (nParen == 0)
                           {
                              if (buf.length() != 3 ||
                                 !sName.equalsIgnoreCase("expires") ||
                                 !m_sName.equalsIgnoreCase(HTTP.HEADER_SET_COOKIE))
                              {
                                 break;
                              }
                           }
                        }
                        else if (ch == ';')
                        {
                           if (nParen == 0)
                           {
                              break;
                           }
                        }
                        else if (ch == '(')
                        {
                           ++nParen;
                        }
                        else if (ch == ')')
                        {
                           if (nParen != 0)
                           {
                              --nParen;
                           }
                        }

                        buf.append(ch);
                        ++i;
                     }

                     if (value.findArg(sName) == null)
                     {
                        value.addArg(sName, buf.toString(), bQuote);
                     }

                     buf.setLength(0);

                     break;
                  }
                  else if (ch == ';' || ch == ',')
                  {
                     break;
                  }

                  buf.append(ch);
                  ++i;
               }
               
               while (buf.length() != 0 && Character.isWhitespace(buf.charAt(buf.length() - 1)))
               {
                  buf.setLength(buf.length() - 1);
               }

               if (buf.length() != 0)
               {
                  value.addArg(buf.toString(), "", false);
                  buf.setLength(0);
               }
            }

            if (findValue(value.getName()) == null)
            {
               addValue(value);
            }

            if (i != nLen)
            {
               ch = s.charAt(i);
               
               if (ch == ',')
               {
                  ++i;
               }
            }

            buf.setLength(0);
         }
      }
      finally
      {
         m_sValue = s;
      }
   }

   /**
    * Determines if a character is a MIME separator.
    * @param ch The character to check.
    * @return True if the character is a separator.
    */
   public static boolean isSeparator(char ch)
   {
      switch (ch)
      {
         case '\t':
         case ' ':
         case '(':
         case ')':
         case '<':
         case '>':
         case '@':
         case ',':
         case ';':
         case ':':
         case '\\':
         case '"':
         case '/':
         case '[':
         case ']':
         case '?':
         case '=':
         case '{':
         case '}':
            return true;

         default:
            return false;
      }
   }

   /**
    * Appends the values of a header to this header values.
    * @param header The header to append. 
    */
   public void append(MIMEHeader header)
   {
      for (int i = 0, n = header.getValueCount(); i < n; ++i)
      {
         addValue(header.getValue(i));
      }
   }

   /**
    * Appends the value to a string buffer.
    * @param buf The destination buffer.
    */
   public void appendValue(StringBuffer buf)
   {
      if (m_sValue != null)
      {
         buf.append(m_sValue);
      }
      else if (m_nValueCount != 0)
      {
         boolean bFirst = true;

         for (int i = 0; i < m_nValueCount; ++i)
         {
            if (bFirst)
            {
               bFirst = false;
            }
            else
            {
               buf.append(", ");
            }

            buf.append(m_valueArray[i]);
         }
      }
   }
   
   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(64);
      
      buf.append(m_sName);
      buf.append(": ");
      appendValue(buf);

      return buf.toString();
   }

   // inner classes

   /**
    * Represents a header value with options.
    */
   public static class Value
   {
      // constants

      /**
       * Offset between consecutive entries.
       */
      protected final static int OFS = 3;

      // attributes

      /**
       * The value name.
       */
      protected String m_sName;

      /**
       * The argument count.
       */
      protected int m_nArgCount;

      // associations

      /**
       * Value argument array: sName[3*n], sValue[3*n+1], quoted[3*n+2].
       */
      protected String[] m_argArray;

      // constructors

      /**
       * Constructs the value.
       * @param sName The value name.
       */
      public Value(String sName)
      {
         m_sName = sName;
      }

      // operations

      /**
       * @return The value name.
       */
      public String getName()
      {
         return m_sName;
      }
      
      /**
       * Adds a named argument to the value.
       * @param sName Te argument name.
       * @param sValue The argument value.
       * @param bQuoted True if the value is a quoted string.
       */
      public void addArg(String sName, String sValue, boolean bQuoted)
      {
         assert sName != null;
         assert sValue != null;

         if (m_argArray == null)
         {
            m_argArray = new String[OFS * 2]; 
         }
         
         int i = OFS * m_nArgCount;

         if (i == m_argArray.length)
         {
            String[] argArray = new String[i << 1];
            
            System.arraycopy(m_argArray, 0, argArray, 0, i);
            m_argArray = argArray;
         }

         m_argArray[i] = sName;
         m_argArray[i + 1] = sValue;
         m_argArray[i + 2] = (bQuoted) ? "" : null;
         ++m_nArgCount;
      }

      /**
       * Finds a named argument.
       * @param sName The argument name.
       * @return The argument value, or null if not found.
       */
      public String findArg(String sName)
      {
         for (int i = 0; i < m_nArgCount; ++i)
         {
            if (m_argArray[OFS * i].equalsIgnoreCase(sName))
            {
               return m_argArray[OFS * i + 1];
            }
         }

         return null;
      }
      
      /**
       * Gets an argument name by ordinal number.
       * @param nOrdinal The argument ordinal number.
       * @return The argument name. 
       */
      public String getArgName(int nOrdinal)
      {
         if (nOrdinal >= m_nArgCount)
         {
            throw new ArrayIndexOutOfBoundsException(nOrdinal);
         }
         
         return m_argArray[OFS * nOrdinal];
      }

      /**
       * Gets an argument value by ordinal number.
       * @param nOrdinal The argument ordinal number.
       * @return The argument value.
       */
      public String getArgValue(int nOrdinal)
      {
         if (nOrdinal >= m_nArgCount)
         {
            throw new ArrayIndexOutOfBoundsException(nOrdinal);
         }
         
         return m_argArray[OFS * nOrdinal + 1];
      }

      /**
       * Determines if an argument value is quoted.
       * @param nOrdinal The argument ordinal number.
       * @return True if the argument value is quoted. 
       */
      public boolean isArgQuoted(int nOrdinal)
      {
         if (nOrdinal >= m_nArgCount)
         {
            throw new ArrayIndexOutOfBoundsException(nOrdinal);
         }

         return m_argArray[OFS * nOrdinal + 2] != null;
      }

      /**
       * @return The named argument count.
       */
      public int getArgCount()
      {
         return m_nArgCount;
      }

      /**
       * Appends the value to a string buffer.
       * @param buf The destination buffer.
       */
      public void append(StringBuffer buf)
      {
         buf.append(m_sName);

         for (int i = 0; i < m_nArgCount; ++i)
         {
            buf.append("; ");
            buf.append(getArgName(i));
            buf.append('=');

            boolean bQuoted = isArgQuoted(i);

            if (bQuoted)
            {
               buf.append('"');

               String s = getArgValue(i);

               for (int k = 0, n = s.length(); k != n; ++k)
               {
                  char ch = s.charAt(k);

                  if (isSeparator(ch))
                  {
                     buf.append('\\');
                  }

                  buf.append(ch);
               }

               buf.append('"');
            }
            else
            {
               buf.append(getArgValue(i));
            }
         }
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         StringBuffer buf = new StringBuffer(64);

         append(buf);

         return buf.toString();
      }
   }
}
