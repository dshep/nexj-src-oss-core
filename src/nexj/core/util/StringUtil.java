// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.Writer;
import java.sql.Timestamp;
import java.text.BreakIterator;
import java.text.DateFormatSymbols;
import java.text.FieldPosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * String utilities.
 */
public class StringUtil
{
   // constants

   /**
    * Hex digit table.
    */
   private final static char[] HEX_DIGITS = "0123456789ABCDEF".toCharArray();

   /**
    * Array of spaces.
    */
   protected final static char[] SPACES = new char[80];

   static
   {
      Arrays.fill(SPACES, ' ');
   }

   /**
    * Pattern used for matching/splitting a string into separate digits and non-digits groups.
    */
   private final static Pattern STRING_DIGIT_PARTS = Pattern.compile("^(?:(\\d+)?(\\D+)?)*$");

   /**
    * The timestamp string format.
    */
   private final static SimpleDateFormat s_timestampOutFormat = new SimpleDateFormat("Gyyyy'-'MM'-'dd HH':'mm':'ss", Locale.ENGLISH);

   static
   {
      s_timestampOutFormat.setTimeZone(TZ.UTC);

      DateFormatSymbols dfs = s_timestampOutFormat.getDateFormatSymbols();

      dfs.setEras(new String[]{"-", ""});
      s_timestampOutFormat.setDateFormatSymbols(dfs);
   }


   /**
    * The ignored field position.
    */
   private final static FieldPosition s_ignoredFieldPosition = new FieldPosition(SimpleDateFormat.AM_PM_FIELD)
   {
      /**
       * @see java.text.FieldPosition#setBeginIndex(int)
       */
      public void setBeginIndex(int nIndex)
      {
      }

      /**
       * @see java.text.FieldPosition#setEndIndex(int)
       */
      public void setEndIndex(int nIndex)
      {
      }
   };

   /**
    * The zero char array.
    */
   private final static char[] s_zeroCharArray = new char[]{'0', '0', '0', '0', '0', '0', '0', '0', '0'};

   // associations

   /**
    * String identity set for interning: String[].
    */
   protected final static Holder s_stringSet = new HashHolder();

   // constructors

   /**
    * Prevents construction.
    */
   protected StringUtil()
   {
   }

   // operations

   /**
    * Allocates unique strings in the regular heap.
    * @param s The string to intern. Can be null.
    * @return The interned string.
    */
   public synchronized static String intern(String s)
   {
      if (s == null)
      {
         return null;
      }

      String sInterned = (String)s_stringSet.get(s);

      if (sInterned != null)
      {
         return sInterned;
      }

      s_stringSet.add(s);

      return s;
   }

   /**
    * Finds the index in a given string of any of the characters from a set.
    * @param s The string to search in.
    * @param sSet The character set.
    * @param nStart The start index. Can be negative.
    * @param nEnd The end index (past the last considered character). Can be beyond the length of the string.
    * @return The index, or -1 if not found.
    */
   public static int findSetIndex(String s, String sSet, int nStart, int nEnd)
   {
      int nCount = sSet.length();

      if (nStart < 0)
      {
         nStart = 0;
      }

      if (nEnd > s.length())
      {
         nEnd = s.length();
      }

      while (nStart < nEnd)
      {
         char ch = s.charAt(nStart);

         for (int i = 0; i != nCount; ++i)
         {
            if (ch == sSet.charAt(i))
            {
               return nStart;
            }
         }

         ++nStart;
      }

      return -1;
   }

   /**
    * Parses a digit from a character.
    * @param ch The character to parse.
    * @param nRadix The digit radix.
    * @return The resulting digit.
    * @throws NumberFormatException if the digit is invalid.
    */
   public static int parseDigit(char ch, int nRadix) throws NumberFormatException
   {
      int n = Character.digit(ch, nRadix);

      if (n < 0)
      {
         throw new NumberFormatException("Invalid digit");
      }

      return n;
   }

   /**
    * Parses an integer from a substring.
    * @param s The string.
    * @param nStart The start offset.
    * @param nEnd The end offset.
    * @throws NumberFormatException if the number is invalid.
    */
   public static int parseInt(String s, int nStart, int nEnd) throws NumberFormatException
   {
      int n = 0;

      while (nStart < nEnd)
      {
         n = n * 10 + parseDigit(s.charAt(nStart++), 10);
      }

      return n;
   }

   /**
    * Parses a long from a substring.
    * 
    * @param s The string.
    * @param nStart The start offset.
    * @param nEnd The end offset.
    * @throws NumberFormatException if the number is invalid.
    */
   public static long parseLong(String s, int nStart, int nEnd) throws NumberFormatException
   {
      long n = 0;

      while (nStart < nEnd)
      {
         n = n * 10 + parseDigit(s.charAt(nStart++), 10);
      }

      return n;
   }

   /**
    * Verifies that a character occurs at a given offset in a string.
    * @param s The string.
    * @param nOffset The character offset from the start.
    * @param ch The character.
    * @throws IllegalArgumentException if the character is missing at that offset.
    */
   public static void verifyDelimiter(String s, int nOffset, char ch) throws IllegalArgumentException
   {
      if (nOffset >= s.length() || s.charAt(nOffset) != ch)
      {
         throw new IllegalArgumentException("Missing delimiter \"" + ch + "\" at offset " + nOffset);
      }
   }

   /**
    * Parses a boolean value.
    * @param sValue The string to parse.
    * @return The boolean value.
    * @throws IllegalArgumentException if the string is is not a valid boolean.
    */
   public static boolean parseBoolean(String sValue)
   {
      if (sValue != null)
      {
         if (sValue.equals("true") ||
            sValue.equals("1"))
         {
            return true;
         }

         if (sValue.equals("false") ||
            sValue.equals("0"))
         {
            return false;
         }
      }

      throw new IllegalArgumentException(sValue);
   }

   /**
    * Joins a CharSequence list into a single CharSequence using the specific delimiter.
    * @param buf The destination buffer (not null).
    * @param array The list to join.
    * @param sPrefix The prefix to prepend if multiple values being joined (null == "").
    * @param sDelim The delimiter string (null == "").
    * @param sSuffix The suffix to append if multiple values being joined (null == "").
    * @return The buffer containing the joined arguments.
    * @throws RuntimeException on IOexception during append.
    */
   public static Appendable join(
      Appendable buf, CharSequence[] array, String sPrefix, String sDelim, String sSuffix)
      throws RuntimeException
   {
      if (array == null || array.length == 0)
      {
         return buf;
      }

      if (sDelim == null)
      {
         sDelim = "";
      }

      assert buf != null;

      try
      {
         if (array.length > 1 && sPrefix != null)
         {
            buf.append(sPrefix);
         }

         buf.append(array[0]);

         for (int i = 1, nCount = array.length; i < nCount; ++i)
         {
            buf.append(sDelim);
            buf.append(array[i]);
         }

         if (array.length > 1 && sSuffix != null)
         {
            buf.append(sSuffix);
         }
      }
      catch (IOException e)
      {
         ObjUtil.rethrow(e);
      }

      return buf;
   }

   /**
    * Splits a string into a string array using a character delimiter.
    * Note: does not skip consecutive delimiters.
    * @param str String to be split into an array
    * @param chDelim Character to be used as a delimiter
    * @return String[] containing the various substrings as elements
    */
   public static String[] split(String str, char chDelim)
   {
      ArrayList results = new ArrayList();
      int nCurrIdx = 0;
      int nDelimIdx = str.indexOf(chDelim);

      while (nDelimIdx != -1)
      {
         results.add(str.substring(nCurrIdx, nDelimIdx));
         nCurrIdx = nDelimIdx + 1;
         nDelimIdx = str.indexOf(chDelim, nCurrIdx);
      }

      if (nCurrIdx < str.length())
      {
         results.add(str.substring(nCurrIdx));
      }

      return (String[]) results.toArray(new String[results.size()]);
   }

   /**
    * Splits a string into a string array, delimited on whitespace characters.
    * Note: Skips consecutive whitespace characters.
    * @param str String to be split into an array.
    * @return String array containing the various substrings as elements.
    */
   public static String[] split(String str)
   {
      if (isEmpty(str))
      {
         return new String[0];
      }

      ArrayList results = new ArrayList();
      int nLength = str.length();

      for (int i = 0; i < nLength; i++)
      {
         while (i < nLength && Character.isWhitespace(str.charAt(i)))
         {
            i++;
         }

         if (i >= nLength)
         {
            break;
         }

         int nMark = i++;

         while (i < nLength && !Character.isWhitespace(str.charAt(i)))
         {
            i++;
         }

         results.add(str.substring(nMark, i));
      }

      return (String[])results.toArray(new String[results.size()]);
   }

   /**
    * Compares two strings ignoring the case.
    * @param sLeft The left string. Can be null.
    * @param sRight The right string. Can be null.
    * @return True if equal, false otherwise.
    */
   public static boolean equalIgnoreCase(String sLeft, String sRight)
   {
      if (sLeft == null)
      {
         return sRight == null;
      }

      return sLeft.equalsIgnoreCase(sRight);
   }

   /**
    * Compares two strings ignoring differences in end-of-line encoding.
    * Case sensitive.  Empty strings are equal to null strings.
    * @param sLeft The left string. Can be null.
    * @param sRight The right string. Can be null.
    * @return True if equal, false otherwise.
    */
   public static boolean equalEOL(String sLeft, String sRight)
   {
      if (sLeft == sRight)
      {
         return true;
      }

      if (sRight == null)
      {
         sRight = "";
      }

      if (sLeft == null)
      {
         sLeft = "";
      }

      int nLeftLength = sLeft.length();
      int nRightLength = sRight.length();
      int nLeftPos = 0;
      int nRightPos = 0;

      while (nLeftPos < nLeftLength && nRightPos < nRightLength)
      {
         char chLeft = sLeft.charAt(nLeftPos);
         char chRight = sRight.charAt(nRightPos);

         if (chLeft == '\r')
         {
            if (nLeftPos + 1 < nLeftLength && sLeft.charAt(nLeftPos + 1) == '\n')
            {
               ++nLeftPos;
            }

            chLeft = '\n';
         }

         if (chRight == '\r')
         {
            if (nRightPos + 1 < nRightLength && sRight.charAt(nRightPos + 1) == '\n')
            {
               ++nRightPos;
            }

            chRight = '\n';
         }

         if (chLeft != chRight)
         {
            return false;
         }

         ++nLeftPos;
         ++nRightPos;
      }

      return nLeftPos == nLeftLength && nRightPos == nRightLength;
   }

   /**
    * Checks if a string is empty.
    * @param s The string to check.
    * @return True if the string is null or zero-length.
    */
   public static boolean isEmpty(String s)
   {
      return s == null || s.length() == 0;
   }

   /**
    * Trims a string.
    * @param s The string to trim. Can be null.
    * @return The trimmed string. Null if the string is empty.
    * @see java.lang.String#trim()
    */
   public static String trimToNull(String s)
   {
      if (s != null)
      {
         s = s.trim();

         if (s.length() == 0)
         {
            s = null;
         }
      }

      return s;
   }

   /**
    * Appends the 16-bit hexadecimal representation of the given character. The
    * result is left-padded with zeroes to make 4 digits.
    * @param buf The destination buffer.
    * @param ch The character to append.
    */
   public static void appendHex(StringBuffer buf, char ch)
   {
      buf.append(HEX_DIGITS[(ch >> 12) & 0xf]);
      buf.append(HEX_DIGITS[(ch >> 8) & 0xf]);
      buf.append(HEX_DIGITS[(ch >> 4) & 0xf]);
      buf.append(HEX_DIGITS[ch & 0xf]);
   }

   /**
    * Appends the lower 16-bit hexadecimal representation of the given int.
    * The result is left-padded with zeroes to make 2 digits.
    * @param buf The destination buffer.
    * @param ch The character to append.
    */
   public static void appendHex(StringBuilder buf, int n)
   {
      buf.append(HEX_DIGITS[(n >> 4) & 0xf]);
      buf.append(HEX_DIGITS[n & 0xf]);
   }

   /**
    * Writes the 16-bit hexadecimal representation of the given character. The
    * result is left-padded with zeroes to make 4 digits.
    * @param writer The output character stream.
    * @param ch The character to write.
    */
   public static void writeHex(Writer writer, char ch) throws IOException
   {
      writer.write(HEX_DIGITS[(ch >> 12) & 0xf]);
      writer.write(HEX_DIGITS[(ch >> 8) & 0xf]);
      writer.write(HEX_DIGITS[(ch >> 4) & 0xf]);
      writer.write(HEX_DIGITS[ch & 0xf]);
   }

   /**
    * Writes the given number of spaces.
    * @param writer The output character stream.
    * @param nCount The number of spaces to write.
    */
   public static void writeSpaces(Writer writer, int nCount) throws IOException
   {
      int nLength = SPACES.length;

      while(nCount > 0)
      {
         int n = Math.min(nCount, nLength);

         writer.write(SPACES, 0, n);
         nCount -= n;
      }
   }

   /**
    * Appends a timestamp to a string buffer.
    * @param buf The destination string buffer.
    * @param ts The timestamp to append.
    * @param bLiteral True to append in a literal format (with T instead of a space).
    */
   public static void appendUTC(StringBuffer buf, Timestamp ts, boolean bLiteral)
   {
      int nStart = buf.length();

      ((SimpleDateFormat)s_timestampOutFormat.clone()).format(ts, buf, s_ignoredFieldPosition);

      int nEnd = buf.append('.').length();

      buf.append(ts.getNanos());
      buf.insert(nEnd, s_zeroCharArray, 0, 9 - (buf.length() - nEnd));

      if (bLiteral)
      {
         for (int i = nStart + 10; i < nEnd; ++i)
         {
            if (buf.charAt(i) == ' ')
            {
               buf.setCharAt(i, 'T');
               break;
            }
         }
      }
   }

   /**
    * Appends an upper-cased string to a string buffer.
    * @param buf The destination buffer.
    * @param s The string to append. Can be null.
    */
   public static void appendLowerCase(StringBuffer buf, String s)
   {
      if (s != null)
      {
         for (int i = 0, n = s.length(); i != n; ++i)
         {
            buf.append(Character.toLowerCase(s.charAt(i)));
         }
      }
   }

   /**
    * Appends an upper-cased string to a string buffer.
    * @param buf The destination buffer.
    * @param s The string to append. Can be null.
    */
   public static void appendUpperCase(StringBuffer buf, String s)
   {
      if (s != null)
      {
         for (int i = 0, n = s.length(); i != n; ++i)
         {
            buf.append(Character.toUpperCase(s.charAt(i)));
         }
      }
   }

   /**
    * Lexicographic comparison - "." separated version elements are considered individually,
    * from left to right. leading digits are parsed to numbers and numbers compared by their
    * magnitude. The rest of the version part is compared alphabetically, in case-insensitive order.
    * Ignores final character, if final character is "+".
    * For return values of string comparison:
    * @see java.lang.String#compareTo(String)
    * @param sLeft The left version.
    * @param sRight The right version.
    * @return The result is a negative if this sLeft < sRight, and positive if sLeft > sRight.
    */
   public static int compareVersionRanges(String sLeft, String sRight)
   {
      if (sLeft.length() > 0 && sLeft.charAt(sLeft.length() - 1) == '+')
      {
         sLeft = sLeft.substring(0, sLeft.length() - 1);
      }

      if (sRight.length() > 0 && sRight.charAt(sRight.length() - 1) == '+')
      {
         sRight = sRight.substring(0, sRight.length() - 1);
      }

      return compareVersions(sLeft, sRight);
   }

   /**
    * Lexicographic comparison - "." separated version elements are considered individually,
    * from left to right. leading digits are parsed to numbers and numbers compared by their
    * magnitude. The rest of the version part is compared alphabetically, in case-insensitive order.
    * For return values of string comparison:
    * @see java.lang.String#compareTo(String)
    * @param sLeft The left version.
    * @param sRight The right version.
    * @return The result is a negative if this sLeft < sRight, and positive if sLeft > sRight.
    */
   public static int compareVersions(String sLeft, String sRight)
   {
      assert sLeft != null && sRight != null;

      int nLeftLen = 0; // length of left string already processed
      int nRightLen = 0; // length of right string already processed

      // for every section delimited by '.' compare the values
      // init n*End for first for-loop condition to pass
      for (int nLeftEnd = 0, nLeftStart = 0, nRightEnd = 0, nRightStart = 0;
           nLeftEnd >= 0 && nRightEnd >= 0;
           nLeftStart = nLeftEnd + 1, nRightStart = nRightEnd + 1)
      {
         nLeftEnd = sLeft.indexOf('.', nLeftStart);
         nRightEnd = sRight.indexOf('.', nRightStart);

         nLeftLen = nLeftStart;
         nRightLen = nRightStart;

         String sLeftPart = (nLeftEnd < 0) ? sLeft.substring(nLeftStart)
                                           : sLeft.substring(nLeftStart, nLeftEnd);
         String sRightPart = (nRightEnd < 0) ? sRight.substring(nRightStart)
                                             : sRight.substring(nRightStart, nRightEnd);
         Matcher leftMatch = STRING_DIGIT_PARTS.matcher(sLeftPart);
         Matcher rightMatch = STRING_DIGIT_PARTS.matcher(sRightPart);

         if (!leftMatch.find())
         {
            if (!rightMatch.find())
            {
               break; // finished this section
            }

            return rightMatch.group().compareTo("");
         }
         else if (!rightMatch.find())
         {
            return -leftMatch.group().compareTo("");
         }

         String sLeftMatch = leftMatch.group(1); // 1 == digit portion of regex (?:(\\d+)?(\\D+)?)*
         String sRightMatch = rightMatch.group(1); //1 == digit portion of regex (?:(\\d+)?(\\D+)?)*
         int nMatch;

         if (sLeftMatch != null && sRightMatch != null)
         {
            nMatch = Long.signum(Long.parseLong(sLeftMatch) - Long.parseLong(sRightMatch));
         }
         else // NPE can occur if one of the digits is absent, treat same as string comparison
         {
            sLeftMatch = (sLeftMatch == null) ? "" : sLeftMatch; // treat null same as empty value
            sRightMatch = (sRightMatch == null) ? "" : sRightMatch; //treat null same as empty value
            nMatch = sLeftMatch.compareToIgnoreCase(sRightMatch);
         }

         if (nMatch == 0)
         {
            sLeftMatch = leftMatch.group(2); // 2 == non-digit portion of regex (?:(\\d+)?(\\D+)?)*
            sLeftMatch = (sLeftMatch == null) ? "" : sLeftMatch; // treat null same as empty value
            sRightMatch = rightMatch.group(2); //2 == non-digit portion of regex (?:(\\d+)?(\\D+)?)*
            sRightMatch = (sRightMatch == null) ? "" : sRightMatch; //treat null same as empty value
            nMatch = sLeftMatch.compareToIgnoreCase(sRightMatch);
         }

         if (nMatch != 0)
         {
            return nMatch;
         }
      }

      return sLeft.length() - sRight.length() + nRightLen - nLeftLen;
   }

   /**
    * Converts a timestamp value to string.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static String toString(Timestamp value)
   {
      if (value == null)
      {
         return null;
      }

      StringBuffer buf = new StringBuffer(26);

      appendUTC(buf, value, false);

      return buf.toString();
   }

   /**
    * Derives a caption from an identifier.
    * @param sName The identifier. Can be null.
    * @param sSep Separator for redundant prefixes. 
    * @param bCapital True to capitalize each word in the result.
    * @return The resulting caption.
    */
   public static String toCaption(String sName, char chSep, boolean bCapital)
   {
      if (sName == null)
      {
         return sName;
      }

      sName = sName.substring(sName.lastIndexOf(chSep) + 1);

      int nCount = sName.length();

      if (nCount == 0)
      {
         return sName;
      }

      StringBuffer buf = new StringBuffer(nCount + 8);

      final int TITLE = 1 << Character.UPPERCASE_LETTER | 1 << Character.TITLECASE_LETTER;
      final int SPACE = 1 << Character.SPACE_SEPARATOR;
      int nMask = 0;
      int nPrevMask = SPACE;
      int nStart = 0;
      int i = 0;

      do
      {
         if (i == nCount ||
            (nMask = (1 << Character.getType(sName.charAt(i))) & (TITLE | SPACE)) != 0 &&
            ((nMask & TITLE) != 0 &&
               ((nPrevMask & TITLE) == 0 ||
                  i < nCount - 1 && Character.isLowerCase(sName.charAt(i + 1))) ||
            ((nMask | nPrevMask) & SPACE) != 0))
         {
            if ((nPrevMask & SPACE) == 0)
            {
               if (buf.length() != 0)
               {
                  buf.append(' ');
               }

               if (bCapital)
               {
                  buf.append(Character.toTitleCase(sName.charAt(nStart)));
                  buf.append(sName, nStart + 1, i);
               }
               else
               {
                  while (nStart < i)
                  {
                     buf.append(Character.toLowerCase(sName.charAt(nStart++)));
                  }
               }
            }

            nStart = i;
         }

         nPrevMask = nMask;
         ++i;
      }
      while (nStart < nCount);

      return buf.toString();
   }

   /**
    * Calculate UTF-8 byte length of string.
    * For UTF-8 ranges @see http://en.wikipedia.org/wiki/Comparison_of_Unicode_encodings
    * @param sValue The string to calculate byte length for. Can be null.
    * @return Byte length of string.
    */
   public static int utf8Length(String sValue)
   {
      if (sValue == null)
      {
         return 0;
      }

      int nSize = 0;

      for (int i = 0, nCount = sValue.length(); i < nCount; ++i)
      {
         int nCh = sValue.codePointAt(i);

         if (nCh <= 0x00007F)
         {
            ++nSize; // codepoints 0x000000 - 00007F use 1 byte in UTF-8
         }
         else if (nCh <= 0x0007FF)
         {
            nSize += 2; // codepoints 0x000080 - 0x0007FF use 2 bytes in UTF-8
         }
         else if (nCh <= 0x00FFFF)
         {
            nSize += 3; // codepoints 0x000800 - 0x00FFFF use 3 bytes in UTF-8
         }
         else // Unicode does not allocate codepoint > 0x10FFFF
         {
            nSize += 4; // 2 UTF-16 chars always 4 bytes in UTF-8
            ++i; // consume next char since this codepoint requires 2 UTF-16 chars
         }
      }

      return nSize;
   }

   /**
    * Converts all of the characters in the given string to title case
    * using the rules of the given locale.
    * @param sValue The string to convert to title case.
    * @param locale The locale.
    * @return The string, converted to title case.
    */
   public static String toTitleCase(String sValue, Locale locale)
   {
      int nLength = sValue.length();
      StringBuilder sBuf = new StringBuilder(sValue.toLowerCase(locale));
      BreakIterator itr = BreakIterator.getWordInstance(locale);

      itr.setText(sValue);

      for (int i = itr.first(); i < nLength; i = itr.next())
      {
         sBuf.setCharAt(i, Character.toTitleCase(sValue.charAt(i)));
      }

      return sBuf.toString();
   }

   /**
    * Computes a case-insensitive string hash.
    * @param s The string.
    * @return The hash code.
    */
   public static int hashIgnoreCase(String s)
   {
      int nHash = 0;

      for (int i = s.length() - 1; i >= 0 && nHash < 0x100000; i -= 2)
      {
         nHash = (nHash << 1) ^ Character.toLowerCase(s.charAt(i));
      }

      return nHash;
   }
}
