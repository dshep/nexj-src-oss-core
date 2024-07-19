// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Locale;
import java.util.Properties;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * MIME type related utilities.
 */
public class MIMEUtil
{
   // constants

   /**
    * Unknown binary data MIME type.
    */
   public final static String BINARY_MIME_TYPE = "application/octet-stream";

   /**
    * Pattern for extracting the HTML before the BODY tag.
    */
   protected final static Pattern HTML_PREFIX_PATTERN =
      Pattern.compile("(.*?)<body\\b.*?", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);

   /**
    * Pattern for extracting the HTML charset attribute.
    */
   protected final static Pattern HTML_CHARSET_PATTERN = Pattern.compile(
      ".*?<meta\\b[^>]*?\\bcontent\\s*=\\s*\"?[^\">]*?\\bcharset\\s*=\\s*\"?([^\\s>/\"]+).*?",
      Pattern.CASE_INSENSITIVE | Pattern.DOTALL);

   // associations

   /**
    * Map of file extension to MIME type.
    */
   protected final static Lookup s_mimeMap;

   /**
    * Map of MIME type to file extension.
    */
   protected final static Lookup s_extMap;

   /**
    * Set of MIME types which are unsafe for storing.
    */
   protected final static Set s_unsafeMIMESet = new HashHolder(5);

   /**
    * Set of file extension corresponding to universally recognizable image formats.
    */
   protected final static Set s_safeImageExtSet = new HashHolder(5);

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(MIMEUtil.class);

   static
   {
      Properties properties = new Properties();
      InputStream istream = null; 

      try
      {
         istream = URLUtil.openResource(MIMEUtil.class, "mime.properties");
         properties.load(istream);
      }
      catch (IOException e)
      {
         s_logger.error("Unable to load the default MIME type map", e);
      }
      finally
      {
         IOUtil.close(istream);
      }

      int nSize = properties.size();

      s_mimeMap = new HashTab(nSize);
      s_extMap = new HashTab(nSize);

      for (Enumeration enm = properties.propertyNames(); enm.hasMoreElements();)
      {
         String sKey = (String)enm.nextElement();
         String sValue = (String)properties.get(sKey);

         // Code below is same as StringUtil.split(sValue, ' ') but uses less intermediate objects
         // for copying data wince MimeUtil is used by mobile clients.
         int nLen = sValue.length();
         int nStart = 0;

         for (boolean bFirst = true;;)
         {
            while (nStart < nLen && sValue.charAt(nStart) == ' ')
            {
               ++nStart;
            }

            int nEnd = sValue.indexOf(' ', nStart);

            if (nEnd < 0)
            {
               if (nStart == nLen)
               {
                  break;
               }

               nEnd = nLen;
            }

            String sExt = sValue.substring(nStart, nEnd);

            if (bFirst)
            {
               bFirst = false;
               s_extMap.put(sKey, sExt);
            }

            nStart = nEnd;
            s_mimeMap.put(sExt, sKey);
         }
      }

      s_unsafeMIMESet.add("application/hta");
      s_unsafeMIMESet.add("application/x-javascript");
      s_unsafeMIMESet.add("application/xhtml+xml");
      s_unsafeMIMESet.add("message/rfc822");
      s_unsafeMIMESet.add("text/html");

      s_safeImageExtSet.add("bmp");
      s_safeImageExtSet.add("gif");
      s_safeImageExtSet.add("jpeg");
      s_safeImageExtSet.add("jpg");
      s_safeImageExtSet.add("png");
   }

   // constructors
   
   /**
    * Prevents construction.
    */
   protected MIMEUtil()
   {
   }
   
   // operations
   
   /**
    * Finds a MIME type by file extension.
    * @param sExt The file extension, lower case (e.g. "html"). Can be null.
    * @return The MIME type, or null if not found.
    */
   public static String findMIMEType(String sExt)
   {
      if (sExt == null)
      {
         return null;
      }

      return (String)s_mimeMap.get(sExt);
   }

   /**
    * Finds a file extension by MIME type.
    * @param sType The MIME type. May contain ";" modifiers. Can be null.
    * @return The file extension, lower case, or null if not found.
    */
   public static String findExt(String sType)
   {
      sType = normalizeMIMEType(sType);

      if (sType == null)
      {
         return null;
      }

      return (String)s_extMap.get(sType);
   }

   /**
    * Try to determine the MIME type automatically from the object. The object state is not
    * modified e.g. If the object is a reader or a stream the read position will not be advanced.
    * @param obj The object to examine.
    * @return The MIME type of the object or null if unknown.
    */
   public static String getMIMEType(Object obj)
   {
      if (obj instanceof Binary || obj instanceof InputStream)
      {
         return "application/octet-stream";
      }

      if (obj instanceof CharSequence)
      {
         CharSequence charSeq = (CharSequence)obj;
         int nStart = 0;
         int nCount = charSeq.length();

         for (; nStart < nCount && Character.isWhitespace(charSeq.charAt(nStart)); ++nStart);

         int nLength = nCount - nStart;

         if (nLength > 1 && charSeq.charAt(nStart) == '<')
         {
            char ch = charSeq.charAt(nStart + 1);

            if (nLength > "<xml".length() && // case sensitive
                ch == 'x' &&
                charSeq.charAt(nStart + 2) == 'm' &&
                charSeq.charAt(nStart + 3) == 'l')
            {
               return "text/xml; charset=UTF-8";
            }

            ch = Character.toLowerCase(ch);

            if (nLength > "<html".length() && // case insensitive
                ch == 'h' &&
                Character.toLowerCase(charSeq.charAt(nStart + 2)) == 't' &&
                Character.toLowerCase(charSeq.charAt(nStart + 3)) == 'm' &&
                Character.toLowerCase(charSeq.charAt(nStart + 4)) == 'l')
            {
               String sCharset = getHTMLCharSet(charSeq);

               return (sCharset == null) ? "text/html" : ("text/html; charset=" + sCharset);
            }

            if (nLength > "<!doctype ".length() && // case insensitive
                ch == '!' &&
                Character.toLowerCase(charSeq.charAt(nStart + 2)) == 'd' &&
                Character.toLowerCase(charSeq.charAt(nStart + 3)) == 'o' &&
                Character.toLowerCase(charSeq.charAt(nStart + 4)) == 'c' &&
                Character.toLowerCase(charSeq.charAt(nStart + 5)) == 't' &&
                Character.toLowerCase(charSeq.charAt(nStart + 6)) == 'y' &&
                Character.toLowerCase(charSeq.charAt(nStart + 7)) == 'p' &&
                Character.toLowerCase(charSeq.charAt(nStart + 8)) == 'e' &&
                Character.isWhitespace(charSeq.charAt(nStart + 9)))
            {
               for (nStart += "<!doctype ".length();
                    nStart < nCount && Character.isWhitespace(charSeq.charAt(nStart));
                    ++nStart);

               nLength = nCount - nStart;

               if (nLength >= "html ".length() && // case insensitive
                   Character.toLowerCase(charSeq.charAt(nStart + 0)) == 'h' &&
                   Character.toLowerCase(charSeq.charAt(nStart + 1)) == 't' &&
                   Character.toLowerCase(charSeq.charAt(nStart + 2)) == 'm' &&
                   Character.toLowerCase(charSeq.charAt(nStart + 3)) == 'l' &&
                   Character.isWhitespace(charSeq.charAt(nStart + 4)))
               {
                  String sCharset = getHTMLCharSet(charSeq);

                  return (sCharset == null) ? "text/html" : ("text/html; charset=" + sCharset);
               }
            }
         }

         return "text/plain";
      }

      return null;
   }

   /**
    * Extracts the HTML character set from a META tag.
    * @param sHTML The HTML string.
    * @return The extracted character set, or the null if not found.
    */
   public static String getHTMLCharSet(CharSequence sHTML)
   {
      Matcher matcher = HTML_PREFIX_PATTERN.matcher(sHTML);

      if (matcher.matches())
      {
         matcher = HTML_CHARSET_PATTERN.matcher(matcher.group(1));

         if (matcher.matches())
         {
            return matcher.group(1);
         }
      }

      return null;
   }

   /**
    * Determines if a given MIME type is safe for storing.
    * @param sType The MIME type. Can be null.
    * @return True if the MIME type is safe for storing.
    */
   public static boolean isSafeMIMEType(String sType)
   {
      sType = normalizeMIMEType(sType);

      if (sType == null)
      {
         return false;
      }

      return s_extMap.contains(sType) && !s_unsafeMIMESet.contains(sType);
   }

   /**
    * Determines if a file with a given extension is safe for storing.
    * @param sExt File extension to be checked, lower case. Can be null.
    * @return True if the file is safe for storing.
    */
   public static boolean isSafeExt(String sExt)
   {
      String sType = findMIMEType(sExt);

      if (sType == null)
      {
         return false;
      }

      return !s_unsafeMIMESet.contains(sType);
   }

   /**
    * Determines if a given file extension corresponds to a universally recognizable image format.
    * @param sExt File extension to be checked, lower case. Can be null.
    * @return True if the given file extension corresponds to a universally recognizable image format.
    */
   public static boolean isSafeImageExt(String sExt)
   {
      if (sExt == null)
      {
         return false;
      }

      return s_safeImageExtSet.contains(sExt);
   }

   /**
    * @return The safe image extension iterator.
    */
   public static Iterator getSafeImageExtIterator()
   {
      return s_safeImageExtSet.iterator();
   }

   /**
    * Determines is a MIME type represents binary data.
    * @param sType The MIME type. May contain ";" modifiers. Can be null.
    * @return True if this is a binary MIME type.
    */
   public static boolean isBinaryMIMEType(String sType)
   {
      if (sType == null)
      {
         return true;
      }

      if (sType.regionMatches(true, 0, "text/", 0, 5))
      {
         return false;
      }

      int i = sType.indexOf(';') + 1;

      if (i != 0)
      {
         for (int n = sType.length() - 9; i < n; ++i)
         {
            if (sType.regionMatches(true, i, "charset", 0, 7))
            {
               char ch = sType.charAt(i - 1);

               if (Character.isWhitespace(ch) || ch == ';')
               {
                  ch = sType.charAt(i + 7);

                  if (Character.isWhitespace(ch) || ch == '=')
                  {
                     return false;
                  }
               }
            }
         }
      }

      return true;
   }

   /**
    * Normalizes a MIME type by removing ";" modifiers and lowercasing the name.
    * @param sType The MIME type to normalize. Can be null.
    * @return The normalized MIME type.
    */
   public static String normalizeMIMEType(String sType)
   {
      if (sType == null)
      {
         return null;
      }

      int i = sType.indexOf(';');

      if (i >= 0)
      {
         while (i > 0 && Character.isWhitespace(sType.charAt(i - 1)))
         {
            --i;
         }

         sType = sType.substring(0, i);
      }

      return sType.toLowerCase(Locale.ENGLISH);
   }
}
