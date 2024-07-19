// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * URI-related utilities.
 */
public class URIUtil
{
   // constants
   
   protected final static Pattern ESCAPE_PATTERN = Pattern.compile("[+\\s\\{\\}\\[\\]\\u007F-\\uFFFF\\^|`]"); 
   
   // constructors

   /**
    * Prevents construction.
    */
   protected URIUtil()
   {
   }

   // operations
   
   /**
    * Parses a URI and fixes some common errors in it.
    * @param sURI The URI to parse.
    * @throws URISyntaxException if the URI is malformed.
    */
   public static URI parse(String sURI) throws URISyntaxException
   {
      if (sURI == null)
      {
         return null;
      }
      
      Matcher matcher = ESCAPE_PATTERN.matcher(sURI);
      StringBuffer buf = new StringBuffer(sURI.length() + 16);

      try
      {
         while (matcher.find())
         {
            String s = matcher.group();

            switch (s.charAt(0))
            {
               case ' ':
               case '+':
                  s = "%20";
                  break;

               default:
                  s = URLEncoder.encode(s, "UTF-8");
            }

            matcher.appendReplacement(buf, s);
         }
      }
      catch (UnsupportedEncodingException e)
      {
         throw new URISyntaxException(sURI, e.getMessage());
      }

      matcher.appendTail(buf);

      return new URI(buf.toString());
   }

   /**
    * Extracts host name and port from a URI String.
    * 
    * @param sURI The URI String.
    * @return The "host.name:port" specified in the URI; null if argument contains
    *         no host name.
    */
   public static String getHostPort(String sURI)
   {
      if (sURI == null)
      {
         return null;
      }

      String sHostName;

      try
      {
         URI locationURI = new URI(sURI);

         sHostName = locationURI.getHost();

         if (sHostName != null && locationURI.getPort() != -1)
         {
            sHostName += ":" + locationURI.getPort();
         }
      }
      catch (Exception ex)
      {
         sHostName = null;
      }

      return sHostName;
   }
}
