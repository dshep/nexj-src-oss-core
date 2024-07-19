// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.net.URI;
import java.net.URISyntaxException;
import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
 * HTTP utilities.
 */
public class HTTP
{
   // constants

   // authentication constants
   
   /**
    * Authentication request header.
    */
   public final static String AUTH_REQUEST_HEADER = "Authorization";

   /**
    * Authentication response header.
    */
   public final static String AUTH_RESPONSE_HEADER = "WWW-Authenticate";

   /**
    * Proxy authentication request header.
    */
   public final static String PROXY_AUTH_REQUEST_HEADER = "Proxy-Authorization";

   /**
    * Proxy authentication response header.
    */
   public final static String PROXY_AUTH_RESPONSE_HEADER = "Proxy-Authenticate";

   /**
    * Non-standard proxy support header field for SPNEGO authentication through a proxy. From RFC4559.
    */
   public final static String PROXY_SUPPORT_RESPONSE_HEADER = "Proxy-Support";

   /**
    * Non-standard proxy support header value for SPNEGO authentication through a proxy. From RFC4559.
    */
   public final static String PROXY_SUPPORT_SESSION_BASED_AUTH = "Session-Based-Authentication";

   /**
    * The basic authentication protocol name.
    */
   public final static String BASIC_PROTOCOL = "Basic";
   
   /**
    * The basic authentication protocol name length.
    */
   public final static int BASIC_LENGTH = BASIC_PROTOCOL.length();
   
   /**
    * The SPNEGO authentication protocol name.
    */
   public final static String SPNEGO_PROTOCOL = "Negotiate";
   
   /**
    * The SPNEGO authentication protocol name length.
    */
   public final static int SPNEGO_LENGTH = SPNEGO_PROTOCOL.length();

   // HTTP constants

   /**
    * HTTP scheme.
    */
   public final static String SCHEME_HTTP = "http";

   /**
    * HTTPS scheme.
    */
   public final static String SCHEME_SSL = "https";

   /**
    * HTTP protocol 1.0 string.
    */
   public final static String HTTP_1_0 = "HTTP/1.0";

   /**
    * HTTP protocol 1.1 string.
    */
   public final static String HTTP_1_1 = "HTTP/1.1";

   /**
    * Length of the HTTP/ string.
    */
   public final static int HTTP_LENGTH = 5;

   /**
    * CONNECT method.
    */
   public final static String METHOD_CONNECT = "CONNECT";

   /**
    * DELETE method.
    */
   public final static String METHOD_DELETE = "DELETE";

   /**
    * HEAD method.
    */
   public final static String METHOD_HEAD = "HEAD";

   /**
    * GET method.
    */
   public final static String METHOD_GET = "GET";

   /**
    * OPTIONS method.
    */
   public final static String METHOD_OPTIONS = "OPTIONS";

   /**
    * POST method.
    */
   public final static String METHOD_POST = "POST";

   /**
    * PUT method.
    */
   public final static String METHOD_PUT = "PUT";

   /**
    * TRACE method.
    */
   public final static String METHOD_TRACE = "TRACE";

   /**
    * Accept header.
    */
   public final static String HEADER_ACCEPT = "Accept";

   /**
    * Accept-Encoding header.
    */
   public final static String HEADER_ACCEPT_ENCODING = "Accept-Encoding";

   /**
    * Cache-Control header.
    */
   public final static String HEADER_CACHE_CONTROL = "Cache-Control";

   /**
    * Connection header.
    */
   public final static String HEADER_CONNECTION = "Connection";

   /**
    * Content-Encoding header.
    */
   public final static String HEADER_CONTENT_ENCODING = "Content-Encoding";

   /**
    * Content-Length header.
    */
   public final static String HEADER_CONTENT_LENGTH = "Content-Length";

   /**
    * Content-Type header.
    */
   public final static String HEADER_CONTENT_TYPE = "Content-Type";

   /**
    * Cookie header.
    */
   public final static String HEADER_COOKIE = "Cookie";

   /**
    * Host header.
    */
   public final static String HEADER_HOST = "Host";

   /**
    * Keep-Alive header.
    */
   public final static String HEADER_KEEP_ALIVE = "Keep-Alive";

   /**
    * Pragma header.
    */
   public final static String HEADER_PRAGMA = "Pragma";

   /**
    * Proxy-Connection (non-standard) header.
    */
   public final static String HEADER_PROXY_CONNECTION = "Proxy-Connection";

   /**
    * Set-Cookie header.
    */
   public final static String HEADER_SET_COOKIE = "Set-Cookie";

   /**
    * Transfer-Encoding header.
    */
   public final static String HEADER_TRANSFER_ENCODING = "Transfer-Encoding";

   /**
    * User-Agent header.
    */
   public final static String HEADER_USER_AGENT = "User-Agent";

   /**
    * Info - Continue status code.
    */
   public final static int STATUS_CONTINUE = 100;

   /**
    * Info - Switching Protocols status code.
    */
   public final static int STATUS_SWITCHING_PROTOCOLS = 101;

   /**
    * Success - OK status code.
    */
   public final static int STATUS_OK = 200;

   /**
    * Success - Created status code.
    */
   public final static int STATUS_CREATED = 201;

   /**
    * Success - Accepted status code.
    */
   public final static int STATUS_ACCEPTED = 202;

   /**
    * Success - Non-authoritative information status code.
    */
   public final static int STATUS_NON_AUTHORITATIVE = 203;

   /**
    * Success - No content status code.
    */
   public final static int STATUS_NO_CONTENT = 204;

   /**
    * Success - Reset content status code.
    */
   public final static int STATUS_RESET_CONTENT = 205;

   /**
    * Success - Partial content status code.
    */
   public final static int STATUS_PARTIAL_CONTENT = 206;

   /**
    * Redirection - Multiple choices status code.
    */
   public final static int STATUS_MULTIPLE_CHOICES = 300;

   /**
    * Redirection - Moved permanently status code.
    */
   public final static int STATUS_MOVED_PERMANENTLY = 301;

   /**
    * Redirection - Found status code.
    */
   public final static int STATUS_FOUND = 302;

   /**
    * Redirection - See other status code.
    */
   public final static int STATUS_SEE_OTHER = 303;

   /**
    * Redirection - Not modified status code.
    */
   public final static int STATUS_NOT_MODIFIED = 304;

   /**
    * Redirection - Use proxy status code.
    */
   public final static int STATUS_USE_PROXY = 305;

   /**
    * Redirection - Temporary redirect status code.
    */
   public final static int STATUS_TEMPORARY_REDIRECT = 307;

   /**
    * Client error - Bad request status code.
    */
   public final static int STATUS_BAD_REQUEST = 400;

   /**
    * Client error - Unauthorized status code.
    */
   public final static int STATUS_UNAUTHORIZED = 401;

   /**
    * Client error - Forbidden status code.
    */
   public final static int STATUS_FORBIDDEN = 403;

   /**
    * Client error - Not found status code.
    */
   public final static int STATUS_NOT_FOUND = 404;

   /**
    * Client error - Method not allowed status code.
    */
   public final static int STATUS_METHOD_NOT_ALLOWED = 405;

   /**
    * Client error - Not acceptable status code.
    */
   public final static int STATUS_NOT_ACCEPTABLE = 406;

   /**
    * Client error - Proxy authentication required status code.
    */
   public final static int STATUS_PROXY_AUTHENTICATION = 407;

   /**
    * Client error - Request timeout status code.
    */
   public final static int STATUS_REQUEST_TIMEOUT = 408;

   /**
    * Client error - Conflict status code.
    */
   public final static int STATUS_CONFLICT = 409;

   /**
    * Client error - Gone status code.
    */
   public final static int STATUS_GONE = 410;

   /**
    * Client error - Length required status code.
    */
   public final static int STATUS_LENGTH_REQUIRED = 411;

   /**
    * Client error - Precondition failed status code.
    */
   public final static int STATUS_PRECONDITION_FAILED = 412;

   /**
    * Client error - Request entity too large status code.
    */
   public final static int STATUS_REQUEST_TOO_LARGE = 413;

   /**
    * Client error - Request URI too long status code.
    */
   public final static int STATUS_REQUEST_URI_TOO_LONG = 414;

   /**
    * Client error - Unsupported media type status code.
    */
   public final static int STATUS_UNSUPPORTED_MEDIA = 415;

   /**
    * Client error - Request range not satisfiable status code.
    */
   public final static int STATUS_REQUEST_RANGE_UNSATISFIABLE = 416;

   /**
    * Client error - Expectation failed status code.
    */
   public final static int STATUS_EXPECTATION_FAILED = 417;

   /**
    * Server error - Internal server error status code.
    */
   public final static int STATUS_INTERNAL_SERVER_ERROR = 500;

   /**
    * Server error - Not implemented status code.
    */
   public final static int STATUS_NOT_IMPLEMENTED = 501;

   /**
    * Server error - Bad gateway status code.
    */
   public final static int STATUS_BAD_GATEWAY = 502;

   /**
    * Server error - Service unavailable status code.
    */
   public final static int STATUS_SERVICE_UNAVAILABLE = 503;

   /**
    * Server error - Gateway timeout status code.
    */
   public final static int STATUS_GATEWAY_TIMEOUT = 504;

   /**
    * Server error - HTTP version not supported status code.
    */
   public final static int STATUS_HTTP_VERSION_NOT_SUPPORTED = 505;

   /**
    * Encoding - application/x-www-form-urlencoded
    */
   public final static String ENCODING_FORM = "application/x-www-form-urlencoded";

   // header type constants

   /**
    * String header.
    */
   public final static byte TYPE_STRING = 0;
   
   /**
    * Integer header.
    */
   public final static byte TYPE_INTEGER = 1;

   /**
    * Date header.
    */
   public final static byte TYPE_DATE = 2;

   /**
    * The GMT time zone.
    */
   protected final static TimeZone s_tzGMT = TimeZone.getTimeZone("GMT");

   /**
    * The header date format (RFC 822/RFC 1123).
    */
   protected final static SimpleDateFormat s_headerDateTimeFormat = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss zzz", Locale.ENGLISH);

   /**
    * An obsolete header date format (RFC 850/RFC 1036).
    */
   protected final static SimpleDateFormat s_rfc850DateTimeFormat = new SimpleDateFormat("EEEEE, dd-MMM-yy HH:mm:ss zzz", Locale.ENGLISH);

   /**
    * An obsolete header date format (ANSI C).
    */
   protected final static SimpleDateFormat s_cDateTimeFormat = new SimpleDateFormat("EEE MMM dd HH:mm:ss yyyy", Locale.ENGLISH);

   /**
    * The Netscape cookie expiration format.
    */
   protected final static SimpleDateFormat s_cookieDateTimeFormat = new SimpleDateFormat("EEE, dd-MMM-yyyy HH:mm:ss zzz", Locale.ENGLISH);

   /**
    * Header name to type map: Byte[String].
    */
   protected final static Lookup s_typeMap = new HashTab();
   
   static
   {
      Byte integerType = new Byte(TYPE_INTEGER);
      String[] integerHeaders = new String[]{"age", "content-length", "max-forwards"};
      
      for (int i = 0; i != integerHeaders.length; ++i)
      {
         s_typeMap.put(integerHeaders[i], integerType);
      }

      Byte dateType = new Byte(TYPE_DATE);
      String[] dateHeaders = new String[]{"date", "expires", "if-modified-since",
         "if-unmodified-since", "last-modified"};

      for (int i = 0; i != dateHeaders.length; ++i)
      {
         s_typeMap.put(dateHeaders[i], dateType);
      }
      
      s_headerDateTimeFormat.setTimeZone(s_tzGMT);
      s_rfc850DateTimeFormat.setTimeZone(s_tzGMT);
      s_cDateTimeFormat.setTimeZone(s_tzGMT);
      s_cookieDateTimeFormat.setTimeZone(s_tzGMT);
   }

   // constructors

   /**
    * Prevents construction.
    */
   protected HTTP()
   {
   }
   
   // operations
   
   /**
    * Determines the header type from its name.
    * @param sName The header name (lower case).
    * @return One of the TYPE_* constants.
    */
   public static byte getHeaderType(String sName)
   {
      Byte type = (Byte)s_typeMap.get(sName);
      
      if (type == null)
      {
         return TYPE_STRING;
      }

      return type.byteValue();
   }

   /**
    * Formats a datetime value.
    * @param dt The date value.
    * @return The formatted value.
    */
   public static String formatDateTime(Date dt)
   {
      return ((SimpleDateFormat)s_headerDateTimeFormat.clone()).format(dt);
   }

   /**
    * Parses a datetime string.
    * @param s The string to parse.
    * @return The parsed timestamp.
    * @throws IllegalArgumentException if the dateatime syntax is incorrect.
    */
   public static Timestamp parseDateTime(String s)
   {
      SimpleDateFormat format;
      
      if (s.indexOf(',') < 0)
      {
         format = s_cDateTimeFormat;
      }
      else if (s.indexOf('-') > 0)
      {
         format = s_rfc850DateTimeFormat;
      }
      else
      {
         format = s_headerDateTimeFormat;
      }

      try
      {
         return new Timestamp(((SimpleDateFormat)format.clone()).parse(s).getTime());
      }
      catch (ParseException e)
      {
         throw new IllegalArgumentException("Invalid HTTP date", e);
      }
   }

   /**
    * Formats a Netscape cookie datetime value.
    * @param dt The date value.
    * @return The formatted value.
    */
   public static String formatCookieDateTime(Date dt)
   {
      return ((SimpleDateFormat)s_cookieDateTimeFormat.clone()).format(dt);
   }

   /**
    * Parses a Netscape cookie datetime string.
    * @param s The string to parse.
    * @return The parsed timestamp.
    * @throws IllegalArgumentException if the datetime syntax is incorrect.
    */
   public static Timestamp parseCookieDateTime(String s)
   {
      try
      {
         return new Timestamp(((SimpleDateFormat)s_cookieDateTimeFormat.clone()).parse(s).getTime());
      }
      catch (ParseException e)
      {
         throw new IllegalArgumentException("Invalid cookie date", e);
      }
   }

   /**
    * Converts a primitive value to a string suitable for HTTP headers.
    * @param value The value to convert.
    * @return The resulting string.
    */
   public static String toString(Object value)
   {
      if (value == null)
      {
         return "";
      }
      
      if (value instanceof String)
      {
         return (String)value;
      }
      
      if (value instanceof Date)
      {
         return formatDateTime((Date)value);
      }
      
      return String.valueOf(value);
   }

   /**
    * Gets the path portion of a URI. E.g. converts "http://example.com/test"
    * to "/test".
    * 
    * @param sURI The URI. May be null.
    * @return The path part of the URI; null if there was no path part specified.
    */
   public static String getContextPath(String sURI)
   {
      if (sURI == null || sURI.length() == 0)
      {
         return null;
      }
   
      try
      {
         URI fullURI = new URI(sURI);
         String sResult = StringUtil.trimToNull(fullURI.getPath());
   
         if (sResult != null)
         {
            if (sResult.length() > 1 && sResult.charAt(sResult.length() - 1) == '/')
            {
               sResult = sResult.substring(0, sResult.length() - 1);
            }
   
            return sResult;
         }
      }
      catch (URISyntaxException ex)
      {
         return null;
      }
   
      return null;
   }
}
