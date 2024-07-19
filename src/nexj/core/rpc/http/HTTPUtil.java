// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLDecoder;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Set;
import java.util.zip.CRC32;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import nexj.core.meta.Metadata;
import nexj.core.rpc.RPCSizeException;
import nexj.core.util.Binary;
import nexj.core.util.HTTP;
import nexj.core.util.IOUtil;
import nexj.core.util.LimitIOException;
import nexj.core.util.LimitInputStream;
import nexj.core.util.Lookup;
import nexj.core.util.LookupHashMap;
import nexj.core.util.MIMEHeader;
import nexj.core.util.MultipartDataException;
import nexj.core.util.MultipartInputStream;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * HTTP server utility class.
 */
public class HTTPUtil
{
   // constants

   /**
    * The name of the HttpServletRequest attribute that contains the X.509 certificate
    * chain produced by the client to authenticate itself.
    */
   public final static String CLIENT_CERTIFICATE_ATTRIBUTE_NAME = "javax.servlet.request.X509Certificate";

   // operations

   /**
    * @param sAnonymousAddress Anonymous Address.
    * @param sContextPath Request context path.
    * @param sScheme Request address scheme.
    * @param nPort Request port.
    * @param sServerName Request server name.
    * @param bSecure True if secure request.
    * @return The anonymous server root.
    */
   public static String computeAnonymousRoot(String sAnonymousAddress, String sContextPath, 
      String sScheme, int nPort, String sServerName, boolean bSecure)
   {
      String sAnonymousRoot = null;
      
      if (sAnonymousAddress != null)
      {
         sAnonymousRoot = sAnonymousAddress + sContextPath;
      }
      else
      {
         if (bSecure)
         {
            sScheme = "http";
            nPort = computePort(nPort, false);
         }

         try
         {
            sAnonymousRoot = new URI(sScheme, null, sServerName, nPort, sContextPath, null, null).toString();
         }
         catch (URISyntaxException e)
         {
            sAnonymousRoot = "";
         }
      }

      return sAnonymousRoot;
   }


   /**
    * Computes the server root.
    * 
    * @param sAddress     Address, can be null.
    * @param sContextPath Request context path.
    * @param sScheme      Request address scheme.
    * @param nPort        Request port.
    * @param sServerName  Request server name.
    * @param bSecure      True if secure request.
    * 
    * @return A String URI of the form "scheme://host.domain:port/context/path".
    */
   public static String computeRoot(String sAddress, String sContextPath, String sScheme,
      int nPort, String sServerName, boolean bSecure)
   {
      String sRoot = null;

      if (sAddress != null)
      {
         sRoot = sAddress + sContextPath;
      }
      else
      {
         if (bSecure)
         {
            sScheme = "https";
            nPort = computePort(nPort, true);
         }

         try
         {
            sRoot = new URI(sScheme, null, sServerName, nPort, sContextPath, null, null).toString();
         }
         catch (URISyntaxException e)
         {
            sRoot = "";
         }
      }

      return sRoot;
   }

   /**
    * Computes an HTTPS port based on an HTTP port.
    * @param nHTTPPort The HTTP port.
    * @return The HTTPS port.
    */
   public static int computePort(int nPort, boolean bSecure)
   {
      return (nPort / 1000) * 1000 + ((bSecure) ? 443 : 80);
   }

   /**
    * Gets the host URI from a URI. E.g. converts
    * "http://example.com/test" to "http://example.com".
    * 
    * @param sURI The URI. May be null.
    * @return The host URI; null if there was no host specified.
    */
   public static String getHostURI(String sURI)
   {
      if (sURI == null || sURI.length() == 0 || sURI.charAt(0) == '/')
      {
         return null;
      }

      try
      {
         URI fullURI = new URI(sURI);
         URI hostURI = new URI(fullURI.getScheme(), null, fullURI.getHost(), fullURI.getPort(), null, null, null);

         return StringUtil.trimToNull(hostURI.toString());
      }
      catch (URISyntaxException ex)
      {
         return null;
      }
   }

   /**
    * Gets the name of the resource requested by a given URI. This is determined
    * by the last segment found in the URI before any query parameters.
    * 
    * @param sURI
    * @return Name of the resource requested.
    */
   public static String getResourceName(String sURI)
   {
      if (sURI == null)
      {
         return null;
      }
      
      String sResourceName = sURI;

      int nQueryParamIdx = sURI.lastIndexOf('?');

      if (nQueryParamIdx > -1)
      {
         sResourceName = sResourceName.substring(0, nQueryParamIdx);
      }

      int nLastSlashIdx = sResourceName.lastIndexOf('/');

      if (nLastSlashIdx > -1)
      {
         sResourceName = sResourceName.substring(nLastSlashIdx + 1);
      }

      return sResourceName;
   }

   /**
    * Computes the relative path (without trailing slash) needed to obtain the
    * context root given a servlet path.
    * 
    * @param sServletPath
    * @return A path prefix that can be used to obtain the context root. The
    *         path will be in the form of '.' or '../..'
    */
   public static String computeRelativeRoot(String sServletPath)
   {
      int nLastIdx = (sServletPath == null) ? -1 : sServletPath.lastIndexOf('/');

      // Case where path is immediately off the context root
      if (nLastIdx <= 0)
      {
         return ".";
      }

      // Case where path is several levels deep
      StringBuilder sbPath = new StringBuilder("..");
      nLastIdx = sServletPath.lastIndexOf('/', nLastIdx - 1);

      while (nLastIdx > -1)
      {
         if (nLastIdx > 0)
         {
            sbPath.append("/..");
         }
         else
         {
            break;
         }

         nLastIdx = sServletPath.lastIndexOf('/', nLastIdx - 1);
      }

      return sbPath.toString();
   }

   /**
    * Sets the response caching headers.
    * @param response The HTTP response.
    * @param bCached True to cache.
    */
   public static void setCached(HttpServletResponse response, boolean bCached)
   {
      response.setHeader(HTTP.HEADER_CACHE_CONTROL, (bCached) ? "public" : "no-cache");

      if (response.containsHeader(HTTP.HEADER_PRAGMA))
      {
         response.setHeader(HTTP.HEADER_PRAGMA, (bCached) ? "public" : "no-cache");
      }
   }

   /**
    * Verifies that a given trusted certificate authorizes a client (that presented
    * the given client certificate chain). 
    * 
    * @param trustedCert The trusted certificate to look for.
    * @param clientChain The certificate chain presented by the client.
    * @return True if trustedCert is in clientChain; false otherwise.
    */
   public static boolean isCertificateMatched(Certificate trustedCert, X509Certificate[] clientChain)
   {
      for (int nCertificate = 0; nCertificate < clientChain.length; nCertificate++)
      {
         X509Certificate clientCert = clientChain[nCertificate];

         if (clientCert.equals(trustedCert))
         {
            return true;
         }
      }

      return false;
   }

   /**
    * Inspects an HTTP request to see if client certificate authentication is
    * being used.
    * 
    * @param request The request to inspect.
    * @return True if the request was made with client certificate authentication;
    * false otherwise.
    */
   public static boolean isUsingClientCertificateAuthentication(HttpServletRequest request)
   {
      /*
       * Tomcat version 5.5.20 bug 40524: Tomcat returns "CLIENT-CERT" instead
       * of CLIENT_CERT_AUTH (which is "CLIENT_CERT").
       * http://issues.apache.org/bugzilla/show_bug.cgi?id=40524
       */
      return HttpServletRequest.CLIENT_CERT_AUTH.equals(request.getAuthType()) ||
         "CLIENT-CERT".equals(request.getAuthType());
   }

   /**
    * Determines if the HTTP request is being made on the anonymous HTTP context.
    * 
    * @param request The request to inspect.
    * @param metadata The metadata.
    * @return True if the HTTP request is being made on the anonymous HTTP context.
    */
   public static boolean isAnonymousRequest(HttpServletRequest request, Metadata metadata)
   {
      String sPath = request.getContextPath();

      return (sPath != null && sPath.equals(metadata.getHTTPAnonymousContextRoot()));
   }

   /**
    * Parses a string containing parameters and their values into a parameter map.
    * 
    * @param sQuery The string containing the parameters and their values.
    * @param ignoreSet The set of parameters to ignore.
    * @return A map containing the parameters and their values. 
    */
   public static Lookup parseQuery(String sQuery, Set ignoreSet)
   {
      Lookup result = null;
      
      int nQueryLength = (sQuery == null) ? 0 : sQuery.length();

      for (int i = 0; i < nQueryLength;)
      {
         int j = sQuery.indexOf('&', i);

         if (j < 0)
         {
            j = nQueryLength;
         }

         int k = sQuery.indexOf('=', i);

         if (k < 0 || k > j)
         {
            k = j;
         }

         try
         {
            String sName = URLDecoder.decode(sQuery.substring(i, k), GenericHTTPServer.ENCODING);
            
            if (ignoreSet == null || !ignoreSet.contains(sName))
            {
               String sValue;

               if (k != j && k + 1 < nQueryLength)
               {
                  sValue = URLDecoder.decode(sQuery.substring(k + 1, j), GenericHTTPServer.ENCODING);
               }
               else
               {
                  sValue = "";
               }

               if (result == null)
               {
                  result = new LookupHashMap();
               }

               String[] sValueArray = (String[])result.get(sName);

               if (sValueArray != null)
               {
                  int n = sValueArray.length;
                  String[] sNewValueArray = new String[n + 1];

                  System.arraycopy(sValueArray, 0, sNewValueArray, 0, n);
                  sNewValueArray[n] = sValue;
                  result.put(sName, sNewValueArray);
               }
               else
               {            
                  result.put(sName, new String[]{sValue});
               }
            }
         }
         // If there is a decoding issue then we do not add the key value pair to the result map
         // and fall back on the value in the request parameter map.
         catch (UnsupportedEncodingException e)
         {
         }

         i = j + 1;
      }
      
      return result;
   }
   
   /**
    * Returns a cache cookie based on the metadata passed-in. This key is 
    * designed to be almost unique for every build. 
    * 
    * @param metadata The metadata to calculate the cache cookie against. 
    * @return A unique cache cookie string with length less than 8. Not null.
    */
   public static String getCacheCookie(Metadata metadata)
   {
      String sBuildKey = null;
      CRC32 crc = new CRC32();
      
      try
      {
         crc.update(metadata.getChecksum().getBytes(GenericHTTPServer.ENCODING));
         crc.update(metadata.getVersion().getBytes(GenericHTTPServer.ENCODING));
         updateCRC(crc, (int)SysUtil.BUILD_NUMBER);
         updateCRC(crc, (int)SysUtil.BUILD_TIME);
         updateCRC(crc, (int)(SysUtil.BUILD_TIME >>> 32));
         
         sBuildKey = Long.toString(crc.getValue() & 0xffffffffL, Character.MAX_RADIX);
      }
      catch (UnsupportedEncodingException e)
      {
         ObjUtil.rethrow(e);
      }
      
      return sBuildKey;
   }
   
   /**
    * Updates the given CRC32 checksum with all 32 bits of an integer.
    * 
    * @param crc The target CRC32 checksum to update.
    * @param n A 32-bit integer to update the checksum.
    */
   private static void updateCRC(CRC32 crc, int n)
   {
      for (int i = 0; i < 4; i++)
      {
         crc.update(n);
         n >>>= 8;
      }
   }
   
   /**
    * Adds multi-part form parameters from an input stream to a given Lookup
    * instance.
    * 
    * @param requestInputStream InputStream from which to read request.
    * @param sEncoding Character encoding to be used when reading request.
    * @param sSeparator String used to delimit parts of a multi-part request.
    * @param paramMap Lookup to which request parameters should be added.
    * @param sizeMap Lookup containing max size information for request parameters.
    * @param lDefMaxSize Default max size for a parameter value. 
    * @throws MultipartDataException
    * @throws UnsupportedEncodingException
    * @throws IOException
    */
   public static void addMultiPartParameters(InputStream requestInputStream, String sEncoding, String sSeparator,
      Lookup paramMap, Lookup sizeMap, long lDefMaxSize) throws IOException
   {
      if (paramMap == null)
      {
         return; 
      }
      
      byte[] buf = null;
      MultipartInputStream multipartStream = new MultipartInputStream(requestInputStream, sSeparator.getBytes(sEncoding),
         sEncoding);
      
      while (multipartStream.nextPart())
      {
         MIMEHeader header = multipartStream.getHeaders().find("Content-Disposition");

         if (header == null)
         {
            continue;
         }

         MIMEHeader.Value value = header.getFirstValue();

         if (value == null || !value.getName().equals("form-data") && !value.getName().equals("attachment"))
         {
            continue;
         }

         String sName = value.findArg("name");

         if (sName == null)
         {
            continue;
         }

         String sFileName = getFilenameForUpload(header);
         
         header = multipartStream.getHeaders().find("Content-Type");

         if (header != null && header.getFirstValue() != null && header.getFirstValue().getName().startsWith("multipart/"))
         {
            throw new MultipartDataException("Multiple files per input are not supported");
         }

         long lMaxSize = lDefMaxSize;

         if (sizeMap != null)
         {
            Number maxSize = (Number)sizeMap.get(sName);

            if (maxSize != null)
            {
               lMaxSize = maxSize.longValue();
            }
         }

         InputStream is = multipartStream;
         
         
         if (lMaxSize > 0)
         {
            final boolean bParam = (sFileName == null);

            is = new LimitInputStream(multipartStream, lMaxSize, true)
            {
               /**
                * @see nexj.core.util.LimitInputStream#throwLimitException()
                */
               protected int throwLimitException() throws LimitIOException
               {
                  throw new RPCSizeException((bParam) ? "err.rpc.parameterSize" : "err.rpc.attachmentSize", new Object[]
                  {
                     new Long(m_lMaxCount)
                  });
               }
            };
         }

         if (sFileName == null)
         {
            StringWriter writer = new StringWriter();

            IOUtil.copy(writer, new InputStreamReader(is, sEncoding));
            
            Object currentValArray = paramMap.get(sName);
            String[] valArray = null;
            
            if (!(currentValArray instanceof String[]))
            {
               if (currentValArray == null)
               {
                  valArray = new String[]
                  {
                     writer.toString()
                  };
               }
               else
               {
                  valArray = new String[]
                  {
                     String.valueOf(currentValArray),
                     writer.toString()
                  };
               }
            }
            else
            {
               valArray = new String[((String[])currentValArray).length + 1];
               System.arraycopy(currentValArray, 0, valArray, 0, ((Object[])currentValArray).length);
               valArray[valArray.length - 1] = writer.toString();
            }
            
            paramMap.put(sName, valArray);
         }
         else
         {
            ByteArrayOutputStream ostream = new ByteArrayOutputStream(8192);

            if (buf == null)
            {
               buf = new byte[8192];
            }

            IOUtil.copy(ostream, multipartStream, buf);
            paramMap.put(sName, new Binary(ostream.toByteArray()));
            paramMap.put(sName + ".filename", sFileName);
         }
      }
   }
   
   /**
    * Retrieve's the filename for an upload from the given MIMEHeader instance.
    * 
    * @param header MIMEHeader from which to extract the filename for an upload.
    * @return String containing the filename or NULL if not found.
    */
   public static String getFilenameForUpload(MIMEHeader header)
   {
      if (header != null)
      {
         MIMEHeader.Value value = header.getFirstValue();

         if (value == null || !value.getName().equals("form-data") && !value.getName().equals("attachment"))
         {
            return null;
         }

         String sFileName = value.findArg("filename");

         return (sFileName == null) ? null : sFileName.substring(Math.max(sFileName.lastIndexOf('/'), sFileName
            .lastIndexOf('\\')) + 1);
      }

      return null;
   }
}
