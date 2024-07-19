// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.net.JarURLConnection;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;

/**
 * URL-related utilities.
 */
public final class URLUtil
{
   // constants

   private final static Pattern s_excludePathRegexp = Pattern.compile("CVS|\\..*|.*\\.(rej|orig|bak)|.*~");

   // constructors

   /**
    * Prevents construction.
    */
   protected URLUtil()
   {
   }

   // operations

   /**
    * Checks if the string represents a URL.
    * @param sString The string to check.
    * @return true if the string represents a URL.
    */
   public static boolean isURL(String sString)
   {
      if (sString.indexOf(':') <= 1)
      {
         return false;
      }

      try
      {
         new URL(sString);
      }
      catch (Exception e)
      {
         return false;
      }

      return true;
   }

   /**
    * Returns the URL object corresponding to the given string if it
    * represents a URL, null otherwise.
    * @param sString The string to check.
    * @return The URL object if the string represents a URL, or null.
    */
   public static URL getURL(String sString)
   {
      if (sString.indexOf(':') <= 1)
      {
         return null;
      }

      try
      {
         return new URL(sString);
      }
      catch (Exception e)
      {
         return null;
      }
   }

   /**
    * Converts a path to URL name.
    * @param sPath The path to convert.
    * @return The converted URL name.
    * @throws java.io.IOException if an error occurs.
    */
   public static String toURL(String sPath) throws IOException
   {
      if (URLUtil.isURL(sPath))
      {
         return sPath;
      }

      return new File(sPath).toURL().toString();
   }

   /**
    * Converts a URL to File object.
    * @param url The URL to convert.
    * @return The file object, or null if the conversion failed.
    */
   public static File fileFromURL(URL url)
   {
      try
      {
         return new File(URIUtil.parse(url.toExternalForm()));
      }
      catch (Exception e)
      {
         return null;
      }
   }

   /**
    * Opens a URL stream.
    *
    * @param url The URL to open.
    * @return The input stream.
    * @throws IOException if the stream cannot be opened.
    */
   public static InputStream openStream(URL url) throws IOException
   {
      return openStream(openConnection(url));
   }

   /**
    * Opens a URL stream with additional error handling for a given URL connection.
    * This is used instead of URL.openStream() because IBM JRE 1.4.2 shipped with
    * WebSphere 6.0 does not throw any exceptions.
    *
    * @param url The URL to open.
    * @return The URL connection.
    * @throws IOException if the stream cannot be opened.
    */
   public static InputStream openStream(URLConnection con) throws IOException
   {
      InputStream istream = con.getInputStream();

      if (istream != null)
      {
         return istream;
      }

      throw new IOException("Cannot open stream: \"" + con.getURL() + "\"");
   }

   /**
    * Opens a URL connection with additional error handling.
    * This is used instead of URL.openConnection() for two reasons:
    * 1. IBM JRE 1.4.2 shipped with WebSphere 6.0 does not throw any exceptions.
    * 2. The URLConnection caching must be turned off, otherwise Sun's Java
    * leaves locked files around and the JBoss and the Eclipse plugin cannot
    * delete/overwrite them.
    *
    * @param url The URL to open.
    * @return The URL connection.
    * @throws IOException if the connection cannot be opened.
    */
   public static URLConnection openConnection(URL url) throws IOException
   {
      URLConnection con = url.openConnection();

      if (con != null)
      {
         con.setUseCaches(false);
         con.setDoOutput(false);
         con.setDoInput(true);
         con.connect();

         return con;
      }

      throw new IOException("URL not found: \"" + url + "\"");
   }

   /**
    * Finds and opens an input stream on a resource relative to a class object.
    * @param clazz The class object.
    * @param sName The resource name.
    * @return The input stream.
    * @throws IOException If the resource cannot be found or opened.
    */
   public static InputStream openResource(Class clazz, String sName) throws IOException
   {
      URL url = clazz.getResource(sName);

      if (url == null)
      {
         throw new IOException("Resource \"" + sName + "\" not found");
      }

      return openStream(url);
   }

   /**
    * Return the resource last modified date for a given connection.
    * This is used instead of URLConnection.getLastModified() because the
    * default implementation of the Jar URLConnection does not read the
    * Modified property of the zip file entry.
    *
    * @param con The URL connection.
    * @return The last modified time of the resource, or -1 if not known.
    */
   public static long getLastModified(URLConnection con)
   {
      if (con instanceof JarURLConnection)
      {
         try
         {
            JarEntry entry = ((JarURLConnection)con).getJarEntry();

            if (entry != null)
            {
               return entry.getTime();
            }
         }
         catch (IOException e)
         {
            // should not reach here
            ObjUtil.rethrow(e);
         }
      }

      return con.getLastModified();
   }

   /**
    * Retrieves a list of resources at the specified relative location.
    * If the location is a file then it is returned.
    * If location is a directory then a listing for directory contents is returned.
    * @param uriCollection The collection to which to add retrieved URIs.
    * @param rootURL The URL relative to which to look for resources (not null).
    * @param sName The resource name to look at, directories delimited by '/'. Relative to rootURL.
    * @param bRecurse If true, recurses into sub-directories and returns only files; otherwise, returns
    * files and directories one level deep.
    * @throws IOException On I/O error.
    */
   public static void addResourceNames(Collection uriCollection, URL rootURL, String sName, boolean bRecurse) throws IOException
   {
      if ("bundleresource".equals(rootURL.getProtocol())) // eclipse bundle
      {
         URLConnection con = rootURL.openConnection(); // get a connection, but don't connect: a connection for a directory URL will raise an exception if connect is called.

         try
         {
            rootURL = (URL)con.getClass().getMethod("getLocalURL", null).invoke(con, null);
         }
         catch (Throwable t)
         {
            throw new IOException("Failed to get resource from \"" + con.getClass().getName() + " for URL: " + rootURL + '"');
         }
      }

      sName = (sName == null) ? "" : sName;

      URL searchURL = new URL(rootURL, sName);
      String sResourcePrefix = (sName.length() > 0) ? sName + "/" : "";

      if ("file".equals(searchURL.getProtocol())) // regular filesystem path
      {
         File path;

         try
         {
            path = new File(searchURL.getPath());
         }
         catch (Exception e)
         {
            IOException x = new IOException(e.getMessage());

            x.initCause(e);

            throw x;
         }

         if (!path.isDirectory())
         {
            uriCollection.add(sName); // the resource is a single file

            return;
         }

         String[] sNameArray = path.list();

         for (int i = 0; i < sNameArray.length; ++i)
         {
            if (path.isHidden() || s_excludePathRegexp.matcher(sNameArray[i]).matches())
            {
               continue; // skip version control directories and hidden files
            }

            String sFoundName = sResourcePrefix + sNameArray[i];

            if (bRecurse)
            {
               URLUtil.addResourceNames(uriCollection, rootURL, sFoundName, bRecurse);
            }
            else
            {
               uriCollection.add(sFoundName);
            }
         }

         return;
      }

      URLConnection con = openConnection(searchURL);

      if ("vfszip".equals(searchURL.getProtocol())) // JBoss 5 virtual file system
      {
         try
         {
            Method getContent = con.getClass().getMethod("getContent", null);
            Class virtualFileClass = getContent.getReturnType();
            Method isLeaf = virtualFileClass.getMethod("isLeaf", null);
            Object vf = getContent.invoke(con, null);

            if (((Boolean)isLeaf.invoke(vf, null)).booleanValue())
            {
               uriCollection.add(sName);

               return;
            }

            Method toURI = virtualFileClass.getMethod("toURI", null);
            Method getChildren = virtualFileClass.getMethod((bRecurse) ? "getChildrenRecursively" : "getChildren", null);
            int nURIPrefixLength = ((URI)toURI.invoke(vf, null)).toString().length();
            List children = (List)getChildren.invoke(vf, null);

            for (Iterator iter = children.iterator(); iter.hasNext(); )
            {
               vf = iter.next();

               if (!bRecurse || ((Boolean)isLeaf.invoke(vf, null)).booleanValue())
               {
                  // assume all descendants will share the URI prefix of the file
                  uriCollection.add(sResourcePrefix + ((URI)toURI.invoke(vf, null)).toString().substring(nURIPrefixLength));
               }
            }
         }
         catch (Throwable t)
         {
            throw new IOException("Failed to get resource from \"" + con.getClass().getName() + " for URL: " + rootURL + '"');
         }

         return;
      }

      String sPrefix = null;
      Iterator resourceNameIterator = null;
      JarFile jar;

      if (con instanceof JarURLConnection) // JAR filesystem path (ANT or jBoss)
      {
         JarURLConnection jarCon = (JarURLConnection)con;

         jar = jarCon.getJarFile();
         sPrefix = jarCon.getJarEntry().toString();
         resourceNameIterator = new ZipEntryNameIterator(jar.entries());
      }
      else if ("wsjar".equals(searchURL.getProtocol())) // WebSphere (does not use JarURLConnection)
      {
         sPrefix = searchURL.getPath();

         try
         {
            jar = new JarFile(new URL(searchURL, sPrefix.substring(0, sPrefix.indexOf('!'))).getPath());
         }
         catch (Exception e)
         {
            IOException x = new IOException("Unsupported URL path \"" + searchURL + '"');

            x.initCause(e);

            throw x;
         }

         sPrefix = sPrefix.substring(sPrefix.indexOf('!') + 2); // 2 == "!/"
         resourceNameIterator = new ZipEntryNameIterator(jar.entries());
      }
      else if (con instanceof ResourceCollection)
      {
         ResourceCollection jarCon = (ResourceCollection)con;

         sPrefix = jarCon.getURLPrefix();
         resourceNameIterator = jarCon.getResourceNameIterator();
      }

      if (resourceNameIterator != null) // JAR access
      {
         if (sPrefix.endsWith("/"))
         {
            sPrefix = sPrefix.substring(0, sPrefix.length() - 1);
         }

         while (resourceNameIterator.hasNext())
         {
            String sResource = (String)resourceNameIterator.next();
            int n;

            if (sResource.startsWith(sPrefix) &&
                (sResource.length() == sPrefix.length() || // exact path
                 (sResource.charAt(sPrefix.length()) == '/' && // a directory
                  sResource.length() > sPrefix.length() + 1 && // has a path inside the directory
                  ((n = sResource.indexOf('/', sPrefix.length() + 1)) < 0 || // the path is a file, direct child
                   (bRecurse && sResource.charAt(sResource.length() - 1) != '/') || // the is a file, accessible recursively
                    (!bRecurse && n == sResource.length() - 1))))) // the path is a direct subdirectory, and the search is not recursive
            {
               int nRelativePathOffset = sPrefix.length() - sName.length() + ((sName.length() > 0 && sName.charAt(sName.length() - 1) != '/') ? 0 : 1);

               uriCollection.add(sResource.substring(nRelativePathOffset));
            }
         }

         return;
      }

      throw new IOException("Unsupported URLConnection type \"" + con.getClass().getName() + " for URL: " + searchURL + '"');
   }

   /**
    * Retrieves a list of resources at the specified relative location.
    * If the location is a file then only its URL is returned.
    * If location is a directory then URL listing for directory contents is returned.
    * @param uriCollection The collection to which to add retrieved URIs.
    * @param clazz The class object relative to which to look for resources (not null).
    * @param sName The resource name, directories delimited by '/' (not null). Relative to clazz.
    * @throws IOException On I/O error.
    */
   public static void addResourceNames(Collection uriCollection, Class clazz, String sName) throws IOException
   {
      String sPackage = clazz.getPackage().getName().replace('.', '/') + '/' + sName;
      Enumeration/*<URL>*/ packages = clazz.getClassLoader().getResources(sPackage);

      if (!packages.hasMoreElements())
      {
         return;
      }

      List uriList = new ArrayList();

      do
      {
         URLUtil.addResourceNames(uriList, (URL)packages.nextElement(), null, true);
      }
      while (packages.hasMoreElements());

      for (int i = 0; i < uriList.size(); i++)
      {
         String sResource = (String)uriList.get(i);

         uriCollection.add(sName + sResource);
      }
   }

   // inner classes

   /**
    * Implemented by custom URLConnection implementations to allow them to work with addResourceNames.
    */
   public interface ResourceCollection
   {
      /**
       * Gets the path of the resources that are being returned.
       * @return The URL prefix.
       */
      public String getURLPrefix() throws IOException;

      /**
       * Gets an iterator over the names of the archive entries.
       * @return An iterator of strings.
       */
      public Iterator getResourceNameIterator() throws IOException;
   }

   /**
    * Adapts an Enumeration of ZipEntry to an Iterator of String, where
    * the strings are the result of calling getName() on each ZipEntry.
    */
   protected static class ZipEntryNameIterator implements Iterator
   {
      /**
       * The Enumeration of ZipEntry.
       */
      protected Enumeration m_enumeration;

      // constructors

      /**
       * @param enumeration An Enumeration of ZipEntry.
       */
      public ZipEntryNameIterator(Enumeration enumeration)
      {
         m_enumeration = enumeration;
      }

      // operations

      /**
       * @see java.util.Iterator#hasNext()
       */
      public boolean hasNext()
      {
         return m_enumeration.hasMoreElements();
      }

      /**
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         return ((ZipEntry)m_enumeration.nextElement()).getName();
      }

      /**
       * @see java.util.Iterator#remove()
       */
      public void remove()
      {
         throw new UnsupportedOperationException();
      }
   }
}