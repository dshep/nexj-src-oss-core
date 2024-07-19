// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;

/**
 * Caches data for a binary content.
 */
public class ContentCache
{
   /**
    * Data cache.
    */
   protected Lookup m_contentMap = new SoftHashTab();

   /**
    * Retrieve a cached content for a given URL.
    * 
    * @param url The URL.
    * @param The cached content.
    * @throws IOException if an error occurs.
    */
   public Content getContent(URL url) throws IOException
   {
      String sURI = url.toExternalForm();
      Content content;

      synchronized (m_contentMap)
      {
         content = (Content)m_contentMap.get(sURI);
      }

      if (content == null)
      {
         URLConnection con = URLUtil.openConnection(url);
         String sMIMEType = con.getContentType();
         InputStream in = null;

         try
         {
            in = URLUtil.openStream(con);

            ByteArrayOutputStream out = new ByteArrayOutputStream();

            IOUtil.copy(out, in, null);

            content =  new Content(new Binary(out.toByteArray()), sMIMEType);

            synchronized (m_contentMap)
            {
               Content previous = (Content)m_contentMap.put(sURI, content);

               if (previous != null)
               {
                  m_contentMap.put(sURI, previous);
                  content = previous;
               }
            }
         }
         finally
         {
            IOUtil.close(in);
         }
      }

      return content;
   }

   /**
    * The content structure
    */
   public static class Content
   {
      /**
       * The binary data.
       */
      protected Binary m_body;

      /**
       * The content type.
       */
      protected String m_sMIMEType;

      /**
       * Constructor.
       * 
       * @param body The content binary data.
       * @param sMIMEType The content type.
       */
      public Content(Binary body, String sMIMEType)
      {
         m_body = body;
         m_sMIMEType = sMIMEType;
      }

      /**
       * @return The content binary data.
       */
      public Binary getBody()
      {
         return m_body;
      }

      /**
       * @return The content type.
       */
      public String getMIMEType()
      {
         return m_sMIMEType;
      }
   }
}
