// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.Properties;

/**
 * Property-file related utility functions.
 */
public class PropertyUtil
{
   // constructors 
   
   /**
    * Prevents instantiation.
    */
   protected PropertyUtil()
   {
   }
   
   // operations
   
   /**
    * Parses a propery map out of a string.
    * @param sProperties The string containing the properties.
    * @return The property map.
    * @throws IOException if an IO error occurs.
    * @throws IllegalArgumentException if the format is incorrect.
    */
   public static Properties fromString(String sProperties) throws IOException, IllegalArgumentException
   {
      Properties properties = new Properties();

      if (sProperties != null)
      {
         Charset charset = Charset.forName(IOUtil.ENCODING);
         ByteBuffer byteBuffer = charset.encode(sProperties);
         ByteArrayInputStream is = new ByteArrayInputStream(byteBuffer.array(), 0, byteBuffer.limit());
   
         properties.load(is);
      }

      return properties;
   }
   
   /**
    * Serializes a property map to string.
    * @param properties The property map.
    * @return The string representation of the property map.
    */
   public static String toString(Properties properties)
   {
      ByteArrayOutputStream os = new ByteArrayOutputStream();

      try
      {
         properties.store(os, null);

         byte[] a = os.toByteArray();
         int i = 0;

         if (i < a.length)
         {
            if (a[i] == '#')
            {
               while (i < a.length && (a[i] & 0xFF) >= ' ')
               {
                  ++i;
               }

               while (i < a.length && (a[i] & 0xFF) < ' ')
               {
                  ++i;
               }
            }
         }

         return new String(a, i, a.length - i, "ISO8859_1");
      }
      catch (IOException e)
      {
         ObjUtil.rethrow(e);

         return null;
      }
   }
}
