// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * Utilities for manipulating GUIDs.
 */
public class GUIDUtil
{
   // constants

   /**
    * GUID consisting entirely of zeros.
    */
   public final static Binary NULL_GUID = new Binary(new byte[16]);

   // constructors
   
   /**
    * Prevents construction.
    */
   protected GUIDUtil()
   {
   }
   
   // operations
   
   /**
    * Generates a new random (version 4) GUID, using the format described in
    * http://www.opengroup.org/dce/info/draft-leach-uuids-guids-01.txt
    * @return A new generated GUID.
    */
   public static Binary generateGUID()
   {
      byte[] guid = new byte[16];

      RandUtil.getSecureRandom().nextBytes(guid);

      // Version 4
      guid[6] &= 0x0f;
      guid[6] |= 0x40;

      // Variant specified in the above document
      guid[8] &= 0x3f;
      guid[8] |= 0x80;

      return new Binary(guid);
   }
   
   /**
    * Generate a name-based (version 3) GUID from specified data.
    * @param namespace The namespace for the GUID. Can be null.
    * @param data The data for generating the GUID.
    * @return A new GUID generated from the data.
    * It will always be the same for the same namespace and data.
    */
   public static Binary generateGUID(byte[] namespace, byte[] data)
   {
       MessageDigest md;

       try
       {
          md = MessageDigest.getInstance("MD5");
       }
       catch (NoSuchAlgorithmException e)
       {
          ObjUtil.rethrow(e);
          return null;
       }

       if (namespace != null)
       {
          md.update(namespace);
       }

       byte[] guid = md.digest(data);

       // Version 3
       guid[6] &= 0x0f;
       guid[6] |= 0x30;

       // Variant specified in the above document
       guid[8] &= 0x3f;
       guid[8] |= 0x80;

       return new Binary(guid);
   }
}
