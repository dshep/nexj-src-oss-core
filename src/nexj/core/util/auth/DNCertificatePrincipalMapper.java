// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.auth;

import java.security.Principal;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import nexj.core.util.Logger;

/**
 * Obtains the principal from a specified field of the certificate's subject principal.
 */
public class DNCertificatePrincipalMapper implements CertificatePrincipalMapper
{
   // attributes

   /**
    * The field in a Certificate's distinguished name from which the principal
    * will be extracted. Defaults to "CN". Use "DN" to take the entire certificate
    * distinguished name.
    */
   protected String m_sField = "CN";

   // associations

   /**
    * The pattern to be used to extract a principal from a certificate's distinguished name.
    */
   protected Pattern m_pattern = getPattern();

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(DNCertificatePrincipalMapper.class);

   // operations

   /**
    * @see nexj.core.util.auth.CertificatePrincipalMapper#getPrincipal(java.security.cert.Certificate[])
    */
   public String getPrincipal(Certificate[] certificateArray)
   {
      if (certificateArray == null || certificateArray.length < 1 || certificateArray[0] == null)
      {
         return null;
      }

      Principal principal = ((X509Certificate)certificateArray[0]).getSubjectX500Principal();
      String sDN = principal.getName();
      Matcher matcher = m_pattern.matcher(sDN);

      if (matcher.find())
      {
         String sPrincipal = matcher.group(2);

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Successfully mapped certificate DN \"" + sDN
               + "\" to principal \"" + sPrincipal + "\"");
         }

         return sPrincipal;
      }

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Failed to map certificate DN \"" + sDN + "\" to a principal");
      }

      return null;
   }

   /**
    * @param sField The certificate's user field.
    */
   public void setField(String sField)
   {
      m_sField = sField;
      m_pattern = getPattern();
   }

   /**
    * @return The certificate's user field.
    */
   public String getField()
   {
      return m_sField;
   }

   /**
    * Get the pattern to be used to extract a principal from a certificate's distinguished name.
    * @return The pattern.
    */
   protected Pattern getPattern()
   {
      if (m_sField.equalsIgnoreCase("DN"))
      {
         // match the entire string
         return Pattern.compile("(^)(.*)$");
      }

      // match specified field name
      return Pattern.compile("(^|,)\\s*" + Pattern.quote(m_sField) + "=([^,]*)", Pattern.CASE_INSENSITIVE);
   }
}
