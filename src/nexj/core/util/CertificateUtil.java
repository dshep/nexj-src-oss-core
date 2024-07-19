// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.cert.Certificate;
import java.security.cert.CertificateFactory;

import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;

/**
 * PKI certificate utility routines.
 */
public class CertificateUtil
{
   // associations

   /**
    * A cache of trust managers for each trusted certificate: (TrustManager[])[Certificate]
    */
   protected static Lookup s_trustedCertificateMap = new SoftHashTab();

   /**
    * A cache of key managers for each client certificate: (KeyManager[])[KeyStore]
    */
   protected static Lookup s_clientCertificateMap = new SoftHashTab();

   // operations

   /**
    * Parses a string containing a certificate.
    * @param sCertificate An X.509 certificate as a base64-encoded string.
    * @return The parsed certificate.
    */
   public static Certificate parseCertificate(String sCertificate)
   {
      if (sCertificate == null)
      {
         return null;
      }

      try
      {
         CertificateFactory factory = CertificateFactory.getInstance("X.509");
         ByteArrayInputStream bis = new ByteArrayInputStream(sCertificate.getBytes("UTF-8"));

         return factory.generateCertificate(bis);
      }
      catch (Exception ex)
      {
         throw new IllegalArgumentException("Invalid certificate", ex);
      }
   }

   /**
    * The inverse of parseCertificate(String).
    * @param certificate The certificate object to format.
    * @return A String representing the certificate, formatted in base64-encoding with
    * the standard line breaks and wrapped with -----BEGIN CERTIFICATE----- and
    * -----END CERTIFICATE-----.
    */
   public static String formatCertificate(Certificate certificate)
   {
      if (certificate == null)
      {
         return null;
      }

      try
      {
         ByteArrayInputStream bis = new ByteArrayInputStream(certificate.getEncoded());
         StringWriter writer = new StringWriter();

         writer.write("-----BEGIN CERTIFICATE-----\r\n");
         Base64Util.encode(bis, writer, -1, true);
         writer.write("\r\n-----END CERTIFICATE-----");

         return writer.toString();
      }
      catch (Exception ex)
      {
         throw new IllegalArgumentException("Invalid certificate", ex);
      }
   }

   /**
    * Parses a string containing a key store.
    * @param sKeyStore The PKCS #12 key store as a base64-encoded string.
    * @param sPassword The password for the key store.
    * @return The parsed key store.
    */
   public static KeyStore parseKeyStore(String sKeyStore, String sPassword)
   {
      if (sKeyStore == null)
      {
         return null;
      }

      try
      {
         ByteArrayOutputStream os = new ByteArrayOutputStream(((sKeyStore.length() + 3) >> 2) * 3);

         Base64Util.decode(new StringReader(sKeyStore), os, -1, true);

         ByteArrayInputStream bis = new ByteArrayInputStream(os.toByteArray());
         KeyStore ks = KeyStore.getInstance("pkcs12");

         ks.load(bis, (sPassword == null) ? null : sPassword.toCharArray());

         return ks;
      }
      catch (Exception ex)
      {
         throw new IllegalArgumentException("Invalid KeyStore", ex);
      }
   }

   /**
    * Formats a key store into a base64-encoded String.
    * @param keyStore The PKCS #12 key store to format.
    * @param sPassword The password for the key store.
    * @return The key store as a base64-encoded String.
    */
   public static String formatKeyStore(KeyStore keyStore, String sPassword)
   {
      if (keyStore instanceof EncryptedKeyStore)
      {
         return ((EncryptedKeyStore)keyStore).getText();
      }

      if (keyStore == null || sPassword == null)
      {
         return null;
      }

      try
      {
         ByteArrayOutputStream bos = new ByteArrayOutputStream();

         keyStore.store(bos, sPassword.toCharArray());

         ByteArrayInputStream bis = new ByteArrayInputStream(bos.toByteArray());
         StringWriter writer = new StringWriter();

         Base64Util.encode(bis, writer, -1, true);

         return writer.toString();
      }
      catch (Exception ex)
      {
         throw new IllegalArgumentException("Invalid key store", ex);
      }
   }

   /**
    * Gets the key managers to use when establishing an SSL connection.
    * @param certificate The key store for which to get the key managers array.
    * @param achPassword The key store's password.
    * @return The array of key managers.
    */
   public static KeyManager[] getKeyManagers(KeyStore certificate, char[] achPassword)
   {
      if (certificate == null)
      {
         return null;
      }

      KeyManager[] keyManagers;

      synchronized (s_clientCertificateMap)
      {
         keyManagers = (KeyManager[])s_clientCertificateMap.get(certificate);
      }

      if (keyManagers == null)
      {
         try
         {
            KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());

            kmf.init(certificate, achPassword);
            keyManagers = kmf.getKeyManagers();

            synchronized (s_clientCertificateMap)
            {
               KeyManager[] old = (KeyManager[])s_clientCertificateMap.put(certificate, keyManagers);

               if (old != null)
               {
                  s_clientCertificateMap.put(certificate, old);
                  keyManagers = old;
               }
            }
         }
         catch (GeneralSecurityException ex)
         {
            throw ObjUtil.rethrow(ex);
         }
      }

      return keyManagers;
   }

   /**
    * Gets the trust managers to use when establishing an SSL connection that uses client
    * certificate authentication.
    * @param trust The trusted certificate for which to get the trust managers array.
    * @return The array of trust managers.
    */
   public static TrustManager[] getTrustManagers(Certificate trust)
   {
      if (trust == null)
      {
         return null;
      }

      TrustManager[] trustManagers;

      synchronized (s_trustedCertificateMap)
      {
         trustManagers = (TrustManager[])s_trustedCertificateMap.get(trust);
      }

      if (trustManagers == null)
      {
         try
         {
            KeyStore trustStore = KeyStore.getInstance(KeyStore.getDefaultType());

            trustStore.load(null, null);
            trustStore.setCertificateEntry("default", trust);

            TrustManagerFactory tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());

            tmf.init(trustStore);
            trustManagers = tmf.getTrustManagers();

            synchronized (s_trustedCertificateMap)
            {
               TrustManager[] old = (TrustManager[])s_trustedCertificateMap.put(trust, trustManagers);

               if (old != null)
               {
                  s_trustedCertificateMap.put(trust, old);
                  trustManagers = old;
               }
            }
         }
         catch (IOException ex)
         {
            throw ObjUtil.rethrow(ex);
         }
         catch (GeneralSecurityException ex)
         {
            throw ObjUtil.rethrow(ex);
         }
      }

      return trustManagers;
   }
}
