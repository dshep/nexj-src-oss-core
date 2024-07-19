package nexj.core.util.auth;

import java.security.cert.Certificate;

/**
 * Maps a certificate array to a principal.
 */
public interface CertificatePrincipalMapper
{

   /**
    * Return a principal based on the given certificate chain.
    * @param certificateArray The certificate chain.
    * @return The corresponding principal. Can be null.
    */
   public String getPrincipal(Certificate[] certificateArray);
}
