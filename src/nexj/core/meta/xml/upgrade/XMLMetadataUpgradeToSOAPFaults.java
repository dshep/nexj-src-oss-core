package nexj.core.meta.xml.upgrade;

import java.io.UnsupportedEncodingException;

import nexj.core.util.Lookup;

/**
 * The XML parser now uses message metadata to parse SOAP faults. These messages are required to be present.
 * These metadata have been added to the core, but will not be available when running earlier versions of metadata
 * using a later framework.
 */
public class XMLMetadataUpgradeToSOAPFaults
{
   // constants

   /**
    * This upgrade applicable to all Metadata prior to this version.
    */
   public final static String VERSION = "7.0.0.45";

   // operations

   public void addResources(Lookup nameContentMap) throws UnsupportedEncodingException
   {
      Class clazz = XMLMetadataUpgradeToSOAPFaults.class;

      nameContentMap.put("messages/SOAP11Fault.message", clazz.getResource("soapfaults/SOAP11Fault.message"));
      nameContentMap.put("messages/SOAP12Fault.message", clazz.getResource("soapfaults/SOAP12Fault.message"));
      nameContentMap.put("messages/SOAP12Fault_Subcode.message", clazz.getResource("soapfaults/SOAP12Fault_Subcode.message"));
   }
}
