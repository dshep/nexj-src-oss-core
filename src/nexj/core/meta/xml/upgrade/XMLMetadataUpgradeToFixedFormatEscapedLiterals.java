package nexj.core.meta.xml.upgrade;

import java.util.Iterator;

import org.w3c.dom.Element;

import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.util.StringUtil;
import nexj.core.util.XMLUtil;

/**
 * Change fixed format prefix and suffix to be literal strings, i.e. escape the escape character \.
 */
public class XMLMetadataUpgradeToFixedFormatEscapedLiterals
{
   // constants

   /**
    * This upgrade applicable to all Metadata prior to this version.
    */
   public final static String VERSION = "7.1.123.0";

   // operations

   /**
    * Update fixed format messages.
    */
   public void upgradeMessage(Element element, String sName, XMLMetadataHelper helper)
   {
      String sFormat = element.getAttribute("format");

      if (sFormat != null && sFormat.equals("Fixed"))
      {
         sFormat = null;

         for (Iterator itr = XMLUtil.findElementRecur(element, "FixedMapping").iterator(); itr.hasNext();)
         {
            Element fixedMapping = (Element)itr.next();
            String sPrefix = fixedMapping.getAttribute("prefix");
            String sSuffix = fixedMapping.getAttribute("suffix");

            if (!StringUtil.isEmpty(sPrefix))
            {
               fixedMapping.setAttribute("prefix", sPrefix.replace("\\", "\\\\"));
            }

            if (!StringUtil.isEmpty(sSuffix))
            {
               fixedMapping.setAttribute("suffix", sSuffix.replace("\\", "\\\\"));
            }
         }
      }
   }
}
