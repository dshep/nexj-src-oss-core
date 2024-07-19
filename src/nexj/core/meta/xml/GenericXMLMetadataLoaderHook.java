package nexj.core.meta.xml;

import java.util.Properties;
import java.util.Set;

import org.w3c.dom.Element;


/**
 * Generic external metadata loading hook.
 */
public class GenericXMLMetadataLoaderHook implements XMLMetadataLoaderHook
{
   /**
    * @see nexj.core.meta.xml.XMLMetadataLoaderHook#initialize(nexj.core.meta.xml.XMLMetadataLoader, java.util.Properties)
    */
   public void initialize(XMLMetadataLoader loader, Properties properties)
   {
   }

   /**
    * @see nexj.core.meta.xml.XMLMetadataLoaderHook#loadChannel(org.w3c.dom.Element, java.lang.String)
    */
   public boolean loadChannel(Element channelElement, String sName)
   {
      return false;
   }

   /**
    * @see nexj.core.meta.xml.XMLMetadataLoaderHook#loadConnection(org.w3c.dom.Element, java.lang.String, java.lang.String, java.util.Set)
    */
   public boolean loadConnection(Element connectionElement, String sConnectionsName, String sChannelName,
      Set connectionSet)
   {
      return false;
   }
}
