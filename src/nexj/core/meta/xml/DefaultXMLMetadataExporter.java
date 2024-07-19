package nexj.core.meta.xml;

import java.io.Writer;

/**
 * Selects the XMLMetadataExporter as the default metadata exporter.
 */
public class DefaultXMLMetadataExporter extends XMLMetadataExporter
{
   /**
    * @param writer
    */
   public DefaultXMLMetadataExporter(Writer writer)
   {
      super(writer);
   }
}
