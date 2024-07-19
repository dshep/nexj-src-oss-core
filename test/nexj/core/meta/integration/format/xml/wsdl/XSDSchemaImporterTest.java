package nexj.core.meta.integration.format.xml.wsdl;

import java.net.URL;

import nexj.core.meta.integration.format.xml.schema.CompositeType;
import nexj.core.meta.integration.format.xml.schema.Element;
import nexj.core.meta.integration.format.xml.schema.PrimitiveType;
import nexj.core.meta.integration.format.xml.schema.Schema;
import nexj.core.meta.integration.format.xml.schema.SchemaItem;
import nexj.core.meta.integration.format.xml.schema.SchemaUniverse;

import junit.framework.TestCase;

/**
 * JUnit test for XSDSchemaImporter
 */
public class XSDSchemaImporterTest extends TestCase
{
   public void testImport() throws Exception
   {
      URL url = XSDSchemaImporterTest.class.getResource("ImportService/electronic.xsd");

      SchemaUniverse universe = XSDSchemaImporter.parse(url);
      Schema schema = universe.findSchemaByURI("http://nexj.example.com/electronic/");

      assertNotNull(schema);

      Element battery = (Element)schema.findItem("battery", SchemaItem.ELEMENT);

      assertNotNull(battery);
      assertEquals(PrimitiveType.STRING, battery.getType());

      Element computer = (Element)schema.findItem("computer", SchemaItem.ELEMENT);

      assertNotNull(computer);
      assertNotNull(computer.getType());
      assertEquals(SchemaItem.COMPOSITE_TYPE, computer.getType().getItemType());

      CompositeType computerType = (CompositeType)computer.getType();

      assertEquals(2, computerType.getChildCount());

      Element gpu = (Element)computerType.getChild(0);

      assertNotNull(gpu.getType());
      assertEquals(SchemaItem.COMPOSITE_TYPE, gpu.getType().getItemType());

      CompositeType gpuType = (CompositeType)gpu.getType();

      assertEquals(1, gpuType.getChildCount());
   }
}