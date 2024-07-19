// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml;

import java.io.StringWriter;

import junit.framework.TestCase;

import nexj.core.integration.MessageFormatter;
import nexj.core.integration.io.WriterOutput;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataLoaderDispatcher;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.format.xml.schema.MessageSchemaConverter;
import nexj.core.meta.integration.format.xml.schema.Schema;
import nexj.core.meta.integration.format.xml.schema.SchemaUniverse;
import nexj.core.meta.integration.format.xml.schema.XSDSchemaExporter;
import nexj.core.meta.integration.service.Interface;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.SysUtil;

/**
 * Tests the XSDMessageExporter.
 */
public class XSDMessageExporterTest extends TestCase
{
   // constants

   protected final static String NILLABLE_MESSAGE_SCHEMA =
      "<complexType name=\"XML_NillableSubMessage\">" +
         "<sequence>" +
            "<element name=\"a\" minOccurs=\"0\" type=\"xsd:string\"/>" +
            "<element name=\"b\" minOccurs=\"0\" nillable=\"true\" type=\"xsd:string\"/>" +
            "<element name=\"c\" minOccurs=\"0\" nillable=\"true\">" +
               "<complexType>" +
                  "<sequence>" +
                     "<element name=\"x\" type=\"xsd:string\"/>" +
                     "<element name=\"y\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                  "</sequence>" +
                  "<attribute name=\"z\" type=\"xsd:string\"/>" +
               "</complexType>" +
            "</element>" +
         "</sequence>" +
      "</complexType>";

   protected final static String SUBMIT_OBJECTS_SCHEMA_1 = // urn:oasis:names:tc:ebxml-regrep:xsd:rs:3.0
      "<complexType name=\"XDS_SubmitObjects\">" +
         "<annotation>" +
            "<documentation>" +
               "The SubmitObjectsRequest allows one to submit a list of RegistryObject elements." +
            "</documentation>" +
         "</annotation>" +
         "<sequence>" +
            "<element minOccurs=\"0\" ref=\"rim:LeafRegistryObjectList\"/>" +
         "</sequence>" +
      "</complexType>" +
      "<element name=\"SubmitObjectsRequest\" type=\"rs:XDS_SubmitObjects\"/>";

   protected final static String SUBMIT_OBJECTS_SCHEMA_2 = // urn:oasis:names:tc:ebxml-regrep:xsd:rim:3.0
      "<element name=\"LeafRegistryObjectList\">" +
         "<complexType>" +
            "<choice maxOccurs=\"unbounded\">" +
               "<element name=\"Classification\" minOccurs=\"0\" form=\"qualified\">" +
                  "<complexType>" +
                  "</complexType>" +
               "</element>" +
               "<element name=\"ExtrinsicObject\" minOccurs=\"0\" form=\"qualified\">" +
                  "<complexType>" +
                     "<sequence>" +
                        "<element name=\"Name\" minOccurs=\"0\" form=\"qualified\">" +
                           "<complexType>" +
                              "<sequence>" +
                                 "<element name=\"LocalizedString\" minOccurs=\"0\" form=\"qualified\">" +
                                    "<complexType>" +
                                       "<attribute name=\"value\" type=\"xsd:string\"/>" +
                                    "</complexType>" +
                                 "</element>" +
                              "</sequence>" +
                           "</complexType>" +
                        "</element>" +
                        "<element name=\"Slot\" minOccurs=\"0\" maxOccurs=\"unbounded\" form=\"qualified\">" +
                           "<complexType>" +
                              "<sequence>" +
                                 "<element name=\"ValueList\" minOccurs=\"0\" form=\"qualified\">" +
                                    "<complexType>" +
                                       "<sequence>" +
                                          "<element name=\"Value\" minOccurs=\"0\" form=\"qualified\" type=\"xsd:string\"/>" +
                                       "</sequence>" +
                                    "</complexType>" +
                                 "</element>" +
                              "</sequence>" +
                              "<attribute name=\"name\" type=\"xsd:string\"/>" +
                           "</complexType>" +
                        "</element>" +
                        "<element name=\"Classification\" minOccurs=\"0\" form=\"qualified\">" +
                           "<complexType>" +
                              "<sequence>" +
                                 "<element name=\"Name\" minOccurs=\"0\" form=\"qualified\">" +
                                    "<complexType>" +
                                       "<sequence>" +
                                          "<element name=\"LocalizedString\" minOccurs=\"0\" form=\"qualified\">" +
                                             "<complexType>" +
                                                "<attribute name=\"value\" type=\"xsd:string\"/>" +
                                             "</complexType>" +
                                          "</element>" +
                                       "</sequence>" +
                                    "</complexType>" +
                                 "</element>" +
                              "</sequence>" +
                              "<attribute name=\"classificationScheme\" type=\"xsd:string\"/>" +
                           "</complexType>" +
                        "</element>" +
                        "<element name=\"ExternalIdentifier\" minOccurs=\"0\" maxOccurs=\"unbounded\" form=\"qualified\">" +
                           "<complexType>" +
                              "<sequence>" +
                                 "<element name=\"Name\" minOccurs=\"0\" form=\"qualified\">" +
                                    "<complexType>" +
                                       "<sequence>" +
                                          "<element name=\"LocalizedString\" minOccurs=\"0\" form=\"qualified\">" +
                                             "<complexType>" +
                                                "<attribute name=\"value\" type=\"xsd:string\"/>" +
                                             "</complexType>" +
                                          "</element>" +
                                       "</sequence>" +
                                    "</complexType>" +
                                 "</element>" +
                              "</sequence>" +
                              "<attribute name=\"identificationScheme\" type=\"xsd:string\"/>" +
                              "<attribute name=\"value\" type=\"xsd:string\"/>" +
                           "</complexType>" +
                        "</element>" +
                     "</sequence>" +
                     "<attribute name=\"id\" type=\"xsd:string\"/>" +
                     "<attribute name=\"mimeType\" type=\"xsd:string\"/>" +
                  "</complexType>" +
               "</element>" +
            "</choice>" +
         "</complexType>" +
      "</element>";


   protected final static String COMPREHENSIVE_MESSAGE_SCHEMA = 
      "<complexType name=\"XML_XSDExportComprehensive\" mixed=\"true\">" +
         "<annotation>" +
            "<documentation>" +
               "A message containing almost all XML message features to provide good coverage for XSD export." +
            "</documentation>" +
         "</annotation>" +
         "<sequence>" +
            "<element name=\"self\" minOccurs=\"0\" type=\"test:XML_XSDExportComprehensive\">" +
               "<annotation>" +
                  "<documentation>" +
                     "Self-reference." +
                  "</documentation>" +
               "</annotation>" +
            "</element>" +
            "<element name=\"a\" form=\"qualified\" type=\"xsd:string\">" +
               "<annotation>" +
                  "<documentation>" +
                     "The o1 string." +
                  "</documentation>" +
               "</annotation>" +
            "</element>" +
            "<element name=\"b\" minOccurs=\"0\" nillable=\"true\" form=\"qualified\" type=\"xsd:long\"/>" +
            "<element name=\"c\" minOccurs=\"0\" maxOccurs=\"unbounded\" form=\"qualified\" type=\"xsd:int\"/>" +

            // Sequential aggregation
            "<element name=\"d\" maxOccurs=\"unbounded\" form=\"qualified\">" +
               "<complexType>" +
                  "<sequence>" +
                     "<element name=\"d1\" minOccurs=\"0\" form=\"qualified\" type=\"xsd:boolean\"/>" +
                     "<element name=\"d2\" minOccurs=\"0\" type=\"xsd:decimal\"/>" +
                     "<element name=\"d3\" minOccurs=\"0\" type=\"xsd:float\"/>" +
                  "</sequence>" +
               "</complexType>" +
            "</element>" +

            // Single aggregation
            "<element name=\"e\" minOccurs=\"0\" maxOccurs=\"unbounded\">" +
               "<complexType>" +
                  "<choice>" +
                     "<element name=\"e1\" minOccurs=\"0\" form=\"qualified\" type=\"xsd:double\"/>" +
                     "<element name=\"e2\" minOccurs=\"0\" type=\"xsd:dateTime\"/>" +
                     "<element name=\"e3\" minOccurs=\"0\" type=\"xsd:base64Binary\"/>" +
                  "</choice>" +
               "</complexType>" +
            "</element>" +

            // Random aggregation --- PARTIAL SUPPORT
            "<element name=\"f\">" +
               "<complexType>" +
                  "<choice maxOccurs=\"unbounded\">" +
                     "<element name=\"f1\" minOccurs=\"0\" type=\"xsd:base64Binary\"/>" +
                     "<element name=\"f2\" minOccurs=\"0\" type=\"xsd:hexBinary\"/>" +
                     "<element name=\"f3\" minOccurs=\"0\" type=\"xsd:dateTime\"/>" +
                  "</choice>" +
               "</complexType>" +
            "</element>" +

            // Attributes Alone
            "<element name=\"g\" maxOccurs=\"unbounded\">" +
               "<annotation>" +
                  "<documentation>" +
                  "The g anonymous type." +
                  "</documentation>" +
               "</annotation>" +
               "<complexType>" +
                  "<attribute name=\"g1\" type=\"xsd:date\">" +
                     "<annotation>" +
                        "<documentation>" +
                           "The g1 attribute." +
                        "</documentation>" +
                     "</annotation>" +
                  "</attribute>" +
                  "<attribute name=\"g2\" use=\"required\" type=\"xsd:time\"/>" +
               "</complexType>" +
            "</element>" +

            // Enumerations
            "<element name=\"h\" minOccurs=\"0\">" +
               "<simpleType>" +
                  "<restriction base=\"xsd:string\">" +
                     "<enumeration value=\"five\"/>" +
                     "<enumeration value=\"four\"/>" +
                     "<enumeration value=\"one\"/>" +
                     "<enumeration value=\"three\"/>" +
                     "<enumeration value=\"two\"/>" +
                  "</restriction>" +
               "</simpleType>" +
            "</element>" +
            "<element name=\"i\" minOccurs=\"0\" form=\"qualified\">" +
               "<simpleType>" +
                  "<restriction base=\"xsd:int\">" +
                     "<enumeration value=\"2\"/>" +
                     "<enumeration value=\"3\"/>" +
                     "<enumeration value=\"5\"/>" +
                     "<enumeration value=\"7\"/>" +
                     "<enumeration value=\"11\"/>" +
                     "<enumeration value=\"13\"/>" +
                     "<enumeration value=\"17\"/>" +
                  "</restriction>" +
               "</simpleType>" +
               "</element>" +

               // xsd:anyType (no interface specified on mapping)
               "<element name=\"k\" minOccurs=\"0\"/>" +
               "<element name=\"m\" minOccurs=\"0\">" +
                  "<complexType mixed=\"true\">" +
                     "<sequence>" +
                        "<any processContents=\"lax\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>" +
                     "</sequence>" +
                     "<attribute name=\"n\" type=\"xsd:string\"/>" +
                  "</complexType>" +
               "</element>" +

               // xsd:anyType (interface "AnyTypeTest" specified on mapping)
               "<element name=\"p\" minOccurs=\"0\">" +
                  "<complexType>" +
                     "<choice>" +
                        "<element ref=\"test:top\"/>" +
                        "<element ref=\"rs:SubmitObjectsRequest\"/>" +
                     "</choice>" +
                  "</complexType>" +
               "</element>" +
         "</sequence>" +
      "</complexType>";

   protected final static String SOAP_HEADERS_PATIENT_MESSAGE_SCHEMA_2 =
      "<attribute name=\"id\" type=\"xsd:string\"/>";

   protected final static String SOAP_HEADERS_RESERVATION_SCHEMA =
      "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\" targetNamespace=\"http://travelcompany.example.org/reservation\">" +
      "<element name=\"reservation\"><complexType><sequence><element name=\"reference\" minOccurs=\"0\" form=\"qualified\" " +
      "type=\"xsd:string\"/></sequence></complexType></element></schema>";

   protected final static String SOAP_HEADERS_WSCOOR_SCHEMA =
      "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\" targetNamespace=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\">" + 
      "<complexType name=\"SOAP_WsCoor_Header\"><sequence><element name=\"Expires\" minOccurs=\"0\" type=\"xsd:long\"/>" +
      "</sequence></complexType><element name=\"CoordinationContext\" type=\"wscoor:SOAP_WsCoor_Header\"/></schema>";

   protected final static String SOAP_HEADERS_PATIENT_MESSAGE_PART =
      "<part name=\"reservation\" element=\"m:reservation\"/>" +
      "<part name=\"context\" element=\"wscoor:CoordinationContext\"/>";

   protected final static String SOAP_HEADERS_PATIENT_MESSAGE_BIND =
      "<soapbind:header message=\"tns:SOAP_Headers_PatientDemographics\" part=\"reservation\" use=\"literal\"/>" +
      "<soapbind:header message=\"tns:SOAP_Headers_PatientDemographics\" part=\"context\" use=\"literal\"/>";

   protected final static String SOAP_HEADERS_PATIENT_MESSAGE_SCHEMA =
      "<complexType name=\"SOAP_Headers_PatientDemographics\">" +
         "<complexContent>" +
            "<extension base=\"a:SOAP_PatientDemographics\">" +
            "</extension>" +
         "</complexContent>" +
      "</complexType>" +
      "<complexType name=\"SOAP_PatientDemographics\">" +
         "<sequence>" +
            "<element name=\"player\" minOccurs=\"0\">" +
               "<complexType>" +
                  "<sequence>" +
                     "<element name=\"firstName\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                     "<element name=\"lastName\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                     "<element name=\"fullName\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                     "<element name=\"initials\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                     "<element name=\"title\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                     "<element name=\"affix\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                     "<element name=\"birthTime\" minOccurs=\"0\" type=\"xsd:dateTime\"/>" +
                     "<element name=\"genderCode\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                     "<element name=\"guid\" minOccurs=\"0\" type=\"xsd:base64Binary\"/>" +
                     "<element name=\"id\" minOccurs=\"0\" maxOccurs=\"unbounded\">" +
                        "<complexType>" +
                           "<sequence>" +
                              "<any processContents=\"skip\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>" +
                           "</sequence>" +
                           "<attribute name=\"type\" use=\"required\" type=\"xsd:string\"/>" +
                           "<attribute use=\"required\" ref=\"b:id\"/>" +
                        "</complexType>" +
                     "</element>" +
                     "<element name=\"addr\" minOccurs=\"0\" maxOccurs=\"unbounded\">" +
                        "<complexType>" +
                           "<sequence>" +
                              "<element name=\"useCode\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                              "<element name=\"city\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                              "<element name=\"address1\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                              "<element name=\"address2\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                              "<element name=\"address3\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                              "<element name=\"state\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                              "<element name=\"country\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                              "<element name=\"zip\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                           "</sequence>" +
                        "</complexType>" +
                     "</element>" +
                     "<element name=\"telcom\" minOccurs=\"0\" maxOccurs=\"unbounded\">" +
                        "<complexType>" +
                           "<sequence>" +
                              "<element name=\"useCode\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                              "<element name=\"address\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                           "</sequence>" +
                        "</complexType>" +
                     "</element>" +
                  "</sequence>" +
               "</complexType>" +
            "</element>" +
         "</sequence>" +
      "</complexType>";

   protected final static String MULTIPLE_NAMESPACES_COMPLEX_TYPE =
      "<complexType name=\"XML_XSDExportMultipleNamespaces\">" +
         "<sequence>" +
            "<element ref=\"test2:comp\"/>" +
            "<element ref=\"test2:pns2\"/>" +
            "<element name=\"p2ns1\" form=\"qualified\" type=\"xsd:string\"/>" +
            "<element name=\"nons2\" type=\"xsd:string\"/>" +
            "<element ref=\"test3:pns3\"/>" +
         "</sequence>" +
      "</complexType>";

   protected final static String MULTIPLE_NAMESPACES_SCHEMA_1 =
      MULTIPLE_NAMESPACES_COMPLEX_TYPE +
      "<element name=\"pns1\" type=\"xsd:string\"/>" +
      "<element name=\"top\" type=\"test1:XML_XSDExportMultipleNamespaces\"/>";

   protected final static String MULTIPLE_NAMESPACES_SCHEMA_2 =
      "<element name=\"comp\">" +
         "<complexType>" +
            "<sequence>" +
               "<element name=\"pns2\" form=\"qualified\" type=\"xsd:string\"/>" +
               "<element ref=\"test1:pns1\"/>" +
               "<element name=\"nons1\" type=\"xsd:string\"/>" +
               "<element ref=\"test3:pns3\"/>" +
            "</sequence>" +
         "</complexType>" +
      "</element>" +
      "<element name=\"pns2\" type=\"xsd:string\"/>";

   protected final static String MULTIPLE_NAMESPACES_SCHEMA_3 =
      "<element name=\"pns3\" type=\"xsd:string\"/>";

   // associations

   protected XSDMessageExporter m_exporter;

   protected StringWriter m_resultWriter;

   /**
    * The metadata repository.
    */
   protected Metadata m_metadata;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * Metadata with documentation strings loaded.
    */
   protected static Metadata s_documentedMetadata = new MetadataLoaderDispatcher().load(null, null, XMLMetadataLoader.DOCUMENTATION_INCLUDED, null);

   // operations

   public void setUp() throws Exception
   {
      m_metadata = s_documentedMetadata;
      m_context = (InvocationContext)m_metadata.getComponent("System.InvocationContext").getInstance(null);
      m_exporter = new XSDMessageExporter(m_context);
      m_resultWriter = new StringWriter();
   }

   public void testNillable() throws Exception
   {
      Message message = m_metadata.getMessage("XML_NillableSubMessage");

      m_exporter.export(message, m_resultWriter);

      assertEquals(
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:test1=\"http://www.nexj.com/schema/test/one\"" +
         " xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " targetNamespace=\"http://www.nexj.com/schema/test/one\">" +
            NILLABLE_MESSAGE_SCHEMA +
            "<element name=\"outer\" type=\"test1:XML_NillableSubMessage\"/>" +
         "</schema>",
         m_resultWriter.toString());
   }

   public void testComprehensiveXSD() throws Exception
   {
      Message message = m_metadata.getMessage("XML_XSDExportComprehensive");

      m_exporter.export(message, m_resultWriter);

      assertEquals(
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:rim=\"urn:oasis:names:tc:ebxml-regrep:xsd:rim:3.0\"" +
         " xmlns:rs=\"urn:oasis:names:tc:ebxml-regrep:xsd:rs:3.0\"" +
         " xmlns:test=\"http://www.nexj.com/schema/test/one\"" +
         " xmlns:test2=\"http://www.nexj.com/schema/test/two\"" +
         " xmlns:test3=\"http://www.nexj.com/schema/test/three\"" +
         " xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " targetNamespace=\"http://www.nexj.com/schema/test/one\"" +
         ">" +
            "<import namespace=\"urn:oasis:names:tc:ebxml-regrep:xsd:rs:3.0\"/>" +
            "<import namespace=\"http://www.nexj.com/schema/test/two\"/>" +
            "<import namespace=\"http://www.nexj.com/schema/test/three\"/>" +
            COMPREHENSIVE_MESSAGE_SCHEMA +
            MULTIPLE_NAMESPACES_COMPLEX_TYPE +
            "<element name=\"comp\" type=\"test:XML_XSDExportComprehensive\"/>" +
            "<element name=\"pns1\" type=\"xsd:string\"/>" +
            "<element name=\"top\" type=\"test:XML_XSDExportMultipleNamespaces\"/>" +
         "</schema>",
         m_resultWriter.toString());
   }

   public void testInheritance1() throws Exception
   {
      SchemaUniverse universe = new SchemaUniverse();
      XSDSchemaExporter xsdExporter = new XSDSchemaExporter(universe);
      MessageSchemaConverter schemaExporter = new MessageSchemaConverter(universe);
      Message msg = m_context.getMetadata().getMessage("XML_Inherit_Schema_Reference");
      Schema schema = schemaExporter.add(msg).getSchema();

      xsdExporter.exportSchema(schema, m_resultWriter);

      assertEquals(
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:is1=\"urn:com.nexjsystems:ns:test:inherit:schema1\"" +
         " xmlns:is3=\"urn:com.nexjsystems:ns:test:inherit:schema3\"" +
         " xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " targetNamespace=\"urn:com.nexjsystems:ns:test:inherit:schema1\"" +
         ">" +
         "<import namespace=\"urn:com.nexjsystems:ns:test:inherit:schema3\"/>" +
         "<complexType name=\"Schema1Type\">" +
            "<sequence>" +
               "<element name=\"a\" minOccurs=\"0\" type=\"xsd:string\"/>" +
               "<element name=\"x\" minOccurs=\"0\" type=\"xsd:string\"/>" +
            "</sequence>" +
         "</complexType>" +
         "<complexType name=\"Schema2Type\" block=\"#all\">" +
            "<complexContent>" +
               "<extension base=\"is1:Schema1Type\">" +
                  "<sequence>" +
                     "<element name=\"b\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                  "</sequence>" +
               "</extension>" +
            "</complexContent>" +
         "</complexType>" +
         "<complexType name=\"Schema4Type\">" +
            "<sequence>" +
               "<element name=\"a\" minOccurs=\"0\" type=\"xsd:int\"/>" +
               "<element name=\"x\" minOccurs=\"0\" type=\"xsd:string\"/>" +
               "<element name=\"extra\" type=\"xsd:string\"/>" +
            "</sequence>" +
         "</complexType>" +
         "<complexType name=\"XML_Inherit_Schema_Reference\">" +
            "<sequence>" +
               "<element name=\"schema1\" minOccurs=\"0\" form=\"qualified\"/>" +
               "<element name=\"schema2\" minOccurs=\"0\" form=\"qualified\" type=\"is1:Schema2Type\"/>" +
               "<element name=\"schema3\" minOccurs=\"0\" form=\"qualified\" type=\"is3:Schema3Type\"/>" +
               "<element name=\"schema4\" minOccurs=\"0\" form=\"qualified\" type=\"is1:Schema4Type\"/>" +
            "</sequence>" +
         "</complexType>" +
         "<element name=\"references\" type=\"is1:XML_Inherit_Schema_Reference\"/>" +
         "</schema>",
         m_resultWriter.toString());

      m_resultWriter = new StringWriter();
      schema = universe.findSchema("is3");
      xsdExporter.exportSchema(schema, m_resultWriter);

      assertEquals(
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:is1=\"urn:com.nexjsystems:ns:test:inherit:schema1\"" +
         " xmlns:is3=\"urn:com.nexjsystems:ns:test:inherit:schema3\"" +
         " xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " targetNamespace=\"urn:com.nexjsystems:ns:test:inherit:schema3\"" +
         ">" +
         "<import namespace=\"urn:com.nexjsystems:ns:test:inherit:schema1\"/>" +
         "<complexType name=\"Schema3Type\">" +
            "<complexContent>" +
               "<extension base=\"is1:Schema2Type\">" +
                  "<sequence>" +
                     "<element name=\"cReal\" minOccurs=\"0\" type=\"xsd:string\"/>" +
                  "</sequence>" +
               "</extension>" +
            "</complexContent>" +
         "</complexType>" +
         "</schema>",
         m_resultWriter.toString());

      /*
       * Test that the XML formats with the same namespace qualifications as described
       * in the XML schema.
       */
      StringWriter writer;
      WriterOutput out;
      TransferObject root, ref;
      Format format = m_context.getMetadata().getFormat("XML");
      MessageFormatter formatter = (MessageFormatter)format.getFormatter().getInstance(m_context);

      root = new TransferObject("XML_Inherit_Schema_Reference", 1);
      ref = new TransferObject("XML_Inherit_Schema1", 2);
      root.setValue("s1", ref);
      ref.setValue("a", "aValue");
      ref.setValue("x", "xValue");
      out = new WriterOutput(writer = new StringWriter());
      formatter.format(root, msg, out);

      assertEquals(
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<is1:references" +
            " xmlns:is3=\"urn:com.nexjsystems:ns:test:inherit:schema3\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" +
            " xmlns:is1=\"urn:com.nexjsystems:ns:test:inherit:schema1\"" +
            " xsi:schemaLocation=\"" +
               "urn:com.nexjsystems:ns:test:inherit:schema3 http://www.nexjsystems.com/ns/test/inherit/schema3.xsd" +
               " urn:com.nexjsystems:ns:test:inherit:schema1 http://www.nexjsystems.com/ns/test/inherit/schema1.xsd\"" +
            ">" +
            "<is1:schema1>" +
            "<a>aValue</a>" +
            "<x>xValue</x>" +
            "</is1:schema1>" +
         "</is1:references>", writer.toString());
   }

   /**
    * TODO: Improve this test when additional format strings are fully supported.
    */
   public void testFormatStrings() throws Exception
   {
      Message message = m_metadata.getMessage("XML_FormatString");

      m_exporter.export(message, m_resultWriter);

      assertEquals(
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         ">" +
            "<complexType name=\"XML_FormatString\">" +
               "<sequence>" +
                  "<element name=\"normalDT\" minOccurs=\"0\" type=\"xsd:dateTime\"/>" +
                  "<element name=\"formatDT\" minOccurs=\"0\"></element>" +
                  "<element name=\"formatLocaleDT\" minOccurs=\"0\"></element>" +
                  "<element name=\"normalInteger\" minOccurs=\"0\" maxOccurs=\"unbounded\" type=\"xsd:int\"/>" +
                  "<element name=\"formatInteger\" minOccurs=\"0\" maxOccurs=\"unbounded\"></element>" +
                  "<element name=\"normalDouble\" minOccurs=\"0\" maxOccurs=\"unbounded\" type=\"xsd:double\"/>" +
                  "<element name=\"formatDouble\" minOccurs=\"0\" maxOccurs=\"unbounded\"></element>" +
                  "<element name=\"normalBool\" minOccurs=\"0\" type=\"xsd:boolean\"/>" +
                  "<element name=\"formatBool\" minOccurs=\"0\">" +
                     "<simpleType>" +
                        "<restriction base=\"xsd:boolean\">" +
                           "<enumeration value=\"Yes\"/>" +
                           "<enumeration value=\"Oui\"/>" +
                           "<enumeration value=\"No\"/>" +
                           "<enumeration value=\"Non\"/>" +
                        "</restriction>" +
                     "</simpleType>" +
                  "</element>" +
                  "<element name=\"localizedBool\" minOccurs=\"0\">" +
                     "<simpleType>" +
                        "<restriction base=\"xsd:boolean\"></restriction>" +
                     "</simpleType>" +
                  "</element>" +
                  "<element name=\"normalDecimal\" minOccurs=\"0\" type=\"xsd:decimal\"/>" +
                  "<element name=\"formatDecimal\" minOccurs=\"0\"></element>" +
               "</sequence>" +
            "</complexType>" +
            "<element name=\"format\" type=\"XML_FormatString\"/>" +
         "</schema>",
         m_resultWriter.toString());
   }

   public void testCircular() throws Exception
   {
      Message message = m_metadata.getMessage("XML_XSDExportCircular1");

      m_exporter.export(message, m_resultWriter);

      assertEquals(
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:test=\"urn:test\"" +
         " xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " targetNamespace=\"urn:test\">" +
            "<complexType name=\"XML_XSDExportCircular1\">" +
               "<sequence>" +
                  "<element name=\"circ2\" form=\"qualified\" type=\"test:XML_XSDExportCircular2\"/>" +
                  "<element name=\"a\" minOccurs=\"0\" form=\"qualified\" type=\"xsd:string\"/>" +
               "</sequence>" +
            "</complexType>" +
            "<complexType name=\"XML_XSDExportCircular2\">" +
               "<sequence>" +
                  "<element name=\"b\" minOccurs=\"0\" form=\"qualified\" type=\"xsd:string\"/>" +
                  "<element name=\"circ1\" minOccurs=\"0\" form=\"qualified\" type=\"test:XML_XSDExportCircular1\"/>" +
               "</sequence>" +
            "</complexType>" +
            "<element name=\"circ1\" type=\"test:XML_XSDExportCircular1\"/>" +
         "</schema>",
         m_resultWriter.toString());
   }

   /**
    * Tests that an XSD can be generated from an interface that has
    * messages using the same namespace.
    */
   public void testInterfaceToXSDWithOneNamespace() throws Exception
   {
      Interface iface = m_metadata.getInterface("XSDExportTest1");

      m_exporter.setOutputMode(XSDMessageExporter.OUTMODE_XSD);
      m_exporter.export(iface, m_resultWriter);

      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:rim=\"urn:oasis:names:tc:ebxml-regrep:xsd:rim:3.0\"" +
         " xmlns:rs=\"urn:oasis:names:tc:ebxml-regrep:xsd:rs:3.0\"" +
         " xmlns:test1=\"http://www.nexj.com/schema/test/one\"" +
         " xmlns:test2=\"http://www.nexj.com/schema/test/two\"" +
         " xmlns:test3=\"http://www.nexj.com/schema/test/three\"" +
         " xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " targetNamespace=\"http://www.nexj.com/schema/test/one\"" +
         ">" +
            "<import namespace=\"urn:oasis:names:tc:ebxml-regrep:xsd:rs:3.0\"/>" +
            "<import namespace=\"http://www.nexj.com/schema/test/two\"/>" +
            "<import namespace=\"http://www.nexj.com/schema/test/three\"/>" +
            NILLABLE_MESSAGE_SCHEMA +
            COMPREHENSIVE_MESSAGE_SCHEMA.replaceAll("test:", "test1:") +
            MULTIPLE_NAMESPACES_COMPLEX_TYPE +
            "<element name=\"comp\" type=\"test1:XML_XSDExportComprehensive\"/>" +
            "<element name=\"outer\" type=\"test1:XML_NillableSubMessage\"/>" +
            "<element name=\"pns1\" type=\"xsd:string\"/>" +
            "<element name=\"top\" type=\"test1:XML_XSDExportMultipleNamespaces\"/>" +
         "</schema>",
         m_resultWriter.toString());
   }

   /**
    * Tests that a WSDL can be generated from an interface that has
    * messages using different namespaces.
    * 
    * Also tests that SOAP header parts are not included in the message
    * export.
    */
   public void testInterfaceToWSDLWithSeveralNamespaces() throws Exception
   {
      Interface iface = m_metadata.getInterface("WSDLExportTest1");
      Channel channel = m_metadata.getChannel("WSDLExportTestChannel1");

      m_exporter.setOutputMode(XSDMessageExporter.OUTMODE_WSDL);
      m_exporter.export(iface, channel, m_resultWriter);

      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<definitions name=\"WSDLExportTestChannel1\"" +
         " targetNamespace=\"urn:WSDLExportTestChannel1\"" +
         " xmlns=\"http://schemas.xmlsoap.org/wsdl/\"" +
         " xmlns:a=\"urn:com.nexjsystems:ns:test:a\"" +
         " xmlns:b=\"urn:com.nexjsystems:ns:test:b\"" +
         " xmlns:m=\"http://travelcompany.example.org/reservation\"" +
         " xmlns:rim=\"urn:oasis:names:tc:ebxml-regrep:xsd:rim:3.0\"" +
         " xmlns:rs=\"urn:oasis:names:tc:ebxml-regrep:xsd:rs:3.0\"" +
         " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
         " xmlns:soapbind=\"http://schemas.xmlsoap.org/wsdl/soap/\"" +
         " xmlns:test1=\"http://www.nexj.com/schema/test/one\"" +
         " xmlns:test2=\"http://www.nexj.com/schema/test/two\"" +
         " xmlns:test3=\"http://www.nexj.com/schema/test/three\"" +
         " xmlns:tns=\"urn:WSDLExportTestChannel1\"" +
         " xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\"" +
         " xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         ">" +
            "<types>" +
               "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
                  " targetNamespace=\"urn:com.nexjsystems:ns:test:a\">" +
                  "<import namespace=\"urn:com.nexjsystems:ns:test:b\"/>" +
                  SOAP_HEADERS_PATIENT_MESSAGE_SCHEMA +
                  "<element name=\"Patient\" type=\"a:SOAP_Headers_PatientDemographics\"/>" +
               "</schema>" +
               "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
                  " targetNamespace=\"urn:com.nexjsystems:ns:test:b\">" +
                  SOAP_HEADERS_PATIENT_MESSAGE_SCHEMA_2 +
               "</schema>" +
                  SOAP_HEADERS_RESERVATION_SCHEMA +
               "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
                  " targetNamespace=\"urn:oasis:names:tc:ebxml-regrep:xsd:rim:3.0\">" +
                  SUBMIT_OBJECTS_SCHEMA_2 +
               "</schema>" +
               "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
                  " targetNamespace=\"urn:oasis:names:tc:ebxml-regrep:xsd:rs:3.0\">" +
                  "<import namespace=\"urn:oasis:names:tc:ebxml-regrep:xsd:rim:3.0\"/>" +
                  SUBMIT_OBJECTS_SCHEMA_1 +
               "</schema>" +
               "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
                  " targetNamespace=\"http://www.nexj.com/schema/test/one\">" +
                  "<import namespace=\"urn:oasis:names:tc:ebxml-regrep:xsd:rs:3.0\"/>" +
                  "<import namespace=\"http://www.nexj.com/schema/test/two\"/>" +
                  "<import namespace=\"http://www.nexj.com/schema/test/three\"/>" +
                  NILLABLE_MESSAGE_SCHEMA +
                  COMPREHENSIVE_MESSAGE_SCHEMA.replaceAll("test:", "test1:") +
                  MULTIPLE_NAMESPACES_COMPLEX_TYPE +
                  "<element name=\"comp\" type=\"test1:XML_XSDExportComprehensive\"/>" +
                  "<element name=\"outer\" type=\"test1:XML_NillableSubMessage\"/>" +
                  "<element name=\"pns1\" type=\"xsd:string\"/>" +
                  "<element name=\"top\" type=\"test1:XML_XSDExportMultipleNamespaces\"/>" +
               "</schema>" +
               "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
                  " targetNamespace=\"http://www.nexj.com/schema/test/two\">" +
                  "<import namespace=\"http://www.nexj.com/schema/test/one\"/>" +
                  "<import namespace=\"http://www.nexj.com/schema/test/three\"/>" +
                  MULTIPLE_NAMESPACES_SCHEMA_2 +
               "</schema>" +
               "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
                  " targetNamespace=\"http://www.nexj.com/schema/test/three\">" +
                  MULTIPLE_NAMESPACES_SCHEMA_3 +
               "</schema>" +
               SOAP_HEADERS_WSCOOR_SCHEMA +
            "</types>" +
            "<message name=\"SOAP_Headers_PatientDemographics\">" +
               "<part name=\"parameters\" element=\"a:Patient\"/>" +
               SOAP_HEADERS_PATIENT_MESSAGE_PART +
            "</message>" +
            "<message name=\"XML_NillableSubMessage\">" +
               "<part name=\"parameters\" element=\"test1:outer\"/>" +
            "</message>" +
            "<message name=\"XML_XSDExportComprehensive\">" +
               "<part name=\"parameters\" element=\"test1:comp\"/>" +
            "</message>" +
            "<portType name=\"WSDLExportTestChannel1\">" +
               "<operation name=\"AddPatient\">" +
                  "<input message=\"tns:SOAP_Headers_PatientDemographics\"/>" +
               "</operation>" +
               "<operation name=\"ComprehensiveOp\">" +
                  "<input message=\"tns:XML_XSDExportComprehensive\"/>" +
                  "<output message=\"tns:XML_XSDExportComprehensive\"/>" +
               "</operation>" +
               "<operation name=\"NillableOp\">" +
                  "<input message=\"tns:XML_NillableSubMessage\"/>" +
               "</operation>" +
            "</portType>" +
            "<binding name=\"WSDLExportTestChannel1\" type=\"tns:WSDLExportTestChannel1\">" +
               "<soapbind:binding style=\"document\" transport=\"http://schemas.xmlsoap.org/soap/http\"/>" +
               "<operation name=\"AddPatient\">" +
                  "<soapbind:operation soapAction=\"Patient#add\"/>" +
                  "<input>" +
                     "<soapbind:body use=\"literal\" parts=\"parameters\"/>" +
                     SOAP_HEADERS_PATIENT_MESSAGE_BIND +
                  "</input>" +
               "</operation>" +
               "<operation name=\"ComprehensiveOp\">" +
                  "<soapbind:operation soapAction=\"http://www.nexj.com/soap/action/ComprehensiveAction\"/>" +
                  "<input>" +
                     "<soapbind:body use=\"literal\" parts=\"parameters\"/>" +
                  "</input>" +
                  "<output>" +
                     "<soapbind:body use=\"literal\" parts=\"parameters\"/>" +
                  "</output>" +
               "</operation>" +
               "<operation name=\"NillableOp\">" +
                  "<soapbind:operation soapAction=\"http://www.nexj.com/soap/action/NillableAction\"/>" +
                  "<input>" +
                     "<soapbind:body use=\"literal\" parts=\"parameters\"/>" +
                  "</input>" +
               "</operation>" +
            "</binding>" +
            "<service name=\"WSDLExportTestChannel1\">" +
               "<port name=\"WSDLExportTestChannel1\" binding=\"tns:WSDLExportTestChannel1\">" +
                  "<soapbind:address location=\"http://host.domain.ext/" + SysUtil.NAMESPACE + "/channel/WSDLExportTestChannel1\"/>" +
               "</port>" +
            "</service>" +
         "</definitions>", m_resultWriter.toString());
   }

   public void testMultipleNamespaces() throws Exception
   {
      Message message = m_metadata.getMessage("XML_XSDExportMultipleNamespaces");
      m_exporter.setOutputMode(XSDMessageExporter.OUTMODE_WSDL);
      m_exporter.export(message, m_resultWriter);

      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<definitions name=\"XML_XSDExportMultipleNamespaces\"" +
         " targetNamespace=\"urn:XML_XSDExportMultipleNamespaces\"" +
         " xmlns=\"http://schemas.xmlsoap.org/wsdl/\"" +
         " xmlns:soapbind=\"http://schemas.xmlsoap.org/wsdl/soap/\"" +
         " xmlns:test1=\"http://www.nexj.com/schema/test/one\"" +
         " xmlns:test2=\"http://www.nexj.com/schema/test/two\"" +
         " xmlns:test3=\"http://www.nexj.com/schema/test/three\"" +
         " xmlns:tns=\"urn:XML_XSDExportMultipleNamespaces\"" +
         " xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         ">" +
            "<types>" +
               "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
               " targetNamespace=\"http://www.nexj.com/schema/test/one\">" +
                  "<import namespace=\"http://www.nexj.com/schema/test/two\"/>" +
                  "<import namespace=\"http://www.nexj.com/schema/test/three\"/>" +
                  MULTIPLE_NAMESPACES_SCHEMA_1 +
               "</schema>" +
               "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
                  " targetNamespace=\"http://www.nexj.com/schema/test/two\">" +
                  "<import namespace=\"http://www.nexj.com/schema/test/one\"/>" +
                  "<import namespace=\"http://www.nexj.com/schema/test/three\"/>" +
                  MULTIPLE_NAMESPACES_SCHEMA_2 +
               "</schema>" +
               "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\"" +
               " targetNamespace=\"http://www.nexj.com/schema/test/three\">" +
                  MULTIPLE_NAMESPACES_SCHEMA_3 +
               "</schema>" +
            "</types>" +
            "<message name=\"XML_XSDExportMultipleNamespaces\">" +
               "<part name=\"parameters\" element=\"test1:top\"/>" +
            "</message>" +
            "<portType name=\"XML_XSDExportMultipleNamespaces\">" +
               "<operation name=\"MultipleNamespacesOp\">" +
                  "<input message=\"tns:XML_XSDExportMultipleNamespaces\"/>" +
                  "<output message=\"tns:XML_XSDExportMultipleNamespaces\"/>" +
               "</operation>" +
            "</portType>" +
            "<binding name=\"XML_XSDExportMultipleNamespaces\" type=\"tns:XML_XSDExportMultipleNamespaces\">" +
               "<soapbind:binding style=\"document\" transport=\"http://schemas.xmlsoap.org/soap/http\"/>" +
               "<operation name=\"MultipleNamespacesOp\">" +
                  "<soapbind:operation soapAction=\"http://www.nexj.com/soap/action/MultipleNamespacesAction\"/>" +
                  "<input>" +
                     "<soapbind:body use=\"literal\" parts=\"parameters\"/>" +
                  "</input>" +
                  "<output>" +
                     "<soapbind:body use=\"literal\" parts=\"parameters\"/>" +
                  "</output>" +
               "</operation>" +
            "</binding>" +
            "<service name=\"XML_XSDExportMultipleNamespaces\">" +
               "<port name=\"XML_XSDExportMultipleNamespaces\" binding=\"tns:XML_XSDExportMultipleNamespaces\">" +
                  "<soapbind:address location=\"http://host.domain.ext/" + SysUtil.NAMESPACE + "/channel/CHANNEL\"/>" +
               "</port>" +
            "</service>" +
         "</definitions>", m_resultWriter.toString());
   }

   public void testMessageReference() throws Exception
   {
      Interface iface = m_metadata.getInterface("WSDLExportTest2");
      Channel channel = m_metadata.getChannel("WSDLExportTestChannel1");

      m_exporter.setOutputMode(XSDMessageExporter.OUTMODE_WSDL);
      m_exporter.export(iface, channel, m_resultWriter);

      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<definitions name=\"WSDLExportTestChannel1\"" +
         " targetNamespace=\"urn:WSDLExportTestChannel1\"" +
         " xmlns=\"http://schemas.xmlsoap.org/wsdl/\"" +
         " xmlns:a=\"urn:com.nexjsystems:ns:test:a\"" +
         " xmlns:b=\"urn:com.nexjsystems:ns:test:b\"" +
         " xmlns:soapbind=\"http://schemas.xmlsoap.org/wsdl/soap/\"" +
         " xmlns:tns=\"urn:WSDLExportTestChannel1\"" +
         " xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         ">" +
            "<types>" +
               "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\" " +
               "targetNamespace=\"urn:com.nexjsystems:ns:test:a\">" +
                  "<import namespace=\"urn:com.nexjsystems:ns:test:b\"/>" +
                  "<complexType name=\"XML_InternationalPrice\">" +
                     "<complexContent>" +
                        "<extension base=\"a:XML_LocalPrice\">" +
                           "<attribute name=\"currency\" use=\"required\">" +
                              "<simpleType>" +
                                 "<restriction base=\"xsd:string\">" +
                                    "<enumeration value=\"CNY\"/>" +
                                    "<enumeration value=\"GBP\"/>" +
                                 "</restriction>" +
                              "</simpleType>" +
                           "</attribute>" +
                        "</extension>" +
                     "</complexContent>" +
                  "</complexType>" +
                  "<complexType name=\"XML_LocalPrice\">" +
                     "<simpleContent>" +
                        "<extension base=\"xsd:decimal\"></extension>" +
                     "</simpleContent>" +
                  "</complexType>" +
                  "<complexType name=\"XML_PatientId\">" +
                     "<sequence>" +
                        "<any processContents=\"skip\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>" +
                     "</sequence>" +
                     "<attribute name=\"type\" use=\"required\" type=\"xsd:string\"/>" +
                     "<attribute use=\"required\" ref=\"b:id\"/>" +
                  "</complexType>" +
                  "<complexType name=\"XML_PatientIdReferrer1\">" +
                     "<sequence>" +
                        "<element name=\"id1\" minOccurs=\"0\" maxOccurs=\"unbounded\" type=\"a:XML_PatientId\"/>" +
                        "<element name=\"id2\" minOccurs=\"0\" maxOccurs=\"unbounded\" type=\"a:XML_PatientId\"/>" +
                     "</sequence>" +
                  "</complexType>" +
                  "<complexType name=\"XML_PatientIdReferrer2\">" +
                     "<sequence>" +
                        "<element name=\"id1\" minOccurs=\"0\" maxOccurs=\"unbounded\" type=\"a:XML_PatientId\"/>" +
                        "<element name=\"internationalPrice\" minOccurs=\"0\" type=\"a:XML_InternationalPrice\"/>" +
                        "<element name=\"localPrice\" minOccurs=\"0\" type=\"a:XML_LocalPrice\"/>" +
                        "<element name=\"singleValueChild\" minOccurs=\"0\">" +
                           "<complexType>" +
                              "<simpleContent>" +
                                 "<extension base=\"xsd:int\"></extension>" +
                              "</simpleContent>" +
                           "</complexType>" +
                        "</element>" +
                     "</sequence>" +
                  "</complexType>" +
                  "<element name=\"PatientIdReferrer1\" type=\"a:XML_PatientIdReferrer1\"/>" +
                  "<element name=\"PatientIdReferrer2\" type=\"a:XML_PatientIdReferrer2\"/>" +
               "</schema>" +
               "<schema xmlns=\"http://www.w3.org/2001/XMLSchema\" " +
               "targetNamespace=\"urn:com.nexjsystems:ns:test:b\">" +
                  "<attribute name=\"id\" type=\"xsd:string\"/>" +
               "</schema>" +
            "</types>" +
            "<message name=\"XML_PatientIdReferrer1\">" +
               "<part name=\"parameters\" element=\"a:PatientIdReferrer1\"/>" +
            "</message>" +
            "<message name=\"XML_PatientIdReferrer2\">" +
               "<part name=\"parameters\" element=\"a:PatientIdReferrer2\"/>" +
            "</message>" +
            "<portType name=\"WSDLExportTestChannel1\">" +
               "<operation name=\"XML_PatientIdReferrer1\">" +
                  "<input message=\"tns:XML_PatientIdReferrer1\"/>" +
               "</operation>" +
               "<operation name=\"XML_PatientIdReferrer2\">" +
                  "<input message=\"tns:XML_PatientIdReferrer2\"/>" +
               "</operation>" +
            "</portType>" +
            "<binding name=\"WSDLExportTestChannel1\" type=\"tns:WSDLExportTestChannel1\">" +
               "<soapbind:binding style=\"document\" transport=\"http://schemas.xmlsoap.org/soap/http\"/>" +
               "<operation name=\"XML_PatientIdReferrer1\">" +
                  "<soapbind:operation soapAction=\"\"/>" +
                  "<input>" +
                     "<soapbind:body use=\"literal\" parts=\"parameters\"/>" +
                  "</input>" +
               "</operation>" +
               "<operation name=\"XML_PatientIdReferrer2\">" +
                  "<soapbind:operation soapAction=\"\"/>" +
                  "<input>" +
                     "<soapbind:body use=\"literal\" parts=\"parameters\"/>" +
                  "</input>" +
               "</operation>" +
            "</binding>" +
            "<service name=\"WSDLExportTestChannel1\">" +
               "<port name=\"WSDLExportTestChannel1\" binding=\"tns:WSDLExportTestChannel1\">" +
                  "<soapbind:address location=\"http://host.domain.ext/" + SysUtil.NAMESPACE + "/channel/WSDLExportTestChannel1\"/>" +
               "</port>" +
            "</service>" +
         "</definitions>", m_resultWriter.toString());
   }
}
