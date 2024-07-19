package nexj.core.meta.integration.format.xml.wsdl;

import java.net.URL;
import java.util.List;

import junit.framework.TestCase;

import nexj.core.meta.integration.format.xml.schema.Attribute;
import nexj.core.meta.integration.format.xml.schema.AttributeRef;
import nexj.core.meta.integration.format.xml.schema.CompositeType;
import nexj.core.meta.integration.format.xml.schema.Element;
import nexj.core.meta.integration.format.xml.schema.ElementRef;
import nexj.core.meta.integration.format.xml.schema.PrimitiveType;
import nexj.core.meta.integration.format.xml.schema.SchemaItem;
import nexj.core.meta.integration.format.xml.schema.Type;

/**
 * JUnit test for WSDLServiceImporter
 */
public class WSDLServiceImporterTest extends TestCase
{
   public void testNamespaceShadowing() throws Exception
   {
      List serviceList = WSDLServiceImporter.parse(WSDLServiceImporter.class.getResource("ExampleService/multiSchemaService.wsdl"));

      assertEquals(1, serviceList.size());

      SOAPService service = (SOAPService)serviceList.get(0);
      Operation op = (Operation)service.m_operationMap.get("Process");
      assertNotNull(op);

      Message input = op.m_input;
      Element elementA = input.m_root;
      CompositeType typeA = (CompositeType)elementA.getType();

      ElementRef elementCRef = (ElementRef)typeA.getChild("elementC", SchemaItem.ELEMENT_REF);

      assertNotNull(elementCRef);
      assertNotNull(elementCRef.getReferent());

      Element elementC = elementCRef.getReferent();

      assertEquals(PrimitiveType.STRING, elementC.getType());
   }

   public void testTypes() throws Exception
   {
      List serviceList = WSDLServiceImporter.parse(WSDLServiceImporter.class.getResource("ExampleService/typesService.wsdl"));

      assertEquals(1, serviceList.size());

      SOAPService service = (SOAPService)serviceList.get(0);
      Operation op = (Operation)service.m_operationMap.get("Process");
      assertNotNull(op);

      Message input = op.m_input;
      Element process = input.m_root;
      CompositeType everythingType = (CompositeType)process.getType();

      ElementRef topLevelElementRef = (ElementRef)everythingType.getChild("topLevelElement", SchemaItem.ELEMENT_REF);

      assertNotNull(topLevelElementRef);

      Element topLevelElement = topLevelElementRef.getReferent();

      assertNotNull(topLevelElement);

      ElementRef topLevelElementTwoRef = (ElementRef)everythingType.getChild("topLevelElementTwo", SchemaItem.ELEMENT_REF);
      Element topLevelElementTwo = topLevelElementTwoRef.getReferent();

      assertNotNull(topLevelElementTwo);

      Type topLevelType = topLevelElementTwo.getType();

      assertNotNull(topLevelType);

      Element childA = (Element)everythingType.getChild("childA", SchemaItem.ELEMENT);

      assertTrue(topLevelType == childA.getType());

      Element childB = (Element)everythingType.getChild("childB", SchemaItem.ELEMENT);
      CompositeType topLevelTypeChoice = (CompositeType)childB.getType();

      assertEquals(CompositeType.AGGREGATION_CHOICE, topLevelTypeChoice.getAggregation());

      Element localElementA = (Element)topLevelTypeChoice.getChild("localElementA", SchemaItem.ELEMENT);

      assertNotNull(localElementA);

      Element localElementB = (Element)topLevelTypeChoice.getChild("localElementB", SchemaItem.ELEMENT);

      assertEquals(PrimitiveType.STRING, localElementB.getType());

      Element localElementC = (Element)topLevelTypeChoice.getChild("localElementC", SchemaItem.ELEMENT);

      assertTrue(topLevelType == localElementC.getType());

      Element localElementD = (Element)topLevelTypeChoice.getChild("localElementD", SchemaItem.ELEMENT);
      CompositeType topLevelTypeSequence = (CompositeType)localElementD.getType();

      assertEquals("topLevelTypeSequence", topLevelTypeSequence.getName());
      assertEquals(0, localElementD.getMaxCount());

      Element localElementE = (Element)topLevelTypeChoice.getChild("localElementE", SchemaItem.ELEMENT);
      CompositeType topLevelTypeAll = (CompositeType)localElementE.getType();

      assertEquals("topLevelTypeAll", topLevelTypeAll.getName());
      assertEquals(3, localElementE.getMaxCount());
      assertEquals(2, localElementE.getMinCount());
      assertTrue(localElementE.isNillable());

      Element localElementF = (Element)topLevelTypeChoice.getChild("localElementF", SchemaItem.ELEMENT);

      assertTrue(localElementF.isNillable());

      Element localElementG = (Element)topLevelTypeChoice.getChild("localElementG", SchemaItem.ELEMENT);

      assertEquals(0, localElementG.getMinCount());

      Element localElementH = (Element)topLevelTypeChoice.getChild("localElementH", SchemaItem.ELEMENT);
      CompositeType hInnerType = (CompositeType)localElementH.getType();
      Element innerElementB = (Element)hInnerType.getChild("innerElementB", SchemaItem.ELEMENT);
      topLevelElementTwoRef = (ElementRef)hInnerType.getChild("topLevelElementTwo", SchemaItem.ELEMENT_REF);;

      assertEquals(3, hInnerType.getChildCount());
      assertEquals(PrimitiveType.DOUBLE, innerElementB.getType());
      assertEquals(topLevelElementTwo, topLevelElementTwoRef.getReferent());

      Element localElementI = (Element)topLevelTypeChoice.getChild("localElementI", SchemaItem.ELEMENT);
      CompositeType iType = (CompositeType)localElementI.getType();

      assertEquals("topLevelTypeChoice.localElementI", iType.getDescription());

      Element iTypeChild = (Element)iType.getChild(0);
      CompositeType iTypeChildType = (CompositeType)iTypeChild.getType();
      String sDesc = "topLevelTypeChoice.localElementI.innerInnerElement" +
            "\r\n\r\nInline defined type with multi-line documentation." +
            "\nThis is the second line.";

      assertEquals(sDesc, iTypeChildType.getDescription());

      ElementRef choiceRef1 = (ElementRef)topLevelTypeChoice.getChild("topLevelElement", SchemaItem.ELEMENT_REF);

      assertTrue(topLevelElement == choiceRef1.getReferent());

      ElementRef choiceRef2 = (ElementRef)topLevelTypeChoice.getChild("topLevelElementTwo", SchemaItem.ELEMENT_REF);

      assertTrue(topLevelElementTwo == choiceRef2.getReferent());
      assertEquals(4, choiceRef2.getMaxCount());
      assertEquals(2, choiceRef2.getMinCount());

      ElementRef choiceRef3 = (ElementRef)topLevelTypeChoice.getChild("topLevelElementThree", SchemaItem.ELEMENT_REF);
      Element topLevelElementThree = choiceRef3.getReferent();

      assertNotNull(topLevelElementThree);
      assertEquals(0, choiceRef3.getMaxCount());
      assertEquals(0, choiceRef3.getMinCount());

      Element childC = (Element)everythingType.getChild("childC", SchemaItem.ELEMENT);

      assertEquals(topLevelTypeSequence, childC.getType());
      assertEquals(CompositeType.AGGREGATION_SEQUENTIAL, topLevelTypeSequence.getAggregation());
      assertEquals(3, topLevelTypeSequence.getChildCount());

      Element childD = (Element)everythingType.getChild("childD", SchemaItem.ELEMENT);

      assertEquals(topLevelTypeAll, childD.getType());
      assertEquals(CompositeType.AGGREGATION_RANDOM, topLevelTypeAll.getAggregation());
      assertEquals(3, topLevelTypeAll.getChildCount());

      Element elementInlineComplexType = (Element)everythingType.getChild("elementInlineComplexType", SchemaItem.ELEMENT);
      CompositeType inlineType = (CompositeType)elementInlineComplexType.getType();
      Element localElement1 = (Element)inlineType.getChild(0);
      CompositeType inlineType2 = (CompositeType)localElement1.getType();

      assertNotNull(inlineType2);

      Element childE = (Element)everythingType.getChild("childE", SchemaItem.ELEMENT);

      assertEquals("topLevelSimpleType", childE.getType().getName());

      Element childF = (Element)everythingType.getChild("childF", SchemaItem.ELEMENT);
      CompositeType topLevelType2 = (CompositeType)childF.getType();

      AttributeRef topLevelAttribute = (AttributeRef)topLevelType2.getChild("topLevelAttribute", SchemaItem.ATTRIBUTE_REF);
      assertNotNull(topLevelAttribute);
      assertEquals(PrimitiveType.BOOLEAN, topLevelAttribute.getType());

      AttributeRef topLevelAttribute2 = (AttributeRef)topLevelType2.getChild("topLevelAttribute2", SchemaItem.ATTRIBUTE_REF);
      assertNotNull(topLevelAttribute2);
      assertEquals("topLevelSimpleType", topLevelAttribute2.getType().getName());
      assertTrue(topLevelAttribute2.isRequired());

      Attribute localAttrA = (Attribute)topLevelType2.getChild("localAttrA", SchemaItem.ATTRIBUTE);
      assertNotNull(localAttrA);
      assertEquals("topLevelSimpleType", localAttrA.getType().getName());

      Attribute localAttrB = (Attribute)topLevelType2.getChild("localAttrB", SchemaItem.ATTRIBUTE);
      assertNotNull(localAttrB);
      assertEquals(PrimitiveType.DOUBLE, localAttrB.getType());
      assertTrue(localAttrB.isRequired());

      Element childG = (Element)everythingType.getChild("childG", SchemaItem.ELEMENT);
      CompositeType topLevelType4 = (CompositeType)childG.getType();

      assertNotNull(topLevelType4);

      Element extensionElement = (Element)topLevelType4.getChild("extensionElement", SchemaItem.ELEMENT);
      Attribute extensionAttribute = (Attribute)topLevelType4.getChild("extensionAttribute", SchemaItem.ATTRIBUTE);
      assertNotNull(extensionElement);
      assertNotNull(extensionAttribute);
      assertEquals(PrimitiveType.DECIMAL, extensionElement.getType());
      assertEquals(PrimitiveType.STRING, extensionAttribute.getType());

      CompositeType topLevelType3 = (CompositeType)topLevelType4.getBase();

      assertNotNull(topLevelType3);

      Element baseElement = (Element)topLevelType3.getChild("baseElement", SchemaItem.ELEMENT);
      Attribute baseAttribute = (Attribute)topLevelType3.getChild("baseAttribute", SchemaItem.ATTRIBUTE);

      assertNotNull(baseElement);
      assertNotNull(baseAttribute);
      assertTrue(topLevelType2 == baseElement.getType());
      assertEquals("topLevelSimpleType", baseAttribute.getType().getName());
   }

   public void testImports() throws Exception
   {
      URL xsdURL = WSDLServiceImporter.class.getResource("ImportService/importExample.wsdl");
      List serviceList = WSDLServiceImporter.parse(xsdURL);

      assertEquals(1, serviceList.size());

      SOAPService service = (SOAPService)serviceList.get(0);

      assertEquals("ImportExample", service.getName());

      Operation processOp = (Operation)service.m_operationMap.get("process");

      assertNotNull(processOp);
      assertEquals("processRequest", processOp.getInput().getName());
      assertEquals("processResponse", processOp.getOutput().getName());

      Element process = processOp.getInput().getRoot();
      CompositeType processType = (CompositeType)process.getType();

      assertEquals(2, processType.getChildCount());

      ElementRef kitchenRef = (ElementRef)processType.getChild("kitchen", SchemaItem.ELEMENT_REF);

      assertNotNull(kitchenRef);
      assertNotNull(kitchenRef.getReferent());
      assertNotNull(kitchenRef.getReferent().getType());

      CompositeType kitchenType = (CompositeType)kitchenRef.getReferent().getType();

      assertEquals(1, kitchenType.getChildCount());

      ElementRef dishwasherRef = (ElementRef)kitchenType.getChild("dishwasher", SchemaItem.ELEMENT_REF);

      assertNotNull(dishwasherRef);
      assertNotNull(dishwasherRef.getReferent());
      assertEquals(PrimitiveType.STRING, dishwasherRef.getReferent().getType());

      ElementRef livingRef = (ElementRef)processType.getChild("livingRoom", SchemaItem.ELEMENT_REF);

      assertNotNull(livingRef);
      assertNotNull(livingRef.getReferent());
      assertNotNull(livingRef.getReferent().getType());

      CompositeType livingType = (CompositeType)livingRef.getReferent().getType();

      assertEquals(1, livingType.getChildCount());

      ElementRef computerRef = (ElementRef)livingType.getChild("computer", SchemaItem.ELEMENT_REF);

      assertNotNull(computerRef);
      assertNotNull(computerRef.getReferent());
      assertNotNull(computerRef.getReferent().getType());

      CompositeType computerType = (CompositeType)computerRef.getReferent().getType();

      assertEquals(2, computerType.getChildCount());

      ElementRef gpuRef = (ElementRef)computerType.getChild("gpu", SchemaItem.ELEMENT_REF);

      assertNotNull(gpuRef);
      assertNotNull(gpuRef.getReferent());
      assertNotNull(gpuRef.getReferent().getType());

      ElementRef moboRef = (ElementRef)computerType.getChild("motherboard", SchemaItem.ELEMENT_REF);

      assertNotNull(moboRef);
      assertNotNull(moboRef.getReferent());
      assertNotNull(moboRef.getReferent().getType());

      CompositeType moboType = (CompositeType)moboRef.getReferent().getType();

      assertEquals(2, moboType.getChildCount());

      ElementRef batteryRef = (ElementRef)moboType.getChild("battery", SchemaItem.ELEMENT_REF);

      assertNotNull(batteryRef);
      assertNotNull(batteryRef.getReferent());
      assertEquals(PrimitiveType.STRING, batteryRef.getReferent().getType());
   }

   public void testPortfolioService() throws Exception
   {
      URL xsdURL = WSDLServiceImporter.class.getResource("PortfolioService/PortfolioService.wsdl");
      List serviceList = WSDLServiceImporter.parse(xsdURL);

      assertEquals(1, serviceList.size());

      SOAPService service = (SOAPService)serviceList.get(0);

      assertEquals("PortfolioService", service.getName());
      assertEquals("http://devesb/ccx/cc-router/PortfolioService/v1", service.getEndpoint());

      Operation ping = (Operation)service.m_operationMap.get("ping");

      assertNotNull(ping);
      assertEquals("pingRequest", ping.getInput().getName());
      assertEquals("pingResponse", ping.getOutput().getName());

      Operation retrieveData = (Operation)service.m_operationMap.get("retrieveData");

      assertNotNull(retrieveData);
      assertEquals("retrieveDataRequest", retrieveData.getInput().getName());
      assertEquals("retrieveDataResponse", retrieveData.getOutput().getName());
   }

   public void testBasicWSDL() throws Exception
   {
      URL xsdURL = WSDLServiceImporter.class.getResource("BasicWSDL.wsdl");
      List serviceList = WSDLServiceImporter.parse(xsdURL);

      assertEquals(1, serviceList.size());

      SOAPService service = ((SOAPService)serviceList.get(0));

      assertEquals("Service", service.getName());

      Operation operation = (Operation)service.m_operationMap.get("OperationOne");
      assertNotNull(operation);
      assertEquals("opInput", operation.m_input.m_sName);
   }

   public void testMultiplePorts() throws Exception
   {
      URL xsdURL = WSDLServiceImporter.class.getResource("MultiplePorts.wsdl");
      List serviceList = WSDLServiceImporter.parse(xsdURL);

      assertEquals(3, serviceList.size());

      assertEquals("ServiceOne", ((SOAPService)serviceList.get(0)).getName());
      assertEquals("ServiceTwo_PortTwo", ((SOAPService)serviceList.get(1)).getName());
      assertEquals("ServiceTwo_PortThree", ((SOAPService)serviceList.get(2)).getName());
   }

   public void testOperationArguments() throws Exception
   {
      URL xsdURL = WSDLServiceImporter.class.getResource("OperationArguments.wsdl");
      List serviceList = WSDLServiceImporter.parse(xsdURL);

      assertEquals(1, serviceList.size());
      assertNotNull(((SOAPService)serviceList.get(0)).m_operationMap.get("OperationOne"));
      assertNotNull(((SOAPService)serviceList.get(0)).m_operationMap.get("OperationTwo"));
      assertNotNull(((SOAPService)serviceList.get(0)).m_operationMap.get("OperationThree"));
      assertNotNull(((SOAPService)serviceList.get(0)).m_operationMap.get("OperationFour"));
   }
}
