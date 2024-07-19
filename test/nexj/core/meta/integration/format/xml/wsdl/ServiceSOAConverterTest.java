package nexj.core.meta.integration.format.xml.wsdl;

import junit.framework.TestCase;

import nexj.core.meta.integration.format.xml.schema.Attribute;
import nexj.core.meta.integration.format.xml.schema.CompositeType;
import nexj.core.meta.integration.format.xml.schema.Element;
import nexj.core.meta.integration.format.xml.schema.EnumType;
import nexj.core.meta.integration.format.xml.schema.PrimitiveType;
import nexj.core.meta.integration.format.xml.schema.Schema;
import nexj.core.meta.soa.Definition;
import nexj.core.meta.soa.Interface;
import nexj.core.meta.soa.Method;
import nexj.core.meta.soa.ModelType;
import nexj.core.meta.soa.Result;

/**
 * Tests the conversion between schema intermediate format and SOA definition objects.
 */
public class ServiceSOAConverterTest extends TestCase
{
   public void testImportMinimum() throws Exception
   {
      SOAPService soapService = new SOAPService("1 This name contains unaccepted characters");
      Definition def = ServiceSOAConverter.convert(soapService);

      assertEquals("__This_name_contains_unaccepted_characters", def.getName());
      assertEquals("__This_name_contains_unaccepted_characters:1.0", def.getGlobalName());
      assertEquals(1, def.getServiceInterfaceCount());

      Interface iface = (Interface)def.getServiceInterfaceIterator().next();

      assertEquals("__This_name_contains_unaccepted_characters", iface.getName());
   }

   public void testImportMethods() throws Exception
   {
      Operation whereOperation = new Operation("que\u0300"); // grave accent
      Operation whatOperation = new Operation("que\u0301"); // acute accent
      SOAPService soapService = new SOAPService("QuestionService");

      soapService.addOperation(whereOperation);
      soapService.addOperation(whatOperation);

      Definition def = ServiceSOAConverter.convert(soapService);

      assertEquals(1, def.getServiceInterfaceCount());

      Interface iface = (Interface)def.getServiceInterfaceIterator().next();

      Method method1 = iface.getMethod("que_", 0);
      Method method2 = iface.getMethod("que_2", 0);

      assertNotNull(method1);
      assertNotNull(method2);
   }

   public void testImportTypes() throws Exception
   {
      Element build = new Element("Build");
      CompositeType buildType = new CompositeType(null);

      Element buildResponse = new Element("BuildResponse");
      CompositeType buildResponseType = new CompositeType(null);

      CompositeType materialsType = new CompositeType("materialsType");
      Attribute colorAttr = new Attribute("color");
      EnumType colorEnum = new EnumType("colorEnum", PrimitiveType.STRING);

      Element accessoriesElement = new Element("Accessories");
      CompositeType accessoriesType = new CompositeType(null);

      Element exteriorElement = new Element("Exterior");
      CompositeType exteriorType = new CompositeType(null);

      Element interiorElement = new Element("Interior");
      CompositeType interiorType = new CompositeType(null);

      Element seatElement = new Element("Seat");
      CompositeType seatType = new CompositeType(null);
      Attribute seatAttr = new Attribute("HasHeadrest");

      Element seatCoveringElement = new Element("SeatCovering");
      EnumType seatCoveringEnum = new EnumType(null, PrimitiveType.STRING);

      Element buildResult = new Element("BuildResult");
      CompositeType carType = new CompositeType("carType");

      Schema schema = new Schema("http://nexj.example.com/car/");

      schema.addItem(materialsType);
      schema.addItem(interiorElement);
      schema.addItem(exteriorElement);
      schema.addItem(accessoriesElement);
      schema.addItem(build);
      schema.addItem(buildResponse);

      carType.addChild(interiorElement);
      carType.addChild(exteriorElement);
      carType.addChild(accessoriesElement);
      carType.setDescription("Describes an entire car");
      carType.setSchema(schema);
      buildResult.setType(carType);
      buildResult.setDescription("Element inside the build response");

      seatCoveringEnum.addValue("Leather");
      seatCoveringEnum.addValue("Cloth");
      seatCoveringEnum.setDescription("Manufacturer's seat options");
      seatCoveringElement.setDescription("The material used to cover a seat");
      seatCoveringElement.setType(seatCoveringEnum);

      seatAttr.setDescription("Whether a seat has a headrest");
      seatAttr.setType(PrimitiveType.BOOLEAN);
      seatType.addChild(seatAttr);
      seatType.addChild(seatCoveringElement);
      seatType.setDescription("Represents a seat");
      seatElement.setDescription("A seat");
      seatElement.setMaxCount(9);
      seatElement.setType(seatType);

      interiorType.setBase(materialsType);
      interiorType.setDescription("Represents the interior of a car");
      interiorType.addChild(seatElement);
      interiorElement.setDescription("The interior");
      interiorElement.setType(interiorType);

      exteriorType.setBase(materialsType);
      exteriorType.setDescription("Represents the exterior of a car");
      exteriorElement.setDescription("The exterior");
      exteriorElement.setType(exteriorType);

      accessoriesType.setAggregation(CompositeType.AGGREGATION_RANDOM);
      accessoriesType.setDescription("Lists the accessories on a car");
      accessoriesElement.setDescription("The accessories");
      accessoriesElement.setNillable(true);
      accessoriesElement.setType(accessoriesType);

      colorEnum.addValue("Black");
      colorEnum.addValue("Red");
      colorEnum.setDescription("Enumeration of known colors");
      colorAttr.setDescription("The color of the material");
      colorAttr.setRequired(true);
      colorAttr.setType(colorEnum);
      materialsType.addChild(colorAttr);
      materialsType.setAbstract(true);
      materialsType.setDescription("Abstract base type for things with a color");
      materialsType.setSchema(schema);

      buildResponseType.addChild(buildResult);
      buildResponseType.setDescription("The response type for the build method");
      buildResponse.setDescription("The output root");
      buildResponse.setType(buildResponseType);
      buildResponse.setSchema(schema);

      buildType.addChild(interiorElement);
      buildType.addChild(exteriorElement);
      buildType.addChild(accessoriesElement);
      buildType.setSchema(schema);
      build.setDescription("The input root");
      build.setType(buildType);
      build.setSchema(schema);

      SOAPService service = new SOAPService("Car Service");
      Operation buildOperation = new Operation("Build");
      Message buildInput = new Message("Input");
      Message buildOutput = new Message("Output");

      buildInput.setRoot(build);
      buildOutput.setRoot(buildResponse);
      buildOperation.setInput(buildInput);
      buildOperation.setOutput(buildOutput);
      service.addMessage(buildInput);
      service.addMessage(buildOutput);
      service.addOperation(buildOperation);
      service.getUniverse().addSchema(schema);

      Definition def = ServiceSOAConverter.convert(service);

      ModelType materialsSOAType = def.findType("materialsType");
      ModelType exteriorSOAType = def.findType("anonymous3");
      ModelType interiorSOAType = def.findType("anonymous");
      ModelType accessoriesSOAType = def.findType("anonymous4");
      ModelType seatSOAType = def.findType("anonymous2");

      assertNotNull(materialsSOAType);
      assertNotNull(accessoriesSOAType);
      assertEquals("Lists the accessories on a car", accessoriesSOAType.getDescription());
      assertNotNull(exteriorSOAType);
      assertNotNull(interiorSOAType);
      assertEquals(1, interiorSOAType.getAttributeCount());
      nexj.core.meta.soa.Attribute seatSOAAttr = (nexj.core.meta.soa.Attribute)interiorSOAType.getAttributeIterator().next();
      assertEquals("Seat", seatSOAAttr.getName());
      assertTrue(seatSOAAttr.isCollection());
      assertTrue(seatSOAAttr.isRequired());
      assertNotNull(seatSOAType);
      assertEquals(2, seatSOAType.getAttributeCount());
      assertEquals("SeatCovering", ((nexj.core.meta.soa.Attribute)seatSOAType.getAttributeIterator().next()).getName());

      nexj.core.meta.soa.EnumType seatCoveringTypeSOAEnum = def.findEnum("anonymous");
      nexj.core.meta.soa.EnumType colorSOAEnum = def.findEnum("colorEnum");

      assertNotNull(colorSOAEnum);
      assertEquals(2, colorSOAEnum.getItemCount());
      assertEquals("Enumeration of known colors", colorSOAEnum.getDescription());
      assertNotNull(seatCoveringTypeSOAEnum);
      assertEquals(2, seatCoveringTypeSOAEnum.getItemCount());
      assertEquals("Manufacturer's seat options", seatCoveringTypeSOAEnum.getDescription());
   }

   public void testReturnTypes() throws Exception
   {
      Schema schema = new Schema("http://nexj.example.com/returnType");
      Element input = new Element("Input");
      CompositeType inputType = new CompositeType(null);
      input.setType(inputType);
      schema.addItem(input);

      SOAPService service = new SOAPService("MethodResponseTest");
      service.getUniverse().addSchema(schema);

      Operation buildOperationA = new Operation("BuildA");
      Message buildInputA = new Message("InputA");
      Message buildOutputA = new Message("OutputA");
      Element outputA = new Element("OutputInteger");

      outputA.setType(PrimitiveType.INTEGER);
      schema.addItem(outputA);
      buildInputA.setRoot(input);
      buildOutputA.setRoot(outputA);
      buildOperationA.setInput(buildInputA);
      buildOperationA.setOutput(buildOutputA);
      service.addMessage(buildInputA);
      service.addMessage(buildOutputA);
      service.addOperation(buildOperationA);

      Operation buildOperationB = new Operation("BuildB");
      Message buildInputB = new Message("InputB");
      Message buildOutputB = new Message("OutputB");
      Element outputB = new Element("OutputSingleElementPrimitive");
      CompositeType outputTypeB = new CompositeType(null);
      Element outputResponseB = new Element("OutputResponseB");

      outputResponseB.setType(PrimitiveType.BOOLEAN);
      outputTypeB.addChild(outputResponseB);
      outputB.setType(outputTypeB);
      schema.addItem(outputB);
      buildInputB.setRoot(input);
      buildOutputB.setRoot(outputB);
      buildOperationB.setInput(buildInputB);
      buildOperationB.setOutput(buildOutputB);
      service.addMessage(buildInputB);
      service.addMessage(buildOutputB);
      service.addOperation(buildOperationB);

      Operation buildOperationC = new Operation("BuildC");
      Message buildInputC = new Message("InputC");
      Message buildOutputC = new Message("OutputC");
      Element outputC = new Element("OutputSingleElementType");
      CompositeType outputTypeC = new CompositeType(null);
      Element outputResponseC = new Element("OutputResponseC");
      CompositeType outputResponseTypeC = new CompositeType("OutputResponseTypeC");

      outputResponseTypeC.setSchema(schema);
      outputResponseC.setType(outputResponseTypeC);
      outputTypeC.addChild(outputResponseC);
      outputC.setType(outputTypeC);
      schema.addItem(outputResponseTypeC);
      schema.addItem(outputResponseC);
      schema.addItem(outputC);
      buildInputC.setRoot(input);
      buildOutputC.setRoot(outputC);
      buildOperationC.setInput(buildInputC);
      buildOperationC.setOutput(buildOutputC);
      service.addMessage(buildInputC);
      service.addMessage(buildOutputC);
      service.addOperation(buildOperationC);

      Operation buildOperationD = new Operation("BuildD");
      Message buildInputD = new Message("InputD");
      Message buildOutputD = new Message("OutputD");
      Element outputD = new Element("OutputMultipleElement");
      CompositeType outputTypeD = new CompositeType(null);
      Element outputResponseD1 = new Element("OutputResponseD1");
      Element outputResponseD2 = new Element("OutputResponseD2");

      outputResponseD1.setType(PrimitiveType.STRING);
      outputResponseD2.setType(PrimitiveType.INTEGER);
      outputTypeD.addChild(outputResponseD1);
      outputTypeD.addChild(outputResponseD2);
      outputD.setType(outputTypeD);
      schema.addItem(outputD);
      buildInputD.setRoot(input);
      buildOutputD.setRoot(outputD);
      buildOperationD.setInput(buildInputD);
      buildOperationD.setOutput(buildOutputD);
      service.addMessage(buildInputD);
      service.addMessage(buildOutputD);
      service.addOperation(buildOperationD);

      Operation buildOperationE = new Operation("BuildE");
      Message buildInputE = new Message("InputE");
      Message buildOutputE = new Message("OutputE");
      Element outputE = new Element("OutputElementWithAttr");
      CompositeType outputTypeE = new CompositeType(null);
      Element outputResponseE = new Element("OutputResponseE1");
      Attribute outputResponseAttrE = new Attribute("OutputResponseAttrE");

      outputResponseAttrE.setType(PrimitiveType.INTEGER);
      outputResponseE.setType(PrimitiveType.STRING);
      outputTypeE.addChild(outputResponseAttrE);
      outputTypeE.addChild(outputResponseE);
      outputE.setType(outputTypeE);
      schema.addItem(outputE);
      buildInputE.setRoot(input);
      buildOutputE.setRoot(outputE);
      buildOperationE.setInput(buildInputE);
      buildOperationE.setOutput(buildOutputE);
      service.addMessage(buildInputE);
      service.addMessage(buildOutputE);
      service.addOperation(buildOperationE);

      Operation buildOperationF = new Operation("BuildF");
      Message buildInputF = new Message("InputF");
      Message buildOutputF = new Message("OutputF");
      Element outputF = new Element("OutputFlementWithAttr");
      CompositeType outputTypeF = new CompositeType(null);
      Element outputResponseF = new Element("OutputResponseF1");
      CompositeType outputResponseTypeF = new CompositeType(null);
      Attribute outputResponseAttrF = new Attribute("OutputResponseAttrF");

      outputResponseAttrF.setType(PrimitiveType.DOUBLE);
      outputResponseTypeF.addChild(outputResponseAttrF);
      outputResponseF.setType(outputResponseTypeF);
      outputTypeF.addChild(outputResponseF);
      outputF.setType(outputTypeF);
      schema.addItem(outputF);
      buildInputF.setRoot(input);
      buildOutputF.setRoot(outputF);
      buildOperationF.setInput(buildInputF);
      buildOperationF.setOutput(buildOutputF);
      service.addMessage(buildInputF);
      service.addMessage(buildOutputF);
      service.addOperation(buildOperationF);

      Definition def = ServiceSOAConverter.convert(service);

      assertEquals(1, def.getDefinedInterfaceCount());

      Interface iface = (Interface)def.getDefinedInterfaceIterator().next();

      Method methodA = iface.getMethod("BuildA", 0);
      Result resultA  = methodA.getResult();
      assertEquals("integer", resultA.getType().toString());

      Method methodB = iface.getMethod("BuildB", 0);
      Result resultB  = methodB.getResult();
      assertEquals("boolean", resultB.getType().toString());

      Method methodC = iface.getMethod("BuildC", 0);
      Result resultC  = methodC.getResult();
      assertEquals("OutputResponseTypeC", resultC.getType().toString());

      Method methodD = iface.getMethod("BuildD", 0);
      Result resultD  = methodD.getResult();
      assertEquals("anonymous", resultD.getType().toString());
      ModelType anonymous = def.findType("anonymous");
      assertNotNull(anonymous);
      assertEquals(2, anonymous.getAttributeCount());
      nexj.core.meta.soa.Attribute attrD1 = anonymous.findAttribute("OutputResponseD1");
      nexj.core.meta.soa.Attribute attrD2 = anonymous.findAttribute("OutputResponseD2");
      assertNotNull(attrD1);
      assertEquals("string", attrD1.getType().toString());
      assertNotNull(attrD2);
      assertEquals("integer", attrD2.getType().toString());

      Method methodE = iface.getMethod("BuildE", 0);
      Result resultE  = methodE.getResult();
      assertEquals("anonymous2", resultE.getType().toString());
      ModelType anonymous2 = def.findType("anonymous2");
      assertNotNull(anonymous2);
      assertEquals(2, anonymous2.getAttributeCount());
      nexj.core.meta.soa.Attribute attrE1 = anonymous2.findAttribute("OutputResponseE1");
      nexj.core.meta.soa.Attribute attrE2 = anonymous2.findAttribute("OutputResponseAttrE");
      assertNotNull(attrE1);
      assertEquals("string", attrD1.getType().toString());
      assertNotNull(attrE2);
      assertEquals("integer", attrE2.getType().toString());

      Method methodF = iface.getMethod("BuildF", 0);
      Result resultF  = methodF.getResult();
      assertEquals("anonymous3", resultF.getType().toString());
      ModelType anonymous3 = def.findType("anonymous3");
      assertNotNull(anonymous3);
      assertEquals(1, anonymous3.getAttributeCount());
      nexj.core.meta.soa.Attribute attrF1 = anonymous3.findAttribute("OutputResponseAttrF");
      assertNotNull(attrF1);
      assertEquals("double", attrF1.getType().toString());

   }
}