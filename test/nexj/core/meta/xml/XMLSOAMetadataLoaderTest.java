// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.xml;

import java.io.StringReader;
import java.util.List;
import java.util.Properties;

import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLoaderDispatcher;
import nexj.core.meta.MetadataLoadingFailureTest;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.SOAClient;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.SchemeParser;
import nexj.core.scripting.ScriptingError;
import nexj.core.scripting.Symbol;
import nexj.core.scripting.object.ClassObject;
import nexj.core.scripting.object.ObjectException;
import nexj.core.scripting.object.ObjectOriented;
import nexj.core.util.Lookup;

import junit.framework.TestCase;

/**
 * Tests the XMLSOAMetadataExporter
 */
public class XMLSOAMetadataLoaderTest extends TestCase
{
   // associations

   /**
    * The invocation context, new for each test.
    */
   protected InvocationContext m_context;

   // operations

   /**
    * @see junit.framework.TestCase#setUp()
    */
   public void setUp()
   {
      m_context = (InvocationContext)Repository.getMetadata().getComponent("System.InvocationContext").getInstance(null);
      m_context.initialize(null);
   }

   /**
    * Verifies that service definitions are loaded.
    */
   public void testServiceExists()
   {
      GlobalEnvironment env = m_context.getMetadata().getGlobalEnvironment();
      ClassObject perfServiceClass = env.findClass(Symbol.define("nexj:example:performance:1.0"));

      assertNotNull(perfServiceClass);
      assertEquals(1, perfServiceClass.getBaseCount());
      assertEquals(env.findClass(Symbol.define("soa:ServiceObject")), perfServiceClass.getBase(0));

      // Verify it again for "mod1:"
      perfServiceClass = env.findClass(Symbol.define("mod1:nexj:example:performance:1.0"));
      assertNotNull(perfServiceClass);
      assertEquals(1, perfServiceClass.getBaseCount());
      assertEquals(env.findClass(Symbol.define("soa:ServiceObject")), perfServiceClass.getBase(0));
   }

   /**
    * Verifies that a method on a service can be invoked.
    */
   public void testInvokeMethod()
   {
      SOAClient client = new SOAClient("nexj:example:performance:1.0", m_context);
      List counterList = (List)client.invoke("getCounters", "srv1");

      assertEquals(0, counterList.size());

      // Verify it again for "mod1:"
      client = new SOAClient("mod1:nexj:example:performance:1.0", m_context);
      counterList = (List)client.invoke("getCounters", "srv1");

      assertEquals(1, counterList.size());
   }

   /**
    * Verifies that state is maintained implicitly for stateful methods.
    */
   public void testPrimitiveStateImplicit()
   {
      SOAClient client = new SOAClient("nexj:example:performance:1.0", m_context);

      assertEquals(Symbol.define("tartare"), client.invoke("next1"));
      assertEquals(Symbol.define("blue"), client.invoke("next1"));
      client.invoke("next1");
      assertEquals(Symbol.define("medium-rare"), client.invoke("next1"));
      client.invoke("next1");
      client.invoke("next1");
      client.invoke("next1");
      assertEquals(Symbol.define("charred"), client.invoke("next1"));
      assertEquals(Symbol.define("charred"), client.invoke("next1"));
      assertEquals(Symbol.define("charred"), client.invoke("next1"));
   }

   /**
    * Verifies that state can be passed explicitly for stateful methods when the proxy is configured with
    * "explicit-state" true.
    */
   public void testPrimitiveStateExplicit()
   {
      Pair properties = parse("((explicit-state . #t))");
      SOAClient client = new SOAClient("nexj:example:performance:1.0", properties, m_context);
      Integer state = null;
      Object[] resultArray;

      resultArray = client.invokeMultiValue("next1", state);
      assertEquals(Symbol.define("tartare"), resultArray[0]);
      state = (Integer)resultArray[1];
      assertEquals(0, state.intValue());

      resultArray = client.invokeMultiValue("next1", state);
      assertEquals(Symbol.define("blue"), resultArray[0]);
      state = (Integer)resultArray[1];
      assertEquals(1, state.intValue());

      resultArray = client.invokeMultiValue("next1", state);
      state = (Integer)resultArray[1];

      resultArray = client.invokeMultiValue("next1", state);
      assertEquals(Symbol.define("medium-rare"), resultArray[0]);
      state = (Integer)resultArray[1];

      resultArray = client.invokeMultiValue("next1", state);
      state = (Integer)resultArray[1];
      resultArray = client.invokeMultiValue("next1", state);
      state = (Integer)resultArray[1];
      resultArray = client.invokeMultiValue("next1", state);
      state = (Integer)resultArray[1];
      resultArray = client.invokeMultiValue("next1", state);
      assertEquals(Symbol.define("charred"), resultArray[0]);
      state = (Integer)resultArray[1];
      resultArray = client.invokeMultiValue("next1", state);
      assertEquals(Symbol.define("charred"), resultArray[0]);
      state = (Integer)resultArray[1];
      resultArray = client.invokeMultiValue("next1", state);
      assertEquals(Symbol.define("charred"), resultArray[0]);
   }

   /**
    * Verifies that state cannot be set to value of wrong type.
    */
   public void testStateTypeError()
   {
      SOAClient client = new SOAClient("nexj:example:performance:1.0", m_context);

      assertEquals(Symbol.define("zero"), client.invoke("next2"));
      assertEquals(Symbol.define("one"), client.invoke("next2"));
      assertEquals(Symbol.define("two"), client.invoke("next2"));

      try
      {
         client.invoke("next2");
         fail("Expected ScriptingError");
      }
      catch (ScriptingError ex)
      {
         assertEquals("fail.soa.stateTypeMismatch", ex.getErrorCode());
      }

      // Repeat test for the implementation in "mod1:"
      client = new SOAClient("mod1:nexj:example:performance:1.0", m_context);

      assertEquals(Symbol.define("zero"), client.invoke("next2"));
      assertEquals(Symbol.define("one"), client.invoke("next2"));
      assertEquals(Symbol.define("two"), client.invoke("next2"));

      try
      {
         client.invoke("next2");
         fail("Expected ScriptingError");
      }
      catch (ScriptingError ex)
      {
         assertEquals("fail.soa.stateTypeMismatch", ex.getErrorCode());
      }
   }

   /**
    * Verifies that an error is generated when state parameter is missing.
    */
   public void testExplitStateMustBePassed()
   {
      Pair properties = parse("((explicit-state . #t))");
      SOAClient client = new SOAClient("nexj:example:performance:1.0", properties, m_context);

      try
      {
         assertEquals(Symbol.define("tartare"), client.invoke("next1"));
         fail("Expected 'missing state' error");
      }
      catch (ScriptingError ex)
      {
      }

      try
      {
         client.invoke("getCounters", "bleh");
         fail();
      }
      catch (ScriptingError ex)
      {
      }
   }

   /**
    * Tests that return values are suppressed when a method is declared void. 
    */
   public void testVoidReturnValue()
   {
      SOAClient client = new SOAClient("nexj:example:performance:1.0", m_context);
      Object retVal = client.invoke("voidReturnType");

      assertEquals(null, retVal);
   }
   
   /**
    * Tests that a metadata loading error happens when an unknown global type is referenced.
    */
   public void testReferenceToUndefinedGlobalType()
   {
      try
      {
         Properties properties = new Properties();

         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, MetadataLoadingFailureTest.BASE_URL);
         properties.setProperty("soa.test01", "true");
         new MetadataLoaderDispatcher().load(MetadataLoadingFailureTest.META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail("Expected MetadataException");
      }
      catch (MetadataException e)
      {
         e = MetadataLoadingFailureTest.extractException(e);
         assertEquals("err.meta.soa.typeLookup", e.getErrorCode());
         assertEquals("nexj:test:Undefined:1.0:type:Undefined", e.getErrorArgs()[0]);
         assertEquals("nexj:test:UndefinedGlobalTypeReference:1.0:interface:main", e.getErrorArgs()[1]);
      }
   }

   /**
    * Tests that a metadata loading error happens when an unknown local type is referenced.
    */
   public void testReferenceToUndefinedLocalType()
   {
      try
      {
         Properties properties = new Properties();

         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, MetadataLoadingFailureTest.BASE_URL);
         properties.setProperty("soa.test02", "true");
         new MetadataLoaderDispatcher().load(MetadataLoadingFailureTest.META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail("Expected MetadataException");
      }
      catch (MetadataException e)
      {
         e = MetadataLoadingFailureTest.extractException(e);
         assertEquals("err.meta.soa.typeLookup", e.getErrorCode());
         assertEquals("Undefined", e.getErrorArgs()[0]);
         assertEquals("nexj:test:UndefinedLocalTypeReference:1.0:interface:main", e.getErrorArgs()[1]);
      }
   }

   /**
    * Tests that only fault types can be used as the fault of a method.
    */
   public void testFaultReferenceToType()
   {
      try
      {
         Properties properties = new Properties();

         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, MetadataLoadingFailureTest.BASE_URL);
         properties.setProperty("soa.test03", "true");
         new MetadataLoaderDispatcher().load(MetadataLoadingFailureTest.META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail("Expected MetadataException");
      }
      catch (MetadataException e)
      {
         e = MetadataLoadingFailureTest.extractException(e);
         assertEquals("err.meta.soa.notFault", e.getErrorCode());
         assertEquals("NotAFault", e.getErrorArgs()[0]);
         assertEquals("test", e.getErrorArgs()[1]);
         assertEquals(Primitive.ONE_INTEGER, e.getErrorArgs()[2]);
         assertEquals("Test", e.getErrorArgs()[3]);
         assertEquals("nexj:test:FaultReferenceToType:1.0", e.getErrorArgs()[4]);
      }
   }

   /**
    * Tests that a fault may not have a type as its base.
    */
   public void testFaultWithTypeBase()
   {
      try
      {
         Properties properties = new Properties();

         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, MetadataLoadingFailureTest.BASE_URL);
         properties.setProperty("soa.test04", "true");
         new MetadataLoaderDispatcher().load(MetadataLoadingFailureTest.META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail("Expected MetadataException");
      }
      catch (MetadataException e)
      {
         e = MetadataLoadingFailureTest.extractException(e);
         assertEquals("err.meta.soa.typeBase", e.getErrorCode());
         assertEquals("Fault1", e.getErrorArgs()[0]);
      }
   }

   /**
    * Tests that a type may not have a fault as its base.
    */
   public void testTypeWithFaultBase()
   {
      try
      {
         Properties properties = new Properties();

         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, MetadataLoadingFailureTest.BASE_URL);
         properties.setProperty("soa.test05", "true");
         new MetadataLoaderDispatcher().load(MetadataLoadingFailureTest.META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail("Expected MetadataException");
      }
      catch (MetadataException e)
      {
         e = MetadataLoadingFailureTest.extractException(e);
         assertEquals("err.meta.soa.faultBase", e.getErrorCode());
         assertEquals("Type1", e.getErrorArgs()[0]);
      }
   }

   /**
    * Tests that the properties specified on a SOA connection are loaded.
    */
   public void testSOAConnectionProperty()
   {
      Machine machine = m_context.getMachine();
      GlobalEnvironment env = Repository.getMetadata().getGlobalEnvironment();
      Lookup localRegistryMap = (Lookup)env.getVariable(XMLSOAMetadataLoader.SOA_LOCAL_REGISTRY);
      ObjectOriented srvInfo = (ObjectOriented)((Lookup)localRegistryMap.get("nexj:example:performance:1.0")).get(null);
      List propList = (List)machine.invoke(srvInfo, XMLSOAMetadataLoader.PROPERTIES, (Object[])null);

      assertEquals(1, propList.size());

      ObjectOriented property = (ObjectOriented)propList.get(0);

      assertEquals("test1", machine.invoke(property, Symbol.NAME, (Object[])null));
      assertEquals(Primitive.createInteger(42), machine.invoke(property, Symbol.VALUE, (Object[])null));
   }

   /**
    * Tests that creating a proxy to an unknown instance raises an error.
    */
   public void testErrorUnknownInstance()
   {
      Pair properties = parse("((instance . \"unknown\"))");

      try
      {
         new SOAClient("nexj:example:performance:1.0", properties, m_context);
         fail("Expected ScriptingError");
      }
      catch (ScriptingError ex)
      {
         assertEquals("fail.soa.serviceInstanceLookup", ex.getErrorCode());
      }
   }

   /**
    * Tests that system exceptions are not propagated to the consumer.
    */
   public void testSystemException()
   {
      SOAClient client = new SOAClient("nexj:example:performance:1.0", m_context);

      try
      {
         client.invoke("error", new Object[]{"system.err", null});
         fail("Expected ObjectException");
      }
      catch (ObjectException ex)
      {
         // Verify that information is not leaked
         assertSystemException(ex);
      }

      try
      {
         client.invoke("error", new Object[]{"system.fail", null});
         fail("Expected ObjectException");
      }
      catch (ObjectException ex)
      {
         // Verify that information is not leaked
         assertSystemException(ex);
      }
   }

   /**
    * Tests that undeclared faults are not propagated to the consumer.
    */
   public void testUndeclaredFault()
   {
      SOAClient client = new SOAClient("nexj:example:performance:1.0", m_context);

      try
      {
         client.invoke("error", new Object[]{"fault.err", null});
         fail("Expected ObjectException");
      }
      catch (ObjectException ex)
      {
         // Verify that information is not leaked
         assertSystemException(ex);
      }

      // Repeat test for "mod1:"
      client = new SOAClient("mod1:nexj:example:performance:1.0", m_context);

      try
      {
         client.invoke("error", new Object[]{"fault.err", null});
         fail("Expected ObjectException");
      }
      catch (ObjectException ex)
      {
         // Verify that information is not leaked
         assertSystemException(ex);
      }
   }

   /**
    * Tests that two different soadefs cannot have the same name and version.
    */
   public void testDuplicateSOADefinition()
   {
      try
      {
         Properties properties = new Properties();

         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, MetadataLoadingFailureTest.BASE_URL);
         properties.setProperty("soa.test06", "true");
         new MetadataLoaderDispatcher().load(MetadataLoadingFailureTest.META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail("Expected MetadataException");
      }
      catch (MetadataException e)
      {
         e = MetadataLoadingFailureTest.extractException(e);
         assertEquals("err.meta.soa.definitionDup", e.getErrorCode());
         assertEquals("nexj:soa:Registry", e.getErrorArgs()[0]);
         assertEquals("1.0", e.getErrorArgs()[1]);
      }
   }

   protected static void assertSystemException(ObjectException ex)
   {
      assertEquals("soa:SystemFault", ex.getClassObject().getName());
      assertEquals("err.rpc.reference", ex.getErrorCode());
      assertEquals(1, ex.getErrorArgs().length);
      assertTrue(ex.getErrorArgs()[0] instanceof String);
   }

   /**
    * Tests that declared faults do get propagated to the consumer.
    */
   public void testDeclaredFault()
   {
      SOAClient client = new SOAClient("nexj:example:performance:1.0", m_context);

      try
      {
         client.invoke("error", new Object[]{"userfault.err", null});
         fail("Expected ObjectException");
      }
      catch (ObjectException ex)
      {
         assertEquals("nexj:example:performance:1.0:type:UserError1", ex.getClassObject().getName());
         assertEquals("err.soa.test", ex.getErrorCode());
         assertTrue(ex.getErrorArgs() == null || ex.getErrorArgs().length == 0);
         assertEquals("hello", ex.getValue("data"));
      }

      try
      {
         client.invoke("error", new Object[]{"userfault.fail", null});
         fail("Expected ObjectException");
      }
      catch (ObjectException ex)
      {
         assertEquals("nexj:example:performance:1.0:type:UserError1", ex.getClassObject().getName());
         assertEquals("err.soa.test", ex.getErrorCode());
         assertTrue(ex.getErrorArgs() == null || ex.getErrorArgs().length == 0);
         assertEquals("hi", ex.getValue("data"));
      }

      // Repeat test for "mod1:"
      client = new SOAClient("mod1:nexj:example:performance:1.0", m_context);

      try
      {
         client.invoke("error", new Object[]{"userfault.err", null});
         fail("Expected ObjectException");
      }
      catch (ObjectException ex)
      {
         assertEquals("mod1:nexj:example:performance:1.0:type:UserError1", ex.getClassObject().getName());
         assertEquals("err.soa.test", ex.getErrorCode());
         assertTrue(ex.getErrorArgs() == null || ex.getErrorArgs().length == 0);
         assertEquals("hello", ex.getValue("data"));
      }

      try
      {
         client.invoke("error", new Object[]{"userfault.fail", null});
         fail("Expected ObjectException");
      }
      catch (ObjectException ex)
      {
         assertEquals("mod1:nexj:example:performance:1.0:type:UserError1", ex.getClassObject().getName());
         assertEquals("err.soa.test", ex.getErrorCode());
         assertTrue(ex.getErrorArgs() == null || ex.getErrorArgs().length == 0);
         assertEquals("hi", ex.getValue("data"));
      }
   }

   /**
    * Parses the string as a scheme expression.
    * @param sExpr The string expression.
    * @return The parse result.
    */
   protected Pair parse(String sExpr)
   {
      return (Pair)new SchemeParser(m_context.getMachine().getGlobalEnvironment())
         .parse(new StringReader(sExpr), null);
   }
}
