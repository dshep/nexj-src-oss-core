package nexj.core.meta.testing.unit;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.StringTokenizer;

import org.w3c.dom.Element;

import nexj.core.meta.MetadataException;
import nexj.core.meta.xml.XMLMetadata;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.IOUtil;
import nexj.core.util.URLUtil;
import nexj.core.util.XMLUtil;
import nexj.core.util.XMLUtil.ElementHandler;

/**
 * The loader for Unit test metadata
 */
public class XMLUnitTestMetadataLoader
{
   // constants

   /**
    * The name of the resource with global definitions for loading before the tests.
    */
   protected final static String INITIAL_SCRIPT = "unittest.scm";

   /**
    * The resource URL of the initial script
    */
   protected final static String INITIAL_SCRIPT_URL= "syslibrary:unittest";

   //associations

   /**
    * The XML metadata.
    */
   protected XMLMetadata m_metadata;

   /**
    * The XML metadata helper.
    */
   protected XMLMetadataHelper m_helper;

   /**
    * Global environment for compilation.
    */
   protected GlobalEnvironment m_env;

   /**
    * The VM for compilation.
    */
   protected Machine m_machine;

   /**
    * Constructs the loader.
    * @param metadata The XML metadata root object.
    * @param helper The XML metadata helper.
    */
   public XMLUnitTestMetadataLoader(XMLMetadata metadata, XMLMetadataHelper helper)
   {
      m_metadata = metadata;
      m_helper = helper;
      m_env = new GlobalEnvironment(m_metadata.getGlobalEnvironment());
      m_machine = new Machine(m_env, (InvocationContext)null);

      InputStream istream = null;
      Reader reader = null;

      try
      {
         istream = URLUtil.openResource(getClass(), INITIAL_SCRIPT);
         reader = IOUtil.openBufferedReader(istream, XMLUtil.ENCODING);
         Intrinsic.load(reader, INITIAL_SCRIPT_URL, m_machine);
      }
      catch (IOException e)
      {
         throw new MetadataException("err.meta.resourceOpen", new Object[]{INITIAL_SCRIPT}, e);
      }
      finally
      {
         IOUtil.close(reader);
         IOUtil.close(istream);
      }
   }

   /**
    * Loads a unit test from a DOM element.
    * @param sName The unit test name.
    * @param testElement The DOM element.
    * @return The UnitTest metadata object.
    */
   public UnitTest unitTest(String sName, final Element testElement) throws MetadataException
   {
      XMLMetadataHelper.verifyRootElement(testElement, "UnitTest");
      final UnitTest uTest = new UnitTest(sName);
      String sVarList = XMLUtil.getStringAttr(testElement, "variables");

      if (sVarList != null)
      {
         Pair variables = null;

         for (StringTokenizer tokenizer = new StringTokenizer(sVarList); tokenizer.hasMoreTokens();)
         {
            String sVarName = tokenizer.nextToken();
            XMLMetadataHelper.validateName(sVarName);
            variables = Pair.append(variables, new Pair(Symbol.define(sVarName)));
         }

         uTest.setVariables(variables);
      }

      uTest.setDump(XMLUtil.getReqStringAttr(testElement, "dump"));

      String sMode = XMLUtil.getStringAttr(testElement, "mode");

      if (sMode != null)
      {
         if (sMode.equals("sequential"))
         {
            uTest.setMode(UnitTest.MODE_SEQUENTIAL);
         }
         else if (sMode.equals("dirty"))
         {
            uTest.setMode(UnitTest.MODE_DIRTY);
         }
         else
         {
            throw new MetadataException("err.meta.invalidUnitTestMode",
               new Object[] {sMode});
         }
      }

      Object dumpResource = m_metadata.getDumpResourceMap().get(uTest.getDump());

      if (dumpResource == null)
      {
         throw new MetadataException("err.meta.invalidUnitTestDump",
            new Object[] {uTest.getDump()});
      }

      uTest.setDumpURL(m_helper.getResource(dumpResource.toString()).getURL());

      XMLUtil.withFirstChildElement(testElement, "Initializer", false, new ElementHandler()
      {
         public void handleElement(Element element)
         {
            uTest.setInitializer((Pair)m_helper.parse(m_helper.getElementValue(element),
               true, uTest.getPosMap(), null, m_metadata.getGlobalEnvironment()));
         }
      });

      XMLUtil.forEachChildElement(testElement, "TestCase", m_helper.new ElementHandler("testCase")
      {
         public void handleElement(Element element, String sName)
         {
            loadTestCase(element, uTest, sName);
         }
      });

      XMLUtil.withFirstChildElement(testElement, "Finalizer", false, new ElementHandler()
      {
         public void handleElement(Element element)
         {
            uTest.setFinalizer((Pair)m_helper.parse(m_helper.getElementValue(element),
               true, uTest.getPosMap(), null, m_metadata.getGlobalEnvironment()));
         }
      });

      XMLUtil.withFirstChildElement(testElement, "Loops", false, new ElementHandler()
      {
         public void handleElement(Element element)
         {
            XMLUtil.forEachChildElement(element, "Loop", new ElementHandler()
            {
               public void handleElement(Element element)
               {
                  String sType = XMLUtil.getStringAttr(element, "type", "list");
                  boolean bList;

                  if (sType.equals("list"))
                  {
                     bList = true;
                  }
                  else if (sType.equals("expression"))
                  {
                     bList = false;
                  }
                  else
                  {
                     throw new MetadataException("err.meta.invalidUnitTestLoopType", new Object[] {sType});
                  }

                  Pair value = (Pair)m_helper.parse(XMLUtil.getReqStringAttr(element, "value"),
                     bList, uTest.getPosMap(), null, m_metadata.getGlobalEnvironment());

                  uTest.addLoop(XMLUtil.getReqStringAttr(element, "variable"), value, bList);
               }
            });
         }
      });

      uTest.compile(m_machine);
      return uTest;
   }

   /**
    * Loads a test case from a DOM element.
    * @param element The DOM element containing the test case.
    * @param unitTest The unit test metadata object.
    */
   protected void loadTestCase(Element element, final UnitTest unitTest, String sName)
   {
      UnitTestCase testCase = new UnitTestCase();

      testCase.setBody((Pair)m_helper.parse(m_helper.getElementValue(element)+"()",
         true, unitTest.getPosMap(), null, m_metadata.getGlobalEnvironment()));

      testCase.setName(sName);
      //XMLUtil.getReqStringAttr(element, "name")
      unitTest.addUnitTestCase(testCase);
   }
}
