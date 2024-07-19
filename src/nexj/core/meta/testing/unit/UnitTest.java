package nexj.core.meta.testing.unit;

import java.net.URL;
import java.util.Iterator;

import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.NamedMetadataObject;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.CompilerException;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.Lookup;
import nexj.core.util.TextPosition;

/**
 * Unit test metadata object, containing a collection of unit test cases.
 * 
 * Unit tests may be parameterized by defining unit test loops. A loop assigns that loop's variable
 * to each of the loop's values in turn and then runs the unit test. Loops may be nested, with
 * previous loop variables in scope for subsequent loop value calculations.
 */
public class UnitTest extends NamedMetadataObject
{
   // constants

   /**
    * Sequential run mode.
    */
   public final static byte MODE_SEQUENTIAL = 0;

   /**
    * Dirty run mode.  Database resetting does not occur between each test case run.
    */
   public final static byte MODE_DIRTY = 1;

   /**
    * Object used as existence marker of initializer/finalizer after compile.
    */
   protected final static Pair EMPTY = new Pair(null);

   /**
    * The transformation code URL prefix.
    */
   protected final static String URL_PREFIX = "unittest:";
   
   // attributes
   
   /**
    * The dump file for importing database.
    */
   protected String m_sDump;

   /**
    * The number of variables (available after compilation).
    */
   protected int m_nAllVariableCount;

   /**
    * The number of loops.
    */
   protected int m_nLoopCount;

   /**
    * The unit test run mode.
    */
   protected byte m_nMode;

   // associations
   
   /**
    * The initializer script.
    */
   protected Pair m_initializer;
   
   /**
    * The finalizer script.
    */
   protected Pair m_finalizer;
   
   /**
    * The test case map: UnitTestCase[String].
    */
   protected Lookup m_unitTestCaseMap = new LinkedHashTab(8);
   
   /**
    * The expression position map.
    */
   protected Lookup m_posMap = new IdentityHashTab();
   
   /**
    * The code URL map.
    */
   protected Lookup m_urlMap = new IdentityHashTab();
   
   /**
    * The compiled function of the initializer, finalizer, and testcases from the
    * following format:
    * 
    * (lambda (loopVar1 loopVar2 ... loopVarN var1 var2 ... varN)
    *    (vector
    *       (lambda() initializer)
    *       (lambda() finalizer)
    *       (lambda() testcase1) ...
    *       (lambda() testcaseN)
    *    )
    * )
    */
   protected Function m_function;
   
   /**
    * The variables from the root UnitTest element.
    */
   protected Pair m_variables;

   /**
    * The loop variables and the variables from the root UnitTest element.
    */
   protected Pair m_allVariables;
   
   /**
    * The dump URL.
    */
   protected URL m_dumpURL;

   /**
    * Loop functions, one per loop, for generating the loop's collection. Functions take the
    * variables of the previous functions as arguments:
    * 
    * (lambda () body1)
    * (lambda (loopVar1) body2)
    * (lambda (loopVar1 loopVar2) body3)
    * (lambda (loopVar1 loopVar2 ... loopVarN-1) bodyN)
    */
   protected Function[] m_loopFunctionArray;

   /**
    * The loop definitions. Loop variable symbols are stored at even indices; code is stored at odd indices.
    */
   protected Object[] m_loopArray;

   // constructors

   /**
    * @param sName The name of the unit test.
    */
   public UnitTest(String sName)
   {
      this.setName(sName);
   }

   // operations
   
   /**
    * Sets the dump file for importing database.
    * @param sDump The dump file for importing database to set.
    */
   public void setDump(String sDump)
   {
      verifyNotReadOnly();
      m_sDump = sDump;
   }

   /**
    * @return The dump file for importing database.
    */
   public String getDump()
   {
      return m_sDump;
   }

   /**
    * Sets the dump URL.
    * @param dumpURL The dump URL to set.
    */
   public void setDumpURL(URL dumpURL)
   {
      verifyNotReadOnly();
      m_dumpURL = dumpURL;
   }

   /**
    * @return The dump URL.
    */
   public URL getDumpURL()
   {
      return m_dumpURL;
   }

   /**
    * Sets the run mode.
    * @param nMode The run mode.
    */
   public void setMode(byte nMode)
   {
      m_nMode = nMode;
   }
   
   /**
    * @return The run mode.
    */
   public byte getMode()
   {
      return m_nMode;
   }
   
   /**
    * Sets the variables.
    * @param variables The variables to set.
    */
   public void setVariables(Pair variables)
   {
      verifyNotReadOnly();
      m_variables = variables;
   }

   /**
    * Gets the variables.
    * 
    * @return The variables from the root UnitTest element.
    */
   public Pair getVariables()
   {
      return m_variables;
   }

   /**
    * @return The length of the AllVariables Pair tree after compilation.
    */
   public int getAllVariableCount()
   {
      return m_nAllVariableCount;
   }

   /**
    * Adds a loop to this unit test. The loops are nested: the first loop added by this method
    * will be the outermost loop; the last loop, the innermost.
    *
    * @param sVarName The loop variable name.
    * @param value The loop value (a list or an expression).
    * @param bList True if loop value is a list; false if it is an expression.
    */
   public void addLoop(String sVarName, Pair value, boolean bList)
   {
      verifyNotReadOnly();

      if (bList)
      {
         value = Pair.list(Symbol.QUOTE, value);
      }

      if (m_loopArray == null)
      {
         m_loopArray = new Object[2];
      }

      if (m_loopArray.length <= (m_nLoopCount << 1))
      {
         Object[] biggerArray = new Object[m_loopArray.length << 1];

         System.arraycopy(m_loopArray, 0, biggerArray, 0, m_nLoopCount << 1);
         m_loopArray = biggerArray;
      }

      m_loopArray[m_nLoopCount << 1] = Symbol.define(sVarName);
      m_loopArray[(m_nLoopCount << 1) + 1] = value;
      m_nLoopCount++;
   }

   /**
    * Gets the loop variable Symbol for the given loop.
    * 
    * @param nLoop The 0-based ordinal of the loop to get the symbol for, 0 being
    * the outermost loop.
    * @return The loop variable Symbol.
    */
   public Symbol getLoopVarSymbol(int nLoop)
   {
      return (Symbol)m_loopArray[nLoop << 1];
   }

   /**
    * Gets the loop variable expression for the given loop.
    * 
    * @param nLoop The 0-based ordinal of the loop to get the expression for, 0 being
    * the outermost loop.
    * @return The loop variable expression.
    */
   public Pair getLoopVarExpr(int nLoop)
   {
      return (Pair)m_loopArray[(nLoop << 1) + 1];
   }

   /**
    * Gets the function that generates the loop values for a given loop.
    * 
    * @param nLoop The 0-based ordinal of the loop to get the function for, 0 being
    * the outermost loop.
    * @return The value generation function.
    */
   public Function getLoopFunction(int nLoop)
   {
      return m_loopFunctionArray[nLoop];
   }

   /**
    * Gets the number of loops defined for this unit test.
    * 
    * @return The number of loops.
    */
   public int getLoopCount()
   {
      return m_nLoopCount;
   }

   /**
    * @return The expression position map.
    */
   public Lookup getPosMap()
   {
      return m_posMap;
   }

   /**
    * Sets the function.
    * @param function The function to set.
    */
   public void setFunction(Function function)
   {
      verifyNotReadOnly();
      m_function = function;
   }

   /**
    * @return The function.
    */
   public Function getFunction()
   {
      return m_function;
   }

   /**
    * Adds a new test case to the unit test.
    * @param unitTestCase The test case to add.
    * @throws MetadataException if a test case
    * with the same name already exists.
    */
   public void addUnitTestCase(UnitTestCase unitTestCase)
   {
      verifyNotReadOnly();

      Object oldUnitTestCase = m_unitTestCaseMap.put(unitTestCase.getName(), unitTestCase);

      if (oldUnitTestCase != null)
      {
         m_unitTestCaseMap.put(unitTestCase.getName(), oldUnitTestCase);

         throw new MetadataException("err.meta.testing.unit.testCaseDup", new Object[]
         {
            unitTestCase.getName(),
            getName()
         });
      }
   }

   /**
    * Gets a test case by name.
    * @param sName The unit test case name.
    * @return The unit test case object.
    * @throws MetadataLookupException if the unit test case does not exist.
    */
   public UnitTestCase getUnitTestCase(String sName)
   {
      UnitTestCase unitTestCase = (UnitTestCase) m_unitTestCaseMap.get(sName);

      if (unitTestCase != null)
      {
         return unitTestCase;
      }

      throw new MetadataLookupException("err.meta.testing.unit.testCaseLookup", sName, this);
   }

   /**
    * @return The unit test case count.
    */
   public int getUnitTestCaseCount()
   {
      return m_unitTestCaseMap.size();
   }

   /**
    * @return An iterator for the contained unit test case objects.
    */
   public Iterator getUnitTestCaseIterator()
   {
      return m_unitTestCaseMap.valueIterator();
   }
   
   /**
    * Sets the finalizer.
    * @param finalizer The finalizer to set.
    */
   public void setFinalizer(Pair finalizer)
   {
      verifyNotReadOnly();
      m_finalizer = finalizer;
   }

   /**
    * @return If a finalizer was used during compile().
    */
   public boolean hasFinalizer()
   {
      return m_finalizer != null;
   }

   /**
    * Sets the initializer.
    * @param initializer The initializer to set.
    */
   public void setInitializer(Pair initializer)
   {
      verifyNotReadOnly();
      m_initializer = initializer;
   }

   /**
    * @return If an initializer was used during compile().
    */
   public boolean hasInitializer()
   {
      return m_initializer != null;
   }

   /**
    * Sets the URL for a given compiled code.
    * @param code The code.
    * @param sName The last URL part.
    */
   protected void setPosURLs(Object code, String sName)
   {
      if (sName != null)
      {
         Compiler.setPosURLs(code, URL_PREFIX + getName() + "." + sName, m_posMap, m_urlMap);
      }
   }

   /**
    * Compiles this unit test.
    * 
    * @param machine The virtual machine.
    */
   public void compile(Machine machine)
   {
      Pair body = null;
      Pair last = null;

      for (Iterator itr = m_unitTestCaseMap.valueIterator(); itr.hasNext();)
      {
         UnitTestCase testCase = (UnitTestCase)itr.next();
         Pair code = testCase.getBody();

         if (code == null)
         {
            code = new Pair(null);
         }

         Pair pair = new Pair(new Pair(Symbol.LAMBDA, new Pair(null, code)));

         if (body == null)
         {
            body = last = pair;
         }
         else
         {
            last.setTail(pair);
         }

         last = pair;
         setPosURLs(testCase.getBody(), testCase.getName());
         testCase.setBody(null); // free memory not used after compile()
      }

      if (m_finalizer != null)
      {
         body = new Pair(new Pair(Symbol.LAMBDA, new Pair(null, m_finalizer)), body);
         setPosURLs(m_finalizer, "finalizer");
      }
      
      if (m_initializer != null)
      {
         body = new Pair(new Pair(Symbol.LAMBDA, new Pair(null, m_initializer)), body);
         setPosURLs(m_initializer, "initializer");
      }

      Function func = null;
      Compiler compiler = new Compiler();

      if (m_nLoopCount > 0)
      {
         Pair argTail = null;
         Pair loopVariables = null;

         m_loopFunctionArray = new Function[m_nLoopCount];

         for (int nLoop = 0; nLoop < m_nLoopCount; nLoop++)
         {
            m_loopFunctionArray[nLoop] = compiler.compile(
               Pair.list(Symbol.LAMBDA, loopVariables, getLoopVarExpr(nLoop)),
               m_posMap, m_urlMap, machine, false
            );

            if (loopVariables == null)
            {
               loopVariables = new Pair(getLoopVarSymbol(nLoop));
               argTail = loopVariables;
            }
            else
            {
               argTail.setTail(argTail = new Pair(getLoopVarSymbol(nLoop)));
            }
         }

         m_allVariables = Pair.append(loopVariables, m_variables);
      }
      else
      {
         m_allVariables = m_variables;
      }

      try
      {
         body = Pair.list(Symbol.LAMBDA, m_allVariables, new Pair(Symbol.VECTOR, body));

         // Set the top level code's URL
         Object pos = new TextPosition(0, 0);
         m_posMap.put(body, pos);
         m_urlMap.put(pos, URL_PREFIX + getName());

         func = compiler.compile(body, m_posMap, m_urlMap, machine, false);
      }
      catch (CompilerException e)
      {
         MetadataValidationException x = new MetadataValidationException(e);

         setProperties(x);

         String sURL = ((CompilerException)e).getURL();

         if (sURL != null)
         {
            int i = URL_PREFIX.length() + getName().length() + 1;
            int k = sURL.indexOf('.', i);

            if (k >= 0)
            {
               x.setProperty("testcase", sURL.substring(k + 1));
            }
            else if (i < sURL.length())
            {
               x.setProperty("script", sURL.substring(i));
            }
         }

         throw x;
      }

      setFunction(func);
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();

      if (m_initializer != null)
      {
         m_initializer = EMPTY; // free memory not used after compile()
      }

      if (m_finalizer != null)
      {
         m_finalizer = EMPTY; // free memory not used after compile()
      }

      m_nAllVariableCount = Pair.length(m_allVariables);
      m_allVariables = null; // free memory not used after compile()
      m_loopArray = null; // free memory not used after compile()
      m_sDump = null; // free memory not used after load
      m_posMap = null; // free memory not used after compile()
      m_urlMap = null; // free memory not used after compile()
      m_variables = null; // free memory not used after compile()
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      marker.setTypeName("UnitTest");
      marker.setProperty("unittest", m_sName);
   }
}
