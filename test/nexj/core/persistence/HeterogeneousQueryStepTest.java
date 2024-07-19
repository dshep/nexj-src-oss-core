package nexj.core.persistence;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Metaclass;
import nexj.core.persistence.sql.ReadCountHook;
import nexj.core.persistence.sql.SQLAdapter;
import nexj.core.persistence.sql.SQLDataTest;
import nexj.core.runtime.Instance;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.SysUtil;

/**
 * Tests heterogeneous query support in GenericCursor.step(Lookup).
 */
public class HeterogeneousQueryStepTest extends SQLDataTest
{
   // constructors

   public HeterogeneousQueryStepTest(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * Q_Patient
    *    ||
    *    || externalVisits
    *    ||
    * Q_ExternalVisit
    *    ||
    *    || requests
    *    ||
    * Q_Request
    */
   public void testInSQL() throws Exception
   {
      String sRegularJoinAttributes = "(firstName lastName (visits reason (requests code)))";
      String sHeteroJoinAttributes = "(firstName lastName (externalVisits reason (requests code)))";
      Metaclass patientClass = getMetadata().getMetaclass("Patient");

      checkSameResults(patientClass, sRegularJoinAttributes, sHeteroJoinAttributes, "((firstName . #f))");
   }

   /**
    *        Q_Patient
    *       /        \\
    *      / children \\  externalVisits
    *     /            \\
    * Q_Patient        Q_ExternalVisit
    *    ||
    *    || externalVisits
    *    ||
    * Q_ExternalVisit
    */
   public void testHeterogeneousJoinOnChildNode() throws Exception
   {
      String sRegularJoinAttributes = "(firstName lastName (visits reason) (children firstName lastName (visits reason)))";
      String sHeteroJoinAttributes = "(firstName lastName (externalVisits reason) (children firstName lastName (externalVisits reason)))";
      Metaclass patientClass = getMetadata().getMetaclass("Patient");

      checkSameResults(patientClass, sRegularJoinAttributes, sHeteroJoinAttributes, "(((@ birthdate) . #f))");
   }

   /**
    * Q_Patient
    *     |
    *     | children
    *     |
    * Q_Patient
    *    ||
    *    || externalVisits
    *    ||
    * Q_ExternalVisit
    *     |
    *     | externalRequests
    *     |
    * Q_ExternalRequest
    */
   public void testHeterogeneousJoinWithHomogeneousJoinedQuery() throws Exception
   {
      // Add ZachJr for the duration of this test only...
      Metaclass patientClass = getMetadata().getMetaclass("Patient");
      Instance parent = Query.createRead(patientClass, null, parse("(= (@ firstName) \"Zach\")"),
         null, 1, 0, false, Query.SEC_ALL, m_context).read().getInstance(0);
      Instance child = new Instance(patientClass, m_context);

      child.setNew();
      child.setValue("firstName", "ZachJr");
      child.setValue("lastName", "Tachoma");
      child.setValue("parent", parent);
      commit();

      ReadCountHook defaultCountHook = (ReadCountHook)((SQLAdapter)getMetadata().getDataSource("DefaultRelationalDatabase").getComponent().getInstance(m_context)).getSQLHook();
      ReadCountHook externalCountHook = (ReadCountHook)((SQLAdapter)getMetadata().getDataSource("ExternalRelationalDatabase").getComponent().getInstance(m_context)).getSQLHook();

      assertNotNull("JUnit configuration error: default.config not up-to-date", defaultCountHook);
      assertNotNull("JUnit configuration error: default.config not up-to-date", externalCountHook);

      String sRegularJoinAttributes = "(firstName lastName (children firstName lastName (visits reason (requests code))))";
      String sHeteroJoinAttributes = "(firstName lastName (children firstName lastName (externalVisits reason (externalRequests code))))";
      int nStartDefaultCount;
      int nStartExternalCount;

      nStartDefaultCount = defaultCountHook.getReadCount();
      nStartExternalCount = externalCountHook.getReadCount();
      checkSameResults(patientClass, sRegularJoinAttributes, sHeteroJoinAttributes, "((firstName . #f))");
      assertEquals(2, defaultCountHook.getReadCount() - nStartDefaultCount);
      assertEquals(1, externalCountHook.getReadCount() - nStartExternalCount);
   }

   /**
    *                  Q_Patient
    *                  //     \\
    * externalVisits  //       \\   externalRequests
    *                //         \\ 
    *     Q_ExternalVisit    Q_ExternalRequest
    */
   public void testTwoHeterogeneousJoins() throws Exception
   {
      String sRegularJoinAttributes = "(firstName lastName (visits reason) (requests code))";
      String sHeteroJoinAttributes = "(firstName lastName (externalVisits reason) (externalRequests code))";
      Metaclass patientClass = getMetadata().getMetaclass("Patient");

      checkSameResults(patientClass, sRegularJoinAttributes, sHeteroJoinAttributes, "((firstName . #f))");
   }

   // helper operations

   /**
    * Compares the results of a test query against a reference query.
    *  
    * @param clazz The root class to query.
    * @param sReferenceQueryAttributes The attribute list for the reference query.
    * @param sTestQueryAttributes The attribute list for the test query.
    * @throws Exception If an error occurs.
    */
   protected void checkSameResults(Metaclass clazz, String sReferenceQueryAttributes, String sTestQueryAttributes, String sOrderBy)
      throws Exception
   {
      Pair orderBy = parse(sOrderBy);
      Pair attributes;
      List expectedData = new ArrayList(8);
      List actualData = new ArrayList(8);

      attributes = parse(sReferenceQueryAttributes);
      query(clazz, attributes, expectedData, orderBy);

      Lookup mapCopy = (Lookup)m_context.getComponentInstanceMap().clone();

      m_context.initialize(m_context.getPrincipal(), m_context.getMachine().getGlobalEnvironment());

      for (Iterator itr = mapCopy.iterator(); itr.hasNext(); )
      {
         Object key = itr.next();

         m_context.getComponentInstanceMap().put(key, mapCopy.get(key));
      }

      attributes = parse(sTestQueryAttributes);
      query(clazz, attributes, actualData, orderBy);

      StringWriter expectedWriter = new StringWriter();;
      StringWriter actualWriter = new StringWriter();

      sortTable(expectedData);
      sortTable(actualData);
      writeTable(expectedWriter, expectedData);
      writeTable(actualWriter, actualData);

      assertEquals(expectedWriter.toString(), actualWriter.toString());
   }

   protected void query(Metaclass metaclass, Pair attributes, List table, Pair orderBy) throws Exception
   {
      Query query = Query.createRead(metaclass, attributes, null, orderBy, -1, 0, false, Query.SEC_ALL, m_context);
      Cursor cursor = query.openCursor();
      Lookup map = new HashTab();

      while (cursor.step(map))
      {
         Instance instance = (Instance)map.get(query);
         List row = new ArrayList();

         for (Pair pair = attributes; pair != null; pair = pair.getNext())
         {
            Object head = pair.getHead();

            if (head instanceof Symbol)
            {
               row.add(instance.getValue(((Symbol)head).getName()));
            }
            else
            {
               addColumn((Pair)head, map, query, row);
            }
         }

         table.add(row);
      }
   }

   protected void addColumn(Pair attributes, Lookup map, Query parentQuery, List row)
   {
      Metaclass parentClass = parentQuery.getMetaclass();
      Query query = parentQuery.findAssoc(Query.ASSOC_QUERY, parentClass.getAttribute((Symbol)attributes.getHead()), null, false);
      Instance instance = (Instance)map.get(query);

      for (attributes = attributes.getNext(); attributes != null; attributes = attributes.getNext())
      {
         Object head = attributes.getHead();

         if (head instanceof Symbol)
         {
            row.add((instance == null) ? "<null>" : instance.getValue(((Symbol)head).getName()));
         }
         else
         {
            addColumn((Pair)head, map, query, row);
         }
      }
   }

   protected void writeTable(Writer writer, List table) throws IOException
   {
      for (Iterator itr = table.iterator(); itr.hasNext(); )
      {
         writer.write(itr.next().toString());
         writer.write(SysUtil.LINE_SEP);
      }
   }

   protected void sortTable(List table)
   {
      Collections.sort(table, new Comparator()
      {
         public int compare(Object arg0, Object arg1)
         {
            List row0 = (List)arg0;
            List row1 = (List)arg1;

            for (int i = 0; i < row0.size(); i++)
            {
               int n = ((String)row0.get(i)).compareTo((String)row1.get(i));

               if (n != 0)
               {
                  return n;
               }
            }

            return 0;
         }
      });
   }
}
