// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.io.StringWriter;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import javax.transaction.Status;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.service.Service;
import nexj.core.meta.persistence.DataSourceAdapter;
import nexj.core.meta.persistence.DataSourceType;
import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgrade;
import nexj.core.meta.upgrade.ScriptUpgrade;
import nexj.core.meta.upgrade.Upgrade;
import nexj.core.meta.upgrade.VersionUpgrade;
import nexj.core.meta.workflow.State;
import nexj.core.persistence.Cursor;
import nexj.core.persistence.DuplicateKeyException;
import nexj.core.persistence.InvalidQueryException;
import nexj.core.persistence.LockTimeoutException;
import nexj.core.persistence.OID;
import nexj.core.persistence.OptimisticLockException;
import nexj.core.persistence.Query;
import nexj.core.persistence.QueryTimeoutException;
import nexj.core.persistence.SchemaVersion;
import nexj.core.persistence.operator.AndOperator;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceArrayList;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.UnitOfWork;
import nexj.core.runtime.ValidationException;
import nexj.core.scripting.Function;
import nexj.core.scripting.Pair;
import nexj.core.scripting.ScriptingException;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.Invalid;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Lookup;
import nexj.core.util.UncheckedException;
import nexj.core.util.Undefined;
import nexj.test.util.AssertUtil;

/**
 * Base test case for SQLAdapter.
 */
public abstract class SQLAdapterTest extends SQLDataTest
{
   public SQLAdapterTest(String name)
   {
      super(name);
   }

   /**
    * Check if the supplied version is compatible with the current SQLAdapter.
    * @param sDBVersion The version returned by DatabaseMetaData.getDatabaseProductVersion().
    * @return The current SQLAdapter is applicable for the specified version.
    */
   protected boolean isCompatibleVersion(String sDBVersion)
   {
      return true;
   }

   public void testAdapterCompatibility() throws SQLException
   {
      SQLConnection connection = m_adapter.getConnection();
      Connection con = connection.getConnection();
      RelationalDatabase db = new RelationalDatabase(null);

      try
      {
         String sDBVersion = con.getMetaData().getDatabaseProductVersion();

         // iterate over DataSourceType instead of DataSource in case no DataSources defined
         for (Iterator/*<DataSourceType>*/ itr =
                 m_context.getMetadata().getDataSourceTypeIterator();
              itr.hasNext();)
         {
            db.setType((DataSourceType)itr.next());

            for (Iterator/*<DataSourceType>*/ itr2 = db.getType().getAdapterIterator();
                 itr2.hasNext();)
            {
               db.setDriver(null);
               db.setAdapter((DataSourceAdapter)itr2.next());
               db.setDefaultProperties(J2EEUtil.NONE);
               assertEquals(m_adapter.getClass().equals(db.getAdapter().getClassObject()) &&
                            isCompatibleVersion(sDBVersion),
                            m_adapter.isCompatible(con, db));
            }
         }
      }
      finally
      {
         connection.decRef();
      }
   }

   /**
    * @return Maximum number of binds (both literal and non-literal) allowed in an SQL statement.
    */
   protected int getMaxBindCount()
   {
      return Integer.MAX_VALUE;
   }

   public void testUpgrade()
   {
      Upgrade upgrade = getMetadata().getUpgrade("Main");

      upgrade.validate(getMetadata(), null);

      VersionUpgrade firstVersion = upgrade.getFirstVersion();
      Lookup stateMap = Upgrade.getInitialState(firstVersion);
      SchemaVersion version = new SchemaVersion();

      version.setNamespace(getMetadata().getNamespace());
      version.setUpgradable(true);

      for (VersionUpgrade u = firstVersion; u != null; u = u.getNext())
      {
         version.setVersion(u.getName());
         version.setStep(0);

         if (u.getName() == null)
         {
            u.apply(Upgrade.getState(stateMap, u));
         }
         else if (u instanceof RelationalSchemaUpgrade)
         {
            m_adapter.upgrade((RelationalSchemaUpgrade)u, Upgrade.getState(stateMap, u), version);
         }
         else if (u instanceof ScriptUpgrade)
         {
            Function fun = ((ScriptUpgrade)u).getFunction();

            if (fun != null)
            {
               m_context.getMachine().invoke(fun, (Pair)null);
            }
         }
      }
   }

   public void testRead()
   {
      Pair attributes;
      Query query;

      // Lazy association type adjustment
      Instance instance = new Instance(getMetadata().getMetaclass("Principal"), true, m_context);

      instance.cache(new OID(new Object[]{Binary.parse("00000000000000000000000000000002")}));
      query = Query.createRead(getMetadata().getMetaclass("UserGroupAssoc"), parse("(user)"),
         parse("(= (@) (oid #z00000000000000000000000000000003))"),
         null, -1, 0, false, Query.SEC_NONE, m_context);
      assertEquals("User", ((Instance)query.read().getInstance(0).getValue("user")).getLazyMetaclass().getName());
      m_context.removeInstance(instance);
      instance = null;

      // Unsupported where clause
      try
      {
         Query.createRead(getMetadata().getMetaclass("Contact"), null, parse("(> (myfun firstName) 0)"), null, -1, 0, false, Query.SEC_NONE, m_context);
         fail("Expected InvalidQueryException");
      }
      catch (InvalidQueryException e)
      {
         assertEquals("err.persistence.unsupportedExpression", e.getErrorCode());
      }

      attributes = parse("(firstName lastName (addresses country city street code) (user name))");
      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes, 
         parse("(= classCode \"CON\")"),
         parse("((firstName . #t)(lastName . #t)((@ addresses) . #t))"),
         -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("read.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes, 
         parse("(= classCode \"CON\")"),
         parse("((firstName . #t)(lastName . #t)((@ addresses) . #t) ((like? firstName \"A*\") . #t))"),
         -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("read.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(and (null? (@ noAddresses city)) (= classCode \"CON\"))"),
         parse("((firstName . #t)(lastName . #t)((@ addresses) . #t))"),
         -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("read.xml"), query.read(), attributes);

      String sSQL = ((SQLGenerator)query.getGenerator()).getSQL();

      query = Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("((user name) lastName (addresses country city street code) firstName)"),
         parse("(and (null? (@ noAddresses city)) (= classCode \"CON\"))"),
         parse("((firstName . #t)(lastName . #t)((@ addresses) . #t))"),
         -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("read.xml"), query.read(), attributes);

      assertEquals(sSQL, ((SQLGenerator)query.getGenerator()).getSQL());

      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(= classCode \"CON\")"),
         parse("((firstName . #t)(lastName . #t)((@) . #t)((@ addresses) . #t))"),
         2, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCount.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(= classCode \"CON\")"),
         parse("((firstName . #t)(lastName . #t)((@) . #t)((@ addresses) . #t))"),
         2, 2, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readOffset.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(and (like? firstName \"J*\") (= lastName \"Smith\") (in? (@ readPrincipal name) \"users\" \"QA\" \"jsmith\")))"),
         parse("((firstName . #t)(lastName . #t)((@) . #t)((@ addresses) . #t))"),
         -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readWhere.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(and (like? firstName \"J*\") (= lastName \"Smith\") (in? (@ readPrincipal name) \"users\" \"QA\" \"jsmith\" " +
                "\"1\" \"2\" \"3\" \"4\" \"5\" \"6\" \"7\" \"8\" \"9\" \"10\" \"11\" \"12\" \"13\" \"14\" \"15\" \"16\" " +
                "\"17\" \"18\" \"19\" \"20\" \"21\" \"22\" \"23\" \"24\" \"25\" \"26\" \"27\" \"28\" \"29\" \"30\")))"),
         parse("((firstName . #t)(lastName . #t)((@) . #t)((@ addresses) . #t))"),
         -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readWhere.xml"), query.read(), attributes);

      List inList = new ArrayList(3000);

      // add smaller of 3000 binds or maximum supported by adapter (-5 for args + like? + (=...))
      for (int i = 0, nCount = Math.min(3000, getMaxBindCount() - 5); i < nCount; ++i)
      {
         inList.add(String.valueOf(i));
      }

      inList.add("users");
      inList.add("QA");
      inList.add("jsmith");

      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(and (like? firstName \"J*\") (= lastName \"Smith\"))")
            .and(Pair.attribute("readPrincipal name").in(new Pair(inList))),
         parse("((firstName . #t)(lastName . #t)((@) . #t)((@ addresses) . #t))"),
         -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readWhere.xml"), query.read(), attributes);
      inList = null;

      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes, parse("(= lastName ())"),
         parse("((firstName . #t)(lastName . #t)((@) . #t)((@ addresses) . #t))"),
         -1, 0, false, Query.SEC_NODE, m_context);
      AssertUtil.assertEquals(getURL("readWhereNull.xml"), query.read(), attributes);

      attributes = parse("(type country city)");
      query = Query.createRead(getMetadata().getMetaclass("Address"), attributes,
         new Pair(Symbol.EQ,
            new Pair(parse("(@@ Contact addresses)"),
            new Pair(new OID(new Object[]{Binary.parse("00000000000000000000000000000001")})))),
         parse("((type . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readReverse.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("Address"), attributes,
         new Pair(Symbol.OR, new Pair(Boolean.FALSE, new Pair(
            new Pair(Symbol.EQ,
               new Pair(parse("(@@ User contact addresses)"),
                  new Pair(new OID(new Object[]{Binary.parse("00000000000000000000000000000001")}))))))),
         parse("((type . #t) ((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readReverse2.xml"), query.read(), attributes);

      InstanceList list = Query.createRead(getMetadata().getMetaclass("City"), parse("(name)"),
         parse("(= (@@ Contact businessAddress2 cityEnum) (oid #z00000000000000000000000000000001))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();

      assertEquals(1, list.size());
      assertEquals("Toronto", list.getInstance(0).getValue("name"));

      query = Query.createRead(getMetadata().getMetaclass("Address"), attributes,
               parse("(= (@@ Contact businessAddress2) (oid #z00000000000000000000000000000001))"),
         parse("((type . #t) ((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readReverse3.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("BusinessAddress"),
         parse("(type country city street code)"),
         parse("(like? city \"T*\")"), parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readClassWhere.xml"), query.read(), attributes);

      attributes = parse("(firstName (addresses city))");
      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(and (like? (@ addresses city) \"T*\") (= classCode \"CON\"))"),
         parse("(((@ addresses city) . #t) ((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCollectionWhere.xml"), query.read(), attributes);

      attributes = parse("(firstName lastName (addresses country city street code) (businessAddress2 country city street code) (user name))");
      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(= classCode \"CON\")"),
         parse("((firstName . #t)(lastName . #t)((@ addresses) . #t))"),
         -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readAssocWhere.xml"), query.read(), attributes);

      attributes = parse("(firstName lastName (qaUser name) (user name))");
      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(= classCode \"CON\")"),
         parse("((firstName . #t)(lastName . #t))"), -1, 0, false, Query.SEC_NONE, m_context);

      list = query.read();

      AssertUtil.assertEquals(getURL("readAssocWhere2.xml"), list, attributes);

      query.setMaxCount(1000000);

      Cursor cursor = query.openCursor();

      try
      {
         int i = 0;

         while ((instance = cursor.next()) != null)
         {
            assertSame(list.getInstance(i++), instance);
         }

         assertEquals(list.size(), i);
      }
      finally
      {
         cursor.close();
      }

      attributes = parse("(addresses)");
      cursor = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(= classCode \"CON\")"), null, -1, 0, false, Query.SEC_NONE, m_context).openCursor();

      try
      {
         cursor.next();
         cursor.next();
         instance = cursor.next();
         assertEquals(2, ((InstanceList)instance.getValue("addresses")).size());
      }
      finally
      {
         cursor.close();
      }

      attributes = parse("(fullName)");
      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(any (= (@ addresses city) \"Toronto\"))"),
         parse("((firstName . #t)(lastName . #t)((@ addresses) . #t))"),
         -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readAny.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(any (= (@ businessAddress3 city) \"Toronto\"))"),
         parse("((firstName . #t)(lastName . #t)((@ addresses) . #t))"),
         -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readAny2.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(any (= (@) ()))"), parse("((firstName . #t)(lastName . #t)((@ addresses) . #t))"),
         -1, 0, false, Query.SEC_NONE, m_context);
      assertEquals(0, query.read().size());

      query = Query.createRead(getMetadata().getMetaclass("Contact"), parse("((qaUser isQAManager))"),
         parse("(= classCode \"CON\")"), null, -1, 0, false, Query.SEC_NONE, m_context);
      assertEquals(5, query.read().size());

      query = Query.createRead(getMetadata().getMetaclass("User"),
         parse("(name (businessContact firstName))"),
         parse("(= name \"jtest\")"),
         null, -1, 0, false, Query.SEC_NONE, m_context);
      list = query.read();
      assertEquals(1, list.size());
      assertEquals("Joe", ((Instance)list.getInstance(0).getValue("businessContact")).getValue("firstName"));

      query = Query.createRead(getMetadata().getMetaclass("User"), parse("(name)"),
         parse("(= (@ contact addresses typeEnum) (oid \"address\" \"en\" \"Business\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context);
      list = query.read();
      assertEquals(1, list.size());
      assertEquals("jtest", list.getInstance(0).getValue("name"));
      // Join optimization
      assertFalse(((SQLJoin)query.findAssoc(Query.ASSOC_QUERY,
         query.getMetaclass().getAttribute("contact"),
         null, false).getMapping()).isEnabled);

      query = Query.createRead(getMetadata().getMetaclass("User"), parse("(name)"),
         parse("(any (= (@ contact addresses typeEnum) (oid \"address\" \"en\" \"Business\")))"),
         null, -1, 0, false, Query.SEC_NONE, m_context);
      list = query.read();
      assertEquals(1, list.size());
      assertEquals("jtest", list.getInstance(0).getValue("name"));
      // Join optimization
      assertFalse(((SQLJoin)query.findAssoc(((AndOperator)query.getWhere()).getOperand(0),
         query.getMetaclass().getAttribute("contact"),
         null, false).getMapping()).isEnabled);

      query = Query.createRead(getMetadata().getMetaclass("Contact"), null,
         parse("(= (@ addresses contact) (oid #z00000000000000000000000000000001))"),
         null, -1, 0, false, Query.SEC_NONE, m_context);
      assertEquals(1, query.read().size());
      // Required flag folding
      assertTrue(query.findAssoc(Query.ASSOC_QUERY, query.getMetaclass().getAttribute("addresses"), null, false).isRequired());

      attributes = parse("(name typeCode)");
      query = Query.createRead(getMetadata().getMetaclass("Principal"), attributes, null,
         parse("((name . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readPolymorphic.xml"), query.read(), attributes);

      attributes = parse("(fullName)");
      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(instance? (@) Patient)"),
         parse("((firstName . #t)(lastName . #t)((@ addresses) . #t))"),
         -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readSubclass.xml"), query.read(), attributes);

      query  = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(and (instance? (@) Patient) (not (like? (@ lastName) \"[S]*\")))"),
         parse("((firstName . #t)(lastName . #t)((@ addresses) . #t))"),
         -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readSubclass.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("Address"), null,
         parse("(= typeEnum (oid \"address\" \"en\" \"Business\"))"),
         parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readEnum.xml"), query.read(), null);

      query = Query.createRead(getMetadata().getMetaclass("Address"), null,
         parse("(= (@ typeEnum type) \"Business\")"),
         parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readEnum.xml"), query.read(), null);

      query = Query.createRead(getMetadata().getMetaclass("Address"), null,
         parse("(and (= typeEnum (oid \"address\" \"en\" \"Business\")) (= (@ typeEnum type) \"Business\"))"),
         parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readEnum.xml"), query.read(), null);

      query = Query.createRead(getMetadata().getMetaclass("Address"), null,
         parse("(and (= typeEnum (oid \"address\" \"en\" \"Business\")) (= (@ typeEnum type) \"1\"))"),
         parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      assertEquals(0, query.read().getCount());

      list = Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(primaryAddresses primaryAddresses2)"),
         parse("(= (@) (oid #z00000000000000000000000000000001))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();
      assertEquals(1, list.size());
      instance = list.getInstance(0);
      assertEquals(1, ((List)instance.getValue("primaryAddresses")).size());
      assertEquals(1, ((List)instance.getValue("primaryAddresses2")).size());

      query = Query.createRead(getMetadata().getMetaclass("ExternalVisit"), null,
         parse("(= patient (oid #z00000000000000000000000000000006))"),
         parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readExternal.xml"), query.read(), null);

      query = Query.createRead(getMetadata().getMetaclass("Patient"), parse("(externalVisits)"),
         parse("(and (= firstName \"Sarah\") (= lastName \"Johnson\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context);
      assertEquals(3, ((InstanceList)query.read().getInstance(0).getValue("externalVisits")).size());

      // Heterogeneous calculated value optimization using the values function
      list = Query.createRead(getMetadata().getMetaclass("Address"), parse("(isCAPhoneless)"),
         parse("(= (@ contact classCode) \"CON\")"), null, -1, 0, false, Query.SEC_NONE, m_context).read();
      assertEquals(7, list.size());

      int nLoadCount = m_context.getLoadCount();
      int nPhonelessCount = 0;

      for (int i = 0; i < list.size(); ++i)
      {
         if (!Boolean.FALSE.equals(list.getInstance(i).getValue("isCAPhoneless")))
         {
            ++nPhonelessCount;
         }
      }

      assertEquals(1, nPhonelessCount);
      assertEquals(nLoadCount, m_context.getLoadCount());

      assertEquals(1, Query.createRead(getMetadata().getMetaclass("Address"), null,
         parse("(and isCAPhoneless (= (@ contact classCode) \"CON\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read().size());

      // Comparison of LOB values, class is not important as long as the attribute is LOB

      Query.createRead(getMetadata().getMetaclass("SysRule"), parse("(name)"),
         parse("(= action \"test\")"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();
      Query.createRead(getMetadata().getMetaclass("SysRule"), parse("(name)"),
         parse("(not (= action \"test\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();
      Query.createRead(getMetadata().getMetaclass("SysTimer"), parse("(period)"),
         parse("(= data #z0)"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();
      Query.createRead(getMetadata().getMetaclass("SysTimer"), parse("(period)"),
         parse("(not (= data #z0))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();

      // Dynamic derived associations

      attributes = parse("(contact)");
      query = Query.createRead(getMetadata().getMetaclass("Address"), attributes,
         parse("(!= (@ contact (instance? (@) Patient) birthdate) ())"),
         parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readDynDerived.xml"), query.read(), attributes);

      attributes = parse("(lastName)");
      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(!= (@ (instance? (@) Patient) birthdate) ())"),
         parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readDynTypeCast.xml"), query.read(), attributes);

      list = Query.createRead(getMetadata().getMetaclass("Contact"), parse("(lastName)"),
         parse("(!= (@ addresses (= (@ country) \"Canada\") contact (= (@ firstName) \"Joe\") addresses) ())"),
         parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context).read();
      assertEquals(1, list.size());
      assertEquals("Joe", list.getInstance(0).getValue("firstName"));

      query = Query.createRead(getMetadata().getMetaclass("Visit"), null,
         parse("(= (@@ Address contact (instance? (@) Patient) visits) (oid #z00000000000000000000000000000008))"),
         parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readDynReverse.xml"), query.read(), null);

      // Calculated attributes
      attributes = parse("(isTorontoBasedEmployee tax lastNameLengthPlus1 lastNameInitial)");
      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(= classCode \"CON\")"),
         parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCalculated.xml"), query.read(), attributes);

      attributes = parse("((businessAddress3 deletable))");
      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(= classCode \"CON\")"),
         parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCalculatedSubq.xml"), query.read(), attributes);

      Query.createRead(getMetadata().getMetaclass("Patient"), parse("(visibleDescription)"),
         null, null, 1, 0, false, Query.SEC_NONE, m_context);

      list = Query.createRead(getMetadata().getMetaclass("Contact"), null,
         Pair.attribute("visible"), null, -1, 0, false, Query.SEC_NONE, m_context).read();

      for (int i = 0; i < list.size(); ++i)
      {
         assertEquals(Boolean.TRUE, list.getInstance(i).getValue("visible"));
      }

      assertEquals(2, Query.createRead(getMetadata().getMetaclass("Contact"), null,
         parse("(and (= classCode \"CON\") (> (/ businessAddressCount 2) 0))"), null, -1, 0, false,
         Query.SEC_NONE, m_context).read().size());

      assertEquals(2, Query.createRead(getMetadata().getMetaclass("Address"), null,
         parse("(= (@@ User addresses) (oid #z00000000000000000000000000000001))"), null, -1, 0, false,
         Query.SEC_NONE, m_context).read().size());

      // Annotations
      attributes = parse("(firstName (: _fnLen (string-length (@ firstName))) (: _n 123) (: _ln (@ lastName)) (: _x2 (let ((x 2)) (* x x))) (: _f (lambda (x) 1)))");
      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(= classCode \"CON\")"),
         parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readAnnotations.xml"), query.read(), attributes);

      // Annotated queries with cached classes (no caching)
      attributes = parse("((: l (string-length (@ name))))");
      query = Query.createRead(getMetadata().getMetaclass("Country"), attributes,
         null, parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readAnnotationsCached.xml"), query.read(), attributes);

      // Aggregate functions
      attributes = parse("(firstName lastName addressCount (: _countryCnt (count (@ addresses country))) " +
         "(: _uniqueCountryCnt (count (unique (@ addresses country)))) (: _nullCodeCnt (count (null? (@ addresses code)))) " +
         "(: _avg (average (string-length (@ addresses city)))) (: _min (minimum (string-length (@ addresses city)))) " +
         "(: _max (maximum (@ addresses city))) (: _sum (sum (string-length (@ addresses city)))))");
      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(= classCode \"CON\")"), parse("(((@) . #t))"),
         -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readAggregate.xml"), query.read(), attributes);

      // Aggregate queries
      list = Query.createAggregate(getMetadata().getMetaclass("Contact"), null,
         parse("(= classCode \"CON\")"), null, null, null, -1, 0, Query.SEC_NONE, m_context).read();
      assertEquals(1, list.size());
      instance = list.getInstance(0);
      assertEquals("Object", instance.getClassName());
      assertNull(instance.getOID());

      attributes = parse("((: c (count (@))) (: n 1))");
      query = Query.createAggregate(getMetadata().getMetaclass("Contact"), attributes, 
         parse("(= classCode \"CON\")"), null, null, null, -1, 0, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readGroupByCountAll.xml"), query.read(), attributes);

      attributes = parse("((: c (* 2 (count (@)))))");
      query = Query.createAggregate(getMetadata().getMetaclass("Contact"), attributes, 
         parse("(= classCode \"CON\")"), parse("((@))"), null, parse("(((@) . #t))"), -1, 0, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readGroupByIndentity.xml"), query.read(), attributes);

      query = Query.createAggregate(getMetadata().getMetaclass("Contact"), parse("(classCode (: c (* 2 (count (@)))) (: n 1))"), 
         parse("(= classCode \"CON\")"), parse("(classCode)"), null, parse("((classCode . #t))"),
         -1, 0, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readGroupByAttribute.xml"), query.read(),
         parse("((: classCode (@ classCode)) (: c (* 2 (count (@)))) (: n 1))"));

      attributes = parse("((: fnl (string-length (@ firstName))))");
      query = Query.createAggregate(getMetadata().getMetaclass("Contact"), attributes, 
         parse("(= classCode \"CON\")"), parse("(classCode (string-length (@ firstName)))"), null,
         parse("((classCode . #t) ((string-length (@ firstName)) . #t))"), -1, 0, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readGroupByExpr.xml"), query.read(), attributes);

      attributes = parse("((: c (* 2 (count (@)))) (: ac (count (@ addresses))))");
      query = Query.createAggregate(getMetadata().getMetaclass("Contact"), attributes, 
         parse("(= classCode \"CON\")"), parse("((@))"), null,
         parse("(((@) . #t))"), -1, 0, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readGroupByIdentityCountAssoc.xml"), query.read(), attributes);

      attributes = parse("((: classCode (@ classCode)) (: c (* 2 (count (@)))) (: ac (count (@ addresses))))");
      query = Query.createAggregate(getMetadata().getMetaclass("Contact"), attributes, 
         parse("(= classCode \"CON\")"), parse("(classCode)"), parse("(> (count (@ addresses)) 1)"),
         parse("((classCode . #t))"), -1, 0, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readGroupByCountAssoc.xml"), query.read(), attributes);

      attributes = parse("((: c (* 2 (count (@)))) (: a (@ addresses)))");
      query = Query.createAggregate(getMetadata().getMetaclass("Contact"), attributes, 
         parse("(= classCode \"CON\")"), parse("((@) addresses)"), null,
         parse("((addresses . #t) ((@) . #t))"), -1, 0, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readGroupByAssoc.xml"), query.read(), attributes);

      query = Query.createAggregate(getMetadata().getMetaclass("Contact"),
         parse("(firstName (: c (count (@))))"), 
         parse("(= classCode \"CON\")"), parse("(firstName)"), null,
         parse("((firstName . #t))"), -1, 0, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readGroupByCase.xml"), query.read(),
         parse("((: firstName (@ firstName)) (: c (count (@))))"));

      attributes = parse("((: ac (@ businessAddress city)) (: c (count (@))))");
      query = Query.createAggregate(getMetadata().getMetaclass("Contact"), attributes, 
         parse("(= classCode \"CON\")"), parse("((@ businessAddress city))"), null,
         parse("(((@ businessAddress city) . #t))"), -1, 0, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readGroupByPrimitiveAnnot.xml"), query.read(), attributes);

      attributes = parse("((: a (@ businessAddress)) (: c (count (@))))");
      query = Query.createAggregate(getMetadata().getMetaclass("Contact"), attributes, 
         parse("(= classCode \"CON\")"), parse("((@ businessAddress))"), null,
         parse("(((@ businessAddress) . #t))"), -1, 0, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readGroupByClassAnnot.xml"), query.read(), attributes);

      query = Query.createAggregate(getMetadata().getMetaclass("Contact"),
         parse("(businessAddress (: c (count (@))))"), 
         parse("(= classCode \"CON\")"), parse("(businessAddress)"), null,
         parse("((businessAddress . #t))"), -1, 0, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readGroupByClassAssoc.xml"), query.read(),
         parse("((: businessAddress (@ businessAddress)) (: c (count (@))))"));

      query = Query.createAggregate(getMetadata().getMetaclass("Contact"),
         parse("(lastName (primaryAddress city state) " +
            "(: md (and (maximum (= (@ firstName) \"John\")) (minimum (= (@ firstName) \"John\")))))"), 
         parse("(= classCode \"CON\")"), parse("(lastName primaryAddress)"), null,
         parse("(((@ lastName) . #t) ((count (@ addresses)) . #t))"), -1, 0, Query.SEC_NONE, m_context);
      list = query.read();
      assertEquals("Richmond Hill", ((Instance)list.getInstance(3).getValue("primaryAddress")).getValue("city"));
      AssertUtil.assertEquals(getURL("readGroupByCount.xml"), list,
         parse("((: lastName (@ lastName)) (: primaryAddress (@ primaryAddress)) " +
            "(: md (and (maximum (= (@ firstName) \"John\")) (minimum (= (@ firstName) \"John\")))))"));

      query = Query.createAggregate(getMetadata().getMetaclass("Contact"),
         parse("(firstName (: x (* (@ version) (count (@ addresses)))))"), 
         parse("(and (= classCode \"CON\") (= firstName \"Joe\"))"), parse("((@))"), null,
         null, -1, 0, Query.SEC_NONE, m_context);
      list = query.read();
      assertEquals(1, list.size());
      assertEquals(new Long(0), list.getInstance(0).findAnnotation("x"));

      attributes = parse("((: c (* 2 (count (@)))))");
      query = Query.createAggregate(getMetadata().getMetaclass("Contact"), attributes, 
         parse("(= classCode \"CON\")"), parse("(addresses)"), null,
         parse("((addresses . #t))"), -1, 0, Query.SEC_NONE, m_context);
      cursor = query.openCursor();

      try
      {
         AssertUtil.assertEquals(getURL("readGroupByCursorCol.xml"), cursor.next(1024), attributes);
      }
      finally
      {
         cursor.close();
      }

      // Aggregate queries with cached classes (no caching)

      query = Query.createAggregate(getMetadata().getMetaclass("Country"), null,
         null, null, null, null, -1, 0, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readGroupByCachedDefault.xml"), query.read(), null);

      attributes = parse("((: c (count (@))))");
      query = Query.createAggregate(getMetadata().getMetaclass("Country"), attributes,
         null, null, null, null, -1, 0, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readGroupByCachedCountAll.xml"), query.read(), attributes);

      attributes = parse("((: m (maximum (@ countryEnum name))))");
      query = Query.createAggregate(getMetadata().getMetaclass("Address"), attributes,
         null, parse("((@))"), null, parse("(((@) . #t))"), -1, 0, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readGroupByCachedAssoc.xml"), query.read(), attributes);

      attributes = parse("((: c (count (@))))");
      query = Query.createAggregate(getMetadata().getMetaclass("Address"), attributes,
         null, parse("((@ countryEnum name))"), null, parse("(((@ countryEnum name) . #t))"), -1, 0, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readGroupByAssocCached.xml"), query.read(), attributes);

      // Nested aggregate functions
      try
      {
         Query.createAggregate(getMetadata().getMetaclass("Contact"), parse("((: ac (minimum (count (@ addresses)))))"),
            null, parse("((@))"), null, null, -1, 0, Query.SEC_NONE, m_context);
         fail("Expected InvalidQueryException");
      }
      catch (InvalidQueryException e)
      {
         assertEquals("err.persistence.nestedAggregate", e.getErrorCode());
      }

      // Unsupported expression
      try
      {
         Query.createAggregate(getMetadata().getMetaclass("Contact"), parse("((: fnl (sin (string-length (@ firstName)))))"),
            null, parse("(classCode (sin (string-length (@ firstName))))"), null, null, -1, 0, Query.SEC_NONE, m_context);
         fail("Expected InvalidQueryException");
      }
      catch (InvalidQueryException e)
      {
         assertEquals("err.persistence.unsupportedExpression", e.getErrorCode());
      }

      // Invalid having clause
      try
      {
         Query.createAggregate(getMetadata().getMetaclass("Contact"), null, null,
            parse("(addresses)"), parse("(> (@) (oid #z1))"), null, -1, 0, Query.SEC_NONE, m_context).read();
         fail("Expected InvalidQueryException");
      }
      catch (InvalidQueryException e)
      {
         assertEquals("err.persistence.ungroupedHaving", e.getErrorCode());
      }

      // Query macros
      assertEquals(2, Query.createRead(getMetadata().getMetaclass("Address"), null,
         parse("(and isQuery (= contact (oid #z00000000000000000000000000000001)))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read().size());

      assertEquals("R", Query.createRead(getMetadata().getMetaclass("Address"), parse("(cityq)"),
         parse("(= contact (oid #z00000000000000000000000000000001))"),
         parse("((city . #t))"), -1, 0, false, Query.SEC_NONE, m_context).read().getInstance(0).getValue("cityq"));

      assertEquals("Richmond Hill", ((InstanceList)Query.createRead(getMetadata().getMetaclass("Contact"), parse("((addresses cityq))"),
         parse("(= (@) (oid #z00000000000000000000000000000001))"),
         parse("(((@ addresses city) . #t))"), -1, 0, false, Query.SEC_NONE, m_context)
         .read().getInstance(0).getValue("addresses")).getInstance(0).getValue("cityq"));

      // Class caching
      attributes = parse("(name)");
      query = Query.createRead(getMetadata().getMetaclass("Country"), attributes,
         parse("(= name \"Canada\")"), null, -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCachedInstance.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("Country"), attributes,
         parse("(= name \"Canada\")"), null, -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCachedInstance.xml"), query.read(), attributes);

      // Instance caching
      attributes = parse("(name country)");
      query = Query.createRead(getMetadata().getMetaclass("City"), attributes,
         parse("(= name \"Toronto\")"), null, -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCachedClass.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("City"), attributes,
         parse("(= name \"Toronto\")"), null, -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCachedClass.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("City"), attributes,
         parse("(= (@) (oid \"Toronto\"))"), null, -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCachedClass.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("City"), attributes,
         parse("(= (@) (oid \"Toronto1\"))"), null, -1, 0, false, Query.SEC_NONE, m_context);
      assertEquals(0, query.read().size());

      query = Query.createRead(getMetadata().getMetaclass("City"), attributes,
         parse("(like? name \"Toro*\")"), null, -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCachedClass.xml"), query.read(), attributes);

      attributes = parse("((countryEnum name) (cityEnum name country))");
      query = Query.createRead(getMetadata().getMetaclass("Address"), attributes,
         null, parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCachedJoin.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("Address"), attributes,
         null, parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCachedJoin.xml"), query.read(), attributes);

      attributes = parse("((primaryAddress cityEnum))");
      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(= (@ primaryAddress cityEnum name) \"Richmond Hill\")"),
         parse("(((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCachedMix.xml"), query.read(), attributes);

      // Wrong subclass
      query = Query.createRead(getMetadata().getMetaclass("Patient"), null,
         parse("(= (@) (oid #z00000000000000000000000000000001))"), null, -1, 0, false, Query.SEC_NONE, m_context);
      assertEquals(0, query.read().size());

      // Reverse assoc subclass, just create the query
      query = Query.createRead(getMetadata().getMetaclass("BusinessAddress"), null,
         parse("(= (@@ Contact addresses) '())"), null, -1, 0, false, Query.SEC_NONE, m_context);

      query = Query.createRead(getMetadata().getMetaclass("Principal"), null,
         parse("(= (@@ User) ())"), null, -1, 0, false, Query.SEC_NONE, m_context);

      // Overriden attribute type, just create the query
      query = Query.createRead(getMetadata().getMetaclass("Patient"), parse("((businessAddress2 main))"),
         null, null, -1, 0, false, Query.SEC_NONE, m_context);

      // SysReader with dispatch

      Instance r1 = new Instance(getMetadata().getMetaclass("SysReader"), Instance.NEW, m_context);

      r1.setValue("class", Symbol.define("Principal"));
      r1.setValue("attributes", parse("(name)"));
      r1.setValue("where", parse("(= name \"users\")"));
      r1.invoke("read");

      Instance r2 = new Instance(getMetadata().getMetaclass("SysReader"), Instance.NEW, m_context);

      r2.setValue("class", Symbol.define("UserGroupAssoc"));
      r2.setValue("attributes", parse("(user group)"));
      r2.setValue("orderBy", parse("(((@) . #t))"));

      InstanceList p = new InstanceArrayList();
      p.add(r1);
      r2.setValue("parents", p);

      List a = new ArrayList();
      a.add(parse("(ugassocs)"));
      r2.setValue("associations", a);
      r2.setValue("dispatchParent", r1);
      r2.setValue("dispatchAttribute", null);
      r2.setValue("dispatchValues", parse("(Group)"));

      AssertUtil.assertEquals(getURL("readFMDispatch1.xml"), r2.invoke("read"), parse("(user group)"));

      r2 = new Instance(getMetadata().getMetaclass("SysReader"), Instance.NEW, m_context);

      r2.setValue("class", Symbol.define("UserGroupAssoc"));
      r2.setValue("attributes", parse("(user group)"));
      r2.setValue("orderBy", parse("(((@) . #t))"));

      p = new InstanceArrayList();
      p.add(r1);
      r2.setValue("parents", p);

      a = new ArrayList();
      a.add(parse("(ugassocs)"));
      r2.setValue("associations", a);
      r2.setValue("dispatchParent", r1);
      r2.setValue("dispatchAttribute", parse("(typeCode)"));
      r2.setValue("dispatchValues", parse("(\"G\")"));

      AssertUtil.assertEquals(getURL("readFMDispatch2.xml"), r2.invoke("read"), parse("(user group)"));

      Instance r3 = new Instance(getMetadata().getMetaclass("SysReader"), Instance.NEW, m_context);

      r3.setValue("class", Symbol.define("Principal"));
      r3.setValue("attributes", parse("(name)"));
      r3.setValue("orderBy", parse("(((@ name) . #t))"));

      p = new InstanceArrayList();
      p.add(r1);
      r3.setValue("parents", p);

      a = new ArrayList();
      a.add(null);
      r3.setValue("associations", a);
      r3.setValue("dispatchParent", r1);
      r3.setValue("dispatchAttribute", null);
      r3.setValue("dispatchValues", parse("(Principal)"));
      r3.setValue("dispatchGroup", r2);
      r3.setValue("dispatched", Boolean.FALSE);

      r2.setValue("dispatched", Boolean.TRUE);

      AssertUtil.assertEquals(getURL("readFMDispatch3.xml"), r3.invoke("read"), parse("(name)"));
      assertEquals(Boolean.TRUE, r2.getValue("dispatched"));
      assertEquals(Boolean.FALSE, r3.getValue("dispatched"));

      r2.setValue("dispatched", Boolean.FALSE);

      AssertUtil.assertEquals(getURL("readFMDispatch4.xml"), r3.invoke("read"), parse("(name)"));
      assertEquals(Boolean.TRUE, r2.getValue("dispatched"));
      assertEquals(Boolean.TRUE, r3.getValue("dispatched"));

      // nameAttribute
      assertEquals("Joe Test [Employee] {jtest}", m_context.getUser().getName());

      // Query CLOB field to see if it is null/not null
      // (Direct IS NULL queries on LOBs fail in Sybase 12.5.4)
      query = Query.createRead(getMetadata().getMetaclass("SysRule"), null,
         parse("(null? condition)"),
         null, -1, 0, false, Query.SEC_NONE, m_context);
      assertEquals(2, query.read().getCount());

      query = Query.createRead(getMetadata().getMetaclass("SysRule"), null,
         parse("(not (null? condition))"),
         null, -1, 0, false, Query.SEC_NONE, m_context);
      assertEquals(4, query.read().getCount());
   }

   public void testLoad()
   {
      Metaclass metaclass = getMetadata().getMetaclass("Patient");
      Query query = Query.createRead(metaclass, null, parse("(= lastName \"Johnson\")"),
         null, -1, 0, false, Query.SEC_NONE, m_context);
      Instance instance = query.read().getInstance(0);
      Pair attributes = Pair.fromArray(Pair.toArray(parse("(type (@@ Contact fullName user) (@@ Contact user) (addresses))")));

      instance.invoke("load", attributes);

      String[] names = new String[]{"type" , "fullName", "user", "addresses"};

      for (int i = 0; i < names.length; ++i)
      {
         assertFalse(instance.getValueDirect(metaclass.getAttribute(names[i]).getOrdinal()) instanceof Undefined);
      }

      reset();

      instance = query.read().getInstance(0);
      instance.load(Pair.fromArray(Pair.toArray(attributes))); // copy the attributes as load() modifies the list

      for (int i = 0; i < names.length; ++i)
      {
         assertFalse(instance.getValueDirect(metaclass.getAttribute(names[i]).getOrdinal()) instanceof Undefined);
      }

      Metaclass addressMetaclass = (Metaclass)metaclass.getAttribute("addresses").getType();
      Attribute cityAttribute = addressMetaclass.getAttribute("city");

      assertTrue(((InstanceList)instance.getValue("addresses")).getInstance(0).getValueDirect(cityAttribute.getOrdinal()) instanceof Undefined);

      instance.invoke("load", Pair.fromArray(Pair.toArray(parse("((addresses city))"))));

      assertFalse(((InstanceList)instance.getValue("addresses")).getInstance(0).getValueDirect(cityAttribute.getOrdinal()) instanceof Undefined);

      instance = new Instance(metaclass, Instance.NEW, m_context);
      instance.invoke("load", attributes);
   }

   /**
    * The Lock Timeout test takes a very long time to complete (timeout length defined in RDBMS).
    * Hence, do not enable this test in regular test runs.
    */
   public void skipTestLockTimeoutException()
   {
      UnitOfWork[] uowArray = new UnitOfWork[2];

      uowArray[0] = m_context.beginTransaction(false);

      InstanceList list =
         Query.createRead(getMetadata().getMetaclass("Contact"),
                          null, null, null, -1, 0, true, Query.SEC_NONE, m_context).read();

      int nTimeout = 1200; // the query timeout in seconds known to be greater than RDBMS setting
      long nStart = System.currentTimeMillis();

      try
      {
         uowArray[1] = m_context.beginTransaction(false);
         assertTrue(list.size() > 0); // need at least one to place lock on

         Query query = Query.createRead(getMetadata().getMetaclass("Contact"),
                                        null, Pair.attribute("").eq(list.get(0)),
                                        null, -1, 0, true, Query.SEC_NONE, m_context);

         query.setTimeout(nTimeout); // ensure query will terminate eventually
         query.read();
         fail(); // LockTimeoutException expected
      }
      catch (LockTimeoutException e) // exception expected == pass
      {
      }
      finally
      {
         m_context.rollbackAndResume(uowArray[1]);
         m_context.rollbackAndResume(uowArray[0]);
      }

      // assert exception was not forced by query timeout because it took less than timeout
      assertTrue(System.currentTimeMillis() - nStart < nTimeout * 1000);
   }

   public void testMatch() throws Exception
   {
      Pair attributes = parse("(firstName (addresses city))");
      Query query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(and (match? (@ addresses city) \"Toronto\" 0.0) (= classCode \"CON\"))"),
         parse("(((@ addresses city) . #t) ((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCollectionWhere.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(and (match? (@ addresses city) '(and \"Toronto\" \"abc\") 0.0) (= classCode \"CON\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context);
      assertEquals(0, query.read().size());

      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(and (match? (@ addresses city) \"Toronto'\" 0.0) (= classCode \"CON\"))"),
         parse("(((@ addresses city) . #t) ((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      query.read(); // test escape of quotes

      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(and (match? (@ addresses city) '(like? \"Toront\") 0.0) (= classCode \"CON\"))"),
         parse("(((@ addresses city) . #t) ((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCollectionWhere.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(and (match? (@ addresses city) '(and \"Toronto\" (not \"Hill\")) 0.0) (= classCode \"CON\"))"),
         parse("(((@ addresses city) . #t) ((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCollectionWhere.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(and (match? (@ addresses city) '(or \"Toronto\" \"abc\") 0.0) (= classCode \"CON\"))"),
         parse("(((@ addresses city) . #t) ((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCollectionWhere.xml"), query.read(), attributes);

      query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(and (match? (@ addresses city) '(* (1.0 \"Toronto\") (0.0 \"Toronto\")) 0.0) (= classCode \"CON\"))"),
         parse("(((@ addresses city) . #t) ((@) . #t))"), -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("readCollectionWhere.xml"), query.read(), attributes);
   }

   public void testNew()
   {
      Metaclass metaclass = getMetadata().getMetaclass("Principal");
      OID oid = new OID(new Object[]{Binary.parse("00000000000000000000000000000004")});
      Instance principal = m_context.findInstance(metaclass, oid);

      if (principal == null)
      {
         principal = new Instance(metaclass, m_context);
         principal.cache(oid);
      }

      Instance contact = new Instance(getMetadata().getMetaclass("Contact"), Instance.NEW, m_context);

      contact.setValue("firstName", "Ozzy");
      contact.setValue("lastName", "Osbourne");
      contact.setValue("testCategory", "");
      contact.setValue("readPrincipal", principal);
      contact.setValue("version", Primitive.ZERO_INTEGER);

      // Test async invocation on new instances
      getMetadata().getMetaclass("SysQueue").invoke("invoke", new Object[]{contact, Symbol.define("testAsync")});
      getMetadata().getMetaclass("SysQueue").invoke("invoke", new Object[]{contact, Symbol.define("testAsync"), new Object[]{"test"}});

      InstanceList list = (InstanceList)contact.getValue("addresses");
      Instance address = new Instance(getMetadata().getMetaclass("Address"), Instance.NEW, m_context);

      address.setValue("type", "Business");
      address.setValue("country", "USA");

      list.add(address);

      Instance cottage = Query.createRead(getMetadata().getMetaclass("AddressType"),
         parse("(type)"), parse("(and (= type \"Cottage\") (= locale \"en\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read().getInstance(0);

      address = new Instance(getMetadata().getMetaclass("Address"), Instance.NEW, m_context);

      address.setValue("typeEnum", cottage);
      address.setValue("country", "Canada");

      list.add(address);
      contact.setValue("primaryAddress", address);

      contact = new Instance(getMetadata().getMetaclass("Contact"), Instance.NEW, m_context);

      contact.setValue("firstName", "Ronnie");
      contact.setValue("lastName", "Dio");
      contact.setValue("readPrincipal", principal);
      contact.setValue("version", Primitive.ZERO_INTEGER);

      list = (InstanceList)contact.getValue("phones");
      Instance phone = new Instance(getMetadata().getMetaclass("Phone"), Instance.NEW, m_context);

      phone.setValue("type", "Business");
      phone.setValue("number", "(111) 222-3344");

      list.add(phone);
      commit();

      assertEquals(Primitive.ZERO_INTEGER, m_context.getMachine().getGlobalEnvironment().getVariable(Symbol.define("update-count")));
      assertEquals(Primitive.createInteger(2), m_context.getMachine().getGlobalEnvironment().getVariable(Symbol.define("commit-count")));

      assertEquals(7, Query.createRead(getMetadata().getMetaclass("Contact"), null,
         parse("(= classCode \"CON\")"), null, -1, 0, false, Query.SEC_NONE, m_context).read().size());

      assertEquals(2, Query.createRead(getMetadata().getMetaclass("Contact"), null,
         parse("(in? lastName \"Osbourne\" \"Dio\")"), null,
         -1, 0, false, Query.SEC_NONE, m_context).read().size());

      list = Query.createRead(getMetadata().getMetaclass("Contact"), null,
         parse("(= testCategory \"\")"), null,
         -1, 0, false, Query.SEC_NONE, m_context).read();

      assertEquals(1, list.size());
      assertEquals("", list.getInstance(0).getValue("testCategory"));

      assertEquals(8, Query.createRead(getMetadata().getMetaclass("Contact"), null,
         parse("(null? testCategory)"), null, -1, 0, false, Query.SEC_NONE, m_context).read().size());

      assertEquals(1, Query.createRead(getMetadata().getMetaclass("Address"), null,
         parse("(and (= (@ contact firstName) \"Ozzy\") (= country \"USA\"))"), null,
         -1, 0, false, Query.SEC_NONE, m_context).read().size());

      assertEquals(1, Query.createRead(getMetadata().getMetaclass("Address"), null,
         parse("(and (= (@ contact firstName) \"Ozzy\") (= country \"Canada\"))"), null,
         -1, 0, false, Query.SEC_NONE, m_context).read().size());

      assertEquals(1, Query.createRead(getMetadata().getMetaclass("Phone"), null,
         parse("(= number \"(111) 222-3344\")"), null,
         -1, 0, false, Query.SEC_NONE, m_context).read().size());

      Instance ref = new Instance(getMetadata().getMetaclass("PrincipalRef2"), Instance.NEW, m_context);

      ref.setValue("name", "a");
      ref.setValue("count", Primitive.createInteger(3));

      ref = new Instance(getMetadata().getMetaclass("PrincipalRef2"), Instance.NEW, m_context);

      ref.setValue("name", "b");

      commit();

      m_context.removeInstance(ref);

      ref = Query.createRead(getMetadata().getMetaclass("PrincipalRef2"), null,
         parse("(= name \"b\")"), null, -1, 0, false, Query.SEC_NONE, m_context).read().getInstance(0);
      ref.setValue("count", Primitive.createInteger(7));

      commit();

      assertEquals(1, Query.createRead(getMetadata().getMetaclass("PrincipalRef2"), null,
         parse("(and (= name \"a\") (= count 3))"), null,
         -1, 0, false, Query.SEC_NONE, m_context).read().size());

      assertEquals(1, Query.createRead(getMetadata().getMetaclass("PrincipalRef2"), null,
         parse("(and (= name \"b\") (= count 7))"), null,
         -1, 0, false, Query.SEC_NONE, m_context).read().size());

      // KeyGenerator.MaxPlus1 and KeyGenerator.Counter test

      new Instance(getMetadata().getMetaclass("Max1Test"), Instance.NEW, m_context);
      new Instance(getMetadata().getMetaclass("Max1Test"), Instance.NEW, m_context);
      new Instance(getMetadata().getMetaclass("CounterTest"), Instance.NEW, m_context);
      new Instance(getMetadata().getMetaclass("CounterTest"), Instance.NEW, m_context);

      commit();

      assertEquals(2, Query.createRead(getMetadata().getMetaclass("Max1Test"), null,
         null, null, -1, 0, false, Query.SEC_NONE, m_context).read().size());

      assertEquals(2, Query.createRead(getMetadata().getMetaclass("CounterTest"), null,
         null, null, -1, 0, false, Query.SEC_NONE, m_context).read().size());

      // Test non-primitive initializer
      new Instance(getMetadata().getMetaclass("ContactWithInits"), Instance.NEW, m_context);

      commit();
      reset();

      list = Query.createRead(getMetadata().getMetaclass("ContactWithInits"),
         parse("((initAddress city))"), null, null, -1, 0, false, Query.SEC_NONE, m_context).read();

      assertEquals(1, list.size());
      assertEquals("Canada", ((Instance)list.getInstance(0).getValue("initAddress")).getValue("country"));

      // Duplicate unique constraint test
      m_context.setSecure(false);

      Instance wfqueue = new Instance(getMetadata().getMetaclass("SysWorkflowQueue"), Instance.NEW, m_context);

      wfqueue.setOID(new OID(new Object[]{Binary.parse("00000000000000000000000000000001")}));
      wfqueue.setValue("name", "custom");
      wfqueue.setValue("type", "WFLIFOQueue");
      wfqueue.setValue("caption", "LIFO");
      wfqueue.setValue("customized", Boolean.FALSE);

      m_context.setSecure(true);

      try
      {
         commit();
         fail("Expected DuplicateKeyException");
      }
      catch (DuplicateKeyException e)
      {
         assertEquals("err.persistence.duplicateKeyClass", e.getErrorCode());
         assertSame(wfqueue, e.getOIDHolder());
         assertEquals("SysWorkflowQueue", e.getClassName());
      }

      rollback();

      // Duplicate unique index test
      m_context.setSecure(false);

      wfqueue = new Instance(getMetadata().getMetaclass("SysWorkflowQueue"), Instance.NEW, m_context);
      wfqueue.setValue("name", "lifo");
      wfqueue.setValue("type", "WFLIFOQueue");
      wfqueue.setValue("caption", "LIFO");
      wfqueue.setValue("customized", Boolean.FALSE);

      m_context.setSecure(true);

      try
      {
         commit();
         fail("Expected DuplicateKeyException");
      }
      catch (DuplicateKeyException e)
      {
         assertEquals("err.persistence.duplicateKey", e.getErrorCode());
         assertSame(wfqueue, e.getOIDHolder());
         assertEquals("SysWorkflowQueue", e.getClassName());
      }

      rollback();

      // Composite data source
      metaclass = getMetadata().findMetaclass("CompositeVisit");

      if (metaclass != null)
      {
         Instance visit = new Instance(metaclass, Instance.NEW, m_context);

         visit.setValue("startDate", new Timestamp(0));
         visit.setValue("reason", "a");
         visit.setAnnotation(":shard", new Pair("DefaultRelationalDatabase"));

         visit = new Instance(metaclass, Instance.NEW, m_context);

         visit.setValue("startDate", new Timestamp(1000));
         visit.setValue("reason", "b");
         visit.setAnnotation(":shard", new Pair("ExternalRelationalDatabase"));

         commit();

         metaclass.setValue(":shard", new Pair("DefaultRelationalDatabase"));
         list = Query.createRead(metaclass, parse("(startDate endDate requests)"),
            parse("(and (= startDate #m1970-01-01) (= reason \"a\"))"), null, -1, 0,
            false, Query.SEC_NONE, m_context).read();

         assertEquals(1, list.size());

         metaclass.setValue(":shard", new Pair("ExternalRelationalDatabase"));
         list = Query.createRead(metaclass, parse("(startDate endDate requests)"),
            parse("(and (= startDate #m1970-01-01) (= reason \"a\"))"), null, -1, 0,
            false, Query.SEC_NONE, m_context).read();

         assertEquals(0, list.size());

         list = Query.createRead(metaclass, parse("(startDate endDate requests)"),
            parse("(and (= startDate #m1970-01-01T00:00:01) (= reason \"b\"))"), null, -1, 0,
            false, Query.SEC_NONE, m_context).read();

         assertEquals(1, list.size());

         metaclass = getMetadata().getMetaclass("CompositeVisit2");
         metaclass.setValue(":shard", new Pair("DefaultRelationalDatabase"));
         list = Query.createRead(metaclass, parse("(startDate endDate requests)"),
            parse("(and (= startDate #m1970-01-01) (= reason \"a\"))"), null, -1, 0,
            false, Query.SEC_NONE, m_context).read();

         assertEquals(1, list.size());
      }
   }

   public void testQueryTimeoutException() throws Exception // exception needed for MySQL
   {
      UnitOfWork[] uowArray = new UnitOfWork[2];

      uowArray[0] = m_context.beginTransaction(false);

      InstanceList list =
         Query.createRead(getMetadata().getMetaclass("Contact"),
                          null, null, null, -1, 0, true, Query.SEC_NONE, m_context).read();

      try
      {
         uowArray[1] = m_context.beginTransaction(false);
         assertTrue(list.size() > 0); // need at least one to place lock on

         Query query = Query.createRead(getMetadata().getMetaclass("Contact"),
                                        null, Pair.attribute("").eq(list.get(0)),
                                        null, -1, 0, true, Query.SEC_NONE, m_context);

         query.setTimeout(1); // test applications specified query timeout
         query.read();
         fail(); // QueryTimeoutException expected
      }
      catch (QueryTimeoutException e) // exception expected == pass
      {
      }
      finally
      {
         m_context.rollbackAndResume(uowArray[1]);
         m_context.rollbackAndResume(uowArray[0]);
      }
   }

   public void testUpdate()
   {
      // Old value with removal

      InstanceList list = Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(firstName lastName (addresses type typeEnum city))"),
         parse("(and (= lastName \"Test\") (= firstName \"Joe\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();

      assertEquals(1, list.size());

      Instance contact = list.getInstance(0);

      assertEquals("Test", contact.getOldValue("lastName"));
      assertFalse(contact.isDirty("lastName"));
      assertEquals("Test", contact.getPreValue("lastName"));
      assertFalse(contact.isUpdated("lastName"));

      contact.setValue("lastName", "Best");

      assertEquals("Test", contact.getOldValue("lastName"));
      assertTrue(contact.isDirty("lastName"));
      assertEquals("Test", contact.getPreValue("lastName"));
      assertTrue(contact.isUpdated("lastName"));

      assertEquals("CON", contact.getOldValueDirect(contact.getMetaclass().getAttribute("classCode").getOrdinal()));

      contact.setValue("type", null);

      assertEquals("Joe Best {jtest}", contact.getValue("fullName"));
      assertEquals("Joe Test [Employee] {jtest}", contact.getOldValue("fullName"));
      assertFalse(contact.isDirty("fullName"));
      assertEquals("Joe Test [Employee] {jtest}", contact.getPreValue("fullName"));
      assertFalse(contact.isUpdated("fullName"));

      assertEquals(2, ((InstanceList)contact.getValue("addresses")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("addresses")).size());
      assertFalse(contact.isDirty("addresses"));
      assertEquals(2, ((InstanceList)contact.getPreValue("addresses")).size());
      assertFalse(contact.isUpdated("addresses"));

      ((InstanceList)contact.getValue("addresses")).remove(0);

      assertEquals(1, ((InstanceList)contact.getValue("addresses")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("addresses")).size());
      assertTrue(contact.isDirty("addresses"));
      assertEquals(2, ((InstanceList)contact.getPreValue("addresses")).size());
      assertTrue(contact.isUpdated("addresses"));

      Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(firstName lastName (addresses type typeEnum city))"),
         parse("(and (= lastName \"Test\") (= firstName \"Joe\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();

      assertEquals(1, ((InstanceList)contact.getValue("addresses")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("addresses")).size());
      assertTrue(contact.isDirty("addresses"));
      assertEquals(2, ((InstanceList)contact.getPreValue("addresses")).size());
      assertTrue(contact.isUpdated("addresses"));

      contact.setValue("lastUpdated", new Timestamp(3));

      assertSame(Invalid.VALUE,
         contact.getValueDirect(contact.getMetaclass().getAttribute("lastUpdatedMillisecond").getOrdinal()));

      contact.invoke("update");

      assertEquals("Test", contact.getOldValue("lastName"));
      assertTrue(contact.isDirty("lastName"));
      assertEquals("Best", contact.getPreValue("lastName"));
      assertFalse(contact.isUpdated("lastName"));
      assertEquals("Joe Best {jtest}", contact.getValue("fullName"));
      assertEquals("Joe Test [Employee] {jtest}", contact.getOldValue("fullName"));
      assertFalse(contact.isDirty("fullName"));
      assertEquals("Joe Best {jtest}", contact.getPreValue("fullName"));
      assertFalse(contact.isUpdated("fullName"));
      assertEquals(1, ((InstanceList)contact.getValue("addresses")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("addresses")).size());
      assertTrue(contact.isDirty("addresses"));
      assertEquals(1, ((InstanceList)contact.getPreValue("addresses")).size());
      assertFalse(contact.isUpdated("addresses"));

      rollback();

      assertEquals(2, ((InstanceList)contact.getValue("addresses")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("addresses")).size());
      assertFalse(contact.isDirty("addresses"));
      assertEquals(2, ((InstanceList)contact.getPreValue("addresses")).size());
      assertFalse(contact.isUpdated("addresses"));

      ((InstanceList)contact.getValue("addresses")).remove(0);

      assertEquals(1, ((InstanceList)contact.getValue("addresses")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("addresses")).size());
      assertTrue(contact.isDirty("addresses"));
      assertEquals(2, ((InstanceList)contact.getPreValue("addresses")).size());
      assertTrue(contact.isUpdated("addresses"));

      Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(firstName lastName (addresses type typeEnum city))"),
         parse("(and (= lastName \"Test\") (= firstName \"Joe\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();

      assertEquals(1, ((InstanceList)contact.getValue("addresses")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("addresses")).size());
      assertTrue(contact.isDirty("addresses"));
      assertEquals(2, ((InstanceList)contact.getPreValue("addresses")).size());
      assertTrue(contact.isUpdated("addresses"));

      rollback();

      assertEquals(2, ((InstanceList)contact.getValue("addresses")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("addresses")).size());
      assertFalse(contact.isDirty("addresses"));
      assertEquals(2, ((InstanceList)contact.getPreValue("addresses")).size());
      assertFalse(contact.isUpdated("addresses"));

      assertEquals(2, ((InstanceList)contact.getValue("phones")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("phones")).size());

      ((InstanceList)contact.getValue("phones")).remove(0);

      assertEquals(1, ((InstanceList)contact.getValue("phones")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("phones")).size());

      Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(phones)"),
         parse("(and (= lastName \"Test\") (= firstName \"Joe\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();

      assertEquals(1, ((InstanceList)contact.getValue("phones")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("phones")).size());

      rollback();

      assertEquals(2, ((InstanceList)contact.getValue("phones")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("phones")).size());

      ((InstanceList)contact.getValue("phones")).remove(0);

      assertEquals(1, ((InstanceList)contact.getValue("phones")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("phones")).size());

      Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(phones)"),
         parse("(and (= lastName \"Test\") (= firstName \"Joe\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();

      assertEquals(1, ((InstanceList)contact.getValue("phones")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("phones")).size());

      rollback();

      assertEquals(2, ((InstanceList)contact.getValue("phones")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("phones")).size());

      reset();

      // Old value with adding

      list = Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(firstName lastName (addresses type typeEnum city))"),
         parse("(and (= lastName \"Test\") (= firstName \"Joe\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();

      contact = list.getInstance(0);

      assertEquals(2, ((InstanceList)contact.getValue("addresses")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("addresses")).size());

      Instance address = new Instance(getMetadata().getMetaclass("Address"), Instance.NEW, m_context);

      address.setValue("contact", contact);

      assertEquals(3, ((InstanceList)contact.getValue("addresses")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("addresses")).size());
      assertTrue(contact.isDirty("addresses"));
      assertEquals(2, ((InstanceList)contact.getPreValue("addresses")).size());
      assertTrue(contact.isUpdated("addresses"));

      Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(firstName lastName (addresses type typeEnum city))"),
         parse("(and (= lastName \"Test\") (= firstName \"Joe\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();

      assertEquals(3, ((InstanceList)contact.getValue("addresses")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("addresses")).size());
      assertTrue(contact.isDirty("addresses"));
      assertEquals(2, ((InstanceList)contact.getPreValue("addresses")).size());
      assertTrue(contact.isUpdated("addresses"));

      contact.invoke("update");

      assertEquals(3, ((InstanceList)contact.getValue("addresses")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("addresses")).size());
      assertTrue(contact.isDirty("addresses"));
      assertEquals(3, ((InstanceList)contact.getPreValue("addresses")).size());
      assertFalse(contact.isUpdated("addresses"));

      rollback();

      assertEquals(2, ((InstanceList)contact.getValue("addresses")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("addresses")).size());
      assertFalse(contact.isDirty("addresses"));
      assertEquals(2, ((InstanceList)contact.getPreValue("addresses")).size());
      assertFalse(contact.isUpdated("addresses"));

      address = new Instance(getMetadata().getMetaclass("Address"), Instance.NEW, m_context);
      address.setValue("contact", contact);

      assertEquals(3, ((InstanceList)contact.getValue("addresses")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("addresses")).size());
      assertTrue(contact.isDirty("addresses"));
      assertEquals(2, ((InstanceList)contact.getPreValue("addresses")).size());
      assertTrue(contact.isUpdated("addresses"));

      Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(firstName lastName (addresses type typeEnum city))"),
         parse("(and (= lastName \"Test\") (= firstName \"Joe\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();

      assertEquals(3, ((InstanceList)contact.getValue("addresses")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("addresses")).size());
      assertTrue(contact.isDirty("addresses"));
      assertEquals(2, ((InstanceList)contact.getPreValue("addresses")).size());
      assertTrue(contact.isUpdated("addresses"));

      rollback();

      assertEquals(2, ((InstanceList)contact.getValue("addresses")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("addresses")).size());
      assertFalse(contact.isDirty("addresses"));
      assertEquals(2, ((InstanceList)contact.getPreValue("addresses")).size());
      assertFalse(contact.isUpdated("addresses"));

      assertEquals(2, ((InstanceList)contact.getValue("phones")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("phones")).size());

      ((InstanceList)contact.getValue("phones")).remove(0);

      assertEquals(1, ((InstanceList)contact.getValue("phones")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("phones")).size());

      Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(phones)"),
         parse("(and (= lastName \"Test\") (= firstName \"Joe\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();

      assertEquals(1, ((InstanceList)contact.getValue("phones")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("phones")).size());

      rollback();

      assertEquals(2, ((InstanceList)contact.getValue("phones")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("phones")).size());

      Instance phone = new Instance(getMetadata().getMetaclass("Phone"), Instance.NEW, m_context);

      phone.setValue("contact", contact);

      assertEquals(3, ((InstanceList)contact.getValue("phones")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("phones")).size());

      Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(phones)"),
         parse("(and (= lastName \"Test\") (= firstName \"Joe\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();

      assertEquals(3, ((InstanceList)contact.getValue("phones")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("phones")).size());

      rollback();

      assertEquals(2, ((InstanceList)contact.getValue("phones")).size());
      assertEquals(2, ((InstanceList)contact.getOldValue("phones")).size());

      reset();

      // Other tests

      list = Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(firstName lastName (addresses type typeEnum city))"),
         parse("(= lastName \"Test\")"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();

      assertEquals(2, list.size());

      Instance cottage = Query.createRead(getMetadata().getMetaclass("AddressType"),
         parse("(type)"), parse("(and (= type \"Cottage\") (= locale \"en\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read().getInstance(0);

      for (int i = 0; i < list.size(); ++i)
      {
         contact = list.getInstance(i);
         contact.setValue("lastName", "QA");

         InstanceList addrs = (InstanceList)contact.getValue("addresses");

         for (int k = 0; k < addrs.size(); ++k)
         {
            Instance addr = addrs.getInstance(k);

            if ("Home".equals(addr.getValue("type")))
            {
               addr.setValue("type", "Residence");
               contact.setValue("primaryAddress", addr);
            }
            else if (((Instance)addr.getValue("typeEnum")).getValue("type").equals("Business"))
            {
               addr.setValue("typeEnum", cottage);
            }
         }
      }

      commit();
      assertEquals(Primitive.createInteger(2), m_context.getMachine().getGlobalEnvironment().getVariable(Symbol.define("update-count")));
      assertEquals(Primitive.createInteger(2), m_context.getMachine().getGlobalEnvironment().getVariable(Symbol.define("commit-count")));

      Pair attributes = parse("(firstName lastName type (addresses type country city street code) (user name) (primaryAddress type))");
      Pair orderBy = parse("((firstName . #t)(lastName . #t)((@ addresses) . #t))");
      Query query = Query.createRead(getMetadata().getMetaclass("Contact"),
         attributes, parse("(= classCode \"CON\")"), orderBy, -1, 0, false, Query.SEC_NONE, m_context);
      list = query.read();

      AssertUtil.assertEquals(getURL("update.xml"), list, attributes);

      for (Iterator itr = list.iterator(); itr.hasNext();)
      {
         ((Instance)itr.next()).setValue("firstName", "");
      }

      commit();

      for (Iterator itr = list.iterator(); itr.hasNext();)
      {
         ((Instance)itr.next()).setValue("type", null);
      }

      commit();
      reset();

      query = Query.createRead(getMetadata().getMetaclass("Contact"),
         attributes, parse("(= classCode \"CON\")"), orderBy,
         -1, 0, false, Query.SEC_NONE, m_context);
      list = query.read();

      AssertUtil.assertEquals(getURL("updateNull.xml"), list, attributes);

      m_context.setSecure(false);

      query = Query.createRead(getMetadata().getMetaclass("SysRule"), parse("(action)"),
         null, parse("(((@) . #t))"), 1, 0, false, Query.SEC_NONE, m_context);
      list = query.read();

      char[] charArray = new char[20000];

      Arrays.fill(charArray, 'a');

      String sCLOB = new String(charArray);

      list.getInstance(0).setValue("action", sCLOB);

      commit();
      reset();

      assertEquals(sCLOB, query.read().getInstance(0).getValue("action"));
   }

   public void testRanges()
   {
      class RangeInfo
      {
         private String m_sName; 
         private Object m_value;
         private String m_sErrCode;

         public RangeInfo(String sName, Object value, String sErrCode)
         {
            m_sName = sName;
            m_value = value;
            m_sErrCode = sErrCode;
         }

         public void setValue(Instance instance)
         {
            instance.setValue(m_sName, m_value);
         }

         public String getErrorCode()
         {
            return m_sErrCode;
         }

         public void assertException(ValidationException e, Instance instance)
         {
            assertNotNull(m_sErrCode);

            assertSame(e.getOIDHolder(), instance);
            assertEquals(1, e.getAttributeCount());
            assertEquals(m_sName, e.getAttributeIterator().next());

            ValidationException x = (ValidationException)e.findException(m_sName);

            assertNotNull(x);
            assertEquals(m_sErrCode, x.getErrorCode());
            assertSame(x.getOIDHolder(), instance);

            if (getLogger().isDebugEnabled())
            {
               getLogger().debug(x.getMessage());
            }
         }
      }

      RangeInfo[] rangeInfoArray = new RangeInfo[]
      {
         new RangeInfo("s", "1", null),
         new RangeInfo("s", "12", "err.validation.maxDataLength"),
         new RangeInfo("bin", Binary.parse("11"), null),
         new RangeInfo("bin", Binary.parse("1122"), "err.validation.maxDataLength"),
         new RangeInfo("n1", new Integer(1000), "err.validation.numberRange"),
         new RangeInfo("n1", new Integer(256), "err.validation.numberRange"),
         new RangeInfo("n1", new Integer(255), null),
         new RangeInfo("n1", new Integer(0), null),
         new RangeInfo("n1", new Integer(-1), "err.validation.numberRange"),
         new RangeInfo("n2", new Integer(40000), "err.validation.numberRange"),
         new RangeInfo("n2", new Integer(32768), "err.validation.numberRange"),
         new RangeInfo("n2", new Integer(32767), null),
         new RangeInfo("n2", new Integer(-32768), null),
         new RangeInfo("n2", new Integer(-32769), "err.validation.numberRange"),
         new RangeInfo("dec", new BigDecimal("12345678901.234567890"), "err.validation.numberRange"),
         new RangeInfo("dec", new BigDecimal("100000.234567890"), "err.validation.numberRange"),
         new RangeInfo("dec", new BigDecimal("-100000.234567890"), "err.validation.numberRange"),
         new RangeInfo("dec", new BigDecimal("99999.99999"), null),
         new RangeInfo("dec", new BigDecimal("-99999.99999"), null),
         new RangeInfo("dec", new BigDecimal("99999.999991"), null),
         new RangeInfo("dec", new BigDecimal("-99999.999991"), null),
         new RangeInfo("dec", new BigDecimal("99999.999999"), "err.validation.numberRange"),
         new RangeInfo("dec", new BigDecimal("-99999.999999"), "err.validation.numberRange"),
         new RangeInfo("tm", new Timestamp(0x8000000000000000L), "err.validation.dateRange"),
         new RangeInfo("tm", new Timestamp(0x7FFFFFFFFFFFFFFFL), "err.validation.dateRange"),
         new RangeInfo("tm", new Timestamp(0), null),
      };

      Metaclass metaclass = m_context.getMetadata().getMetaclass("RangeTest");

      for (int i = 0; i < rangeInfoArray.length; ++i)
      {
         RangeInfo info = rangeInfoArray[i];
         Instance instance = new Instance(metaclass, Instance.NEW, m_context);

         info.setValue(instance);

         try
         {
            commit();

            if (info.getErrorCode() != null)
            {
               fail("Expected exception");
            }
         }
         catch (ValidationException e)
         {
            info.assertException(e, instance);
         }
         catch (Exception t)
         {
            fail("Unexpected exception " + t.getClass().getName());
         }
         finally
         {
            reset();
         }
      }

      Instance instance = new Instance(metaclass, Instance.NEW, m_context);

      commit();

      for (int i = 0; i < rangeInfoArray.length; ++i)
      {
         RangeInfo info = rangeInfoArray[i];

         info.setValue(instance);

         try
         {
            commit();

            if (info.getErrorCode() != null)
            {
               fail("Expected exception");
            }
         }
         catch (ValidationException e)
         {
            info.assertException(e, instance);
         }
         catch (Exception t)
         {
            fail("Unexpected exception " + t.getClass().getName());
         }
         finally
         {
            rollback();
         }
      }
   }

   public void testInvalidation()
   {
      Instance address = Query.createRead(getMetadata().getMetaclass("Address"),
         null, parse("(= street \"100 Front St E\")"), null,
         -1, 0, false, Query.SEC_NONE, m_context).read().getInstance(0);

      address.setValue("type", "Home");
      commit();
      reset();

      Instance contact = Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(businessAddressCount)"), parse("(and (= firstName \"Joe\") (= lastName \"Test\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read().getInstance(0);

      assertEquals(Primitive.ZERO_INTEGER, contact.getValue("businessAddressCount"));
      reset();

      address = Query.createRead(getMetadata().getMetaclass("Address"),
         null, parse("(= street \"100 Front St E\")"), null,
         -1, 0, false, Query.SEC_NONE, m_context).read().getInstance(0);

      address.setValue("type", "Business");
      commit();
      reset();

      contact = Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(businessAddressCount)"), parse("(and (= firstName \"Joe\") (= lastName \"Test\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read().getInstance(0);

      assertEquals(Primitive.ONE_INTEGER, contact.getValue("businessAddressCount"));
      reset();

      address = Query.createRead(getMetadata().getMetaclass("Address"),
         null, parse("(= street \"100 Front St E\")"), null,
         -1, 0, false, Query.SEC_NONE, m_context).read().getInstance(0);
      address.invoke("delete");
      commit();
      reset();

      contact = Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(businessAddressCount)"), parse("(and (= firstName \"Joe\") (= lastName \"Test\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read().getInstance(0);

      assertEquals(Primitive.ZERO_INTEGER, contact.getValue("businessAddressCount"));
   }

   public void testConcurrentResultSets()
   {
      Query query = Query.createRead(getMetadata().getMetaclass("Contact"), parse("(firstName)"),
                                     null, null, -1, 0, false, Query.SEC_NONE, m_context);
      Cursor cursor1 = query.openCursor();
      Cursor cursor2 = query.openCursor();

      assertTrue(cursor2.next(-1).size() > 0);
      assertTrue(cursor1.next(-1).size() > 0);
      cursor1.close();
      cursor2.close();
   }

   public void testDelete()
   {
      m_context.requireTransaction();

      InstanceList list = Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(firstName lastName)"),
         parse("(and (= firstName \"Joe\") (= lastName \"Test\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read();

      assertEquals(1, list.size());

      Instance instance = list.getInstance(0);

      list = (InstanceList)instance.getValue("addresses");
      instance.setDeleted();

      for (int i = 0; i < list.size(); ++i)
      {
         list.getInstance(i).setDeleted();
      }

      commit();

      assertEquals(Primitive.ZERO_INTEGER, m_context.getMachine().getGlobalEnvironment().getVariable(Symbol.define("update-count")));
      assertEquals(Primitive.ZERO_INTEGER, m_context.getMachine().getGlobalEnvironment().getVariable(Symbol.define("commit-count")));

      Pair attributes = parse("(firstName lastName (addresses country city street code) (user name))");
      Query query = Query.createRead(getMetadata().getMetaclass("Contact"),
         attributes, parse("(= classCode \"CON\")"),
         parse("((firstName . #t)(lastName . #t)((@ addresses) . #t))"),
         -1, 0, false, Query.SEC_NONE, m_context);
      AssertUtil.assertEquals(getURL("delete.xml"), query.read(), attributes);

      list = Query.createRead(getMetadata().getMetaclass("User"), parse("(ugassocs)"),
         parse("(= name \"jsmith\")"), null, -1, 0, false, Query.SEC_NONE, m_context).read();

      assertEquals(1, list.size());

      for (Iterator itr = ((InstanceList)list.getInstance(0).getValue("ugassocs")).iterator(); itr.hasNext();)
      {
         ((Instance)itr.next()).delete();
      }

      commit();

      // cached delete

      Metaclass country = getMetadata().getMetaclass("Country");
      int nCountryCount = Query.createRead(country, null, null, null, -1, 0,
         false, Query.SEC_NONE, m_context).read().size();

      instance = new Instance(country, Instance.NEW, m_context);
      instance.setValue("name", "test");
      commit();

      assertEquals(nCountryCount + 1, Query.createRead(country, null, null, null, -1, 0,
         false, Query.SEC_NONE, m_context).read().size());

      instance.delete();
      commit();

      assertEquals(nCountryCount, Query.createRead(country, null, null, null, -1, 0,
         false, Query.SEC_NONE, m_context).read().size());
   }

   public void testSharedLocking()
   {
      m_context.requireTransaction();
      m_context.getUnitOfWork().setLocking(true);

      Pair attributes = parse("(fullName (addresses type city))");
      Pair where = parse("(= classCode \"CON\")");
      Pair orderBy = parse("(((@) . #t)((@ addresses) . #t))");
      Query query = Query.createRead(getMetadata().getMetaclass("Contact"),
         attributes, where, orderBy, -1, 0, false, Query.SEC_NONE, m_context);
      InstanceList list = query.read();

      assertEquals(5, list.size());

      for (int i = 0; i != list.size(); ++i)
      {
         assertTrue(m_context.isLocked(list.getInstance(i)));
      }

      execute("upd_addr");

      Query.createRead(getMetadata().getMetaclass("Address"), parse("(contact type city)"),
         null, null, -1, 0, false, Query.SEC_NONE, m_context).read();

      AssertUtil.assertEquals(getURL("shlockUpdPes1.xml"), list, attributes);

      Query.createRead(getMetadata().getMetaclass("Contact"),
         attributes, where, orderBy, -1, 0, false, Query.SEC_NONE, m_context).read();

      InstanceList addresses = (InstanceList)list.getInstance(0).getValue("addresses");

      AssertUtil.assertEquals(getURL("shlockUpdPes2.xml"), list, attributes);

      execute("upd_cont");

      try
      {
         Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
            where, orderBy, -1, 0, false, Query.SEC_NONE, m_context).read();
         fail("Expected OptimisticLockException");
      }
      catch (OptimisticLockException e)
      {
      }

      m_context.getUnitOfWork().unlock(list.getInstance(0));
      assertFalse(m_context.isLocked(list.getInstance(0)));

      Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         where, orderBy, -1, 0, false, Query.SEC_NONE, m_context).read();

      assertSame(addresses, list.getInstance(0).getValue("addresses"));
      assertTrue(m_context.isLocked(list.getInstance(0)));

      AssertUtil.assertEquals(getURL("shlockUpdOpt.xml"), list, attributes);
   }

   public void testRulesEngine()
   {
      // ((Patient'new '(gender . "M") `(type . ,((ContactType'read '(type) '(= type "Company") '() -1 0 #f)'get 0)))))'icon)

      assertEquals("icon-c",
         ((Instance)getMetadata().getMetaclass("Patient").invoke("new",
            new Object[]{new Pair(Symbol.define("gender"), "M"), new Pair(Symbol.define("type"),
               ((InstanceList)getMetadata().getMetaclass("ContactType").invoke("read",
               new Object[]{parse("(type)"), parse("(= type \"Company\")"), null,
                  new Integer(-1), new Integer(0), Boolean.FALSE})).get(0))})).getValue("icon"));

      assertEquals("icon-m",
         ((Instance)getMetadata().getMetaclass("Patient").invoke("new",
            new Object[]{new Pair(Symbol.define("gender"), "M"), new Pair(Symbol.define("type"),
               ((InstanceList)getMetadata().getMetaclass("ContactType").invoke("read",
               new Object[]{parse("(type)"), parse("(= type \"Person\")"), null,
                  new Integer(-1), new Integer(0), Boolean.FALSE})).get(0))})).getValue("icon"));

      assertEquals("icon-c",
         ((Instance)getMetadata().getMetaclass("Patient").invoke("new",
            new Object[]{new Pair(Symbol.define("gender"), "H"), new Pair(Symbol.define("type"),
               ((InstanceList)getMetadata().getMetaclass("ContactType").invoke("read",
               new Object[]{parse("(type)"), parse("(= type \"Company\")"), null,
                  new Integer(-1), new Integer(0), Boolean.FALSE})).get(0))})).getValue("icon"));
   }

   /**
    * test m_bDenorm in SQLUpdate gets set properly for each instance of the batch
    * (need at least 2 instances in batch for test to work)
    * originally only occurred if SQLAdapter did not support "NoRowsBlock" functionality
    */
   public void testSQLUpdateDenorm()
   {
      Instance principal1 = new Instance(getMetadata().getMetaclass("PrincipalRef"), Instance.NEW, m_context);
      Instance principal2 = new Instance(getMetadata().getMetaclass("PrincipalRef"), Instance.NEW, m_context);

      principal1.setValue("name", "test1");
      principal2.setValue("name", "test2");
      commit();

      principal1.setValue("count", new Integer(1));
      principal2.setValue("count", new Integer(2));
      commit();
   }

   public void testTransactions()
   {
      Metaclass contact = getMetadata().getMetaclass("Contact");

      contact.invoke("testTxSupported", new Object[]{null, Boolean.TRUE});

      assertEquals(1, m_context.getUnitOfWorkCount());
      assertNotNull(m_context.getUnitOfWork().getTransaction());
      assertEquals(Status.STATUS_ACTIVE, m_context.getUnitOfWork().getStatus());
      m_context.commitAndResume(null);
      assertEquals(1, m_context.getUnitOfWorkCount());
      assertNull(m_context.getUnitOfWork().getTransaction());
      assertEquals(Status.STATUS_NO_TRANSACTION, m_context.getUnitOfWork().getStatus());
      m_context.complete(true);
      assertEquals(0, m_context.getUnitOfWorkCount());
      assertEquals(1, Query.createRead(contact, null, parse("(and (= firstName \"tx\") (= lastName \"new\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read().size());
      assertEquals(0, Query.createRead(contact, null, parse("(and (= firstName \"tx\") (= lastName \"error\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context).read().size());
   }

   public void testSchemaUnicodeValidation() throws Exception
   {
      RelationalSchema schema = (RelationalSchema)m_database.getSchema();
      Table origVersionTable = schema.getVersionTable(); // note original
      Table versionTable = schema.getTable("Version");
      RelationalDatabase origDS = (RelationalDatabase)schema.getDataSource();
      RelationalDatabase ds = (RelationalDatabase)origDS.clone();

      try
      {
         schema.setDataSource(ds);
         schema.setVersionTable(versionTable);

         try
         {
            m_adapter.getVersion(schema); // one of getVersion() has to throw exception
            ds.setUnicode(!ds.isUnicode()); // try the oposite Unicode flag
            m_adapter.getVersion(schema); // one of getVersion() has to throw exception
            fail("Unicode flag validation exception expected.");
         }
         catch (UncheckedException e)
         {
            // test online upgrade prevention
            assertEquals("err.persistence.sql.unicodeMismatch", e.getErrorCode());
         }

         // ensure correct result was provided as per the created schema
         SQLConnection con = m_adapter.getConnection();
         Statement stmt = null;
         ResultSet rs = null;

         try
         {
            stmt = con.getConnection().createStatement();
            rs = stmt.executeQuery("select namespace from " +
               m_adapter.createSchemaManager().getTableName(versionTable));

            // 1 == the namespace column
            assertEquals(Boolean.valueOf(origDS.isUnicode()), m_adapter.isUnicode(schema, rs, 1));
         }
         finally
         {
            try
            {
               if (rs != null)
               {
                  rs.close();
               }
            }
            catch (Throwable t)
            {}

            try
            {
               if (stmt != null)
               {
                  stmt.close();
               }
            }
            catch (Throwable t)
            {}

            con.decRef();
         }

         StringWriter writer = new StringWriter();
         SQLSchemaManager manager = m_adapter.createSchemaManager();

         manager.setSQLAppender(manager.new SQLWriterAppender(writer));
         manager.upgrade(schema);

         // test off-line upgrade script guard generation
         assertTrue(writer.toString().contains(getUnicodeCheckGuard(ds.isUnicode())));

         ds.setUnicode(!ds.isUnicode()); // try the oposite Unicode flag
         writer = new StringWriter();
         manager.setSQLAppender(manager.new SQLWriterAppender(writer));
         manager.upgrade(schema);

         // test offline upgrade script guard generation
         assertTrue(writer.toString().contains(getUnicodeCheckGuard(ds.isUnicode())));
      }
      finally
      {
         schema.setVersionTable(origVersionTable); // reset so testUpgrade() will not fail
         schema.setDataSource(origDS); // reset to original
      }
   }

   public void testWorkflow()
   {
      Metaclass reqClass = getMetadata().getMetaclass("HRRequest");
      Instance req = (Instance)reqClass.invoke("new",
         new Object[]
         {
            new Pair(Symbol.define("start"), Primitive.toTimestamp("2000-01-01 00:00:00")),
            new Pair(Symbol.define("end"), Primitive.toTimestamp("2000-02-01 00:00:00")),
            new Pair(Symbol.define("applicant"), m_context.getUser())
         });
      Metaclass workflowClass = getMetadata().getMetaclass(Metadata.WORKFLOW_CLASS_NAME);
      InstanceList workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{req});

      assertEquals(1, workflowList.size());

      commit();

      Pair attributes = parse("(name version class serializedState serializedVariables (assignments ordinal caption assignee manual (nextStates ordinal caption)) (timers ordinal))");
      Pair orderBy = parse("(((@ assignments ordinal) . #t) ((@ assignments nextStates ordinal) . #t))");

      m_context.setSecure(false);
      AssertUtil.assertEqualsIgnoreOIDs(getURL("workflow-HRA1.xml"),
         Query.createRead(workflowClass, attributes, null, orderBy,
            -1, 0, false, Query.SEC_NONE, m_context).read(), attributes);
      m_context.setSecure(true);

      req.setValue("approved", Boolean.TRUE);

      commit();

      m_context.setSecure(false);
      workflowList = Query.createRead(workflowClass, attributes, null, orderBy,
         -1, 0, false, Query.SEC_NONE, m_context).read();

      AssertUtil.assertEqualsIgnoreOIDs(getURL("workflow-HRA2.xml"), workflowList, attributes);

      Instance wfassignment = ((InstanceList)workflowList.getInstance(0).getValue("assignments")).getInstance(0);

      wfassignment.setValue("nextState", ((InstanceList)wfassignment.getValue("nextStates")).getInstance(0));

      wfassignment = ((InstanceList)workflowList.getInstance(0).getValue("assignments")).getInstance(1);;
      wfassignment.setValue("nextState", ((InstanceList)wfassignment.getValue("nextStates")).getInstance(0));
      m_context.setSecure(true);

      commit();

      m_context.setSecure(false);
      workflowList = Query.createRead(workflowClass, attributes, null, orderBy,
         -1, 0, false, Query.SEC_NONE, m_context).read();
      m_context.setSecure(true);

      AssertUtil.assertEqualsIgnoreOIDs(getURL("workflow-HRA3.xml"), workflowList, attributes);
   }

   public void testWorkflow2()
   {
      Metaclass reqClass = getMetadata().getMetaclass("HRRequest");
      Instance req = (Instance)reqClass.invoke("new",
         new Object[]
         {
            new Pair(Symbol.define("start"), Primitive.toTimestamp("2000-01-01 00:00:00")),
            new Pair(Symbol.define("end"), Primitive.toTimestamp("2000-02-01 00:00:00")),
            new Pair(Symbol.define("applicant"), m_context.getUser())
         });

      Metaclass workflowClass = getMetadata().getMetaclass(Metadata.WORKFLOW_CLASS_NAME);
      InstanceList workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{req});

      assertEquals(1, workflowList.size());

      m_context.setSecure(false);

      for (int i = workflowList.size() - 1; i >= 0; --i)
      {
         workflowList.getInstance(i).invoke("delete");
      }

      m_context.setSecure(true);

      workflowClass.invoke("start", new Object[]{req, "HolidayRequest_C", Boolean.FALSE});

      commit();

      Pair attributes = parse("(name version class serializedState serializedVariables (assignments ordinal caption assignee manual (nextStates ordinal caption)) (timers ordinal))");
      Pair orderBy = parse("(((@ assignments ordinal) . #t) ((@ assignments nextStates ordinal) . #t) ((@ timers ordinal) . #t))");

      m_context.setSecure(false);
      AssertUtil.assertEqualsIgnoreOIDs(getURL("workflow-HRC1.xml"),
         Query.createRead(workflowClass, attributes, null, orderBy,
            -1, 0, false, Query.SEC_NONE, m_context).read(), attributes);
      m_context.setSecure(true);

      req.setValue("approved", Boolean.TRUE);

      commit();

      m_context.setSecure(false);
      workflowList = Query.createRead(workflowClass, attributes, null, orderBy,
         -1, 0, false, Query.SEC_NONE, m_context).read();

      AssertUtil.assertEqualsIgnoreOIDs(getURL("workflow-HRC2.xml"), workflowList, attributes);

      Instance wfassignment = ((InstanceList)workflowList.getInstance(0).getValue("assignments")).getInstance(0);

      wfassignment.setValue("nextState", ((InstanceList)wfassignment.getValue("nextStates")).getInstance(0));

      wfassignment = ((InstanceList)workflowList.getInstance(0).getValue("assignments")).getInstance(1);;
      wfassignment.setValue("nextState", ((InstanceList)wfassignment.getValue("nextStates")).getInstance(0));
      m_context.setSecure(true);

      commit();

      m_context.setSecure(false);
      workflowList = Query.createRead(workflowClass, attributes, null, orderBy,
         -1, 0, false, Query.SEC_NONE, m_context).read();
      m_context.setSecure(true);

      AssertUtil.assertEqualsIgnoreOIDs(getURL("workflow-HRC3.xml"), workflowList, attributes);

      req.setValue("comment", "");
      workflowClass.invoke("start", new Object[]{req, "Exception", Boolean.FALSE});

      assertEquals("done", req.getValue("comment"));

      // Test startWorkflow static method
      req.setValue("comment", "");
      commit();
      m_context.setSecure(false);
      workflowClass.invoke("startWorkflow", new Object[]{"Exception", "HRRequest", "(= comment \"\")" , Boolean.FALSE});
      m_context.setSecure(true);
      assertEquals("done", req.getValue("comment"));

      req.setValue("comment", "");
      commit();
      m_context.setSecure(false);
      workflowClass.invoke("startWorkflow", new Object[]{"Exception", "HRRequest", Pair.attribute("comment").eq(""), Boolean.FALSE});
      m_context.setSecure(true);
      assertEquals("done", req.getValue("comment"));
   }

   public void testService()
   {
      Metaclass serviceClass = getMetadata().getMetaclass(Metadata.SERVICE_CLASS_NAME);
      TransferObject tobj = new TransferObject();

      tobj.setValue("ego", "!");

      Instance instance = (Instance)serviceClass.invoke("invoke", new Object[]{"Counter", tobj, null, new Integer(5)});

      assertEquals(Boolean.TRUE, instance.invoke("done"));

      TransferObject result = (TransferObject)instance.invoke("result");

      assertNotSame(tobj, result);
      assertEquals(3, result.getValueCount());
      assertEquals("~boo~", result.getValue("boo"));
      assertEquals("!+++++", result.getValue("ego"));
      assertEquals(new Integer(5), result.getValue("iterations"));

      State state = (State)instance.getValue("state");

      assertTrue(state.isFinal());
      assertNull(state.getValue(Service.OUTPUT));
      assertEquals(new Integer(5), state.getValue("i"));
      assertEquals(new Integer(5), state.getValue("count"));

      assertEquals(Instance.DELETED, instance.getState());

      commit();

      instance = (Instance)serviceClass.invoke("invoke", new Object[]{"Jump", "jmp", null});
      assertEquals("yo! jmp", instance.invoke("result"));

      commit();

      // Test Loops.service, which uses the Loop service element.
      ArrayList matrixRowList = new ArrayList(2);
      ArrayList matrixRow = new ArrayList(3);

      tobj = new TransferObject(2);
      tobj.setValue("data", matrixRowList);

      matrixRowList.add(matrixRow);
      matrixRow.add(Primitive.createInteger(8));
      matrixRow.add(Primitive.createInteger(7));
      matrixRow.add(Primitive.createInteger(6));
      matrixRowList.add(matrixRow = new ArrayList(2));
      matrixRow.add(Primitive.createInteger(5));
      matrixRow.add(Primitive.createInteger(4));

      instance = (Instance)serviceClass.invoke("invoke", new Object[]{"Loops", tobj, null, Primitive.createInteger(2)});
      result = (TransferObject)instance.invoke("result");
      assertEquals(Primitive.createInteger(64), ((List)((List)result.getValue("data")).get(0)).get(0));
      assertEquals(Primitive.createInteger(49), ((List)((List)result.getValue("data")).get(0)).get(1));
      assertEquals(Primitive.createInteger(36), ((List)((List)result.getValue("data")).get(0)).get(2));
      assertEquals(Primitive.createInteger(25), ((List)((List)result.getValue("data")).get(1)).get(0));
      assertEquals(1, ((List)((List)result.getValue("data")).get(1)).size());

      commit();
   }

   public void testUnicodeCharset()
   {
      final String FIRST_NAME = new String(new char[]{(char)26222, (char)29983});
      final String LAST_NAME = "\u9053";

      if (!m_adapter.isUnicode())
      {
         return;
      }

      Metaclass metaclass = getMetadata().getMetaclass("Principal");
      OID oid = new OID(new Object[]{Binary.parse("00000000000000000000000000000004")});
      Instance principal = m_context.findInstance(metaclass, oid);

      if (principal == null)
      {
         principal = new Instance(metaclass, m_context);
         principal.cache(oid);
      }

      Instance contact = new Instance(getMetadata().getMetaclass("Contact"), Instance.NEW, m_context);

      contact.setValue("firstName", FIRST_NAME);
      contact.setValue("lastName", LAST_NAME);
      contact.setValue("readPrincipal", principal);
      contact.setValue("version", Primitive.ZERO_INTEGER);
      commit();

      InstanceList resultList;

      resultList = Query.createRead(getMetadata().getMetaclass("Contact"), null,
         parse("(= lastName \"" + LAST_NAME.charAt(0) + "\")"), null,
         -1, 0, false, Query.SEC_NONE, m_context).read();
      assertEquals(1, resultList.size());
      assertEquals(FIRST_NAME, resultList.getInstance(0).getValue("firstName"));

      resultList = Query.createRead(getMetadata().getMetaclass("Contact"), null,
         parse("(like? firstName \"" + FIRST_NAME.charAt(0) + "*\")"), null,
         -1, 0, false, Query.SEC_NONE, m_context).read();
      assertEquals(1, resultList.size());
      assertEquals(FIRST_NAME, resultList.getInstance(0).getValue("firstName"));
      assertEquals(LAST_NAME, resultList.getInstance(0).getValue("lastName"));

      resultList = Query.createRead(getMetadata().getMetaclass("Contact"), null,
         parse("(like? firstName \"*" + FIRST_NAME.charAt(0) + "*\")"), null,
         -1, 0, false, Query.SEC_NONE, m_context).read();
      assertEquals(1, resultList.size());
      assertEquals(FIRST_NAME, resultList.getInstance(0).getValue("firstName"));
      assertEquals(LAST_NAME, resultList.getInstance(0).getValue("lastName"));

      resultList = Query.createRead(getMetadata().getMetaclass("Contact"), null,
         parse("(like? firstName \"*" + FIRST_NAME.charAt(1) + "*\")"), null,
         -1, 0, false, Query.SEC_NONE, m_context).read();
      assertEquals(1, resultList.size());
      assertEquals(FIRST_NAME, resultList.getInstance(0).getValue("firstName"));
      assertEquals(LAST_NAME, resultList.getInstance(0).getValue("lastName"));
   }

   /**
    * Tests that the finally script for a TryCatch gets executed when an unhandled
    * exception is thrown on a different branch of the Fork/Join.
    */
   public void testWorkflowTryFinallyInFork() throws Exception
   {
      Metaclass assignmentClass = getMetadata().getMetaclass("SysWorkflowAssignment");
      InstanceList assignmentList;
      Metaclass primaryClass = getMetadata().getMetaclass("WorkflowTrace");
      Instance primary = (Instance)primaryClass.invoke("new");

      // Test 1: No exception
      primary.setValue("trace", null);
      primary.setValue("throwUncaughtEx", Boolean.FALSE);
      primary.setValue("throwInnerEx", Boolean.FALSE);
      primary.invoke("goForkTry");
      assignmentList = Query.createRead(assignmentClass, null, Boolean.TRUE, null, -1, 0, false, Query.SEC_NONE, m_context).read();
      assertEquals(1, assignmentList.size());
      primary.invoke("resumeForkTry");
      commit();
      assertTrue("scrStart;scrLeft1;scrLeft2;scrLeft3;scrRight1;scrLeft4;scrRight2;scrRight3;scrInnerFinally;scrOuterFinally;scrFinish;".equals(primary.getValue("trace")) ||
                 "scrStart;scrLeft1;scrLeft2;scrLeft3;scrLeft4;scrRight1;scrRight2;scrRight3;scrInnerFinally;scrOuterFinally;scrFinish;".equals(primary.getValue("trace")));
      assignmentList = Query.createRead(assignmentClass, null, Boolean.TRUE, null, -1, 0, false, Query.SEC_NONE, m_context).read();
      assertEquals(0, assignmentList.size());


      // Test 2: Caught Exception
      primary.setValue("trace", null);
      primary.setValue("throwUncaughtEx", Boolean.FALSE);
      primary.setValue("throwInnerEx", Boolean.TRUE);
      primary.invoke("goForkTry");
      commit();

      // If DEBUG log mode, then State gets sorted so that left branch of Fork/Join is preferred. This prevents
      // entry to right branch's Try block. Thus the right hand (inner) Finally may not be executed.
      assertTrue("scrStart;scrLeft1;scrLeft2;scrLeft3;scrRight1;scrLeft4;scrInnerFinally;scrCatch;scrOuterFinally;scrFinish;".equals(primary.getValue("trace")) ||
                 "scrStart;scrLeft1;scrLeft2;scrLeft3;scrLeft4;scrCatch;scrOuterFinally;scrFinish;".equals(primary.getValue("trace")));
      assignmentList = Query.createRead(assignmentClass, null, Boolean.TRUE, null, -1, 0, false, Query.SEC_NONE, m_context).read();
      assertEquals(0, assignmentList.size());

      // Test 3: Uncaught Exception
      primary.setValue("trace", null);
      primary.setValue("throwUncaughtEx", Boolean.TRUE);
      primary.setValue("throwInnerEx", Boolean.FALSE);

      try
      {
         primary.invoke("goForkTry");
         fail("Exception not thrown");
      }
      catch (ScriptingException ex)
      {

          // If DEBUG log mode, then State gets sorted so that left branch of Fork/Join is preferred. This prevents
          // entry to right branch's Try block. Thus the right hand (inner) Finally may not be executed.
         assertTrue("scrStart;scrLeft1;scrLeft2;scrLeft3;scrRight1;scrLeft4;scrInnerFinally;scrOuterFinally;".equals(primary.getValue("trace")) ||
                    "scrStart;scrLeft1;scrLeft2;scrLeft3;scrLeft4;scrOuterFinally;".equals(primary.getValue("trace")));
         assignmentList = Query.createRead(assignmentClass, null, Boolean.TRUE, null, -1, 0, false, Query.SEC_NONE, m_context).read();
         assertEquals(0, assignmentList.size());
         rollback();
      }
   }

   /**
    * Tests the Finally part of a Workflow Try block.
    */
   public void testWorkflowTryFinally() throws Exception
   {
      Metaclass primaryClass = getMetadata().getMetaclass("WorkflowTrace");
      Instance primary = (Instance)primaryClass.invoke("new");

      primary.invoke("go");
      commit();
      assertEquals("THROWBLOCK;END_THROWBLOCK;F_INNER;F_OUTER;", primary.getValue("trace"));


      primary.setValue("throwInnerEx", Boolean.TRUE);
      primary.setValue("trace", null);
      primary.invoke("go");
      commit();
      assertEquals("THROWBLOCK;CAUGHT_INNER;F_INNER;F_OUTER;", primary.getValue("trace"));


      primary.setValue("throwInnerEx", Boolean.FALSE);
      primary.setValue("throwOuterEx", Boolean.TRUE);
      primary.setValue("trace", null);
      primary.invoke("go");
      commit();
      assertEquals("THROWBLOCK;F_INNER;CAUGHT_OUTER;F_OUTER;", primary.getValue("trace"));


      primary.setValue("throwOuterEx", Boolean.FALSE);
      primary.setValue("throwUncaughtEx", Boolean.TRUE);
      primary.setValue("trace", null);

      try
      {
         primary.invoke("go");
         fail();
      }
      catch (ScriptingException ex)
      {
         assertEquals("THROWBLOCK;F_INNER;F_OUTER;", primary.getValue("trace"));
         rollback();
      }


      // An exception whose handler re-enters a try block
      primary.setValue("throwUncaughtEx", Boolean.FALSE);
      primary.setValue("throwOuterEx", Boolean.FALSE);
      primary.setValue("throwOuter2Ex", Boolean.TRUE);
      primary.setValue("trace", null);
      primary.invoke("go");
      commit();
      assertEquals("THROWBLOCK;F_INNER;CAUGHT_OUTER2;RUNNING_IN_OUTER_TRY;END_IN_OUTER_TRY;F_OUTER;", primary.getValue("trace"));
   }

   /**
    * Tests that a Workflow Semaphore handles normal termination and exceptions
    * appropriately.
    */
   public void testWorkflowSemaphoreFlowControl() throws Exception
   {
      Metaclass workflowClass = getMetadata().getMetaclass(Metadata.WORKFLOW_CLASS_NAME);
      Metaclass primaryClass = getMetadata().getMetaclass("WorkflowTrace");
      Metaclass schedulerClass = getMetadata().getMetaclass("TestSysWorkflowSchedulerBatchJob");
      Instance primary = (Instance)primaryClass.invoke("new");
      InstanceList workflowList, assignmentList;
      Instance workflow, assignment;

      // Set scheduler time outside of the queue time window
      schedulerClass.setValue("time", Primitive.toTimestamp(Primitive.createInteger(15000)));


      // Test: no exception
      primary.setValue("trace", null);
      primary.setValue("throwOuterEx", Boolean.FALSE);
      primary.setValue("throwUncaughtEx", Boolean.FALSE);
      primary.invoke("goSemaphore");
      assertEquals("BEFORE;ASSIGNMENT_CREATE;", primary.getValue("trace"));
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Boolean.TRUE, assignment.getValue("semaphore"));
      assertEquals(Primitive.ZERO_INTEGER, assignment.getValue("status"));
      commit();
      assignment.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals("BEFORE;ASSIGNMENT_CREATE;EXECUTE_SEMAPHORE;END_SEMAPHORE;ASSIGNMENT_DELETE;AFTER;", primary.getValue("trace"));
      commit();



      // Test: caught exception
      primary.setValue("throwOuterEx", Boolean.TRUE);
      primary.setValue("throwUncaughtEx", Boolean.FALSE);
      primary.setValue("trace", null);
      primary.invoke("goSemaphore");
      assertEquals("BEFORE;ASSIGNMENT_CREATE;", primary.getValue("trace"));
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Boolean.TRUE, assignment.getValue("semaphore"));
      assertEquals(Primitive.ZERO_INTEGER, assignment.getValue("status"));
      commit();
      assignment.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals(Primitive.createInteger(2), assignment.getValue("status"));
      assertEquals("BEFORE;ASSIGNMENT_CREATE;EXECUTE_SEMAPHORE;ASSIGNMENT_DELETE;CAUGHT_OUTER;", primary.getValue("trace"));
      commit();


      // Test: uncaught exception
      primary.setValue("throwOuterEx", Boolean.FALSE);
      primary.setValue("throwUncaughtEx", Boolean.TRUE);
      primary.setValue("trace", null);
      primary.invoke("goSemaphore");
      assertEquals("BEFORE;ASSIGNMENT_CREATE;", primary.getValue("trace"));
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Boolean.TRUE, assignment.getValue("semaphore"));
      assertEquals(Primitive.ZERO_INTEGER, assignment.getValue("status"));
      commit();

      try
      {
         assignment.invoke("schedulerRun");
         fail();
      }
      catch (ScriptingException ex)
      {
         assertEquals(Instance.DELETED, assignment.getState());
         assertEquals(Primitive.createInteger(2), assignment.getValue("status"));
         assertEquals("BEFORE;ASSIGNMENT_CREATE;EXECUTE_SEMAPHORE;ASSIGNMENT_DELETE;", primary.getValue("trace"));
         workflow.invoke("delete");
         commit();
      }
   }

   public void testWorkflowSemaphoresInFork() throws Exception
   {
      Metaclass workflowClass = getMetadata().getMetaclass(Metadata.WORKFLOW_CLASS_NAME);
      Metaclass primaryClass = getMetadata().getMetaclass("WorkflowTrace");
      Metaclass schedulerClass = getMetadata().getMetaclass("TestSysWorkflowSchedulerBatchJob");
      Instance primary = (Instance)primaryClass.invoke("new");
      InstanceList workflowList, assignmentList;
      Instance workflow, assignment1, assignment2;
      List invocationList;

      // Initialization
      primary.setValue("trace", null);
      primary.setValue("throwOuterEx", Boolean.FALSE);
      primary.setValue("throwUncaughtEx", Boolean.FALSE);


      /*
       * Test: Two semaphores in a fork.
       */
      schedulerClass.setValue("time", Primitive.toTimestamp(Primitive.createInteger(1000)));
      primary.setValue("trace", null);
      primary.setValue("throwInnerEx", Boolean.FALSE);
      primary.setValue("throwOuterEx", Boolean.FALSE);
      primary.invoke("goForkSemaphore");
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(2, assignmentList.size());
      assignment1 = assignmentList.getInstance(0);
      assignment2 = assignmentList.getInstance(1);

      if (assignment1.getValue("caption").equals("semaphoreRight:2"))
      {
         Instance temp = assignment1;

         assignment1 = assignment2;
         assignment2 = temp;
      }

      assertEquals("semaphoreLeftCaption", assignment1.getValue("caption"));
      assertEquals("semaphoreRight:2", assignment2.getValue("caption"));
      assertTrue(
         (assignment1.getValue("status").equals(Primitive.ONE_INTEGER) &&
         assignment2.getValue("status").equals(Primitive.ZERO_INTEGER)) ||
         (assignment1.getValue("status").equals(Primitive.ZERO_INTEGER) &&
         assignment2.getValue("status").equals(Primitive.ONE_INTEGER))
      );
      assertEquals(Boolean.TRUE, assignment1.getValue("semaphore"));
      assertEquals(Primitive.ONE_INTEGER, assignment1.getValue("status"));
      assertEquals(Boolean.TRUE, assignment2.getValue("semaphore"));
      assertEquals(Primitive.ZERO_INTEGER, assignment2.getValue("status"));
      invocationList = (List)schedulerClass.getValue("asyncInvokeList");
      assertEquals(1, invocationList.size());
      assertEquals(assignment1.getOID(), invocationList.get(0));

      // Simulate run of left semaphore
      assignment1.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment1.getState());
      assertEquals(Instance.DIRTY, workflow.getState());
      assertEquals("scrStart;scrLeft1;", primary.getValue("trace"));

      // Left semaphore done, right semaphore should be running
      assertEquals(Boolean.TRUE, assignment2.getValue("semaphore"));
      assertEquals(Primitive.ONE_INTEGER, assignment2.getValue("status"));
      assertEquals(2, invocationList.size());
      assertEquals(assignment2.getOID(), invocationList.get(1));

      // Simulate run of right semaphore
      assignment2.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment2.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals("scrStart;scrLeft1;scrRight1;scrFinish;", primary.getValue("trace"));
      commit();



      /*
       * Test: Two semaphores in a fork, left Semaphore throws exception
       */
      ((List)schedulerClass.getValue("asyncInvokeList")).clear();
      schedulerClass.setValue("time", Primitive.toTimestamp(Primitive.createInteger(1000)));
      primary.setValue("trace", null);
      primary.setValue("throwInnerEx", Boolean.TRUE);
      primary.setValue("throwOuterEx", Boolean.FALSE);
      primary.invoke("goForkSemaphore");
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(2, assignmentList.size());
      assignment1 = assignmentList.getInstance(0);
      assignment2 = assignmentList.getInstance(1);

      if (assignment1.getValue("caption").equals("semaphoreRight:2"))
      {
         Instance temp = assignment1;

         assignment1 = assignment2;
         assignment2 = temp;
      }

      assertEquals("semaphoreLeftCaption", assignment1.getValue("caption"));
      assertEquals("semaphoreRight:2", assignment2.getValue("caption"));
      assertTrue(
         (assignment1.getValue("status").equals(Primitive.ONE_INTEGER) &&
         assignment2.getValue("status").equals(Primitive.ZERO_INTEGER)) ||
         (assignment1.getValue("status").equals(Primitive.ZERO_INTEGER) &&
         assignment2.getValue("status").equals(Primitive.ONE_INTEGER))
      );
      assertEquals(Boolean.TRUE, assignment1.getValue("semaphore"));
      assertEquals(Primitive.ONE_INTEGER, assignment1.getValue("status"));
      assertEquals(Boolean.TRUE, assignment2.getValue("semaphore"));
      assertEquals(Primitive.ZERO_INTEGER, assignment2.getValue("status"));
      invocationList = (List)schedulerClass.getValue("asyncInvokeList");
      assertEquals(1, invocationList.size());
      assertEquals(assignment1.getOID(), invocationList.get(0));

      // Simulate run of left semaphore
      try
      {
         assignment1.invoke("schedulerRun");
         fail("No exception thrown");
      }
      catch (RuntimeException ex)
      {
         assertEquals(Instance.DELETED, assignment1.getState());
         assertEquals(Instance.DELETED, assignment2.getState());
         assertEquals("scrStart;scrLeft1;", primary.getValue("trace"));
         workflow.invoke("delete");
      }

      commit();



      /*
       * Test: Two semaphores in a fork, right (2nd) Semaphore throws exception
       */
      ((List)schedulerClass.getValue("asyncInvokeList")).clear();
      schedulerClass.setValue("time", Primitive.toTimestamp(Primitive.createInteger(1000)));
      primary.setValue("trace", null);
      primary.setValue("throwInnerEx", Boolean.FALSE);
      primary.setValue("throwOuterEx", Boolean.TRUE);
      primary.invoke("goForkSemaphore");
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(2, assignmentList.size());
      assignment1 = assignmentList.getInstance(0);
      assignment2 = assignmentList.getInstance(1);

      if (assignment1.getValue("caption").equals("semaphoreRight:2"))
      {
         Instance temp = assignment1;

         assignment1 = assignment2;
         assignment2 = temp;
      }

      assertEquals("semaphoreLeftCaption", assignment1.getValue("caption"));
      assertEquals("semaphoreRight:2", assignment2.getValue("caption"));
      assertTrue(
         (assignment1.getValue("status").equals(Primitive.ONE_INTEGER) &&
         assignment2.getValue("status").equals(Primitive.ZERO_INTEGER)) ||
         (assignment1.getValue("status").equals(Primitive.ZERO_INTEGER) &&
         assignment2.getValue("status").equals(Primitive.ONE_INTEGER))
      );
      assertEquals(Boolean.TRUE, assignment1.getValue("semaphore"));
      assertEquals(Primitive.ONE_INTEGER, assignment1.getValue("status"));
      assertEquals(Boolean.TRUE, assignment2.getValue("semaphore"));
      assertEquals(Primitive.ZERO_INTEGER, assignment2.getValue("status"));
      invocationList = (List)schedulerClass.getValue("asyncInvokeList");
      assertEquals(1, invocationList.size());
      assertEquals(assignment1.getOID(), invocationList.get(0));

      // Simulate run of left semaphore
      assignment1.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment1.getState());
      assertEquals(Instance.DIRTY, workflow.getState());
      assertEquals("scrStart;scrLeft1;", primary.getValue("trace"));

      // Left semaphore done, right semaphore should be running
      assertEquals(Boolean.TRUE, assignment2.getValue("semaphore"));
      assertEquals(Primitive.ONE_INTEGER, assignment2.getValue("status"));
      assertEquals(2, invocationList.size());
      assertEquals(assignment2.getOID(), invocationList.get(1));

      // Simulate run of right semaphore
      try
      {
         assignment2.invoke("schedulerRun");
         fail("No exception thrown");
      }
      catch (RuntimeException ex)
      {
         assertEquals(Instance.DELETED, assignment1.getState());
         assertEquals(Instance.DELETED, assignment2.getState());
         assertEquals("scrStart;scrLeft1;scrRight1;", primary.getValue("trace"));
         workflow.invoke("delete");
      }
   }

   /**
    * Tests the scheduling of a Workflow Semaphore.
    */
   public void testWorkflowSemaphoreScheduler() throws Exception
   {
      Metaclass workflowClass = getMetadata().getMetaclass(Metadata.WORKFLOW_CLASS_NAME);
      Metaclass primaryClass = getMetadata().getMetaclass("WorkflowTrace");
      Metaclass schedulerClass = getMetadata().getMetaclass("TestSysWorkflowSchedulerBatchJob");
      Instance primary = (Instance)primaryClass.invoke("new");
      Instance primary2 = (Instance)primaryClass.invoke("new");
      Instance primary3 = (Instance)primaryClass.invoke("new");
      Instance primary4 = (Instance)primaryClass.invoke("new");
      Instance scheduler = (Instance)schedulerClass.getValue("instance");
      InstanceList workflowList, assignmentList;
      Instance workflow, assignment;
      List invocationList;

      // Initialization
      primary.setValue("trace", null);
      primary.setValue("throwOuterEx", Boolean.FALSE);
      primary.setValue("throwUncaughtEx", Boolean.FALSE);
      primary2.setValue("trace", null);
      primary2.setValue("throwOuterEx", Boolean.FALSE);
      primary2.setValue("throwUncaughtEx", Boolean.FALSE);
      primary3.setValue("trace", null);
      primary3.setValue("throwOuterEx", Boolean.FALSE);
      primary3.setValue("throwUncaughtEx", Boolean.FALSE);
      primary4.setValue("trace", null);
      primary4.setValue("throwOuterEx", Boolean.FALSE);
      primary4.setValue("throwUncaughtEx", Boolean.FALSE);


      /*
       * Test: One job, inside time window
       */
      schedulerClass.setValue("time", Primitive.toTimestamp(Primitive.createInteger(1000)));
      primary.setValue("trace", null);
      primary.setValue("queueName", "Semaphore");
      primary.invoke("goSemaphore");
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Boolean.TRUE, assignment.getValue("semaphore"));
      assertEquals(Primitive.ONE_INTEGER, assignment.getValue("status"));
      invocationList = (List)schedulerClass.getValue("asyncInvokeList");
      assertEquals(1, invocationList.size());
      assertEquals(assignment.getOID(), invocationList.get(0));

      // Simulate run of Job
      assignment.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals("BEFORE;ASSIGNMENT_CREATE;EXECUTE_SEMAPHORE;END_SEMAPHORE;ASSIGNMENT_DELETE;AFTER;", primary.getValue("trace"));
      commit();


      /*
       * Test: Two jobs, queue concurrency = 1
       */
      ((List)schedulerClass.getValue("asyncInvokeList")).clear();
      primary.setValue("queueName", "SemaphoreLowConcurrency");
      primary2.setValue("queueName", "SemaphoreLowConcurrency");
      primary.setValue("trace", null);
      primary2.setValue("trace", null);
      primary.invoke("goSemaphore");
      Thread.sleep(50);
      primary2.invoke("goSemaphore");

      // Job 2: NOT RUNNING
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary2});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ZERO_INTEGER, assignment.getValue("status"));
      assertEquals("BEFORE;ASSIGNMENT_CREATE;", primary2.getValue("trace"));

      // Job 1: RUNNING
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ONE_INTEGER, assignment.getValue("status"));
      invocationList = (List)schedulerClass.getValue("asyncInvokeList");
      assertEquals(1, invocationList.size());
      assertEquals(assignment.getOID(), invocationList.get(0));

      // Simulate run of Job 1
      assignment.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals("BEFORE;ASSIGNMENT_CREATE;EXECUTE_SEMAPHORE;END_SEMAPHORE;ASSIGNMENT_DELETE;AFTER;", primary.getValue("trace"));

      // Job 1 finished, Job 2 should have been run
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary2});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ONE_INTEGER, assignment.getValue("status"));
      invocationList = (List)schedulerClass.getValue("asyncInvokeList");
      assertEquals(2, invocationList.size());
      assertEquals(assignment.getOID(), invocationList.get(1));

      // Simulate run of Job 2
      assignment.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals("BEFORE;ASSIGNMENT_CREATE;EXECUTE_SEMAPHORE;END_SEMAPHORE;ASSIGNMENT_DELETE;AFTER;", primary2.getValue("trace"));
      commit();


      /*
       * Test: Four jobs. System concurrency: 2.
       * Job 1: Low concurrency queue    RUN...FINISH
       * Job 2: Low concurrency queue    WAIT       |           RUN...FINISH
       * Job 3: High concurrency queue   RUN........|.....FINISH^
       * Job 4: High concurrency queue   WAIT       RUN...........FINISH
       */
      ((List)schedulerClass.getValue("asyncInvokeList")).clear();
      primary.setValue("queueName", "SemaphoreLowConcurrency");
      primary2.setValue("queueName", "SemaphoreLowConcurrency");
      primary3.setValue("queueName", "Semaphore");
      primary4.setValue("queueName", "Semaphore");
      primary.setValue("trace", null);
      primary2.setValue("trace", null);
      primary3.setValue("trace", null);
      primary4.setValue("trace", null);
      primary.invoke("goSemaphore");
      Thread.sleep(50);
      primary2.invoke("goSemaphore");
      Thread.sleep(50);
      primary3.invoke("goSemaphore");
      Thread.sleep(50);
      primary4.invoke("goSemaphore");


      invocationList = (List)schedulerClass.getValue("asyncInvokeList");
      assertEquals(2, invocationList.size());


      // Job 4: NOT RUNNING
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary4});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ZERO_INTEGER, assignment.getValue("status"));
      assertEquals("BEFORE;ASSIGNMENT_CREATE;", primary4.getValue("trace"));

      // Job 2: NOT RUNNING
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary2});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ZERO_INTEGER, assignment.getValue("status"));
      assertEquals("BEFORE;ASSIGNMENT_CREATE;", primary2.getValue("trace"));

      // Job 3: RUNNING
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary3});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ONE_INTEGER, assignment.getValue("status"));
      assertEquals(assignment.getOID(), invocationList.get(1));

      // Job 1: RUNNING
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ONE_INTEGER, assignment.getValue("status"));
      assertEquals(assignment.getOID(), invocationList.get(0));

      // Simulate run/finish of Job 1
      assignment.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals("BEFORE;ASSIGNMENT_CREATE;EXECUTE_SEMAPHORE;END_SEMAPHORE;ASSIGNMENT_DELETE;AFTER;", primary.getValue("trace"));
      commit();

      // Job 2 or 4 could run: 4 runs because its queue has higher priority
      assertEquals(3, invocationList.size());

      // Job 4: RUNNING
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary4});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ONE_INTEGER, assignment.getValue("status"));
      assertEquals(assignment.getOID(), invocationList.get(2));

      // Job 2: NOT RUNNING
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary2});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ZERO_INTEGER, assignment.getValue("status"));
      assertEquals("BEFORE;ASSIGNMENT_CREATE;", primary2.getValue("trace"));

      // Job 3: RUNNING
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary3});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ONE_INTEGER, assignment.getValue("status"));
      assertEquals(assignment.getOID(), invocationList.get(1));

      // Simulate run/finish of Job 3
      assignment.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals("BEFORE;ASSIGNMENT_CREATE;EXECUTE_SEMAPHORE;END_SEMAPHORE;ASSIGNMENT_DELETE;AFTER;", primary3.getValue("trace"));
      commit();

      // Job 2 runs
      assertEquals(4, invocationList.size());

      // Job 2: RUNNING
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary2});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ONE_INTEGER, assignment.getValue("status"));
      assertEquals(assignment.getOID(), invocationList.get(3));

      // Job 4: RUNNING
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary4});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ONE_INTEGER, assignment.getValue("status"));
      assertEquals(assignment.getOID(), invocationList.get(2));

      // Simulate run/finish of Job 4
      assignment.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals("BEFORE;ASSIGNMENT_CREATE;EXECUTE_SEMAPHORE;END_SEMAPHORE;ASSIGNMENT_DELETE;AFTER;", primary4.getValue("trace"));
      assertEquals(4, invocationList.size());

      // Job 2: RUNNING
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary2});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ONE_INTEGER, assignment.getValue("status"));
      assertEquals(assignment.getOID(), invocationList.get(3));

      // Simulate run/finish of Job 2
      assignment.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals("BEFORE;ASSIGNMENT_CREATE;EXECUTE_SEMAPHORE;END_SEMAPHORE;ASSIGNMENT_DELETE;AFTER;", primary2.getValue("trace"));
      assertEquals(4, invocationList.size());


      /*
       * Test: One job, outside of time window.
       */
      ((List)schedulerClass.getValue("asyncInvokeList")).clear();
      schedulerClass.setValue("time", Primitive.toTimestamp(Primitive.createInteger(15000)));
      primary.setValue("trace", null);
      primary.setValue("queueName", "Semaphore");
      primary.invoke("goSemaphore");
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Boolean.TRUE, assignment.getValue("semaphore"));
      assertEquals(Primitive.ZERO_INTEGER, assignment.getValue("status"));
      invocationList = (List)schedulerClass.getValue("asyncInvokeList");
      assertEquals(0, invocationList.size());

      // Beginning of next time window is at 60 seconds.
      assertEquals(Primitive.toTimestamp(Primitive.createLong(60000)), scheduler.getValue("start"));
      assertEquals(Primitive.createLong(45000), scheduler.getValue("period"));

      // Simulate timer tick at beginning of next window
      schedulerClass.setValue("time", Primitive.toTimestamp(Primitive.createInteger(60000)));
      m_context.setSecure(false);
      scheduler.invoke("run");
      m_context.setSecure(true);

      assertEquals(Primitive.ONE_INTEGER, assignment.getValue("status"));
      assertEquals(1, invocationList.size());
      assertEquals(assignment.getOID(), invocationList.get(0));

      // Simulate run of Job
      assignment.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals("BEFORE;ASSIGNMENT_CREATE;EXECUTE_SEMAPHORE;END_SEMAPHORE;ASSIGNMENT_DELETE;AFTER;", primary.getValue("trace"));
      commit();
   }

   /**
    * Tests the throttling of a Workflow Semaphore.
    */
   public void testWorkflowSemaphoreThrottling() throws Exception
   {
      Metaclass workflowClass = getMetadata().getMetaclass(Metadata.WORKFLOW_CLASS_NAME);
      Metaclass primaryClass = getMetadata().getMetaclass("WorkflowTrace");
      Metaclass schedulerClass = getMetadata().getMetaclass("TestSysWorkflowSchedulerBatchJob");
      Metaclass throttleCounterBatchJobClass = getMetadata().getMetaclass("SysQueueThrottleCounterBatchJob");
      Instance primary = (Instance)primaryClass.invoke("new");
      Instance primary2 = (Instance)primaryClass.invoke("new");
      Instance primary3 = (Instance)primaryClass.invoke("new");
      Instance scheduler = (Instance)schedulerClass.getValue("instance");
      Instance throttleCounterBatchJob = (Instance)throttleCounterBatchJobClass.getValue("instance");
      InstanceList workflowList, assignmentList;
      Instance workflow, assignment;
      List invocationList;

      // Initialization
      primary.setValue("trace", null);
      primary.setValue("throwOuterEx", Boolean.FALSE);
      primary.setValue("throwUncaughtEx", Boolean.FALSE);
      primary2.setValue("trace", null);
      primary2.setValue("throwOuterEx", Boolean.FALSE);
      primary2.setValue("throwUncaughtEx", Boolean.FALSE);
      primary3.setValue("trace", null);
      primary3.setValue("throwOuterEx", Boolean.FALSE);
      primary3.setValue("throwUncaughtEx", Boolean.FALSE);


      /*
       * Test: 3 jobs:
       * Job 1: started at 0 seconds, runs immediately (in window)
       * Job 2: started at 8 seconds, runs immediately (in window)
       * Job 3: started at 9 seconds, doesn't run (throttle exceeded)
       *
       * @ 60 seconds: Timer tick, job counter reset, job 3 resumed.
       */
      schedulerClass.setValue("time", Primitive.toTimestamp(Primitive.ZERO_INTEGER));
      ((List)schedulerClass.getValue("asyncInvokeList")).clear();
      primary.setValue("queueName", "SemaphoreThrottled");
      primary2.setValue("queueName", "SemaphoreThrottled");
      primary3.setValue("queueName", "SemaphoreThrottled");
      primary.setValue("trace", null);
      primary2.setValue("trace", null);
      primary3.setValue("trace", null);
      primary.invoke("goSemaphore");

      invocationList = (List)schedulerClass.getValue("asyncInvokeList");
      assertEquals(1, invocationList.size());

      // Job 1: RUNNING
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ONE_INTEGER, assignment.getValue("status"));
      assertEquals(assignment.getOID(), invocationList.get(0));

      // Simulate run/finish of Job 1
      assignment.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals("BEFORE;ASSIGNMENT_CREATE;EXECUTE_SEMAPHORE;END_SEMAPHORE;ASSIGNMENT_DELETE;AFTER;", primary.getValue("trace"));
      commit();


      // @ 8 seconds
      schedulerClass.setValue("time", Primitive.toTimestamp(Primitive.createInteger(8000)));
      primary2.invoke("goSemaphore");

      // Job 2: RUNNING
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary2});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ONE_INTEGER, assignment.getValue("status"));
      assertEquals(2, invocationList.size());
      assertEquals(assignment.getOID(), invocationList.get(1));

      // Simulate run/finish of Job 2
      assignment.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals("BEFORE;ASSIGNMENT_CREATE;EXECUTE_SEMAPHORE;END_SEMAPHORE;ASSIGNMENT_DELETE;AFTER;", primary2.getValue("trace"));
      commit();

      // @ 9 seconds
      schedulerClass.setValue("time", Primitive.toTimestamp(Primitive.createInteger(9000)));
      primary3.invoke("goSemaphore");

      // Job 3: NOT RUNNING
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary3});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ZERO_INTEGER, assignment.getValue("status"));
      assertEquals("BEFORE;ASSIGNMENT_CREATE;", primary3.getValue("trace"));
      assertEquals(2, invocationList.size());


      // @ 60 seconds
      m_context.setSecure(false);
      throttleCounterBatchJob.invoke("run");
      m_context.setSecure(true);
      schedulerClass.setValue("time", Primitive.toTimestamp(Primitive.createInteger(60000)));
      scheduler.invoke("schedule");
      assertEquals(3, invocationList.size());
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary3});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Primitive.ONE_INTEGER, assignment.getValue("status"));
      assertEquals(assignment.getOID(), invocationList.get(2));

      // Simulate run/finish of Job 3
      assignment.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals("BEFORE;ASSIGNMENT_CREATE;EXECUTE_SEMAPHORE;END_SEMAPHORE;ASSIGNMENT_DELETE;AFTER;", primary3.getValue("trace"));
      commit();
   }

   /**
    * Tests the Workflow Queue ProcessEvent macro (which is implemented
    * with a Semaphore)
    */
   public void testWorkflowProcessEvent() throws Exception
   {
      Metaclass workflowClass = getMetadata().getMetaclass(Metadata.WORKFLOW_CLASS_NAME);
      Metaclass primaryClass = getMetadata().getMetaclass("WorkflowTrace");
      Metaclass schedulerClass = getMetadata().getMetaclass("TestSysWorkflowSchedulerBatchJob");
      Instance primary = (Instance)primaryClass.invoke("new");
      InstanceList workflowList, assignmentList, timerList;
      Instance workflow, assignment, timeoutTimer;
      List invocationList;

      // Initialization
      primary.setValue("trace", null);
      primary.setValue("throwOuterEx", Boolean.FALSE);
      primary.setValue("throwUncaughtEx", Boolean.FALSE);

      /*
       * Test: One job, inside time window.
       */
      schedulerClass.setValue("time", Primitive.toTimestamp(Primitive.createInteger(1000)));
      primary.setValue("trace", null);
      primary.setValue("queueName", "Semaphore");
      timerList = getNonInitialTimers();
      assertEquals(0, timerList.size());
      primary.invoke("goProcessEvent");
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Boolean.TRUE, assignment.getValue("semaphore"));
      assertEquals(Primitive.ONE_INTEGER, assignment.getValue("status"));
      invocationList = (List)schedulerClass.getValue("asyncInvokeList");
      assertEquals(1, invocationList.size());
      assertEquals(assignment.getOID(), invocationList.get(0));
      assertEquals("BEGIN;", primary.getValue("trace"));
      timerList = getNonInitialTimers();
      assertEquals(0, timerList.size());


      // Simulate run
      assignment.invoke("schedulerRun");
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals("BEGIN;PROCESS_EVENT;PROCESS_EVENT_DONE;END;", primary.getValue("trace"));
      timerList = getNonInitialTimers();
      assertEquals(0, timerList.size());
      commit();


      /*
       * Test 2: One job, outside time window, canceled by class event.
       */
      schedulerClass.setValue("time", Primitive.toTimestamp(Primitive.createInteger(30000*3)));
      ((List)schedulerClass.getValue("asyncInvokeList")).clear();
      primary.setValue("trace", null);
      primary.setValue("queueName", "Semaphore");
      timerList = getNonInitialTimers();
      assertEquals(0, timerList.size());
      primary.invoke("goProcessEvent");
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Boolean.TRUE, assignment.getValue("semaphore"));
      assertEquals(Primitive.ZERO_INTEGER, assignment.getValue("status"));
      invocationList = (List)schedulerClass.getValue("asyncInvokeList");
      assertEquals(0, invocationList.size());
      assertEquals("BEGIN;", primary.getValue("trace"));

      // Scheduler runs
      commit();
      timerList = getNonInitialTimers();
      assertEquals(2, timerList.size());  // Scheduler timer and the TimerEvent timer
      assertEquals(Instance.CLEAN, assignment.getState());
      assertEquals(Instance.CLEAN, workflow.getState());
      assertEquals("BEGIN;", primary.getValue("trace"));

      // Cancel event
      primary.invoke("resumeProcessEvent");
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals("BEGIN;CLASS_EVENT;END;", primary.getValue("trace"));
      timerList = getNonInitialTimers();
      assertEquals(1, timerList.size());  // The scheduler timer remains
      timerList.getInstance(0).delete();
      commit();


      /*
       * Test 3: One job, outside time window, times out by Timer Event.
       */
      schedulerClass.setValue("time", Primitive.toTimestamp(Primitive.createInteger(30000*1)));
      ((List)schedulerClass.getValue("asyncInvokeList")).clear();
      primary.setValue("trace", null);
      primary.setValue("queueName", "Semaphore");
      timerList = getNonInitialTimers();
      assertEquals(0, timerList.size());
      primary.invoke("goProcessEvent");
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{primary});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assignmentList = (InstanceList)workflow.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Boolean.TRUE, assignment.getValue("semaphore"));
      assertEquals(Primitive.ZERO_INTEGER, assignment.getValue("status"));
      invocationList = (List)schedulerClass.getValue("asyncInvokeList");
      assertEquals(0, invocationList.size());
      assertEquals("BEGIN;", primary.getValue("trace"));

      // Scheduler runs
      commit();
      timerList = getNonInitialTimers();
      assertEquals(2, timerList.size());  // Scheduler timer and the TimerEvent timer
      assertEquals(Instance.CLEAN, assignment.getState());
      assertEquals(Instance.CLEAN, workflow.getState());
      assertEquals("BEGIN;", primary.getValue("trace"));

      // Timeout event
      timeoutTimer = getTimeoutTimer(workflow);
      assertNotNull(timeoutTimer);
      workflow.invoke("timeout", new Object[] {
         timeoutTimer.getValue("ordinal")
      });
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Instance.DELETED, workflow.getState());
      assertEquals("BEGIN;TIMED_OUT;END;", primary.getValue("trace"));
      timeoutTimer.delete();  // Simulated timeout must remove timer manually
      commit();
      timerList = getNonInitialTimers();
      assertEquals(1, timerList.size());  // The scheduler timer remains
      timerList.getInstance(0).delete();
      commit();
   }

   protected Instance getTimeoutTimer(Instance workflow) throws Exception
   {
      InstanceList timerList = getNonInitialTimers();
      int i = 0;

      while (i < timerList.size())
      {
         Instance timer = timerList.getInstance(i);

         if (timer.getValue("workflow") == workflow)
         {
            return timer;
         }

         i++;
      }

      return null;
   }

   protected InstanceList getNonInitialTimers() throws Exception
   {
      Metaclass timerClass = getMetadata().getMetaclass("SysTimer");
      InstanceList timerList = Query.createRead(timerClass, null, Boolean.TRUE, null, -1, 0, false, Query.SEC_NONE, m_context).read();
      int i = 0;

      while (i < timerList.size())
      {
         Instance timer = timerList.getInstance(i);
         OID timerOID = timer.getOID();

         assertEquals(1, timerOID.getCount());

         if (((Binary)timerOID.getValue(0)).toString().equals("00000000000000000000000000000001"))
         {
            timerList.remove(i);
         }
         else
         {
            i++;
         }
      }

      return timerList;
   }

   /**
    * @param bUnicode if the substring is a check for a unicode DB.
    * @return The substring to search for that garantees that a Unicode flag check guard is present
    *         to prevent upgrades of DBs who's state does not match connection's Unicode flag.
    */
   protected abstract String getUnicodeCheckGuard(boolean bUnicode);

   /**
    * Tests that a Service Semaphore handles normal termination and exceptions
    * appropriately.
    */
   public void testServiceSemaphoreFlowControl() throws Exception
   {
      Metaclass serviceClass = getMetadata().getMetaclass(Metadata.SERVICE_CLASS_NAME);
      Metaclass schedulerClass = getMetadata().getMetaclass("TestSysWorkflowSchedulerBatchJob");
      TransferObject tobj = new TransferObject();
      TransferObject result;
      Instance instance, assignment;
      InstanceList assignmentList;

      // Set scheduler time outside of the queue time window
      schedulerClass.setValue("time", Primitive.toTimestamp(Primitive.createInteger(15000)));


      // Test: no exception
      tobj.setValue("trace", "");
      tobj.setValue("throwOuterEx", Boolean.FALSE);
      tobj.setValue("throwUncaughtEx", Boolean.FALSE);
      instance = (Instance)serviceClass.invoke("invoke", new Object[]{"Semaphore", tobj, null});
      assertEquals(Boolean.FALSE, instance.invoke("done"));
      assertEquals("BEFORE;", tobj.getValue("trace"));
      assignmentList = (InstanceList)instance.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Boolean.TRUE, assignment.getValue("semaphore"));
      assertEquals(Primitive.ZERO_INTEGER, assignment.getValue("status"));
      commit();
      assignment.invoke("schedulerRun");
      assertEquals(Boolean.TRUE, instance.invoke("done"));
      assertEquals(Instance.DELETED, assignment.getState());
      result = (TransferObject)instance.invoke("result");
      assertEquals("BEFORE;EXECUTE_SEMAPHORE;END_SEMAPHORE;AFTER;", result.getValue("trace"));
      instance.delete();
      commit();


      // Test: caught exception
      tobj.setValue("trace", "");
      tobj.setValue("throwOuterEx", Boolean.TRUE);
      tobj.setValue("throwUncaughtEx", Boolean.FALSE);
      instance = (Instance)serviceClass.invoke("invoke", new Object[]{"Semaphore", tobj, null});
      assertEquals(Boolean.FALSE, instance.invoke("done"));
      assertEquals("BEFORE;", tobj.getValue("trace"));
      assignmentList = (InstanceList)instance.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Boolean.TRUE, assignment.getValue("semaphore"));
      assertEquals(Primitive.ZERO_INTEGER, assignment.getValue("status"));
      commit();
      assignment.invoke("schedulerRun");
      assertEquals(Boolean.TRUE, instance.invoke("done"));
      assertEquals(Instance.DELETED, assignment.getState());
      assertEquals(Primitive.createInteger(2), assignment.getValue("status"));
      result = (TransferObject)instance.invoke("result");
      assertEquals("BEFORE;EXECUTE_SEMAPHORE;CAUGHT_OUTER;", result.getValue("trace"));
      instance.delete();
      commit();


      // Test: uncaught exception
      tobj.setValue("trace", "");
      tobj.setValue("throwOuterEx", Boolean.FALSE);
      tobj.setValue("throwUncaughtEx", Boolean.TRUE);
      instance = (Instance)serviceClass.invoke("invoke", new Object[]{"Semaphore", tobj, null});
      assertEquals(Boolean.FALSE, instance.invoke("done"));
      assertEquals("BEFORE;", tobj.getValue("trace"));
      assignmentList = (InstanceList)instance.getValue("assignments");
      assertEquals(1, assignmentList.size());
      assignment = assignmentList.getInstance(0);
      assertEquals(Boolean.TRUE, assignment.getValue("semaphore"));
      assertEquals(Primitive.ZERO_INTEGER, assignment.getValue("status"));
      commit();

      try
      {
         assignment.invoke("schedulerRun");
         fail();
      }
      catch (ScriptingException ex)
      {
         assertEquals(Boolean.TRUE, instance.invoke("done"));
         assertEquals(Instance.DELETED, assignment.getState());
         assertEquals(Primitive.createInteger(2), assignment.getValue("status"));
         result = (TransferObject)instance.invoke("result");
         assertEquals("BEFORE;EXECUTE_SEMAPHORE;", result.getValue("trace"));
         instance.delete();
         commit();
      }
   }

   /**
    * Tests the Finally part of a Service Try block.
    */
   public void testServiceTryFinally() throws Exception
   {
      Metaclass serviceClass = getMetadata().getMetaclass(Metadata.SERVICE_CLASS_NAME);
      TransferObject tobj = new TransferObject();

      tobj.setValue("trace", "");
      tobj.setValue("throwInnerEx", Boolean.FALSE);
      tobj.setValue("throwOuterEx", Boolean.FALSE);
      tobj.setValue("throwUncaughtEx", Boolean.FALSE);

      Instance instance = (Instance)serviceClass.invoke("invoke", new Object[]{"TryFinally", tobj, null});

      assertEquals(Boolean.TRUE, instance.invoke("done"));

      TransferObject result = (TransferObject)instance.invoke("result");

      assertEquals("THROWBLOCK;END_THROWBLOCK;F_INNER;F_OUTER;", result.getValue("trace"));


      tobj.setValue("trace", "");
      tobj.setValue("throwInnerEx", Boolean.TRUE);
      instance = (Instance)serviceClass.invoke("invoke", new Object[]{"TryFinally", tobj, null});
      result = (TransferObject)instance.invoke("result");
      assertEquals("THROWBLOCK;CAUGHT_INNER;F_INNER;F_OUTER;", result.getValue("trace"));


      tobj.setValue("trace", "");
      tobj.setValue("throwInnerEx", Boolean.FALSE);
      tobj.setValue("throwOuterEx", Boolean.TRUE);
      instance = (Instance)serviceClass.invoke("invoke", new Object[]{"TryFinally", tobj, null});
      result = (TransferObject)instance.invoke("result");
      assertEquals("THROWBLOCK;F_INNER;CAUGHT_OUTER;F_OUTER;", result.getValue("trace"));


      tobj.setValue("trace", "");
      tobj.setValue("throwOuterEx", Boolean.FALSE);
      tobj.setValue("throwUncaughtEx", Boolean.TRUE);

      try
      {
         instance = (Instance)serviceClass.invoke("invoke", new Object[]{"TryFinally", tobj, null});
         fail();
      }
      catch (ScriptingException ex)
      {
         result = (TransferObject)instance.invoke("result");
         assertEquals("THROWBLOCK;F_INNER;F_OUTER;", result.getValue("trace"));
      }
   }


   /**
    * Tests the Loop construct for workflows. Specifically, tests that the workflow may be persisted
    * during loop execution.
    */
   public void testWorkflowLoop()
   {
      Metaclass workflowClass = getMetadata().getMetaclass(Metadata.WORKFLOW_CLASS_NAME);
      InstanceList workflowList;
      Instance contact, workflow;
      Query query = Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("((addresses country state city))"),
         parse("(and (= lastName \"Test\") (= firstName \"Joe\"))"),
         parse("((addresses . #t))"), // the first address of the contact is modified by workflow
         -1, 0, false, Query.SEC_NONE, m_context);
      InstanceList resultList = query.read();

      assertEquals(1, resultList.size());
      contact = resultList.getInstance(0);

      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{contact});
      assertEquals(0, workflowList.size());

      contact.invoke("start");
      commit();
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{contact});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assertEquals(1, ((List)workflow.getValue("assignments")).size());

      contact.invoke("process");
      commit();
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{contact});
      assertEquals(1, workflowList.size());
      workflow = workflowList.getInstance(0);
      assertEquals(1, ((List)workflow.getValue("assignments")).size());

      contact.invoke("break");
      commit();
      workflowList = (InstanceList)workflowClass.invoke("forInstance",
         new Object[]{contact});
      assertEquals(0, workflowList.size());

      InstanceList addressList = (InstanceList)contact.getValue("addresses");

      addressList.sort(new Comparator()
      {
         public int compare(Object left, Object right)
         {
            return ((Comparable)((Instance)left).getValue("city")).compareTo(((Instance)right).getValue("city"));
         }
      });

      assertEquals(2, addressList.size());
      assertEquals("Hong Kong", addressList.getInstance(0).getValue("city"));
      assertEquals("Richmond Hill", addressList.getInstance(1).getValue("city"));
   }

   public void testWorkflowTrigger() throws Exception
   {
      Metaclass patientClass = getMetadata().getMetaclass("Patient");
      Instance parentPatient, childPatient;

      childPatient = (Instance)patientClass.invoke("new");
      childPatient.setValue("firstName", "child");
      parentPatient = (Instance)patientClass.invoke("new");
      parentPatient.setValue("firstName", "parent");
      ((InstanceArrayList)parentPatient.getValue("children")).add(childPatient);
      commit();

      getLogger().debug("Starting workflow on parent");
      parentPatient.invoke("triggerTestStart");
      commit();

      // Triggering child to wake from queue
      childPatient.invoke("triggerTest");
      commit();

      // Triggering child to wake from script
      childPatient.invoke("triggerTest");
      commit();

      // Triggering child, should not wake parent
      childPatient.invoke("triggerTest");
      commit();

      List workflowList = ((List)getMetadata().getMetaclass("SysWorkflow").invoke("forInstance",
         new Object[]{parentPatient}));

      assertEquals(1, workflowList.size());

      State state = (State)((Instance)workflowList.get(0)).getValue("state");

      assertEquals("6", state.toString());
      parentPatient.invoke("triggerTest");
      assertTrue(state.isFinal());
   }

   /**
    * Tests that an exception thrown in a trigger script step is caught by
    * a workflow try/catch.
    */
   public void testWorkflowTriggerException()
   {
      Metaclass patientClass = getMetadata().getMetaclass("Patient");
      Instance patient = (Instance)patientClass.invoke("new");

      patient.setValue("firstName", "John");
      commit();

      patient.invoke("triggerExceptionStart");
      commit();

      // Fire the trigger that throws an exception
      patient.invoke("triggerException");
      commit();
   }
}
