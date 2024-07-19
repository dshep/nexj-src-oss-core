// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import java.util.Arrays;
import java.util.List;

import nexj.core.meta.Primitive;
import nexj.core.persistence.OID;
import nexj.core.persistence.sql.SQLDataTest;
import nexj.core.runtime.SecurityViolationException;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.util.Binary;
import nexj.core.util.auth.SimplePrincipal;

public class GenericServerTest extends SQLDataTest
{
   public GenericServerTest(String name)
   {
      super(name);
   }

   public void testInvoke()
   {
      Request req;
      TransferObject tobj;

      req = new Request();
      req.setCommit(false);

      tobj = new TransferObject("User", "create");
      tobj.setValue("name", "test_user");
      req.addInvocation(tobj);

      TransferObject user = tobj;

      tobj = new TransferObject("Principal");
      tobj.setValue("attributes", parse("(name)"));
      req.addFilter(tobj);

      tobj = new TransferObject("User");
      tobj.setValue("attributes", parse("((@@ Principal name) typeCode)"));
      tobj.setValue("instances", Arrays.asList(new Object[]{user}));
      req.addFilter(tobj);

      Server server = (Server)m_context.getComponentInstance("Server.Generic");

      Response res = server.invoke(req);

      assertEquals(1, res.getResultCount());
      assertEquals(2, res.getEventCount());

      tobj = (TransferObject)res.getResult(0);
      assertNotNull(tobj);
      assertEquals("User", tobj.getClassName()); 
      assertEquals("create", tobj.getEventName()); 
      assertNull(tobj.getOID()); 

      List list = (List)res.getEvent(0);

      assertEquals(1, list.size());

      tobj = (TransferObject)list.get(0);

      assertEquals("User", tobj.getClassName());
      assertEquals("create", tobj.getEventName());
      assertEquals(Primitive.ZERO_INTEGER, tobj.getValue("updateCount"));
      assertEquals("U", tobj.getValue("typeCode"));
      assertFalse(tobj.hasValue("name"));
      assertSame(tobj, ((List)res.getEvent(1)).get(0));

      tobj = new TransferObject("Contact");
      tobj.setEventName("create");

      TransferObject phone = new TransferObject("Phone");

      phone.setEventName("delete");
      phone.setValue("type", "business");

      tobj.setValue("phones", Arrays.asList(new Object[]{phone}));
      tobj.setValue("firstName", "test");

      req = new Request();
      req.setCommit(false);
      req.addInvocation(tobj);
      req.addInvocation(phone);

      tobj = new TransferObject("Phone");
      tobj.setValue("attributes", parse("(number)"));
      req.addFilter(tobj);
      res = server.invoke(req);

      assertEquals(2, res.getResultCount());
      assertNotNull(res.getResult(0));
      assertEquals("Contact", ((TransferObject)res.getResult(0)).getClassName());
      assertEquals("create", ((TransferObject)res.getResult(0)).getEventName());
      assertNull(tobj.getOID()); 
      assertEquals(1, res.getEventCount());
      assertEquals(1, ((List)res.getEvent(0)).size());

      tobj.setValue("number", new Pair(new PCodeFunction()));

      try
      {
         server.invoke(req);
         fail("Expected RPCException");
      }
      catch (RPCException e)
      {
         assertEquals("err.rpc.type", e.getErrorCode());
      }

      req = new Request();
      tobj = new TransferObject("Contact", "create");
      tobj.setValue("firstName", "new");
      tobj.setValue("lastName", "contact");
      req.addInvocation(tobj, parse("(firstName lastName version)"));
      req.addInvocation(tobj, "getFullName", new Object[]{" (tm)"}, null);

      res = server.invoke(req);

      assertEquals(2, res.getResultCount());
      tobj = (TransferObject)res.getResult(0);
      assertEquals(3, tobj.getValueCount());
      assertEquals("new", tobj.getValue("firstName"));
      assertEquals("contact", tobj.getValue("lastName"));
      assertEquals(Primitive.ZERO_INTEGER, tobj.getValue("version"));
      assertEquals("Contact", tobj.getClassName());
      assertNull(tobj.getEventName());
      assertNotNull(tobj.getOID());
      assertEquals("new contact (tm)", res.getResult(1));

      req = new Request();
      req.addInvocation(new TransferObject(tobj.getOID(), "Contact", "getFullName", 0), new Object[]{"!!!"});
      req.addInvocation(new TransferObject("Contact", "getFullName"),
         new Object[]{new TransferObject(tobj.getOID(), "Contact", "getFullName", 0), "!!!"});
      res = server.invoke(req);
      assertEquals(2, res.getResultCount());
      assertEquals("new contact!!!", res.getResult(0));
      assertEquals("new contact!!!", res.getResult(1));

      req = new Request();
      req.addInvocation(new TransferObject(tobj.getOID(), "Contact", "update", 1));
      req.getObject(0).setValue("readPrincipal", new TransferObject(new OID(new Object[]{Binary.parse("00000000000000000000000000000002")}), "User", null, 0));
      req.addFilter(new TransferObject("Object", 1));
      req.getFilter(0).setValue("attributes", Pair.attributeList("fullName", null));
      res = server.invoke(req);
      assertEquals(1, res.getResultCount());
      assertNull(res.getResult(0));
      assertEquals(1, res.getEventCount());
      assertEquals(1, res.getEvent(0).size());

      TransferObject evt = (TransferObject)res.getEvent(0).iterator().next();

      assertEquals(tobj.getOID(), evt.getOID());
      assertEquals("Contact", evt.getClassName());
      assertEquals("delete", evt.getEventName());
      assertEquals(0, evt.getValueCount());

      req = new Request();
      req.addInvocation(new TransferObject(tobj.getOID(), "Contact", "delete", 0));

      try
      {
         res = server.invoke(req);
         fail("Expected SecurityViolationException");
      }
      catch (SecurityViolationException e)
      {
      }

      m_context.setSecure(false);
      
      try
      {
         res = server.invoke(req);
      }
      finally
      {
         m_context.setSecure(true);
      }

      assertEquals(1, res.getResultCount());
      assertNull(res.getResult(0));

      // Testing the RPC pre-value support

      req = new Request();
      tobj = new TransferObject("Contact", "getPreFullName");
      tobj.setValue("firstName", "new");
      tobj.setValue("lastName", "contact");

      TransferObject pre = new TransferObject("Contact");

      pre.setValue("firstName", "pre");
      pre.setValue("lastName", "con");

      tobj.setValue(InstanceFactory.PRE_NAME, pre);
      req.addInvocation(tobj);

      res = server.invoke(req);

      assertEquals(1, res.getResultCount());
      assertEquals("pre con", res.getResult(0));
   }

   public void testSecurity()
   {
      m_context.initialize(new SimplePrincipal("jsmith"));

      Server server = (Server)m_context.getComponentInstance("Server.Generic");
      Request req = new Request();

      req.addInvocation(new TransferObject("Contact", "read"),
         new Object[]{null, null, null, new Integer(-1), new Integer(0), Boolean.FALSE});

      try
      {
         server.invoke(req);
         fail("Expected SecurityViolationException");
      }
      catch (SecurityViolationException e)
      {
         assertEquals("err.runtime.privilege", e.getErrorCode());
         assertEquals("readContact", e.getErrorArgs()[0]);
      }

      req = new Request();
      req.addInvocation(new TransferObject("StringEnum", "read"),
         new Object[]{null, parse("(= value (SupportedLocaleEnum'en))"),
         null, new Integer(-1), new Integer(0), Boolean.FALSE});

      Response res = server.invoke(req);

      assertEquals(1, res.getResultCount());
      assertEquals(1, ((List)res.getResult(0)).size());

      req.getInvocation(0).getArguments()[1] = parse("(= (user) (User'getUser \"jsmith\"))");

      try
      {
         res = server.invoke(req);
         fail("Expected SecurityViolationException");
      }
      catch (SecurityViolationException e)
      {
         assertEquals("err.rpc.eventVisibility", e.getErrorCode());
         assertEquals("getUser", e.getErrorArgs()[0]);
         assertEquals("User", e.getErrorArgs()[1]);
      }

      req = new Request();
      req.addInvocation(new TransferObject("Address", "read"),
         new Object[]{null, parse("(!= (@ contact (instance? (@) Patient) birthdate) ())"),
         null, new Integer(-1), new Integer(0), Boolean.FALSE});

      try
      {
         server.invoke(req);
         fail("Expected SecurityViolationException");
      }
      catch (SecurityViolationException e)
      {
         assertEquals("err.rpc.attributeReadPrivilege", e.getErrorCode());
         assertEquals("birthdate", e.getErrorArgs()[0]);
         assertEquals("Patient", e.getErrorArgs()[1]);
         assertEquals("readContact", e.getErrorArgs()[2]);
      }

      m_context.initialize(new SimplePrincipal("jtest"));

      req = new Request();
      req.addInvocation(new TransferObject("Address", "read"),
         new Object[]{null, parse("(!= (@ contact (instance? (@) Patient) birthdate) ())"),
         null, new Integer(-1), new Integer(0), Boolean.FALSE});

      res = server.invoke(req);

      assertEquals(1, res.getResultCount());
      assertEquals(1, ((List)res.getResult(0)).size());
   }
}
