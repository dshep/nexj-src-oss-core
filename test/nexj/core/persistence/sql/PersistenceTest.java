// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.io.FileOutputStream;
import java.io.StringReader;

import junit.framework.TestCase;

import nexj.core.meta.Metaclass;
import nexj.core.meta.Repository;
import nexj.core.persistence.OID;
import nexj.core.persistence.Query;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Pair;
import nexj.core.scripting.SchemeParser;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.Logger;

public class PersistenceTest extends TestCase
{
   private final static Logger s_logger = Logger.getLogger(PersistenceTest.class); 

   private GlobalEnvironment m_env;
   private InvocationContext m_context;

   /**
    * Constructor for PersistenceTest.
    * @param name
    */
   public PersistenceTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_env = new GlobalEnvironment();
      m_context = new InvocationContext(Repository.getMetadata());
   }

   /*
    * @see TestCase#tearDown()
    */
   protected void tearDown() throws Exception
   {
      m_context.complete(false);
      ThreadContextHolder.setContext(null);
      super.tearDown();
   }
   
   public void testMapper()
   {
//      Metaclass contact = Repository.getMetadata().getMetaclass("Contact");
//      Pair attributes = (Pair)parse("(firstName lastName (addresses country city))");
//      Object where = parse("(and (like? firstName \"A*\") (= lastName \"Doe\") (in? (@ readPrincipal name) \"Admin\" \"Guest\" \"User\")))");
//      Pair orderBy = (Pair)parse("((firstName . #t)(lastName . #t))" /* ((@) . #t))" */);

      Metaclass contact = Repository.getMetadata().getMetaclass("XJContact");
      Pair attributes = (Pair)parse("(fullName (addresses country city) type (image mimeData mimeType))");
      Object where = parse("(and (like? firstName \"V*\") (= lastName \"Iordanov\") (= entityType 0n))");
      Pair orderBy = (Pair)parse("((firstName . #t)(lastName . #t)((@) . #t))");

      Query query = Query.createRead(contact, attributes, where, orderBy, 10, 0, false, Query.SEC_NONE, m_context);

      InstanceList list = query.read();
      
      for (int i = 0; i < list.size(); ++i)
      {
         s_logger.dump("fullName[" + i + "] = \"" + list.getInstance(i).getValue("fullName") + "\"");
      }
      
      Metaclass address = Repository.getMetadata().getMetaclass("XJAddress");
      attributes = (Pair)parse("(country city)");
      where = new Pair(Symbol.EQ, new Pair (parse("(@@ XJContact addresses)"), new Pair(new OID(new Object[]{"12345678901234567890123456789012"}))));

      query = Query.createRead(address, attributes, where, null, 10, 0, false, Query.SEC_NONE, m_context);
      query.read();
      
      Metaclass xjobj = Repository.getMetadata().getMetaclass("XJJObj");
      attributes = (Pair)parse("(mimeType mimeData)");
      where = new Pair(Symbol.EQ, new Pair (parse("(@@ XJContact image)"), new Pair(new OID(new Object[]{"78B14B58F66611CEBB8500AA0020A75A"}))));
      query = Query.createRead(xjobj, attributes, where, null, 1, 0, false, Query.SEC_NONE, m_context);
      list = query.read();
      
      if (list.size() == 1)
      {
         s_logger.debug("mime = " + list.getInstance(0).getValue("mimeType"));
         
         try
         {
               FileOutputStream fos = new FileOutputStream("c:/tmp/pic.bin");
               fos.write(((Binary)list.getInstance(0).getValue("mimeData")).getData());
               fos.close();
         }
         catch (Exception e)
         {
            s_logger.error("Exception", e);
         }
      }

      Query.createRead(Repository.getMetadata().getMetaclass("XJContact"), null,
         new Pair(Symbol.EQ, new Pair(parse("(@@ XJDoc conGroups contact)"),
         new Pair(new OID(new Object[]{"1"})))), null, -1, 0, false, Query.SEC_NONE, m_context).read();

      Query.createRead(Repository.getMetadata().getMetaclass("XJAddress"), (Pair)parse("(addrGUID)"),
         new Pair(Symbol.EQ, new Pair(parse("(@@ XJContact addresses)"),
         new Pair(new OID(new Object[]{"1"})))), (Pair)parse("((addrGUID . #t)((@) . #t))"), -1, 0, false,
         Query.SEC_NONE, m_context).read();
   }

   protected Object parse(String sExpr)
   {
      SchemeParser parser = new SchemeParser(m_env);
      return parser.parse(new StringReader(sExpr), null);
   }
}
