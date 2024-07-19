// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import junit.framework.TestCase;

import nexj.core.util.PrintWriter;
import nexj.core.util.SysUtil;
import nexj.core.util.Undefined;

public class AttributeTest extends TestCase
{
   private Attribute m_name;
   private Attribute m_typeCode;
   private Attribute m_ugassocs;
   private Attribute m_contact;
   private Attribute m_contact2;
   private Attribute m_fullName;
   private Attribute m_firstName;
   private Attribute m_lastName;
   private Attribute m_type;
   private Attribute m_user;
   private Attribute m_uname;
   private Attribute m_busAddr; 
   private Attribute m_busAddr2; 
   private Attribute m_dynAddr; 
   private Attribute m_typeName;
   private Attribute m_readPrincipal;
   private Attribute m_addressType;
   private Attribute m_aop1;
   private Attribute m_isDoctorPhone;

   public AttributeTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      Metadata metadata = Repository.getMetadata();
      Metaclass user = metadata.getMetaclass("User");
      
      m_name = user.getAttribute("name");
      m_typeCode = user.getAttribute("typeCode");
      m_ugassocs = user.getAttribute("ugassocs");

      m_contact = metadata.getMetaclass("Address").getAttribute("contact");
      m_contact2 = metadata.getMetaclass("BusinessAddress").getAttribute("contact");
      
      Metaclass contact = Repository.getMetadata().getMetaclass("Contact");
      
      m_fullName = contact.getAttribute("fullName");
      m_firstName = contact.getAttribute("firstName");
      m_lastName = contact.getAttribute("lastName");
      m_type = contact.getAttribute("type");
      m_user = contact.getAttribute("user");
      m_uname = contact.getAttribute("uname");
      m_busAddr = contact.getAttribute("businessAddress");
      m_busAddr2 = contact.getAttribute("businessAddress2");
      m_dynAddr = contact.getAttribute("dynAssocAddresses");
      m_readPrincipal = contact.getAttribute("readPrincipal");
      m_typeName = metadata.getMetaclass("ContactType").getAttribute("type");
      m_addressType = metadata.getMetaclass("BusinessAddress").getAttribute("type");
      m_aop1 = metadata.getMetaclass("TestPointcut").getAttribute("AOP1");
      m_isDoctorPhone = metadata.getMetaclass("Phone").getAttribute("isDoctorPhone");
   }

   /**
    * @see junit.framework.TestCase#tearDown()
    */
   protected void tearDown() throws Exception
   {
      super.tearDown();

      m_name = m_typeCode = m_ugassocs = m_contact = m_contact2 = m_fullName = m_firstName = m_lastName = m_type = m_user = m_uname = 
         m_busAddr = m_busAddr2 = m_dynAddr = m_typeName = m_readPrincipal = m_addressType = m_aop1 = null;
   }

   public void testDeriveFrom()
   {
      Attribute base = new Attribute("base");
      Attribute child = new Attribute("child");

      assertEquals(base.getName(), base.getCaption());

      child.deriveFrom(base);
      assertEquals(child.getName(), child.getCaption());

      base.setCaption("baseCaption");
      child.deriveFrom(base);
      assertEquals(base.getCaption(), child.getCaption());

      child.setCaption("childCaption");
      child.deriveFrom(base);
      assertEquals("childCaption", child.getCaption());
   }

   public void testIsAttribute()
   {
      assertEquals(true, m_name.isAttribute());
   }

   public void testIsRequired()
   {
      assertEquals(true, m_typeCode.isRequired());
      assertEquals(false, m_ugassocs.isRequired());
   }

   public void testIsCollection()
   {
      assertEquals(false, m_name.isCollection());
      assertEquals(true, m_ugassocs.isCollection());
   }
   
   public void testIsCached()
   {
      assertTrue(m_name.isCached());
      assertFalse(m_fullName.isCached());
      assertFalse(Repository.getMetadata().getMetaclass("Doctor").getAttribute("fullName").isCached());
      assertTrue(Repository.getMetadata().getMetaclass("Patient").getAttribute("fullName").isCached());
      assertTrue(m_uname.isCached());
   }
   
   public void testIsReadOnly()
   {
      assertFalse(m_name.isReadOnly());
      assertTrue(m_fullName.isReadOnly());
   }

   public void testGetOrdinal()
   {
      assertEquals(0, m_name.getOrdinal());
      assertEquals(1, m_typeCode.getOrdinal());
      assertEquals((SysUtil.ENTERPRISE) ? 15 : 12, m_ugassocs.getOrdinal());
      assertEquals(2, m_aop1.getOrdinal());
   }

   public void testGetValue()
   {
      assertEquals(Undefined.VALUE, m_name.getValue());
      assertEquals("U", m_typeCode.getValue());
      assertEquals("PRINCIPAL_REF", m_aop1.getValue());
      assertEquals("PRINCIPAL_REF_2", Repository.getMetadata().getMetaclass("TestPointcut2").getAttribute("AOP1").getValue());
   }

   public void testGetValueFunction()
   {
      assertNull(m_name.getValueFunction());
      assertNotNull(m_typeCode.getValueFunction());
   }

   public void testGetInitializer()
   {
      assertEquals(Undefined.VALUE, m_name.getInitializer());
   }
   
   public void testGetWhere()
   {
      assertNull(m_busAddr.getWhere());
      assertEquals("(= type \"Business\")", String.valueOf(m_busAddr2.getWhere()));
   }

   public void testGetDependency()
   {
      assertNull(m_name.getDependency());
      assertEquals("(Attribute firstName Attribute lastName (Attribute type Attribute type) (Attribute user Attribute name))", String.valueOf(m_fullName.getDependency()));
      assertEquals("((Attribute addresses Attribute country (Attribute contact Attribute firstName Attribute addresses)))", String.valueOf(m_dynAddr.getDependency()));
      assertEquals("((Attribute contact Attribute classCode))", String.valueOf(m_isDoctorPhone.getDependency()));
   }

   public void testGetCumulativeDependency()
   {
      assertNull(m_name.getCumulativeDependency());
      
      Attribute fullName = Repository.getMetadata().getMetaclass("Patient").getAttribute("fullName");
      
      assertSame(fullName.getDependency(), fullName.getCumulativeDependency());
      assertNotSame(m_fullName.getDependency(), m_fullName.getCumulativeDependency());
      assertEquals("(Attribute middleName Attribute firstName Attribute lastName (Attribute type Attribute type) (Attribute user Attribute name))", String.valueOf(m_fullName.getCumulativeDependency()));
      assertEquals("(Attribute firstName Attribute middleName Attribute lastName)", String.valueOf(fullName.getCumulativeDependency()));
   }
   
   public void testGetInverseDependency()
   {
      assertEquals("names loginName contact->(uname fullName)", String.valueOf(m_name.getInverseDependency()));
      assertEquals("wizardIcon addresses->(contact->(dynAssocAddresses)) fullNameDetail fullName2 fullName",
         String.valueOf(m_firstName.getInverseDependency()));
      assertEquals("lastNameInitial lastNameLengthPlus1 alphaGroup note fullName2 fullName", String.valueOf(m_lastName.getInverseDependency()));
      assertEquals("isTorontoBasedEmployee fullName", String.valueOf(m_type.getInverseDependency()));
      assertEquals("uname fullName addresses->(user)", String.valueOf(m_user.getInverseDependency()));
      assertNull(m_typeName.getInverseDependency());
      assertEquals("persistent: contact->(persistent: depend1 tax isTorontoBasedEmployee businessAddressCount businessAddresses businessAddress4) isHome typeStr2 dummyAccess updatable deletable readable",
         String.valueOf(Repository.getMetadata().getMetaclass("Address").getAttribute("type").getInverseDependency()));
      assertEquals("persistent: tax isTorontoBasedEmployee firstAddress businessAddressCount addresses->(contact->(dynAssocAddresses)) dynAssocAddresses businessAddresses businessAddress4 addressCount",
         String.valueOf(Repository.getMetadata().getMetaclass("Contact").getAttribute("addresses").getInverseDependency()));
      assertEquals("readable",
         String.valueOf(Repository.getMetadata().getMetaclass("PrincipalRef2").getAttribute("principal").getInverseDependency()));
   }
   
   public void testGetType()
   {
      assertEquals("string", m_name.getType().getName());
      assertEquals("UserGroupAssoc", m_ugassocs.getType().getName());
   }

   public void testGetReverse()
   {
      assertNull(m_name.getReverse());
      assertEquals("user", m_ugassocs.getReverse().getName());
      assertEquals("addresses", m_contact.getReverse().getName());
      assertEquals("addresses", m_contact2.getReverse().getName());
   }

   public void testGetPersistenceRoot()
   {
      assertSame(m_contact, m_contact.getPersistenceRoot());
      assertSame(m_contact, m_contact2.getPersistenceRoot());
      assertEquals("Principal", m_name.getPersistenceRoot().getMetaclass().getName());
   }

   public void testIsStatic()
   {
      assertEquals(false, m_name.isStatic());
      assertEquals(true, Repository.getMetadata().getMetaclass("Contact").getAttribute("defaultReadPrincipalName").isStatic());
      assertEquals(true, m_aop1.isStatic());
   }

   public void testGetMetaclass()
   {
      assertEquals("User", m_name.getMetaclass().getName());
      assertEquals("User", m_typeCode.getMetaclass().getName());
      assertEquals("User", m_ugassocs.getMetaclass().getName());
   }

   public void testGetDeclarator()
   {
      assertEquals("Principal", m_name.getDeclarator().getName());
      assertEquals("User", m_typeCode.getDeclarator().getName());
      assertEquals("User", m_ugassocs.getDeclarator().getName());
   }

   public void testGetRootDeclarator()
   {
      assertEquals("Principal", m_name.getRootDeclarator().getName());
      assertEquals("Principal", m_typeCode.getRootDeclarator().getName());
   }

   public void testGetName()
   {
      assertEquals("name", m_name.getName());
      assertEquals("typeCode", m_typeCode.getName());
      assertEquals("ugassocs", m_ugassocs.getName());
   }

   public void testGetVisibility()
   {
      assertEquals(Metaclass.PUBLIC, m_readPrincipal.getVisibility());
   }
   
   public void testGetReadPrivilege()
   {
      assertEquals("readContact", m_readPrincipal.getReadPrivilege().getName());
   }

   public void testGetUpdatePrivilege()
   {
      assertEquals("updateContactSecurity", m_readPrincipal.getUpdatePrivilege().getName());
   }
   
   public void testGetAccessAttribute()
   {
      assertNull("readable", m_readPrincipal.getAccessAttribute());
   }
   
   public void testGetEnumeration()
   {
      assertNull(m_type.getEnumeration());
      assertEquals("AddressTypeEnum", m_addressType.getEnumeration().getName());
   }

   public void testIsConstrained()
   {
      assertFalse(m_type.isConstrained());
      assertTrue(m_addressType.isConstrained());
   }

   public void testIsValueOverridden()
   {
      assertTrue(m_fullName.isValueOverridden(null));
      assertFalse(m_uname.isValueOverridden(null));
      assertTrue(Repository.getMetadata().getMetaclass("User").getAttribute("fullName").isValueOverridden(null));
      assertFalse(Repository.getMetadata().getMetaclass("Patient").getAttribute("fullName").isValueOverridden(null));
   }

   public void testGetDispatchedValue()
   {
      assertEquals("(if (instance? (@) Patient) ((string-append (@ firstName) \" \" (@ middleName) \" \" (@ lastName)) " +
         "(quote trim)) ((string-append (ifnull (@ firstName) \"\") \" \" (ifnull (@ lastName) \"\") " +
         "(if (null? (@ type)) \"\" (string-append \" [\" (@ type type) \"]\")) (if (null? (@ user)) \"\" " +
         "(string-append \" {\" (((@ user name) (quote toString)) (quote toString)) \"}\"))) (quote trim)))",
         PrintWriter.toString(m_fullName.getDispatchedValue()));
   }

   public void testToString()
   {
      assertEquals("Attribute name", m_name.toString());
   }
}
