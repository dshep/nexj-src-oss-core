// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.Iterator;

import junit.framework.TestCase;

import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.RelationalClassDenorm;
import nexj.core.meta.persistence.sql.RelationalClassMapping;
import nexj.core.meta.persistence.sql.RelationalMapping;
import nexj.core.meta.persistence.sql.RelationalPrimitiveMapping;
import nexj.core.runtime.Context;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

public class MetaclassTest extends TestCase
{
   private Metaclass m_contact;
   private Metaclass m_contactType;
   private Metaclass m_principal;
   private Metaclass m_address;
   private Metaclass m_busAddr;
   private Metaclass m_hrRequest;
   private Metaclass m_accountTypeEnum;
   private Metaclass m_principalRef;
   private ClassAspect m_versioned;
   private ClassAspect m_principalRefAspect;

   public MetaclassTest(String name)
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

      m_contact = metadata.getMetaclass("Contact");
      m_contactType = metadata.getMetaclass("ContactType");
      m_principal = metadata.getMetaclass("Principal");
      m_address = metadata.getMetaclass("Address");
      m_busAddr = metadata.getMetaclass("BusinessAddress");
      m_hrRequest = metadata.getMetaclass("HRRequest");
      m_accountTypeEnum = metadata.getMetaclass("AccountTypeEnum");
      m_principalRef = metadata.getMetaclass("PrincipalRef");
      m_versioned = metadata.getClassAspect("VERSIONED");
      m_principalRefAspect = metadata.getClassAspect("PRINCIPAL_REF");
   }

   public void testAttributeInheritance()
   {
      Metaclass aspectBase = Repository.getMetadata().getClassAspect("PRINCIPAL_REF_2");
      Metaclass aspectDerived = Repository.getMetadata().getClassAspect("PRINCIPAL_REF");
      Metaclass classBase = Repository.getMetadata().getMetaclass("PrincipalRef");
      Metaclass classDerived = Repository.getMetadata().getMetaclass("PrincipalRef2");

      assertEquals("aspectBaseAttrCaption", aspectBase.getAttribute("aspectBaseAttr").getCaption());
      assertEquals(aspectBase.getAttribute("aspectBaseAttr").getCaption(),
                   classDerived.getAttribute("aspectBaseAttr").getCaption());
      assertEquals("aspectDerivedAttrCaption",
                   aspectDerived.getAttribute("aspectDerivedAttr").getCaption());
      assertEquals(aspectDerived.getAttribute("aspectDerivedAttr").getCaption(),
                   classDerived.getAttribute("aspectDerivedAttr").getCaption());
      assertEquals("classBaseAttrCaption", classBase.getAttribute("classBaseAttr").getCaption());
      assertEquals(classBase.getAttribute("classBaseAttr").getCaption(),
                   classDerived.getAttribute("classBaseAttr").getCaption());
      assertEquals("class derived attr", classDerived.getAttribute("classDerivedAttr").getCaption());
   }

   public void testDependency()
   {
      assertEquals("(Attribute classCode)", String.valueOf(m_contact.dependency(
         Pair.list(Symbol.INSTANCE_P, Pair.attribute(""), Symbol.define("Doctor")), false,
         new Machine(new GlobalEnvironment(m_contact.getMetadata().getGlobalEnvironment()), (Context)null))));
   }

   public void testDeriveAttribute()
   {
      Metaclass base = new Metaclass("base");
      Attribute baseAttr = new Attribute("attr");
      ClassAspect baseAspect = new ClassAspect("baseAspect");
      Attribute baseAspectAttr = new Attribute("baseAspectAttr");
      Attribute basePointcutAttr = new Attribute("baseAspectAttr");
      Metaclass derived = new Metaclass("derrived");
      Attribute derivedAttr = new Attribute("attr");
      ClassAspect derivedAspect = new ClassAspect("derivedAspect");
      Attribute derivedAspectAttr = new Attribute("derivedAspectAttr");
      Attribute derivedPointcutAttr = new Attribute("derivedAspectAttr");

      assertEquals(baseAttr.getCaption(), baseAttr.getName());

      baseAttr.setCaption("baseCaption");
      baseAttr.setType(Primitive.STRING);
      baseAspectAttr.setCaption("baseAspectCaption");
      baseAspectAttr.setType(Primitive.DOUBLE);
      basePointcutAttr.setType(Primitive.DOUBLE);
      derivedAttr.setType(Primitive.STRING);
      derivedAspectAttr.setCaption("derrivedAspectCaption");
      derivedAspectAttr.setType(Primitive.LONG);
      derivedPointcutAttr.setType(Primitive.LONG);
      base.addAttribute(baseAttr);
      base.addDerived(derived);
      baseAspect.addAttribute(baseAspectAttr);
      derivedAspect.addAspect(baseAspect);
      derived.addAspect(derivedAspect);
      derived.addAttribute(derivedAttr);
      derived.addAttribute(basePointcutAttr);
      derived.addAttribute(derivedPointcutAttr);
      derivedAspect.addAttribute(derivedAspectAttr);

      baseAspect.resolveInheritance();
      derivedAspect.resolveInheritance();
      base.resolveInheritance();

      assertEquals(baseAttr.getCaption(), derivedAttr.getCaption());
      assertEquals(baseAspectAttr.getCaption(), basePointcutAttr.getCaption());
      assertEquals(derivedAspectAttr.getCaption(), derivedPointcutAttr.getCaption());
   }

   public void testIsPrimitive()
   {
      assertEquals(false, m_contact.isPrimitive());
   }

   public void testGetEvent()
   {
      assertEquals("read", m_contact.getEvent(0).getName());
   }

   public void testGetEventCount()
   {
      assertEquals(53, m_contact.getEventCount());
   }

   public void testGetEventIterator()
   {
      Iterator itr = m_contact.getEventIterator();

      int nCount = m_contact.getEventCount();

      for (int i = 0; i < nCount; ++i)
      {
         itr.next();
      }

      assertFalse(itr.hasNext());
   }

   public void testGetAttribute()
   {
      assertEquals("firstName", m_contact.getAttribute("firstName").getName());

      try
      {
         m_contact.getAttribute("a");
         fail("Expected MetadataLookupException");
      }
      catch (MetadataLookupException e)
      {
      }
   }

   public void testFindAttribute()
   {
      assertEquals("lastName", m_contact.findAttribute("lastName").getName());
      assertNull(m_contact.findAttribute("a"));
   }

   public void testGetInstanceAttribute()
   {
      assertEquals("firstName", m_contact.getInstanceAttribute(0).getName());
   }

   public void testGetInstanceAttributeCount()
   {
      assertEquals(62, m_contact.getInstanceAttributeCount());
   }

   public void testGetStaticAttribute()
   {
      assertEquals("defaultReadPrincipalName", m_contact.getStaticAttribute(0).getName());
   }

   public void testGetStaticAttributeCount()
   {
      assertEquals(1, m_contact.getStaticAttributeCount());
   }

   public void testGetStaticAttributeIterator()
   {
      Iterator itr = m_contact.getStaticAttributeIterator();
      itr.next();
      assertFalse(itr.hasNext());
   }

   public void testGetAttributeCount()
   {
      assertEquals(63, m_contact.getAttributeCount());
      assertEquals(13, m_principalRef.getAttributeCount());
   }

   public void testGetAttributeIterator()
   {
      Iterator itr = m_contact.getAttributeIterator();

      int nCount = m_contact.getAttributeCount();

      for (int i = 0; i < nCount; ++i)
      {
         itr.next();
      }

      assertFalse(itr.hasNext());
   }

   public void testGetDerived()
   {
      assertEquals("Group", m_principal.getDerived(0).getName());
   }

   public void testGetDerivedCount()
   {
      assertEquals(5, m_contact.getDerivedCount());
      assertEquals(3, m_principal.getDerivedCount());
   }

   public void testGetDerivedIterator()
   {
      Iterator itr = m_contact.getDerivedIterator();
      assertTrue(itr.hasNext());
      itr = m_principal.getDerivedIterator();
      itr.next();
      itr.next();
      itr.next();
      assertFalse(itr.hasNext());
   }

   public void testGetBase()
   {
      assertNull(Repository.getMetadata().getMetaclass("Object").getBase());
      assertEquals("Object", m_contact.getBase().getName());
      assertEquals("Object", m_principal.getBase().getName());
      assertSame(m_principal, m_principal.getDerived(0).getBase());
   }

   public void testGetMetadata()
   {
      assertSame(Repository.getMetadata(), m_contact.getMetadata());
   }

   public void testIsUpcast()
   {
      assertTrue(Repository.getMetadata().getMetaclass("Object").isUpcast(m_principal));
      assertFalse(m_principal.isUpcast(Repository.getMetadata().getMetaclass("Object")));
   }

   public void testGetPersistenceMapping()
   {
      assertNotNull(m_contact.getPersistenceMapping());

      RelationalMapping hrRequestMapping = (RelationalMapping)m_hrRequest.getPersistenceMapping();

      assertNotNull(hrRequestMapping);
      assertNotNull(hrRequestMapping.getLockingAttribute());
      assertEquals("version", hrRequestMapping.getLockingAttribute().getName());

      RelationalPrimitiveMapping versionMapping =
         (RelationalPrimitiveMapping)hrRequestMapping.getAttributeMapping(m_hrRequest.getAttribute("version"));

      assertNotNull(versionMapping);
      assertEquals("version", versionMapping.getColumn().getName());
      assertEquals("test.HRRequest", versionMapping.getColumn().getTable().getName());

      RelationalMapping principalRefMapping = (RelationalMapping)m_principalRef.getPersistenceMapping();

      assertNotNull(principalRefMapping);
      assertNotNull(principalRefMapping.getLockingAttribute());
      assertEquals("version", principalRefMapping.getLockingAttribute().getName());

      versionMapping = (RelationalPrimitiveMapping)principalRefMapping
         .getAttributeMapping(m_principalRef.getAttribute("version"));

      assertNotNull(versionMapping);
      assertEquals("version", versionMapping.getColumn().getName());
      assertEquals("PrincipalRef", versionMapping.getColumn().getTable().getName());

      RelationalClassMapping principalMapping = (RelationalClassMapping)principalRefMapping
         .getAttributeMapping(m_principalRef.getAttribute("principal"));

      assertNotNull(principalMapping);
      assertEquals("PrincipalRef.FK_Usr", principalMapping.getSourceKey().getName());
      assertEquals("Usr.PK", ((Index)principalMapping.getDestinationKey()).getName());
      assertEquals(1, principalMapping.getDenormCount());
      assertEquals("PrincipalExt.FK_Usr", ((RelationalClassDenorm)principalMapping.getDenorm(0)).getSourceKey().getName());

      principalMapping = (RelationalClassMapping)principalRefMapping
         .getAttributeMapping(m_principalRef.getAttribute("principal2"));

      assertNotNull(principalMapping);
      assertEquals("PrincipalRef.PK", principalMapping.getSourceKey().getName());
      assertEquals("Usr.PK", ((Index)principalMapping.getDestinationKey()).getName());
      assertEquals(0, principalMapping.getDenormCount());

      RelationalPrimitiveMapping principalIdMapping = (RelationalPrimitiveMapping)principalRefMapping
         .getAttributeMapping(m_principalRef.getAttribute("principalId"));

      assertNotNull(principalIdMapping);
      assertEquals("principalId", principalIdMapping.getColumn().getName());
      assertEquals("PrincipalRef", principalIdMapping.getColumn().getTable().getName());
   }

   public void testGetSelector()
   {
      assertEquals("firstName", m_contact.getSelector("firstName").getName());
      assertEquals("read", m_contact.getSelector("read").getName());
      assertEquals("ref2", m_principalRefAspect.getSelector("ref2").getName());
      assertEquals("ref2", m_principalRef.getSelector("ref2").getName());

      try
      {
         m_contact.getSelector("a");
         fail("Expected MetadataLookupException");
      }
      catch (MetadataLookupException e)
      {
      }
   }

   public void testGetSelectorCount()
   {
      assertEquals(112, m_contact.getSelectorCount());
   }

   public void testGetSelectorIterator()
   {
      Iterator itr = m_contact.getSelectorIterator();

      for (int i = m_contact.getSelectorCount(); i > 0; --i)
      {
         itr.next();
      }

      assertFalse(itr.hasNext());
   }

   public void testGetSortKeys()
   {
      assertEquals("((#t ((@) . #t)) (#t (lastName . #t) (firstName . #t) " +
         "((@) . #t)) (#f (firstName . #t) (lastName . #f)))",
         String.valueOf(m_contact.getSortKeys(null, null)));

      Attribute addresses = m_contact.getAttribute("addresses");

      assertEquals("((#f (country . #t)))",
         String.valueOf(((Metaclass)addresses.getType()).getSortKeys(new Attribute[]{addresses}, null)));

      assertEquals("((#t ((@) . #t)) (#t (name . #t) ((@) . #t)) (#f (version . #t)))",
         String.valueOf(m_principalRef.getSortKeys(null, null)));

      assertEquals("((#t ((@) . #t)) (#t (id2 . #t) (lastName . #t) (firstName . #t) ((@) . #t)))",
         String.valueOf(Repository.getMetadata().getMetaclass("PetOwner").getSortKeys(null, null)));

      assertEquals("((#t (name . #t) (value . #t)) (#t ((@) . #t)))",
         String.valueOf(Repository.getMetadata().getMetaclass("StringEnum").getSortKeys(null, null)));
   }

   public void testGetUniqueKeys()
   {
      assertNull(m_contact.getUniqueKeys());
      assertEquals("((name ruleSetVersion))", String.valueOf(Repository.getMetadata().getMetaclass("SysRule").getUniqueKeys()));
      assertEquals("((name))", String.valueOf(Repository.getMetadata().getMetaclass("City").getUniqueKeys()));
      assertEquals("((type name version oid class local))", String.valueOf(Repository.getMetadata().getMetaclass("SysWorkflow").getUniqueKeys()));
   }

   public void testGetWhere()
   {
      assertNull(null, m_address.getWhere());
      assertEquals("(= type \"Business\")", String.valueOf(m_busAddr.getWhere()));
      assertEquals("(if ((invocation-context) (quote partitioned)) (= (@ principal) (user)) #t)", String.valueOf(m_principalRef.getWhere()));
   }

   public void testGetName()
   {
      assertEquals("Contact", m_contact.getName());
   }

   public void testGetNameAttribute()
   {
      assertEquals("name", m_principal.getNameAttribute().getName());
      assertEquals("name", Repository.getMetadata().getMetaclass("Group").getNameAttribute().getName());
      assertEquals("Principal", Repository.getMetadata().getMetaclass("Group").getNameAttribute().getDeclarator().getName());
      assertEquals("fullName", Repository.getMetadata().getMetaclass("User").getNameAttribute().getName());
   }

   public void testGetVisibility()
   {
      assertEquals(Metaclass.PUBLIC, m_contact.getVisibility());
   }

   public void testGetValidation()
   {
      assertEquals("((lambda (#b) (if (eq? #b #t) (or (null? (@ count)) (> (@ count) 0)) #b)) (@ readable))", String.valueOf(m_principalRef.getValidation()));
   }

   public void testGetReadPrivilege()
   {
      assertEquals("readContact", m_contact.getReadPrivilege().getName());
      assertEquals("readContact", Repository.getMetadata().getMetaclass("Patient").getReadPrivilege().getName());
   }

   public void testGetCreatePrivilege()
   {
      assertEquals("createContact", m_contact.getCreatePrivilege().getName());
      assertEquals("createContact", Repository.getMetadata().getMetaclass("Patient").getCreatePrivilege().getName());
   }

   public void testGetUpdatePrivilege()
   {
      assertEquals("updateContact", m_contact.getUpdatePrivilege().getName());
      assertEquals("updateContact", Repository.getMetadata().getMetaclass("Doctor").getUpdatePrivilege().getName());
      assertEquals("updatePatient", Repository.getMetadata().getMetaclass("Patient").getUpdatePrivilege().getName());
   }

   public void testGetDeletePrivilege()
   {
      assertEquals("deleteContact", m_contact.getDeletePrivilege().getName());
      assertEquals("deleteContact", Repository.getMetadata().getMetaclass("Doctor").getDeletePrivilege().getName());
      assertEquals("deletePatient", Repository.getMetadata().getMetaclass("Patient").getDeletePrivilege().getName());
   }

   public void testGetReadAccessAttribute()
   {
      assertSame(m_contact.getAttribute("readable"), m_contact.getReadAccessAttribute());
      assertEquals(m_contact.getAttribute("readable").getName(), Repository.getMetadata().getMetaclass("Patient").getReadAccessAttribute().getName());
      assertNotSame(m_contact.getAttribute("readable"), Repository.getMetadata().getMetaclass("Patient").getReadAccessAttribute());
   }

   public void testGetUpdateAccessAttribute()
   {
      assertNull(m_contact.getUpdateAccessAttribute());
   }

   public void testGetDeleteAccessAttribute()
   {
      assertNull(m_address.getDeleteAccessAttribute());
   }

   public void testGetUpdateTransactionMode()
   {
      assertEquals(Event.TX_REQUIRED, m_contact.getUpdateTransactionMode());
   }

   public void testIsAspect()
   {
      assertTrue(m_versioned.isAspect());
   }

   public void testGetAspect()
   {
      assertEquals("VERSIONED", m_hrRequest.getAspect(0).getName());
      assertEquals("PRINCIPAL_REF", m_principalRef.getAspect(0).getName());
      assertEquals("VERSIONED", m_principalRef.getAspect(1).getName());
   }

   public void testHasAspect()
   {
      assertTrue(m_hrRequest.hasAspect(m_versioned));
      assertFalse(m_contactType.hasAspect(m_versioned));
      assertFalse(Repository.getMetadata().getMetaclass("AccountTypeEnum").hasAspect(m_versioned));
   }

   public void testGetAspectCount()
   {
      assertEquals(0, m_contactType.getAspectCount());
      assertEquals(1, m_hrRequest.getAspectCount());
      assertEquals(3, m_principalRef.getAspectCount());
   }

   public void testFindAspectOverride()
   {
      assertEquals(0, m_hrRequest.findAspectOverride(m_versioned));
      assertEquals(0, m_contactType.findAspectOverride(m_versioned));
   }

   public void testGetAspectOverride()
   {
      assertEquals("VERSIONED", m_hrRequest.getAspectOverride(0).getName());
      assertEquals("VERSIONED", m_contactType.getAspectOverride(0).getName());
   }

   public void testIsAspectOverrideInclusive()
   {
      assertTrue(m_hrRequest.isAspectOverrideInclusive(0));
      assertFalse(m_contactType.isAspectOverrideInclusive(0));
   }

   public void testGetAspectOverrideCount()
   {
      assertEquals(1, m_hrRequest.getAspectOverrideCount());
      assertEquals(1, m_contactType.getAspectOverrideCount());
      assertEquals(3, m_principalRef.getAspectOverrideCount());
   }

   public void testIsPointcut()
   {
      assertTrue(m_hrRequest.isPointcut());
      assertFalse(m_versioned.isPointcut());
      assertFalse(m_accountTypeEnum.isPointcut());
   }

   /*
    * Test for String toString()
    */
   public void testToString()
   {
      assertEquals("Metaclass Contact", m_contact.toString());
      assertEquals("ClassAspect VERSIONED", m_versioned.toString());
   }
}
