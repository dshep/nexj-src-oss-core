// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import junit.framework.TestCase;

import nexj.core.meta.ui.AttributeMeta;
import nexj.core.scripting.Pair;

/**
 * Test dependencies for calculated attribute value functions.
 */
public class AttributeDependencyTest extends TestCase
{
   public void testContactLastNameLengthPlus1()
   {
      Metadata metadata = Repository.getMetadata();
      Metaclass clazz = metadata.getMetaclass("Contact");
      AttributeMeta attr = clazz.getAttributeMeta("lastNameLengthPlus1");

      // (+ (string-length (@ lastName)) 1)
      assertTrue(attr.isClientCalculable());

      assertNotNull(attr.getValueFunction());
      assertNotNull(attr.getValueDependencyAssociations());

      assertNull(attr.getValueDependencyAssociations().getTail());
      assertEquals(Pair.list("lastName"), attr.getValueDependencyAssociations().getHead());
   }

   public void testContactIsHome()
   {
      Metadata metadata = Repository.getMetadata();
      Metaclass clazz = metadata.getMetaclass("Contact");
      AttributeMeta attr = clazz.getAttributeMeta("isHome");

      // (if (null? (@ firstAddress)) #f (@ firstAddress isHome))
      assertTrue(attr.isClientCalculable());
      assertNotNull(attr.getValueDependencyAssociations());

      Object[] deps = Pair.toArray(attr.getValueDependencyAssociations());

      assertEquals(1, deps.length);
      assertEquals(Pair.list("firstAddress", "isHome"), deps[0]);
   }

   public void testContactIsHome2()
   {
      Metadata metadata = Repository.getMetadata();
      Metaclass clazz = metadata.getMetaclass("Contact");
      AttributeMeta attr = clazz.getAttributeMeta("isHome2");

      // (if (null? (@ supportGroup)) #f (@ firstAddress isHome))
      assertTrue(attr.isClientCalculable());
      assertNotNull(attr.getValueDependencyAssociations());

      Object[] deps = Pair.toArray(attr.getValueDependencyAssociations());

      assertEquals(2, deps.length);
      assertEquals(Pair.list("firstAddress", "isHome"), deps[0]);
      assertEquals(Pair.list("supportGroup"), deps[1]);
   }

   public void testPatientIcon()
   {
      Metadata metadata = Repository.getMetadata();
      Metaclass clazz = metadata.getMetaclass("Patient");
      AttributeMeta attr = clazz.getAttributeMeta("icon");

      assertFalse(attr.isClientCalculable());
   }

   public void testContactReadable()
   {
      Metadata metadata = Repository.getMetadata();
      Metaclass clazz = metadata.getMetaclass("Contact");
      AttributeMeta attr = clazz.getAttributeMeta("readable");

      assertFalse(attr.isClientCalculable());
   }

   public void testContactFullName2()
   {
      Metadata metadata = Repository.getMetadata();
      Metaclass clazz = metadata.getMetaclass("Contact");
      AttributeMeta attr = clazz.getAttributeMeta("fullName2");

      // (string-affix (@ lastName) ", " (@ firstName))
      assertTrue(attr.isClientCalculable());

      Object[] deps = Pair.toArray(attr.getValueDependencyAssociations());

      assertEquals(2, deps.length);
      assertEquals(Pair.list("firstName"), deps[0]);
      assertEquals(Pair.list("lastName"), deps[1]);
   }

   public void testContactFullDetail()
   {
      Metadata metadata = Repository.getMetadata();
      Metaclass clazz = metadata.getMetaclass("Contact");
      AttributeMeta attr = clazz.getAttributeMeta("fullNameDetail");

      // (string-affix (@ fullName2) " " (if (null? (@ firstName)) '()
      // (string-append "(" (@ firstName) ")")))
      assertTrue(attr.isClientCalculable());

      Object[] deps = Pair.toArray(attr.getValueDependencyAssociations());

      assertEquals(2, deps.length);
      assertEquals(Pair.list("firstName"), deps[0]);
      assertEquals(Pair.list("fullName2"), deps[1]);
   }

   public void testContactAllowClearPreferred()
   {
      Metadata metadata = Repository.getMetadata();
      Metaclass clazz = metadata.getMetaclass("Contact");
      AttributeMeta attr = clazz.getAttributeMeta("allowClearPreferred");

      // (and (@ tax) (not (null? ((@ supportGroup) (@ note)))))
      // Dynamic selector: ---------------------------^
      assertFalse(attr.isClientCalculable());
   }

   public void testContactAlphaGroup()
   {
      Metadata metadata = Repository.getMetadata();
      Metaclass clazz = metadata.getMetaclass("Contact");
      AttributeMeta attr = clazz.getAttributeMeta("alphaGroup");

      // (if (or (null? (@ lastName)) (= 0 (string-length (@ lastName))))
      // "(empty)" (string (string-ref (@ lastName) 0)))
      assertTrue(attr.isClientCalculable());

      Object[] deps = Pair.toArray(attr.getValueDependencyAssociations());

      assertEquals(1, deps.length);
      assertEquals(Pair.list("lastName"), deps[0]);
   }

   public void testContactWizardIcon()
   {
      // (if (null? (@ icon)) "32/folder" ((@ icon)'replaceAll "16" "32"))
      Metadata metadata = Repository.getMetadata();
      Metaclass clazz = metadata.getMetaclass("Contact");
      AttributeMeta attr = clazz.getAttributeMeta("wizardIcon");

      assertFalse(attr.isClientCalculable());
   }

   public void testContactIsTorontoBasedEmployee()
   {
      // (and (= (@ type type) "Employee") (any (and (= (@ addresses type)
      // "Business") (= (@ addresses city) "Toronto"))))
      Metadata metadata = Repository.getMetadata();
      Metaclass clazz = metadata.getMetaclass("Contact");
      AttributeMeta attr = clazz.getAttributeMeta("isTorontoBasedEmployee");

      assertFalse(attr.isClientCalculable());
   }

   public void testAddressReadable()
   {
      // (and (not (null? (@ type))) (@ contact readable))
      Metadata metadata = Repository.getMetadata();
      Metaclass clazz = metadata.getMetaclass("Address");
      AttributeMeta attr = clazz.getAttributeMeta("readable");

      assertTrue(attr.isClientCalculable());
   }

   public void testInContactIsCommit()
   {
      // (isCommit)
      Metadata metadata = Repository.getMetadata();
      Metaclass clazz = metadata.getMetaclass("Contact");
      AttributeMeta attr = clazz.getAttributeMeta("isCommit");

      assertFalse(attr.isClientCalculable());
   }

   public void testInContactClientLib()
   {
      // (+ (one-plus (@ tax)) (two))
      Metadata metadata = Repository.getMetadata();
      Metaclass clazz = metadata.getMetaclass("Contact");
      AttributeMeta attr = clazz.getAttributeMeta("clientLib");

      assertTrue(attr.isClientCalculable());

      Object[] deps = Pair.toArray(attr.getValueDependencyAssociations());

      assertEquals(1, deps.length);
      assertEquals(Pair.list("tax"), deps[0]);
   }

   public void testObjSize()
   {
      // (if (null? (@ data)) 0 (vector-length (@ data data)))
      Metadata metadata = Repository.getMetadata();
      Metaclass clazz = metadata.getMetaclass("Obj");
      AttributeMeta attr = clazz.getAttributeMeta("size");

      assertFalse(attr.isClientCalculable());
   }
}
