// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.Properties;

import nexj.core.meta.xml.XMLMetadataLoader;

import junit.framework.TestCase;

public class MetaLoaderDispatcherTest extends TestCase
{
   public MetaLoaderDispatcherTest(String name)
   {
      super(name);
   }

   // load repository with two mixins.  Access a resource from each.  Access an overridden resource.
   public void testSimpleMixinLoading()
   {
      MetadataLoaderDispatcher dispatcher = new MetadataLoaderDispatcher();
      Properties properties = new Properties();

      properties.setProperty(XMLMetadataLoader.CONNECTIONS_URL_PROPERTY, "/nexj/mixin-tests.connections");
      properties.setProperty(XMLMetadataLoader.METADATA_URL_PROPERTY, "nexj/mixin/simple");
      properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_c.url", "nexj/mixin/c");
      properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_d.url", "nexj/mixin/d");

      Metadata metadata = dispatcher.load(null, properties, XMLMetadataLoader.DEFAULT, null);

      Metaclass firstSimple = metadata.findMetaclass("FirstClassSimple");

      assertNotNull(firstSimple);
      assertEquals(1, firstSimple.getAttributeCount());

      Metaclass firstD = metadata.findMetaclass("FirstClassD");

      assertNotNull(firstD);
      assertEquals(2, firstD.getAttributeCount());

      Metaclass firstC = metadata.findMetaclass("FirstClassC");

      assertNotNull(firstC);
      assertEquals(3, firstC.getAttributeCount()); // overridden, the original from /mixin/c has only 2 attributes

      Metaclass secondC = metadata.findMetaclass("SecondClassC");

      assertNotNull(secondC);
      assertEquals(2, secondC.getAttributeCount());
   }

   // override disallowed on mixin with overridden resource
   public void testSimpleInvalidMixinLoading()
   {
      try
      {
         MetadataLoaderDispatcher dispatcher = new MetadataLoaderDispatcher();
         Properties properties = new Properties();

         properties.setProperty(XMLMetadataLoader.CONNECTIONS_URL_PROPERTY, "/nexj/mixin-tests.connections");
         properties.setProperty(XMLMetadataLoader.METADATA_URL_PROPERTY, "nexj/mixin/simple_invalid");
         properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_c.url", "nexj/mixin/c");
         properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_d.url", "nexj/mixin/d");

         dispatcher.load(null, properties, XMLMetadataLoader.DEFAULT, null);
         fail("MetadataException Expected");
      }
      catch (MetadataException e)
      {
         MetadataValidationException extract = extractException(e);

         assertEquals("err.meta.mixinConflict", extract.getErrorCode());
      }
   }

   // load 2 mixins.  One mixin is reference both by parent and by the other mixin.  Read a resource from each.  Read a resource
   // overridden from the shared mixin.
   public void testDualMixinLoading()
   {
      MetadataLoaderDispatcher dispatcher = new MetadataLoaderDispatcher();
      Properties properties = new Properties();

      properties.setProperty(XMLMetadataLoader.CONNECTIONS_URL_PROPERTY, "/nexj/mixin-tests.connections");
      properties.setProperty(XMLMetadataLoader.METADATA_URL_PROPERTY, "nexj/mixin/dual");
      properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_c.url", "nexj/mixin/c");
      properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_b.url", "nexj/mixin/b");

      Metadata metadata = dispatcher.load(null, properties, XMLMetadataLoader.DEFAULT, null);

      Metaclass firstSimple = metadata.findMetaclass("FirstClassDual");

      assertNotNull(firstSimple);
      assertEquals(1, firstSimple.getAttributeCount());

      Metaclass firstD = metadata.findMetaclass("FirstClassB");

      assertNotNull(firstD);
      assertEquals(2, firstD.getAttributeCount());

      Metaclass firstC = metadata.findMetaclass("FirstClassC");

      assertNotNull(firstC);
      assertEquals(3, firstC.getAttributeCount()); // overridden, the original from /mixin/c has only 2 attributes, overridden in /mixin/b to have 4

      Metaclass secondC = metadata.findMetaclass("SecondClassC");

      assertNotNull(secondC);
      assertEquals(2, secondC.getAttributeCount());
   }

   // Shared mixin has resource overridden by parent, but not by the second mixin.  Invalid, since parent is not allowed to override grandchild mixin, even if
   // it is identical to one of its own children.
   public void testSkippedOverrideLoading()
   {
      MetadataLoaderDispatcher dispatcher = new MetadataLoaderDispatcher();
      Properties properties = new Properties();

      properties.setProperty(XMLMetadataLoader.CONNECTIONS_URL_PROPERTY, "/nexj/mixin-tests.connections");
      properties.setProperty(XMLMetadataLoader.METADATA_URL_PROPERTY, "nexj/mixin/dual_override_1");
      properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_c.url", "nexj/mixin/c");
      properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_b_1.url", "nexj/mixin/b_1");

      Metadata metadata = dispatcher.load(null, properties, XMLMetadataLoader.DEFAULT, null);

      Metaclass firstSimple = metadata.findMetaclass("FirstClassDual");

      assertNotNull(firstSimple);
      assertEquals(1, firstSimple.getAttributeCount());

      Metaclass firstB = metadata.findMetaclass("FirstClassB");

      assertNotNull(firstB);
      assertEquals(2, firstB.getAttributeCount());

      Metaclass firstC = metadata.findMetaclass("FirstClassC");

      assertNotNull(firstC);
      assertEquals(3, firstC.getAttributeCount());
   }

   // Shared mixin.  Resource from one instance of the shared mixin is overridden, but the same resource is not overridden from the
   // second instance of the mixin.
   public void testChildOverrideMixinsLoading()
   {
      MetadataLoaderDispatcher dispatcher = new MetadataLoaderDispatcher();
      Properties properties = new Properties();

      properties.setProperty(XMLMetadataLoader.CONNECTIONS_URL_PROPERTY, "/nexj/mixin-tests.connections");
      properties.setProperty(XMLMetadataLoader.METADATA_URL_PROPERTY, "nexj/mixin/dual_override_2");
      properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_c.url", "nexj/mixin/c");
      properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_b.url", "nexj/mixin/b");

      Metadata metadata = dispatcher.load(null, properties, XMLMetadataLoader.DEFAULT, null);

      Metaclass firstSimple = metadata.findMetaclass("FirstClassDual");

      assertNotNull(firstSimple);
      assertEquals(1, firstSimple.getAttributeCount());

      Metaclass firstB = metadata.findMetaclass("FirstClassB");

      assertNotNull(firstB);
      assertEquals(2, firstB.getAttributeCount());

      Metaclass firstC = metadata.findMetaclass("FirstClassC");

      assertNotNull(firstC);
      assertEquals(4, firstC.getAttributeCount());
   }

   // Incorrect version specified for a mixin.
   public void testMixinInvalidVersionLoading()
   {
      try
      {
         MetadataLoaderDispatcher dispatcher = new MetadataLoaderDispatcher();
         Properties properties = new Properties();

         properties.setProperty(XMLMetadataLoader.CONNECTIONS_URL_PROPERTY, "/nexj/mixin-tests.connections");
         properties.setProperty(XMLMetadataLoader.METADATA_URL_PROPERTY, "nexj/mixin/dual_invalid_3");
         properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_c.url", "nexj/mixin/c");
         properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_b.url", "nexj/mixin/b");

         dispatcher.load(null, properties, XMLMetadataLoader.DEFAULT, null);
         fail("MetadataException Expected");
      }
      catch (MetadataException e)
      {
         MetadataValidationException extract = extractException(e);

         assertEquals("err.meta.mixinVersion", extract.getErrorCode());
      }
   }

   // Shared mixin, one link has correct version, the other link to the same mixin is not correct.
   public void testMixinVersionConflictLoading()
   {
      try
      {
         MetadataLoaderDispatcher dispatcher = new MetadataLoaderDispatcher();
         Properties properties = new Properties();

         properties.setProperty(XMLMetadataLoader.CONNECTIONS_URL_PROPERTY, "/nexj/mixin-tests.connections");
         properties.setProperty(XMLMetadataLoader.METADATA_URL_PROPERTY, "nexj/mixin/dual_invalid_4");
         properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_c.url", "nexj/mixin/c");
         properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_b.url", "nexj/mixin/b");

         dispatcher.load(null, properties, XMLMetadataLoader.DEFAULT, null);
         fail("MetadataException Expected");
      }
      catch (MetadataException e)
      {
         MetadataValidationException extract = extractException(e);

         assertEquals("err.meta.mixinVersion", extract.getErrorCode());
      }
   }

   // 2 mixins, one of which is linked from the base repository
   public void testBaseMixin()
   {
      MetadataLoaderDispatcher dispatcher = new MetadataLoaderDispatcher();
      Properties properties = new Properties();

      properties.setProperty(XMLMetadataLoader.CONNECTIONS_URL_PROPERTY, "/nexj/mixin-tests.connections");
      properties.setProperty(XMLMetadataLoader.METADATA_URL_PROPERTY, "nexj/mixin/root");
      properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, "nexj/mixin/base");
      properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_c.url", "nexj/mixin/c");
      properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_d.url", "nexj/mixin/d");

      Metadata metadata = dispatcher.load(null, properties, XMLMetadataLoader.DEFAULT, null);

      Metaclass firstRoot = metadata.findMetaclass("FirstClassRoot");

      assertNotNull(firstRoot);
      assertEquals(1, firstRoot.getAttributeCount());

      Metaclass firstBase = metadata.findMetaclass("FirstClassBase");

      assertNotNull(firstBase);
      assertEquals(1, firstBase.getAttributeCount());

      Metaclass firstD = metadata.findMetaclass("FirstClassD");

      assertNotNull(firstD);
      assertEquals(4, firstD.getAttributeCount()); // overridden, the original from /mixin/d has only 2 attributes

      Metaclass firstC = metadata.findMetaclass("FirstClassC");

      assertNotNull(firstC);
      assertEquals(3, firstC.getAttributeCount()); // overridden, the original from /mixin/c has only 2 attributes

      Metaclass secondC = metadata.findMetaclass("SecondClassC");

      assertNotNull(secondC);
      assertEquals(2, secondC.getAttributeCount());

      Metaclass secondD = metadata.findMetaclass("SecondClassD");

      assertNotNull(secondD);
      assertEquals(2, secondD.getAttributeCount());
   }

   // mixin from base is not overridable, yet is overridden.
   public void testBaseInvalidMixin()
   {
      try
      {
         MetadataLoaderDispatcher dispatcher = new MetadataLoaderDispatcher();
         Properties properties = new Properties();

         properties.setProperty(XMLMetadataLoader.CONNECTIONS_URL_PROPERTY, "/nexj/mixin-tests.connections");
         properties.setProperty(XMLMetadataLoader.METADATA_URL_PROPERTY, "nexj/mixin/root_invalid");
         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, "nexj/mixin/base_invalid");
         properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_c.url", "nexj/mixin/c");
         properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_d.url", "nexj/mixin/d");

         dispatcher.load(null, properties, XMLMetadataLoader.DEFAULT, null);
         fail("MetadataException Expected");
      }
      catch (MetadataException e)
      {
         MetadataValidationException extract = extractException(e);

         assertEquals("err.meta.mixinConflict", extract.getErrorCode());
      }
   }

   // mixin links a mixin which is also the base repository
   public void testBaseAndMixin()
   {
      MetadataLoaderDispatcher dispatcher = new MetadataLoaderDispatcher();
      Properties properties = new Properties();

      properties.setProperty(XMLMetadataLoader.CONNECTIONS_URL_PROPERTY, "/nexj/mixin-tests.connections");
      properties.setProperty(XMLMetadataLoader.METADATA_URL_PROPERTY, "nexj/mixin/root_base_linked");
      properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, "nexj/mixin/c");
      properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_c.url", "nexj/mixin/c");
      properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_b.url", "nexj/mixin/b");

      // should validate
      dispatcher.load(null, properties, XMLMetadataLoader.DEFAULT, null);
   }

   // mixin links a mixin which is also the base repository, but versions do not match
   public void testBaseAndInvalidMixin()
   {
      try
      {
         MetadataLoaderDispatcher dispatcher = new MetadataLoaderDispatcher();
         Properties properties = new Properties();

         properties.setProperty(XMLMetadataLoader.CONNECTIONS_URL_PROPERTY, "/nexj/mixin-tests.connections");
         properties.setProperty(XMLMetadataLoader.METADATA_URL_PROPERTY, "nexj/mixin/root_base_invalid");
         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, "nexj/mixin/c2");
         properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_c.url", "nexj/mixin/c");
         properties.setProperty("meta.mixin.http_www_nexjsystems_com_ns_mixin_test_b.url", "nexj/mixin/b");

         dispatcher.load(null, properties, XMLMetadataLoader.DEFAULT, null);
         fail("MetadataException Expected");
      }
      catch (MetadataException e)
      {
         MetadataValidationException extract = extractException(e);

         assertEquals("err.meta.mixinBaseVersion", extract.getErrorCode());
      }
   }

   private MetadataValidationException extractException(Throwable e)
   {
      if (e instanceof MetadataValidationException)
      {
         return (MetadataValidationException)e;
      }
      else if (e instanceof MetadataCompoundValidationException)
      {
         MetadataCompoundValidationException compound = (MetadataCompoundValidationException)e;
         return extractException((Throwable)compound.getExceptionIterator().next());
      }
      else if (e instanceof MetadataException && e.getCause() != null)
      {
         return extractException(e.getCause());
      }

      fail("Unexpected exception: " + e);

      return null;
   }
}
