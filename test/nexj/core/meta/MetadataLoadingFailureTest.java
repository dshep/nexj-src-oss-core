// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.Properties;

import nexj.core.meta.xml.XMLMetadataLoader;

import junit.framework.TestCase;


public class MetadataLoadingFailureTest extends TestCase
{   
   public final static String META_URL = "/nexj/invalidbase";
   public final static String BASE_URL = "";
   
   /**
    * Constructor for ValidationExceptionTest.
    * @param name
    */
   public MetadataLoadingFailureTest(String name)
   {
      super(name);
   }

   /**
    * Tests that invalid ZIP messages are detected.
    */
   public void testZIPValidationExceptions() throws Exception
   {
      try
      {
         Properties properties = new Properties();

         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, BASE_URL);
         properties.setProperty("zip.test01", "true");
         new MetadataLoaderDispatcher().load(META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail();
      }
      catch (MetadataException e)
      {
         e = extractException(e);
         assertEquals("err.meta.integration.zip.missingNameChild", e.getErrorCode());
         assertEquals("Zip_Invalid_MissingNamePart files", e.getErrorArgs()[0]);
      }
   }


   /**
    * Tests that invalid XML messages are detected.
    */
   public void testXMLValidationExceptions() throws Exception
   {
      MetadataLoaderDispatcher dispatcher = new MetadataLoaderDispatcher();

      //Error: Cannot set nillable on an attribute mapping
      try
      {
         Properties properties = new Properties();
         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, BASE_URL);
         properties.setProperty("xml.test01", "true");
         dispatcher.load(META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail("MetadataException Expected");
      }
      catch (MetadataException e)
      {
         MetadataValidationException extract = extractException(e);

         assertEquals("messages/XML_Invalid_NillableAttribute.message", extract.getResourceName());
         assertEquals("err.meta.integration.xml.misplacedNillable", ((MetadataException)extract.getCause()).getErrorCode());
         assertEquals("XML_Invalid_NillableAttribute attr", ((MetadataException)extract.getCause()).getErrorArgs()[0]);
      }


      //Error: Cannot set nillable on a value mapping
      try
      {
         Properties properties = new Properties();
         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, BASE_URL);
         properties.setProperty("xml.test02", "true");
         dispatcher.load(META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail("MetadataException Expected");
      }
      catch (MetadataException e)
      {
         MetadataValidationException extract = extractException(e);

         assertEquals("messages/XML_Invalid_NillableValue.message", extract.getResourceName());
         assertEquals("err.meta.integration.xml.misplacedNillable", ((MetadataException)extract.getCause()).getErrorCode());
         assertEquals("XML_Invalid_NillableValue val", ((MetadataException)extract.getCause()).getErrorArgs()[0]);
      }
   }


   /**
    * Ensure that metadata definitions of Message objects that use the CSV
    * format comply with all of the restrictions on message structure and
    * mappings for that format.
    */
   public void testCSVValidationExceptions()
   {
      MetadataLoaderDispatcher dispatcher = new MetadataLoaderDispatcher();
      
      //Error: Delimiter and Quote are the same character.
      try
      {
         Properties properties = new Properties();
         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, BASE_URL);
         properties.setProperty("csv.test02", "true");
         dispatcher.load(META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail("Expected MetadataException");
      }
      catch (MetadataException e)
      {
         e = extractException(e);
         assertEquals("err.meta.integration.csv.quoteDelimEscapeSimilar", e.getErrorCode());
         assertEquals("CSV_Invalid_SameQuoteAndDelimiter", e.getErrorArgs()[0]);
         assertEquals(Primitive.createCharacter(','), e.getErrorArgs()[1]);
         assertEquals(Primitive.createCharacter('\\'), e.getErrorArgs()[2]);
         assertEquals(Primitive.createCharacter(','), e.getErrorArgs()[3]);
      }
      
      
      //Error: A field or group of fields has been forced to be quoted, but
      //no quote character is set.
      try
      {
         Properties properties = new Properties();
         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, BASE_URL);
         properties.setProperty("csv.test03", "true");
         dispatcher.load(META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail();
      }
      catch (MetadataException e)
      {
         e = extractException(e);
         assertEquals("err.meta.integration.csv.quotedButMissingQuote", e.getErrorCode());
         assertEquals("CSV_Invalid_MissingQuote Row lastName", e.getErrorArgs()[0]);
      }
      
      
      //Error: Missing the top level collection (CSV file must have rows)
      try
      {
         Properties properties = new Properties();
         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, BASE_URL);
         properties.setProperty("csv.test04", "true");
         dispatcher.load(META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail();
      }
      catch (MetadataException e)
      {
         e = extractException(e);
         assertEquals("err.meta.integration.csv.mustHaveToplevelCollection", e.getErrorCode());
         assertEquals("CSV_Invalid_MissingToplevelCollection firstName", e.getErrorArgs()[0]);
      }
      
      
      //Error: Nested collection (CSV is flat)
      try
      {
         Properties properties = new Properties();
         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, BASE_URL);
         properties.setProperty("csv.test05", "true");
         dispatcher.load(META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail();
      }
      catch (MetadataException e)
      {
         e = extractException(e);
         assertEquals("err.meta.integration.csv.noNestedCollections", e.getErrorCode());
         assertEquals("CSV_Invalid_NestedCollection Row Phones", e.getErrorArgs()[0]);
      }
      
      
      //Error: Two parts map to the same CSV field
      try
      {
         Properties properties = new Properties();
         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, BASE_URL);
         properties.setProperty("csv.test06", "true");
         dispatcher.load(META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail();
      }
      catch (MetadataException e)
      {
         e = extractException(e);
         assertEquals("err.meta.integration.csv.duplicateOrdinal", e.getErrorCode());
         
         //Require additional information from exception: names of parts that are duplicated.
         assertEquals("CSV_Invalid_SameOrdinals Row lastName", e.getErrorArgs()[0]);
         assertEquals("CSV_Invalid_SameOrdinals Row Phone1 number", e.getErrorArgs()[1]);
         assertEquals(Primitive.createInteger(5), e.getErrorArgs()[2]);
      }
      
      
      //Error: A required message part was mapped to a CSV field that
      //occurs after the optional CSV fields have started.
      try
      {
         Properties properties = new Properties();
         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, BASE_URL);
         properties.setProperty("csv.test07", "true");
         dispatcher.load(META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail();
      }
      catch (MetadataException e)
      {
         e = extractException(e);
         assertEquals("err.meta.integration.csv.cannotMixRequiredAndOptionalParts", e.getErrorCode());
         
         //Require additional information from exception: names of parts that are duplicated.
         assertEquals("CSV_Invalid_RequiredAfterOptional Row balance", e.getErrorArgs()[0]);
      }
      
      
      //Error: 
      try
      {
         Properties properties = new Properties();
         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, BASE_URL);
         properties.setProperty("csv.test08", "true");
         dispatcher.load(META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail();
      }
      catch (MetadataException e)
      {
         e = extractException(e);
         assertEquals("err.meta.integration.csv.duplicateFieldNames", e.getErrorCode());
         
         //Require additional information from exception: names of parts with
         //same field name, and the field name they share.
         assertEquals("CSV_Invalid_DuplicateFieldNames Row balance", e.getErrorArgs()[0]);
         assertEquals("CSV_Invalid_DuplicateFieldNames Row Phone1 location", e.getErrorArgs()[1]);
         assertEquals("location", e.getErrorArgs()[2]);
      }
   }

   public void testMessageInheritanceValidationExceptions()
   {
      MetadataLoaderDispatcher dispatcher = new MetadataLoaderDispatcher();
      
      // Error: Cannot mix formats when inheriting
      try
      {
         Properties properties = new Properties();
         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, BASE_URL);
         properties.setProperty("inherit.test01", "true");
         dispatcher.load(META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail("Expected Exception");
      }
      catch (MetadataException e)
      {
         e = extractException(e);
         assertEquals("err.meta.integration.baseFormatMismatch", e.getErrorCode());
      }
   }

   public void testMessageInheritanceCycle()
   {
      MetadataLoaderDispatcher dispatcher = new MetadataLoaderDispatcher();

      // Error: Cannot have circular inheritance
      try
      {
         Properties properties = new Properties();
         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, BASE_URL);
         properties.setProperty("inherit.test02", "true");
         dispatcher.load(META_URL, properties, XMLMetadataLoader.VALIDATED_ONLY, null);
         fail("Expected Exception");
      }
      catch (MetadataException e)
      {
         e = extractException(e);
         assertEquals("err.meta.integration.circularInheritance", e.getErrorCode());
         assertTrue(((String)e.getErrorArgs()[0]).startsWith("CircularInheritance_"));
      }
   }

   public static MetadataValidationException extractException(Throwable e)
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
