// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.Properties;

import nexj.core.util.ProgressListener;

/**
 * Interface for reading the complete metadata graph.
 */
public interface MetadataLoader
{
   // constants

   /**
    * Default read options.
    */
   public final static int DEFAULT = 0x00;

   /**
    * Read the data sources (without schemas), channels
    * and the environments only.
    */
   public final static int ENVIRONMENT_ONLY = 0x01;

   /**
    * Do not read and initialize the runtime data.
    */
   public final static int RUNTIME_EXCLUDED = 0x02;

   /**
    * Do not read and initialize the integration.
    */
   public final static int INTEGRATION_EXCLUDED = 0x04;

   /**
    * Only validate the information. The reader will try to catch certain
    * errors and generate appropriate exceptions for them.
    */
   public final static int VALIDATED_ONLY = 0x08;
   
   /**
    * Load the metadata documentation attributes.
    */
   public final static int DOCUMENTATION_INCLUDED = 0x10;
   
   /**
    * Leave the metadata writable.
    */
   public final static int WRITABLE = 0x20;

   /**
    * Do not decrypt passwords in the metadata; leave them in encrypted form.
    */
   public final static int ENCRYPTED = 0x40;

   /**
    * Generate warnings.
    */
   public final static int WARNING = 0x80;
   
   /**
    * Read the data sources (with schemas), channels
    * and the environments only.
    */
   public final static int DATASOURCE_ONLY = 0x100;

   /**
    * Return the properties.
    */
   public final static int PROPERTIES = 0x200;

   /**
    * Disable dump validation.
    */
   public final static int DUMP_VALIDATION_DISABLED = 0x400;

   /**
    * Metadata URL property name.
    */
   public final static String METADATA_URL_PROPERTY = "meta.url";
   
   /**
    * Default metadata URL string.
    */
   public final static String DEFAULT_METADATA_URL = "/nexj/meta/";

   /**
    * The metadata integration property.
    */
   public final static String METADATA_INTEGRATION_PROPERTY = "meta.integration";

   /**
    * The metadata documentation property.
    */
   public final static String METADATA_DOCUMENTATION_PROPERTY = "meta.documentation";

   /**
    * The metadata warning property.
    */
   public final static String METADATA_WARNING_PROPERTY = "meta.warning";

   /**
    * A space-separated list of dynamic mixins to load. Only the mixin namespace URIs are specified;
    * this property does not specify how the mixins should be found.
    */
   public final static String METADATA_MIXINS_PROPERTY = "meta.mixins";

   /**
    * The name of a class that will serve as a handler for resolving the URLs used to locate metadata.
    * Must be an implementation of java.net.URLStreamHandler.
    *
    * May implement the MetadataURLHandler interface. If it does, then MetadataURLHandler.initialize()
    * will be called immediately after it is constructed.
    */
   public final static String METADATA_URL_HANDLER_PROPERTY = "meta.url.handler";

   // operations

   /**
    * Loads the metadata graph. Parts of the graph
    * might be available on-demand afterwards.
    * @param sURI The URI specifying the metadata location (e.g. the root XML directory).
    * @param properties Property map used to override the metadata attributes. 
    * @param nFlags A combination of bit flags specifying read options:
    * DEFAULT, ENVIRONMENT_ONLY, RUNTIME_EXCLUDED, INTEGRATION_EXCLUDED, VALIDATED_ONLY,
    * DOCUMENTATION_INCLUDED, WRITABLE, ENCRYPTED, WARNING.
    * @param progress Progress listener. Can be null to skip progress reporting.
    * @return The root metadata object.
    * @throws MetadataException if invalid metadata has been encountered.
    */
   public Metadata load(String sURI, Properties properties, int nFlags, ProgressListener progress);
}
