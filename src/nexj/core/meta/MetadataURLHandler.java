// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.net.URL;
import java.util.Properties;

import nexj.core.util.ProgressListener;

/**
 * The metadata URL handler interface.
 */
public interface MetadataURLHandler
{
   /**
    * Initializes the handler. Determines the currently active deployment.
    * @param sURI The URI of the metadata to load.
    * @param properties The properties for loading the deployment management system metadata.
    * @param progress The progress listener to handle progress notifications.
    * @return The metadata, if loaded and immediately accessible; null otherwise.
    */
   public Metadata initialize(String sURI, Properties properties, ProgressListener progress);

   /**
    * Gets the URL for a mixin.
    * @param rootURL The URL to the root model.
    * @param sNamespace The mixin namespace.
    * @return The mixin URL.
    * @throws IllegalStateException If the handler has not been initialized.
    */
   public URL getMixinURL(URL rootURL, String sNamespace) throws IllegalStateException;
}
