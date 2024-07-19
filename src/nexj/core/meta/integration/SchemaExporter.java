// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import java.io.IOException;
import java.io.Writer;

import nexj.core.meta.MetadataObject;

/**
 * Exporter interface for integration formats that can export schema information,
 * e.g. WSDL.
 */
public interface SchemaExporter
{
   /**
    * Exports a schema.
    * 
    * @param metaObj The metadata object for which the schema should be exported.
    * @param sLocation An exporter-dependent location string; specifies the
    * service endpoint.
    * @param writer The writer on which the schema should be written.
    * @throws IOException If an I/O error occurs.
    */
   public void exportSchema(MetadataObject metaObj, String sLocation, Writer writer) throws IOException;
}
