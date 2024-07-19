// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence;

import java.io.IOException;


/**
 * Interface implemented by persistence metadata exporters.
 */
public interface XMLPersistenceMetadataExporter
{
   /**
    * Exports the data source.
    * @param dataSource The data source to export.
    * @throws IOException if an error occurs.
    */
   void exportDataSource(DataSource dataSource) throws IOException;

   /**
    * Exports the persistence mapping.
    * @param mapping The persistence mapping to export.
    * @throws IOException if an error occurs.
    */
   void exportMapping(PersistenceMapping mapping) throws IOException;
   
   /**
    * Exports a J2EE deployment descriptor part.
    * @param dataSource The data source to export.
    * @param nPart The part number, one of the J2EE_* constants.
    * @param sNamespace The part namespace infix. Cannot be null.
    * @param nContainer The container number, one of the J2EEUtil.* constants.
    * @param nContext The export context, one of the XMLPersistenceMetadataExporter.J2EE_CONTEXT_* constants.
    * @throws IOException if an error occurs.
    */
   void exportJ2EEDescriptor(DataSource dataSource, int nPart, String sNamespace, int nContainer, int nContext) throws IOException;
}
