// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import java.io.IOException;

/**
 * Interface implemented by integration metadata exporters.
 */
public interface XMLIntegrationMetadataExporter
{
   /**
    * Exports a J2EE deployment descriptor part.
    * @param channel The channel to export.
    * @param nPart The part number, one of the J2EE_* constants.
    * @param sNamespace The part namespace infix. Cannot be null.
    * @param nContainer The container number, one of the J2EEUtil.* constants.
    * @param nContext The export context, one of the XMLPersistenceMetadataExporter.J2EE_CONTEXT_* constants.
    * @throws IOException if an error occurs.
    */
   void exportJ2EEDescriptor(Channel channel, int nPart, String sNamespace, int nContainer, int nContext) throws IOException;
   
   /**
    * Exports the specified channel.
    * @param channel The channel to export.
    * @throws IOException if an error occurs.
    */
   void exportChannel(Channel channel) throws IOException;
}
