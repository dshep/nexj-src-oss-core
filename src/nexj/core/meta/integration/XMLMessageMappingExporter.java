// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import java.io.IOException;

/**
 * Interface implemented by message mapping exporters.
 */
public interface XMLMessageMappingExporter
{
   // operations

   /**
    * Exports the message mapping.
    * @param message The message for which the mapping refers to. Non-null whenever mapping is a 
    * root mapping.
    * @param mapping The message mapping to export.
    * @throws IOException if an error occurs.
    */
   void exportMapping(Message message, MessagePartMapping mapping) throws IOException;
}
