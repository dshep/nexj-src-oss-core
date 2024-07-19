// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.upgrade;

import nexj.core.meta.MetadataException;

/**
 * Metadata upgrade state.
 */
public interface UpgradeState
{
   /**
    * Validates the initial state and starts the metadata upgrade.
    */
   void start() throws MetadataException;

   /**
    * Validates the final state and end the metadata upgrade.
    */
   void end() throws MetadataException;
}
