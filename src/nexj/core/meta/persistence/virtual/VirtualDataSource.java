// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.virtual;

import nexj.core.meta.persistence.DataSource;

/**
 * The virtual data source.
 */
public class VirtualDataSource extends DataSource
{
   // constructors

   /**
    * Constructs the virtual data source metadata object.
    * @param sName The data source name.
    */
   public VirtualDataSource(String sName)
   {
      super(sName);
      addFragment(new VirtualDataSourceFragment());
   }
}
