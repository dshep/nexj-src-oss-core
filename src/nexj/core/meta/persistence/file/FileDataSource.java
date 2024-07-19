// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.file;

import java.util.Iterator;

import nexj.core.meta.ContextMetadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.persistence.DataSource;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

/**
 * Metadata object representing a file data source. All configuration is stored
 * in the fragments contained in this data source.
 */
public class FileDataSource extends DataSource
{
   // constructors

   /**
    * Creates a new file data source of the given name, initializing it
    * with a default fragment.
    * 
    * @param sName The name of the data source.
    */
   public FileDataSource(String sName)
   {
      super(sName);
      addFragment(new FileDataSourceFragment());
   }


   // operations
   
   /**
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      super.validate(metadata, warnings);

      if (!isEnabled())
      {
         return;
      }

      //Ensure fragments do not duplicate dataDirectory or temporaryDirectory
      Lookup dataDirLookup = new HashTab(getFragmentCount());
      Lookup tempDirLookup = new HashTab(getFragmentCount());
      
      for (Iterator itr = getFragmentIterator(); itr.hasNext(); )
      {
         FileDataSourceFragment fragment = (FileDataSourceFragment)itr.next();
         FileDataSourceFragment oldFragment;
         
         oldFragment = (FileDataSourceFragment)dataDirLookup.put(fragment.getDataDirectory(), fragment);
         
         if (oldFragment != null)
         {
            throw new MetadataException("err.meta.persistence.file.duplicateDataDirectory",
               new Object[]{fragment.getName(), oldFragment.getName(), getName()});
         }
         
         oldFragment = (FileDataSourceFragment)tempDirLookup.put(fragment.getTemporaryDirectory(), fragment);
         
         if (oldFragment != null)
         {
            throw new MetadataException("err.meta.persistence.file.duplicateTemporaryDirectory",
               new Object[]{fragment.getName(), oldFragment.getName(), getName()});
         }
      }
   }
}
