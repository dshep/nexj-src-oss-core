// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * 
 */
public class ExternalLibrary extends NamedMetadataObject
{
   // associations
   
   /**
    * The root metadata object.
    */
   protected Metadata m_metadata;

   /**
    * The library file collection.
    */
   protected List m_fileList = new ArrayList(2); // of type String

   // constructors
   
   /**
    * Creates an external library with a given name.
    * @param sName The external library name.
    */
   public ExternalLibrary(String sName)
   {
      super(sName);
   }
   
   // operations
   
   /**
    * Sets the root metadata object.
    * @param metadata The root metadata object to set.
    */
   public void setMetadata(Metadata metadata)
   {
      verifyNotReadOnly();
      m_metadata = metadata;
   }

   /**
    * @return The root metadata object.
    */
   public Metadata getMetadata()
   {
      return m_metadata;
   }
   
   /**
    * Adds a new library file to the external library.
    * @param file The library file to add.
    */
   public void addFile(String file)
   {
      verifyNotReadOnly();
      m_fileList.add(file);
   }

   /**
    * Gets a library file by ordinal number.
    * @param nOrdinal The library file ordinal number (0-based).
    * @return The library file object.
    */
   public String getFile(int nOrdinal)
   {
      return (String)m_fileList.get(nOrdinal);
   }

   /**
    * @return The library file count.
    */
   public int getFileCount()
   {
      return m_fileList.size();
   }

   /**
    * @return An iterator for the contained library file objects.
    */
   public Iterator getFileIterator()
   {
      return m_fileList.iterator();
   }
}
