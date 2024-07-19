// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.file;

import java.io.IOException;
import java.util.Iterator;

import nexj.core.meta.j2ee.J2EEProperty;
import nexj.core.meta.j2ee.J2EEResourceRef;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.meta.persistence.XMLPersistenceMetadataExporter;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.util.SysUtil;
import nexj.core.util.XMLWriter;

/**
 * Exports metadata for the File Persistence Adapter. It is needed to
 * configure the J2EE platform to instantiate components with the
 * proper configuration properties.
 */
public class XMLFileDataSourceMetadataExporter implements XMLPersistenceMetadataExporter
{
   // associations

   /**
    * The main metadata exporter.
    */
   protected XMLMetadataExporter m_exporter;

   /**
    * The output stream.
    */
   protected XMLWriter m_writer;


   // constructors

   /**
    * Creates the exporter with an XML print stream.
    * @param exporter The main metadata exporter.
    */
   public XMLFileDataSourceMetadataExporter(XMLMetadataExporter exporter)
   {
      m_exporter = exporter;
      m_writer = exporter.getWriter();
   }


   // operations

   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataExporter#exportJ2EEDescriptor(nexj.core.meta.persistence.DataSource, int, java.lang.String, int, int)
    */
   public void exportJ2EEDescriptor(DataSource dataSource, int nPart, String sNamespace, int nContainer, int nContext)
      throws IOException
   {
      FileDataSource ds = (FileDataSource)dataSource;

      switch (nPart)
      {
         case XMLMetadataExporter.J2EE_PLATFORM_CONNECTION_FACTORY:
            // The namespace test value ("File") must match the name of the 
            // File Channel adapter type.
            if (!sNamespace.equals("File"))
            {
               break;
            }
            // else fall through

         case XMLMetadataExporter.J2EE_RESOURCE_REF:
         case XMLMetadataExporter.J2EE_PLATFORM_RESOURCE_REF:
         case XMLMetadataExporter.J2EE_PLATFORM_RESOURCE_REF_EXT:
            if (ds.isEnabled())
            {
               for (Iterator itr = ds.getFragmentIterator(); itr.hasNext(); )
               {
                  FileDataSourceFragment fragment = (FileDataSourceFragment)itr.next();
                  J2EEResourceRef ref = new J2EEResourceRef("fileStorage/" + ds.getName() + fragment.getSuffix(),
                     SysUtil.NAMESPACE + "/fileStorage/" + ds.getName() + fragment.getSuffix(),
                     SysUtil.PACKAGE + ".core.rpc.file.FileConnectionFactory", null);

                  ref.setShareable(true);

                  if (nPart == XMLMetadataExporter.J2EE_PLATFORM_CONNECTION_FACTORY)
                  {
                     ref.setResourceAdapterName(SysUtil.NAMESPACE + "-file.rar");
                     ref.setTxMode(J2EEResourceRef.TX_XA);
                     ref.setMaxConnections(fragment.getMaxPoolSize());
                     
                     ref.addProperty(new J2EEProperty("persistenceConnection", true));
                     ref.addProperty(new J2EEProperty("inputConnection", false));
                     ref.addProperty(new J2EEProperty("outgoingDirectory", fragment.getDataDirectory()));
                     ref.addProperty(new J2EEProperty("outgoingTempDirectory", fragment.getTemporaryDirectory()));
                     
                     ref.addProperty(new J2EEProperty("maxNameSplits", fragment.getMaxNameSplits()));
                     ref.addProperty(new J2EEProperty("nameSplitSize", fragment.getNameSplitSize()));
                     
                     ref.addProperty(new J2EEProperty("journalDirectory", fragment.getJournalPath()));
                  }
                  
                  m_exporter.exportJ2EEResourceRef(dataSource.getType().getMetadata(), ref, nPart, sNamespace, nContainer, nContext);
               }
            }
            break;
      }
   }


   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataExporter#exportMapping(nexj.core.meta.persistence.PersistenceMapping)
    */
   public void exportMapping(PersistenceMapping mapping) throws IOException
   {
      //Do nothing for now.
   }


   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataExporter#exportDataSource(nexj.core.meta.persistence.DataSource)
    */
   public void exportDataSource(DataSource dataSource) throws IOException
   {
      //Do nothing for now.
   }
}
