// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.io.IOException;
import java.sql.Connection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Attribute;
import nexj.core.meta.Primitive;
import nexj.core.meta.j2ee.J2EEProperty;
import nexj.core.meta.j2ee.J2EEResourceRef;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.meta.persistence.XMLPersistenceMetadataExporter;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.persistence.sql.SQLAdapter;
import nexj.core.util.EmptyIterator;
import nexj.core.util.Named;
import nexj.core.util.PropertyUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.XMLWriter;

/**
 * Exports relational metadata to an output stream.
 */
public class XMLRelationalMetadataExporter implements XMLPersistenceMetadataExporter
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
   public XMLRelationalMetadataExporter(XMLMetadataExporter exporter)
   {
      m_exporter = exporter;
      m_writer = exporter.getWriter();
   }

   // operations

   /**
    * Exports the specified relational database metadata.
    * @param database The relational database metadata to export.
    * @throws IOException if an error occurs.
    */
   public void exportDatabase(RelationalDatabase database) throws IOException
   {
      m_writer.openElement("RelationalDatabase");
      
      if (database.getAlias() != null)
      {
         m_writer.writeAttribute("alias", database.getAlias());
      }

      RelationalSchema schema = (RelationalSchema)database.getSchema();

      if (!schema.isPortable())
      {
         m_writer.writeAttribute("portable", false);
      }

      if (schema.getPrefix() != null)
      {
         m_writer.writeAttribute("prefix", schema.getPrefix());
      }

      if (schema.getTablespaceName() != null)
      {
         m_writer.writeAttribute("tablespace", schema.getTablespaceName());
      }

      if (schema.getIndexspaceName() != null)
      {
         m_writer.writeAttribute("indexspace", schema.getIndexspaceName());
      }

      if (schema.getLongspaceName() != null)
      {
         m_writer.writeAttribute("longspace", schema.getLongspaceName());
      }

      if (schema.getRoleName() != null)
      {
         m_writer.writeAttribute("role", schema.getRoleName());
      }

      if (schema.getIndexFill() > 0)
      {
         m_writer.writeAttribute("indexfill", schema.getIndexFill());
      }
      
      if (schema.getVersionTable() != null)
      {
         m_writer.writeAttribute("versionTable", schema.getVersionTable().getName());
      }

      m_writer.closeElement();

      // Sort the tables alphabetically

      List tableList = new ArrayList(schema.getTableCount());

      for (Iterator itr = schema.getTableIterator(); itr.hasNext();)
      {
         tableList.add(itr.next());
      }

      Collections.sort(tableList, new Comparator()
      {
         public int compare(Object o1, Object o2)
         {
            return ((Table)o1).getName().compareToIgnoreCase(((Table)o2).getName());
         }
      });

      m_writer.startElement("Tables");
      
      for (Iterator itr = tableList.iterator(); itr.hasNext();)
      {
         exportTable((Table)itr.next());
      }
      
      m_writer.endElement("Tables");
      m_writer.endElement("RelationalDatabase");
   }
   
   /**
    * Exports the specified table.
    * @param table The table to export.
    * @throws IOException if an error occurs.
    */
   public void exportTable(Table table) throws IOException
   {
      m_writer.openElement("Table");
      m_writer.writeAttribute("name", table.getName());

      Iterator/*<String>*/ hintItr = table.getHintIterator();

      if (hintItr != EmptyIterator.getInstance())
      {
         m_writer.openAttribute("hints");

         while (hintItr.hasNext())
         {
            m_writer.append(hintItr.next().toString());

            if (hintItr.hasNext())
            {
               m_writer.append(' ');
            }
         }

         m_writer.closeAttribute();
      }

      if (table.getPrimaryKey() != null)
      {
         m_writer.writeAttribute("primaryKey", table.getPrimaryKey().getName());
      }
      
      if (table.getTablespaceName() != null)
      {
         m_writer.writeAttribute("tablespace", table.getTablespaceName());
      }

      if (table.getIndexspaceName() != null)
      {
         m_writer.writeAttribute("indexspace", table.getIndexspaceName());
      }

      if (table.getLongspaceName() != null)
      {
         m_writer.writeAttribute("longspace", table.getLongspaceName());
      }

      if (table.getType() != Table.MANAGED)
      {
         m_writer.writeAttribute("type", table.getTypeString());
      }

      String sAspects = XMLMetadataExporter.getAspectOverrides(table);

      if (sAspects != null)
      {
         m_writer.writeAttribute("aspects", sAspects);
      }

      String sPointcuts = XMLMetadataExporter.getPointcuts(table);
      
      if (sPointcuts != null)
      {
         m_writer.writeAttribute("pointcuts", sPointcuts);
      }

      if (table.getAlias() != null)
      {
         m_writer.writeAttribute("alias", table.getAlias());
      }

      if (table.getDescription() != null)
      {
         m_writer.writeAttribute("description", table.getDescription());
      }
      
      m_writer.closeElement();
      m_writer.startElement("Columns");
      
      for (int i = 0; i < table.getColumnCount(); ++i)
      {
         Column column = table.getColumn(i);

         if (!column.isInherited())
         {
            exportColumn(column);
         }
      }

      m_writer.endElement("Columns");

      if (table.getIndexCount() > 0)
      {
         m_writer.startElement("Indexes");

         for (int i = 0; i < table.getIndexCount(); ++i)
         {
            Index index = table.getIndex(i);

            if (!index.isInherited())
            {
               exportIndex(index);
            }
         }
         
         m_writer.endElement("Indexes");
      }

      if (table.getType() == Table.QUERY || table.getType() == Table.VIEW)
      {
         m_writer.openElement("View");

         if (!table.isViewAutoUpdated())
         {
            m_writer.writeAttribute("autoupdated", false);
         }

         m_writer.closeElement();

         SQLScript script = table.getViewScript();

         for (int i = 0; i < script.getStatementCount(); ++i)
         {
            SQLStatement stmt = script.getStatement(i);

            m_writer.openElement("SQL");

            StringBuilder buf = new StringBuilder();

            for (int k = 0, n = stmt.getAdapterCount(); k < n; ++k)
            {
               if (buf.length() != 0)
               {
                  buf.append(' ');
               }

               buf.append(stmt.getAdapterPattern(k));
            }

            if (buf.length() != 0)
            {
               m_writer.writeAttribute("adapter", buf.toString());
            }
            
            m_writer.closeElement();

            if (stmt.getSQL() != null)
            {
               m_writer.writeValue(stmt.getSQL());
            }

            m_writer.endElement("SQL");
         }

         m_writer.endElement("View");
      }
      
      m_writer.endElement("Table");
   }
   
   /**
    * Exports the specified column.
    * @param column The column to export.
    * @throws IOException if an error occurs.
    */
   public void exportColumn(Column column) throws IOException
   {
      m_writer.openElement("Column");
      m_writer.writeAttribute("name", column.getName());
      m_writer.writeAttribute("type", column.getType().getName());
      
      if (column.getPrecision() != 0)
      {
         m_writer.writeAttribute("precision", String.valueOf(column.getPrecision()));
      }
      
      if (column.getScale() != 0)
      {
         m_writer.writeAttribute("scale", String.valueOf(column.getScale()));
      }

      if (column.getType() == Primitive.BINARY || column.getType() == Primitive.STRING)
      {
         m_writer.writeAttribute("allocation", column.getAllocationString());
      }

      if (!column.isNullable())
      {
         m_writer.writeAttribute("nullable", false);
      }
      
      m_writer.writeAttribute("caseInsensitive", column.isCaseInsensitive());

      if (column.getDescription() != null)
      {
         m_writer.writeAttribute("description", column.getDescription());
      }
      
      m_writer.closeEmptyElement();
   }

   /**
    * Exports the specified index.
    * @param index The index to export.
    * @throws IOException if an error occurs.
    */
   public void exportIndex(Index index) throws IOException
   {
      m_writer.openElement("Index");
      m_writer.writeAttribute("name", index.getName());
      m_writer.writeAttribute("type", index.getTypeString());

      if (index.isUnique())
      {
         m_writer.writeAttribute("unique", true);
      }

      if (index.getFill() >= 0)
      {
         m_writer.writeAttribute("fill", index.getFill());
      }

      if (index.getRelatedTable() != null)
      {
         m_writer.writeAttribute("relatedTable", index.getRelatedTable().getName());
      }

      String sAspects = XMLMetadataExporter.getAspectOverrides(index);

      if (sAspects != null)
      {
         m_writer.writeAttribute("aspects", sAspects);
      }

      m_writer.closeElement();

      for (int i = 0; i < index.getIndexColumnCount(); ++i)
      {
         IndexColumn indexColumn = index.getIndexColumn(i);

         if (!indexColumn.getColumn().isInherited())
         {
            exportIndexColumn(indexColumn);
         }
      }

      m_writer.endElement("Index");
   }
   
   /**
    * Exports the specified index column.
    * @param indexColumn The index column to export.
    * @throws IOException if an error occurs.
    */
   public void exportIndexColumn(IndexColumn indexColumn) throws IOException
   {
      m_writer.openElement("IndexColumn");
      m_writer.writeAttribute("name", indexColumn.getColumn().getName());
      
      if (!indexColumn.isAscending())
      {
         m_writer.writeAttribute("ascending", false);
      }

      m_writer.closeEmptyElement();
   }

   /**
    * Exports the specified relational mapping.
    * @param mapping The mapping to export.
    * @throws IOException if an error occurs.
    */
   public void exportRelationalMapping(RelationalMapping mapping) throws IOException
   {
      m_writer.openElement("RelationalMapping");
      m_writer.writeAttribute("primaryTable",
         (mapping.getPrimaryTable() != null) ? mapping.getPrimaryTable().getName() : null);
      
      if (mapping.getKeyGenerator() != null)
      {
         m_writer.writeAttribute("keyGenerator", mapping.getKeyGenerator().getName());
      }
      
      m_writer.closeElement();
      m_writer.startElement("AttributeMappings");

      for (Iterator itr = mapping.getMetaclass().getInstanceAttributeIterator(); itr.hasNext();)
      {
         AttributeMapping attributeMapping = mapping.getAttributeMapping((Attribute)itr.next());
         
         if (attributeMapping != null)
         {
            exportAttributeMapping(attributeMapping);
         }
      }

      m_writer.endElement("AttributeMappings");
      m_writer.endElement("RelationalMapping");
   }

   /**
    * Exports the specified attribute mapping.
    * @param mapping The mapping to export.
    * @throws IOException if an error occurs.
    */
   public void exportAttributeMapping(AttributeMapping mapping) throws IOException
   {
      m_writer.openElement("AttributeMapping");
      m_writer.writeAttribute("name", mapping.getAttribute().getName());
      
      if (mapping instanceof RelationalPrimitiveMapping)
      {
         RelationalPrimitiveMapping primitiveMapping = (RelationalPrimitiveMapping)mapping;

         if (primitiveMapping.getColumn().getTable() !=
            ((RelationalMapping)primitiveMapping.getPersistenceMapping()).getPrimaryTable())
         {
            m_writer.writeAttribute("table", primitiveMapping.getColumn().getTable().getName());
         }

         m_writer.writeAttribute("column", primitiveMapping.getColumn().getName());
         m_writer.closeEmptyElement();
      }
      else
      {
         RelationalClassMapping classMapping = (RelationalClassMapping)mapping;

         if (classMapping.getSourceKey() != null)
         {
            m_writer.writeAttribute("sourceKey", classMapping.getSourceKey().getName());
         }

         if (classMapping.getDestinationKey() instanceof Named)
         {
            m_writer.writeAttribute("destinationKey", ((Named)classMapping.getDestinationKey()).getName());
         }

         if (classMapping.getMapping().isContext())
         {
            m_exporter.exportPersistenceMapping(classMapping.getMapping());
            m_writer.endElement("AttributeMapping");
         }
         else
         {
            m_writer.closeEmptyElement();
         }
      }
   }

   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataExporter#exportDataSource(nexj.core.meta.persistence.DataSource)
    */
   public void exportDataSource(DataSource dataSource) throws IOException
   {
      exportDatabase((RelationalDatabase)dataSource);
   }
   
   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataExporter#exportMapping(nexj.core.meta.persistence.PersistenceMapping)
    */
   public void exportMapping(PersistenceMapping mapping) throws IOException
   {
      exportRelationalMapping((RelationalMapping)mapping);
   }

   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataExporter#exportJ2EEDescriptor(nexj.core.meta.persistence.DataSource, int, String, int, int)
    */
   public void exportJ2EEDescriptor(DataSource dataSource, int nPart, String sNamespace, int nContainer, int nContext) throws IOException
   {
      RelationalDatabase db = (RelationalDatabase)dataSource;

      switch (nPart)
      {
         case XMLMetadataExporter.J2EE_PLATFORM_CONNECTION_FACTORY:
            // The namespace test value ("SQL") must match the name of the
            // data source type.
            if (!"SQL".equals(sNamespace))
            {
               break;
            }
            // else fall through

         case XMLMetadataExporter.J2EE_RESOURCE_REF:
         case XMLMetadataExporter.J2EE_PLATFORM_RESOURCE_REF:
         case XMLMetadataExporter.J2EE_PLATFORM_RESOURCE_REF_EXT:
            if (db.isEnabled())
            {
               for (Iterator itr = db.getFragmentIterator(); itr.hasNext();)
               {
                  RelationalDatabaseFragment fragment = (RelationalDatabaseFragment)itr.next();
                  J2EEResourceRef ref =  new J2EEResourceRef("jdbc/" + db.getName() + fragment.getSuffix(),
                     SysUtil.NAMESPACE + '/' + dataSource.getType().getMetadata().getEnvironment() + "/jdbc/" + fragment.getAlias(),
                     "javax.sql.DataSource", fragment.getUser());

                  // assume nexj.core.rpc.sql.ra.SQLManagedConnectionFactory
                  if (nPart == XMLMetadataExporter.J2EE_PLATFORM_CONNECTION_FACTORY)
                  {
                     if (!fragment.isFirst())
                     {
                        continue;
                     }

                     RelationalDatabase ds = (RelationalDatabase)fragment.getDataSource();
                     SQLAdapter adapter = (SQLAdapter)ds.getComponent().getInstance(null);
                     String sDataSourceClass = ds.getDriver();

                     ref.addProperty(new J2EEProperty("name", ds.getName()));
                     ref.addProperty(new J2EEProperty("dataSource", sDataSourceClass));
                     ref.addProperty(new J2EEProperty("initialSQL", adapter.getInitialSQL()));
                     ref.addProperty(new J2EEProperty("password", fragment.getPassword()));
                     ref.addProperty(new J2EEProperty("properties",
                           PropertyUtil.toString(fragment.getPropertyHolder().getProperties())));
                     ref.addProperty(new J2EEProperty("statementCacheSize", fragment.getStatementCacheSize()));
                     ref.addProperty(new J2EEProperty("testSQL", adapter.getTestSQL()));
                     ref.addProperty(new J2EEProperty("user", fragment.getUser()));
                     ref.addProperty(new J2EEProperty("isolationLevel",
                                                      Connection.TRANSACTION_READ_COMMITTED));
                  }

                  ref.setIsolationLevel(Connection.TRANSACTION_READ_COMMITTED);
                  m_exporter.exportJ2EEResourceRef(dataSource.getType().getMetadata(), ref, nPart, sNamespace, nContainer, nContext);
               }
            }

            break;
      }
   }
}
