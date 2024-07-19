// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import org.w3c.dom.Element;

import nexj.core.meta.Attribute;
import nexj.core.meta.Component;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.DataSourceAdapter;
import nexj.core.meta.persistence.DataSourceType;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.meta.persistence.SchemaUpgrade;
import nexj.core.meta.persistence.XMLPersistenceMetadataLoader;
import nexj.core.meta.persistence.sql.upgrade.AlterColumnStep;
import nexj.core.meta.persistence.sql.upgrade.AlterTableStep;
import nexj.core.meta.persistence.sql.upgrade.ApplyIndexAspectStep;
import nexj.core.meta.persistence.sql.upgrade.ApplyTableAspectStep;
import nexj.core.meta.persistence.sql.upgrade.ColumnOutline;
import nexj.core.meta.persistence.sql.upgrade.CreateColumnStep;
import nexj.core.meta.persistence.sql.upgrade.CreateIndexStep;
import nexj.core.meta.persistence.sql.upgrade.CreateObjectStep;
import nexj.core.meta.persistence.sql.upgrade.CreateTableStep;
import nexj.core.meta.persistence.sql.upgrade.DropColumnStep;
import nexj.core.meta.persistence.sql.upgrade.DropIndexStep;
import nexj.core.meta.persistence.sql.upgrade.DropObjectStep;
import nexj.core.meta.persistence.sql.upgrade.DropTableStep;
import nexj.core.meta.persistence.sql.upgrade.ExecStep;
import nexj.core.meta.persistence.sql.upgrade.IndexOutline;
import nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgrade;
import nexj.core.meta.persistence.sql.upgrade.RemoveIndexAspectStep;
import nexj.core.meta.persistence.sql.upgrade.RemoveTableAspectStep;
import nexj.core.meta.persistence.sql.upgrade.RenameColumnStep;
import nexj.core.meta.persistence.sql.upgrade.RenameIndexStep;
import nexj.core.meta.persistence.sql.upgrade.RenameTableStep;
import nexj.core.meta.persistence.sql.upgrade.SupportAdapterStep;
import nexj.core.meta.xml.XMLMetadata;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataHelper.ContextFixup;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.persistence.sql.SQLAdapter;
import nexj.core.util.HashTab;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Lookup;
import nexj.core.util.SysUtil;
import nexj.core.util.XMLUtil;
import nexj.core.util.XMLUtil.ElementHandler;

/**
 * Persistence metadata loader for RDBMS schemas.
 */
public class XMLRelationalMetadataLoader implements XMLPersistenceMetadataLoader
{
   /**
    * The metadata loader.
    */
   private XMLMetadataLoader m_loader;

   /**
    * The metadata helper.
    */
   private XMLMetadataHelper m_helper;

   /**
    * The schema being currently processed.
    */
   private RelationalSchema m_schema;

   /**
    * The table fixup collection.
    */
   private List m_tableFixupList; // of type Fixup

   /**
    * The collection of Fixups nessesary to run for finalizing load of SQLObjects.
    */
   private List/*<Fixup>*/ m_objectFixupList;

   /**
    * The index fixup collection.
    */
   private List m_indexFixupList; // of type Fixup

   /**
    * The aspect fixup collection.
    */
   private List m_aspectFixupList; // of type Fixup

   /**
    * The key fixup collection.
    */
   private List m_keyFixupList; // of type Fixup

   /**
    * Temporary persistence mapping element used during the parsing.
    */
   private Element m_persistenceMappingElement;
   
   /**
    * Work persistence mapping element handler.
    */
   private ElementHandler m_persistenceMappingElementHandler = new ElementHandler()
   {
      public void handleElement(Element element)
      {
         m_persistenceMappingElement = element;
      }
   };

   // operations

   /**
    * @return The current metadata object.
    */
   protected XMLMetadata getMetadata()
   {
      return m_loader.getMetadata();
   }

   /**
    * @return The metadata helper object.
    */
   protected XMLMetadataHelper getHelper()
   {
      return m_helper;
   }

   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataLoader#loadDataSource(org.w3c.dom.Element, java.lang.String, nexj.core.meta.persistence.DataSourceType, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public DataSource loadDataSource(Element element, String sName, DataSourceType type, XMLMetadataLoader loader)
   {
      final RelationalDatabase db = new RelationalDatabase(sName);

      db.setType(type);
      loader.loadDataSource(element, db);

      db.setAlias(XMLUtil.getStringAttr(element, "alias"));

      if (db.getAlias() == null)
      {
         db.setAlias(XMLMetadataHelper.makeAlias(db.getName()));
      }

      m_loader = loader;
      m_helper = loader.getHelper();
      m_schema = new RelationalSchema();

      db.setSchema(m_schema);
      m_schema.setPortable(XMLUtil.getBooleanAttr(element, "portable", m_schema.isPortable()));
      m_schema.setPrefix(XMLUtil.getStringAttr(element, "prefix", m_schema.getPrefix()));
      m_schema.setTablespaceName(XMLUtil.getStringAttr(element, "tablespace", m_schema.getTablespaceName()));
      m_schema.setIndexspaceName(XMLUtil.getStringAttr(element, "indexspace", m_schema.getIndexspaceName()));
      m_schema.setLongspaceName(XMLUtil.getStringAttr(element, "longspace", m_schema.getLongspaceName()));
      m_schema.setRoleName(XMLUtil.getStringAttr(element, "role", m_schema.getRoleName()));
      m_schema.setIndexFill(XMLUtil.getIntAttr(element, "indexfill", m_schema.getIndexFill()));

      if (!loader.isEnvironmentOnly())
      {
         m_tableFixupList = new ArrayList(256);
         m_objectFixupList = new ArrayList(256);
         m_indexFixupList = new ArrayList(256);
         m_aspectFixupList = new ArrayList(256);
         m_keyFixupList = new ArrayList(256);

         XMLUtil.withFirstChildElement(element, "Tables", false, new ElementHandler()
         {
            public void handleElement(Element tablesElement)
            {
               XMLUtil.forEachChildElement(tablesElement, "Table",
                  getHelper().new ElementHandler("table")
               {
                  public void handleElement(Element tableElement, String sTableName)
                  {
                     XMLMetadataHelper.validateName(sTableName, XMLMetadataHelper.NAME_DOT);

                     Table table = new Table(m_schema);

                     loadTable(tableElement, sTableName, table);
                     m_schema.addTable(table);
                  }
               });
            }
         });

         getHelper().fixup(m_tableFixupList.iterator());
         m_tableFixupList = null;

         RelationalSchemaAspectManager aspectManager = new RelationalSchemaAspectManager()
         {
            protected Iterator getTableAspectIterator()
            {
               return m_schema.getTableIterator();
            }

            protected Iterator getTablePointcutIterator()
            {
               return m_schema.getTableIterator();
            }

            protected Iterator getIndexAspectIterator()
            {
               return m_schema.getIndexIterator();
            }

            protected Iterator getIndexPointcutIterator()
            {
               return m_schema.getIndexIterator();
            }
         };

         aspectManager.applyAspects(0);

         getHelper().fixup(m_indexFixupList.iterator());
         m_indexFixupList = null;

         aspectManager.applyAspects(1);

         getHelper().fixup(m_aspectFixupList.iterator());
         m_aspectFixupList = null;

         aspectManager.applyAspects(2);

         getHelper().fixup(m_keyFixupList.iterator());
         m_keyFixupList = null;

         String sVersionTable = XMLUtil.getStringAttr(element, "versionTable");

         if (sVersionTable != null)
         {
            m_schema.setVersionTable(m_schema.getTable(sVersionTable));
         }

         XMLUtil.withFirstChildElement(element, "Objects", false, new ElementHandler()
         {
            public void handleElement(Element tablesElement)
            {
               XMLUtil.forEachChildElement(tablesElement, "Object",
                  getHelper().new ElementHandler("object")
               {
                  public void handleElement(Element objectElement, String sObjectName)
                  {
                     XMLMetadataHelper.validateName(sObjectName, XMLMetadataHelper.NAME_DOT);

                     final SQLObject sqlObject = new SQLObject(m_schema);

                     loadObject(objectElement, sObjectName, sqlObject);
                     m_schema.addObject(sqlObject);
                  }
               });
            }
         });

         getHelper().fixup(m_objectFixupList.iterator());
         m_objectFixupList = null;
      }

      boolean bFirstRelationalDatabase = true;

      for (Iterator itr = m_loader.getMetadata().getDataSourceIterator(); itr.hasNext() && bFirstRelationalDatabase; )
      {
         DataSource ds = (DataSource)itr.next();

         if (ds instanceof RelationalDatabase)
         {
            bFirstRelationalDatabase = false;
         }
      }

      if (bFirstRelationalDatabase)
      {
         if (!loader.isEnvironmentOnly())
         {
            // Find unmapped columns from mapped tables and set their required flag 
            m_loader.addSingletonFixup(new ContextFixup(getHelper())
            {
               public void fixup()
               {
                  RelationalMapping.resolve(m_loader.getMetadata());
               }
            });
         }

         m_loader.addIOFixup(new ContextFixup(getHelper())
         {
            public void fixup()
            {
               Lookup aliasFragmentMap = new HashTab();

               for (Iterator itr = m_loader.getMetadata().getDataSourceIterator(); itr.hasNext(); )
               {
                  DataSource ds = (DataSource)itr.next();

                  if (ds instanceof RelationalDatabase)
                  {
                     RelationalDatabase rel = (RelationalDatabase)ds;

                     for (Iterator fragItr = rel.getFragmentIterator(); fragItr.hasNext(); )
                     {
                        RelationalDatabaseFragment frag = (RelationalDatabaseFragment)fragItr.next();
                        RelationalDatabaseFragment oldFrag = (RelationalDatabaseFragment)aliasFragmentMap.put(frag.getAlias(), frag);

                        if (oldFrag == null)
                        {
                           frag.setFirst(true);
                        }
                        else
                        {
                           frag.setFirst(false);
                           aliasFragmentMap.put(frag.getAlias(), oldFrag);

                           if (!frag.isCompatible(oldFrag))
                           {
                              throw new MetadataValidationException("err.meta.persistence.fragmentPropertyMismatch",
                                 new Object[]{frag.getDataSource().getName() + frag.getSuffix(), oldFrag.getDataSource().getName() + oldFrag.getSuffix()});
                           }
                        }
                     }
                  }
               }
            }
         });
      }

      return db;
   }

   /**
    * Loads an SQL Object from a DOM element.
    * @param objectElement The DOM element containing the SQL Object definition.
    * @param sObjectName The name of the SQL Object.
    * @param sqlObject The SQL Object to fill with definition.
    */
   protected void loadObject(Element objectElement, String sObjectName, final SQLObject sqlObject)
   {
      sqlObject.setName(sObjectName);

      XMLUtil.withFirstChildElement(objectElement, "Create", false, new ElementHandler()
      {
         public void handleElement(Element scriptElement)
         {
            SQLScript script = new SQLScript();

            loadSQLScript(scriptElement, script);
            sqlObject.setCreateScript(script);
         }
      });

      XMLUtil.withFirstChildElement(objectElement, "Drop", false, new ElementHandler()
      {
         public void handleElement(Element scriptElement)
         {
            SQLScript script = new SQLScript();

            loadSQLScript(scriptElement, script);
            sqlObject.setDropScript(script);
         }
      });

      loadRelationalObject(objectElement, sqlObject);
   }

   /**
    * Loads RelationalObjectBase attributes from a DOM element.
    * @param objElement The DOM element containing the attribute definitions.
    * @param table The relational object.
    */
   protected void loadRelationalObject(Element objElement, final RelationalObject obj)
   {
      XMLUtil.withFirstChildElement(objElement, "Prerequisites", false, new ElementHandler()
      {
         public void handleElement(Element prerequisiteElement)
         {
            XMLUtil.forEachChildElement(
               prerequisiteElement, "Object", getHelper().new ElementHandler("object")
            {
               public void handleElement(Element objectElement, final String sObjectName)
               {
                  m_objectFixupList.add(new ContextFixup(getHelper())
                  {
                     public void fixup()
                     {
                        obj.addPrerequisite(m_schema.getObject(sObjectName));
                     }
                  });
               }
            });

            XMLUtil.forEachChildElement(
               prerequisiteElement, "Table", getHelper().new ElementHandler("table")
            {
               public void handleElement(Element tableElement, final String sTableName)
               {
                  m_objectFixupList.add(new ContextFixup(getHelper())
                  {
                     public void fixup()
                     {
                        obj.addPrerequisite(m_schema.getTable(sTableName));
                     }
                  });
               }
            });
         }
      });
   }

   /**
    * Loads a table from a DOM element.
    * @param tableElement The DOM element containing the table definition.
    * @param table The table object.
    */
   protected void loadTable(Element tableElement, String sTableName, final Table table)
   {
      table.setType(parseTableType(XMLUtil.getStringAttr(tableElement, "type"), table.getType()));
      table.setName(sTableName);
      table.setAlias(XMLUtil.getStringAttr(tableElement, "alias"));
      table.setTablespaceName(XMLUtil.getStringAttr(tableElement, "tablespace", table.getTablespaceName()));
      table.setIndexspaceName(XMLUtil.getStringAttr(tableElement, "indexspace", table.getIndexspaceName()));
      table.setLongspaceName(XMLUtil.getStringAttr(tableElement, "longspace", table.getLongspaceName()));
      m_loader.loadDocumentation(tableElement, table);

      XMLMetadataLoader.parsePatterns(XMLUtil.getStringAttr(tableElement, "aspects"),
         new XMLMetadataLoader.PatternHandler()
         {
            public void handlePattern(final String sName, final boolean bInclusive)
            {
               m_tableFixupList.add(new ContextFixup(getHelper())
               {
                  public void fixup()
                  {
                     table.addAspectOverride(table.getSchema().getTable(sName), bInclusive);
                  }
               });
            }
         });

      XMLMetadataLoader.parsePatterns(XMLUtil.getStringAttr(tableElement, "pointcuts"),
         new XMLMetadataLoader.PatternHandler()
         {
            public void handlePattern(final String sPattern, final boolean bInclusive)
            {
               table.addPointcutPattern(sPattern, bInclusive);
            }
         });

      XMLUtil.withFirstChildElement(tableElement, "Columns", false, new ElementHandler()
      {
         public void handleElement(Element columnsElement)
         {
            XMLUtil.forEachChildElement(columnsElement, "Column",
               getHelper().new ElementHandler("column")
            {
               public void handleElement(Element columnElement, String sColumnName)
               {
                  XMLMetadataHelper.validateName(sColumnName);
                  
                  Column column = new Column(sColumnName, table);

                  loadColumn(columnElement, column);
                  table.addColumn(column);
               }
            });
         }
      });

      XMLUtil.withFirstChildElement(tableElement, "Indexes", false, new ElementHandler()
      {
         public void handleElement(Element indexesElement)
         {
            XMLUtil.forEachChildElement(indexesElement, "Index",
               getHelper().new ElementHandler("index")
            {
               public void handleElement(Element indexElement, String sIndexName)
               {
                  XMLMetadataHelper.validateName(sIndexName, XMLMetadataHelper.NAME_DOT);

                  final Index index = new Index(table);

                  loadIndex(indexElement, sIndexName, index);

                  m_indexFixupList.add(new ContextFixup(getHelper())
                  {
                     public void fixup()
                     {
                        table.addIndex(index);
                     }
                  });
               }
            });
         }
      });

      XMLUtil.withFirstChildElement(tableElement, "View", false, new ElementHandler()
      {
         public void handleElement(Element viewElement)
         {
            final SQLScript script = new SQLScript();

            loadSQLScript(viewElement, script);
            table.setViewScript(script);
            table.setViewAutoUpdated(
               XMLUtil.getBooleanAttr(viewElement, "autoupdated", table.isViewAutoUpdated()));
         }
      });

      loadRelationalObject(tableElement, table);

      String sHints = XMLUtil.getStringAttr(tableElement, "hints");

      if (sHints != null)
      {
         for (StringTokenizer tokenizer = new StringTokenizer(sHints); tokenizer.hasMoreTokens();)
         {
            table.addHint(tokenizer.nextToken());
         }
      }

      final String sPrimaryKey = (table.isAspect()) ? XMLUtil.getStringAttr(tableElement, "primaryKey") :
         XMLUtil.getReqStringAttr(tableElement, "primaryKey");

      m_keyFixupList.add(new ContextFixup(getHelper())
      {
         public void fixup()
         {
            if (sPrimaryKey != null)
            {
               table.setPrimaryKey(table.getIndex(sPrimaryKey));
            }

            table.computePrimaryKeyParts();
         }
      });
   }

   /**
    * Loads a column from a DOM element.
    * @param columnElement The DOM element containing the column definition.
    * @param column The column object.
    */
   protected void loadColumn(Element columnElement, final Column column)
   {
      m_loader.loadDocumentation(columnElement, column);

      // Must precede type setting
      column.setCaseInsensitive(XMLUtil.getBooleanAttr(columnElement, "caseInsensitive",
         column.isCaseInsensitive()));

      column.setType(parseColumnType(XMLUtil.getStringAttr(columnElement, "type")),
         XMLUtil.getIntegerObjAttr(columnElement, "precision"),
         XMLUtil.getIntegerObjAttr(columnElement, "scale"),
         parseColumnAllocation(XMLUtil.getStringAttr(columnElement, "allocation")));

      column.setNullable(XMLUtil.getBooleanAttr(columnElement, "nullable", column.isNullable()));
      column.setLiteral(XMLUtil.getBooleanAttr(columnElement, "literal", column.isLiteral()));

      final String sConverterName = XMLUtil.getStringAttr(columnElement, "converter");

      if (sConverterName != null)
      {
         m_loader.addComponentFixup(new ContextFixup(getHelper())
         {
            public void fixup()
            {
               column.setConverter(column.getTable().getSchema().getMetadata().getComponent(sConverterName));
            }
         });
      }

      if (column.getType() == null)
      {
         m_loader.addComponentFixup(new ContextFixup(getHelper())
         {
            public void fixup()
            {
               column.computeType();
            }
         });
      }

      if (column.getTable().isAspect() && (sConverterName != null || column.getType() == null))
      {
         m_loader.addComponentFixup(new ContextFixup(getHelper())
         {
            public void fixup()
            {
               for (Iterator itr = column.getTable().getSchema().getTableIterator(); itr.hasNext();)
               {
                  Table table = (Table)itr.next();

                  if (table.hasAspect(column.getTable()))
                  {
                     Column col = table.getColumn(column.getName());

                     col.setConverter(column.getConverter());
                     col.computeType();
                  }
               }
            }
         });
      }
   }

   /**
    * Loads an index from a DOM element.
    * @param indexElement The DOM element containing the index definition.
    * @param sName The index name.
    * @param index The index object.
    */
   protected void loadIndex(Element indexElement, String sName, final Index index)
   {
      index.setType(parseIndexType(XMLUtil.getStringAttr(indexElement, "type"), index.getType()));
      index.setName(sName);
      index.setUnique(XMLUtil.getBooleanAttr(indexElement, "unique", index.isUnique()));
      index.setFill(XMLUtil.getIntAttr(indexElement, "fill", index.getFill()));

      XMLMetadataLoader.parsePatterns(XMLUtil.getStringAttr(indexElement, "aspects"),
         new XMLMetadataLoader.PatternHandler()
         {
            public void handlePattern(final String sName, final boolean bInclusive)
            {
               m_aspectFixupList.add(new ContextFixup(getHelper())
               {
                  public void fixup()
                  {
                     index.addAspectOverride(index.getTable().getSchema().getIndex(sName), bInclusive);
                  }
               });
            }
         });

      final String sRelatedTableName = XMLUtil.getStringAttr(indexElement, "relatedTable");

      if (sRelatedTableName != null)
      {
         m_indexFixupList.add(new ContextFixup(getHelper())
         {
            public void fixup()
            {
               index.getTable().getSchema().getTable(sRelatedTableName).addRelatedKey(index);
            }
         });
      }

      XMLUtil.forEachChildElement(indexElement, "IndexColumn",
         getHelper().new ElementHandler("column")
      {
         public void handleElement(Element element, final String sName)
         {
            final boolean bAscending = XMLUtil.getBooleanAttr(element, "ascending", true); 

            m_indexFixupList.add(new ContextFixup(getHelper())
            {
               public void fixup()
               {
                  index.addIndexColumn(new IndexColumn(index.getTable().getColumn(sName), bAscending));
               }
            });
         }
      });
   }

   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataLoader#loadMapping(org.w3c.dom.Element, nexj.core.meta.Metaclass, nexj.core.meta.persistence.DataSource, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public PersistenceMapping loadMapping(
      final Element persistenceMappingElement,
      final Metaclass metaclass,
      final DataSource dataSource,
      final XMLMetadataLoader loader)
   {
      m_loader = loader;
      m_helper = loader.getHelper();
      m_schema = (RelationalSchema)dataSource.getSchema();

      final RelationalMapping relationalMapping = new RelationalMapping();
      int nCookie = getHelper().pushMarker(MetadataValidationException.TYPE_NAME, "RelationalMapping");
      getHelper().pushMarker("class", metaclass.getName());

      try
      {
         relationalMapping.setMetaclass(metaclass);
         relationalMapping.setDataSource(dataSource);

         XMLUtil.withFirstChildElement(persistenceMappingElement, "RelationalMapping", true, new ElementHandler()
         {
            public void handleElement(Element relationalMappingElement)
            {
               String sPrimaryTable = XMLUtil.getStringAttr(relationalMappingElement, "primaryTable");
               
               relationalMapping.setPrimaryTable((sPrimaryTable == null) ? null : m_schema.getTable(sPrimaryTable));

               final String sKeyGeneratorName = XMLUtil.getStringAttr(relationalMappingElement, "keyGenerator");

               if (sKeyGeneratorName != null)
               {
                  if (sKeyGeneratorName.equals("identity"))
                  {
                     relationalMapping.setKeyGenerator(RelationalMapping.KEY_GEN_IDENTITY);
                  }
                  else
                  {
                     m_loader.addComponentFixup(new ContextFixup(getHelper())
                     {
                        public void fixup()
                        {
                           relationalMapping.setKeyGenerator(
                              relationalMapping.getMetaclass().getMetadata()
                              .getComponent(sKeyGeneratorName));
                        }
                     });
                  }
               }

               XMLUtil.withFirstChildElement(relationalMappingElement, "AttributeMappings", false, new ElementHandler()
               {
                  public void handleElement(Element attributeMappingsElement)
                  {
                     XMLUtil.forEachChildElement(attributeMappingsElement, "AttributeMapping",
                        getHelper().new ElementHandler("attribute")
                     {
                        public void handleElement(Element attributeMappingElement, String sAttributeName)
                        {
                           final Attribute attribute = metaclass.getAttribute(sAttributeName);
                           final String sTableName = XMLUtil.getStringAttr(attributeMappingElement, "table");
                           final String sColumnName = XMLUtil.getStringAttr(attributeMappingElement, "column");
                           final String sSourceKeyName = XMLUtil.getStringAttr(attributeMappingElement, "sourceKey");
                           final String sDestinationKeyName = XMLUtil.getStringAttr(attributeMappingElement, "destinationKey");
                           AttributeMapping mapping;
                           
                           m_persistenceMappingElement = null;
                           XMLUtil.withFirstChildElement(attributeMappingElement,
                              "PersistenceMapping", false, m_persistenceMappingElementHandler);

                           if (attribute.getType().isPrimitive())
                           {
                              if (sColumnName == null || sSourceKeyName != null ||
                                 sDestinationKeyName != null || m_persistenceMappingElement != null)
                              {
                                 throw new MetadataException("err.meta.primitiveRelationalMapping",
                                    new Object[]{attribute.getName(), metaclass.getName()});
                              }
                              
                              if (attribute.isCollection())
                              {
                                 throw new MetadataException("err.meta.unsupportedRelationalMapping",
                                    new Object[]{attribute.getName(), metaclass.getName()});
                              }

                              RelationalPrimitiveMapping primitiveMapping = new RelationalPrimitiveMapping();
                              Table table = (sTableName == null) ? relationalMapping.getPrimaryTable() : m_schema.getTable(sTableName);

                              if (table == null)
                              {
                                 throw new MetadataException("err.meta.unspecifiedRelationalMappingTable",
                                    new Object[]{attribute.getName(), metaclass.getName()});
                              }

                              primitiveMapping.setColumn(table.getColumn(sColumnName));
                              primitiveMapping.setAttribute(attribute);
                              mapping = primitiveMapping;
                           }
                           else
                           {
                              if (sTableName != null || sColumnName != null ||
                                 m_persistenceMappingElement != null &&
                                 (sSourceKeyName != null || sDestinationKeyName != null))
                              {
                                 throw new MetadataException("err.meta.classRelationalMapping",
                                    new Object[]{attribute.getName(), metaclass.getName()});
                              }
                              
                              final RelationalClassMapping classMapping = new RelationalClassMapping();
                              
                              classMapping.setAttribute(attribute);

                              if (m_persistenceMappingElement != null)
                              {
                                 if (attribute.isCollection())
                                 {
                                    throw new MetadataException("err.meta.unsupportedRelationalMapping",
                                       new Object[]{attribute.getName(), metaclass.getName()});
                                 }

                                 classMapping.setSourceKey(relationalMapping.getPrimaryTable().getPrimaryKey());
                                 classMapping.setMapping(m_loader.loadPersistenceMapping(m_persistenceMappingElement, (Metaclass)attribute.getType()));
                              }
                              else
                              {
                                 if (sSourceKeyName != null)
                                 {
                                    classMapping.setSourceKey(m_schema.getIndex(sSourceKeyName));
                                 }
                                 else
                                 {
                                    Table primaryTable = relationalMapping.getPrimaryTable();

                                    if (primaryTable != null)
                                    {
                                       classMapping.setSourceKey(primaryTable.getPrimaryKey());
                                    }
                                 }

                                 m_loader.addPersistenceMappingFixup(new ContextFixup(getHelper())
                                 {
                                    public void fixup()
                                    {
                                       Metaclass type = (Metaclass)attribute.getType();
                                       PersistenceMapping mapping = type.getPersistenceMapping();

                                       if (mapping == null)
                                       {
                                          throw new MetadataException("err.meta.missingAssocPersistenceMapping",
                                             new Object[]{attribute.getName(), metaclass.getName(), type.getName()});
                                       }

                                       classMapping.setDestinationKey(mapping.addForeignKey(sDestinationKeyName, classMapping));
                                    }
                                 });
                              }

                              mapping = classMapping;
                           }

                           relationalMapping.addAttributeMapping(mapping);
                        }
                     });
                  }
               });

               XMLUtil.withFirstChildElement(relationalMappingElement, "Hook", false, new ElementHandler()
               {
                  public void handleElement(Element componentElement)
                  {
                     final Component hook = new Component("<PersistenceHook:" + metaclass.getName() + ">"); 

                     m_loader.loadComponent(componentElement, hook);

                     if (!loader.isRuntimeExcluded())
                     {
                        m_loader.addComponentFixup(new ContextFixup(getHelper())
                        {
                           public void fixup()
                           {
                              relationalMapping.setHook(hook);
                           }
                        });
                     }
                  }
               });
            }
         });
      }
      finally
      {
         getHelper().restoreMarker(nCookie);
      }

      return relationalMapping;
   }

   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataLoader#loadConnection(org.w3c.dom.Element, nexj.core.meta.persistence.DataSource, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public void loadConnection(Element element, final DataSource source, final XMLMetadataLoader loader)
   {
      final RelationalDatabase db = (RelationalDatabase)source;
      final RelationalDatabaseFragment defaultFragment = (RelationalDatabaseFragment)db.getDefaultFragment();

      m_loader = loader;
      defaultFragment.setAlias(db.getAlias() + defaultFragment.getSuffix());

      if (element != null)
      {
         db.setUnicode(XMLUtil.getBooleanAttr(element, "unicode", db.isUnicode()));
         db.setPath(XMLUtil.getStringAttr(element, "path"));
         db.setDriver(XMLUtil.getStringAttr(element, "driver"));
         db.setLiteral(XMLUtil.getBooleanAttr(element, "literal", db.isLiteral()));
         db.setPageSize(XMLUtil.getIntAttr(element, "pageSize", db.getPageSize()));
         db.setIndexspaceName(XMLUtil.getStringAttr(element, "indexspace",db.getIndexspaceName()));
         db.setLongspaceName(XMLUtil.getStringAttr(element, "longspace", db.getLongspaceName()));
         db.setTablespaceName(XMLUtil.getStringAttr(element, "tablespace",db.getTablespaceName()));

         loadFragment(element, defaultFragment, false);

         XMLUtil.withFirstChildElement(element, "Fragments", false, new ElementHandler()
         {
            public void handleElement(Element fragmentsElement)
            {
               XMLUtil.forEachChildElement(fragmentsElement, "Fragment", getHelper().new ElementHandler("fragment")
               {
                  protected void handleElement(Element fragmentElement, String sName)
                  {
                     RelationalDatabaseFragment fragment = (RelationalDatabaseFragment)defaultFragment.clone(false);

                     fragment.setName(sName);
                     db.addFragment(fragment);
                     loadFragment(fragmentElement, fragment, true);
                  }
               });
            }
         });
      }

      db.setDefaultProperties(loader.getContainer()); // done after loading config from XML Element

      DataSourceAdapter adapter = source.getAdapter();

      if (adapter == null)
      {
         return; // no RDBMS mapping defined for "optional" DataSource, hence no component required
      }

      final Component component =
         new Component(source.getName(), adapter.getClassObject(), Component.CONTEXT);

      component.setMetadata(loader.getMetadata());

      if (element != null)
      {
         XMLUtil.withFirstChildElement(element, "SQLHook", false, new ElementHandler()
         {
            public void handleElement(Element componentElement)
            {
               Component hook = new Component("<SQLHook:" + source.getName() + ">"); 

               m_loader.loadComponent(componentElement, hook);
               component.addComponentPropertyInitializer("SQLHook", hook);
            }
         });
      }

      db.setComponent(component);

      if (adapter.getVersion() != null)
      {
         component.addPrimitivePropertyInitializer("version", adapter.getVersion());
      }

      if (!db.isUnicode())
      {
         component.addPrimitivePropertyInitializer("unicode", Boolean.FALSE);
      }

      if (db.isLiteral())
      {
         component.addPrimitivePropertyInitializer("literal", Boolean.TRUE);
      }

      if (!loader.isEnvironmentOnly() && !loader.isValidatedOnly())
      {
         loader.addEnvironmentFixup(new ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               Component cf = null;

               for (Iterator fragmentItr = db.getFragmentIterator(); fragmentItr.hasNext();)
               {
                  RelationalDatabaseFragment fragment = (RelationalDatabaseFragment)fragmentItr.next();

                  if (J2EEUtil.isContained() && db.isEnabled())
                  {
                     cf = new Component(source.getName() + ".ConnectionFactory" + fragment.getSuffix(),
                        loader.getHelper().getClassObject(SysUtil.PACKAGE + ".core.persistence.sql.SQLJNDIConnectionFactory"),
                        Component.SINGLETON);

                     cf.addPrimitivePropertyInitializer("dataSource", J2EEUtil.JNDI_ENV_PREFIX + "jdbc/" + db.getName() + fragment.getSuffix());
                  }
                  else
                  {
                     cf = new Component(source.getName() + ".ConnectionFactory" + fragment.getSuffix(),
                        loader.getHelper().getClassObject(SysUtil.PACKAGE + ".core.persistence.sql.SQLRAConnectionFactory"),
                        Component.SINGLETON);

                     SQLAdapter adapterInstance = (SQLAdapter)component.getInstance(null);

                     cf.addPrimitivePropertyInitializer("fragment", fragment);
                     cf.addPrimitivePropertyInitializer("initialSQL", adapterInstance.getInitialSQL());
                     cf.addPrimitivePropertyInitializer("testSQL", adapterInstance.getTestSQL());
                  }

                  cf.setMetadata(loader.getMetadata());
                  fragment.setConnectionFactory(cf);
                  loader.addSingletonFixup(cf);
               }

               if (db.getFragmentCount() > 1)
               {
                  cf = new Component(source.getName() + ".FragmentConnectionFactory",
                           loader.getHelper().getClassObject(SysUtil.PACKAGE + ".core.persistence.sql.SQLFragmentConnectionFactory"),
                     Component.SINGLETON);

                  cf.setMetadata(loader.getMetadata());
                  cf.addPrimitivePropertyInitializer("database", db);
                  component.addPrimitivePropertyInitializer("fragmented", Boolean.TRUE);
               }

               component.addComponentPropertyInitializer("connectionFactory", cf);
            }
         });
      }
   }

   /**
    * Loads a fragment from a DOM element.
    * @param element The DOM element.
    * @param fragment The fragment to load.
    * @param bDefault True to default the required attributes.
    */
   protected void loadFragment(Element element, RelationalDatabaseFragment fragment, boolean bDefault)
   {
      fragment.setAlias(XMLUtil.getStringAttr(element, "alias", ((RelationalDatabase)fragment.getDataSource()).getAlias() + fragment.getSuffix()));

      fragment.setHost((bDefault) ?
         XMLUtil.getStringAttr(element, "host", fragment.getHost()) :
         XMLUtil.getReqStringAttr(element, "host"));

      fragment.setPort(XMLUtil.getIntAttr(element, "port", fragment.getPort()));
      fragment.setInstance(XMLUtil.getStringAttr(element, "instance", fragment.getInstance()));

      fragment.setDatabase((bDefault) ?
         XMLUtil.getStringAttr(element, "database", fragment.getDatabase()) :
         XMLUtil.getReqStringAttr(element, "database"));

      fragment.setUser(XMLUtil.getStringAttr(element, "user", fragment.getUser()));
      fragment.setPassword(m_loader.decryptPassword(XMLUtil.getStringAttr(element, "password", fragment.getPassword())));
      fragment.setMinPoolSize(XMLUtil.getIntAttr(element, "minPoolSize", fragment.getMinPoolSize()));
      fragment.setMaxPoolSize(XMLUtil.getIntAttr(element, "maxPoolSize", fragment.getMaxPoolSize()));
      fragment.setStatementCacheSize(XMLUtil.getIntAttr(element, "statementCacheSize", fragment.getStatementCacheSize()));
      fragment.setQueryTimeout(XMLUtil.getIntAttr(element, "queryTimeout", fragment.getQueryTimeout()));
      fragment.setWarningTimeout(m_loader.getProperty("dataSourceConnection." + fragment.getDataSource().getName() +
         ".warningTimeout", XMLUtil.getLongAttr(element, "warningTimeout", fragment.getWarningTimeout())));

      m_loader.loadProperties(element, "Properties", fragment.getPropertyHolder());
   }

   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataLoader#loadUpgrade(org.w3c.dom.Element, java.lang.String, nexj.core.meta.persistence.DataSource, nexj.core.meta.xml.XMLMetadataHelper)
    */
   public SchemaUpgrade loadUpgrade(Element upgradeElement, final String sName,
      final DataSource dataSource, final XMLMetadataHelper helper)
   {
      final RelationalSchemaUpgrade upgrade = new RelationalSchemaUpgrade(sName);

      m_helper = helper;
      m_schema = (RelationalSchema)dataSource.getSchema();
      upgrade.setDataSource(dataSource);

      XMLUtil.forEachChildElement(upgradeElement, null, helper.new ElementHandler("step")
      {
         private int m_nOrdinal;

         protected void handleElement(Element stepElement, final String sExecOrdinal)
         {
            String sElement = stepElement.getNodeName();

            if (sElement.equals("CreateTable"))
            {
               final CreateTableStep create = new CreateTableStep();

               create.setType(parseTableType(XMLUtil.getStringAttr(stepElement, "type"), create.getType()));
               create.setName(XMLMetadataHelper.getNameAttr(stepElement, "name", XMLMetadataHelper.NAME_DOT));
               create.setAlias(XMLUtil.getStringAttr(stepElement, "alias"));
               create.setTablespaceName(XMLUtil.getStringAttr(stepElement, "tablespace"));
               create.setIndexspaceName(XMLUtil.getStringAttr(stepElement, "indexspace"));
               create.setLongspaceName(XMLUtil.getStringAttr(stepElement, "longspace"));

               XMLMetadataLoader.parsePatterns(XMLUtil.getStringAttr(stepElement, "aspects"),
                  new XMLMetadataLoader.PatternHandler()
                  {
                     public void handlePattern(final String sName, final boolean bInclusive)
                     {
                        create.addAspectOverride(sName, bInclusive);
                     }
                  });

               XMLMetadataLoader.parsePatterns(XMLUtil.getStringAttr(stepElement, "pointcuts"),
                  new XMLMetadataLoader.PatternHandler()
                  {
                     public void handlePattern(final String sPattern, final boolean bInclusive)
                     {
                        create.addPointcutPattern(sPattern, bInclusive);
                     }
                  });

               XMLUtil.withFirstChildElement(stepElement, "Columns", false, new ElementHandler()
               {
                  public void handleElement(Element columnsElement)
                  {
                     XMLUtil.forEachChildElement(columnsElement, "Column",
                        getHelper().new ElementHandler("column")
                     {
                        public void handleElement(Element columnElement, String sColumnName)
                        {
                           ColumnOutline outline = new ColumnOutline();

                           loadColumnOutline(columnElement, outline, true);
                           create.addColumnOutline(outline);
                        }
                     });
                  }
               });
               
               XMLUtil.withFirstChildElement(stepElement, "Indexes", false, new ElementHandler()
               {
                  public void handleElement(Element indexesElement)
                  {
                     XMLUtil.forEachChildElement(indexesElement, "Index",
                        getHelper().new ElementHandler("index")
                     {
                        public void handleElement(Element indexElement, String sIndexName)
                        {
                           IndexOutline outline = new IndexOutline();

                           loadIndexOutline(indexElement, outline);
                           create.addIndexOutline(outline);
                        }
                     });
                  }
               });

               XMLUtil.withFirstChildElement(stepElement, "View", false, new ElementHandler()
               {
                  public void handleElement(Element viewElement)
                  {
                     final SQLScript script = new SQLScript();

                     loadSQLScript(viewElement, script);
                     create.setViewScript(script);
                     create.setViewAutoUpdated(
                        XMLUtil.getBooleanAttr(
                           viewElement, "autoupdated", create.isViewAutoUpdated()));
                  }
               });

               String sHints = XMLUtil.getStringAttr(stepElement, "hints");

               if (sHints != null)
               {
                  for (StringTokenizer tokenizer = new StringTokenizer(sHints);
                       tokenizer.hasMoreTokens();)
                  {
                     create.addHint(tokenizer.nextToken());
                  }
               }

               create.setPrimaryKeyName((create.getType() == Table.ASPECT) ?
                  XMLUtil.getStringAttr(stepElement, "primaryKey") :
                  XMLUtil.getReqStringAttr(stepElement, "primaryKey"));

               upgrade.addStep(create);
            }
            else if (sElement.equals("AlterTable"))
            {
               final AlterTableStep alter = new AlterTableStep();

               alter.setType(parseTableType(XMLUtil.getStringAttr(stepElement, "type"), alter.getType()));
               alter.setName(XMLMetadataHelper.getNameAttr(stepElement, "name", XMLMetadataHelper.NAME_DOT));
               alter.setAlias(XMLUtil.getStringAttr(stepElement, "alias"));
               alter.setPrimaryKeyName(XMLUtil.getStringAttr(stepElement, "primaryKey"));

               XMLMetadataLoader.parsePatterns(
                  XMLUtil.getStringAttr(stepElement, "hints"),
                  new XMLMetadataLoader.PatternHandler()
               {
                  public void handlePattern(String sName, boolean bEnabled)
                  {
                     alter.setHint(sName, bEnabled);
                  }
               });

               upgrade.addStep(alter);
            }
            else if (sElement.equals("RenameTable"))
            {
               RenameTableStep ren = new RenameTableStep();

               ren.setOldName(XMLMetadataHelper.getNameAttr(stepElement, "name", XMLMetadataHelper.NAME_DOT));
               ren.setNewName(XMLMetadataHelper.getNameAttr(stepElement, "to", XMLMetadataHelper.NAME_DOT));

               upgrade.addStep(ren);
            }
            else if (sElement.equals("DropTable"))
            {
               DropTableStep drop = new DropTableStep();

               drop.setName(XMLMetadataHelper.getNameAttr(stepElement, "name", XMLMetadataHelper.NAME_DOT));

               upgrade.addStep(drop);
            }
            else if (sElement.equals("ApplyTableAspect"))
            {
               final ApplyTableAspectStep apply = new ApplyTableAspectStep();

               apply.setAspectName(XMLMetadataHelper.getNameAttr(stepElement, "name", XMLMetadataHelper.NAME_DOT));
               apply.setOverride(XMLUtil.getBooleanAttr(stepElement, "override", apply.isOverride()));

               XMLMetadataLoader.parsePatterns(XMLUtil.getStringAttr(stepElement, "pointcuts"),
                  new XMLMetadataLoader.PatternHandler()
                  {
                     public void handlePattern(String sPattern, boolean bInclusive)
                     {
                        apply.addPointcutPattern(sPattern, bInclusive);
                     }
                  });

               loadSQLScripts(stepElement, apply.getScriptHolder());

               upgrade.addStep(apply);
            }
            else if (sElement.equals("RemoveTableAspect"))
            {
               final RemoveTableAspectStep remove = new RemoveTableAspectStep();

               remove.setAspectName(XMLMetadataHelper.getNameAttr(stepElement, "name", XMLMetadataHelper.NAME_DOT));
               remove.setOverride(XMLUtil.getBooleanAttr(stepElement, "override", remove.isOverride()));

               XMLMetadataLoader.parsePatterns(XMLUtil.getStringAttr(stepElement, "pointcuts"),
                  new XMLMetadataLoader.PatternHandler()
                  {
                     public void handlePattern(String sPattern, boolean bInclusive)
                     {
                        remove.addPointcutPattern(sPattern, bInclusive);
                     }
                  });

               upgrade.addStep(remove);
            }
            else if (sElement.equals("CreateColumn"))
            {
               CreateColumnStep create = new CreateColumnStep();

               create.setTableName(XMLMetadataHelper.getNameAttr(stepElement, "table", XMLMetadataHelper.NAME_DOT));

               ColumnOutline outline = new ColumnOutline();

               loadColumnOutline(stepElement, outline, true);
               create.setOutline(outline);

               loadSQLScripts(stepElement, create.getScriptHolder());

               upgrade.addStep(create);
            }
            else if (sElement.equals("AlterColumn"))
            {
               AlterColumnStep alter = new AlterColumnStep();

               alter.setTableName(XMLMetadataHelper.getNameAttr(stepElement, "table", XMLMetadataHelper.NAME_DOT));

               ColumnOutline outline = new ColumnOutline();

               loadColumnOutline(stepElement, outline, false);
               alter.setOutline(outline);

               upgrade.addStep(alter);
            }
            else if (sElement.equals("RenameColumn"))
            {
               RenameColumnStep ren = new RenameColumnStep();

               ren.setTableName(XMLMetadataHelper.getNameAttr(stepElement, "table", XMLMetadataHelper.NAME_DOT));
               ren.setOldName(XMLMetadataHelper.getNameAttr(stepElement, "name", XMLMetadataHelper.NAME_ID));
               ren.setNewName(XMLMetadataHelper.getNameAttr(stepElement, "to", XMLMetadataHelper.NAME_ID));

               upgrade.addStep(ren);
            }
            else if (sElement.equals("DropColumn"))
            {
               DropColumnStep drop = new DropColumnStep();

               drop.setTableName(XMLMetadataHelper.getNameAttr(stepElement, "table", XMLMetadataHelper.NAME_DOT));
               drop.setName(XMLMetadataHelper.getNameAttr(stepElement, "name", XMLMetadataHelper.NAME_ID));

               upgrade.addStep(drop);
            }
            else if (sElement.equals("CreateIndex"))
            {
               CreateIndexStep create = new CreateIndexStep();

               create.setTableName(XMLMetadataHelper.getNameAttr(stepElement, "table", XMLMetadataHelper.NAME_DOT));

               IndexOutline outline = new IndexOutline();

               loadIndexOutline(stepElement, outline);

               create.setOutline(outline);

               upgrade.addStep(create);
            }
            else if (sElement.equals("RenameIndex"))
            {
               RenameIndexStep ren = new RenameIndexStep();

               ren.setOldName(XMLMetadataHelper.getNameAttr(stepElement, "name", XMLMetadataHelper.NAME_DOT));
               ren.setNewName(XMLMetadataHelper.getNameAttr(stepElement, "to", XMLMetadataHelper.NAME_DOT));

               upgrade.addStep(ren);
            }
            else if (sElement.equals("DropIndex"))
            {
               DropIndexStep drop = new DropIndexStep();

               drop.setName(XMLMetadataHelper.getNameAttr(stepElement, "name", XMLMetadataHelper.NAME_DOT));

               upgrade.addStep(drop);
            }
            else if (sElement.equals("ApplyIndexAspect"))
            {
               final ApplyIndexAspectStep apply = new ApplyIndexAspectStep();

               apply.setAspectName(XMLMetadataHelper.getNameAttr(stepElement, "name", XMLMetadataHelper.NAME_DOT));
               apply.setOverride(XMLUtil.getBooleanAttr(stepElement, "override", apply.isOverride()));

               XMLMetadataLoader.parsePatterns(XMLUtil.getStringAttr(stepElement, "pointcuts"),
                  new XMLMetadataLoader.PatternHandler()
                  {
                     public void handlePattern(String sPattern, boolean bInclusive)
                     {
                        apply.addPointcutPattern(sPattern, bInclusive);
                     }
                  });

               upgrade.addStep(apply);
            }
            else if (sElement.equals("RemoveIndexAspect"))
            {
               final RemoveIndexAspectStep remove = new RemoveIndexAspectStep();

               remove.setAspectName(XMLMetadataHelper.getNameAttr(stepElement, "name", XMLMetadataHelper.NAME_DOT));
               remove.setOverride(XMLUtil.getBooleanAttr(stepElement, "override", remove.isOverride()));

               XMLMetadataLoader.parsePatterns(XMLUtil.getStringAttr(stepElement, "pointcuts"),
                  new XMLMetadataLoader.PatternHandler()
                  {
                     public void handlePattern(String sPattern, boolean bInclusive)
                     {
                        remove.addPointcutPattern(sPattern, bInclusive);
                     }
                  });

               upgrade.addStep(remove);
            }
            else if (sElement.equals("CreateObject"))
            {
               CreateObjectStep create = new CreateObjectStep();

               create.setName(XMLMetadataHelper.getNameAttr(stepElement, "name", XMLMetadataHelper.NAME_DOT));
               loadSQLScript(stepElement, create.getScript());
               upgrade.addStep(create);
            }
            else if (sElement.equals("DropObject"))
            {
               DropObjectStep drop = new DropObjectStep();

               drop.setName(XMLMetadataHelper.getNameAttr(stepElement, "name", XMLMetadataHelper.NAME_DOT));
               loadSQLScript(stepElement, drop.getScript());
               upgrade.addStep(drop);
            }
            else if (sElement.equals("Exec"))
            {
               final ExecStep exec = new ExecStep();

               exec.setUpgrade(upgrade);
               loadSQLScripts(stepElement, exec.getScriptHolder());

               upgrade.addStep(exec);
            }
            else if (sElement.equals("SupportAdapter"))
            {
               SupportAdapterStep adapter = new SupportAdapterStep();

               adapter.setAdapter(
                  dataSource.getType().getAdapter(XMLUtil.getStringAttr(stepElement, "name")));
               upgrade.addStep(adapter);
            }
            else
            {
               throw new MetadataException("err.meta.persistence.sql.upgrade.invalidStepElement",
                  new Object[]{sElement, sName});
            }
         }

         protected String getName(Element element)
         {
            return String.valueOf(m_nOrdinal++);
         }
      });

      return upgrade;
   }

   /**
    * Parses a table type form a string.
    * @param sType The string to parse.
    * @param nDefault The default type value.
    */
   protected byte parseTableType(String sType, byte nDefault)
   {
      if (sType == null)
      {
         return nDefault;
      }

      if (sType.equals("managed"))
      {
         return Table.MANAGED;
      }

      if (sType.equals("external"))
      {
         return Table.EXTERNAL;
      }

      if (sType.equals("query"))
      {
         return Table.QUERY;
      }

      if (sType.equals("view"))
      {
         return Table.VIEW;
      }

      if (sType.equals("aspect"))
      {
         return Table.ASPECT;
      }

      throw new MetadataException("err.meta.tableType", new Object[]{sType});
   }

   /**
    * Parses a column type from a string.
    * @param sType The column type string. Can be null.
    * @return The primitive type, or null if sType is null.
    */
   protected Primitive parseColumnType(String sType) throws MetadataException
   {
      if (sType == null)
      {
         return null;
      }

      return Primitive.parse(sType);
   }

   /**
    * Parses a column allocation from a string.
    * @param sAllocation The column allocation string. Can be null.
    * @return One of the Column.* constants, or -1 if sAllocation is null.
    */
   protected byte parseColumnAllocation(String sAllocation) throws MetadataException
   {
      if (sAllocation == null)
      {
         return -1;
      }

      if (sAllocation.equals("fixed"))
      {
         return Column.FIXED;
      }

      if (sAllocation.equals("varying"))
      {
         return Column.VARYING;
      }

      if (sAllocation.equals("locator"))
      {
         return Column.LOCATOR;
      }

      throw new MetadataException("err.meta.allocation", new Object[]{sAllocation});
   }

   /**
    * Loads a column outline from a DOM element.
    * @param element The column DOM element.
    * @param outline The column outline.
    * @param bRequired True if the full definition for a new column is required.
    */
   protected void loadColumnOutline(Element element, ColumnOutline outline, boolean bRequired)
   {
      outline.setName(XMLMetadataHelper.getNameAttr(element, "name", XMLMetadataHelper.NAME_ID));
      outline.setCaseInsensitive(XMLUtil.getBooleanObjAttr(element, "caseInsensitive"));

      outline.setType(parseColumnType((bRequired) ? XMLUtil.getReqStringAttr(element, "type") :
            XMLUtil.getStringAttr(element, "type")),
         XMLUtil.getIntegerObjAttr(element, "precision"),
         XMLUtil.getIntegerObjAttr(element, "scale"),
         parseColumnAllocation(XMLUtil.getStringAttr(element, "allocation")));

      outline.setNullable(XMLUtil.getBooleanObjAttr(element, "nullable"));
      outline.setLiteral(XMLUtil.getBooleanObjAttr(element, "literal"));
      outline.setConverterName(XMLUtil.getStringAttr(element, "converter"));
   }

   /**
    * Parses an index type from a string.
    * @param sType The index type string.
    * @param nDefault The default index type, one of the Index.* constants.
    * @return The index type, one of the Index.* constants.
    */
   protected byte parseIndexType(String sType, byte nDefault) throws MetadataException
   {
      if (sType == null)
      {
         return nDefault;
      }

      if (sType.equals("btree"))
      {
         return Index.BTREE;
      }

      if (sType.equals("text"))
      {
         return Index.TEXT;
      }

      if (sType.equals("cluster"))
      {
         return Index.CLUSTER;
      }

      if (sType.equals("virtual"))
      {
         return Index.VIRTUAL;
      }

      if (sType.equals("query"))
      {
         return Index.QUERY;
      }

      if (sType.equals("aspect"))
      {
         return Index.ASPECT;
      }

      throw new MetadataException("err.meta.indexType", new Object[]{sType});
   }

   /**
    * Loads an index outline from a DOM element.
    * @param element The DOM element.
    * @param outline The index outline.
    */
   protected void loadIndexOutline(Element element, final IndexOutline outline)
   {
      outline.setName(XMLMetadataHelper.getNameAttr(element, "name", XMLMetadataHelper.NAME_DOT));
      outline.setRelatedTableName(XMLUtil.getStringAttr(element, "relatedTable"));
      outline.setType(parseIndexType(XMLUtil.getStringAttr(element, "type"), outline.getType()));
      outline.setUnique(XMLUtil.getBooleanAttr(element, "unique", outline.isUnique()));
      outline.setFill(XMLUtil.getIntAttr(element, "fill", outline.getFill()));

      XMLMetadataLoader.parsePatterns(XMLUtil.getStringAttr(element, "aspects"),
         new XMLMetadataLoader.PatternHandler()
         {
            public void handlePattern(final String sName, final boolean bInclusive)
            {
               outline.addAspectOverride(sName, bInclusive);
            }
         });

      XMLUtil.forEachChildElement(element, "IndexColumn",
         getHelper().new ElementHandler("column")
      {
         public void handleElement(Element element, String sName)
         {
            outline.addColumn(sName, XMLUtil.getBooleanAttr(element, "ascending", true));
         }
      });
   }

   /**
    * Loads SQL scripts from a DOM element.
    * @param element The DOM element containing the script elements.
    * @param holder The destination script holder.
    */
   protected void loadSQLScripts(Element element, final SQLScriptHolder holder)
   {
      XMLUtil.forEachChildElement(element, null, m_helper.new ElementHandler("script")
      {
         private int m_nOrdinal;

         protected void handleElement(Element scriptElement, String sSQLOrdinal)
         {
            final SQLScript script = new SQLScript();
            String sElement = scriptElement.getNodeName();

            if (sElement.equals("SQL"))
            {
               SQLStatement stmt = new SQLStatement();

               stmt.setSQL(XMLUtil.getElementValue(scriptElement));
               script.addStatement(stmt);
            }
            else if (sElement.equals("Switch"))
            {
               loadSQLScript(scriptElement, script);
            }
            else
            {
               throw new MetadataException(
                  "err.meta.persistence.sql.upgrade.invalidSQLScriptElement",
                  new Object[]{sElement});
            }

            holder.addScript(script);
         }

         protected String getName(Element element)
         {
            return String.valueOf(m_nOrdinal++);
         }
      });
   }

   /**
    * Loads an SQL script from a DOM element.
    * @param scriptElement The DOM element.
    * @param script The script metadata object.
    */
   protected void loadSQLScript(Element scriptElement, final SQLScript script)
   {
      XMLUtil.forEachChildElement(scriptElement, "SQL", m_helper.new ElementHandler("sql")
      {
         private int m_nOrdinal;

         protected void handleElement(Element sqlElement, String sOrdinal)
         {
            SQLStatement stmt = new SQLStatement();

            stmt.setScript(script);

            String sAdapter = XMLUtil.getStringAttr(sqlElement, "adapter");

            if (sAdapter != null)
            {
               for (StringTokenizer tokenizer = new StringTokenizer(sAdapter);
                  tokenizer.hasMoreTokens();)
               {
                  stmt.addAdapter(tokenizer.nextToken(), m_schema.getDataSource().getType());
               }
            }

            stmt.setSQL(XMLUtil.getElementValue(sqlElement));
            script.addStatement(stmt);
         }

         protected String getName(Element element)
         {
            return String.valueOf(m_nOrdinal++);
         }
      });
   }
}
