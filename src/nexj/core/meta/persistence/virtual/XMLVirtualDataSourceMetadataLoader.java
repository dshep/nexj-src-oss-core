// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.virtual;

import java.util.ArrayList;

import org.w3c.dom.Element;

import nexj.core.meta.Attribute;
import nexj.core.meta.Component;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.DataSourceType;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.meta.persistence.SchemaUpgrade;
import nexj.core.meta.persistence.XMLPersistenceMetadataLoader;
import nexj.core.meta.xml.XMLMetadata;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataHelper.ContextFixup;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Lookup;
import nexj.core.util.XMLUtil;
import nexj.core.util.XMLUtil.ElementHandler;

/**
 * Loads virtual persistence adapter data source, connection, and persistence mapping
 * metadata.
 */
public class XMLVirtualDataSourceMetadataLoader implements XMLPersistenceMetadataLoader
{
   // associations

   /**
    * The metadata loader.
    */
   protected XMLMetadataLoader m_loader;

   /**
    * The metadata helper.
    */
   protected XMLMetadataHelper m_helper;

   // operations

   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataLoader#loadConnection(org.w3c.dom.Element, nexj.core.meta.persistence.DataSource, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public void loadConnection(Element element, DataSource source, final XMLMetadataLoader loader)
   {
      final VirtualDataSource ds = (VirtualDataSource)source;
      final VirtualDataSourceFragment defaultFragment = (VirtualDataSourceFragment)ds.getDefaultFragment();
      Component adapter = new Component(ds.getName(), ds.getAdapter().getClassObject(), Component.CONTEXT);

      ds.setComponent(adapter);
      adapter.setMetadata(loader.getMetadata());

      if (element != null)
      {
         loadFragment(element, defaultFragment, loader);

         XMLUtil.withFirstChildElement(element, "Fragments", false, new ElementHandler()
         {
            public void handleElement(Element fragmentsElement)
            {
               XMLUtil.forEachChildElement(fragmentsElement, "Fragment", loader.getHelper().new ElementHandler("fragment")
               {
                  protected void handleElement(Element fragmentElement, String sName)
                  {
                     VirtualDataSourceFragment fragment = (VirtualDataSourceFragment)defaultFragment.clone(false);

                     fragment.setName(sName);
                     loadFragment(fragmentElement, fragment, loader);
                     ds.addFragment(fragment);
                  }
               });
            }
         });

         if (ds.getFragmentCount() > 1)
         {
            adapter.addPrimitivePropertyInitializer("fragmented", Boolean.TRUE);
         }
      }
      else
      {
         // Disable if no connection file entry.
         ds.setCreatable(false);
         ds.setReadable(false);
         ds.setUpdatable(false);
         ds.setDeletable(false);
         ds.setExecutable(false);
      }
   }

   /**
    * Loads fragment metadata.
    * @param element The fragment element.
    * @param fragment The fragment.
    * @param loader The metadata loader.
    */
   protected void loadFragment(Element element, VirtualDataSourceFragment fragment, XMLMetadataLoader loader)
   {
      fragment.setWarningTimeout(loader.getProperty("dataSourceConnection." + fragment.getDataSource().getName() +
         ".warningTimeout", XMLUtil.getLongAttr(element, "warningTimeout", fragment.getWarningTimeout())));

      loader.loadProperties(element, "Properties", fragment.getPropertyHolder());
   }

   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataLoader#loadDataSource(org.w3c.dom.Element, java.lang.String, nexj.core.meta.persistence.DataSourceType, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public DataSource loadDataSource(Element element, String sName, DataSourceType type, XMLMetadataLoader loader)
   {
      VirtualDataSource ds = new VirtualDataSource(sName);

      ds.setType(type);
      loader.loadDataSource(element, ds);

      return ds;
   }

   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataLoader#loadMapping(org.w3c.dom.Element, nexj.core.meta.Metaclass, nexj.core.meta.persistence.DataSource, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public PersistenceMapping loadMapping(Element element, final Metaclass metaclass, DataSource dataSource, final XMLMetadataLoader loader)
   {
      m_loader = loader;
      m_helper = loader.getHelper();

      final VirtualMapping mapping = new VirtualMapping();
      final XMLMetadata metadata = loader.getMetadata();
      final String sURLPrefix = "class:" + metaclass.getName() + ".persistence"; 
      int nCookie = m_helper.pushMarker(MetadataValidationException.TYPE_NAME, "ServiceMapping");

      m_helper.pushMarker("class", metaclass.getName());

      try
      {
         mapping.setMetaclass(metaclass);
         mapping.setDataSource(dataSource);

         XMLUtil.withFirstChildElement(element, "ServiceMapping",
            true, new XMLUtil.ElementHandler()
         {
            public void handleElement(Element element)
            {
               final ArrayList keyTypeList = new ArrayList(4);

               XMLUtil.withFirstChildElement(element, "KeyParts", false,
                  new XMLUtil.ElementHandler()
               {
                  public void handleElement(Element element)
                  {
                     XMLUtil.forEachChildElement(element, "KeyPart", new XMLUtil.ElementHandler()
                     {
                        public void handleElement(Element element)
                        {
                           keyTypeList.add(Primitive.find(XMLUtil.getReqStringAttr(element, "type")));

                           String sAttributeName = XMLUtil.getStringAttr(element, "attribute");

                           if (sAttributeName != null)
                           {
                              Attribute attribute = metaclass.getAttribute(sAttributeName);
                              VirtualPrimitiveMapping primitiveMapping = new VirtualPrimitiveMapping();

                              primitiveMapping.setObjectKeyPart(keyTypeList.size() - 1);
                              primitiveMapping.setAttribute(attribute);
                              mapping.addAttributeMapping(primitiveMapping);
                              primitiveMapping.validate(metadata, m_helper.getWarnings());
                           }
                        }
                     });

                     mapping.setKey((Primitive[])keyTypeList.toArray(new Primitive[keyTypeList.size()]));
                  }
               });

               XMLUtil.withFirstChildElement(element, "AttributeMappings", false,
                  new XMLUtil.ElementHandler()
               {
                  public void handleElement(Element element)
                  {
                     XMLUtil.forEachChildElement(element, "AttributeMapping", new XMLUtil.ElementHandler()
                     {
                        public void handleElement(Element element)
                        {
                           String sName = XMLUtil.getReqStringAttr(element, "name");
                           int nMaxLength = XMLUtil.getIntAttr(element, "maxLength", 0);
                           final Attribute attribute = metaclass.getAttribute(sName);
                           AttributeMapping attrMapping;

                           if (attribute.getType().isPrimitive())
                           {
                              VirtualPrimitiveMapping primitiveMapping = new VirtualPrimitiveMapping();

                              attrMapping = primitiveMapping;

                              if (element.hasAttribute("attributes"))
                              {
                                 throw new MetadataException("err.meta.persistence.virtual.invalidPropertySpecified",
                                    new Object[]{"attributes", sName, metaclass.getName()});
                              }

                              if (XMLUtil.getBooleanAttr(element, "objectSourceKey", false))
                              {
                                 throw new MetadataException("err.meta.persistence.virtual.invalidPropertySpecified",
                                    new Object[]{"objectSourceKey", sName, metaclass.getName()});
                              }

                              if (element.hasAttribute("destinationKey"))
                              {
                                 throw new MetadataException("err.meta.persistence.virtual.invalidPropertySpecified",
                                    new Object[]{"destinationKey", sName, metaclass.getName()});
                              }

                              if (nMaxLength != 0)
                              {
                                 if (attribute.getType() != Primitive.BINARY && attribute.getType() != Primitive.STRING)
                                 {
                                    throw new MetadataException("err.meta.persistence.virtual.maxLengthSpecified",
                                       new Object[]{attribute.getName(), metaclass.getName()});
                                 }

                                 primitiveMapping.setMaxLength(nMaxLength);
                              }
                           }
                           else
                           {
                              final VirtualClassMapping classMapping = new VirtualClassMapping();

                              attrMapping = classMapping;

                              classMapping.setComposition((Pair)m_helper.parse(
                                 XMLUtil.getStringAttr(element, "attributes"),
                                 true, null, null, metadata.getGlobalEnvironment()
                              ));

                              final boolean bObjectSourceKey = XMLUtil.getBooleanAttr(element, "objectSourceKey", false);
                              final String sDestinationKeyName = XMLUtil.getStringAttr(element, "destinationKey");

                              if (nMaxLength != 0)
                              {
                                 throw new MetadataException("err.meta.persistence.virtual.maxLengthSpecified",
                                    new Object[]{attribute.getName(), metaclass.getName()});
                              }

                              loader.addPersistenceMappingFixup(new ContextFixup(m_helper)
                              {
                                 public void fixup()
                                 {
                                    Metaclass type = (Metaclass)attribute.getType();
                                    PersistenceMapping assocClassMapping = type.getPersistenceMapping();

                                    if (assocClassMapping == null)
                                    {
                                       throw new MetadataException("err.meta.missingAssocPersistenceMapping",
                                          new Object[]{attribute.getName(), metaclass.getName(), type.getName()});
                                    }

                                    classMapping.setSourceKey((bObjectSourceKey) ? mapping.getObjectKey() : new VirtualKey(attribute));
                                    classMapping.setDestinationKey(assocClassMapping.addForeignKey(sDestinationKeyName, classMapping));
                                 }
                              });

                              loader.addComponentFixup(new ContextFixup(m_helper)
                              {
                                 public void fixup()
                                 {
                                    classMapping.validate(metadata, m_helper.getWarnings());
                                 }
                              });
                           }

                           attrMapping.setAttribute(attribute);
                           mapping.addAttributeMapping(attrMapping);
                        }
                     });
                  }
               });

               XMLUtil.withFirstChildElement(element, "SortKeys", false,
                  new XMLUtil.ElementHandler()
               {
                  public void handleElement(Element element)
                  {
                     XMLUtil.forEachChildElement(element, "SortKey", new XMLUtil.ElementHandler()
                     {
                        public void handleElement(Element element)
                        {
                           mapping.addSortKey(
                              (Pair)m_helper.parse(XMLUtil.getReqStringAttr(element, "attributes"), true,
                                 mapping.getTextPositionMap(), null, metadata.getGlobalEnvironment()),
                              XMLUtil.getBooleanAttr(element, "unique", false)
                           );
                        }
                     });
                  }
               });

               XMLUtil.withFirstChildElement(element, "ReadMapping", false,
                  new XMLUtil.ElementHandler()
               {
                  public void handleElement(Element element)
                  {
                     final ReadMapping readMapping = mapping.getReadMapping();

                     XMLUtil.forEachChildElement(element, "Case", new XMLUtil.ElementHandler()
                     {
                        private int m_nOrdinal = 0;

                        public void handleElement(Element element)
                        {
                           final String sReadPrefix = sURLPrefix + ".read." + m_nOrdinal;
                           Object whereExpr = m_helper.parse(
                              XMLUtil.getStringAttr(element, "where"),
                              false, sReadPrefix + "$where", mapping.getTextPositionMap(), Symbol.ELSE, metadata.getGlobalEnvironment()
                           );
                           Pair variables = (Pair)m_helper.parse(
                              XMLUtil.getStringAttr(element, "variables"),
                              true, mapping.getTextPositionMap(), null, metadata.getGlobalEnvironment()
                           );
                           final ReadMappingCase readMappingCase = new ReadMappingCase();

                           readMappingCase.setWhere(whereExpr);
                           readMappingCase.setVariables(variables);

                           XMLUtil.withFirstChildElement(element, "Read", true, new XMLUtil.ElementHandler()
                           {
                              public void handleElement(Element element)
                              {
                                 readMappingCase.setReadScript(loadScript(element, sReadPrefix + "$read", mapping.getTextPositionMap()));
                              }
                           });

                           XMLUtil.withFirstChildElement(element, "Close", false, new XMLUtil.ElementHandler()
                           {
                              public void handleElement(Element element)
                              {
                                 readMappingCase.setCloseScript(loadScript(element, sReadPrefix + "$close", mapping.getTextPositionMap()));
                              }
                           });

                           readMapping.addCase(readMappingCase);
                           m_nOrdinal += 1;
                        }
                     });
                  }
               });

               XMLUtil.withFirstChildElement(element, "CreateMapping", false,
                  new XMLUtil.ElementHandler()
               {
                  public void handleElement(Element element)
                  {
                     final WorkMapping operation = new WorkMapping(metaclass);

                     operation.setBatch(XMLUtil.getBooleanAttr(element, "batch", operation.isBatch()));
                     operation.setScript(loadScript(element, sURLPrefix + ".create", mapping.getTextPositionMap()));
                     mapping.setCreateMapping(operation);
                  }
               });

               XMLUtil.withFirstChildElement(element, "UpdateMapping", false,
                  new XMLUtil.ElementHandler()
               {
                  public void handleElement(Element element)
                  {
                     final UpdateMapping updateMapping = new UpdateMapping(mapping);

                     XMLUtil.forEachChildElement(element, "Case", new XMLUtil.ElementHandler()
                     {
                        private int m_nOrdinal = 0;

                        public void handleElement(Element element)
                        {
                           final UpdateMappingCase update = new UpdateMappingCase(metaclass);

                           update.setBatch(XMLUtil.getBooleanAttr(element, "batch", update.isBatch()));
                           update.setDirty(XMLUtil.getBooleanAttr(element, "dirty", update.isDirty()));
                           update.setFull(XMLUtil.getBooleanAttr(element, "full", update.isFull()));

                           Pair attributes = (Pair)m_helper.parse(
                              XMLUtil.getStringAttr(element, "attributes"),
                              true, mapping.getTextPositionMap(), null, metadata.getGlobalEnvironment()
                           );

                           update.setScript(loadScript(element, sURLPrefix + ".update." + m_nOrdinal, mapping.getTextPositionMap()));
                           updateMapping.addCase(update, attributes);
                           m_nOrdinal += 1;
                        }
                     });

                     mapping.setUpdateMapping(updateMapping);
                  }
               });

               XMLUtil.withFirstChildElement(element, "DeleteMapping", false,
                  new XMLUtil.ElementHandler()
               {
                  public void handleElement(Element element)
                  {
                     final WorkMapping operation = new WorkMapping(metaclass);

                     operation.setBatch(XMLUtil.getBooleanAttr(element, "batch", operation.isBatch()));
                     operation.setScript(loadScript(element, sURLPrefix + ".delete", mapping.getTextPositionMap()));
                     mapping.setDeleteMapping(operation);
                  }
               });

               // Key Generator
               final String sKeyGeneratorName = XMLUtil.getStringAttr(element, "keyGenerator");

               if (sKeyGeneratorName != null)
               {
                  loader.addComponentFixup(new ContextFixup(m_helper)
                  {
                     public void fixup()
                     {
                        mapping.setKeyGenerator(metadata.getComponent(sKeyGeneratorName));
                     }
                  });
               }

               mapping.setDerived(XMLUtil.getBooleanAttr(element, "derived", mapping.isDerived()));
            }
         });

         if (metaclass.isPointcut() && mapping.m_objectKey == null)
         {
            loader.addPersistenceMappingFixup(new ContextFixup(m_helper)
            {
               public void fixup()
               {
                  mapping.getObjectKey();
               }
            });
         }

         loader.addSingletonFixup(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               mapping.compile(loader.getMachine());
            }
         });
      }
      finally
      {
         m_helper.restoreMarker(nCookie);
      }

      return mapping;
   }

   /**
    * Loads and parses the first <Script> tag child of element. 
    * @param element The element containing the <Script> tag.
    * @param sURL The code URL. For example class:Name.persistence.create
    * @param textPosMap The parser text position map.
    * @return The parsed script.
    */
   public Pair loadScript(Element element, final String sURL, final Lookup textPosMap)
   {
      final Pair[] script = new Pair[1];

      XMLUtil.withFirstChildElement(element, "Script", true, new XMLUtil.ElementHandler()
      {
         public void handleElement(Element element)
         {
            script[0] = (Pair)m_helper.parse(
               m_helper.getElementValue(element),
               true, sURL, textPosMap, null, m_loader.getMetadata().getGlobalEnvironment());
         }
      });

      return script[0];
   }

   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataLoader#loadUpgrade(org.w3c.dom.Element, java.lang.String, nexj.core.meta.persistence.DataSource, nexj.core.meta.xml.XMLMetadataHelper)
    */
   public SchemaUpgrade loadUpgrade(Element element, String sName, DataSource source, XMLMetadataHelper helper)
   {
      return null;
   }
}
