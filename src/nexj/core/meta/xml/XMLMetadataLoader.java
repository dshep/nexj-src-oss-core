// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.xml;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URL;
import java.net.URLStreamHandler;
import java.sql.Date;
import java.sql.Time;
import java.sql.Timestamp;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Properties;
import java.util.Set;
import java.util.StringTokenizer;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

import nexj.core.admin.etl.DataLoader;
import nexj.core.integration.MessageParser;
import nexj.core.meta.Action;
import nexj.core.meta.Argument;
import nexj.core.meta.Attribute;
import nexj.core.meta.ClassAspect;
import nexj.core.meta.Component;
import nexj.core.meta.ComponentCollectionPropertyInitializer;
import nexj.core.meta.ComponentPropertyInitializer;
import nexj.core.meta.Documented;
import nexj.core.meta.Event;
import nexj.core.meta.ExternalLibrary;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataCompatibilityException;
import nexj.core.meta.MetadataCompoundValidationException;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLoader;
import nexj.core.meta.MetadataLoaderAware;
import nexj.core.meta.MetadataLoaderDispatcher;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataURLHandler;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.Primitive;
import nexj.core.meta.PrimitiveCollectionPropertyInitializer;
import nexj.core.meta.PrimitivePrivilege;
import nexj.core.meta.PrimitivePropertyInitializer;
import nexj.core.meta.Privilege;
import nexj.core.meta.PrivilegeGroup;
import nexj.core.meta.PropertyHolder;
import nexj.core.meta.PropertyInitializer;
import nexj.core.meta.Type;
import nexj.core.meta.TypeConversionException;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.ChannelType;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartInstance;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.MessageTable;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.Transformation;
import nexj.core.meta.integration.TransformationArgument;
import nexj.core.meta.integration.TransformationEndpoint;
import nexj.core.meta.integration.TransformationMapping;
import nexj.core.meta.integration.TransformationSource;
import nexj.core.meta.integration.XMLIntegrationMetadataLoader;
import nexj.core.meta.integration.XMLMessageMappingLoader;
import nexj.core.meta.integration.channel.queueing.ObjectDispatcherQueue;
import nexj.core.meta.integration.format.xml.NameResolver;
import nexj.core.meta.integration.format.xml.XSDMessageImporter;
import nexj.core.meta.integration.service.Binding;
import nexj.core.meta.integration.service.Case;
import nexj.core.meta.integration.service.Dispatch;
import nexj.core.meta.integration.service.Interface;
import nexj.core.meta.integration.service.Jump;
import nexj.core.meta.integration.service.Persist;
import nexj.core.meta.integration.service.Send;
import nexj.core.meta.integration.service.SendReceive;
import nexj.core.meta.integration.service.Service;
import nexj.core.meta.integration.service.Sync;
import nexj.core.meta.integration.service.Transform;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.ClassMapping;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.DataSourceAdapter;
import nexj.core.meta.persistence.DataSourceType;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.meta.persistence.XMLPersistenceMetadataLoader;
import nexj.core.meta.workflow.Activity;
import nexj.core.meta.workflow.Assignment;
import nexj.core.meta.workflow.AutoCompletion;
import nexj.core.meta.workflow.Block;
import nexj.core.meta.workflow.Branch;
import nexj.core.meta.workflow.Catch;
import nexj.core.meta.workflow.Completion;
import nexj.core.meta.workflow.Concurrent;
import nexj.core.meta.workflow.Decision;
import nexj.core.meta.workflow.Flow;
import nexj.core.meta.workflow.FlowMacro;
import nexj.core.meta.workflow.FlowMacroScript;
import nexj.core.meta.workflow.Fork;
import nexj.core.meta.workflow.Goto;
import nexj.core.meta.workflow.Handler;
import nexj.core.meta.workflow.Join;
import nexj.core.meta.workflow.ManualCompletion;
import nexj.core.meta.workflow.Parallel;
import nexj.core.meta.workflow.Script;
import nexj.core.meta.workflow.Scripted;
import nexj.core.meta.workflow.Semaphore;
import nexj.core.meta.workflow.Step;
import nexj.core.meta.workflow.Timeout;
import nexj.core.meta.workflow.Trigger;
import nexj.core.meta.workflow.TryCatch;
import nexj.core.meta.workflow.Variable;
import nexj.core.meta.workflow.Wait;
import nexj.core.meta.workflow.Workflow;
import nexj.core.meta.xml.XMLMetadataHelper.CharacterStreamHandler;
import nexj.core.meta.xml.XMLMetadataHelper.ContextFixup;
import nexj.core.meta.xml.XMLMetadataHelper.Fixup;
import nexj.core.meta.xml.XMLMetadataHelper.ResourceHandler;
import nexj.core.meta.xml.XMLMetadataHelper.ResourceNameHandler;
import nexj.core.runtime.Context;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Macro;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.SchemeParser;
import nexj.core.scripting.Symbol;
import nexj.core.util.CertificateUtil;
import nexj.core.util.GenericException;
import nexj.core.util.GenericHashHolder;
import nexj.core.util.HTTP;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.IOUtil;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.ProgressListener;
import nexj.core.util.ProgressProxy;
import nexj.core.util.PropertyUtil;
import nexj.core.util.StringTable;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.TZ;
import nexj.core.util.TextPositionReader;
import nexj.core.util.UncheckedException;
import nexj.core.util.Undefined;
import nexj.core.util.XMLUtil;
import nexj.core.util.XMLUtil.ElementHandler;
import nexj.core.util.auth.SimplePrincipal;
import nexj.core.util.cipher.CharacterStreamCipherDispatcher;
import nexj.core.version.Version;


/**
 * Metadata loader using an XML serialization format.
 */
public class XMLMetadataLoader implements MetadataLoader
{
   // constants

   /**
    * The base URL property name.
    */
   public final static String BASE_URL_PROPERTY = "meta.base.url";

   /**
    * The compatible URL property name.
    */
   public final static String COMPATIBLE_URL_PROPERTY = "meta.compatible.url";

   /**
    * Connections URL property.
    */
   public final static String CONNECTIONS_URL_PROPERTY = "meta.connections.url";

   /**
    * Array with length of 1, containing the String class object.
    */
   protected final static Class[] STRING_CLASS_ARRAY = new Class[]{String.class};

   /**
    * External metadata loader hook property name.
    */
   protected final static String MEDATATA_LOAD_HOOK_PROPERTY = "meta.loader.hook";

   /**
    * Metadata bootstrap property name.
    */
   public final static String METADATA_BOOTSTRAP_PROPERTY = "meta.bootstrap";

   /**
    * The name of the deployment name property.
    */
   private final static String ENVIRONMENT_NAME_PROPERTY_NAME = "name";

   /**
    * "next" Symbol for use with iterators.
    */
   protected final static Symbol NEXT = Symbol.define("next");

   /**
    * "hasNext" Symbol for use with iterators.
    */
   protected final static Symbol HAS_NEXT = Symbol.define("hasNext");

   /**
    * Loading stage: no metadata loaded.
    */
   public final static byte STAGE_START = 0;

   /**
    * Loading stage: environment metadata loaded.
    */
   public final static byte STAGE_LOADED_ENVIRONMENT = 1;

   /**
    * Loading stage: system components loaded.
    */
   public final static byte STAGE_LOADED_COMPONENTS = 2;

   /**
    * Loading stage: executing environment fixups.
    */
   public final static byte STAGE_LOAD_ENVIRONMENT_FIXUPS = 3;

   /**
    * Loading stage: finished.
    */
   public final static byte STAGE_FINISHED = 4;

   // attributes

   /**
    * The environment or server metadata URL.
    */
   protected String m_sConfigURL;

   /**
    * The environment container, one of the J2EEUtil constants.
    */
   protected int m_nContainer;

   /**
    * The metadata loading stage, one of the STAGE_* constants;
    */
   protected byte m_nStage;

   /**
    * The environment-only flag.
    */
   protected boolean m_bEnvironmentOnly;

   /**
    * The runtime exclusion flag.
    */
   protected boolean m_bRuntimeExcluded;

   /**
    * The integration exclusion flag.
    */
   protected boolean m_bIntegrationExcluded;

   /**
    * The validation-only flag.
    */
   protected boolean m_bValidatedOnly;

   /**
    * The documentation loading flag.
    */
   protected boolean m_bDocumented;

   /**
    * The flag for leaving the metadata writable.
    */
   protected boolean m_bWritable;

   // associations

   /**
    * The password decryptor.
    */
   protected CharacterStreamCipherDispatcher m_decryptionDispatcher;

   /**
    * The metadata helper.
    */
   protected XMLMetadataHelper m_helper;

   /**
    * The metadata that is being loaded.
    */
   protected XMLMetadata m_metadata;

   /**
    * The VM for script loading.
    */
   protected Machine m_machine;

   /**
    * The environment properties.
    */
   protected Properties m_properties;

   /**
    * The enumeration name to value set map: Set[String].
    */
   protected Lookup m_enumerationValueMap; // of type Set[String].

   /**
    * The topologically sorted class aspect array.
    */
   protected ClassAspect[] m_classAspectArray;

   /**
    * The class inheritance check collection.
    */
   protected List m_inheritanceCheckList; // of type Fixup

   /**
    * The class inheritance fixup collection.
    */
   protected List m_inheritanceFixupList; // of type Fixup

   /**
    * The class inheritance additional first pass fixup collection.
    */
   protected List m_inheritance1stPassFixupList; // of type Fixup

   /**
    * The 2nd pass class inheritance fixup collection.
    */
   protected List m_inheritance2ndPassFixupList; // of type Fixup

   /**
    * The class inheritance fourth pass fixup collection.
    */
   protected List m_inheritance4thPassFixupList; // of type Fixup

   /**
    * The attribute generation fixup collection.
    */
   protected List m_attributeGenerationList; // of type Fixup

   /**
    * The attribute fixup collection.
    */
   protected List m_attributeFixupList; // of type Fixup

   /**
    * The attribute expression resolution collection.
    */
   protected List m_attributeResolutionList; // of type Fixup

   /**
    * The attribute dependency resolution collection.
    */
   protected List m_attributeDependencyList; // of type Fixup

   /**
    * The attribute dependency set.
    */
   protected Set m_attributeDependencySet; // of type Attribute

   /**
    * The action fixup collection.
    */
   protected List m_actionFixupList; // of type Fixup

   /**
    * The class second pass collection.
    */
   protected List m_class2ndPassList; // of type Fixup

   /**
    * The flow element fixup collection.
    */
   protected List m_flowFixupList; // of type Fixup.

   /**
    * The pre-inheritance message fixup collection. To be executed before
    * resolution of message inheritance.
    */
   protected List m_preInheritanceMessageFixupList; // of type Fixup

   /**
    * The post-inheritance message fixup collection. To be executed after
    * resolution of message inheritance.
    */
   protected List m_postInheritanceMessageFixupList; // of type Fixup

   /**
    * The transformation fixup list.
    */
   protected List m_transformationFixupList; // of type Fixup

   /**
    * The post-inheritance transformation fixup list. To be executed after
    * resolution of transformation inheritance.
    */
   protected List m_postInheritanceTransformationFixupList; // of type Fixup

   /**
    * The persistence mapping loading collection.
    */
   protected List m_persistenceMappingLoadList; // of type Fixup

   /**
    * The Document used for reparenting fixup Elements to conserve memory.
    */
   protected Document m_tmpDocument;

   /**
    * The persistence mapping fixup collection.
    */
   protected List m_persistenceMappingFixupList; // of type Fixup

   /**
    * The persistence mapping generation collection.
    */
   protected List m_persistenceMappingGenerationList; // of type Fixup

   /**
    * The IO fixup collection.
    */
   protected List m_ioFixupList; // of type Fixup

   /**
    * The environment fixup collection.
    */
   protected List m_environmentFixupList; // of type Fixup

   /**
    * The privilege fixup collection.
    */
   protected List m_privilegeFixupList; // of type Fixup

   /**
    * The component fixup collection.
    */
   protected List m_componentFixupList; // of type Fixup

   /**
    * The singleton component fixup collection.
    */
   protected List m_singletonFixupList; // of type Fixup

   /**
    * The metadata load hook.
    */
   protected XMLMetadataLoaderHook m_loadHook;

   /**
    * The system dispatcher channel.
    */
   protected ObjectDispatcherQueue m_dispatcherChannel;

   /**
    * The Services Oriented Architecture metadata loader.
    */
   protected XMLSOAMetadataLoader m_soaLoader;

   /**
    * The date parse format.
    */
   protected final static SimpleDateFormat s_dateFormat = new SimpleDateFormat("yyyy-MM-dd", Locale.ENGLISH);

   /**
    * The time parse format.
    */
   protected final static SimpleDateFormat s_timeFormat = new SimpleDateFormat("HH:mm:ss", Locale.ENGLISH);

   /**
    * The property parsers.
    */
   protected final static Lookup s_propertyParserMap = new HashTab(32); // of type PropertyParser[Class]

   /**
    * Names of intrinsic types that may be mapped to 'any'.
    */
   protected final static Set s_unsupportedIntrinsicSet = new HashHolder();

   static
   {
      s_dateFormat.setTimeZone(TZ.UTC);
      s_timeFormat.setTimeZone(TZ.UTC);

      s_propertyParserMap.put(Byte.class, new PrimitivePropertyParser(Byte.class, true));
      s_propertyParserMap.put(Byte.TYPE, new PrimitivePropertyParser(Byte.class, false));
      s_propertyParserMap.put(Short.class, new PrimitivePropertyParser(Short.class, true));
      s_propertyParserMap.put(Short.TYPE, new PrimitivePropertyParser(Short.class, false));
      s_propertyParserMap.put(Integer.class, new PrimitivePropertyParser(Integer.class, true));
      s_propertyParserMap.put(Integer.TYPE, new PrimitivePropertyParser(Integer.class, false));
      s_propertyParserMap.put(Long.class, new PrimitivePropertyParser(Long.class, true));
      s_propertyParserMap.put(Long.TYPE, new PrimitivePropertyParser(Long.class, false));
      s_propertyParserMap.put(Float.class, new PrimitivePropertyParser(Float.class, true));
      s_propertyParserMap.put(Float.TYPE, new PrimitivePropertyParser(Float.class, false));
      s_propertyParserMap.put(Double.class, new PrimitivePropertyParser(Double.class, true));
      s_propertyParserMap.put(Double.TYPE, new PrimitivePropertyParser(Double.class, false));
      s_propertyParserMap.put(BigDecimal.class, new PrimitivePropertyParser(BigDecimal.class, true));
      s_propertyParserMap.put(BigInteger.class, new PrimitivePropertyParser(BigInteger.class, true));

      s_propertyParserMap.put(Character.class, new PropertyParser()
      {
         public void parse(String sValue, PropertyInitializer initializer, XMLMetadataLoader loader)
         {
            if (StringUtil.isEmpty(sValue))
            {
               initializer.initializeValue(null);
            }
            else if (sValue.length() == 1)
            {
               initializer.initializeValue(Primitive.createCharacter(sValue.charAt(0)));
            }
            else
            {
               throw new MetadataException("err.meta.charValue", new Object[]{sValue});
            }
         }
      });

      s_propertyParserMap.put(Character.TYPE, new PropertyParser()
      {
         public void parse(String sValue, PropertyInitializer initializer, XMLMetadataLoader loader)
         {
            if (sValue != null && sValue.length() == 1)
            {
               initializer.initializeValue(Primitive.createCharacter(sValue.charAt(0)));
            }
            else
            {
               throw new MetadataException("err.meta.charValue", new Object[]{sValue});
            }
         }
      });

      s_propertyParserMap.put(String.class, new PropertyParser()
      {
         public void parse(String sValue, PropertyInitializer initializer, XMLMetadataLoader loader)
         {
            initializer.initializeValue(sValue);
         }
      });

      s_propertyParserMap.put(Boolean.class, new PropertyParser()
      {
         public void parse(String sValue, PropertyInitializer initializer, XMLMetadataLoader loader)
         {
            if (StringUtil.isEmpty(sValue))
            {
               initializer.initializeValue(null);
            }
            else if (sValue.equals("1") || sValue.equalsIgnoreCase("true") || sValue.equalsIgnoreCase("yes"))
            {
               initializer.initializeValue(Boolean.TRUE);
            }
            else if (sValue.equals("0") || sValue.equalsIgnoreCase("false") || sValue.equalsIgnoreCase("no"))
            {
               initializer.initializeValue(Boolean.FALSE);
            }
            else
            {
               throw new MetadataException("err.meta.booleanValue", new Object[]{sValue});
            }
         }
      });

      s_propertyParserMap.put(Boolean.TYPE, new PropertyParser()
      {
         public void parse(String sValue, PropertyInitializer initializer, XMLMetadataLoader loader)
         {
            if (StringUtil.isEmpty(sValue))
            {
               initializer.initializeValue(Boolean.FALSE);
            }
            else if (sValue.equals("1") || sValue.equalsIgnoreCase("true") || sValue.equalsIgnoreCase("yes"))
            {
               initializer.initializeValue(Boolean.TRUE);
            }
            else if (sValue.equals("0") || sValue.equalsIgnoreCase("false") || sValue.equalsIgnoreCase("no"))
            {
               initializer.initializeValue(Boolean.FALSE);
            }
            else
            {
               throw new MetadataException("err.meta.booleanValue", new Object[]{sValue});
            }
         }
      });

      s_propertyParserMap.put(java.util.Date.class, new PropertyParser()
      {
         public void parse(String sValue, PropertyInitializer initializer, XMLMetadataLoader loader)
         {
            if (StringUtil.isEmpty(sValue))
            {
               initializer.initializeValue(null);
            }
            else
            {
               ParsePosition pos = new ParsePosition(0);
               java.util.Date dt;

               synchronized (s_dateFormat)
               {
                  dt = s_dateFormat.parse(sValue, pos);
               }

               if (pos.getErrorIndex() >= 0 || pos.getIndex() < sValue.length())
               {
                  throw new MetadataException("err.meta.dateFormat", new Object[]{sValue});
               }

               initializer.initializeValue(dt);
            }
         }
      });

      s_propertyParserMap.put(Date.class, new PropertyParser()
      {
         public void parse(String sValue, PropertyInitializer initializer, XMLMetadataLoader loader)
         {
            if (StringUtil.isEmpty(sValue))
            {
               initializer.initializeValue(null);
            }
            else
            {
               ParsePosition pos = new ParsePosition(0);
               java.util.Date dt;

               synchronized (s_dateFormat)
               {
                  dt = s_dateFormat.parse(sValue, pos);
               }

               if (pos.getErrorIndex() >= 0 || pos.getIndex() < sValue.length())
               {
                  throw new MetadataException("err.meta.dateFormat", new Object[]{sValue});
               }

               initializer.initializeValue(new Date(dt.getTime()));
            }
         }
      });

      s_propertyParserMap.put(Time.class, new PropertyParser()
      {
         public void parse(String sValue, PropertyInitializer initializer, XMLMetadataLoader loader)
         {
            if (StringUtil.isEmpty(sValue))
            {
               initializer.initializeValue(null);
            }
            else
            {
               ParsePosition pos = new ParsePosition(0);
               java.util.Date dt;

               synchronized (s_timeFormat)
               {
                  dt = s_timeFormat.parse(sValue, pos);
               }

               if (pos.getErrorIndex() >= 0 || pos.getIndex() < sValue.length())
               {
                  throw new MetadataException("err.meta.timeFormat", new Object[]{sValue});
               }

               initializer.initializeValue(new Time(dt.getTime()));
            }
         }
      });

      s_propertyParserMap.put(Timestamp.class, new PropertyParser()
      {
         public void parse(String sValue, PropertyInitializer initializer, XMLMetadataLoader loader)
         {
            if (StringUtil.isEmpty(sValue))
            {
               initializer.initializeValue(null);
            }
            else
            {
               try
               {
                  initializer.initializeValue(Primitive.toTimestamp(sValue));
               }
               catch (TypeConversionException e)
               {
                  throw new MetadataException("err.meta.timestampFormat", new Object[]{sValue});
               }
            }
         }
      });

      s_propertyParserMap.put(Primitive.class, new PropertyParser()
      {
         public void parse(String sValue, PropertyInitializer initializer, XMLMetadataLoader loader)
         {
            if (StringUtil.isEmpty(sValue))
            {
               initializer.initializeValue(null);
            }
            else
            {
               initializer.initializeValue(Primitive.parse(sValue));
            }
         }
      });

      s_propertyParserMap.put(Component.class, new PropertyParser()
      {
         public void parse(final String sValue, final PropertyInitializer initializer, final XMLMetadataLoader loader)
         {
            if (StringUtil.isEmpty(sValue))
            {
               initializer.initializeValue(null);
            }
            else
            {
               loader.m_componentFixupList.add(new ContextFixup(loader.getHelper())
               {
                  public void fixup()
                  {
                     initializer.initializeValue(loader.getMetadata().getComponent(sValue));
                  }
               });
            }
         }
      });

      s_propertyParserMap.put(Metaclass.class, new PropertyParser()
      {
         public void parse(final String sValue, final PropertyInitializer initializer, final XMLMetadataLoader loader)
         {
            if (StringUtil.isEmpty(sValue))
            {
               initializer.initializeValue(null);
            }
            else
            {
               loader.m_componentFixupList.add(new ContextFixup(loader.getHelper())
               {
                  public void fixup()
                  {
                     initializer.initializeValue(loader.getMetadata().getMetaclass(sValue));
                  }
               });
            }
         }
      });

      s_propertyParserMap.put(Type.class, new PropertyParser()
      {
         public void parse(final String sValue, final PropertyInitializer initializer, final XMLMetadataLoader loader)
         {
            if (StringUtil.isEmpty(sValue))
            {
               initializer.initializeValue(null);
            }
            else
            {
               Type type = Primitive.find(sValue);

               if (type != null)
               {
                  initializer.initializeValue(type);
               }
               else
               {
                  loader.m_componentFixupList.add(new ContextFixup(loader.getHelper())
                  {
                     public void fixup()
                     {
                        initializer.initializeValue(loader.getMetadata().getMetaclass(sValue));
                     }
                  });
               }
            }
         }
      });

      s_propertyParserMap.put(Metadata.class, new PropertyParser()
      {
         public void parse(final String sValue, final PropertyInitializer initializer, final XMLMetadataLoader loader)
         {
            if (sValue != null)
            {
               throw new MetadataException("err.meta.metadataProperty");
            }

            initializer.initializeValue(loader.getMetadata());
         }
      });

      s_propertyParserMap.put(Properties.class, new PropertyParser()
      {
         public void parse(String sValue, PropertyInitializer initializer, XMLMetadataLoader loader)
         {
            try
            {
               initializer.initializeValue(PropertyUtil.fromString((sValue != null) ? sValue : ""));
            }
            catch (Exception e)
            {
               throw new MetadataException("err.meta.propertiesFormat", new Object[]{sValue});
            }
         }
      });

      s_unsupportedIntrinsicSet.add("lambda");
      s_unsupportedIntrinsicSet.add("list");
      s_unsupportedIntrinsicSet.add("pair");
      s_unsupportedIntrinsicSet.add("symbol");
      s_unsupportedIntrinsicSet.add("port");
      s_unsupportedIntrinsicSet.add("vector");
      s_unsupportedIntrinsicSet.add("bytevector");
      s_unsupportedIntrinsicSet.add("lookup");
      s_unsupportedIntrinsicSet.add("map");
      s_unsupportedIntrinsicSet.add("holder");
      s_unsupportedIntrinsicSet.add("set");
      s_unsupportedIntrinsicSet.add("environment");
      s_unsupportedIntrinsicSet.add("invocation-context");
      s_unsupportedIntrinsicSet.add("class");
      s_unsupportedIntrinsicSet.add("oid");
      s_unsupportedIntrinsicSet.add("iterator");
      s_unsupportedIntrinsicSet.add("message");
   }

   /**
    * The logger.
    */
   protected final static Logger s_logger = Logger.getLogger(XMLMetadataLoader.class);

   // operations

   /**
    * Decrypts the password.
    * @param sEncrypted The password to decrypt.
    * @return The decrypted password.
    */
   public String decryptPassword(String sEncrypted)
   {
      if (sEncrypted == null)
      {
         return null;
      }

      if (!m_metadata.isEncrypted())
      {
         // Extract the encryption scheme name
         int nColon = sEncrypted.indexOf(':');

         if (nColon >= 0)
         {
            m_helper.addEncryptionScheme(sEncrypted.substring(0, nColon));
         }

         return m_decryptionDispatcher.decrypt(sEncrypted);
      }

      return sEncrypted;
   }

   /**
    * @return External metadata loader hook.
    */
   private XMLMetadataLoaderHook getLoaderHook()
   {
      String sMetadataLoaderHook = m_properties.getProperty(XMLMetadataLoader.MEDATATA_LOAD_HOOK_PROPERTY);

      if (!StringUtil.isEmpty(sMetadataLoaderHook))
      {
         try
         {
            Class hookClass = Class.forName(sMetadataLoaderHook);

            return (XMLMetadataLoaderHook)hookClass.getConstructor(null).newInstance(null);
         }
         catch(Throwable t)
         {
            throw new MetadataException("err.meta.loadHookCreate", new Object[]{sMetadataLoaderHook}, t);
         }
      }

      return new GenericXMLMetadataLoaderHook();
   }

   /**
    * @see nexj.core.meta.MetadataLoader#load(java.lang.String, java.util.Properties, int, nexj.core.util.ProgressListener)
    */
   public Metadata load(String sURI, Properties properties, int nFlags, ProgressListener progress)
   {
      m_nStage = STAGE_START;

      if (properties == null)
      {
         properties = new Properties();
      }

      if (progress instanceof MetadataLoaderAware)
      {
         ((MetadataLoaderAware)progress).setMetadataLoader(this);
      }

      URLStreamHandler handler = null;
      String sURLHandlerName = properties.getProperty(MetadataLoader.METADATA_URL_HANDLER_PROPERTY);

      if (!StringUtil.isEmpty(sURLHandlerName))
      {
         try
         {
            Class handlerClass = Class.forName(sURLHandlerName);

            handler = (URLStreamHandler)handlerClass.getConstructor(null).newInstance(null);

            if (handler instanceof MetadataURLHandler)
            {
               MetadataURLHandler urlHandler = (MetadataURLHandler)handler;
               Metadata metadata = urlHandler.initialize(sURI, properties, progress);

               if (metadata != null)
               {
                  return metadata;
               }
            }
         }
         catch (Throwable t)
         {
            throw new MetadataException("err.meta.handlerCreate", new Object[]{sURLHandlerName}, t);
         }
      }

      URL rootURL = XMLMetadataHelper.getURL(sURI, true, handler);

      if (s_logger.isInfoEnabled())
      {
         s_logger.info("Metadata URL: \"" + rootURL + '"');
      }

      URL baseURL = null;
      String sBaseURI = properties.getProperty(BASE_URL_PROPERTY);

      if (!StringUtil.isEmpty(sBaseURI) && handler == null)
      {
         baseURL = XMLMetadataHelper.getURL(sBaseURI, true, handler);

         if (s_logger.isInfoEnabled())
         {
            s_logger.info("Base metadata URL: \"" + baseURL + '"');
         }
      }

      String sCompatibleURI = properties.getProperty(COMPATIBLE_URL_PROPERTY);

      URL configURL = null;

      try
      {
         configURL = XMLMetadataHelper.getURL(properties.getProperty(
            SysUtil.CONFIG_PROPERTY, SysUtil.DEFAULT_CONFIG_URL), false, handler);
         m_sConfigURL = configURL.toString();
      }
      catch (MetadataException e)
      {
         throw new MetadataException("err.meta.configURL",
            new Object[]{properties.getProperty(SysUtil.CONFIG_PROPERTY)}, e);
      }

      if (s_logger.isInfoEnabled())
      {
         s_logger.info("Configuration URL: \"" + m_sConfigURL + '"');
      }

      if (!StringUtil.parseBoolean(properties.getProperty(METADATA_INTEGRATION_PROPERTY, "true")))
      {
         nFlags |= INTEGRATION_EXCLUDED;
      }

      if (StringUtil.parseBoolean(properties.getProperty(METADATA_DOCUMENTATION_PROPERTY, "false")))
      {
         nFlags |= DOCUMENTATION_INCLUDED;
      }

      if (StringUtil.parseBoolean(properties.getProperty(METADATA_WARNING_PROPERTY, "false")))
      {
         nFlags |= WARNING;
      }

      boolean bDataSourceOnly = (nFlags & DATASOURCE_ONLY) != 0;
      boolean bDumpValidationDisabled = (nFlags & DUMP_VALIDATION_DISABLED) != 0;

      m_properties = ((nFlags & PROPERTIES) != 0) ? properties : new Properties(properties);

      // Initialize the member variables for loading

      m_bEnvironmentOnly = ((nFlags & ENVIRONMENT_ONLY) != 0);
      m_bRuntimeExcluded = ((nFlags & RUNTIME_EXCLUDED) != 0);
      m_bIntegrationExcluded = ((nFlags & INTEGRATION_EXCLUDED) != 0);
      m_bValidatedOnly = ((nFlags & VALIDATED_ONLY) != 0);
      m_bDocumented = ((nFlags & DOCUMENTATION_INCLUDED) != 0);
      m_bWritable = ((nFlags & WRITABLE) != 0);
      m_helper = new XMLMetadataHelper(null, rootURL, baseURL, m_properties, null, handler);
      m_metadata = createXMLMetadata(sURI, rootURL, baseURL, (m_bValidatedOnly) ? m_helper : null);
      m_metadata.setConfigurationURL(configURL);
      m_machine = new Machine(m_metadata.getGlobalEnvironment(),
         new InvocationContext(m_metadata, m_metadata.getGlobalEnvironment()));
      m_enumerationValueMap = new HashTab(16);
      m_inheritanceCheckList = new ArrayList();
      m_inheritanceFixupList = new ArrayList();
      m_inheritance1stPassFixupList = new ArrayList();
      m_inheritance2ndPassFixupList = new ArrayList(1);
      m_inheritance4thPassFixupList = new ArrayList(1);
      m_attributeGenerationList = new ArrayList();
      m_attributeFixupList = new ArrayList();
      m_attributeResolutionList = new ArrayList(1);
      m_actionFixupList = new ArrayList();
      m_persistenceMappingLoadList = new ArrayList();
      m_persistenceMappingFixupList = new ArrayList();
      m_persistenceMappingGenerationList = new ArrayList();
      m_attributeDependencyList = new ArrayList(1);
      m_attributeDependencySet = new HashHolder();
      m_class2ndPassList = new ArrayList(1);
      m_ioFixupList = new ArrayList();
      m_environmentFixupList = new ArrayList();
      m_privilegeFixupList = new ArrayList();
      m_preInheritanceMessageFixupList = new ArrayList();
      m_postInheritanceMessageFixupList = new ArrayList();
      m_transformationFixupList = new ArrayList();
      m_postInheritanceTransformationFixupList = new ArrayList();
      m_componentFixupList = new ArrayList();
      m_singletonFixupList = new ArrayList();
      m_tmpDocument = XMLUtil.parse(new StringReader("<TmpDocument/>"));

      m_metadata.setEncrypted((nFlags & ENCRYPTED) != 0);
      m_metadata.setEnvironmentOnly(m_bEnvironmentOnly);

      m_loadHook = getLoaderHook();
      m_loadHook.initialize(this, properties);

      if ((nFlags & WARNING) != 0)
      {
         m_helper.setWarnings(new GenericException(null)
         {
            private final static long serialVersionUID = -2984934222963813773L;

            public void addException(Throwable e)
            {
               e.setStackTrace(ObjUtil.EMPTY_STACK_TRACE);
               super.addException(e);
            }
         });
      }

      // Prepare the progress listener
      ProgressProxy progressProxy = new ProgressProxy(progress);

      if (progress != null)
      {
         progress = progressProxy;
      }

      // Load the repository descriptors and process the elements in them

      if (progress != null)
      {
         progress.progress("info.meta.loadingDescriptors", null, 0);
      }

      Element baseDescElement = m_helper.getDescriptorElement(false);
      Element rootDescElement = m_helper.getDescriptorElement(true);

      progressProxy.setRange(0.01, 0.02);

      m_metadata.setListing(m_helper.getListing(new XMLMetadataHelper.MixinHandler()
         {
            /**
             * Metadata repository information, indexed by namespace.
             */
            private Lookup m_metadataMap = new HashTab();

            private String getMetadataName(String sNamespace)
            {
               MetadataInfo info = ((MetadataInfo)m_metadataMap.get(sNamespace));

               return (info == null || info.metadata == null) ? sNamespace : info.metadata.getName();
            }

            public void handleRepository(XMLMixin mixin)
            {
               String sNamespace = mixin.getNamespace();
               XMLMetadataHelper helper = mixin.getHelper();
               MetadataInfo info = (MetadataInfo)m_metadataMap.get(sNamespace);

               if (info == null)
               {
                  info = new MetadataInfo();
               }

               // get the metadata instance
               if (m_metadataMap.size() == 0)
               {
                  info.metadata = m_metadata;
               }
               else
               {
                  URL rootURL = helper.getRootURL();

                  info.metadata = createXMLMetadata(rootURL.getPath(), rootURL, helper.getBaseURL(), helper);
               }

               // load the version
               loadVersion(info.metadata, helper);
               m_metadataMap.put(sNamespace, info);

               // validate loaded mixin
               if (m_metadata != info.metadata)
               {
                  // Adopt the highest coreVersion available from any of the repositories.
                  if (m_metadata.getCoreVersion() == null || (info.metadata.getCoreVersion() != null &&
                     StringUtil.compareVersionRanges(m_metadata.getCoreVersion(), info.metadata.getCoreVersion()) < 0))
                  {
                     m_metadata.setCoreVersion(info.metadata.getCoreVersion());
                  }

                  if (info.ref == null)
                  {
                     throw new MetadataValidationException("err.meta.mixinNamespace", new Object[]{info.metadata.getName(), sNamespace});
                  }

                  String sVersion = info.ref.getVersion();
                  String sChecksum = info.ref.getChecksum();

                  // validate that repository matches mixin declaration
                  if ((sVersion != null && !sVersion.equals(info.metadata.getVersion()))
                     || (sChecksum != null && !sChecksum.equals(info.metadata.getChecksum())))
                  {
                     throw new MetadataValidationException("err.meta.mixinLinkVersion", new Object[]{
                        info.metadata.getName(),
                        info.metadata.getVersion(),
                        info.metadata.getChecksum(),
                        sVersion,
                        sChecksum,
                        getMetadataName(info.parentNamespace)
                     });
                  }
               }
            }

            public void handleMixinReference(XMLMixin ref, XMLMixin parent)
            {
               String sParentNamespace = parent.getNamespace();
               String sNamespace = ref.getNamespace();
               String sVersion = ref.getVersion();
               String sChecksum = ref.getChecksum();
               MetadataInfo info = (MetadataInfo)m_metadataMap.get(sNamespace);

               if (info == null)
               {
                  info = new MetadataInfo();
                  info.parentNamespace = sParentNamespace;
                  info.ref = (XMLMixin)ref.clone();
                  m_metadataMap.put(sNamespace, info);

                  // mixin declared at top-level is also base
                  if (sParentNamespace.equals(m_metadata.getNamespace()) && sNamespace.equals(m_metadata.getBaseNamespace()))
                  {
                     m_helper.addException(new MetadataValidationException("err.meta.mixinDup",
                        new Object[] {sNamespace, getMetadataName(sParentNamespace)}));
                  }

                  // mixin matches base, but has different version/checksum
                  if (sNamespace.equals(m_metadata.getBaseNamespace()) &&
                     (!sVersion.equals(m_metadata.getBaseVersion()) || !sChecksum.equals(m_metadata.getBaseChecksum())))
                  {
                     m_helper.addException(new MetadataValidationException("err.meta.mixinBaseVersion",
                        new Object[]{sNamespace, sVersion, sChecksum, m_metadata.getBaseVersion(), m_metadata.getBaseChecksum()}));
                  }
               }
               else if (info.ref != null)
               {
                  if (sParentNamespace.equals(info.parentNamespace))
                  {
                     // mixin declared twice in same repository
                     m_helper.addException(new MetadataValidationException("err.meta.mixinDup",
                        new Object[] {sNamespace, getMetadataName(sParentNamespace)}));
                  }

                  // mixin declared more than once with different version/checksum
                  if ((info.ref.getVersion() != null && !info.ref.getVersion().equals(sVersion)) ||
                     (info.ref.getChecksum() != null && !info.ref.getChecksum().equals(sChecksum)))
                  {
                     m_helper.addException(new MetadataValidationException("err.meta.mixinVersion", new Object[] {
                        sNamespace,
                        sVersion,
                        sChecksum,
                        getMetadataName(sParentNamespace),
                        info.ref.getVersion(),
                        info.ref.getChecksum(),
                        getMetadataName(info.parentNamespace)
                     }));
                  }
               }
            }

            public void handleMixinOverrideConflict(XMLMixin ref, XMLMixin mixin)
            {
               throw new MetadataValidationException("err.meta.mixinOverride", new Object[]{ref.getName(), mixin.getName()});
            }

            public void handleCircularReference(XMLMixin mixin)
            {
               throw new MetadataValidationException("err.meta.mixinCycle", new Object[]{mixin.getName()});
            }

            public void handleResourceConflict(XMLResource first, XMLResource second)
            {
               m_helper.addException(new MetadataValidationException("err.meta.mixinConflict",
                  new Object[]{
                     first.getName(),
                     getMetadataName(first.getNamespace()),
                     getMetadataName(second.getNamespace())
                  }));
            }

            public void handleLinkFailure(XMLMixin ref, Exception e)
            {
               if (e instanceof MetadataValidationException)
               {
                  m_helper.addException((MetadataValidationException)e);
               }
               else
               {
                  throw new MetadataException("err.meta.mixin", new Object[]{ref}, e);
               }
            }

            public void handleAlternateResource(XMLResource first, XMLResource second)
            {
            }

            public void handleResourceOverride(XMLResource source, XMLResource overridden)
            {
            }

            final class MetadataInfo
            {
               /**
                * The metadata instance of the repository.
                */
               private XMLMetadata metadata;

               /**
                * The namespace of the repository that linked this repository.
                */
               private String parentNamespace;

               /**
                * The reference that caused this repository to be linked.
                */
               private XMLMixin ref;
            }
         }));

      XMLMetadataUpgrader upgrader = new XMLMetadataUpgrader(m_metadata.getCoreVersion(), m_metadata.getUpgradeResources());

      m_helper.setUpgrader(upgrader); // set upgrader for immediate loading
      m_metadata.setUpgrader(upgrader); // set upgrader for lazy loading

      progressProxy.shiftRange(0.05);

      if (!m_bEnvironmentOnly && !bDataSourceOnly)
      {
         Lookup localeMap = new HashTab();
         m_helper.addResources(localeMap, ".locale", "Locale", "locale");
         loadStrings(localeMap.iterator());

         Lookup stringResMap = new HashTab();
         m_helper.addResources(stringResMap, ".strings", "Strings", "strings");
         loadStrings(stringResMap.iterator());

         if (StringTable.getInstance() == null)
         {
            StringTable.setInstance(m_metadata.getStringTable(Metadata.DEFAULT_LOCALE));
            StringTable.setTimeZone(TZ.UTC);
         }
      }

      progressProxy.shiftRange(0.10);

      m_helper.loadEncryptedResourceURL(configURL, "Server", "server", new ResourceHandler()
      {
         public void handleResource(final Element element, final String sName)
         {
            if (element.getNodeName().equals("Environment"))
            {
               String sConnectionsURL = m_properties.getProperty(CONNECTIONS_URL_PROPERTY);

               if (StringUtil.isEmpty(sConnectionsURL))
               {
                  m_properties.setProperty(CONNECTIONS_URL_PROPERTY, m_sConfigURL);
               }
            }

            NamedNodeMap attrMap = element.getAttributes();

            for (int i = 0; i != attrMap.getLength(); ++i)
            {
               Node node = attrMap.item(i);
               String sOldValue = m_properties.getProperty(node.getNodeName());

               if (StringUtil.isEmpty(sOldValue))
               {
                  m_properties.setProperty(node.getNodeName(), node.getNodeValue());
               }
            }
         }
      }, m_properties);

      m_decryptionDispatcher = new CharacterStreamCipherDispatcher();
      m_decryptionDispatcher.init(m_properties);
      m_helper.setEncryptionSchemeSet(new HashHolder(4));

      String sEnvName = m_properties.getProperty(ENVIRONMENT_NAME_PROPERTY_NAME);
      
      m_metadata.setEnvironment((sEnvName != null) ? sEnvName :
         XMLMetadataHelper.getEnvironmentName(m_metadata.getNamespace()));

      loadServer(m_properties);

      String sConnectionsURL = m_properties.getProperty(CONNECTIONS_URL_PROPERTY);
      URL connectionsURL = null;

      if (!StringUtil.isEmpty(sConnectionsURL))
      {
         connectionsURL = XMLMetadataHelper.getURL(sConnectionsURL, false, handler);
         sConnectionsURL = connectionsURL.toString();

         if (s_logger.isInfoEnabled())
         {
            s_logger.info("Connections URL: \"" + sConnectionsURL + "\"");
         }
      }

      m_helper.loadSystemResource("system.dstypes",
         "DataSourceTypes", "dataSourceTypes", new ResourceHandler()
      {
         public void handleResource(Element element, String sName)
         {
            loadDataSourceTypes(element);
         }
      });

      m_helper.loadResources(".dstype", "DataSourceType", "dataSourceType", new ResourceHandler()
      {
         public void handleResource(Element element, String sName)
         {
            loadDataSourceType(element, sName);
         }
      }, progress);

      m_helper.loadResources(".datasource", "DataSource", "dataSource", new ResourceHandler()
      {
         public void handleResource(Element element, String sName)
         {
            loadDataSource(element, sName);
         }
      }, progress);

      if (!m_bIntegrationExcluded)
      {
         m_helper.loadSystemResource("system.chtypes",
            "ChannelTypes", "channelTypes", new ResourceHandler()
         {
            public void handleResource(Element element, String sName)
            {
               loadChannelTypes(element);
            }
         });

         m_helper.loadResources(".chtype", "ChannelType", "channelType", new ResourceHandler()
         {
            public void handleResource(Element element, String sName)
            {
               loadChannelType(element, sName);
            }
         }, progress);

         // Load the SOA definition XML metadata (which may include definitions of other XML metadata)
         m_soaLoader = new XMLSOAMetadataLoader(m_helper);
         m_helper.loadResources(".soadef", "SOADefinition", "soadef", new ResourceHandler()
         {
            public void handleResource(Element element, String sName)
            {
               m_soaLoader.loadDefinition(element, sName);
            }
         }, progress);
         m_soaLoader.resolveReferences();
         m_helper.loadResources(".soaimpl", "SOAImplementation", "soaimpl", new ResourceHandler()
         {
            public void handleResource(Element element, String sName)
            {
               m_soaLoader.loadImplementation(element, sName, m_metadata.getGlobalEnvironment());
            }
         }, progress);

         // dispatcher channel properties loaded from environment.
         ChannelType type = m_metadata.getChannelType("ObjectQueue");

         m_dispatcherChannel.setType(type);
         m_metadata.addChannel(m_dispatcherChannel);

         m_helper.loadResources(".channel", "Channel", "channel", new ResourceHandler()
         {
            public void handleResource(Element element, String sName)
            {
               loadChannel(element, sName);
            }
         }, progress);
      }

      final Set componentNameSet = new HashHolder();

      m_helper.loadResources(".comp", "Component", "component", new ResourceHandler()
      {
         public void handleResource(Element element, String sName)
         {
            if (m_bEnvironmentOnly)
            {
               XMLMetadataHelper.verifyRootElement(element, "Component");

               m_metadata.addComponent(new Component(sName));
            }

            componentNameSet.add(sName);
         }
      }, progress);

      if (componentNameSet.contains("System.ObjectQueueDispatcher") &&
         Boolean.parseBoolean(properties.getProperty("queueing.enabled", "true")))
      {
      // only enable the dispatcher if the metadata supports it.
         m_dispatcherChannel.setSendable(true);
         m_dispatcherChannel.setReceivable(true);
      }

      if (connectionsURL != null)
      {
         boolean bEnvironment = (sConnectionsURL.equals(m_sConfigURL) ||
            sConnectionsURL.toLowerCase(Locale.ENGLISH).endsWith(".environment"));

         m_helper.loadEncryptedResourceURL(connectionsURL,
            (bEnvironment) ? "Environment" : "Connections",
            (bEnvironment) ? "environment" : "connections",
            new ResourceHandler()
         {
            public void handleResource(final Element element, final String sName)
            {
               loadConnections(element, sName);
            }
         }, m_properties);
      }
      else
      {
         loadConnections(null, null);
      }

      // .server file overrides highest scheme used in the connections file
      m_metadata.setEncryptionScheme(m_properties.getProperty("cipher.scheme",
            CharacterStreamCipherDispatcher.computeMostSecure(m_helper.getEncryptionSchemeSet())));

      if (!bDataSourceOnly)
      {
         loadUIMetadata(baseDescElement, rootDescElement);
      }

      progressProxy.shiftRange(0.12);

      m_helper.fixup(m_ioFixupList.iterator());
      m_ioFixupList = null;

      m_helper.loadResources(".extlib", "ExternalLibrary", "externalLibrary", new ResourceHandler()
      {
         public void handleResource(Element element, String sName)
         {
            loadExternalLibrary(element, sName);
         }
      }, progress);

      m_nStage = STAGE_LOADED_ENVIRONMENT;
      progressProxy.progress(null, null, 1.0);

      if (m_bEnvironmentOnly || bDataSourceOnly)
      {
         m_nStage = STAGE_LOAD_ENVIRONMENT_FIXUPS;
         m_helper.fixup(m_environmentFixupList.iterator()); // safe to run since no J2EE container
         m_nStage = STAGE_FINISHED;

         return m_metadata;
      }

      progressProxy.shiftRange(0.13);

      m_helper.loadResources(".security", "SecurityDescriptor", "securityDescriptor", new ResourceHandler()
      {
         public void handleResource(Element element, String sName)
         {
            loadSecurityDescriptor(element);
         }
      }, progress);

      m_helper.fixup(m_privilegeFixupList.iterator());
      m_privilegeFixupList = null;
      PrivilegeGroup.resolve(m_metadata);

      if (progress != null)
      {
         progressProxy.shiftRange(0.15);
         progress.progress("info.meta.loadingSystemResources", null, 0);
      }

      Context contextSaved = ThreadContextHolder.getContext();

      try
      {
         ThreadContextHolder.setContext(m_machine.getContext());

         m_helper.loadSystemResource("scheme.scm",
            "Library", "library", new CharacterStreamHandler()
         {
            public void handleCharacterStream(Reader reader, String sName) throws IOException
            {
               loadLibrary(reader, sName, true);
            }
         });

         // Load the Dynamic Object System
         m_helper.loadSystemResource("object.scm",
            "Library", "library", new CharacterStreamHandler()
         {
            public void handleCharacterStream(Reader reader, String sName) throws IOException
            {
               loadLibrary(reader, sName, true);
            }
         });

         // Load the Services Oriented Architecture functionality
         m_helper.loadSystemResource("soa.scm",
            "Library", "library", new CharacterStreamHandler()
         {
            public void handleCharacterStream(Reader reader, String sName) throws IOException
            {
               loadLibrary(reader, sName, true);
            }
         });

         // Loading of *.scm and *.meta must happen later, so that libraries and classes can consume SOA
         if (m_soaLoader != null)
         {
            m_soaLoader.initDefinitions(m_machine);
         }

         m_helper.loadSystemResource("server.scm",
            "Library", "library", new CharacterStreamHandler()
         {
            public void handleCharacterStream(Reader reader, String sName) throws IOException
            {
               loadLibrary(reader, sName, true);
            }
         });

         m_helper.loadSystemResource(Metadata.ROOT_CLASS_NAME + ".meta",
            "Class", "class", new ResourceHandler()
         {
            public void handleResource(Element rootElement, String sName)
            {
               loadClass(rootElement, sName);
            }
         });

         loadExtendedSystemMetadata();

         progressProxy.shiftRange(0.16);

         m_helper.loadResources(".scm", "Library", "library", new CharacterStreamHandler()
         {
            public void handleCharacterStream(Reader reader, String sName) throws IOException
            {
               loadLibrary(reader, sName, false);
            }
         }, progress);
      }
      finally
      {
         ThreadContextHolder.setContext(contextSaved);
      }

      progressProxy.shiftRange(0.23);

      m_helper.loadResources(".meta", "Class", "class", new ResourceHandler()
      {
         public void handleResource(Element element, String sName)
         {
            String sElement = element.getNodeName();

            if (sElement.equals("Class"))
            {
               loadClass(element, sName);
            }
            else if (sElement.equals("Aspect"))
            {
               loadClassAspect(element, sName);
            }
            else
            {
               throw new MetadataException("err.meta.metaDocRoot", new Object[]{sElement});
            }
         }
      }, progress);

      progressProxy.shiftRange(0.24);

      m_helper.loadResources(".enum", "Enumeration", "enum", new ResourceHandler()
      {
         public void handleResource(Element element, String sName)
         {
            loadEnumeration(element, sName);
         }
      }, progress);

      progressProxy.shiftRange(0.25);

      loadUIEventMetadata(baseDescElement, rootDescElement, progress);

      if (progress != null)
      {
         progressProxy.shiftRange(0.26);
         progress.progress("info.meta.resolvingResourceReferences", null, 0);
      }

      m_helper.fixup(m_inheritanceCheckList.iterator());
      m_inheritanceCheckList = null;
      m_helper.fixup(m_attributeGenerationList.iterator());
      m_attributeGenerationList = null;

      m_helper.fixup(Collections.singleton(new ClassFixup()
      {
         public void fixup()
         {
            m_classAspectArray = ClassAspect.resolveAspects(m_metadata);
         }
      }).iterator());

      m_helper.fixup(m_inheritanceFixupList.iterator());
      m_inheritanceFixupList = null;
      m_enumerationValueMap = null;
      m_helper.fixup(m_attributeFixupList.iterator());
      m_attributeFixupList = null;
      m_helper.fixup(m_actionFixupList.iterator());
      m_actionFixupList = null;

      if (m_classAspectArray != null)
      {
         ClassAspect.resolveMembers(m_classAspectArray);
      }

      m_helper.fixup(m_persistenceMappingLoadList.iterator());
      m_persistenceMappingLoadList = null;
      m_helper.fixup(m_attributeResolutionList.iterator());
      m_attributeResolutionList = null;
      m_helper.fixup(m_inheritance1stPassFixupList.iterator());
      m_inheritance1stPassFixupList = null;

      progressProxy.shiftRange(0.27);

      if (m_soaLoader != null)
      {
         contextSaved = ThreadContextHolder.getContext();

         try
         {
            ThreadContextHolder.setContext(m_machine.getContext());

            // Loading must happen after *.scm and *.meta are loaded so that they can be used by SOA implementations
            m_soaLoader.initImplementations(m_machine);
         }
         finally
         {
            ThreadContextHolder.setContext(contextSaved);
         }
      }

      m_helper.loadResources(".action", "Action", "action", new ResourceHandler()
      {
         public void handleResource(Element element, String sName)
         {
            loadFlowMacro(element, sName);
         }
      }, progress);

      progressProxy.shiftRange(0.32);

      m_helper.loadResources(".workflow", "Workflow", "workflow", new ResourceHandler()
      {
         public void handleResource(Element element, String sName)
         {
            loadWorkflow(element, sName);
         }
      }, progress);

      progressProxy.shiftRange(0.33);

      if (!m_bIntegrationExcluded)
      {
         m_helper.loadSystemResource("system.formats",
            "Formats", "formats", new ResourceHandler()
         {
            public void handleResource(Element element, String sName)
            {
               loadFormats(element);
            }
         });

         m_helper.loadResources(".format", "Format", "format", new ResourceHandler()
         {
            public void handleResource(Element element, String sName)
            {
               loadFormat(element, sName);
            }
         }, progress);

         progressProxy.shiftRange(0.34);

         final Lookup importedMessageMap = new HashTab();

         m_helper.loadResources(".xsd", "XSD", "xsd", new ResourceNameHandler()
         {
            public void handleResource(String sName, String sRelativePath)
            {
               loadXSD(m_helper.getResource(sRelativePath).getURL(), sName, importedMessageMap);
            }
         }, progress);

         m_helper.loadResources(".wsdl", "WSDL", "wsdl", new ResourceNameHandler()
         {
            public void handleResource(String sName, String sRelativePath)
            {
               loadXSD(m_helper.getResource(sRelativePath).getURL(), sName, importedMessageMap);
            }
         }, progress);

         m_helper.loadResources(".message", "Message", "message", new ResourceHandler()
         {
            public void handleResource(Element element, String sName)
            {
               loadMessage(element, sName);
            }
         }, progress);

         // Explicitly-defined Message metadata shall override Message metadata from imported XSDs and WSDLs
         for (Lookup.Iterator itr = importedMessageMap.iterator(); itr.hasNext(); )
         {
            String sName = (String)itr.next();

            if (m_metadata.findMessage(sName) == null)
            {
               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Adding imported message \"" + sName + "\"");
               }

               m_metadata.addMessage((Message)itr.getValue());
            }
            else
            {
               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Skipping the overridden imported message \"" + sName + "\"");
               }
            }
         }

         // Resolve Inheritance
         m_helper.fixup(m_preInheritanceMessageFixupList.iterator());
         m_preInheritanceMessageFixupList = null;

         List sortedMessageList = new ArrayList(m_metadata.getMessageCount());

         Message.resolveInheritance(m_metadata.getMessageIterator(), sortedMessageList);

         m_helper.fixup(m_postInheritanceMessageFixupList.iterator());
         m_postInheritanceMessageFixupList = null;

         Message.resolveReferences(m_metadata.getMessageIterator());

         progressProxy.shiftRange(0.36);

         m_helper.loadResources(".transformation", "Transformation", "transformation", new ResourceHandler()
         {
            public void handleResource(Element element, String sName)
            {
               loadTransformation(element, sName);
            }
         }, progress);

         m_helper.fixup(m_transformationFixupList.iterator());
         m_transformationFixupList = null;

         Transformation.resolveInheritance(m_metadata.getTransformationIterator());

         m_helper.fixup(m_postInheritanceTransformationFixupList.iterator());
         m_postInheritanceTransformationFixupList = null;

         for (Iterator itr = m_metadata.getTransformationIterator(); itr.hasNext(); )
         {
            Transformation transformation = (Transformation)itr.next();

            try
            {
               transformation.finish(m_machine);
            }
            catch (UncheckedException ex)
            {
               m_helper.addException(ex);
            }
         }

         m_helper.checkForError();

         progressProxy.shiftRange(0.37);

         m_helper.loadResources(".interface", "Interface", "interface", new ResourceHandler()
         {
            public void handleResource(Element element, String sName)
            {
               loadInterface(element, sName);
            }
         }, progress);

         progressProxy.shiftRange(0.38);

         m_helper.loadResources(".service", "Service", "service", new ResourceHandler()
         {
            public void handleResource(Element element, String sName)
            {
               loadService(element, sName);
            }
         }, progress);

         finishMessagePartMappings(sortedMessageList.iterator());
      }

      if (progress != null)
      {
         progressProxy.shiftRange(0.39);
         progress.progress("info.meta.resolvingResourceReferences", null, 0);
      }

      m_helper.fixup(m_persistenceMappingFixupList.iterator());
      m_persistenceMappingFixupList = null;
      m_helper.fixup(m_persistenceMappingGenerationList.iterator());
      m_persistenceMappingGenerationList = null;
      m_helper.fixup(m_attributeDependencyList.iterator());
      m_attributeDependencyList = null;
      Attribute.resolveInverseDependency(m_attributeDependencySet.iterator());
      m_attributeDependencySet = null;

      if (m_classAspectArray != null)
      {
         m_helper.fixup(Collections.singleton(new ClassFixup()
         {
            public void fixup()
            {
               ClassAspect.resolvePersistence(m_classAspectArray);
            }
         }).iterator());
      }

      m_helper.fixup(m_inheritance2ndPassFixupList.iterator());
      m_inheritance2ndPassFixupList = null;
      m_helper.fixup(m_class2ndPassList.iterator());
      m_class2ndPassList = null;

      loadLocales();
      progressProxy.shiftRange(0.40);

      m_helper.loadResources(".comp", "Component", "component", new ResourceHandler()
      {
         public void handleResource(Element element, String sName)
         {
            loadComponent(element, sName);
         }
      }, progress);

      m_nStage = STAGE_LOADED_COMPONENTS;
      progressProxy.progress(null, null, 0);

      m_nStage = STAGE_LOAD_ENVIRONMENT_FIXUPS;
      progressProxy.shiftRange(0.405);

      m_helper.fixup(m_environmentFixupList.iterator());

      if (progress != null)
      {
         progressProxy.shiftRange(0.41);
         progress.progress("info.meta.instantiatingComponents", null, 0);
      }

      m_helper.fixup(m_componentFixupList.iterator());
      m_componentFixupList = null;
      m_helper.fixup(m_inheritance4thPassFixupList.iterator());
      m_inheritance4thPassFixupList = null;
      m_helper.fixup(m_singletonFixupList.iterator());
      m_singletonFixupList = null;

      m_helper.addResources(m_metadata.getDumpResourceMap(), ".dump", "Dump", "dump");
      m_helper.addResources(m_metadata.getUnitTestResourceMap(), ".utest", "UnitTest", "unittest");
      m_helper.addResources(m_metadata.getUpgradeResourceMap(), ".upgrade", "Upgrade", "upgrade");

      loadExtendedMetadata(baseDescElement, rootDescElement);

      if (m_bValidatedOnly)
      {
         progressProxy.setRange(0.42, 0.96);

         m_helper.validateResources(m_metadata.getUnitTestResourceMap(), "UnitTest", "unittest", new ResourceNameHandler()
         {
            public void handleResource(String sName, String sFullName)
            {
               m_metadata.getUnitTest(sName);
            }
         }, null);

         progressProxy.shiftRange(0.98);

         m_helper.validateResources(m_metadata.getUpgradeResourceMap(), "Upgrade", "upgrade", new ResourceNameHandler()
         {
            public void handleResource(String sName, String sFullName)
            {
               if (m_helper.findResource(sFullName, XMLMetadataHelper.SEARCH_ROOT_THEN_BASE) != null)
               {
                  m_metadata.getUpgrade(sName).validate(m_metadata, m_helper.getWarnings());
               }
            }
         }, null);

         validateExtendedMetadata(progressProxy);
      }

      try
      {
         m_metadata.validate(m_metadata, m_helper.getWarnings());

         if (!StringUtil.isEmpty(sCompatibleURI))
         {
            progressProxy.shiftRange(0.988);
            progressProxy.progress("info.meta.loadCompatibleModel", null, 0);

            Properties compatibleProperties = new Properties(properties);

            compatibleProperties.setProperty(BASE_URL_PROPERTY, "");
            compatibleProperties.setProperty(COMPATIBLE_URL_PROPERTY, "");

            Metadata compatible;

            try
            {
               compatible = new MetadataLoaderDispatcher().load(sCompatibleURI, compatibleProperties, nFlags, null);
            }
            catch (UncheckedException e)
            {
               throw new MetadataCompatibilityException("err.meta.compatibleMetadataLoadFailure", null, e);
            }

            progressProxy.progress("info.meta.verifyModelCompatibility", null, 1);
            m_metadata.checkCompatibility(compatible);
         }
      }
      catch (UncheckedException e)
      {
         m_helper.addException(e);
      }

      if (m_bValidatedOnly && !bDumpValidationDisabled)
      {
         progressProxy.shiftRange(.989);

         m_helper.validateResources(m_metadata.getDumpResourceMap(), "Dump", "dump", new ResourceNameHandler()
         {
            public void handleResource(String sName, String sFullName)
            {
               if (m_helper.findResource(sFullName, XMLMetadataHelper.SEARCH_ROOT_THEN_BASE) == null)
               {
                  return;
               }

               InputStream is = null;
               Pair dumpVersion = null;

               try
               {
                  dumpVersion = new DataLoader(null).getDumpVersion(is = m_helper.getResourceAsStream(sFullName));
               }
               catch (Exception e)
               {
                  throw new MetadataException("err.meta.dumpLoad", new Object[]
                  {
                     sName
                  }, e);
               }
               finally
               {
                  IOUtil.close(is);
               }

               if (dumpVersion != null
                  && (!m_metadata.getNamespace().equals(dumpVersion.getHead()) || !m_metadata.getVersion().equals(
                     dumpVersion.getTail())))
               {
                  throw new MetadataException("err.meta.dumpVersion", new Object[]
                  {
                     dumpVersion.getHead(),
                     dumpVersion.getTail(),
                     m_metadata.getNamespace(),
                     m_metadata.getVersion()
                  });
               }
            }
         }, null);
      }

      if (!m_bWritable)
      {
         m_metadata.makeReadOnly();
      }

      m_helper.checkForError();

      if (progress != null)
      {
         progressProxy.shiftRange(0.99);
         progress.progress("info.meta.validatingResourceReferentialIntegrity", null, 0);
      }

      if (m_helper.getWarnings() != null)
      {
         if (s_logger.isWarnEnabled())
         {
            for (Iterator itr = m_helper.getWarnings().getExceptionIterator(); itr.hasNext();)
            {
               s_logger.warn(ObjUtil.getMessage((Throwable)itr.next()));
            }
         }
      }

      m_nStage = STAGE_FINISHED;

      return m_metadata;
   }

   /**
    * Load environment descriptors.
    * @param baseDescElement Base repository descriptor element.
    * @param rootDescElement Repository descriptor element.
    * @param progress A progress listener.
    */
   protected void loadUIMetadata(Element baseDescElement, Element rootDescElement)
   {
   }

   /**
    * Create the Metadata instance.
    * @param sURI The repository name.
    * @param rootURL The repository URL.
    * @param baseURL The base repository URL.
    * @param helper The XMLMetadataHelper. May be null.
    * @return an XMLMetadata instance.
    */
   public XMLMetadata createXMLMetadata(String sURI, URL rootURL, URL baseURL, XMLMetadataHelper helper)
   {
       return new XMLMetadata(sURI, rootURL, baseURL, m_properties, helper);
   }

   /**
    * Gets the current metadata loading stage.
    * @return The loading stage, one of the STAGE_* constants.
    */
   public byte getStage()
   {
      return m_nStage;
   }

   /**
    * @return The environment-only flag.
    */
   public boolean isEnvironmentOnly()
   {
      return m_bEnvironmentOnly;
   }

   /**
    * @return The runtime exclusion flag.
    */
   public boolean isRuntimeExcluded()
   {
      return m_bRuntimeExcluded;
   }

   /**
    * @return The validation-only flag.
    */
   public boolean isValidatedOnly()
   {
      return m_bValidatedOnly;
   }

   /**
    * @return The flag for leaving the metadata writable.
    */
   public boolean isWritable()
   {
      return m_bWritable;
   }

   /**
    * @return The environment container, one of the J2EEUtil constants.
    */
   public int getContainer()
   {
      return m_nContainer;
   }

   /**
    * Gets an environment property.
    * @param sName The property name.
    * @return The property value, or null if not specified.
    */
   public String getProperty(String sName)
   {
      if (m_properties == null)
      {
         return null;
      }

      String sValue = m_properties.getProperty(sName);

      if (sValue != null && sValue.length() == 0)
      {
         sValue = null;
      }

      return sValue;
   }

   /**
    * Gets an environment property.
    * @param sName The property name.
    * @param lDefaultValue The default value, if not specified.
    * @return The property value.
    */
   public long getProperty(String sName, long lDefaultValue)
   {
      Long value = Primitive.toLong(getProperty(sName));

      return (value == null) ? lDefaultValue : value.longValue();
   }

   /**
    * Loads a descriptor version.
    * @param metadata The metadata for which to load the version.
    * @param helper The XMLMetadataHelper for metadata.
    */
   protected void loadVersion(XMLMetadata metadata, XMLMetadataHelper helper)
   {
      Element descriptorElement = helper.getDescriptorElement(true);
      Element baseDescriptorElement = helper.getDescriptorElement(false);
      int nCookie = getHelper().pushMarker(MetadataValidationException.TYPE_NAME, "Metadata");

      try
      {
         metadata.setName(helper.getName(true));
         metadata.setRevision(helper.getRevision(true));
         metadata.setNamespace(helper.getNamespace(true));
         metadata.setVersion(helper.getVersion(true));
         metadata.setCoreVersion(XMLUtil.getStringAttr(descriptorElement, "coreVersion"));
         metadata.setChecksum(helper.getChecksum(true));

         String sUpgradeNamespace = XMLUtil.getStringAttr(descriptorElement, "upgradeNamespace");

         if (sUpgradeNamespace != null)
         {
            metadata.setBaseNamespace(sUpgradeNamespace);
            metadata.setBaseVersion(XMLUtil.getStringAttr(descriptorElement, "upgradeVersion"));
            metadata.setBaseChecksum(XMLUtil.getStringAttr(descriptorElement, "upgradeChecksum"));
         }
         else
         {
            metadata.setBaseNamespace(XMLUtil.getStringAttr(descriptorElement, "baseNamespace"));
            metadata.setBaseVersion(XMLUtil.getStringAttr(descriptorElement, "baseVersion"));
            metadata.setBaseChecksum(XMLUtil.getStringAttr(descriptorElement, "baseChecksum"));
         }

         if (s_logger.isInfoEnabled())
         {
            s_logger.info("Metadata URI: \"" + metadata.getNamespace() + '#' +
               metadata.getVersion() + '-' + metadata.getChecksum() + '"');

            if (metadata.getBaseNamespace() != null)
            {
               s_logger.info("Base metadata URI: \"" + metadata.getBaseNamespace() + '#' +
                  metadata.getBaseVersion() + '-' + metadata.getBaseChecksum() + '"');
            }
         }

         if (baseDescriptorElement != null && metadata.getBaseNamespace() != null)
         {
            String sBaseChecksum = "";

            if (!metadata.getBaseChecksum().equals(""))
            {
               sBaseChecksum = helper.getChecksum(false);
            }

            if (!metadata.getBaseNamespace().equals(helper.getNamespace(false)) ||
               !metadata.getBaseVersion().equals(helper.getVersion(false)) ||
               !metadata.getBaseChecksum().equals(sBaseChecksum))
            {
               throw new MetadataException("err.meta.baseVersion",
                  new Object[]
                  {
                     metadata.getBaseNamespace(),
                     metadata.getBaseVersion(),
                     metadata.getBaseChecksum(),
                     helper.getNamespace(false),
                     helper.getVersion(false),
                     sBaseChecksum
                  });
            }

            metadata.setBaseChecksum(sBaseChecksum);

            String sBaseCoreVersion = XMLUtil.getStringAttr(baseDescriptorElement, "coreVersion");

            if (metadata.getCoreVersion() != null)
            {
               if (sBaseCoreVersion != null &&
                  StringUtil.compareVersionRanges(metadata.getCoreVersion(), sBaseCoreVersion) < 0)
               {
                  throw new MetadataException("err.meta.baseCoreVersion",
                     new Object[] {XMLUtil.getStringAttr(baseDescriptorElement, "coreVersion"), metadata.getCoreVersion()}
                  );
               }
            }
            else
            {
               metadata.setCoreVersion(sBaseCoreVersion);
            }

            String sModule = XMLMetadataHelper.normalizeScope(XMLUtil.getStringAttr(descriptorElement, "module"));
            String sBaseModule = XMLMetadataHelper.normalizeScope(XMLUtil.getStringAttr(baseDescriptorElement, "module"));

            if (!ObjUtil.equal(sModule, sBaseModule))
            {
               throw new MetadataException("err.meta.baseModule",
                  new Object[]{(sModule == null) ? "" : sModule, (sBaseModule == null) ? "" : sBaseModule});
            }
         }

         String sCoreVersion = metadata.getCoreVersion();

         if (sCoreVersion != null)
         {
            boolean bPlus = sCoreVersion.length() > 0 &&
                            sCoreVersion.charAt(sCoreVersion.length() - 1) == '+';
            int nCompare = StringUtil.compareVersionRanges(Version.RELEASE, sCoreVersion);

            // if metadata coreVersion is older than Version.RELEASE then coreVersion must have '+'
            // if metadata coreVersion is newer than Version.RELEASE then metadata not compatible
            if (nCompare != 0 && (!bPlus || nCompare < 0))
            {
               throw new MetadataException(
                  "err.meta.coreVersion",
                  new Object[]
                  {
                     Version.RELEASE,
                     sCoreVersion
                  });
            }
         }
      }
      catch (UncheckedException e)
      {
         MetadataValidationException x = new MetadataValidationException(e);
         getHelper().setMarker(x);
         throw x;
      }
      finally
      {
         getHelper().restoreMarker(nCookie);
      }
   }


   /**
    * Loads a library.
    * @param reader The character input stream.
    * @param sName The library name.
    * @param bSystem If true the scripting URL will prefix will be "syslibrary:"
    *           rather than "library:".
    */
   protected void loadLibrary(Reader reader, String sName, boolean bSystem) throws IOException
   {
      GlobalEnvironment.Listener listener = new GlobalEnvironment.Listener()
      {
         protected boolean m_bClient;

         public void setScope(GlobalEnvironment env, Symbol scope)
         {
            String sName = scope.getName();

            if (sName != null)
            {
               if (sName.equals("all") || sName.equals("client"))
               {
                  m_bClient = true;
               }
               else if (sName.equals("server"))
               {
                  m_bClient = false;
               }
               else
               {
                  throw new MetadataException("err.meta.library.scope",
                     new Object[]{sName});
               }
            }
         }

         public void setVariable(GlobalEnvironment env, Symbol name, Object value)
         {
            if (m_bClient && value instanceof PCodeFunction && !(value instanceof Macro))
            {
               m_metadata.addClientSymbol(name);
            }
            else
            {
               m_metadata.removeClientSymbol(name);
            }
         }
      };

      try
      {
         m_metadata.getGlobalEnvironment().addListener(listener);
         Intrinsic.load(reader, ((bSystem) ? "syslibrary:" : "library:") + sName, m_machine);
      }
      finally
      {
         m_metadata.getGlobalEnvironment().removeListener(listener);
      }
   }

   /**
    * Loads data source types from a DOM element.
    * @param dstElement The DOM element containing the data source types.
    */
   protected void loadDataSourceTypes(Element dstElement)
   {
      XMLMetadataHelper.verifyRootElement(dstElement, "DataSourceTypes");

      XMLUtil.forEachChildElement(dstElement, "DataSourceType",
         m_helper.new ElementHandler("dataSourceType")
      {
         public void handleElement(Element element, String sName)
         {
            loadDataSourceType(element, sName);
         }
      });
   }

   /**
    * Loads a data source type from a DOM element.
    * @param dstElement The DOM element containing the data source type.
    * @param sName The data source type name.
    */
   protected void loadDataSourceType(Element dstElement, String sName)
   {
      XMLMetadataHelper.verifyRootElement(dstElement, "DataSourceType");

      final DataSourceType type = new DataSourceType(sName);

      type.setMetadata(m_metadata);
      type.setLoader(m_helper.getClassObject(XMLUtil.getReqStringAttr(dstElement, "loader")));

      String sExporter = XMLUtil.getStringAttr(dstElement, "exporter");

      if (sExporter != null)
      {
         type.setExporter(m_helper.getClassObject(sExporter));
      }

      String sElement = XMLUtil.getReqStringAttr(dstElement, "element");

      XMLMetadataHelper.validateName(sElement);
      m_metadata.addDataSourceTypeElement(sElement, type);

      XMLUtil.withFirstChildElement(dstElement, "Adapters", false, new ElementHandler()
      {
         public void handleElement(Element adaptersElement)
         {
            XMLUtil.forEachChildElement(adaptersElement, "Adapter",
               m_helper.new ElementHandler("adapter")
            {
               public void handleElement(Element adapterElement, String sAdapterName)
               {
                  DataSourceAdapter adapter = new DataSourceAdapter(sAdapterName);

                  try
                  {
                     adapter.setType(type);
                     adapter.setClassObject(m_helper.getClassObject(XMLUtil.getReqStringAttr(adapterElement, "class")));
                     adapter.setVersion(XMLUtil.getStringAttr(adapterElement, "version"));
                     type.addAdapter(adapter);
                  }
                  catch (MetadataException e)
                  {
                     if (!(e.getCause() instanceof ClassNotFoundException) &&
                        !(e.getCause() instanceof NoClassDefFoundError))
                     {
                        throw e;
                     }

                     // if data source type can't be loaded, don't add it to the metadata, but don't fail.
                     s_logger.debug("Cannot load DataSourceAdapter: " + adapter.getName() + " " + XMLUtil.getReqStringAttr(adapterElement, "class"));
                  }
               }
            });
         }
      });

      m_metadata.addDataSourceType(type);
   }

   /**
    * Loads a data source from a DOM element.
    * @param dataSourceElement The DOM element containing the data source.
    * @param sName The data source name.
    */
   protected void loadDataSource(Element dataSourceElement, String sName)
   {
      DataSourceType type = m_metadata.getDataSourceTypeByElement(dataSourceElement);

      m_metadata.addDataSource(((XMLPersistenceMetadataLoader)m_helper.getClassInstance(type.getLoader()))
         .loadDataSource(dataSourceElement, sName, type, this));
   }

   /**
    * Loads data source common attributes from a DOM element.
    * @param element The DOM element containing the data source.
    * @param dataSource The target data source object.
    */
   public void loadDataSource(Element element, final DataSource dataSource)
   {
      dataSource.setReadable(XMLUtil.getBooleanAttr(element, "read", true));
      dataSource.setCreatable(XMLUtil.getBooleanAttr(element, "create", true));
      dataSource.setUpdatable(XMLUtil.getBooleanAttr(element, "update", true));
      dataSource.setDeletable(XMLUtil.getBooleanAttr(element, "delete", true));
      dataSource.setExecutable(XMLUtil.getBooleanAttr(element, "execute", true));
      dataSource.setJoinable(XMLUtil.getBooleanAttr(element, "join", true));
      dataSource.setReadLimit(XMLUtil.getIntAttr(element, "readLimit", dataSource.getReadLimit()));

      XMLUtil.withFirstChildElement(element, "Properties", false, new ElementHandler()
      {
         public void handleElement(final Element propertiesElement)
         {
            addIOFixup(new ContextFixup(m_helper)
            {
               public void fixup()
               {
                  loadComponentProperties(propertiesElement, dataSource.getComponent());
               }
            });
         }
      });
   }

   /**
    * Loads channel types from a DOM element.
    * @param chtElement The DOM element containing the channel types.
    */
   protected void loadChannelTypes(Element chtElement)
   {
      XMLMetadataHelper.verifyRootElement(chtElement, "ChannelTypes");

      XMLUtil.forEachChildElement(chtElement, "ChannelType",
         m_helper.new ElementHandler("channelType")
      {
         public void handleElement(Element element, String sName)
         {
            loadChannelType(element, sName);
         }
      });
   }

   /**
    * Loads a channel type from a DOM element.
    * @param chtElement The DOM element containing the channel type.
    * @param sName The channel type name.
    */
   protected void loadChannelType(Element chtElement, String sName)
   {
      XMLMetadataHelper.verifyRootElement(chtElement, "ChannelType");

      final ChannelType type = new ChannelType(sName);

      type.setMetadata(m_metadata);
      type.setLoader(m_helper.getClassObject(XMLUtil.getReqStringAttr(chtElement, "loader")));

      String sExporter = XMLUtil.getStringAttr(chtElement, "exporter");

      if (sExporter != null)
      {
         type.setExporter(m_helper.getClassObject(sExporter));
      }

      type.setSender(m_helper.getClassObject(XMLUtil.getReqStringAttr(chtElement, "sender")));

      String sReceiver = XMLUtil.getStringAttr(chtElement, "receiver");

      if (sReceiver != null)
      {
         type.setReceiver(m_helper.getClassObject(sReceiver));
      }

      String sElement = XMLUtil.getReqStringAttr(chtElement, "element");

      XMLMetadataHelper.validateName(sElement);
      m_metadata.addChannelTypeElement(sElement, type);
      m_metadata.addChannelType(type);
   }

   /**
    * Loads a channel from a DOM element.
    * @param channelElement The DOM element containing the channel.
    * @param sName The data source name.
    */
   protected void loadChannel(Element channelElement, String sName)
   {
      if (!m_loadHook.loadChannel(channelElement, sName))
      {
         ChannelType type = m_metadata.getChannelTypeByElement(channelElement);

         m_metadata.addChannel(((XMLIntegrationMetadataLoader)m_helper.getClassInstance(type.getLoader()))
            .loadChannel(channelElement, sName, type, this));
      }
   }

   /**
    * Loads channel common attributes from a DOM element.
    * @param element The DOM element containing the channel.
    * @param channel The target channel object.
    */
   public void loadChannel(Element element, final Channel channel)
   {
      channel.setSendable(XMLUtil.getBooleanAttr(element, "send", channel.isSendable()));
      channel.setReceivable(XMLUtil.getBooleanAttr(element, "receive", channel.isReceivable()));
      channel.setStealth(XMLUtil.getBooleanAttr(element, "stealth", channel.isStealth()));

      String sCombine = XMLUtil.getStringAttr(element, "combine", "none");

      if (sCombine.equals("none"))
      {
         channel.setCombinationMode(Channel.COMBINE_NONE);
      }
      else if (sCombine.equals("first"))
      {
         channel.setCombinationMode(Channel.COMBINE_FIRST);
      }
      else if (sCombine.equals("all"))
      {
         channel.setCombinationMode(Channel.COMBINE_ALL);
      }
      else
      {
         throw new MetadataException("err.meta.integration.combine",
            new Object[]{sCombine, channel.getName()});
      }

      XMLUtil.withFirstChildElement(element, "SenderProperties", false, new ElementHandler()
      {
         public void handleElement(final Element propertiesElement)
         {
            addIOFixup(new ContextFixup(m_helper)
            {
               public void fixup()
               {
                  if (channel.getSender() != null)
                  {
                     loadComponentProperties(propertiesElement, channel.getSender());
                  }
               }
            });
         }
      });

      XMLUtil.withFirstChildElement(element, "ReceiverProperties", false, new ElementHandler()
      {
         public void handleElement(final Element propertiesElement)
         {
            addIOFixup(new ContextFixup(m_helper)
            {
               public void fixup()
               {
                  if (channel.getReceiver() != null)
                  {
                     loadComponentProperties(propertiesElement, channel.getReceiver());
                  }
               }
            });
         }
      });

      if (!m_bIntegrationExcluded)
      {
         XMLUtil.withFirstChildElement(element, "ServiceBindings", false, new ElementHandler()
         {
            public void handleElement(final Element bindingsElement)
            {
               addPersistenceMappingFixup(new ContextFixup(m_helper)
               {
                  public void fixup()
                  {
                     XMLUtil.forEachChildElement(bindingsElement, "ServiceBinding", m_helper.new ElementHandler("service")
                     {
                        protected String getName(Element element)
                        {
                           return XMLUtil.getReqStringAttr(element, "service");
                        }

                        public void handleElement(Element bindingElement, String sService)
                        {
                           final Binding binding = new Binding(m_metadata.getService(sService), channel);
                           String sOutput = XMLUtil.getStringAttr(bindingElement, "output");

                           if (sOutput != null)
                           {
                              binding.setOutput(m_metadata.getChannel(sOutput));
                           }

                           XMLUtil.withFirstChildElement(bindingElement, "Arguments", false, new ElementHandler()
                           {
                              public void handleElement(Element argumentsElement)
                              {
                                 XMLUtil.forEachChildElement(argumentsElement, "Argument", m_helper.new ElementHandler("argument")
                                 {
                                    public void handleElement(Element argumentElement, String sArgument)
                                    {
                                       String sChannel = XMLUtil.getStringAttr(argumentElement, "channel");

                                       if (sChannel != null)
                                       {
                                          binding.setArgumentChannel(sArgument, m_metadata.getChannel(sChannel));
                                       }

                                       String sValue = XMLUtil.getStringAttr(argumentElement, "value");

                                       if (sValue != null)
                                       {
                                          binding.setArgumentValue(sArgument,
                                             m_helper.parse(sValue, false, null, null, m_metadata.getGlobalEnvironment()));
                                       }
                                    }
                                 });
                              }
                           });

                           binding.compile(m_machine);
                           channel.addBinding(binding);
                        }
                     });
                  }
               });

               addSingletonFixup(new ContextFixup(m_helper)
               {
                  public void fixup()
                  {
                     MessageTable table = channel.getMessageTable();

                     if (table != null && table.getFormat() != null)
                     {
                        ((MessageParser)table.getFormat().getParser().getInstance(null)).initializeMessageTable(table);
                     }
                  }
               });
            }
         });
      }
   }

   /**
    * Loads the server properties from a property map.
    * @param envProps The environment properties.
    */
   protected void loadServer(Properties envProps)
   {
      m_metadata.setPortOffset(
         Integer.parseInt(SysUtil.getConfigProperties().getProperty("port.offset", "0")));

      String sType = envProps.getProperty("type", "Generic");

      if (sType.equals("JBoss"))
      {
         m_nContainer = J2EEUtil.JBOSS;
      }
      else if (sType.equals("WebSphere"))
      {
         m_nContainer = J2EEUtil.WEBSPHERE;
      }
      else
      {
         m_nContainer = J2EEUtil.NONE;
      }

      String sAuthProtocol = envProps.getProperty("authProtocol", "basic");

      if (sAuthProtocol.equals("basic"))
      {
         m_metadata.setAuthenticationProtocol(Metadata.AUTH_PROTOCOL_BASIC);
      }
      else if (sAuthProtocol.equals("spnego"))
      {
         m_metadata.setAuthenticationProtocol(Metadata.AUTH_PROTOCOL_SPNEGO);
      }
      else if (sAuthProtocol.equals("certificate"))
      {
         m_metadata.setAuthenticationProtocol(Metadata.AUTH_PROTOCOL_CERTIFICATE);
      }
      else if (sAuthProtocol.equals("perimeter"))
      {
         m_metadata.setAuthenticationProtocol(Metadata.AUTH_PROTOCOL_PERIMETER);
      }
      else
      {
         throw new MetadataException("err.meta.authProtocol", new Object[]{sAuthProtocol});
      }

      m_metadata.setAuthenticationService(envProps.getProperty("authService"));
      m_metadata.setAuthenticationDomain(envProps.getProperty("authDomain"));
      m_metadata.setSessionTimeout(Integer.parseInt(envProps.getProperty(
         "sessionTimeout", Integer.toString(m_metadata.getSessionTimeout()))));
      m_metadata.setReplicatedSession(StringUtil.parseBoolean(envProps.getProperty(
         "replicatedSession", Boolean.toString(m_metadata.isReplicatedSession()))));
      m_metadata.setPersistentSession(StringUtil.parseBoolean(envProps.getProperty(
         "persistentSession", Boolean.toString(m_metadata.isPersistentSession()))));
      m_metadata.setSecureTransport(StringUtil.parseBoolean(envProps.getProperty(
         "secureTransport", Boolean.toString(m_metadata.isSecureTransport()))));
      m_metadata.setTestInterceptor(StringUtil.parseBoolean(envProps.getProperty(
         "test.record", Boolean.toString(m_metadata.isTestInterceptor()))));
      m_metadata.setTestEnvironment(StringUtil.parseBoolean(envProps.getProperty(
         "test", Boolean.toString(m_metadata.isTestEnvironment()))));

      m_metadata.setKeyStorePassword(decryptPassword(envProps.getProperty("keystorePassword")));
      String sHTTPURL = envProps.getProperty("httpURL");

      m_metadata.setHTTPRoot(sHTTPURL);
      m_metadata.setHTTPContextRoot(HTTP.getContextPath(sHTTPURL));
      m_metadata.setHTTPAnonymousContextRoot(HTTP.getContextPath(envProps.getProperty("httpAnonymousURL",
         m_metadata.getHTTPAnonymousContextRoot())));
      m_metadata.setHTTPFormContextRoot(HTTP.getContextPath(envProps.getProperty("httpFormURL",
         m_metadata.getHTTPFormContextRoot())));
      m_metadata.setHTTPPushRedirectorContextRoot(HTTP.getContextPath(envProps.getProperty("pushRedirectorURL",
         m_metadata.getHTTPPushRedirectorContextRoot())));

      if (m_metadata.getHTTPContextRoot().equals(m_metadata.getHTTPAnonymousContextRoot()) ||
         m_metadata.getHTTPContextRoot().equals(m_metadata.getHTTPFormContextRoot()) ||
         m_metadata.getHTTPAnonymousContextRoot().equals(m_metadata.getHTTPFormContextRoot()))
      {
         throw new MetadataException("err.meta.contextPathDup");
      }

      m_metadata.setHTTPFormLoginPage(envProps.getProperty("httpFormLoginPage",
         m_metadata.getHTTPFormLoginPage()));
      m_metadata.setHTTPFormErrorPage(envProps.getProperty("httpFormErrorPage",
         m_metadata.getHTTPFormErrorPage()));

      m_metadata.setTrustedCertificate(CertificateUtil.parseCertificate(envProps.getProperty("trust")));
      m_metadata.setGenericRPCAllowed(StringUtil.parseBoolean(envProps.getProperty("authRPC", "true")));

      String sAnonymousUser = envProps.getProperty("anonUser", "anonymous");

      m_metadata.setAnonymousUser(new SimplePrincipal(sAnonymousUser));
      m_metadata.setAnonymousRPCEnabled(StringUtil.parseBoolean(envProps.getProperty("anonRPC", "false")));
      m_metadata.setAnonymousFlatPageEnabled(StringUtil.parseBoolean(envProps.getProperty("anonWeb", "false")));

      final String sRPCPrivilege = envProps.getProperty("rpcPrivilege");

      if (sRPCPrivilege != null)
      {
         m_privilegeFixupList.add(new Fixup()
         {
            public void fixup()
            {
               m_metadata.setGenericRPCPrivilege(m_metadata.getPrimitivePrivilege(sRPCPrivilege));
            }

            public void setMarker(MetadataMarker e)
            {
               e.setResourceName(m_sConfigURL);
            }
         });
      }

      m_dispatcherChannel = new ObjectDispatcherQueue("SysObjectQueueDispatcher");
      m_dispatcherChannel.setMaxReceivers(
         Integer.parseInt(
            envProps.getProperty("maxMessageReceivers", Integer.toString(m_dispatcherChannel.getMaxReceivers()))));
      m_dispatcherChannel.setMaxSenders(
         Integer.parseInt(
            envProps.getProperty("maxMessageSenders", Integer.toString(m_dispatcherChannel.getMaxSenders()))));
   }

   /**
    * Loads connections from a DOM element.
    * @param connectionsElement The DOM element containing the environment. Can be null to load default connections.
    * @param sConnectionsName The name of the environment.
    */
   protected void loadConnections(Element connectionsElement, final String sConnectionsName)
   {
      final Set connectionSet = new HashHolder();

      if (connectionsElement != null)
      {
         XMLUtil.withFirstChildElement(connectionsElement, "DataSourceConnections", false, new ElementHandler()
         {
            public void handleElement(Element connectionsElement)
            {
               XMLUtil.forEachChildElement(connectionsElement, null,
                  m_helper.new ElementHandler("dataSource")
               {
                  protected String getName(Element element)
                  {
                     return XMLUtil.getReqStringAttr(element, "dataSource");
                  }

                  public void handleElement(Element connectionElement, String sDataSource)
                  {
                     DataSource source = m_metadata.getDataSource(sDataSource);
                     String sExpectedElement = m_metadata.getDataSourceTypeElement(source.getType()) + "Connection";

                     if (!sExpectedElement.equals(connectionElement.getNodeName()))
                     {
                        throw new MetadataException("err.meta.dataSourceConnectionElement",
                           new Object[]{connectionElement.getNodeName(), source.getName(),
                           sConnectionsName, sExpectedElement});
                     }

                     if (!connectionSet.add(source))
                     {
                        throw new MetadataException("err.meta.dupDataSourceConnection", new Object[]{sDataSource});
                     }

                     String sAdapter = XMLUtil.getStringAttr(connectionElement, "adapter");

                     if (sAdapter != null)
                     {
                        source.setAdapter(source.getType().getAdapter(sAdapter));
                     }

                     source.setReadLimit(XMLUtil.getIntAttr(connectionElement, "readLimit", source.getReadLimit()));

                     //Load DataSource-specific attributes.
                     ((XMLPersistenceMetadataLoader)m_helper.getClassInstance(source.getType().getLoader()))
                        .loadConnection(connectionElement, source, XMLMetadataLoader.this);
                  }
               });
            }
         });
      }

      //Call loadConnection to initialize DataSources with no entry in the .connections file.
      for (Iterator itr = m_metadata.getDataSourceIterator(); itr.hasNext();)
      {
         DataSource source = (DataSource)itr.next();

         if (!connectionSet.contains(source))
         {
            int nCookie = getHelper().pushMarker(MetadataValidationException.TYPE_NAME, "DataSourceConnection");
            getHelper().pushMarker("dataSourceConnection", source.getName());

            try
            {
               ((XMLPersistenceMetadataLoader)m_helper.getClassInstance(source.getType().getLoader()))
                  .loadConnection(null, source, XMLMetadataLoader.this);
            }
            catch (MetadataException e)
            {
               m_helper.addException(e);
            }
            finally
            {
               m_helper.restoreMarker(nCookie);
            }
         }
      }

      if (connectionsElement != null && !m_bIntegrationExcluded)
      {
         XMLUtil.withFirstChildElement(connectionsElement, "ChannelConnections", false, new ElementHandler()
         {
            public void handleElement(Element connectionsElement)
            {
               XMLUtil.forEachChildElement(connectionsElement, null, m_helper.new ElementHandler("channel")
               {
                  protected String getName(Element element)
                  {
                     return XMLUtil.getReqStringAttr(element, "channel");
                  }

                  public void handleElement(Element connectionElement, String sChannel)
                  {
                     if (!m_loadHook.loadConnection(connectionElement, sConnectionsName, sChannel,
                        connectionSet))
                     {
                        Channel channel = m_metadata.getChannel(sChannel);

                        loadConnection(connectionElement, sConnectionsName, channel, connectionSet);
                     }
                  }
               });
            }
         });
      }

      //Call loadConnection to initialize Channels with no entry in the .connections file.
      for (Iterator itr = m_metadata.getChannelIterator(); itr.hasNext();)
      {
         Channel channel = (Channel)itr.next();

         if (!connectionSet.contains(channel))
         {
            int nCookie = getHelper().pushMarker(MetadataValidationException.TYPE_NAME, "ChannelConnection");

            getHelper().pushMarker("channelConnection", channel.getName());

            try
            {
               ((XMLIntegrationMetadataLoader)m_helper.getClassInstance(channel.getType().getLoader()))
                  .loadConnection(null, channel, XMLMetadataLoader.this);
            }
            catch (MetadataException e)
            {
               m_helper.addException(e);
            }
            finally
            {
               m_helper.restoreMarker(nCookie);
            }
         }
      }

      if (connectionsElement != null && m_soaLoader != null)
      {
         XMLUtil.withFirstChildElement(connectionsElement, "SOAConnections", false, new ElementHandler()
         {
            public void handleElement(Element connectionsElement)
            {
               XMLUtil.forEachChildElement(connectionsElement, "SOAConnection",
                  m_helper.new ElementHandler("soaconnection")
               {
                  protected String getName(Element element)
                  {
                     return XMLUtil.getReqStringAttr(element, "service");
                  }

                  public void handleElement(Element connectionElement, String sService)
                  {
                     m_soaLoader.loadConnection(connectionElement, sService, XMLMetadataLoader.this);
                  }
               });
            }
         });
      }
   }

   /**
    * Loads the connection for a channel.
    * @param connectionElement The element containing the connection information.
    * @param sConnectionsName The name of the environment.
    * @param channel The channel.
    * @param connectionSet Set of loaded connections.
    */
   public void loadConnection(Element connectionElement, String sConnectionsName, Channel channel, Set connectionSet)
   {
      String sExpectedElement = m_metadata.getChannelTypeElement(channel.getType()) + "Connection";

      if (!sExpectedElement.equals(connectionElement.getNodeName()))
      {
         throw new MetadataException("err.meta.channelConnectionElement",
            new Object[]{connectionElement.getNodeName(), channel.getName(), sConnectionsName, sExpectedElement});
      }

      if (!connectionSet.add(channel))
      {
         throw new MetadataException("err.meta.dupChannelConnection", new Object[]{channel.getName()});
      }

      // Load Channel-specific attributes
      ((XMLIntegrationMetadataLoader)m_helper.getClassInstance(channel.getType().getLoader()))
         .loadConnection(connectionElement, channel, XMLMetadataLoader.this);
   }

   /**
    * Loads a security descriptor.
    * @param descriptorElement The DOM element containing the descriptor.
    */
   protected void loadSecurityDescriptor(final Element descriptorElement)
   {
      XMLMetadataHelper.verifyRootElement(descriptorElement, "SecurityDescriptor");

      XMLUtil.withFirstChildElement(descriptorElement, "Privileges", false, new ElementHandler()
      {
         public void handleElement(Element privilegesElement)
         {
            XMLUtil.forEachChildElement(privilegesElement, "Privilege",
               m_helper.new ElementHandler("privilege")
            {
               public void handleElement(Element privilegeElement, String sPrivilegeName)
               {
                  Privilege privilege = new PrimitivePrivilege(sPrivilegeName);

                  m_metadata.addPrivilege(privilege);
                  privilege.setCaption(XMLUtil.getStringAttr(privilegeElement,
                     "caption", privilege.getCaption()));
               }
            });
         }
      });

      XMLUtil.withFirstChildElement(descriptorElement, "PrivilegeGroups", false, new ElementHandler()
      {
         public void handleElement(Element privilegeGroupsElement)
         {
            XMLUtil.forEachChildElement(privilegeGroupsElement, "PrivilegeGroup",
               m_helper.new ElementHandler("privilegeGroup")
            {
               public void handleElement(Element privilegeGroupElement, String sPrivilegeSetName)
               {
                  final PrivilegeGroup privilegeGroup = new PrivilegeGroup(sPrivilegeSetName);

                  m_metadata.addPrivilege(privilegeGroup);
                  privilegeGroup.setCaption(XMLUtil.getStringAttr(privilegeGroupElement,
                     "caption", privilegeGroup.getCaption()));

                  XMLUtil.withFirstChildElement(privilegeGroupElement, "Privileges", false, new ElementHandler()
                  {
                     public void handleElement(Element privilegesElement)
                     {
                        XMLUtil.forEachChildElement(privilegesElement, "Privilege",
                           m_helper.new ElementHandler("privilege")
                        {
                           public void handleElement(Element privilegeElement, final String sPrivilegeName)
                           {
                              m_privilegeFixupList.add(new ContextFixup(m_helper)
                              {
                                 public void fixup()
                                 {
                                    privilegeGroup.addPrivilege(m_metadata.getPrivilege(sPrivilegeName));
                                 }
                              });
                           }
                        });
                     }
                  });
               }
            });
         }
      });
   }

   /**
    * Loads class or aspect metadata from a DOM element.
    * @param classElement The DOM element containing the class.
    * @param metaclass The metaclass object.
    */
   protected void loadMetaclass(final Element classElement, final Metaclass metaclass)
   {
      Lookup posMap = new IdentityHashTab(32);

      XMLMetadataHelper.validateName(metaclass.getName());
      metaclass.setResourceName(m_helper.getCurResourceName());
      metaclass.setForward(false);
      loadDocumentation(classElement, metaclass);

      metaclass.setWhere(m_helper.parse(XMLUtil.getStringAttr(classElement, "where"),
         false, null, null, m_metadata.getGlobalEnvironment()));

      metaclass.setValidation(m_helper.parse(XMLUtil.getStringAttr(classElement, "validation"),
         false, posMap, Undefined.VALUE, m_metadata.getGlobalEnvironment()));

      metaclass.setTextPositionMap(posMap);

      XMLMetadataLoader.parsePatterns(XMLUtil.getStringAttr(classElement, "aspects"),
         new XMLMetadataLoader.PatternHandler()
         {
            public void handlePattern(final String sName, final boolean bInclusive)
            {
               metaclass.addAspectOverride(m_metadata.defineClassAspect(sName, metaclass), bInclusive);
            }
         });

      XMLUtil.withFirstChildElement(classElement, "Attributes", false, new ElementHandler()
      {
         public void handleElement(Element attributesElement)
         {
            XMLUtil.forEachChildElement(attributesElement, "Attribute",
               m_helper.new ElementHandler("attribute")
            {
               public void handleElement(Element attributeElement, String sAttributeName)
               {
                  XMLMetadataHelper.validateName(sAttributeName);

                  Attribute attribute = new Attribute(sAttributeName);

                  attribute.setMetaclass(metaclass);
                  attribute.setDeclarator(metaclass);
                  loadAttribute(attributeElement, attribute);
                  metaclass.addAttribute(attribute);
               }
            });
         }
      });

      XMLUtil.withFirstChildElement(classElement, "Events", false, new ElementHandler()
      {
         public void handleElement(Element eventsElement)
         {
            XMLUtil.forEachChildElement(eventsElement, "Event",
               m_helper.new ElementHandler("event")
            {
               public void handleElement(Element eventElement, String sEventName)
               {
                  XMLMetadataHelper.validateName(sEventName);

                  Event event = new Event(sEventName);

                  event.setMetaclass(metaclass);
                  event.setDeclarator(metaclass);
                  loadEvent(eventElement, event);
                  metaclass.addEvent(event);
               }
            });
         }
      });
   }

   /**
    * Loads a class from a DOM element.
    * @param classElement The DOM element containing the class.
    * @param sName The class name.
    */
   protected void loadClass(Element classElement, final String sClassName)
   {
      XMLMetadataHelper.verifyRootElement(classElement, "Class");

      final Metaclass metaclass = m_metadata.defineMetaclass(sClassName, null);

      if (!metaclass.isForward())
      {
         m_metadata.addMetaclass(metaclass);
      }

      loadMetaclass(classElement, metaclass);

      metaclass.setCaption(XMLUtil.getStringAttr(classElement, "caption", metaclass.getCaption()));
      metaclass.setVisibility(parseVisibility(XMLUtil.getStringAttr(classElement, "visibility"), Metaclass.PUBLIC));

      final String sNameAttribute = XMLUtil.getStringAttr(classElement, "nameAttribute");

      if (sNameAttribute != null)
      {
         m_attributeFixupList.add(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               metaclass.setNameAttribute(sNameAttribute);
            }
         });
      }

      m_inheritanceCheckList.add(new ContextFixup(m_helper)
      {
         public void fixup()
         {
            metaclass.checkInheritance();
         }
      });

      final String sBaseName = XMLUtil.getStringAttr(classElement, "base",
         (sClassName.equals(Metadata.ROOT_CLASS_NAME)) ? null : Metadata.ROOT_CLASS_NAME);

      if (sBaseName != null)
      {
         metaclass.setPointcut(true);
         m_metadata.defineMetaclass(sBaseName, metaclass).addDerived(metaclass);
      }
      else
      {
         m_attributeResolutionList.add(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               metaclass.resolveAttributes(m_machine);
            }
         });

         m_attributeDependencyList.add(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               metaclass.computeInverseDependency(m_attributeDependencySet);
            }
         });

         m_inheritanceFixupList.add(new ClassFixup()
         {
            public void fixup()
            {
               metaclass.resolveInheritance();
            }
         });

         m_inheritance1stPassFixupList.add(new ClassFixup()
         {
            public void fixup()
            {
               metaclass.resolveInheritance1();
            }
         });

         m_inheritance2ndPassFixupList.add(new ClassFixup()
         {
            public void fixup()
            {
               metaclass.compile(m_machine);
            }
         });

         m_inheritance4thPassFixupList.add(new ClassFixup()
         {
            public void fixup()
            {
               metaclass.resolveInheritance4();
            }
         });

         m_class2ndPassList.add(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               metaclass.resolveInitializers(m_machine);
            }
         });
      }

      // Queue for second pass only if a persistence mapping element is present

      XMLUtil.withFirstChildElement(classElement, "PersistenceMapping", false, new ElementHandler()
      {
         public void handleElement(Element element)
         {
            final String sResourceName = m_helper.getCurResourceName();
            final Element clone = (Element)m_tmpDocument.importNode(element, true);

            m_persistenceMappingLoadList.add(new ContextFixup(m_helper)
            {
               public void fixup()
               {
                  int nCookie = m_helper.pushMarker(MetadataValidationException.RESOURCE_NAME, sResourceName);

                  try
                  {
                     metaclass.setPersistenceMapping(loadPersistenceMapping(clone, metaclass));
                  }
                  finally
                  {
                     m_helper.restoreMarker(nCookie);
                  }
               }
            });
         }
      });
   }

   /**
    * Loads a class aspect from a DOM element.
    * @param aspectElement The DOM element containing the aspect.
    */
   protected void loadClassAspect(final Element aspectElement, final String sAspectName)
   {
      XMLMetadataHelper.verifyRootElement(aspectElement, "Aspect");

      final ClassAspect aspect = m_metadata.defineClassAspect(sAspectName, null);

      if (!aspect.isForward())
      {
         m_metadata.addClassAspect(aspect);
      }

      loadMetaclass(aspectElement, aspect);

      XMLMetadataLoader.parsePatterns(XMLUtil.getStringAttr(aspectElement, "pointcuts"),
         new XMLMetadataLoader.PatternHandler()
         {
            public void handlePattern(final String sPattern, final boolean bInclusive)
            {
               aspect.addPointcutPattern(sPattern, bInclusive);
            }
         });

      XMLUtil.withFirstChildElement(aspectElement, "PersistenceMappings", false, new ElementHandler()
      {
         public void handleElement(Element persistenceMappingsElement)
         {
            XMLUtil.forEachChildElement(persistenceMappingsElement, "PersistenceMapping", new ElementHandler()
            {
               public void handleElement(Element persistenceMappingElement)
               {
                  final String sResourceName = m_helper.getCurResourceName();
                  final Element clone = (Element)m_tmpDocument.importNode(persistenceMappingElement, true);

                  m_persistenceMappingLoadList.add(new ContextFixup(m_helper)
                  {
                     public void fixup()
                     {
                        int nCookie = m_helper.pushMarker(MetadataValidationException.RESOURCE_NAME, sResourceName);

                        try
                        {
                           aspect.addPersistenceMapping(loadPersistenceMapping(clone, aspect));
                        }
                        finally
                        {
                           m_helper.restoreMarker(nCookie);
                        }
                     }
                  });
               }
            });
         }
      });
   }

   /**
    * Loads an attribute from a DOM element.
    * @param attributeElement The DOM element containing the attribute.
    * @param attribute The attribute object.
    */
   protected void loadAttribute(Element attributeElement, final Attribute attribute)
   {
      loadDocumentation(attributeElement, attribute);
      attribute.setCaption(XMLUtil.getStringAttr(attributeElement, "caption"));
      attribute.setVisibility(parseVisibility(XMLUtil.getStringAttr(attributeElement, "visibility"), Metaclass.PUBLIC));
      attribute.setReadPrivilege(getPrivilege(attributeElement, "readPrivilege"));
      attribute.setUpdatePrivilege(getPrivilege(attributeElement, "updatePrivilege"));

      final String sURLPrefix = attribute.getURL();
      final String sTypeName = XMLUtil.getReqStringAttr(attributeElement, "type");
      Type type = Primitive.find(sTypeName);

      if (type == null)
      {
         type = m_metadata.defineMetaclass(sTypeName, attribute);
      }

      attribute.setType(type);
      attribute.setRequired(XMLUtil.getBooleanAttr(attributeElement, "required", false));
      attribute.setCollection(XMLUtil.getBooleanAttr(attributeElement, "collection", false));
      attribute.setCompatible(XMLUtil.getBooleanAttr(attributeElement, "compatible", false));
      attribute.setStatic(XMLUtil.getBooleanAttr(attributeElement, "static", false));
      attribute.setReadOnly(XMLUtil.getBooleanAttr(attributeElement, "readOnly", false));
      attribute.setCached(XMLUtil.getBooleanObjAttr(attributeElement, "cached"));

      Lookup posMap = new IdentityHashTab();

      attribute.setValue(m_helper.parse(XMLUtil.getStringAttr(attributeElement, "value"),
         false, sURLPrefix + "$value", posMap, Undefined.VALUE, m_metadata.getGlobalEnvironment()));
      attribute.setInitializer(m_helper.parse(XMLUtil.getStringAttr(attributeElement, "initializer"),
         false, sURLPrefix + "$initializer", posMap, Undefined.VALUE, m_metadata.getGlobalEnvironment()));
      attribute.setValidation(m_helper.parse(XMLUtil.getStringAttr(attributeElement, "validation"),
         false, sURLPrefix + "$validation", posMap, Undefined.VALUE, m_metadata.getGlobalEnvironment()));
      attribute.setWhere(m_helper.parse(XMLUtil.getStringAttr(attributeElement, "where"),
         false, null, null, m_metadata.getGlobalEnvironment()));
      attribute.setCascadeMode(parseCascadeMode(XMLUtil.getStringAttr(attributeElement, "cascade"), Attribute.CASCADE_DEFAULT));

      final String sReverseName = XMLUtil.getStringAttr(attributeElement, "reverse");

      if (sReverseName != null)
      {
         m_attributeFixupList.add(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               if (attribute.getType().isPrimitive())
               {
                  throw new MetadataException("err.meta.primitiveReverseAttrib",
                     new Object[]{sReverseName, attribute.getName(),
                        attribute.getMetaclass().getName()});
               }

               attribute.setReverse(((Metaclass)attribute.getType()).getAttribute(sReverseName));
            }
         });
      }

      final String sEnumeration = XMLUtil.getStringAttr(attributeElement, "enumeration");

      if (sEnumeration != null)
      {
         attribute.setEnumeration(m_metadata.defineMetaclass(sEnumeration, attribute));
      }

      attribute.setConstrained(XMLUtil.getBooleanAttr(attributeElement, "constrained", false));

      if (sEnumeration != null || attribute.isConstrained())
      {
         m_attributeFixupList.add(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               attribute.validateEnumeration(m_metadata);
            }
         });
      }

      final String sAccessAttributeName = XMLUtil.getStringAttr(attributeElement, "access");

      if (sAccessAttributeName != null)
      {
         m_attributeFixupList.add(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               attribute.setAccessAttribute(attribute.getMetaclass().getAttribute(sAccessAttributeName));
            }
         });
      }

      attribute.setDependency((Pair)m_helper.parse(
         XMLUtil.getStringAttr(attributeElement, "dependency"),
         true, null, null, m_metadata.getGlobalEnvironment()));

      attribute.setOrderBy((Pair)m_helper.parse(
         XMLUtil.getStringAttr(attributeElement, "orderBy"),
         true, null, null, m_metadata.getGlobalEnvironment()));

      attribute.setTextPositionMap(posMap);
   }

   /**
    * Load an argument from an XML element, optionally register it with m_classFixupList if
    * required. Fixups must be resolved before class inheritance.
    * @param element The element that defines this argument (not null).
    * @param sName The argument name.
    * @return The defined Argument.
    */
   protected Argument loadArgument(Element element, String sName)
   {
      final Argument arg = new Argument(sName);
      final String sType = XMLUtil.getReqStringAttr(element, "type");
      Type type = Primitive.find(sType);

      if (type == null && s_unsupportedIntrinsicSet.contains(sType))
      {
         type = Primitive.ANY;
      }

      arg.setRequired(XMLUtil.getBooleanAttr(element, "required"));
      arg.setCollection(XMLUtil.getBooleanAttr(element, "collection"));
      arg.setDescription(XMLUtil.getStringAttr(element, "description"));

      if (type == null)
      {
         type = m_metadata.defineMetaclass(sType, arg);
      }

      arg.setType(type);

      return arg;
   }

   /**
    * Loads an event from a DOM element.
    * @param eventElement The DOM element containing the event.
    * @param event The event object.
    */
   protected void loadEvent(Element eventElement, final Event event)
   {
      loadDocumentation(eventElement, event);
      event.setCompatible(XMLUtil.getBooleanAttr(eventElement, "compatible", false));
      event.setStatic(XMLUtil.getBooleanAttr(eventElement, "static", false));
      event.setVisibility(parseVisibility(XMLUtil.getStringAttr(eventElement, "visibility"), Metaclass.PROTECTED));
      event.setPrivilege(getPrivilege(eventElement, "privilege"));
      event.setTransactionMode(parseTransactionMode(XMLUtil.getStringAttr(eventElement, "transaction"), Event.TX_DEFAULT));
      event.setAudited(XMLUtil.getBooleanObjAttr(eventElement, "audit"));
      event.setVarArg(XMLUtil.getBooleanAttr(eventElement, "vararg", false));

      XMLUtil.withFirstChildElement(eventElement, "Result", false, new ElementHandler()
      {
         public void handleElement(Element resultElement)
         {
            event.setResult(loadArgument(resultElement, null));
         }
      });

      String sArgList = XMLUtil.getStringAttr(eventElement, "args");
      final Lookup/*<String, Argument>*/ argMap = new HashTab/*<String, Argument>*/();

      XMLUtil.withFirstChildElement(eventElement, "Arguments", false, new ElementHandler()
      {
         public void handleElement(Element argsElement)
         {
            XMLUtil.forEachChildElement(argsElement, "Argument",
               m_helper.new ElementHandler("argument")
            {
               protected void handleElement(Element argElement, String sArgName)
               {
                  argMap.put(sArgName, loadArgument(argElement, sArgName));
               }
            });
         }
      });

      if (sArgList != null)
      {
         for (StringTokenizer tokenizer = new StringTokenizer(sArgList); tokenizer.hasMoreTokens();)
         {
            String sArgName = tokenizer.nextToken();
            Argument arg = (Argument)argMap.remove(sArgName);

            XMLMetadataHelper.validateName(sArgName);
            event.addArgument((arg == null) ? new Argument(sArgName) : arg);
         }
      }

      MetadataCompoundValidationException e = null;

      for (Iterator/*<Argument>*/ itr = argMap.valueIterator(); itr.hasNext();)
      {
         if (e == null)
         {
            e = new MetadataCompoundValidationException();
         }

         e.addException(
            new MetadataException("err.meta.argLookup", new Object[]{itr.next(), event}));
      }

      if (e != null)
      {
         throw e;
      }

      event.addVariables((Pair)m_helper.parse(XMLUtil.getStringAttr(eventElement, "variables"), true, null, null, m_metadata.getGlobalEnvironment()));

      final String sAccessAttributeName = XMLUtil.getStringAttr(eventElement, "access");

      if (sAccessAttributeName != null)
      {
         m_attributeFixupList.add(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               event.setAccessAttribute(event.getMetaclass().getAttribute(sAccessAttributeName));
            }
         });
      }

      XMLUtil.withFirstChildElement(eventElement, "Actions", false, new ElementHandler()
      {
         public void handleElement(Element actionsElement)
         {
            XMLUtil.forEachChildElement(actionsElement, "Action",
               m_helper.new ElementHandler("action")
            {
               public void handleElement(Element actionElement, String sActionName)
               {
                  String sGroupName = null;
                  int i = sActionName.indexOf(':');

                  if (i > 0 && i < sActionName.length() - 1)
                  {
                     sGroupName = sActionName.substring(0, i);
                     sActionName = sActionName.substring(i + 1);
                  }

                  XMLMetadataHelper.validateName(sActionName);

                  Action action = new Action(sActionName, sGroupName);

                  action.setEvent(event);
                  loadAction(actionElement, action);
                  event.addAction(action);
               }
            });
         }
      });
   }

   /**
    * Loads an action from a DOM element.
    * @param actionElement The DOM element containing the action.
    * @param action The action object.
    */
   protected void loadAction(Element actionElement, final Action action)
   {
      String sType = XMLUtil.getReqStringAttr(actionElement, "type");

      if (sType.equals("main"))
      {
         action.setType(Action.MAIN);
      }
      else if (sType.equals("before"))
      {
         action.setType(Action.BEFORE);
      }
      else if (sType.equals("after"))
      {
         action.setType(Action.AFTER);
      }
      else if (sType.equals("around"))
      {
         action.setType(Action.AROUND);
      }
      else
      {
         throw new MetadataException("err.meta.actionType", new Object[]{sType});
      }

      action.validate(m_metadata, m_helper.getWarnings());

      final String sRelativeName = XMLUtil.getStringAttr(actionElement, "relative");

      if (sRelativeName != null)
      {
         if (action.getType() == Action.MAIN)
         {
            throw new MetadataException("err.meta.relativeMainAction");
         }

         m_actionFixupList.add(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               action.setNextAction(sRelativeName);
            }
         });
      }

      Lookup posMap = new IdentityHashTab();

      action.setCondition(m_helper.parse(XMLUtil.getStringAttr(actionElement, "condition"),
         false, posMap, Boolean.TRUE, m_metadata.getGlobalEnvironment()));

      String sMethodName = XMLUtil.getStringAttr(actionElement, "method");

      if (sMethodName != null)
      {
         int i = sMethodName.lastIndexOf('.');

         if (i < 0)
         {
            throw new MetadataException("err.meta.methodName", new Object[]{sMethodName});
         }

         if (!isRuntimeExcluded())
         {
            String sClassName = sMethodName.substring(0, i);
            sMethodName = sMethodName.substring(i + 1);
            Method[] methodArray;

            try
            {
               Class clazz = Class.forName(sClassName);

               action.setMethodClass(clazz);
               methodArray = clazz.getMethods();
            }
            catch (Exception e)
            {
               throw new MetadataException("err.meta.classLoad", new Object[]{sClassName}, e);
            }

            Method method = null;

            for (i = 0; i < methodArray.length; ++i)
            {
               Method meth = methodArray[i];

               if (Modifier.isPublic(meth.getModifiers()) &&
                  meth.getName().equals(sMethodName))
               {
                  Class[] paramArray = meth.getParameterTypes();

                  if (paramArray.length == action.getEvent().getArgumentCount() + 2)
                  {
                     if (method != null)
                     {
                        throw new MetadataException("err.meta.javaMethodDup", new Object[]{sMethodName, sClassName});
                     }

                     method = meth;
                  }
               }
            }

            if (method == null)
            {
               throw new MetadataException("err.meta.javaMethodLookup",
                  new Object[]{sMethodName, sClassName});
            }

            action.setMethod(method);
         }
      }

      Object body = m_helper.parse(m_helper.getElementValue(actionElement),
         true, posMap, null, m_metadata.getGlobalEnvironment());

      if (body != null)
      {
         if (action.getMethod() != null)
         {
            throw new MetadataException("err.meta.scriptAndMethod", new Object[]{action.getName()});
         }

         action.setBody(body);
      }

      action.setTextPositionMap(posMap);
   }

   /**
    * Loads the values of an enumeration from a DOM element and adds them to the given collection.
    * @param enumerationElement The DOM element containing the enumeration definition.
    * @param valueCollection The collection to which values should be added.
    */
   public static void loadEnumerationValues(Element enumerationElement, final Collection valueCollection)
   {
      XMLMetadataHelper.verifyRootElement(enumerationElement, "Enumeration");

      XMLUtil.withFirstChildElement(enumerationElement, "Values", false, new ElementHandler()
      {
         public void handleElement(Element valuesElement)
         {
            XMLUtil.forEachChildElement(valuesElement, "Value",
               new XMLUtil.ElementHandler()
               {
                  public void handleElement(Element element)
                  {
                     valueCollection.add(XMLUtil.getReqStringAttr(element, "value"));
                  }
               }
            );
         }
      });
   }

   /**
    * Loads an enumeration from a DOM element.
    * @param enumerationElement The DOM element containing the enumeration definition.
    * @param sEnumerationName The enumeration name.
    */
   protected void loadEnumeration(Element enumerationElement, final String sEnumerationName)
   {
      XMLMetadataHelper.verifyRootElement(enumerationElement, "Enumeration");
      XMLMetadataHelper.validateName(sEnumerationName);

      final String sBaseName = XMLUtil.getStringAttr(enumerationElement,
         "base", Metadata.ENUMERATION_CLASS_NAME);

      final Metaclass metaclass = (sEnumerationName.equals(sBaseName)) ? null :
         m_metadata.defineMetaclass(sEnumerationName, null);
      final String sParentEnumeration = XMLUtil.getStringAttr(enumerationElement, "parent");

      if (metaclass != null)
      {
         if (metaclass.isForward())
         {
            metaclass.setResourceName(m_helper.getCurResourceName());
            metaclass.setForward(false);
         }
         else
         {
            m_metadata.addMetaclass(metaclass);
         }

         loadDocumentation(enumerationElement, metaclass);
         m_metadata.defineMetaclass(sBaseName, metaclass).addDerived(metaclass);

         m_inheritanceCheckList.add(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               metaclass.checkInheritance();
            }
         });

         if (sParentEnumeration != null)
         {
            final String sAssociation = XMLUtil.getStringAttr(enumerationElement, "association");

            if (sAssociation == null)
            {
               throw new MetadataException("err.meta.missingEnumAssoc", new Object[]{sEnumerationName});
            }

            final String sReverse = XMLUtil.getStringAttr(enumerationElement, "reverse");

            if (sReverse == null)
            {
               throw new MetadataException("err.meta.missingEnumReverse", new Object[]{sEnumerationName});
            }

            XMLMetadataHelper.validateName(sAssociation);

            m_attributeGenerationList.add(new ContextFixup(m_helper)
            {
               public void fixup()
               {
                  final Metaclass parentMetaclass = m_metadata.getMetaclass(sParentEnumeration);

                  final Attribute parentAttribute = new Attribute(sReverse);

                  parentAttribute.setMetaclass(metaclass);
                  parentAttribute.setDeclarator(metaclass);
                  parentAttribute.setType(parentMetaclass);
                  parentAttribute.setRequired(true);

                  metaclass.addAttribute(parentAttribute);

                  final Attribute childAttribute = new Attribute(sAssociation);

                  childAttribute.setMetaclass(parentMetaclass);
                  childAttribute.setDeclarator(parentMetaclass);
                  childAttribute.setType(metaclass);
                  childAttribute.setCollection(true);
                  childAttribute.setReverse(parentAttribute);

                  parentMetaclass.addAttribute(childAttribute);

                  m_persistenceMappingGenerationList.add(new ContextFixup(m_helper)
                  {
                     public void fixup()
                     {
                        copyAttributeMapping(parentAttribute, metaclass.getAttribute("parent"));
                        copyAttributeMapping(childAttribute, parentMetaclass.getAttribute("children"));
                     }

                     protected void copyAttributeMapping(Attribute dst, Attribute src)
                     {
                        PersistenceMapping mapping = src.getMetaclass().getPersistenceMapping();

                        if (mapping.getAttributeMapping(dst) != null)
                        {
                           return;
                        }

                        for (PersistenceMapping baseMapping = mapping;
                           baseMapping != null && baseMapping.isCompatible(mapping);
                           baseMapping = baseMapping.getBaseMapping())
                        {
                           Attribute attribute = baseMapping.getMetaclass().findAttribute(src.getName());

                           if (attribute != null)
                           {
                              AttributeMapping attributeMapping = baseMapping.getAttributeMapping(attribute);

                              if (attributeMapping != null)
                              {
                                 attributeMapping = (AttributeMapping)attributeMapping.clone();

                                 attributeMapping.setPersistenceMapping(mapping);
                                 attributeMapping.setAttribute(dst);

                                 ((ClassMapping)attributeMapping).setMapping(((Metaclass)dst.getType()).getPersistenceMapping());

                                 mapping.addAttributeMapping(attributeMapping);

                                 return;
                              }
                           }
                        }

                        throw new MetadataException("err.meta.enumAssociation",
                           new Object[]{src.getName(), src.getMetaclass().getName()});
                     }
                  });
               }
            });
         }

         final String sTypeCode = XMLUtil.getReqStringAttr(enumerationElement, "typeCode");

         m_inheritance1stPassFixupList.add(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               Attribute attribute = null;

               for (PersistenceMapping mapping = metaclass.getPersistenceMapping();
                  mapping != null; mapping = mapping.getBaseMapping())
               {
                  attribute = mapping.getTypeCodeAttribute();

                  if (attribute != null)
                  {
                     attribute = metaclass.getDerivedAttribute(attribute);

                     break;
                  }
               }

               if (attribute == null)
               {
                  throw new MetadataException("err.meta.noTypeCodeAttribute",
                     new Object[]{metaclass.getName()});
               }

               attribute.setValue(attribute.getType().convert(sTypeCode));
            }
         });
      }

      if (m_bValidatedOnly)
      {
         XMLUtil.withFirstChildElement(enumerationElement, "Locales", false, new ElementHandler()
         {
            public void handleElement(Element localesElement)
            {
               final Set localeSet = new HashHolder();

               XMLUtil.forEachChildElement(localesElement, "Locale",
                  m_helper.new ElementHandler("locale")
               {
                  public void handleElement(Element localeElement, String sLocale)
                  {
                     if (!localeSet.add(sLocale))
                     {
                        throw new MetadataException("err.meta.enumLocaleDup",
                           new Object[]{sLocale, sEnumerationName});
                     }

                     XMLUtil.getReqStringAttr(localeElement, "caption");
                  }
               });
            }
         });
      }

      final Set valueSet = new GenericHashHolder()
      {
         private final static long serialVersionUID = -9222368445291072422L;

         /**
          * @see nexj.core.util.GenericHashHolder#equal(java.lang.Object, java.lang.Object)
          */
         protected boolean equal(Object left, Object right)
         {
            if (left instanceof String && right instanceof String)
            {
               return ((String)left).equalsIgnoreCase((String)right);
            }

            return left.equals(right);
         }

         /**
          * @see nexj.core.util.GenericHashHolder#hash(java.lang.Object)
          */
         protected int hash(Object key)
         {
            if (key instanceof String)
            {
               return StringUtil.hashIgnoreCase((String)key);
            }

            return key.hashCode();
         }
      };

      m_enumerationValueMap.put(sEnumerationName, valueSet);

      XMLUtil.withFirstChildElement(enumerationElement, "Values", false, new ElementHandler()
      {
         public void handleElement(Element valuesElement)
         {
            final Set nameSet = new HashHolder();
            final Set behaveAsSet = new HashHolder();
            final Set externSet = new HashHolder();
            final List fixupList = new ArrayList();

            XMLUtil.forEachChildElement(valuesElement, "Value",
               m_helper.new ElementHandler("value")
            {
               public void handleElement(Element valueElement, String sName)
               {
                  XMLMetadataHelper.validateName(sName);

                  fixupList.add(sName);

                  if (!nameSet.add(sName))
                  {
                     throw new MetadataException("err.meta.enumValueNameDup",
                        new Object[]{sName, sEnumerationName});
                  }

                  final String sValue = XMLUtil.getReqStringAttr(valueElement, "value");

                  if (!valueSet.add(sValue))
                  {
                     throw new MetadataException("err.meta.enumValueDup",
                        new Object[]{sValue, sEnumerationName});
                  }

                  fixupList.add(sValue);

                  boolean bHasBehavior = XMLUtil.getBooleanAttr(valueElement, "hasBehavior", false);

                  fixupList.add(Boolean.valueOf(bHasBehavior));

                  String sBehaveAsValue = XMLUtil.getStringAttr(valueElement, "behaveAsValue");

                  if (sBehaveAsValue != null)
                  {
                     if (bHasBehavior)
                     {
                        if (!sBehaveAsValue.equals(sValue))
                        {
                           throw new MetadataException("err.meta.enumBehaveAsValueMismatch",
                              new Object[]{sBehaveAsValue, sValue, sEnumerationName});
                        }
                     }
                     else
                     {
                        if (sBehaveAsValue.equals(sValue))
                        {
                           throw new MetadataException("err.meta.enumInvalidSelfReference",
                              new Object[]{sValue, sEnumerationName});
                        }

                        behaveAsSet.add(sBehaveAsValue);
                     }
                  }

                  String sExternValue = XMLUtil.getStringAttr(valueElement, "externalValue");

                  if (sExternValue != null)
                  {
                     if (!externSet.add(sExternValue))
                     {
                        throw new MetadataException("err.meta.enumExternValueDup",
                           new Object[]{sExternValue, sEnumerationName});
                     }
                  }

                  final String sParentValue = XMLUtil.getStringAttr(valueElement, "parentValue");

                  if (sParentValue != null)
                  {
                     if (sParentEnumeration == null)
                     {
                        throw new MetadataException("err.meta.enumParentValue", new Object[]{sEnumerationName});
                     }

                     m_inheritanceFixupList.add(new ContextFixup(m_helper)
                     {
                        public void fixup()
                        {
                           Metaclass parent = m_metadata.getMetaclass(sParentEnumeration);

                           if (!((Set)m_enumerationValueMap.get(sParentEnumeration)).contains(parent.getAttribute("value").getType().convert(sParentValue)))
                           {
                              throw new MetadataLookupException("err.meta.enumParentValueLookup", sParentValue, sParentEnumeration);
                           }
                        }
                     });
                  }
                  else
                  {
                     if (sParentEnumeration != null)
                     {
                        throw new MetadataException("err.meta.enumNoParentValue", new Object[]{sValue, sEnumerationName});
                     }
                  }

                  if (m_bValidatedOnly)
                  {
                     XMLUtil.withFirstChildElement(valueElement, "Locales", false, new ElementHandler()
                     {
                        public void handleElement(Element localesElement)
                        {
                           final Set localeSet = new HashHolder();

                           XMLUtil.forEachChildElement(localesElement, "Locale",
                              m_helper.new ElementHandler("locale")
                           {
                              public void handleElement(Element localeElement, String sLocale)
                              {
                                 if (!localeSet.add(sLocale))
                                 {
                                    throw new MetadataException("err.meta.enumValueLocaleDup",
                                       new Object[]{sLocale, sValue, sEnumerationName});
                                 }

                                 XMLUtil.getReqStringAttr(localeElement, "caption");
                              }
                           });
                        }
                     });
                  }
               }
            });

            if (behaveAsSet.size() != 0)
            {
               // Remove the values that have no behavior
               for (int i = 0, n = fixupList.size(); i != n; i += 3)
               {
                  if (!((Boolean)fixupList.get(i + 2)).booleanValue())
                  {
                     valueSet.remove(fixupList.get(i + 1));
                  }
               }

               for (Iterator itr = behaveAsSet.iterator(); itr.hasNext();)
               {
                  String sValue = (String)itr.next();

                  if (!valueSet.contains(sValue))
                  {
                     throw new MetadataLookupException("err.meta.enumBehaveAsValueLookup",
                        sValue, sEnumerationName);
                  }
               }
            }

            valueSet.clear();

            m_attributeGenerationList.add(new ContextFixup(m_helper)
            {
               public void fixup()
               {
                  Primitive type = null;

                  for (Metaclass base = ((metaclass == null) ?
                     m_metadata.getMetaclass(sEnumerationName) : metaclass).getBase();
                     base != null; base = base.getBase())
                  {
                     Attribute attribute = base.findAttribute("value");

                     if (attribute != null)
                     {
                        if (!attribute.getType().isPrimitive())
                        {
                           throw new MetadataException("err.meta.enumBaseValueType",
                              new Object[]{base.getName(), sEnumerationName});
                        }

                        if (attribute.isStatic())
                        {
                           throw new MetadataException("err.meta.enumBaseValueScope",
                              new Object[]{base.getName(), sEnumerationName});
                        }

                        type = (Primitive)attribute.getType();

                        break;
                     }
                  }

                  if (type == null)
                  {
                     throw new MetadataException("err.meta.enumBaseValue", new Object[]{sEnumerationName});
                  }

                  for (int i = 0, n = fixupList.size(); i != n; i += 3)
                  {
                     Object value = fixupList.get(i + 1);

                     if (type != Primitive.STRING)
                     {
                        value = type.convert(value);
                     }

                     if (!valueSet.add(value))
                     {
                        throw new MetadataException("err.meta.enumValueDup",
                           new Object[]{value, sEnumerationName});
                     }

                     if (metaclass != null && ((Boolean)fixupList.get(i + 2)).booleanValue())
                     {
                        Attribute attribute = new Attribute((String)fixupList.get(i));

                        attribute.setMetaclass(metaclass);
                        attribute.setDeclarator(metaclass);
                        attribute.setStatic(true);
                        attribute.setReadOnly(true);
                        attribute.setType(type);
                        attribute.setValue(value);

                        metaclass.addAttribute(attribute);
                     }
                  }
               }
            });
         }
      });
   }

   /**
    * Loads a flow macro from a DOM element.
    * @param macroElement The DOM element containing the macro definition.
    * @param sMacroName The macro name.
    */
   protected void loadFlowMacro(Element macroElement, String sMacroName)
   {
      XMLMetadataHelper.verifyRootElement(macroElement, "Action");

      final FlowMacro macro = new FlowMacro(sMacroName);

      macro.setWorkflowCompatible(XMLUtil.getBooleanAttr(macroElement, "workflow", macro.isWorkflowCompatible()));
      macro.setServiceCompatible(XMLUtil.getBooleanAttr(macroElement, "service", macro.isServiceCompatible()));

      XMLUtil.withFirstChildElement(macroElement, "Arguments", false, new XMLUtil.ElementHandler()
      {
         public void handleElement(Element argumentsElement)
         {
            XMLUtil.forEachChildElement(argumentsElement, "Argument", m_helper.new ElementHandler("argument")
            {
               protected void handleElement(Element argumentElement, String sName)
               {
                  FlowMacro.Argument arg = new FlowMacro.Argument(sName);
                  String sTypeName = XMLUtil.getReqStringAttr(argumentElement, "type");

                  arg.setType((sTypeName.equals("list")) ? null : Primitive.parse(sTypeName));
                  arg.setDefault(m_helper.parse(XMLUtil.getStringAttr(argumentElement, "default"),
                     false, macro.getTextPositionMap(), arg.getDefault(), m_metadata.getGlobalEnvironment()));

                  macro.addArgument(arg);
               }
            });
         }
      });

      XMLUtil.withFirstChildElement(macroElement, "Script", false, new XMLUtil.ElementHandler()
      {
         public void handleElement(Element script)
         {
            macro.setBody((Pair)m_helper.parse(m_helper.getElementValue(script),
               true, macro.getTextPositionMap(), null, m_metadata.getGlobalEnvironment()));
         }
      });

      macro.compile(m_machine);

      m_metadata.addFlowMacro(macro);
   }

   /**
    * Loads a flow macro step.
    * @param stepElement The macro step element.
    * @param sStepName The macro step name.
    * @param activity The activity where to append the step.
    * @param helper The XML metadata helper.
    * @param machine The VM for compilation.
    * @param bFull true to set function arguments.
    * @return The macro script step, or null if no macro is found.
    */
   public static FlowMacroScript loadFlowMacroScript(Element stepElement, String sStepName, Activity activity,
      XMLMetadataHelper helper, Machine machine, boolean bFull)
   {
      Flow flow = activity.getFlow();
      FlowMacro macro = flow.getMetadata().findFlowMacro(stepElement.getNodeName());

      if (macro == null)
      {
         return null;
      }

      if (flow instanceof Workflow && !macro.isWorkflowCompatible() ||
         flow instanceof Service && !macro.isServiceCompatible())
      {
         throw new MetadataException("err.meta." + flow.getPropName() + ".incompatibleMacro",
            new Object[]{macro.getName(), flow.getFullName()});
      }

      FlowMacroScript script = new FlowMacroScript(sStepName);

      script.setActivity(activity);
      script.setMacro(macro);

      NamedNodeMap attributeMap = stepElement.getAttributes();
      Set attributeSet = new HashHolder(attributeMap.getLength());

      for (int i = 0; i < attributeMap.getLength(); ++i)
      {
         attributeSet.add(attributeMap.item(i).getNodeName());
      }

      Object[] args = new Object[macro.getArgumentCount()];

      for (int i = 0, n = macro.getArgumentCount(); i < n; ++i)
      {
         FlowMacro.Argument arg = macro.getArgument(i);
         String sValue = stepElement.getAttribute(arg.getName());

         if (arg.getType() == null || arg.getType() == Primitive.ANY)
         {
            Object value = helper.parse(sValue, arg.getType() == null,
               flow.getPosMap(), Undefined.VALUE,
               machine.getGlobalEnvironment());

            if (bFull)
            {
               script.setArgumentValue(i, value);
            }

            args[i] = (value == Undefined.VALUE) ? arg.getDefault() : value;
         }
         else
         {
            if (sValue == null)
            {
               args[i] = arg.getDefault();

               if (bFull)
               {
                  script.setArgumentValue(i, Undefined.VALUE);
               }
            }
            else
            {
               args[i] = arg.getType().convert(sValue);

               if (bFull)
               {
                  script.setArgumentValue(i, args[i]);
               }
            }
         }

         if (args[i] == Undefined.VALUE)
         {
            throw new MetadataException("err.meta." + flow.getPropName() + ".missingMacroArg",
               new Object[]{arg.getName(), macro.getName(), flow.getFullName()});
         }

         attributeSet.remove(arg.getName());
      }

      attributeSet.remove("name");
      attributeSet.remove("caption");
      attributeSet.remove("description");
      attributeSet.remove("layout");

      if (attributeSet.size() != 0)
      {
         throw new MetadataException("err.meta." + flow.getPropName() + ".macroArgLookup",
            new Object[]{attributeSet.iterator().next(), macro.getName(), flow.getFullName()});
      }

      script.setBody(new Pair(machine.invoke(macro.getFunction(), args)));

      Lookup flowPosMap = flow.getPosMap();
      Lookup macroPosMap = macro.getTextPositionMap();

      if (flowPosMap != null && macroPosMap != null)
      {
         for (Lookup.Iterator itr = macroPosMap.iterator(); itr.hasNext();)
         {
            itr.next();
            flowPosMap.put(itr.getKey(), itr.getValue());
         }
      }

      return script;
   }

   /**
    * Loads a flow macro step.
    * @param stepElement The macro step element.
    * @param sStepName The macro step name.
    * @param activity The activity where to append the step.
    * @return The macro script step, or null if not macro is found.
    */
   protected FlowMacroScript loadFlowMacroScript(Element stepElement, String sStepName, Activity activity)
   {
      return loadFlowMacroScript(stepElement, sStepName, activity, m_helper, m_machine, m_bDocumented);
   }

   /**
    * Loads a workflow from a DOM element.
    * @param workflowElement The DOM element containing the flow definition.
    * @param sWorkflowName The workflow name.
    */
   protected void loadWorkflow(Element workflowElement, String sWorkflowName)
   {
      XMLMetadataHelper.verifyRootElement(workflowElement, "Workflow");

      int nVersion = 0;
      int i = sWorkflowName.lastIndexOf('.');

      if (i > 0)
      {
         try
         {
            nVersion = Integer.parseInt(sWorkflowName.substring(i + 1));
         }
         catch (NumberFormatException e)
         {
            throw new MetadataException("err.meta.workflow.version", new Object[]{sWorkflowName.substring(i + 1)}, e);
         }

         sWorkflowName = sWorkflowName.substring(0, i);
      }

      XMLMetadataHelper.validateName(sWorkflowName);

      final Workflow workflow = new Workflow(sWorkflowName,
         m_helper.getCurResourceName(),
         nVersion,
         m_metadata.getMetaclass(XMLUtil.getReqStringAttr(workflowElement, "class")),
         (Pair)m_helper.parse(
            XMLUtil.getStringAttr(workflowElement, "attributes"),
            true, null, null, m_metadata.getGlobalEnvironment()),
         XMLUtil.getStringAttr(workflowElement, "caption")
      );

      workflow.setPrivileged(XMLUtil.getBooleanAttr(workflowElement, "privileged", workflow.isPrivileged()));

      String sVarList = XMLUtil.getStringAttr(workflowElement, "variables");

      if (sVarList != null)
      {
         for (StringTokenizer tokenizer = new StringTokenizer(sVarList); tokenizer.hasMoreTokens();)
         {
            String sVarName = tokenizer.nextToken();

            XMLMetadataHelper.validateName(sVarName);
            workflow.addVariable(new Variable(sVarName));
         }
      }

      if (isWorkflowHandler(workflowElement))
      {
         loadWorkflowHandler(workflowElement, workflow, workflow);
      }

      m_flowFixupList = new ArrayList();
      loadActivity(workflowElement.getFirstChild(), workflow);
      m_metadata.addWorkflow(workflow);
      m_helper.fixup(m_flowFixupList.iterator());
      m_flowFixupList = null;

      m_persistenceMappingFixupList.add(new ContextFixup(m_helper)
      {
         public void fixup()
         {
            workflow.resolve(m_machine);
         }
      });
   }

   /**
    * Checks if a DOM element contains the workflow event handler attributes.
    */
   protected static boolean isWorkflowHandler(Element element)
   {
      if (XMLUtil.getStringAttr(element, "event") != null)
      {
         return true;
      }

      if (XMLUtil.getStringAttr(element, "args") != null ||
         XMLUtil.getStringAttr(element, "association") != null ||
         XMLUtil.getStringAttr(element, "condition") != null)
      {
         throw new MetadataException("err.meta.workflow.actionEventAttributes");
      }

      return false;
   }

   /**
    * Loads a workflow event handler attributes.
    * @param element The DOM element from which to load the handler.
    * @param handler The handler to load.
    * @param workflow The containing workflow.
    */
   protected void loadWorkflowHandler(Element element, Handler handler, Flow workflow)
   {
      String sAssociation = XMLUtil.getStringAttr(element, "association");

      if (sAssociation != null)
      {
         for (StringTokenizer tokenizer = new StringTokenizer(sAssociation); tokenizer.hasMoreTokens();)
         {
            handler.addAssociation(handler.getTarget().getAttribute(tokenizer.nextToken()));
         }
      }

      String sEventName = XMLUtil.getReqStringAttr(element, "event");
      int nArgCount = XMLUtil.getIntAttr(element, "args", 0);

      handler.setEvent(sEventName, nArgCount);
      handler.setCondition(m_helper.parse(XMLUtil.getStringAttr(element, "condition"),
         false, workflow.getPosMap(), Boolean.TRUE, m_metadata.getGlobalEnvironment()));
   }

   /**
    * Loads a workflow queue step.
    * @param queueElement The DOM element from which to load the queue step.
    * @param assignment The assignment element, which starts the queue processing.
    * @return The last loaded step after the queue element.
    */
   protected Step loadQueue(Element queueElement, final Assignment assignment)
   {
      final Lookup eventMap = new HashTab();
      final Fork fork = new Fork();
      final Decision decision = new Decision();

      fork.setActivity(assignment.getActivity());
      decision.setActivity(assignment.getActivity());

      XMLUtil.forEachChildElement(queueElement, null,
         m_helper.new ElementHandler("queueEvent")
      {
         private boolean m_bTimer;

         protected String getName(Element eventElement)
         {
            return XMLUtil.getStringAttr(eventElement, "name");
         }

         public void handleElement(Element eventElement, String sEventName)
         {
            Node child = eventElement.getFirstChild();
            String sElement = eventElement.getNodeName();
            Concurrent activity = new Concurrent(fork);
            Branch branch = null;

            if (sElement.equals("TimerEvent"))
            {
               if (m_bTimer)
               {
                  throw new MetadataException("err.meta.workflow.multipleQueueTimers",
                     new Object[]{assignment.getName()});
               }

               Timeout timeout = new Timeout();

               timeout.setActivity(activity);
               timeout.setValue(m_helper.parse(XMLUtil.getReqStringAttr(eventElement, "value"),
                  false, activity.getFlow().getPosMap(), null, m_metadata.getGlobalEnvironment()));
               activity.addStep(timeout);

               Wait wait = new Wait(timeout);

               timeout.setNext(wait);
               activity.addStep(wait);

               m_bTimer = true;
            }
            else if (sElement.equals("ClassEvent"))
            {
               AutoCompletion completion = new AutoCompletion(assignment);

               loadWorkflowHandler(eventElement, completion, activity.getFlow());
               activity.addStep(completion);
            }
            else if (sElement.equals("ManualEvent"))
            {
               Object condition = m_helper.parse(XMLUtil.getStringAttr(eventElement, "condition"),
                  false, assignment.getActivity().getFlow().getPosMap(),
                  Boolean.TRUE, m_metadata.getGlobalEnvironment());
               boolean bTarget = loadTarget(eventElement, assignment, condition, sEventName, false);
               Element first = XMLUtil.findFirstElement(child);

               if (first != null && first.getNodeName().equals("UIAction"))
               {
                  loadTarget(first, assignment, condition, sEventName, bTarget);
                  child = first.getNextSibling();
               }

               if (assignment.getManualCompletion() == null)
               {
                  ManualCompletion completion = new ManualCompletion(assignment);

                  activity.addStep(completion);
               }
               else
               {
                  activity = null;
               }
            }
            else if (sElement.equals("ProcessEvent"))
            {
               branch = new Branch();
               decision.addBranch(branch);

               assignment.setSemaphore(true);

               Semaphore semaphore = new Semaphore(assignment.getName() + ":Semaphore", assignment);

               semaphore.setActivity(activity);
               activity.addStep(semaphore);

               Block block = new Block();

               block.setActivity(branch);
               branch.addStep(block);
               loadActivity(child, block.getContainedActivity());
               block.setCleanupCode(semaphore.getExitCode());
            }
            else
            {
               throw new MetadataException("err.meta.workflow.invalidQueueElement",
                  new Object[]{sElement, assignment.getName()});
            }

            if (eventMap.put(sEventName, Boolean.TRUE) != null)
            {
               throw new MetadataException("err.meta.workflow.queueEventDup",
                  new Object[]{sEventName, assignment.getName()});
            }

            Variable var = assignment.getActivity().getFlow().findVariable(assignment.getName());

            if (var == null)
            {
               var = new Variable(assignment.getName());
               assignment.getActivity().getFlow().addVariable(var);
            }

            if (activity != null)
            {
               Object code;

               if (activity.getStep(0) instanceof ManualCompletion)
               {
                  // ((:state'bind ('<assignment>'targetFunction)) '() (:flow'getToken <step>))
                  code =
                     Pair.list(
                        Pair.list(Symbol._STATE, Pair.quote(Symbol.BIND),
                           Pair.list(Pair.quote(assignment), Pair.quote(Symbol.TARGETFUNCTION))),
                        null,
                        Pair.list(Symbol._FLOW, Pair.quote(Symbol.GETTOKEN), Pair.quote(activity.getStep(0))));
               }
               else
               {
                  code = sEventName;
               }

               Script script = new Script();

               script.setBody(Pair.list(Pair.list(Symbol.SET, var.getSymbol(), code)));
               activity.addStep(script);
               fork.addConcurrent(activity);
            }

            if (branch == null)
            {
               branch = new Branch();
               decision.addBranch(branch);
               loadActivity(child, branch);
            }

            branch.setCondition(Pair.list(Symbol.EQUAL_P, var.getSymbol(), sEventName));
         }
      });

      if (fork.getConcurrentCount() == 0)
      {
         throw new MetadataException("err.meta.workflow.eventlessQueue",
            new Object[]{assignment.getName()});
      }

      if (fork.getConcurrentCount() == 1)
      {
         Activity activity = fork.getConcurrent(0);

         if (activity.getStep(0) instanceof Timeout)
         {
            activity.addStep(new Completion(assignment));
         }

         if (decision.getBranchCount() > 1)
         {
            for (int i = 0; i != activity.getStepCount(); ++i)
            {
               assignment.getActivity().addStep(activity.getStep(i));
            }

            return decision;
         }

         if (decision.getBranch(0).getStepCount() != 0)
         {
            for (int i = 0; i != activity.getStepCount(); ++i)
            {
               assignment.getActivity().addStep(activity.getStep(i));
            }

            activity = decision.getBranch(0);

            for (int i = 0; i != activity.getStepCount() - 1; ++i)
            {
               assignment.getActivity().addStep(activity.getStep(i));
            }
         }
         else
         {
            for (int i = 0; i != activity.getStepCount() - 1; ++i)
            {
               assignment.getActivity().addStep(activity.getStep(i));
            }
         }

         return activity.getStep(activity.getStepCount() - 1);
      }

      assignment.getActivity().addStep(fork);

      Join join = new Join(fork);

      join.setAny(true);
      assignment.getActivity().addStep(join);

      return decision;
   }

   /**
    * Loads a workflow assignment target and form from a DOM element.
    * @param element The DOM element.
    * @param assignment The assignment step.
    * @param condition The target selection condition.
    * @param sBranch The branch name.
    * @param bCheckDup True to check for duplicate target/form attributes in the DOM element.
    * @return True if the target or form attributes have been found.
    */
   protected static boolean loadTarget(Element element, Assignment assignment, Object condition, String sBranch, boolean bCheckDup)
   {
      String sTarget = XMLUtil.getStringAttr(element, "target");
      String sForm = XMLUtil.getStringAttr(element, "form");

      if (sTarget == null && sForm == null)
      {
         if (sBranch != null)
         {
            assignment.addTarget(condition, null, null, Assignment.SDI, sBranch);
         }

         return false;
      }

      if (bCheckDup)
      {
         throw new MetadataException("err.meta.workflow.superfluousUIAction",
            new Object[]{assignment.getName()});
      }

      String sMode = XMLUtil.getStringAttr(element, "uimode", "modeless");
      int nMode;

      if (sMode.equals("modal"))
      {
         nMode = Assignment.MODAL;
      }
      else if (sMode.equals("modeless"))
      {
         nMode = Assignment.MODELESS;
      }
      else if (sMode.equals("sdi"))
      {
         nMode = Assignment.SDI;
      }
      else
      {
         throw new MetadataException("err.meta.uimode", new Object[]{sMode});
      }

      assignment.addTarget(condition, sTarget, sForm, nMode, sBranch);

      return true;
   }

   /**
    * Load extended system metadata.
    */
   protected void loadExtendedSystemMetadata()
   {
   }

   /**
    * Load UIEvents from repository.
    * @param baseDescElement Base repository descriptor element.
    * @param rootDescElement Repository descriptor element.
    * @param progress A progress listener.
    */
   protected void loadUIEventMetadata(Element baseDescElement, Element rootDescElement, ProgressListener progress)
   {
   }

   /**
    * Load extended metadata.
    * @param baseDescElement Base repository descriptor element.
    * @param rootDescElement Repository descriptor element.
    */
   public void loadExtendedMetadata(Element baseDescElement, Element rootDescElement)
   {
   }

   /**
    * Validate extended metadata.
    * @param baseDescElement Base repository descriptor element.
    * @param rootDescElement Repository descriptor element.
    * @param progress A progress listener.
    */
   public void validateExtendedMetadata(ProgressListener progress)
   {
   }

   /**
    * Loads a flow decision step.
    * @param element The DOM element, from which to load the decision.
    * @param decision The decision step.
    * @param helper The XML metadata helper.
    * @param metadata The root metadata object.
    * @param loader The branch loader.
    */
   public static void loadDecision(Element element, final Decision decision,
      final XMLMetadataHelper helper, final Metadata metadata, final BranchLoader loader)
   {
      XMLUtil.forEachChildElement(element, loader.getElementName(), new ElementHandler()
      {
         public void handleElement(Element branchElement)
         {
            Branch branch = loader.createBranch();

            branch.setDecision(decision);
            branch.setCondition(helper.parse(XMLUtil.getStringAttr(branchElement, "condition"),
               false, decision.getActivity().getFlow().getPosMap(), Boolean.TRUE,
               metadata.getGlobalEnvironment()));
            branch.setCaption(XMLUtil.getStringAttr(branchElement, "caption"));

            if (branch.getCaption() == null)
            {
               branch.setCaption(XMLUtil.getStringAttr(branchElement, "name"));
            }

            loader.loadActivity(branchElement, branch);
            decision.addBranch(branch);
         }
      });
   }

   /**
    * Parses the given queue name expression. If it is a symbol and not a flow variable,
    * then treat it as a String. Otherwise, it is an expression.
    *
    * @param sQueueName The queue name expression to parse.
    * @param activity The activity.
    * @param machine The machine for parsing.
    * @return The expression.
    */
   public static Object parseQueueName(String sQueueName, Activity activity, Machine machine)
   {
      if (sQueueName == null)
      {
         return null;
      }

      SchemeParser parser = new SchemeParser(machine.getGlobalEnvironment());
      Reader reader = new TextPositionReader(new StringReader(sQueueName));
      Object expr = parser.parse(reader, activity.getFlow().getPosMap());

      if (expr instanceof Symbol && activity.getFlow().findVariable(((Symbol)expr).getName()) == null)
      {
         return sQueueName;
      }

      return expr;
   }

   /**
    * Loads a flow Semaphore step.
    *
    * @param element The DOM element from which to load the steps to execute within the Semaphore.
    * @param sStepName The Semaphore step name.
    * @param outerActivity The activity that will contain the Semaphore.
    * @param machine The VM for script loading.
    * @param loader The activity loader.
    * @return The outer step.
    */
   public Step loadSemaphore(Element element, String sStepName,
      Activity outerActivity, Machine machine, ActivityLoader loader)
   {
      Assignment assignment = new Assignment(sStepName + ":Assignment", outerActivity,
         parseQueueName(XMLUtil.getStringAttr(element, "queue"), outerActivity, machine),
         m_helper.parse(XMLUtil.getStringAttr(element, "title"),
            false, outerActivity.getFlow().getPosMap(), XMLUtil.getStringAttr(element, "caption"),
            m_metadata.getGlobalEnvironment()));

      assignment.setAssignee(m_helper.parse(XMLUtil.getStringAttr(element, "assignee"),
         false, outerActivity.getFlow().getPosMap(), assignment.getAssignee(), m_metadata.getGlobalEnvironment()));
      assignment.setOwner(m_helper.parse(XMLUtil.getStringAttr(element, "owner"),
         false, outerActivity.getFlow().getPosMap(), assignment.getOwner(), m_metadata.getGlobalEnvironment()));
      assignment.setFactory((Pair)m_helper.parse(XMLUtil.getStringAttr(element, "factory"),
         true, outerActivity.getFlow().getPosMap(), assignment.getFactory(),
         m_metadata.getGlobalEnvironment()));
      assignment.setSemaphore(true);
      outerActivity.addStep(assignment);

      Block block = new Block();

      block.setActivity(outerActivity);
      assignment.setNext(block);

      Activity blockActivity = block.getContainedActivity();

      Semaphore semaphore = new Semaphore(sStepName, assignment);

      semaphore.setActivity(blockActivity);
      blockActivity.addStep(semaphore);
      loader.loadActivity(element, blockActivity);
      block.setCleanupCode(semaphore.getExitCode());

      return block;
   }

   /**
    * Loads a flow try-catch step.
    * @param element The DOM element, from which to load the try-catch.
    * @param tryCatch The TryCatch step.
    * @param outerActivity The activity that will contain the TryCatch.
    * @param helper The XML metadata helper.
    * @param metadata The root metadata object.
    * @param loader The activity loader.
    * @return The outer step.
    */
   public static Step loadTryCatch(Element element, final TryCatch tryCatch,
      final Activity outerActivity, final XMLMetadataHelper helper, final Metadata metadata,
      final ActivityLoader loader)
   {
      final Pair codeHolder = new Pair(null);
      Step outerStep;

      XMLUtil.withFirstChildElement(element, "Finally", false, new ElementHandler()
      {
         public void handleElement(Element finallyElement)
         {
            codeHolder.setHead(
                  helper.parse(helper.getElementValue(finallyElement),
                  true, outerActivity.getFlow().getPosMap(),
                  null, metadata.getGlobalEnvironment()
               )
            );
         }
      });

      if (codeHolder.getHead() != null)
      {
         Block block = new Block(tryCatch.getName() + ".finalizer");

         block.setActivity(outerActivity);
         block.setCleanupCode((Pair)codeHolder.getHead());
         tryCatch.setActivity(block.getContainedActivity());
         block.getContainedActivity().addStep(tryCatch);
         outerStep = block;
      }
      else
      {
         tryCatch.setActivity(outerActivity);
         outerStep = tryCatch;
      }

      String sVariable = XMLUtil.getStringAttr(element, "variable");

      if (sVariable != null)
      {
         tryCatch.setExceptionVariable(outerActivity.getFlow().getVariable(sVariable));
      }

      XMLUtil.withFirstChildElement(element, "Try", false, new ElementHandler()
      {
         public void handleElement(Element tryElement)
         {
            loader.loadActivity(tryElement, tryCatch.getTry());
         }
      });

      loadDecision(element, tryCatch, helper, metadata, new BranchLoader()
      {
         public String getElementName()
         {
            return "Catch";
         }

         public Branch createBranch()
         {
            return new Catch();
         }

         public void loadActivity(Element element, Activity activity)
         {
            String sException = XMLUtil.getStringAttr(element, "exception");

            if (sException != null)
            {
               ((Catch)activity).setException(helper.getClassObject(sException));
            }

            loader.loadActivity(element, activity);
         }
      });

      return outerStep;
   }

   /**
    * Load a Join step.
    * @param element The DOM element, from which to load the Join.
    * @param fork The Fork step.
    * @param sChildName The name of the branch elements.
    * @param outerActivity The activity that will contain the Join.
    * @param helper The XML metadata helper.
    * @param metadata The root metadata object.
    * @param loader The activity loader.
    * @return The join step.
    */
   public static Step loadJoin(Element element, final Fork fork, String sChildName,
      final Activity outerActivity, final XMLMetadataHelper helper, final Metadata metadata,
      final ActivityLoader loader)
   {
      fork.setActivity(outerActivity);

      Join join = new Join(fork);
      String sType = XMLUtil.getStringAttr(element, "type", "all");

      if (sType.equals("all"))
      {
         join.setAny(false);
      }
      else if (sType.equals("any"))
      {
         join.setAny(true);
      }
      else if (sType.equals("parallel"))
      {
         fork.setParallel(true);
      }
      else
      {
         throw new MetadataException("err.meta.workflow.joinType",
            new Object[]{sType, outerActivity.getFlow().getFullName()});
      }

      XMLUtil.forEachChildElement(element, sChildName, new ElementHandler()
      {
         public void handleElement(Element activityElement)
         {
            Concurrent concurrent;

            if (fork.isParallel())
            {
               concurrent = new Parallel(fork, XMLUtil.getStringAttr(activityElement, "variable"),
                  (Pair)helper.parse(XMLUtil.getStringAttr(activityElement, "args"),
                     false, outerActivity.getFlow().getPosMap(), null, metadata.getGlobalEnvironment())
               );
            }
            else
            {
               concurrent = new Concurrent(fork);
            }

            fork.addConcurrent(concurrent);
            loader.loadActivity(activityElement, concurrent);
         }
      });

      outerActivity.addStep(fork);

      return join;
   }

   /**
    * Loads a workflow activity from a DOM element.
    * @param node The first child node of the activity element.
    * @param activity The activity metadata object.
    */
   protected void loadActivity(Node node, final Activity activity)
   {
      for (; node != null; node = node.getNextSibling())
      {
         if (node.getNodeType() != Node.ELEMENT_NODE)
         {
            continue;
         }

         Element element = (Element)node;
         Element nextElement = null;
         String sElement = element.getNodeName();
         String sStepName = XMLUtil.getStringAttr(element, "name");
         int nCookie = m_helper.pushMarker(MetadataValidationException.TYPE_NAME, sElement);
         m_helper.pushMarker("step", sStepName);

         try
         {
            Step step;

            if (sElement.equals("Action"))
            {
               if (isWorkflowHandler(element))
               {
                  step = new Trigger(sStepName, activity);
                  loadWorkflowHandler(element, (Handler)step, activity.getFlow());
               }
               else
               {
                  step = new Script(sStepName);
                  step.setActivity(activity);
               }

               String sCleanup = XMLUtil.getStringAttr(element, "cleanup");

               if (sCleanup != null)
               {
                  for (StringTokenizer tokenizer = new StringTokenizer(sCleanup); tokenizer.hasMoreTokens();)
                  {
                     step.addCleanupAssoc(Symbol.define(tokenizer.nextToken()));
                  }
               }

               ((Scripted)step).setBody((Pair)m_helper.parse(m_helper.getElementValue(element),
                  true, activity.getFlow().getPosMap(), null, m_metadata.getGlobalEnvironment()));
            }
            else if (sElement.equals("Timeout"))
            {
               Timeout timeout = new Timeout(sStepName);

               timeout.setActivity(activity);
               timeout.setValue(m_helper.parse(XMLUtil.getReqStringAttr(element, "value"),
                  false, activity.getFlow().getPosMap(), null, m_metadata.getGlobalEnvironment()));
               activity.addStep(timeout);
               step = new Wait(timeout);
               timeout.setNext(step);
            }
            else if (sElement.equals("Decision"))
            {
               final Decision decision = new Decision(sStepName);

               decision.setActivity(activity);
               decision.setManual(XMLUtil.getBooleanAttr(element, "manual", false));

               loadDecision(element, decision, m_helper, m_metadata, new BranchLoader()
               {
                  public String getElementName()
                  {
                     return "Branch";
                  }

                  public Branch createBranch()
                  {
                     return new Branch();
                  }

                  public void loadActivity(Element element, Activity activity)
                  {
                     XMLMetadataLoader.this.loadActivity(element.getFirstChild(), activity);
                  }
               });

               step = decision;
            }
            else if (sElement.equals("Loop"))
            {
               loadLoop(element, activity, false, m_helper, m_metadata, new ActivityLoader()
               {
                  public void loadActivity(Element element, Activity activity)
                  {
                     XMLMetadataLoader.this.loadActivity(element.getFirstChild(), activity);
                  }
               });
               step = null;
            }
            else if (sElement.equals("TryCatch"))
            {
               TryCatch tryCatch = new TryCatch(sStepName);

               step = loadTryCatch(element, tryCatch, activity, m_helper, m_metadata, new ActivityLoader()
               {
                  public void loadActivity(Element element, Activity activity)
                  {
                     XMLMetadataLoader.this.loadActivity(element.getFirstChild(), activity);
                  }
               });
            }
            else if (sElement.equals("Join"))
            {
               Fork fork = new Fork(sStepName);

               step = loadJoin(element, fork, "Activity", activity, m_helper, m_metadata, new ActivityLoader()
               {
                  public void loadActivity(Element element, Activity activity)
                  {
                     XMLMetadataLoader.this.loadActivity(element.getFirstChild(), activity);
                  }
               });
            }
            else if (sElement.equals("Assignment"))
            {
               Flow workflow = activity.getFlow();
               Assignment assignment = new Assignment(sStepName, activity,
                  parseQueueName(XMLUtil.getStringAttr(element, "queue"), activity, m_machine),
                  m_helper.parse(XMLUtil.getStringAttr(element, "title"),
                     false, workflow.getPosMap(), XMLUtil.getStringAttr(element, "caption"),
                     m_metadata.getGlobalEnvironment()));

               assignment.setPriority(m_helper.parse(XMLUtil.getStringAttr(element, "priority"),
                  false, workflow.getPosMap(), assignment.getPriority(), m_metadata.getGlobalEnvironment()));
               assignment.setAssignee(m_helper.parse(XMLUtil.getStringAttr(element, "assignee"),
                  false, workflow.getPosMap(), assignment.getAssignee(), m_metadata.getGlobalEnvironment()));
               assignment.setOwner(m_helper.parse(XMLUtil.getStringAttr(element, "owner"),
                  false, workflow.getPosMap(), assignment.getOwner(), m_metadata.getGlobalEnvironment()));
               assignment.setFactory((Pair)m_helper.parse(XMLUtil.getStringAttr(element, "factory"),
                  true, workflow.getPosMap(), assignment.getFactory(),
                  m_metadata.getGlobalEnvironment()));

               boolean bTarget = loadTarget(element, assignment, Boolean.TRUE, null, false);

               activity.addStep(assignment);

               if (isWorkflowHandler(element))
               {
                  AutoCompletion unassignment = new AutoCompletion(assignment);

                  loadWorkflowHandler(element, unassignment, workflow);
                  step = unassignment;
               }
               else
               {
                  step = new ManualCompletion(assignment);
               }

               nextElement = XMLUtil.findFirstElement(element.getNextSibling());

               if (nextElement != null)
               {
                  if (nextElement.getNodeName().equals("UIAction"))
                  {
                     loadTarget(element, assignment, Boolean.TRUE, null, bTarget);
                  }
                  else
                  {
                     nextElement = null;
                  }
               }

               assignment.setNext(step);
            }
            else if (sElement.equals("Semaphore"))
            {
               step = loadSemaphore(element, sStepName, activity, m_machine, new ActivityLoader()
               {
                  public void loadActivity(Element element, Activity activity)
                  {
                     XMLMetadataLoader.this.loadActivity(element.getFirstChild(), activity);
                  }
               });
            }
            else if (sElement.equals("Goto"))
            {
               step = new Goto();
               step.setActivity(activity);
            }
            else if (sElement.equals("Queue"))
            {
               final Flow workflow = activity.getFlow();
               Assignment assignment = new Assignment(sStepName, activity,
                  parseQueueName(XMLUtil.getStringAttr(element, "queue"), activity, m_machine),
                  m_helper.parse(XMLUtil.getStringAttr(element, "title"),
                     false, workflow.getPosMap(), XMLUtil.getStringAttr(element, "caption"),
                     m_metadata.getGlobalEnvironment()));

               assignment.setPriority(m_helper.parse(XMLUtil.getStringAttr(element, "priority"),
                  false, workflow.getPosMap(), assignment.getPriority(), m_metadata.getGlobalEnvironment()));
               assignment.setAssignee(m_helper.parse(XMLUtil.getStringAttr(element, "assignee"),
                  false, workflow.getPosMap(), assignment.getAssignee(), m_metadata.getGlobalEnvironment()));
               assignment.setOwner(m_helper.parse(XMLUtil.getStringAttr(element, "owner"),
                  false, workflow.getPosMap(), assignment.getOwner(), m_metadata.getGlobalEnvironment()));
               assignment.setFactory((Pair)m_helper.parse(XMLUtil.getStringAttr(element, "factory"),
                  true, workflow.getPosMap(), assignment.getFactory(),
                  m_metadata.getGlobalEnvironment()));

               activity.addStep(assignment);
               step = loadQueue(element, assignment);
            }
            else if (sElement.equals("UIAction"))
            {
               m_helper.restoreMarker(nCookie);
               throw new MetadataException("err.meta.workflow.misplacedElement",
                  new Object[]{sElement, activity.getFlow().getFullName()});
            }
            else
            {
               step = loadFlowMacroScript(element, sStepName, activity);

               if (step == null)
               {
                  m_helper.restoreMarker(nCookie);
                  throw new MetadataException("err.meta.workflow.invalidElement",
                     new Object[]{sElement, activity.getFlow().getFullName()});
               }
            }

            final String sNextStepName = XMLUtil.getStringAttr(element, "next");

            if (sNextStepName != null)
            {
               if (step == null)
               {
                  step = new Goto();
               }

               final Step fstep = step;

               m_flowFixupList.add(new ContextFixup(getHelper())
               {
                  public void fixup()
                  {
                     fstep.setNext(activity.getFlow().getFlowStep(sNextStepName));
                  }
               });
            }

            if (step != null)
            {
               activity.addStep(step);
            }
         }
         catch (MetadataException e)
         {
            m_helper.addException(e);
         }
         finally
         {
            m_helper.restoreMarker(nCookie);
         }

         if (nextElement != null)
         {
            element = nextElement;
         }
      }
   }

   /**
    * Loads formats from a DOM element.
    * @param fmtElement The DOM element containing the formats.
    */
   protected void loadFormats(Element fmtElement)
   {
      XMLMetadataHelper.verifyRootElement(fmtElement, "Formats");

      XMLUtil.forEachChildElement(fmtElement, "Format",
         m_helper.new ElementHandler("format")
      {
         public void handleElement(Element element, String sName)
         {
            loadFormat(element, sName);
         }
      });
   }

   /**
    * Loads a message format from a DOM element.
    * @param formatElement The DOM element containing the schema.
    * @param sName The persistence engine name.
    */
   protected void loadFormat(Element formatElement, final String sName)
   {
      XMLMetadataHelper.verifyRootElement(formatElement, "Format");

      final Format format = new Format(sName);

      format.setMetadata(m_metadata);
      format.setLoader(m_helper.getClassObject(XMLUtil.getReqStringAttr(formatElement, "loader")));

      String sExporter = XMLUtil.getStringAttr(formatElement, "exporter");

      if (sExporter != null)
      {
         format.setExporter(m_helper.getClassObject(sExporter));
      }

      XMLUtil.withFirstChildElement(formatElement, "Parser", true, new ElementHandler()
      {
         public void handleElement(Element componentElement)
         {
            Component parser = new Component("<MessageParser:" + sName + ">");

            loadComponent(componentElement, parser);
            format.setParser(parser);
         }
      });

      XMLUtil.withFirstChildElement(formatElement, "Formatter", true, new ElementHandler()
      {
         public void handleElement(Element componentElement)
         {
            Component formatter = new Component("<MessageFormatter:" + sName + ">");

            loadComponent(componentElement, formatter);
            format.setFormatter(formatter);
         }
      });

      m_metadata.addFormat(format);
   }

   /**
    * Loads messages from an XSD or WSDL.
    * @param xsdURL The URL to the XSD or WSDL file.
    * @param sName The name of the file being imported.
    * @param messageMap The map into which the imported messages will be placed.
    */
   protected void loadXSD(URL xsdURL, String sName, final Lookup messageMap)
   {
      try
      {
         final String sPrefix = sName;
         Message[] xsdMsgArray = XSDMessageImporter.createMessageParts(xsdURL, m_metadata, new NameResolver()
         {
            private Set m_processedNameSet = new HashHolder();

            public String transform(String sSuggestedName)
            {
               StringBuilder buf = new StringBuilder(sPrefix.length() + sSuggestedName.length() + 1);

               if (sPrefix.length() >= 1)
               {
                  buf.append(sPrefix.substring(0, 1).toUpperCase(Locale.ENGLISH));
                  buf.append(sPrefix, 1, sPrefix.length());
               }

               buf.append('_');
               buf.append(sSuggestedName);

               return buf.toString();
            }

            protected boolean isValid(String sName)
            {
               return !messageMap.contains(sName) && m_processedNameSet.add(sName.toLowerCase(Locale.ENGLISH));
            }
         }, false);

         for (int i = 0; i < xsdMsgArray.length; i++)
         {
            if (messageMap.put(xsdMsgArray[i].getName(), xsdMsgArray[i]) != null)
            {
               throw new MetadataException("err.meta.messageDup",
                  new Object[]
                  {
                     xsdMsgArray[i].getName(),
                     m_metadata.getName()
                  }
               );
            }
         }
      }
      catch (IOException ex)
      {
         throw new MetadataException("err.meta.resourceOpen", new Object[] {sName}, ex);
      }
   }

   /**
    * Loads a message from a DOM element.
    * @param messageElement The DOM element containing the message.
    * @param sName The message name.
    */
   protected void loadMessage(Element messageElement, String sName)
   {
      XMLMetadataHelper.verifyRootElement(messageElement, "Message");

      final Message message = new Message(sName);

      message.setMetadata(m_metadata);
      message.setResourceName(m_helper.getCurResourceName());

      String sFormat = XMLUtil.getStringAttr(messageElement, "format");

      if (sFormat != null)
      {
         message.setFormat(m_metadata.getFormat(sFormat));
      }

      final String sResponse = XMLUtil.getStringAttr(messageElement, "response");

      if (sResponse != null)
      {
         addPreInheritanceMessageFixup(new ContextFixup(getHelper())
         {
            public void fixup()
            {
               message.setResponse(m_metadata.getMessage(sResponse));
            }
         });
      }

      String sDerivation = XMLUtil.getStringAttr(messageElement, "derivation", "virtual");

      if (sDerivation.equals("virtual"))
      {
         message.setDerivation(Message.DERIVATION_VIRTUAL);
      }
      else if (sDerivation.equals("final"))
      {
         message.setDerivation(Message.DERIVATION_FINAL);
      }
      else if (sDerivation.equals("abstract"))
      {
         message.setDerivation(Message.DERIVATION_ABSTRACT);
      }
      else
      {
         throw new MetadataException("err.meta.integration.messageDerivation",
            new Object[] {sDerivation, message.getName()});
      }

      final String sBaseMessage = XMLUtil.getStringAttr(messageElement, "base");

      if (!loadMessageRef(null, message, messageElement, sName))
      {
         CompositeMessagePart compMsgPart = new CompositeMessagePartInstance(sName);

         message.setRoot(compMsgPart);
         loadCompositeMessagePart(messageElement, compMsgPart, message);
      }

      m_metadata.addMessage(message);

      if (sBaseMessage != null)
      {
         addPreInheritanceMessageFixup(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               Message baseMessage = m_metadata.getMessage(sBaseMessage);

               message.setBaseMessage(baseMessage);
               baseMessage.addDerivedMessage(message);
            }
         });
      }
   }

   /**
    * Loads a message part from a DOM element.
    * @param partElement The DOM element.
    * @param part The object to load.
    * @param message The root message object.
    */
   protected void loadMessagePart(Element partElement, MessagePart part, Message message)
   {
      part.setMinCount(XMLUtil.getIntAttr(partElement, "minCount", 0));
      part.setMaxCount(XMLUtil.getIntAttr(partElement, "maxCount", 1));
      part.setDeclarator(message);
      loadDocumentation(partElement, part);

      if (message.getFormat() != null)
      {
         MessagePartMapping mapping = ((XMLMessageMappingLoader)m_helper.getClassInstance(message.getFormat().getLoader()))
            .loadMapping(partElement, message, part, message.getFormat(), this);

         part.setMapping(mapping);

         if (mapping != null)
         {
            mapping.init(part);
         }
      }
   }

   /**
    * Initializes base attributes of a CompositeMessagePart instance
    * @param partElement The DOM element.
    * @param part The object to initialize.
    * @param message The root message object.
    */
   protected void initCompositeMessagePart(Element partElement, final CompositeMessagePart composite, final Message message)
   {
      // Aggregation does not apply to referenced message parts
      if (composite instanceof CompositeMessagePartInstance)
      {
         CompositeMessagePartInstance part = (CompositeMessagePartInstance)composite;
         String sAggregation = XMLUtil.getStringAttr(partElement, "aggregation", "sequential");

         if (sAggregation != null)
         {
            if (sAggregation.equals("sequential"))
            {
               part.setAggregation(CompositeMessagePart.SEQUENTIAL);
            }
            else if (sAggregation.equals("random"))
            {
               part.setAggregation(CompositeMessagePart.RANDOM);
            }
            else if (sAggregation.equals("single"))
            {
               part.setAggregation(CompositeMessagePart.SINGLE);
            }
            else
            {
               throw new MetadataException("err.meta.integration.aggregation",
                  new Object[]{sAggregation});
            }
         }

         part.setLax(XMLUtil.getBooleanAttr(partElement, "lax", part.isLax()));
      }

      loadMessagePart(partElement, composite, message);
   }

   /**
    * Loads a message ref from a DOM element if there exists a "ref" attribute
    * @param composite The parent composite message if any.
    * @param message The root message.
    * @param msgElement The DOM element to check.
    * @param sName The name of the message part to create.
    */
   protected boolean loadMessageRef(final CompositeMessagePart composite, final Message message, Element msgElement, String sName)
   {
      final String sRef = XMLUtil.getStringAttr(msgElement, "ref");

      if (sRef == null)
      {
         return false;
      }

      final CompositeMessagePartRef partRef = new CompositeMessagePartRef(sName);

      if (composite == null)
      {
         message.setRoot(partRef);
      }
      else
      {
         composite.addPart(partRef);
      }

      assert (composite == null) || (composite.getRoot() == message.getRoot());

      initCompositeMessagePart(msgElement, partRef, message);

      int nCookie = m_helper.pushMarker("fullName", partRef.getFullPath());

      try
      {
         addPreInheritanceMessageFixup(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               CompositeMessagePart root = m_metadata.getMessage(sRef).getRoot();

               partRef.setRefPart(root);

               MessagePartMapping rootMapping = root.getMapping();

               if (rootMapping != null)
               {
                  if (partRef.getMapping() == null)
                  {
                     partRef.setMapping(rootMapping);
                  }

                  message.addRef(partRef);
               }
            }
         });
      }
      finally
      {
         m_helper.restoreMarker(nCookie);
      }

      return true;
   }

   /**
    * Loads a composite message part from a DOM element.
    * @param partElement The DOM element.
    * @param part The object to load.
    * @param message The root message object.
    */
   protected void loadCompositeMessagePart(Element partElement, final CompositeMessagePart composite, final Message message)
   {
      initCompositeMessagePart(partElement, composite, message);

      XMLUtil.withFirstChildElement(partElement, "Parts", false, new ElementHandler()
      {
         public void handleElement(Element partsElement)
         {
            XMLUtil.forEachChildElement(partsElement, null,
               m_helper.new ElementHandler("messagePart")
            {
               protected void handleElement(Element element, String sName)
               {
                  if (element.getNodeName().equals("Message"))
                  {
                     if (!loadMessageRef(composite, message, element, sName))
                     {
                        CompositeMessagePart part = new CompositeMessagePartInstance(sName);

                        composite.addPart(part);
                        loadCompositeMessagePart(element, part, message);
                     }
                  }
                  else if (element.getNodeName().equals("Value"))
                  {
                     final PrimitiveMessagePart part = new PrimitiveMessagePart(sName);

                     composite.addPart(part);
                     part.setType(Primitive.parse(XMLUtil.getReqStringAttr(element, "type")));
                     loadMessagePart(element, part, message);

                     XMLUtil.withFirstChildElement(element, "Enumerations", false, new ElementHandler()
                     {
                        public void handleElement(Element enumerationsElement)
                        {
                           part.setLax(XMLUtil.getBooleanAttr(enumerationsElement, "lax", part.isLax()));

                           XMLUtil.forEachChildElement(enumerationsElement, "Enumeration", new ElementHandler()
                           {
                              public void handleElement(Element enumerationElement)
                              {
                                 part.addEnumeration(XMLUtil.getReqStringAttr(enumerationElement, "value"));
                              }
                           });
                        }
                     });
                  }
                  else
                  {
                     throw new MetadataException("err.meta.msgPartElement", new Object[]{element.getNodeName()});
                  }
               }
            });
         }
      });
   }

   /**
    * Calls MessagePartMapping.finish() on the root part of all messages.
    * @param iterator An iterator over the set of messages on which to
    * call finish().
    */
   public static void finishMessagePartMappings(Iterator iterator)
   {
      MetadataCompoundValidationException comp = null;

      while (iterator.hasNext())
      {
         Message message = (Message)iterator.next();
         CompositeMessagePart root = message.getRoot();
         MessagePartMapping mapping = root.getMapping();

         if (mapping != null)
         {
            try
            {
               mapping.finish(root);
            }
            catch (UncheckedException ex)
            {
               if (comp == null)
               {
                  comp = new MetadataCompoundValidationException();
               }

               message.addException(comp, ex);
            }
         }
      }

      if (comp != null)
      {
         throw comp;
      }
   }

   /**
    * Gets a transformation source or destination.
    * @param sName The end-point name.
    * @param sTransformationName The name of the transformation.
    * @return The end-point. The endpoint.
    */
   protected TransformationEndpoint getTransformationEndpoint(String sName, String sTransformationName)
   {
      TransformationEndpoint meta;

      if (sName.startsWith("class:"))
      {
         meta = m_metadata.getMetaclass(sName.substring("class:".length()));
      }
      else
      {
         meta = m_metadata.findMessage(sName);
      }

      if (meta == null)
      {
         meta = m_metadata.findMetaclass(sName);
      }

      if (meta == null)
      {
         throw new MetadataLookupException("err.meta.transformationEndpointLookup", sName, sTransformationName);
      }

      return meta;
   }

   /**
    * Loads a message transformation from a DOM element.
    * @param transformationElement The DOM element containing the message.
    * @param sName The transformation name.
    */
   protected void loadTransformation(Element transformationElement, String sName)
   {
      XMLMetadataHelper.verifyRootElement(transformationElement, "Transformation");

      final Transformation transformation = new Transformation(sName);
      final String sURLPrefix = "transformation:" + sName;

      transformation.setCategory(XMLUtil.getStringAttr(transformationElement, "category"));
      transformation.setResourceName(m_helper.getCurResourceName());
      transformation.setSource(getTransformationEndpoint(XMLUtil.getReqStringAttr(transformationElement, "source"), sName));
      transformation.setDestination(getTransformationEndpoint(XMLUtil.getReqStringAttr(transformationElement, "destination"), sName));

      String sArguments = XMLUtil.getStringAttr(transformationElement, "args");

      if (sArguments != null)
      {
         StringTokenizer tokenizer = new StringTokenizer(sArguments);

         while (tokenizer.hasMoreTokens())
         {
            transformation.addArgument(tokenizer.nextToken());
         }
      }

      final String sBase = XMLUtil.getStringAttr(transformationElement, "base");

      if (sBase != null)
      {
         addTransformationFixup(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               Transformation baseTransformation = m_metadata.getTransformation(sBase);

               transformation.setBaseTransformation(baseTransformation);
               baseTransformation.addDerivedTransformation(transformation);
            }
         });
      }

      String sDerivation = XMLUtil.getStringAttr(transformationElement, "derivation", "virtual");

      if (sDerivation.equals("virtual"))
      {
         transformation.setDerivation(Transformation.DERIVATION_VIRTUAL);
      }
      else if (sDerivation.equals("final"))
      {
         transformation.setDerivation(Transformation.DERIVATION_FINAL);
      }
      else if (sDerivation.equals("abstract"))
      {
         transformation.setDerivation(Transformation.DERIVATION_ABSTRACT);
      }
      else
      {
         throw new MetadataException("err.meta.integration.transformationDerivation",
            new Object[] {sDerivation, transformation.getName()});
      }

      transformation.setPrimary(XMLUtil.getBooleanAttr(transformationElement, "primary", true));

      XMLUtil.withFirstChildElement(transformationElement, "Initializer", false, new ElementHandler()
      {
         public void handleElement(Element initializerElement)
         {
            transformation.setInitializer((Pair)m_helper.parse(m_helper.getElementValue(initializerElement),
               true, sURLPrefix + ".initializer", transformation.getPosMap(), null, m_metadata.getGlobalEnvironment()));
         }
      });

      XMLUtil.withFirstChildElement(transformationElement, "Mappings", false, new ElementHandler()
      {
         public void handleElement(Element mappingsElement)
         {
            XMLUtil.forEachChildElement(mappingsElement, "Mapping",
               m_helper.new ElementHandler("mapping")
            {
               private int m_nOrdinal;

               public void handleElement(Element mappingElement, String sMappingName)
               {
                  TransformationMapping mapping = new TransformationMapping(sMappingName);

                  if (sMappingName == null)
                  {
                     sMappingName = String.valueOf(m_nOrdinal);
                  }

                  loadTransformationMapping(mappingElement, sURLPrefix + ".mapping." + sMappingName, mapping, transformation);
                  transformation.addMapping(mapping);

                  m_nOrdinal += 1;
               }

               protected String getName(Element element)
               {
                  return XMLUtil.getStringAttr(element, "name");
               }
            });
         }
      });

      XMLUtil.withFirstChildElement(transformationElement, "Finalizer", false,new ElementHandler()
      {
         public void handleElement(Element finalizerElement)
         {
            transformation.setFinalizer((Pair)m_helper.parse(m_helper.getElementValue(finalizerElement),
               true, sURLPrefix + ".finalizer", transformation.getPosMap(), null, m_metadata.getGlobalEnvironment()));
         }
      });

      m_metadata.addTransformation(transformation);
   }

   /**
    * Loads a transformation mapping from a DOM element.
    * @param mappingElement The DOM element containing the mapping.
    * @param sCodeURL The code URL for this element
    * @param mapping The mapping to load.
    * @param transformation The containing transformation.
    */
   protected void loadTransformationMapping(Element mappingElement,
      final String sCodeURL, final TransformationMapping mapping, final Transformation transformation)
   {
      mapping.setRootPart(transformation.getDestination());
      mapping.setCondition(m_helper.parse(XMLUtil.getStringAttr(mappingElement, "condition"),
         false, sCodeURL + "$condition", transformation.getPosMap(), mapping.getCondition(), m_metadata.getGlobalEnvironment()));

      String sDestination = XMLUtil.getStringAttr(mappingElement, "destination");

      if (sDestination != null)
      {
         StringTokenizer tokenizer = new StringTokenizer(sDestination);

         while (tokenizer.hasMoreTokens())
         {
            String sPartName = tokenizer.nextToken();
            boolean bFixed = sPartName.endsWith("$");

            if (bFixed)
            {
               sPartName = sPartName.substring(0, sPartName.length() - 1);
            }

            mapping.addDestination(sPartName, bFixed);
         }
      }

      XMLUtil.withFirstChildElement(mappingElement, "Sources", false, new ElementHandler()
      {
         public void handleElement(Element sourcesElement)
         {
            XMLUtil.forEachChildElement(sourcesElement, "Source",
               m_helper.new ElementHandler("source")
            {
               public void handleElement(Element sourceElement, final String sSourceName)
               {
                  String sSource = XMLUtil.getStringAttr(sourceElement, "source");
                  final String sMapping = XMLUtil.getStringAttr(sourceElement, "mapping");

                  if (sSource != null && sMapping != null)
                  {
                     throw new MetadataException("err.meta.transformation.bothSourceMapping",
                        new Object[]{mapping.getName(), transformation.getName()});
                  }

                  final TransformationArgument argument = new TransformationArgument(sSourceName);

                  if (sMapping == null)
                  {
                     if (sSource == null)
                     {
                        sSource = "";
                     }

                     StringTokenizer tokenizer = new StringTokenizer(sSource);
                     TransformationSource source = transformation.getRoot();

                     while (tokenizer.hasMoreTokens())
                     {
                        String sPartName = tokenizer.nextToken();
                        boolean bFixed = sPartName.endsWith("$");

                        if (bFixed)
                        {
                           sPartName = sPartName.substring(0, sPartName.length() - 1);
                        }

                        source = source.addChild(sPartName);

                        if (bFixed)
                        {
                           mapping.setFixedSource(source, true);
                        }
                     }

                     argument.setSource(source);
                  }
                  else
                  {
                     m_postInheritanceTransformationFixupList.add(new ContextFixup(getHelper())
                     {
                        public void fixup()
                        {
                           argument.setMapping(transformation, sMapping);
                        }
                     });
                  }

                  argument.setNull(XMLUtil.getBooleanAttr(sourceElement, "null", argument.isNull()));
                  argument.setDefaultValue(m_helper.parse(XMLUtil.getStringAttr(sourceElement, "default"), false,
                     sCodeURL + '.' + argument.getName(), transformation.getPosMap(), argument.getDefaultValue(),
                     m_metadata.getGlobalEnvironment()));
                  mapping.addArgument(argument);
               }

               protected String getName(Element element)
               {
                  return XMLUtil.getStringAttr(element, "name");
               }
            });
         }
      });

      Element scriptElement = XMLUtil.findChildElement(mappingElement, "Script");
      Element transformElement = XMLUtil.findChildElement(mappingElement, "Transformation");

      if (scriptElement != null && transformElement != null)
      {
         throw new MetadataException("err.meta.transformation.bothScriptTransformation",
            new Object[]{mapping.getName(), transformation.getName()});
      }

      if (scriptElement != null)
      {
         mapping.setScript((Pair)m_helper.parse(m_helper.getElementValue(scriptElement),
            true, sCodeURL, transformation.getPosMap(), null, m_metadata.getGlobalEnvironment()));
      }

      if (transformElement != null)
      {
         mapping.setTransformation(m_helper.parse(XMLUtil.getReqStringAttr(transformElement, "ref"),
            false, transformation.getPosMap(), null, m_metadata.getGlobalEnvironment()));
         mapping.setTransformationArguments(m_helper.parse(XMLUtil.getStringAttr(transformElement, "args"),
            true, transformation.getPosMap(), null, m_metadata.getGlobalEnvironment()));
         mapping.setTransformationInput(m_helper.parse(XMLUtil.getStringAttr(transformElement, "source"),
            false, transformation.getPosMap(), null, m_metadata.getGlobalEnvironment()));
      }
   }

   /**
    * Loads a service interface from a DOM element.
    * @param interfaceElement The DOM element containing the interface definition.
    * @param sInterfaceName The interface name.
    */
   protected void loadInterface(Element interfaceElement, String sInterfaceName)
   {
      XMLMetadataHelper.verifyRootElement(interfaceElement, "Interface");
      XMLMetadataHelper.validateName(sInterfaceName);

      final Interface iface = m_metadata.defineInterface(sInterfaceName, null);

      if (!iface.isForward())
      {
         m_metadata.addInterface(iface);
      }

      iface.setForward(false);
      iface.setResourceName(m_helper.getCurResourceName());

      String sFormatName = XMLUtil.getStringAttr(interfaceElement, "format");

      if (sFormatName != null)
      {
         iface.setFormat(m_metadata.getFormat(sFormatName));
      }

      XMLUtil.withFirstChildElement(interfaceElement, "Requests", false, new ElementHandler()
      {
         public void handleElement(Element requestsElement)
         {
            XMLUtil.forEachChildElement(requestsElement, "Request", m_helper.new ElementHandler("request")
            {
               public void handleElement(Element requestElement, String sMessageName)
               {
                  iface.addRequest(m_metadata.getMessage(sMessageName));
               }

               protected String getName(Element element)
               {
                  return XMLUtil.getReqStringAttr(element, "message");
               }
            });
         }
      });

      XMLUtil.withFirstChildElement(interfaceElement, "Responses", false, new ElementHandler()
      {
         public void handleElement(Element requestsElement)
         {
            XMLUtil.forEachChildElement(requestsElement, "Response", m_helper.new ElementHandler("response")
            {
               public void handleElement(Element requestElement, String sMessageName)
               {
                  iface.addResponse(m_metadata.getMessage(sMessageName));
               }

               protected String getName(Element element)
               {
                  return XMLUtil.getReqStringAttr(element, "message");
               }
            });
         }
      });

      addSingletonFixup(new ContextFixup(m_helper)
      {
         public void fixup()
         {
            MessageTable table = iface.getRequestTable();

            if (table.getFormat() != null)
            {
               ((MessageParser)table.getFormat().getParser().getInstance(null)).initializeMessageTable(table);
            }

            table = iface.getResponseTable();

            if (table.getFormat() != null)
            {
               ((MessageParser)table.getFormat().getParser().getInstance(null)).initializeMessageTable(table);
            }
         }
      });
   }

   /**
    * Loads a service from a DOM element.
    * @param serviceElement The DOM element containing the flow definition.
    * @param sServiceName The service name.
    */
   protected void loadService(Element serviceElement, String sServiceName)
   {
      XMLMetadataHelper.verifyRootElement(serviceElement, "Service");

      int nVersion = 0;
      int i = sServiceName.lastIndexOf('.');

      if (i > 0)
      {
         try
         {
            nVersion = Integer.parseInt(sServiceName.substring(i + 1));
         }
         catch (NumberFormatException e)
         {
            throw new MetadataException("err.meta.service.version", new Object[]{sServiceName.substring(i + 1)}, e);
         }

         sServiceName = sServiceName.substring(0, i);
      }

      XMLMetadataHelper.validateName(sServiceName);

      final Service service = new Service(sServiceName, nVersion, m_metadata);

      service.setResourceName(m_helper.getCurResourceName());
      service.setPrivileged(XMLUtil.getBooleanAttr(serviceElement, "privileged", service.isPrivileged()));
      service.setCaption(XMLUtil.getStringAttr(serviceElement, "caption"));

      String sInterface = XMLUtil.getStringAttr(serviceElement, "interface");

      if (sInterface != null)
      {
         service.setInterface(m_metadata.defineInterface(sInterface, service));
      }

      String sArgList = XMLUtil.getStringAttr(serviceElement, "args");

      if (sArgList != null)
      {
         for (StringTokenizer tokenizer = new StringTokenizer(sArgList); tokenizer.hasMoreTokens();)
         {
            String sArgName = tokenizer.nextToken();

            XMLMetadataHelper.validateName(sArgName);
            service.addArgument(new Variable(sArgName));
         }
      }

      String sVarList = XMLUtil.getStringAttr(serviceElement, "variables");

      if (sVarList != null)
      {
         for (StringTokenizer tokenizer = new StringTokenizer(sVarList); tokenizer.hasMoreTokens();)
         {
            String sVarName = tokenizer.nextToken();

            XMLMetadataHelper.validateName(sVarName);
            service.addVariable(new Variable(sVarName));
         }
      }

      service.setPrivilege(getPrivilege(serviceElement, "privilege"));
      m_flowFixupList = new ArrayList();
      loadSequence(serviceElement, service);
      m_metadata.addService(service);
      m_helper.fixup(m_flowFixupList.iterator());
      m_flowFixupList = null;

      m_persistenceMappingFixupList.add(new ContextFixup(m_helper)
      {
         public void fixup()
         {
            service.resolve(m_machine);
         }
      });
   }

   /**
    * Loads a message flow sequence from a DOM element.
    * @param sequenceElement The DOM element containing the sequence.
    * @param activity The activity metadata object.
    */
   protected void loadSequence(Element sequenceElement, final Activity activity)
   {
      XMLUtil.forEachChildElement(sequenceElement, null, m_helper.new ElementHandler("step")
      {
         public void handleElement(Element element, String sStepName)
         {
            String sElement = element.getNodeName();
            Step step;

            if (sElement.equals("Script"))
            {
               Script script = new Script(sStepName);

               script.setActivity(activity);
               script.setBody((Pair)m_helper.parse(m_helper.getElementValue(element),
                  true, activity.getFlow().getPosMap(), Jump.BODY, m_metadata.getGlobalEnvironment()));
               step = script;
            }
            else if (sElement.equals("Transform"))
            {
               Transform transform = new Transform(sStepName);

               transform.setActivity(activity);
               transform.setTransformationExpression(m_helper.parse(XMLUtil.getReqStringAttr(element, "transformation"),
                  false, activity.getFlow().getPosMap(), null, m_metadata.getGlobalEnvironment()));
               transform.setTransformationArguments(m_helper.parse(XMLUtil.getStringAttr(element, "args", null),
                  true, activity.getFlow().getPosMap(), null, m_metadata.getGlobalEnvironment()));
               step = transform;
            }
            else if (sElement.equals("Dispatch"))
            {
               final Dispatch dispatch = new Dispatch(sStepName, activity);

               loadDecision(element, dispatch, m_helper, m_metadata, new BranchLoader()
               {
                  public String getElementName()
                  {
                     return "Case";
                  }

                  public Branch createBranch()
                  {
                     return new Case();
                  }

                  public void loadActivity(Element element, Activity activity)
                  {
                     String sMessage = XMLUtil.getStringAttr(element, "message");

                     if (sMessage != null)
                     {
                        ((Case)activity).setMessage(m_metadata.getMessage(sMessage));
                     }

                     loadSequence(element, activity);
                  }
               });

               step = dispatch;
            }
            else if (sElement.equals("Loop"))
            {
               loadLoop(element, activity, true, m_helper, m_metadata, new ActivityLoader()
               {
                  public void loadActivity(Element element, Activity activity)
                  {
                     loadSequence(element, activity);
                  }
               });
               step = null;
            }
            else if (sElement.equals("TryCatch"))
            {
               TryCatch tryCatch = new TryCatch(sStepName);

               step = loadTryCatch(element, tryCatch, activity, m_helper, m_metadata, new ActivityLoader()
               {
                  public void loadActivity(Element element, Activity activity)
                  {
                     loadSequence(element, activity);
                  }
               });
            }
            else if (sElement.equals("Persist"))
            {
               final Persist persist = new Persist(sStepName, getOnError(element));

               persist.setActivity(activity);
               persist.setRespond(XMLUtil.getBooleanAttr(element, "respond", persist.isRespond()));
               step = persist;
            }
            else if (sElement.equals("Sync"))
            {
               step = new Sync(sStepName,
                  m_helper.parse(XMLUtil.getReqStringAttr(element, "link"), false, activity.getFlow().getPosMap(), null, m_metadata.getGlobalEnvironment()),
                  (Pair)m_helper.parse(m_helper.getElementValue(element), true, activity.getFlow().getPosMap(), null, m_metadata.getGlobalEnvironment()),
                  getOnError(element));
            }
            else if (sElement.equals("Send"))
            {
               Send send = new Send(sStepName, activity);
               String sOutput = XMLUtil.getStringAttr(element, "output");
               String sInterface = XMLUtil.getStringAttr(element, "interface");

               send.setOutputExpression(m_helper.parse(sOutput, false, activity.getFlow().getPosMap(), null, m_metadata.getGlobalEnvironment()));

               if (sInterface != null)
               {
                  send.setInterface(m_metadata.defineInterface(sInterface, send));
               }

               step = send;
            }
            else if (sElement.equals("SendReceive"))
            {
               SendReceive send = new SendReceive(sStepName, activity);
               String sOutput = XMLUtil.getStringAttr(element, "output");

               if (sOutput != null)
               {
                  Object outputExpression = m_helper.parse(sOutput, false, activity.getFlow().getPosMap(), Boolean.TRUE, m_metadata.getGlobalEnvironment());

                  send.setOutputExpression(outputExpression);
               }

               String sInterface = XMLUtil.getStringAttr(element, "interface");

               if (sInterface != null)
               {
                  send.setInterface(m_metadata.defineInterface(sInterface, send));
               }

               step = send;
            }
            else if (sElement.equals("Semaphore"))
            {
               step = loadSemaphore(element, sStepName, activity, m_machine, new ActivityLoader()
               {
                  public void loadActivity(Element element, Activity activity)
                  {
                     loadSequence(element, activity);
                  }
               });
            }
            else if (sElement.equals("Join"))
            {
               Fork fork = new Fork(sStepName);

               step = loadJoin(element, fork, "Sequence", activity, m_helper, m_metadata, new ActivityLoader()
               {
                  public void loadActivity(Element element, Activity activity)
                  {
                     XMLMetadataLoader.this.loadSequence(element, activity);
                  }
               });
            }
            else if (sElement.equals("Goto"))
            {
               step = new Jump();
               step.setActivity(activity);
            }
            else
            {
               step = loadFlowMacroScript(element, sStepName, activity);

               if (step == null)
               {
                  throw new MetadataException("err.meta.service.invalidElement",
                     new Object[]{sElement, activity.getFlow().getFullName()});
               }
            }

            final String sNextStepName = XMLUtil.getStringAttr(element, "next");

            if (sNextStepName != null)
            {
               if (step == null)
               {
                  step = new Jump();
               }

               final Step fstep = step;

               m_flowFixupList.add(new ContextFixup(getHelper())
               {
                  public void fixup()
                  {
                     fstep.setNext(activity.getFlow().getFlowStep(sNextStepName));
                  }
               });
            }

            if (step != null)
            {
               activity.addStep(step);
            }
         }

         /**
          * Load onError value for the Persist step
          * @param persist step to be configured
          * @param element element to load configuration from
          */
         private byte getOnError(Element element)
         {
            String sOnError = XMLUtil.getStringAttr(element, "onError", "fail");
            byte nOnError = Persist.FAIL_ON_ERROR;

            if (sOnError.equals("collect"))
            {
               nOnError = Persist.COLLECT_ON_ERROR;
            }
            else if (sOnError.equals("commit"))
            {
               nOnError = Persist.COMMIT_ON_ERROR;
            }

            return nOnError;
         }

         protected String getName(Element element)
         {
            return XMLUtil.getStringAttr(element, "name");
         }
      });
   }

   /**
    * Loads a flow loop step:
    *
    *                                        +--------------------------------------------------------------+
    *                                        |                                                              |
    *                                        v                                                              |
    *       +---------------+       +-----------------+      +---------------+                              |
    * ----->| LoopName:init |------>| LoopName:branch |----->| LoopName:next |-----> Inside <Loop> Tag -----+
    *       +---------------+       +-----------------+      +---------------+
    *                                        |
    *                                        +------> Step after <Loop> tag
    *
    * @param element The loop step element.
    * @param activity The activity that will contain the loop.
    * @param bDefaultToThis True if "collection" and "variable" attributes should default to "this".
    * @param helper The XML metadata helper.
    * @param metadata The root metadata object.
    * @param loader The activity loader.
    */
   public static void loadLoop(Element element, Activity activity, boolean bDefaultToThis,
      XMLMetadataHelper helper, Metadata metadata, ActivityLoader loader)
   {
      String sCollectionExpression;
      String sItemVariableName;

      if (bDefaultToThis)
      {
         sCollectionExpression = XMLUtil.getStringAttr(element, "collection", "this");
         sItemVariableName = XMLUtil.getStringAttr(element, "variable", "this");
      }
      else
      {
         sCollectionExpression = XMLUtil.getReqStringAttr(element, "collection");
         sItemVariableName = XMLUtil.getReqStringAttr(element, "variable");
      }

      Variable itemVar = activity.getFlow().findVariable(sItemVariableName);

      if (itemVar == null)
      {
         itemVar = new Variable(sItemVariableName);
         activity.getFlow().addVariable(itemVar);
      }

      String sLoopName = XMLUtil.getReqStringAttr(element, "name");
      Object collectionExpression = helper.parse(sCollectionExpression,
         false, activity.getFlow().getPosMap(), null, metadata.getGlobalEnvironment()
      );
      Variable collectionVar;
      boolean bUseOldCollectionVar = (collectionExpression instanceof Symbol) && !Symbol.THIS.equals(collectionExpression);

      // If collection is already stored in a variable, do not create a new one.
      if (bUseOldCollectionVar)
      {
         collectionVar = activity.getFlow().getVariable(((Symbol)collectionExpression).getName());
      }
      else
      {
         collectionVar = new Variable(sLoopName + ":collection");
         activity.getFlow().addVariable(collectionVar);
      }

      Variable indexVar = new Variable(sLoopName + ":var");

      activity.getFlow().addVariable(indexVar);

      Symbol collectionSym = collectionVar.getSymbol();
      Symbol indexSym = indexVar.getSymbol();
      Symbol itemSym = itemVar.getSymbol();

      // Loop initializer step
      Script initializer = new Script(sLoopName + ":init");

      initializer.setActivity(activity);
      initializer.setBody(new Pair(
         Pair.list(Symbol.SET, indexSym,
            Pair.list(Symbol.COND,
               Pair.list(Pair.list(Symbol.PAIR_P, collectionSym), collectionSym),
               Pair.list(Pair.list(Symbol.VECTOR_P, collectionSym), Primitive.ZERO_INTEGER)
            )
         ),
         activity.getFlow().getDefaultReturnCode()
      ));

      if (!bUseOldCollectionVar)
      {
         initializer.setBody(new Pair(
            Pair.list(Symbol.SET, collectionSym, collectionExpression),
            initializer.getBody()
         ));
      }

      activity.addStep(initializer);

      // Loop test step
      Decision decision = new Decision(sLoopName + ":branch");
      Branch loopBody = new Branch();

      decision.setActivity(activity);
      loopBody.setDecision(decision);
      loopBody.setCondition(Pair.list(Symbol.COND,
         Pair.list(Pair.list(Symbol.PAIR_P, collectionSym),
            Pair.list(Symbol.NOT, Pair.list(Symbol.NULL_P, indexSym))),
         Pair.list(Pair.list(Symbol.VECTOR_P, collectionSym),
            Pair.list(Symbol.LT, indexSym, Pair.list(Symbol.VECTOR_LENGTH, collectionSym))),
         Pair.list(Symbol.ELSE,
            Pair.list(collectionSym, Pair.quote(XMLMetadataLoader.HAS_NEXT)))
      ));

      // Get current item and advance to next step
      Script getItem = new Script(sLoopName + ":next");

      getItem.setActivity(loopBody);
      getItem.setBody(new Pair(
         Pair.list(Symbol.COND,
            Pair.list(Pair.list(Symbol.PAIR_P, collectionSym),
               Pair.list(Symbol.SET, itemSym, Pair.list(Symbol.CAR, indexSym)),
               Pair.list(Symbol.SET, indexSym, Pair.list(Symbol.CDR, indexSym))
            ),
            Pair.list(Pair.list(Symbol.VECTOR_P, collectionSym),
               Pair.list(Symbol.SET, itemSym, Pair.list(collectionSym, indexSym)),
               Pair.list(Symbol.SET, indexSym, Pair.list(Symbol.PLUS, indexSym, Primitive.ONE_INTEGER))
            ),
            Pair.list(Symbol.ELSE,
               Pair.list(Symbol.SET, itemSym, Pair.list(collectionSym, Pair.quote(XMLMetadataLoader.NEXT)))
            )
         ),
         activity.getFlow().getDefaultReturnCode()
      ));
      loopBody.addStep(getItem);

      // Load loop body
      loader.loadActivity(element, loopBody);

      loopBody.getStep(loopBody.getStepCount() - 1).setNext(decision);
      decision.addBranch(loopBody);

      // Branch to follow when done
      Branch doneBranch = new Branch();

      doneBranch.setDecision(decision);
      doneBranch.setCondition(Boolean.TRUE);
      decision.addBranch(doneBranch);

      // Finish the loop expansion
      activity.addStep(decision);
   }

   /**
    * Loads the data-source-specific persistence mapping details from a DOM element.
    * @param element The DOM element containing the mapping.
    * @param metaclass The class for which the mapping applies.
    * @return The loaded persistence mapping object.
    */
   public PersistenceMapping loadPersistenceMappingDetails(Element element, Metaclass metaclass)
   {
      String sDataSource = XMLUtil.getReqStringAttr(element, "dataSource");
      DataSource dataSource = m_metadata.getDataSource(sDataSource);
      return ((XMLPersistenceMetadataLoader)m_helper.getClassInstance(dataSource.getType().getLoader()))
         .loadMapping(element, metaclass, dataSource, XMLMetadataLoader.this);
   }

   /**
    * Loads a persistence mapping from a DOM element.
    * @param element The DOM element containing the mapping.
    * @param metaclass The class for which the mapping applies.
    * @return The loaded persistence mapping object.
    */
   public PersistenceMapping loadPersistenceMapping(Element element, Metaclass metaclass)
   {
      PersistenceMapping mapping = loadPersistenceMappingDetails(element, metaclass);

      String sLockingAttribute = XMLUtil.getStringAttr(element, "lockingAttribute");

      if (sLockingAttribute != null)
      {
         mapping.setLockingAttribute(metaclass.getAttribute(sLockingAttribute));
      }

      String sTypeCodeAttribute = XMLUtil.getStringAttr(element, "classCodeAttribute");

      if (sTypeCodeAttribute != null)
      {
         mapping.setTypeCodeAttribute(metaclass.getAttribute(sTypeCodeAttribute));
      }

      mapping.setTypeCodeForced(XMLUtil.getBooleanAttr(element, "classCodeForced", false));

      String sFragmentAttribute = XMLUtil.getStringAttr(element, "fragmentAttribute");

      if (sFragmentAttribute != null)
      {
         mapping.setFragmentAttribute(metaclass.getAttribute(sFragmentAttribute));
      }

      String sFragmentReplication = XMLUtil.getStringAttr(element, "fragmentReplication");

      if (sFragmentReplication != null)
      {
         if (sFragmentReplication.equals("unicast"))
         {
            mapping.setFragmentReplication(PersistenceMapping.REPLICATION_UNICAST);
         }
         else if (sFragmentReplication.equals("broadcast"))
         {
            mapping.setFragmentReplication(PersistenceMapping.REPLICATION_BROADCAST);
         }
         else
         {
            throw new MetadataException("err.meta.fragmentReplication",
               new Object[]{sFragmentReplication, metaclass.getName()});
         }
      }

      String sCaching = XMLUtil.getStringAttr(element, "caching");

      if (sCaching != null)
      {
         if (sCaching.equals("instance"))
         {
            mapping.setCaching(PersistenceMapping.CACHING_INSTANCE);
         }
         else if (sCaching.equals("class"))
         {
            mapping.setCaching(PersistenceMapping.CACHING_CLASS);
         }
         else
         {
            throw new MetadataException("err.meta.caching",
               new Object[]{sCaching, metaclass.getName()});
         }
      }

      return mapping;
   }

   /**
    * Loads a component from a DOM element.
    * @param componentElement The DOM element containing the component.
    * @param sName The component name.
    */
   protected void loadComponent(Element componentElement, String sName)
   {
      XMLMetadataHelper.verifyRootElement(componentElement, "Component");

      Component component = new Component(sName);

      component.setMetadata(m_metadata);
      loadComponent(componentElement, component);
      m_metadata.addComponent(component);
   }

   /**
    * Loads a component from a DOM element.
    * @param componentElement The DOM element containing the component.
    * @param component The component instance.
    */
   public void loadComponent(Element componentElement, final Component component)
   {
      String sActivation = XMLUtil.getReqStringAttr(componentElement, "activation");

      if (sActivation.equals("singleton"))
      {
         component.setActivation(Component.SINGLETON);

         addSingletonFixup(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               component.getInstance(null);
            }
         });
      }
      else if (sActivation.equals("context"))
      {
         component.setActivation(Component.CONTEXT);
      }
      else if (sActivation.equals("new"))
      {
         component.setActivation(Component.NEW);
      }
      else
      {
         throw new MetadataException("err.meta.componentActivation", new Object[]{sActivation});
      }

      if (!isRuntimeExcluded())
      {
         component.setType(m_helper.getClassObject(XMLUtil.getReqStringAttr(componentElement, "type")));
      }

      XMLUtil.withFirstChildElement(componentElement, "Factory", false, new ElementHandler()
      {
         public void handleElement(Element factoryElement)
         {
            int nCookie = m_helper.pushMarker("factory", "true");

            try
            {
               Component factory = new Component(component.getName() + ".<factory>");

               factory.setMetadata(m_metadata);
               loadComponent(factoryElement, factory);

               if (!m_bRuntimeExcluded)
               {
                  component.setFactory(factory, XMLUtil.getStringAttr(factoryElement, "method", "create"));
               }
            }
            finally
            {
               m_helper.restoreMarker(nCookie);
            }
         }
      });

      XMLUtil.withFirstChildElement(componentElement, "Properties", false, new ElementHandler()
      {
         public void handleElement(Element propertiesElement)
         {
            loadComponentProperties(propertiesElement, component);
         }
      });

      component.validate(m_metadata, m_helper.getWarnings());
   }

   /**
    * Loads a component properties from a DOM element.
    * @param propertiesElement The DOM element containing the properties.
    * @param component The component into which to load the properties.
    */
   public void loadComponentProperties(Element propertiesElement, final Component component)
   {
      XMLUtil.forEachChildElement(propertiesElement, null,
         m_helper.new ElementHandler("property")
      {
         public void handleElement(Element propertyElement, String sName)
         {
            boolean bCollection;

            if (propertyElement.getNodeName().equals("Property"))
            {
               bCollection = false;
            }
            else if (propertyElement.getNodeName().equals("Collection"))
            {
               bCollection = true;
            }
            else
            {
               return;
            }

            XMLMetadataHelper.validateName(sName);

            if (!m_helper.isElementEnabled(propertyElement) || component.getType() == null)
            {
               return;
            }

            Method method = component.getPropertyMethod(sName, bCollection);
            final Class type = method.getParameterTypes()[0];
            final PropertyParser parser = findPropertyParser(type);
            final PropertyInitializer initializer;

            if (parser == null)
            {
               // Assuming a component

               if (bCollection)
               {
                  initializer = new ComponentCollectionPropertyInitializer(sName, type, method, component);

                  XMLUtil.forEachChildElement(propertyElement, "Item", new ElementHandler()
                  {
                     public void handleElement(Element element)
                     {
                        if (m_helper.isElementEnabled(element))
                        {
                           loadComponentProperty(element, initializer, type);
                        }
                     }
                  });
               }
               else
               {
                  initializer = new ComponentPropertyInitializer(sName, type, method, component);
                  loadComponentProperty(propertyElement, initializer, type);
               }
            }
            else
            {
               if (bCollection)
               {
                  initializer = new PrimitiveCollectionPropertyInitializer(sName, type, method, component);

                  XMLUtil.forEachChildElement(propertyElement, "Item", new ElementHandler()
                  {
                     public void handleElement(Element element)
                     {
                        if (m_helper.isElementEnabled(element))
                        {
                           loadPrimitiveProperty(element, initializer, parser, type);
                        }
                     }
                  });
               }
               else
               {
                  initializer = new PrimitivePropertyInitializer(sName, type, method, component);
                  loadPrimitiveProperty(propertyElement, initializer, parser, type);
               }
            }

            component.addPropertyInitializer(initializer);
         }
      });
   }

   /**
    * Loads a primitive property value from a DOM element.
    * @param propertyElement The DOM element containing the value.
    * @param initializer The property initializer where to set the configured value.
    * @param parser The property value parser.
    * @param type The property value type.
    */
   protected void loadPrimitiveProperty(Element propertyElement,
      final PropertyInitializer initializer,
      PropertyParser parser, Class type)
   {
      Element element = findComponentElement(propertyElement);

      if (element == null)
      {
         parser.parse(m_helper.getElementValue(propertyElement), initializer, this);
      }
      else
      {
         if (!type.equals(Component.class))
         {
            throw new MetadataException("err.meta.componentElement",
               new Object[]{initializer.getName(), initializer.getComponent().getName()});
         }

         final Component component = new Component(initializer.getComponent().getName() +
            "/" + initializer.getName());

         component.setMetadata(m_metadata);
         loadComponent(element, component);

         m_componentFixupList.add(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               initializer.initializeValue(component);
            }
         });
      }
   }

   /**
    * Loads a component property (one that is initialized with a
    * component instance) from a DOM element.
    * @param propertyElement The DOM element containing the value.
    * @param initializer The property initializer where to set the configured value.
    * @param type The property value type.
    */
   protected void loadComponentProperty(Element propertyElement,
      final PropertyInitializer initializer, final Class type)
   {
      Element element = findComponentElement(propertyElement);

      if (element == null)
      {
         final String sValue = m_helper.getElementValue(propertyElement);

         m_componentFixupList.add(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               if (StringUtil.isEmpty(sValue))
               {
                  initializer.initializeValue(null);
               }
               else
               {
                  Component component = initializer.getComponent().getMetadata().getComponent(sValue);

                  if (!m_bRuntimeExcluded && !type.isAssignableFrom(component.getType()))
                  {
                     throw new MetadataException("err.meta.propertyTypeMismatch",
                        new Object[]{initializer.getName(), initializer.getComponent().getName()});
                  }

                  initializer.initializeValue(component);
               }
            }
         });
      }
      else
      {
         final Component component = new Component(initializer.getComponent().getName() +
            "/" + initializer.getName());

         component.setMetadata(m_metadata);
         loadComponent(element, component);

         if (!m_bRuntimeExcluded && !type.isAssignableFrom(component.getType()))
         {
            throw new MetadataException("err.meta.propertyTypeMismatch",
               new Object[]{initializer.getName(), initializer.getComponent().getName()});
         }

         m_componentFixupList.add(new ContextFixup(m_helper)
         {
            public void fixup()
            {
               initializer.initializeValue(component);
            }
         });
      }
   }

   /**
    * Loads a extended library from a DOM element.
    * @param externalLibraryElement The DOM element containing the extended library.
    * @param sName The extended library name.
    */
   protected void loadExternalLibrary(Element externalLibraryElement, String sName)
   {
      XMLMetadataHelper.verifyRootElement(externalLibraryElement, "ExternalLibrary");

      final ExternalLibrary externalLibrary = new ExternalLibrary(sName);

      externalLibrary.setMetadata(m_metadata);

      XMLUtil.withFirstChildElement(externalLibraryElement, "Files", false, new ElementHandler()
      {
         public void handleElement(Element filesElement)
         {
            XMLUtil.forEachChildElement(filesElement, "File", new ElementHandler()
            {
               public void handleElement(Element fileElement)
               {
                  externalLibrary.addFile(XMLUtil.getReqStringAttr(fileElement, "name"));
               }
            });
         }
      });

      m_metadata.addExternalLibrary(externalLibrary);
   }

   /**
    * Loads the supported locales.
    */
   protected void loadLocales()
   {
      Metaclass metaclass = m_metadata.findMetaclass(Metadata.LOCALE_CLASS_NAME);

      if (metaclass != null)
      {
         for (int i = 0, n = metaclass.getStaticAttributeCount(); i != n; ++i)
         {
            Attribute attribute = metaclass.getStaticAttribute(i);

            if (attribute.isStatic() && attribute.isReadOnly() &&
               attribute.getValue() instanceof String)
            {
               m_metadata.addLocale((String)attribute.getValue());
            }
         }
      }

      if (!m_metadata.isLocaleSupported(Metadata.DEFAULT_LOCALE))
      {
         m_metadata.addLocale(Metadata.DEFAULT_LOCALE);
      }
   }

   /**
    * Loads the string resources from a map iterator over ResourcePath[ResourceName].
    * @param itr The map iterator.
    */
   protected void loadStrings(Lookup.Iterator itr)
   {
      while (itr.hasNext())
      {
         String sLocale = (String)itr.next();
         int i = sLocale.lastIndexOf('.');

         if (i >= 0)
         {
            sLocale = sLocale.substring(i + 1);
         }

         m_metadata.addString(sLocale, (String)itr.getValue());
      }
   }

   /**
    * Loads the documentation from a given element.
    * @param element The element from which to load the documentation.
    * @param documented The destination object.
    */
   public void loadDocumentation(Element element, Documented documented)
   {
      if (m_bDocumented)
      {
         documented.setDescription(XMLUtil.getStringAttr(element, "description"));
      }
   }

   /**
    * Loads custom properties from a Properties element into a property holder.
    * @param element The element containing the container element element.
    * @param sContainer The container element name.
    * @param holder The property holder.
    */
   public void loadProperties(Element element, String sContainer, final PropertyHolder holder)
   {
      XMLUtil.withFirstChildElement(element, sContainer, false, new ElementHandler()
      {
         public void handleElement(Element propertiesElement)
         {
            XMLUtil.forEachChildElement(propertiesElement, "Property",
               getHelper().new ElementHandler("property")
            {
               public void handleElement(Element propertyElement, String sPropertyName)
               {
                  holder.addProperty(sPropertyName, XMLUtil.getStringAttr(propertyElement, "value", ""));
               }
            });
         }
      });
   }

   /**
    * Gets the privilege from a given attribute.
    * @param element The element containing the attribute.
    * @param sName The name of the attribute.
    * @return The privilege.
    */
   public PrimitivePrivilege getPrivilege(Element element, String sName)
   {
      String sPrivilege = XMLUtil.getStringAttr(element, sName);

      if (sPrivilege == null)
      {
         return null;
      }

      return m_metadata.getPrimitivePrivilege(sPrivilege);
   }

   /**
    * Adds a persistence mapping fixup handler.
    * @param fixup The fixup handler.
    */
   public void addPersistenceMappingFixup(Fixup fixup)
   {
      m_persistenceMappingFixupList.add(fixup);
   }

   /**
    * Adds an I/O fixup handler.
    * @param fixup The fixup handler.
    */
   public void addIOFixup(Fixup fixup)
   {
      m_ioFixupList.add(fixup);
   }

   /**
    * Adds an environment fixup handler.
    * @param fixup The fixup handler.
    */
   public void addEnvironmentFixup(Fixup fixup)
   {
      m_environmentFixupList.add(fixup);
   }

   /**
    * Adds a privilege fixup handler.
    * @param fixup The fixup handler.
    */
   public void addPrivilegeFixup(Fixup fixup)
   {
      m_privilegeFixupList.add(fixup);
   }

   /**
    * Adds a pre-inheritance message fixup handler. Executed before resolution of message inheritance.
    * @param fixup The fixup handler.
    */
   public void addPreInheritanceMessageFixup(Fixup fixup)
   {
      m_preInheritanceMessageFixupList.add(fixup);
   }

   /**
    * Adds a post-inheritance message fixup handler. Executed immediately after resolution of
    * message inheritance.
    * @param fixup The fixup handler.
    */
   public void addPostInheritanceMessageFixup(Fixup fixup)
   {
      m_postInheritanceMessageFixupList.add(fixup);
   }

   /**
    * Adds a transformation fixup handler.
    * @param fixup The fixup handler.
    */
   public void addTransformationFixup(Fixup fixup)
   {
      m_transformationFixupList.add(fixup);
   }

   /**
    * Adds a component fixup handler.
    * @param fixup The fixup handler.
    */
   public void addComponentFixup(Fixup fixup)
   {
      m_componentFixupList.add(fixup);
   }

   /**
    * Adds a singleton fixup handler.
    * @param fixup The fixup handler.
    */
   public void addSingletonFixup(Fixup fixup)
   {
      if (!m_bRuntimeExcluded)
      {
         m_singletonFixupList.add(fixup);
      }
   }

   /**
    * Adds a singleton fixup handler.
    * @param component The component to fixup.
    */
   public void addSingletonFixup(final Component component)
   {
      addSingletonFixup(new ContextFixup(m_helper)
      {
         public void fixup()
         {
            component.getInstance(null);
         }
      });
   }

   /**
    * @return The metadata object currently being loaded.
    */
   public XMLMetadata getMetadata()
   {
      return m_metadata;
   }

   /**
    * @return  The VM for metadata loading.
    */
   public Machine getMachine()
   {
      return m_machine;
   }

   /**
    * @return The current helper object.
    */
   public XMLMetadataHelper getHelper()
   {
      return m_helper;
   }

   /**
    * Parses a visibility string.
    * @param sVisibility The visibility string to parse.
    * @param nDefault The value to return if the string is empty.
    * @return Metaclass.PUBLIC or Metaclass.PROTECTED.
    * @throws MetadataException if the visibility string is invalid.
    */
   public static byte parseVisibility(String sVisibility, byte nDefault) throws MetadataException
   {
      if (sVisibility == null || sVisibility.length() == 0)
      {
         return nDefault;
      }

      if (sVisibility.equals("public"))
      {
         return Metaclass.PUBLIC;
      }

      if (sVisibility.equals("protected"))
      {
         return Metaclass.PROTECTED;
      }

      throw new MetadataException("err.meta.visibility", new Object[]{sVisibility});
   }

   /**
    * Parses a transaction mode string.
    * @param sTransactionMode The transaction mode string to parse.
    * @param nDefault The value to return if the string is empty.
    * @return One of the Metaclass.TX_* constants.
    * @throws MetadataException if the transaction string is invalid.
    */
   public static byte parseTransactionMode(String sTransactionMode, byte nDefault) throws MetadataException
   {
      if (sTransactionMode == null || sTransactionMode.length() == 0)
      {
         return nDefault;
      }

      if (sTransactionMode.equals("supported"))
      {
         return Event.TX_SUPPORTED;
      }

      if (sTransactionMode.equals("required"))
      {
         return Event.TX_REQUIRED;
      }

      if (sTransactionMode.equals("new"))
      {
         return Event.TX_NEW;
      }

      if (sTransactionMode.equals("none"))
      {
         return Event.TX_NONE;
      }

      if (sTransactionMode.equals("mandatory"))
      {
         return Event.TX_MANDATORY;
      }

      if (sTransactionMode.equals("unsupported"))
      {
         return Event.TX_UNSUPPORTED;
      }

      throw new MetadataException("err.meta.transactionMode", new Object[]{sTransactionMode});
   }

   /**
    * Parses a cascade mode string.
    * @param sCascadeMode The cascade mode string to parse.
    * @param nDefault The value to return if the string is empty.
    * @return One of the Attribute.CASCADE_* constants.
    * @throws MetadataException if the cascade string is invalid.
    */
   public static byte parseCascadeMode(String sCascadeMode, byte nDefault) throws MetadataException
   {
      if (sCascadeMode == null || sCascadeMode.length() == 0)
      {
         return nDefault;
      }

      if (sCascadeMode.equals("none"))
      {
         return Attribute.CASCADE_NONE;
      }

      if (sCascadeMode.equals("delete"))
      {
         return Attribute.CASCADE_DELETE;
      }

      if (sCascadeMode.equals("clear"))
      {
         return Attribute.CASCADE_CLEAR;
      }

      if (sCascadeMode.equals("cancel"))
      {
         return Attribute.CASCADE_CANCEL;
      }

      throw new MetadataException("err.meta.cascadeMode", new Object[]{sCascadeMode});
   }

   /**
    * Parses a pattern string: [!]pattern1 ... [!]patternN.
    * @param sPatterns The pattern string. Can be null.
    * @param handler The aspect handler.
    * @throws MetadataException if the aspect string is invalid.
    */
   public static void parsePatterns(String sPatterns, PatternHandler handler) throws MetadataException
   {
      if (sPatterns != null)
      {
         for (StringTokenizer tokenizer = new StringTokenizer(sPatterns); tokenizer.hasMoreTokens();)
         {
            String sToken = tokenizer.nextToken();

            if (sToken.length() != 0)
            {
               if (sToken.charAt(0) == '!')
               {
                  if (sToken.length() == 1 || sToken.charAt(1) == '!')
                  {
                     throw new MetadataException("err.meta.emptyNegation");
                  }

                  handler.handlePattern(sToken.substring(1), false);
               }
               else
               {
                  handler.handlePattern(sToken, true);
               }
            }
         }
      }
   }

   /**
    * Finds a component property parser for a given property type.
    * @param type The property type.
    * @return The property parser, or null if not found.
    */
   public static PropertyParser findPropertyParser(Class type)
   {
      return (PropertyParser)s_propertyParserMap.get(type);
   }

   /**
    * Finds the component tag in a given element.
    * @param element The element containing the component tag.
    * @return The component tag, or null if not found.
    */
   public static Element findComponentElement(Element element)
   {
      return XMLUtil.findChildElement(element, "Component");
   }

   // inner classes

   /**
    * Interface implemented by property parser classes.
    */
   public interface PropertyParser
   {
      /**
       * Parses the property value and sets it on the specified initializer.
       * @param sValue The value to parse.
       * @param initializer The initializer on which to set the value.
       * @param loader The metadata loader.
       */
      void parse(String sValue, PropertyInitializer initializer, XMLMetadataLoader loader);
   }

   /**
    * Parser class for primitive property types.
    */
   private static class PrimitivePropertyParser implements PropertyParser
   {
      private Constructor m_constructor;
      private boolean m_bNullAllowed;

      public PrimitivePropertyParser(Class type, boolean bNullAllowed)
      {
         try
         {
            m_constructor = type.getConstructor(new Class[]{String.class});
         }
         catch (Exception e)
         {
            throw new IllegalArgumentException("Unknown primitive type constructor " + type);
         }

         m_bNullAllowed = bNullAllowed;
      }

      /**
       * @see nexj.core.meta.xml.XMLMetadataLoader.PropertyParser#parse(java.lang.String, nexj.core.meta.PropertyInitializer, nexj.core.meta.xml.XMLMetadataLoader)
       */
      public void parse(String sValue, PropertyInitializer initializer, XMLMetadataLoader loader)
      {
         Object value;

         if (m_bNullAllowed && (sValue == null || sValue.length() == 0))
         {
            value = null;
         }
         else
         {
            try
            {
               value = m_constructor.newInstance(new Object[]{sValue});
            }
            catch (Exception e)
            {
               throw new MetadataException("err.meta.propertyValue",
                  new Object[]{sValue, m_constructor.getDeclaringClass().getName()}, e);
            }
         }

         initializer.initializeValue(value);
      }
   }

   /**
    * Interface implemented by activity loader strategies.
    */
   public interface ActivityLoader
   {
      /**
       * Loads an activity from a DOM element.
       * @param element The DOM element.
       * @param activity The activity to load.
       */
      void loadActivity(Element element, Activity activity);
   }

   /**
    * Interface implemented by flow decision loader strategies.
    */
   public interface BranchLoader extends ActivityLoader
   {
      /**
       * @return The branch element name.
       */
      String getElementName();

      /**
       * @return A new branch.
       */
      Branch createBranch();
   }

   /**
    * Interface for processing a pattern.
    */
   public interface PatternHandler
   {
      /**
       * Processes the pattern.
       * @param sPattern The pattern.
       * @param bInclusive True if the pattern is inclusive.
       */
      void handlePattern(String sPattern, boolean bApply);
   }

   /**
    * Fixup that sets the resource name based on an exception class.
    */
   protected abstract class ClassFixup implements Fixup
   {
      public void setMarker(MetadataMarker marker)
      {
      }
   }
}