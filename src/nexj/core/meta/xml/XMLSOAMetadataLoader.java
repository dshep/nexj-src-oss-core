// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.xml;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.w3c.dom.Element;

import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.Primitive;
import nexj.core.meta.soa.Argument;
import nexj.core.meta.soa.Attribute;
import nexj.core.meta.soa.Definition;
import nexj.core.meta.soa.EnumType;
import nexj.core.meta.soa.Interface;
import nexj.core.meta.soa.Method;
import nexj.core.meta.soa.ModelType;
import nexj.core.meta.soa.Result;
import nexj.core.meta.xml.XMLMetadataHelper.ContextFixup;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.EqualHashTab;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.scripting.object.ClassObject;
import nexj.core.scripting.object.ObjectOriented;
import nexj.core.util.HashTab;
import nexj.core.util.HashTab2D;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.Named;
import nexj.core.util.Null;
import nexj.core.util.ObjUtil;
import nexj.core.util.SingletonIterator;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.TextPosition;
import nexj.core.util.XMLUtil;

/**
 * Loader for SOA Definitions and SOA Implementations.
 */
public class XMLSOAMetadataLoader
{
   // constants

   /**
    * The symbol for defining a new dynamic object system class.
    */
   public final static Symbol DEFINE_CLASS = Symbol.define("define-class");

   /**
    * The symbol for defining a class method of a dynamic object system class.
    */
   public final static Symbol CLASS_METHOD = Symbol.define("class-method");

   /**
    * The symbol for defining a class attribute of a dynamic object system class.
    */
   public final static Symbol CLASS_ATTRIBUTE = Symbol.define("class-attribute");

   /**
    * The symbol for defining an instance attribute of a dynamic object system class.
    */
   public final static Symbol ATTRIBUTE = Symbol.define("attribute");

   /**
    * The interfaces attribute symbol for specifying the interfaces of a service.
    */
   public final static Symbol INTERFACES = Symbol.define("interfaces");

   /**
    * The implementations attribute symbol for specifying the implementations of a service.
    */
   public final static Symbol IMPLEMENTATIONS = Symbol.define("implementations");

   /**
    * The :init symbol to specify attribute initializers.
    */
   public final static Symbol INIT = Symbol.define(":init");

   /**
    * The local registry for SOA lookups that override the remote registry.
    */
   protected final static Symbol SOA_LOCAL_REGISTRY = Symbol.define("soa:local-registry");

   /**
    * The ServiceInstance type symbol.
    */
   protected final static Symbol SERVICE_INSTANCE = Symbol.define(SysUtil.NAMESPACE + ":soa:Registry:1.0:type:ServiceInstance");

   /**
    * The Property type symbol.
    */
   protected final static Symbol PROPERTY = Symbol.define(SysUtil.NAMESPACE + ":soa:Registry:1.0:type:Property");

   /**
    * The properties symbol.
    */
   protected final static Symbol PROPERTIES = Symbol.define("properties");

   /**
    * The service symbol.
    */
   protected final static Symbol SERVICE = Symbol.define("service");

   /**
    * The binding symbol.
    */
   protected final static Symbol BINDING = Symbol.define("binding");

   /**
    * The address symbol.
    */
   protected final static Symbol ADDRESS = Symbol.define("address");

   /**
    * The credential symbol.
    */
   protected final static Symbol CREDENTIAL = Symbol.define("credential");

   /**
    * The BasicCredential type symbol.
    */
   protected final static Symbol BASIC_CREDENTIAL = Symbol.define("nexj:soa:Registry:1.0:type:BasicCredential");

   /**
    * The login symbol.
    */
   protected final static Symbol LOGIN = Symbol.define("login");

   /**
    * The password symbol.
    */
   protected final static Symbol PASSWORD = Symbol.define("password");

   /**
    * The PerimeterCredential type symbol.
    */
   protected final static Symbol PERIMETER_CREDENTIAL = Symbol.define("nexj:soa:Registry:1.0:type:PerimeterCredential");

   /**
    * The :type symbol to specify the attribute and method type information.
    */
   public final static Symbol TYPE = Symbol.define(":type");

   /**
    * The :arg-types symbol to specify method argument type information.
    */
   public final static Symbol ARG_TYPES = Symbol.define(":arg-types");

   /**
    * The :collection symbol to specify attribute and method type information.
    */
   public final static Symbol COLLECTION = Symbol.define(":collection");

   /**
    * The :arg-collections symbol to specify method argument type information.
    */
   public final static Symbol ARG_COLLECTIONS = Symbol.define(":arg-collections");

   /**
    * The :required symbol to specify attribute requiredness.
    */
   public final static Symbol REQUIRED = Symbol.define(":required");

   /**
    * The :derived symbol to specify that a member's type info should be derived from the base type's member.
    */
   public final static Symbol DERIVED = Symbol.define(":derived");

   /**
    * The make-enumeration symbol to construct a new enumeration.
    */
   public final static Symbol MAKE_ENUMERATION = Symbol.define("make-enumeration");

   /**
    * The root of the information model class hierarchy.
    */
   public final static Symbol INFORMATION_MODEL_OBJECT = Symbol.define("soa:TypeObject");

   /**
    * The root of the Fault class hierarchy.
    */
   public final static Symbol FAULT_OBJECT = Symbol.define("soa:Fault");

   /**
    * The root of the SOA interface class hierarchy.
    */
   public final static Symbol INTERFACE_OBJECT = Symbol.define("soa:InterfaceObject");

   /**
    * The root of the SOA service class hierarchy.
    */
   public final static Symbol SERVICE_OBJECT = Symbol.define("soa:ServiceObject");

   /**
    * The root of the SOA implementation class hierarchy.
    */
   protected final static Symbol IMPLEMENTATION_OBJECT = Symbol.define("soa:ImplementationObject");

   /**
    * The metaclass of soa:InterfaceObject.
    */
   public final static Symbol INTERFACE_OBJECT_METACLASS = Symbol.define("soa:InterfaceObject:Metaclass");

   /**
    * The :stateful-method-map symbol.
    */
   public final static Symbol STATEFUL_METHOD_MAP = Symbol.define(":stateful-method-map");

   /**
    * The symbol for the soa:list->hashtable function.
    */
   public final static Symbol LIST_HASHTABLE = Symbol.define("soa:list->hashtable");

   /**
    * The :state symbol.
    */
   public final static Symbol STATE = Symbol.define(":state");

   /**
    * The :service symbol.
    */
   protected final static Symbol _SERVICE = Symbol.define(":service");

   /**
    * The :method-fault-map symbol.
    */
   public final static Symbol METHOD_FAULT_MAP = Symbol.define(":method-fault-map");

   /**
    * Map of common type names to their type symbols.
    */
   protected final static Lookup s_commonTypeSymbolMap = new HashTab(Primitive.MAX_COUNT + 1); // of type Symbol[String]

   static
   {
      for (int i = 0; i < Primitive.MAX_COUNT; ++i)
      {
         Primitive type = Primitive.get(i);

         s_commonTypeSymbolMap.put(type.getName(), Symbol.define("sys:" + type.getName()));
      }

      s_commonTypeSymbolMap.put(INFORMATION_MODEL_OBJECT.getName(), INFORMATION_MODEL_OBJECT);
   }

   /**
    * Service authentication is unspecified (use remote registry).
    */
   protected final static byte AUTH_UNSPECIFIED = -1;

   /**
    * Service uses basic authentication.
    */
   protected final static byte AUTH_BASIC = 0;

   /**
    * Service uses perimeter authentication.
    */
   protected final static byte AUTH_PERIMETER = 1;

   // attributes

   /**
    * The size of the array containing references to verify.
    */
   protected int m_nRefVerificationCount;

   // associations

   /**
    * The metadata loading helper.
    */
   protected XMLMetadataHelper m_helper;

   /**
    * The definitions loaded by this loader.
    */
   protected Lookup m_definitionMap = new HashTab(); // of type Definition[String]

   /**
    * The implementations loaded by this loader.
    */
   protected Lookup m_implementationMap = new HashTab(); // of type Implementation[String]

   /**
    * Fixups for resolving references by name.
    */
   protected List m_resolutionFixupList = new ArrayList();

   /**
    * The array of references to verify. Triplets of referenced name, reference literal, and referrer.
    */
   protected Object[] m_refVerificationArray = new Object[24];  // String[3*n], String[3*n+1], GlobalObject[3*n+2]

   /**
    * Service connections indexed by service name and instance name.
    */
   protected Lookup2D m_connectionMap = new HashTab2D(); // of type SOAConnection[String, Object]

   /**
    * Holds every independently-referenceable object loaded by the loader, indexed by global name.
    */
   protected Lookup m_globalMap = new HashTab(); // of type Object[String]

   /**
    * The text position map for script parsing and compilation.
    */
   protected Lookup m_posMap = new IdentityHashTab();

   // constructors

   /**
    * Creates a new loader for loading SOA metadata from an XML file.
    * @param helper The XML metadata loading helper.
    */
   public XMLSOAMetadataLoader(XMLMetadataHelper helper)
   {
      m_helper = helper;
   }

   // operations

   /**
    * Loads an SOA Definition from a DOM element.
    * @param element The DOM element containing the definition.
    * @param sName The definition name.
    */
   public void loadDefinition(Element element, String sName)
   {
      XMLMetadataHelper.verifyRootElement(element, "SOADefinition");

      final Definition definition = new Definition();
      int nNamespaceEnd = sName.lastIndexOf(Metadata.SCOPE_SEP);
      String sNamespacePrefix = (nNamespaceEnd > 0) ? sName.substring(0, nNamespaceEnd + 1) : "";

      definition.setName(sNamespacePrefix + XMLUtil.getReqStringAttr(element, "name"));
      definition.setDescription(XMLUtil.getStringAttr(element, "description", definition.getDescription()));
      definition.setVersion(XMLUtil.getStringAttr(element, "version", definition.getVersion()));

      if (m_definitionMap.put(definition.getGlobalName(), definition) != null)
      {
         throw new MetadataException("err.meta.soa.definitionDup",
            new Object[] {definition.getName(), definition.getVersion()});
      }

      Element child = XMLUtil.findChildElement(element, "Enumerations");

      if (child != null)
      {
         XMLUtil.forEachChildElement(child, "Enumeration", new XMLUtil.ElementHandler()
         {
            public void handleElement(Element enumElement)
            {
               final EnumType enumeration = new EnumType(definition);

               loadPart(enumElement, enumeration);

               if (definition.addEnum(enumeration) != null)
               {
                  throw new MetadataException("err.meta.soa.enumDup",
                     new Object[] {enumeration.getName(), definition.getGlobalName()});
               }

               m_globalMap.put(enumeration.getGlobalName(), enumeration);

               XMLUtil.forEachChildElement(enumElement, "Item", new XMLUtil.ElementHandler()
               {
                  public void handleElement(Element itemElement)
                  {
                     String sItemName = XMLUtil.getReqStringAttr(itemElement, "name");

                     if (!enumeration.addItem(sItemName))
                     {
                        throw new MetadataException("err.meta.soa.enumItemDup",
                           new Object[] {sItemName, enumeration.getName(), definition.getGlobalName()});
                     }
                  }
               });
            }
         });
      }

      child = XMLUtil.findChildElement(element, "Types");

      if (child != null)
      {
         XMLUtil.forEachChildElement(child, "Type", new TypeLoaderHandler(definition, false));
      }

      child = XMLUtil.findChildElement(element, "Faults");

      if (child != null)
      {
         XMLUtil.forEachChildElement(child, "Fault", new TypeLoaderHandler(definition, true));
      }

      child = XMLUtil.findChildElement(element, "Interfaces");

      if (child != null)
      {
         XMLUtil.forEachChildElement(child, "Interface", new XMLUtil.ElementHandler()
         {
            public void handleElement(Element interfaceElement)
            {
               final Interface iface = definition.defineInterface(XMLUtil.getReqStringAttr(interfaceElement, "name"));

               iface.setDescription(XMLUtil.getStringAttr(interfaceElement, "description", iface.getDescription()));
               m_globalMap.put(iface.getGlobalName(), iface);

               XMLUtil.forEachChildElement(interfaceElement, "Method", new XMLUtil.ElementHandler()
               {
                  public void handleElement(Element methodElement)
                  {
                     final Method method = new Method(iface);
                     String sStateType = XMLUtil.getStringAttr(methodElement, "state", null);

                     loadPart(methodElement, method);

                     if (!StringUtil.isEmpty(sStateType))
                     {
                        method.setStateType(resolvePrimitiveOrTypeRef(sStateType, iface, definition));
                     }

                     Element argumentsElement = XMLUtil.findChildElement(methodElement, "Arguments");

                     if (argumentsElement != null)
                     {
                        XMLUtil.forEachChildElement(argumentsElement, "Argument", new XMLUtil.ElementHandler()
                        {
                           public void handleElement(final Element argumentElement)
                           {
                              final Argument argument = new Argument();

                              loadPart(argumentElement, argument);

                              if (method.addArgument(argument) != null)
                              {
                                 throw new MetadataException("err.meta.soa.argumentDup",
                                    new Object[] {argument.getName(), method.getName(), iface.getName(), definition.getGlobalName()});
                              }

                              String sType = XMLUtil.getReqStringAttr(argumentElement, "type");

                              argument.setType(resolvePrimitiveOrTypeRef(sType, iface, definition));
                              argument.setCollection(XMLUtil.getBooleanAttr(argumentElement, "collection", argument.isCollection()));
                           }
                        });
                     }

                     if (!iface.addMethod(method))
                     {
                        throw new MetadataException("err.meta.soa.methodDup",
                           new Object[] {method.getName(), method.getArgString(), iface.getName(), definition.getGlobalName()});
                     }

                     final Element resultElement = XMLUtil.findChildElement(methodElement, "Result");

                     if (resultElement != null)
                     {
                        final Result result = new Result();
                        String sType = XMLUtil.getReqStringAttr(resultElement, "type");

                        method.setResult(result);
                        result.setType(resolvePrimitiveOrTypeRef(sType, iface, definition));
                        result.setCollection(XMLUtil.getBooleanAttr(resultElement, "collection", result.isCollection()));
                        result.setDescription(XMLUtil.getStringAttr(resultElement, "description", result.getDescription()));
                     }

                     Element faultsElement = XMLUtil.findChildElement(methodElement, "Faults");

                     if (faultsElement != null)
                     {
                        XMLUtil.forEachChildElement(faultsElement, "Fault", new XMLUtil.ElementHandler()
                        {
                           public void handleElement(Element faultElement)
                           {
                              final String sFaultRef = XMLUtil.getReqStringAttr(faultElement, "type");

                              m_resolutionFixupList.add(new ContextFixup(m_helper)
                              {
                                 public void fixup()
                                 {
                                    String sFaultName = definition.resolveTypeRef(sFaultRef);
                                    ModelType faultType = (ModelType)m_globalMap.get(sFaultName);

                                    if (faultType == null)
                                    {
                                       throw new MetadataException("err.meta.soa.faultLookup",
                                          new Object[] {sFaultRef, method.getName(), Primitive.createInteger(method.getArgCount()),
                                          iface.getName(), definition.getGlobalName()});
                                    }

                                    if (!faultType.isFault())
                                    {
                                       throw new MetadataException("err.meta.soa.notFault",
                                          new Object[] {sFaultRef, method.getName(), Primitive.createInteger(method.getArgCount()),
                                          iface.getName(), definition.getGlobalName()});
                                    }

                                    method.addFault(Symbol.define(sFaultName));
                                 }
                              });
                           }
                        });
                     }
                  }
               });
            }
         });
      }

      child = XMLUtil.findChildElement(element, "Service");

      if (child != null)
      {
         m_globalMap.put(definition.getGlobalName(), definition);

         Element interfacesElement = XMLUtil.findChildElement(child, "Interfaces");
         final int nCounter[] = {0};

         if (interfacesElement != null)
         {
            XMLUtil.forEachChildElement(interfacesElement, "Interface", new XMLUtil.ElementHandler()
            {
               public void handleElement(Element interfaceElement)
               {
                  final String sInterfaceName = definition.resolveInterfaceRef(XMLUtil.getReqStringAttr(interfaceElement, "ref"));
                  final boolean bDefault = XMLUtil.getBooleanAttr(interfaceElement, "default", false);

                  if (bDefault)
                  {
                     nCounter[0]++;
                  }

                  m_resolutionFixupList.add(new ContextFixup(m_helper)
                  {
                     public void fixup()
                     {
                        Interface iface = (Interface)m_globalMap.get(sInterfaceName);

                        if (iface == null)
                        {
                           throw new MetadataException("err.meta.soa.interfaceLookup",
                              new Object[] {sInterfaceName, definition.getGlobalName()});
                        }

                        definition.addServiceInterface(iface, bDefault);
                     }
                  });
               }
            });

            if (nCounter[0] == 0)
            {
               throw new MetadataException("err.meta.soa.missingDefaultInterface",
                  new Object[] {definition.getGlobalName()});
            }
            else if (nCounter[0] >= 2)
            {
               throw new MetadataException("err.meta.soa.multipleDefaultInterface",
                  new Object[] {definition.getGlobalName()});
            }
         }
      }

      child = XMLUtil.findChildElement(element, "Bindings");

      if (child != null)
      {
         XMLUtil.forEachChildElement(child, "Binding", new XMLUtil.ElementHandler()
         {
            public void handleElement(Element bindingElement)
            {
               String sBindingName = XMLUtil.getReqStringAttr(bindingElement, "protocol");

               if (!definition.addBinding(sBindingName))
               {
                  throw new MetadataException("err.meta.soa.bindingDup",
                     new Object[] {sBindingName, definition.getGlobalName()});
               }
            }
         });
      }

      EmbeddedResourceURLStreamHandler handler = new EmbeddedResourceURLStreamHandler();
      String sPrefix = definition.getGlobalName();

      loadXMLResources(element, "Channels", "Channel", sPrefix + ":channel:", ".channel", handler);
      loadXMLResources(element, "IntegrationInterfaces", "Interface", sPrefix + ":integration:interface:", ".interface", handler);
      loadTextResources(element, "Libraries", "Library", ".scm", handler);
      loadXMLResources(element, "Messages", "Message", sPrefix + ":message:", ".message", handler);
      loadXMLResources(element, "IntegrationServices", sPrefix + ":integration:service:", "Service", ".service", handler);
      loadXMLResources(element, "Transformations", "Transformation", sPrefix + ":transformation:", ".transformation", handler);
   }

   /**
    * Loads XML resources that are embedded in the SOA Definition. The resources are added to the listing
    * maintained by the XML helper.
    * @param element The parent element of the resource container.
    * @param sContainerElementName The name of the element that contains the resources to load.
    * @param sElementName The name of the element at the resource root.
    * @param sNSPrefix The namespace prefix for the resource.
    * @param sFileExtension The file extension for these resources.
    * @param handler The handler for resolving the URLs.
    */
   protected void loadXMLResources(Element element, String sContainerElementName, String sElementName,
      final String sNSPrefix, final String sFileExtension, final EmbeddedResourceURLStreamHandler handler)
   {
      Element child = XMLUtil.findChildElement(element, sContainerElementName);

      if (child == null)
      {
         return;
      }

      final XMLMetadataListing listing = m_helper.getListing();

      XMLUtil.forEachChildElement(child, sElementName, new XMLUtil.ElementHandler()
      {
         public void handleElement(Element libraryElement)
         {
            String sName = sNSPrefix + XMLUtil.getReqStringAttr(libraryElement, "name") + sFileExtension;

            libraryElement.removeAttribute("name");

            String sBody = XMLUtil.formatXML(libraryElement);
            URL url = handler.getURL(sName);

            handler.register(url, sBody);
            listing.addResource(sName, new XMLResource(sName, url, true));
         }
      });
   }

   /**
    * Loads text resources that are embedded in the SOA Definition. The resources are added to the listing
    * maintained by the XML helper.
    * @param element The parent element of the resource container.
    * @param sContainerElementName The name of the element that contains the resources to load.
    * @param sElementName The name of the element at the resource root.
    * @param sFileExtension The file extension for these resources.
    * @param handler The handler for resolving the URLs.
    */
   protected void loadTextResources(Element element, String sContainerElementName, String sElementName,
      final String sFileExtension, final EmbeddedResourceURLStreamHandler handler)
   {
      Element child = XMLUtil.findChildElement(element, sContainerElementName);

      if (child == null)
      {
         return;
      }

      final XMLMetadataListing listing = m_helper.getListing();

      XMLUtil.forEachChildElement(child, sElementName, new XMLUtil.ElementHandler()
      {
         public void handleElement(Element libraryElement)
         {
            String sName = XMLUtil.getReqStringAttr(libraryElement, "name") + sFileExtension;
            String sBody = XMLUtil.getElementValue(libraryElement);
            URL url = handler.getURL(sName);

            handler.register(url, sBody);
            listing.addResource(sName, new XMLResource(sName, url, true));
         }
      });
   }

   /**
    * Gets an interface by name. If the name is a fully-qualified reference, then look it up
    * in the global environment.
    * This only works after references have been resolved.
    * @param sRef The interface name, relative to this service.
    * @param definition The definition to resolve names against
    * @return The interface; null if not found.
    */
   public Interface findInterface(String sRef, Definition definition)
   {
      String sQName = definition.resolveInterfaceRef(sRef);
      Object obj = m_globalMap.get(sQName);

      if (obj instanceof Interface)
      {
         Interface iface = (Interface)obj;

         // Reference is local but the found interface is on a different definition: not found
         if (sQName != sRef && iface.getDefinition() != definition)
         {
            return null;
         }

         return iface;
      }

      return null;
   }

   /**
    * Resolves a textual type name to a symbol that represents either one of the primitive types
    * or an information model type.
    * @param sRef The type name.
    * @param referrer The referrer, for error reporting purposes.
    * @param definition The definition to resolve names against.
    * @return A symbol representing the resolved type.
    */
   public Symbol resolvePrimitiveOrTypeRef(String sRef, GlobalObject referrer, Definition definition)
   {
      Symbol type = (Symbol)s_commonTypeSymbolMap.get(sRef);

      if (type != null)
      {
         return type;
      }

      return resolveTypeRef(sRef, referrer, definition, true);
   }

   /**
    * Resolves a textual type name to a symbol that represents an information model type.
    * @param sRef The type name.
    * @param referrer The referrer, for error reporting purposes.
    * @param definition The definition to resolve names against.
    * @param bIncludeEnums True to allow references to enums; false to disallow.
    * @return A symbol representing the resolved type.
    */
   public Symbol resolveTypeRef(String sRef, GlobalObject referrer, Definition definition, boolean bIncludeEnums)
   {
      if (sRef.indexOf(':') < 0)
      {
         GlobalObject obj = definition.findType(sRef);

         if (obj != null)
         {
            // Optimization: if type is already known, skip verification
            return Symbol.define(obj.getGlobalName());
         }

         String sName = definition.resolveTypeRef(sRef);

         addReferenceToVerify(sName, sRef, referrer);

         return Symbol.define(sName);
      }
      else if (sRef.charAt(0) == ':')
      {
         if (sRef.startsWith(":type:") || (bIncludeEnums && sRef.startsWith(":enum:")))
         {
            String sName = definition.getGlobalName() + sRef;

            // Optimization: if type is already known, skip verification
            if (!m_globalMap.contains(sName))
            {
               addReferenceToVerify(sName, sRef, referrer);
            }

            return Symbol.define(sName);
         }

         throw new MetadataException("err.meta.soa.typeLookup",
            new Object[] {sRef, referrer.getGlobalName()});
      }

      // Optimization: if type is already known, skip verification
      if (!m_globalMap.contains(sRef))
      {
         addReferenceToVerify(sRef, sRef, referrer);
      }

      return Symbol.define(sRef);
   }

   /**
    * Records information about a reference so that it can be verified once everything has been loaded.
    * @param sName The fully-qualified name of the referent to check against the global objects.
    * @param sRef The literal reference for the error message.
    * @param referrer The referrer object for the error message.
    */
   protected void addReferenceToVerify(String sName, String sRef, GlobalObject referrer)
   {
      if (m_nRefVerificationCount == m_refVerificationArray.length)
      {
         Object[] verificationArray = new Object[m_nRefVerificationCount << 1];

         System.arraycopy(m_refVerificationArray, 0, verificationArray, 0, m_nRefVerificationCount);
         m_refVerificationArray = verificationArray;
      }

      m_refVerificationArray[m_nRefVerificationCount++] = sName;
      m_refVerificationArray[m_nRefVerificationCount++] = sRef;
      m_refVerificationArray[m_nRefVerificationCount++] = referrer;
   }

   /**
    * Resolves references in the SOA metadata. Must be called before any implementations are loaded, but after
    * all definitions have been loaded.
    */
   public void resolveReferences()
   {
      m_helper.fixup(m_resolutionFixupList.iterator());
      m_resolutionFixupList.clear();

      int i = 0;

      while (i < m_nRefVerificationCount)
      {
         if (!m_globalMap.contains(m_refVerificationArray[i++]))
         {
            Object reference = m_refVerificationArray[i++];
            GlobalObject referrer = (GlobalObject)m_refVerificationArray[i++];

            m_helper.addException(new MetadataException("err.meta.soa.typeLookup",
               new Object[] {reference, referrer.getGlobalName()}));
         }
         else
         {
            i += 2;
         }
      }

      m_refVerificationArray = null;
      m_nRefVerificationCount = 0;
   }

   /**
    * Loads an SOA Implementation from a DOM element and store the implementations in this loader.
    * @param element The DOM element containing the implementation.
    * @param sName The implementation name.
    * @param env The environment for script parsing.
    */
   public void loadImplementation(Element element, final String sName, final GlobalEnvironment env)
   {
      XMLMetadataHelper.verifyRootElement(element, "SOAImplementation");

      String sServiceName = XMLUtil.getReqStringAttr(element, "service");

      if (StringUtil.isEmpty(sServiceName))
      {
         return;
      }

      final Definition def = (Definition)m_globalMap.get(sServiceName);

      if (def == null)
      {
         throw new MetadataException("err.meta.soa.serviceLookup", new Object[] {sServiceName});
      }

      final Implementation impl = new Implementation(def);

      if (m_implementationMap.put(impl.getNamePrefix(), impl) != null)
      {
         throw new MetadataException("err.meta.soa.implementationDup",
            new Object[] {impl.getNamePrefix(), def.getGlobalName()});
      }

      XMLUtil.forEachChildElement(element, "Interface", new XMLUtil.ElementHandler()
      {
         public void handleElement(Element interfaceElement)
         {
            final String sInterfaceName = XMLUtil.getReqStringAttr(interfaceElement, "name");
            final Interface iface = findInterface(sInterfaceName, def);

            if (iface == null)
            {
               throw new MetadataException("err.meta.soa.implementation.undefinedInterface",
                  new Object[] {sInterfaceName, def.getGlobalName()});
            }

            final InterfaceImplementation ifaceImpl = new InterfaceImplementation(impl, iface);

            if (!impl.addInterface(ifaceImpl))
            {
               throw new MetadataException("err.meta.soa.implementation.interfaceDup",
                  new Object[] {iface.getGlobalName(), def.getGlobalName()});
            }

            XMLUtil.forEachChildElement(interfaceElement, "Method", new XMLUtil.ElementHandler()
            {
               public void handleElement(Element methodElement)
               {
                  String sMethodName = XMLUtil.getReqStringAttr(methodElement, "name");
                  String sMethodArgs = XMLUtil.getStringAttr(methodElement, "args");

                  MethodImplementation method = new MethodImplementation();

                  method.setName(sMethodName);
                  method.setArgs(sMethodArgs);

                  int nArgCount = StringUtil.split(sMethodArgs).length;

                  if (!ifaceImpl.addMethod(method))
                  {
                     throw new MetadataException("err.meta.soa.implementation.methodDup",
                        new Object[] {method.getName(), method.getArgString(), iface.getName(), def.getGlobalName()});
                  }

                  if (iface.getMethod(method.getName(), method.getArgCount()) == null)
                  {
                     throw new MetadataException("err.meta.soa.implementation.methodLookup",
                        new Object[] {method.getName(), method.getArgString(), iface.getName(), def.getGlobalName()});
                  }

                  Element scriptElement = XMLUtil.findChildElement(methodElement, "Script");

                  if (scriptElement != null)
                  {
                     method.setScript(m_helper.parse(XMLUtil.getElementValue(scriptElement), true,
                        "soaimpl:" + sName + ':' + sInterfaceName + ':' + sMethodName + '/' + nArgCount,
                        m_posMap, null, env));
                  }
               }
            });
         }
      });

      if (m_helper.getWarnings() != null)
      {
         for (Iterator it = def.getServiceInterfaceIterator(); it.hasNext();)
         {
            Interface iface = (Interface)it.next();
            InterfaceImplementation ifaceImpl = (InterfaceImplementation)impl.findInterface(iface.m_sName);

            if (ifaceImpl != null)
            {
               for (Iterator methodItr = iface.getMethodIterator(); methodItr.hasNext();)
               {
                  Method method = (Method)methodItr.next();
                  MethodImplementation methodImpl = (MethodImplementation)ifaceImpl.findMethod(method);

                  if (methodImpl == null)
                  {
                     MetadataValidationException e = new MetadataValidationException("err.meta.soa.implementation.unimplementedMethod",
                        new Object[] {method.getName(), method.getArgString(), iface.getName(), sName});

                     m_helper.setMarker(e);
                     m_helper.getWarnings().addException(e);
                  }
               }
            }
            else
            {
               MetadataValidationException e = new MetadataValidationException("err.meta.soa.implementation.unimplementedInterface",
                  new Object[] {iface.getName(), sName});

               m_helper.setMarker(e);
               m_helper.getWarnings().addException(e);
            }
         }
      }
   }

   /**
    * Loads common attributes from the element into the part.
    * @param element The DOM element to load the attributes.
    * @param part The part to initialize.
    */
   public void loadPart(Element element, SOAObject part)
   {
      part.setName(XMLUtil.getReqStringAttr(element, "name"));
      part.setDescription(XMLUtil.getStringAttr(element, "description", part.getDescription()));
   }

   /**
    * Loads an SOAConnection from the environment configuration file.
    * @param element The SOAConnection element.
    * @param sService The service to which this connection pertains.
    * @param loader The metadata loader.
    */
   public void loadConnection(Element element, String sService, final XMLMetadataLoader loader)
   {
      XMLMetadataHelper.verifyRootElement(element, "SOAConnection");

      if (!m_definitionMap.contains(sService))
      {
         throw new MetadataException("err.meta.soa.serviceLookup", new Object[] {sService});
      }

      String sInstanceName = XMLUtil.getStringAttr(element, "instance");
      final SOAConnection con = new SOAConnection(sService, sInstanceName);
      Object instanceKey = (sInstanceName == null) ? Null.VALUE : (Object)sInstanceName;
      Object oldCon;

      if ((oldCon = m_connectionMap.put(sService, instanceKey, con)) != null)
      {
         m_connectionMap.put(sService, instanceKey, oldCon);

         throw new MetadataException("err.meta.soa.connectionDup",
            new Object[] {sService});
      }

      con.setBinding(XMLUtil.getStringAttr(element, "binding"));
      con.setAddress(XMLUtil.getStringAttr(element, "address"));

      String sAuth = XMLUtil.getStringAttr(element, "auth");

      if ("basic".equals(sAuth))
      {
         con.setUser(XMLUtil.getReqStringAttr(element, "user"));
         con.setPassword(loader.decryptPassword(XMLUtil.getReqStringAttr(element, "password")));
         con.setAuth(AUTH_BASIC);
      }
      else if ("perimeter".equals(sAuth))
      {
         con.setAuth(AUTH_PERIMETER);
      }
      else if (!StringUtil.isEmpty(sAuth))
      {
         throw new MetadataException("err.meta.soa.connection.auth",
            new Object[] {sAuth, sService});
      }

      XMLUtil.withFirstChildElement(element, "Properties", false, new XMLUtil.ElementHandler()
      {
         public void handleElement(Element element)
         {
            XMLUtil.forEachChildElement(element, "Property", new XMLUtil.ElementHandler()
            {
               public void handleElement(Element element)
               {
                  String sName = XMLUtil.getReqStringAttr(element, "name");
                  Object valueExpr = m_helper.parse(XMLUtil.getReqStringAttr(element, "value"), false, m_posMap, null,
                     loader.getMetadata().getGlobalEnvironment());

                  con.setProperty(sName, valueExpr);
               }
            });
         }
      });
   }

   // code generation operations

   /**
    * Defines global SOA objects.
    * @param itr The iterator over objects (CodeProvider and GlobalObject) to define.
    * @param machine The virtual machine.
    * @param compiler The compiler for the CodeProvider.
    * @param posMap The text position map to re-use.
    */
   protected static void define(Iterator itr, Machine machine, Compiler compiler, Lookup posMap)
   {
      while (itr.hasNext())
      {
         Object obj = itr.next();
         Object code = ((CodeProvider)obj).getCode();
         PCodeFunction fun;

         posMap.clear();
         posMap.put(code, new TextPosition(0, 0, "soadef:" + ((GlobalObject)obj).getGlobalName()));
         fun = compiler.compile(code, posMap, machine, false);
         machine.invoke(fun, (Pair)null);
      }
   }

   /**
    * Initializes the definitions into the global environment.
    * @param machine The virtual machine.
    */
   public void initDefinitions(Machine machine)
   {
      Compiler compiler = new Compiler();
      Lookup posMap = new IdentityHashTab();

      for (Iterator defItr = m_definitionMap.valueIterator(); defItr.hasNext(); )
      {
         Definition def = (Definition)defItr.next();

         define(def.getEnumIterator(), machine, compiler, posMap);
         define(def.getTypeIterator(), machine, compiler, posMap);
         define(def.getDefinedInterfaceIterator(), machine, compiler, posMap);
         define(new SingletonIterator(def), machine, compiler, posMap);
      }

      m_definitionMap.clear();

      for (Iterator conItr = m_connectionMap.valueIterator(); conItr.hasNext(); )
      {
         SOAConnection con = (SOAConnection)conItr.next();

         con.define(machine, m_posMap);
      }

      m_connectionMap.clear();
   }

   /**
    * Initializes the implementations into the global environment.
    * @param machine The virtual machine.
    */
   public void initImplementations(Machine machine)
   {
      Compiler compiler = new Compiler();
      Lookup posMapSaved = machine.getGlobalEnvironment().getTextPositionMap();

      try
      {
         machine.getGlobalEnvironment().setTextPositionMap(m_posMap);

         for (Iterator implItr = m_implementationMap.valueIterator(); implItr.hasNext(); )
         {
            Implementation impl = (Implementation)implItr.next();
            Object code = impl.getCode();
            PCodeFunction fun;

            try
            {
               machine.getGlobalEnvironment().defineVariable(Symbol.SYS_CURRENT_LOGGER,
                  Logger.getLogger(SysUtil.NAMESPACE + ".soa." +
                     impl.getDefinition().getGlobalName().replace(':', '.')));
               m_posMap.put(code, new TextPosition(0, 0, "soaimpl:internal"));
               fun = compiler.compile(code, m_posMap, machine, false);
               machine.invoke(fun, (Pair)null);
            }
            finally
            {
               machine.getGlobalEnvironment().removeVariable(Symbol.SYS_CURRENT_LOGGER);
            }
         }
      }
      finally
      {
         machine.getGlobalEnvironment().setTextPositionMap(posMapSaved);
      }

      m_implementationMap.clear();
      m_posMap.clear();
   }

   // inner classes

   /**
    * Handler for loading Information Model Types and Faults.
    */
   protected class TypeLoaderHandler implements XMLUtil.ElementHandler
   {
      /**
       * The definition to which the result shall be added.
       */
      Definition m_definition;

      /**
       * True to load as a Fault; false to load as an Information Model Type.
       */
      boolean m_bFault;

      // constructors

      /**
       * @param definition The definition to which the result shall be added.
       * @param bFault True to load as a Fault; false to load as an Information Model Type.
       */
      public TypeLoaderHandler(Definition definition, boolean bFault)
      {
         m_definition = definition;
         m_bFault = bFault;
      }

      // operations

      /**
       * @see nexj.core.util.XMLUtil.ElementHandler#handleElement(org.w3c.dom.Element)
       */
      public void handleElement(Element typeElement)
      {
         final ModelType type = new ModelType(m_definition);

         type.setFault(m_bFault);
         loadPart(typeElement, type);

         if (m_definition.addType(type) != null)
         {
            throw new MetadataException("err.meta.soa.typeDup",
               new Object[] {type.getName(), m_definition.getGlobalName()});
         }

         m_globalMap.put(type.getGlobalName(), type);

         String sBases = StringUtil.trimToNull(XMLUtil.getStringAttr(typeElement, "bases"));

         if (sBases != null)
         {
            final String[] sBaseArray = StringUtil.split(sBases);

            m_resolutionFixupList.add(new ContextFixup(m_helper)
            {
               public void fixup()
               {
                  for (int i = 0; i < sBaseArray.length; i++)
                  {
                     String sBaseTypeName = m_definition.resolveTypeRef(sBaseArray[i]);
                     ModelType baseType = (ModelType)m_globalMap.get(sBaseTypeName);

                     if (baseType == null)
                     {
                        throw new MetadataException("err.meta.soa.typeLookup",
                           new Object[] {sBaseTypeName, type.getGlobalName()});
                     }

                     if (m_bFault)
                     {
                        if (!baseType.isFault())
                        {
                           throw new MetadataException("err.meta.soa.typeBase", new Object[]{type.getName()});
                        }
                     }
                     else
                     {
                        if (baseType.isFault())
                        {
                           throw new MetadataException("err.meta.soa.faultBase", new Object[]{type.getName()});
                        }
                     }

                     type.addBase(baseType);
                  }
               }
            });
         }

         Element attributesElement = XMLUtil.findChildElement(typeElement, "Attributes");

         if (attributesElement != null)
         {
            XMLUtil.forEachChildElement(attributesElement, "Attribute", new XMLUtil.ElementHandler()
            {
               public void handleElement(final Element attributeElement)
               {
                  final Attribute attribute = new Attribute(type);

                  loadPart(attributeElement, attribute);

                  String sType = XMLUtil.getReqStringAttr(attributeElement, "type");

                  attribute.setType(resolvePrimitiveOrTypeRef(sType, type, m_definition));
                  attribute.setCollection(XMLUtil.getBooleanAttr(attributeElement, "collection", attribute.isCollection()));
                  attribute.setRequired(XMLUtil.getBooleanAttr(attributeElement, "required", attribute.isRequired()));

                  if (!type.addAttribute(attribute))
                  {
                     throw new MetadataException("err.meta.soa.attributeDup",
                        new Object[] {attribute.getName(), type.getName(), m_definition.getGlobalName()});
                  }
               }
            });
         }
      }
   }

   /**
    * URL handler for processing "soadef:/" URLs, which are used to load metadata resources embedded within
    * an SOA definition.
    */
   protected static class EmbeddedResourceURLStreamHandler extends URLStreamHandler
   {
      // constants

      /**
       * The URL scheme handled by this handler.
       */
      protected final static String SCHEME = "soadef";

      // associations

      /**
       * Map of URLs to content data.
       */
      protected Lookup m_urlContentMap = new HashTab(); // of type (byte[])[URL]

      // operations

      /**
       * @see java.net.URLStreamHandler#openConnection(java.net.URL)
       */
      protected URLConnection openConnection(URL u) throws IOException
      {
         byte[] nContentArray = (byte[])m_urlContentMap.get(u);

         if (nContentArray == null)
         {
            throw new IOException("Resource not found: " + u);
         }

         return new MemoryURLConnection(u, nContentArray);
      }

      /**
       * Registers some string data as the content of the given URL.
       * @param url The URL.
       * @param sContent The content of the URL.
       */
      public void register(URL url, String sContent)
      {
         try
         {
            m_urlContentMap.put(url, sContent.getBytes("UTF-8"));
         }
         catch (UnsupportedEncodingException e)
         {
            ObjUtil.rethrow(e);
         }
      }

      /**
       * Gets a URL to the given resource.
       * @param sName The resource name.
       * @return A URL to the resource.
       */
      public URL getURL(String sName)
      {
         try
         {
            return new URL(null, SCHEME + ':' + sName, this);
         }
         catch (MalformedURLException ex)
         {
            throw ObjUtil.rethrow(ex);
         }
      }

      /**
       * Allows soadef URLs to be used as keys in a hash table.
       * @see java.net.URLStreamHandler#equals(java.net.URL, java.net.URL)
       */
      protected boolean equals(URL u1, URL u2)
      {
         if (!u1.getProtocol().equals(u2.getProtocol()))
         {
            return false;
         }

         return u1.getPath().equals(u2.getPath());
      }

      /**
       * Allows soadef URLs to be used as keys in a hash table.
       * @see java.net.URLStreamHandler#hashCode(java.net.URL)
       */
      protected int hashCode(URL u)
      {
         assert u.getProtocol().equals(SCHEME);

         return u.getPath().hashCode();
      }
   }

   /**
    * A URL connection to data that are held in memory.
    */
   protected static class MemoryURLConnection extends URLConnection
   {
      /**
       * The contents of this URL.
       */
      protected byte[] m_nContentArray;

      // constructors

      /**
       * Creates a new connection to data that are held in memory.
       * @param url The URL.
       * @param nContentArray The contents of this URL.
       */
      public MemoryURLConnection(URL url, byte[] nContentArray)
      {
         super(url);
         m_nContentArray = nContentArray;
      }

      // operations

      /**
       * @see java.net.URLConnection#connect()
       */
      public void connect() throws IOException
      {
         if (m_nContentArray == null)
         {
            throw new IOException("Resource not found: " + url);
         }
      }

      /**
       * @see java.net.URLConnection#getInputStream()
       */
      public InputStream getInputStream() throws IOException
      {
         return new ByteArrayInputStream(m_nContentArray);
      }
   }

   /**
    * The object trees loaded by this loader must be converted to Scheme code to load them into
    * the dynamic object system, so they are marked as CodeProvider.
    */
   protected interface CodeProvider
   {
      /**
       * Gets the dynamic object system representation of this object.
       * @return The Scheme code for this object.
       */
      public Object getCode();
   }

   /**
    * An object that is put in the global environment.
    */
   public interface GlobalObject
   {
      /**
       * Gets the fully-qualified name of this object.
       * @return The fully-qualified name of this object.
       */
      public String getGlobalName();
   }

   /**
    * Holds information from the environment about a connection to a service.
    */
   protected static class SOAConnection
   {
      // attributes

      /**
       * The fully-qualified service name.
       */
      protected String m_sService;

      /**
       * The name of the service instance; null for the default instance.
       */
      protected String m_sInstanceName;

      /**
       * The binding to use for connecting to the service.
       */
      protected String m_sBinding;

      /**
       * The endpoint address.
       */
      protected String m_sAddress;

      /**
       * The login for connecting to the service, if using basic authentication.
       */
      protected String m_sUser;

      /**
       * The password for connecting to the service, if using basic authentication.
       */
      protected String m_sPassword;

      /**
       * The authentication mode.
       */
      protected byte m_nAuth = AUTH_UNSPECIFIED;

      // associations

      /**
       * Named properties for extending the capabilities of this connection.
       */
      protected Lookup m_propertyTab;

      // constructors

      /**
       * Creates a new service connection.
       * @param sService The fully-qualified name of the service.
       * @param sInstanceName The name of the service instance; null for the default instance.
       */
      public SOAConnection(String sService, String sInstanceName)
      {
         m_sService = sService;
         m_sInstanceName = sInstanceName;
      }

      // operations

      /**
       * Sets the service binding.
       * @param sBinding The binding to use for connecting to the service.
       */
      public void setBinding(String sBinding)
      {
         m_sBinding = sBinding;
      }

      /**
       * Sets the service address.
       * @param sAddress The endpoint address.
       */
      public void setAddress(String sAddress)
      {
         m_sAddress = sAddress;
      }

      /**
       * Sets the user.
       * @param sUser The login for connecting to the service, if using basic authentication.
       */
      public void setUser(String sUser)
      {
         m_sUser = sUser;
      }

      /**
       * Sets the password.
       * @param sPassword The password for connecting to the service, if using basic authentication.
       */
      public void setPassword(String sPassword)
      {
         m_sPassword = sPassword;
      }

      /**
       * Sets the authentication mode.
       * @param nAuth The authentication mode.
       */
      public void setAuth(byte nAuth)
      {
         m_nAuth = nAuth;
      }

      /**
       * Sets a named property on this connection.
       * @param sName The property name.
       * @param value The property value.
       */
      public void setProperty(String sName, Object value)
      {
         if (m_propertyTab == null)
         {
            m_propertyTab = new HashTab(4);
         }

         m_propertyTab.put(sName, value);
      }

      /**
       * Defines, in the global environment, the soa:local-registry and places this connection's information
       * into the registry.
       * @param machine The virtual machine.
       */
      public void define(Machine machine, Lookup posMap)
      {
         GlobalEnvironment env = machine.getGlobalEnvironment();
         Lookup registry = (Lookup)env.getVariable(SOA_LOCAL_REGISTRY);
         ClassObject srvClass = (ClassObject)env.getVariable(SERVICE_INSTANCE);
         ObjectOriented srv = srvClass.createObject();

         srv.initialize(machine);

         machine.invoke(srv, SERVICE, m_sService, null);

         if (m_sBinding != null)
         {
            machine.invoke(srv, BINDING, m_sBinding, null);
         }

         if (m_sAddress != null)
         {
            machine.invoke(srv, ADDRESS, m_sAddress, null);
         }

         ClassObject credentialClass;
         ObjectOriented credential;

         switch (m_nAuth)
         {
            case AUTH_BASIC:
               credentialClass = (ClassObject)env.getVariable(BASIC_CREDENTIAL);
               credential = credentialClass.createObject();
               credential.initialize(machine);

               machine.invoke(credential, LOGIN, m_sUser, null);
               machine.invoke(credential, PASSWORD, m_sPassword, null);

               break;

            case AUTH_PERIMETER:
               credentialClass = (ClassObject)env.getVariable(PERIMETER_CREDENTIAL);
               credential = credentialClass.createObject();
               credential.initialize(machine);

               break;

            default:
               credential = null;
         }

         if (credential != null)
         {
            machine.invoke(srv, CREDENTIAL, credential, null);
         }

         if (m_propertyTab != null)
         {
            List properties = new ArrayList(m_propertyTab.size());
            ClassObject propClass = (ClassObject)env.getVariable(PROPERTY);
            Compiler compiler = new Compiler();
            ObjectOriented prop;

            // Instantiate Property objects for each property & add them to the collection
            for (Lookup.Iterator itr = m_propertyTab.iterator(); itr.hasNext(); )
            {
               String sName = (String)itr.next();
               Object valueExpr = itr.getValue();
               PCodeFunction valueFun = compiler.compile(valueExpr, posMap, machine, false);
               Object value = machine.invoke(valueFun, (Object[])null);

               prop = propClass.createObject();
               prop.initialize(machine);
               machine.invoke(prop, Symbol.NAME, sName, null);
               machine.invoke(prop, Symbol.VALUE, value, null);
               properties.add(prop);
            }

            if (!properties.isEmpty())
            {
               machine.invoke(srv, PROPERTIES, properties, null);
            }
         }

         Lookup serviceInstanceMap = (Lookup)registry.get(m_sService);

         if (serviceInstanceMap == null)
         {
            serviceInstanceMap = new EqualHashTab();
            registry.put(m_sService, serviceInstanceMap);
         }

         serviceInstanceMap.put(m_sInstanceName, srv);
      }
   }

   /**
    * An intermediate form for holding the definitions of SOA metadata during metadata loading.
    */
   public static abstract class SOAObject implements Named, CodeProvider
   {
      // attributes

      /**
       * The name, local to the context in which it is defined.
       */
      protected String m_sName;

      /**
       * Human-readable documentation about this object.
       */
      protected String m_sDescription = "";

      // operations

      /**
       * @see nexj.core.util.Named#getName()
       */
      public String getName()
      {
         return m_sName;
      }

      /**
       * Sets the object name.
       * @param sName The name.
       */
      public void setName(String sName)
      {
         m_sName = sName;
      }

      /**
       * Gets the documentation on this object.
       * @return The object description.
       */
      public String getDescription()
      {
         return m_sDescription;
      }

      /**
       * Sets the documentation on this object.
       * @param sDescription The object description.
       */
      public void setDescription(String sDescription)
      {
         m_sDescription = (sDescription == null) ? "" : sDescription.trim();
      }

      /**
       * @see nexj.core.meta.xml.XMLSOAMetadataLoader.CodeProvider#getCode()
       */
      public Object getCode()
      {
         return Symbol.define(m_sName);
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return m_sName;
      }
   }

   /**
    * An SOA implementation. This is an implementation of a service.
    */
   public static class Implementation implements CodeProvider
   {
      // associations

      /**
       * The service that is implemented.
       */
      protected Definition m_definition;

      /**
       * The interface implementations, by interface name.
       */
      protected Lookup m_interfaceMap; // of type InterfaceImplementation[String]

      // constructors

      /**
       * Creates a new SOA implementation.
       * @param def The service that is implemented.
       */
      public Implementation(Definition def)
      {
         m_definition = def;
         m_interfaceMap = new HashTab(def.getServiceInterfaceCount());
         def.setImplementation(this);
      }

      // operations

      /**
       * Gets the service that is implemented.
       * @return The service that is implemented.
       */
      public Definition getDefinition()
      {
         return m_definition;
      }

      /**
       * Adds the implementation of an interface.
       * @param iface An interface implementation.
       * @return True if the implementation was added; false if there is already an implementation of the same
       * interface.
       */
      public boolean addInterface(InterfaceImplementation iface)
      {
         return m_interfaceMap.put(iface.getInterface().getName(), iface) == null;
      }

      /**
       * Returns the requested interface implementation or null if none.
       * @param sName The name of the interface.
       * @return The interface implementation or null if none.
       */
      public InterfaceImplementation findInterface(String sName)
      {
         return (InterfaceImplementation)m_interfaceMap.get(sName);
      }

      /**
       * Gets an iterator over the interface implementations.
       * @return An InterfaceImplementation iterator.
       */
      public Iterator getInterfaceImplementationIterator()
      {
         return m_interfaceMap.valueIterator();
      }

      /**
       * Gets the number of interface implementations on this implementation.
       * @return The count of interface implementations.
       */
      public int getInterfaceImplementationCount()
      {
         return m_interfaceMap.size();
      }

      /**
       * Generates the following code:
       *
       *    (begin
       *       (define-class <definition QName>:implementation:<interface name>
       *          ...
       *       )
       *       ...
       *    )
       *
       * @see nexj.core.meta.xml.XMLSOAMetadataLoader.CodeProvider#getCode()
       */
      public Object getCode()
      {
         Pair code = null;

         for (Iterator itr = m_interfaceMap.valueIterator(); itr.hasNext(); )
         {
            InterfaceImplementation iface = (InterfaceImplementation)itr.next();

            code = new Pair(iface.getCode(), code);
         }

         return new Pair(Symbol.BEGIN, code);
      }

      /**
       * Gets the name prefix of this implementation. Used as a prefix for the interface
       * implementations.
       * @return The name prefix of this implementation.
       */
      public String getNamePrefix()
      {
         StringBuilder buf = new StringBuilder();

         buf.append(m_definition.getGlobalName());
         buf.append(":implementation");

         return buf.toString();
      }
   }

   /**
    * The implementation of an interface (within the context of a service implementation).
    */
   public static class InterfaceImplementation implements CodeProvider, GlobalObject
   {
      // associations

      /**
       * The service implementation.
       */
      protected Implementation m_implementation;

      /**
       * The interface that is implemented.
       */
      protected Interface m_interface;

      /**
       * The implemented methods keyed by the name and number of arguments.
       */
      protected Lookup2D m_methodMap = new HashTab2D(); // of type MethodImplementation[String, Integer]

      // constructors

      /**
       * Creates a new implementation of an interface.
       * @param implementation The implementation parent.
       * @param iface The implemented interface.
       */
      public InterfaceImplementation(Implementation implementation, Interface iface)
      {
         m_implementation = implementation;
         m_interface = iface;
      }

      // operations

      /**
       * Returns the interface that is implemented.
       * @return The interface.
       */
      public Interface getInterface()
      {
         return m_interface;
      }

      /**
       * Returns the implementation of the specified method or null if none.
       * @param method The method
       * @return The implementation of the method or null.
       */
      public MethodImplementation findMethod(Method method)
      {
         return (MethodImplementation)m_methodMap.get(method.m_sName, Primitive.createInteger(method.getArgCount()));
      }

      /**
       * Adds the implementation of a method to this interface.
       * @param method A method implementation.
       * @return True if the method was added; false if there is already an implementation for the same method.
       */
      public boolean addMethod(MethodImplementation method)
      {
         return m_methodMap.put(method.m_sName, Primitive.createInteger(method.getArgCount()), method) == null;
      }

      /**
       * Generates the following code:
       *
       *    (define-class <implementation QName>:<interface name> (<interface> soa:ImplementationObject) ""
       *       (class-attribute :state)
       *       (class-attribute :service :derived #t :init <implemented service name>)
       *       (class-method ...)
       *       (class-method ...)
       *       ...
       *    )
       *
       * @see nexj.core.meta.xml.XMLSOAMetadataLoader.CodeProvider#getCode()
       */
      public Object getCode()
      {
         Pair methods = null;

         for (Iterator itr = m_methodMap.valueIterator(); itr.hasNext(); )
         {
            MethodImplementation method = (MethodImplementation)itr.next();

            methods = new Pair(method.getCode(), methods);
         }

         return new Pair(DEFINE_CLASS,
            new Pair(Symbol.define(getGlobalName()),
               new Pair(Pair.list(
                     Symbol.define(m_interface.getGlobalName()),
                     IMPLEMENTATION_OBJECT
                  ),
                  new Pair("",
                     new Pair(Pair.list(CLASS_ATTRIBUTE, STATE),
                        new Pair(Pair.list(CLASS_ATTRIBUTE, _SERVICE, DERIVED, Boolean.TRUE, INIT, Symbol.define(m_implementation.getDefinition().getGlobalName())),
                           methods)
                     )
                  )
               )
            )
         );
      }

      /**
       * @see nexj.core.meta.xml.XMLSOAMetadataLoader.GlobalObject#getGlobalName()
       */
      public String getGlobalName()
      {
         return m_implementation.getNamePrefix() + ':' + m_interface.getName();
      }

      /**
       * Ensures only one implementation of an interface may be added to an SOA implementation.
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         if (obj instanceof InterfaceImplementation)
         {
            return m_interface == ((InterfaceImplementation)obj).m_interface;
         }

         return false;
      }

      /**
       * Ensures only one implementation of an interface may be added to an SOA implementation.
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         return m_interface.hashCode();
      }
   }

   /**
    * The implementation of a method.
    */
   protected static class MethodImplementation implements CodeProvider, Named
   {
      // attributes

      /**
       * The method name.
       */
      protected String m_sName;

      // associations

      /**
       * The method argument list.
       */
      protected Pair m_args;

      /**
       * The method script.
       */
      protected Object m_script;

      // operations

      /**
       * Sets the method name.
       * @param sName The method name.
       */
      public void setName(String sName)
      {
         m_sName = sName;
      }

      /**
       * @see nexj.core.util.Named#getName()
       */
      public String getName()
      {
         return m_sName;
      }

      /**
       * Sets the method script.
       * @param script The method script.
       */
      public void setScript(Object script)
      {
         m_script = script;
      }

      /**
       * Sets the argument list.
       * @param sArgs A space-separated list of argument names.
       */
      public void setArgs(String sArgs)
      {
         sArgs = StringUtil.trimToNull(sArgs);
         m_args = (sArgs == null) ? null : Pair.attributeList(sArgs, null);
      }

      /**
       * Gets a list of the arguments.
       * @return A string representation of the method arguments.
       */
      public String getArgString()
      {
         return (m_args == null) ? "" : m_args.toString();
      }

      /**
       * Gets the number of arguments for this method.
       * @return The method argument count.
       */
      public int getArgCount()
      {
         return Pair.length(m_args);
      }

      /**
       * Generates the following code:
       *
       *    (class-method <name> (<args>) :derived #t ""
       *       <script>
       *    )
       *
       * @see nexj.core.meta.xml.XMLSOAMetadataLoader.SOAObject#getCode()
       */
      public Object getCode()
      {
         return new Pair(CLASS_METHOD,
            new Pair(Symbol.define(m_sName),
               new Pair(m_args,
                  new Pair(DERIVED, new Pair(Boolean.TRUE, new Pair("", m_script)))
               )
            )
         );
      }

      /**
       * Ensures that two methods with the same signature can't be added to the same interface.
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         if (!(obj instanceof MethodImplementation))
         {
            return false;
         }

         MethodImplementation other = (MethodImplementation)obj;

         if (!m_sName.equals(other.m_sName))
         {
            return false;
         }

         if (Pair.length(m_args) != Pair.length(other.m_args))
         {
            return false;
         }

         return true;
      }

      /**
       * Ensures that two methods with the same signature can't be added to the same interface.
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         return m_sName.hashCode() ^ Pair.length(m_args);
      }
   }
}
