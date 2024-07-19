package nexj.core.meta.soa;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import nexj.core.meta.MetadataException;
import nexj.core.meta.xml.XMLSOAMetadataLoader;
import nexj.core.meta.xml.XMLSOAMetadataLoader.GlobalObject;
import nexj.core.meta.xml.XMLSOAMetadataLoader.Implementation;
import nexj.core.meta.xml.XMLSOAMetadataLoader.InterfaceImplementation;
import nexj.core.meta.xml.XMLSOAMetadataLoader.SOAObject;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;


/**
 * An SOA definition.
 */
public class Definition extends SOAObject implements GlobalObject
{
   // attributes

   /**
    * The version of this definition.
    */
   protected String m_sVersion = "1.0";

   // associations

   /**
    * A set of the information model enumeration types.
    */
   protected Lookup m_enumMap = new HashTab(); // of type EnumType[String]

   /**
    * A set of the information model types.
    */
   protected Lookup m_typeMap = new HashTab(); // of type ModelType[String]

   /**
    * A map of the interfaces defined by the definition, indexed by their local name.
    */
   protected Lookup m_interfaceMap = new HashTab(4); // of type Interface[String]

   /**
    * A list of interfaces for the service defined by this definition.
    * The default interface must be the first interface.
    */
   protected List m_interfaceList = new ArrayList(2); // of type Interface[]

   /**
    * The implementation of the service defined by this definition, if any.
    */
   protected Implementation m_implementation;

   /**
    * The bindings supported by this definition.
    */
   protected Set m_bindingSet = new HashSet(2); // of type Symbol[]

   // operations

   /**
    * Sets the version.
    * @param sVersion The version string.
    */
   public void setVersion(String sVersion)
   {
      m_sVersion = sVersion;
   }

   /**
    * Gets the version.
    * @return The version string.
    */
   public String getVersion()
   {
      return m_sVersion;
   }

   /**
    * Adds a binding.
    * @param sBinding The binding to add.
    * @return True if the binding was added; false if it is a duplicate.
    */
   public boolean addBinding(String sBinding)
   {
      return m_bindingSet.add(Symbol.define(sBinding));
   }

   /**
    * Adds an information model enumeration type to this definition.
    * @param enumeration The enumeration to add.
    * @return The previous enumeration with the same name, null if none existed.
    */
   public EnumType addEnum(EnumType enumeration)
   {
      return (EnumType)m_enumMap.put(enumeration.getName(), enumeration);
   }

   /**
    * Returns the information model enumeration type with the specified name or null if none.
    * @param sName The name of the enumeration type.
    * @return The enumeration type or null if none.
    */
   public EnumType findEnum(String sName)
   {
      return (EnumType)m_enumMap.get(sName);
   }

   /**
    * Gets the number of enums defined in this definition.
    * @return The number of interfaces.
    */
   public int getEnumCount()
   {
      return m_enumMap.size();
   }

   /**
    * Gets an iterator over the enumeration types defined in this definition.
    * @return The EnumType iterator.
    */
   public Iterator getEnumIterator()
   {
      return m_enumMap.valueIterator();
   }

   /**
    * Adds an information model type to this definition.
    * @param type The type to add.
    * @return The previous type with the same name, null if none existed.
    */
   public ModelType addType(ModelType type)
   {
      return (ModelType)m_typeMap.put(type.getName(), type);
   }

   /**
    * Returns the information model type with the specified name or null if none.
    * @param sName The name of the type.
    * @return The type or null if none.
    */
   public ModelType findType(String sName)
   {
      return (ModelType)m_typeMap.get(sName);
   }

   /**
    * Gets the number of types defined in this definition.
    * @return The number of interfaces.
    */
   public int getTypeCount()
   {
      return m_typeMap.size();
   }

   /**
    * Gets an iterator over the types defined in this definition.
    * @return The ModelType iterator.
    */
   public Iterator getTypeIterator()
   {
      return m_typeMap.valueIterator();
   }

   /**
    * Defines a new interface.
    * @param sName The local name of the interface.
    * @return The interface that was created.
    */
   public Interface defineInterface(String sName)
   {
      Interface iface = new Interface(this);

      iface.setName(sName);

      if (m_interfaceMap.put(sName, iface) != null)
      {
         throw new MetadataException("err.meta.soa.interfaceDup",
            new Object[] {sName, getGlobalName()});
      }

      return iface;
   }

   /**
    * Gets the number of interfaces defined in this definition.
    * @return The number of interfaces.
    */
   public int getDefinedInterfaceCount()
   {
      return m_interfaceMap.size();
   }

   /**
    * Gets an iterator over the interfaces defined in this definition.
    * @return The Interface iterator.
    */
   public Iterator getDefinedInterfaceIterator()
   {
      return m_interfaceMap.valueIterator();
   }

   /**
    * Adds an interface to the service.
    */
   public void addServiceInterface(Interface iface)
   {
      addServiceInterface(iface, false);
   }
   
   /**
    * Adds an interface to the service.
    * @param iface The interface.
    * @param bDefault True if this interface is the default interface. 
    */
   public void addServiceInterface(Interface iface, boolean bDefault)
   {
      if (bDefault)
      {
         m_interfaceList.add(0, iface);
      }
      else
      {
         m_interfaceList.add(iface);
      }
   }

   /**
    * Gets the number of interfaces on the service.
    * @return The number of interfaces.
    */
   public int getServiceInterfaceCount()
   {
      return m_interfaceList.size();
   }

   /**
    * Gets an iterator over the service's interfaces.
    * @return An iterator over the InterfaceRefs.
    */
   public Iterator getServiceInterfaceIterator()
   {
      return m_interfaceList.iterator();
   }

   /**
    * Sets the implementation of this definition's service.
    * @param implementation The implementation, if any.
    */
   public void setImplementation(Implementation implementation)
   {
      m_implementation = implementation;
   }

   /**
    * Resolves a reference to an interface.
    * @param sRef The interface reference, relative to this definition.
    * @return The fully-qualified name of the interface.
    */
   public String resolveInterfaceRef(String sRef)
   {
      return (sRef.indexOf(':') < 0) ? getGlobalName() + ":interface:" + sRef : sRef;
   }

   /**
    * Resolves a reference to a type.
    * @param sRef The type reference, relative to this definition.
    * @return The fully-qualified name of the type.
    */
   public String resolveTypeRef(String sRef)
   {
      return (sRef.indexOf(':') < 0) ? getGlobalName() + ":type:" + sRef : sRef;
   }

   /**
    * Turns a fully qualified into a local name.
    * @param sFQName The fully qualified name
    * @return The local name if appropriate to this definition
    */
   public String getLocalName(String sFQName)
   {
      String sPrefix = getGlobalName();

      if (sFQName.startsWith(sPrefix))
      {
         if (sFQName.length() > sPrefix.length() && sFQName.charAt(sPrefix.length()) == ':')
         {
            int nNameStart = sFQName.indexOf(':', sPrefix.length() + 1);

            assert sFQName.substring(sPrefix.length() + 1, nNameStart).equals("type") ||
               sFQName.substring(sPrefix.length() + 1, nNameStart).equals("interface") ||
               sFQName.substring(sPrefix.length() + 1, nNameStart).equals("enum");

            if (nNameStart != -1 && sFQName.length() > nNameStart)
            {
               return sFQName.substring(nNameStart + 1);
            }
         }
      }

      //unable to determine local name, use the fully-qualified name
      return sFQName;
   }

   /**
    * @see nexj.core.meta.xml.XMLSOAMetadataLoader.GlobalObject#getGlobalName()
    */
   public String getGlobalName()
   {
      return m_sName + ':' + m_sVersion;
   }

   /**
    * Generates the following code:
    *
    *    (define-class <definition QName> (soa:ServiceObject) "Description"
    *       (class-attribute interfaces :init '(<default interface symbol> <interface #2 symbol> ...))
    *       (class-attribute implementations :init '(<interface impl #1> <interface impl #2> ...))
    *    )
    *
    * @see nexj.core.meta.xml.XMLSOAMetadataLoader.SOAObject#getCode()
    */
   public Object getCode()
   {
      List interfaceList = new ArrayList(m_interfaceList.size());

      for (int i = 0; i < m_interfaceList.size(); i++)
      {
         interfaceList.add(Symbol.define(((Interface)m_interfaceList.get(i)).getGlobalName()));
      }

      List implementationList = null;

      if (m_implementation != null)
      {
         implementationList = new ArrayList(m_implementation.getInterfaceImplementationCount());

         for (Iterator itr = m_implementation.getInterfaceImplementationIterator(); itr.hasNext(); )
         {
            InterfaceImplementation ifaceImpl = (InterfaceImplementation)itr.next();

            implementationList.add(Symbol.define(ifaceImpl.getGlobalName()));
         }
      }

      return Pair.list(
         XMLSOAMetadataLoader.DEFINE_CLASS,
         Symbol.define(getGlobalName()),
         Pair.list(XMLSOAMetadataLoader.SERVICE_OBJECT),
         m_sDescription,
         Pair.list(
            XMLSOAMetadataLoader.CLASS_ATTRIBUTE,
            XMLSOAMetadataLoader.INTERFACES,
            XMLSOAMetadataLoader.INIT,
            Pair.list(Symbol.MAP, Symbol.GET_VALUE, interfaceList),
            XMLSOAMetadataLoader.TYPE,
            XMLSOAMetadataLoader.INTERFACE_OBJECT_METACLASS,
            XMLSOAMetadataLoader.COLLECTION,
            Boolean.TRUE
         ),
         Pair.list(
            XMLSOAMetadataLoader.CLASS_ATTRIBUTE,
            XMLSOAMetadataLoader.IMPLEMENTATIONS,
            XMLSOAMetadataLoader.INIT,
            Pair.list(Symbol.MAP, Symbol.GET_VALUE, implementationList),
            XMLSOAMetadataLoader.TYPE,
            XMLSOAMetadataLoader.INTERFACE_OBJECT_METACLASS,
            XMLSOAMetadataLoader.COLLECTION,
            Boolean.TRUE
         )
      );
   }
}
