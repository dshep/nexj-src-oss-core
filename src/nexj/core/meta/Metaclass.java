// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import nexj.core.meta.integration.EndpointPart;
import nexj.core.meta.integration.Transformation;
import nexj.core.meta.integration.TransformationEndpoint;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.meta.ui.AttributeMeta;
import nexj.core.meta.ui.ClassMeta;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Context;
import nexj.core.runtime.SecurityViolationException;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.runtime.ValidationException;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.Function;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Macro;
import nexj.core.scripting.Pair;
import nexj.core.scripting.ScriptingException;
import nexj.core.scripting.Symbol;
import nexj.core.scripting.syntax.SyntaxFunction;
import nexj.core.util.Captioned;
import nexj.core.util.EmptyIterator;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Holder;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Invalid;
import nexj.core.util.Logger;
import nexj.core.util.LoggerHolder;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.PropertyMap;
import nexj.core.util.StringUtil;
import nexj.core.util.TextPosition;
import nexj.core.util.UncheckedException;
import nexj.core.util.Undefined;

/**
 * The object system metaclass object.
 */
public class Metaclass extends ClassMeta implements MetadataResource, Accessor,
   Function, LoggerHolder, Captioned, Documented, Pointcut, TransformationEndpoint
{
   // constants

   // visibility constants

   /**
    * Public visibility - the item can be accessed through RPC.
    */
   public final static byte PUBLIC = 0;

   /**
    * Protected visibility - the item can be accessed only in-process.
    */
   public final static byte PROTECTED = 1;

   // Enumeration attributes

   /**
    * The name of the enumeration value attribute symbol.
    */
   public final static Symbol ENUMERATION_VALUE = Symbol.VALUE;

   /**
    * The name of the enumeration caption attribute symbol.
    */
   public final static Symbol ENUMERATION_CAPTION = Symbol.CAPTION;

   /**
    * The name of the enumeration display order attribute.
    */
   public final static Symbol ENUMERATION_DISPLAY_ORDER = Symbol.DISPLAY_ORDER;

   // other constants

   /**
    * Prefix prepended to the class name to obtain a log category.
    */
   public final static String LOG_CATEGORY_PREFIX = Metaclass.class.getName() + '.';

   /**
    * Global variable with the currently compiled class for macros.
    */
   protected final static Symbol SYS_CURRENT_CLASS = Symbol.define("sys:current-class");

   /**
    * Symbol used in in combined validation expressions.
    */
   protected final static Symbol VALIDATION_ARG = Symbol.define("#b");

   /**
    * Expressions that are generally true.
    */
   protected final static Object[] GENERALLY_TRUE_EXPRESSIONS = new Object[]
   {
      Pair.list(Pair.list(Symbol.INVOCATION_CONTEXT), Pair.quote(Symbol.define("partitioned"))),
      Boolean.TRUE,
   };

   /**
    * A comparator for sorting the attributes by ordinal number.
    */
   protected final static Comparator ATTRIBUTE_ORDINAL_COMPARATOR = new Comparator()
   {
      public int compare(Object o1, Object o2)
      {
         return ((Attribute)o1).getOrdinal() - ((Attribute)o2).getOrdinal();
      }
   };

   // attributes

   /**
    * The class visibility.
    */
   protected byte m_nVisibility;

   /**
    * The create transaction mode, one of the Event.TX_* constants.
    */
   protected byte m_nCreateTransactionMode;

   /**
    * The update transaction mode, one of the Event.TX_* constants.
    */
   protected byte m_nUpdateTransactionMode;

   /**
    * True if the class is a pointcut.
    */
   protected boolean m_bPointcut;

   /**
    * The direct aspect count - direct aspects have indexes [0..nCount-1].
    */
   protected int m_nDirectAspectCount;

   /**
    * The class caption string id.
    */
   protected String m_sCaption;

   /**
    * The metadata object description.
    */
   protected String m_sDescription;

   /**
    * The metadata resource name.
    */
   protected String m_sResourceName;

   // associations

   /**
    * The class symbol.
    */
   protected Symbol m_symbol;

   /**
    * The create privilege.
    */
   protected PrimitivePrivilege m_createPrivilege;

   /**
    * The read privilege.
    */
   protected PrimitivePrivilege m_readPrivilege;

   /**
    * The update privilege.
    */
   protected PrimitivePrivilege m_updatePrivilege;

   /**
    * The delete privilege.
    */
   protected PrimitivePrivilege m_deletePrivilege;

   /**
    * The read access attribute.
    */
   protected Attribute m_readAccessAttribute;

   /**
    * The update access attribute.
    */
   protected Attribute m_updateAccessAttribute;

   /**
    * The delete access attribute.
    */
   protected Attribute m_deleteAccessAttribute;

   /**
    * The class event handler collection.
    */
   protected List m_eventList = new ArrayList(4); // of type Event[]

   /**
    * The map of the class attributes (name to attribute).
    */
   protected Lookup m_attributeMap = new HashTab(8); // of type Attribute[String]

   /**
    * The instance attribute list, in their order of declaration.
    */
   protected List m_instanceAttributeList = new ArrayList(8); // of type Attribute[]

   /**
    * The static attribute list, in their order of declaration.
    */
   protected List m_staticAttributeList; // of type Attribute[]

   /**
    * The array of initialized static attributes, sorted by
    * initialization priority. Can be null.
    */
   protected Attribute[] m_initializedStaticAttributeArray;

   /**
    * The derived class collection.
    */
   protected List m_derivedList; // of type Metaclass[]

   /**
    * The base class.
    */
   protected Metaclass m_base;

   /**
    * The topmost class with compatible persistence mapping,
    * such that all the base classes between it an this class
    * are with a compatible persistence mapping.
    */
   protected Metaclass m_persistenceRoot = this;

   /**
    * The persistence alias collection.
    * These are classes with compatible persistence mapping and
    * type code attribute which is used to resolve a different hierarchy.
    */
   protected List m_persistenceAliasList; // of type Metaclass[]

   /**
    * The where expression. Can be null.
    */
   protected Object m_where;

   /**
    * Validation expression taking this argument and returning #f,
    * a string id or a list of format arguments if the instance is invalid.
    * Can be Undefined.VALUE.
    */
   protected Object m_validation = Undefined.VALUE;

   /**
    * The validation function.
    */
   protected Function m_validationFunction;

   /**
    * The validation function text position map
    */
   protected Lookup m_textPosMap;

   /**
    * The attribute containing the instance name.
    */
   protected Attribute m_nameAttribute;

   /**
    * The persistence mapping.
    * Null means that the class is not persisted.
    */
   protected PersistenceMapping m_persistenceMapping;

   /**
    * The selector map: Selector[String].
    */
   protected Lookup m_selectorMap = new HashTab(16);

   /**
    * The pointcut helper.
    */
   protected ClassPointcutHelper m_pointcutHelper;

   /**
    * The root metadata object.
    */
   protected Metadata m_metadata;

   /**
    * The class logger.
    */
   protected Logger m_logger;

   // constructors

   /**
    * Creates a class with a given name.
    * @param sName The class name.
    */
   public Metaclass(String sName)
   {
      super(sName);
      m_symbol = Symbol.define(m_sName);
      m_sCaption = StringUtil.toCaption(m_sName, Metadata.SCOPE_SEP, true);
   }

   // operations

   /**
    * @return The class symbol.
    */
   public final Symbol getSymbol()
   {
      return m_symbol;
   }

   /**
    * @see nexj.core.meta.MetadataResource#setResourceName(java.lang.String)
    */
   public void setResourceName(String sName)
   {
      verifyNotReadOnly();
      m_sResourceName = sName;
   }

   /**
    * @see nexj.core.meta.MetadataResource#getResourceName()
    */
   public String getResourceName()
   {
      return m_sResourceName;
   }

   /**
    * @return false
    * @see nexj.core.meta.Type#isPrimitive()
    */
   public final boolean isPrimitive()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.Type#getBaseType()
    */
   public final Type getBaseType()
   {
      return m_base;
   }

   /**
    * @see nexj.core.meta.Type#isUpcast(nexj.core.meta.Type)
    */
   public final boolean isUpcast(Type type)
   {
      if (type instanceof Metaclass)
      {
         return isUpcast((Metaclass)type);
      }

      return super.isUpcast(type);
   }

   /**
    * @see nexj.core.meta.Type#convert(java.lang.Object)
    */
   public final Object convert(Object value) throws TypeConversionException
   {
      if (value == null)
      {
         return null;
      }

      if (value instanceof Accessor &&
         isUpcast(((Accessor)value).getMetaclass()))
      {
         return value;
      }

      if (value instanceof Typed &&
         isUpcast(((Typed)value).getType()))
      {
         return value;
      }

      throw new TypeConversionException(this);
   }

   /**
    * Sets the class visibility.
    * @param nVisibility The class visibility to set.
    */
   public final void setVisibility(byte nVisibility)
   {
      verifyNotReadOnly();
      m_nVisibility = nVisibility;
   }

   /**
    * @return The class visibility.
    */
   public final byte getVisibility()
   {
      return m_nVisibility;
   }

   /**
    * Sets the attribute containing the instance name.
    * @param sAttribute The name of the attribute containing the instance name to set.
    */
   public final void setNameAttribute(String sAttribute)
   {
      setNameAttribute(
         (sAttribute == null || sAttribute.length() == 0) ?
            null : getAttribute(sAttribute));
   }

   /**
    * Sets the attribute containing the instance name.
    * @param attribute The attribute containing the instance name to set.
    */
   public final void setNameAttribute(Attribute attribute)
   {
      verifyNotReadOnly();

      if (attribute != null)
      {
         if (attribute.getType() != Primitive.STRING)
         {
            throw new MetadataException("err.meta.nameAttributeType",
               new Object[]{attribute.getName(), getName()});
         }

         if (attribute.isStatic())
         {
            throw new MetadataException("err.meta.staticNameAttribute",
               new Object[]{attribute.getName(), getName()});
         }
      }

      m_nameAttribute = attribute;
   }

   /**
    * @return The attribute containing the instance name.
    */
   public final Attribute getNameAttribute()
   {
      return m_nameAttribute;
   }

   /**
    * Sets the class caption string id.
    * @param sCaption The class caption string id to set.
    */
   public final void setCaption(String sCaption)
   {
      verifyNotReadOnly();
      m_sCaption = sCaption;
   }

   /**
    * @return The class caption string id.
    */
   public final String getCaption()
   {
      return m_sCaption;
   }

   /**
    * @see nexj.core.meta.Documented#setDescription(java.lang.String)
    */
   public final void setDescription(String sDescription)
   {
      verifyNotReadOnly();

      if (sDescription != null && sDescription.length() == 0)
      {
         sDescription = null;
      }

      m_sDescription = sDescription;
   }

   /**
    * @see nexj.core.meta.Documented#getDescription()
    */
   public final String getDescription()
   {
      return m_sDescription;
   }

   /**
    * Sets the create privilege.
    * @param createPrivilege The create privilege to set.
    */
   public final void setCreatePrivilege(PrimitivePrivilege createPrivilege)
   {
      verifyNotReadOnly();
      m_createPrivilege = createPrivilege;
   }

   /**
    * @return The create privilege.
    */
   public final PrimitivePrivilege getCreatePrivilege()
   {
      return m_createPrivilege;
   }

   /**
    * Sets the read privilege.
    * @param readPrivilege The read privilege to set.
    */
   public final void setReadPrivilege(PrimitivePrivilege readPrivilege)
   {
      verifyNotReadOnly();
      m_readPrivilege = readPrivilege;
   }

   /**
    * @return The read privilege.
    */
   public final PrimitivePrivilege getReadPrivilege()
   {
      return m_readPrivilege;
   }

   /**
    * Sets the update privilege.
    * @param updatePrivilege The update privilege to set.
    */
   public final void setUpdatePrivilege(PrimitivePrivilege updatePrivilege)
   {
      verifyNotReadOnly();
      m_updatePrivilege = updatePrivilege;
   }

   /**
    * @return The update privilege.
    */
   public final PrimitivePrivilege getUpdatePrivilege()
   {
      return m_updatePrivilege;
   }

   /**
    * Sets the delete privilege.
    * @param deletePrivilege The delete privilege to set.
    */
   public final void setDeletePrivilege(PrimitivePrivilege deletePrivilege)
   {
      verifyNotReadOnly();
      m_deletePrivilege = deletePrivilege;
   }

   /**
    * @return The delete privilege.
    */
   public final PrimitivePrivilege getDeletePrivilege()
   {
      return m_deletePrivilege;
   }

   /**
    * Sets the read access attribute.
    * @param readAccessAttribute The read access attribute to set.
    */
   public final void setReadAccessAttribute(Attribute readAccessAttribute)
   {
      verifyNotReadOnly();
      m_readAccessAttribute = readAccessAttribute;
   }

   /**
    * @return The read access attribute.
    */
   public final Attribute getReadAccessAttribute()
   {
      return m_readAccessAttribute;
   }

   /**
    * Sets the update access attribute.
    * @param updateAccessAttribute The update access attribute to set.
    */
   public final void setUpdateAccessAttribute(Attribute updateAccessAttribute)
   {
      verifyNotReadOnly();
      m_updateAccessAttribute = updateAccessAttribute;
   }

   /**
    * Sets the delete access attribute.
    * @param deleteAccessAttribute The delete access attribute to set.
    */
   public final void setDeleteAccessAttribute(Attribute deleteAccessAttribute)
   {
      verifyNotReadOnly();
      m_deleteAccessAttribute = deleteAccessAttribute;
   }

   /**
    * @return The delete access attribute.
    */
   public final Attribute getDeleteAccessAttribute()
   {
      return m_deleteAccessAttribute;
   }

   /**
    * Sets the create transaction mode.
    * @param nCreateTransactionMode The create transaction mode to set, one of the Event.TX_* constants.
    */
   public final void setCreateTransactionMode(byte nCreateTransactionMode)
   {
      verifyNotReadOnly();
      m_nCreateTransactionMode = nCreateTransactionMode;
   }

   /**
    * @return The create transaction mode, one of the Event.TX_* constants.
    */
   public final byte getCreateTransactionMode()
   {
      return m_nCreateTransactionMode;
   }

   /**
    * @return The update access attribute.
    */
   public final Attribute getUpdateAccessAttribute()
   {
      return m_updateAccessAttribute;
   }

   /**
    * Sets the update transaction mode.
    * @param nUpdateTransactionMode The update transaction mode to set, one of the Event.TX_* constants.
    */
   public final void setUpdateTransactionMode(byte nUpdateTransactionMode)
   {
      verifyNotReadOnly();
      m_nUpdateTransactionMode = nUpdateTransactionMode;
   }

   /**
    * @return The update transaction mode, one of the Event.TX_* constants.
    */
   public final byte getUpdateTransactionMode()
   {
      return m_nUpdateTransactionMode;
   }

   /**
    * Determines if a class can be upcast to obtain this class.
    * @param metaclass The class to upcast.
    * @return True if this class can be obtained by upcasting metaclass.
    */
   public final boolean isUpcast(Metaclass metaclass)
   {
      while (metaclass != null)
      {
         if (metaclass == this)
         {
            return true;
         }

         metaclass = metaclass.m_base;
      }

      return false;
   }

   /**
    * Adds a new event handler to the class.
    * @param event The event handler to add.
    */
   public final void addEvent(Event event)
   {
      verifyNotReadOnly();
      m_eventList.add(event);
      event.setMetaclass(this);

      Selector selector = addSelector(event.getName());

      selector.addMember(event, event.getArgumentCount(), event.isVarArg());
      event.setSelector(selector);
   }

   /**
    * Gets a event handler by ordinal number.
    * @param nOrdinal The event handler ordinal number (0-based).
    * @return The event handler object.
    */
   public final Event getEvent(int nOrdinal)
   {
      return (Event)m_eventList.get(nOrdinal);
   }

   /**
    * @return The event handler count.
    */
   public final int getEventCount()
   {
      return m_eventList.size();
   }

   /**
    * @return An iterator for the contained event handler objects.
    */
   public final Iterator getEventIterator()
   {
      return m_eventList.iterator();
   }

   /**
    * Adds a new attribute to the class.
    * @param attribute The attribute to add.
    * @throws MetadataException if an attribute
    * with the same name already exists.
    */
   public final void addAttribute(Attribute attribute)
   {
      verifyNotReadOnly();

      Object oldAttribute = m_attributeMap.put(attribute.getName(), attribute);

      if (oldAttribute != null)
      {
         m_attributeMap.put(attribute.getName(), oldAttribute);

         throw new MetadataException("err.meta.attributeDup", new Object[] {attribute.getName(), getName()});
      }

      attribute.setMetaclass(this);

      if (attribute.isStatic())
      {
         if (m_staticAttributeList == null)
         {
            m_staticAttributeList = new ArrayList(4);
         }

         m_staticAttributeList.add(attribute);
      }
      else
      {
         m_instanceAttributeList.add(attribute);
      }

      Selector selector = addSelector(attribute.getName());

      selector.addMember(attribute, 0, false);
      selector.addMember(attribute, 1, false);
   }

   /**
    * Gets an attribute by name.
    * @param sName The attribute name.
    * @return The attribute object.
    * @throws MetadataLookupException if the attribute does not exist.
    */
   public final Attribute getAttribute(String sName)
   {
      Attribute attribute = (Attribute)m_attributeMap.get(sName);

      if (attribute != null)
      {
         return attribute;
      }

      throw new MetadataLookupException("err.meta.attributeLookup", sName, this);
   }

   /**
    * Gets an attribute by symbol.
    * @param sym The attribute symbol.
    * @return The attribute object.
    * @throws MetadataLookupException if the attribute does not exist.
    */
   public final Attribute getAttribute(Symbol sym)
   {
      return getAttribute(sym.getName());
   }

   /**
    * Finds an attribute by name.
    * @param sName The attribute name.
    * @return The attribute object, or null if not found.
    */
   public final Attribute findAttribute(String sName)
   {
      return (Attribute)m_attributeMap.get(sName);
   }

   /**
    * Finds an attribute by symbol.
    * @param sym The attribute symbol.
    * @return The attribute object, or null if not found.
    */
   public final Attribute findAttribute(Symbol sym)
   {
      return findAttribute(sym.getName());
   }

   /**
    * Gets a derived attribute given a base attribute.
    * @param attribute The base attribute for which to get the derived one.
    * @return The derived attribute.
    */
   public final Attribute getDerivedAttribute(Attribute attribute)
   {
      if (attribute == null)
      {
         return null;
      }

      List list = (attribute.isStatic()) ? m_staticAttributeList : m_instanceAttributeList;

      if (attribute.getOrdinal() >= list.size())
      {
         return attribute;
      }

      return (Attribute)list.get(attribute.getOrdinal());
   }

   /**
    * Gets an instance attribute by ordinal number.
    * @param nOrdinal The instance attribute ordinal number.
    * @return The attribute object.
    */
   public final Attribute getInstanceAttribute(int nOrdinal)
   {
      return (Attribute)m_instanceAttributeList.get(nOrdinal);
   }

   /**
    * @return The instance attribute count.
    */
   public final int getInstanceAttributeCount()
   {
      return m_instanceAttributeList.size();
   }

   /**
    * @return An iterator for the instance attributes.
    */
   public final Iterator getInstanceAttributeIterator()
   {
      if (m_instanceAttributeList == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_instanceAttributeList.iterator();
   }

   /**
    * Gets an instance attribute by ordinal number.
    * @param nOrdinal The instance attribute ordinal number.
    * @return The attribute object.
    */
   public final Attribute getStaticAttribute(int nOrdinal)
   {
      return (Attribute)m_staticAttributeList.get(nOrdinal);
   }

   /**
    * @return The instance attribute count.
    */
   public final int getStaticAttributeCount()
   {
      if (m_staticAttributeList == null)
      {
         return 0;
      }

      return m_staticAttributeList.size();
   }

   /**
    * @return An iterator for the static attributes.
    */
   public final Iterator getStaticAttributeIterator()
   {
      if (m_staticAttributeList == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_staticAttributeList.iterator();
   }

   /**
    * Gets an initialized static attribute by ordinal number.
    * @param nOrdinal The inzitialized static attribute ordinal number.
    * @return The attribute object.
    */
   public final Attribute getInitializedStaticAttribute(int nOrdinal)
   {
      return m_initializedStaticAttributeArray[nOrdinal];
   }

   /**
    * @return The initialized static attribute count.
    */
   public final int getInitializedStaticAttributeCount()
   {
      if (m_initializedStaticAttributeArray == null)
      {
         return 0;
      }

      return m_initializedStaticAttributeArray.length;
   }

   /**
    * @return The attribute count.
    */
   public final int getAttributeCount()
   {
      return m_attributeMap.size();
   }

   /**
    * @return An iterator for the contained attribute objects.
    */
   public final Iterator getAttributeIterator()
   {
      return m_attributeMap.valueIterator();
   }

   /**
    * Adds a new derived class to the class.
    * @param derived The derived class to add.
    */
   public final void addDerived(Metaclass derived)
   {
      verifyNotReadOnly();

      if (m_derivedList == null)
      {
         m_derivedList = new ArrayList(4);
      }

      m_derivedList.add(derived);
      derived.setBase(this);
   }

   /**
    * Gets a derived class by ordinal number.
    * @param nOrdinal The derived class ordinal number (0-based).
    * @return The derived class object.
    */
   public final Metaclass getDerived(int nOrdinal)
   {
      return (Metaclass)m_derivedList.get(nOrdinal);
   }

   /**
    * @return The derived class count.
    */
   public final int getDerivedCount()
   {
      if (m_derivedList == null)
      {
         return 0;
      }

      return m_derivedList.size();
   }

   /**
    * @return An iterator for the contained derived class objects.
    */
   public final Iterator getDerivedIterator()
   {
      if (m_derivedList == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_derivedList.iterator();
   }

   /**
    * Sets the base class.
    * @param base The base class to set.
    */
   public final void setBase(Metaclass base)
   {
      verifyNotReadOnly();
      m_base = base;
   }

   /**
    * @return The base class.
    */
   public final Metaclass getBase()
   {
      return m_base;
   }

   /**
    * Sets the where expression.
    * @param where The where expression to set.
    */
   public final void setWhere(Object where)
   {
      verifyNotReadOnly();
      m_where = where;
   }

   /**
    * @return The where expression.
    */
   public final Object getWhere()
   {
      return m_where;
   }

   /**
    * Sets the validation expression.
    * @param validation The validation expression to set.
    */
   public final void setValidation(Object validation)
   {
      verifyNotReadOnly();
      m_validation = validation;
   }

   /**
    * @return The validation expression.
    */
   public final Object getValidation()
   {
      return m_validation;
   }

   /**
    * @return The validation function.
    */
   public final Function getValidationFunction()
   {
      return m_validationFunction;
   }

   /**
    * @return The validation function text position map
    */
   public final Lookup getTextPositionMap()
   {
      return m_textPosMap;
   }

   /**
    * Sets the validation function text position map
    * @param textPosMap The validation function text position map to set.
    */
   public final void setTextPositionMap(Lookup textPosMap)
   {
      verifyNotReadOnly();
      m_textPosMap = textPosMap;
   }

   /**
    * Sets the root metadata object.
    * @param metadata The root metadata object to set.
    */
   public final void setMetadata(Metadata metadata)
   {
      verifyNotReadOnly();
      m_metadata = metadata;
   }

   /**
    * @return The root metadata object.
    */
   public final Metadata getMetadata()
   {
      return m_metadata;
   }

   /**
    * Sets the persistence mapping.
    * @param persistenceMapping The persistence mapping to set. Can be null.
    */
   public final void setPersistenceMapping(PersistenceMapping persistenceMapping)
   {
      verifyNotReadOnly();
      m_persistenceMapping = persistenceMapping;
   }

   /**
    * @return The persistence mapping.
    */
   public final PersistenceMapping getPersistenceMapping()
   {
      return m_persistenceMapping;
   }

   /**
    * Visits the persistence mapping.
    * @param visitor The persistence mapping visitor.
    */
   public final void visit(PersistenceMapping.Visitor visitor)
   {
      if (m_persistenceMapping != null)
      {
         m_persistenceMapping.visit(visitor);
      }
   }

   /**
    * @return The topmost class with compatible persistence mapping.
    */
   public final Metaclass getPersistenceRoot()
   {
      return m_persistenceRoot;
   }

   /**
    * Adds a new persistence alias to the class.
    * @param persistenceAlias The persistence alias to add.
    */
   public void addPersistenceAlias(Metaclass persistenceAlias)
   {
      verifyNotReadOnly();

      if (m_persistenceAliasList == null)
      {
         m_persistenceAliasList = new ArrayList(2);
      }

      if (!m_persistenceAliasList.contains(persistenceAlias))
      {
         m_persistenceAliasList.add(persistenceAlias);

         for (int i = 0, n = m_persistenceAliasList.size() - 1; i < n; ++i)
         {
            getPersistenceAlias(i).addPersistenceAlias(persistenceAlias);
         }

         persistenceAlias.addPersistenceAlias(this);
      }
   }

   /**
    * Gets a persistence alias by ordinal number.
    * @param nOrdinal The persistence alias ordinal number (0-based).
    * @return The persistence alias object.
    */
   public Metaclass getPersistenceAlias(int nOrdinal)
   {
      return (Metaclass)m_persistenceAliasList.get(nOrdinal);
   }

   /**
    * @return The persistence alias count.
    */
   public int getPersistenceAliasCount()
   {
      if (m_persistenceAliasList == null)
      {
         return 0;
      }

      return m_persistenceAliasList.size();
   }

   /**
    * @return An iterator for the contained persistence alias objects.
    */
   public Iterator getPersistenceAliasIterator()
   {
      if (m_persistenceAliasList == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_persistenceAliasList.iterator();
   }

   /**
    * Adds a selector to the class.
    * @param sName The selector name.
    * @return The added selector (or an old selector with the same name).
    */
   private final Selector addSelector(String sName)
   {
      Selector selector = (Selector)m_selectorMap.get(sName);

      if (selector == null)
      {
         selector = new Selector(sName);
         selector.setMetaclass(this);
         m_selectorMap.put(sName, selector);
      }

      return selector;
   }

   /**
    * Gets a selector by name.
    * @param sName The selector name.
    * @return The selector object.
    * @throws MetadataLookupException if the selector does not exist.
    */
   public final Selector getSelector(String sName)
   {
      Selector selector = (Selector)m_selectorMap.get(sName);

      if (selector != null)
      {
         return selector;
      }

      throw new MetadataLookupException("err.meta.selectorLookup", sName, this);
   }

   /**
    * Gets a selector by symbol.
    * @param sym The selector symbol.
    * @return The selector object.
    * @throws MetadataLookupException if the selector does not exist.
    */
   public final Selector getSelector(Symbol sym)
   {
      return getSelector(sym.getName());
   }

   /**
    * Finds a selector by name.
    * @param sName The selector name.
    * @return The selector object, or null if not found.
    */
   public final Selector findSelector(String sName)
   {
      return (Selector)m_selectorMap.get(sName);
   }

   /**
    * Finds a selector by symbol.
    * @param sym The selector symbol.
    * @return The selector object, or null if not found.
    */
   public final Selector findSelector(Symbol sym)
   {
      return findSelector(sym.getName());
   }

   /**
    * @return The selector count.
    */
   public final int getSelectorCount()
   {
      return m_selectorMap.size();
   }

   /**
    * @return An iterator for the contained selector objects.
    */
   public final Iterator getSelectorIterator()
   {
      return m_selectorMap.valueIterator();
   }

   /**
    * @return The class logger.
    */
   public final Logger getLogger()
   {
      return m_logger;
   }

   /**
    * Template method to set the next action by name.
    * @param action The action on which to set the next one.
    * @param sName The next action name.
    */
   public void setNextAction(Action action, String sName)
   {
      action.setNextAction(action.getEvent().getAction(sName));
   }

   /**
    * Checks if there is a cycle in the inheritance hierarchy.
    * @throws MetadataException if a cycle is found and this class is in it.
    */
   public final void checkInheritance()
   {
      Metaclass base = this;
      Metaclass base2 = base;
      boolean bInLoop = false;

      for (;;)
      {
         base = base.getBase();
         base2 = base2.getBase();

         if (base2 == null)
         {
            return;
         }

         if (base2 == this)
         {
            bInLoop = true;
         }

         base2 = base2.getBase();

         if (base2 == null)
         {
            return;
         }

         if (base2 == this)
         {
            bInLoop = true;
         }

         if (base2 == base)
         {
            if (bInLoop)
            {
               throw new MetadataException("err.meta.inheritanceCycle", new Object[]{getName()});
            }

            return;
         }
      }
   }

   /**
    * Computes and verifies the inherited attributes,
    * events and actions for this class and its subclasses.
    * @throws MetadataException if inconsistencies are found.
    */
   public final void resolveInheritance()
   {
      ExceptionHolder eh = resolveInheritance(null);

      if (eh != null)
      {
         throw (UncheckedException)eh;
      }
   }

   /**
    * Computes and verifies the inherited attributes,
    * events and actions for this class and its subclasses.
    * @param eh The exception holder where to add the exceptions or null to create a new one.
    * @return The exception holder.
    */
   protected ExceptionHolder resolveInheritance(ExceptionHolder eh)
   {
      verifyNotReadOnly();

      // Assign ordinals to the attributes and sort them

      sortAttributes(m_instanceAttributeList);

      if (m_staticAttributeList != null)
      {
         sortAttributes(m_staticAttributeList);
      }

      // Compute the root declarators

      for (int nStatic = 0; nStatic <= 1; ++nStatic)
      {
         List attributeList = (nStatic == 0) ? m_instanceAttributeList : m_staticAttributeList;

         if (attributeList == null)
         {
            continue;
         }

         for (int i = 0, nCount = attributeList.size(); i < nCount; ++i)
         {
            ((Attribute)attributeList.get(i)).completeDerivation();
         }
      }

      if (m_eventList != null)
      {
         for (int i = 0, nCount = m_eventList.size(); i < nCount; ++i)
         {
            Event derivedEvent = (Event)m_eventList.get(i);
            Event baseEvent = (m_base == null) ? null : m_base.findEvent(derivedEvent.getName(), derivedEvent.getArgumentCount());

            derivedEvent.setRootDeclarator((baseEvent == null) ? this : baseEvent.getRootDeclarator());
         }
      }

      // Resolve the derived classes

      for (int nDerived = 0, nDerivedCount = getDerivedCount(); nDerived < nDerivedCount; ++nDerived)
      {
         Metaclass derivedClass = getDerived(nDerived);

         eh = derivedClass.applyAspects(eh);
         eh = deriveMembers(derivedClass, eh);
         eh = derivedClass.resolveInheritance(eh);
      }

      return eh;
   }

   /**
    * Applies the aspects to this class.
    * @param eh The exception holder where to add the exceptions or null to create a new one.
    * @return The exception holder.
    */
   protected ExceptionHolder applyAspects(ExceptionHolder eh)
   {
      // Add matching aspects

      if (isPointcut())
      {
         for (Iterator itr = m_metadata.getClassAspectIterator(); itr.hasNext();)
         {
            ClassAspect aspect = (ClassAspect)itr.next();

            aspect.addTo(this);
         }
      }

      m_nDirectAspectCount = getAspectCount();

      // Apply aspects

      for (int i = 0, n = getAspectCount(); i < n; ++i)
      {
         ClassAspect aspect = (ClassAspect)getAspect(i);

         AspectManager.logApply(aspect, this);
         eh = aspect.deriveMembers(this, eh);
      }

      return eh;
   }

   /**
    * Derives the members from this class.
    * @param derivedClass The derived class.
    * @param eh The exception holder where to add the exceptions or null to create a new one.
    * @return The exception holder.
    */
   protected ExceptionHolder deriveMembers(Metaclass derivedClass, ExceptionHolder eh)
   {
      // Derive the aspects

      for (int i = 0, n = getAspectCount(); i < n; ++i)
      {
         Aspect aspect = getAspect(i);

         if (derivedClass.findAspectOverride(aspect) < 0)
         {
            derivedClass.addAspectOverride(aspect, true);
         }
      }

      // Derive the attributes

      for (int nStatic = 0; nStatic <= 1; ++nStatic)
      {
         boolean bStatic = (nStatic != 0);
         List attributeList = (bStatic) ? m_staticAttributeList : m_instanceAttributeList;

         if (attributeList == null)
         {
            continue;
         }

         for (int nAttr = 0, nAttrCount = attributeList.size(); nAttr < nAttrCount; ++nAttr)
         {
            Attribute baseAttr = (Attribute)attributeList.get(nAttr);
            Attribute derivedAttr = derivedClass.findAttribute(baseAttr.getName());

            if (derivedAttr == null)
            {
               derivedAttr = (Attribute)baseAttr.clone();
               derivedClass.addAttribute(derivedAttr);
            }
            else
            {
               if (!derivedAttr.isCompatibleWith(baseAttr))
               {
                  MetadataValidationException e = new MetadataValidationException(
                     "err.meta.incompatibleOverriddenAttribute",
                     new Object[]{derivedAttr.getName(), derivedClass.getName(), getName()});

                  derivedAttr.setProperties(e);
                  eh = addException(eh, e);
               }

               derivedAttr.deriveFrom(baseAttr);
            }

            completeAttributeDerivation(derivedAttr, baseAttr);
         }
      }

      // Derive the events

      for (int nEvent = 0, nEventCount = getEventCount(); nEvent < nEventCount; ++nEvent)
      {
         Event baseEvent = getEvent(nEvent);
         Selector derivedSelector = derivedClass.addSelector(baseEvent.getName());
         Member derivedMember = derivedSelector.findMember(baseEvent.getArgumentCount());
         Event derivedEvent = null;

         if (derivedMember == null)
         {
            derivedEvent = (Event)baseEvent.clone();
            derivedClass.addEvent(derivedEvent);
         }
         else if (derivedMember instanceof Event)
         {
            derivedEvent = (Event)derivedMember;

            if (!derivedEvent.isCompatibleWith(baseEvent))
            {
               derivedEvent = null;
            }
            else
            {
               // Resolve the actions: 1st pass
               // (The 2nd pass - resolveActions() - is after the action names fixup)

               int nCopiedActionCount = 0;
               for (int nAction = 0; nAction < baseEvent.getActionCount(); ++nAction)
               {
                  Action baseAction = baseEvent.getAction(nAction);
                  Action derivedAction = derivedEvent.findAction(baseAction.getName());

                  if (derivedAction == null)
                  {
                     derivedEvent.addAction((Action)baseAction.clone());
                     ++nCopiedActionCount;
                  }
                  else if (derivedAction.getType() != baseAction.getType())
                  {
                     MetadataValidationException e = new MetadataValidationException(
                        "err.meta.incompatibleOverriddenAction",
                        new Object[]{derivedAction.getName(), derivedEvent.getName(),
                           derivedClass.getName(), getName()});

                     derivedAction.setProperties(e);
                     eh = addException(eh, e);
                  }
               }

               derivedEvent.rotateActions(nCopiedActionCount);
               derivedEvent.inheritVariables(baseEvent);
               derivedEvent.inheritEventTypes(baseEvent);
            }
         }

         if (derivedEvent == null)
         {
            MetadataValidationException e = new MetadataValidationException(
               "err.meta.incompatibleOverriddenEvent",
               new Object[]{derivedMember.getName(), derivedClass.getName(), getName()});

            derivedMember.setProperties(e);
            eh = addException(eh, e);
         }
         else
         {
            completeEventDerivation(derivedEvent, baseEvent);
         }
      }

      return eh;
   }

   /**
    * Template method to complete an attribute derivation.
    * @param derived The derived attribute.
    * @param base The base attribute.
    */
   protected void completeAttributeDerivation(Attribute derived, Attribute base)
   {
      derived.setOrdinal(base.getOrdinal());
   }

   /**
    * Template method to complete an event derivation.
    * @param derived The derived event.
    * @param base The base event.
    */
   protected void completeEventDerivation(Event derived, Event base)
   {
   }

   /**
    * Resolves the attributes.
    * @param machine The VM for macro expansion.
    * @throws MetadataException if an error occurs.
    */
   public void resolveAttributes(final Machine machine)
   {
      ExceptionHolder eh = visit(new AttributeDependencyVisitor()
      {
         protected void computeDependency(Attribute attribute)
         {
            attribute.resolve(machine);
         }
      }, null);

      if (eh != null)
      {
         throw (UncheckedException)eh;
      }
   }

   /**
    * Computes the attribute inverse dependency.
    * @param depSet The attribute dependency set.
    * @throws MetadataException if an error occurs.
    * @see Attribute#computeInverseDependency(Holder)
    */
   public void computeInverseDependency(final Set depSet)
   {
      ExceptionHolder eh = visit(new AttributeDependencyVisitor()
      {
         protected void computeDependency(Attribute attribute)
         {
            attribute.computeInverseDependency(depSet);
         }
      }, null);

      if (eh != null)
      {
         throw (UncheckedException)eh;
      }
   }

   /**
    * Visits recursively all the attributes, including the derived classes.
    * @param visitor The attribute visitor.
    * @param eh The exception holder where to add the exceptions. Can be null.
    * @return The exception holder.
    */
   protected ExceptionHolder visit(AttributeVisitor visitor, ExceptionHolder eh)
   {
      for (int nStatic = 0; nStatic <= 1; ++nStatic)
      {
         List attrList = (nStatic != 0) ? m_staticAttributeList : m_instanceAttributeList;

         if (attrList == null)
         {
            continue;
         }

         for (int nAttr = 0, nAttrCount = attrList.size(); nAttr < nAttrCount; ++nAttr)
         {
            Attribute attr = (Attribute)attrList.get(nAttr);

            try
            {
               visitor.visit(attr);
            }
            catch (MetadataValidationException e)
            {
               eh = addException(eh, e);
            }
            catch (MetadataException e)
            {
               MetadataValidationException x = new MetadataValidationException(e);

               attr.setProperties(x);
               eh = addException(eh, e);
            }
         }
      }

      for (int nDerived = 0, nDerivedCount = getDerivedCount(); nDerived < nDerivedCount; ++nDerived)
      {
         eh = getDerived(nDerived).visit(visitor, eh);
      }

      return eh;
   }

   /**
    * Adds an exception to an exception holder. Optionally creates the exception holder.
    * @param eh The exception holder or null to create a new one.
    * @param e The exception to add.
    * @return The exception holder.
    */
   protected final static ExceptionHolder addException(ExceptionHolder eh, Throwable e)
   {
      if (eh == null)
      {
         eh = new MetadataCompoundValidationException();
      }

      eh.addException(e);

      return eh;
   }

   /**
    * Additional first pass of inheritance resolution.
    * Creates additional persistence mappings.
    */
   public final void resolveInheritance1()
   {
      boolean bBaseCompatible = false;

      if (m_base != null)
      {
         // Inherit the persistence mapping

         if (m_persistenceMapping == null && m_base.m_persistenceMapping != null)
         {
            m_persistenceMapping = m_base.m_persistenceMapping.create();
            m_persistenceMapping.setMetaclass(this);
            m_persistenceMapping.setDataSource(m_base.m_persistenceMapping.getDataSource());
            bBaseCompatible = true;
         }
         else
         {
            bBaseCompatible = m_persistenceMapping != null &&
               m_base.m_persistenceMapping != null &&
               m_persistenceMapping.isCompatible(m_base.m_persistenceMapping);
         }
      }

      for (int nDerived = 0, nDerivedCount = getDerivedCount(); nDerived < nDerivedCount; ++nDerived)
      {
         getDerived(nDerived).resolveInheritance1();
      }

      // Compute the cumulative dependency of the attributes
      if (bBaseCompatible)
      {
         for (int nStatic = 0; nStatic <= 1; ++nStatic)
         {
            List attrList = (nStatic != 0) ? m_base.m_staticAttributeList : m_base.m_instanceAttributeList;

            if (attrList == null)
            {
               continue;
            }

            for (int nAttr = 0, nAttrCount = attrList.size(); nAttr < nAttrCount; ++nAttr)
            {
               Attribute baseAttr = (Attribute)attrList.get(nAttr);
               Attribute derivedAttr = getDerivedAttribute(baseAttr);

               baseAttr.addCumulativeDependency(derivedAttr.getCumulativeDependency());
               baseAttr.addValueDeclarators(derivedAttr);
            }
         }
      }
   }

   /**
    * Aspect member inheritance resolution.
    * @param eh The exception holder where to add the exceptions. Can be null.
    * @return The exception holder.
    */
   protected ExceptionHolder resolveAspectMembers(ExceptionHolder eh)
   {
      if (m_nDirectAspectCount != 0)
      {
         for (Iterator attrItr = getAttributeIterator(); attrItr.hasNext();)
         {
            Attribute attribute = (Attribute)attrItr.next();

            if (attribute.getReadPrivilege() == null ||
               attribute.getUpdatePrivilege() == null ||
               attribute.getAccessAttribute() == null)
            {
               for (int nAspect = 0; nAspect < m_nDirectAspectCount; ++nAspect)
               {
                  ClassAspect aspect = (ClassAspect)getAspect(nAspect);
                  Attribute aspectAttribute = aspect.findAttribute(attribute.getName());

                  if (aspectAttribute != null)
                  {
                     try
                     {
                        if (attribute.getReadPrivilege() == null)
                        {
                           attribute.setReadPrivilege(aspectAttribute.getReadPrivilege());
                        }

                        if (attribute.getUpdatePrivilege() == null)
                        {
                           attribute.setUpdatePrivilege(aspectAttribute.getUpdatePrivilege());
                        }

                        if (attribute.getReverse() == null &&
                           aspectAttribute.getReverse() != null)
                        {
                           attribute.setReverse(aspectAttribute.getReverse());
                        }

                        if (attribute.getAccessAttribute() == null &&
                           aspectAttribute.getAccessAttribute() != null)
                        {
                           attribute.setAccessAttribute(getAttribute(aspectAttribute.getName()));
                        }
                     }
                     catch (MetadataValidationException e)
                     {
                        eh = addException(eh, e);
                     }
                     catch (UncheckedException e)
                     {
                        MetadataValidationException x = new MetadataValidationException(e);

                        attribute.setProperties(x);
                        eh = addException(eh, x);
                     }
                  }
               }
            }
         }

         for (int nEvent = 0, nEventCount = getEventCount(); nEvent < nEventCount; ++nEvent)
         {
            Event event = getEvent(nEvent);

            if (event.getPrivilege() == null ||
               event.getAccessAttribute() == null ||
               event.getTransactionMode() == Event.TX_DEFAULT ||
               event.getAudited() == null)
            {
               for (int nAspect = 0; nAspect < m_nDirectAspectCount; ++nAspect)
               {
                  ClassAspect aspect = (ClassAspect)getAspect(nAspect);
                  Event aspectEvent = aspect.findEvent(event.getName(), event.getArgumentCount());

                  if (aspectEvent != null)
                  {
                     try
                     {
                        if (event.getPrivilege() == null)
                        {
                           event.setPrivilege(aspectEvent.getPrivilege());
                        }

                        if (event.getAccessAttribute() == null &&
                           aspectEvent.getAccessAttribute() != null)
                        {
                           event.setAccessAttribute(getAttribute(aspectEvent.getAccessAttribute().getName()));
                        }

                        if (event.getTransactionMode() == Event.TX_DEFAULT)
                        {
                           event.setTransactionMode(aspectEvent.getTransactionMode());
                        }

                        if (event.getAudited() == null)
                        {
                           event.setAudited(aspectEvent.getAudited());
                        }
                     }
                     catch (MetadataValidationException e)
                     {
                        eh = addException(eh, e);
                     }
                     catch (UncheckedException e)
                     {
                        MetadataValidationException x = new MetadataValidationException(e);

                        event.setProperties(x);
                        eh = addException(eh, x);
                     }
                  }
               }
            }
         }
      }

      return eh;
   }

   /**
    * Second pass of inheritance resolution.
    * Resolves the persistence mappings, inherited privileges and access attributes.
    * @param urlMap The URL map to use for the validation function. May be null.
    * @param eh The exception holder where to add the exceptions. Can be null.
    * @return The exception holder.
    */
   private final ExceptionHolder resolveInheritance2(Lookup urlMap, ExceptionHolder eh)
   {
      eh = resolveAspectMembers(eh);

      boolean bWhereDiff = true;
      boolean bValidationDiff = true;

      if (m_base != null)
      {
         // Resolve the where clause
         if (m_where == null)
         {
            bWhereDiff = false;
         }
         else
         {
            bWhereDiff = !ObjUtil.equal(m_where, m_base.m_where);
         }

         if (!bWhereDiff)
         {
            m_where = m_base.m_where;
         }

         if (m_validation == Undefined.VALUE)
         {
            bValidationDiff = false;
         }
         else
         {
            bValidationDiff = !ObjUtil.equal(m_validation, m_base.m_validation);
         }

         if (!bValidationDiff)
         {
            m_validation = m_base.m_validation;
         }
      }

      for (int nAspect = 0, nAspectCount = getAspectCount(); nAspect < nAspectCount; ++nAspect)
      {
         ClassAspect aspect = (ClassAspect)getAspect(nAspect);

         if (aspect.getWhere() != null)
         {
            if (bWhereDiff || !m_base.hasAspect(aspect))
            {
               m_where = Pair.commutative(Symbol.AND, m_where, aspect.getWhere());
            }
         }

         if (aspect.getValidation() != Undefined.VALUE)
         {
            if (bValidationDiff || !m_base.hasAspect(aspect))
            {
               Lookup aspectTextPosMap = aspect.getTextPositionMap();

               if (m_textPosMap != null && aspectTextPosMap != null)
               {
                  String sAspectURL = "class:" + aspect.getName() + "$validation";

                  for (Lookup.Iterator it = aspectTextPosMap.iterator(); it.hasNext();)
                  {
                     it.next();

                     m_textPosMap.put(it.getKey(), it.getValue());
                     urlMap.put(it.getValue(), sAspectURL);
                  }
               }

               if (m_validation == Undefined.VALUE)
               {
                  m_validation = aspect.getValidation();
               }
               else
               {
                  m_validation = Pair.list(Pair.list(Symbol.LAMBDA, Pair.list(VALIDATION_ARG),
                     Pair.list(Symbol.IF, Pair.list(Symbol.EQ_P, VALIDATION_ARG, Boolean.TRUE),
                        m_validation, VALIDATION_ARG)), aspect.getValidation());
               }
            }
         }
      }

      if (m_base != null)
      {
         // Resolve the attributes

         for (int nStatic = 0; nStatic <= 1; ++nStatic)
         {
            boolean bStatic = (nStatic != 0);
            List baseList = (bStatic) ? m_base.m_staticAttributeList : m_base.m_instanceAttributeList;

            if (baseList == null)
            {
               continue;
            }

            List derivedList = (bStatic) ? m_staticAttributeList : m_instanceAttributeList;

            for (int nAttr = 0, nAttrCount = baseList.size(); nAttr < nAttrCount; ++nAttr)
            {
               Attribute baseAttr = (Attribute)baseList.get(nAttr);
               Attribute derivedAttr = (Attribute)derivedList.get(nAttr);

               try
               {
                  if (derivedAttr.getReadPrivilege() == null)
                  {
                     derivedAttr.setReadPrivilege(baseAttr.getReadPrivilege());
                  }

                  if (derivedAttr.getUpdatePrivilege() == null)
                  {
                     derivedAttr.setUpdatePrivilege(baseAttr.getUpdatePrivilege());
                  }

                  derivedAttr.setAccessAttribute(getDerivedAttribute((derivedAttr.getAccessAttribute() != null) ?
                     derivedAttr.getAccessAttribute() : baseAttr.getAccessAttribute()));

                  if (derivedAttr.getVisibility() > baseAttr.getVisibility())
                  {
                     throw new MetadataException("err.meta.reducedAttributeVisibility",
                        new Object[]{derivedAttr.getName(), getName(), m_base.getName()});
                  }

                  if (derivedAttr.getCascadeMode() == Attribute.CASCADE_DEFAULT)
                  {
                     derivedAttr.setCascadeMode(baseAttr.getCascadeMode());
                  }

                  if (derivedAttr.getDeclarator() != this)
                  {
                     derivedAttr.setCached(baseAttr.isCached());
                  }
               }
               catch (MetadataValidationException e)
               {
                  eh = addException(eh, e);
               }
               catch (UncheckedException e)
               {
                  MetadataValidationException x = new MetadataValidationException(e);

                  derivedAttr.setProperties(x);
                  eh = addException(eh, x);
               }
            }
         }

         // Resolve the events

         for (int nEvent = 0, nEventCount = m_base.getEventCount(); nEvent < nEventCount; ++nEvent)
         {
            Event baseEvent = m_base.getEvent(nEvent);
            Event derivedEvent = (Event)getSelector(baseEvent.getName()).getMember(baseEvent.getArgumentCount());

            try
            {
               if (derivedEvent.getPrivilege() == null)
               {
                  derivedEvent.setPrivilege(baseEvent.getPrivilege());
               }

               derivedEvent.setAccessAttribute(getDerivedAttribute((derivedEvent.getAccessAttribute() != null) ?
                  derivedEvent.getAccessAttribute() : baseEvent.getAccessAttribute()));

               if (derivedEvent.getTransactionMode() == Event.TX_DEFAULT)
               {
                  derivedEvent.setTransactionMode(baseEvent.getTransactionMode());
               }

               if (derivedEvent.getAudited() == null)
               {
                  derivedEvent.setAudited(baseEvent.getAudited());
               }

               if (derivedEvent.getVisibility() > baseEvent.getVisibility())
               {
                  throw new MetadataException("err.meta.reducedEventVisibility",
                     new Object[]{derivedEvent.getName(), getName(), m_base.getName()});
               }
            }
            catch (MetadataValidationException e)
            {
               eh = addException(eh, e);
            }
            catch (UncheckedException e)
            {
               MetadataValidationException x = new MetadataValidationException(e);

               derivedEvent.setProperties(x);
               eh = addException(eh, x);
            }
         }

         // Check the visibility
         if (m_nVisibility > m_base.m_nVisibility)
         {
            MetadataValidationException e = new MetadataValidationException(
               "err.meta.reducedClassVisibility", new Object[]{getName(), m_base.getName()});

            setProperties(e);
            eh = addException(eh, e);
         }

         // Resolve the name attribute
         if (m_nameAttribute == null)
         {
            m_nameAttribute = getDerivedAttribute(m_base.getNameAttribute());
         }
      }

      for (int nStatic = 0; nStatic <= 1; ++nStatic)
      {
         List attrList = (nStatic != 0) ? m_staticAttributeList : m_instanceAttributeList;

         if (attrList == null)
         {
            continue;
         }

         for (int nAttr = 0, nAttrCount = attrList.size(); nAttr < nAttrCount; ++nAttr)
         {
            Attribute attr = (Attribute)attrList.get(nAttr);

            // Set the default cascade mode to CASCADE_NONE.
            if (attr.getCascadeMode() == Attribute.CASCADE_DEFAULT)
            {
               attr.setCascadeMode(Attribute.CASCADE_NONE);
            }
         }
      }

      for (int nEvent = 0, nEventCount = getEventCount(); nEvent < nEventCount; ++nEvent)
      {
         Event event = getEvent(nEvent);

         // Set the default transaction mode to TX_SUPPORTED.
         if (event.getTransactionMode() == Event.TX_DEFAULT)
         {
            event.setTransactionMode(Event.TX_SUPPORTED);
         }
      }

      // Setup the class-level security and transaction mode

      Event event = findEvent("create", 0);

      if (event != null)
      {
         setCreatePrivilege(event.getPrivilege());
         setCreateTransactionMode(event.getTransactionMode());
      }

      event = findEvent("read", 6);

      if (event != null)
      {
         setReadPrivilege(event.getPrivilege());
         setReadAccessAttribute(event.getAccessAttribute());
      }

      event = findEvent("update", 0);

      if (event != null)
      {
         setUpdatePrivilege(event.getPrivilege());
         setUpdateAccessAttribute(event.getAccessAttribute());
         setUpdateTransactionMode(event.getTransactionMode());
      }

      event = findEvent("delete", 0);

      if (event != null)
      {
         setDeletePrivilege(event.getPrivilege());
         setDeleteAccessAttribute(event.getAccessAttribute());
      }

      if (m_persistenceMapping != null)
      {
         try
         {
            m_persistenceMapping.resolveInheritance();

            if (m_base != null)
            {
               PersistenceMapping baseMapping = m_base.getPersistenceMapping();

               if (baseMapping != null &&
                  baseMapping.isCompatible(m_persistenceMapping))
               {
                  Attribute typeCodeAttr = m_persistenceMapping.getTypeCodeAttribute();

                  if (typeCodeAttr != null &&
                     getDerivedAttribute(baseMapping.getTypeCodeAttribute()) == typeCodeAttr)
                  {
                     m_persistenceRoot = m_base.m_persistenceRoot;
                  }
                  else
                  {
                     for (Metaclass base = m_base; base != null; base = base.getBase())
                     {
                        base = base.getPersistenceRoot();

                        if (base.getPersistenceMapping() != null &&
                           (typeCodeAttr == null || base.getDerivedAttribute(typeCodeAttr) != typeCodeAttr))
                        {
                           base.addPersistenceAlias(this);
                        }
                     }
                  }
               }

               for (int nAttr = 0, nAttrCount = m_instanceAttributeList.size(); nAttr < nAttrCount; ++nAttr)
               {
                  Attribute attr = (Attribute)m_instanceAttributeList.get(nAttr);

                  if (attr.isPersistent() && attr.getRootDeclarator() != this)
                  {
                     Attribute baseAttr = m_base.getInstanceAttribute(attr.getOrdinal());

                     if (baseAttr.isPersistent() &&
                        attr.getPersistenceMapping().isAliasOf(baseAttr.getPersistenceMapping()) &&
                        ObjUtil.equal(attr.getWhere(), baseAttr.getWhere()))
                     {
                        attr.setPersistenceRoot(baseAttr.getPersistenceRoot());
                     }
                  }
               }
            }
         }
         catch (MetadataValidationException e)
         {
            eh = addException(eh, e);
         }
         catch (UncheckedException e)
         {
            MetadataValidationException x = new MetadataValidationException(e);

            setProperties(x);
            eh = addException(eh, x);
         }
      }

      return eh;
   }

   /**
    * Third pass of inheritance resolution.
    * Assigns default privileges and access attributes
    * to the attributes, using the class ones.
    */
   private final void resolveInheritance3()
   {
      for (int nStatic = 0; nStatic <= 1; ++nStatic)
      {
         List attributeList = (nStatic != 0) ? m_staticAttributeList : m_instanceAttributeList;

         if (attributeList == null)
         {
            continue;
         }

         for (int nAttr = 0, nAttrCount = attributeList.size(); nAttr < nAttrCount; ++nAttr)
         {
            Attribute attribute = (Attribute)attributeList.get(nAttr);

            if (attribute.getReadPrivilege() == null)
            {
               attribute.setReadPrivilege(m_readPrivilege);
            }

            if (!attribute.isCollection())
            {
               if (attribute.getUpdatePrivilege() == null)
               {
                  attribute.setUpdatePrivilege(m_updatePrivilege);
               }

               if (!attribute.isStatic() && attribute.getAccessAttribute() == null)
               {
                  attribute.setAccessAttribute(m_updateAccessAttribute);
               }
            }
         }
      }
   }

   /**
    * Fourth pass of inheritance resolution.
    * Runs an additional pass on the persistence mappings.
    */
   public final void resolveInheritance4()
   {
      if (m_persistenceMapping != null)
      {
         m_persistenceMapping.resolveInheritance2();

         for (int i = 0, n = getInstanceAttributeCount(); i != n; ++i)
         {
            Attribute attribute = getInstanceAttribute(i);
            AttributeMapping mapping = attribute.getPersistenceMapping();

            if (mapping != null)
            {
               attribute.setMaxLength();

               for (int k = i - 1; k >= 0; --k)
               {
                  Attribute attribute2 = getInstanceAttribute(k);
                  AttributeMapping mapping2 = attribute2.getPersistenceMapping();

                  if (mapping2 != null)
                  {
                     if (mapping.isAliasOf(mapping2))
                     {
                        // Insert into a circular list
                        attribute.setAlias((attribute2.getAlias() == null) ? attribute2 : attribute2.getAlias());
                        attribute2.setAlias(attribute);

                        break;
                     }
                  }
               }
            }
         }
      }

      for (int nDerived = 0, nDerivedCount = getDerivedCount(); nDerived < nDerivedCount; ++nDerived)
      {
         getDerived(nDerived).resolveInheritance4();
      }
   }

   /**
    * Finds an event with a given name and number of arguments.
    * @param sName The event name.
    * @param nArgCount The event argument count.
    * @return The event metadata object or null if not found.
    */
   public final Event findEvent(String sName, int nArgCount)
   {
      Selector selector = findSelector(sName);

      if (selector != null)
      {
         Member member = selector.findMember(nArgCount);

         if (member instanceof Event)
         {
            return (Event)member;
         }
      }

      return null;
   }

   /**
    * Sorts the actions within the events, computes the action invocation
    * table and compiles the events.
    * @param machine The VM to use for compilation.
    */
   public final void compile(Machine machine)
   {
      Metaclass sysSyncClass = ((Metadata)machine.getContext().getContextMetadata())
         .findMetaclass(Metadata.SYNC_CLASS_CLASS_NAME);
      ExceptionHolder eh = (sysSyncClass == null) ? null : generateSyncDependencyActions(sysSyncClass, null);

      eh = compile(machine, eh);

      if (eh != null)
      {
         throw (UncheckedException)eh;
      }
   }

   /**
    * Generates actions necessary to add change information that affects synchronization to unit of work,
    * in order for synchronization engine to react properly to these changes.
    * @param syncClassMetaclass Metaclass derived from SysSyncClass, that contains information required to generate the
    * action.
    * @param eh The exception holder where to add the exceptions or null to create a new one.
    * @return The exception holder.
    */
   private final ExceptionHolder generateSyncDependencyActions(Metaclass syncClassMetaclass, ExceptionHolder eh)
   {
      Attribute dependencyListAttr = syncClassMetaclass.findAttribute("DEPENDENCY_LIST");

      Attribute attr = (dependencyListAttr != null)
         ? dependencyListAttr : syncClassMetaclass.findAttribute("syncDependency");

      if (attr == null)
      {
         return eh;
      }

      Pair value = (Pair)attr.getValue();
      boolean bValid = Symbol.QUOTE.equals(value.getHead()) && (value.getTail() instanceof Pair);

      if (bValid)
      {
         value = (Pair)((Pair)value.getTail()).getHead();

         while (bValid && value != null)
         {
            bValid = (value.getHead() instanceof Pair);

            if (bValid)
            {
               Pair classAttributes = (Pair)value.getHead();

               bValid = (classAttributes.getHead() instanceof Symbol)
                  && (((Pair)classAttributes.getTail()).getHead() instanceof Pair);

               if (bValid)
               {
                  String className = ((Symbol)classAttributes.getHead()).getName();
                  Pair attributes = (Pair)((Pair)classAttributes.getTail()).getHead();
                  Metaclass metaclass = syncClassMetaclass.getMetadata().findMetaclass(className);

                  bValid = (metaclass != null);

                  if (bValid)
                  {
                     generateSyncDependencyAction(metaclass, attributes);
                  }
               }
            }

            value = (Pair)value.getTail();
         }
      }

      if (!bValid)
      {
         MetadataValidationException x = new MetadataValidationException("err.meta.syncDepClassSyntax",
            new Object[] {attr.getName(), syncClassMetaclass.getName()});

         attr.setProperties(x);
         x.setProperty("item", "value");
         eh = addException(eh, x);

         return eh;
      }

      for (int nDerived = 0, nDerivedCount = syncClassMetaclass.getDerivedCount(); nDerived < nDerivedCount; ++nDerived)
      {
         eh = generateSyncDependencyActions(syncClassMetaclass.getDerived(nDerived), eh);
      }

      return eh;
   }

   /**
    * Generates actions necessary to add change information that affects synchronization to unit of work,
    * in order for synchronization engine to react properly to these changes.
    * @param dependentMetaclass A metaclass that affects synchronization.
    * @param attributes A list of attributes of this metaclass, affecting synchronization.
    */
   private void generateSyncDependencyAction(Metaclass dependentMetaclass, Pair attributes)
   {
      dependentMetaclass.findEvent("commit", 0).generateSyncDependencyAction(attributes);
      dependentMetaclass.findEvent("delete", 0).generateSyncDependencyAction(attributes);

      for (Iterator itr = dependentMetaclass.getDerivedIterator(); itr.hasNext();)
      {
         generateSyncDependencyAction((Metaclass)itr.next(), attributes);
      }
   }

   /**
    * Sorts the actions within the events, computes the action invocation
    * table and compiles the events.
    * @param machine The VM to use for compilation.
    * @param eh The exception holder where to add the exceptions or null to create a new one.
    * @return The exception holder.
    */
   private final ExceptionHolder compile(Machine machine, ExceptionHolder eh)
   {
      verifyNotReadOnly();

      Lookup urlMap = (m_textPosMap == null) ? null : new IdentityHashTab(m_textPosMap.size() * 2);

      eh = resolveInheritance2(urlMap, eh);
      m_logger = Logger.getLogger(LOG_CATEGORY_PREFIX + getName());
      setCurrent();

      // Compile the validation function
      if (m_validation != Undefined.VALUE)
      {
         if (m_base != null && m_validation == m_base.m_validation)
         {
            m_validationFunction = m_base.m_validationFunction;
         }
         else
         {
            try
            {
               Object expr = new Pair(Symbol.LAMBDA, new Pair(new Pair(Symbol.THIS), new Pair(m_validation)));

               if (m_textPosMap != null)
               {
                  TextPosition pos = new TextPosition(0,0);

                  m_textPosMap.put(expr, pos);
                  urlMap.put(pos, "class:" + getName() + "$validation");
               }

               m_validationFunction = new Compiler().compile(expr, m_textPosMap, urlMap, machine, false);
            }
            catch (Exception e)
            {
               MetadataValidationException x;

               if (e instanceof UncheckedException)
               {
                  x = new MetadataValidationException((UncheckedException)e);
               }
               else
               {
                  x = new MetadataValidationException("err.meta.classCompilation", e);
               }

               setProperties(x);
               x.setProperty("item", "validation");

               eh = addException(eh, x);
            }
         }
      }

      // Compile the attributes
      for (int nStatic = 0; nStatic <= 1; ++nStatic)
      {
         List attrList = (nStatic != 0) ? m_staticAttributeList : m_instanceAttributeList;

         if (attrList == null)
         {
            continue;
         }

         for (int nAttr = 0, nAttrCount = attrList.size(); nAttr < nAttrCount; ++nAttr)
         {
            Attribute attr = (Attribute)attrList.get(nAttr);

            if (attr.getDeclarator() == this)
            {
               try
               {
                  attr.compile(machine);
               }
               catch (ValidationException e)
               {
                  eh = addException(eh, e);
               }
               catch (UncheckedException e)
               {
                  MetadataValidationException x = new MetadataValidationException(e);

                  attr.setProperties(x);
                  eh = addException(eh, x);
               }
            }
            else
            {
               attr.setFunctions((Attribute)((nStatic != 0) ? m_base.m_staticAttributeList :
                  m_base.m_instanceAttributeList).get(attr.getOrdinal()));
               attr.setTextPositionMap(null);
            }
         }
      }

      // Compile the events
      for (int nEvent = 0; nEvent < getEventCount(); ++nEvent)
      {
         Event baseEvent = getEvent(nEvent);

         try
         {
            if (baseEvent.getRootDeclarator() == this)
            {
               baseEvent.generateCreateFlowAction();
               baseEvent.generateTriggerFlowAction();
               baseEvent.resolveActions();
            }

            baseEvent.sortActions();

            for (int nDerived = 0, nDerivedCount = getDerivedCount(); nDerived < nDerivedCount; ++nDerived)
            {
               Metaclass derivedClass = getDerived(nDerived);
               Event derivedEvent = (Event)derivedClass.getSelector(baseEvent.getName()).findMember(baseEvent.getArgumentCount());

               derivedEvent.generateCreateFlowAction();
               derivedEvent.generateTriggerFlowAction();
               derivedEvent.resolveActions();
            }
         }
         catch (ValidationException e)
         {
            eh = addException(eh, e);
         }
         catch (UncheckedException e)
         {
            MetadataValidationException x = new MetadataValidationException(e);

            baseEvent.setProperties(x);
            eh = addException(eh, x);
         }

         // Compile only after the derived event action processing
         // so that action code could be copied
         baseEvent.compile(machine);
      }

      clearCurrent();

      for (int nDerived = 0, nDerivedCount = getDerivedCount(); nDerived < nDerivedCount; ++nDerived)
      {
         eh = getDerived(nDerived).compile(machine, eh);
      }

      setTextPositionMap(null);
      resolveInheritance3();

      return eh;
   }

   /**
    * Sets the current class in the global environment.
    */
   protected void setCurrent()
   {
      m_metadata.getGlobalEnvironment().defineVariable(Symbol.SYS_CURRENT_LOGGER, m_logger);
      m_metadata.getGlobalEnvironment().defineVariable(SYS_CURRENT_CLASS, this);
   }

   /**
    * Removes the current attribute from the global environment.
    */
   protected void clearCurrent()
   {
      m_metadata.getGlobalEnvironment().removeVariable(SYS_CURRENT_CLASS);
      m_metadata.getGlobalEnvironment().removeVariable(Symbol.SYS_CURRENT_LOGGER);
   }

   /**
    * Assigns ordinal numbers where necessary to all the attributes
    * and sorts the instance and static attributes according to
    * their ordinal number.
    * @param list The attribute list to sort.
    */
   private final static void sortAttributes(List list)
   {
      int nOrdinal = -1;

      for (int i = 0, nCount = list.size(); i < nCount; ++i)
      {
         nOrdinal = Math.max(nOrdinal, ((Attribute)list.get(i)).getOrdinal());
      }

      for (int i = 0, nCount = list.size(); i < nCount; ++i)
      {
         Attribute attr = (Attribute)list.get(i);

         if (attr.getOrdinal() < 0)
         {
            attr.setOrdinal(++nOrdinal);
         }
      }

      Collections.sort(list, ATTRIBUTE_ORDINAL_COMPARATOR);
   }

   /**
    * @param where The where clause. Can be null.
    * @see PersistenceMapping#getSortKeys(Attribute[], PersistenceMapping[], Attribute[])
    */
   public final Pair getSortKeys(Attribute[] assocs, Object where)
   {
      if (m_persistenceMapping == null)
      {
         return null;
      }

      PersistenceMapping[] mappings = null;
      Set restrictionSet = new HashHolder(1);

      addRestrictions(restrictionSet, getWhere());
      addRestrictions(restrictionSet, where);

      if (assocs != null)
      {
         mappings = new PersistenceMapping[assocs.length];

         for (int i = 0; i < assocs.length; ++i)
         {
            Attribute attribute = assocs[i];

            mappings[i] = attribute.getMetaclass().getPersistenceMapping();
            addRestrictions(restrictionSet, attribute.getWhere());
         }
      }

      return m_persistenceMapping.getSortKeys(assocs, mappings, (restrictionSet.isEmpty()) ? null :
            (Attribute[])restrictionSet.toArray(new Attribute[restrictionSet.size()]));
   }

   /**
    * Adds equality restriction attributes from an S-expression to a set.
    * @param attribSet The restriction attribute set.
    * @param expr The S-expression to analyze.
    */
   protected final void addRestrictions(Set attribSet, Object expr)
   {
      Attribute attribute = getAttribute(expr);

      if (attribute != null)
      {
         if (attribute.getType() == Primitive.BOOLEAN)
         {
            if (attribute.isPersistent())
            {
               attribSet.add(attribute);
            }
            else if (attribute.isCalculated())
            {
               addRestrictions(attribSet, attribute.getValue());
            }
         }
      }
      else if (expr instanceof Pair)
      {
         Pair pair = (Pair)expr;
         Object head = pair.getHead();

         if (Symbol.AND.equals(head))
         {
            while (pair.getTail() instanceof Pair)
            {
               pair = pair.getNext();
               addRestrictions(attribSet, pair.getHead());
            }
         }
         else if (Symbol.EQ.equals(head))
         {
            while (pair.getTail() instanceof Pair)
            {
               pair = pair.getNext();
               attribute = getAttribute(pair.getHead());

               if (attribute != null && attribute.isPersistent())
               {
                  attribSet.add(attribute);
               }
            }
         }
         else if (Symbol.IN_P.equals(head))
         {
            if (pair.getTail() instanceof Pair)
            {
               attribute = getAttribute(pair.getHead());

               if (attribute != null && attribute.isPersistent())
               {
                  attribSet.add(attribute);
               }
            }
         }
         else if (Symbol.IF.equals(head))
         {
            if (pair.getTail() instanceof Pair)
            {
               pair = pair.getNext();

               if (pair.getTail() instanceof Pair)
               {
                  for (int i = 0; i < GENERALLY_TRUE_EXPRESSIONS.length; ++i)
                  {
                     if (GENERALLY_TRUE_EXPRESSIONS[i].equals(pair.getHead()))
                     {
                        addRestrictions(attribSet, pair.getNext().getHead());
                        break;
                     }
                  }
               }
            }
         }
         else if (Symbol.FOLD.equals(head))
         {
            if (pair.getTail() instanceof Pair)
            {
               pair = pair.getNext();

               if (pair.getTail() instanceof Pair)
               {
                  addRestrictions(attribSet, pair.getNext().getHead());
               }
            }
         }
      }
   }

   /**
    * Gets an attribute from an S-expression.
    * @param expr The S-expression.
    * @return The attribute, or null if the expression does not designate an attribute.
    */
   protected final Attribute getAttribute(Object expr)
   {
      if (expr instanceof Pair)
      {
         Pair pair = (Pair)expr;

         if (Symbol.AT.equals(pair.getHead()) && pair.getTail() instanceof Pair)
         {
            pair = pair.getNext();

            if (pair.getHead() instanceof Symbol && pair.getTail() == null)
            {
               return findAttribute((Symbol)pair.getHead());
            }
         }
      }
      else if (expr instanceof Symbol)
      {
         return findAttribute((Symbol)expr);
      }

      return null;
   }

   /**
    * @return List of unique secondary keys.
    * @see PersistenceMapping#getUniqueKeys()
    */
   public final Pair getUniqueKeys()
   {
      if (m_persistenceMapping != null)
      {
         return m_persistenceMapping.getUniqueKeys();
      }

      return null;
   }

   /**
    * Adds a new attribute to a dependency list.
    * @param pair The dependency list, to which to add the attribute. The first pair is not used.
    * @param attribute The attribute to add. Can be null.
    * @param bStatic True to look for static dependencies only.
    * @return The pair, in which head the attribute was added, or null for an invalid attribute.
    */
   private final static Pair addDependency(Pair pair, Attribute attribute, boolean bStatic)
   {
      if (attribute == null || attribute.isStatic() != bStatic)
      {
         return null;
      }

      while (pair.getTail() != null)
      {
         pair = pair.getNext();

         if (pair.getHead() == attribute ||
            pair.getHead() instanceof Pair && ((Pair)pair.getHead()).getHead() == attribute)
         {
            return pair;
         }
      }

      pair.setTail(new Pair(attribute));

      return pair.getNext();
   }

   /**
    * Adds dependency for expressions like (... (lambda ... ) expr1 ... exprN).
    * @param pair The pair containing the lambda arguments in its head (i.e. the next pair after lambda).
    * @param expr The list of expressions expr1 .. exprN. 
    * @param bStatic True to look for static dependencies only.
    * @param dep The list to accumulate the dependencies in. The head is true if
    *           dependencies are known fully.
    * @param local The top local variable binding. Can be null.
    * @param machine The VM from where to get and expand the macros.
    * @return The computed dependency list. Can be null.
    */
   protected Pair addFrameDependency(Pair pair, Pair expr, boolean bStatic, Pair dep, Local local, Machine machine)
   {
      Pair adep = null;
      Object head = pair.getHead();

      if (head instanceof Symbol)
      {
         local = new Local((Symbol)head, null, local);
      }
      else if (head instanceof Pair)
      {
         boolean bFirst = true;

         for (Pair arg = (Pair)head;; bFirst = false)
         {
            head = arg.getHead();

            Pair edep = (expr == null) ? null : dependency(expr.getHead(), bStatic, dep, local, machine);

            adep = (bFirst) ? edep : null;

            if (head instanceof Symbol)
            {
               local = new Local((Symbol)head, adep, local);
            }

            if (expr != null)
            {
               if (expr.getTail() instanceof Pair)
               {
                  expr = expr.getNext();
               }
               else
               {
                  if (expr.getTail() != null)
                  {
                     adep = null;
                  }

                  expr = null;
               }
            }

            head = arg.getTail();

            if (head instanceof Pair)
            {
               arg = arg.getNext();
            }
            else
            {
               if (head instanceof Symbol)
               {
                  local = new Local((Symbol)head, null, local);
               }

               break;
            }
         }
      }

      while (expr != null)
      {
         adep = null;
         dependency(expr.getHead(), bStatic, dep, local, machine);

         if (expr.getTail() instanceof Pair)
         {
            expr = expr.getNext();
         }
         else
         {
            break;
         }
      }

      while (pair.getTail() instanceof Pair)
      {
         pair = pair.getNext();
         dependency(pair.getHead(), bStatic, dep, local, machine);
      }

      return adep;
   }

   /**
    * Finds an attribute name from an expression ((quote name)).
    * @param pair The expression.
    * @return The attribute name or null if not found.
    */
   protected static String findAttributeName(Pair pair)
   {
      if (pair != null && pair.getHead() instanceof Pair && pair.getTail() == null)
      {
         pair = (Pair)pair.getHead();

         if (Symbol.QUOTE.equals(pair.getHead()) &&
            pair.getTail() instanceof Pair)
         {
            pair = pair.getNext();

            if (pair.getHead() instanceof Symbol && pair.getTail() == null)
            {
               return pair.getHead().toString();
            }
         }
      }

      return null;
   }

   /**
    * Computes the dependency list of an S-expression relative to this class.
    * @param expr The S-expression.
    * @param bStatic True to look for static dependencies only.
    * @param dep The list to accumulate the dependencies in. The head is true if
    *           dependencies are known fully.
    * @param local The top local variable binding. Can be null.
    * @param machine The VM from where to get and expand the macros.
    * @return The computed dependency list. Can be null.
    */
   protected final Pair dependency(Object expr, boolean bStatic, final Pair dep, Local local, Machine machine)
   {
      return dependency(expr, bStatic, false, dep, local, machine);
   }

   /**
    * Computes the dependency list of an S-expression relative to this class.
    * @param expr The S-expression.
    * @param bStatic True to look for static dependencies only.
    * @param bThis True to consider the instance as a dependency.
    * @param dep The list to accumulate the dependencies in. The head is true if
    *           dependencies are known fully.
    * @param local The top local variable binding. Can be null.
    * @param machine The VM from where to get and expand the macros.
    * @return The computed dependency list. Can be null.
    */
   protected final Pair dependency(Object expr, boolean bStatic, boolean bThis, final Pair dep, Local local, Machine machine)
   {
      if (expr instanceof Pair)
      {
         Pair adep = dep;
         Pair pair = (Pair)expr;
         Object obj = pair.getHead();
         Symbol sym = null;
         boolean bGlobal = false;

         // Extract the function name for simple function calls
         if (obj instanceof Symbol)
         {
            sym = (Symbol)obj;
         }
         else if (obj instanceof Pair)
         {
            Pair head = (Pair)obj;

            // (global <sym>)
            if (Symbol.GLOBAL.equals(head.getHead()) &&
               (obj = head.getTail()) instanceof Pair &&
               (head = (Pair)obj).getTail() == null &&
               (obj = head.getHead()) instanceof Symbol)
            {
               sym = (Symbol)obj;
               bGlobal = true;
            }
         }

         Object tail = pair.getTail();

         if (sym != null)
         {
            if (tail == null || tail instanceof Pair)
            {
               Local var = (bGlobal) ? null : Local.find(sym, local);

               if (var != null)
               {
                  adep = var.dep;
                  pair = (Pair)tail;
               }
               else
               {
                  obj = machine.getGlobalEnvironment().findVariable(sym);

                  if ((obj instanceof Macro || obj instanceof SyntaxFunction)
                     && !Symbol.AND.equals(sym) && !Symbol.OR.equals(sym) && !Symbol.ANY.equals(sym))
                  {
                     if (obj instanceof Macro)
                     {
                        return dependency(machine.invoke((Function)obj, (Pair)tail), bStatic, bThis, dep, local, machine);
                     }

                     if (obj instanceof SyntaxFunction)
                     {
                        return dependency(machine.getTransformerContext().expandTransformer((SyntaxFunction)obj, pair),
                           bStatic, bThis, dep, local, machine);
                     }
                  }

                  pair = (Pair)tail;

                  if (Symbol.THIS.equals(sym))
                  {
                     if (!bGlobal)
                     {
                        String sName = findAttributeName(pair);

                        if (sName != null)
                        {
                           pair = addDependency(dep, findAttribute(sName), bStatic);

                           if (pair == null)
                           {
                              dep.setHead(Boolean.FALSE);
                           }

                           return pair;
                        }

                        dep.setHead(Boolean.FALSE);
                     }
                  }
                  else if (Symbol.QUOTE.equals(sym))
                  {
                     return null;
                  }
                  else if (Symbol.DECLARE.equals(sym))
                  {
                     Pair head;

                     // Anything other than (declare scope client) disables full dependency
                     if (Symbol.SCOPE.equals(pair.getHead()) &&
                        (obj = pair.getTail()) instanceof Pair &&
                        (obj = (head = (Pair)obj).getHead()) instanceof Symbol &&
                        ((Symbol)obj).getName().equals("client") &&
                        head.getTail() == null)
                     {
                        dep.setHead(Boolean.FALSE);
                     }

                     return null;
                  }
                  else if (Symbol.SET.equals(sym) || Symbol.DEFINE.equals(sym))
                  {
                     dep.setHead(Boolean.FALSE);

                     if (pair.getTail() == null || pair.getTail() instanceof Pair)
                     {
                        pair = pair.getNext();
                     }
                  }
                  else if (Symbol.LAMBDA.equals(sym) || Symbol.MACRO.equals(sym))
                  {
                     dep.setHead(Boolean.FALSE);

                     return addFrameDependency(pair, null, bStatic, dep, local, machine);
                  }
                  else if (Symbol.IF.equals(sym) || Symbol.BEGIN.equals(sym) || Symbol.OR.equals(sym) || Symbol.AND.equals(sym))
                  {
                     // Supported for client-side calculation
                  }
                  else if (Symbol.FILTER.equals(sym) || Symbol.FOR_EACH.equals(sym) || Symbol.MAP.equals(sym))
                  {
                     if (pair.getHead() instanceof Pair && pair.getTail() instanceof Pair)
                     {
                        Pair head = (Pair)pair.getHead();

                        if (Symbol.LAMBDA.equals(head.getHead()) && head.getTail() instanceof Pair)
                        {
                           adep = addFrameDependency(head.getNext(), pair.getNext(), bStatic, adep, local, machine);

                           return (Symbol.FILTER.equals(sym)) ? adep : null;
                        }
                     }
                  }
                  else if (Symbol.VALUE_COLLECTION.equals(sym))
                  {
                     if (pair.getTail() == null)
                     {
                        return dependency(pair.getHead(), bStatic, adep, local, machine);
                     }
                  }
                  else if (Symbol.INSTANCE_P.equals(sym))
                  {
                     adep = dependency(pair.getHead(), bStatic, !bStatic, dep, local, machine);

                     if (adep != null && !bStatic)
                     {
                        Metaclass metaclass = null;

                        if (adep.getHead() instanceof Metaclass)
                        {
                           metaclass = (Metaclass)adep.getHead();
                           adep = dep;
                        }
                        else
                        {
                           boolean bPair = (adep.getHead() instanceof Pair);

                           if (bPair)
                           {
                              adep = (Pair)adep.getHead();
                           }

                           Attribute attribute = (Attribute)adep.getHead();
                           Type type = attribute.getType();

                           if (!type.isPrimitive())
                           {
                              metaclass = (Metaclass)type;

                              if (!bPair)
                              {
                                 adep.setHead(new Pair(adep.getHead()));
                                 adep = (Pair)adep.getHead();
                              }
                           }
                        }

                        PersistenceMapping mapping = null;
                        Attribute typeCodeAttribute = null;

                        for (Metaclass base = metaclass; base != null; base = base.getBase())
                        {
                           PersistenceMapping baseMapping = base.getPersistenceMapping();

                           if (baseMapping != null)
                           {
                              if (mapping != null && !mapping.isCompatible(baseMapping))
                              {
                                 break;
                              }

                              mapping = baseMapping;
                              typeCodeAttribute = mapping.getTypeCodeAttribute();

                              if (typeCodeAttribute != null)
                              {
                                 break;
                              }
                           }
                        }

                        if (typeCodeAttribute != null)
                        {
                           addDependency(adep, metaclass.getDerivedAttribute(typeCodeAttribute), bStatic);
                        }
                     }

                     if (pair.getTail() instanceof Pair &&
                        pair.getNext().getHead() instanceof Metaclass)
                     {
                        dep.setHead(Boolean.FALSE);
                     }

                     return null;
                  }
                  else if (dep.getHead() != Boolean.FALSE && !m_metadata.isClientSymbol(sym))
                  {
                     dep.setHead(Boolean.FALSE);
                  }
               }
            }
            else
            {
               dep.setHead(Boolean.FALSE);

               return null;
            }
         }
         else
         {
            Object head = pair.getHead();

            if (head instanceof Pair)
            {
               Pair lambda = (Pair)head;

               if (Symbol.LAMBDA.equals(lambda.getHead()) && lambda.getTail() instanceof Pair && tail instanceof Pair)
               {
                  // Dynamic derived association expansion: ((lambda (this) ... filter ...) assoc)
                  return addFrameDependency(lambda.getNext(), pair.getNext(), bStatic, dep, local, machine);
               }
            }

            adep = dependency(head, bStatic, dep, local, machine);

            if (tail instanceof Pair)
            {
               pair = (Pair)tail;
            }
            else
            {
               pair = null;
            }
         }

         if (adep != dep)
         {
            if (adep != null && !bStatic)
            {
               String sName = findAttributeName(pair);

               if (sName != null)
               {
                  boolean bPair = (adep.getHead() instanceof Pair);

                  if (bPair)
                  {
                     adep = (Pair)adep.getHead();
                  }
                  
                  Attribute attribute = (Attribute)adep.getHead();
                  Type type = attribute.getType();

                  if (type.isPrimitive())
                  {
                     dep.setHead(Boolean.FALSE);

                     return null;
                  }

                  if (!bPair)
                  {
                     adep.setHead(new Pair(adep.getHead()));
                     adep = (Pair)adep.getHead();
                  }

                  adep = addDependency(adep, ((Metaclass)type).findAttribute(sName), bStatic);

                  if (adep == null)
                  {
                     dep.setHead(Boolean.FALSE);
                  }

                  return adep;
               }
            }

            dep.setHead(Boolean.FALSE);
         }

         // Process arguments
         for (; pair != null; pair = pair.getNext())
         {
            dependency(pair.getHead(), bStatic, dep, local, machine);

            if (!(pair.getTail() instanceof Pair))
            {
               break;
            }
         }
      }
      else if (bThis && expr instanceof Symbol)
      {
         Symbol sym = (Symbol)expr;
         Local var = Local.find(sym, local);

         if (var != null)
         {
            return var.dep;
         }

         if (!bStatic && Symbol.THIS.equals(sym))
         {
            return new Pair(this);
         }
      }

      return null;
   }

   /**
    * Computes the dependency list of an S-expression.
    * @param expr The S-expression.
    * @param bStatic True to look for static dependencies only.
    * @param machine The VM from where to get and expand the macros.
    * @return The computed dependency list.
    */
   public final Pair dependency(Object expr, boolean bStatic, Machine machine)
   {
      if (expr instanceof Pair)
      {
         Pair dep = new Pair(Boolean.TRUE);

         dependency(expr, bStatic, dep, null, machine);

         return dep.getNext();
      }

      return null;
   }

   /**
    * Resolves the initializers in this class and its subclasses.
    * @param machine The VM for macro expansion.
    */
   public final void resolveInitializers(Machine machine)
   {
      verifyNotReadOnly();

      sortInitializedAttributes(false, machine);
      m_initializedStaticAttributeArray = sortInitializedAttributes(true, machine);

      for (int i = 0, nCount = getDerivedCount(); i < nCount; ++i)
      {
         getDerived(i).resolveInitializers(machine);
      }
   }

   /**
    * Sorts topologically the attributes with initializers.
    * @param bStatic True to sort the static attributes, false to sort the instance attributes.
    * @param machine The VM for macro expansion.
    * @return The array of initialized attributes, sorted in the order of their dependency. Can be null.
    */
   private final Attribute[] sortInitializedAttributes(boolean bStatic, Machine machine)
   {
      List attributeList = (bStatic) ? m_staticAttributeList : m_instanceAttributeList;

      if (attributeList == null)
      {
         return null;
      }

      List noPredList = null;
      Pair[] depArray = null;
      int[] nCountArray = null;
      int nCount = attributeList.size();
      int nInitCount = 0;

      for (int i = 0; i < nCount; ++i)
      {
         Attribute attribute = (Attribute)attributeList.get(i);

         if (attribute.isStatic() == bStatic && attribute.getInitializer() != Undefined.VALUE)
         {
            if (nInitCount++ == 0)
            {
               noPredList = new ArrayList();
               depArray = new Pair[nCount];
               nCountArray = new int[nCount];
            }

            for (Pair dep = dependency(attribute.getInitializer(), bStatic, machine);
               dep != null; dep = dep.getNext())
            {
               Attribute src = (Attribute)((dep.getHead() instanceof Pair) ?
                  ((Pair)dep.getHead()).getHead() : dep.getHead());

               if (src.isStatic() == bStatic && src.getInitializer() != Undefined.VALUE)
               {
                  depArray[src.getOrdinal()] = new Pair(attribute, depArray[src.getOrdinal()]);
                  ++nCountArray[attribute.getOrdinal()];
               }
            }
         }
      }

      if (depArray == null)
      {
         return null;
      }

      for (int i = 0; i < nCount; ++i)
      {
         Attribute attribute = (Attribute)attributeList.get(i);

         if (attribute.isStatic() == bStatic && attribute.getInitializer() != Undefined.VALUE)
         {
            if (nCountArray[attribute.getOrdinal()] == 0)
            {
               noPredList.add(attribute);
            }
         }
      }

      Attribute[] initArray = new Attribute[nInitCount];
      int nLastInit = 0;

      for (int i = 0; i < noPredList.size(); ++i)
      {
         Attribute attribute = (Attribute)noPredList.get(i);

         initArray[nLastInit++] = attribute;

         for (Pair dep = depArray[attribute.getOrdinal()]; dep != null; dep = dep.getNext())
         {
            attribute = (Attribute)dep.getHead();

            if (--nCountArray[attribute.getOrdinal()] == 0)
            {
               noPredList.add(attribute);
            }
         }
      }

      if (nLastInit != nInitCount)
      {
         for (int i = 0; i < nCountArray.length; ++i)
         {
            if (nCountArray[i] != 0)
            {
               throw new MetadataException("err.meta.attributeDepCycle",
                  new Object[]{((Attribute)attributeList.get(i)).getName(), getName()});
            }
         }
      }

      return initArray;
   }

   /**
    * @see nexj.core.meta.Accessor#getMetaclass()
    */
   public final Metaclass getMetaclass()
   {
      return this;
   }

   /**
    * @see nexj.core.meta.Accessor#getLazyMetaclass()
    */
   public final Metaclass getLazyMetaclass()
   {
      return this;
   }

   /**
    * @see nexj.core.meta.Accessor#isLazy()
    */
   public final boolean isLazy()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.Accessor#getValue(int)
    */
   public final Object getValue(int nOrdinal)
   {
      Object[] valueArray = getState();
      Object value = valueArray[nOrdinal];

      if (value instanceof Undefined)
      {
         Attribute attribute = getStaticAttribute(nOrdinal);

         if (attribute.getValueFunction() != null)
         {
            value = ThreadContextHolder.getContext().getMachine()
               .invoke(attribute.getValueFunction(), this, (Object[])null);
         }
         else
         {
            value = null;
         }

         if (attribute.isCached())
         {
            valueArray[nOrdinal] = value;
         }
      }

      return value;
   }

   /**
    * @see nexj.core.meta.Accessor#getValue(java.lang.String)
    */
   public final Object getValue(String sName)
   {
      Attribute attribute = getAttribute(sName);

      if (attribute.isStatic())
      {
         return getValue(attribute.getOrdinal());
      }

      throw new ScriptingException("err.scripting.staticAttribute",
         new Object[]{attribute.getName(), getName()});
   }

   /**
    * @see nexj.core.meta.Accessor#getValue(java.lang.String, java.lang.String)
    */
   public Object getValue(String sName, String sFallbackName)
   {
      Attribute attribute = findAttribute(sName);

      if (attribute == null)
      {
         if (sFallbackName == null)
         {
            return null;
         }

         attribute = getAttribute(sFallbackName);
      }

      if (attribute.isStatic())
      {
         return getValue(attribute.getOrdinal());
      }

      throw new ScriptingException("err.scripting.staticAttribute",
         new Object[]{attribute.getName(), getName()});
   }

   /**
    * @see nexj.core.meta.Accessor#setValue(int, java.lang.Object)
    */
   public final void setValue(int nOrdinal, Object value)
   {
      Object[] valueArray = getState();

      if (valueArray[nOrdinal] != value)
      {
         Attribute attribute = getStaticAttribute(nOrdinal);

         // Check the access rights

         Context context = ThreadContextHolder.getContext();

         if (context.isSecure())
         {
            if (!attribute.isUpdateable(context.getPrivilegeSet()))
            {
               throw new SecurityViolationException(
                  (attribute.isReadOnly()) ?
                     "err.runtime.attributeReadOnlyAccess" :
                     "err.runtime.attributeUpdateAccess",
                  new Object[]{attribute.getName(), getName()});
            }

            Attribute accessAttribute = attribute.getAccessAttribute();

            if (accessAttribute != null)
            {
               Object accessValue = getValue(accessAttribute.getOrdinal());

               if (accessValue instanceof Boolean && !((Boolean)accessValue).booleanValue())
               {
                  throw new SecurityViolationException("err.runtime.attributeUpdateAccess",
                     new Object[]{attribute.getName(), getName()});
               }
            }
         }
         else
         {
            if (attribute.isReadOnly())
            {
               throw new SecurityViolationException("err.runtime.attributeReadOnlyAccess",
                  new Object[]{attribute.getName(), getName()});
            }
         }

         attribute.invalidateDependency(this, Invalid.VALUE);
         valueArray[nOrdinal] = value;
      }
   }

   /**
    * @see nexj.core.meta.Accessor#setValue(java.lang.String, java.lang.Object)
    */
   public final void setValue(String sName, Object value)
   {
      Attribute attribute = getAttribute(sName);

      if (attribute.isStatic())
      {
         setValue(attribute.getOrdinal(), value);
      }
      else
      {
         throw new ScriptingException("err.scripting.staticAttribute",
            new Object[]{attribute.getName(), getName()});
      }
   }

   /**
    * @see nexj.core.meta.Accessor#getValueDirect(int)
    */
   public final Object getValueDirect(int nOrdinal)
   {
      return getState()[nOrdinal];
   }

   /**
    * @see nexj.core.meta.Accessor#setValueDirect(int, java.lang.Object)
    */
   public final void setValueDirect(int nOrdinal, Object value)
   {
      getState()[nOrdinal] = value;
   }

   /**
    * @see nexj.core.meta.Accessor#invalidate(int, Object)
    */
   public final void invalidate(int nOrdinal, Object value)
   {
      getState()[nOrdinal] = value;
   }

   /**
    * @return The class object state (with lazy construction).
    */
   private final Object[] getState()
   {
      Machine machine = ThreadContextHolder.getContext().getMachine();
      GlobalEnvironment env = machine.getGlobalEnvironment();
      Object[] values = (Object[])env.getState(this);

      if (values == null)
      {
         values = new Object[getStaticAttributeCount()];
         Arrays.fill(values, Undefined.VALUE);
         env.setState(this, values);

         int nCount = getInitializedStaticAttributeCount();

         if (nCount != 0)
         {
            for (int i = 0; i < nCount; ++i)
            {
               Attribute attribute = getInitializedStaticAttribute(i);

               values[attribute.getOrdinal()] = machine.invoke(attribute.getInitializerFunction(), this, (Object[])null);
            }
         }
      }

      return values;
   }

   /**
    * @see nexj.core.meta.Accessor#invoke(java.lang.String, java.lang.Object[])
    */
   public final Object invoke(String sName, Object[] args)
   {
      Event event = (Event)getSelector(sName).getMember((args != null) ? args.length : 0);

      if (!event.isStatic())
      {
         throw new ScriptingException("err.scripting.staticEvent", new Object[]{sName, getName()});
      }

      return event.invoke(this, args, ThreadContextHolder.getContext().getMachine());
   }

   /**
    * @see nexj.core.meta.Accessor#invoke(java.lang.String, nexj.core.scripting.Pair)
    */
   public final Object invoke(String sName, Pair args)
   {
      Event event = (Event)getSelector(sName).getMember(Pair.length(args));

      if (!event.isStatic())
      {
         throw new ScriptingException("err.scripting.staticEvent", new Object[]{sName, getName()});
      }

      return event.invoke(this, args, ThreadContextHolder.getContext().getMachine());
   }

   /**
    * @see nexj.core.meta.Accessor#invoke(java.lang.String)
    */
   public final Object invoke(String sName)
   {
      return invoke(sName, (Object[])null);
   }

   /**
    * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
    */
   public final boolean invoke(int nArgCount, Machine machine)
   {
      if (nArgCount != 0)
      {
         Object sym = machine.getArg(0, nArgCount);

         if (sym instanceof Symbol)
         {
            Selector selector = findSelector(((Symbol)sym).getName());

            if (selector == null)
            {
               try
               {
                  return machine.invokeJavaMethod(this, nArgCount);
               }
               catch (ScriptingException e)
               {
                  if ("err.scripting.unknownMethod".equals(e.getErrorCode()))
                  {
                     throw new MetadataLookupException("err.meta.selectorLookup", sym.toString(), this);
                  }

                  throw e;
               }
            }

            Member member = selector.getMember(nArgCount - 1);

            if (member.isStatic())
            {
               if (member.isAttribute())
               {
                  if (nArgCount == 1)
                  {
                     machine.returnValue(getValue(((Attribute)member).getOrdinal()), nArgCount);
                  }
                  else if (nArgCount == 2)
                  {
                     Object value = machine.getArg(1, nArgCount);

                     setValue(((Attribute)member).getOrdinal(), value);
                     machine.returnValue(value, nArgCount);
                  }
                  else
                  {
                     throw new ScriptingException("err.scripting.maxArgCount",
                        new Object[]{toString(),
                           Primitive.ONE_INTEGER,
                           Primitive.createInteger(nArgCount - 1)});
                  }

                  return false;
               }
               else
               {
                  machine.setArg(0, nArgCount, this);

                  if (m_logger.isDebugEnabled())
                  {
                     ((Event)member).dump(nArgCount, machine);
                  }

                  return ((Event)member).getFunction().invoke(nArgCount, machine);
               }
            }
            else
            {
               try
               {
                  return machine.invokeJavaMethod(this, nArgCount);
               }
               catch (ScriptingException e)
               {
                  if ("err.scripting.unknownMethod".equals(e.getErrorCode()))
                  {
                     throw new ScriptingException(
                        (member.isAttribute()) ?
                           "err.scripting.staticAttribute" :
                           "err.scripting.staticEvent",
                        new Object[]{sym, getName()});
                  }

                  throw e;
               }
            }
         }
      }
      else
      {
         throw new ScriptingException("err.scripting.minArgCount",
            new Object[]{getName(),
               Primitive.ONE_INTEGER,
               Primitive.createInteger(nArgCount)});
      }

      throw new ScriptingException("err.scripting.funCall");
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public final void makeReadOnly()
   {
      for (Iterator itr = getAttributeIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getEventIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getSelectorIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      if (m_persistenceMapping != null)
      {
         m_persistenceMapping.makeReadOnly();
      }

      super.makeReadOnly();

      if (m_derivedList instanceof ArrayList)
      {
         ((ArrayList)m_derivedList).trimToSize();
      }

      if (m_eventList instanceof ArrayList)
      {
         ((ArrayList)m_eventList).trimToSize();
      }

      if (m_instanceAttributeList instanceof ArrayList)
      {
         ((ArrayList)m_instanceAttributeList).trimToSize();
      }

      if (m_persistenceAliasList instanceof ArrayList)
      {
         ((ArrayList)m_persistenceAliasList).trimToSize();
      }

      if (m_staticAttributeList instanceof ArrayList)
      {
         ((ArrayList)m_staticAttributeList).trimToSize();
      }

      if (m_validationFunction instanceof ArrayList)
      {
         ((ArrayList)m_validationFunction).trimToSize();
      }

      m_textPosMap = null; // free memory not used after compile()
   }

   /**
    * @return True if the class is readable according to the visibility and the privilege set.
    */
   public final boolean isReadable(PrivilegeSet privilegeSet)
   {
      return m_nVisibility == PUBLIC &&
         (m_readPrivilege == null || privilegeSet.contains(m_readPrivilege));
   }

   /**
    * Checks the class visibility and read access against a privilege set.
    * @param privilegeSet The privilege set containing the allowed privileges.
    * @throws SecurityViolationException if the visibility is not public or the access is denied.
    */
   public final void checkReadAccess(PrivilegeSet privilegeSet) throws SecurityViolationException
   {
      if (m_nVisibility != PUBLIC)
      {
         throw new SecurityViolationException("err.rpc.classVisibility", new Object[]{getName()});
      }

      if (m_readPrivilege != null && !privilegeSet.contains(m_readPrivilege))
      {
         throw new SecurityViolationException("err.rpc.classReadPrivilege",
            new Object[]{getName(), m_readPrivilege.getName()});
      }
   }

   /**
    * Verifies that the read access to all the attributes of the class is granted.
    * @param attributes The attribute list to check.
    * @param privilegeSet The privilege set of the current principal.
    * @return List of security attributes that can be appended to the input attributes.
    * @throws SecurityViolationException the access is not granted to any of the attributes.
    */
   public Pair checkReadAccess(Pair attributes, PrivilegeSet privilegeSet) throws SecurityViolationException
   {
      Attribute readAccessAttribute = m_readAccessAttribute;
      Pair security = null;

      for (; attributes != null; attributes = attributes.getNext())
      {
         Object head = attributes.getHead();
         Pair pair = null;

         if (head instanceof Pair)
         {
            pair = (Pair)head;
            head = pair.getHead();

            if (Symbol.ATAT.equals(head))
            {
               pair = pair.getNext();
               head = pair.getHead();

               pair = m_metadata.getMetaclass(((Symbol)head).getName())
                  .checkReadAccess(pair.getNext(), privilegeSet);

               if (pair != null)
               {
                  security = new Pair(new Pair(Symbol.ATAT, new Pair(head, pair)), security);
               }

               continue;
            }
         }

         Attribute attribute = findAttribute((Symbol)head);

         if (attribute != null)
         {
            if (attribute == readAccessAttribute)
            {
               readAccessAttribute = null;
            }

            attribute.checkReadAccess(privilegeSet);

            if (!attribute.getType().isPrimitive())
            {
               pair = ((Metaclass)attribute.getType())
                  .checkReadAccess((pair == null) ? null : pair.getNext(), privilegeSet);

               if (pair != null)
               {
                  security = new Pair(new Pair (head, pair), security);
               }
            }
         }
      }

      if (readAccessAttribute != null)
      {
         security = new Pair(readAccessAttribute.getSymbol(), security);
      }

      return security;
   }

   /**
    * Checks the read access of an order by clause.
    * @param orderBy The order by clause.
    * @param privilegeSet The privilege set of the current principal.
    * @throws SecurityViolationException the access is not granted to any of the attributes.
    */
   public void checkOrderByAccess(Object orderBy, PrivilegeSet privilegeSet)
   {
      Pair pair;

      for (; orderBy instanceof Pair; orderBy = pair.getTail())
      {
         pair = (Pair)orderBy;

         if (pair.getHead() instanceof Pair)
         {
            checkExpressionAccess(((Pair)pair.getHead()).getHead(), privilegeSet);
         }
      }
   }

   /**
    * Checks the read access of an expression.
    * @param expr The expression.
    * @param privilegeSet The privilege set of the current principal.
    * @throws SecurityViolationException the access is not granted to any of the attributes.
    */
   public void checkExpressionAccess(Object expr, PrivilegeSet privilegeSet)
   {
      Metaclass metaclass = this;

      if (expr instanceof Pair)
      {
         Pair pair = (Pair)expr;
         Object head = pair.getHead();

         expr = pair.getTail();

         if (head instanceof Symbol)
         {
            if (Symbol.ATAT.equals(head) && expr instanceof Pair)
            {
               pair = (Pair)expr;
               head = pair.getHead();

               if (!(head instanceof Symbol))
               {
                  return;
               }

               metaclass = m_metadata.findMetaclass(head.toString());

               if (metaclass == null)
               {
                  return;
               }

               metaclass.checkReadAccess(privilegeSet);

               head = Symbol.AT;
               expr = pair.getTail();
            }

            if (Symbol.AT.equals(head))
            {
               for (; expr instanceof Pair; expr = pair.getTail())
               {
                  pair = (Pair)expr;
                  head = pair.getHead();

                  if (head instanceof Pair)
                  {
                     metaclass.checkExpressionAccess(head, privilegeSet);
                     metaclass = metaclass.getDerived(head);

                     continue;
                  }

                  if (!(head instanceof Symbol))
                  {
                     break;
                  }

                  Attribute attribute = metaclass.findAttribute((Symbol)head);

                  if (attribute == null)
                  {
                     break;
                  }

                  attribute.checkReadAccess(privilegeSet);

                  if (attribute.getType().isPrimitive())
                  {
                     break;
                  }

                  metaclass = (Metaclass)attribute.getType();
               }

               return;
            }

            if (expr instanceof Pair)
            {
               Metaclass clazz = m_metadata.findMetaclass(head.toString());

               if (clazz != null)
               {
                  if (m_nVisibility != PUBLIC)
                  {
                     throw new SecurityViolationException("err.rpc.classVisibility", new Object[]{clazz.getName()});
                  }

                  pair = (Pair)expr;

                  if (pair.getHead() instanceof Pair)
                  {
                     int nArgCount = 0;
                     Object next;

                     for (next = pair.getTail(); next instanceof Pair; next = ((Pair)next).getTail())
                     {
                        ++nArgCount;
                     }

                     if (next == null)
                     {
                        pair = (Pair)pair.getHead();

                        if (Symbol.QUOTE.equals(pair.getHead()) && pair.getTail() instanceof Pair)
                        {
                           pair = pair.getNext();

                           if (pair.getHead() instanceof Symbol && pair.getTail() == null)
                           {
                              Member member = clazz.getSelector(pair.getHead().toString()).getMember(nArgCount);

                              if (!member.isStatic())
                              {
                                 throw new ScriptingException(
                                    (member.isAttribute()) ?
                                       "err.scripting.staticAttribute" :
                                       "err.scripting.staticEvent",
                                    new Object[]{member.getName(), clazz.getName()});
                              }

                              if (member.isAttribute())
                              {
                                 if (nArgCount == 0)
                                 {
                                    ((Attribute)member).checkReadAccess(privilegeSet);
                                 }
                                 else
                                 {
                                    ((Attribute)member).checkUpdateAccess(privilegeSet);
                                 }
                              }
                              else
                              {
                                 ((Event)member).checkAccess(privilegeSet);
                              }

                              expr = ((Pair)expr).getTail();
                              head = null;
                           }
                        }
                     }
                  }
               }
            }

            if (head != null && !m_metadata.isPublicSymbol((Symbol)head))
            {
               throw new SecurityViolationException("err.rpc.functionVisibility",
                  new Object[]{head});
            }
         }
         else if (head instanceof Pair)
         {
            throw new SecurityViolationException("err.rpc.expressionVisibility",
               new Object[]{head});
         }

         for (; expr instanceof Pair; expr = pair.getTail())
         {
            pair = (Pair)expr;
            metaclass.checkExpressionAccess(pair.getHead(), privilegeSet);
         }
      }
      else if (expr instanceof Symbol)
      {
         Attribute attribute  = metaclass.findAttribute((Symbol)expr);

         if (attribute != null)
         {
            attribute.checkReadAccess(privilegeSet);
         }
      }
   }

   /**
    * Gets the most derived class object according to a filter.
    * @param filter The filter expression. Can be null.
    * @return The most derived class object (can be this).
    */
   public Metaclass getDerived(Object filter)
   {
      Metaclass base = this;

      if (filter instanceof Pair)
      {
         Pair pair = (Pair)filter;
         Object head = pair.getHead();

         if (Symbol.AND.equals(head))
         {
            // (and ...)
            for (pair = pair.getNext(); pair != null; pair = pair.getNext())
            {
               base = base.getDerived(pair.getHead());
            }
         }
         else if (Symbol.INSTANCE_P.equals(head))
         {
            // (instance? (@) <class>)
            Metaclass metaclass = base.getCastMetaclass(pair);

            if (metaclass != null)
            {
               base = metaclass;
            }
         }
      }

      return base;
   }

   /**
    * Gets a class object from an (instance? (@) class) expression.
    * @param expr The instance? expression.
    * @return The class object or null if the expression is not (instance? (@) class).
    */
   public Metaclass getCastMetaclass(Pair expr) throws MetadataException
   {
      if (Symbol.INSTANCE_P.equals(expr.getHead()))
      {
         expr = expr.getNext();

         if (expr != null)
         {
            Object head = expr.getHead();

            if (head instanceof Pair)
            {
               Pair assoc = (Pair)head;

               if (Symbol.AT.equals(assoc.getHead()) && assoc.getTail() == null)
               {
                  expr = expr.getNext();

                  if (expr != null && expr.getTail() == null)
                  {
                     head = expr.getHead();

                     if (head instanceof Symbol)
                     {
                        Metaclass metaclass = m_metadata.getMetaclass(head.toString());

                        if (metaclass.isUpcast(this))
                        {
                           return this;
                        }

                        return metaclass;
                     }
                  }
               }
            }
         }
      }

      return null;
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#findAttributeMeta(java.lang.String)
    */
   public final AttributeMeta findAttributeMeta(String sName)
   {
      return findAttribute(sName);
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#findDerivedClassMeta(java.lang.String)
    */
   public final ClassMeta findDerivedClassMeta(String sDerivedName)
   {
      return findDerivedMetaclass(sDerivedName);
   }

   /**
    * Find a derived metaclass.
    * @param sDerivedName The name of the derived Metaclass.
    * @return The Metaclass; null if not found.
    */
   public final Metaclass findDerivedMetaclass(String sDerivedName)
   {
      Metaclass derived = m_metadata.findMetaclass(sDerivedName);

      if (derived != null && derived.getBase() == this)
      {
         return derived;
      }

      return null;
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getAttributeMeta(java.lang.String)
    */
   public final AttributeMeta getAttributeMeta(String sName)
   {
      return getAttribute(sName);
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getAttributeMetaCount()
    */
   public final int getAttributeMetaCount()
   {
      return m_attributeMap.size();
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getAttributeMetaIterator()
    */
   public final Iterator getAttributeMetaIterator()
   {
      return m_attributeMap.valueIterator();
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getBaseClassMeta()
    */
   public final ClassMeta getBaseClassMeta()
   {
      return m_base;
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getCreatePrivilegeOrdinal()
    */
   public final int getCreatePrivilegeOrdinal()
   {
      if (m_createPrivilege != null)
      {
         return m_createPrivilege.getOrdinal();
      }

      return -1;
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getDeleteAccessAttributeMeta()
    */
   public final AttributeMeta getDeleteAccessAttributeMeta()
   {
      return m_deleteAccessAttribute;
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getDeletePrivilegeOrdinal()
    */
   public final int getDeletePrivilegeOrdinal()
   {
      if (m_deletePrivilege != null)
      {
         return m_deletePrivilege.getOrdinal();
      }

      return -1;
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getDerivedAttributeMetaCount()
    */
   public final int getDerivedAttributeMetaCount()
   {
      return getDerivedCount();
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getDerivedAttributeMetaIterator()
    */
   public final Iterator getDerivedAttributeMetaIterator()
   {
      return getDerivedIterator();
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getDerivedClassMeta(java.lang.String)
    */
   public final ClassMeta getDerivedClassMeta(String sName)
   {
      ClassMeta derived = findDerivedClassMeta(sName);

      if (derived == null)
      {
         throw new MetadataLookupException("err.meta.derivedClassLookup", sName, this);
      }

      return derived;
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getLockingAttributeMeta()
    */
   public final AttributeMeta getLockingAttributeMeta()
   {
      return (m_persistenceMapping == null) ? null : m_persistenceMapping.getLockingAttribute();
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getNameAttributeMeta()
    */
   public final AttributeMeta getNameAttributeMeta()
   {
      return m_nameAttribute;
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getReadPrivilegeOrdinal()
    */
   public final int getReadPrivilegeOrdinal()
   {
      if (m_readPrivilege != null)
      {
         return m_readPrivilege.getOrdinal();
      }

      return -1;
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getUpdateAccessAttributeMeta()
    */
   public final AttributeMeta getUpdateAccessAttributeMeta()
   {
      return m_updateAccessAttribute;
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getUpdatePrivilegeOrdinal()
    */
   public final int getUpdatePrivilegeOrdinal()
   {
      if (m_updatePrivilege != null)
      {
         return m_updatePrivilege.getOrdinal();
      }

      return -1;
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#isUpcast(nexj.core.meta.ui.ClassMeta)
    */
   public final boolean isUpcast(ClassMeta metaclass)
   {
      while (metaclass != null)
      {
         if (metaclass == this)
         {
            return true;
         }

         metaclass = metaclass.getBaseClassMeta();
      }

      return false;
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getInstanceAttributeMeta(int)
    */
   public AttributeMeta getInstanceAttributeMeta(int nOrdinal)
   {
      return getInstanceAttribute(nOrdinal);
   }

   /**
    * @see nexj.core.meta.ui.ClassMeta#getStaticAttributeMeta(int)
    */
   public AttributeMeta getStaticAttributeMeta(int nOrdinal)
   {
      return getStaticAttribute(nOrdinal);
   }
   
   /**
    * Sets the pointcut flag.
    * @param bPointcut The pointcut flag.
    */
   public final void setPointcut(boolean bPointcut)
   {
      verifyNotReadOnly();
      m_bPointcut = bPointcut;
   }

   /**
    * @see nexj.core.meta.Pointcut#isPointcut()
    */
   public final boolean isPointcut()
   {
      return m_bPointcut;
   }

   /**
    * @see nexj.core.meta.Pointcut#addAspect(nexj.core.meta.Aspect)
    */
   public final void addAspect(Aspect aspect) throws MetadataException
   {
      verifyNotReadOnly();

      if (m_pointcutHelper == null)
      {
         m_pointcutHelper = new ClassPointcutHelper(this);
      }

      m_pointcutHelper.addAspect(aspect);
   }

   /**
    * @see nexj.core.meta.Pointcut#removeAspect(nexj.core.meta.Aspect)
    */
   public final boolean removeAspect(Aspect aspect)
   {
      if (m_pointcutHelper != null)
      {
         return m_pointcutHelper.removeAspect(aspect);
      }

      return false;
   }

   /**
    * @see nexj.core.meta.Pointcut#getAspect(int)
    */
   public final Aspect getAspect(int nOrdinal)
   {
      return m_pointcutHelper.getAspect(nOrdinal);
   }

   /**
    * @see nexj.core.meta.Pointcut#hasAspect(nexj.core.meta.Aspect)
    */
   public final boolean hasAspect(Aspect aspect)
   {
      if (m_pointcutHelper == null)
      {
         return false;
      }

      return m_pointcutHelper.hasAspect(aspect);
   }

   /**
    * @see nexj.core.meta.Pointcut#getAspectCount()
    */
   public final int getAspectCount()
   {
      if (m_pointcutHelper == null)
      {
         return 0;
      }

      return m_pointcutHelper.getAspectCount();
   }

   /**
    * @return The direct aspect count - direct aspects have indexes [0..nCount-1].
    */
   public final int getDirectAspectCount()
   {
      return m_nDirectAspectCount;
   }

   /**
    * @see nexj.core.meta.Pointcut#addAspectOverride(nexj.core.meta.Aspect, boolean)
    */
   public final void addAspectOverride(Aspect aspect, boolean bInclusive) throws MetadataException
   {
      verifyNotReadOnly();

      if (m_pointcutHelper == null)
      {
         m_pointcutHelper = new ClassPointcutHelper(this);
      }

      m_pointcutHelper.addAspectOverride(aspect, bInclusive);
   }

   /**
    * @see nexj.core.meta.Pointcut#removeAspectOverride(nexj.core.meta.Aspect)
    */
   public final boolean removeAspectOverride(Aspect aspect)
   {
      if (m_pointcutHelper != null)
      {
         return m_pointcutHelper.removeAspectOverride(aspect);
      }

      return false;
   }

   /**
    * @see nexj.core.meta.Pointcut#findAspectOverride(nexj.core.meta.Aspect)
    */
   public final int findAspectOverride(Aspect aspect)
   {
      if (m_pointcutHelper == null)
      {
         return -1;
      }

      return m_pointcutHelper.findAspectOverride(aspect);
   }

   /**
    * @see nexj.core.meta.Pointcut#getAspectOverride(int)
    */
   public final Aspect getAspectOverride(int nOrdinal)
   {
      return m_pointcutHelper.getAspectOverride(nOrdinal);
   }

   /**
    * @see nexj.core.meta.Pointcut#isAspectOverrideInclusive(int)
    */
   public final boolean isAspectOverrideInclusive(int nOrdinal)
   {
      return m_pointcutHelper.isAspectOverrideInclusive(nOrdinal);
   }

   /**
    * @see nexj.core.meta.Pointcut#getAspectOverrideCount()
    */
   public final int getAspectOverrideCount()
   {
      if (m_pointcutHelper == null)
      {
         return 0;
      }

      return m_pointcutHelper.getAspectOverrideCount();
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      marker.setResourceName(m_sResourceName);
      marker.setTypeName("Class");
      marker.setProperty("class", m_sName);
   }

   /**
    * @see nexj.core.meta.NamedMetadataObject#createLookupException()
    */
   protected MetadataException createLookupException()
   {
      return new MetadataLookupException("err.meta.metaclassLookup", m_sName, m_metadata.getName());
   }

   /**
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      super.validate(metadata, warnings);
      validate(getEventIterator(), metadata, warnings);

      if (m_persistenceMapping != null)
      {
         m_persistenceMapping.validate(metadata, warnings);
      }
   }

   /**
    * Creates an compatibility conflict exception.
    * @param sErrCode The error string identifier.
    * @param argArray The error string arguments.
    */
   public MetadataValidationException createCompatibilityException(String sErrCode, Object[] argArray)
   {
      MetadataValidationException e = new MetadataCompatibilityException(sErrCode, argArray);

      e.setTypeName("Class");
      e.setResourceName(m_sResourceName);
      e.setProperty("class", m_sName);

      return e;
   }

   /**
    * Checks whether a metaclass is compatible with this instance.
    * @param metaclass The metaclass. Can be null if no corresponding metaclass exists.
    * @param eh The holder in which to add exceptions.
    */
   public void checkCompatibility(Metaclass metaclass, ExceptionHolder eh)
   {
      if (checkCompatibility(getAttributeIterator(), metaclass, eh))
      {
         checkCompatibility(getEventIterator(), metaclass, eh);
      }
   }

   /**
    * Checks compatibility over an iteration of members.
    * @param memberItr Iterator over members.
    * @param metaclass The metaclass. Can be null if no corresponding metaclass exists.
    * @param eh The holder in which to add exceptions.
    * @return false if there exists a compatible member but metaclass is null.
    */
   public boolean checkCompatibility(Iterator memberItr, Metaclass metaclass, ExceptionHolder eh)
   {
      while (memberItr.hasNext())
      {
         Member compatibleMember = (Member)memberItr.next();

         if (compatibleMember.isCompatible())
         {
            if (metaclass == null)
            {
               eh.addException(createCompatibilityException("err.meta.missingCompatibleClass", new Object[] {getName()}));
               return false;
            }

            compatibleMember.checkCompatibility(metaclass, eh);
         }
      }

      return true;
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#getChild(java.lang.String)
    */
   public EndpointPart getChild(String sName)
   {
      if (!StringUtil.isEmpty(sName) && sName.charAt(0) == ':')
      {
         if (sName.equals(":oid"))
         {
            return Transformation.OID;
         }

         if (sName.equals(":class"))
         {
            return Transformation.CLASS;
         }

         if (sName.equals(":event"))
         {
            return Transformation.EVENT;
         }
      }

      return getAttribute(sName);
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#findChild(java.lang.String)
    */
   public EndpointPart findChild(String sName)
   {
      if (!StringUtil.isEmpty(sName) && sName.charAt(0) == ':')
      {
         if (sName.equals(":oid"))
         {
            return Transformation.OID;
         }

         if (sName.equals(":class"))
         {
            return Transformation.CLASS;
         }

         if (sName.equals(":event"))
         {
            return Transformation.EVENT;
         }
      }

      return findAttribute(sName);
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#getChildIterator()
    */
   public Iterator getChildIterator()
   {
      return getAttributeIterator();
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#isCollection()
    */
   public boolean isCollection()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#getBaseEndpoint()
    */
   public TransformationEndpoint getBaseEndpoint()
   {
      return m_base;
   }

   /**
    * @see nexj.core.meta.integration.TransformationEndpoint#isUpcast(nexj.core.meta.integration.TransformationEndpoint)
    */
   public boolean isUpcast(TransformationEndpoint endpoint)
   {
      return (endpoint instanceof Metaclass) && isUpcast((Metaclass)endpoint);
   }

   /**
    * @see nexj.core.meta.integration.TransformationEndpoint#getEndpoint(java.lang.String)
    */
   public TransformationEndpoint getEndpoint(String sName)
   {
      return m_metadata.getMetaclass(sName);
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#createObject()
    */
   public TransferObject createObject()
   {
      return new TransferObject(getName());
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#getValue(nexj.core.util.PropertyMap, java.lang.Object)
    */
   public Object getValue(PropertyMap map, Object defValue)
   {
      throw new IllegalStateException();
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#setValue(nexj.core.util.PropertyMap, java.lang.Object)
    */
   public void setValue(PropertyMap map, Object value)
   {
      throw new IllegalStateException();
   }

   // inner classes

   /**
    * Metaclass-specific pointcut helper.
    */
   protected final static class ClassPointcutHelper extends PointcutHelper
   {
      protected Metaclass m_metaclass;

      public ClassPointcutHelper(Metaclass metaclass)
      {
         m_metaclass = metaclass;
      }

      /**
       * @see nexj.core.meta.PointcutHelper#getContainer()
       */
      protected Pointcut getContainer()
      {
         return m_metaclass;
      }

      /**
       * @see nexj.core.meta.PointcutHelper#getContainerType()
       */
      protected String getContainerType()
      {
         return "class";
      }
   }

   /**
    * Interface implemented by attribute visitors.
    */
   protected interface AttributeVisitor
   {
      /**
       * Visits the specified attribute.
       * @param attribute The attribute to visit.
       */
      void visit(Attribute attribute);
   }

   /**
    * Attribute visitor for dependency computation.
    */
   protected abstract static class AttributeDependencyVisitor implements AttributeVisitor
   {
      public final void visit(Attribute attribute)
      {
         if (attribute.getDeclarator() == attribute.getMetaclass() &&
            (attribute.getValue() != Undefined.VALUE ||
               attribute.getDependency() != null ||
               attribute.getOrderBy() != null))
         {
            computeDependency(attribute);
         }
      }

      protected abstract void computeDependency(Attribute attribute);
   }

   /**
    * Represents a local variable dependency binding.
    */
   protected static class Local
   {
      // associations

      /**
       * The variable symbol.
       */
      public Symbol symbol;

      /**
       * The dependency list.
       */
      public Pair dep;

      /**
       * The next binding in the list.
       */
      public Local next;

      // constructors

      /**
       * Constructs the local variable binding.
       * @param symbol The variable name.
       * @param dep The dependency list. Can be null.
       * @param next The next binding in the list. Can be null.
       */
      public Local(Symbol symbol, Pair dep, Local next)
      {
         this.symbol = symbol;
         this.dep = dep;
         this.next = next;
      }

      // operations

      /**
       * Finds a binding by variable symbol.
       * @param symbol The variable symbol.
       * @param first The first binding in the linked list. Can be null.
       * @return The found binding, or null if not found.
       */
      public static Local find(Symbol symbol, Local first)
      {
         while (first != null && !first.symbol.equals(symbol))
         {
            first = first.next;
         }

         return first;
      }
   }
}
