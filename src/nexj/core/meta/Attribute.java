// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.io.InvalidObjectException;
import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import nexj.core.meta.integration.EndpointPart;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.ClassMapping;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.meta.ui.AttributeMeta;
import nexj.core.meta.ui.ClassMeta;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.SecurityViolationException;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Function;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Captioned;
import nexj.core.util.EmptyIterator;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashHolder;
import nexj.core.util.Holder;
import nexj.core.util.IdentityHashHolder;
import nexj.core.util.Invalid;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.PropertyMap;
import nexj.core.util.StringUtil;
import nexj.core.util.UncheckedException;
import nexj.core.util.Undefined;

/**
 * The attribute metadata.
 */
public final class Attribute extends Member implements AttributeMeta, Captioned, Cloneable, Serializable, EndpointPart
{
   // constants
   
   /**
    * Serial Id.
    */
   private static final long serialVersionUID = 916910521644664010L;

   /**
    * Inherit the cascade property, or CASCADE_NONE without inheritance.
    */
   public final static byte CASCADE_DEFAULT = 0;
   
   /**
    * No cascading is performed.
    */
   public final static byte CASCADE_NONE = 1;
   
   /**
    * Cascading deletes are performed.
    */
   public final static byte CASCADE_DELETE = 2;
   
   /**
    * Cascading clearing is performed.
    */
   public final static byte CASCADE_CLEAR = 3;
   
   /**
    * The delete is cancelled if the association is available.
    */
   public final static byte CASCADE_CANCEL = 4;

   /**
    * The arguments for the value expression.
    */
   private final static Pair VALUE_ARGUMENTS = new ConstPair(Symbol.THIS);

   /**
    * The arguments for the validation expression.
    */
   private final static Pair VALIDATION_ARGUMENTS = ConstPair.list(Symbol.THIS, Symbol.VALUE);

   /**
    * The current attribute global variable used by macros.
    */
   private final static Symbol SYS_CURRENT_ATTRIBUTE = Symbol.define("sys:current-attribute");

   /**
    * S-expression designating this object, suitable for the object query.
    */
   private final static Pair THIS = new ConstPair(Symbol.AT);

   // attributes

   /**
    * The attribute ordinal number in the class (0..n).
    * Class and instance attributes have independent numbering.
    */
   private int m_nOrdinal = -1;

   /**
    * The maximum attribute value length.
    */
   private int m_nMaxLength;

   /**
    * True if the attribute cannot be null.
    */
   private boolean m_bRequired;

   /**
    * True if the attribute holds a collection of values.
    */
   private boolean m_bCollection;
   
   /**
    * The constrained enumeration flag.
    */
   private boolean m_bConstrained;

   /**
    * The read-only value flag.
    */
   private boolean m_bReadOnly;

   /**
    * Whether dependency information is known exactly.
    */
   private boolean m_bFullDependency;

   /**
    * The caching flag.
    */
   private byte m_nCached = -1;

   /**
    * The cascade mode - one of the CASCADE_* constants.
    */
   private byte m_nCascadeMode;

   /**
    * The derived association where expression. Can be null.
    */
   private Object m_where;

   /**
    * The attribute caption string id.
    */
   private String m_sCaption;

   // associations

   /**
    * The attribute type.
    */
   private Type m_type;

   /**
    * The enumeration class.
    */
   private Metaclass m_enumeration;

   /**
    * The attribute corresponding to the reverse association.
    */
   private Attribute m_reverse;

   /**
    * The inherited attribute with a compatible mapping from the tompost class.
    */
   private Attribute m_persistenceRoot = this;

   /**
    * The next attribute with the same persistence mapping in a circular list.
    */
   private Attribute m_alias;

   /**
    * The read privilege.
    */
   private PrimitivePrivilege m_readPrivilege;

   /**
    * The update privilege.
    */
   private PrimitivePrivilege m_updatePrivilege;

   /**
    * The expression used to calculate the attribute value.
    * Can be Undefined.VALUE.
    */
   private Object m_value = Undefined.VALUE;

   /**
    * The value function.
    */
   private Function m_valueFunction;

   /**
    * The expression used to calculate the initial attribute value.
    * Can be Undefined.VALUE.
    */
   private Object m_initializer = Undefined.VALUE;

   /**
    * The initializer function.
    */
   private Function m_initializerFunction;

   /**
    * Validation expression taking this and value arguments and returning #f,
    * a string id or a list of format arguments if the attribute is invalid. 
    * Can be Undefined.VALUE.
    */
   private Object m_validation = Undefined.VALUE;

   /**
    * The validation function.
    */
   private Function m_validationFunction;

   /**
    * The attribute dependency list: (attr1 attr2 (assoc1 attr1_1 ... attr1_N) ... attrN).
    */
   private Pair m_dependency;

   /**
    * The cumulative dependency (incl. subclasses): (attr1 attr2 (assoc1 attr1_1 ...) (class2 attr2_1 ...) ...).
    */
   private Pair m_cumulativeDependency;

   /**
    * The attribute inverse dependency list.
    */
   private InverseDependencyList m_inverseDependency;

   /**
    * The attribute alternative order by list: ((attr1 . bAscFlag1) ... (attrN . bAscFlagN)).
    */
   private Pair m_orderBy;

   /**
    * The overriding value declarator collection.
    * These are subclasses with different value expressions for the same attribute.
    */
   private List m_valueDeclaratorList; // of type Metaclass

   /**
    * The text position map for attribute's value, initializer, and validation
    */
   private Lookup m_textPosMap;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(Attribute.class);
   
   // constructor

   /**
    * Constructs an attribute with a given name. 
    * @param sName The attribute name.
    */
   public Attribute(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * @return true
    * @see nexj.core.meta.Member#isAttribute()
    */
   public boolean isAttribute()
   {
     return true;
   }

   /**
    * Sets the attribute ordinal number in the class.
    * @param nOrdinal The attribute ordinal number in the class to set.
    */
   public void setOrdinal(int nOrdinal)
   {
      verifyNotReadOnly();
      m_nOrdinal = nOrdinal;
   }

   /**
    * @return The attribute ordinal number in the class.
    */
   public int getOrdinal()
   {
      return m_nOrdinal;
   }

   /**
    * Sets the maximum attribute value length.
    * @param nMaxLength The maximum attribute value length to set.
    */
   public void setMaxLength(int nMaxLength)
   {
      verifyNotReadOnly();
      m_nMaxLength = nMaxLength;
   }

   /**
    * @return The maximum attribute value length.
    */
   public int getMaxLength()
   {
      return m_nMaxLength;
   }
   
   /**
    * Sets the requiredness flag.
    * @param bRequired The requiredness flag to set.
    */
   public void setRequired(boolean bRequired)
   {
      verifyNotReadOnly();
      m_bRequired = bRequired;
   }

   /**
    * @return The requiredness flag.
    */
   public boolean isRequired()
   {
      return m_bRequired;
   }
   
   /**
    * Sets the collection flag.
    * @param bCollection The collection flag to set.
    */
   public void setCollection(boolean bCollection)
   {
      verifyNotReadOnly();
      m_bCollection = bCollection;
   }

   /**
    * @return The collection flag.
    */
   public boolean isCollection()
   {
      return m_bCollection;
   }
   
   /**
    * Sets the read-only value flag.
    * @param bReadOnly The read-only value flag to set.
    */
   public void setReadOnly(boolean bReadOnly)
   {
      verifyNotReadOnly();
      m_bReadOnly = bReadOnly;
   }

   /**
    * @return The read-only value flag.
    */
   public boolean isReadOnly()
   {
      return m_bReadOnly;
   }

   /**
    * Sets the caching flag.
    * @param cached The caching flag to set.
    */
   public void setCached(Boolean cached)
   {
      verifyNotReadOnly();
      
      if (cached == null)
      {
         m_nCached = -1;
      }
      else
      {
         setCached(cached.booleanValue());
      }
   }

   /**
    * Sets the caching flag.
    * @param bCached The caching flag to set.
    */
   public void setCached(boolean bCached)
   {
      verifyNotReadOnly();
      m_nCached = (bCached) ? (byte)1 : (byte)0;

      if (!bCached)
      {
         m_bReadOnly = true;
      }
   }

   /**
    * @return The caching flag.
    */
   public boolean isCached()
   {
      return m_nCached != 0;
   }

   /**
    * @return True if the attribute should be omitted from lazy loads and caching.  
    */
   public boolean isLazy()
   {
      return m_bStatic || m_bCollection || !isPersistent() ||
         !m_type.isPrimitive() && !((ClassMapping)getPersistenceMapping()).isInner();
   }

   /**
    * Sets the cascade mode - one of the CASCADE_* constants.
    * @param nCascadeMode The cascade mode to set.
    */
   public void setCascadeMode(byte nCascadeMode)
   {
      verifyNotReadOnly();
      
      if (nCascadeMode != CASCADE_DEFAULT && nCascadeMode != CASCADE_NONE)
      {
         if (m_type != null && m_type.isPrimitive())
         {
            throw new MetadataException("err.meta.primitiveCascadeMode", new Object[]{getName(), m_metaclass.getName()});
         }

         if (m_bStatic)
         {
            throw new MetadataException("err.meta.staticCascadeMode", new Object[]{getName(), m_metaclass.getName()});
         }
      }
      
      m_nCascadeMode = nCascadeMode;
   }

   /**
    * @return The cascade mode - one of the CASCADE_* constants.
    */
   public byte getCascadeMode()
   {
      return m_nCascadeMode;
   }
   
   /**
    * Sets the read privilege.
    * @param readPrivilege The read privilege to set.
    */
   public void setReadPrivilege(PrimitivePrivilege readPrivilege)
   {
      verifyNotReadOnly();
      m_readPrivilege = readPrivilege;
   }

   /**
    * @return The read privilege.
    */
   public PrimitivePrivilege getReadPrivilege()
   {
      return m_readPrivilege;
   }

   /**
    * Sets the update privilege.
    * @param updatePrivilege The update privilege to set.
    */
   public void setUpdatePrivilege(PrimitivePrivilege updatePrivilege)
   {
      verifyNotReadOnly();
      m_updatePrivilege = updatePrivilege;
   }

   /**
    * @return The update privilege.
    */
   public PrimitivePrivilege getUpdatePrivilege()
   {
      return m_updatePrivilege;
   }

   /**
    * @see nexj.core.meta.Member#setAccessAttribute(nexj.core.meta.Attribute)
    */
   public void setAccessAttribute(Attribute accessAttribute)
   {
      if (accessAttribute != null && m_bStatic && !accessAttribute.isStatic())
      {
         MetadataValidationException e = new MetadataValidationException(
            "err.meta.accessAttributeAllocation",
            new Object[]{accessAttribute.getName(), getName(), m_metaclass.getName()});

         setProperties(e);

         throw e;
      }

      super.setAccessAttribute(accessAttribute);
   }

   /**
    * Sets the attribute value expression.
    * @param value The attribute value expression to set. Can be Undefined.VALUE.
    */
   public void setValue(Object value)
   {
      verifyNotReadOnly();
      m_value = value;

      if (value == Undefined.VALUE)
      {
         m_nCached = (byte)1;
      }
   }

   /**
    * @return The attribute value expression.
    */
   public Object getValue()
   {
      return m_value;
   }

   /**
    * @return The value function.
    */
   public Function getValueFunction()
   {
      return m_valueFunction;
   }

   /**
    * @see nexj.core.meta.ui.AttributeMeta#getValueDependencyAssociations()
    */
   public Pair getValueDependencyAssociations()
   {
      return getValueDependencyAssociations(getDependency());
   }

   /**
    * Change a dependency list from being "attribute list" form to a list of
    * association paths. Also AttributeMetas are replaced with their names.
    * 
    * @param dep Dependencies
    * @return List of association paths
    */
   private Pair getValueDependencyAssociations(Pair dep)
   {
      Pair result = null;

      while (dep != null)
      {
         Object head = dep.getHead();

         if (head instanceof AttributeMeta)
         {
            result = new Pair(new Pair(((AttributeMeta)head).getName()), result);
         }
         else if (head instanceof Pair && ((Pair)head).getHead() instanceof AttributeMeta)
         {
            Pair next = (Pair)head;
            final String sAttr = ((AttributeMeta)next.getHead()).getName();

            next = getValueDependencyAssociations(next.getNext());

            while (next != null)
            {
               result = new Pair(new Pair(sAttr, next.getHead()), result);
               next = next.getNext();
            }
         }
         else
         {
            assert false;
         }

         dep = dep.getNext();
      }

      return result;
   }

   /**
    * @see nexj.core.meta.ui.AttributeMeta#isClientCalculable()
    */
   public boolean isClientCalculable()
   {
      return isFullDependency() && getValueFunction() != null && getDependency() != null;
   }

   /**
    * @return Whether the dependency information is known exactly - that it
    *         contains all run-time dependencies.
    */
   protected boolean isFullDependency()
   {
      return m_bFullDependency;
   }

   /**
    * Set the new value for whether dependency information is full.
    * 
    * @param bFullDependency The new value for whether dependency is full
    * @see #isFullDependency()
    */
   protected void setFullDependency(boolean bFullDependency)
   {
      verifyNotReadOnly();
      m_bFullDependency = bFullDependency;
   }

   /**
    * Sets the initial attribute value expression.
    * @param initializer The initial attribute value expression to set. Can be Primitive.VALUE.
    */
   public void setInitializer(Object initializer)
   {
      verifyNotReadOnly();
      m_initializer = initializer;
   }

   /**
    * @return The initial attribute value expression.
    */
   public Object getInitializer()
   {
      return m_initializer;
   }
   
   /**
    * @return The initializer function.
    */
   public Function getInitializerFunction()
   {
      return m_initializerFunction;
   }
   
   /**
    * Sets the validation expression.
    * @param validation The validation expression to set.
    */
   public void setValidation(Object validation)
   {
      verifyNotReadOnly();
      
      if (validation != Undefined.VALUE && m_bStatic)
      {
         throw new MetadataException("err.meta.staticValidation",
            new Object[]{getName(), m_metaclass.getName()});
      }
      
      m_validation = validation;
   }

   /**
    * @return The validation expression.
    */
   public Object getValidation()
   {
      return m_validation;
   }

   /**
    * @return The validation function.
    */
   public Function getValidationFunction()
   {
      return m_validationFunction;
   }

   /**
    * Sets the text position map for the attribute's value, initializer, and validation.
    * @param textPosMap The attribute text position map to set.
    */
   public void setTextPositionMap(Lookup textPosMap)
   {
      verifyNotReadOnly();
      m_textPosMap = textPosMap;
   }

   /**
    * Copies the functions from a given attribute.
    * @param src The source attribute providing the functions.
    */
   public void setFunctions(Attribute src)
   {
      verifyNotReadOnly();
      m_valueFunction = src.m_valueFunction;
      m_initializerFunction = src.m_initializerFunction;
      m_validationFunction = src.m_validationFunction;
   }
   
   /**
    * Sets the attribute type.
    * @param type The attribute type to set.
    */
   public void setType(Type type)
   {
      verifyNotReadOnly();
      m_type = type;
   }

   /**
    * @return The attribute type.
    */
   public Type getType()
   {
      return m_type;
   }

   /**
    * Sets the enumeration class.
    * @param enumeration The enumeration class to set.
    */
   public void setEnumeration(Metaclass enumeration)
   {
      verifyNotReadOnly();
      m_enumeration = enumeration;
   }

   /**
    * @return The enumeration class.
    */
   public Metaclass getEnumeration()
   {
      return m_enumeration;
   }

   /**
    * Sets the constrained enumeration flag.
    * @param bConstrained The constrained enumeration flag to set.
    */
   public void setConstrained(boolean bConstrained)
   {
      verifyNotReadOnly();
      m_bConstrained = bConstrained;
   }

   /**
    * @return The constrained enumeration flag.
    */
   public boolean isConstrained()
   {
      return m_bConstrained;
   }
   
   /**
    * Sets the derived association where clause.
    * @param where The derived association where clause to set.
    */
   public void setWhere(Object where)
   {
      verifyNotReadOnly();
      
      if (where != null && m_bStatic)
      {
         throw new MetadataException("err.meta.staticDerivedAssoc",
            new Object[]{getName(), m_metaclass.getName()});
      }

      m_where = where;
   }

   /**
    * @return The derived association where clause.
    */
   public Object getWhere()
   {
      return m_where;
   }

   /**
    * Sets the reverse association attribute.
    * @param reverse The reverse association attribute to set. Can be null.
    */
   public void setReverse(Attribute reverse)
   {
      verifyNotReadOnly();

      if (reverse != null)
      {
         if (reverse.m_type.isPrimitive() || m_type.isPrimitive())
         {
            throw new MetadataException("err.meta.primitiveReverseAttrib",
               new Object[]{reverse.getName(), getName(), m_metaclass.getName()});
         }
         
         if (!((Metaclass)reverse.m_type).isUpcast(m_metaclass) && !(m_metaclass instanceof Aspect) ||
            !reverse.m_metaclass.isUpcast((Metaclass)m_type) ||
            m_reverse != null && reverse.m_nOrdinal != m_reverse.m_nOrdinal ||
            m_bStatic || reverse.isStatic())
         {
            throw new MetadataException("err.meta.reverseAttribMismatch",
               new Object[]{reverse.getName(), reverse.m_metaclass.getName(),
                  getName(), m_metaclass.getName()});
         }
      }

      m_reverse = reverse;

      for (int nDerived = 0, nDerivedCount = m_metaclass.getDerivedCount(); nDerived < nDerivedCount; ++nDerived)
      {
         m_metaclass.getDerived(nDerived).getDerivedAttribute(this).setReverse(reverse);
      }
   }

   /**
    * @return The reverse association attribute.
    */
   public Attribute getReverse()
   {
      return m_reverse;
   }

   /**
    * Determines if this attribute is the reverse of a given attribute.
    * @param attribute The attribute relative to which to perform the test.
    * @return True if this attribute is the reverse of the supplied attribute. 
    */
   public boolean isReverseOf(Attribute attribute)
   {
      return !m_bStatic && attribute.m_reverse != null &&
         attribute.m_reverse.m_nOrdinal == m_nOrdinal; 
   }

   /**
    * @return True if the reverse of the reverse attribute is this attribute
    * or an attribute overridden by this attribute.
    */
   public boolean isSymmetric()
   {
      return m_reverse != null && isReverseOf(m_reverse);
   }

   /**
    * Sets the inherited attribute with a compatible mapping from the topmost class.
    * @param persistenceRoot The inherited attribute with a compatible mapping from the topmost class to set.
    */
   public void setPersistenceRoot(Attribute persistenceRoot)
   {
      verifyNotReadOnly();
      m_persistenceRoot = persistenceRoot;
   }

   /**
    * @return The inherited attribute with a compatible mapping from the topmost class.
    */
   public Attribute getPersistenceRoot()
   {
      return m_persistenceRoot;
   }
   
   /**
    * Sets the next attribute with the same persistence mapping in a circular list.
    * @param alias The next attribute with the same persistence mapping in a circular list to set.
    */
   public void setAlias(Attribute alias)
   {
      verifyNotReadOnly();
      m_alias = alias;
   }

   /**
    * @return The next attribute with the same persistence mapping in a circular list.
    */
   public Attribute getAlias()
   {
      return m_alias;
   }
   
   /**
    * Sets the attribute dependency list.
    * @param dependency The attribute dependency list to set.
    */
   public void setDependency(Pair dependency)
   {
      verifyNotReadOnly();
      m_dependency = dependency;
      m_cumulativeDependency = dependency;
   }

   /**
    * @return The attribute dependency list.
    */
   public Pair getDependency()
   {
      return m_dependency;
   }

   /**
    * @return The cumulative dependency (incl. subclasses).
    */
   public Pair getCumulativeDependency()
   {
      return m_cumulativeDependency;
   }
   
   /**
    * @return The attribute inverse dependency list.
    */
   public InverseDependencyList getInverseDependency()
   {
      return m_inverseDependency;
   }
   
   /**
    * Lazy-creates an inverse dependency list.
    * @return The attribute inverse dependency list.
    */
   protected InverseDependencyList createInverseDependency()
   {
      verifyNotReadOnly();
      
      if (m_inverseDependency == null)
      {
         m_inverseDependency = new InverseDependencyList();
      }
      
      return m_inverseDependency;
   }

   /**
    * @return True if the inverse dependency contains persistent attributes.
    */
   public boolean isInverseDependencyPersistent()
   {
      return m_inverseDependency != null && m_inverseDependency.isPersistent();
   }
   
   /**
    * @return True if the inverse dependency of the reverse attribute containt persistent attributes.
    */
   public boolean isReverseInverseDependencyPersistent()
   {
      return m_reverse != null && m_reverse.m_inverseDependency != null && 
         m_reverse.m_inverseDependency.isPersistent();
   }
   
   /**
    * Sets the attribute alternative order by list - ((attr1 . bAscFlag1) ... (attrN . bAscFlagN)).
    * @param orderBy The attribute alternative order by list to set.
    */
   public void setOrderBy(Pair orderBy)
   {
      verifyNotReadOnly();
      m_orderBy = orderBy;
   }

   /**
    * @return The attribute alternative order by list - ((attr1 . bAscFlag1) ... (attrN . bAscFlagN)).
    */
   public Pair getOrderBy()
   {
      return m_orderBy;
   }
   
   /**
    * Sets the attribute caption string id.
    * @param sCaption The attribute caption string id to set.
    */
   public void setCaption(String sCaption)
   {
      verifyNotReadOnly();
      m_sCaption = sCaption;
   }

   /**
    * @return The attribute caption string id.
    */
   public String getCaption()
   {
      return (m_sCaption == null) ? m_sName : m_sCaption;
   }

   /**
    * Derive member values from base attribute.
    * @param base The attribute to inherit from (null == no base).
    */
   public void deriveFrom(Attribute base)
   {
      verifyNotReadOnly();

      if (m_sCaption == null)
      {
         setCaption(base.m_sCaption); // inherit parent caption if own is unset
      }
   }

   /**
    * Completes the derivation of a given attribute.
    */
   public void completeDerivation()
   {
      verifyNotReadOnly();

      Metaclass base = m_metaclass.getBase();
      Attribute baseAttr = (base == null) ? null : base.findAttribute(m_sName);

      m_rootDeclarator = (baseAttr == null) ? m_metaclass : baseAttr.getRootDeclarator();

      if (m_sCaption == null)
      {
         m_sCaption = StringUtil.toCaption(m_sName, Metadata.SCOPE_SEP, false);
      }
   }

   /**
    * @return The persistence mapping of the attribute, or null if none.
    */
   protected AttributeMapping getPersistenceMapping()
   {
      return findPersistenceMapping(null, false);
   }

   /**
    * Finds an attribute mapping that is compatible with a given persistence mapping.
    * @param compatible The compatible mapping. Can be null to use the class mapping.
    * @param bInverse True to use the attribute type class.
    * @return The attribute mapping, or null if not found. 
    */
   public AttributeMapping findPersistenceMapping(PersistenceMapping compatible, boolean bInverse)
   {
      assert !bInverse || !m_type.isPrimitive();

      PersistenceMapping mapping = ((bInverse) ? (Metaclass)m_type : m_metaclass).getPersistenceMapping();

      if (mapping != null)
      {
         if (compatible != null)
         {
            mapping = mapping.findMapping(compatible);
         }

         if (mapping != null)
         {
            if (bInverse)
            {
               PersistenceMapping persistenceMapping = m_metaclass.getPersistenceMapping();

               if (persistenceMapping != null)
               {
                  return persistenceMapping.findClassMapping(this, mapping);
               }
            }
            else
            {
               return mapping.getAttributeMapping(this);
            }
         }
      }

      return null;
   }

   /**
    * @return True if the attribute is persistent.
    */
   public boolean isPersistent()
   {
      return getPersistenceMapping() != null;
   }

   /**
    * @return True if the attribute is calculated.
    */
   public boolean isCalculated()
   {
      return m_value != Undefined.VALUE || m_dependency != null;
   }
   
   /**
    * Determines if this attribute is derivation compatible with a base attribute.
    * @param base The base attribute.
    * @return True if it is compatible.
    */
   public boolean isCompatibleWith(Attribute base)
   {
      return m_type.isPrimitive() == base.m_type.isPrimitive() && 
         ((m_type.isPrimitive()) ? m_type == base.m_type :
            ((Metaclass)base.m_type).isUpcast((Metaclass)m_type)) &&
         (m_bRequired == base.m_bRequired || !base.m_bRequired) &&
         m_reverse == base.m_reverse &&
         m_bStatic == base.m_bStatic &&
         m_bCollection == base.m_bCollection;
   }

   /**
    * Compiles the initializer and the value expressions to p-code.
    * @see nexj.core.meta.Member#compile(nexj.core.scripting.Machine)
    */
   public void compile(Machine machine)
   {
      verifyNotReadOnly();
      setCurrent();

      try
      {
         if (m_value != Undefined.VALUE)
         {
            m_valueFunction = compile(m_value, VALUE_ARGUMENTS, machine, "value");
         }

         if (m_initializer != Undefined.VALUE)
         {
            m_initializerFunction = compile(m_initializer, VALUE_ARGUMENTS, machine, "initializer");
         }
         else
         {
            m_initializerFunction = m_valueFunction;
         }
         
         if (m_validation != Undefined.VALUE)
         {
            m_validationFunction = compile(m_validation, VALIDATION_ARGUMENTS, machine, "validation");
         }
      }
      finally
      {
         setTextPositionMap(null);
         clearCurrent();
      }
   }

   /**
    * Compiles an expression to a p-code method.
    * @param expr The expression to compile.
    * @param args The expression formal argument list.
    * @param machine The VM to use for compilation.
    * @param sItem The item to use in an error message.
    */
   private PCodeFunction compile(Object expr, Pair args, Machine machine, String sItem)
   {
      try
      {
         return new Compiler().compile(
            new Pair(Symbol.LAMBDA, new Pair(args, new Pair(expr))),
            m_textPosMap, machine, false);
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
            x = new MetadataValidationException("err.meta.attributeCompilation", e);
         }

         setProperties(x);
         x.setProperty("item", sItem);

         throw x;
      }
   }

   /**
    * Resolves the attribute dependency and order by references after it has been loaded.
    * @param machine The VM for macro expansion.
    */
   public void resolve(Machine machine)
   {
      verifyNotReadOnly();
      m_metaclass.setCurrent();
      setCurrent();

      try
      {
         Pair dep = new Pair(Boolean.TRUE);

         m_metaclass.dependency(m_value, m_bStatic, dep, null, machine);

         setDerivedDependency(
            Pair.nconc(dep.getNext(),
            resolveDependency(m_metaclass, m_dependency)), Intrinsic.isTrue(dep.getHead()));

         setDerivedOrderBy(resolveOrderBy(m_metaclass, m_orderBy));
      }
      finally
      {
         clearCurrent();
         m_metaclass.clearCurrent();
      }
   }

   /**
    * Sets the current attribute in the global environment.
    */
   protected void setCurrent()
   {
      m_metaclass.getMetadata().getGlobalEnvironment().defineVariable(SYS_CURRENT_ATTRIBUTE, this);
   }
   
   /**
    * Removes the current attribute from the global environment.
    */
   protected void clearCurrent()
   {
      m_metaclass.getMetadata().getGlobalEnvironment().removeVariable(SYS_CURRENT_ATTRIBUTE);
   }

   /**
    * Copies the dependency list and converts the symbols to attributes.
    * @param metaclass The metaclass where to look for attributes.
    * @param dep The dependency list to convert.
    * @return The converted dependency list.
    * @throws MetadataException if the dependency list is invalid.
    */
   private Pair resolveDependency(Metaclass metaclass, Pair dep) throws MetadataException
   {
      try
      {
         Pair first, last;

         for (first = last = null; dep != null; dep = dep.getNext())
         {
            Pair pair;

            if (dep.getHead() instanceof Pair)
            {
               Pair head = (Pair)dep.getHead();

               if (head == null || head.getHead() == null)
               {
                  throw new MetadataException("err.meta.attributeDep",
                     new Object[]{getName(), m_metaclass.getName()});
               }

               Attribute attribute = metaclass.getAttribute(((Symbol)head.getHead()).getName());
               Type type = attribute.getType();

               if (type.isPrimitive())
               {
                  throw new MetadataException("err.meta.attributeAssocDep", 
                     new Object[]{attribute.getName(), attribute.m_metaclass.getName(),
                        getName(), m_metaclass.getName()});
               } 

               pair = new Pair(new Pair(attribute, resolveDependency((Metaclass)type, head.getNext()))); 
            }
            else if (dep.getHead() instanceof Symbol)
            {
               pair = new Pair(metaclass.getAttribute(((Symbol)dep.getHead()).getName()));
            }
            else
            {
               throw new MetadataException("err.meta.attributeDep",
                  new Object[]{getName(), m_metaclass.getName()});
            }
            
            if (last == null)
            {
               first = last = pair;
            }
            else
            {
               last.setTail(pair);
               last = pair;
            }
         }

         return first;
      }
      catch (MetadataException e)
      {
         throw e;
      }
      catch (ClassCastException e)
      {
         throw new MetadataException("err.meta.attributeDep",
            new Object[]{getName(), getMetaclass().getName()});
      }
   }

   /**
    * Sets the dependency list in a derived attribute, if it is not overridden.
    * @param dep The dependency list to set.
    */
   private void setDerivedDependency(Pair dep, boolean bFullDependency)
   {
      setDependency(dep);
      setFullDependency(bFullDependency);

      if (s_logger.isDumpEnabled() && isClientCalculable())
      {
         s_logger.dump(m_metaclass.getName() + '.' + m_sName + " is eligible for client-side calculation. Dependencies: "
            + getValueDependencyAssociations());
      }

      for (int nDerived = 0, nDerivedCount = m_metaclass.getDerivedCount(); nDerived < nDerivedCount; ++nDerived)
      {
         Attribute attribute = m_metaclass.getDerived(nDerived).getDerivedAttribute(this);

         if (attribute.m_declarator == m_declarator)
         {
            attribute.setDerivedDependency(dep, bFullDependency);
         }
      }
   }

   /**
    * Verifies the dependency list and computes the inverse dependency list.
    * @param depSet The set to which to add the attributes for the
    * inverse dependency calculation.
    */
   public void computeInverseDependency(Set depSet)
   {
      verifyNotReadOnly();

      if (m_dependency != null)
      {
         Holder set = new IdentityHashHolder();

         set.add(this);
         verifyDependency(m_metaclass, m_dependency, set);

         List stack = new ArrayList();

         stack.add(this);
         addInverseDependency(m_dependency, stack, depSet);
      }
   }

   /**
    * Adds an inverse dependency list to the attributes referenced in a dependency list.
    * @param dep The dependency list.
    * @param stack The stack containing the attributes in one branch of the dependency list.
    * @param depSet The set of the attributes, to which inverse dependencies have been added.
    */
   private static void addInverseDependency(Pair dep, List stack, Set depSet)
   {
      for (; dep != null; dep = dep.getNext())
      {
         Attribute attribute; 

         if (dep.getHead() instanceof Pair)
         {
            Pair pair = (Pair)dep.getHead();
            
            attribute = (Attribute)pair.getHead();
            
            if (attribute.getReverse() != null)
            {
               stack.add(attribute);
               addInverseDependency(pair.getNext(), stack, depSet);
               stack.remove(stack.size() - 1);
            }
         }
         else
         {
            attribute = (Attribute)dep.getHead();
         }

         depSet.add(attribute);
         addInverseDependency(attribute.createInverseDependency(), stack, stack.size() - 1);

         for (int i = stack.size() - 2; i > 0; --i)
         {
            attribute = (Attribute)stack.get(i);
            addInverseDependency(attribute.createInverseDependency(), stack, i - 1);
         }
      }
   }

   /**
    * Augments an inverse dependency list.
    * @param depList The inverse dependency list to augment.
    * @param stack The stack with a single dependency list branch.
    * @param nLast The last reference index in the stack.
    * @return The augmented inverse dependency list.
    */
   private static void addInverseDependency(InverseDependencyList depList, List stack, int nLast)
   {
      Attribute reverse = (Attribute)stack.get(nLast);
      
      if (nLast > 0)
      {
         reverse = reverse.getReverse();
      }

      for (InverseDependency dep = depList.getDependency(); dep != null; dep = dep.getNext())
      {
         if (dep.getAttribute().getOrdinal() == reverse.getOrdinal())
         {
            if (dep.isIndirect())
            {
               if (nLast > 0)
               {
                  addInverseDependency((InverseDependencyList)dep, stack, nLast - 1);
               }

               if (dep.isPersistent())
               {
                  depList.setPersistent();
               }
            }

            return;
         }
      }

      if (nLast > 0)
      {
         IndirectInverseDependency dep = new IndirectInverseDependency(reverse);

         addInverseDependency(dep, stack, nLast - 1);
         depList.addDependency(dep);
      }
      else
      {
         depList.addDependency(new DirectInverseDependency(reverse));
      }
   }

   /**
    * Verifies that a dependency list does not contain bogus data.
    * @param metaclass The class relative to which the list is.
    * @param dep The dependency list.
    * @param identitySet The identity set of Attribute for detecting circular references.
    * @throws MetadataException if the verification fails. 
    */
   private void verifyDependency(Metaclass metaclass, Pair dep, Holder identitySet) throws MetadataException
   {
      for (; dep != null; dep = dep.getNext())
      {
         Attribute attribute;

         if (dep.getHead() instanceof Pair)
         {
            Pair pair = (Pair)dep.getHead();

            attribute = (Attribute)pair.getHead();
            pair = pair.getNext();

            if (pair != null)
            {
               if (m_nCached < 0 && attribute.getReverse() == null)
               {
                  if (s_logger.isDebugEnabled())
                  {
                     s_logger.debug(m_metaclass.getName() + '.' + m_sName +
                        " uncached due to missing reverse of the dependency " +
                        attribute.getMetaclass().getName() + '.' + attribute.getName());
                  }

                  setCached(false);
               }

               verifyDependency((Metaclass)attribute.getType(), pair, identitySet);
            }
         }
         else
         {
            attribute = (Attribute)dep.getHead();
         }

         if (!identitySet.add(attribute))
         {
            throw new MetadataException("err.meta.attributeDepCycle",
               new Object[]{getName(), m_metaclass.getName()});
         }

         verifyDependency(metaclass, attribute.getDependency(), identitySet);
         identitySet.remove(attribute);
      }
   }

   /**
    * Adds the inverse dependency list to the derived attributes.
    * @param depList The inverse dependency list to add.
    */
   private void addDerivedInverseDependency(InverseDependencyList depList)
   {
      for (int nDerived = 0, nDerivedCount = m_metaclass.getDerivedCount(); nDerived < nDerivedCount; ++nDerived)
      {
         Attribute attribute = m_metaclass.getDerived(nDerived).getDerivedAttribute(this);

         attribute.createInverseDependency().append(depList);
         attribute.addDerivedInverseDependency(depList);
      }
   }

   /**
    * Adds the inverse dependency list to the derived attributes for all the elements in the iterator.
    * @param itr The iterator containing the attributes to process.
    */
   public static void resolveInverseDependency(Iterator itr)
   {
      while (itr.hasNext())
      {
         Attribute attribute = (Attribute)itr.next();
         
         attribute.addDerivedInverseDependency(attribute.m_inverseDependency);
      }
   }

   /**
    * Invalidates all the attributes referenced in the inverse dependency list.
    * @param depList The inverse dependency list.
    * @param accessor The accessor to invalidate.
    * @param undef The new value to assign.
    */
   protected static void invalidateDependency(InverseDependencyList depList, Accessor accessor, Object undef)
   {
      if (accessor != null)
      {
         for (InverseDependency dep = depList.getDependency() ; dep != null; dep = dep.getNext())
         {
            Attribute attribute = dep.getAttribute();

            if (attribute.getMetaclass().isUpcast(accessor.getMetaclass()))
            {
               if (dep.isIndirect())
               {
                  Object value = accessor.getValueDirect(attribute.getOrdinal());

                  if (value == Undefined.VALUE && undef == Invalid.VALUE && dep.isPersistent())
                  {
                     value = accessor.getValue(attribute.getOrdinal());
                  }

                  if (!(value instanceof Undefined))
                  {
                     if (attribute.isCollection())
                     {
                        InstanceList list = (InstanceList)value;
                        int nCount = list.getCount();

                        for (int i = 0; i < nCount; ++i)
                        {
                           invalidateDependency((InverseDependencyList)dep, (Accessor)list.get(i), undef);
                        }
                     }
                     else
                     {
                        invalidateDependency((InverseDependencyList)dep, (Accessor)value, undef);
                     }
                  }
               }
               else
               {
                  attribute = accessor.getMetaclass().getDerivedAttribute(attribute);

                  if (undef != Undefined.VALUE || !attribute.isPersistent())
                  {
                     InverseDependencyList attrDepList = attribute.getInverseDependency();

                     boolean bDep = (attrDepList != null &&
                        (attrDepList.isPersistent() || accessor.getValueDirect(attribute.getOrdinal()) != undef));

                     accessor.invalidate(attribute.getOrdinal(), undef);

                     if (bDep)
                     {
                        invalidateDependency(attrDepList, accessor, undef);
                     }
                  }
               }
            }
         }
      }
   }

   /**
    * Invalidates all the attributes referenced in the inverse dependency list.
    * @param accessor The accessor to invalidate.
    * @param undef The new value to assign.
    */
   public void invalidateDependency(Accessor accessor, Object undef)
   {
      if (m_inverseDependency != null)
      {
         invalidateDependency(m_inverseDependency, accessor, undef);
      }
   }
   
   /**
    * Adds the specified dependency list to the cumulative dependency.
    * @param dep The dependency list to add.
    */
   public void addCumulativeDependency(Pair dep)
   {
      verifyNotReadOnly();
      
      Pair cumulativeDep = m_cumulativeDependency;

   loop:
      for (; dep != null; dep = dep.getNext())
      {
         Object value = dep.getHead();
         Attribute attr1 = (value instanceof Attribute) ? (Attribute)value : null;

         for (Pair pair = cumulativeDep; pair != null; pair = pair.getNext())
         {
            if (pair.getHead() == value)
            {
               continue loop;
            }

            if (attr1 != null && pair.getHead() instanceof Attribute)
            {
               Attribute attr2 = (Attribute)pair.getHead();

               if (attr1.getOrdinal() == attr2.getOrdinal() &&
                  attr1.isStatic() == attr2.isStatic() &&
                  attr1.getRootDeclarator() == attr2.getRootDeclarator())
               {
                  continue loop;
               }
            }
         }

         m_cumulativeDependency = new Pair(value, m_cumulativeDependency);
      }
   }

   /**
    * Adds a new overriding value declarator to the attribute.
    * @param valueDeclarator The value declarator to add.
    */
   protected void addValueDeclarator(Metaclass valueDeclarator)
   {
      verifyNotReadOnly();

      if (m_valueDeclaratorList == null)
      {
         m_valueDeclaratorList = new ArrayList(4);
      }

      m_valueDeclaratorList.add(valueDeclarator);
   }

   /**
    * Adds value declarators from a given attribute.
    * @param attribute The source attribute.
    */
   protected void addValueDeclarators(Attribute attribute)
   {
      verifyNotReadOnly();

      if (!ObjUtil.equal(m_value, attribute.getValue()))
      {
         addValueDeclarator(attribute.getMetaclass());
      }

      for (int i = 0, n = attribute.getValueDeclaratorCount(); i < n; ++i)
      {
         addValueDeclarator(attribute.getValueDeclarator(i));
      }
   }

   /**
    * Gets a value declarator by ordinal number.
    * @param nOrdinal The value declarator ordinal number (0-based).
    * @return The value declarator object.
    */
   public Metaclass getValueDeclarator(int nOrdinal)
   {
      return (Metaclass)m_valueDeclaratorList.get(nOrdinal);
   }

   /**
    * @return The value declarator count.
    */
   public int getValueDeclaratorCount()
   {
      if (m_valueDeclaratorList == null)
      {
         return 0;
      }

      return m_valueDeclaratorList.size();
   }

   /**
    * @return An iterator for the contained value declarator objects.
    */
   public Iterator getValueDeclaratorIterator()
   {
      if (m_valueDeclaratorList == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_valueDeclaratorList.iterator();
   }

   /**
    * Determines if the value is overridden in a given set of classes.
    * @param metaclassSet The set to test, or null for all classes.
    * @return True if the value is overridden by a member of the set.
    */
   public boolean isValueOverridden(Set metaclassSet)
   {
      if (m_valueDeclaratorList != null)
      {
         for (int i = 0, n = m_valueDeclaratorList.size(); i < n; ++i)
         {
            if (metaclassSet == null || metaclassSet.contains(m_valueDeclaratorList.get(i)))
            {
               return true;
            }
         }
      }

      return isValueOverridden(m_dependency, metaclassSet);
   }

   /**
    * Determines if a value of any attribute in a given dependency list is overridden.
    * @param dep The dependency list.
    * @param metaclassSet The set to test, or null for all classes.
    * @return True if the value is overridden by a member of the set.
    */
   private static boolean isValueOverridden(Pair dep, Set metaclassSet)
   {
      while (dep != null)
      {
         Object head = dep.getHead();

         if (head instanceof Pair)
         {
            if (isValueOverridden((Pair)head, metaclassSet))
            {
               return true;
            }
         }
         else if (((Attribute)head).isValueOverridden(metaclassSet))
         {
            return true;
         }

         dep = dep.getNext();
      }

      return false;
   }

   /**
    * @return Value expression taking into account polymorphic value dispatch.
    */
   public Object getDispatchedValue()
   {
      Object value = m_value;

      if (value != Undefined.VALUE)
      {
         for (int i = 0, n = getValueDeclaratorCount(); i < n; ++i)
         {
            Metaclass metaclass = getValueDeclarator(i);

            if (metaclass.getPersistenceRoot() == m_metaclass.getPersistenceRoot())
            {
               Object derivedValue = metaclass.getDerivedAttribute(this).getValue();

               if (derivedValue == Undefined.VALUE)
               {
                  return derivedValue;
               }

               value = Pair.list(Symbol.IF,
                  Pair.binary(Symbol.INSTANCE_P, THIS, metaclass.getSymbol()),
                     derivedValue, value);
            }
         }
      }

      return value;
   }

   /**
    * Verifies and normalizes the attribute order by specification.
    * @param metaclass The class relative to which the list is.
    * @param orderBy The order by specification.
    * @return The newly allocated order by list.
    * @throws MetadataException if the verification fails.
    */
   private Pair resolveOrderBy(Metaclass metaclass, Pair orderBy) throws MetadataException
   {
      try
      {
         boolean bAscending = true;
         Pair first = null;
         Pair last = null;

         for (boolean bFirst = true; orderBy != null; orderBy = orderBy.getNext(), bFirst = false)
         {
            orderBy = new Pair(orderBy.getHead(), orderBy.getTail());

            if (orderBy.getHead() instanceof Pair)
            {
               Pair head = (Pair)orderBy.getHead();

               if (head.getHead() instanceof Symbol)
               {
                  metaclass.getAttribute(head.getHead().toString());
               }
               else if (!(head.getHead() instanceof Pair))
               {
                  throw new ClassCastException();
               }

               if (!(head.getTail() instanceof Boolean))
               {
                  throw new ClassCastException();
               }
               
               if (bFirst)
               {
                  bAscending = ((Boolean)head.getTail()).booleanValue();
                  head.setTail(Boolean.TRUE);
               }
               else
               {
                  head.setTail(Boolean.valueOf((!((Boolean)head.getTail()).booleanValue() ^ bAscending)));
               }
            }
            else
            {
               Symbol sym = (Symbol)orderBy.getHead();

               if (sym == null)
               {
                  throw new ClassCastException();
               }

               metaclass.getAttribute(sym.getName());
               orderBy.setHead(new Pair(sym, Boolean.valueOf(bAscending)));
            }
            
            if (first == null)
            {
               first = last = orderBy;
            }
            else
            {
               last.setTail(orderBy);
               last = orderBy;
            }
         }

         return first;
      }
      catch (MetadataException e)
      {
         throw e;
      }
      catch (ClassCastException e)
      {
         throw new MetadataException("err.meta.attributeOrderBy",
            new Object[]{getName(), m_metaclass.getName()});
      }
   }

   /**
    * Sets the order by expression in a derived attribute, if it is not overridden.
    * @param orderBy The order by expression to set.
    */
   private void setDerivedOrderBy(Pair orderBy)
   {
      m_orderBy = orderBy;

      for (int nDerived = 0, nDerivedCount = m_metaclass.getDerivedCount(); nDerived < nDerivedCount; ++nDerived)
      {
         Attribute attribute = m_metaclass.getDerived(nDerived).getDerivedAttribute(this);

         if (attribute.m_declarator == m_declarator)
         {
            attribute.setDerivedOrderBy(orderBy);
         }
      }
   }
   
   /**
    * Computes the maximum attribute data length.
    */
   public void setMaxLength()
   {
      verifyNotReadOnly();

      if (m_type == Primitive.STRING || m_type == Primitive.BINARY)
      {
         AttributeMapping attributeMapping = getPersistenceMapping();

         if (attributeMapping != null)
         {
            m_nMaxLength = attributeMapping.getMaxLength((Primitive)m_type);
         }
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      m_metaclass.setProperties(marker);
      marker.setTypeName("Attribute");
      marker.setProperty("attribute", m_sName);
   }

   /**
    * Validates the enumeration associated with the attribute.
    * @see nexj.core.meta.MetadataObject#validate(ContextMetadata, ExceptionHolder)
    */
   public void validateEnumeration(ContextMetadata metadata)
   {
      if (m_bConstrained && m_enumeration == null)
      {
         throw new MetadataException("err.meta.constrainedWithoutEnumeration",
            new Object[]{getName(), m_metaclass.getName()});
      }

      if (m_enumeration != null)
      {
         if (m_type.isPrimitive())
         {
            Attribute attribute = m_enumeration.findAttribute(Metaclass.ENUMERATION_VALUE);

            if (attribute == null)
            {
               throw new MetadataException("err.meta.missingEnumerationValueAttribute",
                  new Object[]{m_enumeration.getName(), getName(), m_metaclass.getName()});
            }

            if (attribute.isStatic())
            {
               throw new MetadataException("err.meta.staticEnumerationValueAttribute",
                  new Object[]{m_enumeration.getName(), getName(), m_metaclass.getName()});
            }
         }
         else
         {
            if (!((Metaclass)m_type).isUpcast(m_enumeration))
            {
               throw new MetadataException("err.meta.enumerationType",
                  new Object[]{m_enumeration.getName(), m_type.getName(),
                     getName(), m_metaclass.getName()});
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#validate(ContextMetadata, ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      super.validate(metadata, warnings);
      validateEnumeration(metadata);
   }

   /**
    * Checks the attribute visibility and read access against a privilege set.
    * @param privilegeSet The privilege set containing the allowed privileges.
    * @throws SecurityViolationException if the visibility is not public or the access is denied.
    */
   public void checkReadAccess(PrivilegeSet privilegeSet) throws SecurityViolationException
   {
      if (m_nVisibility != Metaclass.PUBLIC)
      {
         throw new SecurityViolationException("err.rpc.attributeVisibility",
            new Object[]{getName(), m_metaclass.getName()});
      }

      if (m_readPrivilege != null && !privilegeSet.contains(m_readPrivilege))
      {
         throw new SecurityViolationException("err.rpc.attributeReadPrivilege",
            new Object[]{getName(), m_metaclass.getName(), m_readPrivilege.getName()});
      }
   }

   /**
    * @return True if the attribute is readable according to a privilege set.
    */
   public boolean isReadable(PrivilegeSet privilegeSet)
   {
      return m_nVisibility == Metaclass.PUBLIC && 
         (m_readPrivilege == null || privilegeSet.contains(m_readPrivilege) &&
         (m_type.isPrimitive() || ((Metaclass)m_type).isReadable(privilegeSet)));
   }

   /**
    * Checks the attribute visibility and update access against a privilege set.
    * @param privilegeSet The privilege set containing the allowed privileges.
    * @throws SecurityViolationException if the visibility is not public or the access is denied.
    */
   public void checkUpdateAccess(PrivilegeSet privilegeSet) throws SecurityViolationException
   {
      if (m_nVisibility != Metaclass.PUBLIC)
      {
         throw new SecurityViolationException("err.rpc.attributeVisibility",
            new Object[]{getName(), m_metaclass.getName()});
      }

      if (m_bReadOnly)
      {
         throw new SecurityViolationException("err.runtime.attributeReadOnlyAccess",
            new Object[]{getName(), m_metaclass.getName()});
      }

      if (m_updatePrivilege != null && !privilegeSet.contains(m_updatePrivilege))
      {
         throw new SecurityViolationException("err.rpc.attributeUpdatePrivilege",
            new Object[]{getName(), m_metaclass.getName(), m_updatePrivilege.getName()});
      }
   }

   /**
    * @return True if the attribute is updateable according to a privilege set.
    */
   public boolean isUpdateable(PrivilegeSet privilegeSet)
   {
      return !m_bReadOnly && (m_updatePrivilege == null || privilegeSet.contains(m_updatePrivilege));
   }
   
   /**
    * @see nexj.core.meta.ui.AttributeMeta#getAccessAttributeMeta()
    */
   public AttributeMeta getAccessAttributeMeta()
   {
      return m_accessAttribute;
   }

   /**
    * @see nexj.core.meta.ui.AttributeMeta#getClassMeta()
    */
   public ClassMeta getClassMeta()
   {
      return m_metaclass;
   }

   /**
    * @see nexj.core.meta.ui.AttributeMeta#getDeclaratorClassMeta()
    */
   public ClassMeta getDeclaratorClassMeta()
   {
      return m_declarator;
   }

   /**
    * @see nexj.core.meta.ui.AttributeMeta#getEnumerationClassMeta()
    */
   public ClassMeta getEnumerationClassMeta()
   {
      return m_enumeration;
   }

   /**
    * @see nexj.core.meta.ui.AttributeMeta#getReadPrivilegeOrdinal()
    */
   public int getReadPrivilegeOrdinal()
   {
      if (m_readPrivilege != null)
      {
         return m_readPrivilege.getOrdinal();
      }
      
      return -1;
   }

   /**
    * @see nexj.core.meta.ui.AttributeMeta#getReadPrivilegeOrdinals()
    */
   public Holder getReadPrivilegeOrdinals()
   {
      Holder set = new HashHolder(2);

      if (m_readPrivilege != null && m_readPrivilege.getOrdinal() >= 0)
      {
         set.add(Primitive.createInteger(m_readPrivilege.getOrdinal()));
      }
      
      if (m_type instanceof ClassMeta)
      {
         if (((ClassMeta)m_type).getReadPrivilegeOrdinal() >= 0)
         {
            set.add(Primitive.createInteger(((ClassMeta)m_type).getReadPrivilegeOrdinal()));
         }
      }

      if (set.size() == 0)
      {
         return null;
      }
      
      return set;
   }

   /**
    * @see nexj.core.meta.ui.AttributeMeta#getReverseAttributeMeta()
    */
   public AttributeMeta getReverseAttributeMeta()
   {
      return m_reverse;
   }

   /**
    * @see nexj.core.meta.ui.AttributeMeta#getUpdatePrivilegeOrdinal()
    */
   public int getUpdatePrivilegeOrdinal()
   {
      if (m_updatePrivilege != null)
      {
         return m_updatePrivilege.m_nOrdinal;
      }
      
      return -1;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      if (m_valueDeclaratorList instanceof ArrayList)
      {
         ((ArrayList)m_valueDeclaratorList).trimToSize();
      }

      super.makeReadOnly();
   }

   /**
    * @return The basic URL for this attribute.
    */
   public String getURL()
   {
      return "class:" + m_declarator.getName() + "." + m_sName;
   }

   /**
    * @see java.lang.Comparable#compareTo(java.lang.Object)
    */
   public int compareTo(Object obj)
   {
      if (!(obj instanceof Attribute))
      {
         return getClass().getName().compareTo(obj.getClass().getName());
      }

      int n = m_metaclass.compareTo(((Attribute)obj).m_metaclass);

      if (n == 0)
      {
         n = super.compareTo(obj);
      }

      return n;
   }

   /**
    * @see nexj.core.meta.MetadataObject#clone()
    */
   public Object clone()
   {
      Attribute attribute = (Attribute)super.clone();

      attribute.m_persistenceRoot = attribute;

      return attribute;
   }

   /**
    * Replaces an instance of BindMeta with an instance of SerializedForm during serialization.
    * 
    * @return An instance of SerializedForm which contains all state information needed to deserialize the instance.
    * @throws ObjectStreamException
    */
   protected Object writeReplace() throws ObjectStreamException 
   {
      return new SerializedForm(this);
   }
   
   /**
    * Creates an incompatible attribute exception.
    * @param metaclass The metaclass.
    * @param sErrCode The error string identifier.
    * @param argArray The error string arguments.
    */
   private MetadataValidationException createCompatibilityException(Metaclass metaclass, String sErrCode, Object[] argArray)
   {
      MetadataValidationException mve = metaclass.createCompatibilityException(sErrCode, argArray);
      
      mve.setTypeName("Attribute");
      mve.setProperty("attribute", getName());
      
      return mve;
   }

   /**
    * @see nexj.core.meta.Member#checkCompatibility(nexj.core.meta.Metaclass, nexj.core.util.ExceptionHolder)
    */
   public void checkCompatibility(Metaclass metaclass, ExceptionHolder eh)
   {
      Attribute attribute = metaclass.findAttribute(getName());
      
      if (attribute == null)
      {
         eh.addException(createCompatibilityException(metaclass, "err.meta.missingCompatibleAttribute",
            new Object[] {getName(), metaclass.getName()}));
      }
      else if (!attribute.isCompatible())
      {
         eh.addException(createCompatibilityException(metaclass, "err.meta.incompatibleAttributeProperty",
            new Object[] {"compatible", attribute.getName(), metaclass.getName()}));
      }
      else if (attribute.getVisibility() != getVisibility() && attribute.getVisibility() != Metaclass.PUBLIC)
      {
         eh.addException(createCompatibilityException(metaclass, "err.meta.incompatibleAttributeProperty",
            new Object[] {"visibility", attribute.getName(), metaclass.getName()}));
      }
      else if (attribute.isRequired() != isRequired())
      {
         eh.addException(createCompatibilityException(metaclass, "err.meta.incompatibleAttributeProperty",
            new Object[] {"required", attribute.getName(), metaclass.getName()}));
      }
      else if (attribute.isCollection() != isCollection())
      {
         eh.addException(createCompatibilityException(metaclass, "err.meta.incompatibleAttributeProperty",
            new Object[] {"collection", attribute.getName(), metaclass.getName()}));
      }
      else if (attribute.isStatic() != isStatic())
      {
         eh.addException(createCompatibilityException(metaclass, "err.meta.incompatibleAttributeProperty",
            new Object[] {"static", attribute.getName(), metaclass.getName()}));
      }
      else if (attribute.isReadOnly() != isReadOnly())
      {
         eh.addException(createCompatibilityException(metaclass, "err.meta.incompatibleAttributeProperty",
            new Object[] {"readOnly", attribute.getName(), metaclass.getName()}));
      }
      else if (attribute.getType().isPrimitive() != getType().isPrimitive() || !attribute.getType().getName().equals(getType().getName()))
      {
         eh.addException(createCompatibilityException(metaclass, "err.meta.incompatibleAttributeProperty",
            new Object[] {"type", attribute.getName(), metaclass.getName()}));
      }
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#getChild(java.lang.String)
    */
   public EndpointPart getChild(String sName)
   {
      if (m_type.isPrimitive())
      {
         throw new MetadataLookupException("err.meta.namedLookup",
            new Object[]{EndpointPart.class.getName(), sName});
      }

      return ((Metaclass)m_type).getChild(sName);
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#findChild(java.lang.String)
    */
   public EndpointPart findChild(String sName)
   {
      if (m_type.isPrimitive())
      {
         return null;
      }

      return ((Metaclass)m_type).findChild(sName);
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#getChildIterator()
    */
   public Iterator getChildIterator()
   {
      return ((Metaclass)m_type).getChildIterator();
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#isPrimitive()
    */
   public boolean isPrimitive()
   {
      return m_type.isPrimitive();
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#createObject()
    */
   public TransferObject createObject()
   {
      if (m_type.isPrimitive())
      {
         throw new IllegalStateException(getName());
      }

      return ((Metaclass)m_type).createObject();
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#getValue(nexj.core.util.PropertyMap, java.lang.Object)
    */
   public Object getValue(PropertyMap map, Object defValue)
   {
      if (map instanceof Accessor)
      {
         return map.getValue(getName());
      }

      return map.findValue(getName(), defValue);
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#setValue(nexj.core.util.PropertyMap, java.lang.Object)
    */
   public void setValue(PropertyMap map, Object value)
   {
      map.setValue(getName(), value);
   }

   // inner classes
   
   /**
    * Inner class used to represent a given Attribute instance in serialized form.
    */
   private static class SerializedForm implements Serializable
   {
      /**
       * Serial Id.
       */
      private static final long serialVersionUID = 597784290101879829L;

      /**
       * Name of the ClassMeta to which this attribute belongs.
       */
      private String m_sClassName;
      
      /**
       * Name of the attribute.
       */
      private String m_sAttribName;
      
      /**
       * Constructor for the serialized form of an Attribute instance.
       * 
       * @param Attribute
       * @throws InvalidObjectException
       */
      private SerializedForm (Attribute attrib) throws InvalidObjectException
      {
         m_sClassName = attrib.getClassMeta().getName();
         m_sAttribName = attrib.getName();
      }
      
      /**
       * Resolves an Attribute instance given the serialized state information.
       * 
       * @return Attribute instance representing the serialized information.
       * @throws ObjectStreamException
       */
      private Object readResolve() throws ObjectStreamException 
      {
         try
         {
            ClassMeta classMeta = Repository.getMetadata().getClassMeta(m_sClassName);

            return classMeta.getAttributeMeta(m_sAttribName);
         }
         catch (Exception e)
         {
            throw new InvalidObjectException(e.getMessage());
         }
      }
   }
   
   /**
    * Interface implemented by dependency nodes.
    */
   public static interface InverseDependency extends Cloneable
   {
      /**
       * Sets the next sibling.
       * @param next The next sibling.
       */
      void setNext(InverseDependency next);

      /**
       * @return The next sibling.
       */
      InverseDependency getNext();

      /**
       * @return The dependent attribute.
       */
      Attribute getAttribute();
      
      /**
       * @return True if there is persistent leaf attribute.
       */
      boolean isPersistent();
      
      /**
       * @return True if the node has children, false if it is a leaf.
       */
      boolean isIndirect();
      
      /**
       * @return A shallow copy of the dependency.
       */
      Object clone();
   }
   
   /**
    * Inverse dependency single-linked list.
    */
   public static class InverseDependencyList
   {
      // attributes
      
      /**
       * True if there is persistent leaf attribute.
       */
      protected boolean m_bPersistent;
      
      // associations
      
      /**
       * The first inverse dependency child node.
       */
      protected InverseDependency m_dependency;
      
      // operations
      
      /**
       * Sets the persistence flag.
       */
      public void setPersistent()
      {
         m_bPersistent = true;
      }
      
      /**
       * @return True if there is persistent leaf dependency.
       */
      public boolean isPersistent()
      {
         return m_bPersistent;
      }

      /**
       * @return The first inverse dependency child node.
       */
      public InverseDependency getDependency()
      {
         return m_dependency;
      }

      /**
       * Adds a child dependency node.
       * @param dep The dependency to add.
       */
      public void addDependency(InverseDependency dep)
      {
         m_bPersistent |= dep.isPersistent();
         dep.setNext(m_dependency);
         m_dependency = dep;
      }

      /**
       * Appends a dependency list to this one.
       * @param src The source dependency list.
       */
      public void append(InverseDependencyList src)
      {
         for (InverseDependency dep = src.getDependency(); dep != null; dep = dep.getNext())
         {
            addDependency((InverseDependency)dep.clone());
         }
      }

      /**
       * Appends the list string representation to a string buffer.
       * @param buf The string buffer.
       */
      protected void append(StringBuffer buf)
      {
         if (m_bPersistent)
         {
            buf.append("persistent:");
         }
         
         for (InverseDependency dep = m_dependency; dep != null; dep = dep.getNext())
         {
            if (m_bPersistent || dep != m_dependency)
            {
               buf.append(' ');
            }
            
            buf.append(dep);
         }
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         StringBuffer buf = new StringBuffer(32);
         
         append(buf);
         
         return buf.toString();
      }
   }
   
   /**
    * Direct (leaf) inverse dependency.
    */
   public final static class DirectInverseDependency implements InverseDependency
   {
      // associations
      
      /**
       * The dependent attribute.
       */
      protected Attribute m_attribute;
      
      /**
       * The next dependency.
       */
      protected InverseDependency m_next;
      
      // constructors
      
      /**
       * Constructs the dependency.
       * @param attribute The dependent attribute.
       */
      public DirectInverseDependency(Attribute attribute)
      {
         m_attribute = attribute;
      }

      // operations

      /**
       * @see nexj.core.meta.Attribute.InverseDependency#getAttribute()
       */
      public Attribute getAttribute()
      {
         return m_attribute;
      }

      /**
       * @see nexj.core.meta.Attribute.InverseDependency#setNext(nexj.core.meta.Attribute.InverseDependency)
       */
      public void setNext(InverseDependency next)
      {
         m_next = next;
      }

      /**
       * @see nexj.core.meta.Attribute.InverseDependency#getNext()
       */
      public InverseDependency getNext()
      {
         return m_next;
      }

      /**
       * @see nexj.core.meta.Attribute.InverseDependency#isIndirect()
       */
      public boolean isIndirect()
      {
         return false;
      }

      /**
       * @see nexj.core.meta.Attribute.InverseDependency#isPersistent()
       */
      public boolean isPersistent()
      {
         return m_attribute.isPersistent();
      }

      /**
       * @see java.lang.Object#clone()
       */
      public Object clone()
      {
         try
         {
            return super.clone();
         }
         catch (CloneNotSupportedException e)
         {
            return null;
         }
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return m_attribute.getName();
      }
   }
   
   public final static class IndirectInverseDependency
      extends InverseDependencyList implements InverseDependency
   {
      // associations
      
      /**
       * The dependent attribute.
       */
      protected Attribute m_attribute;
      
      /**
       * The next dependency.
       */
      protected InverseDependency m_next;
      
      // constructors
      
      /**
       * Constructs the dependency.
       * @param attribute The dependent attribute.
       */
      public IndirectInverseDependency(Attribute attribute)
      {
         m_attribute = attribute;
      }

      // operations

      /**
       * @see nexj.core.meta.Attribute.InverseDependency#getAttribute()
       */
      public Attribute getAttribute()
      {
         return m_attribute;
      }

      /**
       * @see nexj.core.meta.Attribute.InverseDependency#setNext(nexj.core.meta.Attribute.InverseDependency)
       */
      public void setNext(InverseDependency next)
      {
         m_next = next;
      }

      /**
       * @see nexj.core.meta.Attribute.InverseDependency#getNext()
       */
      public InverseDependency getNext()
      {
         return m_next;
      }

      /**
       * @see nexj.core.meta.Attribute.InverseDependency#isIndirect()
       */
      public boolean isIndirect()
      {
         return true;
      }

      /**
       * @see java.lang.Object#clone()
       */
      public Object clone()
      {
         try
         {
            return super.clone();
         }
         catch (CloneNotSupportedException e)
         {
            return null;
         }
      }

      /**
       * @see nexj.core.meta.Attribute.InverseDependencyList#toString()
       */
      public String toString()
      {
         StringBuffer buf = new StringBuffer(32);

         buf.append(m_attribute.getName());
         buf.append("->(");
         append(buf);
         buf.append(')');

         return buf.toString();
      }
   }
}
