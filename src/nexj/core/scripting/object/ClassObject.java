// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import java.util.Set;

import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.meta.TypeConversionException;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.Function;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.TextPosition;

/**
 * The generic class object implementation.
 */
public class ClassObject extends NamedObject implements Type
{
   // constants

   /**
    * Empty method object array.
    */
   protected final static MethodObject[] EMPTY_METHOD_ARRAY = new MethodObject[0];

   /**
    * Empty attribute object array.
    */
   protected final static AttributeObject[] EMPTY_ATTRIBUTE_ARRAY = new AttributeObject[0];

   /**
    * Empty class object array.
    */
   protected final static ClassObject[] EMPTY_CLASS_ARRAY = new ClassObject[0];

   /**
    * Empty Java class object array.
    */
   protected final static Class[] EMPTY_JAVA_CLASS_ARRAY = new Class[0];

   /**
    * Empty function array.
    */
   protected final static Function[] EMPTY_FUNCTION_ARRAY = new Function[0];

   /**
    * Empty int array.
    */
   protected final static int[] EMPTY_INT_ARRAY = new int[0];

   /**
    * ":add-options" symbol.
    */
   public final static Symbol ADD_OPTIONS_SYMBOL = Symbol.define(":add-options");

   /**
    * Global variable with the currently compiled class for macros.
    */
   protected final static Symbol SYS_CURRENT_CLASS = Symbol.define("sys:current-class");

   // attributes

   /**
    * The public visibility flag.
    * True means that the member is accessible through RPC.
    */
   protected boolean m_bPublic;

   /**
    * The forward-ref flag.
    * True if the class is a yet-undefined forward reference.
    */
   protected boolean m_bForward;

   // associations

   /**
    * The direct methods of this class (inherited ones are not included here).
    */
   protected MethodObject[] m_methodArray = EMPTY_METHOD_ARRAY;

   /**
    * The direct attributes of this class (inherited ones are not included here).
    */
   protected AttributeObject[] m_attributeArray = EMPTY_ATTRIBUTE_ARRAY;

   /**
    * The resolved attributes ordered by their offset.
    */
   protected AttributeObject[] m_resolvedAttributeArray = EMPTY_ATTRIBUTE_ARRAY;

   /**
    * The array of initializer attribute offsets.
    */
   protected int[] m_initializerArray = EMPTY_INT_ARRAY;

   /**
    * Map of an attribute symbol to an integer offset: Integer[Symbol].
    * Populated during attribute resolution process.
    * Includes inherited attribute symbols.
    */
   protected Lookup m_offsetMap;

   /**
    * The base classes of this class.
    */
   protected ClassObject[] m_baseArray = EMPTY_CLASS_ARRAY;

   /**
    * The derived classes for which this class is a base.
    */
   protected ClassObject[] m_derivedArray = EMPTY_CLASS_ARRAY;

   // constructors

   /**
    * Constructs a named class object.
    * @param metaclass The metaclass object.
    * @param symbol The class symbol.
    */
   public ClassObject(MetaclassObject metaclass, Symbol symbol)
   {
      super(metaclass, symbol);
   }

   // operations

   /**
    * Sets the metaclass object.
    * @param metaclass The metaclass object.
    */
   public void setClassObject(MetaclassObject metaclass)
   {
      if (metaclass != m_class)
      {
         for (int i = 0; i < m_baseArray.length; ++i)
         {
            ClassObject base = m_baseArray[i].getClassObject();

            m_class.removeBase(base);
            metaclass.addBase(base);
         }

         for (int i = 0; i < m_derivedArray.length; ++i)
         {
            m_derivedArray[i].getClassObject().replaceBase(m_class, metaclass);
         }

         m_class = metaclass;
      }
   }

   /**
    * @return The metaclass object for metaobject protocol purposes.
    */
   public MetaclassObject getMetaclass()
   {
      return (MetaclassObject)m_class;
   }

   /**
    * @return True if the class object is a metaclass.
    */
   public boolean isMetaclass()
   {
      return false;
   }

   /**
    * @see nexj.core.scripting.object.GenericObject#setValue(int, java.lang.Object, Machine)
    */
   public void setValue(int nOffset, Object value, Machine machine)
   {
      getMetaclass().setValue(this, nOffset, value, machine);
   }

   /**
    * @see nexj.core.scripting.object.GenericObject#getValue(int, Machine)
    */
   public Object getValue(int nOffset, Machine machine)
   {
      return getMetaclass().getValue(this, nOffset, machine);
   }

   /**
    * Creates a new object instance.
    * Typically mapped to the ":new" method.
    * @return The created object.
    */
   public ObjectOriented createObject()
   {
      return getMetaclass().createInstance(this);
   }

   /**
    * Determines if this class is derived from another one.
    * @param base The other class.
    * @return True if this class is derived from base, directly or indirectly.
    */
   public boolean isDerivedFrom(ClassObject base)
   {
      for (int i = 0; i < m_baseArray.length; ++i)
      {
         if (m_baseArray[i] == base)
         {
            return true;
         }
      }

      for (int i = 0; i < m_baseArray.length; ++i)
      {
         if (m_baseArray[i].isDerivedFrom(base))
         {
            return true;
         }
      }

      return false;
   }

   /**
    * Determines if this class is the same or a derived class.
    * @param base The other class.
    * @return True if this is the same as or a derived from base.
    */
   public boolean isA(ClassObject base)
   {
      return this == base || isDerivedFrom(base);
   }

   /**
    * Sets the forward-ref flag.
    * @param bForward The forward-ref flag to set.
    */
   public void setForward(boolean bForward)
   {
      change();
      m_bForward = bForward;
      complete();
   }

   /**
    * @return The forward-ref flag.
    */
   public boolean isForward()
   {
      return m_bForward;
   }

   /**
    * Sets the public visibility flag.
    * @param bPublic The public visibility flag to set.
    */
   public void setPublic(boolean bPublic)
   {
      change();
      m_bPublic = bPublic;
      complete();
   }

   /**
    * @return The public visibility flag.
    */
   public boolean isPublic()
   {
      return m_bPublic;
   }

   /**
    * Adds a derived class.
    */
   protected void addDerived(ClassObject derived)
   {
      int nCount = m_derivedArray.length;
      ClassObject[] derivedArray = new ClassObject[nCount + 1];

      System.arraycopy(m_derivedArray, 0, derivedArray, 0, nCount);
      derivedArray[nCount] = derived;
      m_derivedArray = derivedArray;
   }

   /**
    * Gets a derived class by ordinal number.
    * @param nOrdinal The derived class ordinal number.
    * @return The derived class.
    */
   public ClassObject getDerived(int nOrdinal)
   {
      return m_derivedArray[nOrdinal];
   }

   /**
    * Removes a derived class.
    * @return The removed object, or null if not found.
    */
   protected ClassObject removeDerived(ClassObject derived)
   {
      int nCount = m_derivedArray.length;

      for (int i = 0; i < nCount; ++i)
      {
         if (m_derivedArray[i] == derived)
         {
            if (nCount == 1)
            {
               m_derivedArray = EMPTY_CLASS_ARRAY;
            }
            else
            {
               ClassObject[] derivedArray = new ClassObject[nCount - 1];

               System.arraycopy(m_derivedArray, 0, derivedArray, 0, i);
               System.arraycopy(m_derivedArray, i + 1, derivedArray, i, nCount - i - 1);
               m_derivedArray = derivedArray;
            }

            return derived;
         }
      }

      return null;
   }

   /**
    * @return The base class count.
    */
   public int getDerivedCount()
   {
      return m_derivedArray.length;
   }

   /**
    * Adds a base class.
    * @param base The base class to add.
    */
   public void addBase(ClassObject base)
   {
      for (int i = 0; i < m_baseArray.length; ++i)
      {
         if (base == m_baseArray[i])
         {
            throw new DeclarationException("err.scripting.baseDup",
               new Object[]{base.getName(), getName()});
         }
      }

      if (base.isA(this))
      {
         throw new DeclarationException("err.scripting.derivationLoop",
            new Object[]{base.getName(), getName()});
      }

      getMetaclass().validateBase(base);

      base.change();
      change();

      int nCount = m_baseArray.length;
      ClassObject[] baseArray = new ClassObject[nCount + 1];

      System.arraycopy(m_baseArray, 0, baseArray, 0, nCount);
      baseArray[nCount] = base;
      m_baseArray = baseArray;

      base.addDerived(this);

      if (m_class != base.getClassObject() && !isMetaclass())
      {
         m_class.addBase(base.getClassObject());
      }

      complete();
   }

   /**
    * Gets a base class by ordinal number.
    * @param nOrdinal The base class ordinal number.
    * @return The base class.
    */
   public ClassObject getBase(int nOrdinal)
   {
      return m_baseArray[nOrdinal];
   }
   /**
    * Replaces the base class with the specified ordinal number.
    * @param nOrdinal The ordinal number.
    * @param base The new base.
    * @return The old base class.
    */
   protected ClassObject setBase(int nOrdinal, ClassObject base)
   {
      ClassObject oldBase = m_baseArray[nOrdinal];

      if (base != oldBase)
      {
         getMetaclass().validateBase(base);

         oldBase.change();
         base.change();
         change();

         m_baseArray[nOrdinal] = base;

         oldBase.removeDerived(this);
         base.addDerived(this);

         if (m_class != base.getClassObject() && !isMetaclass())
         {
            m_class.removeBase(base.getClassObject());
            m_class.addBase(base.getClassObject());
         }

         complete();
      }

      return oldBase;
   }

   /**
    * Replaces a base class object.
    * @param oldBase The old class object.
    * @param newBase The new class object.
    * @return The old class object, or null if not found.
    */
   protected ClassObject replaceBase(ClassObject oldBase, ClassObject newBase)
   {
      for (int i = 0; i < m_baseArray.length; ++i)
      {
         if (m_baseArray[i] == oldBase)
         {
            return setBase(i, newBase);
         }
      }

      return null;
   }

   /**
    * Removes a base class with a given ordinal number.
    * @param nOrdinal The base class ordinal number.
    * @return The removed class.
    */
   protected ClassObject removeBase(int nOrdinal)
   {
      ClassObject base = m_baseArray[nOrdinal];
      int nCount = m_baseArray.length;

      base.change();
      change();

      if (nCount == 1)
      {
         m_baseArray = EMPTY_CLASS_ARRAY;
      }
      else
      {
         ClassObject[] baseArray = new ClassObject[nCount - 1];

         System.arraycopy(m_baseArray, 0, baseArray, 0, nOrdinal);
         System.arraycopy(m_baseArray, nOrdinal + 1, baseArray, nOrdinal, nCount - nOrdinal - 1);
         m_baseArray = baseArray;
      }

      base.removeDerived(this);

      if (m_class != base.getClassObject() && !isMetaclass())
      {
         m_class.removeBase(base.getClassObject());
      }

      complete();

      return base;
   }

   /**
    * Removes a base class.
    * @return The removed class or null if not found.
    */
   public ClassObject removeBase(ClassObject base)
   {
      for (int i = 0; i < m_baseArray.length; ++i)
      {
         if (base == m_baseArray[i])
         {
            return removeBase(i);
         }
      }

      return null;
   }

   /**
    * Removes a base class.
    * @param symbol The base class symbol.
    * @return The removed class or null if not found.
    */
   public ClassObject removeBase(Symbol symbol)
   {
      for (int i = 0; i < m_baseArray.length; ++i)
      {
         if (m_baseArray[i].getSymbol().equals(symbol))
         {
            return removeBase(i);
         }
      }

      return null;
   }

   /**
    * @return The base class count.
    */
   public int getBaseCount()
   {
      return m_baseArray.length;
   }

   /**
    * Adds an attribute to this class.
    * @param attribute The attribute to add. Its holder will be set to this class object.
    * @param bReplace True to replace an already existing attribute with the same name,
    *    false to throw an exception instead.
    */
   public void addAttribute(AttributeObject attribute, boolean bReplace)
   {
      Symbol symbol = attribute.getSymbol();
      int nOrdinal = findAttributeOrdinal(symbol);

      if (nOrdinal >= 0)
      {
         if (!bReplace)
         {
            throw new DeclarationException("err.scripting.attributeDup",
               new Object[]{symbol.getName(), getName()});
         }
      }
      else
      {
         for (int i = 0; i <= 1; ++i)
         {
            if (findMethod(symbol, i) != null)
            {
               throw new DeclarationException("err.scripting.methodDup",
                  new Object[]{symbol.getName(), Primitive.createInteger(i), getName()});
            }
         }
      }

      if (attribute.getHolder() != null && attribute.getHolder() != this)
      {
         throw new DeclarationException("err.scripting.attributeReuse",
            new Object[]{symbol.getName(),
               attribute.getHolder().getName(),
               getName()});
      }

      getMetaclass().validateAttribute(this, attribute);

      change();

      if (nOrdinal >= 0)
      {
         m_attributeArray[nOrdinal] = attribute;
      }
      else
      {
         int nCount = m_attributeArray.length;
         AttributeObject[] attributeArray = new AttributeObject[nCount + 1];

         System.arraycopy(m_attributeArray, 0, attributeArray, 0, nCount);
         attributeArray[nCount] = attribute;
         m_attributeArray = attributeArray;
      }

      attribute.setHolder(this);

      complete();
   }

   /**
    * Removes an attribute from this class.
    * @param nOrdinal The attribute ordinal number.
    * @return The removed attribute.
    */
   protected AttributeObject removeAttribute(int nOrdinal)
   {
      AttributeObject attribute = m_attributeArray[nOrdinal];
      int nCount = m_attributeArray.length;

      change();

      if (nCount == 1)
      {
         m_attributeArray = EMPTY_ATTRIBUTE_ARRAY;
      }
      else
      {
         AttributeObject[] attributeArray = new AttributeObject[nCount - 1];

         System.arraycopy(m_attributeArray, 0, attributeArray, 0, nOrdinal);
         System.arraycopy(m_attributeArray, nOrdinal + 1, attributeArray, nOrdinal, nCount - nOrdinal - 1);
         m_attributeArray = attributeArray;
      }

      attribute.setHolder(null);

      complete();

      return attribute;
   }

   /**
    * Removes an attribute from this class.
    * @param symbol The attribute symbol.
    * @return The removed attribute, or null if not found.
    */
   public AttributeObject removeAttribute(Symbol symbol)
   {
      for (int i = 0; i < m_attributeArray.length; ++i)
      {
         if (m_attributeArray[i].getSymbol().equals(symbol))
         {
            return removeAttribute(i);
         }
      }

      return null;
   }

   /**
    * Creates a method object.
    * @param symbol The method symbol.
    * @param function The implementation function.
    * @return The method object.
    */
   public MethodObject createMethod(Symbol symbol, PCodeFunction function)
   {
      return getMetaclass().createMethod(this, symbol, function.getArgCount() - 1,
         function.isVarArg(), function);
   }

   /**
    * Adds a method to this class.
    * @param sName The method name.
    * @param nArgCount The argument count.
    * @param bVarArg True to indicate that the last argument is of variable length.
    * @param function The implementation function.
    */
   public void addMethod(String sName, int nArgCount, boolean bVarArg, Function function)
   {
      addMethod(getMetaclass().createMethod(this, Symbol.define(sName), nArgCount,
         bVarArg, function), false);
   }

   /**
    * Adds a method to this class.
    * @param name The method name.
    * @param nArgCount The argument count.
    * @param bVarArg True to indicate that the last argument is of variable length.
    * @param function The implementation function.
    */
   public void addMethod(Symbol name, int nArgCount, boolean bVarArg, Function function)
   {
      addMethod(getMetaclass().createMethod(this, name, nArgCount,
         bVarArg, function), false);
   }

   /**
    * Adds a method to the class.
    * @param method The method to add.
    * @param bReplace True to replace an already existing method with the same
    *    dispatch characteristics (e.g. name and argument count),
    *    false to throw an exception instead.
    */
   public void addMethod(MethodObject method, boolean bReplace)
   {
      Symbol symbol = method.getSymbol();
      int nOrdinal = findMethodOrdinal(symbol, method.getMinArgCount());

      if ((nOrdinal >= 0) ? !bReplace :
         method.getMinArgCount() <= 1 && findAttributeOrdinal(symbol) >= 0)
      {
         throw new DeclarationException("err.scripting.methodDup",
            new Object[]{symbol.getName(),
               Primitive.createInteger(method.getArgCount()),
               getName()});
      }

      if (method.getHolder() != null && method.getHolder() != this)
      {
         throw new DeclarationException("err.scripting.methodReuse",
            new Object[]{symbol.getName(),
               Primitive.createInteger(method.getArgCount()),
               method.getHolder().getName(),
               getName()});
      }

      getMetaclass().validateMethod(this, method);

      change();

      if (nOrdinal >= 0)
      {
         m_methodArray[nOrdinal] = method;
      }
      else
      {
         int nCount = m_methodArray.length;
         MethodObject[] methodArray = new MethodObject[nCount + 1];

         System.arraycopy(m_methodArray, 0, methodArray, 0, nCount);
         methodArray[nCount] = method;
         m_methodArray = methodArray;
      }

      method.setHolder(this);

      complete();
   }

   /**
    * Removes a method from this class.
    * @param nOrdinal The method ordinal number.
    * @return The removed method.
    */
   protected MethodObject removeMethod(int nOrdinal)
   {
      MethodObject method = m_methodArray[nOrdinal];
      int nCount = m_methodArray.length;

      change();

      if (nCount == 1)
      {
         m_methodArray = EMPTY_METHOD_ARRAY;
      }
      else
      {
         MethodObject[] methodArray = new MethodObject[nCount - 1];

         System.arraycopy(m_methodArray, 0, methodArray, 0, nOrdinal);
         System.arraycopy(m_methodArray, nOrdinal + 1, methodArray, nOrdinal, nCount - nOrdinal - 1);
         m_methodArray = methodArray;
      }

      method.setHolder(null);

      complete();

      return method;
   }

   /**
    * Removes a method from this class.
    * @param symbol The method symbol.
    * @param nArgCount The argument count.
    * @return The removed method, or null if not found.
    */
   public MethodObject removeMethod(Symbol symbol, int nArgCount)
   {
      for (int i = 0; i < m_methodArray.length; ++i)
      {
         if (m_methodArray[i].isMatching(symbol, nArgCount))
         {
            return removeMethod(i);
         }
      }

      return null;
   }

   /**
    * Finds a direct member of this class by member symbol and argument count.
    * @param symbol The method symbol.
    * @param nArgCount The actual argument count.
    * @return The member, or null if not found.
    */
   public MemberObject findMember(Symbol symbol, int nArgCount)
   {
      MemberObject member = findMethod(symbol, nArgCount);

      if (member == null && nArgCount <= 1)
      {
         member = findAttribute(symbol);
      }

      return member;
   }

   /**
    * Checks whether a given member is contained directly in this class object.
    * @param member The member to look for.
    * @return True if the member is contained directly in this class object.
    */
   public boolean hasMember(MemberObject member)
   {
      if (member instanceof AttributeObject)
      {
         for (int i = 0; i < m_attributeArray.length; ++i)
         {
            if (m_attributeArray[i] == member)
            {
               return true;
            }
         }
      }
      else
      {
         for (int i = 0; i < m_methodArray.length; ++i)
         {
            if (m_methodArray[i] == member)
            {
               return true;
            }
         }
      }

      return false;
   }

   /**
    * Finds a direct or inherited member by member symbol and argument count.
    * @param symbol The method symbol.
    * @param nArgCount The actual argument count.
    * @return The member, or null if not found.
    */
   public MemberObject resolveMember(Symbol symbol, int nArgCount)
   {
      MemberObject member = findMember(symbol, nArgCount);

      if (member == null)
      {
         member = resolveBaseMember(symbol, nArgCount);
      }

      return member;
   }

   /**
    * Finds an inherited member by member symbol and argument count.
    * @param symbol The method symbol.
    * @param nArgCount The actual argument count.
    * @return The member, or null if not found.
    */
   public MemberObject resolveBaseMember(Symbol symbol, int nArgCount)
   {
      for (int i = 0; i < m_baseArray.length; ++i)
      {
         MemberObject member = m_baseArray[i].resolveMember(symbol, nArgCount);

         if (member != null)
         {
            return member;
         }
      }

      return null;
   }

   /**
    * Finds a direct or inherited function by member symbol and argument count.
    * @param symbol The method symbol.
    * @param nArgCount The actual argument count.
    * @return The function, or null if not found.
    */
   public Function resolveFunction(Symbol symbol, int nArgCount)
   {
      ClassEnvironment env = getEnvironment();
      Function function = env.findFunction(this, symbol, nArgCount);

      if (function != null)
      {
         return function;
      }

      MemberObject member = resolveMember(symbol, nArgCount);

      if (member == null)
      {
         return null;
      }

      function = (member instanceof MethodObject) ?
         ((MethodObject)member).getFunction() :
         createAccessor((AttributeObject)member, nArgCount != 0);

      if (function != null)
      {
         env.addFunction(this, symbol, nArgCount, function);
      }

      return function;
   }

   /**
    * Finds an inherited function by member symbol and argument count.
    * @param symbol The method symbol.
    * @param nArgCount The actual argument count.
    * @return The function, or null if not found.
    */
   public Function resolveBaseFunction(Symbol symbol, int nArgCount)
   {
      ClassEnvironment env = getEnvironment();
      Function function = env.findFunction(this, symbol, -1 - nArgCount);

      if (function != null)
      {
         return function;
      }

      MemberObject member = resolveBaseMember(symbol, nArgCount);

      if (member == null)
      {
         return null;
      }

      function = (member instanceof MethodObject) ?
         ((MethodObject)member).getFunction() :
         createAccessor((AttributeObject)member, nArgCount != 0);

      if (function != null)
      {
         env.addFunction(this, symbol, -1 - nArgCount, function);
      }

      return function;
   }

   /**
    * Finds the ordinal number of a direct method of this class by symbol and argument count.
    * @param symbol The method symbol.
    * @return The attribute ordinal number, or -1 if not found.
    */
   protected int findMethodOrdinal(Symbol symbol, int nArgCount)
   {
      for (int i = 0; i < m_methodArray.length; ++i)
      {
         MethodObject method = m_methodArray[i];

         if (method.isMatching(symbol, nArgCount))
         {
            return i;
         }
      }

      return -1;
   }

   /**
    * Finds a direct method of this class by symbol and argument count.
    * @param symbol The method symbol.
    * @param nArgCount The argument count.
    */
   public MethodObject findMethod(Symbol symbol, int nArgCount)
   {
      int i = findMethodOrdinal(symbol, nArgCount);

      return (i < 0) ? null : m_methodArray[i];
   }

   /**
    * @return The direct method count.
    */
   public int getMethodCount()
   {
      return m_methodArray.length;
   }

   /**
    * Gets a direct method.
    * @param nOrdinal The ordinal of the method to get.
    * @return The method.
    */
   public MethodObject getMethod(int nOrdinal)
   {
      return m_methodArray[nOrdinal];
   }

   /**
    * Finds the ordinal number of a direct attribute of this class by symbol.
    * @param symbol The attribute symbol.
    * @return The attribute ordinal number, or -1 if not found.
    */
   protected int findAttributeOrdinal(Symbol symbol)
   {
      for (int i = 0; i < m_attributeArray.length; ++i)
      {
         AttributeObject attribute = m_attributeArray[i];

         if (attribute.getSymbol().equals(symbol))
         {
            return i;
         }
      }

      return -1;
   }

   /**
    * Finds a direct attribute of this class by symbol.
    * @param symbol The attribute symbol.
    * @return The attribute, or null if not found.
    */
   public AttributeObject findAttribute(Symbol symbol)
   {
      int i = findAttributeOrdinal(symbol);

      return (i < 0) ? null : m_attributeArray[i];
   }

   /**
    * @return the direct attribute count.
    */
   public int getAttributeCount()
   {
      return m_attributeArray.length;
   }

   /**
    * Gets a direct attribute.
    * @param nOrdinal The ordinal of the attribute to get.
    * @return The attribute.
    */
   public AttributeObject getAttribute(int nOrdinal)
   {
      return m_attributeArray[nOrdinal];
   }

   /**
    * Gets the array of attributes (including inherited attributes), ordered by attribute ordinal.
    * @return Array of resolved attributes.
    */
   public AttributeObject[] resolveAttributeArray()
   {
      return m_resolvedAttributeArray;
   }

   /**
    * @return The attribute count, taking into account inherited attributes.
    */
   public int resolveAttributeCount()
   {
      return m_resolvedAttributeArray.length;
   }

   /**
    * Gets a resolved attribute by offset.
    * @param nOffset The attribute offset relative to an object start.
    * @return The resolved attribute.
    */
   public AttributeObject resolveAttribute(int nOffset)
   {
      return m_resolvedAttributeArray[nOffset];
   }

   /**
    * Finds an attribute object offset by symbol, relative to an object start.
    * The first attribute has an offset of 0, the second of 1 etc.
    * @param symbol The attribute symbol.
    * @return The attribute offset, or -1 if not found.
    */
   public int resolveAttributeOffset(Symbol symbol)
   {
      if (m_offsetMap != null)
      {
         Integer offset = (Integer)m_offsetMap.get(symbol);

         if (offset != null)
         {
            return offset.intValue();
         }
      }

      return -1;
   }

   /**
    * Finds an attribute object offset, relative to an object start.
    * The first attribute has an offset of 0, the second of 1 etc.
    * @param attribute The attribute object.
    * @return The attribute offset, or -1 if not found.
    */
   public int resolveAttributeOffset(AttributeObject attribute)
   {
      return resolveAttributeOffset(attribute.getSymbol());
   }

   /**
    * Creates an accessor for a given attribute.
    * @param attribute The attribute.
    * @param bSet True for a setter, false for a getter.
    * @return The accessor, or null if not available.
    */
   protected Function createAccessor(AttributeObject attribute, boolean bSet)
   {
      int nOffset = resolveAttributeOffset(attribute);

      if (nOffset < 0)
      {
         return null;
      }

      if (bSet)
      {
         if (attribute.getSetterFunction() != null)
         {
            return new InterceptingSetter(nOffset, attribute.getSetterFunction());
         }

         return new Setter(nOffset);
      }

      return new Getter(nOffset);
   }

   /**
    * Adds the attributes offsets from a given base class, and recursively, its own base classes.
    * @param base The base class from which to take the attributes.
    */
   protected void resolveAttributeOffsets(ClassObject base)
   {
      for (int i = 0; i < base.m_baseArray.length; ++i)
      {
         resolveAttributeOffsets(base.m_baseArray[i]);
      }

      int nCount = base.m_attributeArray.length;

      if (nCount != 0)
      {
         if (m_offsetMap == null)
         {
            m_offsetMap = new HashTab();
         }

         int nDirectCount = 0;

         for (int i = 0; i < nCount; ++i)
         {
            AttributeObject attribute = base.m_attributeArray[i];
            Symbol symbol = attribute.getSymbol();

            Object old = m_offsetMap.put(symbol, Primitive.createInteger(m_offsetMap.size()));

            if (old == null)
            {
               ++nDirectCount;
            }
            else
            {
               m_offsetMap.put(symbol, old);
            }
         }

         AttributeObject[] resolvedAttributeArray;

         if (nDirectCount != 0)
         {
            resolvedAttributeArray =
               new AttributeObject[m_resolvedAttributeArray.length + nDirectCount];
            System.arraycopy(m_resolvedAttributeArray, 0, resolvedAttributeArray, 0,
               m_resolvedAttributeArray.length);
            m_resolvedAttributeArray = resolvedAttributeArray;
         }
         else
         {
            resolvedAttributeArray = m_resolvedAttributeArray;
         }

         for (int i = 0; i < nCount; ++i)
         {
            AttributeObject attribute = base.m_attributeArray[i];

            resolvedAttributeArray[((Integer)m_offsetMap.get(attribute.getSymbol())).intValue()] = attribute;
         }
      }
   }

   /**
    * Gets an initializer attribute offset by ordinal number.
    * @param nOrdinal The initializer ordinal number.
    * @return The initializer attribute offset.
    */
   public int getInitializerOffset(int nOrdinal)
   {
      return m_initializerArray[nOrdinal];
   }

   /**
    * @return The initializer count.
    */
   public int getInitializerCount()
   {
      return m_initializerArray.length;
   }

   /**
    * Resolves the initializers.
    */
   protected void resolveInitializers()
   {
      int nCount = 0;

      for (int i = 0; i < m_resolvedAttributeArray.length; ++i)
      {
         if (m_resolvedAttributeArray[i].getInitializerFunction() != null)
         {
            ++nCount;
         }
      }

      if (nCount == 0)
      {
         m_initializerArray = EMPTY_INT_ARRAY;
      }
      else
      {
         m_initializerArray = new int[nCount];
         int k = 0;

         for (int i = 0; i < m_resolvedAttributeArray.length && k < nCount; ++i)
         {
            AttributeObject attribute = m_resolvedAttributeArray[i];

            if (attribute.getInitializerFunction() != null)
            {
               m_initializerArray[k++] = i;
            }
         }

         getMetaclass().sortInitializers(this, m_initializerArray);
      }
   }

   /**
    * Resolves inherited members.
    */
   protected void resolve()
   {
      m_offsetMap = null;
      m_resolvedAttributeArray = EMPTY_ATTRIBUTE_ARRAY;
      resolveAttributeOffsets(this);
      resolveInitializers();
      getMetaclass().resolve(this);
   }

   /**
    * Prepares the class object for change.
    */
   protected void change()
   {
      getEnvironment().change(this);
   }

   /**
    * Completes the class object change.
    */
   protected void complete()
   {
      getEnvironment().complete();
   }

   /**
    * Completes the class object and its derived classes recursively.
    * @param identitySet Set of already visited class objects.
    */
   public void complete(Set identitySet)
   {
      if (identitySet.add(this))
      {
         resolve();

         for (int i = 0; i < m_derivedArray.length; ++i)
         {
            m_derivedArray[i].complete(identitySet);
         }
      }
   }

   /**
    * Removes all the members and base classes.
    */
   public void reset()
   {
      for (int i = getMethodCount() - 1; i >= 0; --i)
      {
         removeMethod(i);
      }

      for (int i = getAttributeCount() - 1; i >= 0; --i)
      {
         removeAttribute(i);
      }

      for (int i = getBaseCount() - 1; i >= 0; --i)
      {
         removeBase(i);
      }

      m_bForward = false;
      m_bPublic = false;
   }

   /**
    * Removes the class from the inheritance hierarchy.
    */
   public void remove()
   {
      for (int i = getBaseCount() - 1; i >= 0; --i)
      {
         removeBase(i);
      }

      for (int i = m_derivedArray.length - 1; i >= 0; --i)
      {
         m_derivedArray[i].removeBase(this);
      }
   }

   /**
    * Gets a function from a given value.
    * If the value is not a function, its value is compiled and evaluated first.
    * @param fun The value.
    * @param sName The function name. Can be null.
    * @param sQualifier Second part of the function name. Can be null.
    * @param machine The VM.
    * @return The function object.
    */
   protected Function compile(Object fun, final String sName, final String sQualifier, Machine machine)
   {
      if (!(fun instanceof Function))
      {
         GlobalEnvironment env = machine.getGlobalEnvironment();
         Lookup posMap = env.getTextPositionMap();

         env.defineVariable(SYS_CURRENT_CLASS, this);

         try
         {
            if (sName != null && posMap != null && posMap.size() != 0)
            {
               String sURL = Compiler.findPosURL(fun, posMap);

               if (sURL == null)
               {
                  TextPosition pos = (TextPosition)posMap.valueIterator().next();

                  sURL = pos.getURL();
               }

               StringBuilder buf = new StringBuilder(64);

               if (sURL != null)
               {
                  int i = sURL.indexOf('#');

                  buf.append(sURL, 0, (i < 0) ? sURL.length() : i);
               }

               buf.append('#');
               buf.append(getName());
               buf.append(' ');
               buf.append(sName);

               if (sQualifier != null)
               {
                  buf.append(sQualifier);
               }
               else
               {
                  if (fun instanceof Pair)
                  {
                     Pair pair = (Pair)fun;

                     if (Symbol.LAMBDA.equals(pair.getHead()) && pair.getTail() instanceof Pair)
                     {
                        pair = pair.getNext();

                        int nCount = -1;

                        for (Object obj = pair.getHead(); obj instanceof Pair; obj = ((Pair)obj).getTail())
                        {
                           ++nCount;
                        }

                        if (nCount >= 0)
                        {
                           buf.append('/');
                           buf.append(nCount);
                        }
                     }
                  }
               }

               Compiler.setPosURLs(fun, buf.toString(), posMap);
            }

            fun = machine.invoke(new Compiler().compile(fun, posMap, machine, true),
               (Pair)null);
         }
         finally
         {
            env.removeVariable(SYS_CURRENT_CLASS);
         }
      }

      return (Function)fun;
   }

   /**
    * Exposes members from ClassObject.
    * @param classObject The destination class object.
    */
   public static void addMembers(ClassObject classObject)
   {
      NamedObject.addMembers(classObject);
      addOptionsMethod(classObject, "err.scripting.classOption");

      classObject.addMethod(":public?", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(Boolean.valueOf(((ClassObject)machine.getArg(0, nArgCount)).isPublic()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":public", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            boolean bPublic = !Boolean.FALSE.equals(machine.getArg(1, nArgCount));

            ((ClassObject)machine.getArg(0, nArgCount)).setPublic(bPublic);
            machine.returnValue(Boolean.valueOf(bPublic), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":base", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((ClassObject)machine.getArg(0, nArgCount))
               .getBase(((Number)machine.getArg(1, nArgCount)).intValue()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":base-count", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(Primitive.createInteger(((ClassObject)machine.getArg(0, nArgCount))
               .getBaseCount()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":add-base", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            ((ClassObject)machine.getArg(0, nArgCount)).addBase((ClassObject)machine.getArg(1, nArgCount));
            machine.returnValue(null, nArgCount);

            return false;
         }
      });

      classObject.addMethod(":remove-base", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            ClassObject clazz = (ClassObject)machine.getArg(0, nArgCount);
            Object base = machine.getArg(1, nArgCount);

            machine.returnValue(
               (base instanceof ClassObject) ?
                  clazz.removeBase((ClassObject)base) :
                  clazz.removeBase((Symbol)base), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":add-derived", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            ((ClassObject)machine.getArg(1, nArgCount)).addBase((ClassObject)machine.getArg(0, nArgCount));
            machine.returnValue(null, nArgCount);

            return false;
         }
      });

      classObject.addMethod(":derived", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((ClassObject)machine.getArg(0, nArgCount))
               .getDerived(((Number)machine.getArg(1, nArgCount)).intValue()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":derived-count", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(Primitive.createInteger(((ClassObject)machine.getArg(0, nArgCount))
               .getDerivedCount()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":add-attribute", 3, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            ClassObject classObject = (ClassObject)machine.getArg(0, nArgCount);
            Object attribute = machine.getArg(1, nArgCount);
            AttributeObject attributeObject = (attribute instanceof AttributeObject) ?
                  (AttributeObject)attribute :
                  classObject.getMetaclass().createAttribute(classObject, (Symbol)attribute);

            machine.invoke(attributeObject, ADD_OPTIONS_SYMBOL,
               new Pair(machine.getArg(2, nArgCount)));

            ((ClassObject)machine.getArg(0, nArgCount)).addAttribute(
               attributeObject, !Boolean.FALSE.equals(machine.getArg(3, nArgCount)));

            machine.returnValue(attributeObject, nArgCount);

            return false;
         }
      });

      classObject.addMethod(":remove-attribute", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((ClassObject)machine.getArg(0, nArgCount))
               .removeAttribute((Symbol)machine.getArg(1, nArgCount)), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":add-method", 2, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            MethodObject methodObject = (MethodObject)machine.getArg(1, nArgCount);

            ((ClassObject)machine.getArg(0, nArgCount)).addMethod(
               methodObject, !Boolean.FALSE.equals(machine.getArg(2, nArgCount)));

            machine.returnValue(methodObject, nArgCount);

            return false;
         }
      });

      classObject.addMethod(":attribute-count", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(Primitive.createInteger(((ClassObject)machine.getArg(0, nArgCount))
               .getAttributeCount()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":attribute", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((ClassObject)machine.getArg(0, nArgCount))
               .getAttribute(((Number)machine.getArg(1, nArgCount)).intValue()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":class-attribute-count", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(Primitive.createInteger(((ClassObject)machine.getArg(0, nArgCount))
               .getMetaclass().getAttributeCount()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":class-attribute", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((ClassObject)machine.getArg(0, nArgCount))
               .getMetaclass().getAttribute(((Number)machine.getArg(1, nArgCount)).intValue()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":add-method", 4, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            ClassObject classObject = (ClassObject)machine.getArg(0, nArgCount);
            Symbol symbol = (Symbol)machine.getArg(1, nArgCount);

            MethodObject methodObject = classObject.createMethod(symbol,
               (PCodeFunction)classObject.compile(machine.getArg(3, nArgCount),
                  symbol.getName(), null, machine));

            machine.invoke(methodObject, ADD_OPTIONS_SYMBOL,
               new Pair(machine.getArg(2, nArgCount)));

            classObject.addMethod(
               methodObject, !Boolean.FALSE.equals(machine.getArg(4, nArgCount)));

            machine.returnValue(methodObject, nArgCount);

            return false;
         }
      });

      classObject.addMethod(":find-method", 2, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            ClassObject classObject = (ClassObject)machine.getArg(0, nArgCount);
            Symbol symbol = (Symbol)machine.getArg(1, nArgCount);
            int nMethodArgCount = ((Integer)machine.getArg(2, nArgCount)).intValue();

            machine.returnValue(classObject.findMethod(symbol, nMethodArgCount), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":find-class-method", 2, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            ClassObject classObject = ((ClassObject)machine.getArg(0, nArgCount)).getMetaclass();
            Symbol symbol = (Symbol)machine.getArg(1, nArgCount);
            int nMethodArgCount = ((Integer)machine.getArg(2, nArgCount)).intValue();

            machine.returnValue(classObject.findMethod(symbol, nMethodArgCount), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":method-count", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(Primitive.createInteger(((ClassObject)machine.getArg(0, nArgCount))
               .getMethodCount()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":method", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((ClassObject)machine.getArg(0, nArgCount))
               .getMethod(((Number)machine.getArg(1, nArgCount)).intValue()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":class-method-count", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(Primitive.createInteger(((ClassObject)machine.getArg(0, nArgCount))
               .getMetaclass().getMethodCount()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":class-method", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((ClassObject)machine.getArg(0, nArgCount))
               .getMetaclass().getMethod(((Number)machine.getArg(1, nArgCount)).intValue()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":remove-method", 2, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((ClassObject)machine.getArg(0, nArgCount))
               .removeMethod((Symbol)machine.getArg(1, nArgCount),
                  ((Number)machine.getArg(2, nArgCount)).intValue()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":forward?", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(Boolean.valueOf(((ClassObject)machine.getArg(0, nArgCount))
               .isForward()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":reset", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            ((ClassObject)machine.getArg(0, nArgCount)).reset();
            machine.returnValue(null, nArgCount);

            return false;
         }
      });

      classObject.addMethod(":is-a?", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(Boolean.valueOf(((ClassObject)machine.getArg(0, nArgCount))
               .isA((ClassObject)machine.getArg(1, nArgCount))), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":has-member?", 2, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            int nMemberArgCount = ((Number)machine.getArg(2, nArgCount)).intValue();

            machine.returnValue(Boolean.valueOf(nMemberArgCount >= 0 &&
               ((ClassObject)machine.getArg(0, nArgCount)).resolveFunction(
               (Symbol)machine.getArg(1, nArgCount), nMemberArgCount) != null),
               nArgCount);

            return false;
         }
      });

      classObject.addMethod(":new", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((ClassObject)machine.getArg(0, nArgCount)).createObject(), nArgCount);

            return false;
         }
      });

      classObject.addMethod("new", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            ObjectOriented obj = ((ClassObject)machine.getArg(0, nArgCount)).createObject();

            machine.invoke(obj, INITIALIZE_SYMBOL, (Object[])null);
            machine.returnValue(obj, nArgCount);

            return false;
         }
      });
   }

   /**
    * Exposes the ":add-options" method.
    * @param classObject The destination class object.
    * @param sErrCode The error code.
    */
   public static void addOptionsMethod(ClassObject classObject, final String sErrCode)
   {
      classObject.addMethod(ADD_OPTIONS_SYMBOL.getName(), 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            NamedObject obj = (NamedObject)machine.getArg(0, nArgCount);
            Pair arg = null;

            for (Pair opts = (Pair)machine.getArg(1, nArgCount); opts != null; opts = opts.getNext())
            {
               Pair opt = (Pair)opts.getHead();

               if (arg == null)
               {
                  arg = new Pair(opt.getTail());
               }
               else
               {
                  arg.setHead(opt.getTail());
               }

               try
               {
                  machine.invoke(obj, opt.getHead(), arg);
               }
               catch (InvocationException e)
               {
                  throw new DeclarationException(sErrCode,
                     new Object[]{(opts.getHead() instanceof Pair) ?
                        String.valueOf(((Pair)opts.getHead()).getHead()) : null,
                        obj.getName()}, e);
               }
            }

            machine.returnValue(null, nArgCount);

            return false;
         }
      });
   }

   /**
    * @return The class environment.
    */
   public static ClassEnvironment getEnvironment()
   {
      return ThreadContextHolder.getContext().getMachine().getGlobalEnvironment();
   }

   /**
    * @see nexj.core.meta.Type#convert(java.lang.Object)
    */
   public Object convert(Object value) throws TypeConversionException
   {
      throw new UnsupportedOperationException(toString() + ".convert(" + value + '"');
   }

   /**
    * @see nexj.core.meta.Type#getBaseType()
    */
   public Type getBaseType()
   {
      throw new UnsupportedOperationException(toString() + ".getBaseType()");
   }

   /**
    * @see nexj.core.meta.Type#isPrimitive()
    */
   public boolean isPrimitive()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.Type#isUpcast(nexj.core.meta.Type)
    */
   public boolean isUpcast(Type type)
   {
      if (!(type instanceof ClassObject))
      {
         return false;
      }

      ClassObject clazz = (ClassObject)type;

      return clazz.isA(this);
   }
}
