// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.scripting.Machine;
import nexj.core.scripting.Symbol;
import nexj.core.util.ExceptionHolder;

/**
 * Class member metadata - attribute or event.
 */
public abstract class Member extends DocumentedNamedMetadataObject
{
   // attributes

   /**
    * True if this is a static (class object) member.
    */
   protected boolean m_bStatic;

   /**
    * The member visibility.
    */
   protected byte m_nVisibility;

   /**
    * The backward compatibility flag.
    */
   protected boolean m_bCompatible;

   /**
    * The symbol corresponding to this member.
    */
   protected Symbol m_symbol;

   // associations

   /**
    * The access attribute.
    */
   protected Attribute m_accessAttribute;

   /**
    * The containing class.
    */
   protected Metaclass m_metaclass;

   /**
    * The declaring class.
    */
   protected Metaclass m_declarator;

   /**
    * The root declarator.
    */
   protected Metaclass m_rootDeclarator;

   // constructors

   /**
    * Creates a member with a given name.
    * @param sName The member name.
    */
   public Member(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * Determines if the class member is an attribute.
    * @return True if the class member is an attribute.
    */
   public abstract boolean isAttribute();

   /**
    * Sets the static flag.
    * @param bStatic The static flag to set.
    */
   public void setStatic(boolean bStatic)
   {
      verifyNotReadOnly();
      m_bStatic = bStatic;
   }

   /**
    * @return The static flag.
    */
   public boolean isStatic()
   {
      return m_bStatic;
   }
   
   /**
    * Sets the member visibility.
    * @param nVisibility The member visibility to set.
    */
   public void setVisibility(byte nVisibility)
   {
      verifyNotReadOnly();
      m_nVisibility = nVisibility;
   }

   /**
    * @return The member visibility.
    */
   public byte getVisibility()
   {
      return m_nVisibility;
   }

   /**
    * Sets the backward compatibility flag.
    * @param bCompatible The backward compatibility flag to set.
    */
   public void setCompatible(boolean bCompatible)
   {
      verifyNotReadOnly();
      m_bCompatible = bCompatible;
   }

   /**
    * @return The backward compatibility flag.
    */
   public boolean isCompatible()
   {
      return m_bCompatible;
   }

   /**
    * @return The member symbol. 
    */
   public Symbol getSymbol()
   {
      return m_symbol;
   }

   /**
    * Sets the access attribute.
    * @param accessAttribute The access attribute to set.
    */
   public void setAccessAttribute(Attribute accessAttribute)
   {
      verifyNotReadOnly();
      
      if (accessAttribute != null && accessAttribute.getType() != Primitive.BOOLEAN)
      {
         throw new MetadataException("err.meta.accessAttributeType",
            new Object[]{accessAttribute.getName()});
      }

      m_accessAttribute = accessAttribute;
   }

   /**
    * @return The access attribute.
    */
   public Attribute getAccessAttribute()
   {
      return m_accessAttribute;
   }
   
   /**
    * Sets the containing class.
    * @param metaclass The containing class to set.
    */
   public void setMetaclass(Metaclass metaclass)
   {
      verifyNotReadOnly();
      m_metaclass = metaclass;
      m_symbol = Symbol.define(getName());
   }

   /**
    * @return The containing class.
    */
   public Metaclass getMetaclass()
   {
      return m_metaclass;
   }

   /**
    * Checks whether a metaclass has a member that is compatible with this instance.
    * @param metaclass The metaclass.
    * @param eh The holder in which to add exceptions.
    */
   public abstract void checkCompatibility(Metaclass metaclass, ExceptionHolder eh);

   /**
    * Sets the declaring class.
    * @param declarator The declaring class to set.
    */
   public void setDeclarator(Metaclass declarator)
   {
      verifyNotReadOnly();
      m_declarator = declarator;
   }

   /**
    * @return The declaring class.
    */
   public Metaclass getDeclarator()
   {
      return m_declarator;
   }
   
   /**
    * Sets the root declarator.
    * @param rootDeclarator The root declarator to set.
    */
   public void setRootDeclarator(Metaclass rootDeclarator)
   {
      verifyNotReadOnly();
      m_rootDeclarator = rootDeclarator;
   }

   /**
    * @return The root declarator.
    */
   public Metaclass getRootDeclarator()
   {
      return m_rootDeclarator;
   }
   
   /**
    * Compiles the member source code.
    * @param machine The VM to use for compilation.
    */
   public abstract void compile(Machine machine);

   /**
    * @see nexj.core.meta.NamedMetadataObject#hashCode()
    */
   public int hashCode()
   {
      int nHash = super.hashCode();

      if (m_metaclass != null)
      {
         nHash ^= m_metaclass.hashCode() << 8;
      }

      return nHash;
   }
}
