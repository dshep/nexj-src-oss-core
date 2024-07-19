// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.scripting.Symbol;

/**
 * Describes the formal arguments of an event.
 */
public final class Argument extends DocumentedNamedMetadataObject implements Cloneable
{
   // attributes

   /**
    * True if the argument holds a collection of values e.g. array.
    */
   protected boolean m_bCollection;

   /**
    * True if the argument cannot be null.
    */
   protected boolean m_bRequired;

   /**
    * The argument ordinal number.
    */
   protected int m_nOrdinal = -1;

   // associations

   /**
    * The containing event.
    */
   private Event m_event;
   
   /**
    * The argument symbol.
    */
   private Symbol m_symbol;

   /**
    * The argument type (null means that it has not been set, defaulting to ANY).
    */
   private Type m_type;

   // constructors

   /**
    * Creates an argument with a given name.
    * @param sName The argument name.
    */
   public Argument(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * Append Argument description to a StringBuilder.
    * @param buf The object to append argument description to (not null).
    * @see Argument#toString()
    */
   public Appendable appendTo(StringBuilder buf)
   {
      if (m_type == null)
      {
         return buf.append(getName()); // legacy compatibility
      }

      buf.append(getName()).append(':').append(getType().getName());

      if (isCollection())
      {
         buf.append("[]");
      }

      return buf;
   }

   /**
    * @see Argument#m_bCollection
    */
   public boolean isCollection()
   {
      return m_bCollection;
   }

   /**
    * @see Argument#m_bCollection
    */
   public void setCollection(boolean bCollection)
   {
      verifyNotReadOnly();
      m_bCollection = bCollection;
   }

   /**
    * Determines if this argument is derivation type/collection compatible with a base argument.
    * Valid to derive !null from null, valid if this or base is untyped.
    * NOTE: argument "requirement" check is not performed.
    * @param base The base argument (null == always compatible).
    * @return True if it is compatible.
    */
   public boolean isCompatibleWith(Argument base)
   {
      return base == null || // valid for comparison of unset Event result types
             base.m_type == null ||
             m_type == null ||
             (base.getType().isUpcast(getType()) && base.isCollection() == isCollection());
   }

   /**
    * Sets the containing event.
    * @param event The containing event to set.
    */
   public void setEvent(Event event)
   {
      verifyNotReadOnly();
      m_event = event;
      m_symbol = Symbol.define(getName());
   }

   /**
    * @return The containing event.
    */
   public Event getEvent()
   {
      return m_event;
   }

   /**
    * @see Attribute#m_bRequired
    */
   public boolean isRequired()
   {
      return m_bRequired;
   }

   /**
    * @see Attribute#m_bRequired
    */
   public void setRequired(boolean bRequired)
   {
      verifyNotReadOnly();
      m_bRequired = bRequired;
   }

   /**
    * @return The argument symbol.
    */
   public Symbol getSymbol()
   {
      return m_symbol;
   }

   /**
    * Set the argument type.
    * @param type The argument type (not null).
    */
   public void setType(Type type)
   {
      assert type != null;

      verifyNotReadOnly();
      m_type = type;
   }

   /**
    * @return The argument type.
    */
   public Type getType()
   {
      return (m_type == null) ? Primitive.ANY : m_type;
   }

   /**
    * Sets the argument ordinal number.
    * @param nOrdinal The argument ordinal number to set.
    */
   public void setOrdinal(int nOrdinal)
   {
      verifyNotReadOnly();
      m_nOrdinal = nOrdinal;
   }

   /**
    * @return The argument ordinal number.
    */
   public int getOrdinal()
   {
      return m_nOrdinal;
   }

   /**
    * Inherits the argument definitions from the specified argument.
    * The most restrictive definition is chosen.
    * @param arg The argument from which to inherit the arguments (not null).
    * @throws MetadataException if inheritance is not compatible.
    */
   public void inherit(Argument arg)
   {
      assert arg != null;

      if (arg.m_type == null) // untyped source
      {
         return; // nothing to inherit
      }

      if (!isCompatibleWith(arg))
      {
         throw new MetadataException("err.meta.incompatibleOverriddenArgument",
                                     new Object[]{getName(),
                                                  getEvent().getName(),
                                                  getEvent().getMetaclass().getName(),
                                                  arg.getEvent().getMetaclass().getName()});
      }

      if (m_type == null) // untyped requires complete inheritance
      {
         setType(arg.getType());
         setCollection(arg.isCollection());
         setRequired(arg.isRequired());
      }

      // inherit most restrictive type
      if (getType().isUpcast(arg.getType()))
      {
         setType(arg.getType());
      }
   }

   /**
    * @see nexj.core.meta.NamedMetadataObject#toString()
    */
   public String toString()
   {
      if (m_type == null)
      {
         return super.toString(); // legacy support
      }
      else if (!isCollection())
      {
         return super.toString() + ':' + getType().getName();
      }

      return super.toString() + ':' + getType().getName() + "[]";
   }
}