// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.ui;

import nexj.core.meta.Type;
import nexj.core.scripting.Function;
import nexj.core.scripting.Pair;
import nexj.core.util.Holder;

/**
 * Attribute metadata for UI subset of Domain Model.
 */
public interface AttributeMeta 
{
   /**
    * @return Attribute name.
    */
   public String getName();

   /**
    * @return Attribute caption.
    */
   public String getCaption();
   
   /**
    * @return Returns the collection.
    */
   public boolean isCollection();

   /**
    * @return Returns the required.
    */
   public boolean isRequired();

   /**
    * @return Returns the type.
    */
   public Type getType();

   /**
    * @return Returns the value.
    */
   public Object getValue();

   /**
    * @return Returns the ClassMeta.
    */
   public ClassMeta getClassMeta();
   
   /**
    * @return Declarator ClassMeta object.
    */
   public ClassMeta getDeclaratorClassMeta();

   /**
    * @return The enumeration class.
    */
   public ClassMeta getEnumerationClassMeta();

   /**
    * @return Returns the reverse.
    */
   public AttributeMeta getReverseAttributeMeta();

   /**
    * @return Returns the isCalculated.
    */
   public boolean isCalculated();

   /**
    * @return Alternative order by.
    */
   public Pair getOrderBy();

   /**
    * @return True if static.
    */
   public boolean isStatic();

   /**
    * @return Returns the initializer.
    */
   public Object getInitializer();

   /**
    * @return The access attribute.
    */
   public AttributeMeta getAccessAttributeMeta();

   /**
    * @return The update privilege.
    */
   public int getUpdatePrivilegeOrdinal();

   /**
    * @return The read privilege.
    */
   public int getReadPrivilegeOrdinal();

   /**
    * @return The set of read privileges.
    */
   public Holder getReadPrivilegeOrdinals();

   /**
    * @return Whether this is a calculated attribute, and whether the client can
    *         choose for this attribute to be calculated on the client side.
    * @see #getValueFunction()
    * @see #isCalculated()
    */
   public boolean isClientCalculable();

   /**
    * @return Calculated attribute function. May be null if
    *         {@link #isClientCalculable()} is false.
    */
   public Function getValueFunction();

   /**
    * Get the function dependencies. Only applicable if {@link #isClientCalculable()}
    * is true.
    * 
    * @return Data dependencies for the function.
    */
   public Pair getValueDependencyAssociations();
   
   /**
    * @return The attribute ordinal.
    */
   public int getOrdinal();
}