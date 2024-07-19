// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;

/**
 * Definition of an atomic type.
 */
public abstract class AtomicType extends Type
{
   // constructors

   /**
    * Constructs a new atomic type definition.
    * @param sName The type name.
    */
   public AtomicType(String sName)
   {
      m_sName = sName;
   }

   /**
    * Sets the base type.
    * @param base The base type.
    */
   public void setBase(AtomicType base)
   {
      m_base = base;
   }
}
