// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.meta.NamedMetadataObject;
import nexj.core.scripting.Symbol;

/**
 * Flow variable.
 */
public class Variable extends NamedMetadataObject
{
   // constants
   
   /**
    * Argument flag.
    */
   public final static int ARG = 0x0001;
   
   /**
    * Channel type flag.
    */
   public final static int CHANNEL = 0x0002;
   
   // attributes
   
   /**
    * The variable ordinal number.
    */
   protected int m_nOrdinal;

   /**
    * The variable flags.
    */
   protected int m_nFlags;

   // association
   
   /**
    * The variable symbol.
    */
   protected Symbol m_symbol;

   // constructors

   /**
    * Constructs the variable.
    * @param sName The variable name.
    */
   public Variable(String sName)
   {
      super(sName);
      m_symbol = Symbol.define(sName);
   }

   // operations

   /**
    * @return The variable symbol.
    */
   public Symbol getSymbol()
   {
      return m_symbol;
   }
   
   /**
    * Sets the variable ordinal number.
    * @param nOrdinal The variable ordinal number to set.
    */
   public void setOrdinal(int nOrdinal)
   {
      verifyNotReadOnly();
      m_nOrdinal = nOrdinal;
   }

   /**
    * @return The variable ordinal number.
    */
   public int getOrdinal()
   {
      return m_nOrdinal;
   }

   /**
    * Sets or clears the specified flags.
    * @param nFlags The flags.
    * @param bSet True to set, false to clear.
    */
   public void setFlags(int nFlags, boolean bSet)
   {
      verifyNotReadOnly();
      
      if (bSet)
      {
         m_nFlags |= nFlags;
      }
      else
      {
         m_nFlags &= ~nFlags;
      }
   }
   
   /**
    * Sets all the variable flags.
    * @param nFlags The variable flags to set.
    */
   public void setFlags(int nFlags)
   {
      verifyNotReadOnly();
      m_nFlags = nFlags;
   }

   /**
    * @return The variable flags.
    */
   public int getFlags()
   {
      return m_nFlags;
   }
}
