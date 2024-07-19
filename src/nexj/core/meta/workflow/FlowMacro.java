// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.NamedMetadataObject;
import nexj.core.meta.Type;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Lookup;
import nexj.core.util.Undefined;

/**
 * Flow step macro.
 */
public class FlowMacro extends NamedMetadataObject
{
   // attributes

   /**
    * The workflow compatibility flag.
    */
   protected boolean m_bWorkflowCompatible;

   /**
    * The service compatibility flag.
    */
   protected boolean m_bServiceCompatible;

   // associations

   /**
    * The root metadata object.
    */
   protected Metadata m_metadata;

   /**
    * The argument collection.
    */
   protected List m_argList = new ArrayList(4); // of type Argument

   /**
    * The macro body.
    */
   protected Pair m_body;

   /**
    * The compiled code.
    */
   protected Function m_function;
   
   /**
    * The macro body text position map.
    */
   protected Lookup m_textPosMap = new IdentityHashTab();

   // constructors

   /**
    * Constructs the macro.
    * @param sName The macro name.
    */
   public FlowMacro(String sName)
   {
      super(sName);
   }
   
   // operations

   /**
    * Sets the root metadata object.
    * @param metadata The root metadata object to set.
    */
   public void setMetadata(Metadata metadata)
   {
      verifyNotReadOnly();
      m_metadata = metadata;
   }

   /**
    * @return The root metadata object.
    */
   public Metadata getMetadata()
   {
      return m_metadata;
   }
   
   /**
    * Adds a new argument to the macro.
    * @param arg The argument to add.
    */
   public void addArgument(Argument arg)
   {
      verifyNotReadOnly();

      for (int i = m_argList.size() - 1; i >= 0; --i)
      {
         if (((Argument)m_argList.get(i)).getName().equals(arg.getName()))
         {
            throw new MetadataException("err.meta.flow.macroArgDup",
               new Object[]{arg.getName(), getName()});
         }
      }

      m_argList.add(arg);
   }

   /**
    * Gets a argument by ordinal number.
    * @param nOrdinal The argument ordinal number (0-based).
    * @return The argument object.
    */
   public Argument getArgument(int nOrdinal)
   {
      return (Argument)m_argList.get(nOrdinal);
   }

   /**
    * @return The argument count.
    */
   public int getArgumentCount()
   {
      return m_argList.size();
   }

   /**
    * @return An iterator for the contained argument objects.
    */
   public Iterator getArgumentIterator()
   {
      return m_argList.iterator();
   }
   
   /**
    * Sets the macro body.
    * @param body The macro body to set.
    */
   public void setBody(Pair body)
   {
      verifyNotReadOnly();
      m_body = body;
   }

   /**
    * @return The macro body.
    */
   public Pair getBody()
   {
      return m_body;
   }

   /**
    * @return The compiled macro code.
    */
   public Function getFunction()
   {
      return m_function;
   }

   /**
    * Sets the macro body text position map.
    * @param textPosMap The macro body text position map to set.
    */
   public void setTextPositionMap(Lookup textPosMap)
   {
      verifyNotReadOnly();
      m_textPosMap = textPosMap;
   }

   /**
    * @return The macro body text position map.
    */
   public Lookup getTextPositionMap()
   {
      return m_textPosMap;
   }
   
   /**
    * Sets the workflow compatibility flag.
    * @param bWorkflowCompatible The workflow compatibility flag to set.
    */
   public void setWorkflowCompatible(boolean bWorkflowCompatible)
   {
      verifyNotReadOnly();
      m_bWorkflowCompatible = bWorkflowCompatible;
   }

   /**
    * @return The workflow compatibility flag.
    */
   public boolean isWorkflowCompatible()
   {
      return m_bWorkflowCompatible;
   }
   
   /**
    * Sets the service compatibility flag.
    * @param bServiceCompatible The service compatibility flag to set.
    */
   public void setServiceCompatible(boolean bServiceCompatible)
   {
      verifyNotReadOnly();
      m_bServiceCompatible = bServiceCompatible;
   }

   /**
    * @return The service compatibility flag.
    */
   public boolean isServiceCompatible()
   {
      return m_bServiceCompatible;
   }
 
   /**
    * @return The macro code URL.
    */
   public String getURL()
   {
      return "action:" + m_sName;
   }

   /**
    * Compiles the macro.
    * @param machine The VM for compilation.
    */
   public void compile(Machine machine)
   {
      verifyNotReadOnly();

      Pair pair = null;

      for (int i = m_argList.size() - 1; i >= 0; --i)
      {
         pair = new Pair(Symbol.define(((Argument)m_argList.get(i)).getName()), pair);
      }

      // (macro (arg1 ... argN) <body>)

      m_function = new Compiler().compile(
         new Pair(Symbol.MACRO, new Pair(pair, (m_body == null) ? Pair.list(null) : m_body)),
         m_textPosMap, getURL(), machine, false);

      m_body = null;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();
      m_textPosMap = null; // free memory not used after compile()
   }

   // inner classes

   public static class Argument extends NamedMetadataObject
   {
      // attributes

      /**
       * The argument type. Null means list.
       */
      protected Type m_type;

      /**
       * The default argument expression.
       */
      protected Object m_defaultValue = Undefined.VALUE;

      // constructors

      /**
       * Constructs the argument.
       * @param sName The argument name.
       */
      public Argument(String sName)
      {
         super(sName);
      }

      // operations

      /**
       * Sets the argument type.
       * @param type The argument type to set.
       */
      public void setType(Type type)
      {
         verifyNotReadOnly();
         m_type = type;
      }

      /**
       * @return The argument type.
       */
      public Type getType()
      {
         return m_type;
      }

      /**
       * Sets the default argument expression.
       * @param defaultValue The default argument expression to set.
       */
      public void setDefault(Object defaultValue)
      {
         verifyNotReadOnly();
         m_defaultValue = defaultValue;
      }

      /**
       * @return The default argument expression.
       */
      public Object getDefault()
      {
         return m_defaultValue;
      }
   }
}
