// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.virtual;

import java.util.List;

import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.PropertyHolder;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Lookup;
import nexj.core.util.TextPosition;

/**
 * Represents a virtual mapping that performs a Create, Update, or Delete.
 * A virtual mapping is primarily composed of a compiled script that
 * performs the persistence work.
 */
public class WorkMapping extends MetadataObject
{
   // attributes

   /**
    * True if the mapping supports batching; false if the instances must be
    * processed one-at-a-time.
    */
   protected boolean m_bBatch;

   // associations

   /**
    * The declarator metaclass.
    */
   protected final Metaclass m_declarator;

   /**
    * The compiled code for performing the persistence work.
    */
   protected Function m_function;

   /**
    * The code that performs the persistence work. Nullified when the metadata are
    * made read-only.
    */
   protected Pair m_script;

   // constructor

   /**
    * Create a new work mapping.
    * 
    * @param declarator The declarator of the work mapping.
    */
   public WorkMapping(Metaclass declarator)
   {
      m_declarator = declarator;
   }

   // operations

   /**
    * Executes the mapping function.
    * @param tobjList List of transfer objects containing the data on which work
    * will be performed.
    * @param machine The virtual machine for function execution.
    * @param fragment The data source fragment.
    */
   public void invoke(List tobjList, Machine machine, VirtualDataSourceFragment fragment)
   {
      PropertyHolder properties = fragment.getPropertyHolder();

      if (m_bBatch)
      {
         machine.invoke(m_function, tobjList, properties, (Object[])null);
      }
      else
      {
         for (int i = 0, nSize = tobjList.size(); i < nSize; i++)
         {
            machine.invoke(m_function, tobjList.get(i), properties, (Object[])null);
         }
      }
   }

   /**
    * Compiles the mapping function.
    * @param machine The virtual machine for compilation.
    * @param sURL The code URL. For example class:Name.persistence.create
    * @param textPosMap The text position map.
    */
   public void compile(Machine machine, String sURL, Lookup textPosMap)
   {
      /*
       * (lambda (this properties)
       *    <script>
       * )
      */
      Pair code = new Pair(
         Symbol.LAMBDA,
         new Pair(
            Pair.list(Symbol.THIS, VirtualMapping.PROPERTIES),
            (m_script == null) ? new Pair(null) : m_script
         )
      );

      textPosMap.put(code, new TextPosition(0, 0, sURL));

      m_function = new Compiler().compile(code, textPosMap, machine, false);
   }

   /**
    * Sets the mapping script.
    * @param script The mapping script.
    */
   public void setScript(Pair script)
   {
      verifyNotReadOnly();
      m_script = script;
   }

   /**
    * Gets the mapping script.
    * @return The mapping script.
    */
   public Pair getScript()
   {
      return m_script;
   }

   /**
    * Set the batching flag.
    * @param bBatch True if the mapping supports batching; false otherwise.
    */
   public void setBatch(boolean bBatch)
   {
      verifyNotReadOnly();
      m_bBatch = bBatch;
   }

   /**
    * Get the batching flag.
    * @return True if the mapping supports batching; false otherwise.
    */
   public boolean isBatch()
   {
      return m_bBatch;
   }

   /**
    * @return The function for this work mapping if compiled.
    */
   public Function getFunction()
   {
      return m_function;
   }

   /**
    * @return The declarator class.
    */
   public Metaclass getDeclarator()
   {
      return m_declarator;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();
      m_script = null;
   }
}
