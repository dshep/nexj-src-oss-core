// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.upgrade;

import nexj.core.scripting.Compiler;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Lookup;

/**
 * Upgrade running a script.
 */
public final class ScriptUpgrade extends VersionUpgrade
{
   // associations

   /**
    * The upgrade script.
    */
   protected Pair m_body;

   /**
    * The compiled function.
    */
   protected Function m_function;

   /**
    * The source code position map.
    */
   protected Lookup m_posMap = new IdentityHashTab();

   // constructors

   /**
    * Constructs the upgrade.
    * @param sName The upgrade version.
    */
   public ScriptUpgrade(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the upgrade.
    */
   public ScriptUpgrade()
   {
   }

   // operations
   
   /**
    * @return The source code position map.
    */
   public Lookup getPosMap()
   {
      return m_posMap;
   }

   /**
    * Sets the upgrade script.
    * @param body The upgrade script to set.
    */
   public void setBody(Pair body)
   {
      verifyNotReadOnly();
      m_body = body;
   }

   /**
    * @return The upgrade script.
    */
   public Pair getBody()
   {
      return m_body;
   }

   /**
    * @return The compiled function.
    */
   public Function getFunction()
   {
      return m_function;
   }
   
   /**
    * Compiles the upgrade script.
    * @param machine The VM for compilation.
    */
   public void compile(Machine machine)
   {
      if (m_body != null)
      {
         m_function = new Compiler().compile(
            new Pair(Symbol.LAMBDA, new Pair(null, m_body)),
            m_posMap, "upgrade:" + m_upgrade.getName() + '$' + m_sName, machine, false);
         m_body = null;
      }
   }

   /**
    * @see nexj.core.meta.upgrade.VersionUpgrade#getStateKey()
    */
   public Object getStateKey()
   {
      return null;
   }

   /**
    * @see nexj.core.meta.upgrade.VersionUpgrade#createState()
    */
   public UpgradeState createState()
   {
      return null;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();
      m_posMap = null; // free memory not used after compile()
   }

   /**
    * @see nexj.core.meta.upgrade.VersionUpgrade#apply(nexj.core.meta.upgrade.UpgradeState)
    */
   public void apply(UpgradeState state)
   {
   }

   /**
    * @see nexj.core.meta.upgrade.VersionUpgrade#undo(nexj.core.meta.upgrade.UpgradeState)
    */
   public void undo(UpgradeState state)
   {
   }
}
