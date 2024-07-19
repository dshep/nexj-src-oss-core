// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.upgrade;

import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalMapping;
import nexj.core.meta.persistence.sql.SQLScript;
import nexj.core.meta.persistence.sql.SQLStatement;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.meta.persistence.sql.upgrade.ExecStep;
import nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgrade;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Upgrade step generator for triggering a reload via SysVersion'loaded.
 */
public class LoadUpgrade
{
   /**
    * Constructor. Use the load(...) method instead.
    */
   private LoadUpgrade()
   {
   }

   /**
    * Function to generate an appropriate VersionUpgrade depending on where SysVersion is persisted.
    * @param sName The version name.
    * @param metadata The metadata this UpgradeVersion will work with (not null).
    * @param machine The scheme machine used for scheme compilation (not null).
    * @param upgrade The upgrade this UpgradeVersion will be used for (not null).
    */
   public static VersionUpgrade create(
      String sName, Metadata metadata, Machine machine, Upgrade upgrade)
   {
      Metaclass versionClass = metadata.getMetaclass(Metadata.VERSION_CLASS_NAME);
      PersistenceMapping mapping = versionClass.getPersistenceMapping();
      DataSource ds = mapping.getDataSource();

      if (ds instanceof RelationalDatabase) // SysVersion persisted in RDBMS
      {
         Table table = ((RelationalMapping)mapping).getPrimaryTable();
         RelationalSchemaUpgrade version = new RelationalSchemaUpgrade(sName);
         ExecStep step = new ExecStep();
         SQLScript script = new SQLScript();
         SQLStatement stmt = new SQLStatement();

         stmt.setSQL("update ${table:"+ table.getName() + "} set ${keyword:" +
            table.getColumn("loaded").getName() + "} = 0");

         script.addStatement(stmt);
         step.getScriptHolder().addScript(script);
         version.setDataSource(ds);
         version.addStep(step);

         return version;
      }

      // SysVersion accessible via a Scheme function
      ScriptUpgrade version = new ScriptUpgrade(sName);

      // (SysVersion 'unload)
      version.setBody(
         Pair.list(
            Pair.list(versionClass.getSymbol(), Pair.list(Symbol.QUOTE, Symbol.define("unload")))));
      version.setUpgrade(upgrade); // required for compile() to work
      version.compile(machine);

      return version;
   }
}