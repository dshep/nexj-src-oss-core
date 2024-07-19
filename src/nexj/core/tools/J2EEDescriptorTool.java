// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.tools;

import nexj.core.meta.j2ee.J2EEDescriptorExporter;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * Command-line tool for generating J2EE descriptors.
 */
public class J2EEDescriptorTool extends GenericTool
{
   /**
    * @see nexj.core.tools.GenericTool#execute(java.lang.String)
    */
   protected void execute(String sCommand) throws Exception
   {
      new J2EEDescriptorExporter().exportDescriptor(getRequiredProperty("out.dir"),
         getRequiredProperty("root.url"), getProperty("base.url"),
         getRequiredProperty("cfg.url"), getRequiredProperty("con.url"),
         StringUtil.parseBoolean(getProperty("overwrite", "true")), SysUtil.getConfigProperties());
   }

   /**
    * @see nexj.core.tools.GenericTool#getOptionUsage()
    */
   protected String[] getOptionUsage()
   {
      return new String[]
      {
         "-Dout.dir=<output directory>",
         "-Droot.url=<root repository URL>",
         "-Dbase.url=<base repository URL>",
         "-Dcfg.url=<configuration URL>",
         "-Dcon.url=<connections URL>",
         "-Doverwrite=<true/false>"
      };
   }

   /**
    * @see nexj.core.tools.GenericTool#getCommandUsage()
    */
   protected String[] getCommandUsage()
   {
      return null;
   }

   public static void main(String[] args)
   {
      new J2EEDescriptorTool().run(args);
   }
}
