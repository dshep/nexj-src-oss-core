package nexj.core.tools;

import java.io.File;
import java.io.FileOutputStream;
import java.util.Iterator;

import nexj.core.meta.NamedMetadataObject;
import nexj.core.meta.Repository;
import nexj.core.testing.unit.UnitTestLogger;
import nexj.core.testing.unit.UnitTestObjectLogger;
import nexj.core.testing.unit.UnitTestPlayer;
import nexj.core.testing.unit.UnitTestStreamLogger;
import nexj.core.util.J2EEUtil;
import nexj.core.util.LocaleUtil;
import nexj.core.util.Logger;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * UnitTest tool.
 */
public class UnitTestTool extends GenericMetadataObjectTool
{
   // associations

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(UnitTestTool.class);

   // operations

   /**
    * @see nexj.core.tools.GenericTool#end()
    */
   protected void end() throws Exception
   {
      if (!Repository.getMetadata().isTestEnvironment())
      {
         throw new IllegalArgumentException("The connections are not designated for testing (expected test=\"true\")");
      }

      super.end();

      if (m_objectList.isEmpty())
      {
         return;
      }

      String sTrackerURL = getProperty("tracker.url");
      String sLogFile = getProperty("unittest.logfile");
      UnitTestLogger testLogger;

      if (!StringUtil.isEmpty(sTrackerURL))
      {
         testLogger = (UnitTestLogger)Class.forName("nexj.core.testing.unit.UnitTestTrackerLogger").getConstructor(new Class[]
         {
            UnitTestTool.class
         }).newInstance(new Object[]
         {
            UnitTestTool.this
         });
      }
      else if (sLogFile != null)
      {
         testLogger = new UnitTestStreamLogger(new FileOutputStream(new File(sLogFile)))
         {
            /**
             * @see nexj.core.testing.unit.UnitTestStreamLogger#log(nexj.core.testing.unit.UnitTestObjectLogger.LogMessage)
             */
            protected void log(LogMessage msg)
            {
               super.log(msg);
               
               if (msg.getCause() != null)
               {
                  setExitCode(3);
               }
            }
         };
      }
      else
      {
         testLogger = new UnitTestObjectLogger()
         {
            /**
             * @see nexj.core.testing.unit.UnitTestObjectLogger#log(nexj.core.testing.unit.UnitTestObjectLogger.LogMessage)
             */
            protected void log(LogMessage msg)
            {
               super.log(msg);
               
               if (msg.getCause() != null)
               {
                  setExitCode(3);
               }
            } 
         };
      }
      
      UnitTestPlayer player = new UnitTestPlayer(testLogger, m_container);

      String sLocale = getProperty("test.locale");

      if (sLocale != null)
      {
         player.setLocale(LocaleUtil.parse(sLocale));
      }

      player.setDebug(StringUtil.parseBoolean(getProperty("test.debug", "false")));
      player.setSchemaResetEnabled(StringUtil.parseBoolean(getProperty("test.schema.reset", "true")));
      player.setSystemUser(getProperty("test.admin"));
      player.run(m_objectList);

      if (m_container != null && J2EEUtil.CONTAINER == J2EEUtil.TEEE)
      {
         m_container.shutdown();
      }
   }

   /**
    * @see nexj.core.tools.GenericMetadataObjectTool#getObjectNameIterator()
    */
   protected Iterator getObjectNameIterator() 
   {
      return Repository.getMetadata().getUnitTestNameIterator();
   }
   /**
    * @see nexj.core.tools.GenericMetadataObjectTool#getObject(java.lang.String)
    */
   protected NamedMetadataObject getObject(String sName)
   {
      return Repository.getMetadata().getUnitTest(sName);
   }
   /**
    * @see nexj.core.tools.GenericMetadataObjectTool#getObjectTypeName()
    */
   protected String getObjectTypeName()
   {
      return "Unit Test";
   }
   /**
    * @see nexj.core.tools.GenericTool#getCommandUsage()
    */
   protected String[] getCommandUsage()
   {
      return new String[]
      {
         "<test> - runs the unit test",
         "<test1> <test2> <test3> ... <testN> - runs multiple unit tests",
         "!Broken* - excludes the tests with names like \"Broken*\"",
         "\\*Test - runs all unit tests with names like \"*Test\" " +
            "(\"\\\" is a kluge to prevent argument expansion in Eclipse)"
      };
   }

   /**
    * @see nexj.core.tools.GenericTool#getOptionUsage()
    */
   protected String[] getOptionUsage()
   {
      return  append(super.getOptionUsage(),
         new String[]
        {
            "-Ddata.url=<url of the exported database dump file>",
            "-Dtest.admin=<system user for initialization>",
            "-Dtest.locale=<locale name>",
            "-Dtest.debug=true|false"
        });
   }

   public static void main(String[] args)
   {
      SysUtil.getConfigProperties().setProperty("dynamic.enabled", "false");
      new UnitTestTool().run(args);
   }
}
