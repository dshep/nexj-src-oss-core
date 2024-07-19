// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.tools;

import java.io.PrintStream;
import java.util.Properties;

import nexj.core.util.HashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * Abstract command-line tool shell.
 */
public abstract class GenericTool
{
   // constants

   /**
    * Exit code if usage information is printed.
    */
   public final static int EXIT_USAGE = 1;

   /**
    * Exit code if an unhandled exception is caught. Indicates an unexpected error.
    */
   public final static int EXIT_ERROR = 2;

   // attributes

   /**
    * The process exit code.
    */
   private int m_nExitCode;

   // associations

   /**
    * The configuration properties.
    */
   protected Properties m_properties = SysUtil.getConfigProperties();

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(GenericTool.class);

   // operations

   /**
    * Runs the generic tool with command-line arguments.
    * @param args The command-line argument array.
    */
   protected final void run(String[] args)
   {
      String[] sJavaOptionsUsageArray = getOptionUsage();

      if (sJavaOptionsUsageArray != null && sJavaOptionsUsageArray.length == 0)
      {
         sJavaOptionsUsageArray = null;
      }

      String[] sCommandUsageArray = getCommandUsage();

      if (sCommandUsageArray != null && sCommandUsageArray.length == 0)
      {
         sCommandUsageArray = null;
      }

      if ((sCommandUsageArray == null) != (args.length == 0) &&
         (isCommandRequired() && args.length == 0 || args.length != 0) ||
         args.length == 1 && (args[0].equals("/?") || args[0].equals("-?") || args[0].equals("--help")))
      {
         System.err.println("Usage: java [JVM options] " + getClass().getName() +
            ((sCommandUsageArray != null) ? " [options] [commands]" : ""));
         System.err.println("JVM Options:");
         System.err.println("   -Dnexj.config=<config file name>");
         printUsage(System.err, sJavaOptionsUsageArray);

         Option[] optionArray = getOptions();

         if (optionArray != null && optionArray.length != 0)
         {
            String sOptionUsageArray[] = new String[optionArray.length];

            for (int i = 0; i < optionArray.length; i++)
            {
               Option option = optionArray[i];

               sOptionUsageArray[i] = "-" + option.getName() + ": same as -D" + option.getProperty();
            }

            System.err.println("Options:");
            printUsage(System.err, sOptionUsageArray);
         }

         if (sCommandUsageArray != null)
         {
            System.err.println("Commands:");
            printUsage(System.err, sCommandUsageArray);
         }

         System.exit(EXIT_USAGE);
         return;
      }

      try
      {
         int nArg = parseOptions(args);

         begin();

         if (args.length - nArg == 0)
         {
            execute(null);
         }
         else
         {
            for (int i = nArg; i < args.length; ++i)
            {
               if (s_logger.isInfoEnabled())
               {
                  s_logger.info("Executing command \"" + args[i] + "\"");
               }

               execute(args[i]);
            }
         }

         end();
      }
      catch (Throwable e)
      {
         s_logger.error("Error executing the tool command", e);

         String sMsg = e.getMessage();

         for (Throwable t = e.getCause(); t != null && StringUtil.isEmpty(sMsg); t = t.getCause())
         {
            try
            {
               sMsg = t.getMessage();
            }
            catch (Exception x)
            {
            }
         }

         if (StringUtil.isEmpty(sMsg))
         {
            sMsg = e.getClass().getName();
         }

         System.err.println();
         System.err.println("Error: " + sMsg);
         setExitCode(EXIT_ERROR);
      }
      finally
      {
         dispose();
      }

      if (s_logger.isInfoEnabled())
      {
         s_logger.info("Exit code = " + getExitCode());
      }

      System.exit(getExitCode());
   }

   /**
    * Prints the usage array.
    * @param out The print stream where to print the array.
    * @param usageArray The usage array to print.
    */
   private void printUsage(PrintStream out, String[] usageArray)
   {
      if (usageArray == null)
      {
         return;
      }

      for (int i = 0; i < usageArray.length; ++i)
      {
         out.println("   " + usageArray[i]);
      }
   }

   /**
    * Parses the command line options that were passed to this tool.
    * @param sArgArray The command line argument array.
    * @return The index of the first non-optional argument.
    */
   protected int parseOptions(String[] sArgArray)
   {
      int nArg = 0;
      Option[] optionArray = getOptions();

      if (optionArray == null || optionArray.length == 0)
      {
         return 0;
      }

      Lookup optionLookup = new HashTab(optionArray.length);

      for (int i = 0; i < optionArray.length; i++)
      {
         Option option = optionArray[i];

         optionLookup.put(option.getName(), option);
      }

      while (nArg < sArgArray.length)
      {
         String sArg = sArgArray[nArg];

         if (sArg.charAt(0) != '-')
         {
            break;
         }

         nArg++;

         String sName = sArg.substring(1);
         Option option = (Option)optionLookup.get(sName);
         String sValue = option.getValue();

         if (option.isArgRequired())
         {
            if (nArg >= sArgArray.length)
            {
               throw new IllegalArgumentException("Missing argument to option \"" + sName + '"');
            }

            sValue = sArgArray[nArg++];
         }

         setProperty(option.getProperty(), sValue);
      }

      return nArg;
   }

   /**
    * Begins the processing.
    */
   protected void begin() throws Exception
   {
   }

   /**
    * Executes the specified command.
    * @param sCommand The command to execute.
    */
   protected abstract void execute(String sCommand) throws Exception;

   /**
    * Ends the processing.
    */
   protected void end() throws Exception
   {
   }

   /**
    * Releases the allocated system resources.
    */
   protected void dispose()
   {
   }

   /**
    * Sets the process exit code.
    * @param nExitCode The process exit code to set.
    */
   public void setExitCode(int nExitCode)
   {
      m_nExitCode = nExitCode;
   }

   /**
    * @return The process exit code.
    */
   public int getExitCode()
   {
      return m_nExitCode;
   }

   /**
    * @return An array of strings describing the usage of the command-line options.
    */
   protected abstract String[] getOptionUsage();

   /**
    * @return An array of strings describing the usage of the commands.
    */
   protected abstract String[] getCommandUsage();

   /**
    * @return true if a command is required.
    */
   protected boolean isCommandRequired()
   {
      return getCommandUsage() != null;
   }

   /**
    * @return An array of option descriptors.
    */
   protected Option[] getOptions()
   {
      return null;
   }

   /**
    * Gets a configuration property by name.
    * @param sName The property name.
    * @return The property value, or null if not found.
    */
   public final String getProperty(String sName)
   {
      String sProperty = System.getProperty(sName, m_properties.getProperty(sName));

      if (sProperty != null && sProperty.length() == 0)
      {
         sProperty = null;
      }

      return sProperty;
   }

   /**
    * Gets a configuration property by name.
    * @param sName The property name.
    * @param sDefaultValue The default property value.
    * @return The property value, or sDefaultValue if not found.
    */
   protected final String getProperty(String sName, String sDefaultValue)
   {
      String sProperty = System.getProperty(sName, m_properties.getProperty(sName, sDefaultValue));

      if (sProperty != null && sProperty.length() == 0)
      {
         sProperty = null;
      }

      return sProperty;
   }

   /**
    * Gets a required configuration property by name.
    * @param sName The property name.
    * @return The property value.
    * @throws IllegalArgumentException if the property was not found.
    */
   protected final String getRequiredProperty(String sName)
   {
      String sValue = System.getProperty(sName, m_properties.getProperty(sName));

      if (StringUtil.isEmpty(sValue))
      {
         throw new IllegalArgumentException("Missing required property \"" + sName + "\"");
      }

      return sValue;
   }

   /**
    * Gets a flag indicating whether or not a configuration property has been
    * specified.
    * @param sName The property name.
    * @return True if the property has been specified; false otherwise.
    */
   protected final boolean hasProperty(String sName)
   {
      return getProperty(sName, null) != null;
   }

   /**
    * Sets a tool property.
    * @param sName The property name.
    * @param sValue The property value.
    */
   public void setProperty(String sName, String sValue)
   {
      if (m_properties == SysUtil.getConfigProperties())
      {
         m_properties = new Properties(m_properties);
      }

      m_properties.setProperty(sName, sValue);
   }

   /**
    * Appends two string arrays.
    * @param sLeftArray The left array.
    * @param sRightArray The right array.
    * @return The array containing leftArray elements followed by rightArray elements.
    */
   protected static String[] append(String[] sLeftArray, String[] sRightArray)
   {
      if (sLeftArray == null)
      {
         return sRightArray;
      }

      if (sRightArray == null)
      {
         return sLeftArray;
      }

      String[] sArray = new String[sLeftArray.length + sRightArray.length];

      System.arraycopy(sLeftArray, 0, sArray, 0, sLeftArray.length);
      System.arraycopy(sRightArray, 0, sArray, sLeftArray.length, sRightArray.length);

      return sArray;
   }

   // inner classes

   /**
    * Command line option descriptor. Provides a translation from command line options
    * to Java properties.
    */
   protected static class Option
   {
      // attributes

      /**
       * The option name.
       */
      protected String m_sName;

      /**
       * The destination property name.
       */
      protected String m_sProperty;

      /**
       * The property value for an option if an argument is not required.
       */
      protected String m_sValue = "true";

      /**
       * True if the option requires an argument; false if argument is not supported.
       */
      protected boolean m_bArgRequired;

      // constructors

      /**
       * @param sName The option name.
       * @param sProperty The destination property name.
       * @param bArgRequired True if the option requires an argument; false if argument is not supported.
       * @param sValue The property value if an argument is not required; defaults to "true".
       */
      public Option(String sName, String sProperty, boolean bArgRequired, String sValue)
      {
         m_sName = sName;
         m_sProperty = sProperty;
         m_bArgRequired = bArgRequired;
         m_sValue = sValue;
      }

      /**
       * @param sName The option name.
       * @param sProperty The destination property name.
       * @param bArgRequired True if the option requires an argument; false if argument is not supported.
       */
      public Option(String sName, String sProperty, boolean bArgRequired)
      {
         m_sName = sName;
         m_sProperty = sProperty;
         m_bArgRequired = bArgRequired;
      }

      // operations

      /**
       * Gets the option name.
       * @return The option name.
       */
      public String getName()
      {
         return m_sName;
      }

      /**
       * Gets the destination property name.
       * @return The destination property name.
       */
      public String getProperty()
      {
         return m_sProperty;
      }

      /**
       * Gets the property value for a non-argument option.
       * @return The property value.
       */
      public String getValue()
      {
         return m_sValue;
      }

      /**
       * @return True if the option requires an argument; false if an argument is not supported.
       */
      public boolean isArgRequired()
      {
         return m_bArgRequired;
      }
   }
}
