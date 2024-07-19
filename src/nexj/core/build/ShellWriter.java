// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.build;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Environment;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.util.JavaEnvUtils;

import nexj.core.util.IOUtil;
import nexj.core.util.OS;
import nexj.core.util.SysUtil;
import nexj.core.util.XMLUtil;

/**
 * Ant task that generates .bat/.sh file which executes a java program.
 */
public class ShellWriter extends Task
{
   private String m_sFilePath;
   private String m_sClassName;
   
   /**
    * true if writing .sh file
    */
   boolean m_bWriteSh;
   
   /**
    * String buffer.
    */
   private StringBuilder m_buffer;
   
   /**
    * Commands to the JVM
    */
   private Commandline m_vmCommand = new Commandline();

   /**
    * Arguments to the java executable
    */
   private Commandline m_argsCommand = new Commandline();

   /**
    * List of system variables
    */
   private List m_systemVariableList = new ArrayList(); //type Variable
   
   /**
    * Contains classpath definition. 
    */
   private Path m_path;
   
   /**
    * Ant attribute setter for property to store java class name.
    */
   public void setClassName(String sClassName)
   {
      m_sClassName = sClassName;
   }

   /**
    * Ant attribute setter for property to store shell file path.
    */
   public void setDest(String sDestinationFileName)
   {
      m_sFilePath = sDestinationFileName;
   }

   /**
    * Adds a path to the classpath.
    *
    * @return created classpath
    */
   public Path createClasspath() 
   {
      return m_path = new Path(getProject()).createPath();
   }

   /**
    * Adds a command-line argument.
    *
    * @return created argument
    */
   public Commandline.Argument createArg() {
       return m_argsCommand.createArgument();
   }

   /**
    * Adds a JVM argument.
    * @return JVM argument created
    */
   public Commandline.Argument createJvmarg() 
   {
       return m_vmCommand.createArgument();
   }
   
   /**
    * Add a system property
    * @param variable a property to be set in the JVM
    */
   public void addSysproperty(Environment.Variable variable) 
   {
      m_systemVariableList.add(variable);
   }
   
   /**
    * Writes out a new line.
    */
   private void appendNewLine()
   {
      m_buffer.append(SysUtil.LINE_SEP);
   }
   
   /**
    * Writes out line.
    * @param sLine Line to write
    */
   private void appendLine(String sLine)
   {
      m_buffer.append(sLine);
      m_buffer.append(SysUtil.LINE_SEP);
   }
   
   /**
    * @see org.apache.tools.ant.Task#execute()
    */
   public void execute() throws BuildException
   {
      m_buffer = new StringBuilder(4096);
      m_bWriteSh = !OS.isWindows();

      final ShellWriteStrategy shellWriteHelper;
      
      if (m_bWriteSh)
      {
         shellWriteHelper = new ShellWriteStrategy()
         {
            /**
             * @see nexj.core.build.ShellWriter.ShellWriteStrategy#writeAssignmentToShellArg(java.lang.String)
             */
            protected void writeAssignmentToShellArg(String sNum)
            {
               m_buffer.append("arg");
               m_buffer.append(sNum);
               m_buffer.append("=$");
               appendLine(sNum);
            }
            
            /**
             * @see nexj.core.build.ShellWriter.ShellWriteStrategy#getExpansionVariableForScriptArg(java.lang.String)
             */
            protected String getExpansionVariableForScriptArg(String sArg)
            {
               return "$arg" + sArg;
            }
            
            /**
             * @see nexj.core.build.ShellWriter.ShellWriteStrategy#writePrologue(java.lang.String[])
             */
            public void writePrologue(String[] javaArgArray)
            {
               appendLine("#!/bin/bash");
               appendNewLine();
               appendLine("systemProps=\"\"");
               appendNewLine();
               appendLine("while (( \"$#\" )); do");
               appendLine("   str=$1");
               appendLine("   if [ ${str:0:2} == \"-D\" ]; then");
               appendLine("      systemProps=\"$systemProps$str \"");
               appendLine("      shift");
               appendLine("   else");
               assignNumericalArgs("      ", javaArgArray);
               appendLine("      break");
               appendLine("   fi");
               appendLine("done");
               appendNewLine();
            }
            
            public void writeJavaArgs()
            {
               m_buffer.append("\"$@\"");
            }

            public void writeSystemProperties()
            {
               m_buffer.append("$systemProps");
            }

            public void writeEpilogue()
            {
            }
         };
      }
      else
      {
         shellWriteHelper = new ShellWriteStrategy()
         {
            /**
             * @see nexj.core.build.ShellWriter.ShellWriteStrategy#writePrologue(java.lang.String[])
             */
            public void writePrologue(String[] sArgumentArray)
            {
               boolean bUseJavaArgs = false;
               boolean bNumericalArgs = false;
               
               for (int i = 0; i < sArgumentArray.length; ++i)
               {
                  if ("$*".equals(sArgumentArray[i]))
                  {
                     bUseJavaArgs = true;
                  }
                  
                  if (!bNumericalArgs && parseNumericalJavaArg(sArgumentArray[i]) != null)
                  {
                     bNumericalArgs = true;
                  }
               }

               appendLine("@echo off");
               appendLine("setlocal");
               appendNewLine();
               
               if (bUseJavaArgs)
               {
                  appendLine("set JAVAARGS=");
               }
               
               appendLine("set SYSTEMPROPS=");
               appendNewLine();
               appendLine(":processSystemProps");
               appendLine("set STR=%~1");
               appendLine("if \"%STR%\"==\"\" goto success");
               
               String sGotoLabel = (bNumericalArgs) ? "processJavaArgs" : (bUseJavaArgs) ? "processJavaArgsRecurse" : "success";
               
               appendLine("if not \"%STR:~0,2%\"==\"-D\" goto " + sGotoLabel);
               
               appendLine("set SYSTEMPROPS=%SYSTEMPROPS%%STR% "); 
               appendLine("shift");
               appendLine("goto processSystemProps");
               appendNewLine();
               
               if (bNumericalArgs)
               {
                  appendLine(":processJavaArgs");
                  assignNumericalArgs(null, sArgumentArray);
                  appendNewLine();
               }
               
               if (bUseJavaArgs)
               {
                  appendLine(":processJavaArgsRecurse");
                  appendLine("set JAVAARGS=%JAVAARGS%\"%STR%\" "); 
                  appendLine("shift");
                  appendLine("set STR=%~1");
                  appendLine("if \"%STR%\"==\"\" goto success");
                  appendLine("if \"%STR:~0,2%\"==\"-D\" goto failure"); 
                  appendLine("goto processJavaArgsRecurse");
                  appendNewLine();
               }
               
               appendLine(":failure");
               appendLine("echo System properties must be specified before all arguments and enclosed in quotes.");
               appendLine("goto end");
               appendNewLine();
               appendLine(":success");
               appendNewLine();
            }

            /**
             * @see nexj.core.build.ShellWriter.ShellWriteStrategy#getExpansionVariableForScriptArg(java.lang.String)
             */
            protected String getExpansionVariableForScriptArg(String sArg)
            {
               return "%ARG" + sArg + "%";
            }
            
            public void writeJavaArgs()
            {
               m_buffer.append("%JAVAARGS%");
            }

            public void writeSystemProperties()
            {
               m_buffer.append("%SYSTEMPROPS%");
            }
            
            /**
             * @see nexj.core.build.ShellWriter.ShellWriteStrategy#writeAssignmentToShellArg(java.lang.String)
             */
            protected void writeAssignmentToShellArg(String sNum)
            {
               m_buffer.append("set ARG");
               m_buffer.append(sNum);
               m_buffer.append("=%~");
               appendLine(sNum);
            }
            
            public void writeEpilogue()
            {
               appendNewLine();
               appendLine(":end");
               appendLine("endlocal");
            }
         };
      }

      String[] javaArgArray = m_argsCommand.getArguments();

      shellWriteHelper.writePrologue(javaArgArray);
      
      m_buffer.append(JavaEnvUtils.getJreExecutable("java"));
      m_buffer.append(" -cp \"");
      
      String sPathArray[] = m_path.list();
      String sPathSeparator = (m_bWriteSh) ? ":" : ";";
      
      for (int i = 0; i < sPathArray.length; ++i)
      {
         m_buffer.append(sPathArray[i]);
         m_buffer.append(sPathSeparator);
      }
      
      m_buffer.append("\"");
      
      String[] argArray = m_vmCommand.getArguments();
      
      for (int i = 0; i < argArray.length; ++i)
      {
         m_buffer.append(" ");
         m_buffer.append(argArray[i]);
      }
      
      for (Iterator itr = m_systemVariableList.iterator(); itr.hasNext();)
      {
         Environment.Variable var = (Environment.Variable)itr.next();
         writeProperty(m_buffer, var.getKey(), var.getValue());
      }

      m_buffer.append(" ");
      shellWriteHelper.writeSystemProperties();
      m_buffer.append(" ");
      m_buffer.append(m_sClassName);
      
      for (int i = 0; i < javaArgArray.length; ++i)
      {
         if ("$*".equals(javaArgArray[i]))
         {
            m_buffer.append(" ");
            
            shellWriteHelper.writeJavaArgs();
            continue;
         }

         m_buffer.append(" \"");
         
         m_buffer.append(shellWriteHelper.translateJavaArg(javaArgArray[i]));

         m_buffer.append("\"");
      }

      m_buffer.append(SysUtil.LINE_SEP);

      shellWriteHelper.writeEpilogue();
      
      File targetFile = new File(m_sFilePath);
      targetFile = new File(targetFile.getParentFile(), targetFile.getName() + ((m_bWriteSh) ? ".sh" : ".bat"));
      
      Writer writer = null;
      
      try
      {
         writer = IOUtil.openBufferedWriter(targetFile, XMLUtil.ENCODING);
         IOUtil.copy(writer, new StringReader(m_buffer.toString()));
         writer.close();
         writer = null;
      }
      catch (IOException e)
      {
      }
      finally
      {
         if (writer != null)
         {
            try
            {
               writer.close();
            }
            catch (IOException e)
            {
            }
         }
      }
   }
   
   /**
    * Writes out a system property.
    * @param buffer The writer.
    * @param sName The system property name.
    * @param sValue The system property value.
    */
   private void writeProperty(StringBuilder buffer, String sName, String sValue)
   {
      if (sValue == null)
      {
         return;
      }
      
      buffer.append(" -D");
      buffer.append(sName);
      buffer.append("=\"");
      buffer.append(escapePropertyValue(sValue));
      buffer.append("\"");
   }
   
   /**
    * @param sValue The property value to escape
    * @return An escaped value.
    */
   private String escapePropertyValue(String sValue)
   {
      return (m_bWriteSh) ? sValue : sValue.replaceAll("%", "%%");
   }
   
   /**
    * Interface to write out shell script.
    */
   public abstract class ShellWriteStrategy
   {
      /**
       * Iterates the java arguments and writes out assignment statements for any numerical arguments
       * $1, S2, etc.
       * @param sPrefix Prefix string of spaces to prepend or null.
       * @param sJavaArgArray The java args.
       */
      protected void assignNumericalArgs(String sPrefix, String sJavaArgArray[])
      {
         for (int i = 0; i < sJavaArgArray.length; ++i)
         {
            String sNum = parseNumericalJavaArg(sJavaArgArray[i]);
            
            if (sNum != null)
            {
               if (sPrefix != null)
               {
                  ShellWriter.this.m_buffer.append(sPrefix);
               }
               
               writeAssignmentToShellArg(sNum);
            }
         }
      }
      
      /**
       * Writes out an assignment to a variable to a numerical shell argument.
       * @param sNum A number as a string.
       */
      protected abstract void writeAssignmentToShellArg(String sNum);
      
      /**
       * @param sArg The java arg.
       * @return A number as a string if sArg is of the format $1, $2, etc. otherwise null.
       */
      protected String parseNumericalJavaArg(String sArg)
      {
         if (sArg.startsWith("$"))
         {
            try
            {
               String sNumber = sArg.substring(1);
               Integer.parseInt(sNumber);
               return sNumber;
            }
            catch (NumberFormatException e)
            {
               //no-op
            }
         }
         
         return null;
      }
      
      /**
       * Writes script to process bat args.
       * @param javaArgArray The java arguments.
       */
      public abstract void writePrologue(String javaArgArray[]);
      
      /**
       * Writes out expansion variable for system properties
       */
      public abstract void writeSystemProperties();
      
      /**
       * Writes out expansion variable for all java args minus the system properties
       */
      public abstract void writeJavaArgs();
      
      /**
       * Write cleanup script.
       */
      public abstract void writeEpilogue();
      
      /**
       * @param sArg Java arg.
       * @return sArg or output variable if sArg is $1, $2, etc.
       */
      public final String translateJavaArg(String sArg)
      {
         String sNumber = parseNumericalJavaArg(sArg);
         
         return (sNumber == null) ? sArg : getExpansionVariableForScriptArg(sNumber);
      }
      
      /**
       * @param sArg A number as a string.
       * @return An variable expansion which outputs the script argument without enclosing quotes
       */
      protected abstract String getExpansionVariableForScriptArg(String sArg);
   }
}
