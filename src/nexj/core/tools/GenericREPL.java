// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.tools;

import java.io.BufferedReader;
import java.io.FilterWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;

import nexj.core.scripting.Parser;
import nexj.core.util.GenericException;
import nexj.core.util.IOUtil;
import nexj.core.util.Logger;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * Base class for Scheme REPLs
 */
public abstract class GenericREPL extends GenericTool
{
   // constants

   /**
    * Exit code if an error occurred while logging in.
    */
   public final static int EXIT_LOGIN = 11;

   // attributes

   /**
    * True to suppress prompts and result printing.
    */
   private boolean m_bQuiet;

   /**
    * The maximum result print length.
    */
   protected long m_lMaxCount;

   /**
    * The user name to use
    */
   protected String m_sUser;

   // associations

   /**
    * The standard input reader.
    */
   protected Reader m_reader; 

   /**
    * The standard output writer.
    */
   protected Writer m_writer;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(GenericREPL.class);

   // operations

   /**
    * The parse to use to parse input
    * 
    * @return The parser to use
    */
   protected abstract Parser getParser();

   /**
    * Evaluate a parsed expression
    * 
    * @param expr Expression to evaluate
    * @return The result of the evaluation
    */
   protected abstract Object evaluate(Object expr);

   /**
    * Write the given object to the given writer
    * 
    * @param writer Writer to write the object to
    * @param obj Object to write
    * @throws IOException
    */
   protected abstract void write(Writer writer, Object obj) throws IOException;

   /**
    * Get a message from the given throwable
    * 
    * @param t Throwable to get message for
    * @return Localized formatted message
    */
   protected abstract String getMessage(Throwable t);

   /**
    * @see nexj.core.tools.GenericTool#begin()
    */
   protected void begin() throws Exception
   {
      super.begin();

      m_sUser = getProperty("app.user");
      m_bQuiet = StringUtil.parseBoolean(getProperty("scm.quiet", "false"));
      m_lMaxCount = Long.parseLong(getProperty("scm.limit", "0"));
      m_reader = new BufferedReader(new InputStreamReader(System.in, IOUtil.ENCODING));
      m_writer = new FlushWriter(new OutputStreamWriter(System.out, IOUtil.ENCODING));
   }

   /**
    * @see nexj.core.tools.GenericTool#execute(java.lang.String)
    */
   protected final void execute(String sCommand) throws Exception
   {
      if (sCommand != null && sCommand.equals("-"))
      {
         sCommand = null;
      }

      Reader reader = (sCommand != null) ? new StringReader(sCommand) : m_reader;
      Parser parser = getParser();
      String sStartupMessage;

      setReader(reader);

      try
      {
         sStartupMessage = startup();
      }
      catch (nexj.core.tools.GenericREPL.LoginException t)
      {
         Throwable cause = t.getCause();

         setExitCode(EXIT_LOGIN);

         if (!m_bQuiet)
         {
            m_writer.write("; Login error: " + cause.getMessage());
            m_writer.write(SysUtil.LINE_SEP);
            m_writer.flush();
         }

         s_logger.info("Login error", cause);

         return;
      }

      if (sCommand == null && !m_bQuiet)
      {
         m_writer.write("; NexJ Scheme");
         m_writer.write(SysUtil.LINE_SEP);

         if (sStartupMessage != null)
         {
            m_writer.write("; " + sStartupMessage);
            m_writer.write(SysUtil.LINE_SEP);
         }

         m_writer.flush();
      }

      for (;;)
      {
         if (sCommand == null && !m_bQuiet)
         {
            m_writer.write(SysUtil.LINE_SEP);
            m_writer.write("> ");
            m_writer.flush();
         }

         try
         {
            Object expr = parser.parse(reader, null);

            if (expr == Parser.EOF)
            {
               break;
            }

            if (sCommand != null)
            {
               setReader(m_reader);
            }

            Object result;

            try
            {
               result = evaluate(expr);
            }
            finally
            {
               if (sCommand != null)
               {
                  setReader(reader);
               }
            }

            if (!m_bQuiet)
            {
               m_writer.write(SysUtil.LINE_SEP);
               m_writer.write("; ");
               write(m_writer, result);
               m_writer.write(SysUtil.LINE_SEP);
               m_writer.flush();
            }
         }
         catch (Throwable e)
         {
            s_logger.debug("Error", e);

            if (!m_bQuiet)
            {
               m_writer.write(SysUtil.LINE_SEP);
               m_writer.write(";Error: ");

               for (Throwable t = e; t != null; t = t.getCause())
               {
                  if (t != e)
                  {
                     m_writer.write(SysUtil.LINE_SEP);
                     m_writer.write(";Cause: ");
                  }

                  m_writer.write(getMessage(t));

                  StringWriter sw = new StringWriter();

                  t.printStackTrace(new PrintWriter(sw));

                  String s = sw.toString();
                  int i = s.indexOf(SysUtil.LINE_SEP);

                  if (i >= 0 && i < s.length())
                  {
                     m_writer.write(SysUtil.LINE_SEP);
                     m_writer.write(s.substring(i + SysUtil.LINE_SEP.length()));
                  }
               }

               m_writer.flush();
            }

            if (sCommand != null)
            {
               System.exit(1);
            }
         }
      }
   }

   /**
    * Set the reader we are using. Optionally override to handle this.
    * 
    * @param reader Reader we are using to parse user input
    */
   protected void setReader(Reader reader)
   {
   }

   /**
    * Called after fully initialized. May return a single line message to print
    * before accepting input. By default returns null.
    * 
    * @return A startup message or null for none.
    */
   protected String startup()
   {
      return null;
   }

   /**
    * @see nexj.core.tools.GenericTool#getCommandUsage()
    */
   protected String[] getCommandUsage()
   {
      return new String[]
      {
         "<script> - executes the script",
         "- - interactive mode"
      };
   }

   /**
    * @see nexj.core.tools.GenericTool#getOptionUsage()
    */
   protected String[] getOptionUsage()
   {
      return new String[]
      {
         "-Dapp.user=<user name>",
         "-Dscm.quiet=<true/false>",
         "-Dscm.limit=<#max result chars>"
      };
   }

   /**
    * @see nexj.core.tools.GenericTool#isCommandRequired()
    */
   protected boolean isCommandRequired()
   {
      return false;
   }

   /**
    * Notify the user of a change in status
    * 
    * @param sMessage Status message to be printed
    */
   protected final void status(String sMessage)
   {
      if (m_bQuiet)
      {
         return;
      }

      try
      {
         m_writer.write("; " + sMessage);
         m_writer.write(SysUtil.LINE_SEP);
         m_writer.flush();
      }
      catch (IOException e)
      {
         s_logger.warn("Exception writing to output", e);
      }

   }

   // inner classes

   private static class FlushWriter extends FilterWriter
   {
      protected FlushWriter(Writer out)
      {
         super(out);
      }

      /**
       * @see java.io.Writer#write(char[], int, int)
       */
      public void write(char[] cbuf, int off, int len) throws IOException
      {
         super.write(cbuf, off, len);
         flush();
      }

      /**
       * @see java.io.Writer#write(int)
       */
      public void write(int c) throws IOException
      {
         super.write(c);
         flush();
      }

      /**
       * @see java.io.Writer#write(java.lang.String, int, int)
       */
      public void write(String str, int off, int len) throws IOException
      {
         super.write(str, off, len);
         flush();
      }
   }

   /**
    * An exception indicating an error occurred while logging in
    */
   protected class LoginException extends GenericException
   {
      private static final long serialVersionUID = 1480781571552138131L;

      public LoginException(Throwable cause)
      {
         super("err.repl.login", null, cause);
      }
   }
}
