// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;

import nexj.core.runtime.InvocationContext;
import nexj.core.util.GUIDUtil;
import nexj.core.util.IOUtil;
import nexj.core.util.SubstReader;
import nexj.core.util.SysUtil;

/**
 * Takes a template file name and fills in the appropriate values.
 * 
 * Template substitutions include:
 *    ${orig} - The original file path, if any.
 *    ${origName} - The original file name, if any.
 *    ${ts}   - A time stamp generated by currentTimeMillis
 *    ${seq}  - A globally-unique sequence number, created by SysCounter
 *    ${guid} - A GUID
 */
public class FileNameExpander extends SubstReader
{
   // attributes

   /**
    * The original file name, needed to fill out ${orig} expansion.
    */
   protected String m_sOriginalName;


   // associations

   /**
    * The invocation context to use for accessing SysCounter.
    */
   protected InvocationContext m_context;


   // constructors

   /**
    * Creates a new instance.
    * 
    * @param reader        The reader that holds the template.
    * @param context       The InvocationContext to use for accessing SysCounter.
    * @param sOriginalName The original file name and relative path from the
    *                      incoming directory (for filling out ${orig} expansion).
    */
   public FileNameExpander(Reader reader, InvocationContext context, String sOriginalName)
   {
      super(reader);
      
      m_context = context;
      m_sOriginalName = (sOriginalName != null) ? sOriginalName : "";
   }


   // operations

   /**
    * @see nexj.core.util.SubstReader#getValue(java.lang.String)
    */
   protected String getValue(String sName) throws IOException
   {
      if ("ts".equals(sName))
      {
         return String.valueOf(System.currentTimeMillis());
      }
      else if ("seq".equals(sName))
      {
         if (m_context != null)
         {
            Object result = m_context.getMetadata().getMetaclass("SysCounter").invoke("next", new Object[]{"file.seq"});
         
            return result.toString();
         }
         else
         {
            return "";
         }
      }
      else if ("guid".equals(sName))
      {
         return GUIDUtil.generateGUID().toString();
      }
      else if ("orig".equals(sName))
      {
         return m_sOriginalName;
      }
      else if("origName".equals(sName))
      {
         return m_sOriginalName.substring(m_sOriginalName.lastIndexOf(SysUtil.FILE_SEP) + 1);
      }

      return null;
   }


   /**
    * Helper method to perform template string expansion of a file name.
    * 
    * @param sTemplate     The template string.
    * @param context       InvocationContext for accessing SysCounter.
    * @param sOriginalName The original file name, or null if not applicable.
    * @return The expanded file name.
    */
   public static String expandString(String sTemplate, InvocationContext context, String sOriginalName)
   {
      Reader reader = new StringReader(sTemplate);
      FileNameExpander expander = new FileNameExpander(reader, context, sOriginalName);
      StringWriter writer = new StringWriter(sTemplate.length());
      
      try
      {
         IOUtil.copy(writer, expander);
         reader.close();
      }
      catch (IOException ex)
      {
         return null;
      }
      
      return writer.toString();
   }
}