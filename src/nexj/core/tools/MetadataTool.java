// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.tools;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataLoaderDispatcher;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.util.IOUtil;
import nexj.core.util.Logger;
import nexj.core.util.SysUtil;
import nexj.core.util.URLUtil;
import nexj.core.util.XMLUtil;

/**
 * Tool for manipulating metadata descriptors.
 */
public class MetadataTool extends GenericTool
{
   /**
    * The logger.
    */
   private final static Logger s_logger = Logger.getLogger(MetadataTool.class);

   /**
    * @see nexj.core.tools.GenericTool#execute(java.lang.String)
    */
   protected void execute(String sCommand) throws Exception
   {
      if (sCommand.equals("merge"))
      {
         URL rootURL = XMLMetadataHelper.getURL(URLUtil.toURL(getRequiredProperty("root.url")), true);
         URL baseURL = null;

         String sBaseURL = getProperty("base.url");

         if (sBaseURL != null)
         {
            baseURL = XMLMetadataHelper.getURL(URLUtil.toURL(sBaseURL), true);
         }

         String sOutFile = getRequiredProperty("out.file");

         XMLMetadataHelper helper = new XMLMetadataHelper(rootURL, baseURL, null, null);

         List rootList = null;
         String sRootListFile = getProperty("root.list");

         if (sRootListFile != null)
         {
            rootList = new ArrayList();
         }

         List baseList = null;
         String sBaseListFile = getProperty("base.list");

         if (sBaseListFile != null)
         {
            baseList = new ArrayList();
         }

         String sXML = XMLUtil.formatXML(
            helper.mergeDescriptorElements(baseList, rootList));

         Writer writer = null;

         try
         {
            writer = IOUtil.openBufferedWriter(new File(sOutFile), XMLUtil.ENCODING);
            writer.write(sXML);
            writer.close();
            writer = null;

            if (sBaseListFile != null)
            {
               writer = IOUtil.openBufferedWriter(new File(sBaseListFile), XMLUtil.ENCODING);
               write(writer, baseList);
               writer.close();
               writer = null;
            }

            if (sRootListFile != null)
            {
               writer = IOUtil.openBufferedWriter(new File(sRootListFile), XMLUtil.ENCODING);
               write(writer, rootList);
               writer.close();
               writer = null;
            }
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
      else if (sCommand.equals("sort"))
      {
         URL rootURL = XMLMetadataHelper.getURL(URLUtil.toURL(getRequiredProperty("root.url")), true);
         URL baseURL = null;

         String sBaseURL = getProperty("base.url");

         if (sBaseURL != null)
         {
            baseURL = XMLMetadataHelper.getURL(URLUtil.toURL(sBaseURL), true);
         }

         String sOutFile = getRequiredProperty("out.file");
         XMLMetadataHelper helper = new XMLMetadataHelper(rootURL, baseURL, null, null);
         List jarList = (List)helper.addMixinsTo(new ArrayList());
         Writer writer = null;

         Collections.reverse(jarList);

         try
         {
            writer = IOUtil.openBufferedWriter(new File(sOutFile), XMLUtil.ENCODING);

            for (int i = 0; i < jarList.size(); i++)
            {
               writer.write((String)jarList.get(i));
               writer.write('\n');
            }

            writer.close();
            writer = null;
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
      else if (sCommand.equals("validate"))
      {
         Properties properties = new Properties(SysUtil.getConfigProperties());
         String sBase = getProperty("base.url");

         if (sBase == null)
         {
            sBase = "";
         }

         properties.setProperty(XMLMetadataLoader.BASE_URL_PROPERTY, sBase);

         Metadata metadata = new MetadataLoaderDispatcher().load(getRequiredProperty("root.url"),
            properties, XMLMetadataLoader.VALIDATED_ONLY, null);

         s_logger.info("Version: " + metadata.getVersion());
         s_logger.info("Revision: " + metadata.getRevision());
         s_logger.info("Checksum: " + metadata.getChecksum());
         s_logger.info("Name: " + metadata.getName());
         s_logger.info("Namespace: " + metadata.getNamespace());

         s_logger.info("");
         s_logger.info("Base");
         s_logger.info("Version: " + metadata.getBaseVersion());
         s_logger.info("Checksum: " + metadata.getBaseChecksum());
         s_logger.info("Namespace: " + metadata.getBaseNamespace());

         s_logger.debug("");
         s_logger.debug("Statistics");
         s_logger.debug("Channels: " + metadata.getChannelCount());
         s_logger.debug("Channel Types: " + metadata.getChannelTypeCount());
         s_logger.debug("Class Aspects: " + metadata.getClassAspectCount());
         s_logger.debug("Data Sources: " + metadata.getDataSourceCount());
         s_logger.debug("Flow Macros: " + metadata.getFlowMacroCount());
         s_logger.debug("Formats: " + metadata.getFormatCount());
         s_logger.debug("Interfaces: " + metadata.getInterfaceCount());
         s_logger.debug("Locales: " + metadata.getLocaleCount());
         s_logger.debug("Messages: " + metadata.getMessageCount());
         s_logger.debug("Primitive Privileges: " + metadata.getPrimitivePrivilegeCount());
         s_logger.debug("Transformations: " + metadata.getTransformationCount());
      }
      else
      {
         throw new IllegalArgumentException("Invalid command \"" + sCommand + "\"");
      }
   }

   /**
    * Writes a collection to a character stream.
    * @param writer The character stream writer.
    * @param col The collection to write.
    */
   protected static void write(Writer writer, Collection col) throws IOException
   {
      for (Iterator itr = col.iterator(); itr.hasNext();)
      {
         writer.write(String.valueOf(itr.next()));
         writer.write(SysUtil.LINE_SEP);
      }
   }

   /**
    * @see nexj.core.tools.GenericTool#getOptionUsage()
    */
   protected String[] getOptionUsage()
   {
      return new String[]
      {
         "-Dout.file=<output file>",
         "-Droot.list=<root output list file>",
         "-Dbase.list=<base output list file>",
         "-Droot.url=<root repository URL>",
         "-Dbase.url=<base repository URL>",
      };
   }

   /**
    * @see nexj.core.tools.GenericTool#getCommandUsage()
    */
   protected String[] getCommandUsage()
   {
      return new String[]
      {
         "merge - merges the metadata descriptors",
         "sort - generates topological ordering of repository jars, base and root urls must be jar entries"
      };
   }

   public static void main(String[] args)
   {
      new MetadataTool().run(args);
   }
}
