// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.zip;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import nexj.core.integration.Input;
import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageParser;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessageTable;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.format.zip.ZipMessagePartMapping;
import nexj.core.rpc.TransferObject;
import nexj.core.util.Binary;
import nexj.core.util.IOUtil;
import nexj.core.util.Logger;
import nexj.core.util.PagedBinary;
import nexj.core.util.SysUtil;

/**
 * Parses a Zip stream into a TransferObject graph.
 */
public class ZipMessageParser implements MessageParser
{
   // associations

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(ZipMessageParser.class);


   // operations

   /**
    * There is little point in having more than one Zip-format message in
    * the metadata, and no way of distinguishing the messages. Therefore,
    * only message tables with a single Zip-format message are accepted.
    * 
    * @see nexj.core.integration.MessageParser#initializeMessageTable(nexj.core.meta.integration.MessageTable)
    */
   public void initializeMessageTable(MessageTable table) throws IntegrationException
   {
      if (table.getMessageCount() != 1)
      {
         throw new IntegrationException("err.integration.zip.messageTableConflict");
      }

      table.setParserTable(table.getMessage(0));
   }

   /**
    * @see nexj.core.integration.MessageParser#parse(nexj.core.integration.Input, nexj.core.meta.integration.Message)
    */
   public TransferObject parse(Input in, Message msg) throws IntegrationException
   {
      InputStream istream = in.getInputStream();

      CompositeMessagePart rootPart = msg.getRoot();
      CompositeMessagePart entriesPart = (CompositeMessagePart)rootPart.getPart(0);

      TransferObject root = new TransferObject(msg.getName());
      List entryList = new ArrayList();

      ZipInputStream zipStream = new ZipInputStream(istream);
      ZipEntry zipEntry;

      root.setValue(entriesPart.getName(), entryList);

      try
      {
         while ((zipEntry = zipStream.getNextEntry()) != null)
         {
            String sKeySize = null;
            long lValueSize = 0;
            String sKeyContents = null;
            Primitive sizeType = null;
            TransferObject entry = new TransferObject();

            entryList.add(entry);

            // Iterate the parts, assigning data from the Zip entry
            for (int i = 0; i < entriesPart.getPartCount(); i++)
            {
               PrimitiveMessagePart part = (PrimitiveMessagePart)entriesPart.getPart(i);
               ZipMessagePartMapping mapping = (ZipMessagePartMapping)part.getMapping();
               Object datum = null;
               Primitive partType = part.getType();

               switch (mapping.getValue())
               {
                  case ZipMessagePartMapping.VALUE_CONTENTS:
                     Binary contents = new PagedBinary(zipStream);

                     zipStream.closeEntry();

                     if (s_logger.isDebugEnabled())
                     {
                        s_logger.debug("Extracted contents of \"" + zipEntry.getName() + "\" size=" + contents.getSize());
                     }

                     if (contents.getSize() > 0)
                     {
                        datum = partType.getConverter(Primitive.BINARY).invoke(contents);
                     }

                     sKeyContents = part.getName();
                     break;

                  case ZipMessagePartMapping.VALUE_COMMENT:
                     datum = partType.getConverter(Primitive.STRING).invoke(zipEntry.getComment());
                     break;

                  case ZipMessagePartMapping.VALUE_DIRECTORY:
                     datum = partType.getConverter(Primitive.BOOLEAN).invoke(Boolean.valueOf(zipEntry.isDirectory()));
                     break;

                  case ZipMessagePartMapping.VALUE_EXTRA:
                     if (zipEntry.getExtra() != null)
                     {
                        datum = partType.getConverter(Primitive.BINARY).invoke(new Binary(zipEntry.getExtra()));
                     }

                     break;

                  case ZipMessagePartMapping.VALUE_NAME:
                     datum = partType.getConverter(Primitive.STRING).invoke(convertZipSeparatorToPlatformSeparator(zipEntry.getName()));
                     break;

                  case ZipMessagePartMapping.VALUE_SIZE:
                     datum = partType.getConverter(Primitive.LONG).invoke(Primitive.createLong(zipEntry.getSize()));
                     sKeySize = part.getName();
                     lValueSize = zipEntry.getSize();
                     sizeType = partType;
                     break;

                  case ZipMessagePartMapping.VALUE_TIME:
                     datum = partType.getConverter(Primitive.LONG).invoke(Primitive.createLong(zipEntry.getTime()));
                     break;

                  default:
                     throw new IllegalStateException("Unknown value: " + mapping.getValue());
               }

               entry.setValue(part.getName(), datum);
            }

            // Process the size entry
            if (sKeySize != null && sKeyContents != null)
            {
               Binary contents = (Binary)entry.getValue(sKeyContents);
               long lContentsSize = (contents != null) ? contents.getSize() : 0;

               if (lValueSize == -1)
               {
                  // Populate the size entry from the Binary's size
                  entry.setValue(sKeySize, sizeType.getConverter(Primitive.LONG).invoke(Primitive.createLong(lContentsSize)));
               }
               else
               {
                  // Ensure size entry matches Binary's size
                  if (lValueSize != lContentsSize)
                  {
                     throw new IllegalStateException("Size from Zip entry header should match data size");
                  }
               }
            }
         }
      }
      catch (IOException ex)
      {
         throw new IntegrationException("err.integration.io", ex);
      }
      finally
      {
         IOUtil.close(zipStream);
         IOUtil.close(istream);
      }

      return root;
   }

   /**
    * There is little point in having more than one Zip-format message in
    * the metadata, and no way of distinguishing the messages. Therefore,
    * only message tables with a single Zip-format message are accepted.
    * 
    * @see nexj.core.integration.MessageParser#parse(nexj.core.integration.Input, nexj.core.meta.integration.MessageTable)
    */
   public TransferObject parse(Input in, MessageTable table) throws IntegrationException
   {
      if (table == null || !(table.getParserTable() instanceof Message))
      {
         throw new IntegrationException("err.integration.messageTableUninitialized");
      }

      return parse(in, (Message)table.getParserTable());
   }

   /**
    * Converts a path using the file separator used internally by the Zip format
    * to the current platform's separator.
    * 
    * @param sName The file path and name in Zip format.
    * @return The file path and name in the current platform's format.
    */
   public static String convertZipSeparatorToPlatformSeparator(String sName)
   {
      return sName.replace("/", SysUtil.FILE_SEP);
   }
}
