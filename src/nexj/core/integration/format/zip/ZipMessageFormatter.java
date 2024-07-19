// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.zip;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.Output;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.format.zip.ZipMessagePartMapping;
import nexj.core.rpc.TransferObject;
import nexj.core.util.IOUtil;
import nexj.core.util.SysUtil;

/**
 * Formats a graph of TransferObjects into a Zip stream.
 */
public class ZipMessageFormatter implements MessageFormatter
{
   /**
    * @see nexj.core.integration.MessageFormatter#format(nexj.core.rpc.TransferObject, nexj.core.meta.integration.Message, nexj.core.integration.Output)
    */
   public void format(TransferObject tobj, Message message, Output out) throws IntegrationException
   {
      OutputStream ostream = out.getOutputStream();
      ZipOutputStream zipStream = null;
      CompositeMessagePart rootPart = message.getRoot();
      ZipMessagePartMapping rootMapping = (ZipMessagePartMapping)rootPart.getMapping();
      CompositeMessagePart entriesPart = (CompositeMessagePart)rootPart.getPart(0);

      // Pre-compute the transfer object keys for the various Zip entry attributes
      String sKeyComment = null;
      String sKeyContents = null;
      String sKeyExtra = null;
      String sKeyName = null;
      String sKeySize = null;
      String sKeyTime = null;

      for (int i = 0; i < entriesPart.getPartCount(); i++)
      {
         PrimitiveMessagePart part = (PrimitiveMessagePart)entriesPart.getPart(i);
         ZipMessagePartMapping mapping = (ZipMessagePartMapping)part.getMapping();

         switch (mapping.getValue())
         {
            case ZipMessagePartMapping.VALUE_COMMENT:
               sKeyComment = part.getName();
               break;

            case ZipMessagePartMapping.VALUE_CONTENTS:
               sKeyContents = part.getName();
               break;

            case ZipMessagePartMapping.VALUE_EXTRA:
               sKeyExtra = part.getName();
               break;

            case ZipMessagePartMapping.VALUE_NAME:
               sKeyName = part.getName();
               break;

            case ZipMessagePartMapping.VALUE_SIZE:
               sKeySize = part.getName();
               break;

            case ZipMessagePartMapping.VALUE_TIME:
               sKeyTime = part.getName();
               break;

            default:
               // Ignore values that are not for output
               break;
         }
      }

      // Format the transfer object into a Zip stream
      try
      {
         zipStream = new ZipOutputStream(ostream);

         if (rootMapping != null)
         {
            zipStream.setComment(rootMapping.getComment());
            zipStream.setLevel((int)rootMapping.getCompression());
         }

         List entryList = (List)tobj.getValue(entriesPart.getName());

         for (int i = 0; i < entryList.size(); i++)
         {
            TransferObject entry = (TransferObject)entryList.get(i);

            if (!entry.hasValue(sKeyName))
            {
               continue;
            }

            ZipEntry zipEntry = new ZipEntry(convertPlatformSeparatorToZipSeparator(Primitive.toString(entry.getValue(sKeyName))));

            if (sKeyComment != null && entry.hasValue(sKeyComment))
            {
               zipEntry.setComment(Primitive.toString(entry.getValue(sKeyComment)));
            }

            if (sKeyExtra != null && entry.hasValue(sKeyExtra))
            {
               zipEntry.setExtra(Primitive.toBinary(entry.getValue(sKeyExtra)).getData());
            }

            if (sKeySize != null && entry.hasValue(sKeySize))
            {
               zipEntry.setSize(Primitive.toLong(entry.getValue(sKeySize)).longValue());
            }

            if (sKeyTime != null && entry.hasValue(sKeyTime))
            {
               zipEntry.setTime(Primitive.toLong(entry.getValue(sKeyTime)).longValue());
            }

            zipStream.putNextEntry(zipEntry);

            // Compress the file contents
            if (entry.hasValue(sKeyContents))
            {
               InputStream contentsStream = null;

               try
               {
                  contentsStream = Primitive.toBinary(entry.getValue(sKeyContents)).getInputStream();
                  IOUtil.copy(zipStream, contentsStream);
                  zipStream.closeEntry();
               }
               finally
               {
                  contentsStream.close();
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
         try
         {
            if (zipStream != null)
            {
               zipStream.finish();
            }
         }
         catch (IOException ex2)
         {
            throw new IntegrationException("err.integration.io", ex2);
         }
      }
   }

   /**
    * Converts a path using the current platform's file separator to
    * one using the the separator used internally by the Zip format.
    * 
    * @param sName The file path and name in the current platform's format.
    * @return The file path and name in Zip format.
    */
   public static String convertPlatformSeparatorToZipSeparator(String sName)
   {
      return sName.replace(SysUtil.FILE_SEP, "/");
   }
}
