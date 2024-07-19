// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.hl7;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.TimeZone;

import nexj.core.integration.Input;
import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageParser;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessageTable;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.format.hl7.HL7MessagePartMapping;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.HL7Util;
import nexj.core.util.HashTab;
import nexj.core.util.HashTab2D;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.Undefined;

/**
 * HL7 message parser.
 */
public class HL7MessageParser implements MessageParser, InvocationContextAware
{
   // constants

   /**
    * The repeat separator index.
    */
   protected final static int SEP_SEGMENT = 0;

   /**
    * The field separator index.
    */
   protected final static int SEP_FIELD = 1;

   /**
    * The component separator index.
    */
   protected final static int SEP_COMPONENT = 2;

   /**
    * The subcomponent separator index.
    */
   protected final static int SEP_SUBCOMPONENT = 3;

   /**
    * The repeat separator index.
    */
   protected final static int SEP_REPEAT = 4;

   /**
    * The escape separator index.
    */
   protected final static int SEP_ESCAPE = 5;

   /**
    * The EOF character.
    */
   protected final static int CHAR_EOF = -1;

   /**
    * Maximum MSH header length.
    */
   protected final static int MAX_MSH = 1024; 

   // attributes

   /**
    * The current separator level.
    */
   protected int m_nSepLevel;

   /**
    * True if a repeat separator is encountered.
    */
   protected boolean m_bRepeat;

   /**
    * The current token. Null if none.
    */
   protected String m_sToken;

   /**
    * The default time zone.
    */
   protected TimeZone m_defTZ;

   // associations

   /**
    * The work string buffer.
    */
   protected StringBuffer m_buf;

   /**
    * The string reader for parsing.
    */
   protected Reader m_reader;

   /**
    * Separator character array.
    */
   protected char[] m_sepArray;

   /**
    * The message header part. Can be null.
    */
   protected CompositeMessagePart m_msh;

   /**
    * Lazily created transfer object.
    */
   protected TransferObject m_tobj;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(HL7MessageParser.class);

   // operations

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * @see nexj.core.integration.MessageParser#parse(nexj.core.integration.Input, nexj.core.meta.integration.Message)
    */
   public TransferObject parse(Input in, Message message) throws IntegrationException
   {
      try
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Parsing HL7 message \"" + message.getName() + "\"");
            s_logger.dump(in);
         }

         initParser(in);
         m_msh = HL7MessagePartMapping.findMSH(message);

         if (m_msh != null)
         {
            mark(MAX_MSH);
         }

         parseMSH(null);

         if (m_msh != null)
         {
            resetReader();
            nextToken();
         }

         TransferObject tobj = new TransferObject(message.getName());

         parse(message.getRoot(), null, null, tobj);

         return tobj;
      }
      catch (IntegrationException e)
      {
         throw e;
      }
      catch (Exception e)
      {
         throw new IntegrationException("err.integration.hl7.messageSyntax",
            new Object[]{message.getName()}, e);
      }
   }

   /**
    * @see nexj.core.integration.MessageParser#parse(nexj.core.integration.Input, nexj.core.meta.integration.MessageTable)
    */
   public TransferObject parse(Input in, MessageTable table) throws IntegrationException
   {
      MSH msh = new MSH();

      try
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Identifying and parsing an HL7 message");
            s_logger.dump(in);
         }

         initParser(in);
         mark(MAX_MSH);
         parseMSH(msh);
      }
      catch (Exception e)
      {
         throw new IntegrationException("err.integration.hl7.unparseableMessage", e);
      }

      Lookup parserTable = (Lookup)table.getParserTable();
      Lookup2D map = (Lookup2D)parserTable.get(msh.version);

      if (map == null)
      {
         map = (Lookup2D)parserTable.get(Undefined.VALUE);
      }

      if (map == null)
      {
         throw new IntegrationException("err.integration.hl7.unsupportedVersion", new Object[]{msh.version});
      }

      Message message = (Message)map.get(msh.type, msh.event);

      if (message == null)
      {
         String s = msh.structure;

         if (s.length() != 0)
         {
            int i = s.indexOf('_');

            message = (Message)((i < 0) ? map.get(s, "") : map.get(s.substring(0, i), s.substring(i + 1)));
         }

         if (message == null)
         {
            if (msh.event.length() != 0)
            {
               message = (Message)map.get(msh.type, "");
            }

            if (message == null)
            {
               throw new IntegrationException("err.integration.hl7.unsupportedMessage", new Object[]{msh.type,
                     msh.event, msh.version});
            }
         }
      }

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Identified the HL7 message as \"" + message.getName() + "\"");
      }

      m_msh = HL7MessagePartMapping.findMSH(message);

      if (m_msh != null)
      {
         resetReader();
         nextToken();
      }

      TransferObject tobj = new TransferObject(message.getName());

      try
      {
         parse(message.getRoot(), null, null, tobj);
      }
      catch (IntegrationException e)
      {
         throw e;
      }
      catch (Exception e)
      {
         throw new IntegrationException("err.integration.hl7.messageSyntax",
            new Object[]{message.getName()}, e);
      }

      return tobj;
   }

   /**
    * @see nexj.core.integration.MessageParser#initializeMessageTable(nexj.core.meta.integration.MessageTable)
    */
   public void initializeMessageTable(MessageTable table) throws IntegrationException
   {
      Lookup map = new HashTab(4);

      for (Iterator itr = table.getMessageIterator(); itr.hasNext();)
      {
         Message message = (Message)itr.next();
         HL7MessagePartMapping mapping = (HL7MessagePartMapping)message.getRoot().getMapping();

         if (mapping.getLevel() != HL7MessagePartMapping.LEVEL_MESSAGE)
         {
            throw new MetadataException("err.meta.integration.hl7.messageLevel",
               new Object[]{message.getName()});
         }

         String sName = mapping.getName();
         String sVersion = mapping.getVersion();

         if (sName == null)
         {
            throw new MetadataException("err.meta.integration.hl7.missingMessageType",
               new Object[]{message.getName()});
         }

         String sType;
         String sEvent;
         int i = sName.indexOf('^');

         if (i < 0)
         {
            sType = sName;
            sEvent = "";
         }
         else
         {
            sType = sName.substring(0, i);

            int k = sName.indexOf('^', i + 1);

            if (k < 0)
            {
               sEvent = sName.substring(i + 1);
            }
            else
            {
               sEvent = sName.substring(i + 1, k);
            }
         }

         Object key = (sVersion == null) ? (Object)Undefined.VALUE : sVersion;
         Lookup2D map2D = (Lookup2D)map.get(key);

         if (map2D == null)
         {
            map2D = new HashTab2D();

            map.put(key, map2D);
         }

         Message old = (Message)map2D.put(sType, sEvent, message);

         if (old != null)
         {
            throw new MetadataException("err.meta.integration.hl7.dupMessageType",
               new Object[]{sType, sEvent, old.getName(), message.getName()});
         }
      }

      table.setParserTable(map);
   }

   /**
    * Initializes the parser form a given message input.
    * @param in The message input.
    */
   protected void initParser(Input in)
   {
      m_reader = in.getReader();
      m_buf = new StringBuffer();
   }

   /**
    * @see Reader#mark(int)
    */
   protected void mark(int nCount)
   {
      try
      {
         m_reader.mark(nCount);
      }
      catch (IOException e)
      {
         throw new IntegrationException("err.integration.io",
            new Object[]{ObjUtil.getMessage(e)}, e);
      }
   }

   /**
    * @see Reader#reset()
    */
   protected void resetReader()
   {
      try
      {
         m_reader.reset();
      }
      catch (IOException e)
      {
         throw new IntegrationException("err.integration.io",
            new Object[]{ObjUtil.getMessage(e)}, e);
      }
   }

   /**
    * @return The next character from the current reader.
    */
   protected int getNextChar()
   {
      try
      {
         return m_reader.read();
      }
      catch (IOException e)
      {
         throw new IntegrationException("err.integration.io",
            new Object[]{ObjUtil.getMessage(e)}, e);
      }
   }

   /**
    * Resets the buffer and the current token.
    */
   protected void resetBuf()
   {
      m_buf.setLength(0);
      m_sToken = null;
   }

   /**
    * @return The current token, right-trimmed.
    */
   protected String getCurToken()
   {
      if (m_sToken == null)
      {
         m_sToken = (m_buf.length() == 0) ? "" : m_buf.substring(0);
      }

      return m_sToken;
   }

   /**
    * Parses out the next token and sets the separator level and repeat flag.
    */
   protected void nextToken()
   {
      resetBuf();

      for (;;)
      {
         int ch = getNextChar();

         if (ch == CHAR_EOF || ch == 0x1C)
         {
            m_nSepLevel = HL7MessagePartMapping.LEVEL_MESSAGE;
            m_bRepeat = false;

            break;
         }

         int i;

         for (i = m_sepArray.length - 1; i >= 0; --i)
         {
            if (ch == m_sepArray[i])
            {
               break;
            }
         }

         if (i >= 0)
         {
            if (i == SEP_ESCAPE)
            {
               ch = getNextChar();

               switch (ch)
               {
                  case 'F':
                     m_buf.append(m_sepArray[SEP_FIELD]);
                     ch = getNextChar();
                     break;

                  case 'S':
                     m_buf.append(m_sepArray[SEP_COMPONENT]);
                     ch = getNextChar();
                     break;

                  case 'T':
                     m_buf.append(m_sepArray[SEP_SUBCOMPONENT]);
                     ch = getNextChar();
                     break;

                  case 'R':
                     m_buf.append(m_sepArray[SEP_FIELD]);
                     ch = getNextChar();
                     break;

                  case 'E':
                     m_buf.append(m_sepArray[SEP_ESCAPE]);
                     ch = getNextChar();
                     break;

                  case 'X':
                     while ((ch = getNextChar()) != m_sepArray[SEP_ESCAPE])
                     {
                        m_buf.append((char)((StringUtil.parseDigit((char)ch, 16) << 4) |
                           StringUtil.parseDigit((char)getNextChar(), 16)));
                     }

                     break;

                  case 'H':
                  case 'N':
                     // Ignore the highlight
                     ch = getNextChar();
                     break;

                  case 'C':
                  case 'M':
                  case 'Z':
                     // TODO: Process HL7 character encoding sequences

                     // For now, ignore the escape sequence
                     while ((ch = getNextChar()) != -1)
                     {
                        if (ch == m_sepArray[SEP_ESCAPE])
                        {
                           break;
                        }
                     }

                     break;

                  default:
                     throw new IntegrationException("err.integration.hl7.escapeChar",
                        new Object[]{Primitive.createCharacter(ch)});
               }

               if (ch != m_sepArray[SEP_ESCAPE])
               {
                  throw new IntegrationException("err.integration.hl7.unterminatedEscape");
               }
            }
            else if (i == SEP_REPEAT)
            {
               m_nSepLevel = HL7MessagePartMapping.LEVEL_FIELD;
               m_bRepeat = true;

               break;
            }
            else
            {
               m_nSepLevel = HL7MessagePartMapping.LEVEL_SEGMENT + i;
               m_bRepeat = false;

               break;
            }
         }
         else
         {
            m_buf.append((char)ch);
         }
      }
   }

   /**
    * Skips a specified number of tokens, including the current one.
    * @param nLevel The separator level.
    * @param nCount The number of tokens to skip.
    */
   protected void skipTokens(int nLevel, int nCount)
   {
      while (nCount > 0)
      {
         if (nLevel > m_nSepLevel)
         {
            resetBuf();
            break;
         }

         if (nLevel == m_nSepLevel)
         {
            --nCount;
         }

         nextToken();
      }
   }

   /**
    * Parses a message header.
    * @param msh The header structure modified by this method. Can be null.
    */
   protected void parseMSH(MSH msh)
   {
      if (getNextChar() != 'M' || getNextChar() != 'S' || getNextChar() != 'H')
      {
         throw new IntegrationException("err.integration.hl7.msh");
      }

      m_sepArray = new char[6];
      m_sepArray[SEP_SEGMENT] = '\r';
      m_sepArray[SEP_FIELD] = (char)getNextChar();
      m_sepArray[SEP_COMPONENT] = (char)getNextChar();
      m_sepArray[SEP_REPEAT] = (char)getNextChar();

      int ch = getNextChar();

      if (ch != m_sepArray[SEP_FIELD])
      {
         m_sepArray[SEP_ESCAPE] = (char)ch;
         ch = getNextChar();
      }
      else
      {
         m_sepArray[SEP_ESCAPE] = '\\';
      }

      if (ch != m_sepArray[SEP_FIELD])
      {
         m_sepArray[SEP_SUBCOMPONENT] = (char)ch;
         ch = getNextChar();
      }
      else
      {
         m_sepArray[SEP_SUBCOMPONENT] = '&';
      }

      if (ch != m_sepArray[SEP_FIELD])
      {
         throw new IntegrationException("err.integration.hl7.sepField");
      }

      for (int i = m_sepArray.length - 1; i >= 0; --i)
      {
         ch = m_sepArray[i];

         for (int j = i - 1; j >= 0; --j)
         {
            if (m_sepArray[j] == ch)
            {
               throw new IntegrationException("err.integration.hl7.dupSep",
                  new Object[]{Primitive.createCharacter(ch)});
            }
         }
      }

      nextToken();
      skipTokens(HL7MessagePartMapping.LEVEL_FIELD, 4);

      String s = getCurToken();

      try
      {
         m_defTZ = HL7Util.parseTimeZone(s);

         if (m_defTZ == null)
         {
            m_defTZ = m_context.getTimeZone();
         }
      }
      catch (Exception e)
      {
         throw new IntegrationException("err.integration.hl7.timeZone", new Object[]{s}, e);
      }

      if (msh != null)
      {
         skipTokens(HL7MessagePartMapping.LEVEL_FIELD, 2);
         msh.type = getCurToken();

         skipTokens(HL7MessagePartMapping.LEVEL_COMPONENT, 1);
         msh.event = getCurToken();

         skipTokens(HL7MessagePartMapping.LEVEL_COMPONENT, 1);
         msh.structure = getCurToken();

         skipTokens(HL7MessagePartMapping.LEVEL_FIELD, 3);
         msh.version = getCurToken();
     }

      skipTokens(HL7MessagePartMapping.LEVEL_SEGMENT, 1);
   }

   /**
    * Parses a message part from the current reader.
    * @param msgPart The message part metadata.
    * @param parentMsgPart The parent message part metadata.
    * @param grandParentMsgPart The grandparent message part metadata.
    * @param parent The parent transfer object. Null to instantiate one in m_tobj.
    * @return True if the part was found.
    */
   protected boolean parse(final MessagePart msgPart, final CompositeMessagePart parentMsgPart,
      final CompositeMessagePart grandParentMsgPart, TransferObject parent)
   {
      HL7MessagePartMapping mapping = (HL7MessagePartMapping)msgPart.getMapping();
      int nLevel = mapping.getLevel();
      List list = null;
      int nCount = 0;

      if (nLevel >= HL7MessagePartMapping.LEVEL_FIELD)
      {
         nLevel = Math.max(nLevel, ((HL7MessagePartMapping)parentMsgPart.getMapping()).getLevel() + 1);
      }

   loop:
      for (;;)
      {
         Object value = null;
         boolean bMatch = false;

         if (msgPart instanceof PrimitiveMessagePart)
         {
            if (nLevel < HL7MessagePartMapping.LEVEL_FIELD)
            {
               throw new IntegrationException("err.meta.integration.hl7.refLevel", new Object[]{msgPart.getFullPath()});
            }

            PrimitiveMessagePart part = (PrimitiveMessagePart)msgPart;
            String s = getCurToken();

            if (s.length() != 0)
            {
               if (s.equals("\"\""))
               {
                  value = null;
               }
               else
               {
                  value = convert(part, s);
               }

               bMatch = true;
            }
         }
         else
         {
            if (nLevel == HL7MessagePartMapping.LEVEL_SEGMENT)
            {
               String sName = getCurToken();

               if (!mapping.getName().equals(sName))
               {
                  if (parentMsgPart.isLax() && nCount == 0 && sName.length() != 0 && sName.charAt(0) == 'Z')
                  {
                     skipTokens(nLevel, 1);

                     continue;
                  }

                  break;
               }

               skipTokens(nLevel + 1, 1);
               bMatch = true;
            }

            final CompositeMessagePart composite = (CompositeMessagePart)msgPart;
            TransferObject tobj = (parentMsgPart == null) ? parent : null;
            int nSeq = 1;

            if (msgPart == m_msh)
            {
               m_buf.append(m_sepArray[SEP_COMPONENT]);

               for (;;)
               {
                  int ch = getNextChar();

                  if (ch == m_sepArray[SEP_FIELD])
                  {
                     break;
                  }

                  m_buf.append((char)ch);
               }

               m_nSepLevel = HL7MessagePartMapping.LEVEL_FIELD;
               nSeq = 2;
            }

            for (int nPart = 0, nPartCount = composite.getPartCount(); nPart != nPartCount; ++nPart)
            {
               MessagePart part = composite.getPart(nPart);

               if (nLevel > HL7MessagePartMapping.LEVEL_GROUP)
               {
                  int nNextSeq = ((HL7MessagePartMapping)part.getMapping()).getSeq();

                  if (msgPart == m_msh && nNextSeq <= 2)
                  {
                     if (nNextSeq == 1)
                     {
                        m_sToken = new String(m_sepArray, SEP_FIELD, 1);
                     }
                     else
                     {
                        m_sToken = m_buf.toString();
                     }

                     nNextSeq = 2;
                  }

                  skipTokens(nLevel + 1, nNextSeq - nSeq);
                  nSeq = nNextSeq;
               }

               if (parse(part, composite, parentMsgPart, tobj))
               {
                  bMatch = true;

                  if (tobj == null)
                  {
                     value = tobj = m_tobj;
                  }
               }
               else
               {
                  if (part.isRequired())
                  {
                     if (bMatch ||
                        nLevel == HL7MessagePartMapping.LEVEL_SEGMENT ||
                        nLevel > HL7MessagePartMapping.LEVEL_SEGMENT && m_nSepLevel > nLevel)
                     {
                        throw new IntegrationException("err.integration.minPartCount", new Object[]{part.getFullPath()});
                     }
                     else
                     {
                        break loop;
                     }
                  }
               }
            }
         }

         if (bMatch || m_bRepeat && nLevel == HL7MessagePartMapping.LEVEL_FIELD)
         {
            ++nCount;

            if (parentMsgPart != null)
            {
               if (parent == null)
               {
                  parent = new TransferObject();
               }

               m_tobj = parent;

               if (value == null)
               {
                  if (nLevel == HL7MessagePartMapping.LEVEL_SEGMENT)
                  {
                     value = new TransferObject(((CompositeMessagePart)msgPart).getPartCount());
                  }
                  else if (m_bRepeat && nLevel == HL7MessagePartMapping.LEVEL_FIELD)
                  {
                     value = new TransferObject();
                  }
               }

               if (msgPart.isCollection())
               {
                  if (list == null)
                  {
                     list = new ArrayList();
                     parent.setValue(msgPart.getName(), list);
                  }

                  list.add(value);
               }
               else
               {
                  parent.setValue(msgPart.getName(), value);
               }
            }
         }
         else
         {
            if (nLevel == HL7MessagePartMapping.LEVEL_MESSAGE)
            {
               throw new IntegrationException("err.integration.hl7.segment",
                  new Object[]{m_sToken, msgPart.getFullPath()});
            }

            if (nLevel == HL7MessagePartMapping.LEVEL_GROUP)
            {
               break;
            }
         }

         if (nLevel > HL7MessagePartMapping.LEVEL_SEGMENT)
         {
            if (!m_bRepeat || m_nSepLevel != nLevel)
            {
               break;
            }

            skipTokens(nLevel, 1);
         }
         else if (nLevel == HL7MessagePartMapping.LEVEL_SEGMENT)
         {
            if (nCount >= msgPart.getMaxCount())
            {
               skipTokens(nLevel, 1);

               break;
            }

            skipTokens(nLevel, 1);
         }
         else
         {
            if (nCount >= msgPart.getMaxCount())
            {
               break;
            }
         }
      }

      if (nCount < msgPart.getMinCount())
      {
         if (nCount == 0 && nLevel != HL7MessagePartMapping.LEVEL_FIELD &&
            parentMsgPart != null && grandParentMsgPart != null)
         {
            return false;
         }

         throw new IntegrationException("err.integration.minPartCount", new Object[]{msgPart.getFullPath()});
      }

      if (nCount > msgPart.getMaxCount())
      {
         throw new IntegrationException("err.integration.maxPartCount", new Object[]{msgPart.getFullPath()});
      }

      return (nCount != 0);
   }

   /**
    * Converts a string to a primitive type.
    * @param part The message part.
    * @param sValue The value to convert.
    * @return The converted value.
    */
   protected Object convert(PrimitiveMessagePart part, String sValue) throws IntegrationException
   {
      Object value;

      if (part.getType() == Primitive.TIMESTAMP)
      {
         try
         {
            switch (((HL7MessagePartMapping)part.getMapping()).getSubtype())
            {
               case HL7MessagePartMapping.SUBTYPE_DT:
                  value = HL7Util.parseDateTime(sValue, true, false, m_defTZ);
                  break;

               case HL7MessagePartMapping.SUBTYPE_TM:
                  value = HL7Util.parseDateTime(sValue, false, true, m_defTZ);
                  break;

               default:
                  value = HL7Util.parseDateTime(sValue, true, true, m_defTZ);
                  break;
            }
         }
         catch (Exception e)
         {
            throw new IntegrationException("err.integration.hl7.dtm", new Object[]{sValue}, e);
         }
      }
      else
      {
         value = part.getType().getConverter(Primitive.STRING).invoke(sValue);
      }

      part.validateValue(value);

      return value;
   }

   // inner classes

   /**
    * HL7 message header structure.
    */
   protected static class MSH
   {
      public String type;
      public String event;
      public String structure;
      public String version;
   }
}
