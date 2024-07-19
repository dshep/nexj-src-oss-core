// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.MetadataObject;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

/**
 * Message table for specifying multiple messages for parsing and detection.
 * The format metadata loaders set up a parser table for detecting the message.
 */
public class MessageTable extends MetadataObject 
{
   // associations

   /**
    * The message format.
    */
   protected Format m_format;

   /**
    * Message name to object map: Message[String].
    */
   protected Lookup m_messageMap = new HashTab(8);
   
   /**
    * The message collection.
    */
   protected List m_messageList = new ArrayList(8); // of type Message

   /**
    * The table used by the parser for message detection.
    */
   protected Object m_parserTable;

   // operations

   /**
    * Sets the message format.
    * @param format The message format to set.
    */
   public void setFormat(Format format)
   {
      verifyNotReadOnly();
      m_format = format;
   }

   /**
    * @return The message format.
    */
   public Format getFormat()
   {
      return m_format;
   }
   
   /**
    * Adds a new message to the table.
    * @param message The message to add.
    */
   public void addMessage(Message message) throws MetadataException
   {
      verifyNotReadOnly();

      if (m_format == null)
      {
         m_format = message.getFormat();
      }
      else if (m_format != message.getFormat())
      {
         throw new MetadataException("err.meta.integration.messageTableMismatch",
            new Object[]{message.getName(), m_format.getName(),
               (message.getFormat() == null) ? "" : message.getFormat().getName()});
      }
      
      Object oldMessage = m_messageMap.put(message.getName(), message);
      
      if (oldMessage != null)
      {
         m_messageMap.put(message.getName(), oldMessage);
         throw new MetadataException("err.meta.integration.messageTableDup", 
            new Object[]{message.getName()});
      }

      m_messageList.add(message);
   }

   /**
    * Finds a message by name.
    * @param sName The message name. Can be null.
    * @return The message object, or null if not found.
    */
   public Message findMessage(String sName)
   {
      if (sName == null)
      {
         return null;
      }
      
      return (Message)m_messageMap.get(sName);
   }
   
   /**
    * Gets a message by name.
    * @param sName The message name. Can be null.
    * @return The message object.
    * @throws MetadataLookupException if the message is not found.
    */
   public Message getMessage(String sName) throws MetadataLookupException
   {
      if (sName == null)
      {
         throw new MetadataLookupException("err.meta.integration.messageTableLookup", "", this);
      }

      Message message = (Message)m_messageMap.get(sName);
      
      if (message == null)
      {
         throw new MetadataLookupException("err.meta.integration.messageTableLookup", sName, this);
      }

      return message;
   }
   
   /**
    * Gets a message by ordinal number.
    * @param nOrdinal The message ordinal number (0-based).
    * @return The message object.
    */
   public Message getMessage(int nOrdinal)
   {
      return (Message)m_messageList.get(nOrdinal);
   }

   /**
    * @return The message count.
    */
   public int getMessageCount()
   {
      return m_messageList.size();
   }

   /**
    * @return An iterator for the contained message objects.
    */
   public Iterator getMessageIterator()
   {
      return m_messageList.iterator();
   }
   
   /**
    * Sets the parser table.
    * @param parserTable The parser table to set.
    */
   public void setParserTable(Object parserTable)
   {
      verifyNotReadOnly();
      m_parserTable = parserTable;
   }

   /**
    * @return The parser table.
    */
   public Object getParserTable()
   {
      return m_parserTable;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "MessageTable(" + m_format + ", " + m_messageList + ")";
   }

   /**
    * Finds a message that is either derivedMessage or is a base message of derivedMessage.
    * 
    * @param message The message for which to find base messages.
    * @return The base message; null if neither derivedMessage nor any of its base
    * messages are in the table.
    */
   public Message findBaseMessage(Message message)
   {
      while (message != null && !m_messageMap.contains(message.getName()))
      {
         message = message.getBaseMessage();
      }

      return message;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();
      ((ArrayList)m_messageList).trimToSize();

      if (m_parserTable instanceof MetadataObject)
      {
         ((MetadataObject)m_parserTable).makeReadOnly();
      }
   }
}
