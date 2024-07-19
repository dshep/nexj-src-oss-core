// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.service;

import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.MetadataResource;
import nexj.core.meta.NamedMetadataObject;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessageTable;

/**
 * Service interface metadata.
 */
public class Interface extends NamedMetadataObject implements MetadataResource
{
   // attributes

   /**
    * The metadata resource name.
    */
   protected String m_sResourceName;

   // associations

   /**
    * The request message table.
    */
   protected MessageTable m_requestTable = new MessageTable();

   /**
    * The response message table.
    */
   protected MessageTable m_responseTable = new MessageTable();

   /**
    * The root metadata object.
    */
   protected Metadata m_metadata;

   // constructors

   /**
    * Constructs the interface.
    * @param sName The interface name.
    */
   public Interface(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * Sets the root metadata object.
    * @param metadata The root metadata object to set.
    */
   public void setMetadata(Metadata metadata)
   {
      verifyNotReadOnly();
      m_metadata = metadata;
   }

   /**
    * @return The root metadata object.
    */
   public Metadata getMetadata()
   {
      return m_metadata;
   }

   /**
    * @see nexj.core.meta.MetadataResource#setResourceName(java.lang.String)
    */
   public void setResourceName(String sName)
   {
      verifyNotReadOnly();
      m_sResourceName = sName;
   }

   /**
    * @see nexj.core.meta.MetadataResource#getResourceName()
    */
   public String getResourceName()
   {
      return m_sResourceName;
   }

   /**
    * Sets the message format.
    * @param format The message format to set.
    */
   public void setFormat(Format format)
   {
      verifyNotReadOnly();
      m_requestTable.setFormat(format);
      m_responseTable.setFormat(format);
   }

   /**
    * @return The message format.
    */
   public Format getFormat()
   {
      return m_requestTable.getFormat();
   }

   /**
    * Adds a message to the request table.
    * @param message The message to add.
    */
   public void addRequest(Message message)
   {
      m_requestTable.addMessage(message);
      
      if (message.getResponse() != null)
      {
         addResponse(message.getResponse());
      }
   }

   /**
    * Adds a message to the response table.
    * @param message The message to add.
    */
   public void addResponse(Message message)
   {
      if (m_responseTable.findMessage(message.getName()) == null)
      {
         m_responseTable.addMessage(message);
      }
   }

   /**
    * @return The request message table.
    */
   public MessageTable getRequestTable()
   {
      return m_requestTable;
   }

   /**
    * @return The response message table.
    */
   public MessageTable getResponseTable()
   {
      return m_responseTable;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();
      m_requestTable.makeReadOnly();
      m_responseTable.makeReadOnly();
   }

   /**
    * @see nexj.core.meta.NamedMetadataObject#createLookupException()
    */
   protected MetadataException createLookupException()
   {
      return new MetadataLookupException("err.meta.interfaceLookup", m_sName, m_metadata.getName());
   }
}
