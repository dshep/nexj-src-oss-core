// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.wsdl;

import java.util.Iterator;

import nexj.core.meta.integration.format.xml.schema.SchemaUniverse;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.Named;

/**
 * Web service definition.
 */
public class SOAPService implements Named
{
   // attributes

   /**
    * The location at which the web service end-point may be accessed.
    */
   protected String m_sEndpoint;

   /**
    * The web service name, without namespace/prefix.
    */
   protected String m_sName;

   /**
    * The URI identifying the web service.
    */
   protected String m_sURI;

   // associations

   /**
    * The schemas for this web service.
    */
   protected SchemaUniverse m_universe;

   /**
    * Map of operation name to operation: Operation[String].
    */
   protected Lookup m_operationMap = new HashTab();

   /**
    * Map of message name to message: Message[String].
    */
   protected Lookup m_messageMap = new HashTab();

   // constructors

   /**
    * Constructs a new web service definition.
    * @param sName The web service name, without namespace/prefix.
    */
   public SOAPService(String sName)
   {
      m_sName = sName;
      m_universe = new SchemaUniverse();
   }

   /**
    * Constructs a new web service definition.
    * @param sName The web service name, without namespace/prefix.
    * @param universe The schema universe.
    */
   public SOAPService(String sName, SchemaUniverse universe)
   {
      m_sName = sName;
      m_universe = universe;
   }

   // operations

   /**
    * Adds an operation to the web service.
    * @param operation The operation to add.
    */
   public void addOperation(Operation operation)
   {
      if (m_operationMap.put(operation.getName(), operation) != null)
      {
         throw new IllegalStateException("Duplicate operation " + operation);
      }
   }

   /**
    * Gets an iterator of the operations of the web service.
    * @return An iterator over the operations.
    */
   public Iterator getOperationIterator()
   {
      return m_operationMap.valueIterator();
   }

   /**
    * Adds a message to the web service.
    * @param message The message to add.
    */
   public void addMessage(Message message)
   {
      if (m_messageMap.put(message.getName(), message) != null)
      {
         throw new IllegalStateException("Duplicate message " + message);
      }
   }

   /**
    * Finds a message.
    * @param sName The message name.
    * @return The message if found; null if not found.
    */
   public Message findMessage(String sName)
   {
      return (Message)m_messageMap.get(sName);
   }

   /**
    * Gets an iterator of the messages on the web service.
    * @return An iterator over the messages.
    */
   public Iterator getMessageIterator()
   {
      return m_messageMap.valueIterator();
   }

   /**
    * Sets the service end-point location.
    * @param sEndpoint The location at which the web service end-point may be accessed.
    */
   public void setEndpoint(String sEndpoint)
   {
      m_sEndpoint = sEndpoint;
   }

   /**
    * Gets the service end-point location.
    * @return The location at which the web service end-point may be accessed.
    */
   public String getEndpoint()
   {
      return m_sEndpoint;
   }

   /**
    * Sets the service URI.
    * @param sURI The URI identifying the web service.
    */
   public void setURI(String sURI)
   {
      m_sURI = sURI;
   }

   /**
    * Gets the service URI.
    * @return The URI identifying the web service.
    */
   public String getURI()
   {
      return m_sURI;
   }

   /**
    * Gets the schemas for this web service.
    * @return The set of schemas.
    */
   public SchemaUniverse getUniverse()
   {
      return m_universe;
   }

   /**
    * @see nexj.core.util.Named#getName()
    */
   public String getName()
   {
      return m_sName;
   }
}
