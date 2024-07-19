// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import nexj.core.meta.Metadata;
import nexj.core.meta.NamedMetadataObject;

/**
 * Describes the communication channel type.
 */
public class ChannelType extends NamedMetadataObject
{
   // associations
   
   /**
    * The root metadata object.
    */
   protected Metadata m_metadata;

   /**
    * The channel metadata loader class object.
    */
   protected Class m_loader;

   /**
    * The channel metadata exporter class object.
    */
   protected Class m_exporter;

   /**
    * The sender adapter class object.
    */
   protected Class m_sender;

   /**
    * The receiver adapter class object.
    */
   protected Class m_receiver;

   // constructors

   /**
    * Construct the channel type.
    * @param sName The channel type name.
    */
   public ChannelType(String sName)
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
    * Sets the channel metadata loader class object.
    * @param loader The channel metadata loader class object to set.
    */
   public void setLoader(Class loader)
   {
      verifyNotReadOnly();
      m_loader = loader;
   }

   /**
    * @return The channel metadata loader class object.
    */
   public Class getLoader()
   {
      return m_loader;
   }
   
   /**
    * Sets the channel metadata exporter class object.
    * @param exporter The channel metadata exporter class object to set.
    */
   public void setExporter(Class exporter)
   {
      verifyNotReadOnly();
      m_exporter = exporter;
   }

   /**
    * @return The channel metadata exporter class object.
    */
   public Class getExporter()
   {
      return m_exporter;
   }
   
   /**
    * Sets the sender adapter class object.
    * @param sender The sender adapter class object to set.
    */
   public void setSender(Class sender)
   {
      verifyNotReadOnly();
      m_sender = sender;
   }

   /**
    * @return The sender adapter class object.
    */
   public Class getSender()
   {
      return m_sender;
   }
   
   /**
    * Sets the receiver adapter class object.
    * @param receiver The server adapter class object to set.
    */
   public void setReceiver(Class receiver)
   {
      verifyNotReadOnly();
      m_receiver = receiver;
   }

   /**
    * @return The receiver adapter class object.
    */
   public Class getReceiver()
   {
      return m_receiver;
   }
}
