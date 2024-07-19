// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import nexj.core.meta.Component;
import nexj.core.meta.Metadata;
import nexj.core.meta.NamedMetadataObject;

/**
 * Message format metadata.
 */
public class Format extends NamedMetadataObject
{
   // associations

   /**
    * The root metadata object.
    */
   protected Metadata m_metadata;

   /**
    * The message mapping loader class.
    */
   protected Class m_loader;

   /**
    * The message mapping exporter class.
    */
   protected Class m_exporter;

   /**
    * The message parser component.
    */
   protected Component m_parser;

   /**
    * The message formatter component.
    */
   protected Component m_formatter;

   // constructors

   /**
    * Constructs the message format. 
    * @param sName The message format name.
    */
   public Format(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the message format. 
    */
   public Format()
   {
      super();
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
    * Sets the message mapping loader class.
    * @param loader The message mapping loader class to set.
    */
   public void setLoader(Class loader)
   {
      verifyNotReadOnly();
      m_loader = loader;
   }

   /**
    * @return The message mapping loader class.
    */
   public Class getLoader()
   {
      return m_loader;
   }
   
   /**
    * Sets the message mapping exporter class.
    * @param exporter The message mapping exporter class to set.
    */
   public void setExporter(Class exporter)
   {
      verifyNotReadOnly();
      m_exporter = exporter;
   }

   /**
    * @return The message mapping exporter class.
    */
   public Class getExporter()
   {
      return m_exporter;
   }
   
   /**
    * Sets the message parser component.
    * @param parser The message parser component. to set.
    */
   public void setParser(Component parser)
   {
      verifyNotReadOnly();
      m_parser = parser;
   }

   /**
    * @return The message parser component.
    */
   public Component getParser()
   {
      return m_parser;
   }
   
   /**
    * Sets the message formatter component.
    * @param formatter The message formatter component to set.
    */
   public void setFormatter(Component formatter)
   {
      verifyNotReadOnly();
      m_formatter = formatter;
   }

   /**
    * @return The message formatter component.
    */
   public Component getFormatter()
   {
      return m_formatter;
   }
}
