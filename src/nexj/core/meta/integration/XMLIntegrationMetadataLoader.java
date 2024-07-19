// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import nexj.core.meta.xml.XMLMetadataLoader;

import org.w3c.dom.Element;

/**
 * Interface implemented by components reading
 * channel metadata and connection data from XML DOM.
 */
public interface XMLIntegrationMetadataLoader
{
   // operations

   /**
    * Loads the channel.
    * @param element The element containing the channel.
    * @param sName The channel name.
    * @param type The channel type.
    * @param loader The metadata loader.
    * @return The loaded channel.
    */
   public Channel loadChannel(Element element, String sName, ChannelType type, XMLMetadataLoader loader);

   /**
    * Loads the connection.
    * @param element The element containing the connection information.
    * @param channel The channel.
    * @param loader The metadata loader.
    */
   public void loadConnection(Element element, Channel channel, XMLMetadataLoader loader);
}
