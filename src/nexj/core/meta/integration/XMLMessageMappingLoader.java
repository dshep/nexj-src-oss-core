// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import nexj.core.meta.xml.XMLMetadataLoader;

import org.w3c.dom.Element;

/**
 * Interface implemented by message mapping loaders.
 */
public interface XMLMessageMappingLoader
{
   /**
    * Loads a message part mapping.
    * @param element The DOM element containing the message part.
    * @param msg The containing message
    * @param part The message part for which to load the mapping.
    * @param format The message format.
    * @param loader The root metadata loader.
    * @return The loaded mapping.
    */
   MessagePartMapping loadMapping(Element element, Message msg, MessagePart part,
      Format format, XMLMetadataLoader loader);
}
