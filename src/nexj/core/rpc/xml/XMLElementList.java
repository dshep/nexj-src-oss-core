// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import javax.xml.namespace.QName;

import nexj.core.meta.Type;

/**
 * Object to be marshalled as a list representation.
 */
public class XMLElementList extends XMLElement
{
   /**
    * Constructor.
    * @see nexj.core.rpc.xml.XMLElement#XMLElement(Object, javax.xml.namespace.QName, nexj.core.meta.Type)
    */
   public XMLElementList(Object object, QName element, Type type)
   {
      super(object, element, type);
   }
}