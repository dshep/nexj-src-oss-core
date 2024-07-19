// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import javax.xml.namespace.QName;

import nexj.core.meta.Type;

/**
 * Object to be marshalled.
 */
public class XMLElement
{
   // associations

   /**
    * The qualified name of this element tag.
    */
   protected QName m_element;

   /**
    * The object/body of this element.
    */
   protected Object m_object;

   /**
    * The expected object type (null == dynamic).
    */
   protected Type m_type;

   // constructors

   /**
    * Constructor.
    * @param object The object/body of this element.
    * @param element The tag type of this element (not null).
    * @param type The expected object type (null == dynamic).
    */
   public XMLElement(Object object, QName element, Type type)
   {
      assert element != null;

      m_element = element;
      m_object = object;
      m_type = type;
   }

   // operations

   /**
    * @see XMLElement#m_element
    */
   public QName getElement()
   {
      return m_element;
   }

   /**
    * @see XMLElement#m_object
    */
   public Object getObject()
   {
      return m_object;
   }

   /**
    * @see XMLElement#m_type
    */
   public Type getType()
   {
      return m_type;
   }
}