// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.wsdl;

import nexj.core.meta.integration.format.xml.schema.Element;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.Named;

/**
 * Web services message definition, the input or output of an operation.
 */
public class Message implements Named
{
   // attributes

   /**
    * The message name, without namespace/prefix.
    */
   protected String m_sName;

   // associations

   /**
    * The root element of the message's schema.
    */
   protected Element m_root;

   /**
    * The header map of the message's schema.
    */
   protected Lookup m_headerMap; // of type String -> Element

   // constructors

   /**
    * Constructs a new web services message definition.
    * @param sName The message name, without namespace/prefix.
    */
   public Message(String sName)
   {
      m_sName = sName;
   }

   // operations

   /**
    * Sets the root element of the message.
    * @param root The schema element representing this message.
    */
   public void setRoot(Element root)
   {
      m_root = root;
   }

   /**
    * Gets the root element of the message.
    * @return The schema element representing this message.
    */
   public Element getRoot()
   {
      return m_root;
   }

   /**
    * @see nexj.core.util.Named#getName()
    */
   public String getName()
   {
      return m_sName;
   }

   /**
    * Add envelope headers.
    * @param envelopeHeaderMap Envelope header map.
    */
   public void addHeaders(Lookup envelopeHeaderMap)
   {
      m_headerMap = (Lookup)envelopeHeaderMap.clone();
   }

   /**
    * Gets the envelope header iterator.
    * @return The envelope header iterator.
    */
   public Lookup.Iterator getHeaderIterator()
   {
      return (m_headerMap == null) ? HashTab.EMPTY_ITERATOR : m_headerMap.iterator();
   }

   /**
    * Gets the envelope header count.
    * @return The envelope header count.
    */
   public int getHeaderCount()
   {
      return (m_headerMap == null) ? 0 : m_headerMap.size();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "Message(" + m_sName + ")";
   }
}
