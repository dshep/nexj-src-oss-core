// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.xml;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import nexj.core.rpc.TransferObject;

/**
 * Accepts XML document parse events.
 */
public interface ParseEventAcceptor
{
   /**
    * Accepts XML document character data for parsing.
    * @param cbuf The characters from the XML document.
    * @param nOffset The character data start position in the array.
    * @param nLength The number of characters to read from the array.
    * @return True if the character data are accepted.
    * @throws SAXException If a parse error occurs.
    */
   public boolean acceptCharacters(char[] cbuf, int nOffset, int nLength)
      throws SAXException;

   /**
    * Accepts an end element tag from an XML document.
    * @param sURI The namespace URI; empty string if no namespace.
    * @param sLocalName The local name (without prefix).
    * @param sQualName The qualified name (with prefix).
    * @return True if the end element is accepted.
    * @throws SAXException If a parse error occurs.
    */
   public boolean acceptEndElement(String sURI, String sLocalName, String sQualName)
      throws SAXException;

   /**
    * Accepts a start element tag from an XML document.
    * @param sURI The namespace URI; empty string if no namespace.
    * @param sLocalName The local name (without prefix).
    * @param sQualName The qualified name (with prefix).
    * @param attributes The attributes attached to the element.
    * @return True if the start element is accepted.
    * @throws SAXException If a parse error occurs.
    */
   public boolean acceptStartElement(String sURI, String sLocalName, String sQualName,
      Attributes attributes) throws SAXException;

   /**
    * Checks to see if the acceptor has encountered an error.
    * @param root The root of the parse result.
    * @throws Exception If an error was encountered.
    */
   public void checkFault(TransferObject root) throws Exception;

   /**
    * Resets the acceptor.
    */
   public void reset();

   /**
    * Returns the parse result, if any.
    * @return The parse result; null if none.
    */
   public Object getValue();
}