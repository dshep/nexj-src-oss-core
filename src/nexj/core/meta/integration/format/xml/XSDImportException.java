// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml;

import nexj.core.meta.Primitive;
import nexj.core.util.TextPosition;
import nexj.core.util.TextPositionHolder;
import nexj.core.util.UncheckedException;

/**
 * Exception thrown when an import operation fails.
 */
public class XSDImportException extends UncheckedException implements TextPositionHolder
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = -5026108791382368621L;

   // associations

   /**
    * The text position of the error.
    */
   protected TextPosition m_textPos;

   // constructors

   public XSDImportException(String sErrCode, Object[] argArray, TextPosition textPos)
   {
      super(sErrCode, argArray);
      m_textPos = textPos;
      setValue("url", textPos.getURL());
      setValue("line", Primitive.createInteger(textPos.getLine()));
      setValue("column", Primitive.createInteger(textPos.getColumn()));
   }

   /**
    * @see nexj.core.util.TextPositionHolder#getTextPosition()
    */
   public TextPosition getTextPosition()
   {
      return m_textPos;
   }
}
