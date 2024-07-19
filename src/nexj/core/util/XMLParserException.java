// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Exception indicating an XML parse error and
 * containing the error text position.
 */
public class XMLParserException extends XMLException implements TextPositionHolder
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = 1730825087706254440L;

   // association

   /**
    * The text position of the error.
    */
   private TextPosition m_textPos;

   // constructor

   public XMLParserException(String sMessage, int nLine, int nColumn, Throwable e)
   {
      super("err.xml.parser", new Object[]{sMessage}, e);
      m_textPos = new TextPosition(nLine - 1, nColumn - 1);
   }

   // operations

   /**
    * @return The text position of the error.
    */
   public TextPosition getTextPosition()
   {
      return m_textPos;
   }

   /**
    * @see java.lang.Throwable#getMessage()
    */
   public String getMessage()
   {
      if (m_textPos == null)
      {
         return super.getMessage();
      }

      return super.getMessage() + SysUtil.LINE_SEP +
         "Line=" + (m_textPos.getLine() + 1) + ", " +
         "Column=" + (m_textPos.getColumn() + 1);
   }
}
