// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import nexj.core.util.SysUtil;
import nexj.core.util.TextPosition;
import nexj.core.util.TextPositionHolder;
import nexj.core.util.UncheckedException;

/**
 * Error during parsing.
 */
public class ParserException extends UncheckedException implements TextPositionHolder
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = -5573939398828392298L;

   // attributes

   private TextPosition m_textPos;

   // constructors

   /**
    * Create a parser exception.
    * @param sErrCode The error code.
    * @param argArray The message argument array.
    * @param cause The causing exception.
    * @param pos The text position at which the error occurred.
    */
   public ParserException(String sErrCode, Object[] argArray, Throwable cause, TextPosition pos)
   {
      super(sErrCode, argArray, cause);
      m_textPos = pos;
   }

   // operations

   /**
    * @return The text position at which the exception has occured.
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
