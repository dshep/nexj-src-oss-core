// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import nexj.core.util.SysUtil;
import nexj.core.util.TextPosition;
import nexj.core.util.UncheckedException;

/**
 * Error during compilation.
 */
public class CompilerException extends UncheckedException implements SourceLocator
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 7739669131271271727L;

   // attributes

   /**
    * The text position. 
    */
   private TextPosition m_textPos;

   // constructors

   /**
    * Constructs a compiler exception.
    * @param sErrCode The error code.
    * @param argArray The message argument array.
    * @param cause The causing exception.
    * @param pos The text position at which the error occurred.
    */
   public CompilerException(String sErrCode, Object[] argArray, Throwable cause, TextPosition pos)
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
    * Sets the source code URL.
    * @param sURL The source code URL to set.
    */
   public void setURL(String sURL)
   {
      if (m_textPos == null)
      {
         m_textPos = new TextPosition(0, -1, sURL);
      }
      else
      {
         m_textPos.setURL(sURL);
      }
   }

   /**
    * @return The source code URL.
    */
   public String getURL()
   {
      if (m_textPos == null)
      {
         return null;
      }

      return m_textPos.getURL();
   }

   /**
    * @see java.lang.Throwable#getMessage()
    */
   public String getMessage()
   {
      String sMessage = super.getMessage() + SysUtil.LINE_SEP + "URL=" + getURL();

      if (m_textPos != null)
      {
         sMessage += ", Line=" + (m_textPos.getLine() + 1) + ", " +
         "Column=" + (m_textPos.getColumn() + 1);
      }

      return sMessage;
         
   }
}
