// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import nexj.core.util.ErrorCode;
import nexj.core.util.TextPosition;
import nexj.core.util.TextPositionHolder;

/**
 * Stores information about a parser error.
 */
public class ParserError implements ErrorCode, TextPositionHolder
{
   // attributes

   /**
    * The error code.
    */
   protected String m_sErrCode;
   
   // associations

   /**
    * The error arguments. Can be null.
    */
   protected Object[] m_errArgs;
   
   /**
    * The text position.
    */
   protected TextPosition m_textPos;
   
   // constructors

   public ParserError(String sErrCode, Object[] errArgs, TextPosition pos)
   {
      m_sErrCode = sErrCode;
      m_errArgs = errArgs;
      m_textPos = pos;
   }

   // operations

   /**
    * @see nexj.core.util.ErrorCode#isSystem()
    */
   public boolean isSystem()
   {
      return true;
   }

   /**
    * @see nexj.core.util.ErrorCode#getErrorCode()
    */
   public String getErrorCode()
   {
      return m_sErrCode;
   }

   /**
    * @see nexj.core.util.ErrorCode#getErrorArgs()
    */
   public Object[] getErrorArgs()
   {
      return m_errArgs;
   }

   /**
    * @see nexj.core.util.TextPositionHolder#getTextPosition()
    */
   public TextPosition getTextPosition()
   {
      return m_textPos;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(128);

      if (m_textPos != null)
      {
         buf.append(m_textPos);
         buf.append(": ");
      }

      buf.append(m_sErrCode);

      if (m_errArgs != null)
      {
         buf.append('(');

         for (int i = 0; i < m_errArgs.length; ++i)
         {
            if (i != 0)
            {
               buf.append(", ");
            }

            buf.append(m_errArgs[i]);
         }

         buf.append(')');
      }

      return buf.toString();
   }
}
