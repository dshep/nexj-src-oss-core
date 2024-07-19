// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Base class for all localizeable unchecked exceptions.
 * Specific classes must be derived so that an exception handler could
 * distinguish between exception classes. If an exception handler has to
 * receive some additional information about the exception, do not pass
 * it in the argument array - it is for error messages only. Instead,
 * derive a new exception and pass the data in its members.
 * The naming convention of the error code is the following:
 * err.[facility].[error]
 * e.g. "err.meta.lookup"
 */
public class UncheckedException extends RuntimeException implements ErrorCode, PropertyMap
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = -6680054847632357951L;

   // attributes

   /**
    * The error code, used also as a string id for the error message.
    */
   protected String m_sErrCode;

   /**
    * The array of error message arguments.
    */
   protected Object[] m_argArray;

   // associations

   /**
    * The exception data.
    */
   protected transient PropertyMap m_dataMap;

   // constructors

   /**
    * Create an unchecked exception with an error code.
    * @param sErrCode The error code/string id of the error message.
    */
   public UncheckedException(String sErrCode)
   {
      super((String)null);
      m_sErrCode = sErrCode;
      m_argArray = null;
   }

   /**
    * Create an unchecked exception with an error code and a message argument array.
    * @param sErrCode The error code/string id of the error message.
    * @param argArray The message argument array.
    */
   public UncheckedException(String sErrCode, Object[] argArray)
   {
      super((String)null);
      m_sErrCode = sErrCode;
      m_argArray = argArray;
   }

   /**
    * Create an unchecked exception with an error code and a nested exception.
    * @param sErrCode The error code/string id of the error message.
    * @param cause The nested exception that has caused this exception.
    */
   public UncheckedException(String sErrCode, Throwable cause)
   {
      super(null, cause);
      m_sErrCode = sErrCode;
      m_argArray = null;
   }

   /**
    * Create an unchecked exception with an error code, message argument array
    * and a nested exception.
    * @param sErrCode The error code/string id of the error message.
    * @param argArray The message argument array.
    * @param cause The nested exception that has caused this exception.
    */
   public UncheckedException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(null, cause);
      m_sErrCode = sErrCode;
      m_argArray = argArray;
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
      return m_argArray;
   }

   /**
    * @see java.lang.Throwable#getMessage()
    */
   public String getMessage()
   {
      StringBuffer buf = new StringBuffer(128);
      StringTable stringTable = StringTable.getInstance();

      if (stringTable != null)
      {
         String sErrCode = stringTable.get(m_sErrCode);

         if (sErrCode != m_sErrCode)
         {
            buf.append(stringTable.format(m_sErrCode, m_argArray, StringTable.getTimeZone()));
            buf.append(" (");
            buf.append(m_sErrCode);
            buf.append(')');

            if (m_dataMap != null)
            {
               buf.append(" - ");
               buf.append(m_dataMap);
            }

            return buf.toString();
         }
      }

      buf.append(m_sErrCode);

      if (m_argArray != null)
      {
         buf.append('(');

         for (int i = 0; i < m_argArray.length; ++i)
         {
            if (i > 0)
            {
               buf.append(", ");
            }

            Object arg = m_argArray[i];

            if (arg instanceof String)
            {
               buf.append('"');
               buf.append(arg);
               buf.append('"');
            }
            else if (arg instanceof Named)
            {
               buf.append(((Named)arg).getName());
            }
            else
            {
               buf.append(arg);
            }
         }

         buf.append(')');
      }

      if (m_dataMap != null)
      {
         buf.append(" - ");
         buf.append(m_dataMap);
      }

      return buf.toString();
   }

   /**
    * @see nexj.core.util.PropertyMap#findValue(java.lang.String)
    */
   public Object findValue(String sName)
   {
      return (m_dataMap == null) ? null : m_dataMap.findValue(sName);
   }

   /**
    * @see nexj.core.util.PropertyMap#findValue(java.lang.String, java.lang.Object)
    */
   public Object findValue(String sName, Object defaultValue)
   {
      return (m_dataMap == null) ? defaultValue : m_dataMap.findValue(sName, defaultValue);
   }

   /**
    * @see nexj.core.util.PropertyMap#getClassName()
    */
   public String getClassName()
   {
      return getClass().getName();
   }

   /**
    * @see nexj.core.util.PropertyMap#getIterator()
    */
   public PropertyIterator getIterator()
   {
      return (m_dataMap == null) ? PropertyHashTab.EMPTY_ITERATOR : m_dataMap.getIterator();
   }

   /**
    * @see nexj.core.util.PropertyMap#getValue(java.lang.String)
    */
   public Object getValue(String sName)
   {
      return (m_dataMap == null) ? null : m_dataMap.getValue(sName);
   }

   /**
    * @see nexj.core.util.PropertyMap#getValueCount()
    */
   public int getValueCount()
   {
      return (m_dataMap == null) ? 0 : m_dataMap.getValueCount();
   }

   /**
    * @see nexj.core.util.PropertyMap#hasValue(java.lang.String)
    */
   public boolean hasValue(String sName)
   {
      return (m_dataMap == null) ? false : m_dataMap.hasValue(sName);
   }

   /**
    * @see nexj.core.util.PropertyMap#setValue(java.lang.String, java.lang.Object)
    */
   public void setValue(String sName, Object value)
   {
      if (m_dataMap == null)
      {
         m_dataMap = new PropertyHashTab(1);
      }

      m_dataMap.setValue(sName, value);
   }
}
