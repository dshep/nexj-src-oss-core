// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.util.CheckedException;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.SysUtil;
import nexj.core.util.TextPosition;
import nexj.core.util.TextPositionHolder;
import nexj.core.util.UncheckedException;

/**
 * Exception thrown when invalid metadata has been loaded.
 * It provides information about the location of the invalid resource element.
 */
public class MetadataValidationException extends MetadataException implements MetadataMarker
{
   // constants

   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = -9060247999414156327L;

   // attributes

   /**
    * Metadata element location properties.
    */
   private Lookup m_propertyMap;

   // constructors

   public MetadataValidationException(String sErrCode)
   {
      super(sErrCode);
   }

   public MetadataValidationException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public MetadataValidationException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public MetadataValidationException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }

   public MetadataValidationException(UncheckedException e)
   {
      super(e.getErrorCode(), e.getErrorArgs(), e);
      setStackTrace(e.getStackTrace());
      
      if (e instanceof TextPositionHolder)
      {
         setLineAndColumn(((TextPositionHolder)e).getTextPosition());
      }
   }

   public MetadataValidationException(CheckedException e)
   {
      super(e.getErrorCode(), e.getErrorArgs(), e);
      setStackTrace(e.getStackTrace());
      
      if (e instanceof TextPositionHolder)
      {
         setLineAndColumn(((TextPositionHolder)e).getTextPosition());
      }
   }

   // operations

   /**
    * Sets the line and column properties from a text position.
    * @param pos The text position.
    */
   private void setLineAndColumn(TextPosition pos)
   {
      if (pos != null)
      {
         setProperty("line", Primitive.createInteger(pos.getLine() + 1));
         setProperty("column", Primitive.createInteger(pos.getColumn() + 1));
      }
   }

   /**
    * Sets a metadata element location property.
    * @param sName Property name.
    * @param value Property value.
    */
   public void setProperty(String sName, Object value)
   {
      if (m_propertyMap == null)
      {
         m_propertyMap = new HashTab();
      }
      
      m_propertyMap.put(sName, value);
   }
   
   /**
    * Gets a metadata element location property.
    * @param sName Property name.
    * @return The property value. Null if not found.
    */
   public Object getProperty(String sName)
   {
      if (m_propertyMap == null)
      {
         return null;
      }
      
      return m_propertyMap.get(sName);
   }
   
   /**
    * @return A property iterator, the key and value hold
    * the property name and its value correspondingly.
    */
   public Lookup.Iterator getPropertyIterator()
   {
      if (m_propertyMap == null)
      {
         return HashTab.EMPTY_ITERATOR;
      }
      
      return m_propertyMap.iterator();
   }
   
   /**
    * @return The property count.
    */
   public int getPropertyCount()
   {
      if (m_propertyMap == null)
      {
         return 0;
      }
      
      return m_propertyMap.size();
   }
   
   /**
    * Sets the metadata resource name (e.g. XML file path
    * relative to the metadata root URL).
    * @param sName The resource name to set.
    */
   public void setResourceName(String sName)
   {
      setProperty(RESOURCE_NAME, sName);
   }
   
   /**
    * @return The metadata resource name (e.g. relative XML file path).
    */
   public String getResourceName()
   {
      return (String)getProperty(RESOURCE_NAME);
   }
   
   /**
    * Sets the invalid metadata element type name (e.g. Metaclass, Attribute).
    * @param sName The metadata element type name.
    */
   public void setTypeName(String sName)
   {
      setProperty(TYPE_NAME, sName);
   }
   
   /**
    * @return The metadata element type name (e.g. Metaclass, Attribute).
    */
   public String getTypeName()
   {
      return (String)getProperty(TYPE_NAME);
   }

   /**
    * @see java.lang.Throwable#getMessage()
    */
   public String getMessage()
   {
      String sMsg = super.getMessage();
      
      if (m_propertyMap != null)
      {
         sMsg += SysUtil.LINE_SEP + "Location=" + m_propertyMap.toString();
      }
      
      return sMsg;
   }
}
