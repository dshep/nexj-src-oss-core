// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.util.TextPosition;
import nexj.core.util.TextPositionHolder;
import nexj.core.util.UncheckedException;

/**
 * Metadata handling error - base class for all metadata exceptions.
 */
public class MetadataException extends UncheckedException implements TextPositionHolder
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = -2441383606189913794L;

   // constructors
   
   public MetadataException(String sErrCode)
   {
      super(sErrCode);
   }

   public MetadataException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public MetadataException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public MetadataException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }

   /**
    * @see nexj.core.util.TextPositionHolder#getTextPosition()
    */
   public TextPosition getTextPosition()
   {
      if (getCause() instanceof TextPositionHolder)
      {
         return ((TextPositionHolder)getCause()).getTextPosition();
      }
      
      return null;
   }
}
