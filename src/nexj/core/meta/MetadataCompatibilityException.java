// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

/**
 * Exception thrown when metadata is found to be incompatible with an earlier version.
 */
public class MetadataCompatibilityException extends MetadataValidationException
{
   // constants
   
   /**
    * Serialization version. 
    */
   private static final long serialVersionUID = 2441176549076029838L;

   // constructors

   /**
    * @param sErrCode The error code/string id of the error message.
    * @param argArray The message argument array.
    */
   public MetadataCompatibilityException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }
   
   /**
    * @param sErrCode The error code/string id of the error message.
    * @param argArray The message argument array.
    * @param cause The nested exception that has caused this exception.
    */
   public MetadataCompatibilityException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
   
   /**
    * @see nexj.core.meta.MetadataValidationException#getMessage()
    */
   public String getMessage()
   {
      String sMsg = super.getMessage();
      
      return "Model compatibility error: " + sMsg;
   }
}
