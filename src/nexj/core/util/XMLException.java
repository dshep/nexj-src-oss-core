// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Exception thrown by XMLUtil and XSDUtil when
 * an incorrect element is encountered.
 */
public class XMLException extends UncheckedException
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 2325633350426741881L;

   // constructors
   
   public XMLException(String sErrCode)
   {
      super(sErrCode);
   }

   public XMLException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public XMLException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public XMLException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
