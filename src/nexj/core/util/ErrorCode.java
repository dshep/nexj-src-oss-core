// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Interface implemented by all exceptions providing localizable messages.
 */
public interface ErrorCode
{
   /**
    * @return True for system errors, which should not be shown directly to end-users. 
    */
   boolean isSystem();

   /**
    * @return The error code, also used as a localized string id for the error message.
    */
   String getErrorCode();

   /**
    * @return The error message arguments. Can be null.
    * String Ids are represented by instances of the StringId class.
    */
   Object[] getErrorArgs(); 
}
