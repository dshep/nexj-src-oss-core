// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Interface implemented by objects, which keep information
 * about the text position.
 */
public interface TextPositionHolder
{
   /**
    * @return The text position at which the exception has occured.
    */
   TextPosition getTextPosition();
}
