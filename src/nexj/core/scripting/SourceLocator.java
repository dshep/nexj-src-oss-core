// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import nexj.core.util.TextPositionHolder;

/**
 * Interface representing a source code location
 * consisting of a URL and a text position.
 */
public interface SourceLocator extends TextPositionHolder
{
   /**
    * @return The URL of the source code.
    */
   String getURL();
}
