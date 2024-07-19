// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.scripting.Pair;

/**
 * Interface implemented by custom-scripted steps.
 */
public interface Scripted
{
   /**
    * Sets the action body.
    * @param body The action body to set.
    */
   void setBody(Pair body);

   /**
    * @return The action body.
    */
   Pair getBody();
}
