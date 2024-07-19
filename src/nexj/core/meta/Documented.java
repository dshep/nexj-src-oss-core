// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

/**
 * Interface implemented by documented objects.
 */
public interface Documented
{
   /**
    * Sets the object description.
    * @param sDescription The description string.
    */
   void setDescription(String sDescription);
   
   /**
    * @return The object description.
    */
   String getDescription();
}
