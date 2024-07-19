// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Properties;

/**
 * Interface for objects that work with a Properties object
 * such as applying properties to an underlying bean.
 */
public interface PropertiesAware
{
   /**
    * Sets the properties.
    * @param properties The properties to set.
    * @throws Throwable 
    */
   public void setProperties(Properties properties) throws Throwable;
}
