// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import nexj.core.persistence.Field;

/**
 * Interface for SQL custom type conversions. 
 */
public interface SQLConverter
{
   /**
    * Appends a custom SQL type conversion expression to a string buffer.
    * @param buf The destination buffer.
    * @param field The field to be converted.
    * @param gen The SQLGenerator instance.
    */
   public void appendConversion(StringBuffer buf, Field field, SQLGenerator gen);
}
