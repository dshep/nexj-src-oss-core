// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.io.IOException;
import java.io.Reader;

import nexj.core.util.SubstReader;

/**
 * A SubstReader that can additionally provide default values using the following syntax:
 * ${<name>} => <value>
 * ${<name>:<default>} => if <value> != null then <value> else <default>
 * Users of class just need to implement getValue(String sKey, String defaultValue) to return the
 * value corresponding to name.
 */
public abstract class SQLTemplateSubstReader extends SubstReader
{
   /**
    * Creates the reader.
    * @param reader The stream in which to substitute the macro variables.
    */
   public SQLTemplateSubstReader(Reader reader)
   {
      super(reader);
   }

   /**
    * @see nexj.core.util.SubstReader#getValue(java.lang.String)
    */
   protected String getValue(String sName) throws IOException
   {
      int nColonIndex = sName.indexOf(':');
      String sKey = (nColonIndex < 0) ? sName : sName.substring(0, nColonIndex);
      Object value = getValue(sKey, (nColonIndex < 0) ? null : sName.substring(nColonIndex + 1));

      return (value == null) ? null : value.toString();
   }

   /**
    * Returns the substitution value for a given variable.
    * @param sKey the variable name to get the value for.
    * @param sDefaultValue The default value to use if none is available.
    * @return The substitution value.
    * @throws IOException On IO error during evaluation.
    */
   protected abstract Object getValue(String sKey, String sDefaultValue) throws IOException;
}