// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

/**
 * Metadata object holding a format string.
 */
public interface FormatHolder
{
   /**
    * Gets the format string.
    * @return The format string; null if none.
    */
   public String getFormat();

   /**
    * Sets the format string.
    * @param sFormat The format string; null for none.
    */
   public void setFormat(String sFormat);
}
