// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.virtual;

import nexj.core.meta.persistence.PropertyDataSourceFragment;

/**
 * A virtual data source fragment.
 */
public class VirtualDataSourceFragment extends PropertyDataSourceFragment
{
   // attributes

   /**
    * The warning timeout (milliseconds).
    */
   protected long m_lWarningTimeout;

   // operations

   /**
    * Sets the warning timeout in milliseconds.
    * @param lWarningTimeout The warning timeout in milliseconds to set (0 for unlimited).
    */
   public void setWarningTimeout(long lWarningTimeout)
   {
      verifyNotReadOnly();

      if (lWarningTimeout == Long.MAX_VALUE)
      {
         lWarningTimeout = 0;
      }
      
      m_lWarningTimeout = lWarningTimeout;
   }

   /**
    * @return The warning timeout in milliseconds.
    */
   public long getWarningTimeout()
   {
      return m_lWarningTimeout;
   }
}
