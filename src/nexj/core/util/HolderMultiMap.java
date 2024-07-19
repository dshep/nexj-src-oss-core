// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Collection;

/**
 * A multi map that stores values in a Holder instead of a List
 */
public class HolderMultiMap extends MultiMap
{
   private static final long serialVersionUID = 6701972059449204996L;

   // operations

   /**
    * @see nexj.core.util.MultiMap#createCollection()
    */
   protected Collection createCollection()
   {
      return new HashHolder(2);
   }
}
