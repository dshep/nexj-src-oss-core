// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.util.Named;

/**
 * Exception indicating an attempt to retrieve missing metadata.
 */
public class MetadataLookupException extends MetadataException
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 4214838308707760853L;

   // constructors
   
   /**
    * Creates a metadata lookup exception given the name of the
    * missing item and the object where it has been sought.
    * @param sErrCode The error code.
    * @param sWhat The name of the missing item.
    * @param where The container metadata object.
    */
   public MetadataLookupException(String sErrCode, String sWhat, MetadataObject where)
   {
      this(sErrCode, sWhat, (where instanceof Named) ? ((Named)where).getName() : where.toString());
   }
   
   /**
    * Creates a metadata lookup exception given the name of the
    * missing item and the name of the object where it has been sought.
    * @param sErrCode The error code.
    * @param sWhat The name of the missing item.
    * @param sWhere The container metadata object name.
    */
   public MetadataLookupException(String sErrCode, String sWhat, String sWhere)
   {
      super(sErrCode, new Object[]{sWhat, sWhere});
   }
   
   /**
    * Create a metadata lookup exception given an error code and message argument array.
    * @param sErrCode The error code/string id of the error message.
    * @param argArray The message argument array.
    */
   public MetadataLookupException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }
}
