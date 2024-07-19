// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;


/**
 * Exception indicating a broken association.
 */
public class AssociationIntegrityException extends ReferentialIntegrityException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -438615171714323977L;

   // constructors

   /**
    * Constructs an exception indicating a broken association.
    * @param location The location of the broken association.
    * @param sAttribute The association attribute.
    */
   public AssociationIntegrityException(LazyLocation location, String sAttribute)
   {
      super("err.persistence.brokenAssoc",
         new Object[]{sAttribute,
            (location == null) ? "Object" : location.getLazyClassName(),
            (location == null) ? null : location.getOID()});
      setLocation(location);
      addException(sAttribute, this);
   }

   /**
    * Constructs an exception indicating a broken association.
    * @param location The location of the broken association.
    */
   public AssociationIntegrityException(LazyLocation location)
   {
      super("err.persistence.someBrokenAssoc",
         new Object[]{(location == null) ? "Object" : location.getLazyClassName(),
            (location == null) ? "Object" : location.getLazyClassName(),
               (location == null) ? null : location.getOID()});
      setLocation(location);
   }
}
