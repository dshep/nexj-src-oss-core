// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

/**
 * Interface implemented by objects which can identify 
 * an object instance, e.g. Instance, OID, TransferObject.
 */
public interface OIDHolder
{
   // operations

   /**
    * Returns the object identifier.
    * @return The object identifier.
    */
   public OID getOID();

   /**
    * Sets the object identifier.
    * @param oid The object identifier to set.
    */
   public void setOID(OID oid);
}
