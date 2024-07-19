// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;


import nexj.core.util.HashHolder;

/**
 * Set of OIDs. Any OID holders can be added or removed,
 * but only their OIDs will be stored.
 */
public class OIDSet extends HashHolder
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = -1298526654844647152L;

   // constructors

   /**
    * @see HashHolder#HashHolder()
    */
   public OIDSet()
   {
      super();
   }

   /**
    * @see HashHolder#HashHolder(int)
    */
   public OIDSet(int initialCapacity)
   {
      super(initialCapacity);
   }

   // operations
   
   /**
    * @see java.util.Collection#add(java.lang.Object)
    */
   public boolean add(Object o)
   {
      if (o == null)
      {
         return false;
      }

      OID oid = ((OIDHolder)o).getOID();

      if (oid == null)
      {
         return false;
      }

      return super.add(oid);
   }

   /**
    * @see java.util.Collection#contains(java.lang.Object)
    */
   public boolean contains(Object o)
   {
      if (o == null)
      {
         return false;
      }

      OID oid = ((OIDHolder)o).getOID();

      if (oid == null)
      {
         return false;
      }

      return super.contains(oid);
   }

   /**
    * @see nexj.core.util.HashHolder#get(java.lang.Object)
    */
   public Object get(Object o)
   {
      OID oid = ((OIDHolder)o).getOID();

      return super.get(oid);
   }

   /**
    * @see java.util.Collection#remove(java.lang.Object)
    */
   public boolean remove(Object o)
   {
      if (o == null)
      {
         return false;
      }

      OID oid = ((OIDHolder)o).getOID();

      if (oid == null)
      {
         return false;
      }

      return super.remove(oid);
   }
}
