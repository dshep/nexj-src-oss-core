// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence;


/**
 * Mapping of an attribute referencing an object.
 */
public abstract class ClassMapping extends AttributeMapping
{
   // associations

   /**
    * The persistence mapping of the associated class.
    */
   protected PersistenceMapping m_mapping;

   // operations

   /**
    * Determines whether the persistence info is stored with this mapping's container class.
    * If true, then no significant extra cost is incurred by querying for this attribute during
    * a query of the container class.
    * 
    * <pre>
    * Example:
    * 
    * Class A has attribute "b" of type Class B.
    * 
    * Storage case #1: ((A'b).isInner() == true)
    * 
    * Table A:         Table B:
    * +----+-----+     +----+
    * | id | bId |     | id |
    * +----+-----+     +----+
    *         |           ^
    *         +-----------+
    * 
    * When querying Class A, one can know the value of A'b without requiring
    * a join to Table B. The only cost incurred is that of accessing the "bId"
    * column (which is much less than involving another join). This cost
    * is not considered "significant".
    * 
    * 
    * 
    * Storage case #2: ((A'b).isInner() == false)
    * 
    * Table A:         Table B:
    * +----+           +----+-----+
    * | id |           | id | aId |
    * +----+           +----+-----+
    *    ^                     |
    *    +---------------------+
    * 
    * It is impossible to know the value of A'b when querying Class A unless
    * Table B is included in the joins.
    * </pre>
    * 
    * @return True if the persistence info is stored with
    * this mapping's container class. 
    */
   public abstract boolean isInner();

   /**
    * @return True if the mapping can retrieve only
    * a unique instance by itself, i.e. without
    * attribute or class where clauses.
    */
   public abstract boolean isUnique();

   /**
    * @return True if either side of the mapping
    * indicates a unique instance.
    */
   public abstract boolean isPure();

   /**
    * Gets the source or destination key of the mapping.
    * The source key is on the class containing the association attribute.
    * The destination key is on the class at the association end.
    * @param bDestination True for the destination key, false for the source key.
    * @return The requested key, or null if the mapping does not support it.
    */
   public abstract Key getKey(boolean bDestination);

   /**
    * Sets the persistence mapping of the associated class.
    * @param mapping The persistence mapping of the associated class to set.
    */
   public void setMapping(PersistenceMapping mapping)
   {
      verifyNotReadOnly();
      m_mapping = mapping;
   }

   /**
    * @return The persistence mapping of the associated class.
    */
   public PersistenceMapping getMapping()
   {
      return m_mapping;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      if (m_mapping != null && m_mapping != m_mapping.getMetaclass().getPersistenceMapping())
      {
         m_mapping.makeReadOnly();
      }
      
      super.makeReadOnly();
   }
}
