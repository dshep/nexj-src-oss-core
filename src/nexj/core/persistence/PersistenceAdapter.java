// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import java.util.List;

import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.Key;
import nexj.core.meta.persistence.Schema;
import nexj.core.meta.persistence.SchemaUpgrade;
import nexj.core.meta.upgrade.UpgradeState;
import nexj.core.persistence.operator.ConversionMapper;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.UnitOfWork;
import nexj.core.runtime.ValidationException;
import nexj.core.util.PropertyMap;

/**
 * Interface implemented by components, which translate an
 * OO query to an engine-specific request and execute it.
 */
public interface PersistenceAdapter
{
   /**
    * Maps a query to its persistent storage.
    * @param query The query to map.
    */
   void mapQuery(Query query) throws PersistenceException;

   /**
    * Reads the class instances specified by the query.
    * The query must already be mapped using mapQuery().
    * @param query The OO query.
    * @return The class instance list.
    */
   InstanceList read(Query query) throws PersistenceException;

   /**
    * Opens a cursor over the class instances specified by the query.
    * The query must already be mapped using mapQuery().
    * @param query The OO query.
    * @return The cursor.
    */
   Cursor openCursor(Query query) throws PersistenceException;

   /**
    * Determines if a query selects a unique instance.
    * @param query The query node.
    * @param sourceList The collection of equality conjunction sources. 
    */
   boolean isUnique(Query query, List sourceList);

   /**
    * Gets the query corresponding to the given source (taking into account denormalization).
    * @param source The source.
    * @return The query.
    */
   Query getQuery(Source source); 

   /**
    * Expands an OID source into fields.
    * @param source The OID source (e.g. a query node).
    * @return The field array corresponding to the OID components,
    * or null if expansion is not supported.
    */
   Field[] getFields(Source source) throws PersistenceException;

   /**
    * Gets the values of an OID corresponding to a given source.
    * @param oid The OID.
    * @param source The OID source.
    * @return The array of OID component values, the same order as
    * the fields returned by getFields().
    */
   Object[] getValues(OID oid, Source source) throws PersistenceException;

   /**
    * Gets the value of a given source from an OID.
    * @param oid The OID.
    * @param source The value source.
    * @return The value, or Undefined.VALUE if it cannot be determined.
    */
   Object getValue(OID oid, Source source);

   /**
    * Gets the value of a given source from an instance.
    * @param instance The instance.
    * @param source The value source.
    * @return The value, or Undefined.VALUE if it cannot be determined.
    */
   Object getValue(PropertyMap instance, Source source);

   /**
    * Gets the value of a given mapping item from an instance.
    * @param instance The instance.
    * @param item The mapping item.
    * @return The value or null if it cannot be determined.
    */
   OID getOID(PropertyMap instance, Object item);

   /**
    * Checks if a given value can fit in the allocated persistent storage.
    * @param mapping The attribute mapping.
    * @param value The attribute value. 
    */
   void validate(AttributeMapping mapping, Object value) throws ValidationException;

   /**
    * Determines if an operator is supported by the persistence engine.
    * @param op The operator. Only its operands have been normalized.
    * @param nFlags Normalization flags, Operator.NORMALIZE_*.
    * @return True if supported.
    */
   boolean isSupported(Operator op, int nFlags);

   /**
    * @return The operator type conversion mapper.
    */
   ConversionMapper getConversionMapper() throws PersistenceException;

   /**
    * Creates work items for a specific instance
    * and adds them to the unit of work.
    * @param uow The unit of work to which to add the work items.
    * @param instance The instance for which to create the work items.
    */
   void addWork(UnitOfWork uow, Instance instance) throws PersistenceException;

   /**
    * Adds successor or predecessor work items for a given work item.
    * @param uow The unit of work to which to add the work items.
    * @param work The given work item.
    * @param instance The instance for which to create the work items.
    * @param dstKey The dependent instance key that has to be determined.
    * @param srcKey The instance key that has to be calculated before the dependent key.
    * @param bSuccessor True to add a successor of work, false to add a predecessor of work.
    */
   void addDependency(UnitOfWork uow, Work work, Instance instance, Key dstKey, Key srcKey, boolean bSuccessor) throws PersistenceException;

   /**
    * Adds work items for denormalization of a given attribute.
    * @param uow The unit of work to which to add the work items.
    * @param instance The source instance.
    * @param mapping The attribute mapping.
    */
   void addDenorm(UnitOfWork uow, Instance instance, AttributeMapping mapping) throws PersistenceException;
   
   /**
    * Executes the work items in a specified order.
    * @param workArray The array containing the work items.
    * @param nStart The start index.
    * @param nEnd The end index (exclusive).
    */
   void execute(Work[] workArray, int nStart, int nEnd) throws PersistenceException;

   /**
    * Gets the persistent storage schema version.
    * @param schema The persistence schema.
    * @return The schema version, or null if versioning is not supported.
    */
   SchemaVersion getVersion(Schema schema) throws PersistenceException;

   /**
    * Sets the persistent storage schema version.
    * @param schema The persistence schema.
    * @param version The schema version.
    */
   void setVersion(Schema schema, SchemaVersion version) throws PersistenceException;

   /**
    * Upgrades the persistent storage schema.
    * @param upgrade The version upgrade.
    * @param state The upgrade state.
    * @param version The schema version. Updated by this method.
    */
   void upgrade(SchemaUpgrade upgrade, UpgradeState state, SchemaVersion version) throws PersistenceException;
}
