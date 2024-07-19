// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.util.Iterator;
import java.util.Set;

import nexj.core.meta.ContextMetadata;
import nexj.core.meta.DocumentedNamedMetadataObject;
import nexj.core.meta.MetadataException;
import nexj.core.util.EmptyIterator;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashHolder;
import nexj.core.util.IdentityHashHolder;

/**
 * Relational database managed object metadata, e.g. table/view/SQLObject.
 */
public abstract class RelationalObject extends DocumentedNamedMetadataObject
{
   // associations

   /**
    * The collection of objects which must be present before this object can be present (lazy init).
    */
   protected Set/*<RelationalObject>*/ m_prerequisiteSet;

   /**
    * The relational schema which the object is registered with.
    */
   protected RelationalSchema m_schema;

   // constructors

   /**
    * Constructs the object.
    * @param schema The relational schema.
    */
   protected RelationalObject(RelationalSchema schema)
   {
      m_schema = schema;
   }

   // operations

   /**
    * @return The object owner name.
    */
   public abstract String getOwnerName();

   /**
    * @return The object name (without the owner name).
    */
   public abstract String getObjectName();

   /**
    * Sets the relational schema of this object.
    * @param schema The schema to set.
    */
   public void setSchema(RelationalSchema schema)
   {
      verifyNotReadOnly();
      m_schema = schema;
   }

   /**
    * @return The relational schema of this object.
    */
   public RelationalSchema getSchema()
   {
      return m_schema;
   }

   /**
    * Add specified existence relational prerequisite.
    * 
    * @param obj The object to add (not null).
    */
   public void addPrerequisite(RelationalObject obj)
   {
      verifyNotReadOnly();

      if (m_prerequisiteSet == null)
      {
         m_prerequisiteSet = new HashHolder/* <RelationalObject> */(2);
      }

      if (!m_prerequisiteSet.add(obj))
      {
         throw new MetadataException("err.meta.sql.prerequisiteDup",
            new Object[]{obj, getName()});
      }
   }

   /**
    * @return An iterator over all the set existence relational prerequisites.
    */
   public Iterator/*<RelationalObject>*/ getPrerequisiteIterator()
   {
      return (m_prerequisiteSet == null || m_prerequisiteSet.isEmpty())
             ? EmptyIterator.getInstance() : m_prerequisiteSet.iterator();
   }

   /**
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, nexj.core.util.ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      super.validate(metadata, warnings);

      // ensure there are no existence prerequisite loops
      if (m_prerequisiteSet != null)
      {
         validateNoLoop(new IdentityHashHolder/*<RelationalObject>*/(4));
      }
   }

   /**
    * Ensure there are no existence prerequisite loops.
    * @param seenSet Set to use for tracking already seen objects.
    */
   protected void validateNoLoop(Set/*<RelationalObject>*/ seenSet)
   {
      if (!seenSet.add(this))
      {
         throw new MetadataException("err.meta.sql.prerequisiteLoop", new Object[]{m_sName});
      }

      for (Iterator/*<RelationalObject>*/ itr = getPrerequisiteIterator(); itr.hasNext();)
      {
         ((RelationalObject)itr.next()).validateNoLoop(seenSet);
      }

      seenSet.remove(this);
   }

   /**
    * Clones the object (deep copy).
    * @see nexj.core.meta.MetadataObject#clone()
    */
   public Object clone()
   {
      RelationalObject obj = (RelationalObject)super.clone();

      if (m_prerequisiteSet != null)
      {
         obj.m_prerequisiteSet = (Set)((HashHolder)m_prerequisiteSet).clone();
      }

      return obj;
   }
}