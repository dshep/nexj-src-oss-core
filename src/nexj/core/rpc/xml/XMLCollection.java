// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.util.Collection;
import java.util.Collections;

import nexj.core.meta.Metaclass;

/**
 * Collection.
 */
public class XMLCollection
{
   // attributes

   /**
    * The type name.
    */
   protected String m_sType;

   // associations

   /**
    * The metaclass.
    */
   protected Metaclass m_metaclass;
   
   /**
    * The collection.
    */
   protected Collection m_collection;

   // constructors

   /**
    * Constructs the collection response.
    */
   public XMLCollection()
   {
   }

   /**
    * Constructs the collection response.
    * @param metaclass The metaclass.
    * @param collection The collection.
    * @param sType The type name.
    */
   public XMLCollection(Metaclass metaclass, Collection collection, String sType)
   {
      setMetaclass(metaclass);
      setCollection(collection);
      setType(sType);
   }

   // operations

   /**
    * Sets the metaclass.
    * @param metaclass The metaclass to set.
    */
   public void setMetaclass(Metaclass metaclass)
   {
      m_metaclass = metaclass;
   }

   /**
    * @return The metaclass.
    */
   public Metaclass getMetaclass()
   {
      return m_metaclass;
   }

   /**
    * Sets the collection.
    * @param collection The collection to set.
    */
   public void setCollection(Collection collection)
   {
      m_collection = (collection != null) ? collection : Collections.EMPTY_LIST;
   }

   /**
    * @return The collection.
    */
   public Collection getCollection()
   {
      return m_collection;
   }

   /**
    * Sets the type name.
    * @param sType The type name to set.
    */
   public void setType(String sType)
   {
      m_sType = sType;
   }

   /**
    * @return The type name.
    */
   public String getType()
   {
      return m_sType;
   }
   
   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "XMLCollection(class=" + m_metaclass + ", collection=" + m_collection + ")";
   }
}
