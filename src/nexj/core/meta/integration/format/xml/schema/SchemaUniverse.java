// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.schema;

import java.util.Iterator;

import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * The universe of schemas. Provides scheme namespace prefix resolution functionality.
 */
public class SchemaUniverse
{
   // associations

   /**
    * Map of schema URI to schema: Schema[String].
    */
   protected Lookup m_uriSchemaMap = new HashTab();

   /**
    * Map of schema prefix to Schema: Schema[String].
    */
   protected Lookup m_prefixSchemaMap;

   /**
    * Map of Schema to schema prefix: String[Schema].
    */
   protected Lookup m_schemaPrefixMap;

   // operations

   /**
    * Creates a schema with the given namespace, or returns it if it already exists.
    * @param sURI The schema namespace; null or empty string for the no-namespace namespace.
    * @param sPreferredPrefix The preferred prefix; null if unknown.
    * @return The schema with namespace sURI.
    */
   public Schema getSchema(String sURI, String sPreferredPrefix)
   {
      sURI = (sURI == null) ? "" : sURI;

      Schema schema = (Schema)m_uriSchemaMap.get(sURI);

      if (schema != null)
      {
         if (schema.getPreferredPrefix() == null && !StringUtil.isEmpty(sPreferredPrefix))
         {
            schema.setPreferredPrefix(sPreferredPrefix);
         }

         return schema;
      }

      schema = new Schema(sURI);
      schema.setPreferredPrefix(sPreferredPrefix);
      m_uriSchemaMap.put(sURI, schema);

      return schema;
   }

   /**
    * Adds a schema to the universe.
    * @param schema The schema to add.
    */
   public void addSchema(Schema schema)
   {
      Schema oldSchema = (Schema)m_uriSchemaMap.put(schema.getURI(), schema);

      if (oldSchema != null && oldSchema != schema)
      {
         throw new IllegalStateException("Schema already defined for URI \"" + schema.getURI() + '"');
      }
   }

   /**
    * Gets the prefix to use for a schema.
    * @param schema The schema.
    * @return Its prefix.
    */
   public String getPrefix(Schema schema)
   {
      return (String)m_schemaPrefixMap.get(schema);
   }

   /**
    * Finds the schema identified by a prefix.
    * @param sPrefix The schema prefix.
    * @return The schema.
    */
   public Schema findSchema(String sPrefix)
   {
      return (Schema)m_prefixSchemaMap.get(sPrefix);
   }

   /**
    * Finds the schema identified by namespace.
    * @param sURI The schema namespace.
    * @return The schema.
    */
   public Schema findSchemaByURI(String sURI)
   {
      Schema schema = (Schema)m_uriSchemaMap.get(sURI);

      if (schema != null)
      {
         return schema;
      }

      return schema;
   }

   /**
    * Gets an iterator over the schema prefixes.
    * @return A schema prefix iterator.
    */
   public Iterator getPrefixIterator()
   {
      return m_prefixSchemaMap.iterator();
   }

   /**
    * Gets the number of schema prefixes that are defined.
    * @return The schema prefix count.
    */
   public int getPrefixCount()
   {
      return m_prefixSchemaMap.size();
   }

   /**
    * Gets the number of schemas in the universe.
    * @return The schema count.
    */
   public int getSchemaCount()
   {
      return m_uriSchemaMap.size();
   }

   /**
    * Gets an iterator over all the schemas in the universe.
    * @return A schema iterator.
    */
   public Iterator getSchemaIterator()
   {
      return m_uriSchemaMap.valueIterator();
   }

   /**
    * Gets the qualified name (e.g. "prefix:name") of an item.
    * @param item The schema item.
    * @return The qualified name string; null if unknown, its name alone if no prefix.
    */
   public String getQualifiedName(SchemaItem item)
   {
      if (!item.isTopLevel())
      {
         return null;
      }

      if (!StringUtil.isEmpty(item.getSchema().getURI()))
      {
         String sPrefix = getPrefix(item.getSchema());
         StringBuilder buf = new StringBuilder(sPrefix.length() + 1 + item.getName().length());

         buf.append(sPrefix);
         buf.append(':');
         buf.append(item.getName());

         return buf.toString();
      }

      return item.getName();
   }

   /**
    * Computes the prefixes for all the schemas in the universe from their preferred prefixes.
    */
   public void resolvePrefixes()
   {
      if (m_prefixSchemaMap == null)
      {
         m_prefixSchemaMap = new HashTab(m_uriSchemaMap.size());
      }

      if (m_schemaPrefixMap == null)
      {
         m_schemaPrefixMap = new HashTab(m_uriSchemaMap.size());
      }

      for (Iterator itr = m_uriSchemaMap.valueIterator(); itr.hasNext(); )
      {
         Schema schema = (Schema)itr.next();

         if (StringUtil.isEmpty(schema.getURI()))
         {
            continue;
         }

         String sPreferredPrefix = (StringUtil.isEmpty(schema.getPreferredPrefix())) ? SysUtil.NAMESPACE : schema.getPreferredPrefix();
         String sPrefix = sPreferredPrefix;
         int nAttempt = 0;
         Schema oldSchema;

         while ((oldSchema = (Schema)m_prefixSchemaMap.put(sPrefix, schema)) != null && oldSchema != schema)
         {
            m_prefixSchemaMap.put(sPrefix, oldSchema);
            sPrefix = sPreferredPrefix + '-' + nAttempt++;
         }

         m_schemaPrefixMap.put(schema, sPrefix);
      }
   }
}
