// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.persistence.Query;

/**
 * Represents a joined table in the query.
 */
public class SQLJoin
{
   // attributes
   
   /**
    * The table alias.
    */
   public String alias;
   
   /**
    * Enablement flag. If false, then the table will
    * be skipped during query generation.
    */
   public boolean isEnabled;

   /**
    * Inner join flag.
    */
   public boolean isInner;

   // associations

   /**
    * The next join for the same query node.
    * Null if this is the last one.
    */
   public SQLJoin next;

   /**
    * The containing query.
    */
   public Query query;
   
   /**
    * The parent join. Null if this is a root query node join.
    */
   public SQLJoin parent;

   /**
    * The joined table.
    */
   public Table table;

   /**
    * The parent join key.
    */
   public Index sourceKey;

   /**
    * This table join key.
    */
   public Index destinationKey;

   // constructors

   /**
    * Creates a join.
    * @param query The containing query.
    * @param parent The join parent. Can be null for a root query node join.
    * @param table The join table.
    * @param alias The table alias.
    */
   public SQLJoin(Query query, SQLJoin parent, Table table, String alias)
   {
      this.query = query;
      this.parent = parent;
      this.table = table;
      this.alias = alias;
   }

   /**
    * Creates a join.
    * @param query The containing query.
    * @param parent The join parent. Can be null for a root query node join.
    * @param table The join table.
    * @param sourceKey The join source key.
    * @param destinationKey The join destination key.
    * @param isInner True if this is an inner join.
    * @param alias The table alias.
    */
   public SQLJoin(Query query, SQLJoin parent, Table table, Index sourceKey,
      Index destinationKey, boolean isInner, String alias)
   {
      this.query = query;
      this.parent = parent;
      this.table = table;
      this.sourceKey = sourceKey;
      this.destinationKey = destinationKey;
      this.next = null;
      this.isInner = isInner;
      this.alias = alias;
   }

   // operations

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(64);

      buf.append("SQLJoin(");

      if (this.alias != null)
      {
         buf.append("alias=");
         buf.append(this.alias);
         buf.append(", ");
      }

      if (this.table != null)
      {
         buf.append("table=");
         buf.append(this.table.getName());
         buf.append(", ");
      }

      if (this.parent != null)
      {
         buf.append("parent=");

         if (this.parent.table != null)
         {
            buf.append(this.parent.table.getName());
         }

         buf.append(", ");
      }

      if (this.sourceKey != null)
      {
         buf.append("sourceKey=");
         buf.append(this.sourceKey.getName());
         buf.append(", ");
      }

      if (this.destinationKey != null)
      {
         buf.append("destinationKey=");
         buf.append(this.destinationKey.getName());
         buf.append(", ");
      }

      buf.append("isInner=");
      buf.append(this.isInner);

      if (this.next != null)
      {
         buf.append(", next=");

         if (this.next.table != null)
         {
            buf.append(this.next.table.getName());
         }
      }

      buf.append(')');

      return buf.toString();
   }
}
