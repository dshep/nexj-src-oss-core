// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence;

import java.util.Iterator;

import nexj.core.meta.Component;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.NamedMetadataObject;
import nexj.core.persistence.PersistenceException;
import nexj.core.util.EmptyIterator;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.SingletonIterator;
import nexj.core.util.StringUtil;

/**
 * Base class for data sources.
 */
public abstract class DataSource extends NamedMetadataObject
{
   // attributes

   /**
    * The maximum number of instances that can be returned by read().
    * 0 means unlimited.
    */
   protected int m_nReadLimit = 4096;

   /**
    * The readable flag.
    */
   protected boolean m_bReadable;

   /**
    * The creatable flag.
    */
   protected boolean m_bCreatable;

   /**
    * The updatable flag.
    */
   protected boolean m_bUpdatable;

   /**
    * The deletable flag.
    */
   protected boolean m_bDeletable;

   /**
    * The executable flag.
    */
   protected boolean m_bExecutable;

   /**
    * The joinable flag.
    */
   protected boolean m_bJoinable;

   // associations

   /**
    * The data source type.
    */
   protected DataSourceType m_type;

   /**
    * The data source schema.
    */
   protected Schema m_schema;

   /**
    * The data source adapter.
    */
   protected DataSourceAdapter m_adapter;

   /**
    * The connection component.
    */
   protected Component m_component;

   /**
    * The default fragment.
    */
   protected DataSourceFragment m_defaultFragment;

   /**
    * The fragment map: DataSourceFragment[String].
    */
   protected Lookup m_fragmentMap;

   // constructors

   /**
    * Constructs the data source connection.
    * @param sName The data source connection name.
    */
   public DataSource(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * Sets the data source type.
    * @param type The data source type to set.
    */
   public void setType(DataSourceType type)
   {
      verifyNotReadOnly();
      m_type = type;

      if (type != null && m_adapter == null && type.getAdapterCount() == 1)
      {
         m_adapter = (DataSourceAdapter)type.getAdapterIterator().next();
      }
   }

   /**
    * @return The data source type.
    */
   public DataSourceType getType()
   {
      return m_type;
   }
   
   /**
    * Sets the data source schema.
    * @param schema The data source schema to set.
    */
   public void setSchema(Schema schema)
   {
      verifyNotReadOnly();
      m_schema = schema;
      
      if (schema != null)
      {
         schema.setDataSource(this);
      }
   }

   /**
    * @return The data source schema.
    */
   public Schema getSchema()
   {
      return m_schema;
   }

   /**
    * Sets the maximum number of instances that can be returned by read().
    * @param nReadLimit The maximum number of instances that can be returned by read() to set.
    */
   public void setReadLimit(int nReadLimit)
   {
      verifyNotReadOnly();
      m_nReadLimit = nReadLimit;
   }

   /**
    * @return The maximum number of instances that can be returned by read().
    */
   public int getReadLimit()
   {
      return m_nReadLimit;
   }
   
   /**
    * Sets the readable flag.
    * @param bReadable The readable flag to set.
    */
   public void setReadable(boolean bReadable)
   {
      verifyNotReadOnly();
      m_bReadable = bReadable;
   }

   /**
    * @return The readable flag.
    */
   public boolean isReadable()
   {
      return m_bReadable;
   }

   /**
    * Sets the creatable flag.
    * @param bCreatable The creatable flag to set.
    */
   public void setCreatable(boolean bCreatable)
   {
      verifyNotReadOnly();
      m_bCreatable = bCreatable;
   }

   /**
    * @return The creatable flag.
    */
   public boolean isCreatable()
   {
      return m_bCreatable;
   }
   
   /**
    * Sets the updatable flag.
    * @param bUpdatable The updatable flag to set.
    */
   public void setUpdatable(boolean bUpdatable)
   {
      verifyNotReadOnly();
      m_bUpdatable = bUpdatable;
   }

   /**
    * @return The updatable flag.
    */
   public boolean isUpdatable()
   {
      return m_bUpdatable;
   }
   
   /**
    * Sets the deletable flag.
    * @param bDeletable The deletable flag to set.
    */
   public void setDeletable(boolean bDeletable)
   {
      verifyNotReadOnly();
      m_bDeletable = bDeletable;
   }

   /**
    * @return The deletable flag.
    */
   public boolean isDeletable()
   {
      return m_bDeletable;
   }
   
   /**
    * Sets the executable flag.
    * @param bExecutable The executable flag to set.
    */
   public void setExecutable(boolean bExecutable)
   {
      verifyNotReadOnly();
      m_bExecutable = bExecutable;
   }

   /**
    * @return The executable flag.
    */
   public boolean isExecutable()
   {
      return m_bExecutable;
   }

   /**
    * @return True if the data source is readable, creatable,
    * updatable, deletable or executable.
    */
   public boolean isEnabled()
   {
      return m_adapter != null &&
         (m_bReadable || m_bCreatable || m_bUpdatable || m_bDeletable || m_bExecutable);
   }

   /**
    * Sets the joinable flag.
    * @param bJoinable The joinable flag to set.
    */
   public void setJoinable(boolean bJoinable)
   {
      verifyNotReadOnly();
      m_bJoinable = bJoinable;
   }

   /**
    * @return The joinable flag.
    */
   public boolean isJoinable()
   {
      return m_bJoinable;
   }
   
   /**
    * Sets the data source adapter.
    * @param adapter The data source adapter to set.
    */
   public void setAdapter(DataSourceAdapter adapter)
   {
      verifyNotReadOnly();
      m_adapter = adapter;
   }

   /**
    * @return The data source adapter.
    */
   public DataSourceAdapter getAdapter()
   {
      return m_adapter;
   }

   /**
    * Sets the connection component.
    * @param component The connection component to set.
    */
   public void setComponent(Component component)
   {
      verifyNotReadOnly();
      m_component = component;
   }

   /**
    * @return The connection component.
    */
   public Component getComponent()
   {
      if (m_component == null)
      {
         throw new PersistenceException("err.persistence.dataSourceDisabled", new Object[]{getName()});
      }

      return m_component;
   }

   /**
    * @return The default fragment.
    */
   public DataSourceFragment getDefaultFragment()
   {
      return m_defaultFragment;
   }

   /**
    * Adds a new fragment to the data source.
    * @param fragment The fragment to add.
    * @throws MetadataException if a fragment
    * with the same name already exists.
    */
   public void addFragment(DataSourceFragment fragment)
   {
      verifyNotReadOnly();

      String sName = fragment.getName();

      if (StringUtil.isEmpty(sName))
      {
         if (m_defaultFragment != null)
         {
            throw new MetadataException("err.meta.persistence.fragmentDup",
               new Object[]{"", getName()});
         }

         m_defaultFragment = fragment;

         if (m_fragmentMap != null)
         {
            m_fragmentMap.put("", fragment);
         }
      }
      else
      {
         if (m_fragmentMap == null)
         {
            m_fragmentMap = new HashTab(4);

            if (m_defaultFragment != null)
            {
               m_fragmentMap.put("", m_defaultFragment);
            }
         }

         Object oldFragment = m_fragmentMap.put(sName, fragment);

         if (oldFragment != null)
         {
            m_fragmentMap.put(sName, oldFragment);

            throw new MetadataException("err.meta.persistence.fragmentDup",
               new Object[]{sName, getName()});
         }
      }

      fragment.setDataSource(this);
   }

   /**
    * Gets a fragment by name.
    * @param sName The fragment name.
    * @return The fragment object.
    * @throws MetadataLookupException if the fragment does not exist.
    */
   public DataSourceFragment getFragment(String sName)
   {
      DataSourceFragment fragment = findFragment(sName);

      if (fragment != null)
      {
         return fragment;
      }

      throw new MetadataLookupException("err.meta.persistence.fragmentLookup",
         (sName == null) ? "" : sName, this);
   }

   /**
    * Finds a fragment by name.
    * @param sName The fragment name.
    * @return The fragment object, or null if not found.
    * @throws MetadataLookupException if the fragment does not exist.
    */
   public DataSourceFragment findFragment(String sName)
   {
      if (StringUtil.isEmpty(sName))
      {
         return m_defaultFragment;
      }

      if (m_fragmentMap != null)
      {
         return (DataSourceFragment)m_fragmentMap.get(sName);
      }

      return null;
   }

   /**
    * @return The fragment count.
    */
   public int getFragmentCount()
   {
      if (m_fragmentMap == null)
      {
         return (m_defaultFragment == null) ? 0 : 1;
      }
      
      return m_fragmentMap.size();
   }

   /**
    * @return An iterator for the contained fragment objects.
    */
   public Iterator getFragmentIterator()
   {
      if (m_fragmentMap == null)
      {
         if (m_defaultFragment == null)
         {
            return EmptyIterator.getInstance();
         }

         return new SingletonIterator(m_defaultFragment);
      }

      return m_fragmentMap.valueIterator();
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      for (Iterator itr = getFragmentIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      super.makeReadOnly();
   }
}
