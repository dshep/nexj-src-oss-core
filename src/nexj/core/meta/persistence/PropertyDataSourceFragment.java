// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence;

import nexj.core.meta.PropertyHolder;

/**
 * Fragment with property holder for custom fragment properties.
 */
public abstract class PropertyDataSourceFragment extends DataSourceFragment
{
   // associations

   /**
    * The data source fragment connection properties.
    */
   protected PropertyHolder m_props = new PropertyHolder();

   // constructors

   /**
    * Constructs a default (nameless) fragment.
    */
   public PropertyDataSourceFragment()
   {
      super();
   }

   /**
    * Constructs a fragment.
    * @param sName The fragment name.
    */
   public PropertyDataSourceFragment(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * Gets the data source fragment connection properties.
    * @return The connection property holder.
    */
   public PropertyHolder getPropertyHolder()
   {
      return m_props;
   }

   /**
    * Clones the fragment.
    * @param bProperties True to clone the properties; false to allocate
    * an empty property holder.
    * @see nexj.core.meta.MetadataObject#clone()
    */
   public Object clone(boolean bProperties)
   {
      PropertyDataSourceFragment fragment = (PropertyDataSourceFragment)super.clone();

      if (bProperties)
      {
         fragment.m_props = (PropertyHolder)m_props.clone();
      }
      else
      {
         fragment.m_props = new PropertyHolder();
      }

      return fragment;
   }

   /**
    * @see nexj.core.meta.MetadataObject#clone()
    */
   public Object clone()
   {
      return clone(true);
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();
      m_props.makeReadOnly();
   }
}
