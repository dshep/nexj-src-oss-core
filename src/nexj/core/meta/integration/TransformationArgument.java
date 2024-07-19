// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import nexj.core.meta.MetadataException;
import nexj.core.meta.NamedMetadataObject;
import nexj.core.scripting.Function;
import nexj.core.util.Undefined;

/**
 * Transformation argument representing a source path.
 */
public class TransformationArgument extends NamedMetadataObject
{
   // attributes

   /**
    * The start level.
    */
   protected int m_nStartLevel;

   /**
    * The null flag. True to map null values; false to ignore them.
    */
   protected boolean m_bNull;

   // associations

   /**
    * The transformation source.
    */
   protected TransformationSource m_source;

   /**
    * The source mapping.
    */
   protected TransformationMapping m_mapping;

   /**
    * The default value.
    */
   protected Object m_defaultValue = Undefined.VALUE;

   /**
    * The default value function.
    */
   protected Function m_defaultValueFunction;

   // constructors

   /**
    * Constructs the metadata object.
    * @param sName The argument name.
    */
   public TransformationArgument(String sName)
   {
      super(sName);
   }

   // operations
   
   /**
    * Sets the transformation source.
    * @param source The transformation source to set.
    */
   public void setSource(TransformationSource source)
   {
      verifyNotReadOnly();
      m_source = source;
   }

   /**
    * @return The transformation source.
    */
   public TransformationSource getSource()
   {
      return m_source;
   }

   /**
    * Sets the transformation mapping.
    * @param mapping The transformation mapping to set.
    */
   public void setMapping(TransformationMapping mapping)
   {
      verifyNotReadOnly();
      m_mapping = mapping;
   }

   /**
    * Sets the source mapping by name. If there is no mapping with the name
    * in transformation, then the base transformations are examined.
    * @param transformation The transformation to find the mapping.
    * @param sMapping The name of the mapping.
    * @throws MetadataException If there is no mapping of the given name
    * in "transformation" or any of its base transformations.
    */
   public void setMapping(Transformation transformation, String sMapping)
   {
      verifyNotReadOnly();

      Transformation root = transformation;

      while (root != null)
      {
         TransformationMapping mapping = root.findMapping(sMapping);

         if (mapping != null)
         {
            m_mapping = mapping;

            return;
         }

         root = root.getBaseTransformation();
      }

      throw new MetadataException("err.meta.transformation.mappingLookup",
         new Object[]{sMapping, transformation});
   }

   /**
    * @return The transformation mapping.
    */
   public TransformationMapping getMapping()
   {
      return m_mapping;
   }

   /**
    * Sets the null flag.
    * @param bNull The null flag to set. True to map null values; false to ignore them.
    */
   public void setNull(boolean bNull)
   {
      verifyNotReadOnly();
      m_bNull = bNull;

      if (bNull && m_source != null)
      {
         m_source.setNull(true);
      }
   }

   /**
    * @return The null flag. True to map null values; false to ignore them.
    */
   public boolean isNull()
   {
      return m_bNull;
   }

   /**
    * Sets the default value.
    * @param defaultValue The default value to set.
    */
   public void setDefaultValue(Object defaultValue)
   {
      verifyNotReadOnly();
      m_defaultValue = defaultValue;

      if (defaultValue != Undefined.VALUE && m_source != null)
      {
         m_source.setDefault(true);
      }
   }

   /**
    * @return The default value.
    */
   public Object getDefaultValue()
   {
      return m_defaultValue;
   }

   /**
    * Sets the default value function.
    * @param function The default value function.
    */
   public void setDefaultValueFunction(Function function)
   {
      verifyNotReadOnly();
      m_defaultValueFunction = function;
   }
   
   /**
    * @return The default value function.
    */
   public Function getDefaultValueFunction()
   {
      return m_defaultValueFunction;
   }

   /**
    * @return True if the default value is supplied for a missing value.
    */
   public boolean isDefault()
   {
      return m_defaultValue != Undefined.VALUE;
   }

   /**
    * Sets the start level.
    * @param nStartLevel The start level to set.
    */
   public void setStartLevel(int nStartLevel)
   {
      verifyNotReadOnly();
      m_nStartLevel = nStartLevel;
   }

   /**
    * @return The start level.
    */
   public int getStartLevel()
   {
      return m_nStartLevel;
   }

   /**
    * @see nexj.core.meta.NamedMetadataObject#toString()
    */
   public String toString()
   {
      return super.toString() + " from " + m_source;
   }
}
