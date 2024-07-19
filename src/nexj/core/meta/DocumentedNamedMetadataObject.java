// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.util.StringUtil;

/**
 * Base class for all named metadata objects with documentation.
 */
public abstract class DocumentedNamedMetadataObject extends NamedMetadataObject implements Documented
{
   // attributes
   
   /**
    * The metadata object description.
    */
   protected String m_sDescription;
   
   // constructors
   
   /**
    * Constructs the metadata object.
    * @param sName The object name.
    */
   public DocumentedNamedMetadataObject(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the metadata object.
    */
   public DocumentedNamedMetadataObject()
   {
      super();
   }

   // operations
   
   /**
    * @see nexj.core.meta.Documented#setDescription(java.lang.String)
    */
   public void setDescription(String sDescription)
   {
      verifyNotReadOnly();
      
      if (StringUtil.isEmpty(sDescription))
      {
         sDescription = null;
      }

      m_sDescription = sDescription;
   }

   /**
    * @see nexj.core.meta.Documented#getDescription()
    */
   public String getDescription()
   {
      return m_sDescription;
   }
}
