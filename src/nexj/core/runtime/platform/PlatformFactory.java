// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.platform;

import java.util.Locale;

import nexj.core.runtime.Factory;
import nexj.core.util.J2EEUtil;

/**
 * Factory for platform-dependent components. 
 */
public class PlatformFactory implements Factory
{
   // attributes
   
   /**
    * The class object.
    */
   private Class m_class;

   // operations
   
   /**
    * Sets the class template.
    * @param sClassTemplate The class template to set.
    */
   public void setClassTemplate(String sClassTemplate) throws Exception
   {
      String sPlatform = J2EEUtil.getPlatformName();

      sClassTemplate = sClassTemplate.replaceAll("\\$\\{Platform\\}", sPlatform);
      sClassTemplate = sClassTemplate.replaceAll("\\$\\{platform\\}", sPlatform.toLowerCase(Locale.ENGLISH));

      m_class = Class.forName(sClassTemplate);
   }
   
   /**
    * Creates the object.
    */
   public Object create() throws Exception
   {
      return m_class.newInstance();
   }
}
