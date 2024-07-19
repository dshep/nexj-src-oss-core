// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import nexj.core.util.ObjUtil;

/**
 * Schema version information.
 */
public class SchemaVersion implements Cloneable
{
   // attributes
   
   /**
    * The metadata namespace.
    */
   protected String m_sNamespace;

   /**
    * The metadata version.
    */
   protected String m_sVersion;

   /**
    * The upgrade step ordinal. -1 means that no upgrade is needed.
    */
   protected int m_nStep;

   /**
    * The upgradable flag.
    */
   protected boolean m_bUpgradable;

   /**
    * The test data flag.
    */
   protected boolean m_bTest;

   // operations
   
   /**
    * Sets the metadata namespace.
    * @param sNamespace The metadata namespace to set.
    */
   public void setNamespace(String sNamespace)
   {
      m_sNamespace = sNamespace;
   }

   /**
    * @return The metadata namespace.
    */
   public String getNamespace()
   {
      return m_sNamespace;
   }
   
   /**
    * Sets the metadata version.
    * @param sVersion The metadata version to set.
    */
   public void setVersion(String sVersion)
   {
      m_sVersion = sVersion;
   }

   /**
    * @return The metadata version.
    */
   public String getVersion()
   {
      return m_sVersion;
   }

   /**
    * Sets the upgrade step ordinal.
    * @param nStep The upgrade step ordinal to set.
    */
   public void setStep(int nStep)
   {
      m_nStep = nStep;
   }

   /**
    * @return The upgrade step ordinal.
    */
   public int getStep()
   {
      return m_nStep;
   }

   /**
    * Sets the upgradable flag.
    * @param bUpgradable The upgradable flag to set.
    */
   public void setUpgradable(boolean bUpgradable)
   {
      m_bUpgradable = bUpgradable;
   }

   /**
    * @return The upgradable flag.
    */
   public boolean isUpgradable()
   {
      return m_bUpgradable;
   }

   /**
    * Sets the test data flag.
    * @param bTest The test data flag to set.
    */
   public void setTest(boolean bTest)
   {
      m_bTest = bTest;
   }

   /**
    * @return The test data flag.
    */
   public boolean isTest()
   {
      return m_bTest;
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         return super.clone();
      }
      catch (CloneNotSupportedException e)
      {
         return null;
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(128);

      buf.append(ObjUtil.getShortClassName(this));
      buf.append("(namespace=");
      buf.append(m_sNamespace);
      buf.append(", version=");
      buf.append(m_sVersion);
      buf.append(", step=");
      buf.append(m_nStep);
      buf.append(", upgradable=");
      buf.append(m_bUpgradable);
      buf.append(", test=");
      buf.append(m_bTest);
      buf.append(')');

      return buf.toString();
   }
}
