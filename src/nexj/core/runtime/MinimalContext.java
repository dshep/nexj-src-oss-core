// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.io.IOException;
import java.security.Principal;
import java.text.MessageFormat;
import java.util.Locale;
import java.util.TimeZone;

import nexj.core.meta.ContextMetadata;
import nexj.core.meta.PrivilegeSet;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.Machine;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.SoftHashTab;
import nexj.core.util.StringTable;
import nexj.core.util.UncheckedException;

/**
 * A context that has no dependencies outside of the scripting engine.
 */
public class MinimalContext implements Context
{
   /**
    * The machine for this context
    */
   protected Machine m_machine;

   /**
    * The locale
    */
   protected Locale m_locale;

   /**
    * The principal initialized with
    */
   protected Principal m_principal;

   /**
    * The time zone
    */
   protected TimeZone m_timeZone;

   /**
    * The privilege set, null by default.
    */
   protected PrivilegeSet m_privilegeSet;

   /**
    * Instance map
    * 
    * @see #getClassInstance(Class)
    */
   protected Lookup m_cacheMap;

   // operations

   /**
    * @see nexj.core.runtime.Context#getStringTable()
    */
   public StringTable getStringTable()
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.runtime.Context#isSecure()
    */
   public boolean isSecure()
   {
      return false;
   }

   /**
    * @see nexj.core.runtime.Context#getContextMetadata()
    */
   public ContextMetadata getContextMetadata()
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.runtime.Context#initialize(java.security.Principal,
    *      nexj.core.scripting.GlobalEnvironment)
    */
   public void initialize(Principal principal, GlobalEnvironment env)
   {
      m_principal = (principal == null) ? ANONYMOUS_PRINCIPAL : principal;

      if (m_machine == null || env != null)
      {
         if (env == null)
         {
            env = new GlobalEnvironment();
         }

         m_machine = new Machine(env, this);
      }

      try
      {
         Intrinsic.load("library:scheme", "/nexj/core/meta/sys/scheme.scm", m_machine);
      }
      catch (IOException e)
      {
         ObjUtil.rethrow(e);
      }
   }
   
   /**
    * @see nexj.core.runtime.Context#formatString(java.lang.String,
    *      java.lang.Object[])
    */
   public String formatString(String sName, Object[] argArray)
   {
      return MessageFormat.format(sName, argArray);
   }

   /**
    * @see nexj.core.runtime.Context#getString(java.lang.String)
    */
   public String getString(String name)
   {
      return formatString(name, null);
   }

   /**
    * @see nexj.core.runtime.Context#getClassInstance(java.lang.Class)
    */
   public Object getClassInstance(Class clazz)
   {
      Object instance = null;

      if (m_cacheMap == null)
      {
         m_cacheMap = new SoftHashTab(4);
      }
      else
      {
         instance = m_cacheMap.get(clazz);
      }

      if (instance == null)
      {
         try
         {
            instance = clazz.newInstance();
         }
         catch (Exception e)
         {
            throw new UncheckedException("err.runtime.class", new Object[]
            {
               clazz.getName()
            }, e);
         }

         m_cacheMap.put(clazz, instance);
      }

      return instance;
   }

   /**
    * @see nexj.core.runtime.Context#getLocale()
    */
   public Locale getLocale()
   {
      return m_locale;
   }

   /**
    * @see nexj.core.runtime.Context#setLocale(java.util.Locale)
    */
   public void setLocale(Locale locale)
   {
      m_locale = locale;
   }

   /**
    * @see nexj.core.runtime.Context#setLocale(java.lang.String)
    */
   public void setLocale(String sLocale)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.runtime.Context#getLocaleName()
    */
   public String getLocaleName()
   {
      return (m_locale == null) ? null : m_locale.toString();
   }

   /**
    * @see nexj.core.runtime.Context#getTimeZone()
    */
   public TimeZone getTimeZone()
   {
      return m_timeZone;
   }

   /**
    * @see nexj.core.runtime.Context#setTimeZone(java.util.TimeZone)
    */
   public void setTimeZone(TimeZone timeZone)
   {
      m_timeZone = timeZone;
   }

   /**
    * @see nexj.core.runtime.Context#setTimeZone(java.lang.String)
    */
   public void setTimeZone(String sTimeZone)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.runtime.Context#setMachine(nexj.core.scripting.Machine)
    */
   public void setMachine(Machine machine)
   {
      m_machine = machine;
   }

   /**
    * @see nexj.core.runtime.Context#getMachine()
    */
   public Machine getMachine()
   {
      return m_machine;
   }

   /**
    * @see nexj.core.runtime.Context#getPrincipal()
    */
   public Principal getPrincipal()
   {
      return m_principal;
   }

   /**
    * @see nexj.core.runtime.Context#getPrivilegeSet()
    */
   public PrivilegeSet getPrivilegeSet()
   {
      return m_privilegeSet;
   }

   public void setPrivilegeSet(PrivilegeSet set)
   {
      m_privilegeSet = set;
   }
}