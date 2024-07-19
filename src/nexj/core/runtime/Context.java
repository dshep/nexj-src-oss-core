// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.security.Principal;
import java.util.Locale;
import java.util.TimeZone;

import nexj.core.meta.ContextMetadata;
import nexj.core.meta.PrivilegeSet;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Machine;
import nexj.core.util.StringTable;
import nexj.core.util.auth.SimplePrincipal;

/**
 * Interface implemented by application context objects.
 */
public interface Context
{
   // constants

   /**
    * Anonymous principal.
    */
   public final static Principal ANONYMOUS_PRINCIPAL = new SimplePrincipal("");

   /**
    * Initializes the invocation context.
    * @param principal The user security principal.
    * @param env The global environment.
    */
   void initialize(Principal principal, GlobalEnvironment env);

   /**
    * @return The context metadata.
    */
   ContextMetadata getContextMetadata();
   
   /**
    * @return The security principal of the user associated with the invocation context.
    */
   Principal getPrincipal();

   /**
    * Sets the scripting virtual machine.
    * This method is for INTERNAL USE ONLY.
    * @param machine The machine to set.
    */
   void setMachine(Machine machine);

   /**
    * @return The scripting virtual machine.
    */
   Machine getMachine();

   /**
    * Sets the invocation context locale.
    * @param locale The locale to set.
    */
   void setLocale(Locale locale);
   
   /**
    * Sets the invocation context locale.
    * @param sLocale The locale name.
    */
   void setLocale(String sLocale);
   
   /**
    * @return The invocation context locale.
    */
   Locale getLocale();

   /**
    * @return The invocation context locale name.
    */
   String getLocaleName();

   /**
    * Sets the invocation context time zone.
    * @param timeZone The time zone to set.
    */
   void setTimeZone(TimeZone timeZone);

   /**
    * Sets the invocation context time zone.
    * @param sTimeZone The time zone name.
    */
   void setTimeZone(String sTimeZone);
   
   /**
    * @return The invocation context time zone.
    */
   TimeZone getTimeZone();

   /**
    * @return The string table for the invocation context locale.
    */
   StringTable getStringTable();
   
   /**
    * Gets a localized string for the invocation context locale.
    * @param sName The string name.
    * @return The localized string.
    */
   String getString(String sName);

   /**
    * Formats a string according to the invocation context locale.
    * @param sName The string name. It must correspond to a string table
    * entry specifying a java.text.MessageFormat pattern. 
    * @param args The argument array. Can be null.
    * @return The formatted string.
    */
   String formatString(String sName, Object[] args);

   /**
    * Gets a per-context class instance.
    * @param clazz The class for which to get the instance.
    */
   Object getClassInstance(Class clazz);
   
   /**
    * @return The current user privilege set.
    */
   PrivilegeSet getPrivilegeSet();

   /**
    * @return The secure access flag.
    */
   boolean isSecure();
}
