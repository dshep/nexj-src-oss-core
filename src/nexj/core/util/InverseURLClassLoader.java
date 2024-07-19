// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * This class loader should be used to ensure that classes are loaded from the jars on
 * the application classpath as opposed to the jars used by the bootstrap ClassLoader.
 */
public class InverseURLClassLoader extends URLClassLoader
{
   // associations
   
   /**
    * A regexp pattern which specifies which classes to load with this class loader.
    * All others will be delegated to the caller class loader.
    */
   protected Pattern m_classesPattern;
   
   /**
    * Maps of caller class loader and pattern to an inverse class loader instance:
    * InverseURLClassLoader[ClassLoader][String].
    */
   protected static Lookup2D s_classLoaderMap = new HashTab2D();

   /**
    * An array of URLs holding all the classpath entries from java.class.path system property.
    */
   protected static final URL[] s_classpathURLArray;

   // Initializes the s_classpathURLs with entries from the classpath.
   static
   {
      String[] classpathArray = System.getProperty("java.class.path").split(File.pathSeparator);
      List classpathURLList = new ArrayList(classpathArray.length);

      for (int i = 0; i < classpathArray.length; i++)
      {
         try
         {
            classpathURLList.add(new File(classpathArray[i]).toURI().toURL());
         }
         catch (MalformedURLException e)
         {
         }
      }

      s_classpathURLArray = new URL[classpathURLList.size()];
      classpathURLList.toArray(s_classpathURLArray);
   }

   // constructors
   
   /**
    * Constructs the NexJ class loader.  Private to enforce access through getInstance() only.
    * @param callerClassLoader The ClassLoader of the caller.  This will be used to set the parent ClassLoader for delegation.
    * @params ClassesRegexp Regular expression indicating which classes should be loaded by this ClassLoader.  All non matching classes will be delegated to the caller's ClassLoader.  If null is supplied, all classes will be loaded by this ClassLoader.
    */
   private InverseURLClassLoader(ClassLoader callerClassLoader, String sClassesRegExp)
   {
      super(s_classpathURLArray, callerClassLoader);
      m_classesPattern = Pattern.compile(sClassesRegExp);
   }

   // operations
   
   /**
    * Creates or retrieves an existing instance of the inverse class loader.
    * @param callerClassLoader The ClassLoader of the caller.
    * @param sClassesRegExp A regexp for determining which classes should be loaded by this ClassLoader.
    * All non-matching classes will be delegated to callerClassLoader.
    * If null is supplied, all classes will be loaded by this ClassLoader.
    * @return The created instance.
    */
   public static InverseURLClassLoader getInstance(ClassLoader callerClassLoader, String sClassesRegExp)
   {
      synchronized (s_classLoaderMap)
      {
         InverseURLClassLoader classLoader = (InverseURLClassLoader)s_classLoaderMap.get(callerClassLoader, sClassesRegExp);

         if (classLoader == null)
         {
            classLoader = new InverseURLClassLoader(callerClassLoader, sClassesRegExp);
            s_classLoaderMap.put(callerClassLoader, sClassesRegExp, classLoader);
         }

         return classLoader;
      }
   }
   
   /**
    * @see java.lang.ClassLoader#loadClass(java.lang.String, boolean)
    * 
    * This method differs from the superclass in that if the class being loaded is matched by
    * m_classesPattern (or if m_classesPattern is null) it will try to load the class itself
    * before delegating to the parent (the caller's) ClassLoader.
    */
   protected Class loadClass(String sClassName, boolean bResolve) throws ClassNotFoundException
   {
      if (m_classesPattern != null && !m_classesPattern.matcher(sClassName).matches())
      {
         return super.loadClass(sClassName, bResolve);
      }

      Class loadedClass = super.findLoadedClass(sClassName);

      if (loadedClass != null)
      {
         return loadedClass;
      }

      try
      {
         return super.findClass(sClassName);
      }
      catch (ClassNotFoundException e)
      {
         return super.loadClass(sClassName, bResolve);
      }
   }
}