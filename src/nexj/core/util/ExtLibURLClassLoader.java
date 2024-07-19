// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.net.URL;
import java.net.URLClassLoader;

/**
 * The ClassLoader allows new folders or JAR files to be added to the classpath at
 * runtime.
 */
public class ExtLibURLClassLoader extends URLClassLoader
{
   // associations

   /**
    * Map of generic control to the new class loader with an extended classpath.
    */
   protected static Lookup s_classLoaderMap = new WeakHashTab();
   
   // attributes
   
   protected Lookup m_classDataMap = new HashTab();

   // constructors

   /**
    * Constructs the NexJ class loader.  Private to enforce access through getInstance() only.
    * @param parentClassLoader The parent classloader to delegate class loading.
    */
   private ExtLibURLClassLoader(ClassLoader parentClassLoader)
   {
      super(new URL[] { }, parentClassLoader);
   }

   // operations
   
   /**
    * Creates or retrieves an existing instance of the class loader.
    * @param meta The control that the classloader is associated to for loading external libraries.
    * @param parentClassLoader The parent classloader to delegate class loading.
    * All non-matching classes will be delegated to callerClassLoader.
    * If null is supplied, all classes will be loaded by this ClassLoader.
    * @return The created instance.
    */
   public static ExtLibURLClassLoader getInstance(Object meta, ClassLoader parentClassLoader)
   {
      synchronized (s_classLoaderMap)
      {
         ExtLibURLClassLoader classLoader = (ExtLibURLClassLoader)s_classLoaderMap.get(meta);

         if (classLoader == null)
         {
            classLoader = new ExtLibURLClassLoader(parentClassLoader);
            s_classLoaderMap.put(meta, classLoader);
         }

         return classLoader;
      }
   }
   
   /**
    * Check if a classloader has been created for a control.
    * @param meta The control that the classloader is associated to for loading external libraries.
    * @return True if the a classloader exists, false otherwise.
    */
   public static boolean hasInstance(Object meta)
   {
      return (s_classLoaderMap.get(meta) != null);
   }

   /**
    * Defines a class in the classloader from the byte code.
    * @param sClassName The name of the class to be loaded.
    * @param byteCode The bytes that make up the class data.
    * @param nOffset The start offset in byteCode of the class data.
    * @param nLength The length of the class data.
    */
   public void createClass(String sClassName, byte[] byteCode, int nOffset, int nLength)
   {
      m_classDataMap.put(sClassName, new ClassBinary(sClassName, byteCode, nOffset, nLength));
   }
   
   /**
    * @see java.net.URLClassLoader#findClass(java.lang.String)
    */
   protected Class findClass(String sName) throws ClassNotFoundException
   {
      Class clz = null;

      try
      {
         clz = super.findClass(sName);
      }
      catch(ClassNotFoundException e)
      {
         ClassBinary classBin = (ClassBinary)m_classDataMap.get(sName);
         
         if(classBin == null)
         {
            throw e;
         }

         clz = defineClass(sName, classBin.getData(), 0, classBin.getSize());
         resolveClass(clz);

         //Remove reference to class binary so it can be garbage collected.
         m_classDataMap.put(sName, null);
      }
      
      return clz;
   }

   /**
    * Appends a url to the classpath.
    * @param url The classpath to be appended.
    */
   public void addURL(URL url)
   {
      super.addURL(url);      
   }
   
   /**
    * Appends a list of urls to the classpath.
    * @param urlArray The classpaths to be appended.
    */
   public void addURL(URL[] urlArray)
   {
      if(urlArray == null)
      {
         return;
      }
      
      for(int i=0; i<urlArray.length; i++)
      {
         addURL(urlArray[i]);
      }
   }
   
   /**
    * Container for a class byte code.
    */
   protected static class ClassBinary
   {
      private byte[] m_byteCodeArray;
      private String m_sName;

      /**
       * Creates a new instance of ClassBinary.
       * @param sName The name of the class.
       * @param bytes The byte code data array.
       * @param nOffset The starting array index of the class byte code.
       * @param nLength The size of the byte code.
       */
      public ClassBinary(String sName, byte[] bytes, int nOffset, int nLength)
      {
         m_sName = sName;
         m_byteCodeArray = new byte[nLength];
         
         System.arraycopy(bytes, nOffset, m_byteCodeArray, 0, nLength);
      }

      /**
       * Gets the byte code of the class.
       * @return Byte code for the class.
       */
      public byte[] getData()
      {
         return m_byteCodeArray;
      }

      /**
       * Gets the size in bytes of the byte code.
       * @return The number of bytes in the byte code.
       */
      public int getSize()
      {
         return m_byteCodeArray.length;
      }
      
      /**
       * Gets the name of the class.
       * @return The name of the class.
       */
      public String getName()
      {
         return m_sName;
      }
   }
}