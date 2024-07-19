// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectStreamClass;
import java.lang.reflect.Proxy;

/**
 * Object input stream with a class loader override.
 */
public class ClassLoaderObjectInputStream extends ObjectInputStream
{
   // associations

   /**
    * The overridden class loader.
    */
   protected ClassLoader m_loader;

   // constructors

   /**
    * Constructs the imput stream.
    * @param istream The wrapped input stream.
    * @throws IOException if the underlying input stream throws an exception.
    */
   public ClassLoaderObjectInputStream(InputStream istream, ClassLoader loader) throws IOException
   {
      super(istream);
      m_loader = loader;
   }

   /**
    * @see java.io.ObjectInputStream#resolveClass(java.io.ObjectStreamClass)
    */
   protected Class resolveClass(ObjectStreamClass desc) throws IOException, ClassNotFoundException
   {
      try
      {
         return Class.forName(desc.getName(), false, m_loader);
      }
      catch (ClassNotFoundException e)
      {
         return super.resolveClass(desc);
      }
   }

   /**
    * @see java.io.ObjectInputStream#resolveProxyClass(java.lang.String[])
    */
   protected Class resolveProxyClass(String[] names) throws IOException, ClassNotFoundException
   {
      int nCount = names.length;
      Class[] classes = new Class[nCount];

      for (int i = 0; i < nCount; ++i)
      {
         classes[i] = Class.forName(names[i], false, m_loader);
      }

      try
      {
         return Proxy.getProxyClass(m_loader, classes);
      }
      catch (IllegalArgumentException e)
      {
         throw new ClassNotFoundException("proxy", e);
      }
   }

   // operations
}
