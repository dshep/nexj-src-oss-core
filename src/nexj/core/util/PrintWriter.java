// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.Array;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

/**
 * Writer wrapper providing object identity tracking and indentation.   
 */
public class PrintWriter extends Writer
{
   // constants

   /**
    * String representation of null references.
    */
   public final static String NULL = "()";

   /**
    * Reference prefix.
    */
   public final static String REF = "REF:";

   /**
    * Suppresses the object identifiers. 
    */
   public final static int FMT_NOREF = 0x01;

   /**
    * Default format. 
    */
   public final static int FMT_DEFAULT = 0x00;

   // attributes

   /**
    * The current indentation level.
    */
   protected int m_nIndent;

   /**
    * Print format, combination of FMT_* flags.
    */
   protected int m_nFormat;

   // associations

   /**
    * The wrapped writer.
    */
   protected Writer m_writer;
   
   /**
    * The set for tracking object identity.
    */
   protected Set m_identitySet;

   // constructors

   /**
    * Constructs the writer.
    * @param writer The wrapped writer.
    */
   public PrintWriter(Writer writer)
   {
      super(writer);
      m_writer = writer;
   }

   /**
    * Constructs the writer.
    * @param writer The wrapped writer.
    * @param nFormat Print format, combination of FMT_* flags.
    */
   public PrintWriter(Writer writer, int nFormat)
   {
      super(writer);
      m_writer = writer;
      m_nFormat = nFormat;
   }

   // operations

   /**
    * @see java.io.Writer#close()
    */
   public void close() throws IOException
   {
      m_writer.close();
      clearObjects();
      clearIndent();
   }

   /**
    * @see java.io.Writer#flush()
    */
   public void flush() throws IOException
   {
      m_writer.flush();
   }

   /**
    * @see java.io.Writer#write(java.lang.String, int, int)
    */
   public void write(String s, int nOfs, int nCount) throws IOException
   {
      if (s == null)
      {
         m_writer.write("null");
      }
      else
      {
         m_writer.write(s, nOfs, nCount);
      }
   }

   /**
    * @see java.io.Writer#write(java.lang.String)
    */
   public void write(String s) throws IOException
   {
      m_writer.write((s == null) ? "null" : s);
   }

   /**
    * @see java.io.Writer#write(char[], int, int)
    */
   public void write(char[] cbuf, int off, int len) throws IOException
   {
      m_writer.write(cbuf, off, len);
   }

   /**
    * @see java.io.Writer#write(char[])
    */
   public void write(char[] cbuf) throws IOException
   {
      m_writer.write(cbuf);
   }

   /**
    * @see java.io.Writer#write(int)
    */
   public void write(int ch) throws IOException
   {
      m_writer.write(ch);
   }

   /**
    * @return The wrapped writer.
    */
   public Writer getWriter()
   {
      return m_writer;
   }

   /**
    * Adds an object to the identity set.
    * @param obj The object to add.
    * @return True if the object has been added, false if it existed before.
    */
   public boolean addObject(Object obj)
   {
      if (m_identitySet == null)
      {
         m_identitySet = new IdentityHashHolder();
      }

      return m_identitySet.add(obj);
   }

   /**
    * Removes an object from the identity set.
    * @param obj The object to remove.
    * @return True if the object has been removed, false if not found.
    */
   public boolean removeObject(Object obj)
   {
      if (m_identitySet == null)
      {
         return false;
      }

      return m_identitySet.remove(obj);
   }
   
   /**
    * Clears the identity set.
    */
   public void clearObjects()
   {
      m_identitySet = null;
   }

   /**
    * Sets the indentation level.
    * @param nIndent The indentation level.
    * @return The old indentation level.
    */
   public int setIndent(int nIndent)
   {
      int nIndentSaved = m_nIndent;

      m_nIndent = nIndent;

      return nIndentSaved;
   }

   /**
    * Increases the indentation level by the specified amount.
    * @param nDelta The indentation level delta.
    */
   public void addIndent(int nDelta)
   {
      m_nIndent += nDelta;
   }

   /**
    * Resets the indentation level to 0.
    */
   public void clearIndent()
   {
      m_nIndent = 0;
   }

   /**
    * Prints a new line followed by an indentation.
    * @param nDelta The amount of indentation to add to the current level.
    */
   public void indent(int nDelta) throws IOException
   {
      write(SysUtil.LINE_SEP);

      for (int i = 0, n = m_nIndent + nDelta; i < n; ++i)
      {
         write("   ");
      }
   }

   /**
    * Prints a new line followed by an indentation at the current level.
    */
   public void indent() throws IOException
   {
      write(SysUtil.LINE_SEP);

      for (int i = 0; i < m_nIndent; ++i)
      {
         write("   ");
      }
   }

   /**
    * Prints a string.
    * @param s The string to print. Can be null.
    */
   public void print(String s) throws IOException
   {
      if (s == null)
      {
         write(NULL);
      }
      else
      {
         write('"');
         write(s);
         write('"');
      }
   }

   /**
    * Prints a boolean.
    * @param b The boolean to print.
    */
   public void print(boolean b) throws IOException
   {
      write((b) ? "#t" : "#f");
   }

   /**
    * Prints an integer.
    * @param n The integer to print.
    */
   public void print(int n) throws IOException
   {
      write(String.valueOf(n));
   }

   /**
    * Prints an object implementing the Printable interface.
    * @param obj The object to print. Can be null.
    */
   public void print(Printable obj) throws IOException
   {
      if (obj == null)
      {
         write(NULL);
      }
      else
      {
         obj.printOn(this);
      }
   }

   /**
    * Prints an object.
    * @param obj The object to print. Can be null.
    */
   public void print(Object obj) throws IOException
   {
      if (obj == null)
      {
         write(NULL);
      }
      else if (obj instanceof Printable)
      {
         ((Printable)obj).printOn(this);
      }
      else if (obj instanceof String)
      {
         print((String)obj);
      }
      else if (obj instanceof Number)
      {
         write(obj.toString());
      }
      else if (obj instanceof Boolean)
      {
         print(((Boolean)obj).booleanValue());
      }
      else if (obj instanceof Timestamp)
      {
         write(StringUtil.toString((Timestamp)obj));
      }
      else if (obj instanceof Collection)
      {
         write("(collection");
         ++m_nIndent;

         for (Iterator itr = ((Collection)obj).iterator(); itr.hasNext();)
         {
            write(' ');
            print(itr.next());
         }

         --m_nIndent;
         write(')');
      }
      else if (obj.getClass().isArray())
      {
         write("#(");
         ++m_nIndent;

         int nCount = Array.getLength(obj);

         for (int i = 0; i < nCount; ++i)
         {
            if (i > 0)
            {
               write(' ');
            }

            print(Array.get(obj, i));
         }

         --m_nIndent;
         write(')');
      }
      else
      {
         String s = obj.toString();

         write((s == null) ? "" : s);
      }
   }

   /**
    * Prints an object reference.
    * @param obj The object. Can be null.
    */
   public void printRef(Object obj) throws IOException
   {
      if (obj == null)
      {
         write(NULL);
      }
      else
      {
         write(REF);

         String sName = obj.getClass().getName();
         int i = sName.lastIndexOf('.') + 1;

         write(sName, i, sName.length() - i);
         printId(obj);
      }
   }

   /**
    * Prints an object id (identity hash code).
    * @param obj The object. Can be null.
    */
   public void printId(Object obj) throws IOException
   {
      write('@');

      if ((m_nFormat & FMT_NOREF) == 0)
      {
         write(String.valueOf(System.identityHashCode(obj)));
      }
   }

   /**
    * Prints the specified object to a string, handling specially some primitive
    * types and Printable implementations.
    * @param obj The object to dump.
    * @return The resulting string.
    */
   public static String toString(Object obj)
   {
      return toString(obj, FMT_DEFAULT);
   }

   /**
    * Prints the specified object to a string, handling specially some primitive
    * types and Printable implementations.
    * @param obj The object to dump.
    * @param nFormat Print format, combination of FMT_* flags.
    * @return The resulting string.
    */
   public static String toString(Object obj, int nFormat)
   {
      StringWriter writer = new StringWriter(128);

      try
      {
         new PrintWriter(writer, nFormat).print(obj);

         return writer.toString();
      }
      catch (IOException e)
      {
         return "#<I/O Error>";
      }
   }

   /**
    * Prints the specified object to a string.
    * @param printable The object to print.
    * @return The resulting string.
    */
   public static String toString(Printable printable)
   {
      return toString(printable, FMT_DEFAULT);
   }

   /**
    * Prints the specified object to a string.
    * @param printable The object to print.
    * @param nFormat Print format, combination of FMT_* flags.
    * @return The resulting string.
    */
   public static String toString(Printable printable, int nFormat)
   {
      StringWriter writer = new StringWriter(64);

      try
      {
         new PrintWriter(writer, nFormat).print(printable);

         return writer.toString();
      }
      catch (IOException e)
      {
         return "#<I/O Error>";
      }
   }
}
