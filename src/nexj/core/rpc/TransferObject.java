// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import java.io.Externalizable;
import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.util.Collection;

import nexj.core.meta.Primitive;
import nexj.core.meta.TypeMismatchException;
import nexj.core.persistence.OID;
import nexj.core.persistence.OIDHolder;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.ScriptingException;
import nexj.core.scripting.Symbol;
import nexj.core.util.LookupException;
import nexj.core.util.MathUtil;
import nexj.core.util.PrintWriter;
import nexj.core.util.Printable;
import nexj.core.util.PropertyIterator;
import nexj.core.util.PropertyMap;

/**
 * Serializable data transfer object used to pass object state across
 * process boundaries.
 * The Transfer Objects must contain Object Identifiers (OID). The OIDs are
 * used to match the persisted instance to its remote copies. The whole
 * Transfer Object graph is serialized along with the request. Individual
 * protocols, e.g. SOAP might use internal protocol-specific tables to
 * describe the instance references within the serialized graph.
 */
public class TransferObject implements PropertyMap, OIDHolder, Cloneable, Externalizable, Function, Printable
{
   // constants

   /**
    * The Java stream unique identifier for the class.
    */
   private final static long serialVersionUID = 7698169377151199940L;

   /**
    * Serialization version.
    */
   private final static byte SERIAL_VERSION = 0;

   /**
    * Key value indicating deallocated cells.
    */
   private final static Object EMPTY = new Object();

   // attributes

   /**
    * The key and value table.
    * The keys are at even indexes, the values are at odd indexes.
    */
   private Object[] m_table;
   
   /**
    * The key-value pair count.
    */
   private int m_nCount;

   /**
    * The empty element count.
    */
   private int m_nEmpty;

   /**
    * The name of the class corresponding to the transfer object.
    */
   private String m_sClassName;

   /**
    * The name of the event that will be invoked on the instance represented
    * by the Transfer Object. Can be null. Events have no arguments. If a
    * method invocation with arguments is needed, the Command pattern should
    * be used.
    */
   private String m_sEventName;

   /**
    * The release version of the Domain Class represented by this transfer
    * object. The server converts the transfer objects based on this version
    * and on the request version.
    */
   private short m_nVersion;

   // associations

   /**
    * The Transfer Object OID.
    */
   private OID m_oid;

   // constructor
   
   /**
    * Creates a transfer object with a class name and estimated value count.
    * @param sClassName The name of the class stored in the transfer object.
    * @param nValueCount The estimated number of attribute values that will be stored.
    */
   public TransferObject(String sClassName, int nValueCount)
   {
      m_sClassName = sClassName;
      m_table = new Object[getTableSize(nValueCount)];
   }

   /**
    * Creates a transfer object with a class name and estimated value count of 8.
    * @param sClassName The name of the class stored in the transfer object.
    */
   public TransferObject(String sClassName)
   {
      m_sClassName = sClassName;
      m_table = new Object[32];
   }

   /**
    * Creates a transfer object with an estimated value count.
    * @param nValueCount The estimated number of attribute values that will be stored.
    */
   public TransferObject(int nValueCount)
   {
      this(null, nValueCount);
   }

   /**
    * Creates a transfer object with class and event names and an estimated value count.
    * @param sClassName The name of the class stored in the transfer object.
    * @param sEventName The event name.
    * @param nValueCount The estimated number of attribute values that will be stored.
    */
   public TransferObject(String sClassName, String sEventName, int nValueCount)
   {
      this(sClassName, nValueCount);
      m_sEventName = sEventName;
   }

   /**
    * Creates a transfer object with an OID, class and event names and estimated value count.
    * @param oid The OID of the object. 
    * @param sClassName The name of the class stored in the transfer object.
    * @param sEventName The event name.
    * @param nValueCount The estimated number of attribute values that will be stored.
    */
   public TransferObject(OID oid, String sClassName, String sEventName, int nValueCount)
   {
      this(sClassName, nValueCount);
      m_oid = oid;
      m_sEventName = sEventName;
   }

   /**
    * Creates a transfer object with class and event names and estimated value count of 8.
    * @param sClassName The name of the class stored in the transfer object.
    * @param sEventName The event name.
    */
   public TransferObject(String sClassName, String sEventName)
   {
      this(sClassName);
      m_sEventName = sEventName;
   }

   /**
    * Creates a transfer object with an estimated value count of 8.
    */
   public TransferObject()
   {
      // Need to explicitly cast to a String in order to allow compiler to
      // properly resolve the constructor when a copy constructor is
      // generated during J2ME translation
      this((String)null);
   }

   // operations
   
   /**
    * Calculates the table size as a power of two.
    * @param nCount The count of key-value pairs to store.
    * @return The calculated size.
    */
   private static int getTableSize(int nCount)
   {
      return MathUtil.ceil2(Math.max(nCount, 1)) << 2;
   }

   /**
    * Sets the transfer object class release version.
    * @param nVersion The transfer object release version to set.
    */
   public void setVersion(short nVersion)
   {
      m_nVersion = nVersion;
   }

   /**
    * @return The transfer object class release version.
    */
   public short getVersion()
   {
      return m_nVersion;
   }

   /**
    * Sets the class name.
    * @param sName The class name to set.
    */
   public void setClassName(String sName)
   {
      m_sClassName = sName;
   }

   /**
    * @return The class name.
    */
   public String getClassName()
   {
      return m_sClassName;
   }

   /**
    * Sets the name of the event that will be invoked on the instance
    * represented by this Transfer Object.
    * @param sName The event name. Can be null.
    */
   public void setEventName(String sName)
   {
      m_sEventName = sName;
   }        

   /**
    * Returns the name of the event that will be invoked on the instance
    * represented by this Transfer Object.
    * @return The event name. Can be null if no event is used.
    */
   public String getEventName()
   {
      return m_sEventName;
   }        

   /**
    * Sets the object identifier.
    * @param oid The object identifier to set.
    */
   public void setOID(OID oid)
   {
      m_oid = oid;
   }

   /**
    * @see nexj.core.persistence.OIDHolder#getOID()
    */
   public OID getOID()
   {
      return m_oid;
   }

   /**
    * Rehashes the transfer object.
    * @param nSize The new table size.
    */
   protected void rehash(int nSize)
   {
      int nMask2 = nSize - 1;
      Object[] table2 = new Object[nSize];
      
      for (int k = 0, nLen = m_table.length; k < nLen; k += 2)
      {
         Object key = m_table[k];
         
         if (key != null && key != EMPTY)
         {
            int i = (key.hashCode() << 1) & nMask2;
            
            for (;;)
            {
               if (table2[i] == null)
               {
                  table2[i] = key;
                  table2[i + 1] = m_table[k + 1];

                  break; 
               }
               
               i = (i + 2) & nMask2;
            }
         }
      }
      
      m_table = table2;
      m_nEmpty = 0;
   }
   
   /**
    * Sets an attribute value.
    * @param sName The name of the attribute.
    * @param value The value of the attribute.
    */
   public void setValue(String sName, Object value)
   {
      int nMask = m_table.length - 1;
      int i = (sName.hashCode() << 1) & nMask;
      int k = -1;

      for (;;)
      {
         Object key2 = m_table[i];

         if (key2 == null)
         {
            if (k >= 0)
            {
               i = k;
               --m_nEmpty;
            }
            
            m_table[i] = sName;
            m_table[i + 1] = value;

            if ((++m_nCount << 2) - 1 > nMask)
            {
               rehash((nMask + 1) << 1);
            }

            break;
         }
         
         if (key2 == EMPTY)
         {
            if (k < 0)
            {
               k = i;
            }
         }
         else if (sName.equals(key2))
         {
            m_table[i + 1] = value;

            break;
         }

         i = (i + 2) & nMask;
      } 
   }        

   /**
    * Gets an attribute value by name.
    * @param sName The attribute name.
    * @return The attribute value.
    * @throws LookupException if the named value does not exist.
    */
   public Object getValue(String sName) throws LookupException
   {
      int nMask = m_table.length - 1;
      int i = (sName.hashCode() << 1) & nMask;

      for (;;)
      {
         Object key2 = m_table[i];

         if (key2 == null)
         {
            throw new LookupException("err.rpc.valueLookup", new Object[]{sName});
         }

         if (sName.equals(key2))
         {
            return m_table[i + 1];
         }

         i = (i + 2) & nMask;
      } 
   }

   /**
    * Finds an attribute value by name.
    * @param sName The attribute name.
    * @param defaultValue The value to return if the attribute was not found.
    * @return The attribute value, or defaultValue if not found.
    */
   public Object findValue(String sName, Object defaultValue)
   {
      int nMask = m_table.length - 1;
      int i = (sName.hashCode() << 1) & nMask;

      for (;;)
      {
         Object key2 = m_table[i];

         if (key2 == null)
         {
            return defaultValue;
         }

         if (sName.equals(key2))
         {
            return m_table[i + 1];
         }

         i = (i + 2) & nMask;
      } 
   }

   /**
    * Finds an attribute value by name.
    * @param sName The attribute name.
    * @return The attribute value, or null if not found.
    */
   public Object findValue(String sName)
   {
      return findValue(sName, null);
   }

   /**
    * Checks in the named attribute value exists in the transfer object.
    * @param sName The attribute name.
    * @return True if it exists.
    */
   public boolean hasValue(String sName)
   {
      int nMask = m_table.length - 1;
      int i = (sName.hashCode() << 1) & nMask;

      for (;;)
      {
         Object key2 = m_table[i];

         if (key2 == null)
         {
            return false;
         }

         if (sName.equals(key2))
         {
            return true;
         }

         i = (i + 2) & nMask;
      } 
   }
   
   /**
    * Removes an attribute value by name, if it exists.
    * @param sName The attribute name.
    * @return True if the value has been removed, false if not found.
    */
   public boolean removeValue(String sName)
   {
      int nMask = m_table.length - 1;
      int i = (sName.hashCode() << 1) & nMask;

      for (;;)
      {
         Object key2 = m_table[i];

         if (key2 == null)
         {
            return false;
         }

         if (sName.equals(key2))
         {
            --m_nCount;
            ++m_nEmpty;
            m_table[i + 1] = null;

            if (m_table[(i + 2) & nMask] == null)
            {
               do
               {
                  m_table[i] = null;
                  i = (i - 2) & nMask;
                  --m_nEmpty;
               }
               while (m_table[i] == EMPTY);
            }
            else
            {
               m_table[i] = EMPTY;

               if (m_nEmpty >= m_nCount)
               {
                  rehash(getTableSize(m_nCount));
               }
            }

            return true;
         }

         i = (i + 2) & nMask;
      } 
   }
   
   /**
    * Removes all the values.
    */
   public void removeAllValues()
   {
      java.util.Arrays.fill(m_table, null);
      m_nCount = m_nEmpty = 0;
   }

   /**
    * @return The stored attribute value count.
    */
   public int getValueCount()
   {
      return m_nCount;
   }

   /**
    * @return PropertyIterator for attribute keys and values.
    */
   public PropertyIterator getIterator()
   {
      return new Iterator();
   }

   /**
    * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
    */
   public boolean invoke(int nArgCount, Machine machine)
   {
      if (nArgCount != 0)
      {
         Object sym = machine.getArg(0, nArgCount);

         if (sym instanceof Symbol || sym instanceof String)
         {
            String sName = sym.toString();

            if (nArgCount == 1)
            {
               if (sName.length() > 0 && sName.charAt(0) == ':')
               {
                  if (Symbol._EVENT.equals(sym))
                  {
                     machine.returnValue(getEventName(), nArgCount);
                  }
                  else if (Symbol._CLASS.equals(sym))
                  {
                     machine.returnValue(getClassName(), nArgCount);
                  }
                  else if (Symbol._ITERATOR.equals(sym))
                  {
                     machine.returnValue(getIterator(), nArgCount);
                  }
                  else if (Symbol._OID.equals(sym))
                  {
                     machine.returnValue(getOID(), nArgCount);
                  }
                  else if (Symbol._VERSION.equals(sym))
                  {
                     machine.returnValue(Primitive.createInteger(m_nVersion), nArgCount);
                  }
                  else
                  {
                     machine.returnValue(findValue(sName), nArgCount);
                  }
               }
               else
               {
                  machine.returnValue(findValue(sName), nArgCount);
               }
            }
            else if (nArgCount == 2)
            {
               Object value = machine.getArg(1, nArgCount);

               if (sName.length() > 0 && sName.charAt(0) == ':')
               {
                  if (Symbol._EVENT.equals(sym))
                  {
                     if (value != null && !(value instanceof String))
                     {
                        throw new TypeMismatchException(Symbol._EVENT);
                     }
                     
                     setEventName((String)value);
                  }
                  else if (Symbol._CLASS.equals(sym))
                  {
                     if (value != null && !(value instanceof String))
                     {
                        throw new TypeMismatchException(Symbol._CLASS);
                     }
                     
                     setClassName((String)value);
                  }
                  else if (Symbol._OID.equals(sym))
                  {
                     if (value != null && !(value instanceof OID))
                     {
                        throw new TypeMismatchException(Symbol._OID);
                     }
                     
                     setOID((OID)value);
                  }
                  else if (Symbol._VERSION.equals(sym))
                  {
                     if (!(value instanceof Number))
                     {
                        throw new TypeMismatchException(Symbol._VERSION);
                     }
                     
                     setVersion(((Number)value).shortValue());
                  }
                  else if (Symbol._CONTAINS.equals(sym))
                  {
                     if (value instanceof Symbol)
                     {
                        value = Boolean.valueOf(hasValue(value.toString()));
                     }
                     else if (value instanceof String)
                     {
                        value = Boolean.valueOf(hasValue((String)value));
                     }
                     else
                     {
                        throw new TypeMismatchException(Symbol._CONTAINS);
                     }
                  }
                  else if (Symbol._GET.equals(sym))
                  {
                     if (value instanceof Symbol)
                     {
                        value = getValue(value.toString());
                     }
                     else if (value instanceof String)
                     {
                        value = getValue((String)value);
                     }
                     else
                     {
                        throw new TypeMismatchException(Symbol._GET);
                     }
                  }
                  else if (Symbol._REMOVE.equals(sym))
                  {
                     if (value instanceof Symbol)
                     {
                        value = Boolean.valueOf(removeValue(value.toString()));
                     }
                     else if (value instanceof String)
                     {
                        value = Boolean.valueOf(removeValue((String)value));
                     }
                     else
                     {
                        throw new TypeMismatchException(Symbol._REMOVE);
                     }
                  }
                  else
                  {
                     setValue(sName, value);
                  }
               }
               else
               {
                  setValue(sName, value);
               }

               machine.returnValue(value, nArgCount);
            }
            else
            {
               throw new ScriptingException("err.scripting.maxArgCount",
                  new Object[]{sym.toString(),
                     Primitive.ONE_INTEGER,
                     Primitive.createInteger(nArgCount - 1)});
            }

            return false;
         }
         else
         {
            throw new ScriptingException("err.scripting.funCall");
         }
      }
      else
      {
         throw new ScriptingException("err.scripting.minArgCount",
            new Object[]{"TransferObject",
               Primitive.ONE_INTEGER,
               Primitive.createInteger(nArgCount)});
      }
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         TransferObject tobj = (TransferObject)super.clone();

         tobj.m_table = new Object[m_table.length];
         System.arraycopy(m_table, 0, tobj.m_table, 0, m_table.length);

         return tobj;
      }
      catch (CloneNotSupportedException e)
      {
         return null;
      }
   }

   /**
    * @see java.io.Externalizable#writeExternal(java.io.ObjectOutput)
    */
   public void writeExternal(ObjectOutput out) throws IOException
   {
      out.writeByte(SERIAL_VERSION);
      out.writeObject(m_sClassName);
      out.writeObject(m_sEventName);
      out.writeShort(m_nVersion);
      out.writeObject(m_oid);
      out.writeInt(m_nCount);

      int nCount = m_nCount;

      for (int i = 0; nCount != 0; i += 2)
      {
         Object key = m_table[i];
         
         if (key != null && key != EMPTY)
         {
            out.writeObject(key);
            out.writeObject(m_table[i + 1]);
            --nCount;
         }
      }
   }

   /**
    * @see java.io.Externalizable#readExternal(java.io.ObjectInput)
    */
   public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException
   {
      // Read the serial version
      in.readByte();
      
      m_sClassName = (String)in.readObject();
      m_sEventName = (String)in.readObject();
      m_nVersion = in.readShort();
      m_oid = (OID)in.readObject();
      
      int nCount = in.readInt();

      if (nCount < 0)
      {
         throw new InvalidObjectException("Negative TransferObject value count");
      }
      
      int nLen = getTableSize(nCount);
      
      if (nLen != m_table.length)
      {
         m_table = new Object[nLen];
      }
      
      while (nCount-- != 0)
      {
         Object key = in.readObject();
         setValue((String)key, in.readObject());
      }
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      boolean bNew = writer.addObject(this);

      if (!bNew)
      {
         writer.write(PrintWriter.REF);
      }

      writer.write("TO<");

      if (m_sClassName != null)
      {
         writer.write(m_sClassName);
      }

      if (m_sEventName != null || m_nVersion != 0)
      {
         if (m_sEventName != null)
         {
            writer.write('\'');
            writer.write(m_sEventName);
         }

         writer.write(',');

         if (m_nVersion != 0)
         {
            writer.write(' ');
            writer.print(m_nVersion);
         }
      }

      writer.write(", ");

      if (m_oid != null)
      {
         writer.write(m_oid.toString());
      }
      else
      {
         writer.printId(this);
      }

      writer.write('>');

      if (bNew)
      {
         writer.write('(');
         writer.addIndent(1);

         for (PropertyIterator propItr = getIterator(); propItr.hasNext();)
         {
            propItr.next();
            writer.indent();
            writer.write(propItr.getName());
            writer.write('=');

            Object value = propItr.getValue();

            if (value instanceof Collection)
            {
               writer.addIndent(1);
               writer.write('{');

               Collection col = (Collection)value;

               for (java.util.Iterator itemItr = ((Collection)value).iterator(); itemItr.hasNext();)
               {
                  writer.indent();
                  writer.print(itemItr.next());

                  if (itemItr.hasNext())
                  {
                     writer.write(',');
                  }
               }

               writer.addIndent(-1);

               if (!col.isEmpty())
               {
                  writer.indent();
               }

               writer.write('}');
            }
            else
            {
               writer.print(value);
            }

            if (propItr.hasNext())
            {
               writer.write(',');
            }
         }

         writer.addIndent(-1);

         if (getValueCount() != 0)
         {
            writer.indent();
         }

         writer.write(')');
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return PrintWriter.toString(this);
   }

   // inner classes

   /**
    * Transfer Object iterator - can return the current key and the value.
    * NOTE: This iterator does not check for errors for efficiency reasons.
    */
   protected class Iterator implements PropertyIterator
   {
      /**
       * The next key index.
       */
      private int m_nNext = -2;
      
      /**
       * The current key index.
       */
      private int m_nCur;

      /**
       * The table for iteration.
       */
      protected Object[] m_table = TransferObject.this.m_table;

      /**
       * Creates an iterator.
       */
      protected Iterator()
      {
         incr();
      }

      /**
       * Advances to the next item.
       * @return True if there is next item.
       */
      private boolean incr()
      {
         m_nCur = m_nNext;
         m_nNext += 2;
         
         while (m_nNext < m_table.length)
         {
            if (m_table[m_nNext] != null && m_table[m_nNext] != EMPTY)
            {
               return true;
            }
            
            m_nNext += 2;
         }
         
         return false;
      }
      
      /**
       * @see java.util.Iterator#hasNext()
       */
      public boolean hasNext()
      {
         return m_nNext < m_table.length;
      }

      /**
       * @return The next available element key.
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         if (m_nNext < m_table.length)
         {
            incr();

            return m_table[m_nCur];
         }

         throw new java.util.NoSuchElementException();
      }

      /**
       * @see java.util.Iterator#remove()
       */
      public void remove()
      {
         if (m_table[m_nCur] == null || m_table[m_nCur] == EMPTY)
         {
            return;
         }

         if (m_table == TransferObject.this.m_table)
         {
            int nMask = m_table.length - 1;
            
            --m_nCount;
            ++m_nEmpty;
            m_table[m_nCur + 1] = null;

            if (m_table[(m_nCur + 2) & nMask] == null)
            {
               int i = m_nCur;
               
               do
               {
                  m_table[i] = null;
                  i = (i - 2) & nMask;
                  --m_nEmpty;
               }
               while (m_table[i] == EMPTY);
            }
            else
            {
               m_table[m_nCur] = EMPTY;
               
               if (m_nEmpty >= m_nCount)
               {
                  rehash(getTableSize(m_nCount));
               }
            }
         }
         else
         {
            removeValue((String)m_table[m_nCur]);
            m_table[m_nCur] = m_table[m_nCur + 1] = null;
         }
      }
      
      /**
       * @return The value name retrieved by the last next() invocation.
       */
      public String getName()
      {
         return (String)m_table[m_nCur];
      }
      
      /**
       * @return The value associated with the last next() invocation.
       */
      public Object getValue()
      {
         return m_table[m_nCur + 1];
      }
      
      /**
       * Replaces the value associated with the last next() invocation.
       * @param value The value to set.
       */
      public void setValue(Object value)
      {
         m_table[m_nCur + 1] = value;

         if (m_table != TransferObject.this.m_table)
         {
            TransferObject.this.setValue((String)m_table[m_nCur], value);
         }
      }
   }
}
