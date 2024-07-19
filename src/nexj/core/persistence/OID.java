// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import java.io.ByteArrayOutputStream;
import java.io.DataInput;
import java.io.DataInputStream;
import java.io.DataOutput;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.math.BigDecimal;
import java.util.Date;

import nexj.core.meta.Primitive;
import nexj.core.meta.TypeConversionException;
import nexj.core.util.Binary;
import nexj.core.util.BinaryUtil;
import nexj.core.util.IOUtil;
import nexj.core.util.ObjUtil;

/**
 * Object Identifier - a value object used to match the copies of an object
 * instance across process boundaries. Usually, they contain the primary
 * key and can be compared based on its values, including for total
 * ordering.
 */
public final class OID implements OIDHolder, Comparable, Externalizable
{
   // constants

   /**
    * The Java stream unique identifier for the class.
    */
   private final static long serialVersionUID = 1875795639836903377L;

   // attributes

   /**
    * Stores the primary key values.
    */
   private Object[] m_valueArray;

   /**
    * The cached hash code.
    */
   private int m_nHashCode = 0;

   // constructors

   /**
    * Creates an OID.
    * @param valueArray The OID components.
    */
   public OID(Object[] valueArray)
   {
      m_valueArray = valueArray;
   }

   /**
    * Creates an OID.
    * This constructor is for INTERNAL USE ONLY.
    */
   public OID()
   {
   }

   // operations

   /**
    * Sets the value array.
    * OIDs are immutable - this method is for INTERNAL USE ONLY.
    * @param valueArray The value array to set.
    */
   public void setValueArray(Object[] valueArray)
   {
      if (m_valueArray != null)
      {
         throw new IllegalStateException("Attempt to reset the state on OID");
      }

      m_valueArray = valueArray;
   }

   /**
    * @return The value array. This method is for INTERNAL USE ONLY.
    */
   public Object[] getValueArray()
   {
      return m_valueArray;
   }

   /**
    * Returns an OID component value by ordinal number.
    * @param nOrdinal The OID component ordinal (0-based).
    * @return The OID component value.
    */
   public Object getValue(int nOrdinal)
   {
      return m_valueArray[nOrdinal];
   }

   /**
    * Returns the OID component count.
    * @return The OID component count.
    */
   public int getCount()
   {
      return m_valueArray.length;
   }

   /**
    * @see java.lang.Comparable#compareTo(java.lang.Object)
    */
   public int compareTo(Object o)
   {
      return compareTo((OID)o);
   }

   /**
    * Typed version of compareTo(Object).
    * @see java.lang.Comparable#compareTo(java.lang.Object)
    */
   public int compareTo(OID oid)
   {
      if (oid == null)
      {
         return 1;
      }

      if (oid == this)
      {
         return 0;
      }

      int nCount = m_valueArray.length;

      if (nCount != oid.m_valueArray.length)
      {
         throw new ClassCastException("OID value array length mismatch");
      }

      int nCmp = 0;

      for (int i = 0; nCmp == 0 && i < nCount; ++i)
      {
         nCmp = ObjUtil.compare((Comparable)m_valueArray[i], oid.m_valueArray[i]);
      }

      return nCmp;
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (!(obj instanceof OID))
      {
         return false;
      }

      if (this == obj)
      {
         return true;
      }

      OID oid = (OID)obj;

      if (m_nHashCode != oid.m_nHashCode &&
         m_nHashCode != 0 &&
         oid.m_nHashCode != 0)
      {
         return false;
      }

      int nCount = m_valueArray.length;

      if (nCount != oid.m_valueArray.length)
      {
         return false;
      }

      for (int i = 0; i != nCount; ++i)
      {
         Object left = m_valueArray[i];
         Object right = oid.m_valueArray[i];

         if (left == null)
         {
            if (right != null)
            {
               return false;
            }
         }
         else
         {
            if (!left.equals(right))
            {
               return false;
            }
         }
      }

      return true;
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      if (m_nHashCode == 0)
      {
         int n = 0;

         for (int i = 0; i < m_valueArray.length; ++i)
         {
            if (m_valueArray[i] != null)
            {
               n ^= m_valueArray[i].hashCode();
            }
         }

         m_nHashCode = (n == 0) ? 1 : n;
      }

      return m_nHashCode;
   }

   /**
    * Serializes the OID to a string.
    * @return The stringified OID.
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder(32);

      buf.append("OID:");
      buf.append(m_valueArray.length);

      for (int i = 0; i < m_valueArray.length; ++i)
      {
         Object value = m_valueArray[i];
         String s;

         buf.append(':');

         if (value != null)
         {
            if (value instanceof String)
            {
               buf.append('S');
               s = (String)value;
            }
            else if (value instanceof Number)
            {
               if (value instanceof Integer)
               {
                  buf.append('I');
               }
               else if (value instanceof Long)
               {
                  buf.append('L');
               }
               else if (value instanceof Float)
               {
                  buf.append('F');
               }
               else if (value instanceof Double)
               {
                  buf.append('D');
               }
               else if (value instanceof BigDecimal)
               {
                  buf.append('N');
               }
               else
               {
                  buf.append('?');
               }

               s = value.toString();
            }
            else if (value instanceof Date)
            {
               buf.append('T');
               s = Long.toString(((Date)value).getTime());
            }
            else if (value instanceof Boolean)
            {
               buf.append('B');
               buf.append((((Boolean)value).booleanValue()) ? 'T' : 'F');
               s = null;
            }
            else if (value instanceof Binary)
            {
               buf.append('V');
               s = value.toString();
            }
            else
            {
               buf.append('?');
               s = "";
            }

            if (s != null)
            {
               buf.append(s.length());
               buf.append(':');
               buf.append(s);
            }
         }
      }

      return buf.toString();
   }

   /**
    * Returns this object.
    * @see nexj.core.persistence.OIDHolder#getOID()
    */
   public OID getOID()
   {
      return this;
   }

   /**
    * This is not supported, as OIDs are immutable.
    * @throws UnsupportedOperationException
    * @see nexj.core.persistence.OIDHolder#setOID(nexj.core.meta.OID)
    */
   public void setOID(OID oid)
   {
      throw new UnsupportedOperationException("Attempt to invoke OID.setOID(OID)");
   }

   /**
    * Writes the OID to a data output stream.
    * @param os The output stream, to which to write the data.
    * @throws IOException if an error occurs.
    */
   public void write(DataOutput os) throws IOException
   {
      for (int i = 0; i < m_valueArray.length; ++i)
      {
         BinaryUtil.write(os, m_valueArray[i]);
      }
   }

   /**
    * Reads the contents of the OID from a data input stream.
    * This method is for INTERNAL USE ONLY.
    * @param is The input stream.
    * @param nValueCount The number of components to read, or -1 to read until the end of the stream.
    */
   public void read(DataInput is, int nValueCount) throws IOException
   {
      int nCount = 0;
      m_valueArray =  new Object[(nValueCount < 0) ? 1 : nValueCount];

      while (nCount != nValueCount)
      {
         Object value;

         try
         {
            value = BinaryUtil.read(is);
         }
         catch (EOFException e)
         {
            if (nValueCount > 0)
            {
               throw e;
            }

            break;
         }

         if (nCount == m_valueArray.length)
         {
            Object[] values = new Object[nCount << 1];

            System.arraycopy(m_valueArray, 0, values, 0, nCount);
            m_valueArray = values;
         }

         m_valueArray[nCount++] = value;
      }

      if (nCount != m_valueArray.length)
      {
         Object[] values = new Object[nCount];

         System.arraycopy(m_valueArray, 0, values, 0, nCount);
         m_valueArray = values;
      }
   }

   /**
    * @return The OID serialized as a binary value.
    */
   public Binary toBinary()
   {
      ByteArrayOutputStream bos = new ByteArrayOutputStream(17);
      DataOutputStream dos = new DataOutputStream(bos);

      try
      {
         write(dos);
         dos.close();

         return new Binary(bos.toByteArray());
      }
      catch (IOException e)
      {
         throw new TypeConversionException(Primitive.ANY, e);
      }
   }

   /**
    * Converts a binary value to OID.
    * @param binary The binary value.
    * @return The converted OID.
    */
   public static OID fromBinary(Binary binary)
   {
      DataInputStream is = new DataInputStream(binary.getInputStream());
      OID oid = new OID();

      try
      {
         oid.read(is, -1);
      }
      catch (IOException e)
      {
         throw new TypeConversionException(Primitive.ANY, e);
      }
      finally
      {
         IOUtil.close(is);
      }

      return oid;
   }

   /**
    * @see java.io.Externalizable#readExternal(java.io.ObjectInput)
    */
   public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException
   {
      read(in, in.readUnsignedByte());
   }

   /**
    * @see java.io.Externalizable#writeExternal(java.io.ObjectOutput)
    */
   public void writeExternal(ObjectOutput out) throws IOException
   {
      out.writeByte(m_valueArray.length);
      write(out);
   }
}
