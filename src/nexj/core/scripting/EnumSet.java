package nexj.core.scripting;

import java.io.IOException;
import java.io.Serializable;
import java.util.Iterator;

import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.meta.TypeConversionException;
import nexj.core.persistence.OID;
import nexj.core.persistence.OIDHolder;
import nexj.core.util.ArrayIterator;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.PrintWriter;
import nexj.core.util.Printable;

/**
 * Enumeration set implementation
 */
public class EnumSet implements Cloneable, Serializable, Printable, Type
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = -2279326572228681798L;

   /**
    * The number of bits to shift to obtain the int index into the mask array
    * from an element index.
    */
   protected static final int BIT_SHIFT = 5;

   /**
    * The bit mask to use on an element index to obtain the bit index into
    * an int in the mask array.
    */
   protected static final int BIT_MASK = 31;

   /**
    * Name prefix for global EnumSet objects defined in define-enumeration.
    */
   public static final String GLOBAL_OBJECT_PREFIX = "#enum:";

   // attributes

   /**
    * Flag indicating whether this enumeration set is empty.
    */
   protected final boolean m_bEmpty;

   // associations

   /**
    * The map containing mappings from each element in the enumeration
    * type to its index. Shared among EnumSet objects of the same type.
    */
   protected final Lookup m_map;

   /**
    * The array containing the string values of all the elements in the
    * enumeration type. Shared among EnumSet objects of the same type.
    */
   protected final String[] m_sValueArray;

   /**
    * The master set containing all the elements in this enumeration.
    * Unique for each enumeration type and shared among EnumSet objects
    * of the same type.
    */
   protected final EnumSet m_masterSet;

   /**
    * The int array containing bit masks, indicating whether each element
    * in the enumeration type appears in this EnumSet. Indexing starts from
    * the lowest bit of the int at index 0 and corresponds to the element
    * ordering of the master set. Null for the master set.
    */
   protected final int[] m_nMaskArray;

   /**
    * Creates a new master set from the given map.
    * @param map Contains mappings from the enumeration set elements to
    * their corresponding 0-based indices in the set.
    */
   public EnumSet(Lookup map)
   {
      int nSize = map.size();

      m_masterSet = this;
      m_map = map;
      m_nMaskArray = null;
      m_sValueArray = new String[nSize];
      m_bEmpty = nSize == 0;

      for (Lookup.Iterator itr = map.iterator(); itr.hasNext();)
      {
         itr.next();
         m_sValueArray[((Integer)itr.getValue()).intValue()] = (String)itr.getKey();
      }
   }

   /**
    * Creates a subset of the given master set.
    * @param masterSet The set containing all the elements in the enumeration.
    * @param nMaskArray An int array containing bit masks, indicating whether
    * each element in the enumeration appears in the new subset. Indexing starts
    * from the lowest bit of the int at index 0 and corresponds to the element
    * ordering of the master set.
    */
   protected EnumSet(EnumSet masterSet, int[] nMaskArray)
   {
      m_masterSet = masterSet;
      m_map = masterSet.m_map;
      m_sValueArray = masterSet.m_sValueArray;
      m_nMaskArray = nMaskArray;

      for (int i = 0, nLength = nMaskArray.length; i < nLength; i++)
      {
         if (nMaskArray[i] != 0)
         {
            m_bEmpty = false;

            return;
         }
      }

      m_bEmpty = true;
   }

   /**
    * Creates and returns the master set of a new enumeration type.
    * @param elements A Pair object containing the elements of the new enumeration type
    * in order. Duplicates, if any, will be removed.
    * @return The master set containing all the elements of the new enumeration type.
    * @throws ClassCastException if the argument is not a proper list or contains
    * elements other than Symbol objects.
    */
   public static EnumSet createMasterSet(Object elements)
   {
      Lookup map = new HashTab();
      int nIndex = 0;

      for (Pair pair; elements != null; elements = pair.getTail())
      {
         if (!(elements instanceof Pair))
         {
            throw new ScriptingException("err.scripting.enum.badList");
         }

         pair = (Pair)elements;

         Object head = pair.getHead();

         if (!(head instanceof Symbol))
         {
            throw new ScriptingException("err.scripting.enum.unsupportedValueType");
         }

         String sValue = ((Symbol)head).getName();

         if (!map.contains(sValue))
         {
            map.put(sValue, Primitive.createInteger(nIndex++));
         }
      }

      return new EnumSet(map);
   }

   /**
    * Returns an enumeration set of the same enumeration type as the
    * given set and containing the elements given in the Pair object.
    * @param enumSet The enumeration set of the same type as the new set.
    * @param elements A Pair object containing the elements of the new set
    * in no particular order. Duplicates, if any, will be removed.
    * @return An enumeration set of the same type as the given set and
    * containing the given elements. The set is newly allocated, unless
    * it is the master set.
    * @throws ClassCastException if the Pair object is not a proper list
    * or contains elements other than Symbol objects.
    * @throws ScriptingException if the Pair object contains an element
    * that does not exist in the master set.
    */
   public static EnumSet createSubset(EnumSet enumSet, Object elements)
   {
      int nLength = enumSet.getMaskArrayLength();
      int[] nMaskArray = new int[nLength];

      for (Pair pair; elements != null; elements = pair.getTail())
      {
         if (!(elements instanceof Pair))
         {
            throw new ScriptingException("err.scripting.enum.badList");
         }

         pair = (Pair)elements;

         Object head = pair.getHead();

         if (!(head instanceof Symbol))
         {
            throw new ScriptingException("err.scripting.enum.unsupportedValueType");
         }

         String sValue = ((Symbol)head).getName();
         Object index = enumSet.m_map.get(sValue);

         if (index == null)
         {
            throw new ScriptingException("err.scripting.enum.valueNotFound",
               new Object[]{sValue});
         }

         int nIndex = ((Integer)index).intValue();

         nMaskArray[nIndex >> BIT_SHIFT] |= 1 << (nIndex & BIT_MASK);
      }

      return createSubset(enumSet, nMaskArray);
   }

   /**
    * Returns an enumeration set of the same enumeration type as the
    * given set and containing the elements corresponding to the set
    * bits in the given int array. If all bits are set, the existing
    * master set is returned instead.
    * @param enumSet The enumeration set of the same type as the new set.
    * @param nMaskArray An int array containing bit masks, indicating whether
    * each element in the enumeration appears in the new subset. Indexing starts
    * from the lowest bit of the int at index 0 and corresponds to the element
    * ordering of the master set of enumSet.
    * @return An enumeration set of the same type as enumSet and containing
    * the elements corresponding to the set bits in the given int array.
    * The set is newly allocated, unless it is the master set.
    */
   protected static EnumSet createSubset(EnumSet enumSet, int[] nMaskArray)
   {
      if (nMaskArray != null)
      {
         int i = nMaskArray.length - 1;
         int nLastMaskSize = enumSet.m_sValueArray.length & BIT_MASK;

         if (nLastMaskSize != 0) // last mask not full
         {
            int nMask = ~(-1 << nLastMaskSize);

            if ((nMaskArray[i--] & nMask) != nMask)
            {
               return new EnumSet(enumSet.m_masterSet, nMaskArray);
            }
         }

         while (i >= 0)
         {
            if (nMaskArray[i--] != -1)
            {
               return new EnumSet(enumSet.m_masterSet, nMaskArray);
            }
         }
      }

      return enumSet.m_masterSet;
   }

   /**
    * Returns the complement of this EnumSet, which is of the same
    * enumeration type and contains all the elements that do not appear
    * in this EnumSet.
    * @return The complement of this EnumSet.
    */
   public EnumSet complement()
   {
      if (m_bEmpty)
      {
         return m_masterSet;
      }

      int nMaskLength = getMaskArrayLength();
      int[] nMaskArray = new int[nMaskLength];

      if (m_nMaskArray != null)
      {
         int nLastMaskSize = m_sValueArray.length & BIT_MASK;

         if (nLastMaskSize != 0) // last mask not full
         {
            --nMaskLength;
            nMaskArray[nMaskLength] = m_nMaskArray[nMaskLength] ^ ~(-1 << nLastMaskSize);
         }

         for (int i = 0; i < nMaskLength; i++)
         {
            nMaskArray[i] = ~m_nMaskArray[i];
         }
      }

      return new EnumSet(m_masterSet, nMaskArray);
   }

   /**
    * Determines whether the given object is contained in this EnumSet.
    * @param element The element to look up.
    * @return True if element is found in this set, false otherwise.
    */
   public boolean contains(Object element)
   {
      String sValue;
      Object index;

      if (m_bEmpty || (sValue = getString(element)) == null || (index = m_map.get(sValue)) == null)
      {
         return false;
      }

      if (m_nMaskArray == null)
      {
         return true;
      }

      int nIndex = ((Integer)index).intValue();

      return (m_nMaskArray[nIndex >> BIT_SHIFT] & (1 << (nIndex & BIT_MASK))) != 0;
   }

   /**
    * Returns the difference of this and the given EnumSet, which is an EnumSet
    * of the same enumeration type containing the elements that appear in this
    * set but not in the given set, or null if the given set is of a different
    * enumeration type.
    * @param enumSet The enumeration set with which the difference is computed.
    * @return The difference of this set and the given set or null if the given
    * set is of a different enumeration type.
    */
   public EnumSet difference(EnumSet enumSet)
   {
      if (m_map != enumSet.m_map)
      {
         return null;
      }

      if (m_bEmpty || enumSet.m_bEmpty)
      {
         return this;
      }

      if (m_nMaskArray == null)
      {
         return enumSet.complement();
      }

      if (enumSet.m_nMaskArray == null || this == enumSet)
      {
         return m_masterSet.complement();
      }

      int nLength = m_nMaskArray.length;
      int[] nMaskArray = new int[nLength];

      for (int i = 0; i < nLength; i++)
      {
         nMaskArray[i] = m_nMaskArray[i] & ~enumSet.m_nMaskArray[i];
      }

      return new EnumSet(m_masterSet, nMaskArray);
   }

   /**
    * Determines if this EnumSet contains the exact same elements as
    * the given EnumSet. The two sets do not necessarily belong to the
    * same enumeration type.
    * @param enumSet The enumeration set to compare to.
    * @return True if the given set contains the same values.
    */
   public boolean equals(EnumSet enumSet)
   {
      return subsets(enumSet) && enumSet.subsets(this);
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      return obj instanceof EnumSet && equals((EnumSet)obj);
   }

   /**
    * @return The length of the mask array, based on the number of
    * elements in the enumeration.
    */
   protected int getMaskArrayLength()
   {
      return (m_sValueArray.length + BIT_MASK) >> BIT_SHIFT;
   }

   /**
    * Returns the index of the given element, according to the element
    * ordering of the master set, or null if the element is not found.
    * @param element The element to look up.
    * @return The index of element, or null if it is not found.
    */
   public Integer getMasterIndex(Object element)
   {
      String sValue = getString(element);

      return (sValue == null) ? null : (Integer)m_map.get(sValue);
   }

   /**
    * Returns the master set of the same enumeration type as this EnumSet.
    * @return The master set.
    */
   public EnumSet getMasterSet()
   {
      return m_masterSet;
   }

   /**
    * Returns the String value of the given element, or throws an exception
    * if the element type is not supported. Supported types are String, Symbol,
    * and OIDHolder.
    * @param element The element whose String value is returned.
    * @return The String value of element.
    */
   protected static String getString(Object element)
   {
      if (element instanceof String)
      {
         return (String)element;
      }

      if (element instanceof Symbol)
      {
         return ((Symbol)element).getName();
      }

      if (element instanceof OIDHolder)
      {
         OID oid = ((OIDHolder)element).getOID();

         if (oid == null)
         {
            return null;
         }

         Object[] valueArray = oid.getValueArray();

         if (valueArray.length > 0 && valueArray[0] instanceof String)
         {
            return (String)valueArray[0];
         }

         throw new ScriptingException("err.scripting.enum.invalidOID");
      }

      throw new ScriptingException("err.scripting.enum.unsupportedValueType");
   }

   /**
    * Returns the intersection of this and the given EnumSet, which is an EnumSet
    * of the same enumeration type containing the elements that appear in both
    * sets, or null if the given set is of a different enumeration type.
    * @param enumSet The enumeration set with which the intersection is formed.
    * @return The intersection of this and the given set, or null if the given
    * set is of a different enumeration type.
    */
   public EnumSet intersection(EnumSet enumSet)
   {
      if (m_map != enumSet.m_map)
      {
         return null;
      }

      if (enumSet.m_nMaskArray == null || m_bEmpty || this == enumSet)
      {
         return this;
      }

      if (m_nMaskArray == null || enumSet.m_bEmpty)
      {
         return enumSet;
      }

      int nLength = m_nMaskArray.length;
      int[] nMaskArray = new int[nLength];

      for (int i = 0; i < nLength; i++)
      {
         nMaskArray[i] = m_nMaskArray[i] & enumSet.m_nMaskArray[i];
      }

      return new EnumSet(m_masterSet, nMaskArray);
   }

   /**
    * @return An iterator over the String values of the elements of this EnumSet in order.
    */
   public Iterator iterator()
   {
      if (m_nMaskArray == null)
      {
         return new ArrayIterator(m_sValueArray);
      }

      return new EnumSetIterator();
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write("enumset(");

      Iterator itr = iterator();

      if (itr.hasNext())
      {
         writer.write((String)itr.next());

         while (itr.hasNext())
         {
            writer.write(", ");
            writer.write((String)itr.next());
         }
      }

      writer.write(')');
   }

   /**
    * Returns the projection of this set onto the universe of the given
    * set, which is an EnumSet of the same enumeration type as the
    * latter and contains any elements in this set that also belong
    * in the master set of the given set.
    * @param enumSet The enumeration set whose universe is projected onto.
    * @return The projection of this set onto the universe of the given set,
    * which has the same enumeration type as the EnumSet argument.
    */
   public EnumSet projection(EnumSet enumSet)
   {
      if (m_map == enumSet.m_map)
      {
         return this;
      }

      int[] nMaskArray = new int[enumSet.getMaskArrayLength()];
      Iterator itr = iterator();

      while (itr.hasNext())
      {
         Object index = enumSet.m_map.get(itr.next());

         if (index != null)
         {
            int nIndex = ((Integer)index).intValue();

            nMaskArray[nIndex >> BIT_SHIFT] |= 1 << (nIndex & BIT_MASK);
         }
      }

      return createSubset(enumSet, nMaskArray);
   }

   /**
    * Returns true if every element contained in this set is also an element
    * of the given set, and if every element contained the master set of this
    * set is also an element of the master set of the given set.
    * @param enumSet The enumeration set to compare to.
    * @return True if this set and its master set are subsets of the given set
    * and its master set, respectively.
    */
   public boolean subsets(EnumSet enumSet)
   {
      if (this == enumSet || m_masterSet == enumSet)
      {
         return true;
      }

      Iterator itr = iterator();

      while (itr.hasNext())
      {
         if (!enumSet.contains(itr.next()))
         {
            return false;
         }
      }

      return m_nMaskArray == null || m_masterSet.subsets(enumSet.m_masterSet);
   }

   /**
    * Returns the elements contained in this EnumSet as a Pair.
    * @return The elements contained in this EnumSet as a Pair.
    */
   public Pair toPair()
   {
      Pair head = null;
      Pair tail = null;

      for (Iterator itr = iterator(); itr.hasNext();)
      {
         Pair pair = new Pair(Symbol.define((String)itr.next()));

         if (tail == null)
         {
            head = tail = pair;
         }
         else
         {
            tail.setTail(pair);
            tail = pair;
         }
      }
      
      return head;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return PrintWriter.toString(this);
   }

   /**
    * Returns the union of this and the given EnumSet, which is an EnumSet of
    * the same enumeration type containing the elements that appear in either
    * sets, or null if the given set is of a different enumeration type.
    * @param enumSet The enumeration set with which the union is formed.
    * @return The union of this and the given set, or null if the given set is
    * of a different enumeration type.
    */
   public EnumSet union(EnumSet enumSet)
   {
      if (m_map != enumSet.m_map)
      {
         return null;
      }

      if (m_nMaskArray == null || enumSet.m_bEmpty || this == enumSet)
      {
         return this;
      }

      if (enumSet.m_nMaskArray == null || m_bEmpty)
      {
         return enumSet;
      }

      int nLength = m_nMaskArray.length;
      int[] nMaskArray = new int[nLength];

      for (int i = 0; i < nLength; i++)
      {
         nMaskArray[i] = m_nMaskArray[i] | enumSet.m_nMaskArray[i];
      }

      return createSubset(m_masterSet, nMaskArray);
   }

   /**
    * Returns a representation of this enumeration set that can be exported through RPC.
    * @param sName The name of the enumeration set variable.
    * @return A representation of this enumeration set that can be exported through RPC.
    */
   public Object marshal(String sName)
   {
      return (sName.startsWith(GLOBAL_OBJECT_PREFIX)) ? toPair() : null;
   }

   /**
    * EnumSet iterator.
    */
   protected class EnumSetIterator implements Iterator
   {
      // attributes

      /**
       * The current index into the mask array.
       */
      protected int m_nMaskIndex = 0;

      /**
       * The current bit index.
       */
      protected int m_nBitIndex = -1;

      /**
       * Flag indicating whether the iterator has more elements.
       */
      protected boolean m_bHasNext;

      // constructor

      public EnumSetIterator()
      {
         if (m_bHasNext = !m_bEmpty)
         {
            incr();
         }
      }

      // operations

      /**
       * Advances to the next item.
       * @return True if there is next item.
       */
      protected boolean incr()
      {
         while (m_nMaskIndex < m_nMaskArray.length)
         {
            if (m_nBitIndex != BIT_MASK)
            {
               m_nBitIndex = Integer.numberOfTrailingZeros(m_nMaskArray[m_nMaskIndex] & (-1 << (m_nBitIndex + 1)));

               if (m_nBitIndex <= BIT_MASK)
               {
                  return true;
               }
            }

            m_nMaskIndex++;
            m_nBitIndex = -1;
         }

         return m_bHasNext = false;
      }

      /**
       * @see java.util.Iterator#hasNext()
       */
      public boolean hasNext()
      {
         return m_bHasNext;
      }

      /**
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         int nIndex = m_nMaskIndex << BIT_SHIFT | m_nBitIndex;

         if (m_bHasNext)
         {
            incr();

            return m_sValueArray[nIndex];
         }

         throw new java.util.NoSuchElementException("nIndex=" + nIndex
            + ", m_sValueArray.length=" + m_sValueArray.length);
      }

      /**
       * @see java.util.Iterator#remove()
       */
      public void remove()
      {
         throw new ScriptingException("err.scripting.readOnlyObject");
      }
   }

   /**
    * @see nexj.core.util.Named#getName()
    */
   public String getName()
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.meta.Type#isPrimitive()
    */
   public boolean isPrimitive()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.Type#getBaseType()
    */
   public Type getBaseType()
   {
      return null;
   }

   /**
    * @see nexj.core.meta.Type#isUpcast(nexj.core.meta.Type)
    */
   public boolean isUpcast(Type type)
   {
      return type == this;
   }

   /**
    * @see nexj.core.meta.Type#convert(java.lang.Object)
    */
   public Object convert(Object value) throws TypeConversionException
   {
      throw new TypeConversionException(this);
   }
}
