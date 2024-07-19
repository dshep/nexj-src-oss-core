package nexj.core.persistence;

/**
 * Generic implementation of a Cartesian product iterator.
 * To use, override the hasNextDirect(), nextDirect(), and reset() template methods.
 */
public abstract class GenericCartesianProductIterator implements CartesianProductIterator
{
   // attributes

   /**
    * The number of child iterator nodes.
    */
   protected int m_nChildCount;

   // associations

   /**
    * The current iteration value at this node. Useful for child nodes
    * that are dependent on their parent node.
    */
   protected Object m_current;

   /**
    * The child iterator nodes.
    */
   protected CartesianProductIterator[] m_childArray;

   // operations

   /**
    * @see nexj.core.persistence.CartesianProductIterator#add(nexj.core.persistence.CartesianProductIterator)
    */
   public void add(CartesianProductIterator child)
   {
      if (m_childArray == null)
      {
         m_childArray = new CartesianProductIterator[2];
      }

      if (m_nChildCount == m_childArray.length)
      {
         CartesianProductIterator[] childArray = new CartesianProductIterator[m_nChildCount << 1];

         System.arraycopy(m_childArray, 0, childArray, 0, m_nChildCount);
         m_childArray = childArray;
      }

      m_childArray[m_nChildCount++] = child;
   }

   /**
    * @see nexj.core.persistence.CartesianProductIterator#hasNext()
    */
   public boolean hasNext()
   {
      for (int i = m_nChildCount - 1; i >= 0; i--)
      {
         if (m_childArray[i].hasNext())
         {
            return true;
         }
      }

      return hasNextDirect();
   }

   /**
    * @see nexj.core.persistence.CartesianProductIterator#next()
    */
   public Object next()
   {
      int nNext;

      for (nNext = m_nChildCount - 1; nNext >= 0; nNext--)
      {
         if (advanceChild(nNext, false))
         {
            break;
         }
      }

      if (nNext < 0)
      {
         m_current = nextDirect();
      }

      for (nNext++; nNext < m_nChildCount; nNext++)
      {
         advanceChild(nNext, true);
      }

      return m_current;
   }

   /**
    * Advances the child iterator to the next item.
    * @param nOrdinal The child iterator ordinal.
    * @param bReset True to reset the child before advancing.
    * @return True if the child iterator moved to the next item; false otherwise.
    */
   protected boolean advanceChild(int nOrdinal, boolean bReset)
   {
      CartesianProductIterator child = m_childArray[nOrdinal];

      if (bReset)
      {
         child.reset();
      }

      if (child.hasNext())
      {
         child.next();

         return true;
      }

      return false;
   }

   /**
    * @see nexj.core.persistence.CartesianProductIterator#reset()
    */
   public abstract void reset();

   /**
    * @return The next element at this level of the iteration.
    * @see java.util.Iterator#next()
    */
   public abstract Object nextDirect();

   /**
    * @return True if there are more items at this level of the iteration.
    * @see java.util.Iterator#hasNext()
    */
   public abstract boolean hasNextDirect();

   /**
    * @see java.util.Iterator#remove()
    */
   public void remove()
   {
      throw new UnsupportedOperationException();
   }
}
