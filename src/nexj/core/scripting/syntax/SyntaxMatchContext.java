package nexj.core.scripting.syntax;

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;

import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.scripting.syntax.SyntaxMatcher.SyntaxVariable;

/**
 * Match binding context implementation. One per SyntaxMatcher function invocation.
 */
public class SyntaxMatchContext
{
   // constants

   /**
    * Indicator of an empty binding, i.e., that a variable is not yet bound.
    */
   protected final static String EMPTY = new String("EMPTY");

   /**
    * Indicator of a no-value match of an ellipsis pattern variable.
    */
   protected final static String NULL_VALUE = new String("NULL_VALUE");

   /**
    * The value separator.
    */
   public final static String SEPARATOR = new String("|");

   // attributes

   /**
    * The binding stack top offset. Starts at 0.
    * Disable tracking by setting this to a negative value.
    */
   protected int m_nBindingTop;

   /**
    * The number of separators between the last value of a {@link MultiValueMatch} and the next value.
    */
   protected int m_nSepCount;

   // associations

   /**
    * The transformer context. Never null.
    */
   protected final SyntaxTransformerContext m_transformerContext;

   /**
    * The syntax matcher that supplies variables for this match context. Never null.
    */
   protected final SyntaxMatcher m_matcher;

   /**
    * The binding value array, indexed by the variable identifiers.
    */
   protected final Object[] m_valueArray;

   /**
    * The parent match binding context. May be null.
    */
   protected SyntaxMatchContext m_parent;

   /**
    * Stack that tracks the variable identifiers of bindings created, to allow backtracking.
    */
   protected int[] m_bindingStack;

   // constructors

   /**
    * Creates a new binding context.
    * @param transformerContext The transformer context.
    * @param nVarCount The number of variables.
    */
   protected SyntaxMatchContext(SyntaxTransformerContext transformerContext, SyntaxMatcher matcher)
   {
      m_valueArray = new Object[matcher.getVariableCount()];
      m_bindingStack = new int[16];
      m_nBindingTop = -1;
      m_transformerContext = transformerContext;
      m_matcher = matcher;

      Arrays.fill(m_valueArray, EMPTY);
   }

   // operations

   /**
    * @return The parent match context.
    */
   protected SyntaxMatchContext getParent()
   {
      return m_parent;
   }

   /**
    * Sets the parent match context.
    * @param context The parent match context.
    */
   protected void setParent(SyntaxMatchContext context)
   {
      m_parent = context;
   }

   /**
    * Returns true if the given matcher is associated with this match context.
    * @param matcher The matcher to test.
    * @return True if the given matcher is associated with this match context.
    */
   protected boolean equalsMatcher(SyntaxMatcher matcher)
   {
      return m_matcher == matcher;
   }

   /**
    * Adds the given value to the variable binding.
    * @param var The variable.
    * @param value The value of the binding.
    */
   protected void addBinding(SyntaxVariable var, Object value)
   {
      int nVarId = var.varId;

      value = processSymbols(value);

      if (var.ellipsisCount == 0)
      {
         assert m_valueArray[nVarId] == EMPTY;

         m_valueArray[nVarId] = value;
      }
      else
      {
         if (m_valueArray[nVarId] == EMPTY)
         {
            m_valueArray[nVarId] = new MultiValueMatch(value);
         }
         else
         {
            ((MultiValueMatch)m_valueArray[nVarId]).add(value, m_nSepCount);
         }
      }

      trackVariable(nVarId);
   }

   /**
    * Processes the symbols in the given Scheme expression.
    * @param expr The expression.
    * @return The processed expression.
    */
   protected Object processSymbols(Object expr)
   {
      if (expr instanceof Symbol)
      {
         return m_transformerContext.getIdentifier((Symbol)expr);
      }

      if (expr instanceof Pair)
      {
         Pair pair = (Pair)expr;

         return new Pair(processSymbols(pair.getHead()), processSymbols(pair.getTail()));
      }

      return expr;
   }

   /**
    * Adds a null-value marker to the variable binding.
    * @param The variable.
    */
   protected void markNull(SyntaxVariable var)
   {
      assert var.ellipsisCount != 0;

      int nVarId = var.varId;

      if (m_valueArray[nVarId] == EMPTY)
      {
         m_valueArray[nVarId] = new MultiValueMatch(NULL_VALUE);
      }
      else
      {         
         ((MultiValueMatch)m_valueArray[nVarId]).add(NULL_VALUE, m_nSepCount);
      }

      trackVariable(nVarId);
   }

   /**
    * Returns true if the value is a null-value marker.
    * @param value The value to check.
    * @return True if the value is a null-value marker.
    */
   public static boolean isNull(Object value)
   {
      return value == NULL_VALUE;
   }

   /**
    * Increments the separator counter.
    * @return The old separator counter value.
    */
   protected int incrSepCount()
   {
      return m_nSepCount++;
   }

   /**
    * Resets the separator counter after matching an ellipsis pattern.
    */
   protected void resetSepCount()
   {
      m_nSepCount = 1;
   }

   /**
    * Sets the separator counter to the given value.
    * @param nSepCount The new separator counter value.
    */
   protected void resetSepCount(int nSepCount)
   {
      m_nSepCount = nSepCount;
   }

   /**
    * Enables tracking and returns true if tracking state was changed.
    * @return True if tracking was previously disabled.
    */
   protected boolean enableTracking()
   {
      if (m_nBindingTop < 0)
      {
         m_nBindingTop = 0;

         return true;
      }

      return false;
   }

   /**
    * Marks a restore point.
    */
   protected void markRestorePoint()
   {
      trackVariable(-1);
   }

   /**
    * Undoes changes to the binding map since the most recent restore point.
    */
   protected void restore()
   {
      while (m_nBindingTop > 0)
      {
         int nVarIndex = m_bindingStack[--m_nBindingTop];

         if (nVarIndex == -1)
         {
            return;
         }

         Object value = m_valueArray[nVarIndex];

         if (value instanceof MultiValueMatch)
         {
            ((MultiValueMatch)value).pop();
         }
         else
         {
            m_valueArray[nVarIndex] = EMPTY;
         }
      }
   }

   /**
    * Disables tracking if it was previously disabled.
    * @param bSavedState The saved tracking state. True if tracking was previously disabled.
    */
   protected void disableTracking(boolean bSavedState)
   {
      if (bSavedState)
      {
         m_nBindingTop = -1;
      }
   }

   /**
    * Tracks a binding variable to allow backtracking, if tracking is enabled.
    * @param nVarId The identifier of the variable used as key in the binding being tracked.
    */
   protected void trackVariable(int nVarId)
   {
      if (m_nBindingTop == -1)
      {
         return;
      }

      if (m_nBindingTop == m_bindingStack.length)
      {
         int[] stack = new int[m_nBindingTop << 1];

         System.arraycopy(m_bindingStack, 0, stack, 0, m_nBindingTop);
         m_bindingStack = stack;
      }

      m_bindingStack[m_nBindingTop++] = nVarId;
   }

   /**
    * Returns the value bound to the variable with the given identifier.
    * @param nVarId The variable identifier.
    * @return The value bound to the identified variable.
    */
   protected Object getValue(int nVarId)
   {
      Object value = m_valueArray[nVarId];

      if (value == EMPTY)
      {
         throw new NoSuchElementException();
      }

      return value;
   }

   // inner classes

   /**
    * Multi-value match implementation, for pattern variables with ellipses.
    */
   protected static class MultiValueMatch
   {
      // attributes

      /**
       * The value count.
       */
      protected int m_nLength;

      // associations

      /**
       * The value array.
       */
      protected Object[] m_valueArray;

      // constructors

      /**
       * Creates a new multi-value match.
       * @param firstValue The first value.
       */
      public MultiValueMatch(Object firstValue)
      {
         m_valueArray = new Object[8];
         m_valueArray[0] = firstValue;
         m_nLength = 1;
      }

      // operations

      /**
       * Adds the given value.
       * @param value The value.
       * @param nSepCount The number of separators between the last value (if any) and
       * the one being added. Must be at least 1 if this MultiValueMatch is not empty.
       */
      public void add(Object value, int nSepCount)
      {
         assert nSepCount > 0 || m_nLength == 0;

         int nRequiredLength = m_nLength + nSepCount + 1;

         if (nRequiredLength > m_valueArray.length)
         {
            Object[] array = new Object[nRequiredLength << 1];

            System.arraycopy(m_valueArray, 0, array, 0, m_nLength);
            m_valueArray = array;
         }

         for (int i = 0; i < nSepCount; i++)
         {
            m_valueArray[m_nLength++] = SEPARATOR;
         }

         m_valueArray[m_nLength++] = value;
      }

      /**
       * Pops the most recently added value and its separators.
       */
      public void pop()
      {
         if (m_nLength > 0)
         {
            m_nLength--; // pops the last value

            // pops separators before the last value
            while (m_nLength > 0 && m_valueArray[m_nLength - 1] == SEPARATOR)
            {
               m_nLength--;
            }
         }
      }

      /**
       * Creates a new iterator over the values.
       * @param nullValue The value used to indicate a no-value return.
       * @return A new value iterator.
       */
      public MultiValueMatchIterator iterator()
      {
         return new MultiValueMatchIterator();
      }

      // inner classes

      /**
       * {@link MultiValueMatch} iterator class implementation.
       */
      protected class MultiValueMatchIterator implements Iterator
      {
         // attributes

         /**
          * The current index.
          */
         protected int m_nIndex;

         /**
          * The number of wrap-arounds that have occurred.
          */
         protected int m_nWrapCount;

         // operations

         /**
          * Advances the index, skipping over the indicated maximum number of separators,
          * until a value is reached.
          * @param nMaxCount The maximum number of separators to skip over.
          * @return True if the index points to a value after skipping over separators.
          * False if the index points to a separator or past the end of the value array.
          */
         protected boolean skipToValue(int nMaxCount)
         {
            if (m_nIndex >= m_nLength)
            {
               return false;
            }

            for (int nEnd = m_nIndex + nMaxCount; m_nIndex < nEnd; m_nIndex++)
            {
               if (m_valueArray[m_nIndex] != SEPARATOR)
               {
                  return true;
               }
            }

            return m_valueArray[m_nIndex] != SEPARATOR;
         }

         /**
          * Returns the variable value at the current index, or the no-value indicator
          * if the index does not point to a value. No separators are skipped.
          * @return The value at the current index, or the no-value indicator.
          */
         public Object next()
         {
            if (m_nIndex < m_nLength)
            {
               Object value = m_valueArray[m_nIndex++];

               if (value != SEPARATOR)
               {
                  return value;
               }
            }

            return NULL_VALUE;
         }

         /**
          * @see java.util.Iterator#hasNext()
          */
         public boolean hasNext()
         {
            return m_nIndex < m_nLength;
         }

         /**
          * @return True if the index is at the start of the array.
          */
         protected boolean isReset()
         {
            return m_nIndex == 0;
         }

         /**
          * @return The number of wrap-arounds.
          */
         protected int getWrapCount()
         {
            return m_nWrapCount;
         }

         /**
          * Resets the iterator to wrap around the value array.
          */
         protected void wrap()
         {
            m_nIndex = 0;
            m_nWrapCount++;
         }

         /**
          * @see java.util.Iterator#remove()
          */
         public void remove()
         {
            throw new UnsupportedOperationException();
         }
      }
   }
}
