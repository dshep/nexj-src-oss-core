// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.match;

import nexj.core.runtime.Context;
import nexj.core.scripting.Pair;
import nexj.core.scripting.ScriptingException;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

/**
 * An enum of valid match expression operator symbols.
 */
public abstract class MatchNode
{
   /**
    * & (...) (...) (...) -> (...) & (...) & (...)
    */
   public final static MatchNode AND = new MatchNode(Symbol.AND)
   {
      protected double evaluateNode(String sValue, Pair expression,
         IntArrayRef intArray, Context context) throws ScriptingException
      {
         boolean bExclude = false;
         double nScore = -1; // mark first score

         while ((expression = expression.getNext()) != null) // need all for nMaxScore
         {
            double nValue = evaluate(sValue, expression.getHead(), intArray, context);
            double nItem = Math.abs(nValue);

            if (nValue == Double.NEGATIVE_INFINITY)
            {
               continue; // ignore excluded expressions that did not match
            }

            bExclude |= nValue < 0; // propagate to exclude record from evaluate()
            nScore = (nScore < 0) ? nItem : Math.min(nScore, nItem); // take min score
         }

         if (nScore < 0)
         {
            nScore = 0; // no expressions evaluated to a score
         }

         return (bExclude) ? (0 - nScore) : nScore;
      }

      protected Pair parseNode(Pair expression) throws ScriptingException
      {
         Object next = expression.getTail();

         if (!(next instanceof Pair) || ((Pair)next).getTail() == null)
         {
            return expression; // require at least two arguments
         }

         for (Pair fault = expression; next != null; fault = (Pair)next, next = fault.getTail())
         {
            if (!(next instanceof Pair))
            {
               return fault;
            }

            parse(((Pair)next).getHead());
         }

         return null;
      }
   };

   /**
    * ~ "..."
    */
   public final static MatchNode FUZZY = new MatchNode(Symbol.LIKE_P)
   {
      // Modified Levenshtein distance algorithm to compute fuzzy score, 0 == no match
      // @see http://en.wikipedia.org/wiki/Levenshtein_distance
      protected double evaluateNode(String sHaystack, Pair expression,
         IntArrayRef intArray, Context context) throws ScriptingException
      {
         String sNeedle =
            expression.getNext().getHead().toString().toUpperCase(context.getLocale()); // case
         int nNumExact = -1;                                                         // insensitive
         int nHaystackCount = sHaystack.length();
         int nNeedleCount = sNeedle.length();
         long nScore = nNeedleCount; // worst possible score
         int nCount = nNeedleCount << 1;
         int nOffset = 0; // offset to track which index of m_nScoreArray is current/previous

         if (intArray.array == null || intArray.array.length < nCount + 2);
         {
            intArray.array = new int[nCount + 2];
         }

         intArray.array[0] = 0; // reset
         intArray.array[1] = 0; // reset

         for (int i = 2 + 1 - nOffset; i < intArray.array.length; i += 2)
         {
            intArray.array[i] = i >> 1; // reset
         }

         for (int i = 1; i <= nHaystackCount; ++i) // for each character in haystack
         {
            // always check from beginning for case where more exact substring repeats later
            // have to check to end due to earlier partial matches having better score
            for (int j = 1, nThis = (j << 1) + nOffset, nPrev = (j << 1) + 1 - nOffset;
                 j <= nNeedleCount;
                 ++j, nThis += 2, nPrev += 2) // for each character in needle calculate score
            {
               intArray.array[nThis] =
                  intArray.array[nPrev - 2] +
                  ((sHaystack.charAt(i - 1) == sNeedle.charAt(j - 1)) ? 0 : 1); // substitution
               intArray.array[nThis] =
                  Math.min(intArray.array[nThis], intArray.array[nPrev] + 1); // insertion
               intArray.array[nThis] =
                  Math.min(intArray.array[nThis], intArray.array[nThis - 2] + 1); //deletion
            }

            if (intArray.array[nCount + nOffset] <= nScore)
            {
               nScore = intArray.array[nCount + nOffset]; // found a better or equal score

               if (nScore == 0)
               {
                  ++nNumExact;
               }
            }

            nOffset = 1 - nOffset; // switch this/previous lines
         }

         nScore = Math.max(0, nNeedleCount - nScore);

         if (nScore == nNeedleCount)
         {
            nScore += nNeedleCount * nNumExact; // add number of additional exact matches
         }

         // max score == (nHaystackCount/nNeedleCount)*nNeedleCount
         // because nNeedleCount is score for full match of a single token
         return (nHaystackCount == 0) ? 0 : (nScore / (double)nHaystackCount);
      }

      protected Pair parseNode(Pair expression) throws ScriptingException
      {
         return (expression.getTail() instanceof Pair)
                ? VALUE.parseNode(expression.getNext())
                : expression;
      }
   };

   /**
    * ! (...)
    */
   public final static MatchNode NOT = new MatchNode(Symbol.NOT)
   {
      protected double evaluateNode(String sValue, Pair expression,
         IntArrayRef intArray, Context context) throws ScriptingException
      {
         double nValue = evaluate(sValue, expression.getNext(), intArray, context);

         // double negation == non-match, use NEGATIVE_INFINITY reserved value to indicate state
         return (nValue > 0) ? (0 - nValue) : Double.NEGATIVE_INFINITY;
      }

      protected Pair parseNode(Pair expression) throws ScriptingException
      {
         if (expression.getTail() instanceof Pair && expression.getNext().getTail() == null)
         {
            parse(expression.getNext().getHead());

            return null;
         }
         else if (SYMBOL.equals(expression.getHead()) && expression.getTail() instanceof Pair)
         {
            return null; // previously validating NOT node
         }

         return expression;
      }
   };

   /**
    * | (...) (...) (...) -> (...) | (...) | (...)
    */
   public final static MatchNode OR = new MatchNode(Symbol.OR) // same as AND
   {                                                                     // (different symbol)
      protected double evaluateNode(String sValue, Pair expression,
         IntArrayRef intArray, Context context) throws ScriptingException
      {
         double nScore = 0;

         while ((expression = expression.getNext()) != null) // need all for nMaxScore
         {
            // take max score
            nScore = Math.max(nScore, evaluate(sValue, expression.getHead(), intArray, context));
         }

         return nScore;
      }

      protected Pair parseNode(Pair expression) throws ScriptingException
      {
         Object next = expression.getTail();

         if (!(next instanceof Pair) || ((Pair)next).getTail() == null)
         {
            return expression; // require at least two arguments
         }

         for (Pair fault = expression; next != null; fault = (Pair)next, next = fault.getTail())
         {
            if (!(next instanceof Pair))
            {
               return fault;
            }

            parse(((Pair)next).getHead());
         }

         return null;
      }
   };

   /**
    * Symbol representing an actual string value.
    */
   public final static MatchNode VALUE = new MatchNode(null)
   {
      protected double evaluateNode(String sValue, Pair expression,
         IntArrayRef intArray, Context context) throws ScriptingException
      {
         String sComp =
            expression.getHead().toString().toUpperCase(context.getLocale()); // case insensitive
         int nCompLen = sComp.length();
         int nValueLen = sValue.length();
         long nScore = 0;

         for (int i = -1; (i = sValue.indexOf(sComp, i + 1)) >= 0;)
         {
            // ensure that the identified string is a complete token i.e. whitespace surrounded
            if (isToken(sValue, i, i + nCompLen))
            {
               ++nScore;
               i += nCompLen - 1; // do not count overlapping matches
            }
         }

         // maximum possible full sComp in sValue, +1 for single whitespace delimited
         return nScore / (double)((nValueLen + 1) / (nCompLen + 1));
      }

      protected Pair parseNode(Pair expression) throws ScriptingException
      {
         return (expression.getHead() instanceof String && expression.getTail() == null)
                ? null : expression;
      }

      /**
       * @return Is the substring[nStart, nEnd) a token, false == substring is a subtoken.
       */
      private boolean isToken(String sValue, int nStart, int nEnd)
      {
         // false == has a token portion prefix || has a token portion suffix
         return !((nStart != 0 && Character.isLetterOrDigit(sValue.codePointAt(nStart - 1))) ||
                  (nEnd < sValue.length() && Character.isLetterOrDigit(sValue.codePointAt(nEnd))));
      }
   };

   /**
    * * (l "...") (m "...") (n "...") <- -1.0 <= l,m,n <= 1.0 (l + m + n can be > 1.0)
    */
   public final static MatchNode WEIGHT = new MatchNode(Symbol.MUL)
   {
      protected double evaluateNode(String sValue, Pair expression,
         IntArrayRef intArray, Context context) throws ScriptingException
      {
         double nScore = 0;

         while ((expression = expression.getNext()) != null) // need all for nMaxScore
         {
            double nWeight = 1 + ((Number)((Pair)expression.getHead()).getHead()).doubleValue();
            double nValue =
               evaluate(sValue, ((Pair)expression.getHead()).getTail(), intArray, context);

            if (nValue >= 0) // skip excluded scores since their score is effectively 0
            {
               nScore = Math.min(1, nScore += nValue * nWeight); // add scores limit to max score
            }
         }

         return nScore;
      }

      protected Pair parseNode(Pair expression) throws ScriptingException
      {
         Object next = expression.getTail();

         if (!(next instanceof Pair) || ((Pair)next).getTail() == null)
         {
            return expression; // require at least two arguments
         }

         for (Pair fault = expression; next != null; fault = (Pair)next, next = fault.getTail())
         {
            if (!(next instanceof Pair) ||
                !(((Pair)next).getHead() instanceof Pair) ||
                !(((Pair)((Pair)next).getHead()).getTail() instanceof Pair)) // need >1 args
            {
               return fault; // invalid branch
            }

            Pair pair = (Pair)((Pair)next).getHead(); // (n "...") list

            fault = pair; // reset error position to current node

            if (!(pair.getHead() instanceof Number) ||
                ((Number)pair.getHead()).doubleValue() < -1 || // valid values -1.0 < = n <= 1.0
                ((Number)pair.getHead()).doubleValue() > 1 || // valid values -1.0 < = n <= 1.0
                (fault = VALUE.parseNode(pair.getNext())) != null) // not valid double + string
             {
                return fault;
             }
         }

         return null;
      }
   };

   /**
    * The symbol to match.
    */
   protected final Symbol SYMBOL;

   /**
    * Map user for finding the proper object to evaluate an expression branch, mapping a Symbol
    * object to an object that can evaluate a branch.
    * This map excludes VALUE due to lack of Symbol.
    */
   private static Lookup/*<Symbol, MatchNode>*/ m_operatorMap =
      new HashTab/*<Symbol, MatchNode>*/(5);

   static
   {
      m_operatorMap.put(AND.SYMBOL, AND);
      m_operatorMap.put(FUZZY.SYMBOL, FUZZY);
      m_operatorMap.put(NOT.SYMBOL, NOT);
      m_operatorMap.put(OR.SYMBOL, OR);
      m_operatorMap.put(WEIGHT.SYMBOL, WEIGHT);
   }

   private MatchNode(Symbol symbol)
   {
      SYMBOL = symbol;
   }

   /**
    * Computes the match/maximum score for a given value according to the given expression.
    * @param sValue The value for which to compute the score.
    * @param expression The expression to use for computing the score (must be already parsed).
    * @param intArray The object holding an int[] used for storing runtime state (can be null).
    * @param context The invocation context to query for runtime configuration.
    * @return Fractional score as a double, if -ve => value should not be included
    *         (e.g. "a not b", matching against: a => false, b => true).
    * @throws ScriptingException If an invalid expression is provided.
    */
   public static double evaluate(String sValue, Object expression,
      IntArrayRef intArray, Context context) throws ScriptingException
   {
      if (expression instanceof String)
      {
         return VALUE.evaluateNode(sValue, new Pair(expression), intArray, context);
      }
      else if (expression instanceof Pair)
      {
         MatchNode symbol = null;

         if (((Pair)expression).getHead() instanceof String)
         {
            symbol = VALUE;
         }
         else if (((Pair)expression).getHead() instanceof MatchNode)
         {
            symbol = (MatchNode)((Pair)expression).getHead();
         }
         else if (((Pair)expression).getHead() != null) // HashTab cannot handle nulls
         {
            symbol = (MatchNode)m_operatorMap.get(((Pair)expression).getHead());
         }

         if (intArray == null)
         {
            intArray = new IntArrayRef(); // create a runtime state container
         }

         if (symbol != null)
         {
            return symbol.evaluateNode(sValue, (Pair)expression, intArray, context);
         }
     }

     throw new ScriptingException("err.scripting.invalidMatchExpression",   // can get here
                                     new Object[] {expression});     // through a recursive call
   }

   /**
    * Computes the match/maximum score for a given value according to the given expression.
    * @param context The invocation context to query for runtime configuration.
    * @param intArray The object holding an int[] used for storing runtime state.
    * @param sValue The value for which to compute the score (head == matching Symbol).
    * @param expression The expression to use for computing the score.
    * @return Fractional score double, if -ve => value should not be included
    *         (e.g. "a not b", matching against: a => false, b => true).
    * @throws ScriptingException If an invalid expression is provided.
    */
   protected abstract double evaluateNode(String sValue, Pair expression,
      IntArrayRef intArray, Context context) throws ScriptingException;

   /**
    * Parse/validate argument.
    * @param expression The expression tree to parse (if not a Pair it will be wrapped into one).
    * @return Possibly modified and validated expression node.
    * @throws ScriptingException on invalid expression.
    */
   public static Pair parse(Object expression) throws ScriptingException
   {
      Object fault = expression;

      if (expression instanceof String)
      {
         expression = new Pair(expression); // wrap in a Pair
      }

      if (expression instanceof Pair)
      {
         Pair node = (Pair)expression;
         MatchNode symbol = null;

         if (node.getHead() instanceof String || Symbol.STRING.equals(node.getHead()))
         {
            symbol = VALUE;
         }
         else if (node.getHead() instanceof MatchNode)
         {
            symbol = (MatchNode)node.getHead(); // happens if parsing second time
         }
         else if (node.getHead() instanceof Symbol)
         {
            symbol = (MatchNode)m_operatorMap.get(node.getHead());
         }

         if (symbol != null && (fault = symbol.parseNode(node)) == null)
         {
            return node;
         }
      }

      throw new ScriptingException("err.scripting.invalidMatchExpression",
                                      new Object[] {fault});
   }

   /**
    * Destructively parse/validate argument replacing Symbols with MatchNodes.
    * @param expression The expression tree to parse (head == matching Symbol).
    * @return Node where parse fault has occurred or null if all valid.
    * @throws ScriptingException on invalid expression.
    */
   protected abstract Pair parseNode(Pair expression) throws ScriptingException;

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return (SYMBOL == null) ? "\"...\"" : SYMBOL.toString(); // null symbol == value
   }

   /**
    * A reference to an int[] array.
    */
   public static class IntArrayRef
   {
      public int[] array = null;
   }
}
