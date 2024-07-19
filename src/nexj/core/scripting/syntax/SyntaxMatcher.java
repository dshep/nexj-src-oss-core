package nexj.core.scripting.syntax;

import java.io.Serializable;
import java.lang.reflect.Array;

import nexj.core.meta.Primitive;
import nexj.core.scripting.Function;
import nexj.core.scripting.ImmutableLookup;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.ScriptingException;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.PrintWriter;

/**
 * Pattern matcher implementation.
 */
public class SyntaxMatcher implements Function, Serializable
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = 8215985098174742368L;

   /**
    * The pattern for the underscore symbol (match any).
    */
   public final static SyntaxPattern UNDERSCORE_PATTERN = new SyntaxPattern()
   {
      /**
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SyntaxPattern#match(java.lang.Object, SyntaxMatchContext)
       */
      public boolean match(Object expr, SyntaxMatchContext matchContext)
      {
         return true;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SyntaxPattern#markNull(SyntaxMatchContext)
       */
      public void markNull(SyntaxMatchContext matchContext)
      {
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return "_";
      }
   };

   // attributes

   /**
    * The next identifier to assign to a symbol template with ellipses within the scope of this matcher.
    */
   protected int m_nTemplateId;

   // associations

   /**
    * The pattern object.
    */
   protected final Object m_pattern;

   /**
    * Mappings from each literal to its corresponding {@link SyntaxPattern} object.
    */
   protected final Lookup m_literalMap; // of type SyntaxPattern[Symbol]

   /**
    * Mappings from each symbol to its {@link SyntaxVariable}.
    */
   protected final Lookup m_varMap; // of type SyntaxVariable[Symbol]

   /**
    * The parent matcher. May be null.
    */
   protected final SyntaxMatcher m_parent;

   // constructors

   /**
    * Creates a new SyntaxMatcher.
    * @param pattern The pattern.
    * @param literalMap Mappings from each literal to its corresponding {@link SyntaxPattern} object.
    * @param parent The parent matcher. May be null.
    */
   public SyntaxMatcher(Object pattern, Lookup literalMap, SyntaxMatcher parent)
   {
      assert literalMap != null;

      Lookup varMap = new HashTab();

      m_literalMap = literalMap;
      m_parent = parent;
      m_pattern = processPattern(pattern, 0, varMap);
      m_varMap = new ImmutableLookup(varMap);
   }

   // operations

   /**
    * Processes and returns a map with the given literals as keys and their {@link SyntaxPattern} objects as values.
    * @param literalList A Scheme list of literals in the transformer.
    * @return Mappings from each literal to its corresponding {@link SyntaxPattern} object.
    */
   public static Lookup createLiteralMap(Object literalList) throws ClassCastException
   {
      Lookup map = new HashTab();

      for (Pair pair = (Pair)literalList; pair != null; pair = pair.getNext())
      {
         Object literal = pair.getHead();

         if (literal instanceof Symbol)
         {
            if (!Symbol.ELLIPSIS.equals(literal) && !Symbol.UNDERSCORE.equals(literal))
            {
               if (!map.contains(literal))
               {
                  map.put(literal, new Literal((Symbol)literal));
               }

               continue;
            }
         }

         throw new ScriptingException("err.scripting.syntax.invalidLiteral", new Object[]{literal});
      }

      return new ImmutableLookup(map);
   }

   /**
    * Returns true if the argument is a literal symbol in this matcher or any parent matcher.
    * @param arg The argument to look up.
    * @return True if the argument is a literal symbol.
    */
   public boolean isLiteral(Symbol arg)
   {
      return m_literalMap.contains(arg) || m_parent != null && m_parent.isLiteral(arg);
   }

   /**
    * Returns true if the argument is a pattern variable in this matcher or any parent matcher.
    * @param arg The argument to look up.
    * @return True if the argument is a pattern variable.
    */
   public boolean isVariable(Symbol arg)
   {
      return m_varMap.contains(arg) || m_parent != null && m_parent.isVariable(arg);
   }

   /**
    * Returns the {@link SyntaxVariable} object associated with the given symbol in this
    * matcher or any parent matcher, or null if not found.
    * @param arg The argument to look up.
    * @return The {@link SyntaxVariable} object associated with the given symbol, or null.
    */
   public SyntaxVariable getVariable(Symbol arg)
   {
      SyntaxVariable var = (SyntaxVariable)m_varMap.get(arg);

      if (var == null && m_parent != null)
      {
         return m_parent.getVariable(arg);
      }

      return var;
   }

   /**
    * @return The number of variables in this matcher.
    */
   public int getVariableCount()
   {
      return m_varMap.size();
   }

   /**
    * @return The next template identifier.
    */
   protected int getNextTemplateId()
   {
      return m_nTemplateId++;
   }

   /**
    * @return An immutable iterator over the variable map of this matcher. The keys are
    * the symbols representing the variables, and the values are {@link SyntaxVariable} objects.
    */
   public Lookup.Iterator iterator()
   {
      return m_varMap.iterator();
   }

   /**
    * @return The root pattern, as a Scheme expression, if the pattern matches the format
    * of a function call; null otherwise.
    */
   public Pair getPattern()
   {
      if (m_pattern instanceof ListPattern)
      {
         return ((ListPattern)m_pattern).getFunctionCallPattern();
      }

      return null;
   }

   /**
    * @return The number of arguments accepted by the root pattern, if it matches the format
    * of a function; -1 otherwise.
    */
   public int getArgCount()
   {
      return (m_pattern instanceof ListPattern) ? ((ListPattern)m_pattern).getArgCount() : -1;
   }

   /**
    * @return True if the root pattern matches a list of a variable number of elements.
    */
   public boolean acceptsVarArgs()
   {
      return m_pattern instanceof ListPattern && ((ListPattern)m_pattern).acceptsVarArg();
   }

   /**
    * Processes the given pattern and verifies that it is valid.
    * A pattern can be:
    * - The ellipsis symbol, the underscore symbol, a symbol contained in the literal set,
    *   or a symbol not already contained in the pattern variable map.
    * - A proper or improper list of patterns, containing at most one ellipsis symbol. If exists,
    *   the ellipsis must not appear at the beginning, or at the end of an improper list.
    * - A vector of patterns, containing at most one ellipsis symbol. If exists, the ellipsis
    *   must not appear at the beginning.
    * - Any other Scheme datum.
    * @param pattern The pattern.
    * @param nEllipsisCount The number of ellipses applied to this pattern (directly or through nesting).
    * @param varMap Mappings from each symbol to its {@link SyntaxVariable}.
    * @return The processed pattern object. Can be any Scheme object or a {@link SyntaxPattern}.
    * @throws ClassCastException
    */
   protected Object processPattern(Object pattern, int nEllipsisCount, Lookup varMap) throws ClassCastException
   {
      if (pattern instanceof Symbol)
      {
         Symbol symbol = (Symbol)pattern;

         if (Symbol.UNDERSCORE.equals(symbol))
         {
            return UNDERSCORE_PATTERN;
         }

         if (Symbol.ELLIPSIS.equals(symbol))
         {
            return Symbol.ELLIPSIS;
         }

         Object literalTemplate = m_literalMap.get(symbol);

         if (literalTemplate != null)
         {
            return literalTemplate;
         }

         if (varMap.contains(symbol))
         {
            throw new ScriptingException("err.scripting.syntax.dupPatternVar", new Object[]{symbol});
         }

         pattern = new SyntaxVariable(symbol, nEllipsisCount, varMap.size(), this);
         varMap.put(symbol, pattern);
      }
      else if (pattern instanceof Pair)
      {
         Object current = pattern;
         Pair pair;
         boolean bProper = true;
         int nEllipsisIndex = -1;
         int i;

         for (i = 0; current instanceof Pair; i++)
         {
            pair = (Pair)current;

            if (Symbol.ELLIPSIS.equals(pair.getHead()))
            {
               verifyEllipsisPosition(nEllipsisIndex, i);
               nEllipsisIndex = i;
            }

            current = pair.getTail();
         }

         if (current != null)
         {
            if (Symbol.ELLIPSIS.equals(current))
            {
               // (<pattern>+ . <ellipsis>)
               throw new ScriptingException("err.scripting.syntax.invalidEllipsisPosition");
            }

            bProper = false;
            i++;
         }

         int nCount = (nEllipsisIndex == -1) ? i : i - 1;
         Pair patternPair = new Pair(null);
         Pair currentPair = patternPair;

         for (i = 1; pattern instanceof Pair; i++)
         {
            pair = (Pair)pattern;

            if (nEllipsisIndex == i)
            {
               current = new Ellipsis(
                  processPattern(pair.getHead(), nEllipsisCount + 1, varMap));
               pair = pair.getNext();
            }
            else
            {
               current = processPattern(pair.getHead(), nEllipsisCount, varMap);
            }

            Pair next = new Pair(current);

            currentPair.setTail(next);
            currentPair = next;
            pattern = pair.getTail();
         }

         if (pattern != null)
         {
            currentPair.setTail(processPattern(pattern, nEllipsisCount, varMap));
         }

         pattern = ListPattern.create(patternPair.getNext(), nCount, bProper, nEllipsisIndex != -1);
      }
      else if (pattern != null && pattern.getClass().isArray())
      {
         int nEllipsisIndex = -1;
         int nLength = Array.getLength(pattern);
         Object[] array = new Object[nLength];

         for (int i = nLength - 1; i >= 0; i--)
         {
            Object subpattern = processPattern(Array.get(pattern, i), nEllipsisCount, varMap);

            if (Symbol.ELLIPSIS.equals(subpattern))
            {
               verifyEllipsisPosition(nEllipsisIndex, i); // ensures i != 0
               nEllipsisIndex = i;
               subpattern = processPattern(Array.get(pattern, --i), nEllipsisCount + 1, varMap);

               if (Symbol.ELLIPSIS.equals(subpattern))
               {
                  throw new ScriptingException("err.scripting.syntax.maxPatternEllipsisCount");
               }

               array[i] = new Ellipsis(subpattern);
            }
            else
            {
               array[i] = subpattern;
            }
         }

         if (nEllipsisIndex == -1)
         {
            pattern = new VectorPattern(array);
         }
         else
         {
            pattern = new VectorEllipsisPattern(array);
         }
      }

      return pattern;
   }

   /**
    * Verifies that the ellipsis position is valid.
    * @param nPrevIndex The index of the previous ellipsis in the pattern sequence. -1 if none.
    * @param nCurrentIndex The index of the ellipsis being verified.
    */
   protected static void verifyEllipsisPosition(int nPrevIndex, int nCurrentIndex)
   {
      if (nPrevIndex != -1)
      {
         throw new ScriptingException("err.scripting.syntax.maxPatternEllipsisCount");
      }

      if (nCurrentIndex == 0)
      {
         throw new ScriptingException("err.scripting.syntax.invalidEllipsisPosition");
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "matcher:" + PrintWriter.toString(m_pattern);
   }

   /**
    * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
    */
   public boolean invoke(int nArgCount, Machine machine)
   {
      if (nArgCount != 1)
      {
         throw new ScriptingException("err.scripting.syntax.transformerArgCount",
            new Object[]{toString(), Primitive.ONE_INTEGER, Primitive.createInteger(nArgCount)});
      }

      Object expr = machine.getArg(0, nArgCount);

      machine.returnValue(Boolean.valueOf(machine.getTransformerContext().match(this, expr)), nArgCount);

      return false;
   }

   /**
    * Returns true if the expression matches this pattern. Variable binding values are
    * distributed to {@link SyntaxExpander} objects registered to this matcher.
    * @param expr The expression to match.
    * @param matchContext The binding match context.
    * @return True if the expression matches this pattern.
    */
   public boolean match(Object expr, SyntaxMatchContext matchContext)
   {
      return genericMatch(expr, m_pattern, matchContext);
   }

   /**
    * Returns true if the expression matches against the pattern.
    * @param expr The expression.
    * @param pattern The pattern.
    * @param matchContext The match binding context.
    * @return True if the expression matches against the pattern.
    */
   protected static boolean genericMatch(Object expr, Object pattern, SyntaxMatchContext matchContext)
   {
      if (pattern instanceof SyntaxPattern)
      {
         return ((SyntaxPattern)pattern).match(expr, matchContext);
      }

      return Intrinsic.equal(expr, pattern);
   }

   /**
    * Adds a null-value marker to all variables found in pattern.
    * @param pattern The pattern.
    * @param matchContext The match binding context.
    */
   protected static void genericMarkNull(Object pattern, SyntaxMatchContext matchContext)
   {
      if (pattern != null)
      {
         if (pattern instanceof Pair)
         {
            Pair pair = (Pair)pattern;

            genericMarkNull(pair.getHead(), matchContext);
            genericMarkNull(pair.getTail(), matchContext);
         }
         else if (pattern instanceof SyntaxPattern)
         {
            ((SyntaxPattern)pattern).markNull(matchContext);
         }
         else if (pattern instanceof Object[])
         {
            Object[] array = (Object[])pattern;

            for (int i = 0; i < array.length; i++)
            {
               genericMarkNull(array[i], matchContext);
            }
         }
      }
   }

   // inner interfaces

   /**
    * Pattern interface.
    */
   protected static interface SyntaxPattern
   {
      /**
       * Returns true if the expression matches against this pattern.
       * @param expr The expression.
       * @param matchContext The match binding context.
       * @return True if the expression matches against the pattern.
       */
      boolean match(Object expr, SyntaxMatchContext matchContext);

      /**
       * Adds a null-value marker to all variables found in this pattern.
       * @param matchContext The match binding context.
       */
      void markNull(SyntaxMatchContext matchContext);
   }

   // inner classes

   /**
    * Literal pattern. Each literal is a Symbol and has to be matched by identity.
    */
   protected static class Literal implements SyntaxPattern
   {
      // associations

      /**
       * The literal.
       */
      protected final Symbol m_literal;

      // constructors

      /**
       * Creates a new {@link SyntaxPattern} that represents a literal.
       * @param literal The literal.
       */
      protected Literal(Symbol literal)
      {
         m_literal = literal;
      }

      // operations

      /**
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SyntaxPattern#match(java.lang.Object, SyntaxMatchContext)
       */
      public boolean match(Object expr, SyntaxMatchContext matchContext)
      {
         return m_literal.equals(expr);
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SyntaxPattern#markNull(SyntaxMatchContext)
       */
      public void markNull(SyntaxMatchContext matchContext)
      {
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return m_literal.toString();
      }
   }

   /**
    * Variable pattern.
    */
   public static class SyntaxVariable implements SyntaxPattern
   {
      // attributes

      /**
       * The number of pattern ellipses applied to this variable.
       */
      public final int ellipsisCount;

      /**
       * The variable identifier. Unique per matcher.
       */
      public final int varId;

      // associations

      /**
       * The variable symbol.
       */
      public final Symbol symbol;

      /**
       * The matcher whose pattern contains this pattern variable.
       */
      public final SyntaxMatcher matcher;

      // constructors

      /**
       * Creates a new variable.
       * @param symbol The variable symbol.
       * @param nEllipsisCount The number of pattern ellipses applied to this variable.
       * @param nVarId The variable identifier. Unique per matcher.
       * @param matcher The matcher whose pattern contains this pattern variable.
       */
      protected SyntaxVariable(Symbol symbol, int nEllipsisCount, int nVarId, SyntaxMatcher matcher)
      {
         ellipsisCount = nEllipsisCount;
         varId = nVarId;
         this.symbol = symbol;
         this.matcher = matcher;
      }

      // operations

      /**
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SyntaxPattern#match(java.lang.Object, nexj.core.scripting.syntax.SyntaxMatchContext)
       */
      public boolean match(Object expr, SyntaxMatchContext matchContext)
      {
         matchContext.addBinding(this, expr);

         return true;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SyntaxPattern#markNull(nexj.core.scripting.syntax.SyntaxMatchContext)
       */
      public void markNull(SyntaxMatchContext matchContext)
      {
         matchContext.markNull(this);
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return symbol.toString();
      }
   }

   /**
    * Sequence pattern containing 0 or 1 ellipsis.
    */
   protected static abstract class SequencePattern implements SyntaxPattern
   {
      // attributes

      /**
       * The number of subpatterns in this sequence pattern.
       */
      protected final int m_nCount;

      /**
       * The sequence pattern. Either a Pair or an Object[].
       */
      protected final Object m_pattern;

      // constructors

      /**
       * Creates a new {@link SyntaxPattern} that represents a sequence containing no ellipsis.
       * @param pattern The sequence. Either a Pair or an Object[].
       * @param nCount The number of subpatterns in this sequence pattern.
       */
      protected SequencePattern(Object pattern, int nCount)
      {
         m_pattern = pattern;
         m_nCount = nCount;
      }

      // operations

      /**
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SyntaxPattern#match(java.lang.Object, nexj.core.scripting.syntax.SyntaxMatcher.SyntaxMatchContext)
       */
      public final boolean match(Object expr, SyntaxMatchContext matchContext)
      {
         int nEllipsisMatchCount = verifyExpression(expr);

         if (nEllipsisMatchCount < 0)
         {
            return false;
         }

         return match(expr, nEllipsisMatchCount, matchContext);
      }

      /**
       * Returns the expected number of matches against an ellipsis pattern or an improper tail pattern.
       * @param nLength The length of the expression being matched.
       * @param nMultiMatchPatternCount The number of patterns that can match 0 or more elements
       * (either an ellipsis pattern or an improper tail pattern). 
       * @return The expected number of matches.
       */
      protected final int getMultiValueMatchCount(int nLength, int nMultiMatchPatternCount)
      {
         return nLength - m_nCount + nMultiMatchPatternCount;
      }

      /**
       * Verifies that the expression is a potential match, based on length.
       * @param expr The expression.
       * @return A negative number if the expression is not a potential match.
       * If the pattern contains an ellipsis, the return value is the expected
       * number of ellipsis matches.
       */
      protected abstract int verifyExpression(Object expr);

      /**
       * Returns true if the expression matches against this pattern.
       * @param expr The expression.
       * @param nEllipsisMatchCount The number of ellipsis matches.
       * Ignored if the pattern does not contain any ellipsis.
       * @param matchContext The match binding context.
       * @return True if the expression matches against this pattern.
       */
      protected abstract boolean match(Object expr, int nEllipsisMatchCount, SyntaxMatchContext matchContext);

      /**
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SyntaxPattern#markNull(SyntaxMatchContext)
       */
      public void markNull(SyntaxMatchContext matchContext)
      {
         genericMarkNull(m_pattern, matchContext);
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return PrintWriter.toString(m_pattern);
      }
   }

   /**
    * List pattern containing 0 or 1 ellipsis.
    */
   protected static abstract class ListPattern extends SequencePattern
   {
      // constructors

      /**
       * Creates a new {@link SyntaxPattern} that represents a Scheme list.
       * @param pattern The Pair object.
       * @param nCount The number of elements in the Pair object.
       */
      protected ListPattern(Pair pattern, int nCount)
      {
         super(pattern, nCount);
      }

      // operations

      /**
       * Creates a {@link ListPattern} that represents a Scheme list pattern.
       * @param pattern The Scheme list pattern.
       * @param nCount The number of subpatterns in this list.
       * @param bProper Whether the pattern is a proper list.
       * @param bEllipsis Whether the list contains an ellipsis among its subpatterns.
       * @return A new {@link SequencePattern}.
       */
      protected static ListPattern create(Pair pattern, int nCount, boolean bProper, boolean bEllipsis)
      {
         if (bEllipsis)
         {
            if (bProper)
            {
               return new ProperListEllipsisPattern(pattern, nCount);
            }

            return new ImproperListEllipsisPattern(pattern, nCount);
         }

         if (bProper)
         {
            return new ProperListPattern(pattern, nCount);
         }

         return new ImproperListPattern(pattern, nCount);
      }

      /**
       * @return This pattern as a Scheme expression, if it matches the format of a function call; null otherwise.
       */
      protected Pair getFunctionCallPattern()
      {
         Pair pair = (Pair)m_pattern;
         Object head = pair.getHead();

         if (head == UNDERSCORE_PATTERN || head instanceof SyntaxVariable
             || (head instanceof Literal && !Symbol.SET.equals(((Literal)head).m_literal)))
         {
            return pair;
         }

         return null;
      }

      /**
       * @return The number of arguments accepted by this pattern, which is 1 less than the element count.
       */
      protected int getArgCount()
      {
         return m_nCount - 1;
      }

      /**
       * @return True if this pattern matches a list of a variable number of elements.
       */
      protected boolean acceptsVarArg()
      {
         return true;
      }
   }

   /**
    * Proper list pattern containing no ellipses.
    */
   protected static class ProperListPattern extends ListPattern
   {
      // constructors

      /**
       * Creates a new {@link SyntaxPattern} that represents a proper list containing no ellipses.
       * @param pattern The Pair object.
       * @param nCount The number of elements in the Pair object.
       */
      protected ProperListPattern(Pair pattern, int nCount)
      {
         super(pattern, nCount);
      }

      // operations

      /**
       * Verifies that the given expression is a proper list of the same length as the pattern.
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SequencePattern#verifyExpression(java.lang.Object)
       */
      protected int verifyExpression(Object expr)
      {
         int nLength = 0;

         for (; expr instanceof Pair; nLength++)
         {
            expr = ((Pair)expr).getTail();
         }

         return (expr == null && m_nCount == nLength) ? 0 : -1;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SequencePattern#match(java.lang.Object, int, nexj.core.scripting.syntax.SyntaxMatcher.SyntaxMatchContext)
       */
      protected boolean match(Object expr, int nEllipsisMatchCount, SyntaxMatchContext matchContext)
      {
         Pair pat = (Pair)m_pattern;
         Pair pair = (Pair)expr;

         while (pat != null)
         {
            if (genericMatch(pair.getHead(), pat.getHead(), matchContext))
            {
               pair = pair.getNext();
               pat = pat.getNext();
            }
            else
            {
               return false;
            }
         }

         return true;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxMatcher.ListPattern#acceptsVarArg()
       */
      protected boolean acceptsVarArg()
      {
         return false;
      }
   }

   /**
    * Proper list pattern containing 1 ellipsis at the top level.
    */
   protected static class ProperListEllipsisPattern extends ListPattern
   {
      // constructors

      /**
       * Creates a new {@link SyntaxPattern} that represents a proper list containing an ellipsis.
       * @param pattern The Pair object.
       * @param nCount The number of elements in the Pair object, not including the ellipsis.
       */
      protected ProperListEllipsisPattern(Pair pattern, int nCount)
      {
         super(pattern, nCount);
      }

      // operations

      /**
       * Verifies that the given expression is a proper list with length at least
       * the length of this pattern minus 1.
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SequencePattern#verifyExpression(java.lang.Object)
       */
      protected int verifyExpression(Object expr)
      {
         int nLength = 0;

         for (; expr instanceof Pair; nLength++)
         {
            expr = ((Pair)expr).getTail();
         }

         return (expr == null) ? getMultiValueMatchCount(nLength, 1) : -1;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SequencePattern#match(java.lang.Object, int, nexj.core.scripting.syntax.SyntaxMatcher.SyntaxMatchContext)
       */
      protected boolean match(Object expr, int nEllipsisMatchCount, SyntaxMatchContext matchContext)
      {
         Pair pat = (Pair)m_pattern;
         Pair pair = (Pair)expr;

         while (pat != null)
         {
            Object head = pat.getHead();

            if (head instanceof Ellipsis)
            {
               Ellipsis ellipsis = (Ellipsis)head;
               int nCurSepCount = matchContext.incrSepCount();

               try
               {
                  if (nEllipsisMatchCount == 0)
                  {
                     ellipsis.markNull(matchContext);
                  }
                  else
                  {
                     for (int i = 0; i < nEllipsisMatchCount; i++)
                     {
                        if (!ellipsis.match(pair.getHead(), matchContext))
                        {
                           return false;
                        }

                        matchContext.resetSepCount();
                        pair = pair.getNext();
                     }
                  }
               }
               finally
               {
                  matchContext.resetSepCount(nCurSepCount);
               }
            }
            else if (genericMatch(pair.getHead(), head, matchContext))
            {
               pair = pair.getNext();
            }
            else
            {
               return false;
            }

            pat = pat.getNext();
         }

         return true;
      }
   }

   /**
    * Improper list pattern containing no ellipses.
    */
   protected static class ImproperListPattern extends ListPattern
   {
      // constructors

      /**
       * Creates a new {@link SyntaxPattern} that represents an improper list containing no ellipses.
       * @param pattern The Pair object.
       * @param nCount The number of elements in the Pair object, including the last non-Pair tail.
       */
      protected ImproperListPattern(Pair pattern, int nCount)
      {
         super(pattern, nCount);
      }

      // operations

      /**
       * Verifies that the given expression is a (proper or improper) list whose length is
       * greater than or equal to the length of the pattern.
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SequencePattern#verifyExpression(java.lang.Object)
       */
      protected int verifyExpression(Object expr)
      {
         int nLength = 0;

         for (; expr instanceof Pair; nLength++)
         {
            expr = ((Pair)expr).getTail();
         }

         return getMultiValueMatchCount(nLength, 1);
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SequencePattern#match(java.lang.Object, int, nexj.core.scripting.syntax.SyntaxMatcher.SyntaxMatchContext)
       */
      protected boolean match(Object expr, int nEllipsisMatchCount, SyntaxMatchContext matchContext)
      {
         Pair pat = (Pair)m_pattern;
         Pair pair = (Pair)expr;

         for (int i = 2; i < m_nCount; i++) // stop at 2 elements from the end
         {
            if (!genericMatch(pair.getHead(), pat.getHead(), matchContext))
            {
               return false;
            }

            pair = pair.getNext();
            pat = pat.getNext();
         }

         return genericMatch(pair.getHead(), pat.getHead(), matchContext)
             && genericMatch(pair.getTail(), pat.getTail(), matchContext);
      }
   }

   /**
    * Improper list pattern containing 1 ellipsis at the top level.
    */
   protected static class ImproperListEllipsisPattern extends ListPattern
   {
      // constructors

      /**
       * Creates a new {@link SyntaxPattern} that represents an improper list containing an ellipsis.
       * @param pattern The Pair object.
       * @param nCount The number of elements in the Pair object, not including the ellipsis but
       * including the last non-Pair tail.
       */
      protected ImproperListEllipsisPattern(Pair pattern, int nCount)
      {
         super(pattern, nCount);
      }

      // operations

      /**
       * Verifies that the given expression is a (proper or improper) list with length
       * at least the length of this pattern minus 1.
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SequencePattern#verifyExpression(java.lang.Object)
       */
      protected int verifyExpression(Object expr)
      {
         int nLength = 0;

         for (; expr instanceof Pair; nLength++)
         {
            expr = ((Pair)expr).getTail();
         }

         return getMultiValueMatchCount(nLength, 2); // last match can be null
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SequencePattern#match(java.lang.Object, int, nexj.core.scripting.syntax.SyntaxMatcher.SyntaxMatchContext)
       */
      protected boolean match(Object expr, int nEllipsisMatchCount, SyntaxMatchContext matchContext)
      {
         Pair pair = (Pair)expr;
         Pair pat = (Pair)m_pattern;
         Object head = pat.getHead();

         if (!(head instanceof Ellipsis))
         {
            for (;;)
            {
               if (!genericMatch(pair.getHead(), head, matchContext))
               {
                  return false;
               }

               pat = pat.getNext();
               head = pat.getHead();
               pair = pair.getNext();

               if (head instanceof Ellipsis)
               {
                  break;
               }
            }
         }

         boolean bSavedState = matchContext.enableTracking();

         try
         {
            return (matchFromEllipsisPattern(pair, (Ellipsis)head, pat.getTail(), nEllipsisMatchCount, matchContext, true));
         }
         finally
         {
            matchContext.disableTracking(bSavedState);
         }
      }

      /**
       * Returns true if the expression matches against the ellipsis pattern and the remaining patterns, if any.
       * The ellipsis pattern is matched against the first elements of the Pair expression; the number of matches
       * must be between the given match count and 0 (inclusive). A greedy match is performed, so that the number
       * of elements from the input expression that are matched with the ellipsis pattern is maximized.
       * @param expr The expression.
       * @param ellipsis The ellipsis pattern.
       * @param pattern The pattern(s) following the {@link Ellipsis} pattern.
       * @param nEllipsisMatchCount The maximum number of ellipsis matches.
       * @param matchContext The match binding context.
       * @param bNullEllipsisMatch True if the ellipsis pattern hasn't been matched.
       * @return True if the expression matches against the pattern.
       */
      protected boolean matchFromEllipsisPattern(Pair expr, Ellipsis ellipsis, Object pattern,
         int nEllipsisMatchCount, SyntaxMatchContext matchContext, boolean bNullEllipsisMatch)
      {
         if (nEllipsisMatchCount > 0)
         {
            matchContext.markRestorePoint();

            int nCurSepCount = matchContext.incrSepCount();

            if (!bNullEllipsisMatch)
            {
               matchContext.resetSepCount();
            }

            if (ellipsis.match(expr.getHead(), matchContext))
            {
               matchContext.resetSepCount(nCurSepCount);

               if (matchFromEllipsisPattern(expr.getNext(), ellipsis, pattern, nEllipsisMatchCount - 1, matchContext, false))
               {
                  return true;
               }
            }

            matchContext.restore();
         }
         else if (bNullEllipsisMatch)
         {
            int nCurSepCount = matchContext.incrSepCount();

            ellipsis.markNull(matchContext);
            matchContext.resetSepCount(nCurSepCount);
         }

         Object nextExpr = expr;

         for (Pair patternPair; pattern instanceof Pair; pattern = patternPair.getTail(), nextExpr = expr.getTail())
         {
            patternPair = (Pair)pattern;
            expr = (Pair)nextExpr;

            if (!genericMatch(expr.getHead(), patternPair.getHead(), matchContext))
            {
               return false;
            }
         }

         return genericMatch(nextExpr, pattern, matchContext);
      }
   }

   /**
    * Vector pattern containing no ellipses.
    */
   protected static class VectorPattern extends SequencePattern
   {
      // constructors

      /**
       * Creates a new {@link SyntaxPattern} that represents a vector containing no ellipses.
       * @param pattern The vector.
       */
      protected VectorPattern(Object[] pattern)
      {
         super(pattern, pattern.length);
      }

      // operations

      /**
       * Verifies that the given expression is an array of the same length as this pattern.
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SequencePattern#verifyExpression(java.lang.Object)
       */
      protected int verifyExpression(Object expr)
      {
         return (expr != null && expr.getClass().isArray() && Array.getLength(expr) == m_nCount) ? 0 : -1;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SequencePattern#match(java.lang.Object, int, nexj.core.scripting.syntax.SyntaxMatcher.SyntaxMatchContext)
       */
      protected boolean match(Object expr, int nEllipsisMatchCount, SyntaxMatchContext matchContext)
      {
         Object[] patternArray = (Object[])m_pattern;

         for (int i = 0; i < m_nCount; i++)
         {
            if (!genericMatch(Array.get(expr, i), patternArray[i], matchContext))
            {
               return false;
            }
         }

         return true;
      }
   }

   /**
    * Vector pattern containing 1 ellipsis at the top level.
    */
   protected static class VectorEllipsisPattern extends SequencePattern
   {
      // constructors

      /**
       * Creates a new {@link SyntaxPattern} that represents a vector containing an ellipsis.
       * @param pattern The vector, with a null value following the ellipsis pattern.
       */
      protected VectorEllipsisPattern(Object[] pattern)
      {
         super(pattern, pattern.length - 1);
      }

      // operations

      /**
       * Verifies that the given expression is an array of the length at least the length of this pattern minus 1.
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SequencePattern#verifyExpression(java.lang.Object)
       */
      protected int verifyExpression(Object expr)
      {
         return (expr != null && expr.getClass().isArray()) ? getMultiValueMatchCount(Array.getLength(expr), 1) : -1;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SequencePattern#match(java.lang.Object, SyntaxMatchContext)
       */
      protected boolean match(Object expr, int nEllipsisMatchCount, SyntaxMatchContext matchContext)
      {
         Object[] patternArray = (Object[])m_pattern;

         for (int nExprIndex = 0, nPatIndex = 0; nPatIndex <= m_nCount; nPatIndex++)
         {
            if (patternArray[nPatIndex] instanceof Ellipsis)
            {
               Ellipsis ellipsis = (Ellipsis)patternArray[nPatIndex++]; // next pattern is null
               int nCurSepCount = matchContext.incrSepCount();

               try
               {
                  if (nEllipsisMatchCount == 0)
                  {
                     ellipsis.markNull(matchContext);
                  }
                  else
                  {
                     for (int nEnd = nExprIndex + nEllipsisMatchCount; nExprIndex < nEnd; nExprIndex++)
                     {
                        if (!ellipsis.match(Array.get(expr, nExprIndex), matchContext))
                        {
                           return false;
                        }

                        matchContext.resetSepCount();
                     }
                  }
               }
               finally
               {
                  matchContext.resetSepCount(nCurSepCount);
               }
            }
            else if (!genericMatch(Array.get(expr, nExprIndex++), patternArray[nPatIndex], matchContext))
            {
               return false;
            }
         }

         return true;
      }
   }

   /**
    * Wrapper for a pattern that is followed by an ellipsis.
    */
   protected static class Ellipsis implements SyntaxPattern
   {
      // attributes

      /**
       * The pattern followed by an ellipsis. Can be any Scheme object or a {@link SyntaxPattern}.
       */
      protected final Object m_pattern;

      // constructors

      /**
       * Creates a new ellipsis pattern.
       * @param pattern The pattern followed by an ellipsis. Can be any Scheme object or a {@link SyntaxPattern}.
       */
      protected Ellipsis(Object pattern)
      {
         this.m_pattern = pattern;
      }

      // operations

      /**
       * Returns true if the expression matches against this pattern.
       * @param expr The expression.
       * @return True if the expression matches against the pattern.
       */
      public boolean match(Object expr, SyntaxMatchContext matchContext)
      {
         return genericMatch(expr, m_pattern, matchContext);
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxMatcher.SyntaxPattern#markNull(SyntaxMatchContext)
       */
      public void markNull(SyntaxMatchContext matchContext)
      {
         genericMarkNull(m_pattern, matchContext);
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return m_pattern.toString() + " ...";
      }
   }
}
