package nexj.core.scripting.syntax;

import java.io.Serializable;
import java.lang.reflect.Array;

import nexj.core.meta.Primitive;
import nexj.core.scripting.Function;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.ScriptingException;
import nexj.core.scripting.Symbol;
import nexj.core.scripting.syntax.SyntaxMatchContext.MultiValueMatch.MultiValueMatchIterator;
import nexj.core.scripting.syntax.SyntaxMatcher.SyntaxVariable;
import nexj.core.util.PrintWriter;

/**
 * Transformer expander implementation.
 */
public class SyntaxExpander implements Function, Serializable
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = -5619242033807744629L;

   /**
    * Indicator of empty expansion output for internal use.
    */
   protected final static Pair NULL = new Pair("NULL");

   /**
    * Variable code for templates containing no variable.
    */
   protected final static byte NO_VARIABLE = 0;

   /**
    * Variable code for templates containing single-valued variables that do not loop.
    */
   protected final static byte SINGLE_VALUED_VARIABLE = 1;

   /**
    * Variable code for templates containing looping single-valued variables.
    */
   protected final static byte LOOPING_SINGLE_VALUED_VARIABLE = 2;

   /**
    * Variable code for templates containing looping multi-valued variables.
    */
   protected final static byte LOOPING_MULTI_VALUED_VARIABLE = 4;

   /**
    * Variable code for templates containing multi-valued variables that do not loop.
    */
   protected final static byte MULTI_VALUED_VARIABLE = 8;

   /**
    * Mask to get the variable code bits indicating looping variables.
    */
   protected final static byte LOOPING_VARIABLE_MASK = LOOPING_SINGLE_VALUED_VARIABLE | LOOPING_MULTI_VALUED_VARIABLE;

   /**
    * Mask to get the variable code bits indicating multi-valued variables.
    */
   protected final static byte MULTI_VALUED_VARIABLE_MASK = LOOPING_MULTI_VALUED_VARIABLE | MULTI_VALUED_VARIABLE;

   // associations

   /**
    * The root template to expand.
    */
   protected final Template m_rootTemplate;

   // constructors

   /**
    * Creates an expander.
    */
   private SyntaxExpander(Template template)
   {
      m_rootTemplate = template;
   }

   // operations

   /**
    * Creates a new syntax expander from the given expression.
    * @param expr The template expression.
    * @param matcher The matcher that supplies variable bindings to this expander. May not be null.
    * @return A new syntax expander.
    */
   public static SyntaxExpander createExpander(Object expr, SyntaxMatcher matcher)
   {
      Template rootTemplate = process(expr, 0, 0, false, matcher);

      if (rootTemplate instanceof SequenceTemplate && ((SequenceTemplate)rootTemplate).isUnanchored())
      {
         throw new ScriptingException("err.scripting.syntax.maxTemplateEllipsisCount");
      }

      return new SyntaxExpander(rootTemplate);
   }

   /**
    * Verifies that the given template is valid and processes the raw template expression, so that:<br />
    * - Pattern data and identifiers that are not pattern variables or ellipses are not substituted
    *   in the output expansion.<br />
    * - A subtemplate followed by an ellipsis must contain at least 1 pattern variable at some nested
    *   level. This subtemplate will expand into 0 or more occurrences.<br />
    * - Pattern variables that occur in subpatterns followed by one or more ellipses may occur only
    *   in subtemplates that are followed by (at least) as many ellipses.<br />
    * - Expansion maintains the bindings and value distribution of the input expression.<br />
    * - If a pattern variable is followed by more ellipses in the subtemplate than in the associated
    *   subpattern, the input form is replicated as necessary.
    *   The subtemplate must contain at least one pattern variable from a subpattern followed by an
    *   ellipsis, and for at least one such pattern variable, the subtemplate must be followed by
    *   exactly as many ellipses as the subpattern in which the pattern variable appears.
    * @param expr The template expression.
    * @param nDirectEllipses Number of ellipses directly following the template expression being processed.
    * @param nTotalEllipses Number of ellipses that apply to the template expression being processed
    * (both directly and through nesting).
    * @param bLiteralEllipsis Flag indicating if ellipses are to be processed literally (as normal symbols).
    * @param matcher The matcher that supplies variable bindings to this expander. May not be null.
    * @return The processed Template object.
    */
   protected static Template process(Object expr, int nDirectEllipses, int nTotalEllipses, boolean bLiteralEllipsis, SyntaxMatcher matcher)
   {
      if (expr != null)
      {
         if (expr instanceof Symbol)
         {
            return processSymbol((Symbol)expr, nDirectEllipses, nTotalEllipses, matcher);
         }

         if (expr instanceof Pair)
         {
            return processPair((Pair)expr, nDirectEllipses, nTotalEllipses, bLiteralEllipsis, matcher);
         }

         if (expr.getClass().isArray())
         {
            return processVector(expr, nDirectEllipses, nTotalEllipses, bLiteralEllipsis, matcher);
         }

         if (expr instanceof Template)
         {
            return (Template)expr;
         }
      }

      if (nDirectEllipses != 0)
      {
         throw new ScriptingException("err.scripting.syntax.invalidEllipsis",
            new Object[]{expr, Primitive.createInteger(nDirectEllipses)});
      }

      return new ConstantTemplate(expr);
   }

   /**
    * Processes a Symbol template expression and returns a Template object.
    * @param symbol The Symbol object.
    * @param nDirectEllipses Number of ellipses directly following the template expression being processed.
    * @param nTotalEllipses Number of ellipses that apply to the template expression being processed
    * (both directly and through nesting).
    * @param matcher The matcher that supplies variable bindings to this expander. May not be null.
    * @return The processed Template object.
    */
   protected static Template processSymbol(Symbol symbol, int nDirectEllipses, int nTotalEllipses, SyntaxMatcher matcher)
   {
      assert nDirectEllipses >= 0;
      assert nTotalEllipses >= nDirectEllipses;

      if (Symbol.UNDERSCORE.equals(symbol) || Symbol.ELLIPSIS.equals(symbol) || matcher.isLiteral(symbol))
      {
         return new ConstantTemplate(symbol);
      }

      SyntaxVariable var = matcher.getVariable(symbol);
      Template template;

      if (var == null) // Not a recognized pattern variable.
      {
         if (nDirectEllipses != 0)
         {
            throw new ScriptingException("err.scripting.syntax.invalidEllipsis",
               new Object[]{symbol, Primitive.createInteger(nDirectEllipses)});
         }

         template = new ConstantTemplate(symbol);
      }
      else
      {
         if (var.ellipsisCount > nTotalEllipses)
         {
            throw new ScriptingException("err.scripting.syntax.minEllipsisCount",
               new Object[]{var.symbol, Primitive.createInteger(var.ellipsisCount),
                  Primitive.createInteger(nTotalEllipses)});
         }

         if (var.ellipsisCount == 0)
         {
            if (nDirectEllipses != 0)
            {
               throw new ScriptingException("err.scripting.syntax.maxTemplateEllipsisCount.detailed",
                  new Object[]{var.symbol, Primitive.ZERO_INTEGER,
                     Primitive.createInteger(nTotalEllipses)}); // error message uses total ellipsis count
            }

            template = new VariableTemplate(var, nTotalEllipses);
         }
         else
         {
            template = new VariableEllipsisTemplate(
               var, nDirectEllipses, nTotalEllipses, matcher.getNextTemplateId());
         }
      }

      return template;
   }

   /**
    * Processes a Pair template expression and returns a Template object. The global escape ## is prepended
    * to the first head of the list only, if this head resolves to a globally defined function.
    * @param pair The Pair template expression.
    * @param nDirectEllipses Number of ellipses directly following the template expression being processed.
    * @param nTotalEllipses Number of ellipses that apply to the template expression being processed
    * (both directly and through nesting).
    * @param bLiteralEllipsis Flag indicating if ellipses are to be processed literally (as normal symbols).
    * @param matcher The matcher that supplies variable bindings to this expander. May not be null.
    * @return The processed Template object.
    */
   protected static Template processPair(Pair pair, int nDirectEllipses, int nTotalEllipses, boolean bLiteralEllipsis, SyntaxMatcher matcher)
   {
      Object head = pair.getHead();
      Object tail = pair.getTail();
      Object nextHead;

      if (Symbol.ELLIPSIS.equals(head))
      {
         // (... <template>)
         if (tail instanceof Pair)
         {
            Pair tailPair = (Pair)tail;

            if (tailPair.getTail() == null)
            {
               return process(tailPair.getHead(), 0, nTotalEllipses, true, matcher);
            }
         }

         throw new ScriptingException("err.scripting.syntax.invalidEllipsisTemplate");
      }

      final ListTemplate listTemplate = new ListTemplate();
      boolean bProcessHead = true;

      if (bLiteralEllipsis)
      {
         for (Pair nextPair; tail instanceof Pair; head = nextPair.getHead(), tail = nextPair.getTail())
         {
            nextPair = (Pair)tail;
            listTemplate.appendSubtemplate(process(head, 0, nTotalEllipses, bLiteralEllipsis, matcher));
         }
      }
      else
      {
         for (Pair nextPair; tail instanceof Pair; head = nextHead, tail = nextPair.getTail())
         {
            int nCurDirectEllipses = 0;

            nextPair = (Pair)tail;
            nextHead = nextPair.getHead();

            while (Symbol.ELLIPSIS.equals(nextHead))
            {
               nCurDirectEllipses++;
               nextHead = nextPair.getTail();

               if (nextHead instanceof Pair)
               {
                  nextPair = (Pair)nextHead;
                  nextHead = nextPair.getHead();
               }
               else
               {
                  bProcessHead = false;
                  break;
               }
            }

            listTemplate.appendSubtemplate(process(head, nCurDirectEllipses, nCurDirectEllipses + nTotalEllipses, bLiteralEllipsis, matcher));
         }
      }

      if (bProcessHead)
      {
         listTemplate.appendSubtemplate(process(head, 0, nTotalEllipses, bLiteralEllipsis, matcher));
      }

      if (tail != null)
      {
         listTemplate.appendSubtemplate(process(tail, 0, nTotalEllipses, bLiteralEllipsis, matcher));
         listTemplate.setListType(false);
      }

      if (nDirectEllipses == 0)
      {
         return listTemplate;
      }

      return new ListEllipsisTemplate(listTemplate, nDirectEllipses);
   }

   /**
    * Processes an array template expression and returns a Template object. If the array contains Scheme lists
    * as elements, the global escape ## is not added to list heads, and local variables are not renamed (similar
    * to quoted list templates).
    * @param array The array template expression.
    * @param nDirectEllipses Number of ellipses directly following the template expression being processed.
    * @param nTotalEllipses Number of ellipses that apply to the template expression being processed
    * (both directly and through nesting).
    * @param bLiteralEllipsis Flag indicating if ellipses are to be processed literally (as normal symbols).
    * @param matcher The matcher that supplies variable bindings to this expander. May not be null.
    * @return The processed Template object.
    */
   protected static Template processVector(Object array, int nDirectEllipses, int nTotalEllipses, boolean bLiteralEllipsis, SyntaxMatcher matcher)
   {
      final VectorTemplate vectorTemplate = new VectorTemplate();
      int nLength = Array.getLength(array);

      if (nLength > 0 && Symbol.ELLIPSIS.equals(Array.get(array, 0)))
      {
         throw new ScriptingException("err.scripting.syntax.invalidEllipsisPosition");
      }

      if (bLiteralEllipsis)
      {
         for (int i = 0; i < nLength; i++)
         {
            vectorTemplate.appendSubtemplate(
               process(Array.get(array, i), 0, nTotalEllipses, bLiteralEllipsis, matcher));
         }
      }
      else
      {
         for (int i = 0, k = 0; i < nLength; i = k)
         {
            int nCurDirectEllipses = 0;

            while (++k < nLength && Symbol.ELLIPSIS.equals(Array.get(array, k)))
            {
               nCurDirectEllipses++;
            }

            vectorTemplate.appendSubtemplate(
               process(Array.get(array, i), nCurDirectEllipses, nCurDirectEllipses + nTotalEllipses,
                  bLiteralEllipsis, matcher));
         }
      }

      if (nDirectEllipses == 0)
      {
         return vectorTemplate;
      }

      return new ListEllipsisTemplate(vectorTemplate, nDirectEllipses);
   }

   /**
    * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
    */
   public boolean invoke(int nArgCount, Machine machine)
   {
      if (nArgCount != 0)
      {
         throw new ScriptingException("err.scripting.syntax.transformerArgCount",
            new Object[]{toString(), Primitive.ZERO_INTEGER, Primitive.createInteger(nArgCount)});
      }

      Object expansion = expand(machine.getTransformerContext());

      machine.returnValue(expansion, nArgCount);

      return false;
   }

   /**
    * Expands the template.
    * @param transformerContext The transformer context.
    * @return The expansion (a Scheme expression).
    */
   public Object expand(SyntaxTransformerContext transformerContext)
   {
      return m_rootTemplate.expand(transformerContext);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "expander:" + m_rootTemplate.toString();
   }

   // inner interfaces

   /**
    * The Template interface.
    */
   protected interface Template
   {
      /**
       * @return A code indicating the types of variable found in this Template.
       */
      byte getVariableCode();

      /**
       * @return The previous sibling template.
       */
      Template getPrevious();

      /**
       * Sets the previous sibling template.
       * @param template The previous sibling template.
       */
      void setPrevious(Template template);

      /**
       * For templates linked to arrays of values (either directly or through nesting),
       * for each of these arrays, skip over separators until either a variable value is
       * reached or the number of separators skipped is equal to the given count, whichever
       * occurs first. For templates not linked to value arrays, no action is performed.
       * @param nMaxCount The maximum number of separators to skip over.
       * @param transformerContext The transformer context.
       * @return True if there are no more values in this round of expansion (that is,
       * if the number of separators skipped over is not enough to advance the index to
       * a value position, or if the index has advanced past the end of the array);
       * false otherwise. Always false if no value array is associated with this template.
       */
      boolean skipSeparators(int nMaxCount, SyntaxTransformerContext transformerContext);

      /**
       * Expands this template. If the expansion is a Scheme list, returns the list with its
       * innermost tail set to the given argument. If the expansion is empty, returns the tail.
       * @param tail The innermost tail.
       * @param transformerContext The transformer context.
       * @return The expansion of this template.
       */
      Object addExpansion(Object tail, SyntaxTransformerContext transformerContext);

      /**
       * Expands this template and returns the expansion.
       * @param transformerContext The transformer context.
       * @return The expansion of this template.
       */
      Object expand(SyntaxTransformerContext transformerContext);
   }

   // inner classes

   /**
    * Expander template implementation.
    */
   protected static abstract class GenericTemplate implements Template
   {
      // associations

      /**
       * The previous sibling template.
       */
      protected Template m_prevSibling;

      // operations

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.Template#getPrevious()
       */
      public Template getPrevious()
      {
         return m_prevSibling;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.Template#setPrevious(nexj.core.scripting.syntax.SyntaxExpander.Template)
       */
      public void setPrevious(Template template)
      {
         m_prevSibling = template;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.Template#getVariableCode()
       */
      public byte getVariableCode()
      {
         return NO_VARIABLE;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.Template#skipSeparators(int, nexj.core.scripting.syntax.SyntaxTransformerContext)
       */
      public boolean skipSeparators(int nMaxCount, SyntaxTransformerContext transformerContext)
      {
         return false;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.Template#addExpansion(java.lang.Object, nexj.core.scripting.syntax.SyntaxTransformerContext)
       */
      public Object addExpansion(Object tail, SyntaxTransformerContext transformerContext)
      {
         Object value = expand(transformerContext);

         // Note: have to expand() even if NONE is returned, to advance indices and keep values aligned

         if (tail == NULL || value == NULL)
         {
            return NULL;
         }

         return new Pair(value, tail);
      }
   }

   /**
    * Expander template that contains ellipses and may expand to a variable number of elements.
    */
   protected static abstract class EllipsisTemplate extends GenericTemplate
   {
      // attributes

      /**
       * The number of ellipses directly following the nested template or expression.
       */
      protected final int m_nDirectEllipses;

      /**
       * The variable code.
       */
      protected byte m_nVarCode;

      /**
       * The template expression.
       */
      protected final Object m_templateExpr;

      // constructors

      /**
       * Creates a new EllipsisTemplate.
       * @param expr The template expression.
       * @param nDirectEllipses The number of template ellipses directly following the variable.
       * @param nVarCode The variable code.
       */
      protected EllipsisTemplate(Object expr, int nDirectEllipses, byte nVarCode)
      {
         if (nVarCode == NO_VARIABLE)
         {
            throw new ScriptingException("err.scripting.syntax.invalidExpansion");
         }

         m_templateExpr = expr;
         m_nDirectEllipses = nDirectEllipses;
         m_nVarCode = nVarCode;
      }

      // operations

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.Template#expand(nexj.core.scripting.syntax.SyntaxTransformerContext)
       */
      public Object expand(SyntaxTransformerContext transformerContext)
      {
         Object value = addExpansion(null, transformerContext);

         if (value instanceof Pair)
         {
            Pair pair = (Pair)value;

            if (pair.getTail() == null)
            {
               return pair.getHead();
            }
         }

         if (value == null)
         {
            return NULL;
         }

         throw new ScriptingException("err.scripting.syntax.invalidEllipsisContext");
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.GenericTemplate#getVariableCode()
       */
      public final byte getVariableCode()
      {
         return m_nVarCode;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public final String toString()
      {
         String sValue = PrintWriter.toString(m_templateExpr);

         if (m_nDirectEllipses == 0)
         {
            return sValue;
         }

         StringBuilder str = new StringBuilder(sValue.length() + 4 * m_nDirectEllipses);

         str.append(sValue);

         for (int i = 0; i < m_nDirectEllipses; i++)
         {
            str.append(" ...");
         }

         return str.toString();
      }
   }

   /**
    * Expander template for a constant or a literal identifier.
    */
   protected static class ConstantTemplate extends GenericTemplate
   {
      // attributes

      /**
       * The template expression.
       */
      protected final Object m_templateExpr;

      // constructors

      /**
       * Creates a new ConstantTemplate.
       * @param expr The constant expression.
       */
      public ConstantTemplate(Object expr)
      {
         m_templateExpr = expr;
      }

      // operations

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.Template#expand(nexj.core.scripting.syntax.SyntaxTransformerContext)
       */
      public Object expand(SyntaxTransformerContext transformerContext)
      {
         return m_templateExpr;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return PrintWriter.toString(m_templateExpr);
      }
   }

   /**
    * Expander template for a variable identifier bound to a single value.
    */
   protected static class VariableTemplate extends ConstantTemplate
   {
      // attributes

      /**
       * The variable code.
       */
      protected final byte m_nVarCode;

      // constructors

      /**
       * Creates a new VariableTemplate.
       * @param var The corresponding pattern variable.
       * @param nTotalEllipses The total number of template ellipses applied to the variable.
       */
      public VariableTemplate(SyntaxVariable var, int nTotalEllipses)
      {
         super(var);
         assert var.ellipsisCount == 0;

         m_nVarCode = (nTotalEllipses == 0) ? SINGLE_VALUED_VARIABLE : LOOPING_SINGLE_VALUED_VARIABLE;
      }

      // operations

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.ConstantTemplate#expand(nexj.core.scripting.syntax.SyntaxTransformerContext)
       */
      public Object expand(SyntaxTransformerContext transformerContext)
      {
         return transformerContext.getValue((SyntaxVariable)m_templateExpr);
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.GenericTemplate#getVariableCode()
       */
      public byte getVariableCode()
      {
         return m_nVarCode;
      }
   }

   /**
    * Expander template for a variable bound to multiple values (i.e. it is followed by,
    * or nested in sequences that are followed by, at least 1 ellipsis in the pattern).
    * The variable may be directly followed by 0 or more ellipses in the template. The total
    * number of template ellipses (both directly and indirectly) must be at least the number
    * of pattern ellipses applied to this variable.
    */
   protected static class VariableEllipsisTemplate extends EllipsisTemplate
   {
      // attributes

      /**
       * The template identifier.
       */
      protected final int m_nTemplateId;

      // constructors

      /**
       * Creates a new VariableEllipsisTemplate.
       * @param var The corresponding pattern variable.
       * @param nDirectEllipses The number of template ellipses directly following the variable.
       * @param nTotalEllipses The total number of template ellipses applied to the variable.
       * @param nTemplateId The template identifier.
       */
      public VariableEllipsisTemplate(SyntaxVariable var, int nDirectEllipses, int nTotalEllipses, int nTemplateId)
      {
         super(var, nDirectEllipses,
            (var.ellipsisCount == nTotalEllipses) ? MULTI_VALUED_VARIABLE : LOOPING_MULTI_VALUED_VARIABLE);

         m_nTemplateId = nTemplateId;
      }

      // operations

      /**
       * Gets the iterator over the values of this variable from the expansion context.
       * @param transformerContext The transformer context.
       * @return The variable value iterator.
       */
      protected MultiValueMatchIterator getIter(SyntaxTransformerContext transformerContext)
      {
         return transformerContext.getMatchIterator((SyntaxVariable)m_templateExpr, m_nTemplateId);
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.GenericTemplate#addExpansion(java.lang.Object, nexj.core.scripting.syntax.SyntaxTransformerContext)
       */
      public final Object addExpansion(Object tail, SyntaxTransformerContext transformerContext)
      {
         ExpansionBuilder builder = new ExpansionBuilder();
         MultiValueMatchIterator iter = getIter(transformerContext);

         for (Object next = iter.next(); next != NULL; next = iter.next())
         {
            if (SyntaxMatchContext.isNull(next) && m_nDirectEllipses == 0)
            {
               skipSeparators(m_nDirectEllipses, transformerContext); // for alignment verification

               return NULL;
            }

            builder.append(next);

            if (skipSeparators(m_nDirectEllipses, transformerContext))
            {
               break;
            }
         }

         return builder.getExpansion(tail);
      }

      /**
       * @return True if there are still values to be expanded.
       */
      protected boolean hasNext(SyntaxTransformerContext transformerContext)
      {
         return getIter(transformerContext).hasNext() || m_nVarCode == LOOPING_MULTI_VALUED_VARIABLE;
      }

      /**
       * @return The number of wrap-arounds.
       */
      protected int getWrapCount(SyntaxTransformerContext transformerContext)
      {
         return getIter(transformerContext).getWrapCount();
      }

      /**
       * @return True if the variable expansion starts from the first value.
       */
      protected boolean isReset(SyntaxTransformerContext transformerContext)
      {
         return getIter(transformerContext).isReset();
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.GenericTemplate#skipSeparators(int, nexj.core.scripting.syntax.SyntaxTransformerContext)
       */
      public boolean skipSeparators(int nMaxCount, SyntaxTransformerContext transformerContext)
      {
         MultiValueMatchIterator iter = getIter(transformerContext);

         if (iter.skipToValue(nMaxCount))
         {
            return false;
         }

         if (iter.hasNext())
         {
            transformerContext.verifyAlignment(this);
         }
         else
         {
            if (m_nVarCode == LOOPING_MULTI_VALUED_VARIABLE)
            {
               iter.wrap();
            }

            transformerContext.verifyWrapAlignment(this);
         }

         return true;
      }
   }

   /**
    * Expander template for a sequence (list or vector).
    */
   protected static abstract class SequenceTemplate extends GenericTemplate
   {
      // attributes

      /**
       * The count of subtemplates containing at least one {@link VariableEllipsisTemplate}.
       */
      protected int m_nMultiValuedVarCount;

      /**
       * The variable code.
       */
      protected byte m_nVarCode;

      /**
       * Flag indicating if the expansion is a proper list.
       */
      protected boolean m_bProper = true;

      // associations

      /**
       * The last Template in the sequence.
       */
      protected Template m_lastSubtemplate;

      // operations

      /**
       * Appends the given template to the sequence.
       * @param template The template to append.
       */
      protected final void appendSubtemplate(Template template)
      {
         template.setPrevious(m_lastSubtemplate);
         m_lastSubtemplate = template;

         int nVarCode = template.getVariableCode();

         m_nVarCode |= nVarCode;

         if ((nVarCode & MULTI_VALUED_VARIABLE_MASK) != 0)
         {
            m_nMultiValuedVarCount++;
         }
      }

      /**
       * @return True if the sequence loops without an anchor, that is, it contains
       * at least 1 looping variable and no multi-valued, non-looping variable.
       */
      protected final boolean isUnanchored()
      {
         return (m_nVarCode & MULTI_VALUED_VARIABLE) == 0 && (m_nVarCode & LOOPING_VARIABLE_MASK) != 0;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.GenericTemplate#getVariableCode()
       */
      public final byte getVariableCode()
      {
         return m_nVarCode;
      }

      /**
       * @return The number of multi-valued variables nested at any level in this sequence.
       */
      protected final int getMultiValuedVariableCount()
      {
         return m_nMultiValuedVarCount;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.GenericTemplate#skipSeparators(int, nexj.core.scripting.syntax.SyntaxTransformerContext)
       */
      public final boolean skipSeparators(int nMaxCount, SyntaxTransformerContext transformerContext)
      {
         boolean bReturn = false;

         for (Template template = m_lastSubtemplate; template != null; template = template.getPrevious())
         {
            // Note: need to skip all templates
            bReturn |= template.skipSeparators(nMaxCount, transformerContext);
         }

         return bReturn;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.Template#expand(nexj.core.scripting.syntax.SyntaxTransformerContext)
       */
      public Object expand(SyntaxTransformerContext transformerContext)
      {
         if (m_lastSubtemplate == null)
         {
            return null;
         }

         Object value = (m_bProper) ? m_lastSubtemplate.addExpansion(null, transformerContext) :
            m_lastSubtemplate.expand(transformerContext);

         for (Template template = m_lastSubtemplate.getPrevious(); template != null; template = template.getPrevious())
         {
            value = template.addExpansion(value, transformerContext);
         }

         return value;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public final String toString()
      {
         StringBuilder str = new StringBuilder(128);
         Template template = m_lastSubtemplate;

         addPrefix(str);
         str.append('(');

         if (template != null)
         {
            int nIndex = str.length();

            str.append(template.toString());
            template = template.getPrevious();

            if (!m_bProper && template != null)
            {
               str.insert(nIndex, ". ");
            }

            for (; template != null; template = template.getPrevious())
            {
               str.insert(nIndex, ' ');
               str.insert(nIndex, template.toString());
            }
         }

         return str.append(')').toString();
      }

      /**
       * Appends a prefix to the character stream.
       * @param str The character stream.
       */
      protected abstract void addPrefix(StringBuilder str);
   }

   /**
    * Expander template for a list of subtemplates, either proper (by default) or improper.
    */
   protected static class ListTemplate extends SequenceTemplate
   {
      // operations

      /**
       * Sets the list type, either proper (true) or improper (false).
       * @param bProperList Flag indicating whether the list is a proper list.
       */
      protected void setListType(boolean bProperList)
      {
         m_bProper = bProperList;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.SequenceTemplate#addPrefix(java.lang.StringBuilder)
       */
      protected void addPrefix(StringBuilder str)
      {
      }
   }

   /**
    * Expander template for a vector of subtemplates.
    */
   protected static class VectorTemplate extends SequenceTemplate
   {
      // operations

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.SequenceTemplate#expand(nexj.core.scripting.syntax.SyntaxTransformerContext)
       */
      public Object expand(SyntaxTransformerContext transformerContext)
      {
         Object value = super.expand(transformerContext);

         if (value == NULL)
         {
            return NULL;
         }

         if (value == null)
         {
            return Intrinsic.EMPTY_ARRAY;
         }

         if (!(value instanceof Pair))
         {
            throw new ScriptingException("err.scripting.syntax.invalidExpansion");
         }

         return Pair.toArray((Pair)value);
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.SequenceTemplate#addPrefix(java.lang.StringBuilder)
       */
      protected void addPrefix(StringBuilder str)
      {
         str.append('#');
      }
   }

   /**
    * Expander template containing a SequenceTemplate directly followed by 1 or more ellipsis,
    * e.g., [seqTemplate] ... ...
    * The expanded output is a proper list containing 0 or more expansions of seqTemplate.
    * seqTemplate must contain at least one variable nested at any level.
    */
   protected static class ListEllipsisTemplate extends EllipsisTemplate
   {
      // associations

      /**
       * The AlignmentCheck object used to verify alignment among multi-valued
       * variables. May be null in cases where alignment is not required.
       */
      protected final AlignmentVerifier m_align;

      // constructors

      /**
       * Creates a new ListEllipsisTemplate.
       * @param template The embedded sequence template.
       * @param nDirectEllipses The number of template ellipses directly following the sequence.
       */
      public ListEllipsisTemplate(SequenceTemplate template, int nDirectEllipses)
      {
         super(template, nDirectEllipses, template.getVariableCode());

         // Only need alignment check if the sequence is has at least 1 multi-valued
         // variable that does not loop, and has at least 2 multi-valued variables in total.
         m_align = (((m_nVarCode & MULTI_VALUED_VARIABLE) != 0) && template.getMultiValuedVariableCount() > 1) ?
            new AlignmentVerifier() : null;
      }

      // operations

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.GenericTemplate#skipSeparators(int, nexj.core.scripting.syntax.SyntaxTransformerContext)
       */
      public boolean skipSeparators(int nCount, SyntaxTransformerContext transformerContext)
      {
         return ((Template)m_templateExpr).skipSeparators(nCount, transformerContext);
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxExpander.GenericTemplate#addExpansion(java.lang.Object, nexj.core.scripting.syntax.SyntaxTransformerContext)
       */
      public final Object addExpansion(Object tail, SyntaxTransformerContext transformerContext)
      {
         SequenceTemplate seqTemplate = (SequenceTemplate)m_templateExpr;

         if (m_nVarCode == SINGLE_VALUED_VARIABLE)
         {
            return addEllipses(seqTemplate.expand(transformerContext), m_nDirectEllipses, tail);
         }

         if (m_align != null)
         {
            transformerContext.pushAlignmentCheck(m_align);
         }

         ExpansionBuilder expansion = new ExpansionBuilder();

         transformerContext.trackWrapCount();

         for (;;)
         {
            Object value = seqTemplate.expand(transformerContext);

            if (value != NULL)
            {
               expansion.append(value);
            }
            // Note: no action is needed if NULL

            if (seqTemplate.skipSeparators(m_nDirectEllipses, transformerContext)
               || (m_align == null
                  && seqTemplate.isUnanchored()
                  && !transformerContext.equalsWrapCount(0))
               )
            {
               break;
            }
         }

         transformerContext.untrackWrapCount();

         if (m_align != null)
         {
            transformerContext.verifyAlignment();
            transformerContext.popAlignmentCheck();
         }

         return expansion.getExpansion(tail);
      }

      /**
       * Adds ellipses following head and preceding tail.
       * @param head The head object.
       * @param nEllipsisCount The number of ellipses to add.
       * @param tail The tail object.
       * @return The new Pair.
       */
      protected static Pair addEllipses(Object head, int nEllipsisCount, Object tail)
      {
         if (tail == NULL)
         {
            return NULL;
         }

         for (int i = 0; i < nEllipsisCount; i++)
         {
            tail = new Pair(Symbol.ELLIPSIS, tail);
         }

         return new Pair(head, tail);
      }
   }

   /**
    * Expansion accumulator.
    */
   protected static class ExpansionBuilder
   {
      // associations

      /**
       * The Pair object whose tail is the first element of the expansion.
       */
      protected final Pair m_head = new Pair(null);

      /**
       * The current tail Pair.
       */
      protected Pair m_tail = m_head;

      // operations

      /**
       * Appends the given value to the expansion.
       * @param tailValue The value to append.
       */
      protected void append(Object tailValue)
      {
         if (SyntaxMatchContext.isNull(tailValue))
         {
            return;
         }

         Pair tail = new Pair(tailValue);

         m_tail.setTail(tail);
         m_tail = tail;
      }

      /**
       * Sets the final tail and returns the accumulated expansion.
       * @param tail The final tail.
       * @return The accumulated expansion.
       */
      protected Object getExpansion(Object tail)
      {
         if (tail != NULL && m_head != m_tail)
         {
            m_tail.setTail(tail);
            tail = m_head.getTail();
         }

         return tail;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         Object expansion = m_head.getTail();

         return (expansion == null) ? "()" : expansion.toString();
      }
   }

   /**
    * Monitors and verifies alignment among multi-valued variables.
    */
   protected static class AlignmentVerifier
   {
      // associations

      /**
       * The last multi-valued, nonlooping variable monitored by this AlignmentCheck.
       */
      protected VariableEllipsisTemplate m_lastNonLoopingVar;

      /**
       * The last looping variable monitored by this AlignmentCheck.
       */
      protected VariableEllipsisTemplate m_lastLoopingVar;

      // operations

      /**
       * Checks for misalignment among ellipsis variables.
       * @param template The ellipsis variable being verified.
       * @param transformerContext The transformer context.
       */
      protected void verifyAlignment(VariableEllipsisTemplate template, SyntaxTransformerContext transformerContext)
      {
         if (template.getVariableCode() == MULTI_VALUED_VARIABLE)
         {
            if (m_lastNonLoopingVar == null)
            {
               m_lastNonLoopingVar = template;
            }
            else if (m_lastNonLoopingVar != template
               && m_lastNonLoopingVar.hasNext(transformerContext) != template.hasNext(transformerContext))
            {
               throw new ScriptingException("err.scripting.syntax.incompatibleEllipsisExpansion", new Object[]{template, m_lastNonLoopingVar});
            }
         }
         else
         {
            assert template.getVariableCode() == LOOPING_MULTI_VALUED_VARIABLE;

            if (m_lastLoopingVar == null)
            {
               m_lastLoopingVar = template;
            }
            else if (m_lastLoopingVar != template
               && m_lastLoopingVar.getWrapCount(transformerContext) != template.getWrapCount(transformerContext))
            {
               throw new ScriptingException("err.scripting.syntax.incompatibleEllipsisExpansion", new Object[]{template, m_lastLoopingVar});
            }
         }
      }

      /**
       * Checks for misalignment between looping variables and non-looping variables.
       * @param transformerContext The transformer context.
       */
      protected void verifyAlignment(SyntaxTransformerContext transformerContext)
      {
         if (m_lastNonLoopingVar != null && m_lastLoopingVar != null
            && !m_lastNonLoopingVar.hasNext(transformerContext) && !m_lastLoopingVar.isReset(transformerContext))
         {
            throw new ScriptingException("err.scripting.syntax.incompatibleEllipsisExpansion", new Object[]{m_lastNonLoopingVar, m_lastLoopingVar});
         }
      }
   }
}
