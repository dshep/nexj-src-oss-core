package nexj.core.scripting.syntax;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.NoSuchElementException;

import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.scripting.syntax.SyntaxExpander.AlignmentVerifier;
import nexj.core.scripting.syntax.SyntaxExpander.VariableEllipsisTemplate;
import nexj.core.scripting.syntax.SyntaxMatchContext.MultiValueMatch;
import nexj.core.scripting.syntax.SyntaxMatchContext.MultiValueMatch.MultiValueMatchIterator;
import nexj.core.scripting.syntax.SyntaxMatcher.SyntaxVariable;
import nexj.core.util.HashTab;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Lookup;

/**
 * The transformer expansion context. Manages identifier resolution,
 * global symbol substitution, pattern variable value expansion,
 * ellipsis expansion alignment verification and wrap count tracking. 
 */
public class SyntaxTransformerContext
{
   // attributes

   /**
    * The number of tracked wrap-arounds.
    */
   protected int m_nWrapCount = -1;

   /**
    * The AlignmentCheck stack top offset.
    */
   protected int m_nAlignTop = -1;

   /**
    * The number of transformers being expanded and compiled.
    * Zero if no transformer expansion and compilation is in progress.
    */
   protected int m_nExpCount;

   // associations

   /**
    * The machine to use for transformer expansion.
    */
   protected final Machine m_machine;

   /**
    * The global variable rename map.
    * Used when expanding syntax transformers that introduce new global variables.
    */
   protected final Lookup m_globalVarRenameMap; // of type Symbol[Symbol]

   /**
    * Mappings from symbol names to the corresponding linked list of identifiers.
    * Used for local variable resolution.
    */
   protected final Lookup m_localVarResolutionMap; // of type Node[String]

   /**
    * The AlignmentCheck stack.
    */
   protected AlignmentVerifier[] m_alignStack;

   /**
    * Array of iterators for template variables with ellipses.
    * Valid per call to {@link SyntaxTransformerContext#match(SyntaxMatcher, Object)}.
    */
   protected MultiValueMatchIterator[] m_iterArray;

   /**
    * The most recent transformer being expanded and compiled.
    * Null if no transformer expansion and compilation is in progress.
    */
   protected SyntaxFunction m_transformer;

   /**
    * The last match context of the current transformer expansion.
    * Null if no transformer expansion and compilation is in progress.
    */
   protected SyntaxMatchContext m_matchContext;

   // constructors

   /**
    * Creates a new syntax transformer expansion context.
    * @param machine The machine to use for transformer expansion.
    */
   public SyntaxTransformerContext(Machine machine)
   {
      m_machine = machine;
      m_globalVarRenameMap = new IdentityHashTab(4);
      m_localVarResolutionMap = new HashTab();
      m_alignStack = new AlignmentVerifier[16];
      m_iterArray = new MultiValueMatchIterator[16];
   }

   // operations

   /**
    * @return The number of transformer expansion and compilation in progress.
    */
   public int getExpansionCount()
   {
      return m_nExpCount;
   }

   /**
    * @return The most recent transformer being expanded and compiled. May be null;
    */
   public SyntaxFunction getTransformer()
   {
      return m_transformer;
   }

   /**
    * Initializes the transformer context for expanding and compiling a transformer call.
    */
   public void initCompilation()
   {
      m_nExpCount++;
   }

   /**
    * Updates the transformer expansion count after compiling a transformer expansion.
    * Also resets the expansion context after compiling a top-level transformer expansion.
    */
   public void finishCompilation()
   {
      if (--m_nExpCount == 0)
      {
         m_transformer = null;
         m_globalVarRenameMap.clear();
         m_localVarResolutionMap.clear();
      }
   }

   /**
    * Expands the transformer and returns the expansion.
    * @param transformer The transformer to expand.
    * @param expr The expression to match.
    * @return The expansion, as a Scheme expression.
    */
   public Object expandTransformer(SyntaxFunction transformer, Object expr)
   {
      SyntaxMatchContext matchContextSaved = m_matchContext;

      m_transformer = transformer;

      try
      {
         return m_machine.invoke(transformer, expr, (Pair)null);
      }
      finally
      {
         m_matchContext = matchContextSaved;
      }
   }

   /**
    * Matches the given expression against the given syntax matcher.
    * @param matcher The syntax matcher object.
    * @param expr The expression to match.
    * @return Whether the expression matches the pattern of the syntax matcher.
    */
   public boolean match(SyntaxMatcher matcher, Object expr)
   {
      SyntaxMatchContext matchContext = new SyntaxMatchContext(this, matcher);
      boolean bMatch = matcher.match(expr, matchContext);

      if (bMatch)
      {
         matchContext.setParent(m_matchContext);
         m_matchContext = matchContext;

         // Resets variables to be used during expansion
         m_nWrapCount = -1;
         m_nAlignTop = -1;
         Arrays.fill(m_iterArray, null);
      }

      return bMatch;
   }

   // operations: global symbol substitution

   /**
    * Returns the symbol corresponding to the given symbol that can be used for global
    * definition without affecting hygiene. A new symbol is generated if necessary;
    * otherwise, the argument itself is returned.
    * @param symbol The original symbol.
    * @return The symbol that can be used for global definition.
    */
   public Symbol generateGlobalVarName(Symbol symbol)
   {
      if (m_transformer != null && !isIdentifier(symbol))
      {
         Symbol renamedSym = Symbol.generateSymbol(symbol, m_transformer.hashCode());

         m_globalVarRenameMap.put(symbol, renamedSym);

         return renamedSym;
      }

      return symbol;
   }

   /**
    * Returns the symbol corresponding to the given symbol that can be used at the
    * global scope without affecting hygiene, if such a symbol was previously generated.
    * Otherwise, returns the argument itself.
    * @param symbol The original symbol.
    * @return The symbol that can be used for global definition.
    */
   public Symbol getGlobalVarName(Symbol symbol)
   {
      if (m_transformer != null)
      {
         Object renamedSymbol = m_globalVarRenameMap.get(symbol);

         if (renamedSymbol != null)
         {
            return (Symbol)renamedSymbol;
         }
      }

      return symbol;
   }

   // operations: identifier resolution

   /**
    * Returns the identifier that represents the given symbol and the current transformer
    * being expanded. Generates a new identifier when necessary, if one doesn't already exist.
    * @param symbol The symbol.
    * @return An identifier representing the symbol argument, or the argument itself if
    * an identifier is not needed.
    */
   protected Symbol getIdentifier(Symbol symbol)
   {
      if (m_nExpCount == 0)
      {
         return symbol;
      }

      String sName = symbol.getName();
      Node chain = (Node)m_localVarResolutionMap.get(sName);

      if (chain == null)
      {
         chain = new RootNode(symbol, null, m_transformer, m_nExpCount);
      }
      else
      {
         Node node = chain.findNode(m_transformer, m_nExpCount);

         while (node != null)
         {
            if (symbol == node.getOriginal() || symbol == node.identifier)
            {
               return node.identifier;
            }

            node = node.findNext(m_transformer, m_nExpCount);
         }

         node = chain.findNode(symbol);

         if (node == null)
         {
            chain = new LinkedRootNode(symbol, null, m_transformer, chain, m_nExpCount);
         }
         else
         {
            chain = new LinkedNode(node.identifier, node, m_transformer, chain, m_nExpCount);
         }
      }

      m_localVarResolutionMap.put(chain.identifier.getName(), chain);

      return chain.identifier;
   }

   /**
    * Clones the expansion history of the given identifier onto the symbols found in
    * the given expression.
    * @param identifier The identifier.
    * @param expr The Scheme expression.
    * @return The new Scheme expression, identical to the given expression except for
    * the symbols being replaced by like-named identifiers.
    */
   public Object cloneExpansionHistory(Symbol identifier, Object expr)
   {
      Node idNode = (Node)m_localVarResolutionMap.get(identifier.getName());

      if (idNode == null || (idNode = idNode.findNode(identifier)) == null)
      {
         return expr;
      }

      return cloneExpansionHistory(idNode, expr);
   }

   /**
    * Clones the expansion history of the given identifier node onto the symbols found in
    * the given expression.
    * @param idNode The identifier node.
    * @param expr The Scheme expression.
    * @return The new Scheme expression, identical to the given expression except for
    * the symbols being replaced by like-named identifiers.
    */
   protected Object cloneExpansionHistory(Node idNode, Object expr)
   {
      if (expr instanceof Symbol)
      {
         return cloneExpansionHistory(idNode, (Symbol)expr);
      }

      if (expr instanceof Pair)
      {
         Pair pair = (Pair)expr;

         return new Pair(cloneExpansionHistory(idNode, pair.getHead()), cloneExpansionHistory(idNode, pair.getTail()));
      }

      if (expr != null && expr.getClass().isArray())
      {
         int nLength = Array.getLength(expr);
         Object[] array = new Object[nLength];

         for (int i = 0; i < nLength; i++)
         {
            array[i] = cloneExpansionHistory(idNode, Array.get(expr, i));
         }

         return array;
      }

      return expr;
   }

   /**
    * Returns the identifier corresponding to symbol with the given identifier node's
    * expansion history. If such an identifier does not exist, create it by cloning
    * the given node's expansion history.
    * @param idNode The identifier node to be cloned.
    * @param symbol The symbol value of the new cloned identifier.
    * @return The cloned identifier.
    */
   protected Symbol cloneExpansionHistory(Node idNode, final Symbol symbol)
   {
      Node chain = (Node)m_localVarResolutionMap.get(symbol.getName());

      if (chain == null)
      {
         chain = new RootNode(symbol, idNode.parent, idNode.transformer, m_nExpCount);
      }
      else
      {
         Node symNode = chain.findNode(idNode.transformer, idNode.expLevel);

         while (symNode != null)
         {
            if (Node.compareNodes(idNode, symNode))
            {
               return symNode.identifier;
            }

            symNode = symNode.findNext(idNode.transformer, idNode.expLevel);
         }

         chain = new LinkedRootNode(symbol, idNode.parent, idNode.transformer, chain, m_nExpCount);
      }

      m_localVarResolutionMap.put(chain.identifier.getName(), chain);

      return chain.identifier;
   }

   /**
    * Removes the expansion history of identifiers found in the given expression.
    * @param expr The Scheme expression.
    * @return The new Scheme expression, identical to the given expression except for
    * the identifiers being replaced by like-named original symbols.
    */
   public Object removeExpansionHistory(Object expr)
   {
      if (expr instanceof Symbol)
      {
         Symbol symbol = (Symbol)expr;
         Node symNode = (Node)m_localVarResolutionMap.get(symbol.getName());

         if (symNode != null && (symNode = symNode.findNode(symbol)) != null)
         {
            return symNode.getRoot();
         }
      }
      else if (expr instanceof Pair)
      {
         Pair pair = (Pair)expr;

         return new Pair(removeExpansionHistory(pair.getHead()), removeExpansionHistory(pair.getTail()));
      }
      else if (expr != null && expr.getClass().isArray())
      {
         int nLength = Array.getLength(expr);
         Object[] array = new Object[nLength];

         for (int i = 0; i < nLength; i++)
         {
            array[i] = removeExpansionHistory(Array.get(expr, i));
         }

         return array;
      }

      return expr;
   }

   /**
    * Returns true if the given symbol is an identifier.
    * @param symbol The symbol to check.
    * @return True if the given symbol is an identifier.
    */
   public boolean isIdentifier(Symbol symbol)
   {
      if (m_nExpCount == 0)
      {
         return true;
      }

      Node chain = (Node)m_localVarResolutionMap.get(symbol.getName());

      return chain != null && chain.isIdentifier(symbol) ;
   }

   /**
    * Compares the given symbols and returns true if they are identical identifiers.
    * Two identifiers are considered identical if they are the same object or if
    * they have the same symbol name and the same expansion history.
    * @param left The first symbol.
    * @param right The second symbol.
    * @return True if the given symbols are identical identifiers.
    */
   public boolean compareSymbols(Symbol left, Symbol right)
   {
      assert left != null;
      assert right != null;

      if (left == right)
      {
         return true;
      }

      if (left.equals(right))
      {
         Node chain = (Node)m_localVarResolutionMap.get(left.getName());

         return chain == null || chain.compareNodes(left, right);
      }

      return false;
   }

   /**
    * Returns a positive number if the given identifier resolves to the given symbol,
    * 0 if the identifier is identical to the symbol, and a negative number otherwise.
    * An identifier resolves to a symbol if they have the same symbol name and
    * the identifier has a parent symbol that is identical to the given symbol.
    * @param identifier The identifier to resolve.
    * @param symbol The symbol to test. 
    * @return A positive number if the given identifier resolves to the given symbol,
    * 0 if identical, and a negative number otherwise.
    */
   public int resolveIdentifier(Symbol identifier, Symbol symbol)
   {
      assert identifier != null;
      assert symbol != null;

      if (identifier == symbol)
      {
         return 0;
      }

      if (identifier.equals(symbol))
      {
         Node chain = (Node)m_localVarResolutionMap.get(identifier.getName());

         if (chain != null)
         {
            return chain.resolveIdentifier(identifier, symbol);
         }
      }

      return -1;
   }

   // operations: pattern variable value expansion

   /**
    * Returns the value iterator associated with the given variable.
    * @param var The variable.
    * @param nTemplateId The template identifier. Unique per SyntaxMatcher instance.
    * @return The value iterator associated with the given variable.
    */
   protected MultiValueMatchIterator getMatchIterator(SyntaxVariable var, int nTemplateId)
   {
      if (nTemplateId < m_iterArray.length)
      {
         MultiValueMatchIterator iter = m_iterArray[nTemplateId];

         if (iter != null)
         {
            return iter;
         }
      }
      else
      {
         MultiValueMatchIterator[] array = new MultiValueMatchIterator[nTemplateId << 1];

         System.arraycopy(m_iterArray, 0, array, 0, m_iterArray.length);
         m_iterArray = array;
      }

      Object value = getValue(var);

      assert value instanceof MultiValueMatch;

      return m_iterArray[nTemplateId] = ((MultiValueMatch)value).iterator();
   }

   /**
    * Returns the value associated with the given variable.
    * @param var The variable.
    * @return The value associated with the given variable.
    */
   protected Object getValue(SyntaxVariable var)
   {
      for (SyntaxMatchContext context = m_matchContext; context != null; context = context.getParent())
      {
         if (context.equalsMatcher(var.matcher))
         {
            return context.getValue(var.varId);
         }
      }

      throw new NoSuchElementException();
   }

   // operations: wrap count tracking

   /**
    * Enables wrap count tracking.
    */
   protected void trackWrapCount()
   {
      m_nWrapCount = 0;
   }

   /**
    * Disables wrap count tracking.
    */
   protected void untrackWrapCount()
   {
      m_nWrapCount = -1;
   }

   /**
    * Verifies the current wrap count.
    * @param nCount The expected wrap count.
    * @return True if the actual wrap count is different from the expected value.
    */
   protected boolean equalsWrapCount(int nCount)
   {
      return m_nWrapCount == nCount;
   }

   // operations: alignment verification

   /**
    * Pushes the given alignment check.
    * @param align The {@link AlignmentVerifier} object.
    */
   protected void pushAlignmentCheck(AlignmentVerifier align)
   {
      if (m_alignStack.length == ++m_nAlignTop)
      {
         AlignmentVerifier[] stack = new AlignmentVerifier[m_nAlignTop << 1];

         System.arraycopy(m_alignStack, 0, stack, 0, m_nAlignTop);

         m_alignStack = stack;
      }

      m_alignStack[m_nAlignTop] = align;
   }

   /**
    * Checks for misalignment among the given ellipsis variable and other ellipsis variables
    * of the same kind (looping or nonlooping) after a wrap-around.
    * @param template The ellipsis variable being verified.
    */
   protected void verifyWrapAlignment(VariableEllipsisTemplate template)
   {
      if (m_nWrapCount >= 0)
      {
         m_nWrapCount++;
      }

      verifyAlignment(template);
   }

   /**
    * Checks for misalignment among the given ellipsis variable and other ellipsis variables
    * of the same kind (looping or nonlooping).
    * @param template The ellipsis variable being verified.
    */
   protected void verifyAlignment(VariableEllipsisTemplate template)
   {
      for (int i = m_nAlignTop; i >= 0; i--)
      {
         m_alignStack[i].verifyAlignment(template, this);
      }
   }

   /**
    * Checks for misalignment between looping and nonlooping ellipsis variables.
    */
   protected void verifyAlignment()
   {
      for (int i = m_nAlignTop; i >= 0; i--)
      {
         m_alignStack[i].verifyAlignment(this);
      }
   }

   /**
    * Pops the top alignment check.
    */
   protected void popAlignmentCheck()
   {
      m_nAlignTop--;
   }

   // inner classes

   /**
    * An node in the linked list.
    */
   protected static abstract class Node
   {
      // attributes

      /**
       * The transformer expansion level at which this identifier was created.
       * This and the transformer function uniquely identify the expansion source.
       */
      public final int expLevel;

      // associations

      /**
       * The identifier that this node represents. Unique to this node, that is,
       * no other node in the linked list has this as its identifier.
       */
      public final Symbol identifier;

      /**
       * The transformer during the expansion of which this identifier was created.
       * This and the transformer expansion level uniquely identify the expansion source.
       */
      public final SyntaxFunction transformer;

      /**
       * The parent node.
       */
      public final Node parent;

      // constructors

      /**
       * Creates a new linked list node.
       * @param original The original symbol that this node's identifier replaces.
       * @param parent The parent node.
       * @param transformer The transformer being expanded.
       * @param nExpLevel The transformer expansion level at which this identifier was created.
       */
      protected Node(Symbol original, Node parent, SyntaxFunction transformer, int nExpLevel)
      {
         this.transformer = transformer;
         this.parent = parent;
         identifier = Symbol.cloneSymbol(original);
         expLevel = nExpLevel;
      }

      // operations

      /**
       * @return The original symbol that this this node's identifier replaces.
       * Never null and has the same name as the node's identifier.
       */
      protected abstract Symbol getOriginal();

      /**
       * @return The root symbol of this resolution chain.
       * Never null and has the same name as the node's identifier.
       */
      protected abstract Symbol getRoot();

      /**
       * Returns true if the given symbol exists as an identifier in this linked list.
       * @param sym The symbol to look for.
       * @return True if the symbol is an identifier in this linked list.
       */
      protected boolean isIdentifier(Symbol sym)
      {
         return identifier == sym;
      }

      /**
       * Compares the nodes corresponding to the given identifiers and returns true if the nodes
       * are identical objects or have a common parent node at the same expansion level at the
       * same distance from the nodes in the linked node hierarchy.
       * @param left The first symbol.
       * @param right The second symbol.
       * @return True if the given identifiers are equivalent in the node hierarchy.
       */
      protected final boolean compareNodes(Symbol left, Symbol right)
      {
         return compareNodes(findNode(left), findNode(right));
      }

      /**
       * Compares the given nodes and returns true if they are identical or have a common parent
       * node at the same expansion level at the same distance from the nodes in the linked node
       * hierarchy. The nodes do not necessarily have the same identifier symbol name.
       * @param left The first node.
       * @param right The second node.
       * @return True if the given nodes are equivalent in the node hierarchy.
       */
      protected static boolean compareNodes(Node left, Node right)
      {
         while (left != right)
         {
            if (left == null || right == null || left.expLevel != right.expLevel)
            {
               return false;
            }

            left = left.parent;
            right = right.parent;
         }

         return true;
      }

      /**
       * Returns a positive number if the given identifier resolves to the given symbol,
       * 0 if the identifier is identical to the symbol, and a negative number otherwise.
       * An identifier resolves to a symbol if it has a greater expansion level and
       * a parent node that is equivalent to the symbol node in the node hierarchy.
       * @param id The identifier to resolve.
       * @param symbol The symbol to test. 
       * @return A positive number if the given identifier resolves to the given symbol,
       * 0 if identical, and a negative number otherwise.
       */
      protected final int resolveIdentifier(Symbol id, Symbol symbol)
      {
         Node idNode = findNode(id);
         Node symNode = findNode(symbol);

         if (symNode == null) // symbol is not an identifier
         {
            if (idNode == null)
            {
               return 0;
            }

            if (idNode.getRoot() == symbol)
            {
               return 1;
            }
         }
         else if (idNode != null) // both symbol and id are identifiers
         {
            int nVal = idNode.expLevel - symNode.expLevel;

            if (nVal >= 0)
            {
               while (idNode.expLevel > symNode.expLevel)
               {
                  if ((idNode = idNode.parent) == null)
                  {
                     return -1;
                  }
               }

               if (compareNodes(idNode, symNode))
               {
                  return nVal;
               }
            }
         }

         return -1;
      }

      /**
       * Returns the node with the given identifier in this linked list.
       * @param id The identifier to look for (compared by identity).
       * @return The node of the given identifier.
       */
      protected Node findNode(Symbol id)
      {
         return (identifier == id) ? this : null;
      }

      /**
       * Returns the node in this linked list that came from the given transformer
       * at the given expansion level.
       * @param syntaxTransformer The transformer to look for (compared by identity).
       * @param nExpLevel The transformer expansion level to look for.
       * @return The node with the given transformer.
       */
      protected Node findNode(SyntaxFunction syntaxTransformer, int nExpLevel)
      {
         return (transformer == syntaxTransformer && expLevel == nExpLevel) ? this : null;
      }

      /**
       * Returns the next node (not including this node) in this linked list that
       * came from the given transformer at the given expansion level.
       * @param syntaxTransformer The transformer to look for (compared by identity).
       * @param nExpLevel The transformer expansion level to look for.
       * @return The node with the given transformer.
       */
      protected Node findNext(SyntaxFunction syntaxTransformer, int nExpLevel)
      {
         return null;
      }
   }

   /**
    * The first root node in the linked list. The node is the root of a resolution chain,
    * and does not have a next node pointer. The parent node indicates the node's expansion
    * history, and does not have the same name as the node's identifier.
    */
   protected static class RootNode extends Node
   {
      /**
       * The root symbol that this node's identifier replaces.
       */
      protected final Symbol m_root;

      /**
       * Creates the first root node in the node linked list.
       * @param root The root symbol of the resolution chain.
       * @param parent The parent node. May be null. The parent node's identifier does not
       * necessarily have the same symbol name as the given identifier.
       * @param transformer The transformer being expanded.
       * @param nExpLevel The transformer expansion level at which this identifier was created.
       */
      protected RootNode(Symbol root, Node parent, SyntaxFunction transformer, int nExpLevel)
      {
         super(root, parent, transformer, nExpLevel);
         m_root = root;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxTransformerContext.Node#getOriginal()
       */
      protected Symbol getOriginal()
      {
         return m_root;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxTransformerContext.Node#getRoot()
       */
      protected Symbol getRoot()
      {
         return m_root;
      }
   }

   /**
    * A linked node in the linked list. A part of a resolution chain, with a
    * parent resolution node of a like-named identifier, and a next node pointer.
    */
   protected static class LinkedNode extends Node
   {
      /**
       * The next node in the linked list. May not be null.
       */
      protected final Node m_next;

      /**
       * Creates a linked node in the node linked list, whose identifier has the same symbol
       * value as its parent node.
       * @param original The original symbol that this node's identifier replaces.
       * @param parent The parent node. May not be null.
       * @param transformer The transformer being expanded.
       * @param next The next node in the linked list. May not be null. The node's identifier
       * must have the same symbol name as the original symbol.
       * @param nExpLevel The transformer expansion level at which this identifier was created.
       */
      protected LinkedNode(Symbol original, Node parent, SyntaxFunction transformer, Node next, int nExpLevel)
      {
         super(original, parent, transformer, nExpLevel);

         assert next != null && next.identifier.equals(original);
         m_next = next;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxTransformerContext.Node#getOriginal()
       */
      protected Symbol getOriginal()
      {
         return parent.identifier;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxTransformerContext.Node#getRoot()
       */
      protected Symbol getRoot()
      {
         return parent.getRoot();
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxTransformerContext.Node#isIdentifier(nexj.core.scripting.Symbol)
       */
      protected boolean isIdentifier(Symbol sym)
      {
         return super.isIdentifier(sym) || m_next.isIdentifier(sym);
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxTransformerContext.Node#findNode(nexj.core.scripting.Symbol)
       */
      protected Node findNode(Symbol id)
      {
         return (identifier == id) ? this : m_next.findNode(id);
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxTransformerContext.Node#findNode(nexj.core.scripting.syntax.SyntaxFunction, int)
       */
      protected Node findNode(SyntaxFunction syntaxTransformer, int nExpLevel)
      {
         return super.findNode(syntaxTransformer, nExpLevel) != null ? this : m_next.findNode(syntaxTransformer, nExpLevel);
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxTransformerContext.Node#findNext(nexj.core.scripting.syntax.SyntaxFunction, int)
       */
      protected Node findNext(SyntaxFunction syntaxTransformer, int nExpLevel)
      {
         return m_next.findNode(syntaxTransformer, nExpLevel);
      }
   }

   /**
    * A linked root node in the linked list. The node is the root of a resolution chain,
    * and has a next node pointer. The parent node indicates the node's expansion history,
    * and does not have the same name as the node's identifier.
    */
   protected static class LinkedRootNode extends LinkedNode
   {
      // associations

      /**
       * The root symbol that this node's identifier replaces.
       */
      protected final Symbol m_root;

      // constructors

      /**
       * Creates a linked root node in the node linked list.
       * @param root The root symbol of the resolution chain.
       * @param parent The parent node. May be null. The parent node's identifier does not
       * necessarily have the same symbol name as the given identifier.
       * @param transformer The transformer being expanded.
       * @param next The next node in the linked list. May not be null. The node's identifier
       * must have the same symbol name as the original symbol.
       * @param nExpLevel The transformer expansion level at which this identifier was created.
       */
      protected LinkedRootNode(Symbol root, Node parent, SyntaxFunction transformer, Node next, int nExpLevel)
      {
         super(root, parent, transformer, next, nExpLevel);
         m_root = root;
      }

      // operations

      /**
       * @see nexj.core.scripting.syntax.SyntaxTransformerContext.LinkedNode#getOriginal()
       */
      protected Symbol getOriginal()
      {
         return m_root;
      }

      /**
       * @see nexj.core.scripting.syntax.SyntaxTransformerContext.LinkedNode#getRoot()
       */
      protected Symbol getRoot()
      {
         return m_root;
      }
   }
}
