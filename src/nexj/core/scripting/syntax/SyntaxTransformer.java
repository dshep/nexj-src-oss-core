package nexj.core.scripting.syntax;

import java.io.Serializable;

import nexj.core.meta.Primitive;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.ScriptingException;
import nexj.core.scripting.Symbol;

/**
 * Syntax transformer function implementation.
 */
public class SyntaxTransformer implements SyntaxFunction, Serializable
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = -1972985081122682732L;

   // attributes

   /**
    * Flag indicating whether this is a variable transformer.
    */
   protected final boolean m_bVariable;

   // associations

   /**
    * The symbol representing the transformer function.
    */
   protected Symbol m_symbol;

   /**
    * The embedded function.
    */
   protected final Function m_func;

   // constructors

   /**
    * Creates a new transformer function.
    * @param symbol The symbol associated with this transformer function.
    * @param func The actual function to invoke in case of a transformer call.
    * @param bVariable Flag indicating a variable transformer.
    */
   public SyntaxTransformer(Symbol symbol, Function func, boolean bVariable)
   {
      m_func = func;
      m_symbol = symbol;
      m_bVariable = bVariable;
   }

   /**
    * Creates a new, variable transformer function, which allows the set!-pattern.
    * @param func The actual function to invoke in case of a transformer call.
    */
   public SyntaxTransformer(Function func)
   {
      this(null, func, true);
   }

   // operations

   /**
    * @see nexj.core.scripting.syntax.SyntaxFunction#isVariable()
    */
   public boolean isVariable()
   {
      return m_bVariable;
   }

   /**
    * @see nexj.core.scripting.syntax.SyntaxFunction#getSymbol()
    */
   public Symbol getSymbol()
   {
      return m_symbol;
   }

   /**
    * @see nexj.core.scripting.syntax.SyntaxFunction#setSymbol(nexj.core.scripting.Symbol)
    */
   public void setSymbol(Symbol symbol)
   {
      assert m_symbol == null;

      m_symbol = symbol;
   }

   /**
    * @see nexj.core.scripting.syntax.SyntaxFunction#isContained(java.lang.Object)
    */
   public boolean isContained(Object frame)
   {
      return false;
   }

   /**
    * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
    */
   public final boolean invoke(int nArgCount, Machine machine)
   {
      if (nArgCount != 1)
      {
         throw new ScriptingException("err.scripting.syntax.transformerArgCount",
            new Object[]{m_symbol, Primitive.ONE_INTEGER, Primitive.createInteger(nArgCount)});
      }

      machine.apply(m_func, nArgCount);

      return true;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "transformer:" + m_symbol.toString();
   }
}
