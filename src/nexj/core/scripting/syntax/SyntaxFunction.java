package nexj.core.scripting.syntax;

import nexj.core.scripting.Function;
import nexj.core.scripting.Symbol;

/**
 * Syntax transformer function interface.
 */
public interface SyntaxFunction extends Function
{
   /**
    * @return True if this is a variable transformer function, which allows a set!-pattern.
    */
   boolean isVariable();

   /**
    * @return The symbol that represents this transformer function.
    */
   Symbol getSymbol();

   /**
    * Sets the symbol that represents this transformer function.
    * @param symbol The symbol.
    */
   void setSymbol(Symbol symbol);

   /**
    * Returns true if the frame containing the syntax function definition
    * is contained in the given frame.
    * @param frame The frame to test.
    * @return True if the frame containing the syntax function definition
    * is contained in the given frame.
    */
   boolean isContained(Object frame);
}
