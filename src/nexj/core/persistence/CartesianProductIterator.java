package nexj.core.persistence;

import java.util.Iterator;

/**
 * Cartesian product iterator interface.
 */
public interface CartesianProductIterator extends Iterator
{
   /**
    * Adds a child iterator to the product.
    * @param child The iterator to add.
    */
   public void add(CartesianProductIterator child);

   /**
    * Resets this iterator to the beginning of its iteration.
    */
   public void reset();
}
