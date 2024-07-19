// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.io.IOException;
import java.io.Serializable;
import java.util.Iterator;
import java.util.NoSuchElementException;

import nexj.core.util.ObjUtil;
import nexj.core.util.PrintWriter;
import nexj.core.util.Printable;

/**
 * The Lisp Pair (cons).
 * Also an expression node. The internal representation of the expression is the
 * same as in Lisp, i.e. pairs with symbols for operators. This class
 * provides convenience expression building API for use in Java code.
 * 
 * Sample Usage
 * 
 * Java-like syntax:
 * (address.city like "A*") and (birthDate >= #1-Jan-1980#)
 * 
 * Scheme syntax:
 * (and (like? (@ address city) "A*")
 *      (>= birthDate (cast timestamp "1980-01-01 00:00:00")))
 * 
 * Internal representation:
 * (and . ((like? . ((@ . (address . (city . '())) . ("A*" . '())))) . ((>=
 * . (birthdate . ((cast . (timestamp . ("1980-01-01 00:00:00" . '())))) . '())))
 * 
 * Java API for Scheme expressions:
 * Pair.attribute("city").from("address").like("A*")
 * .and(Pair.attribute("birthDate").ge(Primitive.toTimestamp("1980-01-01 00:00:00"));
 */
public class Pair implements Cloneable, Serializable, Printable
{
   // constants
   
   /**
    * The Java stream unique identifier for the class.
    */
   private final static long serialVersionUID = 4905439272318294107L;

   /**
    * Empty object array.
    */
   private final static Object[] EMPTY_ARRAY = new Object[0];
   
   // attributes

   /**
    * The first element of the pair.
    */
   protected Object m_head;

   /**
    * The second element of the pair.
    */
   protected Object m_tail;

   // constructors
   
   /**
    * Create a single-element list (head).
    * @param head The head object. 
    */
   public Pair(Object head)
   {
      m_head = head;
      m_tail = null;
   }
   
   /**
    * Create a pair (head . tail).
    * @param head The head object.
    * @param tail The tail object.
    */
   public Pair(Object head, Object tail)
   {
      m_head = head;
      m_tail = tail;
   }
   
   // operations
   
   /**
    * Sets the pair head.
    * @param head The object to set as the head.
    */
   public void setHead(Object head)
   {
      m_head = head;
   }

   /**
    * @return The pair head.
    */
   public final Object getHead()
   {
      return m_head;
   }

   /**
    * Sets the pair tail.
    * @param tail The object to set as a tail.
    */
   public void setTail(Object tail)
   {
      m_tail = tail;
   }

   /**
    * @return The pair tail.
    */
   public final Object getTail()
   {
      return m_tail;
   }
   
   /**
    * @return The next pair.
    * @throws ClassCastException if the tail is not null and not a pair. 
    */
   public final Pair getNext()
   {
      return (Pair)m_tail;
   }
   
   /**
    * Computes the length of a list.
    * @param The first pair of the linked list.
    * @return The list length.
    */
   public static int length(Pair pair)
   {
      int n = 0;

      while (pair != null)
      {
         ++n;
         pair = pair.getNext();
      }

      return n;
   }
   
   /**
    * Appends one list of pairs to another.
    * @param left The list to copy if necessary and prepend.
    * @param right The list to append, unchanged.
    * @return The resulting list.
    */
   public static Pair append(Pair left, Pair right)
   {
      if (left == null)
      {
         return right;
      }
   
      if (right == null)
      {
         return left;
      }
   
      Pair head, tail;
   
      for (head = tail = null; left != null; left = left.getNext())
      {
         Pair pair = new Pair(left.getHead());
   
         if (head == null)
         {
            head = tail = pair;
         }
         else
         {
            tail.setTail(pair);
            tail = pair;
         }
      }
   
      tail.setTail(right);
   
      return head;
   }

   /**
    * Concatenates destructively two lists of pairs. 
    * @param left The list to prepend, modified.
    * @param right The list to append, unchanged.
    * @return The resulting list.
    */
   public static Pair nconc(Pair left, Pair right)
   {
      if (left == null)
      {
         return right;
      }

      if (right == null)
      {
         return left;
      }

      Pair pair;

      for (pair = left; pair.getTail() != null; pair = pair.getNext()) ;

      pair.setTail(right);

      return left;
   }

   /**
    * Destructively reverses a list of pairs.
    * @param pair The list to reverse.
    * @return The reversed list.
    */
   public static Pair nreverse(Pair pair)
   {
      Pair prev = null; 

      while (pair != null)
      {
         Pair next = pair.getNext();

         pair.setTail(prev);
         prev = pair;
         pair = next;
      }

      return prev;
   }

   /**
    * Creates a linked pair list from an iterator.
    * @param itr The iterator providing the values for the linked list.
    * @return The first pair in the linked list.
    */
   public static Pair fromIterator(Iterator itr)
   {
      Pair head;
      Pair tail;
      
      if (itr.hasNext())
      {
         head = tail = new Pair(itr.next());
      }
      else
      {
         return null;
      }
      
      while (itr.hasNext())
      {
         Pair pair = new Pair(itr.next());
         
         tail.m_tail = pair;
         tail = pair;
      }
      
      return head;
   } 
   
   /**
    * Creates a linked pair list from an array.
    * @param array The array from which to create the list.
    * @return The first pair of the linked list.
    */
   public static Pair fromArray(Object[] array)
   {
      return fromArray(array, 0, array.length);
   }

   /**
    * Creates a linked pair list from the given range of an array.
    * @param array The array from which to create the list.
    * @param nOffset The starting index in the array.
    * @param nLength The count of items to use.
    * @return The first pair of the linked list.
    */
   public static Pair fromArray(Object[] array, int nOffset, int nLength)
   {
      Pair pair = null;

      if (nOffset < 0 || nLength < 0 || nOffset + nLength > array.length)
      {
         throw new IndexOutOfBoundsException();
      }

      for (int i = nOffset + nLength - 1; i >= nOffset; --i)
      {
         pair = new Pair(array[i], pair);
      }

      return pair;
   }

   /**
    * Creates an object array from a linked list.
    * @param pair The first pair of the linked list.
    * @return The object array.
    */
   public static Object[] toArray(Pair pair)
   {
      if (pair == null)
      {
         return EMPTY_ARRAY;
      }
      
      Object[] array = new Object[length(pair)];
      int i = 0;
      
      while (pair != null)
      {
         array[i++] = pair.getHead();
         pair = pair.getNext();
      }
      
      return array;
   }

   /**
    * Gets an iterator over the pair.
    * 
    * @param pair The pair to iterate.
    * @return The iterator.
    */
   public static Iterator getIterator(Pair pair)
   {
      return new PairIterator(pair);
   }
   
   /**
    * @return The operator symbol.
    */
   public Symbol getOperator()
   {
      return (Symbol)m_head;
   }

   /**
    * Constructs an association path given the association name.
    * @param sName The association name.
    * Can contain a space-separated association path.
    * @return The new expression node.
    */
   public Pair from(String sName)
   {
      if (!Symbol.AT.equals(m_head))
      {
         throw new IllegalStateException("Attempt to call from() on a non-attribute expression");
      }

      return new Pair(Symbol.AT, attributeList(sName, (Pair)m_tail));
   }

   /**
    * Inserts the first operand.
    * @param expr The expression to prepend.
    * @return The new expression.
    */
   public Pair after(Object expr)
   {
      return new Pair((Symbol)m_head, new Pair(expr, m_tail));
   }

   /**
    * Creates a binary expression.
    * @param op The operator symbol.
    * @param expr The second operand.
    * @return The new expression.
    */
   public Pair binary(Symbol op, Object expr)
   {
      return binary(op, this, expr);
   }

   /**
    * Creates a unary expression.
    * @param op The operand symbol.
    * @return The new expression.
    */
   public Pair unary(Symbol op)
   {
      return unary(op, this);
   }

   /**
    * Creates a disjunction.
    * @param expr The second operand.
    * @return The new expression.
    */
   public Pair or(Object expr)
   {
      return binary(Symbol.OR, expr);
   }

   /**
    * Creates a conjunction.
    * @param expr The second operand.
    * @return The new expression.
    */
   public Pair and(Object expr)
   {
      return binary(Symbol.AND, expr);
   }

   /**
    * Creates an equality comparison.
    * @param expr The second operand.
    * @return The new expression.
    */
   public Pair eq(Object expr)
   {
      return binary(Symbol.EQ, expr);
   }

   /**
    * Creates an inequality comparison.
    * @param expr The second operand.
    * @return The new expression.
    */
   public Pair ne(Object expr)
   {
      return binary(Symbol.NE, expr);
   }

   /**
    * Creates a greater-than comparison.
    * @param expr The second operand.
    * @return The new expression.
    */
   public Pair gt(Object expr)
   {
      return binary(Symbol.GT, expr);
   }

   /**
    * Creates a greater-than-or-equal comparison.
    * @param expr The second operand.
    * @return The new expression.
    */
   public Pair ge(Object expr)
   {
      return binary(Symbol.GE, expr);
   }

   /**
    * Creates a less-than comparison.
    * @param expr The second operand.
    * @return The new expression.
    */
   public Pair lt(Object expr)
   {
      return binary(Symbol.LT, expr);
   }

   /**
    * Creates a less-than-or-equal comparison.
    * @param expr The second operand.
    * @return The new expression.
    */
   public Pair le(Object expr)
   {
      return binary(Symbol.LE, expr);
   }

   /**
    * Creates a like comparison.
    * @param expr The second operand.
    * @return The new expression.
    */
   public Pair like(Object expr)
   {
      return binary(Symbol.LIKE_P, expr);
   }

   /**
    * Creates an in comparison.
    * @param values The in value list.
    * @return The new expression.
    */
   public Pair in(Pair values)
   {
      return new Pair(Symbol.IN_P, new Pair(this, values));
   }

   /**
    * Creates an addition.
    * @param expr The second operand.
    * @return The new expression.
    */
   public Pair plus(Object expr)
   {
      return binary(Symbol.PLUS, expr);
   }

   /**
    * Creates a subtraction.
    * @param expr The second operand.
    * @return The new expression.
    */
   public Pair minus(Object expr)
   {
      return binary(Symbol.MINUS, expr);
   }

   /**
    * Creates a multiplication.
    * @param expr The second operand.
    * @return The new expression.
    */
   public Pair mul(Object expr)
   {
      return binary(Symbol.MUL, expr);
   }

   /**
    * Creates a division.
    * @param expr The second operand.
    * @return The new expression.
    */
   public Pair div(Object expr)
   {
      return binary(Symbol.DIVIDE, expr);
   }

   /**
    * Creates an arithmetical negation.
    * @return The new expression.
    */
   public Pair neg()
   {
      return unary(Symbol.MINUS);
   }

   /**
    * Creates a logical negation.
    * @return The new expression.
    */
   public Pair not()
   {
      return unary(Symbol.NOT);
   }

   /**
    * Creates an any() operation.
    * @return The new expression.
    */
   public Pair any()
   {
      return unary(Symbol.ANY);
   }

   /**
    * Creates an attribute list: a1 a2 ... aN -&gt; (a1 a2 ... aN)
    * @param sName The name of the attribute.
    * Can contain a space-separated association path.
    * @param tail The list tail.
    * @return The attribute list.
    */
   public static Pair attributeList(String sName, Pair tail)
   {
      int i = sName.lastIndexOf(' ');
      int k = sName.length();

      if (i < 0)
      {
         if (k == 0)
         {
            return tail;
         }

         return new Pair(Symbol.define(sName), tail);
      }

      Pair pair = tail;

      for (;;)
      {
         pair = new Pair(Symbol.define(sName.substring(i + 1, k)), pair);
         
         while (i > 0 && sName.charAt(i - 1) == ' ')
         {
            --i;
         }
         
         k = i;
         
         if (k < 0)
         {
            break;
         }
         
         i = sName.lastIndexOf(' ', k - 1);
      }

      return pair;
   }

   /**
    * Creates an attribute query: a1 a2 ... aN -&gt; (a1 (a2 ... aN)).
    * @param sName The name of the attribute or a space-separated association path.
    * @param tail The list tail, appended to the innermost attribute.
    * @return The attribute query.
    */
   public static Pair attributeQuery(String sName, Pair tail)
   {
      int i = sName.lastIndexOf(' ');
      int k = sName.length();
      Pair pair;

      if (i < 0)
      {
         if (k == 0)
         {
            return tail;
         }

         pair = new Pair(Symbol.define(sName), tail);
         
         if (tail != null)
         {
            pair = new Pair(pair);
         }
         
         return pair;
      }

      pair = tail;
      
      for (;;)
      {
         tail = pair;
         pair = new Pair(Symbol.define(sName.substring(i + 1, k)), pair);

         if (tail != null)
         {
            pair = new Pair(pair);
         }

         while (i > 0 && sName.charAt(i - 1) == ' ')
         {
            --i;
         }
         
         k = i;
         
         if (k < 0)
         {
            break;
         }
         
         i = sName.lastIndexOf(' ', k - 1);
      }

      return pair;
   }
   
   /**
    * Creates an attribute expression.
    * @param symbol The attribute symbol.
    * @return The new expression.
    */
   public static Pair attribute(Symbol symbol)
   {
      return Pair.list(Symbol.AT, symbol);
   }

   /**
    * Creates an attribute expression.
    * @param sName The name of the attribute.
    * Can contain a space-separated association path.
    * @return The new expression.
    */
   public static Pair attribute(String sName)
   {
      return new Pair(Symbol.AT, attributeList(sName, null));
   }

   /**
    * Creates a reverse association expression.
    * @param sClassName The starting class name.
    * @param sAssoc The space-separated association path.
    */
   public static Pair attribute(String sClassName, String sAssoc)
   {
      return new Pair(Symbol.ATAT, new Pair(Symbol.define(sClassName), attributeList(sAssoc, null)));
   }

   /**
    * Creates an n-ary commutative expression. Optimized for cases when
    * the operator matches one of the operands.
    * @param op The operator symbol.
    * @param left The left operand. Can be null.
    * @param right The right operand. Can be null.
    * @return The new expression.
    */
   public static Object commutative(Symbol op, Object left, Object right)
   {
      if (left == null)
      {
         return right;
      }
      
      if (right == null)
      {
         return left;
      }
      
      if (left instanceof Pair)
      {
         Pair pair = (Pair)left;
         
         if (pair.getHead() == op)
         {
            return new Pair(op, new Pair (right, pair.getTail()));
         }
      }
      
      if (right instanceof Pair)
      {
         Pair pair = (Pair)right;

         if (pair.getHead() == op)
         {
            return new Pair(op, new Pair (left, pair.getTail()));
         }
      }

      return binary(op, left, right);
   }

   /**
    * Creates a binary expression.
    * @param op The operator symbol.
    * @param left The left operand.
    * @param right The right operand.
    * @return The new expression.
    */
   public static Pair binary(Symbol op, Object left, Object right)
   {
      return new Pair(op, new Pair(left, new Pair(right)));
   }

   /**
    * Creates a unary expression.
    * @param op The operator symbol.
    * @param expr The operand.
    * @return The new expression.
    */
   public static Pair unary(Symbol op, Object expr)
   {
      return new Pair(op, new Pair(expr));
   }
   
   /**
    * Creates a quoted expression: (quote expr)
    * @param expr The expression to quote.
    * @return The quoted expression.
    */
   public static Pair quote(Object expr)
   {
      return unary(Symbol.QUOTE, expr);
   }

   /**
    * Creates a chain of Pairs whose cars are the objNN arguments and whose final cdr is tail.
    * @param obj1 The 1st car.
    * @param obj2 The 2nd car.
    * @param tail The final cdr.
    * @return The created chain of Pairs.
    */
   public static Pair cons(Object obj1, Object obj2, Object tail)
   {
      return new Pair(obj1, new Pair(obj2, tail));
   }

   /**
    * Creates a chain of Pairs whose cars are the objNN arguments and whose final cdr is tail.
    * @param obj1 The 1st car.
    * @param obj2 The 2nd car.
    * @param obj3 The 3rd car.
    * @param obj4 The 4th car.
    * @param tail The final cdr.
    * @return The created chain of Pairs.
    */
   public static Pair cons(Object obj1, Object obj2, Object obj3, Object obj4, Object tail)
   {
      return new Pair(obj1, new Pair(obj2, new Pair(obj3, new Pair(obj4, tail))));
   }

   /**
    * Creates a 1-element list from the argument.
    * @param obj The list element.
    * @return The created list.
    */
   public static Pair list(Object obj)
   {
      return new Pair(obj);
   }
   
   /**
    * Creates a 2-element list from its arguments.
    * @param obj1 The 1st list element.
    * @param obj2 The 2nd list element.
    * @return The created list.
    */
   public static Pair list(Object obj1, Object obj2)
   {
      return new Pair(obj1, new Pair(obj2));
   }
   
   /**
    * Creates a 3-element list from its arguments.
    * @param obj1 The 1st list element.
    * @param obj2 The 2nd list element.
    * @param obj3 The 3rd list element.
    * @return The created list.
    */
   public static Pair list(Object obj1, Object obj2, Object obj3)
   {
      return new Pair(obj1, new Pair(obj2, new Pair(obj3)));
   }

   /**
    * Creates a 4-element list from its arguments.
    * @param obj1 The 1st list element.
    * @param obj2 The 2nd list element.
    * @param obj3 The 3rd list element.
    * @param obj4 The 4th list element.
    * @return The created list.
    */
   public static Pair list(Object obj1, Object obj2, Object obj3, Object obj4)
   {
      return new Pair(obj1, new Pair(obj2, new Pair(obj3, new Pair(obj4))));
   }

   /**
    * Creates a 5-element list from its arguments.
    * @param obj1 The 1st list element.
    * @param obj2 The 2nd list element.
    * @param obj3 The 3rd list element.
    * @param obj4 The 4th list element.
    * @param obj5 The 5th list element.
    * @return The created list.
    */
   public static Pair list(Object obj1, Object obj2, Object obj3, Object obj4, Object obj5)
   {
      return new Pair(obj1, new Pair(obj2, new Pair(obj3, new Pair(obj4, new Pair(obj5)))));
   }

   /**
    * Creates a 6-element list from its arguments.
    * @param obj1 The 1st list element.
    * @param obj2 The 2nd list element.
    * @param obj3 The 3rd list element.
    * @param obj4 The 4th list element.
    * @param obj5 The 5th list element.
    * @param obj6 The 6th list element.
    * @return The created list.
    */
   public static Pair list(Object obj1, Object obj2, Object obj3, Object obj4, Object obj5, Object obj6)
   {
      return new Pair(obj1, new Pair(obj2, new Pair(obj3, new Pair(obj4, new Pair(obj5, new Pair(obj6))))));
   }

   /**
    * Creates a 7-element list from its arguments.
    * @param obj1 The 1st list element.
    * @param obj2 The 2nd list element.
    * @param obj3 The 3rd list element.
    * @param obj4 The 4th list element.
    * @param obj5 The 5th list element.
    * @param obj6 The 6th list element.
    * @param obj7 The 7th list element.
    * @return The created list.
    */
   public static Pair list(Object obj1, Object obj2, Object obj3, Object obj4, Object obj5, Object obj6, Object obj7)
   {
      return new Pair(obj1, new Pair(obj2, new Pair(obj3, new Pair(obj4, new Pair(obj5, new Pair(obj6, new Pair(obj7)))))));
   }

   /**
    * Creates an 8-element list from its arguments.
    * @param obj1 The 1st list element.
    * @param obj2 The 2nd list element.
    * @param obj3 The 3rd list element.
    * @param obj4 The 4th list element.
    * @param obj5 The 5th list element.
    * @param obj6 The 6th list element.
    * @param obj7 The 7th list element.
    * @param obj8 The 8th list element.
    * @return The created list.
    */
   public static Pair list(Object obj1, Object obj2, Object obj3, Object obj4, Object obj5, Object obj6, Object obj7, Object obj8)
   {
      return new Pair(obj1, new Pair(obj2, new Pair(obj3, new Pair(obj4, new Pair(obj5, new Pair(obj6, new Pair(obj7, new Pair(obj8))))))));
   }

   /**
    * Creates a 9-element list from its arguments.
    * @param obj1 The 1st list element.
    * @param obj2 The 2nd list element.
    * @param obj3 The 3rd list element.
    * @param obj4 The 4th list element.
    * @param obj5 The 5th list element.
    * @param obj6 The 6th list element.
    * @param obj7 The 7th list element.
    * @param obj8 The 8th list element.
    * @param obj9 The 9th list element.
    * @return The created list.
    */
   public static Pair list(Object obj1, Object obj2, Object obj3, Object obj4, Object obj5, Object obj6, Object obj7, Object obj8, Object obj9)
   {
      return new Pair(obj1, new Pair(obj2, new Pair(obj3, new Pair(obj4, new Pair(obj5, new Pair(obj6, new Pair(obj7, new Pair(obj8, new Pair(obj9)))))))));
   }

   /**
    * Creates a 10-element list from its arguments.
    * @param obj1 The 1st list element.
    * @param obj2 The 2nd list element.
    * @param obj3 The 3rd list element.
    * @param obj4 The 4th list element.
    * @param obj5 The 5th list element.
    * @param obj6 The 6th list element.
    * @param obj7 The 7th list element.
    * @param obj8 The 8th list element.
    * @param obj9 The 9th list element.
    * @param obj10 The 10th list element.
    * @return The created list.
    */
   public static Pair list(Object obj1, Object obj2, Object obj3, Object obj4, Object obj5, Object obj6, Object obj7, Object obj8, Object obj9, Object obj10)
   {
      return new Pair(obj1, new Pair(obj2, new Pair(obj3, new Pair(obj4, new Pair(obj5, new Pair(obj6, new Pair(obj7, new Pair(obj8, new Pair(obj9, new Pair(obj10))))))))));
   }

   /**
    * Creates an 11-element list from its arguments.
    * @param obj1 The 1st list element.
    * @param obj2 The 2nd list element.
    * @param obj3 The 3rd list element.
    * @param obj4 The 4th list element.
    * @param obj5 The 5th list element.
    * @param obj6 The 6th list element.
    * @param obj7 The 7th list element.
    * @param obj8 The 8th list element.
    * @param obj9 The 9th list element.
    * @param obj10 The 10th list element.
    * @param obj11 The 11th list element.
    * @return The created list.
    */
   public static Pair list(Object obj1, Object obj2, Object obj3, Object obj4, Object obj5, Object obj6, Object obj7, Object obj8, Object obj9, Object obj10, Object obj11)
   {
      return new Pair(obj1, new Pair(obj2, new Pair(obj3, new Pair(obj4, new Pair(obj5, new Pair(obj6, new Pair(obj7, new Pair(obj8, new Pair(obj9, new Pair(obj10, new Pair(obj11)))))))))));
   }

   /**
    * Creates a 12-element list from its arguments.
    * @param obj1 The 1st list element.
    * @param obj2 The 2nd list element.
    * @param obj3 The 3rd list element.
    * @param obj4 The 4th list element.
    * @param obj5 The 5th list element.
    * @param obj6 The 6th list element.
    * @param obj7 The 7th list element.
    * @param obj8 The 8th list element.
    * @param obj9 The 9th list element.
    * @param obj10 The 10th list element.
    * @param obj11 The 11th list element.
    * @param obj12 The 12th list element.
    * @return The created list.
    */
   public static Pair list(Object obj1, Object obj2, Object obj3, Object obj4, Object obj5, Object obj6, Object obj7, Object obj8, Object obj9, Object obj10, Object obj11, Object obj12)
   {
      return new Pair(obj1, new Pair(obj2, new Pair(obj3, new Pair(obj4, new Pair(obj5, new Pair(obj6, new Pair(obj7, new Pair(obj8, new Pair(obj9, new Pair(obj10, new Pair(obj11, new Pair(obj12))))))))))));
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         return super.clone();
      }
      catch (CloneNotSupportedException e)
      {
         return null;
      }
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj2)
   {
      if (this == obj2)
      {
         return true;
      }

      if (!(obj2 instanceof Pair))
      {
         return false;
      }

      Object obj1;
      Pair pair1 = this;
      Pair pair2 = (Pair)obj2;

      for (;;)
      {
         if (!ObjUtil.equal(pair1.getHead(), pair2.getHead()))
         {
            return false;
         }
         
         obj1 = pair1.getTail();
         obj2 = pair2.getTail();
         
         if (obj1 instanceof Pair && obj2 instanceof Pair)
         {
            pair1 = (Pair)obj1;
            pair2 = (Pair)obj2;
         }
         else
         {
            return ObjUtil.equal(obj1, obj2);
         }
      }
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return ((m_head == null) ? 123 : m_head.hashCode()) ^
         ((m_tail == null) ? 456 : m_tail.hashCode());
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      if (writer.addObject(this))
      {
         writer.write('(');

         int nIndentSaved = writer.setIndent(0);
         Pair pair = this;

         for (;;)
         {
            writer.print(pair.m_head);

            if (pair.m_tail instanceof Pair)
            {
               writer.write(' ');
               pair = (Pair)pair.m_tail;
            }
            else
            {
               if (pair.m_tail != null)
               {
                  writer.write(" . ");
                  writer.print(pair.m_tail);
               }

               break;
            }
         }

         writer.setIndent(nIndentSaved);
         writer.removeObject(this);
         writer.write(')');
      }
      else
      {
         writer.write(PrintWriter.REF);
         writer.write("(...)");
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return PrintWriter.toString(this);
   }

   // inner classes

   /**
    * An Iterator over a Scheme list (a chain of Pairs).
    */
   protected static class PairIterator implements Iterator
   {
      // associations

      /**
       * The current head of iteration.
       */
      protected Pair m_head;

      // constructors

      /**
       * Creates a Iterator over a scheme list (a chain of Pairs).
       * 
       * @param head The head of the list to iterate.
       */
      public PairIterator(Pair head)
      {
         m_head = head;
      }

      // operations

      /**
       * @see java.util.Iterator#hasNext()
       */
      public boolean hasNext()
      {
         return (m_head != null);
      }

      /**
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         if (m_head == null)
         {
            throw new NoSuchElementException();
         }

         Object value = m_head.getHead();

         m_head = (Pair)m_head.getTail();

         return value;
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
