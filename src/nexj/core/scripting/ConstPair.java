// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.io.ObjectStreamException;

/**
 * Read-only pair implementation.
 */
public class ConstPair extends Pair
{
   // attributes
   
   /**
    * The Java stream unique identifier for the class.
    */
   private final static long serialVersionUID = 4905439272318294107L;

   // constructors

   /**
    * Constructs the pair.
    * @param head The pair head.
    */
   public ConstPair(Object head)
   {
      super(head);
   }

   /**
    * Constructs the pair.
    * @param head The pair head.
    * @param tail The pair tail.
    */
   public ConstPair(Object head, Object tail)
   {
      super(head, tail);
   }
   
   // operations

   /**
    * Prevents modification.
    * @see nexj.core.scripting.Pair#setHead(java.lang.Object)
    */
   public void setHead(Object head)
   {
      if (head != m_head)
      {
         throw new ScriptingException("err.scripting.readOnlyPair");
      }
   }

   /**
    * Prevents modification.
    * @see nexj.core.scripting.Pair#setTail(java.lang.Object)
    */
   public void setTail(Object tail)
   {
      if (tail != m_tail)
      {
         throw new ScriptingException("err.scripting.readOnlyPair");
      }
   }
   
   /**
    * Replaces the object with Pair during serialization.
    */
   private Object writeReplace() throws ObjectStreamException
   {
      return new Pair(m_head, m_tail);
   }

   /**
    * Creates a chain of ConstPairs whose cars are the objNN arguments and whose final cdr is tail.
    * @param obj1 The 1st car.
    * @param obj2 The 2nd car.
    * @param tail The final cdr.
    * @return The created chain of ConstPairs.
    */
   public static Pair cons(Object obj1, Object obj2, Object tail)
   {
      return new ConstPair(obj1, new ConstPair(obj2, tail));
   }

   /**
    * Creates a chain of ConstPairs whose cars are the objNN arguments and whose final cdr is tail.
    * @param obj1 The 1st car.
    * @param obj2 The 2nd car.
    * @param obj3 The 3rd car.
    * @param obj4 The 4th car.
    * @param tail The final cdr.
    * @return The created chain of ConstPairs.
    */
   public static Pair cons(Object obj1, Object obj2, Object obj3, Object obj4, Object tail)
   {
      return new ConstPair(obj1, new ConstPair(obj2, new ConstPair(obj3, new ConstPair(obj4, tail))));
   }

   /**
    * Creates a 1-element list from the argument.
    * @param obj The list element.
    * @return The created list.
    */
   public static Pair list(Object obj)
   {
      return new ConstPair(obj);
   }

   /**
    * Creates a 2-element list from its arguments.
    * @param obj1 The 1st list element.
    * @param obj2 The 2nd list element.
    * @return The created list.
    */
   public static Pair list(Object obj1, Object obj2)
   {
      return new ConstPair(obj1, new ConstPair(obj2));
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
      return new ConstPair(obj1, new ConstPair(obj2, new ConstPair(obj3)));
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
      return new ConstPair(obj1, new ConstPair(obj2, new ConstPair(obj3, new ConstPair(obj4))));
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
      return new ConstPair(obj1, new ConstPair(obj2, new ConstPair(obj3, new ConstPair(obj4, new ConstPair(obj5)))));
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
      return new ConstPair(obj1, new ConstPair(obj2, new ConstPair(obj3, new ConstPair(obj4, new ConstPair(obj5, new ConstPair(obj6))))));
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
      return new ConstPair(obj1, new ConstPair(obj2, new ConstPair(obj3, new ConstPair(obj4, new ConstPair(obj5, new ConstPair(obj6, new ConstPair(obj7)))))));
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
      return new ConstPair(obj1, new ConstPair(obj2, new ConstPair(obj3, new ConstPair(obj4, new ConstPair(obj5, new ConstPair(obj6, new ConstPair(obj7, new ConstPair(obj8))))))));
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
      return new ConstPair(obj1, new ConstPair(obj2, new ConstPair(obj3, new ConstPair(obj4, new ConstPair(obj5, new ConstPair(obj6, new ConstPair(obj7, new ConstPair(obj8, new ConstPair(obj9)))))))));
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
      return new ConstPair(obj1, new ConstPair(obj2, new ConstPair(obj3, new ConstPair(obj4, new ConstPair(obj5, new ConstPair(obj6, new ConstPair(obj7, new ConstPair(obj8, new ConstPair(obj9, new ConstPair(obj10))))))))));
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
      return new ConstPair(obj1, new ConstPair(obj2, new ConstPair(obj3, new ConstPair(obj4, new ConstPair(obj5, new ConstPair(obj6, new ConstPair(obj7, new ConstPair(obj8, new ConstPair(obj9, new ConstPair(obj10, new ConstPair(obj11)))))))))));
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
      return new ConstPair(obj1, new ConstPair(obj2, new ConstPair(obj3, new ConstPair(obj4, new ConstPair(obj5, new ConstPair(obj6, new ConstPair(obj7, new ConstPair(obj8, new ConstPair(obj9, new ConstPair(obj10, new ConstPair(obj11, new ConstPair(obj12))))))))))));
   }
}
