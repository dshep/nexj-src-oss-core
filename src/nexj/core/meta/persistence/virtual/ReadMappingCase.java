// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.virtual;

import nexj.core.meta.MetadataObject;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.ObjUtil;

/**
 * A read mapping case. There is one read mapping case for each where clause.
 */
public class ReadMappingCase extends MetadataObject
{
   // constants

   protected final static Symbol GENERATOR_ITERATOR = Symbol.define("generator->iterator");

   /**
    * The read method result.
    */
   protected final static Symbol READ_RESULT = Symbol.define("#readResult");

   /**
    * Symbol for the assert function for performing run-time checks.
    */
   protected final static Symbol ASSERT = Symbol.define("assert");

   /**
    * The read script for this case.
    */
   protected Pair m_readScript;

   /**
    * The cursor close script.
    */
   protected Pair m_closeScript;

   /**
    * The where clause that is matched by this case.
    */
   protected Object m_where;

   /**
    * The outer closure variables for cursor reads.
    */
   protected Pair m_variables;

   // operations

   /**
    * Gets the script to execute when reading.
    * @return The script to execute when reading.
    */
   public Pair getScript()
   {
      /* 
       * Uses an outer closure to maintain the variable state between invocations.
       * 
       * (
       *    (lambda (var1 var2 ... varN)
       *       (cons
       *          (
       *             (lambda (readResult)
       *                (if (instance? readResult PCodeFunction)
       *                   (generator->iterator readResult)
       *                   readResult
       *                )
       *             )
       *             (
       *                (lambda ()  ; New block to allow define statements
       *                   <case1.read>
       *                )
       *             )
       *          )
       *          (lambda ()
       *             <case1.close>
       *          )
       *       )
       *    )
       *    '(() () ... ())
       * )
       */
      Pair cursorScript = new Pair(
         Pair.list(Symbol.LAMBDA, m_variables,
            Pair.list(Symbol.CONS,
               Pair.list(
                  Pair.list(Symbol.LAMBDA, Pair.list(READ_RESULT),
                     Pair.list(Symbol.IF, Pair.list(Symbol.INSTANCE_P, READ_RESULT, PCodeFunction.class),
                        Pair.list(GENERATOR_ITERATOR, READ_RESULT),
                        READ_RESULT
                     )
                  ),
                  Pair.list(
                     new Pair(Symbol.LAMBDA, new Pair(null, (m_readScript == null) ? new Pair(null) : m_readScript))
                  )
               ),
               new Pair(Symbol.LAMBDA, new Pair(null, (m_closeScript == null) ? new Pair(null) : m_closeScript))
            )
         ),
         getVariableInitializer()
      );

      return cursorScript;
   }

   /**
    * Gets a list of nulls for initializing the script variables.
    * @return A list of nulls for initializing the variables.
    */
   private Pair getVariableInitializer()
   {
      Pair result = null;
      Pair var = m_variables;

      while (var != null)
      {
         result = new Pair(null, result);
         var = var.getNext();
      }

      return result;
   }

   /**
    * Sets the read script for this case.
    * @param script The read script.
    */
   public void setReadScript(Pair script)
   {
      verifyNotReadOnly();
      m_readScript = script;
   }

   /**
    * Gets the read script for this case.
    * @return The read script.
    */
   public Pair getReadScript()
   {
      return m_readScript;
   }

   /**
    * Sets the cursor close script for this case.
    * @param script The close script.
    */
   public void setCloseScript(Pair script)
   {
      verifyNotReadOnly();
      m_closeScript = script;
   }

   /**
    * Gets the cursor close script for this case.
    * @return The close script.
    */
   public Pair getCloseScript()
   {
      return m_closeScript;
   }

   /**
    * Sets the where clause for this case.
    * @param where The where clause.
    */
   public void setWhere(Object where)
   {
      verifyNotReadOnly();
      m_where = where;
   }

   /**
    * Gets the where clause for this case.
    * @return The where clause.
    */
   public Object getWhere()
   {
      return m_where;
   }

   /**
    * Sets the outer closure variables for cursor reads.
    * @param variables A list of variable name symbols.
    */
   public void setVariables(Pair variables)
   {
      verifyNotReadOnly();
      m_variables = variables;
   }

   /**
    * Gets the outer closure variables for cursor reads.
    * @return A list of variable name symbols.
    */
   public Pair getVariables()
   {
      return m_variables;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();

      m_readScript = null;
      m_closeScript = null;
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (obj instanceof ReadMappingCase)
      {
         return ObjUtil.equal(m_where, ((ReadMappingCase)obj).m_where);
      }

      return false;
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return (m_where == null) ? 0 : m_where.hashCode();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder(64);

      buf.append("ReadMappingCase(");

      if (m_where == null)
      {
         buf.append("()");
      }
      else
      {
         buf.append(m_where);
      }

      if (m_variables != null)
      {
         buf.append(", variables=[");
         buf.append(m_variables);
         buf.append(']');
      }

      buf.append(')');

      return buf.toString();
   }
}