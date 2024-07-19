// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import nexj.core.meta.Action;
import nexj.core.runtime.ActionContext;
import nexj.core.util.ObjUtil;

/**
 * Wrapper for a Java method implementing an action body.
 */
public class JavaAction implements Function, FrameAware
{
   // associations

   /**
    * The action metadata.
    */
   private Action m_action;

   // constructors

   /**
    * Constructs the wrapper.
    * @param action The action metadata.
    */
   public JavaAction(Action action)
   {
      m_action = action;
   }

   // operations

   /**
    * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
    */
   public boolean invoke(int nArgCount, Machine machine)
   {
      assert nArgCount == 1;

      PCodeFunction fun = (PCodeFunction)machine.getArg(0, nArgCount);
      Object[] frame = fun.frame;
      Object obj;

      while ((obj = frame[0]) != null)
      {
         frame = (Object[])obj;
      }

      int nCount = m_action.getEvent().getArgumentCount();
      Object[] args = new Object[nCount + 2];

      for (int i = 0; i <= nCount; ++i)
      {
         args[i] = frame[i + 1];
      }

      args[nCount + 1] = new JavaActionContext(fun, frame, machine, m_action);

      try
      {
         Method method = m_action.getMethod();

         machine.returnValue(method.invoke(
            ((method.getModifiers() & Modifier.STATIC) != 0) ? null :
            machine.getContext().getClassInstance(m_action.getMethodClass()), args),
            nArgCount);
      }
      catch (Exception e)
      {
         Throwable t = e;

         if (t instanceof InvocationTargetException)
         {
            t = ((InvocationTargetException)e).getCause();
         }

         if (t instanceof RuntimeException)
         {
            throw (RuntimeException)t;
         }

         throw new ScriptingException("err.scripting.exception",
            new Object[]{ObjUtil.getMessage(t)}, t);
      }

      return false;
   }

   // inner classes

   public final static class JavaActionContext implements ActionContext
   {
      /**
       * The next action closure.
       */
      private PCodeFunction m_fun;

      /**
       * The event argument frame.
       */
      private Object[] m_frame;

      /**
       * The VM.
       */
      private Machine m_machine;

      /**
       * The action metadata.
       */
      private Action m_action;

      /**
       * Constructs the action context.
       * @param fun The next action closure.
       * @param frame The event argument frame.
       * @param machine The VM.
       * @param action The action metadata.
       */
      public JavaActionContext(PCodeFunction fun, Object[] frame, Machine machine, Action action)
      {
         m_fun = fun;
         m_frame = frame;
         m_machine = machine;
         m_action = action;
      }

      /**
       * @see nexj.core.runtime.ActionContext#getAction()
       */
      public Action getAction()
      {
         return m_action;
      }

      /**
       * @see nexj.core.runtime.ActionContext#setArg(int, java.lang.Object)
       */
      public void setArg(int nOrdinal, Object value)
      {
         if (nOrdinal < 0)
         {
            throw new ArrayIndexOutOfBoundsException(nOrdinal);
         }

         m_frame[nOrdinal + 2] = value;
      }

      /**
       * @see nexj.core.runtime.ActionContext#getArg(int)
       */
      public Object getArg(int nOrdinal)
      {
         if (nOrdinal < 0)
         {
            throw new ArrayIndexOutOfBoundsException(nOrdinal);
         }

         return m_frame[nOrdinal + 2];
      }

      /**
       * @see nexj.core.runtime.ActionContext#callNext()
       */
      public Object callNext()
      {
         return m_machine.invoke(m_fun, (Pair)null);
      }
   }
}
