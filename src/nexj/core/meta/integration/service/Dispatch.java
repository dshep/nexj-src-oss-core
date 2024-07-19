// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.service;

import java.util.ArrayList;
import java.util.List;

import nexj.core.meta.integration.Message;
import nexj.core.meta.workflow.Activity;
import nexj.core.meta.workflow.Branch;
import nexj.core.meta.workflow.Decision;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.Lookup;

/**
 * Message dispatch step.
 */
public class Dispatch extends Decision
{
   // constants
   
   /**
    * Dispatch error code: ((else (error "err.integration.dispatch")))
    */
   protected final static Pair DISPATCH_ERROR_CODE = Pair.list(Pair.list(Symbol.ELSE,
      Pair.list(Symbol.ERROR, "err.integration.dispatch")));
   
   // constructors

   /**
    * Constructs dispatch the step.
    * @param sName The dispatch step name.
    * @param activity The containing activity.
    */
   public Dispatch(String sName, Activity activity)
   {
      super(sName);

      setActivity(activity);
   }

   // operations

   /**
    * @see nexj.core.meta.workflow.Decision#generate(nexj.core.scripting.Machine)
    */
   public void generate(Machine machine)
   {
      verifyNotReadOnly();

      Lookup messageMap = new LinkedHashTab();
      List defaultList = new ArrayList(4);

      // Group the branches by message
      for (int i = 0, n = getBranchCount(); i != n; ++i)
      {
         Branch branch = getBranch(i);
         Message message = (branch instanceof Case) ? ((Case)branch).getMessage() : null;

         if (message != null)
         {
            List list = (List)messageMap.get(message.getName());

            if (list == null)
            {
               list = new ArrayList(4);
               messageMap.put(message.getName(), list);
            }
            
            list.add(branch);
         }
         else
         {
            defaultList.add(branch);
         }
      }

      // Generate the grouped branches

      Pair body = Jump.BODY;
      Pair cond = new Pair(Symbol.COND, generate(defaultList));

      if (messageMap.size() == 0)
      {
         body = new Pair(cond, body);
      }
      else
      {
         // (case (this':class) (("msg1") (cond ...)) ... (("msgN") (cond ...)))

         Pair code = new Pair(Pair.list(Symbol.THIS, Pair.quote(Symbol._CLASS)));

         body = new Pair(new Pair(Symbol.CASE, code), body);

         for (Lookup.Iterator itr = messageMap.iterator(); itr.hasNext();)
         {
            itr.next();

            Pair tail = new Pair(Pair.list(Pair.list(itr.getKey()),
               new Pair(Symbol.COND, generate((List)itr.getValue()))));

            code.setTail(tail);
            code = tail;
         }

         code.setTail(new Pair(Pair.list(Symbol.ELSE, cond)));
      }

      if (m_activity.getFlow().isPrivileged())
      {
         body = Pair.list(new Pair(Symbol.BEGIN_PRIVILEGED, body));
      }

      m_function = compile(ARGUMENTS, body, machine);
   }

   /**
    * Generates the conditions for a given branch list.
    * @param branchList The branch list.
    * @return The generated code.
    */
   protected Pair generate(List branchList)
   {
      boolean bDefault = false;

      for (int i = 0, n = branchList.size(); i != n; ++i)
      {
         if (((Branch)branchList.get(i)).getCondition() == Boolean.TRUE)
         {
            bDefault = true;
            break;
         }
      }

      Pair code = (bDefault) ? null : DISPATCH_ERROR_CODE;

      for (int i = branchList.size() - 1; i >= 0; --i)
      {
         code = new Pair(((Branch)branchList.get(i)).generate(), code);
      }
      
      return code;
   }
}
