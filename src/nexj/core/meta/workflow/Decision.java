// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Flow decision step.
 */
public class Decision extends Step
{
   // attributes

   /**
    * The manual decision flag.
    */
   protected boolean m_bManual;

   // associations

   /**
    * The branch collection.
    */
   protected List m_branchList = new ArrayList(2); // of type Branch

   /**
    * The state management function.
    */
   protected PCodeFunction m_function;

   // constructors
   
   /**
    * Constructs the decision.
    * @param sName The decision name.
    */
   public Decision(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the decision. 
    */
   public Decision()
   {
      super();
   }

   // operations
   
   /**
    * Sets the manual decision flag.
    * @param bManual The manual decision flag to set.
    */
   public void setManual(boolean bManual)
   {
      verifyNotReadOnly();
      m_bManual = bManual;
   }

   /**
    * @return The manual decision flag.
    */
   public boolean isManual()
   {
      return m_bManual;
   }
   
   /**
    * Adds a new branch to the decision.
    * @param branch The branch to add.
    */
   public void addBranch(Branch branch)
   {
      verifyNotReadOnly();
      m_branchList.add(branch);
      branch.setDecision(this);
   }

   /**
    * Gets a branch by ordinal number.
    * @param nOrdinal The branch ordinal number (0-based).
    * @return The branch object.
    */
   public Branch getBranch(int nOrdinal)
   {
      return (Branch)m_branchList.get(nOrdinal);
   }

   /**
    * @return The branch count.
    */
   public int getBranchCount()
   {
      return m_branchList.size();
   }

   /**
    * @return An iterator for the contained branch objects.
    */
   public Iterator getBranchIterator()
   {
      return m_branchList.iterator();
   }

   /**
    * @see nexj.core.meta.workflow.Step#generate(nexj.core.scripting.Machine)
    */
   public void generate(Machine machine)
   {
      verifyNotReadOnly();

      Pair code = null;

      for (int i = getBranchCount() - 1; i >= 0; --i)
      {
         code = new Pair(getBranch(i).generate(), code);
      }

      code = new Pair(Symbol.COND, code);

      if (m_activity.getFlow().isPrivileged())
      {
         code = Pair.list(Symbol.BEGIN_PRIVILEGED, code);
      }

      m_function = compile(ARGUMENTS, Pair.list(code), machine);
   }

   /**
    * @see nexj.core.meta.workflow.Step#step(nexj.core.meta.workflow.State)
    */
   public PCodeFunction step(State state)
   {
      return m_function;
   }
   
   /**
    * @see nexj.core.meta.workflow.Step#visit(nexj.core.meta.workflow.Step.Visitor)
    */
   public void visit(Visitor visitor)
   {
      super.visit(visitor);

      for (int i = 0, nCount = m_branchList.size(); i < nCount; ++i)
      {
         ((Branch)m_branchList.get(i)).visit(visitor);
      }
   }

   /**
    * @see nexj.core.meta.workflow.Step#visit(nexj.core.meta.workflow.Activity.Visitor)
    */
   public void visit(Activity.Visitor visitor)
   {
      super.visit(visitor);

      for (int i = 0, nCount = m_branchList.size(); i < nCount; ++i)
      {
         ((Branch)m_branchList.get(i)).visit(visitor);
      }
   }
}
