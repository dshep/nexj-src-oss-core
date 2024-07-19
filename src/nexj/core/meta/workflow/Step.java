// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.MetadataMarker;
import nexj.core.meta.NamedMetadataObject;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.EmptyIterator;
import nexj.core.util.Lookup;
import nexj.core.util.TextPosition;

/**
 * Abstract flow step.
 */
public abstract class Step extends NamedMetadataObject
{
   // constants
   
   /**
    * Arguments, which are passed to a step function.
    */
   protected final static Pair ARGUMENTS = new ConstPair(Symbol.THIS);
   
   // attributes
   
   /**
    * The step ordinal number.
    */
   protected int m_nOrdinal = -1;

   /**
    * The layout coordinates for this workflow step.
    */
   protected String m_sLayout;

   // associations

   /**
    * The containing activity.
    */
   protected Activity m_activity;

   /**
    * The containing fork.
    */
   protected Fork m_fork;

   /**
    * The next step.
    */
   protected Step m_next;

   /**
    * The cleanup association symbol collection: Symbol[].
    */
   protected List m_cleanupAssocList;

   // constructors

   /**
    * Constructs the flow step.
    * @param sName The name of the flow step.
    */
   public Step(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the flow step. 
    */
   public Step()
   {
      super();
   }
   
   // operations

   /**
    * Sets the containing activity.
    * @param activity The containing activity to set.
    */
   public void setActivity(Activity activity)
   {
      verifyNotReadOnly();
      m_activity = activity;
      
      if (m_activity != null)
      {
         m_fork = m_activity.getFork();
      }
   }

   /**
    * @return The containing activity.
    */
   public Activity getActivity()
   {
      return m_activity;
   }

   /**
    * @return The containing fork.
    */
   public Fork getFork()
   {
      return m_fork;
   }
   
   /**
    * Sets the next step.
    * @param next The next step to set.
    */
   public void setNext(Step next)
   {
      verifyNotReadOnly();
      m_next = next;
   }

   /**
    * @return The next step.
    */
   public Step getNext()
   {
      return m_next;
   }

   /**
    * Sets the step ordinal number.
    * @param nOrdinal The step ordinal number to set.
    */
   public void setOrdinal(int nOrdinal)
   {
      verifyNotReadOnly();
      m_nOrdinal = nOrdinal;
   }

   /**
    * @return The step ordinal number.
    */
   public int getOrdinal()
   {
      return m_nOrdinal;
   }
   
   /**
    * Sets the layout coordinates string.
    * @param sLayout The layout coordinate string
    */
   public void setLayout(String sLayout)
   {
      verifyNotReadOnly();
      m_sLayout = sLayout;
   }

   /**
    * Gets the layout coordinates string.
    * @return The layout
    */
   public String getLayout()
   {
      return m_sLayout;
   }

   /**
    * @return The ordinal number to cleanup.
    */
   public int getCleanupOrdinal()
   {
      return m_nOrdinal;
   }
   
   /**
    * @return True if only a dedicated step type
    * can designate this step as next. 
    */
   public boolean isDependent()
   {
      return false;
   }
   
   /**
    * @return True if the step is persistent.
    */
   public boolean isPersistent()
   {
      return false;
   }

   /**
    * Checks if the state contains this step.
    * @param state The state.
    * @return True if the state contains this step.
    */
   public boolean isActive(State state)
   {
      return state.contains(this);
   }

   /**
    * Proceeds to the next step.
    * @param state The flow state.
    * @return A function to execute, or null if none.
    */
   public abstract PCodeFunction step(State state);

   /**
    * Adds a new cleanup association symbol to the flow.
    * @param assoc The cleanup association symbol to add.
    */
   public void addCleanupAssoc(Symbol assoc)
   {
      verifyNotReadOnly();
      
      if (m_cleanupAssocList == null)
      {
         m_cleanupAssocList = new ArrayList(2);
      }
      
      m_cleanupAssocList.add(assoc);
   }

   /**
    * Gets a cleanup association symbol by ordinal number.
    * @param nOrdinal The cleanup association symbol ordinal number (0-based).
    * @return The cleanup association symbol.
    */
   public Symbol getCleanupAssoc(int nOrdinal)
   {
      return (Symbol)m_cleanupAssocList.get(nOrdinal);
   }

   /**
    * @return The cleanup association symbol count.
    */
   public int getCleanupAssocCount()
   {
      if (m_cleanupAssocList == null)
      {
         return 0;
      }
      
      return m_cleanupAssocList.size();
   }

   /**
    * @return An iterator for the contained cleanup association symbols.
    */
   public Iterator getCleanupAssocIterator()
   {
      if (m_cleanupAssocList == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_cleanupAssocList.iterator();
   }

   /**
    * Sets the text position URLs for the step.
    * @param body The function body to compile.
    */
   protected void setPosURLs(Pair body)
   {
      m_activity.getFlow().setPosURLs(body, getName());
   }

   /**
    * Compiles code for this step.
    * @param args The function arguments.
    * @param body The function body to compile.
    * @param machine The VM for compilation.
    * @return The compiled function.
    */
   protected PCodeFunction compile(Pair args, Pair body, Machine machine)
   {
      Flow flow = m_activity.getFlow();
      Lookup posMap = flow.getPosMap(); 

      if (posMap != null && body != null && !posMap.contains(body))
      {
         posMap.put(body, new TextPosition(0, 0));
      }

      setPosURLs(body);

      return new Compiler().compile(flow.getVariables(),
         args, body, flow.getPosMap(), flow.getURLMap(), machine);
   }
   
   /**
    * Generates the code for the step.
    * @param machine The scripting VM for compilation.
    */
   public void generate(Machine machine)
   {
   }
   
   /**
    * Visits the step.
    * @param visitor The step visitor.
    */
   public void visit(Visitor visitor)
   {
      visitor.visit(this);
   }

   /**
    * Visits the activities contained in the step.
    * @param visitor The activity visitor.
    */
   public void visit(Activity.Visitor visitor)
   {
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      m_activity.getFlow().setProperties(marker);
      marker.setTypeName("Step");
      marker.setProperty("step", m_sName);
   }

   // inner classes

   /**
    * Flow step visitor interface.
    */
   public interface Visitor
   {
      /**
       * Visits the flow steps.
       * @param step The step to visit.
       */
      void visit(Step step);
   }
}
