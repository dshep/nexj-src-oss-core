// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Event;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.MetadataResource;
import nexj.core.meta.MetadataValidationException;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.CompilerException;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashTab;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Logger;
import nexj.core.util.LoggerHolder;
import nexj.core.util.Lookup;
import nexj.core.util.Named;
import nexj.core.util.StringUtil;

/**
 * Flow engine metadata.
 */
public abstract class Flow extends Activity implements MetadataResource, Named, LoggerHolder
{
   // attributes

   /**
    * The metadata resource name.
    */
   protected String m_sResourceName;

   /**
    * The flow name.
    */
   protected String m_sName;

   /**
    * The flow caption.
    */
   protected String m_sCaption;

   /**
    * The flow version.
    */
   protected int m_nVersion;

   /**
    * The privileged flow flag.
    */
   protected boolean m_bPrivileged;

   // associations

   /**
    * The flow step map: Step[String].
    */
   protected Lookup m_stepMap = new HashTab(); 

   /**
    * The persistent step collection: Step[].
    */
   protected List m_stepList = new ArrayList(8);
   
   /**
    * The variable map: Variable[String].
    */
   protected Lookup m_variableMap = new HashTab(8);

   /**
    * The variable list: Variable[].
    */
   protected List m_variableList = new ArrayList(8);
   
   /**
    * The variable symbol pair list.
    */
   protected Pair m_variables;

   /**
    * The expression position map.
    */
   protected Lookup m_posMap = new IdentityHashTab();
   
   /**
    * The expression URL map.
    */
   protected Lookup m_urlMap = new IdentityHashTab();

   /**
    * The flow instance logger.
    */
   protected Logger m_logger;

   /**
    * Run function for parallel execution.
    */
   protected PCodeFunction m_runFunction;

   // constructors

   /**
    * Constructs a flow.
    * @param sName The flow name, must be unique within the repository.
    * @param nVersion The flow version.
    */
   public Flow(String sName, int nVersion)
   {
      setName(sName);
      m_sTokenName = ":0";
      m_nVersion = nVersion;
   }

   // operations

   /**
    * @see nexj.core.meta.MetadataResource#setResourceName(java.lang.String)
    */
   public void setResourceName(String sName)
   {
      verifyNotReadOnly();
      m_sResourceName = sName;
   }

   /**
    * @see nexj.core.meta.MetadataResource#getResourceName()
    */
   public String getResourceName()
   {
      return m_sResourceName;
   }

   /**
    * @see nexj.core.util.Named#setName(java.lang.String)
    */
   public void setName(String sName)
   {
      m_sName = StringUtil.intern(sName);
      m_logger = Logger.getLogger(getClass().getName() + '.' + m_sName);
   }

   /**
    * @see nexj.core.util.Named#getName()
    */
   public String getName()
   {
      return m_sName;
   }

   /**
    * Sets the flow version.
    * @param nVersion The flow version to set.
    */
   public void setVersion(int nVersion)
   {
      verifyNotReadOnly();
      m_nVersion = nVersion;
   }

   /**
    * @return The flow version.
    */
   public int getVersion()
   {
      return m_nVersion;
   }

   /**
    * @return The full flow name (name.version).
    */
   public String getFullName()
   {
      return getName() + "." + m_nVersion;
   }
   
   /**
    * Sets the flow caption.
    * @param sCaption The flow caption to set.
    */
   public void setCaption(String sCaption)
   {
      verifyNotReadOnly();
      m_sCaption = sCaption;
   }

   /**
    * @return The flow caption.
    */
   public String getCaption()
   {
      return m_sCaption;
   }
   
   /**
    * Sets the privileged flow flag.
    * @param bPrivileged The privileged flow flag to set.
    */
   public void setPrivileged(boolean bPrivileged)
   {
      verifyNotReadOnly();
      m_bPrivileged = bPrivileged;
   }

   /**
    * @return The privileged flow flag.
    */
   public boolean isPrivileged()
   {
      return m_bPrivileged;
   }
   
   /**
    * @return The root metadata object.
    */
   public abstract Metadata getMetadata();

   /**
    * @return True if the flow version is current.
    */
   public abstract boolean isCurrentVersion();

   /**
    * @return The flow primary class. Can be null.
    */
   public abstract Metaclass getMetaclass();

   /**
    * @return The name of the associated system metaclass, e.g. "SysService", "SysWorkflow". Can be null.
    */
   public String getSysMetaclass()
   {
      return null;
   }

   /**
    * @return The attribute list for primary object retrieval.
    */
   public abstract Pair getAttributes();

   /**
    * Gets a state management function list for a given event.
    * @param assoc The association path to the instance handling the event.
    * @param event The event for which to get the function.
    * @return The state management function list.
    */
   public abstract Pair getFunctions(Pair assoc, Event event);

   /**
    * Gets the function to return from State.run() if the step
    * does not return a function but cleanup is still required.
    * @return The empty function.
    */
   public abstract PCodeFunction getEmptyFunction();

   /**
    * Gets the default return code.
    * @return The default return code.
    */
   public abstract Pair getDefaultReturnCode();

   /**
    * @return The metadata type name.
    */
   protected abstract String getTypeName();

   /**
    * @return The metadata property name.
    */
   public abstract String getPropName();
   
   /**
    * @return The code URL prefix.
    */
   protected abstract String getURLPrefix();
   
   /**
    * @return The expression position map.
    */
   public Lookup getPosMap()
   {
      return m_posMap;
   }
   
   /**
    * @return The expression URL map.
    */
   public Lookup getURLMap()
   {
      return m_urlMap;
   }

   /**
    * @see nexj.core.util.LoggerHolder#getLogger()
    */
   public Logger getLogger()
   {
      return m_logger;
   }

   /**
    * @see nexj.core.meta.workflow.Activity#getFlow()
    */
   public Flow getFlow()
   {
      return this;
   }

   /**
    * @see nexj.core.meta.workflow.Activity#getParent()
    */
   public Activity getParent()
   {
      return null;
   }
   
   /**
    * @see nexj.core.meta.workflow.Activity#getContainer()
    */
   public Step getContainer()
   {
      return null;
   }

   /**
    * @see nexj.core.meta.workflow.Activity#getFork()
    */
   public Fork getFork()
   {
      return null;
   }

   /**
    * Gets a argument by ordinal number.
    * @param nOrdinal The argument ordinal number (0-based).
    * @return The argument object.
    */
   public abstract Variable getArgument(int nOrdinal);

   /**
    * Gets an argument by name.
    * @param sName The argument name.
    * @return The argument object.
    * @throws MetadataLookupException if the argument does not exist.
    */
   public abstract Variable getArgument(String sName) throws MetadataLookupException;

   /**
    * @return The argument count.
    */
   public abstract int getArgumentCount();

   /**
    * @return An iterator for the contained argument objects.
    */
   public abstract Iterator getArgumentIterator();

   /**
    * Adds a new variable to the flow.
    * @param variable The variable to add.
    * @throws MetadataException if a variable
    * with the same name already exists.
    */
   public void addVariable(Variable variable)
   {
      verifyNotReadOnly();

      Object oldVariable = m_variableMap.put(variable.getName(), variable);

      if (oldVariable != null)
      {
         m_variableMap.put(variable.getName(), oldVariable);

         throw new MetadataException("err.meta." + getPropName() + ".variableDup", new Object[]
         {
            variable.getName(),
            getName()
         });
      }

      variable.setOrdinal(m_variableList.size());
      m_variableList.add(variable);
   }

   /**
    * Finds a variable by name.
    * @param sName The variable name.
    * @return The variable object, or null if not found.
    */
   public Variable findVariable(String sName)
   {
      return (Variable)m_variableMap.get(sName);
   }
   
   /**
    * Gets a variable by name.
    * @param sName The variable name.
    * @return The variable object.
    * @throws MetadataLookupException if the variable does not exist.
    */
   public Variable getVariable(String sName)
   {
      Variable variable = (Variable)m_variableMap.get(sName);

      if (variable != null)
      {
         return variable;
      }

      throw new MetadataLookupException("err.meta." + getPropName() + ".variableLookup", sName, this);
   }
   
   /**
    * Gets a variable by ordinal number.
    * @param nOrdinal The variable ordinal number.
    */
   public Variable getVariable(int nOrdinal)
   {
      return (Variable)m_variableList.get(nOrdinal);
   }

   /**
    * @return The variable count.
    */
   public int getVariableCount()
   {
      return m_variableMap.size();
   }

   /**
    * @return An iterator for the contained variable objects.
    */
   public Iterator getVariableIterator()
   {
      return m_variableList.iterator();
   }

   /**
    * @return The variable symbol list.
    */
   public Pair getVariables()
   {
      return m_variables;
   }

   /**
    * Adds a new step to the flow.
    * @param step The flow step to add.
    * @throws MetadataValidationException if a flow step
    * with the same name already exists.
    */
   public void addFlowStep(Step step)
   {
      verifyNotReadOnly();

      step.setOrdinal(m_stepList.size());
      m_stepList.add(step);
      
      if (step.isDependent() || step.getName() == null)
      {
         return;
      }

      Object oldFlowStep = m_stepMap.put(step.getName(), step);

      if (oldFlowStep != null)
      {
         m_stepMap.put(step.getName(), oldFlowStep);
         m_stepList.remove(step.getOrdinal());

         throw new MetadataException("err.meta." + getPropName() + ".stepDup",
            new Object[] {step.getName(), getFullName()});
      }
   }

   /**
    * Gets a flow step by name.
    * @param sName The flow step name.
    * @return The flow step object.
    * @throws MetadataLookupException if the flow step does not exist.
    */
   public Step getFlowStep(String sName)
   {
      Step step = (Step)m_stepMap.get(sName);

      if (step != null)
      {
         return step;
      }

      throw new MetadataLookupException("err.meta." + getPropName() + ".stepLookup", sName, this);
   }

   /**
    * Gets a flow step by ordinal number.
    * @param nOrdinal The step ordinal number (0-based).
    * @return The step object.
    */
   public Step getFlowStep(int nOrdinal)
   {
      return (Step)m_stepList.get(nOrdinal);
   }

   /**
    * @return The flow step count.
    */
   public int getFlowStepCount()
   {
      return m_stepList.size();
   }

   /**
    * @return An iterator for the contained flow step objects.
    */
   public Iterator getFlowStepIterator()
   {
      return m_stepList.iterator();
   }

   /**
    * Sets the URL for a given compiled code.
    * @param code The code.
    * @param sName The last URL part.
    */
   public void setPosURLs(Object code, String sName)
   {
      if (sName != null)
      {
         Compiler.setPosURLs(code, getURLPrefix() + getFullName() + "." + sName, m_posMap, m_urlMap);
      }
   }
   
   /**
    * Performs the flow graph computations.
    * @param machine The scripting VM for compilation.
    * @throws nexj.core.meta.MetadataException if an error is detected.
    */
   public void resolve(Machine machine) throws MetadataException
   {
      verifyNotReadOnly();
      init();
      link();
      generate(machine);
   }

   /**
    * @return The reserved variable count.
    */
   public int getReservedVarCount()
   {
      return 2;
   }
   
   /**
    * Initializes the resolution.
    * Creates the variable list.
    */
   protected void init()
   {
      for (int i = m_variableList.size() - 1; i >= 0; --i)
      {
         m_variables = new ConstPair(getVariable(i).getSymbol(), m_variables);
      }

      // nReservedVarCount reserved variables are prepended
      m_variables = new ConstPair(Symbol._FLOW, new ConstPair(Symbol._STATE, m_variables));
   }

   /**
    * Links the steps in the flow and check for link inconsistencies.
    */
   protected void link()
   {
      final Lookup map = new HashTab();

      // Link the steps with unspecified successors
      visit(new Activity.Visitor()
      {
         public void visit(Activity activity)
         {
            // Generate unique token name among the concurrent activities
            if (activity instanceof Concurrent)
            {
               Fork fork = activity.getFork();
               
               for (int i = 0, n = fork.getConcurrentCount(); i != n; ++i)
               {
                  if (fork.getConcurrent(i) == activity)
                  {
                     activity.setTokenName(fork.getActivity().getTokenName() + ":" + i);
                     break;
                  }
               }
            }
            else
            {
               Activity parent = activity.getParent();
               
               if (parent != null)
               {
                  activity.setTokenName(parent.getTokenName());
               }
            }

            for (int i = 0; i < activity.getStepCount(); ++i)
            {
               Step step = activity.getStep(i);

               if (i == 0)
               {
                  map.put(step, Boolean.TRUE);
               }

               if (step.getNext() == null)
               {
                  if (!(step instanceof Redirector))
                  {
                     if (i == activity.getStepCount() - 1)
                     {
                        if (activity.getContainer() != null)
                        {
                           step.setNext(activity.getContainer().getNext());
                        }
                     }
                     else
                     {
                        step.setNext(activity.getStep(i + 1));
                     }
                  }
               }
               else if (activity.getFork() != null)
               {
                  // Take into account the added Join element
                  if (step.getNext() == activity.getFork().getNext().getNext())
                  {
                     step.setNext(activity.getFork().getNext());
                  }
                  // See if this element should terminate the fork 
                  else if (step.getNext().getFork() != activity.getFork())
                  {
                     Join join = new Join(step.getFork());

                     join.setAny(true);
                     join.setNext(step.getNext());
                     step.setNext(join);
                     activity.addStep(++i, join);
                  }
               }
            }
         }
      });

      // Check the successor step validity and mark the reachable steps
      visit(new Step.Visitor()
      {
         public void visit(Step step)
         {
            if (!map.contains(step))
            {
               map.put(step, Boolean.FALSE);
            }

            Step next = step.getNext();

            if (next != null)
            {
               if (!next.getActivity().getRootConcurrent().contains(step.getActivity()))
               {
                  throw new MetadataException("err.meta." + getPropName() + ".invalidNextStep",
                     new Object[]{next.getName(), getStepName(step),
                     step.getActivity().getFlow().getFullName()});
               }

               map.put(next, Boolean.TRUE);
            }
         }
      });

      // Find the unreachable steps
      for (Lookup.Iterator itr = map.valueIterator(); itr.hasNext();)
      {
         if (itr.next() == Boolean.FALSE)
         {
            Step step = (Step)itr.getKey();

            throw new MetadataException("err.meta." + getPropName() + ".unreachableStep",
               new Object[]{step.getName(), step.getActivity().getFlow().getFullName()});
         }
      }

      // Log the persistent state values
      if (m_logger.isDumpEnabled())
      {
         m_logger.dump(toString() + " states:");
         
         for (Iterator itr = getFlowStepIterator(); itr.hasNext();)
         {
            Step step = (Step)itr.next();

            m_logger.dump("[" + step.getOrdinal() + "]: " + step);
         }
      }
   }

   /**
    * Generates the code for the steps.
    * @param machine The scripting VM for compilation.
    */
   protected void generate(final Machine machine)
   {
      machine.getGlobalEnvironment().defineVariable(Symbol.SYS_CURRENT_LOGGER, m_logger);

      try
      {
         // Generate each step
         visit(new Step.Visitor()
         {
            public void visit(Step step)
            {
               step.generate(machine);
            }
         });

         compile(machine);
      }
      catch (CompilerException e)
      {
         MetadataValidationException x = new MetadataValidationException(e);
         String sFullName = getFullName();

         x.setTypeName(getTypeName());
         x.setProperty(getPropName(), sFullName);

         String sURL = ((CompilerException)e).getURL();

         if (sURL != null && sURL.startsWith(getURLPrefix()))
         {
            x.setProperty("step", sURL.substring(getURLPrefix().length() + sFullName.length() + 1));
         }

         throw x;
      }
      finally
      {
         machine.getGlobalEnvironment().removeVariable(Symbol.SYS_CURRENT_LOGGER);
      }

      m_posMap = null;
      m_urlMap = null;
   }

   /**
    * Get the run function used for parallel execution.
    * @return The run function used for parallel execution.
    */
   public PCodeFunction getRunFunction()
   {
      return m_runFunction;
   }

   /**
    * Template method to compile the code.
    * @param machine The scripting VM for compilation.
    */
   protected void compile(final Machine machine) throws CompilerException
   {
      m_runFunction = new Compiler().compile(m_variables,
         ConstPair.list(Symbol.THIS),
         Pair.list(Pair.list(Symbol.define(":flow"), Pair.quote(Symbol.define("run")))),
         m_posMap, m_urlMap, machine);
   }

   /**
    * Gets the name of a step. If null and the step is a Redirector,
    * searches for the name of a previous step.
    * @param step The step.
    * @return The name of the step or null if unknown.
    */
   protected String getStepName(final Step step)
   {
      if (step.getName() != null || !(step instanceof Redirector))
      {
         return step.getName();
      }

      if (step.getActivity().getStep(0) == step)
      {
         return step.getActivity().getContainer().getName();
      }

      final String[] name = new String[1];

      visit(new Step.Visitor()
      {
         public void visit(Step prev)
         {
            if (prev.getNext() == step)
            {
               name[0] = getStepName(prev);
            }
         }
      });
      
      return name[0];
   }

   /**
    * @see nexj.core.meta.MetadataObject#createLookupException()
    */
   protected MetadataException createLookupException()
   {
      return new MetadataLookupException("err.meta." + getPropName() + "serviceLookup",
         getFullName(), getMetadata().getName());
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      marker.setResourceName(m_sResourceName);
      marker.setTypeName(getTypeName());
      marker.setProperty(getPropName(), getFullName());
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();

      visit(new Step.Visitor()
      {
         public void visit(Step step)
         {
            step.makeReadOnly();
         }
      });

      for (int i = 0, nCount = m_variableList.size(); i < nCount; i++)
      {
         ((MetadataObject)m_variableList.get(i)).makeReadOnly();
      }

      ((ArrayList)m_stepList).trimToSize();
      ((ArrayList)m_variableList).trimToSize();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      String sName = getClass().getName();
      
      return sName.substring(sName.lastIndexOf('.') + 1) +  " " + getFullName();
   }
}
