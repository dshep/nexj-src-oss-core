// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.timer;

import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.Executable;

/**
 * Command for deferring a persistent timer timeout.
 */
public class DeferTimeoutCommand implements Executable, InvocationContextAware, java.io.Serializable
{
   // constants
   
   /**
    * The Java stream unique identifier for the class.
    */
   private final static long serialVersionUID = 520769862698268611L;   
   
   // attributes
   
   /**
    * The component to defer.
    */
   protected String m_sComponent;
   
   // associations
   
   /**
    * The invocation context.
    */
   protected transient InvocationContext m_context;

   // constructors
   
   /**
    * Constructs the command.
    * @param sComponent The name of the component to defer.
    */
   public DeferTimeoutCommand(String sComponent)
   {
      m_sComponent = sComponent;
   }
   
   // operations

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * @see nexj.core.util.Executable#execute()
    */
   public void execute()
   {
      ((PersistentTimer)m_context.getComponentInstance(m_sComponent)).defer();
   }
}
