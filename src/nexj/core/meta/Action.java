// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.lang.reflect.Method;

import nexj.core.scripting.Symbol;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.Lookup;

/**
 * An event action. It is executed when an event is triggered,
 * if its associated condition is satisfied.
 */
public final class Action extends NamedMetadataObject implements Cloneable
{
   // constants

   /**
    * The main action.
    */
   public final static int MAIN = 0;

   /**
    * Before-action. Executed automatically before the main action.
    * The return value is ignored.
    */
   public final static int BEFORE = 1;

   /**
    * After-action. Executed automatically after the main action.
    * The return value is ignored.
    */
   public final static int AFTER = 2;

   /**
    * Around-action. Executed instead of the main action. It is the
    * responsibility of this action to invoke the main action and
    * to provide a return value.
    */
   public final static int AROUND = 3;
   
   /**
    * Reserved MAIN action name.
    */
   public final static String MAIN_ACTION_NAME = "main";
   
   /**
    * Action type symbol array, indexed by action type number.
    */
   protected final static Symbol[] TYPE_SYMBOL_ARRAY = new Symbol[]
   {
      Symbol.MAIN,
      Symbol.BEFORE,
      Symbol.AFTER,
      Symbol.AROUND,
   };

   // attributes

   /**
    * The action group name. Only one action within a group will be executed.
    * Null means that the action is not grouped.
    */
   protected String m_sGroupName;

   /**
    * The action type. One of the Action.* constants.
    */
   protected int m_nType = MAIN;

   /**
    * The ordinal number. Used in sorting.
    */
   protected int m_nOrdinal = Integer.MAX_VALUE;

   // associations

   /**
    * The conditional expression associated with the action.
    */
   protected Object m_condition;

   /**
    * The action body.
    */
   protected Object m_body;

   /**
    * The action body text position map.
    */
   protected Lookup m_textPosMap;

   /**
    * The containing event.
    */
   protected Event m_event;

   /**
    * The next action. Action priorities are calculated during initialization
    * based on topological sorting of actions.
    */
   protected Action m_nextAction;
   
   /**
    * Java implementation method object.
    */
   protected Method m_method;

   /**
    * The declarator of this action.
    */
   protected Metaclass m_declarator;

   /**
    * The Java class of this action.
    */
   protected Class m_class;

   // constructors

   /**
    * Creates an action with a given name.
    * @param sName The name of the action.
    */
   public Action(String sName)
   {
      super(sName);
   }

   /**
    * Creates an action with a given name and group name.
    * @param sName The name of the action.
    * @param sGroupName The name of the action group.
    */
   public Action(String sName, String sGroupName)
   {
      super(sName);
      m_sGroupName = sGroupName;
   }
   // operations

   /**
    * Sets the action group name.
    * @param sGroupName The action group name to set.
    */
   public void setGroupName(String sGroupName)
   {
      verifyNotReadOnly();
      m_sGroupName = sGroupName;
   }

   /**
    * @return The action group name.
    */
   public String getGroupName()
   {
      return m_sGroupName;
   }

   /**
    * @return The action full name (action/group).
    */
   public String getFullName()
   {
      if (m_sGroupName != null)
      {
         return m_sGroupName + ":" + getName();
      }

      return getName();
   }

   /**
    * Sets the action type.
    * @param nType The action type to set.
    */
   public void setType(int nType)
   {
      verifyNotReadOnly();
      m_nType = nType;
   }

   /**
    * @return The action type.
    */
   public int getType()
   {
      return m_nType;
   }

   /**
    * @return The symbol corresponding to the action type.
    */
   public Symbol getTypeSymbol()
   {
      return TYPE_SYMBOL_ARRAY[m_nType];
   }

   /**
    * Sets the action conditional expression.
    * @param condition The action conditional expression to set. Can be null.
    */
   public void setCondition(Object condition)
   {
      verifyNotReadOnly();
      m_condition = condition;
   }

   /**
    * @return The action conditional expression.
    */
   public Object getCondition()
   {
      return m_condition;
   }

   /**
    * Sets the action body.
    * @param body The action body to set.
    */
   public void setBody(Object body)
   {
      verifyNotReadOnly();
      m_body = body;
   }

   /**
    * @return The action body.
    */
   public Object getBody()
   {
      return m_body;
   }
   
   /**
    * Sets the action body text position map.
    * @param textPosMap The action body text position map to set.
    */
   public void setTextPositionMap(Lookup textPosMap)
   {
      verifyNotReadOnly();
      m_textPosMap = textPosMap;
   }

   /**
    * @return The action body text position map.
    */
   public Lookup getTextPositionMap()
   {
      return m_textPosMap;
   }
   
   /**
    * Sets the containing event.
    * @param event The containing event to set.
    */
   public void setEvent(Event event)
   {
      verifyNotReadOnly();
      m_event = event;

      if (m_declarator == null)
      {
         m_declarator = event.getDeclarator();
      }
   }

   /**
    * @return The containing event.
    */
   public Event getEvent()
   {
      return m_event;
   }

   /**
    * Sets the next action by name.
    * @param sName The next action name.
    */
   public void setNextAction(String sName)
   {
      m_event.getMetaclass().setNextAction(this, sName);
   }
   
   /**
    * Sets the next action.
    * @param nextAction The next action to set.
    */
   public void setNextAction(Action nextAction)
   {
      verifyNotReadOnly();
      m_nextAction = nextAction;
   }

   /**
    * @return The next action.
    */
   public Action getNextAction()
   {
      return m_nextAction;
   }

   /**
    * Sets the Java implementation method.
    * @param method The Java implementation method to set.
    */
   public void setMethod(Method method)
   {
      verifyNotReadOnly();
      m_method = method;
   }

   /**
    * @return The Java implementation method.
    */
   public Method getMethod()
   {
      return m_method;
   }

   /**
    * @return The metaclass that declared this action
    */
   public Metaclass getDeclarator()
   {
      return m_declarator;
   }

   /**
    * @param sName Declarator metaclass.
    */
   public void setDeclarator(Metaclass declarator)
   {
      m_declarator = declarator;
   }

   /**
    * Get the method class.
    * @return The method class.
    */
   public Class getMethodClass()
   {
      return m_class;
   }

   /**
    * Set the method class.
    * @param clazz The method class.
    */
   public void setMethodClass(Class clazz)
   {
      m_class = clazz;
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      m_event.setProperties(marker);
      marker.setTypeName("Action");
      marker.setProperty("action", getFullName());
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();
      m_textPosMap = null; // free memory not used after Event.compile()
   }

   /**
    * Verifies the consistency of the action.
    * @param metadata The root metadata object.
    * @throws MetadataException if the validation fails.
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      super.validate(metadata, warnings);

      if ((m_nType == MAIN) != getName().equals(Action.MAIN_ACTION_NAME))
      {
         throw new MetadataException("err.meta.actionName",
            new Object[]{getName(), getTypeSymbol()});
      }

      if (m_sGroupName != null && m_nType != BEFORE && m_nType != AFTER)
      {
         throw new MetadataException("err.meta.groupedActionType", new Object[]{getTypeSymbol()});
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(64);

      buf.append("Action ");

      if (m_event != null)
      {
         if (m_event.getMetaclass() != null)
         {
            buf.append(m_event.getMetaclass().getName());
            buf.append('.');
         }
         
         buf.append(m_event.getName());
         buf.append('.');
      }

      buf.append(getName());

      if (m_sGroupName != null)
      {
         buf.append('/');
         buf.append(m_sGroupName);
      }

      switch (m_nType)
      {
         case MAIN:
            buf.append(" <main>");
            break;
         
         case BEFORE:
            buf.append(" <before>");
            break;
            
         case AROUND:
            buf.append(" <around>");
            break;
            
         case AFTER:
            buf.append(" <after>");
            break;
            
         default:
            buf.append(" <?>");
            break;
      }
      
      if (m_nextAction != null)
      {
         buf.append(' ');
         buf.append(m_nextAction.getName());
      }

      return buf.toString();
   }
}
