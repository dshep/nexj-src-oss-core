package nexj.core.rpc.msg;

import nexj.core.runtime.ActionContext;
import nexj.core.runtime.GenericSerializablePropertyMap;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.runtime.SerializablePropertyMap;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Implements method to deserialize SysMessage body.
 * @deprecated
 */
public class SysMessage implements InvocationContextAware
{
   // associations
   
   /**
    * The invocation context.
    */
   protected InvocationContext m_context;
   
   // operations
   
   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }
   
   /**
    * Loads the body attribute.
    */
   public void load(Instance instance, Pair attributes, ActionContext actx)
   {
      for (; attributes != null; attributes = attributes.getNext())
      {
         if (!(attributes.getHead() instanceof Symbol))
         {
            continue;
         }

         String sName = attributes.getHead().toString();

         if (sName.equals("body") || sName.equals("values"))
         {
            SerializablePropertyMap map = new GenericSerializablePropertyMap(GenericSerializablePropertyMap.LEAVE);

            map.deserializeValues((String)instance.getValue("serializedValues"), m_context);
            instance.setValue("values", map);
            instance.setValue("body", map.findValue("body"));
         }
      }
   }
}
