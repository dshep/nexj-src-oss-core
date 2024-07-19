// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.sys;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.persistence.OID;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.ActionContext;
import nexj.core.runtime.GenericSerializablePropertyMap;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.runtime.SerializablePropertyMap;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.PropertyIterator;
import nexj.core.util.PropertyMap;
import nexj.core.util.StringUtil;

/**
 * SysAuditLog implementation.
 */
public class SysAuditLog implements InvocationContextAware
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
    * Loads the object, state and variables attributes.
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
         
         if (sName.equals("object"))
         {
            InstanceList list = null;
            String sClassName = (String)instance.getValue("class");
            Binary serializedOID = (Binary)instance.getValue("oid");
            
            if (sClassName != null && serializedOID != null)
            {
               Metaclass metaclass = m_context.getMetadata().findMetaclass(sClassName);
               
               if (metaclass != null && metaclass.getPersistenceMapping() != null)
               {
                  list = (InstanceList)metaclass.invoke("read",
                     new Object[]{null, new Pair(Symbol.EQ, new Pair(new Pair(Symbol.AT),
                        new Pair(OID.fromBinary(serializedOID)))), null, null, null, null});
               }
            }

            instance.setValue("object", (list == null || list.isEmpty()) ? null : list.get(0));
         }
         else if (sName.equals("values"))
         {
            SerializablePropertyMap map = new GenericSerializablePropertyMap(GenericSerializablePropertyMap.LEAVE);

            map.deserializeValues((String)instance.getValue("serializedValues"), m_context);
            instance.setValue("values", map);
         }
         else if (sName.equals("objectCaption"))
         {
            StringBuffer buf = new StringBuffer(64);
            Object obj = instance.getValue("object");

            if (obj != null)
            {
               appendValue(buf, obj, false);
            }
            else
            {
               Binary serializedOID = (Binary)instance.getValue("oid");

               if (serializedOID != null)
               {
                  buf.append('<');
                  buf.append(OID.fromBinary(serializedOID));
                  buf.append('>');
               }
            }

            instance.setValue("objectCaption", buf.toString());
         }
         else if (sName.equals("valuesCaption"))
         {
            PropertyMap valueMap = (PropertyMap)instance.getValue("values");
            StringBuffer buf = new StringBuffer(128);
            String sEvent = (String)instance.getValue("event");
            int nEvent = 0;
            
            if ("read".equals(sEvent))
            {
               nEvent = 2;
            }
            else if ("update".equals(sEvent))
            {
               nEvent = 1;
            }

            List nameList = new ArrayList();

            for (PropertyIterator itr = valueMap.getIterator(); itr.hasNext();)
            {
               String sValue = (String)itr.next();

               if (sValue.length() != 0 && sValue.charAt(0) != '-')
               {
                  nameList.add(sValue);
               }
            }
            
            Collections.sort(nameList, String.CASE_INSENSITIVE_ORDER);

            for (int i = 0; i != nameList.size(); ++i)
            {
               String sValue = (String)nameList.get(i);

               if (buf.length() != 0)
               {
                  buf.append(", ");
               }

               buf.append(sValue);

               switch (nEvent)
               {
                  case 0:
                     buf.append('=');
                     appendValue(buf, valueMap.getValue(sValue), true);
                     break;

                  case 1:
                     String sOldName = '-' + sValue;
                     Object oldValue = valueMap.getValue(sOldName);

                     if (oldValue != null || valueMap.hasValue(sOldName))
                     {
                        buf.append(": ");
                        appendValue(buf, valueMap.getValue(sOldName), true);
                        buf.append(" -> ");
                     }
                     else
                     {
                        buf.append('=');
                     }

                     appendValue(buf, valueMap.getValue(sValue), true);
                     break;
               }
            }

            instance.setValue("valuesCaption", buf.toString());
         }
      }
   }

   /**
    * Appends the formatted value to the string buffer.
    * @param buf The string buffer.
    * @param value The value to append.
    * @param bClassName True to append the class name for object types.
    */
   protected static void appendValue(StringBuffer buf, Object value, boolean bClassName)
   {
      if (value instanceof String)
      {
         buf.append('"');
         
         String s = (String)value;

         for (int i = 0; i < s.length(); ++i)
         {
            if (i == 128)
            {
               buf.append("...");
               break;
            }

            char ch = s.charAt(i);

            if (Character.isWhitespace(ch))
            {
               ch = ' ';
            }
            
            buf.append(ch);
         }
         
         buf.append('"');
      }
      else if (value instanceof Instance)
      {
         buf.append('<');

         Instance instance = (Instance)value;

         try
         {
            Attribute attr = instance.getMetaclass().getNameAttribute();

            if (attr != null)
            {
               appendValue(buf, instance.getValue(attr.getOrdinal()), true);
               buf.append(": ");
            }
         }
         catch (Exception e)
         {
         }

         if (bClassName)
         {
            try
            {
               buf.append(instance.getMetaclass().getName());
               buf.append(": ");
            }
            catch (Exception e)
            {
            }
         }

         if (instance.getOID() == null)
         {
            buf.append("OID");
         }
         else
         {
            buf.append(instance.getOID());
         }

         buf.append('>');
      }
      else if (value instanceof TransferObject)
      {
         buf.append('<');

         TransferObject tobj = (TransferObject)value;

         if (bClassName)
         {
            buf.append(tobj.getClassName());
            buf.append(": ");
         }

         if (tobj.getOID() == null)
         {
            buf.append("OID");
         }
         else
         {
            buf.append(tobj.getOID());
         }

         buf.append('>');
      }
      else if (value instanceof Collection)
      {
         buf.append("<...>");
      }
      else if (value instanceof Timestamp)
      {
         StringUtil.appendUTC(buf, (Timestamp)value, false);

         while (buf.length() > 0 && buf.charAt(buf.length() - 1) == '0')
         {
            buf.setLength(buf.length() - 1);
         }

         if (buf.length() > 0 && buf.charAt(buf.length() - 1) == '.')
         {
            buf.setLength(buf.length() - 1);

            if (buf.length() > 9 && buf.indexOf(" 00:00:00", buf.length() - 9) > 0)
            {
               buf.setLength(buf.length() - 9);
            }
         }
      }
      else
      {
         buf.append(value);
      }
   }
}
