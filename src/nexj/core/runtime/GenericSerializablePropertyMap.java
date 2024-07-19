// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.List;

import nexj.core.rpc.CharacterStreamMarshaller;
import nexj.core.rpc.CharacterStreamUnmarshaller;
import nexj.core.rpc.InstanceFactory;
import nexj.core.rpc.RPCUtil;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.text.TextMarshaller;
import nexj.core.rpc.text.TextUnmarshaller;
import nexj.core.scripting.GenericPropertyMap;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.PropertyHashTab;
import nexj.core.util.PropertyIterator;
import nexj.core.util.UncheckedException;

/**
 * Generic serializable property map.
 */
public class GenericSerializablePropertyMap extends GenericPropertyMap implements SerializablePropertyMap
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -505290948979143451L;

   /**
    * Fail on deserialization error.
    */
   public final static byte FAIL = 0;

   /**
    * Skip a transfer object that fails to instantiate.
    */
   public final static byte SKIP = 1;

   /**
    * Leave a transfer object that fails to instantiate.
    */
   public final static byte LEAVE = 2;
   
   /**
    * Don't try to instantiate transfer objects.
    */
   public final static byte RAW = 3;

   // attributes

   /**
    * The safe mode, one of the FAIL, SKIP, LEAVE or RAW.
    */
   protected byte m_nSafeMode;

   // constructors

   /**
    * Constructs the property map.
    */
   public GenericSerializablePropertyMap()
   {
   }

   /**
    * Constructs the property map.
    * @param nSaveMode The mode, one of the FAIL, SKIP, LEAVE or RAW.
    */
   public GenericSerializablePropertyMap(byte nSaveMode)
   {
      m_nSafeMode = nSaveMode;
   }

   // operations

   /**
    * Serializes values to a writer.
    * @param writer The writer.
    * @param context The context.
    */
   protected void serializeValues(Writer writer, InvocationContext context)
   {
      TransferObject tobj = new TransferObject(m_map.size());
      Lookup identityMap = null;

      for (Lookup.Iterator itr = m_map.valueIterator(); itr.hasNext();)
      {
         Object value = itr.next();
         String sName = (String)itr.getKey();

         if (value instanceof Instance || value instanceof InstanceList)
         {
            if (identityMap == null)
            {
               identityMap = new HashTab();
            }

            value = RPCUtil.transferState(value, null, identityMap,
               (m_nSafeMode == FAIL || m_nSafeMode == RAW) ? RPCUtil.TF_ALL : RPCUtil.TF_ALL | RPCUtil.TF_SERIALIZABLE);
         }

         tobj.setValue(sName, value);
      }

      try
      {
         createMarshaller(context).serialize(tobj, writer);
      }
      catch (IOException e)
      {
         throw new UncheckedException("err.rpc.valueSerialization", e);
      }
   }
   
   /**
    * @see nexj.core.runtime.SerializablePropertyMap#serializeValuesToBinary(nexj.core.runtime.InvocationContext)
    */
   public Binary serializeValuesToBinary(InvocationContext context)
   {
      if (m_map == null)
      {
         return null;
      }

      ByteArrayOutputStream bos = new ByteArrayOutputStream();

      try
      {
         Writer writer = new BufferedWriter(new OutputStreamWriter(bos, "UTF-8"));

         serializeValues(writer, context);
         writer.flush();
      }
      catch (IOException e)
      {
         throw new UncheckedException("err.rpc.valueSerialization", e);
      }
      
      return new Binary(bos.toByteArray());
   }
   
   /**
    * @see nexj.core.runtime.SerializablePropertyMap#serializeValues(InvocationContext)
    */
   public String serializeValues(InvocationContext context)
   {
      if (m_map == null)
      {
         return null;
      }

      StringWriter writer = new StringWriter();
      
      serializeValues(writer, context);
      
      return writer.toString();
   }

   /**
    * @see nexj.core.runtime.SerializablePropertyMap#deserializeValues(java.lang.String, InvocationContext)
    */
   public void deserializeValues(Object serializedValues, InvocationContext context)
   {
      if (serializedValues == null)
      {
         m_map = null;
      }
      else
      {
         TransferObject tobj;

         try
         {
            Reader reader;

            if (serializedValues instanceof String)
            {
               reader = new StringReader((String)serializedValues);
            }
            else
            {
               reader = new InputStreamReader(((Binary)serializedValues).getInputStream(), "UTF-8");
            }

            tobj = (TransferObject)createUnmarshaller(context).deserialize(reader);
         }
         catch (IOException e)
         {
            throw new UncheckedException("err.rpc.valueDeserialization", e);
         }

         m_map = new PropertyHashTab(tobj.getValueCount());

         InstanceFactory factory = null;

         for (PropertyIterator itr = tobj.getIterator(); itr.hasNext();)
         {
            String sName = (String)itr.next();
            Object value = itr.getValue();

            if ((value instanceof List || value instanceof TransferObject) && m_nSafeMode != RAW)
            {
               if (factory == null)
               {
                  factory = new InstanceFactory(context);
               }

               if (m_nSafeMode != FAIL)
               {
                  try
                  {
                     value = factory.instantiate(value);
                  }
                  catch (RuntimeException e)
                  {
                     if (m_nSafeMode == SKIP)
                     {
                        continue;
                     }
                  }
               }
               else
               {
                  value = factory.instantiate(value);
               }
            }

            m_map.put(sName, value);
         }
      }
   }

   /**
    * @see nexj.core.scripting.ScriptedPropertyHolder#invoke(nexj.core.scripting.Symbol, int, nexj.core.scripting.Machine)
    */
   protected Object invoke(Symbol sym, int nArgCount, Machine machine)
   {
      if (nArgCount == 3 && sym.getName().equals(":deserialize"))
      {
         deserializeValues(machine.getArg(1, nArgCount), (InvocationContext)machine.getArg(2, nArgCount));

         return null;
      }

      return super.invoke(sym, nArgCount, machine);
   }

   /**
    * Marshaller factory method.
    * @param context The invocation context.
    * @return The marshaller.
    */
   protected CharacterStreamMarshaller createMarshaller(InvocationContext context)
   {
      return new TextMarshaller(context);
   }

   /**
    * Unmarshaller factory method.
    * @param context The invocation context.
    * @return The unmarshaller.
    */
   protected CharacterStreamUnmarshaller createUnmarshaller(InvocationContext context)
   {
      return new TextUnmarshaller(context);
   }
}
