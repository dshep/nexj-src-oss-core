// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.text;

import java.io.IOException;

import nexj.core.runtime.Context;
import nexj.core.util.Lookup;

/**
 * Text marshaller which places all the strings into the reference table.
 */
public class RefTextMarshaller extends TextMarshaller
{
   // constants
   
   /**
    * The marshaller map, class to marshaller implementation.
    */
   protected final static Lookup s_mshMap; // of type Marshaller[Class]

   static
   {
      s_mshMap = (Lookup)TextMarshaller.s_mshMap.clone();

      s_mshMap.put(String.class, new Marshaller()
      {
         public void marshal(Object obj, TextMarshaller msh) throws IOException
         {
            String s = (String)obj;
            msh.writePrefix(s.length(), Text.STRING_OBJECT);
            msh.write(s);
         }

         public boolean isPrimitive()
         {
            return false;
         }
      });
   }
   
   // constructors

   /**
    * Creates a metadata text marshaller with a given runtime context.
    * @param context The runtime context.
    */
   public RefTextMarshaller(Context context)
   {
      super(context);
   }

   // operations

   /**
    * @see nexj.core.rpc.text.TextMarshaller#getMarshallerMap()
    */
   protected Lookup getMarshallerMap()
   {
      return s_mshMap;
   }
}
