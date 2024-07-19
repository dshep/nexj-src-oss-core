// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.test.util;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URL;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.xml.XMLMetadata;
import nexj.core.rpc.RPCUtil;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.soap.SOAPMarshaller;
import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.Pair;
import nexj.core.util.HashHolder;
import nexj.core.util.Holder;
import nexj.core.util.IOUtil;
import nexj.core.util.Logger;
import nexj.core.util.PropertyIterator;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.URLUtil;
import nexj.core.util.UncheckedException;
import nexj.core.util.XMLUtil;

/**
 * Test comparison utilities.
 */
public class CmpUtil
{
   // constants

   /**
    * Name of the property specifying the output directory.
    */
   public static String OUT_DIR_PROPERTY = "test.out.dir";
   
   /**
    * Name of the property specifying whether to generate the reference files.
    */
   public static String REF_GENERATE_PROPERTY = "test.ref.generate";

   // associations
   
   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(CmpUtil.class);

   // constructors

   /**
    * Prevents construction.
    */
   protected CmpUtil()
   {
   }

   // operations

   /**
    * Compares an object to its serialized version.
    * Writes the serialized representation of the object
    * to the directory specified in OUT_DIR_PROPERTY.
    * @param url The URL of the reference serialized representation.
    * @param obj The object to serialize.
    * @return True if the serialized representations match. 
    */
   public static boolean equal(URL url, Object obj)
   {
      Reader reader = null;
      Writer writer = null;
      
      try
      {
         StringWriter sw = new StringWriter(4096);
         new SOAPMarshaller(new InvocationContext(
            new XMLMetadata("", null, null, null, null))).serialize(obj, sw);

         String str = XMLUtil.formatXML(sw.toString());
         StringBuffer buf = new StringBuffer(4096);
         boolean bGenerate = StringUtil.parseBoolean(SysUtil.getConfigProperties().getProperty(REF_GENERATE_PROPERTY, "false"));
         
         if (!bGenerate)
         {
            char[] cbuf = new char[4096];
            int nCount;

            reader = new InputStreamReader(URLUtil.openStream(url), XMLUtil.ENCODING);
   
            while ((nCount = reader.read(cbuf)) >= 0)
            {
               buf.append(cbuf, 0, nCount);
            }
         }

         String sOutDir = SysUtil.getConfigProperties().getProperty(OUT_DIR_PROPERTY);

         if (sOutDir != null && sOutDir.length() > 0)
         {
            File file = new File(sOutDir, url.getPath().replace(':', '/'));
            
            file.getParentFile().mkdirs();
            writer = new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(file)), XMLUtil.ENCODING);
            writer.write(str);
         }

         String sRef = buf.toString();
         
         if (sRef.length() > 0)
         {
            sRef = XMLUtil.formatXML(sRef);
         }

         if (bGenerate)
         {
            s_logger.warn("Generation mode - skipped comparison to \"" + url + "\"");
            
            return true;
         }

         if (str.equals(sRef))
         {
            return true;
         }

         s_logger.error("Expected:\n");
         s_logger.error(sRef);
         s_logger.error("----------------\n");
         s_logger.error("Received:\n");
         s_logger.error(str);
         s_logger.dump("Expected:\n");
         s_logger.dump(sRef);
         s_logger.dump("----------------\n");
         s_logger.dump("Received:\n");
         s_logger.dump(str);

         return false;
      }
      catch (IOException e)
      {
         throw new UncheckedException("err.test.io", e);
      } 
      finally
      {
         IOUtil.close(reader);

         if (writer != null)
         {
            try
            {
               writer.close();
            }
            catch (IOException e)
            {
            }
         }
      }
   }
   
   /**
    * Compares an object to its serialized version.
    * Writes the serialized representation of the object
    * to the directory specified in OUT_DIR_PROPERTY.
    * @param url The URL of the reference serialized representation.
    * @param obj The object to serialize.
    * @param attributes The attributes to extract from a transfer object instance list.
    * @return True if the serialized representations match. 
    */
   public static boolean equal(URL url, Object obj, Pair attributes)
   {
      return equal(url, RPCUtil.transfer(obj, attributes, RPCUtil.TF_ALL));
   }
   
   /**
    * Compares an object to its serialized version, ignoring the OIDs.
    * Writes the serialized representation of the object
    * to the directory specified in OUT_DIR_PROPERTY.
    * @param url The URL of the reference serialized representation.
    * @param obj The object to serialize.
    * @param attributes The attributes to extract from a transfer object instance list.
    * @return True if the serialized representations match. 
    */
   public static boolean equalIgnoreOIDs(URL url, Object obj, Pair attributes)
   {
      obj = RPCUtil.transfer(obj, attributes, RPCUtil.TF_ALL);
      removeOIDs(obj, new HashHolder());
      
      return equal(url, obj);
   }
   
   /**
    * Removes the OIDs from the transfer object graph.
    * @param obj The object, which is the entry point to the graph.
    * @param identitySet Set for tracking already visited objects.
    */
   protected static void removeOIDs(Object obj, Holder identitySet)
   {
      if (obj == null || !identitySet.add(obj))
      {
         return;
      }

      if (obj instanceof TransferObject)
      {
         TransferObject tobj = (TransferObject)obj;

         tobj.setOID(null);
         
         for (PropertyIterator itr = tobj.getIterator(); itr.hasNext();)
         {
            itr.next();
            removeOIDs(itr.getValue(), identitySet);
         }
      }
      else if (obj instanceof List)
      {
         List list = (List)obj;
         
         for (Iterator itr = list.iterator(); itr.hasNext();)
         {
            removeOIDs(itr.next(), identitySet);
         }
      }
   }
}
