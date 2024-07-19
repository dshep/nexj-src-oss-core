// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

import nexj.core.meta.Repository;
import nexj.core.persistence.OID;
import nexj.core.rpc.MarshallerException;
import nexj.core.rpc.Request;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.soap.SOAPMarshaller;
import nexj.core.rpc.soap.SOAPUnmarshaller;
import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.Pair;

public class XMLMarshallerProfiler
{   
   public static void profileMarshaller() throws IOException
   {
      Request req = new Request();
      List instanceList = new ArrayList();
      int nIterations = 0xffff;
      int nWarmup = nIterations * 4; // 80%
      int nSizeMultiplier = 5; // ~50k chars
      
      for (int i = 0; i < nSizeMultiplier; ++i)
      {
         // create argument transfer object
         req.addInvocation(sizedTransferObject("arg", i, nSizeMultiplier), new Pair("pair" + i));

         // create filter transfer object
         // (don't reuse arg because SOAP can map same object to multiple places,
         //  XML can't and hence skews results)
         req.addFilter(sizedTransferObject("filter", i, nSizeMultiplier));

         // create instance transfer object
         // (don't reuse arg because SOAP can map same object to multiple places,
         //  XML can't and hence skews results)
         instanceList.add(sizedTransferObject("instance", i, nSizeMultiplier));
      }

      TransferObject instancesFilter = new TransferObject();
      instancesFilter.setValue("instances", instanceList);
      req.addFilter(instancesFilter);

      final long[] counterArray = new long[1];
      Writer writer = new Writer() {
         public void close() {}
         public void flush() {}
         public void write(char[] cbuf, int off, int len) { counterArray[0] += len; }
         public void write(int c) { ++counterArray[0]; }
         public void write(String str, int off, int len) { counterArray[0] += len; }
      };
      
      System.err.println("XML vs. SOAP Marshaller Profiling Comparison");

      // --- XML test

      SOAPMarshaller marshaller =
         new XMLMarshaller(new InvocationContext(Repository.getMetadata()),
                           Repository.getMetadata());

      for (int i = 0; i < nWarmup; ++i)
      {
         marshaller.serialize(req, writer);
      }

      long lStartTime = System.currentTimeMillis();
      
      for (int i = 0; i < nIterations; ++i)
      {
         counterArray[0] = 0;
         marshaller.serialize(req, writer);
      }

      long lEndTime = System.currentTimeMillis();
      long nSizeXML = counterArray[0];
      long nTotalTimeXML = lEndTime - lStartTime;

      System.err.println("Run: " + nIterations + " iterations in: " + nTotalTimeXML/(double)1000 +
                         " sec");
      System.err.println("XML Time: " + nTotalTimeXML/(double)nIterations + " ms");
      System.err.println("XML Size: " + nSizeXML + " chars");
      System.err.println("XML Unit: " + nTotalTimeXML/(double)nIterations/nSizeXML * 1000000 +
                         " ns/char");

      //--- SOAP test

      marshaller = new SOAPMarshaller(new InvocationContext(Repository.getMetadata()));

      for (int i = 0; i < nWarmup; ++i)
      {
         marshaller.serialize(req, writer);
      }

      lStartTime = System.currentTimeMillis();
      
      for (int i = 0; i < nIterations; ++i)
      {
         counterArray[0] = 0;
         marshaller.serialize(req, writer);
      }

      lEndTime = System.currentTimeMillis();
      long nSizeSOAP = counterArray[0];
      long nTotalTimeSOAP = lEndTime - lStartTime;

      System.err.println("Run: " + nIterations + " iterations in: " + nTotalTimeSOAP/(double)1000 +
                         " sec");
      System.err.println("SOAP Time: " + nTotalTimeSOAP/(double)nIterations + " ns");
      System.err.println("SOAP Size: " + nSizeSOAP + " chars");
      System.err.println("SOAP Unit: " + nTotalTimeSOAP/(double)nIterations/nSizeSOAP * 1000000 +
                         " ns/char");
      
      System.err.println("Time Ratio: " + nTotalTimeXML/(double)nTotalTimeSOAP);
      System.err.println("Size Ratio: " + nSizeXML/(double)nSizeSOAP);
   }
      
   public static void profileUnmarshaller() throws MarshallerException, IOException
   {
      Request req = new Request();
      List instanceList = new ArrayList();
      int nIterations = 0xffff;
      int nWarmup = nIterations * 4; // 80%
      int nSizeMultiplier = 5; // ~50k chars
      
      for (int i = 0; i < nSizeMultiplier; ++i)
      {
         // create argument transfer object
         req.addInvocation(sizedTransferObject("arg", i, nSizeMultiplier), new Pair("pair" + i));

         // create filter transfer object 
         // (don't reuse arg because SOAP can map same object to multiple places,
         //  XML can't and hence scews results)
         req.addFilter(sizedTransferObject("filter", i, nSizeMultiplier));

         // create instance transfer object
         // (don't reuse arg because SOAP can map same object to multiple places,
         //  XML can't and hence skews results)
         instanceList.add(sizedTransferObject("instance", i, nSizeMultiplier));
      }

      TransferObject instancesFilter = new TransferObject();
      instancesFilter.setValue("instances", instanceList);
      req.addFilter(instancesFilter);

      StringWriter xmlOut = new StringWriter();
      StringWriter soapOut = new StringWriter();
      
      new XMLMarshaller(new InvocationContext(Repository.getMetadata()), Repository.getMetadata())
             .serialize(req, xmlOut);
      new SOAPMarshaller(new InvocationContext(Repository.getMetadata())).serialize(req, soapOut);

      System.err.println("XML vs. SOAP Unmarshaller Profiling Comparison");

      // --- XML test

      SOAPUnmarshaller unmarshaller = 
         new XMLUnmarshaller(new InvocationContext(Repository.getMetadata()),
                             Repository.getMetadata());
      
      for (int i = 0; i < nWarmup; ++i)
      { 
         unmarshaller.deserialize(new StringReader(xmlOut.toString()));
      }
      
      long lStartTime = System.currentTimeMillis();
      
      for (int i = 0; i < nIterations; ++i)
      {
         unmarshaller.deserialize(new StringReader(xmlOut.toString()));
      }

      long lEndTime = System.currentTimeMillis();      
      long nSizeXML = xmlOut.toString().length();
      long nTotalTimeXML = lEndTime - lStartTime;

      System.err.println("Run: " + nIterations + " iterations in: " + nTotalTimeXML/(double)1000 +
                         " sec");
      System.err.println("XML Time: " + nTotalTimeXML/(double)nIterations + " ms");
      System.err.println("XML Size: " + nSizeXML + " chars");
      System.err.println("XML Unit: " + nTotalTimeXML/(double)nIterations/nSizeXML * 1000000 +
                         " ns/char");

      //--- SOAP test
 
      unmarshaller = new SOAPUnmarshaller(new InvocationContext(Repository.getMetadata()));
      
      for (int i = 0; i < nWarmup; ++i)
      { 
         unmarshaller.deserialize(new StringReader(soapOut.toString()));
      }

      lStartTime = System.currentTimeMillis();
      
      for (int i = 0; i < nIterations; ++i)
      {
         unmarshaller.deserialize(new StringReader(soapOut.toString()));
      }

      lEndTime = System.currentTimeMillis();
      long nSizeSOAP = soapOut.toString().length();
      long nTotalTimeSOAP = lEndTime - lStartTime;

      System.err.println("Run: " + nIterations + " iterations in: " + nTotalTimeSOAP/(double)1000 +
                         " sec");
      System.err.println("SOAP Time: " + nTotalTimeSOAP/(double)nIterations + " ms");
      System.err.println("SOAP Size: " + nSizeSOAP + " chars");
      System.err.println("SOAP Unit: " + nTotalTimeSOAP/(double)nIterations/nSizeSOAP * 1000000 + 
                         " ns/char");
      
      System.err.println("Time Ratio: " + nTotalTimeXML/(double)nTotalTimeSOAP);
      System.err.println("Size Ratio: " + nSizeXML/(double)nSizeSOAP);
   }
   
   private static TransferObject sizedTransferObject(String sName, int nSuffix, int nSizeMultiplier)
   {
      // create argument transfer object
      TransferObject tobj = new TransferObject(sName);
      
      tobj.setEventName("test" + nSuffix);
      tobj.setOID(new OID(new Object[] { "oid" + nSuffix }));
      tobj.setVersion((short)nSuffix);
      
      for (int i = 0; i < nSizeMultiplier; ++i)
      {
         List l = new ArrayList();
         tobj.setValue("valueA" + i, l);
         tobj.setValue("valueL" + i, new Long(i));
         tobj.setValue("valueS" + i, "value" + i);
         
         for (int j = 0; j < nSizeMultiplier; ++j)
         {
            l.add(j + ":" + System.currentTimeMillis());
         }
      }
      
      return tobj;
   }  

   public static void main(String[] args) throws Exception
   {
      Repository.getMetadata();
      profileMarshaller();
      System.err.println();
      profileUnmarshaller();
   }
}
