// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.test.tools;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import nexj.core.meta.Repository;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.persistence.Query;
import nexj.core.rpc.InstanceFactory;
import nexj.core.rpc.RPCUtil;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.soap.SOAPMarshaller;
import nexj.core.rpc.soap.SOAPUnmarshaller;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.tools.GenericTool;
import nexj.core.util.IOUtil;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Lookup;
import nexj.core.util.SubstReader;
import nexj.core.util.URLUtil;
import nexj.core.util.XMLUtil;
import nexj.core.util.auth.SimplePrincipal;

/**
 * Data import/export tool.
 */
public class DataTool extends GenericTool
{
   // associations
   
   /**
    * The invocation context.
    */
   protected InvocationContext m_context;
   
   // operations
   
   /**
    * @see nexj.core.tools.GenericTool#execute(java.lang.String)
    */
   protected void execute(String sCommand) throws Exception
   {
      if (sCommand.equals("export"))
      {
         init();
         exportData();
      }
      else if (sCommand.equals("import"))
      {
         init();
         importData();
      }
      else
      {
         throw new IllegalArgumentException("Invalid command \"" + sCommand + "\"");
      }
   }

   /**
    * Initializes the invocation context.
    */
   protected void init()
   {
      m_context = (InvocationContext)Repository.getMetadata().getComponent("System.InvocationContext").getInstance(null);
      m_context.initialize(new SimplePrincipal(getProperty("app.user", "anonymous")));
      m_context.setLocale(Locale.getDefault());
      m_context.setProtected(false);
      m_context.setSecure(false);
   }
   
   /**
    * Exports the data from the system.
    */
   protected void exportData() throws Exception
   {
      String sDataURL = getRequiredProperty("data.url");
      File file = (URLUtil.isURL(sDataURL)) ? new File(new URI(sDataURL)) : new File(sDataURL);
      List resultList = new ArrayList();
      Lookup identityMap = new IdentityHashTab();
      boolean bDeleteFile = false;
      Writer writer = null;
      
      try
      {
         for (int i = 0;; ++i)
         {
            String sQuery = (i == 0) ? getRequiredProperty("data.query") : getProperty("data.query" + i);
            
            if (sQuery == null)
            {
               break;
            }
            
            Pair pair = (Pair)new XMLMetadataHelper().parse(sQuery, true, null, null, m_context.getMachine().getGlobalEnvironment());
            
            if (pair == null || !(pair.getHead() instanceof Symbol))
            {
               throw new IllegalArgumentException("Invalid query class name");
            }
            
            Symbol classSymbol = (Symbol)pair.getHead();

            pair = pair.getNext();
            
            Pair attributes = null;
            Object where = null;
            
            if (pair != null)
            {
               attributes = (Pair)pair.getHead();
               pair = pair.getNext();
               
               if (pair != null)
               {
                  where = pair.getHead();
                  
                  if (pair.getTail() != null)
                  {
                     throw new IllegalArgumentException("Too many query arguments");
                  }
               }
            }
            
            Query query = Query.createRead(m_context.getMetadata().getMetaclass(classSymbol.getName()),
               attributes, where, null, -1, 0, false, Query.SEC_NONE, m_context);

            resultList.addAll((List)RPCUtil.transfer(query.read(), attributes, identityMap, RPCUtil.TF_ALL));
         }

         writer = new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(file)), XMLUtil.ENCODING);

         new SOAPMarshaller(m_context).serialize(resultList, writer);
      }
      catch (Exception e)
      {
         bDeleteFile = true;
         throw e;
      }
      finally
      {
         if (writer != null)
         {
            try
            {
               writer.close();
            }
            catch (IOException e)
            {
            }

            if (bDeleteFile)
            {
               file.delete();
            }
         }
      }
   }
   
   /**
    * Imports the data into the system.
    */
   protected void importData() throws Exception
   {
      String sDataURL = getRequiredProperty("data.url");
      URL url = (URLUtil.isURL(sDataURL)) ? new URL(sDataURL) : new File(sDataURL).toURL();
      Reader reader = new SubstReader(new InputStreamReader(new BufferedInputStream(URLUtil.openStream(url)), XMLUtil.ENCODING))
      {
         protected String getValue(String sName) throws IOException
         {
            return getRequiredProperty("param." + sName);
         }
      };

      try
      {
         List list = (List)new SOAPUnmarshaller(m_context).deserialize(reader);
         InstanceFactory factory = new InstanceFactory(InstanceFactory.STATE, m_context);

         for (int i = 0; i != list.size(); ++i)
         {
            factory.instantiate((TransferObject)list.get(i));
         }

         factory.complete();

         for (Iterator itr = factory.getIdentityMap().valueIterator(); itr.hasNext();)
         {
            Object obj = itr.next();

            if (obj instanceof Instance)
            {
               Instance inst = (Instance)obj;

               if (inst.getOID() != null)
               {
                  if (Query.createRead(inst.getMetaclass(), null, Pair.attribute("").eq(inst.getOID()),
                     null, -1, 0, false, Query.SEC_NONE, m_context).read().isEmpty())
                  {
                     inst.setClean();
                     inst.setNew();
                  }
               }

               inst.setEventPending(false);
            }
         }

         m_context.getUnitOfWork().commit();
      }
      finally
      {
         IOUtil.close(reader);
      }
   }

   /**
    * @see nexj.core.tools.GenericTool#getOptionUsage()
    */
   protected String[] getOptionUsage()
   {
      return new String[]
      {
         "-Dapp.user=User name",
         "-Ddata.url=Location of the data (must be a file when exporting)",
         "-Ddata.query=Read query: class attributes where",
         "-Ddata.query#=Additional read queries",
         "-Dparam.<name>=Specifies a template parameter value"
      };
   }

   /**
    * @see nexj.core.tools.GenericTool#getCommandUsage()
    */
   protected String[] getCommandUsage()
   {
      return new String[]
      {
         "export - Exports the data",
         "import - Imports the data"
      };
   }

   public static void main(String[] args)
   {
      new DataTool().run(args);
   }
}