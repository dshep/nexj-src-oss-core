// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.build;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.taskdefs.XSLTProcess;
import org.apache.tools.ant.taskdefs.XSLTProcess.Param;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

import nexj.core.util.Base64Util;
import nexj.core.util.IOUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.XMLUtil;
import nexj.core.util.cipher.CharacterStreamCipher;
import nexj.core.util.cipher.CharacterStreamCipherDispatcher;
import nexj.core.util.cipher.CharacterStreamCipherDispatcher.DropPrefixWriter;

/**
 * This is the <xmlpropertydecryptor .../> task for Ant.
 * It has two modes of operation:
 *
 * 1) Read properties
 * In this mode, properties are read into the project from the file
 * specified in the "file" task parameter. The properties will be read
 * from the attributes of the root element of the specified XML
 * file. The properties will be named "RootElement(AttributeName)".
 * If the "prefix" task parameter is specified, the properties will
 * be named "Prefix(AttributeName)". The "style" attribute may be
 * specified if the document should be transformed before its properties
 * are read.
 *
 * 2) Transform file
 * In this mode, the file specified in the "file" task parameter is
 * transformed by an XSLT stylesheet and copied to the file specified in the
 * "tofile" task parameter. No properties are read into memory or added to
 * the current project. The "prefix" task parameter has no effect. The file
 * containing the stylesheet is specified by the "style" task parameter.
 * Named parameters may be passed to the XSLT transformation process through
 * the use of "param" tags, as is done in the standard Ant task "xslt":
 *    <param name="param name" expression="param value" if="..." unless="..."/>
 *
 * The "force" task parameter may be set to "true" to force the output file
 * to be updated, even if no changes should be necessary (as determined by
 * the input, output, and stylesheet file timestamps).
 */
public class XMLPropertyDecryptor extends Task
{
   // attributes

   /**
    * The prefix to use for properties read by this task.
    */
   protected String m_sPrefix;

   /**
    * Forces the output file to be updated, even if no changes should be necessary.
    */
   protected boolean m_bForce;

   /**
    * True if the keystore properties are to be included.
    */
   protected boolean m_bKeystoreEnabled;

   // associations

   /**
    * The file to read.
    */
   protected File m_propFile;

   /**
    * The file to write if injecting new properties; null if
    * reading properties.
    */
   protected File m_outFile;

   /**
    * The properties to inject into the file.
    */
   protected List m_paramList;

   /**
    * The file containing the stylesheet to apply; null if
    * no transformation is being done.
    */
   protected File m_styleFile;

   /**
    * The file containing the cluster keystore; null if a cluster keystore is not availabe.
    */
   protected File m_keystoreFile;


   // operations

   /**
    * @see org.apache.tools.ant.Task#execute()
    */
   public void execute() throws BuildException
   {
      // Validate attributes
      if (m_propFile == null)
      {
         throw new BuildException("Undefined file attribute of " + getTaskName());
      }

      InputStream fis = null;
      Reader encReader = null;
      FileOutputStream fos = null;
      OutputStreamWriter encWriter = null;

      if (!m_bKeystoreEnabled)
      {
         m_keystoreFile = null;
      }

      try
      {
         // Skip everything if outputting and no rebuild is required.
         if (m_outFile != null)
         {
            if (!m_bForce &&
               !(m_propFile.lastModified() > m_outFile.lastModified()) &&
               !(m_styleFile != null && (m_styleFile.lastModified() > m_outFile.lastModified())) &&
               !(m_keystoreFile != null && (m_keystoreFile.lastModified() > m_outFile.lastModified())))
            {
               return;
            }
         }

         // Configure decryption (system properties as defaults, then ant properties)
         CharacterStreamCipherDispatcher dispatcher = new CharacterStreamCipherDispatcher();
         Properties props = new Properties(SysUtil.getConfigProperties());

         props.putAll(getProject().getProperties());
         dispatcher.init(props);

         // Process file
         try
         {
            if (m_keystoreFile != null)
            {
               boolean bHasKeyStore = false;
               Param param = null;

               if (m_paramList != null)
               {
                  for (Iterator paramItr = m_paramList.iterator(); paramItr.hasNext();)
                  {
                     param = (Param)paramItr.next();

                     if (param.getName().equals("clusterKeyStore") && param.shouldUse())
                     {
                        bHasKeyStore = true;

                        break;
                     }
                  }
               }
               
               if (!bHasKeyStore)
               {
                  param = new Param();
                  fis = new BufferedInputStream(new FileInputStream(m_keystoreFile));

                  ByteArrayOutputStream dataStream = new ByteArrayOutputStream();

                  IOUtil.copy(dataStream, fis);
                  param.setName("clusterKeyStore");
                  param.setExpression(Base64Util.encode(dataStream.toByteArray()));
                  addConfiguredParam(param);

                  IOUtil.close(fis);
               }
            }

            fis = new BufferedInputStream(new FileInputStream(m_propFile));
            encReader = new InputStreamReader(fis, "UTF-8");

            Object cipher = dispatcher.create(encReader);
            Reader decReader = null;

            if (cipher instanceof CharacterStreamCipher)
            {
               decReader = ((CharacterStreamCipher)cipher).createDecryptedReader(encReader);
            }
            else
            {
               IOUtil.close(encReader);
               IOUtil.close(fis);
               fis = null;

               encReader = new StringReader((String)cipher);
               decReader = encReader;
               cipher = dispatcher;

               // The file was not encrypted at all
               props.setProperty("cipher.scheme",
                  props.getProperty("cipher.scheme", CharacterStreamCipherDispatcher.SCHEME_TEXT));
               dispatcher.init(props);
            }

            // Read from disk, applying XSLT transformation if necessary
            Document doc;

            if (m_styleFile != null)
            {
               TransformerFactory factory = (TransformerFactory)Class.forName(
               "org.apache.xalan.processor.TransformerFactoryImpl").newInstance();

               FileInputStream styleStream = new FileInputStream(m_styleFile);
               Transformer transformer = factory.newTransformer(new StreamSource(styleStream));

               // Read and set the transformation parameters
               if (m_paramList != null)
               {
                  for (int i = 0; i < m_paramList.size(); i++)
                  {
                     XSLTProcess.Param param = (XSLTProcess.Param)m_paramList.get(i);

                     if (param.shouldUse())
                     {
                        transformer.setParameter(param.getName(), param.getExpression());
                     }
                  }
               }

               Source source = new StreamSource(decReader);
               DOMResult result = new DOMResult();

               transformer.transform(source, result);
               doc = (Document)result.getNode();
            }
            else
            {
               doc = XMLUtil.parse(decReader);
            }

            Element root = XMLUtil.findFirstElement(doc.getFirstChild());
            String sRootName = root.getNodeName();
            NamedNodeMap attributes = root.getAttributes();

            if (m_outFile == null)
            {
               String sPrefix = (m_sPrefix != null) ? m_sPrefix : (sRootName + '.');

               // Read properties
               for (int i = 0; i < attributes.getLength(); i++)
               {
                  Node attribute = attributes.item(i);
                  StringBuilder nameBuilder = new StringBuilder();

                  nameBuilder.append(sPrefix);
                  nameBuilder.append(attribute.getNodeName());

                  getProject().setProperty(nameBuilder.toString(), dispatcher.decrypt(attribute.getNodeValue()));
               }
            }
            else
            {
               fos = new FileOutputStream(m_outFile);
               encWriter = new OutputStreamWriter(fos, "UTF-8");

               Writer decWriter = ((CharacterStreamCipher)cipher).createEncryptedWriter(
                  new DropPrefixWriter(encWriter, "text:"));

               XMLUtil.formatXML(new DOMSource(doc), false, decWriter);
               decWriter.close();
            }
         }
         catch (IOException ex)
         {
            throw new BuildException("Error processing property file in " + getTaskName(), ex);
         }
         finally
         {
            IOUtil.close(encReader);
            IOUtil.close(fis);

            if (encWriter != null)
            {
               encWriter.close();
            }

            if (fos != null)
            {
               fos.close();
            }
         }
      }
      catch (Throwable t)
      {
         t.printStackTrace(System.err);
         throw new BuildException("Error processing property file", t);
      }
   }

   /**
    * Sets the encrypted XML file containing properties.
    *
    * @param propFile The encrypted XML file containing properties.
    */
   public void setFile(File propFile)
   {
      m_propFile = propFile;
   }

   /**
    * Sets the encrypted XML file for output.
    *
    * @param outFile The encrypted XML file for output.
    */
   public void setTofile(File outFile)
   {
      m_outFile = outFile;
   }

   /**
    * Sets the file containing the cluster keystore.
    *
    * @param keystoreFile The file containing the cluster keystore.
    */
   public void setKeystoreFile(File keystoreFile)
   {
      m_keystoreFile = keystoreFile;
   }

   /**
    * Sets the prefix to use for properties loaded into the current project.
    *
    * @param sPrefix The prefix to use for properties loaded into the current
    *                project.
    */
   public void setPrefix(String sPrefix)
   {
      if (sPrefix == null || sPrefix.length() == 0)
      {
         m_sPrefix = null;
      }
      else if (sPrefix.charAt(sPrefix.length() - 1) != '.')
      {
         m_sPrefix = sPrefix + '.';
      }
      else
      {
         m_sPrefix = sPrefix;
      }
   }

   /**
    * Sets the file containing the stylesheet to apply to the document, if any.
    *
    * @param styleFile The file containing the stylesheet to apply.
    */
   public void setStyle(File styleFile)
   {
      m_styleFile = styleFile;
   }

   /**
    * Sets the force flag.
    *
    * @param bForce True to always write to the output file; false to write only
    *               when the timestamps indicate that an update may be necessary.
    */
   public void setForce(boolean bForce)
   {
      m_bForce = bForce;
   }

   /**
    * Adds the representation of a "param" tag.
    *
    * @param aParam The "param" tag that contains information about a
    *               parameter to set when copying to an output file.
    */
   public void addConfiguredParam(XSLTProcess.Param aParam)
   {
      if (m_paramList == null)
      {
         m_paramList = new ArrayList();
      }

      if (aParam.getName().equals("clusterPassword") && aParam.shouldUse())
      {
         m_bKeystoreEnabled = true;
      }

      m_paramList.add(aParam);
   }
}
