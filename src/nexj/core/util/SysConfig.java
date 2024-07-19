// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.Properties;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import nexj.core.util.cipher.CharacterStreamCipherDispatcher;

/**
 * Implements configuration functionality that is not part of the minimal
 * scripting engine.
 */
public class SysConfig
{
   // operations

   /**
    * Load a configuration file and apply properties defined to the given
    * properties.
    * 
    * Warning: called from static initializer
    * 
    * @param sCfgFileName The file name property
    * @param properties (Output) The properties to load into
    */
   public static void load(String sCfgFileName, final Properties properties)
   {
      // Read the config properties file
      InputStream istream = null;
      Reader decReader = null;
      Reader encReader = null;

      try
      {
         if (URLUtil.isURL(sCfgFileName))
         {
            istream = URLUtil.openStream(new URL(sCfgFileName));
         }
         else
         {
            if (sCfgFileName.charAt(0) != '/')
            {
               sCfgFileName = '/' + sCfgFileName;
            }

            istream = URLUtil.openResource(SysUtil.class, sCfgFileName);
         }

         // Decrypt .server file to read properties
         encReader = new InputStreamReader(new BufferedInputStream(istream), IOUtil.ENCODING);
         CharacterStreamCipherDispatcher dispatcher = new CharacterStreamCipherDispatcher();

         dispatcher.init(properties);
         decReader = dispatcher.createDecryptedReader(encReader);

         XMLUtil.parse(decReader, new DefaultHandler()
         {
            protected boolean m_bProcessed;

            /**
             * @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String,
             *      java.lang.String, java.lang.String, org.xml.sax.Attributes)
             */
            public void startElement(String sNamespace, String sLocalName, String sQName, Attributes attrs) throws SAXException
            {
               if (!m_bProcessed)
               {
                  for (int i = 0; i != attrs.getLength(); ++i)
                  {
                     String sName = attrs.getLocalName(i);
                     String sOldValue = properties.getProperty(sName);

                     if (sOldValue == null || sOldValue.length() == 0)
                     {
                        properties.setProperty(sName, attrs.getValue(i));
                     }
                  }

                  m_bProcessed = true;
               }
            }
         });
      }
      catch (Exception e)
      {
         throw new UncheckedException("err.sys.configOpen", new Object[]
         {
            sCfgFileName
         }, e);
      }
      finally
      {
         IOUtil.close(decReader);
         IOUtil.close(encReader);
         IOUtil.close(istream);
      }
   }
}
