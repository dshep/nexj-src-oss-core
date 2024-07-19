// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.http;

import java.net.InetSocketAddress;
import java.net.Proxy;
import java.security.AccessController;
import java.security.PrivilegedAction;

import org.w3c.dom.Element;

import nexj.core.meta.Component;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.ChannelType;
import nexj.core.meta.integration.XMLIntegrationMetadataLoader;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.util.CertificateUtil;
import nexj.core.util.XMLUtil;

/**
 * XML HTTP channel metadata loader.
 */
public class XMLHTTPMetadataLoader implements XMLIntegrationMetadataLoader
{
   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataLoader#loadChannel(org.w3c.dom.Element, java.lang.String, nexj.core.meta.integration.ChannelType, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public Channel loadChannel(Element element, String sName, ChannelType type, final XMLMetadataLoader loader)
   {
      final HTTPChannel channel = new HTTPChannel(sName);

      channel.setType(type);
      loader.loadChannel(element, channel);

      channel.setURL(XMLUtil.getStringAttr(element, "url"));
      channel.setAgent(XMLUtil.getStringAttr(element, "agent"));
      channel.setContentType(XMLUtil.getStringAttr(element, "contentType"));
      channel.setDeleteImplemented(XMLUtil.getBooleanAttr(element, "delete", channel.isDeleteImplemented()));
      channel.setGetImplemented(XMLUtil.getBooleanAttr(element, "get", channel.isGetImplemented()));
      channel.setHeadImplemented(XMLUtil.getBooleanAttr(element, "head", channel.isHeadImplemented()));
      channel.setOptionsImplemented(XMLUtil.getBooleanAttr(element, "options", channel.isOptionsImplemented()));
      channel.setPostImplemented(XMLUtil.getBooleanAttr(element, "post", channel.isPostImplemented()));
      channel.setPutImplemented(XMLUtil.getBooleanAttr(element, "put", channel.isPutImplemented()));
      channel.setTraceImplemented(XMLUtil.getBooleanAttr(element, "trace", channel.isTraceImplemented()));
      channel.setReadTimeout(XMLUtil.getIntAttr(element, "readTimeout", channel.getReadTimeout()));
      channel.setConnectionTimeout(XMLUtil.getIntAttr(element, "connectionTimeout", channel.getConnectionTimeout()));

      Boolean binary = XMLUtil.getBooleanObjAttr(element, "binary");

      if (binary != null)
      {
         channel.setDataType((binary.booleanValue()) ? Primitive.BINARY : Primitive.STRING);
      }

      final String sPrivilege = XMLUtil.getStringAttr(element, "privilege");

      if (sPrivilege != null)
      {
         loader.addPrivilegeFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               channel.setPrivilege(loader.getMetadata().getPrimitivePrivilege(sPrivilege));
            }
         });
      }

      final Object errorExpr = loader.getHelper().parse(XMLUtil.getStringAttr(element, "error"),
         false, null, null, loader.getMetadata().getGlobalEnvironment());

      if (errorExpr != null)
      {
         loader.addPostInheritanceMessageFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               channel.setErrorExpression(errorExpr, loader.getMachine());
            }
         });
      }

      return channel;
   }

   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataLoader#loadConnection(org.w3c.dom.Element, nexj.core.meta.integration.Channel, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public void loadConnection(final Element element, Channel channel, XMLMetadataLoader loader)
   {
      final HTTPChannel http = (HTTPChannel)channel;

      if (element != null)
      {
         http.setURL(XMLUtil.getStringAttr(element, "url", http.getURL()));
         http.setMaxRequestSize(XMLUtil.getLongAttr(element, "maxSize", http.getMaxRequestSize()));
         http.setSecure(XMLUtil.getBooleanAttr(element, "secure", http.isSecure()));
         http.setUser(XMLUtil.getStringAttr(element, "user"));
         http.setPassword(loader.decryptPassword(XMLUtil.getStringAttr(element, "password")));
         http.setProxyUser(XMLUtil.getStringAttr(element, "proxyUser"));
         http.setProxyPassword(loader.decryptPassword(XMLUtil.getStringAttr(element, "proxyPassword")));
         http.setTrustedCertificate(XMLUtil.getStringAttr(element, "trust"));
         http.setReadTimeout(XMLUtil.getIntAttr(element, "readTimeout", http.getReadTimeout()));
         http.setConnectionTimeout(XMLUtil.getIntAttr(element, "connectionTimeout", http.getConnectionTimeout()));

         String sAuth = XMLUtil.getStringAttr(element, "authentication", "basic");

         if (sAuth.equals("basic"))
         {
            http.setAuthMode(HTTPChannel.AUTH_BASIC);
         }
         else if (sAuth.equals("proactive"))
         {
            http.setAuthMode(HTTPChannel.AUTH_PROACTIVE);
         }
         else if (sAuth.equals("credential"))
         {
            http.setAuthMode(HTTPChannel.AUTH_CRED);
         }
         else if (sAuth.equals("server"))
         {
            http.setAuthMode(HTTPChannel.AUTH_SERVER);
         }
         else if (sAuth.equals("client"))
         {
            http.setAuthMode(HTTPChannel.AUTH_CLIENT);
         }
         else if (sAuth.equals("certificate"))
         {
            http.setAuthMode(HTTPChannel.AUTH_CERT);

            String sCertificate = XMLUtil.getStringAttr(element, "certificate");

            // Client certificate is not needed when passwords are encrypted.
            if (!loader.getMetadata().isEncrypted())
            {
               try
               {
                  http.setClientCertificate(CertificateUtil.parseKeyStore(sCertificate, http.getPassword()));
               }
               catch (Exception ex)
               {
                  throw new MetadataException("err.meta.integration.http.invalidCertificate",
                        new Object[] {http.getName()}, ex);
               }
            }
         }
         else if (sAuth.equals("none"))
         {
            http.setAuthMode(HTTPChannel.AUTH_NONE);
         }
         else
         {
            throw new MetadataException("err.meta.integration.http.authentication",
               new Object[]{sAuth, channel.getName()});
         }

         final int nProxyPort = XMLUtil.getIntAttr(element, "proxyPort", -1);

         if (nProxyPort >= 0)
         {
            Proxy proxy = new Proxy(Proxy.Type.HTTP, (InetSocketAddress)AccessController.doPrivileged(
               new PrivilegedAction()
               {
                  public Object run()
                  {
                     return InetSocketAddress.createUnresolved(XMLUtil.getStringAttr(element, "proxyHost"), nProxyPort);
                  }
               }
            ));

            http.setProxy(proxy);
         }

         String sProxyAuth = XMLUtil.getStringAttr(element, "proxyAuthentication", "basic");

         if (sProxyAuth.equals("basic"))
         {
            http.setProxyAuthMode(HTTPChannel.AUTH_BASIC);
         }
         else if (sAuth.equals("proactive"))
         {
            http.setProxyAuthMode(HTTPChannel.AUTH_PROACTIVE);
         }
         else if (sProxyAuth.equals("credential"))
         {
            http.setProxyAuthMode(HTTPChannel.AUTH_CRED);
         }
         else if (sProxyAuth.equals("server"))
         {
            http.setProxyAuthMode(HTTPChannel.AUTH_SERVER);
         }
         else
         {
            throw new MetadataException("err.meta.integration.http.proxyAuthentication",
               new Object[]{sProxyAuth, channel.getName()});
         }
      }

      if (http.isEnabled())
      {
         Component component = new Component("HTTPAdapter." + channel.getName(),
            channel.getType().getSender(), Component.CONTEXT);
   
         http.setSender(component);
         http.setReceiver(component);
         component.setMetadata(loader.getMetadata());
         component.addPrimitivePropertyInitializer("channel", channel);
      }
   }
}
