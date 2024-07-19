// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.mail;

import java.util.Iterator;

import org.w3c.dom.Element;

import nexj.core.meta.Component;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.ChannelType;
import nexj.core.meta.integration.XMLIntegrationMetadataLoader;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.SysUtil;
import nexj.core.util.XMLUtil;

/**
 * XML mail metadata loader.
 */
public class XMLMailMetadataLoader implements XMLIntegrationMetadataLoader
{
   // operations


   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataLoader#loadChannel(org.w3c.dom.Element, java.lang.String, nexj.core.meta.integration.ChannelType, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public Channel loadChannel(Element element, String sName, ChannelType type, final XMLMetadataLoader loader)
   {
      final Mail mail = new Mail(sName);

      mail.setType(type);
      loader.loadChannel(element, mail);
      mail.setAlias(XMLUtil.getStringAttr(element, "alias"));
      mail.setDefaultUser(XMLUtil.getStringAttr(element, "defaultUser", mail.getDefaultUser()));

      final String sQueue = XMLUtil.getStringAttr(element, "queue");

      if (sQueue != null)
      {
         loader.addIOFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               mail.setQueue(loader.getMetadata().getChannel(sQueue));
            }
         });
      }

      if (mail.getAlias() == null)
      {
         mail.setAlias(XMLMetadataHelper.makeAlias(mail.getName()));
      }

      boolean bFirstMailChannel = true;

      for (Iterator itr = loader.getMetadata().getChannelIterator(); itr.hasNext() && bFirstMailChannel; )
      {
         Channel channel = (Channel)itr.next();

         if (channel instanceof Mail)
         {
            bFirstMailChannel = false;
         }
      }

      if (bFirstMailChannel)
      {
         loader.addIOFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               Lookup aliasMailMap = new HashTab();

               for (Iterator itr = loader.getMetadata().getChannelIterator(); itr.hasNext(); )
               {
                  Channel channel = (Channel)itr.next();

                  if (channel instanceof Mail)
                  {
                     Mail mailChannel = (Mail)channel;
                     Mail oldMailChannel = (Mail)aliasMailMap.put(mailChannel.getAlias(), mailChannel);

                     if (oldMailChannel == null)
                     {
                        mailChannel.setFirst(true);
                     }
                     else
                     {
                        mailChannel.setFirst(false);
                        aliasMailMap.put(mailChannel.getAlias(), oldMailChannel);

                        MetadataValidationException e = new MetadataValidationException(
                           "err.meta.mail.duplicateAlias", new Object[]{mailChannel.getName(), oldMailChannel.getName()});

                        mailChannel.setProperties(e);

                        if (loader.getHelper().getWarnings() != null)
                        {
                           loader.getHelper().getWarnings().addException(e);
                        }
                     }
                  }
               }
            }
         });
      }

      return mail;
   }

   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataLoader#loadConnection(org.w3c.dom.Element, nexj.core.meta.integration.Channel, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public void loadConnection(Element element, Channel channel, final XMLMetadataLoader loader)
   {
      final Mail mail = (Mail)channel;

      if (element != null)
      {
         mail.setAlias(XMLUtil.getStringAttr(element, "alias", mail.getAlias()));
         mail.setUser(XMLUtil.getStringAttr(element, "user"));
         mail.setPassword(loader.decryptPassword(XMLUtil.getStringAttr(element, "password", mail.getPassword())));
         mail.setInHost(XMLUtil.getStringAttr(element, "inHost"));
         mail.setInPort(XMLUtil.getIntAttr(element, "inPort", mail.getInPort()));
         mail.setInProtocol(XMLUtil.getStringAttr(element, "inProtocol", mail.getInProtocol()));
         mail.setInFolder(XMLUtil.getStringAttr(element, "inFolder", mail.getInFolder()));
         mail.setOutHost(XMLUtil.getStringAttr(element, "outHost"));
         mail.setOutPort(XMLUtil.getIntAttr(element, "outPort", mail.getOutPort()));
         mail.setOutProtocol(XMLUtil.getStringAttr(element, "outProtocol", mail.getOutProtocol()));
         mail.setFrom(XMLUtil.getStringAttr(element, "from"));
         loader.loadProperties(element, "Properties", mail.getPropertyHolder());

         String sInEnc = XMLUtil.getStringAttr(element, "inEncryption");

         if ("none".equalsIgnoreCase(sInEnc))
         {
            mail.setInEncryption(Mail.ENCRYPTION_NONE);
         }
         else if ("SSL".equalsIgnoreCase(sInEnc))
         {
            mail.setInEncryption(Mail.ENCRYPTION_SSL);
         }
         else if ("TLS".equalsIgnoreCase(sInEnc))
         {
            mail.setInEncryption(Mail.ENCRYPTION_TLS);
         }
         else if (sInEnc != null) // leave defaults as is
         {
            throw new MetadataException("err.meta.integration.mail.inEncryption",
                                        new Object[]{sInEnc, channel.getName()});
         }

         String sOutAuth = XMLUtil.getStringAttr(element, "outAuthentication");

         if ("none".equalsIgnoreCase(sOutAuth))
         {
            mail.setOutAuth(Mail.OUTAUTH_NONE);
         }
         else if ("receive".equalsIgnoreCase(sOutAuth))
         {
            mail.setOutAuth(Mail.OUTAUTH_INFIRST);
         }
         else if ("basic".equalsIgnoreCase(sOutAuth))
         {
            mail.setOutAuth(Mail.OUTAUTH_CREDENTIAL);
         }
         else if (sOutAuth != null) // leave defaults as is
         {
            throw new MetadataException("err.meta.integration.mail.outAuth",
                                        new Object[]{sOutAuth, channel.getName()});
         }

         String sOutEnc = XMLUtil.getStringAttr(element, "outEncryption");

         if ("none".equalsIgnoreCase(sOutEnc))
         {
            mail.setOutEncryption(Mail.ENCRYPTION_NONE);
         }
         else if ("SSL".equalsIgnoreCase(sOutEnc))
         {
            mail.setOutEncryption(Mail.ENCRYPTION_SSL);
         }
         else if ("TLS".equalsIgnoreCase(sOutEnc))
         {
            mail.setOutEncryption(Mail.ENCRYPTION_TLS);
         }
         else if (sOutEnc != null) // leave defaults as is
         {
            throw new MetadataException("err.meta.integration.mail.outEncryption",
                                        new Object[]{sOutEnc, channel.getName()});
         }
      }

      if (mail.getInHost() == null)
      {
         mail.setReceivable(false);
      }
      
      if (mail.getOutHost() == null)
      {
         mail.setSendable(false);
      }

      mail.setDefaultProperties();

      Component factory = new Component(
            "Mail.ConnectionFactory." + channel.getName(),
            loader.getHelper().getClassObject(
               SysUtil.PACKAGE + ".core.rpc.mail.MailConnectionFactoryLocator"),
            Component.SINGLETON);

      factory.addPrimitivePropertyInitializer("properties",
                                              mail.getPropertyHolder().getProperties());
      factory.addPrimitivePropertyInitializer("dataSource", mail.getName());
      factory.addPrimitivePropertyInitializer("password", mail.getPassword());
      factory.addPrimitivePropertyInitializer("user", mail.getUser());

      if (mail.isSendable())
      {
         Component component = new Component("MailSender." + channel.getName(),
                                             channel.getType().getSender(),
                                             Component.SINGLETON);

         component.addPrimitivePropertyInitializer("channel", mail);
         component.addComponentPropertyInitializer("connectionFactory", factory);
         loader.getMetadata().addComponent(component);
         loader.addSingletonFixup(component);
         mail.setSender(component);
      }

      if (mail.isReceivable())
      {
         Component component = new Component("MailReceiver." + channel.getName(),
                                             channel.getType().getReceiver(),
                                             Component.SINGLETON);

         component.addPrimitivePropertyInitializer("channel", mail);
         component.addComponentPropertyInitializer("connectionFactory", factory);
         loader.getMetadata().addComponent(component);
         loader.addSingletonFixup(component);
         mail.setReceiver(component);
      }
   }
}
