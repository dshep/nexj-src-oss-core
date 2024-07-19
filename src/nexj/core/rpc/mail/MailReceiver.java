// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.mail;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.mail.Address;
import javax.mail.Flags;
import javax.mail.Folder;
import javax.mail.Header;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.Part;
import javax.mail.Store;
import javax.mail.internet.InternetAddress;
import javax.resource.ResourceException;

import nexj.core.integration.CompoundIntegrationException;
import nexj.core.integration.Receiver;
import nexj.core.integration.Sender;
import nexj.core.meta.PropertyHolder;
import nexj.core.rpc.ServerException;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Initializable;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.util.Binary;
import nexj.core.util.IOUtil;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.auth.SimplePrincipal;

public class MailReceiver extends Receiver implements Initializable
{
   // attributes

   /**
    * Pattern for extracting the charset from a content-type value.
    */
   protected final static Pattern CHARSET_PATTERN = Pattern.compile(
      ".*?\\bcharset\\s*=\\s*\"?([^\\s>/\"]+).*?",
      Pattern.CASE_INSENSITIVE | Pattern.DOTALL);

   /**
    * The default character set to use for decoding the messages.
    */
   protected final static String DEFAULT_CHARSET = "ISO8859_1";

   // associations

   /**
    * The channel metadata object.
    */
   protected nexj.core.meta.integration.channel.mail.Mail m_channel;

   /**
    * The Mail connection factory.
    */
   protected MailConnectionFactoryLocator m_factory;

   /**
    * The receiver logger.
    */
   protected Logger m_logger;

   // operations

   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      m_logger = m_channel.getLogger();
   }

   /**
    * Dumps a message to a string builder.
    * @param buf The destination buffer.
    * @param msg The message to dump.
    * @returns The 'buf' argument.
    * @throws MessagingException if a message access error occurs.
    */
   protected static StringBuilder dump(StringBuilder buf, Message msg) throws MessagingException
   {
      buf.append("Message [").append(msg.getMessageNumber() - 1).append(']');
      buf.append(" from:");

      Address[] from = msg.getFrom();

      for (int i = 0; i < from.length; ++i)
      {
         buf.append(' ');
         buf.append(((InternetAddress)from[i]).getAddress());
      }

      buf.append(", to:");

      Address[] to = msg.getRecipients(Message.RecipientType.TO);

      for (int i = 0; i < to.length; ++i)
      {
         buf.append(' ');
         buf.append(((InternetAddress)to[i]).getAddress());
      }

      buf.append(", subject: \"");
      buf.append(msg.getSubject());
      buf.append("\", sent: ");
      buf.append(msg.getSentDate());

      return buf;
   }

   /**
    * Parse a Mail message into a TransferObject.
    * @param msg The message to parse (not null).
    * @return The TransferObject representing the message.
    * @throws MessagingException On message parsing error.
    */
   protected TransferObject parse(Message msg) throws MessagingException
   {
      assert msg != null;

      TransferObject content;
      Timestamp receivedDate = toTimestamp(msg.getReceivedDate());

      if (receivedDate == null)
      {
         receivedDate = new Timestamp(System.currentTimeMillis()); // received now if unset
      }

      try
      {
         content = toTransferObject(msg);
      }
      catch (IOException e)
      {
         throw new MessagingException("I/O error", e);
      }

      content.setValue(Mail.FROM, toAddress(msg.getFrom()));
      content.setValue(Mail.TO, toAddress(msg.getRecipients(Message.RecipientType.TO)));
      content.setValue(Mail.CC, toAddress(msg.getRecipients(Message.RecipientType.CC)));
      content.setValue(Mail.BCC, toAddress(msg.getRecipients(Message.RecipientType.BCC)));
      content.setValue(Mail.REPLY, toAddress(msg.getReplyTo()));
      content.setValue(Mail.SUBJECT, msg.getSubject());
      content.setValue(Mail.DATE_SENT, toTimestamp(msg.getSentDate()));
      content.setValue(Mail.DATE_RECEIVED, receivedDate);

      return content;
   }

   /**
    * Polls the server for new messages and invokes the configured server components.
    */
   public void poll()
   {
      if (!m_channel.isReceivable())
      {
         m_logger.debug("The mail client is disabled, skipping poll request.");

         return;
      }

      InvocationContext context = (InvocationContext)ThreadContextHolder.getContext();
      Instance oldUser = context.getUser();
      String sOldAddress = context.getClientAddress();
      int nCookie = -1;

      try
      {
         String sAddress = m_channel.getInHost();
         String sUser = m_channel.getDefaultUser();

         context.setClientAddress(sAddress);

         if (sAddress != null)
         {
            nCookie = Logger.pushContext(sAddress);
         }

         context.login((sUser != null) ? new SimplePrincipal(sUser) : null);

         int nUserCookie = Logger.pushContext(context.getPrincipal().getName());

         if (nCookie == -1)
         {
            nCookie = nUserCookie;
         }

         if (m_logger.isDebugEnabled())
         {
            m_logger.debug("Received a Mail message from " +
               context.getPrincipal().getName() + ((sAddress == null) ? "" : " @ " + sAddress) +
               " on channel \"" + m_channel.getName() + "\"");
         }

         poll(context);

         if (m_logger.isDebugEnabled())
         {
            m_logger.debug("Completed processing the Mail message");
         }
      }
      catch (Throwable e)
      {
         int nLevel = (ObjUtil.isError(e)) ? Logger.ERROR : Logger.DEBUG;

         m_logger.log(nLevel, "Error processing the Mail message", e);

         throw new ServerException("err.rpc.mail", e);
      }
      finally
      {
         if (nCookie != -1)
         {
            Logger.resetContext(nCookie);
         }

         context.setClientAddress(sOldAddress);
         context.login(oldUser);
      }
   }

   /**
    * Poll the mail channel for new messages and process them via 'context'.
    * @param context The context used when processing the messages.
    * @throws MessagingException On message read error.
    * @throws ResourceException On connection open failure.
    */
   protected void poll(InvocationContext context) throws MessagingException, ResourceException
   {
      MailConnection con = null;
      Folder folder = null;
      boolean bFolderOpen = false;

      if (m_channel.getQueue() == null && !isBound(m_channel, context))
      {
         return; // NOOP, no place to send any potential messages
      }

      try
      {
         con = m_factory.openConnection(null);

         Store store = con.getStore();

         folder = store.getDefaultFolder(); // get root folder
         folder = folder.getFolder(m_channel.getInFolder());
         folder.open(Folder.READ_WRITE);
         bFolderOpen = true;

         int nMessageCount = folder.getMessageCount();
         CompoundIntegrationException e = null;

         if (m_logger.isDebugEnabled())
         {
            if (nMessageCount != 0)
            {
               m_logger.debug("Receiving " + nMessageCount + " mail message(s)");
            }
         }

         for (int nMessage = 1; nMessage <= nMessageCount; ++nMessage)
         {
            try
            {
               process(context, folder.getMessage(nMessage));
            }
            catch (Throwable t)
            {
               if (e == null)
               {
                  e = new CompoundIntegrationException("err.rpc.mailUnhandled");
               }

               e.addException(t);
            }
         }

         if (e != null)
         {
            throw e;
         }
      }
      finally
      {
         if (bFolderOpen)
         {
            try
            {
               folder.close(true);
            }
            catch (Exception e)
            {
               m_logger.error("Error closing the folder", e);
            }
         }

         if (con != null)
         {
            con.close();
         }
      }
   }

   /**
    * Process a message then delete if from Store.
    * @param msg The message to process.
    * @throws MessagingException On message parsing or deletion error.
    */
   protected void process(InvocationContext context, Message msg) throws MessagingException
   {
      if (m_logger.isDumpEnabled())
      {
         m_logger.dump(dump(new StringBuilder(128), msg));
      }

      TransferObject tobj = parse(msg);

      tobj.setClassName("Mail");
      tobj.setValue(Mail.CONFIG, toTransferObject(m_channel.getPropertyHolder()));
      tobj.setValue(Mail.USER, m_channel.getUser());
      tobj.setValue(Mail.CHANNEL, m_channel.getName());

      if (m_channel.getQueue() != null)
      {
         TransferObject mqTObj = new TransferObject("MessageQueue", 1);

         mqTObj.setValue(Sender.BODY, tobj);
         mqTObj.setValue(Sender.CHANNEL, m_channel.getName());
         context.getUnitOfWork().addMessage(m_channel.getQueue(), mqTObj);
      }

      if (isBound(m_channel, context))
      {
         receive(tobj, m_channel, context);
      }

      msg.setFlag(Flags.Flag.DELETED, true); // mark as deleted only after reception
   }

   /**
    * Sets the channel metadata object.
    * @param channel The channel metadata object to set.
    */
   public void setChannel(nexj.core.meta.integration.channel.mail.Mail channel)
   {
      m_channel = channel;
   }

   /**
    * Sets the connection factory to query for Mail connections.
    * @param connectionFactory The connection factory to set.
    */
   public void setConnectionFactory(MailConnectionFactoryLocator connectionFactory)
   {
      m_factory = connectionFactory;
   }

   /**
    * Converts an address to TransferObject based on MailAddress.message.
    * @param address The address to convert.
    * @return A TransferObject in the form of MailAddress.message.
    */
   protected static Object toAddress(InternetAddress address)
   {
      if (address == null)
      {
         return null;
      }

      TransferObject tobj = new TransferObject(2);

      tobj.setValue(Mail.ADDRESS, address.getAddress());
      tobj.setValue(Mail.PERSONAL, address.getPersonal());

      return tobj;
   }

   /**
    * Converts an address array to TransferObject based on MailAddress.message, or if multiple
    * addresses then to a List<TransferObject>.
    * @param addressArray The array of addresses to convert.
    * @return A TransferObject or List<TransferObject> representing the array.
    */
   protected static Object toAddress(Address[] addressArray)
   {
      if (addressArray == null)
      {
         return null;
      }

      if (addressArray.length == 1)
      {
         return toAddress((InternetAddress)addressArray[0]);
      }

      ArrayList list = new ArrayList(addressArray.length);

      for (int i = addressArray.length - 1; i >= 0; --i)
      {
         list.add(toAddress((InternetAddress)addressArray[i]));
      }

      return list;
   }

   /**
    * Converts a date to timestamp.
    * @param date The date to convert.
    * @return The resulting timestamp.
    */
   protected static Timestamp toTimestamp(Date date)
   {
      return (date == null) ? null : new Timestamp(date.getTime());
   }

   /**
    * Converts an enumeration of Header objects to a TransferObject of key->vales.
    * @param itr The enumeration to extract headers from.
    * @return The PropertyMap containing all the headers from the enumeration.
    */
   protected static TransferObject toTransferObject(Enumeration/*<Header>*/ itr)
   {
      TransferObject tobj = new TransferObject();

      while (itr.hasMoreElements())
      {
         Header header = (Header)itr.nextElement();

         tobj.setValue(header.getName().toLowerCase(Locale.ENGLISH), header.getValue());
      }

      return tobj;
   }

   /**
    * Converts a Message Part to a TransferObject of MailUtil key constants to corresponding values.
    * @param part The part to convert.
    * @return The converted list of pairs in the form of (BODY, HEADERS).
    * @throws MessagingException if a message access error occurs.
    * @throws IOException if an I/O error occurs.
    */
   protected TransferObject toTransferObject(Part part)
      throws MessagingException, IOException
   {
      Object body = null;

      if (part.isMimeType("multipart/*")) // multipart content
      {
         Multipart multipart = (Multipart)part.getContent();
         ArrayList list = new ArrayList(2);

         for (int i = multipart.getCount() - 1; i >= 0; --i)
         {
            list.add(toTransferObject(multipart.getBodyPart(i))); // add to list of parts
         }

         body = list;
      }
      else if (part.isMimeType("text/*")) // textual content
      {
         String sCharset = DEFAULT_CHARSET;
         Matcher matcher = CHARSET_PATTERN.matcher(part.getContentType());
         int nSize = part.getSize(); // Return -1 if the size cannot be determined.
         StringWriter sw = (nSize > 0) ? new StringWriter(nSize) : new StringWriter();
         Reader reader;

         if (matcher.matches())
         {
            sCharset = matcher.group(1);
         }

         try
         {
            reader = new InputStreamReader(part.getInputStream(), sCharset);
         }
         catch (UnsupportedEncodingException e) // try default encoding instead of Part encoding
         {
            if (sCharset == DEFAULT_CHARSET)
            {
               throw e;
            }

            if (m_logger.isDebugEnabled())
            {
               m_logger.debug("Invalid e-mail message encoding \"" + sCharset + "\"");
            }

            reader = new InputStreamReader(part.getInputStream(), DEFAULT_CHARSET);
         }

         IOUtil.copy(sw, reader);
         reader.close();
         body = sw.toString();
      }
      else // unknown content
      {
         ByteArrayOutputStream os = new ByteArrayOutputStream(part.getSize());

         part.getDataHandler().writeTo(os);
         body = new Binary(os.toByteArray());
      }

      TransferObject content = new TransferObject();

      content.setValue(Mail.FILENAME, part.getFileName());
      content.setValue(Mail.HEADERS, toTransferObject(part.getAllHeaders()));
      content.setValue(Mail.BODY, body);

      return content;
   }

   /**
    * Converts a PropertyHolder to a TransferObject.
    * @param propHolder The PropertyHolder to convert.
    * @return The converted PropertyHolder in the form of a TransferObject.
    */
   protected static TransferObject toTransferObject(PropertyHolder holder)
   {
      if (holder == null)
      {
         return null;
      }

      TransferObject tobj = new TransferObject(holder.getPropertyCount());

      for (Lookup.Iterator/*<String, String>*/ itr = holder.getPropertyIterator(); itr.hasNext();)
      {
         itr.next();
         tobj.setValue((String)itr.getKey(), itr.getValue());
      }

      return tobj;
   }
}