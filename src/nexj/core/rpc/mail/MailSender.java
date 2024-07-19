// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.mail;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.mail.Address;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.Transport;
import javax.mail.Message.RecipientType;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.internet.MimePart;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.Sender;
import nexj.core.integration.io.ObjectOutput;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.Message;
import nexj.core.monitoring.ThreadLocalCounter;
import nexj.core.monitoring.jmx.StatUtil;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.util.Binary;
import nexj.core.util.Logger;
import nexj.core.util.MIMEUtil;
import nexj.core.util.PropertyIterator;
import nexj.core.util.SysUtil;

/**
 * Mail sender.
 * The keys of the sending TransferObject must be from Key inner class.
 */
public class MailSender implements Sender
{
   // attributes

   /**
    * The default outgoing e-mail charset.
    */
   protected final static String DEFAULT_CHARSET = "UTF-8";

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
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(MailSender.class);

   /**
    * Counter of messages sent since the creation of this component
    */
   protected ThreadLocalCounter m_sentCounter = new ThreadLocalCounter();

   // operations

   /**
    * Add Addresses to a list.
    * @param list The list to add Addresses to.
    * @param addresses The addresses to add, as a String, TransferObject, or List<TransferObject>
    * @returns The address list being filled.
    */
   protected static List/*<Address>*/ addAddresses(List/*<Address>*/ list, Object addresses)
   {
      try
      {
         if (addresses instanceof TransferObject)
         {
            addresses = new InternetAddress(
               (String)((TransferObject)addresses).getValue(Mail.ADDRESS), // required
               (String)((TransferObject)addresses).findValue(Mail.PERSONAL), // optional
               DEFAULT_CHARSET);
         }
         else if (addresses instanceof String)
         {
            addresses = InternetAddress.parse((String)addresses, false);
         }
      }
      catch (Exception e)
      {
         throw new IntegrationException("err.rpc.mailAddress", e);
      }

      if (addresses instanceof Address)
      {
         list.add(addresses);
      }
      else if (addresses instanceof Object[])
      {
         for (int i = 0, nCount = ((Object[])addresses).length; i < nCount; ++i)
         {
            addAddresses(list, ((Object[])addresses)[i]); // might cause expansion of nested lists
         }
      }
      else if (addresses instanceof List)
      {
         for (int i = 0, nCount = ((List)addresses).size(); i < nCount; ++i)
         {
            addAddresses(list, ((List)addresses).get(i)); // might cause expansion of nested lists
         }
      }
      else if (addresses != null)
      {
         throw new IntegrationException("err.rpc.mailAddress"); // unknown object type
      }

      return list;
   }

   /**
    * @see nexj.core.integration.Sender#createOutput()
    */
   public ObjectOutput createOutput()
   {
      return new ObjectOutput();
   }

   /**
    * @see nexj.core.integration.Sender#getSentCount()
    */
   public long getSentCount()
   {
      return m_sentCounter.get();
   }

   /**
    * Normalize the MIME type.
    * @param sMIMEType The known MIME type of object.
    * @param obj The object to examine during MIME type normalization.
    * @return The normalized MIME type.
    * @throws IntegrationException if MIME Type could not be determined.
    */
   protected static String normalizeMIMEType(String sMIMEType, Object obj)
      throws IntegrationException
   {
      // Try to determine the MIME type automatically
      if (sMIMEType == null)
      {
         if (obj instanceof List/*<TransferObject>*/)
         {
            sMIMEType = "multipart/mixed";
         }
         else if (obj instanceof Multipart)
         {
            sMIMEType = ((Multipart)obj).getContentType();
         }
         else if (obj instanceof Reader && ((Reader)obj).markSupported())
         {
            Reader reader = (Reader)obj;
            char[] cbuf = new char[512]; // arbitrary sized header to examine for MIME type
            int nLength;

            try
            {
               reader.mark(cbuf.length);
               nLength = reader.read(cbuf);
               reader.reset();
            }
            catch (IOException e)
            {
               throw new IntegrationException("err.rpc.mailType", e); // MIME type search failure
            }

            if (nLength > 0)
            {
               sMIMEType = MIMEUtil.getMIMEType(new String(cbuf, 0, nLength));
            }
         }
         else if (!(obj instanceof Binary || obj instanceof InputStream))
         {
            sMIMEType = MIMEUtil.getMIMEType(obj);
         }
      }

      if (sMIMEType == null)
      {
         throw new IntegrationException("err.rpc.mailType"); // MIME type could not be determined
      }

      // Determine the character set for text MIME types
      if (sMIMEType.startsWith("text/") && sMIMEType.indexOf("charset") < 0)
      {
         if (sMIMEType.equals("text/html") && (obj instanceof CharSequence))
         {
            String sCharset = MIMEUtil.getHTMLCharSet((CharSequence)obj);

            sMIMEType += "; charset=" + ((sCharset == null) ? DEFAULT_CHARSET : sCharset);
         }
         else
         {
            sMIMEType += "; charset=" + DEFAULT_CHARSET;
         }
      }

      return sMIMEType;
   }

   /**
    * @see nexj.core.integration.Sender#prepare(nexj.core.rpc.TransferObject, nexj.core.rpc.TransferObject, nexj.core.meta.integration.Message)
    */
   public void prepare(TransferObject raw, TransferObject tobj, Message message)
      throws IntegrationException
   {
   }

   /**
    * @see nexj.core.integration.Sender#send(nexj.core.rpc.TransferObject)
    */
   public void send(TransferObject tobj) throws IntegrationException
   {
      if (!m_channel.isSendable())
      {
         throw new IntegrationException("err.rpc.notSender", new Object[]{m_channel.getName()});
      }

      long lStartTime = System.nanoTime();
      String sSenderStatPath = m_channel.getSenderStatPath();

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Sending a message on channel \"" + m_channel.getName() + "\"");
         s_logger.dump(tobj);
      }

      MailConnection con = null;

      try
      {
         con = m_factory.openConnection(tobj);

         MimeMessage msg = con.createMessage();
         List/*<Address>*/ list = new ArrayList/*<Address>*/(1);

         if (addAddresses(list, tobj.findValue(Mail.FROM, m_channel.getFrom())).isEmpty())
         {
            msg.setFrom();
         }
         else if (list.size() == 1)
         {
            msg.setFrom((Address)list.get(0));
         }
         else // FROM field cannot have more than 1 value
         {
            throw new IntegrationException("err.rpc.mailFromCount");
         }

         list.clear();

         if (addAddresses(list, tobj.findValue(Mail.TO)).isEmpty()) // TO field required
         {
            throw new IntegrationException("err.rpc.mailToMissing");
         }

         msg.setRecipients(javax.mail.Message.RecipientType.TO,
                           (Address[])list.toArray(new Address[list.size()]));
         list.clear();

         if (!addAddresses(list, tobj.findValue(Mail.CC)).isEmpty())
         {
            msg.setRecipients(javax.mail.Message.RecipientType.CC,
                              (Address[])list.toArray(new Address[list.size()]));
         }

         list.clear();

         if (!addAddresses(list, tobj.findValue(Mail.BCC)).isEmpty())
         {
            msg.setRecipients(javax.mail.Message.RecipientType.BCC,
                              (Address[])list.toArray(new Address[list.size()]));
         }

         list.clear();

         if (!addAddresses(list, tobj.findValue(Mail.REPLY)).isEmpty())
         {
            msg.setReplyTo((Address[])list.toArray(new Address[list.size()]));
         }

         String sSubject = (String)tobj.findValue(Mail.SUBJECT);

         if (sSubject != null)
         {
            msg.setSubject(sSubject);
         }

         msg.setSentDate(new Date());
         setContent(msg, tobj);

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Sending an e-mail to " + msg.getRecipients(RecipientType.TO) +
                           " from " + msg.getFrom());

            if (s_logger.isDumpEnabled())
            {
               Object headers = tobj.findValue(Mail.HEADERS);

               s_logger.dump(
                  "Subject: \"" + msg.getSubject() + "\"" + SysUtil.LINE_SEP +
                  ((headers != null) ? "Headers: " + headers + SysUtil.LINE_SEP : "") +
                  tobj.findValue(Mail.BODY));
            }
         }

         m_sentCounter.add(1);
         ((InvocationContext)ThreadContextHolder.getContext()).addRPCCount(1);
         StatUtil.incrCounter(m_channel.getType().getMetadata(),
            sSenderStatPath, Channel.STAT_TOTAL_COUNT, 1);

         Transport.send(msg);
      }
      catch (Exception e)
      {
         throw new IntegrationException("err.rpc.mail", e);
      }
      finally
      {
         if (con != null)
         {
            con.close();
         }

         StatUtil.updateAverage(m_channel.getType().getMetadata(), sSenderStatPath,
               Channel.STAT_AVERAGE_SEND_TIME, (double)(System.nanoTime() - lStartTime) / 1000000);
      }
   }

   /**
    * @see nexj.core.integration.Sender#send(java.util.Collection)
    */
   public void send(Collection/*<TransferObject>*/ col) throws IntegrationException
   {
      for (Iterator/*<TransferObject>*/ itr = col.iterator(); itr.hasNext();)
      {
         send((TransferObject)itr.next());
      }
   }

   /**
    * Set the body of the mimePart.
    * @param MimePart The object to set the body for.
    * @param body The message body.
    * @param sContentType The content type of the body.
    * @param sDescription The description/name of the body.
    */
   protected static void setBody(
      MimePart part, Object body, String sContentType, String sDescription)
   {
      String sName = (sDescription == null) ? "noname" : sDescription;
      String sMIMEType = normalizeMIMEType(sContentType, body);

      try // set the body content
      {
         if (body == null)
         {
            part.setContent((sMIMEType.startsWith("text/")) ? (Object)"" : new byte[0], sMIMEType);
         }
         else if (body instanceof String)
         {
            part.setContent(body, sMIMEType);
         }
         else if (body instanceof Binary)
         {
            part.setDataHandler(
               new DataHandler(new InputDataSource(sName, sMIMEType, ((Binary)body).getData())));
         }
         else if (body instanceof Reader)
         {
            part.setContent(body, sMIMEType);
         }
         else if (body instanceof InputStream)
         {
            part.setDataHandler(
               new DataHandler(new InputDataSource(sName, sMIMEType, (InputStream)body)));
         }
         else if (body instanceof Multipart)
         {
            part.setContent((Multipart)body);
         }
         else if (body instanceof List/*<TransferObject>*/) // interpret as a multi-part collection
         {
            MimeMultipart multipart;
            int nPos = sMIMEType.indexOf('/');

            if (nPos < 0)
            {
               multipart = new MimeMultipart();
            }
            else
            {
               sMIMEType = sMIMEType.substring(nPos + 1);
               nPos = sMIMEType.indexOf(';');
               multipart = new MimeMultipart((nPos < 0) ? sMIMEType : sMIMEType.substring(0, nPos));
            }

            for (int i = 0, nCount = ((List)body).size(); i < nCount; ++i)
            {
               MimeBodyPart bodyPart = new MimeBodyPart();

               setContent(bodyPart, (TransferObject)((List)body).get(i));
               multipart.addBodyPart(bodyPart);
            }

            part.setContent(multipart); // set the message content to the MultiPart
         }
         else
         {
            throw new IntegrationException("err.rpc.mailMessage");//i.e. unknown/unhandled body type
         }
      }
      catch (Exception e) // MessagingException, IOException
      {
         throw new IntegrationException("err.rpc.mailMessage", e);
      }
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
    * Set the content of the mimePart from the TransferObject description.
    * @param mimePart The object to set the content for.
    * @param msg The message description containing the content.
    */
   protected static void setContent(MimePart part, TransferObject content)
   {
      TransferObject headers = (TransferObject)content.findValue(Mail.HEADERS);
      String sDescription = null;
      String sContentType = null;

      if (headers != null)
      {
         for (PropertyIterator itr = headers.getIterator(); itr.hasNext();)
         {
            itr.next();

            String sKey = itr.getName();
            Object value = itr.getValue();

            if (value instanceof String)
            {
               if ("Content-Type".equalsIgnoreCase(sKey))
               {
                  sContentType = (String)value;
               }
               else
               {
                  if ("Content-Description".equalsIgnoreCase(sKey))
                  {
                     sDescription = (String)value;
                  }

                  try
                  {
                     part.addHeader(sKey, (String)value);
                  }
                  catch (MessagingException e)
                  {
                     throw new IntegrationException("err.rpc.mailHeaderFormat", e);
                  }
               }
            }
            else // only String->String headers are valid
            {
               throw new IntegrationException("err.rpc.mailHeaderFormat");
            }
         }
      }

      setBody(part, content.findValue(Mail.BODY), sContentType, sDescription);
   }

   // inner classes

   /**
    * DataSource for input only, internally stores data as a byte[].
    * NOTE: DataSource specification requires getInputStream()/getOutputStream() to return new
    *       stream objects for every call.
    */
   protected static class InputDataSource implements DataSource
   {
      /**
       * The input data stream.
       */
      protected byte[] m_input;

      /**
       * The valid byte length of m_data from m_nOffset.
       */
      protected int m_nLength;

      /**
       * The starting position of the data in m_data.
       */
      protected int m_nOffset;

      /**
       * The stream MIME Type.
       */
      protected String m_sMIMEType;

      /**
       * The stream name.
       */
      protected String m_sName;

      /**
       * Constructor.
       * @param sName The data stream name.
       * @param sMIMEType The data stream MIME type.
       * @param input The input data stream.
       * @throws IOException On input data stream read error.
       */
      public InputDataSource(String sName, String sMIMEType, InputStream input) throws IOException
      {
         m_input = new byte[2048];
         m_nLength = 0;
         m_nOffset = 0;

         for (;;)
         {
            if (m_input.length - m_nLength < 2048)
            {
               byte[] buf = new byte[m_input.length << 1];

               System.arraycopy(m_input, 0, buf, 0, m_nLength);
               m_input = buf;
            }

            int nFree = m_input.length - m_nLength;
            int nCount = input.read(m_input, m_nLength, nFree);

            if (nCount > 0)
            {
               m_nLength += nCount;
            }

            if (nCount < nFree)
            {
               break; // read last chunk, i.e. chunk smaller then buffer size allocated for it
            }
         }
      }

      /**
       * Constructor.
       * @param sName The data stream name.
       * @param sMIMEType The data stream MIME type.
       * @param input The input data stream.
       */
      public InputDataSource(String sName, String sMIMEType, byte[] input)
      {
         this(sName, sMIMEType, input, 0, input.length);
      }

      /**
       * Constructor.
       * @param sName The data stream name.
       * @param sMIMEType The data stream MIME type.
       * @param input The input data stream.
       * @param nOffset The starting position of the data in input.
       * @param nLength The valid byte length of m_data from nOffset.
       */
      public InputDataSource(
         String sName, String sMIMEType, byte[] input, int nOffset, int nLength)
      {
         m_sName = sName;
         m_sMIMEType = sMIMEType;
         m_input = input;
         m_nOffset = nOffset;
         m_nLength = nLength;
      }

      /**
       * @see javax.activation.DataSource#getContentType()
       */
      public String getContentType()
      {
         return m_sMIMEType;
      }

      /**
       * @see javax.activation.DataSource#getInputStream()
       */
      public InputStream getInputStream() throws IOException
      {
         // DataSource specification requires returning a new stream object for every call
         return new ByteArrayInputStream(m_input, m_nOffset, m_nLength);
      }

      /**
       * @see javax.activation.DataSource#getName()
       */
      public String getName()
      {
         return m_sName;
      }

      /**
       * @see javax.activation.DataSource#getOutputStream()
       */
      public OutputStream getOutputStream() throws IOException
      {
         throw new UnsupportedOperationException();
      }
   }
}