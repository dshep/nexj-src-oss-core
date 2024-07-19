// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.sys;

import java.util.ArrayList;

import nexj.core.meta.Metaclass;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.mail.Mail;
import nexj.core.rpc.mail.MailSender;
import nexj.core.runtime.ActionContext;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Mail client wrapper.
 */
public class SysMail implements InvocationContextAware
{
   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The mail Sender.
    */
   protected MailSender m_sender;

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
      m_sender = (MailSender)m_context.getComponentInstance("MailSender.Mail");
   }

   /**
    * Sends an e-mail (backward-compatibility function).
    * @param to The primary recipient address. A string, (addr . name), or a list thereof.
    * @param from The sender address. A string, (addr . name), or null to use the default.
    * @param sSubject The message subject.
    * @param message A string, Multipart, null, or
    *    ((string|inputStream|reader|null|(part1 ... partN)) (header1 . val1) ... (headerN . valN)).
    * @param properties A list of message properties: ((prop1 . val1) ... (propN . valN)).
    * The property can be a symbol or a string: CC, BCC, Reply-To.
    * @throws nexj.core.rpc.RPCException if an error has occurred.
    */
   public void send(
      Metaclass metaclass, Object to, Object from, String sSubject, Object message, Pair properties,
      ActionContext actx)
   {
      TransferObject msg = (message instanceof TransferObject) // if already in Mail.message format
                         ? (TransferObject)((TransferObject)message).clone() : toMessage(message);

      msg.setValue(Mail.TO, toAddress(to));
      msg.setValue(Mail.FROM, toAddress(from));
      msg.setValue(Mail.SUBJECT, sSubject);

      // set all previously supported headers as key->value pairs in msg
      for (Pair props = properties; props != null; props = props.getNext())
      {
         if (!(props.getHead() instanceof Pair))
         {
            throw new RPCException("err.rpc.mailPropertyFormat");
         }

         Pair pair = (Pair)props.getHead();
         Object name = pair.getHead();

         if (!(name instanceof Symbol) && !(name instanceof String))
         {
            throw new RPCException("err.rpc.mailPropertyFormat");
         }

         String sName = name.toString();

         if (sName.equals("CC"))
         {
            msg.setValue(Mail.CC, toAddress(pair.getNext()));
         }
         else if (sName.equals("BCC"))
         {
            msg.setValue(Mail.BCC, toAddress(pair.getNext()));
         }
         else if (sName.equals("Reply-To"))
         {
            msg.setValue(Mail.REPLY, toAddress(pair.getNext()));
         }
         else
         {
            throw new RPCException("err.rpc.mailProperty", new Object[]{sName});
         }

         if (props.getTail() != null && !(props.getTail() instanceof Pair))
         {
            throw new RPCException("err.rpc.mailPropertyFormat");
         }
      }

      m_sender.send(msg);
   }

   /**
    * Transform an address pair into a TransferObject.
    * In the form of: (<address> . <personal>)
    * or ((<address1> . <personal1>) ... (<addressN> . <personalN>))
    * @param addr The object to transform.
    * @return The TransferObject representation of the address or List<TransferObject>.
    */
   protected static Object toAddress(Object addr)
   {
      if (!(addr instanceof Pair)) // not a Pair list
      {
         return addr;
      }

      Pair pair = (Pair)addr;

      if (pair.getHead() instanceof String &&
          (pair.getTail() == null || pair.getTail() instanceof String))
      {
         TransferObject address = new TransferObject(2);

         address.setValue(Mail.ADDRESS, ((Pair)addr).getHead());
         address.setValue(Mail.PERSONAL, ((Pair)addr).getTail());

         return address;
      }

      ArrayList array = new ArrayList(2);

      for (Pair list = pair; list != null; list = list.getNext())
      {
         array.add(toAddress(list.getHead()));
      }

      return array;
   }

   /**
    * Transform mail content potentially containing Pair lists into TransferObjects.
    * Content is a string, Multipart, null, or list.
    * @param content The content to transform.
    * @return The transformed content.
    */
   protected static Object toBody(Object content)
   {
      if (content instanceof Pair) // pair list: ((msg (header1 . val1) ... (headerN . valN)) ...)
      {
         ArrayList array = new ArrayList(2);

         for (Pair list = (Pair)content; list != null; list = list.getNext())
         {
            array.add(toMessage(list.getHead()));
         }

         content = array;
      }

      return content;
   }

   /**
    * Transform a header pair list into a TransferObject.
    * The list is in the form of: ((header1 . val1) ... (headerN . valN))
    * @param list The head of the Pair list.
    * @return The TransferObject representation of the Pair list.
    */
   protected static TransferObject toHeaders(Pair list)
   {
      if (list == null)
      {
         return null;
      }

      TransferObject tobj = new TransferObject();

      for (; list != null; list = list.getNext())
      {
         Pair header = (Pair)list.getHead();
         String sKey = (header.getHead() instanceof Symbol)
                     ? ((Symbol)header.getHead()).getName() : (String)header.getHead();
         String sValue = (String)header.getTail(); // string required by protocol

         tobj.setValue(sKey, sValue);
      }

      return tobj;
   }

   /**
    * Transform message content into a TransferObject.
    * @param content The content to transform.
    * @return The TransferObject message representation with the content.
    */
   protected static TransferObject toMessage(Object content)
   {
      TransferObject msg = new TransferObject(2);

      if (content instanceof Pair) // (msg (header1 . val1) ... (headerN . valN))
      {
         msg.setValue(Mail.BODY, toBody(((Pair)content).getHead()));
         msg.setValue(Mail.HEADERS, toHeaders(((Pair)content).getNext()));
      }
      else // A string, Multipart, null, or list
      {
         msg.setValue(Mail.BODY, content);
      }

      return msg;
   }
}