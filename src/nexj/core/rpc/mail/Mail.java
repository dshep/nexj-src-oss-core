// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.mail;

import nexj.core.integration.Sender;

/**
 * Constants common to both MailReceiver and MailSender.
 */
public interface Mail
{
   // attributes

   /**
    * Key to identify Mail origination channel.
    */
   public final static String CHANNEL = Sender.CHANNEL;

   /**
    * Key to identify Mail connection configuration property overrides.
    */
   public final static String CONFIG = "server";

   /**
    * Key to identify Mail server username override value.
    */
   public final static String USER = "user";

   /**
    * Key to identify Mail server password override value.
    */
   public final static String PASSWORD = "password";

   /**
    * Key to identify additional Mail custom headers.
    */
   public final static String HEADERS = "headers";

   /**
    * Key to identify Mail part filename (valid only in received messages).
    */
   public final static String FILENAME = "filename";

   /**
    * Key to identify Mail reception date (valid only in received messages).
    */
   public final static String DATE_RECEIVED = "receivedDate";

   /**
    * Key to identify Mail sent date (valid only in received messages).
    */
   public final static String DATE_SENT = "sentDate";

   /**
    * Key to identify Mail origination address.
    */

   public final static String FROM = "from";

   /**
    * Key to identify Mail destination addresses.
    */
   public final static String TO = "to";

   /**
    * Key to identify Mail carbon-copy addresses.
    */
   public final static String CC = "cc";

   /**
    * Key to identify Mail blind-carbon-copy addresses.
    */
   public final static String BCC = "bcc";

   /**
    * Key to identify Mail Reply-To addresses.
    */
   public final static String REPLY = "replyTo";

   /**
    * Key to identify the Mail subject value.
    */
   public final static String SUBJECT = "subject";

   /**
    * Key to identify Mail body.
    */
   public final static String BODY = Sender.BODY; // Receiver.java uses Sender.BODY

   /**
    * Key to identify e-mail address in MailAddress messages.
    */
   public final static String ADDRESS = "address";

   /**
    * Key to identify e-mail personal portion in MailAddress messages.
    */
   public final static String PERSONAL = "name";
}