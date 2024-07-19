package nexj.core.integration;

/**
 * Sender for messages that must be received while logged into an invocation context
 * as the sender.
 */
public interface SecureSender extends Sender
{
   /**
    * The message user property: String.
    */
   public final static String USER = "user";

   /**
    * The message protection property: boolean.
    */
   public final static String PROTECTED = "protected";
}
