package nexj.core.rpc.queueing.ra;

import java.io.Serializable;

import nexj.core.rpc.queueing.DispatcherMessage;

/**
 * An response to a request.
 */
public class MessageResponse implements Serializable
{
   /**
    * The serial version UID.
    */
   private final static long serialVersionUID = 4276860639321914972L;

   /**
    * The empty response.
    */
   public final static MessageResponse NONE = new MessageResponse(null);

   // associations

   /**
    * The response message.
    */
   protected DispatcherMessage m_response;

   // constructors

   public MessageResponse(DispatcherMessage response)
   {
      m_response = response;
   }

   // operations

   /**
    * @return the response.
    */
   public Object getResponse()
   {
      return m_response;
   }
}
