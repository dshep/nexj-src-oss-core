package nexj.core.rpc.tcp;

import java.io.InputStream;
import java.io.OutputStream;

import nexj.core.integration.MessageInputStream;
import nexj.core.integration.MessageOutputStream;
import nexj.core.integration.MessageStreamFactory;

/**
 * Factory to create ATNA message input and output streams.
 */
public class ATNAMessageStreamFactory implements MessageStreamFactory
{
   // operations

   /**
    * @see nexj.core.integration.MessageStreamFactory#createMessageInputStream(java.io.InputStream)
    */
   public MessageInputStream createMessageInputStream(InputStream istream)
   {
      return new ATNAMessageInputStream(istream);
   }

   /**
    * @see nexj.core.integration.MessageStreamFactory#createMessageOutputStream(java.io.OutputStream)
    */
   public MessageOutputStream createMessageOutputStream(OutputStream ostream)
   {
      return new ATNAMessageOutputStream(ostream);
   }
}
