package nexj.core.rpc.tcp;

import java.io.IOException;
import java.io.OutputStream;

import nexj.core.integration.MessageOutputStream;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.TransferObject;

/**
 * An MLLP output stream.
 */
public class MLLPMessageOutputStream extends MessageOutputStream
{
   // associations

   /**
    * The MLLP message stream factory.
    */
   protected MLLPMessageStreamFactory m_factory;

   // constructors

   /**
    * @param ostream The underlying output stream.
    * @param factory The message stream factory (caller).
    */
   public MLLPMessageOutputStream(OutputStream ostream, MLLPMessageStreamFactory factory)
   {
      super(ostream);

      m_factory = factory;
   }

   // operations

   /**
    * @see nexj.core.integration.MessageOutputStream#start(nexj.core.rpc.TransferObject)
    */
   public boolean start(TransferObject raw) throws IOException
   {
      m_ostream.write(m_factory.getStartBlock());

      return true;
   }

   /**
    * @see java.io.OutputStream#write(int)
    */
   public void write(int n) throws IOException
   {
      if (!m_factory.isValidByte(n))
      {
         throw new RPCException("err.rpc.tcp.mllp.invalidByte",
            new Object[] { new Integer(n), new Integer(m_factory.getMinByte()) });
      }

      m_ostream.write(n);
   }

   /**
    * @see nexj.core.integration.MessageOutputStream#end(nexj.core.rpc.TransferObject)
    */
   public void end(TransferObject raw) throws IOException
   {
      m_ostream.write(m_factory.getEndBlock());
      m_ostream.write(m_factory.getMessageSeparator());
   }
}
