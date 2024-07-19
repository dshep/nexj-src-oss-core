package nexj.core.rpc.tcp;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import nexj.core.integration.MessageOutputStream;
import nexj.core.rpc.TransferObject;

/**
 * An ATNA output stream.
 */
public class ATNAMessageOutputStream extends MessageOutputStream
{
   // associations

   /**
    * Buffer for the message data.
    */
   protected ByteArrayOutputStream m_byteArrayOutputStream;

   // constructors

   /**
    * Construct an ATNAMessageOutputStream.
    * @param ostream The underlying output stream.
    */
   public ATNAMessageOutputStream(OutputStream ostream)
   {
      super(ostream);

      m_byteArrayOutputStream = new ByteArrayOutputStream();
   }

   // operations

   /**
    * @see nexj.core.integration.MessageOutputStream#start(nexj.core.rpc.TransferObject)
    */
   public boolean start(TransferObject raw) throws IOException
   {
      return true;
   }

   /**
    * @see java.io.OutputStream#write(int)
    */
   public void write(int b) throws IOException
   {
      m_byteArrayOutputStream.write(b);
   }

   /**
    * @see java.io.OutputStream#write(byte[], int, int)
    */
   public void write(byte b[], int off, int len) throws IOException
   {
      m_byteArrayOutputStream.write(b, off, len);
   }

   /**
    * @see nexj.core.integration.MessageOutputStream#end(nexj.core.rpc.TransferObject)
    */
   public void end(TransferObject raw) throws IOException
   {
      // write the message size
      m_ostream.write(Integer.toString(m_byteArrayOutputStream.size()).getBytes("utf-8"));

      // write a space
      m_ostream.write(0x20);

      // write the message
      m_byteArrayOutputStream.writeTo(m_ostream);
      m_byteArrayOutputStream.reset();
   }
}
