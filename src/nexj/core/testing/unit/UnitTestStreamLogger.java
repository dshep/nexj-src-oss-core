package nexj.core.testing.unit;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.OutputStream;

import nexj.core.util.ObjUtil;

/**
 * Concrete implementation of UnitTestObjectLogger that logs messages to an output stream.
 */
public class UnitTestStreamLogger extends UnitTestObjectLogger
{
   /**
    * The output stream to which log messages will be written.
    */
   protected OutputStream m_out;

   /**
    * @param out The output stream to log to
    */
   public UnitTestStreamLogger(OutputStream out)
   {
      super();
      m_out = out;
   }

   /**
    * Writes an integer indicating the length of the upcoming serialized object and the object to the output stream.
    * 
    * @see nexj.core.testing.unit.UnitTestObjectLogger#log(nexj.core.testing.unit.UnitTestObjectLogger.LogMessage)
    * @param msg The message object to write to the output stream.
    */
   protected void log(LogMessage msg)
   {
      super.log(msg);
      
      try
      {
         ByteArrayOutputStream bout = new ByteArrayOutputStream(2048);
         ObjectOutputStream oos = new ObjectOutputStream(bout);
         
         oos.writeObject(msg);
         byte[] objArray = bout.toByteArray();
         
         writeInt(objArray.length);
         m_out.write(objArray);
      }
      catch (IOException e)
      {
         ObjUtil.rethrow(e);
      }
   }

   /**
    * Writes an integer to the output stream.
    * 
    * @param value The integer to write out.
    */
   private void writeInt(int value) throws IOException
   {
      m_out.write(value >> 24);
      m_out.write(value >> 16);
      m_out.write(value >> 8);
      m_out.write(value);
   }

}
