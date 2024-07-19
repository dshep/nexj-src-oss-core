package nexj.core.rpc.queueing.ra;

import java.io.IOException;
import java.net.SocketTimeoutException;

import nexj.core.util.Logger;

/**
 * Implementation of the socket interface which does not use the network.
 */
public class LocalObjectSocket implements ObjectSocket
{
   // attributes

   /**
    * The closed flag.
    */
   protected boolean m_bClosed;

   /**
    * The index position of the next object to return from read().
    */
   protected int m_nArrayPos;

   /**
    * A count of the number of threads currently executing (or waiting) inside write(Object,boolean).
    */
   protected int m_nWriterCount;

   // associations

   /**
    * The array of payload objects.
    */
   protected Object[] m_payloadArray;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(LocalObjectSocket.class);

   // constructors

   /**
    * Create a LocalObjectSocket with a maximum payload buffer size.
    * @param nMaxArraySize The maximum payload buffer size.
    */
   public LocalObjectSocket(int nMaxArraySize)
   {
      assert nMaxArraySize > 0;
      m_payloadArray = new Object[nMaxArraySize];
   }

   /**
    * Create a LocalObjectSocket that buffers only one payload object.
    */
   public LocalObjectSocket()
   {
      m_payloadArray = new Object[1];
   }

   // operations

   /**
    * Get the next payload object to return
    * and remove it from the buffer.
    * @return The next payload object.
    */
   protected Object nextObject()
   {
      Object obj = m_payloadArray[m_nArrayPos];

      m_payloadArray[m_nArrayPos] = null;
      m_nArrayPos = (m_nArrayPos + 1) % m_payloadArray.length;

      return obj;
   }

   /**
    * Add a payload object to the buffer.
    * @param obj The payload object to add.
    * @return The index position that obj was added to.
    * @throws IOException If the buffer size is exceeded.
    */
   protected int addObject(Object obj) throws IOException
   {
      assert obj != null;

      int i = m_nArrayPos;

      do
      {
         if (m_payloadArray[i] == null)
         {
            m_payloadArray[i] = obj;

            return i;
         }

      } while ((i = (i + 1) % m_payloadArray.length) != m_nArrayPos);

      throw new IOException("Maximum buffer size exceeded.");
   }

   /**
    * @see nexj.core.rpc.queueing.ra.ObjectSocket#close()
    */
   public synchronized void close() throws IOException
   {
      s_logger.dump("closing socket " + this.toString());

      m_bClosed = true;
      notifyAll();
   }

   /**
    * @see nexj.core.rpc.queueing.ra.ObjectSocket#isClosed()
    */
   public synchronized boolean isClosed()
   {
      return m_bClosed;
   }

   /**
    * Read with a timeout.
    * @param lTimeout the timeout in milliseconds.
    * @return the object read.
    */
   protected synchronized Object read(long lTimeout) throws IOException
   {
      s_logger.dump("reading socket " + this.toString());

      try
      {
         long lStartTime = (lTimeout == 0) ? 0 : System.currentTimeMillis();
         long lEndTime = lStartTime + lTimeout;

         while (m_payloadArray[m_nArrayPos] == null && !m_bClosed)
         {
            if (lTimeout > 0 && lEndTime <= lStartTime)
            {
               throw new SocketTimeoutException();
            }

            wait(lEndTime - lStartTime);
         }
      }
      catch (InterruptedException e)
      {
         throw new IOException("Reader interrupted");
      }

      if (m_bClosed)
      {
         throw new IOException("Socked closed");
      }

      Object payload = nextObject();

      s_logger.dump("read socket " + this.toString());
      notifyAll();

      return payload;
   }

   /**
    * @see nexj.core.rpc.queueing.ra.ObjectSocket#read()
    */
   public Object read() throws IOException
   {
      return read(0);
   }

   /**
    * @see nexj.core.rpc.queueing.ra.ObjectSocket#write(java.lang.Object)
    */
   public void write(Object obj) throws IOException
   {
      write(obj, true);
   }
   
   /**
    * Writes an object to the socket.
    * @param obj the object to write.
    * @param bSynchronous whether to wait for the object to be read before returning.
    */
   public synchronized void write(Object obj, boolean bSynchronous) throws IOException
   {
      s_logger.dump("waiting to write socket " + this.toString());

      try
      {
         ++m_nWriterCount;

         while (m_nWriterCount > m_payloadArray.length && !m_bClosed)
         {
            wait();
         }

         if (m_bClosed)
         {
            throw new IOException("Socked closed");
         }
   
         s_logger.dump("writing socket " + this.toString());
   
         int nIndex = addObject(obj);
   
         notifyAll();
         
         if (bSynchronous)
         {
            while (m_payloadArray[nIndex] != null && !m_bClosed)
            {
               wait();
            }
      
            if (m_bClosed)
            {
               throw new IOException("Socked closed");
            }
         }
      }
      catch (InterruptedException e)
      {
         throw new IOException("Writer interrupted");
      }
      finally
      {
         --m_nWriterCount;
         notifyAll();
      }

      s_logger.dump("wrote socket " + this.toString());
   }

   /**
    * @see nexj.core.rpc.queueing.ra.ObjectSocket#invalidate()
    */
   public void invalidate() throws IOException
   {
      close();
   }
}
