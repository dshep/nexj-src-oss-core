// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CodingErrorAction;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

import nexj.core.util.Binary;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.RandomAccessFileInputStream;
import nexj.core.util.RandomAccessFileOutputStream;

/**
 * A thread-safe file lock implementation that works around deficiencies and
 * unexpected behaviour in the Java file locking model.
 * 
 * In particular, this class handles two problems:
 * 
 * 1) Java's file locks are held at the process level, not at the thread level.
 * If a thread requests a lock that is already held by the process, then
 * an OverlappingFileLockException will be thrown (unless using Java version less
 * than 6 and FileChannels are not the same).
 * 
 * 2) Closing a channel *may* release all locks held by the JVM on the underlying
 * file regardless of whether the locks were acquired via that channel or via
 * another channel open on the same file.
 * 
 * To fix #1, this class uses a locking queue so that when a thread locks
 * a file that is already locked by another thread, that thread will block.
 * 
 * To fix #2, this class keeps track of the files and channels that have been
 * opened for locking, and ensures that the locks all share the same underlying
 * channel, and that the channel is not closed until all locks have been released.
 * 
 * 
 * Instances of this class are not safe for concurrent access from multiple threads.
 * Instead, each thread should create an instance of this class on the same underlying
 * file.
 */
public class LockableFile
{
   // constants

   /**
    * The size of the buffer to use for the binary to string decoding
    * performed by getDataAsString().
    */
   protected final static int DECODE_BUFFER_SIZE = 8192;

   /**
    * The block size used by the transfer method.
    */
   protected final static int BLOCK_SIZE = 8192;


   // attributes

   /**
    * True if this instance has a lock on the underlying file.
    */
   protected boolean m_bInstanceIsLocked;


   // associations

   /**
    * The file that this instance accesses.
    */
   protected File m_file;

   /**
    * The handle to the open file.
    */
   protected RandomAccessFile m_io;

   /**
    * Channel to use for reading/writing the file.
    */
   protected FileChannel m_channel;

   /**
    * Mapping used to look up a FileAndChannelHandles pair of (RandomAccessFile, FileChannel)
    * given a File object. This is used to ensure that multiple instances created
    * on the same File will share the same underlying FileChannel.
    * 
    * Type: FileAndChannelHandles[File]
    */
   protected final static Lookup s_fileToHandleMap = new HashTab();

   /**
    * Mapping used to look up a BlockingHolder (Semaphore, Transaction, FileLock)
    * tuple given a File object. For each File, this holds the intra-process
    * queue of threads waiting for the file (Semaphore), the transaction
    * in which the current lock for the file is held (Transaction), and the
    * filesystem lock object for the file (FileLock).
    * 
    * Type: BlockingHolder[File]
    */
   protected final static Lookup s_fileBlockingMap = new HashTab();


   // constructors

   /**
    * Opens a new instance on the given file. The newly-created object
    * will have an open handle on the file, but will not be locked.
    * 
    * @param theFile The file to open.
    */
   public LockableFile(File theFile)
   {
      open(theFile);
   }


   // operations

   /**
    * Opens a handle on the given file.
    * 
    * @param file The file to open.
    */
   protected synchronized void open(File file)
   {
      synchronized (s_fileToHandleMap)
      {
         FileAndChannelHandles handles = (FileAndChannelHandles)s_fileToHandleMap.get(file);
         
         if (handles != null)
         {
            m_file = file;
            m_io = handles.getFileHandle();
            m_channel = handles.getChannelHandle();
            handles.incrementReferenceCount();
            
            return;
         }
         
         try
         {
            m_file = file;
            m_io = new RandomAccessFile(file, "rw");
            m_channel = m_io.getChannel();
            
            handles = new FileAndChannelHandles(m_io, m_channel);
            s_fileToHandleMap.put(file, handles);
         }
         catch (IOException ex)
         {
            try
            {
               if (m_channel != null && m_channel.isOpen())
               {
                  m_channel.close();
               }
               
               if (m_io != null)
               {
                  m_io.close();
               }
            }
            catch (IOException ex2)
            {
               //Something is very wrong, but an exception will get thrown regardless.
            }
            
            m_channel = null;
            m_io = null;
            m_file = null;
            
            throw new FileConnectionException("err.rpc.file.ioErr", ex);
         }
      }
   }


   /**
    * Closes this instance and then deletes the file.
    * 
    * @throws IOException If not able to delete the file.
    */
   public synchronized void closeAndDelete() throws IOException
   {
      File toDelete = m_file;
      
      if (toDelete == null)
      {
         return;
      }
      
      close();
      
      if (!toDelete.delete())
      {
         throw new IOException("Unable to delete");
      }
   }


   /**
    * Closes this instance, unlocking it first if it is locked. If the channel
    * and file handle are not being shared by any other instances, then those
    * are closed as well.
    */
   public synchronized void close()
   {
      if (m_file == null)
      {
         return;
      }
      
      if (m_bInstanceIsLocked)
      {
         unlock();
      }
      
      synchronized (s_fileToHandleMap)
      {
         FileAndChannelHandles handles = (FileAndChannelHandles)s_fileToHandleMap.get(m_file);
         
         //This will be null if the handle was closed by testingCloseChannel()
         //and then the finalizer is called (which it will, eventually).
         if (handles == null)
         {
            m_file = null;
            return;
         }
         
         handles.decrementReferenceCount();
         
         if (handles.getReferenceCount() <= 0)
         {
            try
            {
               m_channel.close();
               m_io.close();
            }
            catch (IOException ex)
            {
               throw new FileConnectionException("err.rpc.file.ioErr", ex);
            }
            
            s_fileToHandleMap.remove(m_file);
         }
         
         m_file = null;
      }
   }


   /**
    * Ensures that resources are released when this gets garbage collected.
    * 
    * In particular, because the class keeps references to file and channel
    * handles in a static variable, neglecting to close an instance
    * will definitely result in a resource leak, unless a finalizer is used.
    * 
    * @see java.lang.Object#finalize()
    */
   protected void finalize() throws Throwable
   {
      close();
   }


   /**
    * Gets the blocking information for the file on this instance, using the
    * arguments to determine whether or not the locks are compatible. If
    * they are compatible then no blocking is necessary and no blocking info
    * will be returned.
    * 
    * @param bShared True if the lock being requested is a shared lock; false otherwise.
    * @return The blocking information object; null if the given lock parameters
    *         are compatible with the current lock.
    */
   protected synchronized BlockingHolder getInfo(boolean bShared)
   {
      BlockingHolder fileUsageInfo;
      
      synchronized (s_fileBlockingMap)
      {
         fileUsageInfo = (BlockingHolder)s_fileBlockingMap.get(m_file);
         
         if (fileUsageInfo == null)
         {
            Semaphore blockingQ = new Semaphore(1, true);
            
            fileUsageInfo = new BlockingHolder();
            fileUsageInfo.setBlockingQueue(blockingQ);
            fileUsageInfo.setSharedLock(bShared);
            s_fileBlockingMap.put(m_file, fileUsageInfo);
         }
         else
         {
            //Skip blocking if locks are compatible (both are shared)
            if (bShared && fileUsageInfo.isSharedLock())
            {
               fileUsageInfo.incrementConcurrentCount();
               
               return null;
            }
         }
      }
      
      return fileUsageInfo;
   }


   /**
    * Attempts to lock the file. Succeeds if no incompatible intra-process or
    * inter-process locks are held on the file. If it fails, then it returns
    * immediately and does not block. 
    * 
    * @param bShared True if the lock being requested is a shared lock; false otherwise.
    * @return True if the lock was acquired successfully; false if locking now
    *         would block.
    */
   public synchronized boolean tryLock(boolean bShared)
   {
      boolean bGotThreadLock = false;
      boolean bGotFileLock = false;
      BlockingHolder fileUsageInfo;

      synchronized (s_fileBlockingMap)
      {
         fileUsageInfo = getInfo(bShared);

         if (fileUsageInfo == null)
         {
            m_bInstanceIsLocked = true;
            return true;
         }

         bGotThreadLock = fileUsageInfo.getBlockingQueue().tryAcquire();

         if (!bGotThreadLock)
         {
            return false;
         }
      }

      try
      {
         if (fileUsageInfo.getLock() == null)
         {
            FileLock lock = m_channel.tryLock(0, Long.MAX_VALUE, bShared);
            
            if (lock != null)
            {
               synchronized (s_fileBlockingMap)
               {
                  bGotFileLock = true;
                  fileUsageInfo.setLock(lock);
                  fileUsageInfo.setSharedLock(bShared);
               }
            }
         }
         else
         {
            bGotFileLock = true;
         }
      }
      catch (OverlappingFileLockException olapEx)
      {
         //This means that the file has been locked by a thread in this
         //process *outside* of the functionality provided by this class.
         
         //==> PROGRAMMING ERROR if execution reaches this point.
         throw new IllegalStateException();
      }
      catch (IOException ioEx)
      {
         throw new FileConnectionException("err.rpc.file.ioErr", ioEx);
      }
      
      m_bInstanceIsLocked = bGotFileLock;
      return bGotFileLock;
   }


   /**
    * Locks the file. Returns immediately if no incompatible intra-process or
    * inter-process locks are held on the file. If incompatible locks are
    * already held on the file, then it will block. If it is interrupted while
    * blocking, it will return false.
    * 
    * @param bShared True if the lock being requested is a shared lock; false otherwise.
    * @return True if the lock was acquired successfully; false if it was
    *         interrupted while blocking.
    */
   public synchronized boolean lock(boolean bShared)
   {
      BlockingHolder fileUsageInfo;
      Semaphore blockingQueue = null;

      synchronized (s_fileBlockingMap)
      {
         fileUsageInfo = getInfo(bShared);

         if (fileUsageInfo == null)
         {
            m_bInstanceIsLocked = true;
            return true;
         }

         blockingQueue = fileUsageInfo.getBlockingQueue();
      }

      try
      {
         //(BLOCKING, intra-process)
         blockingQueue.acquire();
      }
      catch (InterruptedException ex)
      {
         return false;
      }

      
      //Now lock the file (BLOCKING, inter-process)
      try
      {
         synchronized (s_fileBlockingMap)
         {
            if (fileUsageInfo.getLock() == null)
            {
               FileLock lock = m_channel.lock(0, Long.MAX_VALUE, bShared);
            
               fileUsageInfo.setLock(lock);
            }
            
            fileUsageInfo.setSharedLock(bShared);
         }
      }
      catch (OverlappingFileLockException olapEx)
      {
         //This means that the file has been locked by a thread in this
         //process *outside* of the functionality provided by this class.
         
         //==> PROGRAMMING ERROR if execution reaches this point.
         throw new IllegalStateException();
      }
      catch (IOException ex)
      {
         throw new FileConnectionException("err.rpc.file.ioErr", ex);
      }
      
      m_bInstanceIsLocked = true;
      return true;
   }


   /**
    * Unlocks the file.
    */
   public synchronized void unlock()
   {
      if (!m_bInstanceIsLocked || m_file == null)
      {
         return;
      }
      
      BlockingHolder fileUsageInfo;
      
      synchronized (s_fileBlockingMap)
      {
         fileUsageInfo = (BlockingHolder)s_fileBlockingMap.get(m_file);
         
         //This will be null if the handle was closed by closeChannel()
         //and then the finalizer is called (which it will, eventually).
         if (fileUsageInfo == null)
         {
            m_bInstanceIsLocked = false;
            return;
         }
         
         if (fileUsageInfo.getConcurrentCount() == 0)
         {
            fileUsageInfo.getBlockingQueue().release();
            
            boolean bGotLock = false;
            
            try
            {
               bGotLock = fileUsageInfo.getBlockingQueue().tryAcquire(0, TimeUnit.SECONDS);
            }
            catch (InterruptedException ex)
            {
               //Exit early, without unlocking.
               return;
            }
            
            if (bGotLock)
            {
               //This queue is finished.
               fileUsageInfo.setBlockingQueue(null);
               s_fileBlockingMap.remove(m_file);
               
               FileLock lock = fileUsageInfo.getLock();
               
               if (lock != null && lock.isValid())
               {
                  try
                  {
                     lock.release();
                  }
                  catch (IOException ex)
                  {
                     throw new FileConnectionException("err.rpc.file.ioErr", ex);
                  }
               }
            }
            else
            {
               //There are still others waiting on the blocking queue.
            }
         }
         else
         {
            fileUsageInfo.decrementConcurrentCount();
         }
      }
      
      m_bInstanceIsLocked = false;
   }


   /**
    * @return True if this instance is locked; false otherwise.
    */
   public synchronized boolean isLocked()
   {
      return m_bInstanceIsLocked;
   }


   /**
    * Copies the contents of sourceFile, overwriting the contents of this file.
    * 
    * @param sourceFile The file to copy.
    * @throws IOException
    */
   public synchronized void copyFromFile(File sourceFile) throws IOException
   {
      FileInputStream istream = new FileInputStream(sourceFile);
      FileChannel ichannel = istream.getChannel();
      long lSourceSize = ichannel.size();

      m_channel.position(0);

      long lCopiedSize = transfer(ichannel, m_channel, 0, lSourceSize);
      
      if (lCopiedSize != lSourceSize)
      {
         throw new IOException("Copy stopped before entire file copied");
      }
      
      m_channel.truncate(lSourceSize);
      ichannel.close();
      istream.close();
   }


   /**
    * Copies the contents of this file to destFile, overwriting the contents
    * of destFile.
    * 
    * @param destFile The destination to copy this file to.
    * @throws IOException
    */
   public synchronized void copyToFile(File destFile) throws IOException
   {
      FileOutputStream ostream = new FileOutputStream(destFile);
      FileChannel ochannel = ostream.getChannel();
      long lSourceSize = m_channel.size();

      long lCopiedSize = transfer(m_channel, ochannel, 0, lSourceSize);
      
      if (lCopiedSize != lSourceSize)
      {
         throw new IOException("Copy stopped before entire file copied");
      }
      
      ochannel.truncate(lSourceSize);
      ochannel.close();
      ostream.close();
   }


   /**
    * Copies data from the source channel to the destination channel in large blocks,
    * to avoid I/O errors when transferring very large files.
    * 
    * @param src The source channel.
    * @param dst The destination channel.
    * @param lPosition The position within the source channel to start reading.
    * @param lCount The number of bytes to transfer.
    * @return The number of bytes actually transferred.
    * @throws IOException If an I/O error occurs.
    */
   protected static long transfer(FileChannel src, FileChannel dst, long lPosition, long lCount) throws IOException
   {
      long lTotalCopied = 0;
      ByteBuffer buffer = ByteBuffer.allocate(BLOCK_SIZE);

      while (lCount > 0)
      {
         int nBlockSize = (int)Math.min(lCount, BLOCK_SIZE);

         buffer.rewind();
         buffer.limit(nBlockSize);

         int nBytesCopied = src.read(buffer, lPosition);

         if (nBytesCopied <= 0)
         {
            throw new IOException("Premature EOF");
         }

         buffer.flip();

         int nBytesWritten = dst.write(buffer, lTotalCopied);

         lTotalCopied += nBytesWritten;
         lCount -= nBytesWritten;
         lPosition += nBytesWritten;

         if (nBytesWritten < nBlockSize)
         {
            return lTotalCopied;
         }
      }

      return lTotalCopied;
   }


   /**
    * Writes the given data to this file, replacing anything that already
    * existed.
    * 
    * @param data The data to write.
    * @throws IOException
    */
   public synchronized void writeData(Binary data) throws IOException
   {
      m_io.seek(0L);
      m_io.write(data.getData());
      m_io.setLength(data.getData().length);
   }


   /**
    * Appends the given data to the end of this file.
    * 
    * @param data The data to write.
    * @throws IOException
    */
   public synchronized void appendData(Binary data) throws IOException
   {
      m_io.seek(m_io.length());
      m_io.write(data.getData());
   }


   /**
    * Writes the given string to this file, replacing anything that already
    * existed. The string will be written with UTF-8 encoding.
    * 
    * @param sData The string to write.
    * @throws IOException
    */
   public synchronized void writeData(String sData) throws IOException
   {
      writeData(new Binary(sData.getBytes("utf-8")));
   }


   /**
    * Flushes any pending write operations to disk. When this method returns,
    * it is guaranteed that any previous writes have been durably recorded
    * on disk.
    * 
    * @throws IOException
    */
   public synchronized void force() throws IOException
   {
      m_channel.force(true);
   }


   /**
    * Truncates the file.
    * 
    * @throws IOException
    */
   public synchronized void truncate() throws IOException
   {
      m_channel.truncate(0L);
   }


   /**
    * Gets an output stream to use for writing to this file.
    * 
    * @return An output stream whose destination is this file.
    */
   public synchronized OutputStream getOutputStream()
   {
      return new RandomAccessFileOutputStream(m_io);
   }


   /**
    * Gets an input stream to use for reading from this file.
    * 
    * @return An input stream whose source is this file.
    */
   public synchronized InputStream getInputStream() throws IOException
   {
      m_io.seek(0L);
   
      return new RandomAccessFileInputStream(m_io);
   }


   /**
    * Reads the contents of the file in binary mode.
    * 
    * @return A Binary object containing all the data in the file.
    * @throws IOException
    */
   public synchronized Binary getDataAsBinary() throws IOException
   {
      if (m_io.length() > Integer.MAX_VALUE)
      {
         throw new IndexOutOfBoundsException("File too large to read into memory");
      }
      
      byte[] nDataArray = new byte[(int)m_io.length()];
      
      m_io.seek(0L);
      
      int nBytesRead = m_io.read(nDataArray);
      
      if (nBytesRead != (int)m_io.length())
      {
         throw new IOException("Read stopped early");
      }
      
      return new Binary(nDataArray);
   }


   /**
    * Reads the contents of the file into a string, using UTF-8 to
    * decode the data.
    * 
    * @return The text from the file.
    * @throws IOException
    */
   public synchronized String getDataAsString() throws IOException
   {
      //Get and configure decoder
      Charset cs = Charset.forName("utf-8");
      CharsetDecoder decoder = cs.newDecoder();
      
      decoder.onMalformedInput(CodingErrorAction.REPLACE);
      decoder.onUnmappableCharacter(CodingErrorAction.REPLACE);
      
      //Set up buffers
      ByteBuffer bytesIn = ByteBuffer.allocate(DECODE_BUFFER_SIZE);
      CharBuffer charsOut = CharBuffer.allocate(DECODE_BUFFER_SIZE);
      StringBuilder result = new StringBuilder(DECODE_BUFFER_SIZE);
      
      //Decode
      long lInPosition = 0L;
      int nBytesRead = 0;
      
      while (nBytesRead > -1)
      {
         lInPosition += nBytesRead;
         nBytesRead = m_channel.read(bytesIn, lInPosition);
         
         //Decode only bytes that were read
         bytesIn.flip();
         decoder.decode(bytesIn, charsOut, false);
         
         //Append only characters that were decoded
         charsOut.flip();
         result.append(charsOut);
         
         //Reset buffers. Input buffer must save unprocessed bytes.
         charsOut.clear();
         bytesIn.compact();
      }
      
      //Final decoding
      bytesIn.flip();
      decoder.decode(bytesIn, charsOut, true);
      decoder.flush(charsOut);
      
      //Append characters produced by decoder dumping internal state
      charsOut.flip();
      result.append(charsOut);
      
      return result.toString();
   }


   /**
    * Closes a channel associated with the given file object, and deletes
    * the static mapping from that file object to the channel object. For
    * TESTING PURPOSES ONLY.
    * 
    * @param file The file instance whose channel should be closed.
    */
   public static void closeChannel(File file)
   {
      if (file == null)
      {
         return;
      }
      
      synchronized (s_fileBlockingMap)
      {
         BlockingHolder fileUsageInfo = (BlockingHolder)s_fileBlockingMap.get(file);
         
         if (fileUsageInfo == null)
         {
            return;
         }
         
         fileUsageInfo.getBlockingQueue().release();
         
         try
         {
            fileUsageInfo.getLock().release();
         }
         catch (ClosedChannelException ex)
         {
            //This is to be expected ==> Do nothing.
         }
         catch (IOException ex)
         {
            throw new FileConnectionException("err.rpc.file.ioErr", ex);
         }
         
         s_fileBlockingMap.remove(file);
      }
      
      //Close the channel itself.
      synchronized (s_fileToHandleMap)
      {
         FileAndChannelHandles handles = (FileAndChannelHandles)s_fileToHandleMap.get(file);
         
         if (handles == null)
         {
            return;
         }
         
         try
         {
            FileChannel ch = handles.getChannelHandle();
            
            if (ch != null && ch.isOpen())
            {
               ch.close();
            }
            
            RandomAccessFile fh = handles.getFileHandle();
            
            if (fh != null)
            {
               fh.close();
            }
         }
         catch (IOException ex)
         {
            throw new FileConnectionException("err.rpc.file.ioErr", ex);
         }
         
         s_fileToHandleMap.remove(file);
      }
   }


   // inner classes

   /**
    * Holds a (Semaphore, Transaction, FileLock) tuple, along with information
    * about the number of instances currently sharing the lock and whether or
    * not the lock is a read lock (shared) or a write lock (exclusive).
    * 
    * The Semaphore is the intra-process queue of threads waiting for the file.
    * The Transaction is the transaction in which the current lock for the file is
    * held. The FileLock is the filesystem lock object for the file.
    */
   protected static class BlockingHolder
   {
      // attributes

      /**
       * The number of LockableFile instances sharing the lock for
       * concurrent access.
       */
      protected int m_nConcurrentCount = 0;

      /**
       * True if the lock a shared (read) lock.
       */
      protected boolean m_bSharedLock;


      // associations

      /**
       * The intra-process queue of threads waiting for the file to become free.
       */
      protected Semaphore m_blockingQueue;

      /**
       * The filesystem lock object for the file.
       */
      protected FileLock m_lock;


      // operations

      /**
       * Sets the intra-process queue of threads waiting for the file to
       * become free.
       * 
       * @param blockingQueue The intra-process blocking queue.
       */
      public void setBlockingQueue(Semaphore blockingQueue)
      {
         m_blockingQueue = blockingQueue;
      }

      /**
       * @return The intra-process queue of threads waiting for the file to
       * become free.
       */
      public Semaphore getBlockingQueue()
      {
         return m_blockingQueue;
      }

      /**
       * Sets the filesystem lock object for the file.
       * 
       * @param lock The filesystem lock object to set.
       */
      public void setLock(FileLock lock)
      {
         m_lock = lock;
      }

      /**
       * @return The filesystem lock object for the file.
       */
      public FileLock getLock()
      {
         return m_lock;
      }

      /**
       * Increases the concurrent count by 1.
       */
      public void incrementConcurrentCount()
      {
         m_nConcurrentCount++;
      }

      /**
       * Decreases the concurrent count by 1.
       */
      public void decrementConcurrentCount()
      {
         m_nConcurrentCount--;
      }

      /**
       * Gets the concurrent count.
       * 
       * @return The concurrent count.
       */
      public int getConcurrentCount()
      {
         return m_nConcurrentCount;
      }

      /**
       * Sets the lock mode for this file.
       * 
       * @param bShared True if the lock is a shared lock (and is therefore
       *                compatible with shared locks in threads in
       *                other transactions and processes); false otherwise.
       */
      public void setSharedLock(boolean bShared)
      {
         m_bSharedLock = bShared;
      }

      /**
       * Gets the lock mode for this file.
       * 
       * @return True if the lock is a shared lock; false if it is an exclusive lock.
       */
      public boolean isSharedLock()
      {
         return m_bSharedLock;
      }
   }


   /**
    * Holds a (RandomAccessFile, FileChannel) tuple, along with a reference count.
    * Used by the to support sharing of the file handle and channel handle
    * between lock instances.
    */
   protected static class FileAndChannelHandles
   {
      // attributes

      /**
       * A count of the number of GenericJournal instances using this file
       * handle and channel pair.
       */
      protected int m_nReferenceCount;


      // associations

      /**
       * The file handle to share among GenericJournal instances.
       */
      protected RandomAccessFile m_fileHandle;

      /**
       * The file channel to share among GenericJournal instances.
       */
      protected FileChannel m_channelHandle;


      // constructors

      /**
       * Creates a new (RandomAccessFile, FileChannel) tuple, and initializes
       * reference count to 1.
       * 
       * @param fileHandle The file handle to use in this tuple.
       * @param channelHandle The channel to use in this tuple.
       */
      public FileAndChannelHandles(RandomAccessFile fileHandle, FileChannel channelHandle)
      {
         m_fileHandle = fileHandle;
         m_channelHandle = channelHandle;
         
         m_nReferenceCount = 1;
      }


      // operations

      /**
       * Gets the handle to the file that is shared among lock instances.
       * 
       * @return The handle to the file.
       */
      public RandomAccessFile getFileHandle()
      {
         return m_fileHandle;
      }

      /**
       * Gets the channel to the file that is shared among lock instances.
       * 
       * @return The channel for the file.
       */
      public FileChannel getChannelHandle()
      {
         return m_channelHandle;
      }

      /**
       * Gets the reference count.
       * 
       * @return The reference count.
       */
      public int getReferenceCount()
      {
         return m_nReferenceCount;
      }

      /**
       * Increases the reference count by 1.
       */
      public void incrementReferenceCount()
      {
         m_nReferenceCount++;
      }

      /**
       * Decreases the reference count by 1.
       */
      public void decrementReferenceCount()
      {
         m_nReferenceCount--;
      }
   }
}
