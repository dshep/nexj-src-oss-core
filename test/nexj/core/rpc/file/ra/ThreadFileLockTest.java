// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;

import nexj.core.util.ObjUtil;
import nexj.test.util.TempFileUtil;

import junit.framework.TestCase;

/**
 * Tests the thread-safe file lock.
 */
public class ThreadFileLockTest extends TestCase
{
   /**
    * Test the basic functionality of LockableFile, ensuring that it will lock
    * the file between threads.
    */
   public void testThreadsBlockOtherThreads() throws Exception
   {
      final File tempFile = TempFileUtil.makeTemporaryFile(getClass().getName() + "." + getName());
      final CyclicBarrier barrier = new CyclicBarrier(2);
      final List threadExceptionsList = Collections.synchronizedList(new ArrayList(3));
      final List operationsList = Collections.synchronizedList(new ArrayList(9));
      
      final Thread.UncaughtExceptionHandler exHandler = new Thread.UncaughtExceptionHandler()
      {
         public void uncaughtException(Thread t, Throwable e)
         {
            threadExceptionsList.add(e);
         }
      };
      
      Thread threadA = new Thread()
      {
         public void run()
         {
            try
            {
               //SYNC 1
               barrier.await();
               
               //Lock the file.
               LockableFile f1 = new LockableFile(tempFile);
               
               operationsList.add("Locking f1...");
               f1.lock(false);
               operationsList.add("f1 LOCKED");
               
               //SYNC 2
               barrier.await();
               
               //Thread B blocks here.
               
               //SYNC 3
               barrier.await();
               
               operationsList.add("Unlocking f1...");
               f1.unlock();
               
               //Thread B unblocks here and locks the file.
               
               //SYNC 4
               barrier.await();
               operationsList.add("f1 UNLOCKED");
               
               //SYNC 5
               barrier.await();

            }
            catch (BrokenBarrierException ex)
            {
               fail(ex.getMessage());
            }
            catch (InterruptedException ex)
            {
               fail(ex.getMessage());
            }
         }
      };
      
      Thread threadB  = new Thread()
      {
         public void run()
         {
            try
            {
               //SYNC 1
               barrier.await();
               
               //Thread A locks file here.
               
               //SYNC 2
               barrier.await();
               
               //Try to lock the file--should BLOCK
               final LockableFile f2 = new LockableFile(tempFile);
               
               Thread blockingThread = new Thread()
               {
                  public void run()
                  {
                     operationsList.add("Locking f2...");
                     f2.lock(false);
                     operationsList.add("f2 LOCKED");
                  }
               };
               
               blockingThread.setUncaughtExceptionHandler(exHandler);
               
               blockingThread.start();
               
               while (blockingThread.getState() != Thread.State.WAITING)
               {
                  sleep(50);
               }
               
               
               //SYNC 3
               barrier.await();
               
               //Thread A should unlock here, allowing blockingThread to complete.
               
               while (blockingThread.isAlive())
               {
                  sleep(50);
               }
               
               //SYNC 4
               barrier.await();               
               
               //SYNC 5
               barrier.await();
               
               blockingThread.join();
               
               f2.unlock();
               operationsList.add("f2 UNLOCKED");
            }
            catch (BrokenBarrierException ex)
            {
               fail(ex.getMessage());
            }
            catch (InterruptedException ex)
            {
               fail(ex.getMessage());
            }
         }
      };
      
      threadA.setUncaughtExceptionHandler(exHandler);
      threadB.setUncaughtExceptionHandler(exHandler);
      
      threadA.start();
      threadB.start();
      
      threadA.join(1000);
      threadB.join(1000);
      
      if (threadExceptionsList.size() > 0)
      {
         ObjUtil.rethrow((Throwable)threadExceptionsList.get(0));
      }
      
      assertTrue(Arrays.toString(operationsList.toArray()),
         !threadA.isAlive() && !threadB.isAlive());
      
      
      String[] expected = new String[] {
         "Locking f1...",
         "f1 LOCKED",
         "Locking f2...",
         "Unlocking f1...",
         "f2 LOCKED",
         "f1 UNLOCKED",
         "f2 UNLOCKED"
      };
      
      assertTrue(operationsList.toString(),
         Arrays.equals(expected, operationsList.toArray()));
   }


   /**
    * Two threads accessing the file in read mode with shared locks should not block.
    */
   public void testSharedLocksDontBlock() throws Exception
   {
      final File tempFile = TempFileUtil.makeTemporaryFile(getClass().getName() + "." + getName());
      final CyclicBarrier barrier = new CyclicBarrier(2);
      final List threadExceptionsList = Collections.synchronizedList(new ArrayList(3));
      final List operationsList = Collections.synchronizedList(new ArrayList(9));
      
      Thread.UncaughtExceptionHandler exHandler = new Thread.UncaughtExceptionHandler()
      {
         public void uncaughtException(Thread t, Throwable e)
         {
            threadExceptionsList.add(e);
         }
      };
      
      Thread threadA = new Thread()
      {
         public void run()
         {
            try
            {
               //SYNC 1
               barrier.await();
               
               //Lock the file.
               LockableFile f1 = new LockableFile(tempFile);
               
               operationsList.add("Locking f1...");
               f1.lock(true);
               operationsList.add("f1 LOCKED");
               
               //SYNC 2
               barrier.await();
               
               //Thread B should lock successfully here.
               
               
               //SYNC 3
               barrier.await();
               
               //Thread B unlocks here
               
               //SYNC 4
               barrier.await();
               
               f1.unlock();
               operationsList.add("f1 UNLOCKED");
            }
            catch (BrokenBarrierException ex)
            {
               fail(ex.getMessage());
            }
            catch (InterruptedException ex)
            {
               fail(ex.getMessage());
            }
         }
      };
      
      Thread threadB  = new Thread()
      {
         public void run()
         {
            try
            {
               //SYNC 1
               barrier.await();
               
               //Thread A does locking here.
               
               //SYNC 2
               barrier.await();
               
               //Try to lock the file--should not block, because SHARED
               LockableFile f2 = new LockableFile(tempFile);
               
               operationsList.add("Locking f2...");
               f2.lock(true);
               operationsList.add("f2 LOCKED");
               
               //SYNC 3
               barrier.await();
               
               f2.unlock();
               operationsList.add("f2 UNLOCKED");
               
               //SYNC 4
               barrier.await();
               
               //Thread A unlocks here.
            }
            catch (BrokenBarrierException ex)
            {
               fail(ex.getMessage());
            }
            catch (InterruptedException ex)
            {
               fail(ex.getMessage());
            }
         }
      };
      
      threadA.setUncaughtExceptionHandler(exHandler);
      threadB.setUncaughtExceptionHandler(exHandler);
      
      threadA.start();
      threadB.start();
      
      threadA.join(1000);
      threadB.join(1000);
      
      if (threadExceptionsList.size() > 0)
      {
         ObjUtil.rethrow((Throwable)threadExceptionsList.get(0));
      }
      
      assertTrue(!threadA.isAlive() && !threadB.isAlive());
      
      
      String[] expected = new String[] {
         "Locking f1...",
         "f1 LOCKED",
         "Locking f2...",
         "f2 LOCKED",
         "f2 UNLOCKED",
         "f1 UNLOCKED"
      };
      
      assertTrue(operationsList.toString(),
         Arrays.equals(expected, operationsList.toArray()));
   }
   
   
   
   /**
    * Test that a shared lock will block an exclusive lock
    */
   public void testSharedBlocksExclusiveLock() throws Exception
   {
      final File tempFile = TempFileUtil.makeTemporaryFile(getClass().getName() + "." + getName());
      final CyclicBarrier barrier = new CyclicBarrier(2);
      final List threadExceptionsList = Collections.synchronizedList(new ArrayList(3));
      final List operationsList = Collections.synchronizedList(new ArrayList(9));
      
      final Thread.UncaughtExceptionHandler exHandler = new Thread.UncaughtExceptionHandler()
      {
         public void uncaughtException(Thread t, Throwable e)
         {
            threadExceptionsList.add(e);
         }
      };
      
      Thread threadA = new Thread()
      {
         public void run()
         {
            try
            {
               //SYNC 1
               barrier.await();
               
               //Lock the file. (SHARED)
               LockableFile f1 = new LockableFile(tempFile);
               
               operationsList.add("Locking f1...");
               f1.lock(true);
               operationsList.add("f1 LOCKED");
               
               //SYNC 2
               barrier.await();
               
               //Thread B blocks here.
               
               //SYNC 3
               barrier.await();
               
               operationsList.add("Unlocking f1...");
               f1.unlock();
               
               //Thread B unblocks here and locks the file.
               
               //SYNC 4
               barrier.await();
               operationsList.add("f1 UNLOCKED");
               
               //SYNC 5
               barrier.await();

            }
            catch (BrokenBarrierException ex)
            {
               fail(ex.getMessage());
            }
            catch (InterruptedException ex)
            {
               fail(ex.getMessage());
            }
         }
      };
      
      Thread threadB  = new Thread()
      {
         public void run()
         {
            try
            {
               //SYNC 1
               barrier.await();
               
               //Thread A locks file here.
               
               //SYNC 2
               barrier.await();
               
               //Try to lock the file--should BLOCK
               final LockableFile f2 = new LockableFile(tempFile);
               
               Thread blockingThread = new Thread()
               {
                  public void run()
                  {
                     operationsList.add("Locking f2...");
                     f2.lock(false);
                     operationsList.add("f2 LOCKED");
                  }
               };
               
               blockingThread.setUncaughtExceptionHandler(exHandler);
               
               blockingThread.start();
               
               while (blockingThread.getState() != Thread.State.WAITING)
               {
                  sleep(50);
               }
               
               
               //SYNC 3
               barrier.await();
               
               //Thread A should unlock here, allowing blockingThread to complete.
               
               while (blockingThread.isAlive())
               {
                  sleep(50);
               }
               
               //SYNC 4
               barrier.await();               
               
               //SYNC 5
               barrier.await();
               
               blockingThread.join();
               
               f2.unlock();
               operationsList.add("f2 UNLOCKED");
            }
            catch (BrokenBarrierException ex)
            {
               fail(ex.getMessage());
            }
            catch (InterruptedException ex)
            {
               fail(ex.getMessage());
            }
         }
      };
      
      threadA.setUncaughtExceptionHandler(exHandler);
      threadB.setUncaughtExceptionHandler(exHandler);
      
      threadA.start();
      threadB.start();
      
      threadA.join(1000);
      threadB.join(1000);
      
      if (threadExceptionsList.size() > 0)
      {
         ObjUtil.rethrow((Throwable)threadExceptionsList.get(0));
      }
      
      assertTrue(Arrays.toString(operationsList.toArray()),
         !threadA.isAlive() && !threadB.isAlive());
      
      String[] expected = new String[] {
         "Locking f1...",
         "f1 LOCKED",
         "Locking f2...",
         "Unlocking f1...",
         "f2 LOCKED",
         "f1 UNLOCKED",
         "f2 UNLOCKED"
      };
      
      assertTrue(operationsList.toString(),
         Arrays.equals(expected, operationsList.toArray()));
   }
   
   
   /**
    * Test that an exclusive lock will block a shared lock
    */
   public void testExclusiveBlocksSharedLock() throws Exception
   {
      final File tempFile = TempFileUtil.makeTemporaryFile(getClass().getName() + "." + getName());
      final CyclicBarrier barrier = new CyclicBarrier(2);
      final List threadExceptionsList = Collections.synchronizedList(new ArrayList(3));
      final List operationsList = Collections.synchronizedList(new ArrayList(9));
      
      final Thread.UncaughtExceptionHandler exHandler = new Thread.UncaughtExceptionHandler()
      {
         public void uncaughtException(Thread t, Throwable e)
         {
            threadExceptionsList.add(e);
         }
      };
      
      Thread threadA = new Thread()
      {
         public void run()
         {
            try
            {
               //SYNC 1
               barrier.await();
               
               //Lock the file. (EXCLUSIVE)
               LockableFile f1 = new LockableFile(tempFile);
               
               operationsList.add("Locking f1...");
               f1.lock(false);
               operationsList.add("f1 LOCKED");
               
               //SYNC 2
               barrier.await();
               
               //Thread B blocks here.
               
               //SYNC 3
               barrier.await();
               
               operationsList.add("Unlocking f1...");
               f1.unlock();
               
               //Thread B unblocks here and locks the file.
               
               //SYNC 4
               barrier.await();
               operationsList.add("f1 UNLOCKED");
               
               //SYNC 5
               barrier.await();

            }
            catch (BrokenBarrierException ex)
            {
               fail(ex.getMessage());
            }
            catch (InterruptedException ex)
            {
               fail(ex.getMessage());
            }
         }
      };
      
      Thread threadB  = new Thread()
      {
         public void run()
         {
            try
            {
               //SYNC 1
               barrier.await();
               
               //Thread A locks file here.
               
               //SYNC 2
               barrier.await();
               
               //Try to lock the file--should BLOCK
               final LockableFile f2 = new LockableFile(tempFile);
               
               Thread blockingThread = new Thread()
               {
                  public void run()
                  {
                     operationsList.add("Locking f2...");
                     f2.lock(true);
                     operationsList.add("f2 LOCKED");
                  }
               };
               
               blockingThread.setUncaughtExceptionHandler(exHandler);
               
               blockingThread.start();
               
               while (blockingThread.getState() != Thread.State.WAITING)
               {
                  sleep(50);
               }
               
               
               //SYNC 3
               barrier.await();
               
               //Thread A should unlock here, allowing blockingThread to complete.
               
               while (blockingThread.isAlive())
               {
                  sleep(50);
               }
               
               //SYNC 4
               barrier.await();               
               
               //SYNC 5
               barrier.await();
               
               blockingThread.join();
               
               f2.unlock();
               operationsList.add("f2 UNLOCKED");
            }
            catch (BrokenBarrierException ex)
            {
               fail(ex.getMessage());
            }
            catch (InterruptedException ex)
            {
               fail(ex.getMessage());
            }
         }
      };
      
      threadA.setUncaughtExceptionHandler(exHandler);
      threadB.setUncaughtExceptionHandler(exHandler);
      
      threadA.start();
      threadB.start();
      
      threadA.join(1000);
      threadB.join(1000);
      
      if (threadExceptionsList.size() > 0)
      {
         ObjUtil.rethrow((Throwable)threadExceptionsList.get(0));
      }
      
      assertTrue(Arrays.toString(operationsList.toArray()),
         !threadA.isAlive() && !threadB.isAlive());
      
      String[] expected = new String[] {
         "Locking f1...",
         "f1 LOCKED",
         "Locking f2...",
         "Unlocking f1...",
         "f2 LOCKED",
         "f1 UNLOCKED",
         "f2 UNLOCKED"
      };
      
      assertTrue(operationsList.toString(),
         Arrays.equals(expected, operationsList.toArray()));
   }


   /**
    * Test that the try lock method returns false if the file is already
    * locked.
    */
   public void testTryLockReturnsFalseIfAlreadyLocked() throws Exception
   {
      File tempFile = TempFileUtil.makeTemporaryFile(getClass().getName() + "." + getName());
      LockableFile f1, f2;
      
      //LOCK f1 as EXCLUSIVE
      f1 = new LockableFile(tempFile);
      assertTrue(f1.lock(false));
      
      //f2 should not be able to lock at all
      f2 = new LockableFile(tempFile);
      assertFalse(f2.tryLock(false));
      assertFalse(f2.tryLock(true));
      
      //Reset f1 to SHARED LOCK
      f1.unlock();
      assertTrue(f1.lock(true));
      
      //f2 should also be able to lock SHARED, but NOT EXCLUSIVE
      assertFalse(f2.tryLock(false));
      assertTrue(f2.tryLock(true));
      f2.unlock();
      
      f1.unlock();
      
      f1.close();
      f2.close();
   }


   /**
    * Tests that the lockable file doesn't react badly to repeated method
    * invocations and other forms of misuse.
    */
   public void testHandlesMisuseGracefully() throws Exception
   {
      File tempFile = TempFileUtil.makeTemporaryFile(getClass().getName() + "." + getName());
      LockableFile f1, f2;
      
      f1 = new LockableFile(tempFile);
      
      f1.unlock();
      f1.unlock();
      f1.close();
      f1.close();
      
      f1 = new LockableFile(tempFile);
      f2 = new LockableFile(tempFile);
      assertTrue(f1.lock(false));
      f1.close();
      assertTrue(f2.tryLock(false));
      f2.unlock();
      f1.unlock();
      f1.close();
      f2.close();
   }
}
