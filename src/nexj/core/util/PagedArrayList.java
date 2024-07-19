// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.io.Serializable;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

/**
 * A List implementation that saves heap space by paging elements that are not
 * in use to disk.
 */
public class PagedArrayList extends AbstractList implements Serializable
{
   // constants

   /**
    * The serialization version.
    */
   private final static long serialVersionUID = 8499334478345285099L;

   /**
    * The default page size for the array.
    */
   public final static int DEFAULT_PAGE_SIZE = 100;


   // attributes

   /**
    * The page size for the array. Pages may be smaller than this, but never
    * larger.
    */
   protected int m_nMaxPageSize;

   /**
    * The number of elements in the array.
    */
   protected transient int m_nSize;


   // associations

   /**
    * The StreamTable instance for storing serialized pages.
    */
   protected transient StreamTable m_streamTable;

   /**
    * Stores information for all of the pages.
    */
   protected transient List m_pageList;

   /**
    * The current page.
    */
   protected transient PageInfo m_activePage;

   /**
    * A weak reference cache of externally referenced elements. Prevents multiple
    * object instances for the same element from being loaded into memory.
    */
   protected transient ListObjectCache m_externallyReferencedElements;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(PagedArrayList.class);


   // constructors

   /**
    * Constructs a new paged array list using the default page size.
    */
   public PagedArrayList()
   {
      this(DEFAULT_PAGE_SIZE);
   }

   /**
    * Constructs a new paged array list with a user-defined page size.
    * 
    * @param nElementsPerPage The page size to use.
    */
   public PagedArrayList(int nElementsPerPage)
   {
      if (nElementsPerPage < 1)
      {
         throw new IllegalArgumentException("Elements per page not positive");
      }

      m_nMaxPageSize = nElementsPerPage;
      initEmptyList();
   }


   // operations

   /**
    * Checks that the given index is a valid index into this List, or
    * one index after the end of the list.
    * 
    * @param nIndex The index to check.
    * @throws IllegalArgumentException If the index is invalid.
    */
   protected void checkBoundsInclusive(int nIndex)
   {
      if (nIndex < 0 || nIndex > m_nSize)
      {
         throw new IndexOutOfBoundsException("Index " + nIndex + " on list of size " + size());
      }
   }

   /**
    * Checks that the given index is a valid index into this List.
    * 
    * @param nIndex The index to check.
    * @throws IllegalArgumentException If the index is invalid.
    */
   protected void checkBoundsExclusive(int nIndex)
   {
      if (nIndex < 0 || nIndex >= m_nSize)
      {
         throw new IndexOutOfBoundsException("Index " + nIndex + " on list of size " + size());
      }
   }

   /**
    * Initializes this list as an empty list.
    */
   protected void initEmptyList()
   {
      m_nSize = 0;
      m_pageList = new ArrayList();
      m_activePage = new PageInfo();
      m_externallyReferencedElements = new ListObjectCache();
      m_pageList.add(m_activePage);
   }

   /**
    * Disposes all resources uses by the paged array list, deleting data from disk.
    * Any subsequent method invocations will throw an IllegalStateException.
    */
   public void dispose()
   {
      if (m_streamTable == null)
      {
         return;
      }

      StreamTable table = getStreamTable();

      m_streamTable = null;
      m_pageList = null;
      m_activePage = null;
      m_externallyReferencedElements = null;

      try
      {
         table.close();
      }
      catch (IOException ex)
      {
         ObjUtil.rethrow(ex);
      }
   }

   /**
    * @see java.lang.Object#finalize()
    */
   protected void finalize() throws Throwable
   {
      dispose();
   }

   /**
    * Finds the PageInfo record for the page that contains the element
    * at the given index. Performs a binary search, taking an intelligent
    * guess at the beginning of the search.
    * 
    * @param nIndex The index of the element whose page should be found.
    * @return The index if the page of the element of the given index.
    */
   protected int findPageWithIndex(int nIndex)
   {
      if (nIndex == m_nSize)
      {
         return m_pageList.size() - 1;
      }

      // Find page with this index
      int nPageNumber = nIndex / m_nMaxPageSize;
      PageInfo page = null;
      boolean bDone = false;
      int nBottomInclusive = 0;
      int nTopNonInclusive = m_pageList.size();

      while (!bDone)
      {
         page = (PageInfo)m_pageList.get(nPageNumber);

         if (nIndex < page.getPageOffset())
         {
            nTopNonInclusive = nPageNumber;
            nPageNumber = nBottomInclusive + ((nPageNumber - nBottomInclusive) >> 1);
         }
         else if (nIndex >= (page.getPageOffset() + page.getPageSize()))
         {
            nBottomInclusive = nPageNumber + 1;
            nPageNumber = nPageNumber + ((nTopNonInclusive - nPageNumber) >> 1);
         }
         else
         {
            bDone = true;
         }
      }

      return nPageNumber;
   }

   /**
    * Saves and invalidates the given page.
    * 
    * @param page The page to save.
    */
   protected void swapPageOut(PageInfo page)
   {
      StreamTable table = getStreamTable();
      OutputStream ostream = null;
      ObjectOutputStream oos = null;

      // Get stream id, if necessary
      if (page.getStreamId() == PageInfo.STREAM_UNSET)
      {
         page.setStreamId(table.getFreeStreamId());
      }

      try
      {
         ostream = table.getOutputStream(page.getStreamId());
         oos = new ObjectOutputStream(ostream);

         ArrayList pageDataList = page.getPageDataList();
         int nSize = (pageDataList != null) ? pageDataList.size() : 0;

         oos.writeInt(nSize);

         for (int i = 0; i < nSize; i++)
         {
            oos.writeObject(pageDataList.get(i));
         }

         page.setPageDataList(null);

         oos.close();
         ostream.close();
      }
      catch (IOException ex)
      {
         try
         {
            if (oos != null)
            {
               oos.close();
            }

            if (ostream != null)
            {
               ostream.close();
            }
         }
         catch (IOException ex2)
         {
            // Suppress it because an I/O error is already going to be thrown
         }

         dispose();
         ObjUtil.rethrow(ex);
      }
   }

   /**
    * Deletes the given page from the stream storage.
    * 
    * @param page The page to delete.
    */
   protected void deletePage(PageInfo page)
   {
      try
      {
         getStreamTable().deleteStream(page.getStreamId());
      }
      catch (IOException ex)
      {
         dispose();
         ObjUtil.rethrow(ex);
      }
   }

   /**
    * Switches to the given page. First saves the currently active page, if any, then
    * loads the given page from disk.
    * 
    * @param newPage The page to swap to.
    */
   protected void swapTo(PageInfo newPage)
   {
      if (newPage == m_activePage)
      {
         return;
      }

      if (m_activePage != null)
      {
         swapPageOut(m_activePage);
      }

      if (newPage != null)
      {
         // Load the specified page
         StreamTable table = getStreamTable();
         InputStream istream = null;
         ObjectInputStream ois = null;

         try
         {
            istream = table.getInputStream(newPage.getStreamId());
            ois = new ObjectInputStream(istream);

            int nSize = ois.readInt();
            ArrayList pageDataList = new ArrayList(nSize);

            for (int i = 0; i < nSize; i++)
            {
               Object obj = ois.readObject();
               Object cached = m_externallyReferencedElements.get(newPage.getPageOffset() + i);

               pageDataList.add((cached != null) ? cached : obj);
            }

            newPage.setPageDataList(pageDataList);
         }
         catch (IOException ex)
         {
            dispose();
            ObjUtil.rethrow(ex);
         }
         catch (ClassNotFoundException ex)
         {
            ObjUtil.rethrow(ex);
         }
         finally
         {
            IOUtil.close(ois);
            IOUtil.close(istream);
         }

         m_activePage = newPage;
      }
      else
      {
         m_activePage = null;
      }
   }


   /**
    * Gets the StreamTable instance used for storing serialized pages.
    * Calls createStreamTable() to make the instance, if one hasn't yet
    * been made.
    * 
    * @return The StreamTable instance for storing pages.
    */
   protected StreamTable getStreamTable()
   {
      if (m_streamTable == null)
      {
         m_streamTable = createStreamTable();
      }

      return m_streamTable;
   }

   /**
    * Creates a new StreamTable instance for this PagedArrayList to use.
    * Will only be called once by the PagedArrayList. Can be overridden
    * by subclasses to substitute a better implementation for testing.
    * 
    * @return The StreamTable instance to use for this list.
    */
   protected StreamTable createStreamTable()
   {
      FileBackedStreamTable table = new FileBackedStreamTable();

      try
      {
         File tempDir = J2EEUtil.getTempDir();

         if (!tempDir.exists())
         {
            tempDir.mkdir();
         }

         File temp = File.createTempFile("pal", ".swp", tempDir);

         table.open(temp);
      }
      catch (IOException ex)
      {
         ObjUtil.rethrow(ex);
      }

      return table;
   }

   /**
    * Checks that this instance hasn't been disposed.
    * @throws IllegalStateException If this instance has been disposed.
    */
   protected void checkNotDisposed()
   {
      if (m_pageList == null)
      {
         throw new IllegalStateException("Paged array list has been disposed");
      }
   }

   /**
    * @see java.io.Serializable
    */
   private void writeObject(ObjectOutputStream out) throws IOException
   {
      checkNotDisposed();

      out.defaultWriteObject();
      out.writeInt(m_nSize);

      for (Iterator itr = iterator(); itr.hasNext(); )
      {
         out.writeObject(itr.next());
      }
   }

   /**
    * @see java.io.Serializable
    */
   private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException
   {
      in.defaultReadObject();

      int nSize = in.readInt();

      initEmptyList();

      // Get the elements from the stream
      for (int i = 0; i < nSize; i++)
      {
         add(in.readObject());
      }
   }

   /**
    * Checks to see if an index is on the active page; if not, swap in the correct page.
    * @param nIndex The index whose page should be made active.
    * @return The index of the new active page.
    */
   protected int swapToPageByIndex(int nIndex)
   {
      PageInfo page = m_activePage;
      int nPageNumber = -1;

      if (page == null || nIndex < page.getPageOffset() || nIndex >= (page.getPageOffset() + page.getPageSize()))
      {
         nPageNumber = findPageWithIndex(nIndex);
         swapTo((PageInfo)m_pageList.get(nPageNumber));
      }
      else
      {
         return m_pageList.indexOf(m_activePage);
      }

      return nPageNumber;
   }


   // ******************** java.util.List implementation ********************

   /**
    * @see java.util.List#add(int, java.lang.Object)
    */
   public void add(int nIndex, Object o)
   {
      checkNotDisposed();
      checkBoundsInclusive(nIndex);

      int nPageNumber = swapToPageByIndex(nIndex);
      PageInfo page = (nPageNumber >= 0) ? (PageInfo)m_pageList.get(nPageNumber) : m_activePage;

      if (nIndex == m_nSize)
      {
         // Append mode
         if (page.getPageSize() < m_nMaxPageSize)
         {
            swapTo(page);
         }
         else
         {
            // Make a new page, make it active, and put it as the last page
            swapTo(null);
            page = new PageInfo();
            m_pageList.add(page);
            nPageNumber = m_pageList.size() - 1;
            page.setPageOffset(m_nSize);

            m_activePage = page;
         }
      }

      if (page.getPageSize() < m_nMaxPageSize)
      {
         // Insert in active page
         page.getPageDataList().add(nIndex - page.getPageOffset(), o);
         page.setPageSize(page.getPageSize() + 1);
      }
      else
      {
         // Split page and insert
         int i;
         int nPageOffset = page.getPageOffset();
         int nOffsetInPage = nIndex - nPageOffset;
         int nMidOffset = m_nMaxPageSize >> 1;
         PageInfo residualPage = new PageInfo();

         ArrayList srcList = page.getPageDataList();
         ArrayList botList = new ArrayList(m_nMaxPageSize);
         ArrayList topList = new ArrayList(m_nMaxPageSize);

         for (i = 0; i < nMidOffset; i++)
         {
            botList.add(srcList.get(i));
         }

         for (; i < m_nMaxPageSize; i++)
         {
            topList.add(srcList.get(i));
         }

         PageInfo topPage = page;
         PageInfo botPage = residualPage;

         if (nOffsetInPage < nMidOffset)
         {
            // residualPage gets upper indices
            topPage = residualPage;
            botPage = page;
            m_pageList.add(nPageNumber + 1, residualPage);
         }
         else
         {
            m_pageList.add(nPageNumber, residualPage);
            nPageNumber++;
         }

         botPage.setPageOffset(nPageOffset);
         botPage.setPageDataList(botList);
         botPage.setPageSize(botList.size());

         topPage.setPageOffset(nPageOffset + botList.size());
         topPage.setPageDataList(topList);
         topPage.setPageSize(topList.size());

         // Serialize the residual page
         swapPageOut(residualPage);

         // No serialization of "page" is necessary because it is the active page
         page.getPageDataList().add(nIndex - page.getPageOffset(), o);
         page.setPageSize(page.getPageSize() + 1);
      }

      // Update offsets of subsequent pages
      for (int i = nPageNumber + 1; i < m_pageList.size(); i++)
      {
         page = (PageInfo)m_pageList.get(i);
         page.setPageOffset(page.getPageOffset() + 1);
      }

      // Register it with the list of known external elements
      m_externallyReferencedElements.insert(nIndex, o);

      m_nSize++;
   }

   /**
    * @see java.util.List#clear()
    */
   public void clear()
   {
      checkNotDisposed();

      if (m_streamTable != null)
      {
         m_streamTable.clear();
      }

      initEmptyList();
   }

   /**
    * @see java.util.List#get(int)
    */
   public Object get(int nIndex)
   {
      checkNotDisposed();
      checkBoundsExclusive(nIndex);

      int nPageNumber = swapToPageByIndex(nIndex);
      PageInfo page = (nPageNumber >= 0) ? (PageInfo)m_pageList.get(nPageNumber) : m_activePage;

      // Return element from the active page
      Object element = page.getPageDataList().get(nIndex - page.getPageOffset());

      m_externallyReferencedElements.set(nIndex, element);

      return element;
   }

   /**
    * @see java.util.List#remove(int)
    */
   public Object remove(int nIndex)
   {
      checkNotDisposed();
      checkBoundsExclusive(nIndex);

      int nPageNumber = swapToPageByIndex(nIndex);
      PageInfo page = (nPageNumber >= 0) ? (PageInfo)m_pageList.get(nPageNumber) : m_activePage;

      // Remove from active page and save (to return, as per Collections contract)
      Object result = page.getPageDataList().remove(nIndex - page.getPageOffset());
      int nNewSize = page.getPageSize() - 1;

      if (nNewSize > 0)
      {
         page.setPageSize(nNewSize);
      }
      else if (m_pageList.size() > 1)
      {
         deletePage(page);
         m_pageList.remove(nPageNumber);
         m_activePage = null;
         nPageNumber--;
      }
      else
      {
         page.setPageSize(0);
      }

      // Update offsets of subsequent pages
      for (int i = nPageNumber + 1; i < m_pageList.size(); i++)
      {
         page = (PageInfo)m_pageList.get(i);
         page.setPageOffset(page.getPageOffset() - 1);
      }

      m_nSize--;

      // Remove it from the list of known external elements
      m_externallyReferencedElements.remove(nIndex);

      return result;
   }

   /**
    * @see java.util.List#set(int, java.lang.Object)
    */
   public Object set(int nIndex, Object element)
   {
      checkNotDisposed();
      checkBoundsExclusive(nIndex);

      int nPageNumber = swapToPageByIndex(nIndex);
      PageInfo page = (nPageNumber >= 0) ? (PageInfo)m_pageList.get(nPageNumber) : m_activePage;

      // Register it with the list of known external elements
      m_externallyReferencedElements.set(nIndex, element);

      // Set new element and return old element
      return page.getPageDataList().set(nIndex - page.getPageOffset(), element);
   }

   /**
    * @see java.util.List#size()
    */
   public int size()
   {
      checkNotDisposed();
      return m_nSize;
   }


   // inner classes

   /**
    * A sparse cache of the elements in the list that have been given
    * to external clients. Each element has an associated index, and
    * when an element at an index is inserted or removed the other
    * indices in the cache are updated.
    */
   protected static class ListObjectCache
   {
      // constants

      /**
       * The minimum cache size.
       */
      protected final static int MINIMUM_SIZE = 4;


      // attributes

      /**
       * The number of nulled and non-null weak references in the cache.
       */
      protected int m_nSize;

      /**
       * The number of stale references in the cache.
       */
      protected int m_nStaleRefCount;


      // associations

      /**
       * The cache values.
       */
      protected Ref[] m_valueArray;

      /**
       * The reference queue.
       */
      protected ReferenceQueue m_refq = new ReferenceQueue();


      // constructors

      /**
       * Constructs a new cache instance.
       */
      public ListObjectCache()
      {
         m_valueArray = new Ref[MINIMUM_SIZE];
      }


      // operations

      /**
       * Removes the stale weak references, making the arrays smaller if possible.
       */
      protected void removeStaleReferences()
      {
         while (m_refq.poll() != null)
         {
            m_nStaleRefCount++;
         }

         if (m_nStaleRefCount > (m_nSize >> 1))
         {
            int nHalfSize = m_valueArray.length >> 1;

            if (nHalfSize >= MINIMUM_SIZE)
            {
               assert (m_nSize - m_nStaleRefCount) <= nHalfSize;

               Ref[] newValuesArray = new Ref[nHalfSize];
               int k = 0;

               for (int i = 0; i < m_nSize; i++)
               {
                  if (m_valueArray[i].get() != null)
                  {
                     newValuesArray[k] = m_valueArray[i];
                     k++;
                  }
                  else
                  {
                     m_nStaleRefCount--;
                  }
               }

               m_valueArray = newValuesArray;
               m_nSize = k;
            }
         }
      }

      /**
       * Replaces the element in the cache at the given index with the
       * given element. Does not shift the indices of higher-indexed elements.
       * 
       * @param nIndex The index to change.
       * @param element The new element for that index.
       */
      public void set(int nIndex, Object element)
      {
         removeStaleReferences();

         int nSparseIndex = getSparseIndexForIndex(nIndex);
         Ref value = new Ref(element, m_refq, nIndex);

         if (nSparseIndex < m_nSize && m_valueArray[nSparseIndex].getIndex() == nIndex)
         {
            if (m_valueArray[nSparseIndex].get() == null)
            {
               m_nStaleRefCount--;
            }

            m_valueArray[nSparseIndex] = value;
         }
         else
         {
            shiftInsert(nSparseIndex, 0);
            m_valueArray[nSparseIndex] = value;
         }
      }

      /**
       * Inserts an element into the cache at the given index, shifting the
       * indices of all higher-indexed elements.
       * 
       * @param nIndex The index at which the element should be inserted.
       * @param element The element to insert.
       */
      public void insert(int nIndex, Object element)
      {
         removeStaleReferences();

         int nSparseIndex = getSparseIndexForIndex(nIndex);
         Ref value = new Ref(element, m_refq, nIndex);

         shiftInsert(nSparseIndex, +1);
         m_valueArray[nSparseIndex] = value;
      }

      /**
       * Removes an element from the cache, shifting the indices of all
       * higher-indexed elements in the cache.
       * 
       * @param nIndex The index of the element to remove.
       */
      public void remove(int nIndex)
      {
         removeStaleReferences();

         int nSparseIndex = getSparseIndexForIndex(nIndex);

         if (m_valueArray[nSparseIndex].getIndex() == nIndex)
         {
            Ref removedRef = m_valueArray[nSparseIndex];

            for (int i = nSparseIndex; i < m_nSize - 1; i++)
            {
               m_valueArray[i] = m_valueArray[i + 1];
               m_valueArray[i].decIndex();
            }

            m_valueArray[m_nSize - 1] = null;

            if (removedRef.get() != null)
            {
               m_nSize--;
            }
            else
            {
               m_nSize--;
               m_nStaleRefCount--;
            }

            // Shrink arrays, if necessary
            int nHalfSize = m_valueArray.length >> 1;

            if (m_nSize < nHalfSize && nHalfSize >= MINIMUM_SIZE)
            {
               Ref[] newValuesArray = new Ref[nHalfSize];

               System.arraycopy(m_valueArray, 0, newValuesArray, 0, m_nSize);

               m_valueArray = newValuesArray;
            }
         }
      }

      /**
       * Gets the element for the given index from the cache.
       * 
       * @param nIndex The index of the element in the paged array list.
       * @return The element at that index; null if it is not in the cache.
       */
      public Object get(int nIndex)
      {
         removeStaleReferences();

         int nSparseIndex = getSparseIndexForIndex(nIndex);

         if (nSparseIndex < m_nSize && m_valueArray[nSparseIndex].getIndex() == nIndex)
         {
            return m_valueArray[nSparseIndex].get();
         }

         return null;
      }

      /**
       * Shifts the elements, starting at the given sparse index, to
       * the right by one position. Additionally, an adjustment value
       * may be added to the indices.
       * 
       * @param nSparseIndex The position in m_indexArray to shift.
       * @param nAdjustment The adjustment to apply to the indices that
       *                    are shifted.
       */
      protected void shiftInsert(int nSparseIndex, int nAdjustment)
      {
         // Expand the arrays first
         if (m_nSize == m_valueArray.length)
         {
            int nNewSize = m_valueArray.length << 1;
            Ref[] newValuesArray = new Ref[nNewSize];

            System.arraycopy(m_valueArray, 0, newValuesArray, 0, m_valueArray.length);

            m_valueArray = newValuesArray;
         }

         // Shift the entries
         for (int i = m_nSize - 1; i >= nSparseIndex; i--)
         {
            m_valueArray[i].incIndex(nAdjustment);
            m_valueArray[i + 1] = m_valueArray[i];
         }

         m_nSize++;
      }

      /**
       * Gets the position in the m_indexArray array of the given index.
       * If not found, returns the position at which the index should
       * be inserted.
       * 
       * @param nIndex The index to locate.
       * @return The position of that index.
       */
      protected int getSparseIndexForIndex(int nIndex)
      {
         int nBottomInclusive = 0;
         int nTopNonInclusive = m_nSize;
         int nSparseIndex = m_nSize >> 1;
         int nCurrentIndex;

         while (nBottomInclusive < nTopNonInclusive)
         {
            nCurrentIndex = m_valueArray[nSparseIndex].getIndex();

            if (nIndex < nCurrentIndex)
            {
               nTopNonInclusive = nSparseIndex;
               nSparseIndex = nBottomInclusive + ((nSparseIndex - nBottomInclusive) >> 1); 
            }
            else if (nIndex > nCurrentIndex)
            {
               nBottomInclusive = nSparseIndex + 1;
               nSparseIndex = nSparseIndex + ((nTopNonInclusive - nSparseIndex + 1) >> 1);
            }
            else
            {
               break;
            }
         }

         return nSparseIndex;
      }


      // inner classes

      /**
       * An indexed weak reference.
       */
      protected static class Ref extends WeakReference
      {
         // attributes

         /**
          * The index of this value.
          */
         protected int m_nIndex;


         // constructors

         /**
          * Constructs a new weak reference to a list cache object.
          * 
          * @param referent The cached object.
          * @param refq The reference queue.
          * @param nIndex The index for this value.
          */
         public Ref(Object referent, ReferenceQueue refq, int nIndex)
         {
            super(referent, refq);

            m_nIndex = nIndex;
         }


         // operations

         /**
          * Gets the index for this value.
          * 
          * @return The index at which this value is stored.
          */
         public int getIndex()
         {
            return m_nIndex;
         }

         /**
          * Increments the index at which this value is stored by the specified increment.
          * @param The amount by which to increment the index.
          */
         public void incIndex(int nIncrement)
         {
            m_nIndex += nIncrement;
         }

         /**
          * Decrements the index at which this value is stored.
          */
         public void decIndex()
         {
            m_nIndex--;
         }
      }
   }

   /**
    * Stores all the information about page, including its data.
    */
   protected class PageInfo
   {
      // constants

      /**
       * The value of a stream id that hasn't been set.
       */
      public final static long STREAM_UNSET = Long.MIN_VALUE;


      // attributes

      /**
       * The index of the first element of the page.
       */
      protected int m_nPageOffset;

      /**
       * The number of elements in the page.
       */
      protected int m_nPageSize;

      /**
       * The id of the stream to which this page's data are serialized.
       */
      protected long m_lStreamId;


      // associations

      /**
       * The elements stored in this page. This field is nulled when
       * the page is swapped to disk.
       */
      protected ArrayList m_pageDataList;


      // constructors

      /**
       * Creates a new page information object.
       */
      public PageInfo()
      {
         m_pageDataList = new ArrayList(m_nMaxPageSize);
         m_lStreamId = STREAM_UNSET;
      }


      // operations

      /**
       * Gets the list of elements stored in this page.
       * 
       * @return The elements stored in this page.
       */
      public ArrayList getPageDataList()
      {
         return m_pageDataList;
      }

      /**
       * Sets the list of elements stored in this page.
       * 
       * @param list The elements stored in this page.
       */
      public void setPageDataList(ArrayList list)
      {
         m_pageDataList = list;
      }

      /**
       * Sets the index of the first element in this page.
       * 
       * @param nPageOffset The index of the first element.
       */
      public void setPageOffset(int nPageOffset)
      {
         m_nPageOffset = nPageOffset;
      }

      /**
       * Gets the index of the first element in this page.
       * 
       * @return The index of the first element.
       */
      public int getPageOffset()
      {
         return m_nPageOffset;
      }

      /**
       * Sets the number of elements in this page.
       * 
       * @param nPageSize The size of the page.
       */
      public void setPageSize(int nPageSize)
      {
         m_nPageSize = nPageSize;
      }

      /**
       * Gets the number of elements in this page.
       * 
       * @return The size of the page.
       */
      public int getPageSize()
      {
         return m_nPageSize;
      }

      /**
       * Sets the id of the stream to which this page's data are serialized.
       * 
       * @param lStreamId The stream id for this page's data.
       */
      public void setStreamId(long lStreamId)
      {
         m_lStreamId = lStreamId;
      }

      /**
       * Gets the id of the stream to which this page's data are serialized.
       * 
       * @return The stream id for this page's data.
       */
      public long getStreamId()
      {
         return m_lStreamId;
      }

      /**
       * String value for debugging.
       * 
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         StringBuilder buf = new StringBuilder();

         buf.append("Page for elements ");
         buf.append(m_nPageOffset);
         buf.append(" to ");
         buf.append(m_nPageOffset + m_nPageSize - 1);
         buf.append(" (inclusive). Size is ");
         buf.append(m_nPageSize);

         return buf.toString();
      }
   }

   /**
    * Provides access to byte streams, indexed by a stream id.
    */
   public static interface StreamTable extends Closeable
   {
      /**
       * Gets an input stream with the given id from the table.
       * 
       * @param lStreamId The id of the stream to retrieve.
       * @return The stream; null if no stream available for that id.
       */
      public InputStream getInputStream(long lStreamId) throws IOException;

      /**
       * Gets an output stream with the given id from the table. If no
       * stream of the given id exists, it is created.
       * 
       * @param lStreamId The id of the stream to retrieve.
       * @return The stream.
       */
      public OutputStream getOutputStream(long lStreamId) throws IOException;

      /**
       * Deletes the stream with the given id.
       * 
       * @param lStreamId The id of the stream to delete.
       */
      public void deleteStream(long lStreamId) throws IOException;

      /**
       * Gets an unused stream id.
       * 
       * @return An unused stream id.
       */
      public long getFreeStreamId();

      /**
       * Clear all of the byte streams in the table.
       */
      public void clear();
   }

   /**
    * A StreamTable implementation that stores the stream data in a file.
    */
   public static class FileBackedStreamTable implements StreamTable
   {
      // constants

      /**
       * The default block size, in bytes.
       */
      public final static int DEFAULT_BLOCK_SIZE = 8192;

      /**
       * A marker that indicates that a block is empty.
       */
      public final static Integer EMPTY_BLOCK = Integer.valueOf(Integer.MIN_VALUE);


      // attributes

      /**
       * The size of the blocks in which data are stored on disk. This is
       * the smallest unit of allocation. Measured in bytes.
       */
      protected int m_nBlockSize;

      /**
       * The next free stream id.
       */
      protected long m_lFreeStreamId;


      // associations

      /**
       * Mapping from stream ID number to the number of the first block
       * in that stream.
       */
      protected Lookup m_streamIdBlockLookup;

      /**
       * Mapping from a block number to the number of the next block
       * in the chain or negative to end the chain. If negative, the
       * absolute value of the negative number gives the number of bytes
       * stored in the block.
       */
      protected List m_chainPointerList;

      /**
       * The block numbers corresponding to free blocks in the file.
       * Free blocks at the end of the file are not added to this list;
       * the file is shrunk.
       */
      protected Heap m_freeBlockHeap;

      /**
       * The file in which the streams will be stored.
       */
      protected File m_file;

      /**
       * A set of references to all the streams issued by the stream table.
       */
      protected WeakHashHolder m_issuedStreamsHolder;

      /**
       * The class logger.
       */
      protected static Logger s_logger = Logger.getLogger(FileBackedStreamTable.class);


      // constructors

      /**
       * Creates a new file-backed stream table using the default block size.
       */
      public FileBackedStreamTable()
      {
         this(DEFAULT_BLOCK_SIZE);
      }

      /**
       * Creates a new file-backed stream table using the specified block size.
       * 
       * @param nBlockSize The size of the allocation blocks, in bytes.
       */
      public FileBackedStreamTable(int nBlockSize)
      {
         m_nBlockSize = nBlockSize;
      }


      // operations

      /**
       * @see nexj.core.util.PagedArrayList.StreamTable#getFreeStreamId()
       */
      public long getFreeStreamId()
      {
         return m_lFreeStreamId;
      }

      /**
       * Checks that this stream table is open.
       * @throws IOException If not open.
       */
      protected void checkOpen() throws IOException
      {
         if (m_file == null)
         {
            throw new IOException("Stream table closed");
         }
      }

      /**
       * Opens the given file to use as the backing store for this stream table.
       * 
       * @param file The file to use to store the stream data.
       */
      public void open(File file)
      {
         if (m_file != null)
         {
            throw new IllegalStateException("Already open");
         }

         m_streamIdBlockLookup = new HashTab();
         m_chainPointerList = new ArrayList();
         m_freeBlockHeap = new BinaryHeap(new Comparator() {
            public int compare(Object left, Object right)
            {
               return ((Comparable)left).compareTo(right);
            }
         });
         m_issuedStreamsHolder = new WeakHashHolder();
         m_file = file;
      }

      /**
       * @see nexj.core.util.PagedArrayList.StreamTable#clear()
       */
      public void clear()
      {
         m_streamIdBlockLookup.clear();
         m_chainPointerList.clear();
         m_freeBlockHeap.clear();
         m_issuedStreamsHolder.clear();
         m_lFreeStreamId = 0;

         try
         {
            RandomAccessFile handle = new RandomAccessFile(m_file, "rw");

            handle.getChannel().truncate(0);
            handle.close();
         }
         catch (IOException e)
         {
            ObjUtil.rethrow(e);
         }
      }

      /**
       * @see nexj.core.util.PagedArrayList.StreamTable#deleteStream(long)
       */
      public void deleteStream(long lStreamId) throws IOException
      {
         checkOpen();

         Integer block = (Integer)m_streamIdBlockLookup.remove(Long.valueOf(lStreamId));

         while (block != null && block.intValue() >= 0)
         {
            m_freeBlockHeap.add(block);
            block = (Integer)m_chainPointerList.set(block.intValue(), EMPTY_BLOCK);
         }

         compactFreeSpaceAtEnd();
      }

      /**
       * @see nexj.core.util.PagedArrayList.StreamTable#getInputStream(long)
       */
      public InputStream getInputStream(long lStreamId) throws IOException
      {
         checkOpen();

         Integer block = (Integer)m_streamIdBlockLookup.get(Long.valueOf(lStreamId));

         if (block != null)
         {
            InputStream result = new StreamTableInputStream(block.intValue());

            m_issuedStreamsHolder.add(result);
            return result;
         }

         return null;
      }

      /**
       * @see nexj.core.util.PagedArrayList.StreamTable#getOutputStream(long)
       */
      public OutputStream getOutputStream(long lStreamId) throws IOException
      {
         checkOpen();

         if (lStreamId >= m_lFreeStreamId)
         {
            m_lFreeStreamId = lStreamId + 1;
         }

         Long streamId = Long.valueOf(lStreamId);

         // Remove chain for old stream
         Integer block = (Integer)m_streamIdBlockLookup.remove(streamId);

         while (block != null && block.intValue() >= 0)
         {
            m_freeBlockHeap.add(block);
            block = (Integer)m_chainPointerList.set(block.intValue(), EMPTY_BLOCK);
         }

         // Create entries for new stream
         block = Integer.valueOf(getNextFreeBlock());
         m_streamIdBlockLookup.put(streamId, block);

         OutputStream result = new StreamTableOutputStream(block.intValue());

         m_issuedStreamsHolder.add(result);
         return result;
      }

      /**
       * Last-ditch effort to clean up the resource.
       */
      protected void finalize() throws Throwable
      {
         close();
      }

      /**
       * @see java.io.Closeable#close()
       */
      public void close() throws IOException
      {
         // Close all streams that have been issued
         if (m_issuedStreamsHolder != null)
         {
            for (Iterator itr = m_issuedStreamsHolder.iterator(); itr.hasNext(); )
            {
               Closeable stream = (Closeable)itr.next();
   
               if (stream != null)
               {
                  stream.close();
               }
            }
         }

         // Deletes the current backing file from disk.
         if (m_file != null && m_file.exists())
         {
            if (!m_file.delete())
            {
               if (s_logger.isWarnEnabled())
               {
                  s_logger.warn("Unable to delete \"" + m_file.getAbsolutePath() + "\"; scheduling for deletion on exit.");
               }

               m_file.deleteOnExit();
            }
         }

         m_file = null;
         m_chainPointerList = null;
         m_freeBlockHeap = null;
         m_streamIdBlockLookup = null;
         m_issuedStreamsHolder = null;
      }

      /**
       * Removes any free blocks that are at the end of the file by
       * shrinking the file.
       * 
       * @throws IOException If an I/O error occurs.
       */
      protected void compactFreeSpaceAtEnd() throws IOException
      {
         checkOpen();

         boolean bChanged = false;

         while (m_chainPointerList.size() > 0 &&
            m_chainPointerList.get(m_chainPointerList.size() - 1) == EMPTY_BLOCK)
         {
            Object block = m_chainPointerList.remove(m_chainPointerList.size() - 1);

            m_freeBlockHeap.remove(block);
            bChanged = true;
         }

         if (bChanged)
         {
            RandomAccessFile handle = new RandomAccessFile(m_file, "rw");

            try
            {
               handle.setLength((long)m_chainPointerList.size() * (long)m_nBlockSize);
            }
            finally
            {
               handle.close();
            }
         }
      }

      /**
       * Gets the number of a free block to use for storing data.
       * 
       * @return The number of the next free block in the file.
       * @throws IOException If an I/O error occurs.
       */
      protected int getNextFreeBlock() throws IOException
      {
         checkOpen();

         if (m_freeBlockHeap.size() > 0)
         {
            Integer firstBlock = (Integer)m_freeBlockHeap.removeFirst();

            return firstBlock.intValue();
         }

         m_chainPointerList.add(EMPTY_BLOCK);

         return m_chainPointerList.size() - 1;
      }

      /**
       * Gets the block after the given block in the chain.
       * 
       * @param nCurrentBlock The number of a block in the chain.
       * @return The number of the next block in the chain; or a negative number
       *         indicating that the given block is the last block in the chain.
       *         The absolute value of the negative number will be the number of
       *         bytes left in the chain.
       */
      protected int getNextBlock(int nCurrentBlock)
      {
         Integer value = (Integer)m_chainPointerList.get(nCurrentBlock);

         assert value != EMPTY_BLOCK;

         return value.intValue();
      }

      /**
       * Sets the number of the block that should follow the given block number
       * in the chain.
       * 
       * @param nCurrentBlock The number of a block.
       * @param nNextBlock    The number of the block following that block.
       */
      protected void setNextBlock(int nCurrentBlock, int nNextBlock)
      {
         if (nCurrentBlock >= m_chainPointerList.size())
         {
            m_chainPointerList.add(EMPTY_BLOCK);
         }

         m_chainPointerList.set(nCurrentBlock, Integer.valueOf(nNextBlock));
      }


      // inner classes

      /**
       * The OutputStream implementation returned by the FileBackedStreamTable.
       */
      protected class StreamTableOutputStream extends OutputStream
      {
         // attributes

         /**
          * The number of the current block of data.
          */
         protected int m_nCurrentBlock;

         /**
          * The offset into the current block of data.
          */
         protected int m_nCurrentOffset;


         // associations

         /**
          * The file that contains this stream's data.
          */
         protected RandomAccessFile m_handle;

         /**
          * The channel through which the file is to be accessed.
          */
         protected FileChannel m_channel;

         /**
          * The block buffer.
          */
         protected ByteBuffer m_buffer;

         /**
          * Saved exception from underlying channel.
          */
         protected IOException m_channelException;

         // constructors

         /**
          * Creates a new output stream that starts writing at the given block number.
          * 
          * @param nStartBlock The number of the block at which to begin writing data.
          * @throws IOException If an I/O error occurs.
          */
         public StreamTableOutputStream(int nStartBlock) throws IOException
         {
            m_handle = new RandomAccessFile(m_file, "rw");
            m_channel = m_handle.getChannel();
            m_buffer = ByteBuffer.allocateDirect(m_nBlockSize);
            m_nCurrentBlock = nStartBlock;
            m_nCurrentOffset = 0;
         }


         // operations

         /**
          * Writes the current data block to disk.
          * 
          * @throws IOException If an I/O error occurs.
          */
         protected void writeCurrentBlock() throws IOException
         {
            m_buffer.flip();
            assert m_buffer.limit() == m_nCurrentOffset;

            try
            {
               m_channel.write(m_buffer, (long)m_nCurrentBlock * (long)m_nBlockSize);
            }
            catch (IOException ex)
            {
               m_channelException = ex;
               m_handle.close();
               m_handle = null;
               m_channel.close();
               m_channel = null;

               setNextBlock(m_nCurrentBlock, 0);

               throw ex;
            }

            m_buffer.rewind();
         }

         /**
          * @see java.io.OutputStream#write(int)
          */
         public void write(int nByte) throws IOException
         {
            if (m_channelException != null)
            {
               throw m_channelException;
            }

            if (m_handle == null)
            {
               throw new IOException("Stream closed");
            }

            if (m_nCurrentOffset < m_nBlockSize)
            {
               m_buffer.put((byte)nByte);
               m_nCurrentOffset++;
            }
            else
            {
               writeCurrentBlock();

               // Get new block and update chain pointers
               int nNextBlock = getNextFreeBlock();

               setNextBlock(m_nCurrentBlock, nNextBlock);

               // Begin writing data to new block
               m_nCurrentBlock = nNextBlock;
               m_nCurrentOffset = 0;
               this.write(nByte);
            }
         }

         /**
          * @see java.io.OutputStream#close()
          */
         public void close() throws IOException
         {
            if (m_handle != null)
            {
               writeCurrentBlock();

               m_handle.close();
               m_handle = null;
               m_channel.close();
               m_channel = null;

               setNextBlock(m_nCurrentBlock, -m_nCurrentOffset);

               compactFreeSpaceAtEnd();
            }
         }

         /**
          * Cleans up the resources held by this stream.
          * 
          * @see java.lang.Object#finalize()
          */
         protected void finalize() throws Throwable
         {
            close();
         }
      }


      /**
       * The InputStream implementation returned by the FileBackedStreamTable.
       */
      protected class StreamTableInputStream extends InputStream
      {
         // attributes

         /**
          * The number of the next block of data in the stream.
          */
         protected int m_nNextBlock;

         /**
          * The offset into the current block of data.
          */
         protected int m_nCurrentOffset;

         /**
          * The length of the current block of data.
          */
         protected int m_nCurrentBlockSize;


         // associations

         /**
          * The current block of data.
          */
         protected byte[] m_nBlockDataArray;

         /**
          * The file that contains this stream's data.
          */
         protected RandomAccessFile m_handle;


         // constructors

         /**
          * Creates a new input stream that starts from the given block number.
          * 
          * @param nStartBlock The number of the block containing the beginning
          *                    of the stream's data.
          * @throws IOException If an I/O error occurs.
          */
         public StreamTableInputStream(int nStartBlock) throws IOException
         {
            m_nBlockDataArray = new byte[m_nBlockSize];
            m_handle = new RandomAccessFile(m_file, "r");
            advanceToBlock(nStartBlock);
         }


         // operations

         /**
          * Makes the argument the current block by reading it into memory
          * and setting the m_nNextBlock field to the next block to read.
          * 
          * @param nBlock The number of the block to read.
          * @throws IOException If an I/O error occurs.
          */
         protected void advanceToBlock(int nBlock) throws IOException
         {
            m_nNextBlock = getNextBlock(nBlock);
            m_nCurrentOffset = 0;

            if (m_nNextBlock > 0)
            {
               m_nCurrentBlockSize = m_nBlockSize;
            }
            else
            {
               m_nCurrentBlockSize = -m_nNextBlock;
            }

            m_handle.seek((long)nBlock * (long)m_nBlockSize);

            for (int nTotalBytesRead = 0, nBytesRead; nTotalBytesRead < m_nCurrentBlockSize;
                  nTotalBytesRead += nBytesRead)
            {
               nBytesRead = m_handle.read(m_nBlockDataArray, nTotalBytesRead, m_nCurrentBlockSize -
                     nTotalBytesRead);

               if (nBytesRead < 0)
               {
                  throw new IOException("Unexpected end of file");
               }
            }
         }

         /**
          * @see java.io.InputStream#read()
          */
         public int read() throws IOException
         {
            if (m_handle == null)
            {
               return -1;
            }

            if (m_nCurrentOffset < m_nCurrentBlockSize)
            {
               return m_nBlockDataArray[m_nCurrentOffset++] & 0xff;
            }
            else if (m_nNextBlock > 0)
            {
               advanceToBlock(m_nNextBlock);
               return m_nBlockDataArray[m_nCurrentOffset++] & 0xff;
            }
            else
            {
               // EOF
               m_handle.close();
               m_handle = null;
               return -1;
            }
         }

         /**
          * @see java.io.InputStream#close()
          */
         public void close() throws IOException
         {
            if (m_handle != null)
            {
               m_handle.close();
               m_handle = null;
            }
         }

         /**
          * Cleans up the resources held by this stream.
          * 
          * @see java.lang.Object#finalize()
          */
         protected void finalize() throws Throwable
         {
            close();
         }
      }
   }
}
