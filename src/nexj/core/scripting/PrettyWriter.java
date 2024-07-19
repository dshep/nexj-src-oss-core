package nexj.core.scripting;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * The PrettyWriter class implementation.
 * Formats and writes Scheme expression over multiple lines.
 */
public class PrettyWriter extends FormattingWriter
{
   // attributes

   /**
    * The maximum line length, including indentation.
    */
   protected int m_nLineLength;

   /**
    * The character count threshold, excluding indentation, above which line breaks are allowed.
    */
   protected int m_nBreakThreshold;

   /**
    * The number of characters for the first indentation level.
    */
   protected int m_nFirstIndent;

   /**
    * The number of characters per indentation level.
    */
   protected int m_nIncrIndent;

   /**
    * The index of the last line break inserted into the buffer.
    */
   protected int m_nLastLineBreak;

   /**
    * The minimum capacity of the writer.
    */
   protected int m_nCapacity;

   // associations

   /**
    * The internal buffer.
    */
   protected StringWriter m_bufferWriter;

   /**
    * The writer currently in use, either the underlying output writer or the internal buffer.
    */
   protected Writer m_currentWriter;

   /**
    * The buffer for special characters preceding an open parenthesis.
    */
   protected final StringBuffer m_tokenBuf;

   /**
    * The head of the Marker linked list. May be null.
    */
   protected Marker m_firstMarker;

   /**
    * The tail of the Marker linked list. May be null.
    */
   protected Marker m_currentMarker;

   /**
    * The last OpenMarker created. May be null.
    */
   protected OpenMarker m_currentOpenMarker;

   // constructors

   /**
    * Create a new pretty-print writer with the default settings: maximum 80 characters
    * per line (including indentation), more than 30 characters (excluding indentation)
    * required for multiple line formatting, and 3 spaces per indentation level.
    * @param writer The writer to output to.
    * @param bnitSepEnabled True to automatically add a line break separator between code units.
    */
   public PrettyWriter(Writer writer, boolean bnitSepEnabled)
   {
      this(writer, 80, 30, 3, SysUtil.LINE_SEP, bnitSepEnabled, 256);
   }

   /**
    * Create a new pretty-print writer.
    * @param writer The writer to output to.
    * @param nLineLength The maximum character count per line, including indentation.
    * @param nThreshold The character count threshold, excluding indentation, above
    * which line breaks are allowed.
    * @param nIndentCount The number of indentation characters per indentation level.
    * Must be a positive number.
    * @param bUnitSepEnabled True to automatically add a line break separator between code units.
    */
   public PrettyWriter(Writer writer, int nLineLength, int nThreshold, int nIndentCount, boolean bUnitSepEnabled)
   {
      this(writer, nLineLength, nThreshold, nIndentCount, SysUtil.LINE_SEP, bUnitSepEnabled, 256);
   }

   /**
    * Create a new pretty-print writer.
    * @param writer The writer to output to.
    * @param nLineLength The maximum character count per line, including indentation.
    * @param nThreshold The character count threshold, excluding indentation, above
    * which line breaks are allowed.
    * @param nIndentCount The number of indentation characters per indentation level.
    * Must be a positive number.
    * @param sLineSep The line separator string.
    * @param bUnitSepEnabled True to automatically add a line break separator between code units.
    * @param nInitialCapacity The minimum capacity of the writer.
    */
   public PrettyWriter(Writer writer, int nLineLength, int nThreshold, int nIndentCount,
      String sLineSep, boolean bUnitSepEnabled, int nInitialCapacity)
   {
      super(writer, bUnitSepEnabled);

      assert nIndentCount > 0;

      m_currentWriter = writer;
      m_nLineLength = nLineLength;
      m_nBreakThreshold = nThreshold;
      m_nIncrIndent = nIndentCount;
      m_nCapacity = nInitialCapacity;
      m_sUnitSep = m_sLineSep = sLineSep;
      m_tokenBuf = new StringBuffer(5);
   }

   // operations

   /**
    * Ensure the minimum capacity of the output character stream.
    * @param nCapacity The minimum capacity.
    */
   public void ensureCapacity(int nCapacity)
   {
      m_nCapacity = nCapacity;

      if (m_bufferWriter != null)
      {
         m_bufferWriter.getBuffer().ensureCapacity(nCapacity);
      }

      if (m_outputWriter instanceof StringWriter)
      {
         ((StringWriter)m_outputWriter).getBuffer().ensureCapacity(nCapacity);
      }
   }

   /**
    * @see nexj.core.scripting.FormattingWriter#getLineLength()
    */
   public int getLineLength()
   {
      return m_nLineLength;
   }

   /**
    * @see nexj.core.scripting.FormattingWriter#getLineBreakThreshold()
    */
   public int getLineBreakThreshold()
   {
      return m_nBreakThreshold;
   }

   /**
    * @see nexj.core.scripting.FormattingWriter#getIndentation()
    */
   public int getIndentation()
   {
      return m_nIncrIndent;
   }

   /**
    * @see nexj.core.scripting.FormattingWriter#setLineLength(int)
    */
   public void setLineLength(int nLineLength)
   {
      m_nLineLength = nLineLength;
   }

   /**
    * @see nexj.core.scripting.FormattingWriter#setLineBreakThreshold(int)
    */
   public void setLineBreakThreshold(int nThreshold)
   {
      m_nBreakThreshold = nThreshold;
   }

   /**
    * @see nexj.core.scripting.FormattingWriter#setLineSeparator(java.lang.String)
    */
   public void setLineSeparator(String sLineSep)
   {
      m_sUnitSep = m_sLineSep = sLineSep;
   }

   /**
    * Force line breaks into the current code unit.
    */
   public void forceLineBreaks()
   {
      if (m_currentOpenMarker != null)
      {
         m_currentOpenMarker.forceLineBreaks();
      }
   }

   /**
    * Sets the indentation space count of the first code unit.
    * @param nIndent The number of indentation spaces.
    */
   public void setFirstIndentation(int nIndent)
   {
      assert nIndent >= 0;
      m_nFirstIndent = nIndent;
   }

   /**
    * @return The current indentation space count.
    */
   public int getCurrentIndentation()
   {
      return (m_currentOpenMarker == null) ? m_nFirstIndent : m_currentOpenMarker.getIndentation() + m_nIncrIndent;
   }

   /**
    * @see nexj.core.scripting.FormattingWriter#setIndentation(int)
    */
   public void setIndentation(int nCount)
   {
      assert nCount > 0;
      m_nIncrIndent = nCount;
   }

   /**
    * Returns true if the characters written since the last OpenMarker (including
    * indentation) fits into one line. If no OpenMarker exists, returns false.
    */
   protected boolean fitsLine()
   {
      if (m_currentOpenMarker == null)
      {
         return false;
      }

      return m_bufferWriter.getBuffer().length() - m_currentOpenMarker.getIndex()
             + m_currentOpenMarker.getIndentation() <= m_nLineLength;
   }

   /**
    * Sets the given marker to be obsolete. This effectively means
    * no line break will be inserted to this position.
    * @param marker The Marker object to set.
    */
   protected void markObsolete(Object marker)
   {
      assert marker instanceof Marker;

      ((Marker)marker).markObsolete();
   }

   /**
    * Lazily instantiates and returns a buffer writer.
    */
   protected StringWriter getBufferWriter()
   {
      if (m_bufferWriter == null)
      {
         m_bufferWriter = new StringWriter(m_nCapacity);
      }

      return m_bufferWriter;
   }

   /**
    * Creates a new OpenMarker.
    */
   protected void createOpenToken()
   {
      if (m_currentMarker == null)
      {
         m_currentWriter = getBufferWriter();
         m_firstMarker = m_currentMarker = m_currentOpenMarker = new OpenMarker(0, null, m_currentOpenMarker);
      }
      else
      {
         m_currentOpenMarker = new OpenMarker(m_bufferWriter.getBuffer().length(), m_currentMarker, m_currentOpenMarker);
         m_currentMarker = m_currentMarker.setNext(m_currentOpenMarker);
      }
   }

   /**
    * @see nexj.core.scripting.FormattingWriter#writeOpenToken(char)
    */
   public void writeOpenToken(char ch) throws IOException
   {
      createOpenToken();
      flushSpecialTokens();
      write(ch);
   }

   /**
    * @see nexj.core.scripting.FormattingWriter#writeOpenToken(java.lang.String)
    */
   public void writeOpenToken(String sToken) throws IOException
   {
      createOpenToken();
      flushSpecialTokens();
      write(sToken);
   }

   /**
    * Removes the last OpenMarker and rollbacks to the Marker right before it.
    */
   protected void rollbackOpenMarker()
   {
      m_currentMarker = m_currentOpenMarker.getPrevious();

      if (m_currentMarker == null)
      {
         m_firstMarker = null;
      }
      else
      {
         m_currentMarker.setNext(null);
      }
   }

   /**
    * @see nexj.core.scripting.FormattingWriter#writeCloseToken(char)
    */
   public void writeCloseToken(char ch) throws IOException
   {
      if (m_currentOpenMarker == null)
      {
         write(ch);
      }
      else
      {
         boolean bRequireLineBreaks = m_currentOpenMarker.isForcedLineBreak();

         if (m_currentMarker instanceof OpenMarker && !bRequireLineBreaks)
         {
            // don't break unit that doesn't "branch out", e.g. ((((a)))), even when it's long.
            rollbackOpenMarker();
         }
         else
         {
            Marker closeMarker = new CloseMarker(m_bufferWriter.getBuffer().length(), m_currentOpenMarker);

            if (m_currentOpenMarker.isLineBreak())
            {
               m_currentMarker = m_currentMarker.setNext(closeMarker);
            }
            else
            {
               rollbackOpenMarker();
            }
         }

         write(ch);
         m_currentOpenMarker = m_currentOpenMarker.getPreviousOpen();

         if (m_currentOpenMarker == null)
         {
            writeAll();
            m_currentWriter = m_outputWriter;
         }
         else if (bRequireLineBreaks)
         {
            m_currentOpenMarker.forceLineBreaks(); // propagate requirement up to parent 
         }
      }
   }

   /**
    * @see nexj.core.scripting.FormattingWriter#writeSeparator(boolean)
    */
   public void writeSeparator(boolean bSpaceRequired) throws IOException
   {
      if (m_currentOpenMarker != null)
      {
         m_currentMarker = m_currentMarker.setNext(
            new SeparatorMarker(m_bufferWriter.getBuffer().length(), m_currentOpenMarker, bSpaceRequired));
      }

      super.writeSeparator(bSpaceRequired);
   }

   /**
    * Removes the current marker if it is a SeparatorMarker and no characters have been written since this marker.
    * This effectively deletes the space or line break just inserted to the buffer stream.
    */
   protected void rollbackSeparatorMarker()
   {
      if (m_currentMarker instanceof SeparatorMarker)
      {
         assert m_bufferWriter != null;

         SeparatorMarker marker = (SeparatorMarker)m_currentMarker;
         int nIndex = marker.getIndex();
         int nCharCount = marker.getCharCount();
         StringBuffer buf = m_bufferWriter.getBuffer();

         if (nIndex + nCharCount == buf.length())
         {
            buf.setLength(nIndex);
         }

         m_currentMarker.markObsolete();
         m_bUnitSepRequired = false;
      }
   }

   /**
    * @see nexj.core.scripting.FormattingWriter#writeSpecialToken(java.lang.String)
    */
   public void writeSpecialToken(String sToken)
   {
      m_tokenBuf.append(sToken);  
   }

   /**
    * @see nexj.core.scripting.FormattingWriter#flushSpecialTokens()
    */
   public void flushSpecialTokens() throws IOException
   {
      if (m_tokenBuf.length() > 0)
      {
         write(m_tokenBuf.toString());
         m_tokenBuf.setLength(0);
      }
   }

   /**
    * Writes a comment and ensures that the code unit will have line breaks.
    * @see nexj.core.scripting.FormattingWriter#writeComment(nexj.core.scripting.Comment)
    */
   protected void writeComment(Comment comment) throws IOException
   {
      forceLineBreaks();
      comment.writeFormatted(m_currentWriter, getCurrentIndentation(), m_nLineLength, m_nBreakThreshold, m_sLineSep, false);
   }

   /**
    * Writes the entire buffer to the writer and clears the buffer.
    */
   protected void writeAll() throws IOException
   {
      StringBuffer buf;

      if (m_bufferWriter == null || (buf = m_bufferWriter.getBuffer()).length() == 0)
      {
         m_nLastLineBreak = 0;
      }
      else
      {
         int nLastWritten = 0;

         for (Marker cur = m_firstMarker; cur != null; cur = cur.getNext())
         {
            if (cur.isObsolete())
            {
               continue;
            }

            int nIndex = cur.getIndex();

            if (m_nLastLineBreak != nIndex)
            {
               m_outputWriter.write(buf.substring(nLastWritten, nIndex));
               nLastWritten = nIndex;

               m_outputWriter.write(m_sLineSep);
               StringUtil.writeSpaces(m_outputWriter, cur.getIndentation());
               m_nLastLineBreak = nIndex;

               if (cur instanceof SeparatorMarker)
               {
                  // account for the space already inserted
                  m_nLastLineBreak += ((SeparatorMarker)cur).getCharCount();
               }
            }
         }

         if (nLastWritten < buf.length())
         {
            m_outputWriter.write(buf.substring(nLastWritten));
         }

         buf.setLength(0);
         m_firstMarker = m_currentMarker = null;
         m_nLastLineBreak = 0;
      }
   }

   /**
    * @see java.io.FilterWriter#flush()
    */
   public void flush() throws IOException
   {
      flushSpecialTokens();
      writeAll();
      m_outputWriter.flush();
   }

   /**
    * @see nexj.core.scripting.FormattingWriter#clear()
    */
   public void clear()
   {
      super.clear();

      if (m_bufferWriter != null)
      {
         m_bufferWriter.getBuffer().setLength(0);
      }
   }

   /**
    * @see java.io.FilterWriter#write(char[], int, int)
    */
   public void write(char[] buf, int nOffset, int nLength) throws IOException
   {
      m_currentWriter.write(buf, nOffset, nLength);
   }

   /**
    * @see java.io.FilterWriter#write(int)
    */
   public void write(int ch) throws IOException
   {
      m_currentWriter.write(ch);
   }

   /**
    * @see java.io.Writer#write(java.lang.String)
    */
   public void write(String sValue) throws IOException
   {
      m_currentWriter.write(sValue);
   }

   /**
    * @see nexj.core.scripting.FormattingWriter#indent(java.lang.Object, int)
    */
   protected void indent(Object expr, int nExistingIndent) throws IOException
   {
      StringUtil.writeSpaces(m_outputWriter, m_nFirstIndent - nExistingIndent);
   }

   /**
    * Marker class implementation.
    */
   protected abstract class Marker
   {
      // attributes

      /**
       * Marker index.
       */
      protected final int m_nIndex;

      // associations

      /**
       * The next Marker, if any.
       */
      protected Marker m_next;

      /**
       * Whether this Marker should be ignored.
       */
      protected boolean m_bObsolete;

      // constructors

      /**
       * Creates a new Marker.
       * @param nIndex The index into the buffer.
       */
      protected Marker(int nIndex)
      {
         m_nIndex = nIndex;
      }

      // operations

      /**
       * @return The index of this marker in the buffer.
       */
      protected int getIndex()
      {
         return m_nIndex;
      }

      /**
       * @return The next marker.
       */
      protected Marker getNext()
      {
         return m_next;
      }

      /**
       * Sets and returns the next marker. 
       * @param next The next marker.
       * @return The next marker.
       */
      protected Marker setNext(Marker next)
      {
         return m_next = next;
      }

      /**
       * @return Whether this Marker should be ignored.
       */
      protected boolean isObsolete()
      {
         return m_bObsolete;
      }

      /**
       * Sets this Marker to be obsolete. This effectively means no line break
       * will be inserted to this position.
       */
      protected void markObsolete()
      {
         m_bObsolete = true;
      }

      /**
       * @return The indentation character count at this marker, if a line break is needed.
       */
      protected abstract int getIndentation();
   }

   /**
    * OpenMarker class implementation.
    */
   protected class OpenMarker extends Marker
   {
      // attributes

      /**
       * The indentation character count at this marker if the unit should be on its own line.
       */
      protected final int m_nIndent;

      /**
       * The index of the matching close token.
       */
      protected int m_nEndIndex;

      /**
       * True if the unit to which this marker belongs is the head of another unit (Pair, etc.).
       */
      protected final boolean m_bHead;

      /**
       * True if the code unit must have line breaks.
       */
      protected boolean m_bForceLineBreaks;

      // associations

      /**
       * The previous Marker.
       */
      protected Marker m_prev;

      /**
       * The previous OpenMarker.
       */
      protected OpenMarker m_prevOpen;

      // constructors

      /**
       * Creates a new OpenMarker.
       * @param nIndex The index into the buffer.
       * @param prevMarker The previous Marker. May be null.
       * @param prevOpenMarker The previous OpenMarker. May be null. 
       */
      protected OpenMarker(int nIndex, Marker prevMarker, OpenMarker prevOpenMarker)
      {
         super(nIndex);

         m_prev = prevMarker;
         m_prevOpen = prevOpenMarker;

         if (prevOpenMarker == null)
         {
            m_nIndent = m_nFirstIndent;
            m_bHead = false;
         }
         else
         {
            m_nIndent = prevOpenMarker.getIndentation() + m_nIncrIndent;
            m_bHead = prevMarker == prevOpenMarker;
         }
      }

      // operations

      /**
       * @return The previous Marker in the linked list.
       */
      protected Marker getPrevious()
      {
         return m_prev;
      }

      /**
       * @return The previous OpenMarker in the linked list.
       */
      protected OpenMarker getPreviousOpen()
      {
         return m_prevOpen;
      }

      /**
       * @return True if the code unit associated with this marker must have line breaks.
       */
      protected boolean isForcedLineBreak()
      {
         return m_bForceLineBreaks;
      }

      /**
       * Sets the index of the close token.
       * @param nEndIndex The index of the close token.
       */
      protected void setEndIndex(int nEndIndex)
      {
         m_nEndIndex = nEndIndex;
      }

      /**
       * Forces line breaks.
       */
      protected void forceLineBreaks()
      {
         m_bForceLineBreaks = true;
      }

      /**
       * @return True if a line break is needed at this marker.
       */
      protected boolean isLineBreak()
      {
         if (m_bForceLineBreaks)
         {
            return true;
         }

         if (m_bHead && m_nEndIndex - m_prevOpen.m_nIndex + m_prevOpen.m_nIndent < m_nLineLength)
         {
            return false;
         }

         return m_nEndIndex - m_nIndex >= Math.max(m_nBreakThreshold, m_nLineLength - m_nIndent);
      }

      /**
       * @see nexj.core.scripting.PrettyWriter.Marker#getIndentation()
       */
      protected int getIndentation()
      {
         return m_nIndent;
      }
   }

   /**
    * CloseMarker class implementation.
    */
   protected class CloseMarker extends Marker
   {
      // associations

      /**
       * The matching OpenMarker.
       */
      protected OpenMarker m_openMarker;

      // constructors

      /**
       * Creates a new CloseMarker.
       * @param nIndex The index into the buffer.
       * @param marker The OpenMarker matching this CloseMarker.
       */
      protected CloseMarker(int nIndex, OpenMarker marker)
      {
         super(nIndex);

         m_openMarker = marker;
         marker.setEndIndex(nIndex);
      }

      // operations

      /**
       * @see nexj.core.scripting.PrettyWriter.Marker#getIndentation()
       */
      protected int getIndentation()
      {
         return m_openMarker.getIndentation();
      }
   }

   /**
    * SeparatorMarker class implementation.
    */
   protected class SeparatorMarker extends Marker
   {
      // attributes

      /**
       * The number of indentation characters already added.
       */
      protected int m_nCharCount;

      // associations

      /**
       * The associated OpenMarker.
       */
      protected OpenMarker m_openMarker;

      // constructors

      /**
       * Creates a new SeparatorMarker.
       * @param nIndex The index into the buffer.
       * @param marker The OpenMarker associated with this SeparatorMarker.
       * @param bSpaceAdded True if a whitespace character has been added to the buffer.
       */
      protected SeparatorMarker(int nIndex, OpenMarker marker, boolean bSpaceAdded)
      {
         super(nIndex);

         m_openMarker = marker;
         m_nCharCount = (bSpaceAdded) ? 1 : 0;
      }

      // operations

      /**
       * @return The number of indentation characters already added.
       */
      protected int getCharCount()
      {
         return m_nCharCount;
      }

      /**
       * @see nexj.core.scripting.PrettyWriter.Marker#getIndentation()
       */
      protected int getIndentation()
      {
         return m_openMarker.getIndentation() + m_nIncrIndent - m_nCharCount;
      }
   }
}
