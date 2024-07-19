package nexj.core.scripting;

import java.io.IOException;
import java.io.Writer;

import nexj.core.util.StringUtil;

/**
 * The Comment class implementation
 */
public class Comment
{
   // attributes

   /**
    * The comment string.
    */
   protected String m_sValue;

   // constructors

   /**
    * Creates a new comment with the given string value.
    * @param sValue The comment string.
    */
   public Comment(String sValue)
   {
      m_sValue = sValue;
   }

   /**
    * Returns the comment as a string.
    */
   public String getText()
   {
      return m_sValue;
   }

   /**
    * Formats and writes the comment.
    * @param writer The output character stream.
    * @param nIndent The number of indentation spaces.
    * @param nLineLength The maximum character count per line, including indentation.
    * @param nBreakThreshold The character count threshold, excluding indentation,
    * above which line breaks are allowed.
    * @param sLineSep The line separator.
    * @param bIndentFirst Whether the first line needs to be indented.
    */
   public void writeFormatted(Writer writer, int nIndent, int nLineLength, int nBreakThreshold,
      String sLineSep, boolean bIndentFirst) throws IOException
   {
      int nLength = m_sValue.length();

      nLineLength -= nIndent + 1;
      nBreakThreshold -= 1;

      if (nLength <= nLineLength)
      {
         if (bIndentFirst)
         {
            StringUtil.writeSpaces(writer, nIndent);
         }

         writer.write(';');
         writer.write(m_sValue);
      }
      else
      {
         char[] chArray = m_sValue.toCharArray();
         int nWrite = 0;

         for (int nRead = nLineLength; nRead < nLength; nRead = nWrite + nLineLength)
         {
            int nSpaceIndex = m_sValue.lastIndexOf(' ', nRead);

            if (nSpaceIndex < nWrite + nBreakThreshold)
            {
               if ((nSpaceIndex = m_sValue.indexOf(' ', nRead)) == -1)
               {
                  break; // line break not possible
               }
            }

            if (bIndentFirst)
            {
               StringUtil.writeSpaces(writer, nIndent);
            }
            else
            {
               bIndentFirst = true; // true to indent in the next iteration
            }

            writer.write(';');
            writer.write(chArray, nWrite, nSpaceIndex - nWrite);
            writer.write(sLineSep);
            nWrite = nSpaceIndex;
         }

         if (nWrite < nLength)
         {
            if (bIndentFirst)
            {
               StringUtil.writeSpaces(writer, nIndent);
            }

            writer.write(';');
            writer.write(chArray, nWrite, nLength - nWrite);
         }
      }
   }
}
