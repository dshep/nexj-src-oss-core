// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;

/**
 * Character stream reader that invokes a template method to perform
 * simple substitution of macro variables in the form of ${VarName}.
 */
public abstract class SubstReader extends Reader
{
   // attributes

   /**
    * The current output string.
    */
   private String m_sOutput; 

   /**
    * The current string output character count.
    */
   private int m_nCount;

   /**
    * The last read character, -1 if none.
    */
   private int m_nLastChar = -1;

   // associations

   /**
    * The contained stream.
    */
   private Reader m_reader;

   // constructors

   /**
    * Creates the reader.
    * @param reader The stream in which to substitute the macro variables. 
    */
   public SubstReader(Reader reader)
   {
      super(reader);
      m_reader = reader;
   }

   // operations

   /**
    * Evaluates subexpression.
    * @param output The destination for the evaluated input.
    * @param input The reader containing the subexpression to evaluate.
    * @return The writer containing the evaluated subexpression.
    * @throws IOException On IO error during sub-expression evaluation.
    */
   protected Writer substitute(Writer output, Reader input) throws IOException
   {
      IOUtil.copy(output, new SubstReader(input)
      {
         protected String getValue(String sName) throws IOException
         {
            return SubstReader.this.getValue(sName);
         }
      });

      return output;
   }

   /**
    * Evaluates subexpression.
    * @param sValue The subexpression to evaluate.
    * @return The evaluated subexpression.
    * @throws IOException On IO error during sub-expression evaluation.
    */
   protected String substitute(String sValue) throws IOException
   {
      return substitute(
         new StringWriter(Math.max(32, sValue.length())), new StringReader(sValue)).toString();
   }

   /**
    * @see java.io.Reader#read()
    */
   public int read() throws IOException
   {
      int ch;
      
      if (m_sOutput != null)
      {
         ch = m_sOutput.charAt(m_nCount++);

         if (m_nCount == m_sOutput.length())
         {
            m_sOutput = null;
         }

         return ch;
      }

      for (;;)
      {
         if (m_nLastChar != -1)
         {
            ch = m_nLastChar;
            m_nLastChar = -1;
         }
         else
         {
            ch = m_reader.read();
         }

         if (ch == '$')
         {
            ch = m_reader.read();
   
            if (ch == '{')
            {
               StringBuffer buf = new StringBuffer(32);
               int nLevel = 1;
               
               for (;;)
               {
                  ch = m_reader.read();
                  
                  if (ch == -1)
                  {
                     return -1;
                  }

                  if (ch == '}' && --nLevel == 0)
                  {
                     m_sOutput = getValue(buf.toString());
                     
                     if (m_sOutput != null)
                     {
                        int nLen = m_sOutput.length();

                        if (nLen == 0)
                        {
                           m_sOutput = null;
                        }
                        else if (nLen == 1)
                        {
                           ch = m_sOutput.charAt(0);
                           m_sOutput = null;
                           
                           return ch;
                        }
                        else
                        {
                           m_nCount = 1;
                           
                           return m_sOutput.charAt(0);
                        }
                     }

                     break;
                  }
                  else
                  {
                     if (ch == '{' && buf.length() > 0 && buf.charAt(buf.length() - 1) == '$')
                     {
                        ++nLevel;
                     }

                     buf.append((char)ch);
                  }
               }
            }
            else
            {
               m_nLastChar = ch;
   
               return '$';
            }
         }
         else
         {
            return ch;
         }
      }
   }

   /**
    * @see java.io.Reader#read(char[], int, int)
    */
   public int read(char[] cbuf, int nOff, int nLen) throws IOException
   {
      int nCount = 0;

      while (nLen-- > 0)
      {
         int ch = read();
         
         if (ch == -1)
         {
            if (nCount == 0)
            {
               return -1;
            }
            
            break;
         }
         
         cbuf[nOff++] = (char)ch;
         ++nCount;
      }
      
      return nCount;
   }

   /**
    * @see java.io.Reader#ready()
    */
   public boolean ready() throws IOException
   {
      return m_nLastChar != -1 || m_sOutput != null || m_reader.ready();
   }

   /**
    * @see java.io.Reader#close()
    */
   public void close() throws IOException
   {
      m_nLastChar = -1;
      m_sOutput = null;
      m_reader.close();
   }

   /**
    * Returns the substitution value for a given variable.
    * @param sName the variable name.
    * @return The substitution value. 
    */
   protected abstract String getValue(String sName) throws IOException;
}
