// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.cipher;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.Constructor;
import java.nio.CharBuffer;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;

import nexj.core.util.IOUtil;
import nexj.core.util.ObjUtil;
import nexj.core.util.XMLUtil;

/**
 * Provides read/write access to streams with a plain text cipher.
 */
public class CharacterStreamCipherDispatcher implements CharacterStreamCipher
{
   /**
    * The "text" scheme: passes through the plaintext, untouched.
    */
   public final static String SCHEME_TEXT = "text";

   /**
    * The "base64" scheme: encodes the plaintext using base64.
    */
   public final static String SCHEME_BASE64 = "base64";

   /**
    * The "hex" scheme: encodes the plaintext using hexadecimal notation.
    */
   public final static String SCHEME_HEX = "hex";

   /**
    * The "master" scheme: encrypts the plaintext with a master password and encodes with base64.
    */
   public final static String SCHEME_MASTER = "master";

   /**
    * The separator between the scheme name prefix and the encrypted data.
    */
   public final static char PREFIX_SEPARATOR = ':';
   
   /**
    * The default scheme to use when a scheme is not specified during encryption.
    */
   public final static String DEFAULT_SCHEME = SCHEME_MASTER;
   
   // attributes

   /**
    * The scheme to use for encryption.
    */
   protected String m_sSchemeName = DEFAULT_SCHEME;

   // associations

   /**
    * The properties used to initialize this dispatcher instance.
    */
   protected Properties m_properties;

   /**
    * Encryption scheme set to add encryptions to.
    */
   protected Set m_encryptionSchemeSet;

   // operations

   /**
    * @see nexj.core.util.cipher.CharacterStreamCipher#createDecryptedReader(java.io.Reader)
    */
   public Reader createDecryptedReader(Reader input) throws IOException
   {
      StringBuilder buf = new StringBuilder();
      int nCh = input.read();

      while (nCh != -1 && nCh != PREFIX_SEPARATOR)
      {
         buf.append((char)nCh);
         nCh = input.read();
      }

      String sScheme = buf.toString();

      if (sScheme.equals(SCHEME_TEXT))
      {
         addEncryptionScheme(SCHEME_TEXT);

         return input;
      }
      else if (sScheme.equals(SCHEME_BASE64))
      {
         addEncryptionScheme(SCHEME_BASE64);

         return getBase64CharacterStreamCipher(new IdentityStreamCipher()).createDecryptedReader(input);
      }
      else if (sScheme.equals(SCHEME_HEX))
      {
         addEncryptionScheme(SCHEME_HEX);

         return new InputStreamReader(
            getHexStreamCipher().createDecryptedInputStream(new ASCIIReaderStream(input)),
            XMLUtil.ENCODING);
      }
      else if (sScheme.equals(SCHEME_MASTER))
      {
         addEncryptionScheme(SCHEME_MASTER);

         StreamCipher cipher = getMasterPasswordStreamCipher();

         cipher.init(m_properties);
         return getBase64CharacterStreamCipher(cipher).createDecryptedReader(input);
      }
      else
      {
         if (nCh != -1)
         {
            buf.append((char)nCh);
            sScheme = buf.toString();
         }

         return new SequenceReader(new StringReader(sScheme), input);
      }
   }

   /**
    * Add encryption scheme to the encryption scheme set.
    * @param sEncryptionScheme Encryption scheme.
    */
   private void addEncryptionScheme(String sEncryptionScheme)
   {
      if (m_encryptionSchemeSet != null)
      {
         m_encryptionSchemeSet.add(sEncryptionScheme);
      }
   }

   /**
    * @see nexj.core.util.cipher.CharacterStreamCipher#createEncryptedWriter(java.io.Writer)
    */
   public Writer createEncryptedWriter(Writer output) throws IOException
   {
      if (m_sSchemeName.equals(SCHEME_BASE64))
      {
         // Scheme: "base64"
         output.write(SCHEME_BASE64);
         output.write(PREFIX_SEPARATOR);

         return getBase64CharacterStreamCipher(getEncryptionStreamCipher()).createEncryptedWriter(output);
      }
      else if (m_sSchemeName.equals(SCHEME_HEX))
      {
         // Scheme: "hex"
         output.write(SCHEME_HEX);
         output.write(PREFIX_SEPARATOR);

         return new OutputStreamWriter(
            getHexStreamCipher().createEncryptedOutputStream(new ASCIIWriterStream(output)),
            XMLUtil.ENCODING);
      }
      else if (m_sSchemeName.equals(SCHEME_MASTER))
      {
         // Scheme: "master"
         output.write(SCHEME_MASTER);
         output.write(PREFIX_SEPARATOR);

         return getBase64CharacterStreamCipher(getEncryptionStreamCipher()).createEncryptedWriter(output);
      }
      else
      {
         // Scheme: "text"
         output.write(SCHEME_TEXT);
         output.write(PREFIX_SEPARATOR);

         return output;
      }
   }

   /**
    * @see nexj.core.util.cipher.CharacterStreamCipher#init(java.util.Properties)
    */
   public void init(Properties properties)
   {
      m_sSchemeName = properties.getProperty("cipher.scheme", DEFAULT_SCHEME);
      m_properties = properties;
   }

   /**
    * Gets the StreamCipher instance that this dispatcher will use to
    * encrypt data.
    * 
    * @return The StreamCipher instance that this dispatcher will use to
    *         encrypt data.
    */
   public StreamCipher getEncryptionStreamCipher()
   {
      StreamCipher result = null;

      if (m_sSchemeName.equals(SCHEME_TEXT))
      {
         result = new IdentityStreamCipher();
      }
      else if (m_sSchemeName.equals(SCHEME_BASE64))
      {
         result = new IdentityStreamCipher();
      }
      else if (m_sSchemeName.equals(SCHEME_HEX))
      {
         result = getHexStreamCipher();
      }
      else if (m_sSchemeName.equals(SCHEME_MASTER))
      {
         result = getMasterPasswordStreamCipher();
         result.init(m_properties);
      }

      return result;
   }

   /**
    * Creates a CharacterStreamCipher that is configured to use the
    * same encryption scheme as the parameter. The returned cipher
    * will always encrypt and decrypt using the same encryptiong scheme
    * as was used in the parameter. On decryption, the returned cipher
    * will ignore the scheme name prefix.
    * 
    * @param input The reader of encrypted data. This will be advanced
    *              to the position of the first character of encrypted data.
    * @return A cipher that will encrypt using the same encryption scheme
    *         as the parameter; a String containing the data read if no
    *         scheme could be found.
    * @throws IOException If an I/O error occurs.
    */
   public Object create(Reader input) throws IOException
   {
      StringWriter buf = new StringWriter();
      int nCh = input.read();

      while (nCh != -1 && nCh != PREFIX_SEPARATOR)
      {
         buf.append((char)nCh);
         nCh = input.read();
      }

      String sScheme = buf.toString();

      if (nCh != -1 &&
          (sScheme.equals(SCHEME_TEXT) ||
           sScheme.equals(SCHEME_BASE64) ||
           sScheme.equals(SCHEME_HEX) ||
           sScheme.equals(SCHEME_MASTER)))
      {
         CharacterStreamCipher result = new SameSchemeCharacterStreamCipher();
         Properties properties = new Properties(m_properties);

         properties.setProperty("cipher.scheme", sScheme);
         result.init(properties);

         return result;
      }
      else
      {
         if (nCh != -1)
         {
            buf.write((char)nCh);
            IOUtil.copy(buf, input);

            return buf.toString();
         }

         return sScheme;
      }
   }
   
   /**
    * Identifies the highest-security scheme in the set of scheme names.
    * 
    * @param schemeNameSet The set of schemes to examine.
    * @return The most secure scheme in schemeNames; SCHEME_TEXT if nothing found.
    */
   public static String computeMostSecure(Set schemeNameSet)
   {
      if (schemeNameSet.contains(SCHEME_MASTER))
      {
         return SCHEME_MASTER;
      }
      else if (schemeNameSet.contains(SCHEME_BASE64))
      {
         return SCHEME_BASE64;
      }
      else if (schemeNameSet.contains(SCHEME_HEX))
      {
         return SCHEME_HEX;
      }
      else
      {
         return SCHEME_TEXT;
      }
   } 
   
   /**
    * Convenience method that encrypts the argument and returns it
    * as a String.
    * 
    * @param sPlainText The String to encrypt. Can  be null.
    * @return The encrypted String.
    */
   public String encrypt(String sPlainText)
   {
      if (sPlainText != null)
      {
         try
         {
            StringWriter encWriter = new StringWriter();
            Writer decWriter = createEncryptedWriter(encWriter);

            decWriter.write(sPlainText);
            decWriter.close();

            return encWriter.toString();
         }
         catch (IOException ex)
         {
            ObjUtil.rethrow(ex);
         }
      }

      return null;
   }

   /**
    * Convenience method that decrypts the argument and returns is
    * as a String.
    * 
    * @param sEncrypted The String to decrypt. Can be null.
    * @return The decrypted String.
    */
   public String decrypt(String sEncrypted)
   {
      if (sEncrypted != null)
      {
         try
         {
            StringReader encReader = new StringReader(sEncrypted);
            Reader decReader = createDecryptedReader(encReader);
            StringWriter decWriter = new StringWriter();

            IOUtil.copy(decWriter, decReader);
            decReader.close();

            return decWriter.toString();
         }
         catch (IOException ex)
         {
            ObjUtil.rethrow(ex);
         }
      }

      return null;
   }

   /**
    * Build a CharacterStreamCipher for the base64 cipher scheme.
    * @param cipher The byte cipher to use.
    * @return a CharacterStreamCipher implementation.
    */
   public CharacterStreamCipher getBase64CharacterStreamCipher(StreamCipher cipher)
   {
      try
      {
         // todo: classname should not be hard-coded, but rather should be a configuration property
         Constructor construct = Class.forName("nexj.core.util.cipher.Base64CharacterStreamCipher").getConstructor(new Class[] {StreamCipher.class});
         
         return (CharacterStreamCipher)construct.newInstance(new Object[] {cipher});
      }
      catch (Throwable t)
      {
         ObjUtil.rethrow(t);
         
         return null;
      }
   }

   /**
    * Build a StreamCipher for the Hex cipher scheme.
    * @return a StreamCipher implementation.
    */
   protected StreamCipher getHexStreamCipher()
   {
      BinaryStreamCipherDispatcher dispatcher = new BinaryStreamCipherDispatcher();

      dispatcher.init(m_properties);

      return dispatcher.createStreamCipher(BinaryStreamCipherDispatcher.HEX);
   }

   /**
    * Build a CharacterStreamCipher for the master cipher scheme.
    * @return a CharacterStreamCipher implementation.
    */
   public StreamCipher getMasterPasswordStreamCipher()
   {
      try
      {
         // todo: classname should not be hard-coded, but rather should be a configuration property
         return (StreamCipher)Class.forName("nexj.core.util.cipher.MasterPasswordStreamCipher").newInstance();
      }
      catch (Throwable t)
      {
         ObjUtil.rethrow(t);
         
         return null;
      }
   }
   
   // inner classes

   /**
    * This is the CharacterStreamCipher implementation returned by the create() method.
    * It is designed to encrypt and decrypt using the scheme specified by the "cipher.scheme"
    * property. It expects no cipher prefix when decrypting. It will, however, output a cipher
    * prefix when encrypting.
    */
   protected static class SameSchemeCharacterStreamCipher extends CharacterStreamCipherDispatcher
   {
      // operations

      /**
       * The method in the superclass dispatches based on the scheme name prefix at the
       * beginning of the stream. This method dispatches based on the "cipher.scheme"
       * property that was given to init().
       * 
       * @see nexj.core.util.cipher.CharacterStreamCipherDispatcher#createDecryptedReader(java.io.Reader)
       */
      public Reader createDecryptedReader(Reader input) throws IOException
      {
         return super.createDecryptedReader(
            new SequenceReader(
               new StringReader(m_sSchemeName + PREFIX_SEPARATOR),
               input
         ));
      }
   }

   /**
    * An input stream that can read from an ASCII Reader and use only the lower byte of a char.
    */
   protected static class ASCIIReaderStream extends InputStream
   {
      /**
       * The reader to get input from.
       */
      protected Reader m_reader;

      /**
       * Constructor.
       * @param reader The object to get input from.
       */
      public ASCIIReaderStream(Reader reader)
      {
         m_reader = reader;
      }

      /**
       * @see java.io.InputStream#close()
       */
      public void close() throws IOException
      {
         m_reader.close();
      }

      /**
       * @see java.io.InputStream#mark(int)
       */
      public synchronized void mark(int nReadAheadLimit)
      {
         try
         {
            m_reader.mark(nReadAheadLimit);
         }
         catch (IOException e)
         {
            ObjUtil.rethrow(e);
         }
      }

      /**
       * @see java.io.InputStream#markSupported()
       */
      public boolean markSupported()
      {
         return m_reader.markSupported();
      }

      /**
       * @see java.io.InputStream#read()
       */
      public int read() throws IOException
      {
         return m_reader.read();
      }

      /**
       * @see java.io.InputStream#reset()
       */
      public synchronized void reset() throws IOException
      {
         m_reader.reset();
      }

      /**
       * @see java.io.InputStream#skip(long)
       */
      public long skip(long n) throws IOException
      {
         return m_reader.skip(n);
      }
   }

   /**
    * An output stream that can write to an ASCII Writer and use only the lower byte of a char.
    */
   protected static class ASCIIWriterStream extends OutputStream
   {
      /**
       * The writer to send output to.
       */
      protected Writer m_writer;

      /**
       * Constructor.
       * @param reader The object to get input from.
       */
      public ASCIIWriterStream(Writer writer)
      {
         m_writer = writer;
      }

      /**
       * @see java.io.OutputStream#close()
       */
      public void close() throws IOException
      {
         m_writer.close();
      }

      /**
       * @see java.io.OutputStream#flush()
       */
      public void flush() throws IOException
      {
         m_writer.flush();
      }

      /**
       * @see java.io.OutputStream#write(int)
       */
      public void write(int n) throws IOException
      {
         m_writer.write(n);
      }
   }

   /**
    * Represents the concatenation of Readers.
    */
   public static class SequenceReader extends Reader
   {
      // associations

      /**
       * The current Reader; null if at end of all readers. 
       */
      Reader m_reader;

      /**
       * The iterator over the Readers being concatenated.
       */
      Iterator m_readerIterator;


      // constructors

      /**
       * Creates a new SequenceReader that concatenates the Readers
       * returned by the given iterator.
       * 
       * @param i An iterator over the Readers to concatenate.
       */
      public SequenceReader(Iterator i)
      {
         m_readerIterator = i;

         if (m_readerIterator.hasNext())
         {
            m_reader = (Reader)m_readerIterator.next();
         }
      }

      /**
       * Conveniently creates a new SequenceReader that is the concatenation
       * of the two supplied Readers.
       * 
       * @param r1 The first Reader in the concatenation.
       * @param r2 The second Reader in the concatenation.
       */
      public SequenceReader(Reader r1, Reader r2)
      {
         ArrayList seqList = new ArrayList(2);

         seqList.add(r1);
         seqList.add(r2);

         m_readerIterator = seqList.iterator();
         m_reader = (Reader)m_readerIterator.next();
      }


      // operations

      /**
       * @see java.io.Reader#close()
       */
      public void close() throws IOException
      {
         IOUtil.close(m_reader);

         // Request and close remaining streams
         while (m_readerIterator.hasNext())
         {
            IOUtil.close((Reader)m_readerIterator.next());
         }
      }

      /**
       * @see java.io.Reader#read(char[], int, int)
       */
      public int read(char[] cbuf, int off, int len) throws IOException
      {
         if (m_reader == null)
         {
            return -1; //EOF
         }

         int nCharsRead = m_reader.read(cbuf, off, len);
         int nTotalRead = Math.max(0, nCharsRead);

         while (nTotalRead < len && m_reader != null)
         {
            IOUtil.close(m_reader);

            if (m_readerIterator.hasNext())
            {
               m_reader = (Reader)m_readerIterator.next();
               nCharsRead = m_reader.read(cbuf, off + nTotalRead, len - nTotalRead);
               nTotalRead += Math.max(0, nCharsRead);
            }
            else
            {
               m_reader = null;

               if (nTotalRead == 0 && nCharsRead == -1)
               {
                  return -1; //EOF
               }
            }
         }

         return nTotalRead;
      }

      /**
       * @see java.io.Reader#mark(int)
       */
      public void mark(int readAheadLimit) throws IOException
      {
         throw new IOException("Mark not supported");
      }

      /**
       * @see java.io.Reader#markSupported()
       */
      public boolean markSupported()
      {
         return false;
      }

      /**
       * @see java.io.Reader#read()
       */
      public int read() throws IOException
      {
         if (m_reader == null)
         {
            return -1; //EOF
         }

         int nCh = m_reader.read();

         while (nCh == -1 && m_reader != null)
         {
            IOUtil.close(m_reader);

            if (m_readerIterator.hasNext())
            {
               m_reader = (Reader)m_readerIterator.next();
               nCh = m_reader.read();
            }
            else
            {
               m_reader = null;
            }
         }

         return nCh;
      }

      /**
       * @see java.io.Reader#read(char[])
       */
      public int read(char[] cbuf) throws IOException
      {
         return read(cbuf, 0, cbuf.length);
      }

      /**
       * @see java.io.Reader#read(java.nio.CharBuffer)
       */
      public int read(CharBuffer target) throws IOException
      {
         // Superclass will use 3-arg form of read()
         return super.read(target);
      }

      /**
       * @see java.io.Reader#ready()
       */
      public boolean ready() throws IOException
      {
         if (m_reader == null)
         {
            return true;
         }

         return m_reader.ready();
      }

      /**
       * @see java.io.Reader#reset()
       */
      public void reset() throws IOException
      {
         throw new IOException("Reset not supported");
      }

      /**
       * @see java.io.Reader#skip(long)
       */
      public long skip(long n) throws IOException
      {
         if (m_reader == null)
         {
            return 0;
         }

         if (n < 0)
         {
            throw new IllegalArgumentException("Negative number of characters");
         }

         long lSkippedTotal = m_reader.skip(n);

         while (lSkippedTotal < n && m_reader != null)
         {
            IOUtil.close(m_reader);

            if (m_readerIterator.hasNext())
            {
               m_reader = (Reader)m_readerIterator.next();
               lSkippedTotal += m_reader.skip(n - lSkippedTotal);
            }
            else
            {
               m_reader = null;
            }
         }

         return lSkippedTotal;
      }
   }

   /**
    * A filtering writer that drops an optional prefix from the data written to it.
    * Currently, this class is used by the XMLPropertyDecryptor custom ant task
    * to drop "text:" from the files it writes out.
    */
   public static class DropPrefixWriter extends Writer
   {
      // attributes

      /**
       * The prefix to drop.
       */
      protected String m_sPrefix;

      /**
       * The position of the current character being written
       * in the prefix.
       */
      protected int m_nPrefixPos;


      // associations

      /**
       * The wrapped writer.
       */
      protected Writer m_writer;


      // constructors

      /**
       * Create a writer that drops the prefix sPrefix if that prefix comprises
       * the first characters written to it. Otherwise, all characters are
       * written to the output writer.
       * 
       * @param writer  The output writer.
       * @param sPrefix The optional prefix to drop.
       */
      public DropPrefixWriter(Writer writer, String sPrefix)
      {
         m_writer = writer;
         m_sPrefix = sPrefix;
         m_nPrefixPos = 0;
      }


      // operations

      /**
       * @see java.io.Writer#close()
       */
      public void close() throws IOException
      {
         if (m_writer != null)
         {
            m_writer.close();
            m_writer = null;
         }
      }

      /**
       * @see java.io.Writer#flush()
       */
      public void flush() throws IOException
      {
         if (m_writer != null)
         {
            m_writer.flush();
         }
      }

      /**
       * @see java.io.Writer#write(char[], int, int)
       */
      public void write(char[] cbuf, int off, int len) throws IOException
      {
         if (m_writer == null)
         {
            throw new IOException("Writer closed");
         }

         int nPos = off;
         int nRemaining = len;

         while (m_nPrefixPos < m_sPrefix.length() && nPos < len)
         {
            this.write(cbuf[nPos]);
            nPos++;
            nRemaining--;
         }

         m_writer.write(cbuf, nPos, nRemaining);
      }

      /**
       * @see java.io.Writer#write(char[])
       */
      public void write(char[] cbuf) throws IOException
      {
         this.write(cbuf, 0, cbuf.length);
      }

      /**
       * @see java.io.Writer#write(int)
       */
      public void write(int c) throws IOException
      {
         if (m_writer == null)
         {
            throw new IOException("Writer closed");
         }

         if (m_nPrefixPos >= m_sPrefix.length())
         {
            m_writer.write(c);
         }
         else
         {
            if (m_sPrefix.charAt(m_nPrefixPos) != c)
            {
               m_writer.write(m_sPrefix.substring(0, m_nPrefixPos));
               m_writer.write(c);
               m_nPrefixPos = m_sPrefix.length();
            }
            else
            {
               m_nPrefixPos++;
            }
         }
      }
   }

   /**
    * Set encryption scheme set to add encryptions to.
    * @param encryptionSchemeSet Encryption scheme set.
    */
   public void setEncryptionSchemeSet(Set encryptionSchemeSet)
   {
      m_encryptionSchemeSet = encryptionSchemeSet;
   }

   /**
    * Get encryption scheme set.
    * @return Encryption scheme set.
    */
   public Set getEncryptionSchemeSet()
   {
      return m_encryptionSchemeSet;
   }
}
