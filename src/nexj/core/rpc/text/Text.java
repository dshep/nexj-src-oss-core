// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.text;

/**
 * Text format constants.
 */
public final class Text
{
   // constants

   /**
    * Type code character constants.
    */
   public final static char NULL = 'U';
   public final static char REFERENCE = '#';
   public final static char INTEGER = 'I';
   public final static char LONG = 'L';
   public final static char FLOAT = 'f';
   public final static char DOUBLE = 'F';
   public final static char DECIMAL = 'N';
   public final static char TIMESTAMP = 'D';
   public final static char BOOLEAN = 'B';
   public final static char STRING = 's';
   public final static char STRING_OBJECT = 'S';
   public final static char STRING_ID = '$';
   public final static char CHARACTER = 'h';
   public final static char BINARY = 'Z';
   public final static char LOCALE = 'C';
   public final static char TIME_ZONE = 'c';
   public final static char ARRAY = 'A';
   public final static char SEQUENCE = 'a';
   public final static char TRANSFER_OBJECT = 'T';
   public final static char OID = 'O';
   public final static char SYMBOL = 'Y';
   public final static char PAIR = 'P';
   public final static char BVECTOR = 'z';
   public final static char CVECTOR = 'H';
   public final static char SVECTOR = 'W';
   public final static char VECTOR = 'V';
   public final static char PRIVILEGE_SET = 'G';
   public final static char FUNCTION = 'm';
   public final static char MACRO = 'M';
   public final static char REQUEST = 'Q';
   public final static char RESPONSE = 'R';
   public final static char EXCEPTION = 'E';
   public final static char EXPRESSION = 'X';
   public final static char VERSION = 'v';
   public final static char BASIC_OBJECT = 'o';

   // constructors

   /**
    * Prevents construction. 
    */
   private Text()
   {
   }
}
