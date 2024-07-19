// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.io.Reader;

import nexj.core.util.Lookup;
import nexj.core.util.Undefined;

/**
 * Interface implemented by text stream parsers.
 */
public interface Parser
{
   // constants

   /**
    * Constant returned if no syntax elements were found.
    */
   public final static Object EOF = Undefined.VALUE; 

   // operations
   
   /**
    * Parses the text stream represented by the reader and returns
    * the syntax tree. Only one minimal complete input language
    * construct is parsed on one invocation.
    * 
    * @param reader The reader from which to get the text to parse.
    * @param posMap Map of syntax tree nodes to TextPosition objects.
    * If not null, and if the reader is a TextPositionReader,
    * the parser populates it. The posMap should compare keys by reference,
    * not with the .equals operation (e.g. nexj.core.util.IdentityHashTab).
    * @throws ParserException If an error occurs during parsing.
    */
   Object parse(Reader reader, Lookup posMap);
}
