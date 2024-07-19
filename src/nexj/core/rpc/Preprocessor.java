// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

/**
 * Interface implemented by request preprocessors.
 */
public interface Preprocessor
{
   /**
    * Preprocesses the request.
    * @param request The request to preprocess.
    */
   void preprocess(Request request);
}
