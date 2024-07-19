// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

/**
 * Delayed message input interface.
 */
public interface DelayedInput extends Input
{
   /**
    * Output the message on the given Output.
    * @param output The Output object.
    */
   public void output(Output output);
}
