// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Interface implemented by objects that invoke blocking
 * operations which can be cancelled from another thread.
 */
public interface Cancellable
{
   void cancel();
}
