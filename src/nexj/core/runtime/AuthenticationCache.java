// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

/**
 * Interface representing the authentication cache.
 */
public interface AuthenticationCache
{
   /**
    * Flushes the cache for a given principal.
    * @param sName The authentication principal name.
    */
   void flushPrincipal(String sName);
}
