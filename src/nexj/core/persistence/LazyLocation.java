// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

/**
 * Interface for returning location information without any additional queries.
 */
public interface LazyLocation extends OIDHolder
{
   /**
    * @return The lazy class name.
    */
   String getLazyClassName();
   
   /**
    * @return The lazy caption.
    */
   String getLazyCaption();
}
