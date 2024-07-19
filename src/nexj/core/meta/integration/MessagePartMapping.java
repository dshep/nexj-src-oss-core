// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import nexj.core.meta.ContextMetadata;
import nexj.core.meta.MetadataException;
import nexj.core.util.ExceptionHolder;

/**
 * Interface implemented by message part mapping metadata classes.
 */
public interface MessagePartMapping extends Cloneable
{
   /**
    * Initializes this mapping with a message part that doesn't have
    * an internal reference to another message part.
    * The only guarantee is that the message part's parent, if any, will
    * be set, and the parent's mapping will have been loaded and initialized.
    * Child message parts may not have been populated.
    * @param part The non-null message part to initialize this mapping with.
    * @throws nexj.core.meta.MetadataException if this mapping is invalid.
    */
   public void init(MessagePart part);

   /**
    * Resolves inheritance on a mapping when it is defined on equivalent parts
    * from a base message and a derived message.
    * @param baseMapping The mapping on the equivalent part (i.e. with the same
    * full path) in the base message.
    */
   public void resolveInheritance(MessagePartMapping baseMapping);

   /**
    * Initializes this mapping with a reference part. This will be called
    * on the mapping associated with the reference part.
    * @param ref The reference part.
    */
   public void refer(CompositeMessagePartRef ref);

   /**
    * Finishes initialization of this mapping. The message structure is fully populated now.
    * Only the finish() method on the mapping associated with the root message part will
    * be called automatically; mappings associated with child parts should be called
    * from their parent's mapping's finish() method.
    * @param part The non-null message part with which to initialize this mapping. 
    */
   public void finish(MessagePart part);
   
   /**
    * This is invoked on objects that have been fully loaded to check the object validity.
    * @param metadata The root metadata object.
    * @param warnings The exception holder where warnings should be appended. Can be null.
    * @param part The message part to which this mapping belongs.
    * @throws MetadataException if the object is invalid, e.g. with broken referential integrity.
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings, MessagePart part);

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone();
}
