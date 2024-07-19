// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import java.util.Iterator;

import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.util.Sortable;

/**
 * Message part representing a composite of several messages.
 */
public abstract class CompositeMessagePart extends MessagePart implements Sortable
{
   // constants
   
   /**
    * Sequential aggregation - the contained parts must
    * appear in their order of declaration.
    */
   public final static byte SEQUENTIAL = 0;
   
   /**
    * Random aggregation - the contained parts
    * may appear in any order.
    */
   public final static byte RANDOM = 1;
   
   /**
    * Single aggregation - only one of the
    * contained parts may appear.
    */
   public final static byte SINGLE = 2;

   // constructors
   
   /**
    * Constructs the message part.
    * @param sName The part name.
    */
   public CompositeMessagePart(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the message part.
    */
   public CompositeMessagePart()
   {
      super();
   }

   // operations

   /**
    * @return The aggregation mode.
    */
   public abstract byte getAggregation();

   /**
    * @return True if unidentified child parts are allowed
    * in the external representation of the message. 
    */
   public abstract boolean isLax();
   
   /**
    * Adds a new child part to the composite message part.
    * @param part The child part to add.
    * @throws MetadataException if a child part
    * with the same name already exists.
    */
   public abstract void addPart(MessagePart part);

   /**
    * Verifies whether a part exists
    * @return true if part exists.
    */
   public abstract boolean hasPart(String sName);
   
   /**
    * Gets a child part by name.
    * @param sName The child part name.
    * @return The child part object.
    * @throws MetadataLookupException if the child part does not exist.
    */
   public abstract MessagePart getPart(String sName);

   /**
    * Finds a child part by name.
    * @param sName The child part name.
    * @return The child part object; null if the child part does not exist.
    */
   public abstract MessagePart findPart(String sName);

   /**
    * Gets a child part by ordinal number.
    * @param nOrdinal The part ordinal number.
    * @return The message part.
    */
   public abstract MessagePart getPart(int nOrdinal);

   /**
    * @return The child part count.
    */
   public abstract int getPartCount();

   /**
    * @return An iterator for the contained child part objects.
    */
   public abstract Iterator getPartIterator();

   /**
    * @see nexj.core.meta.integration.EndpointPart#getChildIterator()
    */
   public Iterator getChildIterator()
   {
      return getPartIterator();
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#isPrimitive()
    */
   public boolean isPrimitive()
   {
      return false;
   }
}
