// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import nexj.core.meta.MetadataLookupException;
import nexj.core.rpc.TransferObject;
import nexj.core.util.Named;
import nexj.core.util.PropertyMap;
import java.util.Iterator;

/**
 * Transformation end-point part defines the structure of the source or destination of
 * a transformation. 
 */
public interface EndpointPart extends Named
{
   /**
    * @return True if this end-point part represents a primitive value; false otherwise.
    */
   public boolean isPrimitive();

   /**
    * @return True if this end-point part represents a collection value; false otherwise.
    */
   public boolean isCollection();

   /**
    * Gets a child end-point part.
    * @param sName The name of the child end-point part.
    * @return The child end-point part.
    * @throws MetadataLookupException If no child is found with the given name.
    */
   public EndpointPart getChild(String sName) throws MetadataLookupException;

   /**
    * Finds a child end-point part.
    * @param sName The name of the child end-point part.
    * @return The child end-point part with the given name, or null if it doesn't exist.
    */
   public EndpointPart findChild(String sName);

   /**
    * @return An iterator for the child EndpointParts.
    */
   public Iterator getChildIterator();

   /**
    * Creates an output map if this end-point part is non-primitive.
    * @return A new output map.
    * @throws IllegalStateException If the end-point part is primitive.
    */
   public TransferObject createObject() throws IllegalStateException;

   /**
    * Gets the value corresponding to this end-point part from the given map.
    * @param map The map from which to get the value.
    * @param defValue The default value to be returned if the map doesn't contain
    * anything corresponding to this end-point part.
    * @return The value.
    */
   public Object getValue(PropertyMap map, Object defValue);

   /**
    * Sets the value corresponding to this end-point part in the given map.
    * @param map The map to set the value.
    * @param value The value to set.
    */
   public void setValue(PropertyMap map, Object value);
}
