// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import nexj.core.meta.MetadataLookupException;

/**
 * TransformationEndpoint defines the root of a structure that may be transformed.
 */
public interface TransformationEndpoint extends EndpointPart
{
   /**
    * Gets the base end-point.
    * @return The base end-point.
    */
   public TransformationEndpoint getBaseEndpoint();

   /**
    * Determines if an end-point can be upcast to obtain this end-point.
    * @param spec The end-point to upcast.
    * @return True if this end-point can be obtained by upcasting endpoint.
    */
   public boolean isUpcast(TransformationEndpoint endpoint);

   /**
    * Gets a transformation end-point from the metadata.
    * @param sName The name of the end-point to get.
    * @return The end-point.
    * @throws MetadataLookupException If no end-point is found.
    */
   public TransformationEndpoint getEndpoint(String sName) throws MetadataLookupException;
}
