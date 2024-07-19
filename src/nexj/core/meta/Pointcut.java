// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.util.Named;


/**
 * Interface implemented by pointcuts.
 */
public interface Pointcut extends Named
{
   /**
    * @return True if the object is a pointcut.
    */
   boolean isPointcut();

   /**
    * Adds an aspect override to the pointcut.
    * @param aspect The aspect to add.
    * @param bInclusive True if the aspect if inclusive, false if exclusive.
    * @throws MetadataException if there is a duplicate override.
    */
   void addAspectOverride(Aspect aspect, boolean bInclusive) throws MetadataException;

   /**
    * Removes an aspect override.
    * @param aspect The aspect which override to remove.
    * @return True if the aspect override has been found and removed.
    */
   boolean removeAspectOverride(Aspect aspect);

   /**
    * Finds an aspect override ordinal number.
    * @param aspect the aspect to find.
    * @return The aspect override ordinal number, or -1 if not found.
    */
   int findAspectOverride(Aspect aspect);

   /**
    * Gets an aspect override by ordinal number.
    * @param nOrdinal The aspect override ordinal number.
    * @return The aspect.
    */
   Aspect getAspectOverride(int nOrdinal);

   /**
    * Gets the aspect override inclusivity flag.
    * @param nOrdinal The aspect override ordinal number.
    * @return True if the aspect is inclusive.
    */
   boolean isAspectOverrideInclusive(int nOrdinal);

   /**
    * @return The aspect override count.
    */
   int getAspectOverrideCount();

   /**
    * Adds an aspect to the pointcut.
    * @param aspect The aspect to add.
    * @throws MetadataException if there is a duplicate aspect.
    */
   void addAspect(Aspect aspect) throws MetadataException;

   /**
    * Removes an aspect from the pointcut.
    * @param aspect The aspect to remove.
    * @return True if the aspect has been found and removed.
    */
   boolean removeAspect(Aspect aspect);

   /**
    * Determines if a pointcut has a given aspect.
    * @param aspect The aspect to look for.
    * @return True if the pointcut has the aspect.
    */
   boolean hasAspect(Aspect aspect);

   /**
    * Gets an aspect by ordinal number.
    * @param nOrdinal The aspect ordinal number.
    */
   Aspect getAspect(int nOrdinal);

   /**
    * @return The aspect count.
    */
   int getAspectCount();
}
