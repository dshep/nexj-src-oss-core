// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.util.Named;

/**
 * Interface implemented by aspects.
 */
public interface Aspect extends Named
{
   /**
    * @return True if the object is an aspect.
    */
   boolean isAspect();
   
   /**
    * Adds a pointcut pattern to the aspect.
    * @param sPattern The pattern to add (with * and ? wildcards).
    * @param bInclusive True to use pointcuts that match the pattern, false to reject them.
    */
   void addPointcutPattern(String sPattern, boolean bInclusive);

   /**
    * Gets a pointcut pattern by ordinal number.
    * @param nOrdinal The pattern ordinal number.
    * @return The pointcut pattern.
    */
   String getPointcutPattern(int nOrdinal);

   /**
    * Gets a pointcut pattern inclusivity flag by ordinal number.
    * @param nOrdinal The pattern ordinal number.
    * @return True if the pattern is inclusive.
    */
   boolean isPointcutPatternInclusive(int nOrdinal);

   /**
    * @return The pointcut pattern count.
    */
   int getPointcutPatternCount();

   /**
    * Determines if the aspect is matching the pointcut.
    * @param pointcut The pointcut to test.
    * @return True if the aspect is matching the pointcut.
    */
   boolean isMatching(Pointcut pointcut);

   /**
    * Adds the aspect to the pointcut aspect collection, if it matches.
    * @param pointcut The destination pointcut.
    */
   void addTo(Pointcut pointcut);

   /**
    * Applies the aspect to the pointcut.
    * @param pointcut The pointcut, modified in the process.
    * @param nPass The pass number, 0-based.
    */
   void applyTo(Pointcut pointcut, int nPass) throws MetadataException;
   
   /**
    * Removes the aspect from the pointcut.
    * @param pointcut The pointcut.
    * @return True if the aspect has been found and removed from the pointcut.
    */
   boolean removeFrom(Pointcut pointcut);
}
