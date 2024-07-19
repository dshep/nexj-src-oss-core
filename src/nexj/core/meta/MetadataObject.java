// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.Iterator;

import nexj.core.util.ExceptionHolder;
import nexj.core.util.ObjUtil;
import nexj.core.util.UncheckedException;

/**
 * Base class for all metadata objects.
 */
public abstract class MetadataObject implements Cloneable
{
   // attributes

   /**
    * True if the object is read-only. Any attempt to modify the object it
    * will throw an IllegalAccessException.
    */
   private transient boolean m_bReadOnly;

   /**
    * True if the object has not been defined yet.
    */
   protected transient boolean m_bForward;

   // associations

   /**
    * An object that refers to this object, for diagnostic purposes.
    */
   protected MetadataObject m_referrer;

   // operations

   /**
    * Sets the read-only flag to true.
    */
   public void makeReadOnly()
   {
      m_bReadOnly = true;
   }

   /**
    * @throws MetadataException if the object is read-only.
    */
   protected void verifyNotReadOnly()
   {
      if (m_bReadOnly)
      {
         throw new MetadataException("err.meta.readOnly", new Object[]{this});
      }
   }

   /**
    * Sets the forward definition flag.
    * @param bForward The forward definition flag.
    */
   public void setForward(boolean bForward)
   {
      verifyNotReadOnly();
      m_bForward = bForward;

      if (!bForward)
      {
         m_referrer = null;
      }
   }

   /**
    * @return True if the object has not been defined yet.
    */
   public boolean isForward()
   {
      return m_bForward;
   }

   /**
    * Adds a referrer to the object.
    * @param referrer The referrer.
    */
   public void addReferrer(MetadataObject referrer)
   {
      if (m_bForward && m_referrer == null)
      {
         verifyNotReadOnly();
         m_referrer = referrer;
      }
   }

   /**
    * @return A lookup exception.
    */
   protected MetadataException createLookupException()
   {
      return new MetadataLookupException("err.meta.lookup", new Object[]{ObjUtil.getShortClassName(this)});
   }

   /**
    * This is invoked on objects that have been fully loaded to check the object validity.
    * @param metadata The root metadata object.
    * @param warnings The exception holder where warnings should be appended. Can be null.
    * @throws MetadataException if the object is invalid, e.g. with broken referential integrity.
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      if (m_bForward)
      {
         MetadataException e = createLookupException();

         if (m_referrer != null)
         {
            e = m_referrer.addException(null, e);
         }

         throw e;
      }
   }

   /**
    * Helper function to validate over a metadata object iterator.
    * @param iterator The iterator over metadata objects.
    * @param metadata The metadata.
    * @param warnings The exception holder where warnings should be appended. Can be null.
    * @throws MetadataCompoundValidationException if a validation error is encountered.
    */
   public final static void validate(Iterator iterator, ContextMetadata metadata,
      ExceptionHolder warnings) throws MetadataCompoundValidationException
   {
      MetadataCompoundValidationException eh = null;

      while (iterator.hasNext())
      {
         Object obj = iterator.next();

         if (obj instanceof MetadataObject)
         {
            MetadataObject mobj = (MetadataObject)obj;

            try
            {
               mobj.validate(metadata, warnings);
            }
            catch (UncheckedException e)
            {
               eh = mobj.addException(eh, e);
            }
         }
      }

      if (eh != null)
      {
         throw eh;
      }
   }

   /**
    * Adds an exception to a compound validation exception.
    * @param eh The exception holder, or null if not yet allocated.
    * @param e The exception to add.
    * @return The exception holder.
    */
   public final MetadataCompoundValidationException addException(
      MetadataCompoundValidationException eh, UncheckedException e)
   {
      if (eh == null)
      {
         eh = new MetadataCompoundValidationException();
      }

      if (e instanceof MetadataCompoundValidationException)
      {
         eh.addExceptions((MetadataCompoundValidationException)e);
      }
      else if (e instanceof MetadataValidationException)
      {
         eh.addException(e);
      }
      else
      {
         MetadataValidationException x = new MetadataValidationException(e);

         setProperties(x);
         eh.addException(x);
      }

      return eh;
   }

   /**
    * Sets the metadata marker properties.
    * @param marker The destination marker.
    */
   public void setProperties(MetadataMarker marker)
   {
   }

   /**
    * Clones the object (shallow copy) and makes the object writable.
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         MetadataObject obj = (MetadataObject)super.clone();

         obj.m_bReadOnly = false;

         return obj;
      }
      catch (CloneNotSupportedException e)
      {
         ObjUtil.rethrow(e);

         return null;
      }
   }
}
