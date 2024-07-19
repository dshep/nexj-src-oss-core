package nexj.core.persistence;

import nexj.core.meta.persistence.DataSourceFragment;

/**
 * Exception indicating that the persistence store is unavailable. 
 */
public class AvailabilityException extends PersistenceException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -5933971556333992354L;

   // constructors

   public AvailabilityException(DataSourceFragment fragment, Throwable cause)
   {
      super((fragment == null) ? "err.persistence.unavailable0" :
            (fragment.isDefault()) ? "err.persistence.unavailable" : "err.persistence.unavailableFragment",
         (fragment == null) ? null : (fragment.isDefault()) ? new Object[]{fragment.getDataSource().getName()} :
            new Object[]{fragment.getName(), fragment.getDataSource().getName()}, cause);
   }
}
