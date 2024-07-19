package nexj.core.util;

import java.lang.ref.WeakReference;
import java.util.Observable;
import java.util.Observer;

/**
 * An observer wrapper that references the wrapped observer with a weak reference. This
 * allows the observer to be reclaimed by the garbage collector.
 */
public class WeakObserver implements Observer
{
   // associations

   /**
    * Weak reference to wrapped observer.
    */
   protected WeakReference m_ref;

   // constructors

   /**
    * Creates a new weak reference observer wrapper.
    * @param observer The observer to wrap.
    */
   public WeakObserver(Observer observer)
   {
      m_ref = new WeakReference(observer);
   }

   // operations

   /**
    * @see java.util.Observer#update(java.util.Observable, java.lang.Object)
    */
   public void update(Observable o, Object arg)
   {
      Object referent = m_ref.get();

      if (referent == null)
      {
         o.deleteObserver(this);
      }
      else
      {
         ((Observer)referent).update(o, arg);
      }
   }
}
