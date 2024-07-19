// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.Arrays;
import java.util.Iterator;

import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.Named;
import nexj.core.util.UncheckedException;

/**
 * Metaclass aspect.
 * Derived from Metaclass for simplicity, to avoid
 * creating multiple hierarchies for the related classes.
 */
public final class ClassAspect extends Metaclass implements Aspect
{
   // associations

   /**
    * The metaclass aspect helper.
    */
   protected ClassAspectHelper m_aspectHelper = new ClassAspectHelper(this);

   /**
    * Data source to persistence mapping map: PersistenceMapping[DataSource].
    */
   protected Lookup m_persistenceMappingMap = new HashTab(2);

   // constructors

   /**
    * Constructs the aspect.
    * @param sName The aspect name.
    */
   public ClassAspect(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * Adds a new persistence mapping to the aspect.
    * @param persistenceMapping The persistence mapping to add.
    * @throws MetadataException if a persistence mapping
    * with the same data source already exists.
    */
   public void addPersistenceMapping(PersistenceMapping persistenceMapping)
   {
      verifyNotReadOnly();

      Object oldPersistenceMapping = m_persistenceMappingMap.put(
         persistenceMapping.getDataSource(), persistenceMapping);

      if (oldPersistenceMapping != null)
      {
         m_persistenceMappingMap.put(persistenceMapping.getDataSource(), oldPersistenceMapping);

         throw new MetadataException("err.meta.persistenceMappingDup",
            new Object[]
            {
               persistenceMapping.getDataSource().getName(),
               getName()
            });
      }

      persistenceMapping.setMetaclass(this);
   }

   /**
    * Gets a persistence mapping by data source.
    * @param dataSource The persistence mapping data source.
    * @return The persistence mapping object.
    * @throws MetadataLookupException if the persistence mapping does not exist.
    */
   public PersistenceMapping getPersistenceMapping(DataSource dataSource)
   {
      PersistenceMapping persistenceMapping = (PersistenceMapping)m_persistenceMappingMap.get(dataSource);

      if (persistenceMapping != null)
      {
         return persistenceMapping;
      }

      throw new MetadataLookupException("err.meta.persistenceMappingLookup", dataSource.getName(), this);
   }

   /**
    * Finds a persistence mapping by data source.
    * @param dataSource The persistence mapping data source.
    * @return The persistence mapping object, or null if not found.
    */
   public PersistenceMapping findPersistenceMapping(DataSource dataSource)
   {
      return (PersistenceMapping)m_persistenceMappingMap.get(dataSource);
   }

   /**
    * @return The persistence mapping count.
    */
   public int getPersistenceMappingCount()
   {
      return m_persistenceMappingMap.size();
   }

   /**
    * @return An iterator for the contained persistence mapping objects.
    */
   public Iterator getPersistenceMappingIterator()
   {
      return m_persistenceMappingMap.valueIterator();
   }

   /**
    * @see nexj.core.meta.Aspect#isAspect()
    */
   public boolean isAspect()
   {
      return true;
   }

   /**
    * @see nexj.core.meta.Aspect#addPointcutPattern(java.lang.String, boolean)
    */
   public void addPointcutPattern(String sPattern, boolean bInclusive)
   {
      verifyNotReadOnly();
      m_aspectHelper.addPointcutPattern(sPattern, bInclusive);
   }

   /**
    * @see nexj.core.meta.Aspect#getPointcutPattern(int)
    */
   public String getPointcutPattern(int nOrdinal)
   {
      return m_aspectHelper.getPointcutPattern(nOrdinal);
   }

   /**
    * @see nexj.core.meta.Aspect#isPointcutPatternInclusive(int)
    */
   public boolean isPointcutPatternInclusive(int nOrdinal)
   {
      return m_aspectHelper.isPointcutPatternInclusive(nOrdinal);
   }

   /**
    * @see nexj.core.meta.Aspect#getPointcutPatternCount()
    */
   public int getPointcutPatternCount()
   {
      return m_aspectHelper.getPointcutPatternCount();
   }

   /**
    * @see nexj.core.meta.Aspect#isMatching(nexj.core.meta.Pointcut)
    */
   public boolean isMatching(Pointcut pointcut)
   {
      return m_aspectHelper.isMatching(pointcut);
   }

   /**
    * @see nexj.core.meta.Aspect#addTo(nexj.core.meta.Pointcut)
    */
   public void addTo(Pointcut pointcut)
   {
      m_aspectHelper.addTo(pointcut);
   }

   /**
    * @see nexj.core.meta.Aspect#applyTo(nexj.core.meta.Pointcut, int)
    */
   public void applyTo(Pointcut pointcut, int nPass) throws MetadataException
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.meta.Aspect#removeFrom(nexj.core.meta.Pointcut)
    */
   public boolean removeFrom(Pointcut pointcut)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.meta.Metaclass#setNextAction(nexj.core.meta.Action, java.lang.String)
    */
   public void setNextAction(Action action, String sName)
   {
      Action nextAction = action.getEvent().findAction(sName);

      if (nextAction == null)
      {
         nextAction = new Action(sName);
         nextAction.setEvent(action.getEvent());
      }

      action.setNextAction(nextAction);
   }

   /**
    * @see nexj.core.meta.Metaclass#resolveInheritance(nexj.core.util.ExceptionHolder)
    */
   protected ExceptionHolder resolveInheritance(ExceptionHolder eh)
   {
      verifyNotReadOnly();

      return super.resolveInheritance(applyAspects(eh));
   }

   /**
    * @see nexj.core.meta.Metaclass#completeAttributeDerivation(nexj.core.meta.Attribute, nexj.core.meta.Attribute)
    */
   protected void completeAttributeDerivation(Attribute derived, Attribute base)
   {
      derived.setOrdinal(-1);
      derived.setDeclarator(derived.getMetaclass());
   }

   /**
    * @see nexj.core.meta.Metaclass#completeEventDerivation(nexj.core.meta.Event, nexj.core.meta.Event)
    */
   protected void completeEventDerivation(Event derived, Event base)
   {
      derived.setDeclarator(derived.getMetaclass());
   }

   /**
    * @see nexj.core.meta.Metaclass#resolveAspectMembers(nexj.core.util.ExceptionHolder)
    */
   protected ExceptionHolder resolveAspectMembers(ExceptionHolder eh)
   {
      eh = super.resolveAspectMembers(eh);

      for (int i = 0, n = getEventCount(); i < n; ++i)
      {
         Event event = getEvent(i);

         event.resolveActions();
         event.sortActions();
      }

      return eh;
   }

   /**
    * Resolves the class aspects in the metadata.
    * @param metadata The root metadata object.
    * @return The topologically sorted aspect array. 
    * @throws MetadataException if an error occurs.
    */
   public static ClassAspect[] resolveAspects(final Metadata metadata) throws MetadataException
   {
      // Sort topologically the aspects, the least dependency distance
      // first, and then by name within the aspects with the same distance

      ClassAspect[] aspectArray = new ClassAspect[metadata.getClassAspectCount()];
      Lookup predCountMap = new HashTab(metadata.getClassAspectCount());
      int nStart = 0;
      int nEnd = 0;

      for (Iterator itr = metadata.getClassAspectIterator(); itr.hasNext();)
      {
         ClassAspect aspect = (ClassAspect)itr.next();
         int nCount = aspect.getAspectCount();

         if (nCount == 0)
         {
            aspectArray[nEnd++] = aspect;
         }
         else
         {
            predCountMap.put(aspect, Primitive.createInteger(nCount));
         }
      }

      while (nStart < nEnd)
      {
         Arrays.sort(aspectArray, nStart, nEnd, Named.COMPARATOR);

         int nNewEnd;

         for (nNewEnd = nEnd; nStart < nEnd; ++nStart)
         {
            ClassAspect aspect = aspectArray[nStart];

            for (Iterator itr = metadata.getClassAspectIterator(); itr.hasNext();)
            {
               ClassAspect successor = (ClassAspect)itr.next();

               if (successor.hasAspect(aspect))
               {
                  Integer predCount = (Integer)predCountMap.get(successor);

                  if (predCount != null)
                  {
                     int nPredCount = predCount.intValue();

                     if (--nPredCount == 0)
                     {
                        predCountMap.remove(successor);
                        aspectArray[nNewEnd++] = successor;
                     }
                     else
                     {
                        predCountMap.put(successor, Primitive.createInteger(nPredCount));
                     }
                  }
               }
            }
         }

         nEnd = nNewEnd;
      }

      ExceptionHolder eh = null;

      if (predCountMap.size() != 0)
      {
         for (Iterator itr = predCountMap.iterator(); itr.hasNext();)
         {
            ClassAspect aspect = (ClassAspect)itr.next();
            MetadataValidationException e = new MetadataValidationException("err.meta.classAspectCircularDep",
               new Object[]{aspect.getName()});

            aspect.setProperties(e);
            eh = addException(eh, e);
         }
      }

      if (eh != null)
      {
         throw (MetadataException)eh;
      }

      // Resolve the aspect inheritance

      for (int i = 0; i < nEnd; ++i)
      {
         eh = aspectArray[i].resolveInheritance(eh);
      }

      if (eh != null)
      {
         throw (MetadataException)eh;
      }

      if (nEnd == aspectArray.length)
      {
         return aspectArray;
      }

      ClassAspect[] aspects = new ClassAspect[nEnd];

      System.arraycopy(aspectArray, 0, aspects, 0, nEnd);

      return aspects;
   }

   /**
    * Resolves the class aspect members.
    * @param aspects The topologically sorted aspect array (from resolveAspects())
    * @throws MetadataException if an error occurs.
    */
   public static void resolveMembers(ClassAspect[] aspects) throws MetadataException
   {
      ExceptionHolder eh = null;

      for (int i = 0; i < aspects.length; ++i)
      {
         eh = aspects[i].resolveAspectMembers(eh);
      }

      if (eh != null)
      {
         throw (MetadataException)eh;
      }
   }
   
   /**
    * Resolves the class aspect persistence.
    * @param aspects The topologically sorted aspect array (from resolveAspects())
    * @throws MetadataException if an error occurs.
    */
   public static void resolvePersistence(ClassAspect[] aspects) throws MetadataException
   {
      ExceptionHolder eh = null;

      for (int i = 0; i < aspects.length; ++i)
      {
         ClassAspect aspect = aspects[i];

         for (int k = 0, n = aspect.getAspectCount(); k < n; ++k)
         {
            ClassAspect base = (ClassAspect)aspect.getAspect(k);

            for (Iterator itr = base.getPersistenceMappingIterator(); itr.hasNext();)
            {
               PersistenceMapping baseMapping = (PersistenceMapping)itr.next();

               if (aspect.findPersistenceMapping(baseMapping.getDataSource()) == null)
               {
                  PersistenceMapping mapping = baseMapping.create();

                  mapping.setMetaclass(aspect);
                  mapping.setDataSource(baseMapping.getDataSource());

                  aspect.addPersistenceMapping(mapping);
               }
            }
         }

         for (Iterator itr = aspect.getPersistenceMappingIterator(); itr.hasNext();)
         {
            PersistenceMapping mapping = (PersistenceMapping)itr.next();

            try
            {
               mapping.resolveInheritance();
            }
            catch (MetadataValidationException e)
            {
               eh = addException(eh, e);
            }
            catch (UncheckedException e)
            {
               MetadataValidationException x = new MetadataValidationException(e);

               mapping.setProperties(x);
               eh = addException(eh, x);
            }
         }
      }

      if (eh != null)
      {
         throw (MetadataException)eh;
      }
   }

   // inner classes

   /**
    * Metaclass-specific aspect helper.
    */
   protected final static class ClassAspectHelper extends AspectHelper
   {
      protected ClassAspect m_aspect;

      public ClassAspectHelper(ClassAspect aspect)
      {
         m_aspect = aspect;
      }

      /**
       * @see nexj.core.meta.AspectHelper#getContainer()
       */
      protected Aspect getContainer()
      {
         return m_aspect;
      }
   }
}
