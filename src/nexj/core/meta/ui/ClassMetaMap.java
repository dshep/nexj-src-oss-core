// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.ui;

/**
 * Provides access to ClassMeta instances.
 */
public interface ClassMetaMap
{
   /**
    * Gets class metadata from the client cache.
    * @param sName Metaclass name.
    * @return ClassMeta object.
    */
   ClassMeta getClassMeta(String sName);
   
   /**
    * Finds class metadata from the client cache.
    * @param sName Metaclass name.
    * @return ClassMeta object.
    */
   ClassMeta findClassMeta(String sName);
}
