// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.object;

import java.util.Set;

import nexj.core.meta.Attribute;
import nexj.core.meta.ContextMetadata;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartInstance;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;

/**
 * Object message part mapping.
 */
public class ObjectMessagePartMapping extends MetadataObject implements MessagePartMapping
{
   // constants

   /**
    * System attribute value for TransferObject class
    */
   public final static byte ATTR_CLASS = 0;

   /**
    * System attribute value for TransferObject event
    */
   public final static byte ATTR_EVENT = 1;

   /**
    * System attribute value for TransferObject object ID
    */
   public final static byte ATTR_OID = 2;

   /**
    * System attribute value for external key for the synchronized TransferObject
    */
   public final static byte ATTR_SYNC_KEY = 3;

   /**
    * System attribute value for external URL (if supported) for the synchronized TransferObject
    */
   public final static byte ATTR_SYNC_URL = 4;

   /**
    * System attribute value for external group ID (if supported) for the synchronized TransferObject. 
    * Group ID is used by an external system to link different instances together 
    * (e.g. appointment instances in multiple Exchange mailboxes, corresponding to the same meeting).
    */
   public final static byte ATTR_SYNC_GROUP = 5;

   /**
    * System attribute value for the version or locking attribute (if supported) of the synchronized TransferObject. 
    */
   public final static byte ATTR_SYNC_VERSION = 6;

   /**
    * Ordered values of system attributes of TransferObject
    */
   protected final static String[] SYSTEM_ATTRIBUTES = new String[]
   {
      ":class",
      ":event",
      ":oid",
      ":sync-key",
      ":sync-url",
      ":sync-group",
      ":sync-version",
   };

   // attributes

   /**
    * The association code. Can be null.
    */
   protected String m_sAssociationCode;

   /**
    * Index in the SYSTEM_ATTRIBUTES list of the name of the part system attribute (like :oid or :sync-key); -1 for regular attributes
    */
   protected byte m_nSystemAttribute = -1;

   /**
    * The key flag.
    * True if the value is used as lookup key
    * for the parent part instance.
    */
   protected boolean m_bKey;

   /**
    * The subkey flag.
    * True if the value is used as a lookup key
    * for the parent part instance, which in turn is
    * a lookup key for the grand-parent part instance.
    */
   protected boolean m_bSubKey;

   /**
    * The subkey parent.
    * True if there is a child message part, which is a subkey.
    */
   protected boolean m_bSubKeyParent;

   /**
    * The local key flag.
    * True to lookup the instance only within
    * the subcollection scoped by the parent part instance.
    */
   protected boolean m_bLocal = true;
   
   /**
    * The create-if-missing flag.
    * If true, a missing instance will be created.
    * If false, a failed instance lookup is an error. 
    */
   protected boolean m_bCreate = true;

   /**
    * The update-if-found flag.
    * If false, the underlying attribute (specified by m_attribute)
    * will not be updated on an already existing instance.
    */
   protected boolean m_bUpdate = true;
   
   /**
    * The delete-if-not-found flag.
    * If true, all unmatched instances of the underlying collection attribute 
    * (specified by m_attribute) will be deleted.
    */
   protected boolean m_bDelete;
   
   /**
    * The truncate flag.
    * If true, the underlying attribute value will be truncated to fit within the maximum length.
    */
   protected boolean m_bTruncated;
   
   /**
    * The dependency list created flag.
    * If true, the dependency list for the message part has been created.
    */
   protected boolean m_bDependencyMapCreated;

   /**
    * The primary flag.
    * Used to disambiguate the selection of a message when more than one message in
    * the inheritance hierarchy is mapped to the same class.
    */
   protected boolean m_bPrimary;

   /**
    * True, if the object mapping has another key (not subkey) defined besides the :sync-key.
    */
   protected boolean m_bAlternativeSyncKey;

   // associations
   
   /**
    * The part class.
    * Specifies the class to which the part maps.
    */
   protected Metaclass m_metaclass;

   /**
    * The part attribute.
    * Specifies the attribute to which the part maps.
    * Null for system attributes (like :oid or :sync-key)
    */
   protected Attribute m_attribute;
   
   /**
    * The update access attribute.
    * If its value is false, the update to the attribute (specified by m_attribute)
    * or the whole instance (in root part) will be skipped. 
    */
   protected Attribute m_accessAttribute;

   /**
    * The where clause for instance lookup.
    * Can be used only with a composite message parts.
    */
   protected Object m_where = Boolean.TRUE;

   /**
    * The child message part corresponding to the "updatable" attribute. Can be null.
    */
   protected MessagePart m_updateAccessPart;

   /**
    * The child message parts corresponding to the system attributes. Can be null.
    */
   protected MessagePart[] m_systemPartArray;
   
   /**
    * The map containing association paths indexed by dependent message parts.
    */
   protected Lookup m_assocPathByDependentMsgPartMap;

   /**
    * Map of metaclass to message (that can be up-cast to this mapping's message).
    * Valid only on mapping associated with root node. Message[Metaclass]
    */
   protected Lookup m_classMessageMap;

   /**
    * The message with which this mapping is associated.
    */
   protected Message m_message;

   /**
    * The map containing association paths indexed by association code.
    */
   protected Lookup m_assocPathByAssocCodeMap;

   // operations

   /**
    * Sets the part class.
    * @param metaclass The part class to set.
    */
   public void setMetaclass(Metaclass metaclass)
   {
      verifyNotReadOnly();
      m_metaclass = metaclass;
   }

   /**
    * @return The part class.
    */
   public Metaclass getMetaclass()
   {
      return m_metaclass;
   }

   /**
    * Sets the part attribute and update access attributes. Must be called after inheritance
    * has been resolved so that the attributes can be set correctly on the derived messages as well.
    * 
    * @param sAttributeName The attribute name; can be null.
    * @param sAccessAttributeName The update access attribute name; can be null.
    * @param part The message part.
    * @param message The message.
    */
   public void setAttributes(String sAttributeName, String sAccessAttributeName, MessagePart part, Message message)
   {
      setAttribute(sAttributeName, part);
      setAccessAttribute(sAccessAttributeName);

      String[] sPartNameArray = null;

      for (int i = 0, nCount = message.getDerivedMessageCount(); i < nCount; i++)
      {
         Message derivedMessage = message.getDerivedMessage(i);

         if (sPartNameArray == null)
         {
            int nSize = 0;

            for (MessagePart parent = part; parent.getParent() != null; parent = parent.getParent())
            {
               nSize++;
            }

            sPartNameArray = new String[nSize];

            for (int k = nSize - 1; k >= 0; k--)
            {
               sPartNameArray[k] = part.getName();
               part = part.getParent();
            }
         }

         if (sPartNameArray != null)
         {
            part = derivedMessage.getRoot();

            for (int k = 0; k < sPartNameArray.length; k++)
            {
               part = ((CompositeMessagePart)part).getPart(sPartNameArray[k]);
            }

            ObjectMessagePartMapping mapping = (ObjectMessagePartMapping)part.getMapping();

            mapping.setAttributes(sAttributeName, sAccessAttributeName, part, derivedMessage);
         }
      }
   }

   /**
    * Sets the part attribute.
    * @param sName The attribute name. Can be null.
    * @param part The message part.
    */
   public void setAttribute(String sName, MessagePart part)
   {
      verifyNotReadOnly();

      if (sName == null && part.getParent() != null)
      {
         sName = part.getName();
      }

      if (sName != null)
      {
         if (part.getParent() == null)
         {
            throw new MetadataException("err.meta.integration.object.mapping.misplacedAttribute");
         }

         if (sName.length() > 0 && sName.charAt(0) == ':')
         {
            for (byte i = 0; i < SYSTEM_ATTRIBUTES.length; ++i)
            {
               if (SYSTEM_ATTRIBUTES[i].equals(sName))
               {
                  m_nSystemAttribute = i;

                  return;
               }
            }
         }

         ObjectMessagePartMapping mapping = (ObjectMessagePartMapping)part.getParent().getMapping();

         if (mapping != null && mapping.getMetaclass() != null)
         {
            m_attribute = mapping.getMetaclass().getAttribute(sName);

            if (m_metaclass == null && !m_attribute.getType().isPrimitive())
            {
               m_metaclass = (Metaclass)m_attribute.getType();
            }
         }
      }
   }

   /**
    * @return One of the ATTR_* constants.
    */
   public byte getSystemAttribute()
   {
      return m_nSystemAttribute;
   }

   /**
    * @return The system attribute name or null.
    */
   public String getSystemAttributeName()
   {
      return (m_nSystemAttribute >= 0) ? (String)SYSTEM_ATTRIBUTES[m_nSystemAttribute] : null; 
   }

   /**
    * @return The part attribute.
    */
   public Attribute getAttribute()
   {
      return m_attribute;
   }

   /**
    * Sets the key flag.
    * @param bKey The key flag to set.
    */
   public void setKey(boolean bKey)
   {
      verifyNotReadOnly();
      m_bKey = bKey;

      if (m_bKey)
      {
         m_bLocal = false;
      }
   }

   /**
    * @return The key flag.
    */
   public boolean isKey()
   {
      return m_bKey;
   }

   /**
    * Sets the subkey flag.
    * @param bSubKey The subkey flag to set.
    */
   public void setSubKey(boolean bSubKey)
   {
      verifyNotReadOnly();
      m_bSubKey = bSubKey;
   }

   /**
    * @return The subkey flag.
    */
   public boolean isSubKey()
   {
      return m_bSubKey;
   }

   /**
    * @return The subkey parent.
    */
   public boolean isSubKeyParent()
   {
      return m_bSubKeyParent;
   }
   
   /**
    * Sets the local key flag.
    * @param bLocal The local key flag to set.
    */
   public void setLocal(boolean bLocal)
   {
      verifyNotReadOnly();
      m_bLocal = bLocal;
   }

   /**
    * @return The local key flag.
    */
   public boolean isLocal()
   {
      return m_bLocal;
   }

   /**
    * Sets the create-if-missing flag.
    * @param bCreate The create-if-missing flag to set.
    */
   public void setCreate(boolean bCreate)
   {
      verifyNotReadOnly();
      m_bCreate = bCreate;
   }

   /**
    * @return The create-if-missing flag.
    */
   public boolean isCreate()
   {
      return m_bCreate;
   }
   
   /**
    * Sets the update-if-found flag.
    * @param bUpdate The update-if-found flag to set.
    */
   public void setUpdate(boolean bUpdate)
   {
      verifyNotReadOnly();
      m_bUpdate = bUpdate;
   }

   /**
    * @return The update-if-found flag.
    */
   public boolean isUpdate()
   {
      return m_bUpdate;
   }

   /**
    * @return The delete-if-not-found flag.
    */
   public boolean isDelete()
   {
      return m_bDelete;
   }
   
   /**
    * Sets the delete-if-not-found flag.
    * @param bDelete The delete-if-not-found flag to set.
    */
   public void setDelete(boolean bDelete)
   {
      verifyNotReadOnly();
      m_bDelete = bDelete;
   }

   /**
    * Sets the truncate flag.
    * @param bTruncated The truncate flag. to set.
    */
   public void setTruncated(boolean bTruncated)
   {
      verifyNotReadOnly();
      m_bTruncated = bTruncated;
   }

   /**
    * @return The truncate flag.
    */
   public boolean isTruncated()
   {
      return m_bTruncated;
   }

   /**
    * Sets the primary flag. Used to disambiguate the selection of a message when more
    * than one message in the inheritance hierarchy is mapped to the same class.
    * @param bPrimary The value of the primary flag.
    */
   public void setPrimary(boolean bPrimary)
   {
      verifyNotReadOnly();
      m_bPrimary = bPrimary;
   }

   /**
    * Gets the primary flag. Used to disambiguate the selection of a message when more
    * than one message in the inheritance hierarchy is mapped to the same class.
    * @return The value of the primary flag.
    */
   public boolean isPrimary()
   {
      return m_bPrimary;
   }

   /**
    * Sets the update access attribute.
    * @param sName The attribute name. Can be null.
    */
   public void setAccessAttribute(String sName)
   {
      verifyNotReadOnly();

      if (sName == null)
      {
         m_accessAttribute = null;
      }
      else
      {
         Metaclass metaclass;

         if (m_attribute != null)
         {
            metaclass = m_attribute.getMetaclass();
         }
         else
         {
            metaclass = m_metaclass;
         }

         if (metaclass != null)
         {
            m_accessAttribute = metaclass.getAttribute(sName);
         }
      }
   }

   /**
    * Sets the update access attribute.
    * @param accessAttribute The update access attribute to set.
    */
   public void setAccessAttribute(Attribute accessAttribute)
   {
      verifyNotReadOnly();
      m_accessAttribute = accessAttribute;
   }

   /**
    * @return The update access attribute.
    */
   public Attribute getAccessAttribute()
   {
      return m_accessAttribute;
   }

   /**
    * Sets the child message part corresponding to the "updatable" attribute.
    * @param attrib The child message part corresponding to the "updatable" attribute to set. Can be null.
    */
   public void setUpdateAccessPart(MessagePart part)
   {
      verifyNotReadOnly();
      m_updateAccessPart = part;
   }

   /**
    * @return The child message part corresponding to the "updatable" attribute. Can be null.
    */
   public MessagePart getUpdateAccessPart()
   {
      return m_updateAccessPart;
   }

   /**
    * Sets the association code.
    * @param sValue The association code.
    */
   public void setAssociationCode(String sValue)
   {
      verifyNotReadOnly();
      m_sAssociationCode = sValue;
   }

   /**
    * @return The association code. Can be null.
    */
   public String getAssociationCode()
   {
      return m_sAssociationCode;
   }

   /**
    * Sets a child message part corresponding to a system attribute.
    * @param part The child message part to set. Can be a non-system part, in which case it is ignored.
    * @throws MetadataException if the mapping is duplicate.
    */
   public void setSystemPart(MessagePart part)
   {
      byte nAttr = ((ObjectMessagePartMapping)part.getMapping()).getSystemAttribute();

      if (nAttr >= 0)
      {
         if (m_systemPartArray == null)
         {
            m_systemPartArray = new MessagePart[SYSTEM_ATTRIBUTES.length];
         }

         if (m_systemPartArray[nAttr] != null && part != m_systemPartArray[nAttr])
         {
            throw new MetadataException("err.meta.integration.object.mapping.systemMappingDup",
               new Object[]{SYSTEM_ATTRIBUTES[nAttr], part.getFullPath()});
         }

         m_systemPartArray[nAttr] = part;
      }
   }

   /**
    * Gets the child message part mapped to a given system attribute.
    * @param nAttr One of the ATTR_* constants.
    * @return The child message part or null.
    */
   public MessagePart getSystemPart(int nAttr)
   {
      if (m_systemPartArray != null)
      {
         return m_systemPartArray[nAttr];
      }

      return null;
   }

   /**
    * Sets the where clause for lookup.
    * @param where The where clause for lookup to set.
    */
   public void setWhere(Object where)
   {
      verifyNotReadOnly();
      m_where = where;
   }

   /**
    * @return The where clause for lookup.
    */
   public Object getWhere()
   {
      return m_where;
   }

   /**
    * Gets the message with which this mapping is associated.
    * @return The message of this mapping.
    */
   public Message getMessage()
   {
      return m_message;
   }

   /**
    * @return True, if the object mapping has another key (not subkey) defined besides the :sync-key.
    */
   public boolean hasAlternativeSyncKey()
   {
      return m_bAlternativeSyncKey;
   }

   /**
    * Adds a mapping from a metaclass to its message.
    * @param clazz The class.
    * @param mappedMessage The message to which the class maps.
    */
   private void addClassMessage(Metaclass clazz, Message mappedMessage)
   {
      if (m_classMessageMap == null)
      {
         m_classMessageMap = new HashTab();
      }

      Message other = (Message)m_classMessageMap.put(clazz, mappedMessage);
      Message baseMessage;

      if (other == null)
      {
         // No collision, add mapping to parent as well.
         if ((baseMessage = m_message.getBaseMessage()) != null)
         {
            ((ObjectMessagePartMapping)baseMessage.getRoot().getMapping()).addClassMessage(clazz, mappedMessage);
         }
      }
      else
      {
         // If old entry is base of new entry, new entry is left in map: it is more specific.
         // Otherwise, update map with a more general entry.
         if (!other.isUpcast(mappedMessage))
         {
            if (!mappedMessage.isUpcast(other))
            {
               // The old entry and new entry do not inherit from each other.
               ObjectMessagePartMapping mappedMessageMapping = (ObjectMessagePartMapping)other.getRoot().getMapping();
               ObjectMessagePartMapping otherMapping = (ObjectMessagePartMapping)other.getRoot().getMapping();

               if (otherMapping.isPrimary() ^ mappedMessageMapping.isPrimary())
               {
                  if (otherMapping.isPrimary())
                  {
                     // The old entry takes precedence over the new entry.
                     m_classMessageMap.put(clazz, mappedMessage = other);
                  }
               }
               else
               {
                  // Unable to disambiguate; map to their common base.
                  assert m_message.isUpcast(other) && m_message.isUpcast(mappedMessage);
                  m_classMessageMap.put(clazz, mappedMessage = m_message);
               }
            }

            // Add the mapping to our parent as well.
            if ((baseMessage = m_message.getBaseMessage()) != null)
            {
               ((ObjectMessagePartMapping)baseMessage.getRoot().getMapping()).addClassMessage(clazz, mappedMessage);
            }
         }
      }
   }

   /**
    * Finds a message that can be up-cast to the message associated with this mapping and
    * that best represents the given metaclass (i.e. most-strictly encloses the metaclass).
    * @param clazz The metaclass.
    * @return The message that most-strictly encloses the metaclass.
    */
   public Message findMessage(Metaclass clazz)
   {
      if (m_classMessageMap == null)
      {
         return (m_metaclass == clazz) ? m_message : null;
      }

      return (Message)m_classMessageMap.get(clazz);
   }

   /**
    * Build a list of all attributes included in the given message part.
    * @param composite The composite message part.
    * @return list of all attributes included in the message
    */
   public static Pair getAttributes(CompositeMessagePart composite)
   {
      ObjectMessagePartMapping mapping = (ObjectMessagePartMapping)composite.getMapping();
      Set currPathSet = new HashHolder();     // keeps track of cycle(s) in messages

      currPathSet.add(composite);

      return (Pair)mapping.getAttributes(composite, currPathSet, null);
   }

   /**
    * Build a list of all attributes included in the given message part and its descendants.
    * @param part The message part.
    * @param currPathSet The current path.
    * @return The list of attributes or an attribute symbol.
    */
   private Object getAttributes(MessagePart part, Set currPathSet, Message message)
   {
      CompositeMessagePart refPart = null;
      ObjectMessagePartMapping partMapping = (ObjectMessagePartMapping)part.getMapping();

      if (part instanceof PrimitiveMessagePart)
      {
         if (message != null && part.getDeclarator() == message && partMapping.getMessage() != message)
         {
            return null;
         }

         return (m_attribute == null) ? null : m_attribute.getSymbol();
      }
      else if (part instanceof CompositeMessagePartRef)
      {
         refPart = ((CompositeMessagePartRef)part).getRefPart();
         message = refPart.getDeclarator();

         if (!currPathSet.add(refPart))
         {
            return (m_attribute == null) ? null : new Pair(m_attribute.getSymbol());
         }
      }

      CompositeMessagePart composite = (CompositeMessagePart)part;
      Pair attributes = null;

      for (int i = composite.getPartCount() - 1; i >= 0; --i)
      {
         MessagePart childPart = composite.getPart(i);
         ObjectMessagePartMapping childMapping = (ObjectMessagePartMapping)childPart.getMapping();
         Object childAttributes = childMapping.getAttributes(childPart, currPathSet, message);

         if (childAttributes != null)
         {
            attributes = new Pair(childAttributes, attributes);
         }
      }

      // Add attributes from derived messages
      if (refPart != null)
      {
         ObjectMessagePartMapping refMapping = (ObjectMessagePartMapping)refPart.getMapping();
         Message refMessage = refMapping.getMessage();

         for (int i = 0, nCount = refMessage.getDerivedMessageCount(); i < nCount; i++)
         {
            Message derivedMessage = refMessage.getDerivedMessage(i);
            CompositeMessagePart derivedRoot = derivedMessage.getRoot();
            ObjectMessagePartMapping derivedMapping = (ObjectMessagePartMapping)derivedRoot.getMapping();
            Object derivedAttributes = derivedMapping.getAttributes(derivedRoot, currPathSet, refMessage);

            if (derivedAttributes != null)
            {
               if (derivedAttributes instanceof Symbol)
               {
                  derivedAttributes = new Pair(derivedAttributes);
               }

               if (derivedMapping.getMetaclass() != refMapping.getMetaclass())
               {
                  derivedAttributes = new Pair(Symbol.ATAT,
                     new Pair(derivedMapping.getMetaclass().getSymbol(), derivedAttributes));
                  attributes = new Pair(derivedAttributes, attributes);
               }
               else
               {
                  attributes = Pair.nconc((Pair)derivedAttributes, attributes);
               }
            }
         }

         currPathSet.remove(refPart);
      }

      return (m_attribute == null) ? attributes : new Pair(m_attribute.getSymbol(), attributes);
   }

   /**
    * Computes the shortest association code path, one of which is a suffix of the other,
    * which prefix refers to its own beginning, i.e. makes a full cycle.
    * @param associationPath The association path.
    * @param oldPath The existing association path.
    * @return The shortest association path or null if no such path can be found.
    */
   private Pair findCommonAssocSuffix(Pair associationPath, Pair oldPath)
   {
      int nDiffLen = Pair.length(associationPath) - Pair.length(oldPath);
      Pair longerPath = (nDiffLen > 0) ? associationPath : oldPath;

      if (nDiffLen != 0)
      {
         CompositeMessagePart rootPart = ((CompositeMessagePart)longerPath.getHead()).getRoot();

         for (int i = Math.abs(nDiffLen); i > 1; i--, longerPath = longerPath.getNext()) ;

         CompositeMessagePart recursivePart = (CompositeMessagePart)longerPath.getHead();

         if (recursivePart instanceof CompositeMessagePartRef)
         {
            CompositeMessagePart refPart = ((CompositeMessagePartRef)recursivePart).getRefPart();

            if (refPart != rootPart)
            {
               return null;
            }
         }

         longerPath = longerPath.getNext();
      }

      if (!ObjUtil.equal((nDiffLen <= 0) ? associationPath : oldPath, longerPath))
      {
         return null;
      }

      return longerPath;
   }

   /**
    * Adds a dependent message part and its corresponding association path to the map.
    * @param part The dependent message part.
    * @param associationPath The association path.
    * @param bRoot True if root message part.
    * @throws MetadataValidationException if the association code is not unique.
    */
   private void addAssociationPath(MessagePart part, Pair associationPath, boolean bRoot)
   {
      if (associationPath != null)
      {
         Pair pathToAdd = associationPath;

         if (m_assocPathByDependentMsgPartMap == null)
         {
            m_assocPathByDependentMsgPartMap = new HashTab();
         }
         else
         {
            Pair oldPath = (Pair)m_assocPathByDependentMsgPartMap.get(part);

            if (oldPath != null)
            {
               pathToAdd = findCommonAssocSuffix(associationPath, oldPath);
            }
            else if (m_assocPathByAssocCodeMap != null)
            {
               ObjectMessagePartMapping partMapping = (ObjectMessagePartMapping)part.getMapping();

               if (m_assocPathByAssocCodeMap.get(partMapping.getAssociationCode()) != null)
               {
                  pathToAdd = null;
               }
            }

            if (pathToAdd == null)
            {
               ObjectMessagePartMapping partMapping = (ObjectMessagePartMapping)part.getMapping();

               throw new MetadataValidationException("err.meta.integration.object.mapping.associationCodeDup",
                  new Object[]{partMapping.getAssociationCode(), part.getFullPath()});
            }
         }

         m_assocPathByDependentMsgPartMap.put(part, pathToAdd);

         if (bRoot)
         {
            if (m_assocPathByAssocCodeMap == null)
            {
               m_assocPathByAssocCodeMap = new HashTab();
            }

            m_assocPathByAssocCodeMap.put(
               ((ObjectMessagePartMapping)part.getMapping()).getAssociationCode(), pathToAdd);
         }
      }
   }

   /**
    * @return The reverse association path for the dependentPart.
    * @param dependentPart The dependent message part.
    */
   public Pair getReverseAssociationPath(MessagePart dependentPart)
   {
      Pair associationPath = (dependentPart != null && m_assocPathByDependentMsgPartMap != null) ?
         (Pair)m_assocPathByDependentMsgPartMap.get(dependentPart) : null;

      Pair reverseAssocPath = null;

      if (associationPath != null)
      {
         for (; associationPath != null; associationPath = associationPath.getNext())
         {
            Symbol partSymbol = ((ObjectMessagePartMapping)((MessagePart)associationPath.getHead()).getMapping())
               .getAttribute().getSymbol();

            reverseAssocPath = new Pair(partSymbol, reverseAssocPath);
         }

         reverseAssocPath = new Pair(Symbol.ATAT, new Pair(getMetaclass().getSymbol(),
            Pair.nreverse(reverseAssocPath)));
      }

      return reverseAssocPath;
   }

   /**
    * @return The dependent message parts count.
    */
   public int getDependentMessagePartsCount()
   {
      return (m_assocPathByDependentMsgPartMap != null) ? m_assocPathByDependentMsgPartMap.size() : 0;
   }

   /**
    * @return The dependent message parts iterator.
    */
   public Lookup.Iterator getDependentMessagePartsIterator()
   {
      return (m_assocPathByDependentMsgPartMap != null) ? m_assocPathByDependentMsgPartMap.iterator() : null;
   }

   /**
    * Builds a map of dependent message parts and their corresponding association paths.
    * @param composite The composite message part.
    */
   protected void buildDependentMsgPartsMap(CompositeMessagePart composite)
   {
      if (!m_bDependencyMapCreated)
      {
         Set currPathSet = new HashHolder();

         currPathSet.add(composite);
         buildDependentMsgPartsMap(composite, currPathSet);
         m_bDependencyMapCreated = true;
      }
   }

   /**
    * @return The association path corresponding to the association code.
    */
   public Pair getAssociationPath(Object associationCode)
   {
      return (associationCode != null && m_assocPathByAssocCodeMap != null) ?
         (Pair)m_assocPathByAssocCodeMap.get(associationCode) :
         null;
   }

   /**
    * Builds a map of dependent message parts and their association paths. If the referenced message contains
    * a non-null value for the associationCode attribute, add this part's mapping to its parent's dependency
    * map along with the association path. This process continues until root is reached.
    */
   protected void buildDependentMsgPartsMap(CompositeMessagePart composite, Set currPathSet)
   {
      for (int i = 0, n = composite.getPartCount(); i != n; ++i)
      {
         MessagePart msgPart = composite.getPart(i);

         if (msgPart instanceof CompositeMessagePart)
         {
            CompositeMessagePart part = (CompositeMessagePart)msgPart;
            ObjectMessagePartMapping partMapping = (ObjectMessagePartMapping)part.getMapping();

            if (partMapping.getAssociationCode() != null)
            {
               Pair associationPath = new Pair(part);
               CompositeMessagePart rootPart = part.getRoot();

               for (MessagePart parentMsgPart = part.getParent(); parentMsgPart != null; parentMsgPart = parentMsgPart.getParent())
               {
                  ObjectMessagePartMapping parentMsgPartMapping = (ObjectMessagePartMapping)parentMsgPart.getMapping();
                  boolean bRoot = (parentMsgPart == rootPart);

                  if (parentMsgPartMapping.isDelete() || bRoot)
                  {
                     parentMsgPartMapping.addAssociationPath(part, associationPath, bRoot);
                  }

                  if (parentMsgPartMapping.getAttribute() != null)
                  {
                     associationPath = new Pair(parentMsgPart, associationPath);
                  }
               }
            }

            if (currPathSet.add(part))
            {
               buildDependentMsgPartsMap(part, currPathSet);
               currPathSet.remove(part);
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#init(nexj.core.meta.integration.MessagePart)
    */
   public void init(MessagePart part)
   {
      m_message = part.getRoot().getDeclarator();
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#resolveInheritance(nexj.core.meta.integration.MessagePartMapping)
    */
   public void resolveInheritance(MessagePartMapping baseMapping)
   {
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#refer(CompositeMessagePartRef)
    */
   public void refer(CompositeMessagePartRef ref)
   {
      ObjectMessagePartMapping mapping = (ObjectMessagePartMapping)ref.getRefPart().getMapping();
      CompositeMessagePart rootPart = ref.getRoot();
      ObjectMessagePartMapping rootMapping = (ObjectMessagePartMapping)rootPart.getMapping();

      // Build the dependent message parts map for the root message.
      if (rootMapping != null)
      {
         rootMapping.buildDependentMsgPartsMap(rootPart);
      }

      if (mapping != this)
      {
         if (mapping.m_systemPartArray != null)
         {
            for (int i = 0; i < SYSTEM_ATTRIBUTES.length; ++i)
            {
               MessagePart part = mapping.getSystemPart(i);

               if (part != null)
               {
                  setSystemPart(part);
               }
            }
         }

         // Build the dependent message parts map for the referenced message.
         mapping.buildDependentMsgPartsMap(ref.getRefPart());

         Pair localAssocPath = null;

         // Copy the dependency list from the referenced part to the referee while appending to the association paths.
         for (CompositeMessagePart partToResolve = ref; partToResolve != null; partToResolve = partToResolve.getParent())
         {
            ObjectMessagePartMapping partParentMapping = (ObjectMessagePartMapping)partToResolve.getMapping();

            if (mapping.getDependentMessagePartsCount() > 0)
            {
               for (Lookup.Iterator itr = mapping.getDependentMessagePartsIterator(); itr.hasNext();)
               {
                  CompositeMessagePart part = (CompositeMessagePart)itr.next();
                  boolean bRoot = (partToResolve == rootPart);

                  // skip itself, however add to root even if the delete flag is not set on it
                  if (partToResolve != part && (partParentMapping.isDelete() || bRoot))
                  {
                     partParentMapping.addAssociationPath(part, Pair.append(localAssocPath, (Pair)itr.getValue()),
                        bRoot);
                  }
               }
            }

            if (partParentMapping != null && partParentMapping.getAttribute() != null)   // skip root
            {
               localAssocPath = new Pair(partToResolve, localAssocPath);
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#finish(nexj.core.meta.integration.MessagePart)
    */
   public void finish(MessagePart part)
   {
      MessagePart parent = part.getParent();

      if (m_attribute != null)
      {
         if (m_attribute.isStatic())
         {
            throw new MetadataException("err.meta.integration.object.mapping.staticAttribute",
               new Object[]{part.getFullPath()});
         }

         if ((part instanceof PrimitiveMessagePart) != m_attribute.getType().isPrimitive())
         {
            throw new MetadataException("err.meta.integration.object.mapping.attributeTypeMismatch",
               new Object[]{part.getFullPath()});
         }

         if (m_bLocal && part instanceof CompositeMessagePart && m_attribute.getReverse() == null)
         {
            throw new MetadataException("err.meta.integration.object.mapping.missingLocalReverseAssoc",
               new Object[]{m_attribute.getName(), part.getFullPath()});
         }
      }

      if (m_metaclass != null)
      {
         if (part instanceof PrimitiveMessagePart)
         {
            throw new MetadataException("err.meta.integration.object.mapping.misplacedClass");
         }
         
         if (m_attribute != null && !((Metaclass)m_attribute.getType()).isUpcast(m_metaclass))
         {
            throw new MetadataException("err.meta.integration.object.mapping.classTypeMismatch",
               new Object[]{m_metaclass.getName()});
         }
      }
      else
      {
         if (parent == null)
         {
            if (m_message.getDerivation() != Message.DERIVATION_ABSTRACT)
            {
               throw new MetadataException("err.meta.integration.object.mapping.missingClass");
            }
         }
         
         if (m_attribute != null && !m_attribute.getType().isPrimitive())
         {
            m_metaclass = (Metaclass)m_attribute.getType();
         }
      }

      if (part instanceof PrimitiveMessagePart)
      {
         if (part.isCollection())
         {
            throw new MetadataException("err.meta.integration.object.mapping.primitiveCollection");
         }

         if (m_where != Boolean.TRUE)
         {
            throw new MetadataException("err.meta.integration.object.mapping.primitivePartWhere",
               new Object[]{part.getFullPath()});
         }
      }

      if (parent == null)
      {
         m_bLocal = false;
      }
      else
      {
         ObjectMessagePartMapping mapping = (ObjectMessagePartMapping)parent.getMapping();

         if (mapping != null)
         {
            mapping.setSystemPart(part);
         }
      }

      if (m_bSubKey)
      {
         ObjectMessagePartMapping mapping = (parent == null) ? null : (ObjectMessagePartMapping)parent.getMapping();

         if (mapping != null)
         {
            mapping.m_bSubKeyParent = true;
         }
      }

      if (m_accessAttribute != null && m_accessAttribute.getType() != Primitive.BOOLEAN)
      {
         throw new MetadataException("err.meta.integration.object.mapping.accessAttributeType",
            new Object[]{m_accessAttribute.getName()});
      }

      if (parent == null && m_metaclass != null &&
         (m_message.getBaseMessage() != null || m_message.getDerivedMessageCount() > 0))
      {
         addClassMessage(m_metaclass, m_message);
      }

      if (part instanceof CompositeMessagePartInstance)
      {
         CompositeMessagePart composite = (CompositeMessagePartInstance)part;

         for (int i = 0, nCount = composite.getPartCount(); i < nCount; i++)
         {
            MessagePart child = composite.getPart(i);
            MessagePartMapping mapping = child.getMapping();

            if (mapping != null)
            {
               mapping.finish(child);
            }
         }
      }

      if (part.equals(part.getRoot()) && part instanceof CompositeMessagePart)
      {
         CompositeMessagePart rootComposite = (CompositeMessagePart)part;
         ObjectMessagePartMapping rootMapping = (ObjectMessagePartMapping)rootComposite.getMapping();
         MessagePart syncKeyPart = rootMapping.getSystemPart(ATTR_SYNC_KEY);

         if (syncKeyPart != null)
         {
            for (int i = 0, nCount = rootComposite.getPartCount(); i < nCount; i++)
            {
               MessagePart childPart = rootComposite.getPart(i);

               if (syncKeyPart != childPart)
               {
                  ObjectMessagePartMapping childMapping = (ObjectMessagePartMapping)childPart.getMapping();

                  if (childMapping.isKey())
                  {
                     rootMapping.m_bAlternativeSyncKey = true;
                  }
               }
            }
         }
      }
   }

   /**
    * Resolves attributes that could not be determined during the original
    * initialization due to incomplete metadata.
    */
   public void finish2(MessagePart part)
   {
      if (part instanceof PrimitiveMessagePart)
      {
         return;
      }

      CompositeMessagePart composite = (CompositeMessagePart)part;

      for (int i = composite.getPartCount() - 1; i >= 0; --i)
      {
         MessagePart childPart = composite.getPart(i);
         ObjectMessagePartMapping partMapping = (ObjectMessagePartMapping)childPart.getMapping();

         if (partMapping.getAttribute() != null &&
            partMapping.getAttribute() == m_metaclass.getUpdateAccessAttribute())
         {
            setUpdateAccessPart(childPart);

            break;
         }
      }

      if (composite instanceof CompositeMessagePartInstance)
      {
         for (int i = 0, nSize = composite.getPartCount(); i < nSize; i++)
         {
            MessagePart child = composite.getPart(i);

            ((ObjectMessagePartMapping)child.getMapping()).finish2(child);
         }
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#clone()
    */
   public Object clone()
   {
      ObjectMessagePartMapping copy = (ObjectMessagePartMapping)super.clone();

      copy.m_assocPathByDependentMsgPartMap = null;
      copy.m_bDependencyMapCreated = false;
      copy.m_bSubKeyParent = false;
      copy.m_updateAccessPart = null;
      copy.m_systemPartArray = null;

      return copy;
   }

   /**
    * @see nexj.core.meta.integration.MessagePartMapping#validate(nexj.core.meta.ContextMetadata, nexj.core.util.ExceptionHolder, nexj.core.meta.integration.MessagePart)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings, MessagePart part)
   {
      if (warnings != null)
      {
         MessagePart parent = part.getParent();
         ObjectMessagePartMapping mapping = (parent == null) ? null : (ObjectMessagePartMapping)parent.getMapping();

         if (m_bSubKey && (mapping == null || !mapping.isKey() || !mapping.isLocal()))
         {
            MetadataValidationException e = new MetadataValidationException("err.meta.integration.object.mapping.nonKeyLocalSubKey");

            setProperties(e, part);
            warnings.addException(e);
         }

         if (m_bKey && part.isCollection() && !m_bLocal)
         {
            MetadataValidationException e = new MetadataValidationException("err.meta.integration.object.mapping.collectionKeyNeedsSubKey");

            setProperties(e, part);
            warnings.addException(e);
         }

         if (m_bKey && m_bLocal && !m_bSubKeyParent)
         {
            MetadataValidationException e = new MetadataValidationException("err.meta.integration.object.mapping.localKeyNeedsSubKey");

            setProperties(e, part);
            warnings.addException(e);
         }
      }
   }

   /**
    * Sets the metadata marker properties.
    * @param marker The destination marker.
    * @param part The message part
    */
   public void setProperties(MetadataMarker marker, MessagePart part)
   {
      marker.setTypeName("Message");
      marker.setProperty("part", part.getName());
      marker.setProperty("message", part.getRoot().getName());
   }
}
