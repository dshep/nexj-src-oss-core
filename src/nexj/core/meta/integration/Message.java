// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import nexj.core.integration.IntegrationException;
import nexj.core.meta.ContextMetadata;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataCompoundValidationException;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataResource;
import nexj.core.meta.NamedMetadataObject;
import nexj.core.rpc.TransferObject;
import nexj.core.util.EmptyIterator;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.Holder;
import nexj.core.util.IdentityHashHolder;
import nexj.core.util.PropertyMap;
import nexj.core.util.StringUtil;
import nexj.core.util.UncheckedException;

/**
 * Message metadata.
 */
public final class Message extends NamedMetadataObject implements MetadataResource, TransformationEndpoint
{
   // constants

   /**
    * Polymorphism allowed for the message.
    */
   public final static byte DERIVATION_VIRTUAL = 0;

   /**
    * Polymorphism not allowed for the message.
    * 
    * When set, a message parser may not parse this message or a reference to
    * this message as anything other than this message. Applies to the
    * formatter as well.
    */
   public final static byte DERIVATION_FINAL = 1;

   /**
    * Polymorphism allowed, derived message must be used.
    * 
    * When set, a message parser must parse this message or a reference to
    * this message as a non-abstract derived message of this message. Applies
    * to the formatter as well.
    */
   public final static byte DERIVATION_ABSTRACT = 2;

   // attributes

   /**
    * The polymorphism setting for this message. One of the DERIVATION_* constants.
    */
   protected byte m_nDerivation;

   /**
    * Whether references have been resolved.
    */
   protected boolean m_bReferencesResolved;

   /**
    * The metadata resource name.
    */
   protected String m_sResourceName;

   // associations

   /**
    * The message format.
    */
   protected Format m_format;

   /**
    * The root metadata object.
    */
   protected Metadata m_metadata;

   /**
    * The response message.
    */
   protected Message m_response;

   /**
    * The base message, if any.
    */
   protected Message m_base;

   /**
    * The derived messages, if any.
    */
   protected List m_derivedMessageList; // Message[]

   /**
    * The root composite message part.
    */
   protected CompositeMessagePart m_root;

   /**
    * List of ref parts.
    */
   protected List m_refPartList; // CompositeMessagePartRef[]

   /**
    * List of referrer messages.
    */
   protected List m_referrerList; // Message[]

   // constructors

   /**
    * Constructs the message.
    * @param sName The message name.
    */
   public Message(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the message.
    */
   public Message()
   {
      super();
   }

   // operations

   /**
    * Sets the root metadata object.
    * @param metadata The root metadata object to set.
    */
   public void setMetadata(Metadata metadata)
   {
      verifyNotReadOnly();
      m_metadata = metadata;
   }

   /**
    * @return The root metadata object.
    */
   public Metadata getMetadata()
   {
      return m_metadata;
   }

   /**
    * @see nexj.core.meta.MetadataResource#setResourceName(java.lang.String)
    */
   public void setResourceName(String sName)
   {
      verifyNotReadOnly();
      m_sResourceName = sName;
   }

   /**
    * @see nexj.core.meta.MetadataResource#getResourceName()
    */
   public String getResourceName()
   {
      return m_sResourceName;
   }

   /**
    * Sets the message format.
    * @param format The message format to set.
    */
   public void setFormat(Format format)
   {
      verifyNotReadOnly();
      m_format = format;
   }

   /**
    * @return The message format.
    */
   public Format getFormat()
   {
      return m_format;
   }

   /**
    * Sets the response message.
    * @param response The response message to set.
    */
   public void setResponse(Message response) throws MetadataException
   {
      verifyNotReadOnly();
      m_response = response;
      
      if (response != null && m_format != null && response.getFormat() != m_format)
      {
         throw new MetadataException("err.meta.integration.responseFormat",
            new Object[]{getName()});
      }
   }

   /**
    * @return The response message.
    */
   public Message getResponse()
   {
      return m_response;
   }
   
   /**
    * Sets the root composite message part.
    * @param root The root composite message part to set.
    */
   public void setRoot(CompositeMessagePart root)
   {
      verifyNotReadOnly();
      m_root = root;
   }

   /**
    * @return The root composite message part.
    */
   public CompositeMessagePart getRoot()
   {
      return m_root;
   }

   /**
    * Sets the base message.
    * @param msg The base message to set, if any.
    */
   public void setBaseMessage(Message msg)
   {
      verifyNotReadOnly();
      m_base = msg;
   }

   /**
    * Gets the base message.
    * @return The base message, if any.
    */
   public Message getBaseMessage()
   {
      return m_base;
   }

   /**
    * Gets the root base message. The root base message is the highest message above
    * (or including) this message in the message inheritance hierarchy. If this message
    * does not inherit from any other messages, then it is the root base message.
    * @return The root base message.
    */
   public Message getRootBaseMessage()
   {
      Message root = this;

      while (root.m_base != null)
      {
         root = root.m_base;
      }

      return root;
   }

   /**
    * Determines if a message can be upcast to obtain this message.
    * @param message The message to upcast.
    * @return True if this message can be obtained by upcasting message.
    */
   public boolean isUpcast(Message message)
   {
      while (message != null)
      {
         if (message == this)
         {
            return true;
         }

         message = message.m_base;
      }

      return false;
   }

   /**
    * Adds a derived message.
    * @param msg The derived message to add.
    */
   public void addDerivedMessage(Message msg)
   {
      verifyNotReadOnly();

      if (m_derivedMessageList == null)
      {
         m_derivedMessageList = new ArrayList();
      }

      m_derivedMessageList.add(msg);
   }

   /**
    * Gets a derived message of the given index.
    * @param i The index of the derived message to get.
    * @return The derived message.
    */
   public Message getDerivedMessage(int i)
   {
      if (m_derivedMessageList == null)
      {
         throw new IndexOutOfBoundsException();
      }

      return (Message)m_derivedMessageList.get(i);
   }

   /**
    * Gets the number of derived messages.
    * @return The count of derived messages.
    */
   public int getDerivedMessageCount()
   {
      return (m_derivedMessageList == null) ? 0 : m_derivedMessageList.size();
   }

   /**
    * Sets the derivation flag.
    * @param nDerivation One of the DERIVATION_* constants.
    */
   public void setDerivation(byte nDerivation)
   {
      verifyNotReadOnly();
      m_nDerivation = nDerivation;
   }

   /**
    * Gets the derivation flag.
    * @return One of the DERIVATION_* constants
    */
   public byte getDerivation()
   {
      return m_nDerivation;
   }

   /**
    * Resolves references for the messages specified by the iterator.
    * @param msgIterator An iterator over the set of messages for which
    * reference resolution shall be performed.
    */
   public static void resolveReferences(Iterator msgIterator)
   {
      while (msgIterator.hasNext())
      {
         ((Message)msgIterator.next()).resolveReferences();
      }
   }

   /**
    * Resolves inheritance for the messages specified by the iterator.
    * @param msgIterator An iterator over the set of messages for which
    * inheritance resolution shall be performed.
    * @param sortedMessageList The list to populate with messages in the
    * order they are resolved. Can be null.
    */
   public static void resolveInheritance(Iterator msgIterator, List sortedMessageList)
   {
      MetadataCompoundValidationException comp = null;
      Holder circularMessageSet = new IdentityHashHolder();
      Holder resolvedMessageSet = new IdentityHashHolder();

      while (msgIterator.hasNext())
      {
         Message message = (Message)msgIterator.next();

         circularMessageSet.add(message);

         // Process inheritance starting on the root messages.
         if (message.m_base == null)
         {
            try
            {
               message.resolveInheritance(resolvedMessageSet, sortedMessageList);
            }
            catch (UncheckedException ex)
            {
               if (comp == null)
               {
                  comp = new MetadataCompoundValidationException();
               }

               message.addException(comp, ex);
            }
         }
      }

      // Inheritance cycles have no message where m_base == null, so they are never resolved.
      if (circularMessageSet.size() != resolvedMessageSet.size())
      {
         circularMessageSet.removeAll(resolvedMessageSet);

         if (comp == null)
         {
            comp = new MetadataCompoundValidationException();
         }

         for (Iterator itr = circularMessageSet.iterator(); itr.hasNext(); )
         {
            Message circularMessage = (Message)itr.next();
            MetadataException x = new MetadataException(
               "err.meta.integration.circularInheritance",
               new Object[] {circularMessage.getName()});

            circularMessage.addException(comp, x);
         }
      }

      if (comp != null)
      {
         throw comp;
      }
   }

   /**
    * Resolves inheritance for the message hierarchy starting at this message.
    * @param resolvedMessageSet The set to which messages shall be added
    * after processing.
    * @param sortedMessageList The list to populate with messages in the
    * order they are resolved. Can be null.
    */
   protected void resolveInheritance(Holder resolvedMessageSet, List sortedMessageList)
   {
      if (sortedMessageList != null)
      {
         sortedMessageList.add(this);
      }

      for (int i = 0, nSize = getDerivedMessageCount(); i < nSize; i++)
      {
         Message child = (Message)m_derivedMessageList.get(i);

         if (m_format != child.m_format)
         {
            throw new MetadataException("err.meta.integration.baseFormatMismatch",
               new Object[]{getName(), child.getName()});
         }

         child.getRoot().resolveInheritance(m_root);
         child.resolveInheritance(resolvedMessageSet, sortedMessageList);
      }

      resolvedMessageSet.add(this);
   }

   /**
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, nexj.core.util.ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      super.validate(metadata, warnings);

      m_root.validate(metadata, warnings);
   }

   /**
    * Checks that derivedMessage can be substituted for baseMessage during a message formatting
    * or parsing operation. Verifies the derivation mode and the inheritance relationship.
    * 
    * @param baseMessage The message to be substituted.
    * @param derivedMessage The message to substitute.
    * @param part The message part being checked.
    * @throws IntegrationException If derivedMessage may not be substituted for baseMessage.
    */
   public static void validatePolymorphism(Message baseMessage, Message derivedMessage, MessagePart part)
      throws IntegrationException
   {
      if (baseMessage.getDerivation() != DERIVATION_FINAL)
      {
         if (!baseMessage.isUpcast(derivedMessage))
         {
            throw new IntegrationException("err.integration.messageTypeMismatch",
               new Object[]{part.getFullPath(), baseMessage.getName(), derivedMessage.getName()});
         }

         if (derivedMessage.getDerivation() == DERIVATION_ABSTRACT)
         {
            throw new IntegrationException("err.integration.abstractMessage",
               new Object[]{derivedMessage.getName(), part.getFullPath()});
         }
      }
      else if (derivedMessage != baseMessage)
      {
         throw new IntegrationException("err.integration.messageTypeMismatch",
            new Object[]{part.getFullPath(), baseMessage.getName(), derivedMessage.getName()});
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      marker.setTypeName("Message");
      marker.setResourceName(m_sResourceName);
      marker.setProperty("message", m_sName);
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();

      m_root.makeReadOnly();

      if (m_derivedMessageList instanceof ArrayList)
      {
         ((ArrayList)m_derivedMessageList).trimToSize();
      }

      if (m_refPartList != null)
      {
         ((ArrayList)m_refPartList).trimToSize();
      }

      if (m_referrerList != null)
      {
         ((ArrayList)m_referrerList).trimToSize();
      }
   }

   /**
    * Add the ref part to the list of ref parts and this message to the list of referrers of the given ref
    * part.
    * @param refPart Ref part.
    */
   public void addRef(CompositeMessagePartRef refPart)
   {
      verifyNotReadOnly();

      if (m_refPartList == null)
      {
         m_refPartList = new ArrayList();
      }

      m_refPartList.add(refPart);

      Message message = refPart.getRefPart().getDeclarator();

      if (message.m_referrerList == null)
      {
         message.m_referrerList = new ArrayList();
      }

      message.m_referrerList.add(this);
   }

   /**
    * Get referrer list iterator.
    * @return Referrer list iterator.
    */
   public Iterator getReferrerListIterator()
   {
      if (m_referrerList == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_referrerList.iterator();
   }

   /**
    * Resolve message references.
    */
   public void resolveReferences()
   {
      if (m_bReferencesResolved)
      {
         return;
      }

      resolveReferences(new IdentityHashHolder());
      m_bReferencesResolved = true;
   }

   /**
    * Resolve message references.
    * @param visitedSet Set of visited messages.
    */
   public void resolveReferences(Set visitedSet)
   {
      if (m_bReferencesResolved || !visitedSet.add(this))
      {
         return;
      }

      if (m_refPartList != null)
      {
         for (Iterator itr = m_refPartList.iterator(); itr.hasNext();)
         {
            CompositeMessagePartRef refPart = (CompositeMessagePartRef)itr.next();

            refPart.getRefPart().getDeclarator().resolveReferences(visitedSet);
            refPart.getMapping().refer(refPart);
         }
      }
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#getChild(java.lang.String)
    */
   public EndpointPart getChild(String sName)
   {
      if (!StringUtil.isEmpty(sName) && sName.charAt(0) == ':')
      {
         if (sName.equals(":oid"))
         {
            return Transformation.OID;
         }

         if (sName.equals(":class"))
         {
            return Transformation.CLASS;
         }

         if (sName.equals(":event"))
         {
            return Transformation.EVENT;
         }
      }

      return getRoot().getPart(sName);
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#findChild(java.lang.String)
    */
   public EndpointPart findChild(String sName)
   {
      if (!StringUtil.isEmpty(sName) && sName.charAt(0) == ':')
      {
         if (sName.equals(":oid"))
         {
            return Transformation.OID;
         }

         if (sName.equals(":class"))
         {
            return Transformation.CLASS;
         }

         if (sName.equals(":event"))
         {
            return Transformation.EVENT;
         }
      }

      return getRoot().findPart(sName);
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#getChildIterator()
    */
   public Iterator getChildIterator()
   {
      return getRoot().getPartIterator();
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#isCollection()
    */
   public boolean isCollection()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#isPrimitive()
    */
   public boolean isPrimitive()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.integration.TransformationEndpoint#getBaseEndpoint()
    */
   public TransformationEndpoint getBaseEndpoint()
   {
      return m_base;
   }

   /**
    * @see nexj.core.meta.integration.TransformationEndpoint#isUpcast(nexj.core.meta.integration.TransformationEndpoint)
    */
   public boolean isUpcast(TransformationEndpoint endpoint)
   {
      return (endpoint instanceof Message) && isUpcast((Message)endpoint);
   }

   /**
    * @see nexj.core.meta.integration.TransformationEndpoint#getEndpoint(java.lang.String)
    */
   public TransformationEndpoint getEndpoint(String sName)
   {
      return m_metadata.getMessage(sName);
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#createObject()
    */
   public TransferObject createObject()
   {
      return new TransferObject(getName());
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#getValue(nexj.core.util.PropertyMap, java.lang.Object)
    */
   public Object getValue(PropertyMap map, Object defValue)
   {
      throw new IllegalStateException(getName());
   }

   /**
    * @see nexj.core.meta.integration.EndpointPart#setValue(nexj.core.util.PropertyMap, java.lang.Object)
    */
   public void setValue(PropertyMap map, Object value)
   {
      throw new IllegalStateException(getName());
   }
}
