// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import java.util.ArrayList;
import java.util.List;

import nexj.core.integration.io.ObjectInput;
import nexj.core.integration.io.ObjectOutput;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.Transformation;
import nexj.core.meta.integration.TransformationArgument;
import nexj.core.meta.integration.TransformationEndpoint;
import nexj.core.meta.integration.TransformationMapping;
import nexj.core.meta.integration.TransformationSource;
import nexj.core.meta.integration.EndpointPart;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.scripting.Function;
import nexj.core.scripting.Pair;
import nexj.core.util.HashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.PropertyMap;
import nexj.core.util.StringUtil;
import nexj.core.util.Undefined;

/**
 * Transforms data in a PropertyMap to a TransferObject. The source and destination structures are
 * described by TransformationEndpoints, and the mapping of data is described by a Transformation.
 */
public class Transformer implements InvocationContextAware
{
   // constants
   
   /**
    * Source array element size.
    */
   protected final static int SOURCE_SIZE = 3; 

   // attributes

   /**
    * True if this Transformer is polymorphic.
    */
   protected boolean m_bPolymorphic = true;
   
   // associations
   
   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The output data.
    */
   protected TransferObject m_output;

   /**
    * The source object list: Object[3*n], List[3*n+1], Integer[3*n+2]
    */
   protected Object[] m_sourceArray;
   
   /**
    * Maps a combination of source and destination lists and a source index
    * to a resulting transfer object: TransferObject[ListKey].
    */
   protected Lookup m_listMap;
   
   /**
    * The list key.
    */
   protected ListKey m_listKey;

   /**
    * Array of mapping functions, indexed by mapping ordinal number.
    */
   protected Object[] m_scriptArray;

   /**
    * The argument value list for script invocation.
    */
   protected List m_argList = new ArrayList(4);

   /**
    * A cache of message parsers. MessageParser[Format].
    */
   protected Lookup m_parserMap;

   /**
    * A cache of message formatters. MessageFormatter[Format].
    */
   protected Lookup m_formatterMap;

   // constructors

   /**
    * Constructs the transformer.
    * @param context The invocation context.
    */
   public Transformer(InvocationContext context)
   {
      m_context = context;
   }
   
   /**
    * Constructs the transformer.
    */
   public Transformer()
   {
      super();
   }

   // operations

   /**
    * Set whether this Transformer is polymorphic or not.
    * @param bPolymorphic True if this Transformer should be polymorphic, false otherwise.
    */
   public void setPolymorphic(boolean bPolymorphic)
   {
      m_bPolymorphic = bPolymorphic;
   }

   /**
    * Get whether this Transformer is polymorphic or not.
    * @return True if this Transformer should be polymorphic, false otherwise.
    */
   public boolean isPolymorphic()
   {
      return m_bPolymorphic;
   }
   
   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * Transforms a message (without passing any arguments to the transformation).
    * @param obj The source message.
    * @param transformation The transformation metadata.
    * @return The transformed message.
    * @throws IntegrationException if a transformation error occurs.
    */
   public TransferObject transform(PropertyMap obj, Transformation transformation) throws IntegrationException
   {
      return transform(obj, transformation, null);
   }

   /**
    * Transforms a message.
    * @param obj The source message.
    * @param transformation The transformation metadata.
    * @param arguments The transformation parameters.
    * @return The transformed message.
    * @throws IntegrationException If a transformation error occurs.
    */
   public TransferObject transform(PropertyMap obj, Transformation transformation, Pair arguments) throws IntegrationException
   {
      Logger logger = transformation.getLogger();

      m_output = new TransferObject();
      m_listMap = new HashTab();

      try
      {
         if (logger.isDebugEnabled())
         {
            logger.debug("Transforming a message with transformation " + transformation.getName());
            logger.dump(obj);
         }

         Transformation oldTransformation = transformation;

         if (!StringUtil.isEmpty(obj.getClassName()) && m_bPolymorphic)
         {
            TransformationEndpoint derivedEndpoint = transformation.getSource().getEndpoint(obj.getClassName());
            Transformation derivedTransformation = findDerivedTransformation(transformation, derivedEndpoint);

            if (oldTransformation.getDerivation() == Transformation.DERIVATION_FINAL && derivedTransformation != oldTransformation)
            {
               throw new IntegrationException("err.integration.messageTypeMismatch",
                  new Object[] {oldTransformation.toString(), oldTransformation.getSource().toString(), derivedEndpoint.toString()});
            }

            if (derivedTransformation != null && derivedTransformation != oldTransformation)
            {
               transformation = derivedTransformation;

               if (logger.isDebugEnabled())
               {
                  logger.debug("Using derived transformation \"" + derivedTransformation.getName() + "\"");
               }
            }
         }

         if (oldTransformation.getDerivation() == Transformation.DERIVATION_ABSTRACT)
         {
            if (transformation == oldTransformation)
            {
               throw new IntegrationException("err.integration.abstractTransformation",
                  new Object[]{oldTransformation.getName()});
            }
         }

         m_sourceArray = new Object[SOURCE_SIZE * (transformation.getMaxLevel() + 1)];

         if (transformation.getFunction() != null)
         {
            m_argList.clear();
            m_argList.add(this);
            m_argList.add(m_output);
            m_argList.add(obj);

            Pair head = arguments;
            int nArgCount = transformation.getArgumentCount();

            while (head != null)
            {
               nArgCount--;
               m_argList.add(head.getHead());
               head = head.getNext();
            }

            if (nArgCount != 0)
            {
               throw new IntegrationException("err.meta.transformation.argumentCount",
                  new Object[] {Primitive.createInteger(transformation.getArgumentCount()),
                     Primitive.createInteger(Pair.length(arguments))});
            }

            m_scriptArray = (Object[])m_context.getMachine().invoke(transformation.getFunction(), m_argList);
         }
         else
         {
            m_scriptArray = null;
         }

         transform(obj, transformation.getRoot());

         if (m_scriptArray != null)
         {
            m_output = (TransferObject)m_context.getMachine().invoke((Function)m_scriptArray[0], (Pair)null);
         }

         if (m_output.getClassName() == null)
         {
            m_output.setClassName(transformation.getDestination().getName());
         }
      }
      catch (Exception e)
      {
         throw new IntegrationException("err.integration.transformation", 
            new Object[]{transformation.getName()}, e);
      }

      if (logger.isDumpEnabled())
      {
         logger.dump("Transformation result:");
         logger.dump(m_output);
      }

      return m_output;
   }

   /**
    * Transforms an object or a collection corresponding to a given transformation source.
    * @param obj The object to transform.
    * @param source The corresponding transformation source.
    */
   protected void transform(Object obj, TransformationSource source)
   {
      if (source.getPart().isCollection())
      {
         if (obj != null && obj != Undefined.VALUE)
         {
            List list = (List)obj;

            setSourceList(source.getLevel(), list);

            for (int i = 0, n = list.size(); i != n; ++i)
            {
               setSourceIndex(source.getLevel(), i);
               transform1(list.get(i), source);
            }
         }
      }
      else
      {
         transform1(obj, source);
      }
   }

   /**
    * Transforms a single object corresponding to a transformation source.
    * @param obj The object to transform.
    * @param source The transformation source. 
    */
   protected void transform1(Object obj, TransformationSource source)
   {
      setSourceObject(source.getLevel(), obj);

      for (int nMapping = 0, nMappingCount = source.getMappingCount(); nMapping != nMappingCount; ++nMapping)
      {
         TransformationMapping mapping = source.getMapping(nMapping);

         if (mapping.getDestinationCount() != 0)
         {
            boolean bMissing = false;

            m_argList.clear();

            for (int nArg = 0, nArgCount = mapping.getArgumentCount(); nArg != nArgCount; ++nArg)
            {
               TransformationArgument arg = mapping.getArgument(nArg);
               TransformationSource src = arg.getSource();
               int nStartLevel = arg.getStartLevel();
               Object value = getSourceObject(nStartLevel);
               Object parent = getSourceObject(Math.max(0, nStartLevel - 1));
               int nLevel;

               for (nLevel = nStartLevel; nLevel < src.getLevel() && value != null && value != Undefined.VALUE; ++nLevel)
               {
                  parent = value;
                  value = src.getPath(nLevel + 1).getPart().getValue((PropertyMap)parent, Undefined.VALUE);
               }

               if (value == null)
               {
                  if (nLevel != src.getLevel() || !arg.isNull())
                  {
                     bMissing = true;

                     break;
                  }
               }
               else if (value == Undefined.VALUE)
               {
                  if (nLevel != src.getLevel())
                  {
                     bMissing = true;

                     break;
                  }

                  Function fun = arg.getDefaultValueFunction();

                  if (fun == null)
                  {
                     bMissing = true;

                     break;
                  }

                  if (parent == Undefined.VALUE)
                  {
                     parent = null;
                  }

                  value = m_context.getMachine().invoke(fun, parent, (Object[])null);
               }

               m_argList.add(value);
            }

            if (!bMissing)
            {
               Object value;
               
               if (mapping.getOrdinal() != 0)
               {
                  value = m_context.getMachine().invoke(
                     (Function)m_scriptArray[mapping.getOrdinal()], m_argList);
               }
               else
               {
                  value = m_argList.get(0);
               }

               if (value != Undefined.VALUE)
               {
                  setDestination(mapping, source, value);
               }
            }
         }
      }

      if (obj != null && obj != Undefined.VALUE)
      {
         int nChildCount = source.getChildCount();

         if (nChildCount != 0)
         {
            PropertyMap tobj = (PropertyMap)obj;

            for (int nChild = 0; nChild != nChildCount; ++nChild)
            {
               TransformationSource child = source.getChild(nChild);

               if (child.isPrimary())
               {
                  Object value = child.getPart().getValue(tobj, Undefined.VALUE);

                  if (value == null)
                  {
                     if (child.isNull())
                     {
                        transform(null, child);
                     }
                  }
                  else if (value == Undefined.VALUE)
                  {
                     if (child.isDefault())
                     {
                        transform(value, child);
                     }
                  }
                  else
                  {
                     transform(value, child);
                  }
               }
            }
         }
      }
   }

   /**
    * Assigns a value to a given destination.
    * @param mapping The transformation mapping containing the destination.
    * @param source The transformation source.
    * @param value The value to assign.
    */
   protected void setDestination(TransformationMapping mapping, TransformationSource source, Object value)
   {
      TransferObject tobj = m_output;
      int nList = -1;

      value = convert(value, source, mapping);

      for (int i = 0, n = mapping.getDestinationCount() - 1; i <= n; ++i)
      {
         EndpointPart destinationPart = mapping.getDestination(i);
         String sDestinationName = destinationPart.getName();

         if (destinationPart.isCollection())
         {
            List list = (List)tobj.findValue(sDestinationName);

            if (list == null)
            {
               list = new ArrayList(4);
               destinationPart.setValue(tobj, list);
            }

            if (destinationPart.isPrimitive())
            {
               if (value instanceof List)
               {
                  list.addAll((List)value);
               }
               else
               {
                  list.add(value);
               }
            }
            else if (mapping.isFixedDestination(i))
            {
               if (list.isEmpty())
               {
                  tobj = destinationPart.createObject();
                  list.add(tobj);
               }
               else
               {
                  tobj = (TransferObject)list.get(0);
               }
            }
            else if (i == n && value instanceof List)
            {
               list.addAll((List)value);
            }
            else
            {
               int nLastCollection = mapping.getLastCollectionDestinationOrdinal();
               int nParentLevel = (mapping.getCollectionParent() == null) ? 0 : mapping.getCollectionParent().getLevel();
               boolean bListKey = false;

               tobj = null;
               
               while (++nList <= source.getLevel())
               {
                  TransformationSource path = source.getPath(nList);

                  if (!mapping.isFixedSource(path))
                  {
                     EndpointPart sourcePart = path.getPart(); 

                     if (sourcePart.isCollection())
                     {
                        if (m_listKey == null)
                        {
                           m_listKey = new ListKey();
                        }

                        if (i == nLastCollection)
                        {
                           nList = nParentLevel;
                        }

                        m_listKey.set(list, getSourceList(nList), getSourceIndex(nList), sourcePart);
                        tobj = (TransferObject)m_listMap.get(m_listKey);
                        bListKey = true;

                        break;
                     }
                     else if (i == nLastCollection && nList > nParentLevel)
                     {
                        Object sourceObject = getSourceObject(nList);

                        if (sourcePart.isPrimitive() ||
                           sourceObject == null ||
                           sourceObject == Undefined.VALUE)
                        {
                           sourceObject = getSourceObject(nList - 1);
                        }

                        if (m_listKey == null)
                        {
                           m_listKey = new ListKey();
                        }

                        m_listKey.set(list, sourceObject, 0, sourcePart);
                        tobj = (TransferObject)m_listMap.get(m_listKey);
                        bListKey = true;

                        break;
                     }
                  }
               }

               if (tobj == null)
               {
                  if (i == n)
                  {
                     tobj = (TransferObject)value;
                     list.add(tobj);
                  }
                  else
                  {
                     tobj = destinationPart.createObject();
                     list.add(tobj);
                  }

                  if (bListKey)
                  {
                     m_listMap.put(m_listKey, tobj);
                     m_listKey = null;
                  }
               }
            }
         }
         else
         {
            if (i == n)
            {
               destinationPart.setValue(tobj, value);
            }
            else
            {
               TransferObject obj = (TransferObject)tobj.findValue(sDestinationName);
               
               if (obj == null)
               {
                  obj = destinationPart.createObject();
                  destinationPart.setValue(tobj, obj);
               }
               
               tobj = obj;
            }
         }
      }
   }

   /**
    * Sets an object in the source array.
    * @param nLevel The object level.
    * @param obj The object to set.
    */
   protected void setSourceObject(int nLevel, Object obj)
   {
      m_sourceArray[SOURCE_SIZE * nLevel] = obj;
   }
   
   /**
    * Gets an object from the source array.
    * @param nLevel The object level.
    * @return The object.
    */
   protected Object getSourceObject(int nLevel)
   {
      return m_sourceArray[SOURCE_SIZE * nLevel];
   }

   /**
    * Sets a list in the source array.
    * @param nLevel The list level.
    * @param list The list to set.
    */
   protected void setSourceList(int nLevel, List list)
   {
      m_sourceArray[SOURCE_SIZE * nLevel + 1] = list;
   }

   /**
    * Gets a list from the source array.
    * @param nLevel The list level.
    * @return The list.
    */
   protected List getSourceList(int nLevel)
   {
      return (List)m_sourceArray[SOURCE_SIZE * nLevel + 1];
   }

   /**
    * Sets an index in the source array.
    * @param nLevel The index level.
    * @param nIndex The index.
    */
   protected void setSourceIndex(int nLevel, int nIndex)
   {
      m_sourceArray[SOURCE_SIZE * nLevel + 2] = Primitive.createInteger(nIndex);
   }
   
   /**
    * Gets an index from the source array.
    * @param nLevel The index level.
    * @return The index.
    */
   protected int getSourceIndex(int nLevel)
   {
      return ((Integer)m_sourceArray[SOURCE_SIZE * nLevel + 2]).intValue();
   }

   /**
    * Parses or formats the value, if the mapping has no script and is a mapping between
    * a primitive message part and a message reference.
    * @param value The value to parse/format.
    * @param source The source to parse/format.
    * @param mapping The mapping to parse/format.
    * 
    * @return The parsed/formatted value.
    */
   protected Object convert(Object value, TransformationSource source, TransformationMapping mapping)
   {
      // Process only if there is no script.
      if (mapping.getOrdinal() == 0)
      {
         EndpointPart srcPart = source.getPart();
         EndpointPart dstPart = mapping.getDestination(mapping.getDestinationCount() - 1);
         Message message = null;
         boolean bParse = false;

         // Handle ref -> primitive and primitive -> ref.
         if (srcPart instanceof CompositeMessagePartRef && dstPart.isPrimitive())
         {
            message = m_context.getMetadata().getMessage(((CompositeMessagePartRef)srcPart).getRefPart().getName());
         }
         else if (dstPart instanceof CompositeMessagePartRef && srcPart.isPrimitive())
         {
            message = m_context.getMetadata().getMessage(((CompositeMessagePartRef)dstPart).getRefPart().getName());
            bParse = true;
         }

         if (message != null && message.getFormat() != null)
         {
            if (bParse)
            {
               Input in = new ObjectInput(value);
               MessageParser parser;

               if (m_parserMap == null)
               {
                  m_parserMap = new HashTab(m_context.getMetadata().getFormatCount());
               }

               parser = (MessageParser)m_parserMap.get(message.getFormat());

               if (parser == null)
               {
                  parser = (MessageParser)message.getFormat().getParser().getInstance(m_context);
                  m_parserMap.put(message.getFormat(), parser);
               }

               value = parser.parse(in, message);
            }
            else
            {
               ObjectOutput out = new ObjectOutput();
               MessageFormatter formatter;

               if (m_formatterMap == null)
               {
                  m_formatterMap = new HashTab(m_context.getMetadata().getFormatCount());
               }

               formatter = (MessageFormatter)m_formatterMap.get(message.getFormat());

               if (formatter == null)
               {
                  formatter = (MessageFormatter)message.getFormat().getFormatter().getInstance(m_context);
                  m_formatterMap.put(message.getFormat(), formatter);
               }

               formatter.format((TransferObject)value, message, out);
               value = out.getObject();
            }
         }
      }

      return value;
   }

   /**
    * Transforms a property map. Saves the current state of this Transformer instance to
    * the call stack before executing the transformation, and restores the state
    * before returning the result.
    * @param tobj The source property map.
    * @param sTransformationName The transformation name.
    * @param arguments The transformation parameters.
    * @return The transformed message.
    * @throws IntegrationException If a transformation error occurs.
    */
   public TransferObject transformNested(PropertyMap tobj, String sTransformationName, Pair arguments) throws IntegrationException
   {
      Transformation transformation = m_context.getMetadata().getTransformation(sTransformationName);
      TransferObject outputSaved = m_output;
      Object[] sourceArraySaved = m_sourceArray;
      Lookup listMapSaved = m_listMap;
      ListKey listKeySaved = m_listKey;
      Object[] scriptArraySaved = m_scriptArray;

      try
      {
         m_output = null;
         m_sourceArray = null;
         m_listMap = null;
         m_listKey = null;
         m_scriptArray = null;

         return transform(tobj, transformation, arguments);
      }
      finally
      {
         m_output = outputSaved;
         m_sourceArray = sourceArraySaved;
         m_listMap = listMapSaved;
         m_listKey = listKeySaved;
         m_scriptArray = scriptArraySaved;
      }
   }

   /**
    * Selects a derived transformation of "txf" for transforming
    * the given source end-point.
    * @param txf The base transformation to use for transforming the end-point.
    * @param endpoint The end-point to transform.
    * @return A derived transformation of "txf", or "txf" itself.
    */
   public static Transformation findDerivedTransformation(Transformation txf, TransformationEndpoint endpoint)
   {
      if (endpoint != null && txf.getDerivation() != Transformation.DERIVATION_FINAL)
      {
         Transformation result = txf.findTransformation(endpoint);

         if (result == null)
         {
            return findDerivedTransformation(txf, endpoint.getBaseEndpoint());
         }

         return (txf.isUpcast(result)) ? result : null;
      }

      if (txf.getSource().isUpcast(endpoint))
      {
         return txf;
      }

      return null;
   }

   // inner classes

   /**
    * The key for looking up a transfer object corresponding
    * to a source list index and a destination list.
    */
   protected static class ListKey
   {
      /**
       * The destination list.
       */
      protected List m_dst;
      
      /**
       * The source object.
       */
      protected Object m_src;
      
      /**
       * The source index.
       */
      protected int m_nIndex;
      
      /**
       * The end-point part.
       */
      protected EndpointPart m_part;
      
      /**
       * Sets the key data.
       * @param dst The destination list.
       * @param src The source object.
       * @param nIndex The source index.
       * @param part The source end-point part.
       */
      public void set(List dst, Object src, int nIndex, EndpointPart part)
      {
         m_dst = dst;
         m_src = src;
         m_part = part;
         m_nIndex = nIndex;
      }
      
      /**
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         if (!(obj instanceof ListKey))
         {
            return false;
         }
         
         ListKey key = (ListKey)obj;
         
         return m_nIndex == key.m_nIndex && m_dst == key.m_dst &&
            m_src == key.m_src && m_part == key.m_part;
      }
      
      /**
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         return System.identityHashCode(m_dst) ^ System.identityHashCode(m_src) << 8 ^
            m_nIndex << 16 ^ System.identityHashCode(m_part);
      }
   }
}
