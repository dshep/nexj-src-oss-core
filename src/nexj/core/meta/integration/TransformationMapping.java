// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.NamedMetadataObject;
import nexj.core.meta.Primitive;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Undefined;

/**
 * Transformation mapping for translating between multiple source values
 * and one destination value.
 */
public class TransformationMapping extends NamedMetadataObject
{
   // constants

   /**
    * The Transformer.transformNested() method symbol.
    */
   protected final static Symbol TRANSFORM_NESTED = Symbol.define("transformNested");

   // attributes

   /**
    * The mapping script ordinal number in the transformation.
    */
   protected int m_nOrdinal;
   
   /**
    * The last collection destination ordinal number.
    */
   protected int m_nLastCollectionDestinationOrdinal = -1;

   /**
    * Generation bit mask.
    */
   protected byte m_nGeneration;

   // associations

   /**
    * The condition expression.
    */
   protected Object m_condition = Boolean.TRUE;

   /**
    * The mapping script.
    */
   protected Pair m_script;

   /**
    * The destination root end-point part.
    */
   protected EndpointPart m_rootPart;

   /**
    * The destination end-point part collection.
    */
   protected List m_destinationList = new ArrayList(2); // of type EndpointPart

   /**
    * The transformation mapping argument collection.
    */
   protected List m_argumentList = new ArrayList(2); // of type TransformationArgument
   
   /**
    * An array of fixed association flags corresponding to TransformationSource level
    */
   protected boolean m_bFixedSrcArray[];
   
   /**
    * An array of fixed association flags corresponding to destination end-point part level
    */
   protected boolean m_bFixedDstArray[];

   /**
    * The closest collection parent transformation source.
    */
   protected TransformationSource m_collectionParent;

   /**
    * The transformation expression. Evaluates to the name of the nested transformation
    * to use.
    */
   protected Object m_transformation;

   /**
    * The transformation arguments expression.
    */
   protected Pair m_transformationArguments;

   /**
    * The expression evaluating to the input to the transformation; null to
    * use the first source instead.
    */
   protected Object m_transformationInput;

   // constructors
   
   /**
    * Constructs the metadata object.
    * @param sName The transformation mapping name.
    */
   public TransformationMapping(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * Sets the destination root end-point part.
    * @param rootPart The destination root end-point part to set.
    */
   public void setRootPart(EndpointPart rootPart)
   {
      verifyNotReadOnly();
      m_rootPart = rootPart;
   }

   /**
    * @return The destination root end-point part.
    */
   public EndpointPart getRootPart()
   {
      return m_rootPart;
   }

   /**
    * Adds a new destination end-point part to the mapping.
    * @param sName The name of the end-point part.
    * @param bFixed Indicates fixed collection.
    */
   public void addDestination(String sName, boolean bFixed)
   {
      EndpointPart part = (m_destinationList.isEmpty()) ? m_rootPart :
         (EndpointPart)m_destinationList.get(m_destinationList.size() - 1);
      
      if (part.isPrimitive())
      {
         throw new MetadataException("err.meta.transformation.primitiveParent", new Object[]{part.getName()});
      }

      addDestination(part.getChild(sName), bFixed);
   }
   
   /**
    * Adds a new destination end-point part to the mapping.
    * @param destination The destination end-point part to add.
    * @param bFixed Indicates fixed collection.
    */
   public void addDestination(EndpointPart destination, boolean bFixed)
   {
      verifyNotReadOnly();

      if (destination.isCollection() && !bFixed)
      {
         m_nLastCollectionDestinationOrdinal = m_destinationList.size();
      }

      m_destinationList.add(destination);
      setFixedDestination(m_destinationList.size() - 1, bFixed);
   }

   /**
    * Gets a destination end-point part by ordinal number.
    * @param nOrdinal The destination end-point part ordinal number (0-based).
    * @return The destination end-point part.
    */
   public EndpointPart getDestination(int nOrdinal)
   {
      return (EndpointPart)m_destinationList.get(nOrdinal);
   }

   /**
    * @return The destination end-point part count.
    */
   public int getDestinationCount()
   {
      return m_destinationList.size();
   }

   /**
    * @return The last collection destination ordinal number.
    */
   public int getLastCollectionDestinationOrdinal()
   {
      return m_nLastCollectionDestinationOrdinal;
   }
   
   /**
    * Adds a new transformation argument to the mapping.
    * @param nOrdinal The ordinal number of the argument.
    * @param argument The transformation argument to add.
    */
   public void addArgument(int nOrdinal, TransformationArgument argument)
   {
      verifyNotReadOnly();

      if (argument.getName() != null)
      {
         for (int i = m_argumentList.size() - 1; i >= 0; --i)
         {
            if (argument.getName().equals(((TransformationArgument)m_argumentList.get(i)).getName()))
            {
               throw new MetadataException("err.meta.transformation.argumentDup",
                  new Object[]{argument.getName(), getName()});
            }
         }
      }

      m_argumentList.add(nOrdinal, argument);
   }

   /**
    * Adds a new transformation argument to the mapping.
    * @param argument The transformation argument to add.
    */
   public void addArgument(TransformationArgument argument)
   {
      addArgument(m_argumentList.size(), argument);
   }

   /**
    * Gets a transformation mapping argument by ordinal number.
    * @param nOrdinal The transformation mapping argument ordinal number (0-based).
    * @return The transformation mapping argument object.
    */
   public TransformationArgument getArgument(int nOrdinal)
   {
      return (TransformationArgument) m_argumentList.get(nOrdinal);
   }

   /**
    * Removes a transformation mapping argument by ordinal number.
    * @param nOrdinal The transformation mapping argument ordinal number.
    */
   public void removeArgument(int nOrdinal)
   {
      m_argumentList.remove(nOrdinal);
   }
   
   /**
    * @return The transformation mapping argument count.
    */
   public int getArgumentCount()
   {
      return m_argumentList.size();
   }

   /**
    * @return An iterator for the contained transformation argument objects.
    */
   public Iterator getArgumentIterator()
   {
      return m_argumentList.iterator();
   }
   
   /**
    * Sets the condition expression.
    * @param condition The condition expression to set.
    */
   public void setCondition(Object condition)
   {
      verifyNotReadOnly();
      m_condition = condition;
   }

   /**
    * @return The condition expression.
    */
   public Object getCondition()
   {
      return m_condition;
   }
   
   /**
    * Sets the mapping script.
    * @param script The mapping script to set.
    */
   public void setScript(Pair script)
   {
      verifyNotReadOnly();
      m_script = script;
   }

   /**
    * @return The mapping script.
    */
   public Pair getScript()
   {
      return m_script;
   }

   /**
    * Sets the nested transformation to be executed by this mapping.
    * @param transformation An expression that evaluates to the name of
    * the transformation to execute; null if not set.
    */
   public void setTransformation(Object transformation)
   {
      verifyNotReadOnly();
      m_transformation = transformation;
   }

   /**
    * Gets the nested transformation to be executed by this mapping.
    * @return Expression evaluating to the name of the transformation.
    */
   public Object getTransformation()
   {
      return m_transformation;
   }

   /**
    * Sets the nested transformation parameters.
    * @param arguments A list of expressions evaluating to the parameters
    * for the nested transformation; may be null.
    */
   public void setTransformationArguments(Object arguments)
   {
      verifyNotReadOnly();
      m_transformationArguments = new Pair(Symbol.LIST, arguments);
   }

   /**
    * Gets the nested transformation parameters.
    * @return A list of expressions evaluating to the parameters for the
    * nested transformation.
    */
   public Object getTransformationArguments()
   {
      return (m_transformationArguments == null) ? null : m_transformationArguments.getTail();
   }

   /**
    * Sets the nested transformation input.
    * @param input An expression evaluating to the TransferObject to use
    * as input to the nested transformation; null to use the first source.
    */
   public void setTransformationInput(Object input)
   {
      verifyNotReadOnly();
      m_transformationInput = input;
   }

   /**
    * Gets the nested transformation input.
    * @return An expression evaluating to the TransferObject to use as input
    * to the nested transformation; null to use the first source.
    */
   public Object getTransformationInput()
   {
      return m_transformationInput;
   }

   /**
    * Sets the mapping script ordinal number in the transformation.
    * @param nOrdinal The mapping script ordinal number in the transformation (0-based) to set.
    */
   public void setOrdinal(int nOrdinal)
   {
      verifyNotReadOnly();
      m_nOrdinal = nOrdinal;
   }

   /**
    * @return The mapping script ordinal number in the transformation.
    */
   public int getOrdinal()
   {
      return m_nOrdinal;
   }

   /**
    * Adds the mapping to an associated source (through the arguments).
    * @param The containing transformation
    */
   protected void addToSource(Transformation transformation)
   {
      int nCount = getArgumentCount();
      
      if (nCount == 0)
      {
         MetadataValidationException e = new MetadataValidationException(
            "err.meta.transformation.sourceRequired", new Object[]{getName()});

         transformation.setProperties(e);
         setProperties(e);

         throw e;
      }
    
      if (getDestinationCount() == 0)
      {
         return;
      }
      
      int nLevel = 0;
      int nOrdinal = 0;
      int nColLevel = -1;
      
      for (int i = 0; i != nCount; ++i)
      {
         TransformationSource src = getArgument(i).getSource();
         int nCollectionLevel = -1;
         
         for (int k = src.getLevel(); k >= 0; --k)
         {
            if (src.getPath(k).getPart().isCollection())
            {
               nCollectionLevel = k;
               
               break;
            }
         }

         if (nCollectionLevel > nColLevel ||
            src.getLevel() > nLevel && nCollectionLevel >= nColLevel)
         {
            nLevel = src.getLevel();
            nOrdinal = i;
            nColLevel = nCollectionLevel;
         }
      }

      TransformationArgument primaryArg = getArgument(nOrdinal);
      TransformationSource primarySource = primaryArg.getSource();

      primaryArg.setStartLevel(primarySource.getLevel());
      primarySource.addMapping(this);

      for (TransformationSource source = primarySource; source != null; source = source.getParent())
      {
         if (source.getPart().isCollection() && !isFixedSource(source))
         {
            m_collectionParent = source;
            break;
         }
      }
      
      for (int i = 0; i != nCount; ++i)
      {
         if (i == nOrdinal)
         {
            continue;
         }

         TransformationArgument arg = getArgument(i);
         TransformationSource src = arg.getSource();

         for (int k = src.getLevel(); k >= 0; --k)
         {
            TransformationSource path = src.getPath(k);

            if (k <= primarySource.getLevel() && path == primarySource.getPath(k))
            {
               arg.setStartLevel(k);
               
               break;
            }

            if (path.getPart().isCollection())
            {
               MetadataValidationException e = new MetadataValidationException(
                  "err.meta.transformation.parentCollection",
                  new Object[]{src.getPart().getName(), getName()});

               transformation.setProperties(e);
               setProperties(e);

               throw e;
            }
         }
      }
   }

   /**
    * Generates the source code for the mapping.
    * @param transformation The transformation containing the mapping.
    */
   public void generate(Transformation transformation)
   {
      verifyNotReadOnly();

      if (m_nGeneration != 0)
      {
         if (m_nGeneration == 1)
         {
            MetadataValidationException e = new MetadataValidationException(
               "err.meta.transformation.mappingCircularRef", new Object[]{getName()});

            transformation.setProperties(e);
            setProperties(e);

            throw e;
         }

         return;
      }

      ++m_nGeneration;

      boolean bMapping = false;

      for (int nArg = getArgumentCount() - 1; nArg >= 0; --nArg)
      {
         TransformationArgument arg = getArgument(nArg);

         if (arg.getName() == null)
         {
            arg.setName("#arg" + nArg);
         }

         if (arg.getMapping() != null)
         {
            bMapping = true;

            break;
         }
      }

      Pair script = m_script;

      if (m_transformation != null && getArgumentCount() > 0)
      {
         // (#transformer'transformNested TOBJ <Transformation> <Arguments>)
         m_script = Pair.list(
            Pair.list(
               Transformation.TRANSFORMER,
               Pair.quote(TRANSFORM_NESTED),
               (m_transformationInput == null) ? Symbol.define(getArgument(0).getName()) : m_transformationInput,
               m_transformation,
               m_transformationArguments
            )
         );
      }

      if (m_condition != Boolean.TRUE || m_script != null)
      {
         Pair args = null;

         for (int i = getArgumentCount() - 1; i >= 0; --i)
         {
            args = new Pair(Symbol.define(getArgument(i).getName()), args);
         }

         if (bMapping)
         {
            if (m_condition == Boolean.TRUE)
            {
               m_condition = null;
            }
            else
            {
               m_condition = new Pair(m_condition);
            }

            for (int i = getArgumentCount() - 1; i >= 0; --i)
            {
               TransformationArgument arg = getArgument(i);

               if (arg.getMapping() != null)
               {
                  m_condition = new Pair(Pair.binary(Symbol.EQ_P, Symbol.define(arg.getName()),
                     Pair.quote(Undefined.VALUE)).not(), m_condition);
               }
            }

            if (((Pair)m_condition).getTail() == null)
            {
               m_condition = ((Pair)m_condition).getHead();
            }
            else
            {
               m_condition = new Pair(Symbol.AND, m_condition);
            }
         }

         if (m_script != null)
         {
            if (m_condition != Boolean.TRUE)
            {
               m_script = Pair.list(Pair.list(Symbol.IF, m_condition,
                  new Pair(new Pair(Symbol.LAMBDA, new Pair(null, m_script))),
                  Pair.quote(Undefined.VALUE)));
            }
         }
         else
         {
            m_script = Pair.list(Pair.list(Symbol.IF, m_condition,
               args.getHead(), Pair.quote(Undefined.VALUE)));
         }

         m_script = new Pair(Symbol.LAMBDA, new Pair(args, m_script));
      }

      if (bMapping)
      {
         int nNewArgCount = 0;
         Pair args = null;

         for (int nArg = getArgumentCount() - 1; nArg >= 0; --nArg)
         {
            TransformationArgument arg = getArgument(nArg);
            TransformationMapping mapping = arg.getMapping();

            if (mapping != null)
            {
               removeArgument(nArg);
               mapping.generate(transformation);

               Object code = null; 
               TransformationArgument srcArg = null;

               for (int i = mapping.getArgumentCount() - 1; i >= 0 ; --i)
               {
                  TransformationArgument mapArg = mapping.getArgument(i);
                  TransformationSource src = mapArg.getSource();
                  int nSrc;

                  for (nSrc = getArgumentCount() - 1; nSrc >= 0; --nSrc)
                  {
                     srcArg = getArgument(nSrc);

                     if (srcArg.getSource() == src)
                     {
                        break;
                     }
                  }

                  if (nSrc < 0)
                  {
                     srcArg = new TransformationArgument("#" + nNewArgCount++);
                     srcArg.setSource(src);
                     srcArg.setNull(mapArg.isNull());
                     srcArg.setDefaultValue(mapArg.getDefaultValue());
                     srcArg.setDefaultValueFunction(mapArg.getDefaultValueFunction());
                     addArgument(nArg, srcArg);
                  }

                  if (arg.isNull())
                  {
                     src.setNull(true);
                  }

                  if (arg.isDefault())
                  {
                     src.setDefault(true);
                  }

                  code = new Pair(Symbol.define(srcArg.getName()), code);
               }

               if (mapping.getOrdinal() == 0)
               {
                  code = ((Pair)code).getHead();
               }
               else
               {
                  code = new Pair(Pair.list(Symbol.VECTOR_REF, Transformation.MAPPERS,
                     Primitive.createInteger(mapping.getOrdinal())), code);
               }

               args = new Pair(code, args);
            }
            else
            {
               args = new Pair(Symbol.define(arg.getName()), args);
            }
         }

         Pair code = null;

         for (int i = getArgumentCount() - 1; i >= 0; --i)
         {
            code = new Pair(Symbol.define(getArgument(i).getName()), code);
         }

         if (m_script == null)
         {
            m_script = Pair.list(Symbol.LAMBDA, code, args.getHead());
         }
         else
         {
            m_script = Pair.list(Symbol.LAMBDA, code, new Pair(m_script, args));
         }
      }

      if (m_script != null)
      {
         if (script != null && script.getHead() == Transformation.BASE)
         {
            // Script was a reference to a base mapping; base ordinal now valid so use it to complete the script.
            TransformationMapping baseMapping = (TransformationMapping)script.getTail();

            m_script = Pair.list(Transformation.BASE, Primitive.createInteger(baseMapping.getOrdinal()));
         }

         m_nOrdinal = transformation.addScript(m_script);
         m_condition = null;
         m_script = null;
      }

      addToSource(transformation);
      ++m_nGeneration;
   }

   /**
    * Compiles the mapping.
    * @param transformation The transformation containing the mapping.
    * @param machine The VM used for compilation.
    */
   public void compile(Transformation transformation, Machine machine)
   {
      for (int i =  0, n = getArgumentCount(); i < n; ++i)
      {
         TransformationArgument arg = getArgument(i);

         if (arg.isDefault())
         {
            arg.setDefaultValueFunction(transformation.compile(
               Pair.list(Symbol.LAMBDA, Pair.list(Symbol.THIS), arg.getDefaultValue()),
               "mapping." + m_sName + '.' + arg.getName(), machine));
         }
      }
   }

   /**
    * Sets a fixed flag in the destination fixed flag array.
    * @param nDestIndex The index for which to set the fixed flag.
    * @param bFixed The desired flag.
    */
   public void setFixedDestination(int nDestIndex, boolean bFixed)
   {
      verifyNotReadOnly();
      
      if (m_bFixedDstArray == null)
      {
         if (!bFixed)
         {
            return;
         }

         m_bFixedDstArray = new boolean[nDestIndex + 4];
      }
      else if (nDestIndex >= m_bFixedDstArray.length)
      {
         boolean bNewArray[] = new boolean[nDestIndex + 4];

         System.arraycopy(m_bFixedDstArray, 0, bNewArray, 0, m_bFixedDstArray.length);
         m_bFixedDstArray = bNewArray;
      }

      m_bFixedDstArray[nDestIndex] = bFixed;      
   }

   /**
    * Sets a fixed flag in the source fixed flag array.
    * @param source The transformation source for which to set the fixed flag.
    * @param bFixed The desired flag.
    */
   public void setFixedSource(TransformationSource source, boolean bFixed)
   {
      verifyNotReadOnly();

      if (m_bFixedSrcArray == null)
      {
         m_bFixedSrcArray = new boolean[source.getLevel() + 4];
      }
      else if (source.getLevel() >= m_bFixedSrcArray.length)
      {
         boolean bNewArray[] = new boolean[source.getLevel() + 4];

         System.arraycopy(m_bFixedSrcArray, 0, bNewArray, 0, m_bFixedSrcArray.length);
         m_bFixedSrcArray = bNewArray;
      }

      m_bFixedSrcArray[source.getLevel()] = bFixed;      
   }

   /**
    * @param source The source for which to return a fixed flag.
    * @return The fixed collection flag for a given source in this mapping.
    */
   public boolean isFixedSource(TransformationSource source)
   {
      if (m_bFixedSrcArray == null)
      {
         return false;
      }
      
      return (source.getLevel() < m_bFixedSrcArray.length) ? m_bFixedSrcArray[source.getLevel()] : false;
   }

   /**
    * @param nDestIndex The array index for which to return a fixed flag.
    * @return The fixed collection flag for a destination index in this mapping.
    */
   public boolean isFixedDestination(int nDestIndex)
   {
      if (m_bFixedDstArray == null)
      {
         return false;
      }
      
      return (nDestIndex < m_bFixedDstArray.length) ? m_bFixedDstArray[nDestIndex] : false;
   }

   /**
    * @return The closest collection parent transformation source.
    */
   public TransformationSource getCollectionParent()
   {
      return m_collectionParent;
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      marker.setProperty("mapping", m_sName);
   }

   /**
    * Finishes loading the transformation mapping, first pass.
    * The first pass is where the inherited mappings are fixed up. Two things must be fixed:
    * 1) The path of destination end-point parts on the derived transformation's mapping must
    *    be changed so that it uses parts from the derived transformation's
    *    destination end-point (and not the base transformation's destination end-point).
    * 2) For an inherited mapping, the mapping arguments will not have their source mappings
    *    (if any) set. Get the correct mapping source from the base mapping.
    * @param transformation The transformation containing the mapping.
    * @param machine The VM used for compilation.
    */
   public void finish1(Transformation transformation, Machine machine)
   {
      verifyNotReadOnly();

      // Process only inherited mappings (ensure object references are to objects in the derived transformation)
      if (m_rootPart != transformation.getDestination())
      {
         // Fix the root end-point part and destination end-point part lists.
         EndpointPart part = m_rootPart = transformation.getDestination();

         for (int i = 0; i < m_destinationList.size(); i++)
         {
            EndpointPart oldPart = (EndpointPart)m_destinationList.get(i);

            part = part.getChild(oldPart.getName());
            m_destinationList.set(i, part);
         }

         // Fixup arguments with source mappings
         for (int i = 0, nCount = m_argumentList.size(); i < nCount; i++)
         {
            TransformationArgument arg = (TransformationArgument)m_argumentList.get(i);

            if (arg.getSource() == null)
            {
               assert arg.getMapping() == null;

               TransformationMapping baseMapping = getEquivalentMappingInBase(this, transformation);

               // Find same argument by ordinal position.
               TransformationArgument baseArg = baseMapping.getArgument(i);

               assert baseArg.getSource() == null;
               assert baseArg.getMapping() != null;

               arg.setMapping(transformation.getMapping(baseArg.getMapping().getName()));
            }
         }
      }
   }

   /**
    * Given a mapping and its transformation, finds the equivalent mapping in the
    * base transformation.
    * 
    * Works on the principle that the mapping order from base to derived transformation
    * is maintained by inheritance resolution.
    * 
    * @param mapping The mapping to find.
    * @param transformation The mapping's transformation.
    * @return The equivalent mapping from the base transformation; null if not found.
    */
   private static TransformationMapping getEquivalentMappingInBase(TransformationMapping mapping, Transformation transformation)
   {
      Iterator baseItr = transformation.getBaseTransformation().getMappingIterator();
      Iterator derivedItr = transformation.getMappingIterator();

      while (baseItr.hasNext())
      {
         TransformationMapping baseMapping = (TransformationMapping)baseItr.next();
         TransformationMapping derivedMapping = (TransformationMapping)derivedItr.next();

         if (derivedMapping == mapping)
         {
            return baseMapping;
         }
      }

      return null;
   }

   /**
    * Finishes loading the transformation mapping, second pass.
    * The second pass is where code generation/compilation is performed.
    * @param transformation The transformation containing the mapping.
    * @param machine The VM used for compilation.
    */
   public void finish2(Transformation transformation, Machine machine)
   {
      // Build the mapping function
      generate(transformation);
      compile(transformation, machine);
   }

   /**
    * @see nexj.core.meta.MetadataObject#clone()
    */
   public Object clone()
   {
      TransformationMapping copy = (TransformationMapping)super.clone();

      copy.m_argumentList = new ArrayList(m_argumentList.size());

      for (int i = 0; i < m_argumentList.size(); i++)
      {
         copy.m_argumentList.add(((TransformationArgument)m_argumentList.get(i)).clone());
      }

      copy.m_destinationList = (ArrayList)((ArrayList)m_destinationList).clone();

      return copy;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();

      for (Iterator itr = m_argumentList.iterator(); itr.hasNext(); )
      {
         ((TransformationArgument)itr.next()).makeReadOnly();
      }

      ((ArrayList)m_argumentList).trimToSize();
      ((ArrayList)m_destinationList).trimToSize();
   }

   /**
    * @see nexj.core.meta.NamedMetadataObject#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(128);

      buf.append(super.toString());
      buf.append(" to ");

      if (m_rootPart != null)
      {
         buf.append(m_rootPart.getName());
      }

      for (int i = 0, n = getDestinationCount(); i < n; ++i)
      {
         buf.append(' ');
         buf.append(getDestination(i).getName());
      }

      return buf.toString();
   }
}
