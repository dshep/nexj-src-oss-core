// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import nexj.core.meta.MetadataCompoundValidationException;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataResource;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.NamedMetadataObject;
import nexj.core.meta.Primitive;
import nexj.core.persistence.OID;
import nexj.core.persistence.OIDHolder;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.CompilerException;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.EmptyIterator;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Holder;
import nexj.core.util.IdentityHashHolder;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Logger;
import nexj.core.util.LoggerHolder;
import nexj.core.util.Lookup;
import nexj.core.util.PropertyMap;
import nexj.core.util.TextPosition;
import nexj.core.util.UncheckedException;

/**
 * A transformation takes data from a PropertyMap whose structure is described
 * by the source TransformationEndpoint and puts it into a TransferObject described by the
 * destination TransformationEndpoint. 
 */
public class Transformation extends NamedMetadataObject implements MetadataResource, LoggerHolder
{
   // constants

   /**
    * Polymorphism allowed for the transformation.
    */
   public final static byte DERIVATION_VIRTUAL = 0;

   /**
    * Polymorphism not allowed for the transformation.
    */
   public final static byte DERIVATION_FINAL = 1;

   /**
    * Polymorphism allowed, derived transformation must be used.
    */
   public final static byte DERIVATION_ABSTRACT = 2;

   /**
    * The transformation code URL prefix.
    */
   protected final static String URL_PREFIX = "transformation:";

   /**
    * The mappers variable symbol.
    */
   protected final static Symbol MAPPERS = Symbol.define("#mappers");

   /**
    * The base variable symbol.
    */
   protected final static Symbol BASE = Symbol.define("#base");

   /**
    * The symbol for the base function. (Used because a direct call to a Function
    * cannot be compiled directly--it must be put into a Symbol to be compilable). 
    */

   protected final static Symbol BASE_FN = Symbol.define("#basefn");

   /**
    * The finalizeBase flag variable symbol. True if the base finalizer needs to be
    * executed; false if it has been executed.
    */
   protected final static Symbol FINALIZE_BASE = Symbol.define("#finalizeBase");

   /**
    * The assert macro symbol.
    */
   protected final static Symbol ASSERT = Symbol.define("assert");

   /**
    * The transformer variable symbol.
    */
   public final static Symbol TRANSFORMER = Symbol.define("#transformer");

   /**
    * End-point part for transforming an OID.
    */
   public final static EndpointPart OID = new SystemAttributeEndpointPart(":oid")
   {
      public Object getValue(PropertyMap map, Object defValue)
      {
         OID oid = ((OIDHolder)map).getOID();

         return (oid == null) ? defValue : oid;
      }

      public void setValue(PropertyMap map, Object value)
      {
         ((OIDHolder)map).setOID((OID)value);
      }
   };

   /**
    * End-point part for transforming a class name.
    */
   public final static EndpointPart CLASS = new SystemAttributeEndpointPart(":class")
   {
      public Object getValue(PropertyMap map, Object defValue)
      {
         String sName = map.getClassName();

         return (sName == null) ? defValue : sName;
      }

      public void setValue(PropertyMap map, Object value)
      {
         ((TransferObject)map).setClassName((String)value);
      }
   };

   /**
    * End-point part for transforming an event name.
    */
   public final static EndpointPart EVENT = new SystemAttributeEndpointPart(":event")
   {
      public Object getValue(PropertyMap map, Object defValue)
      {
         String sName = null;

         if (map instanceof TransferObject)
         {
            sName = ((TransferObject)map).getEventName();
         }
         else if (map instanceof Instance)
         {
            switch (((Instance)map).getState())
            {
               case Instance.NEW:
                  sName = "create";
                  break;

               case Instance.DIRTY:
                  sName = "update";
                  break;

               case Instance.DELETED:
                  sName = "delete";
                  break;
            }
         }

         return (sName == null) ? defValue : sName;
      }

      public void setValue(PropertyMap map, Object value)
      {
         ((TransferObject)map).setEventName((String)value);
      }
   };

   // attributes

   /**
    * The maximum transformation source level.
    */
   protected int m_nMaxLevel;

   /**
    * The polymorphism setting for this message. One of the DERIVATION_* constants.
    */
   protected byte m_nDerivation;

   /**
    * The primary flag. Used to disambiguate the selection of a transformation when
    * more than one transformation in the inheritance hierarchy uses the same source
    * message.
    */
   protected boolean m_bPrimary;

   /**
    * The category of the transformation.
    */
   protected String m_sCategory;

   /**
    * The metadata resource name.
    */
   protected String m_sResourceName;

   // associations

   /**
    * The base transformation, if any.
    */
   protected Transformation m_base;

   /**
    * The derived transformations, if any.
    */
   protected List m_derivedTransformationList;

   /**
    * The map of source end-points to transformations. Non-null only
    * on the root base transformation in a hierarchy.
    */
   protected Lookup m_endpointTransformationMap;  // of type Transformation[TransformationEndpoint]

   /**
    * The source end-point.
    */
   protected TransformationEndpoint m_source;

   /**
    * The destination end-point.
    */
   protected TransformationEndpoint m_destination;

   /**
    * The list of transformation arguments.
    */
   protected List m_argumentList;

   /**
    * The initializer code.
    */
   protected Pair m_initializer;

   /**
    * The root transformation source.
    */
   protected TransformationSource m_root;

   /**
    * The finalizer code.
    */
   protected Pair m_finalizer;

   /**
    * The transformation function, containing all the compiled code.
    */
   protected Function m_function;

   /**
    * The named transformation mapping map: TransformationMapping[String].
    * Holds only those transformation mappings that have been given a name
    * in the metadata.
    */
   protected Lookup m_mappingMap = new HashTab();

   /**
    * Ordered set of transformation mappings.
    */
   protected List/*<TransformationMapping>*/ m_mappingList =
      new ArrayList/*<TransformationMapping>*/();

   /**
    * The code position map.
    */
   protected Lookup m_posMap = new IdentityHashTab();

   /**
    * The mapping script collection.
    */
   protected List m_scriptList = new ArrayList(8); // of type Object

   {
      m_scriptList.add(null);
   }

   /**
    * The transformation logger.
    */
   protected Logger m_logger;

   // constructors

   /**
    * Constructs the metadata object.
    * @param sName The transformation name.
    */
   public Transformation(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the metadata object.
    */
   public Transformation()
   {
      super();
   }
   
   // operations

   /**
    * Sets the category of the transformation.
    * @param sCategory The category of the transformation.
    */
   public void setCategory(String sCategory)
   {
      verifyNotReadOnly();
      m_sCategory = sCategory;
   }

   /**
    * @return The category of the transformation.
    */
   public String getCategory()
   {
      return m_sCategory;
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
    * Sets the primary flag. Used to disambiguate the selection of a transformation when
    * more than one transformation in the inheritance hierarchy uses the same source
    * message.
    * @param bPrimary The value of the primary flag.
    */
   public void setPrimary(boolean bPrimary)
   {
      verifyNotReadOnly();
      m_bPrimary = bPrimary;
   }

   /**
    * Gets the primary flag. Used to disambiguate the selection of a transformation when
    * more than one transformation in the inheritance hierarchy uses the same source
    * message.
    * @return The value of the primary flag.
    */
   public boolean isPrimary()
   {
      return m_bPrimary;
   }

   /**
    * Sets the base transformation.
    * @param base The base transformation to set; may be null.
    */
   public void setBaseTransformation(Transformation base)
   {
      verifyNotReadOnly();
      m_base = base;
   }

   /**
    * @return The base transformation; null if none.
    */
   public Transformation getBaseTransformation()
   {
      return m_base;
   }

   /**
    * Determines if a transformation can be upcast to obtain this transformation.
    * @param txf The transformation to upcast.
    * @return True if this transformation can be obtained by upcasting "txf".
    */
   public boolean isUpcast(Transformation txf)
   {
      while (txf != null)
      {
         if (txf == this)
         {
            return true;
         }

         txf = txf.m_base;
      }

      return false;
   }

   /**
    * Adds a derived transformation.
    * @param msg The derived transformation to add.
    */
   public void addDerivedTransformation(Transformation derived)
   {
      verifyNotReadOnly();

      if (m_derivedTransformationList == null)
      {
         m_derivedTransformationList = new ArrayList();
      }

      m_derivedTransformationList.add(derived);
   }

   /**
    * Gets a derived transformation of the given index.
    * @param i The index of the derived transformation to get.
    * @return The derived transformation.
    */
   public Transformation getDerivedTransformation(int i)
   {
      if (m_derivedTransformationList == null)
      {
         throw new IndexOutOfBoundsException();
      }

      return (Transformation)m_derivedTransformationList.get(i);
   }

   /**
    * Gets the number of derived transformations.
    * @return The count of derived transformations.
    */
   public int getDerivedTransformationCount()
   {
      return (m_derivedTransformationList == null) ? 0 : m_derivedTransformationList.size();
   }

   /**
    * Sets the source end-point.
    * @param source The source end-point to set.
    */
   public void setSource(TransformationEndpoint source)
   {
      verifyNotReadOnly();
      m_source = source;
      
      if (source != null)
      {
         setRoot(new TransformationSource(source));
      }
   }

   /**
    * @return The source end-point.
    */
   public TransformationEndpoint getSource()
   {
      return m_source;
   }
   
   /**
    * Sets the destination end-point.
    * @param destination The destination end-point to set.
    */
   public void setDestination(TransformationEndpoint destination)
   {
      verifyNotReadOnly();
      m_destination = destination;
   }

   /**
    * @return The destination end-point.
    */
   public TransformationEndpoint getDestination()
   {
      return m_destination;
   }
   
   /**
    * Sets the initializer code.
    * @param initializer The initializer code to set.
    */
   public void setInitializer(Pair initializer)
   {
      verifyNotReadOnly();
      m_initializer = initializer;
   }

   /**
    * @return The initializer code.
    */
   public Pair getInitializer()
   {
      return m_initializer;
   }
   
   /**
    * Sets the finalizer code.
    * @param finalizer The finalizer code to set.
    */
   public void setFinalizer(Pair finalizer)
   {
      verifyNotReadOnly();
      m_finalizer = finalizer;
   }

   /**
    * @return The finalizer code.
    */
   public Pair getFinalizer()
   {
      return m_finalizer;
   }

   /**
    * Adds a new transformation mapping to the transformation.
    * @param mapping The transformation mapping to add.
    * @throws MetadataException if a transformation mapping
    * with the same name already exists.
    */
   public void addMapping(TransformationMapping mapping)
   {
      verifyNotReadOnly();

      if (mapping.getName() != null)
      {
         Object oldMapping = m_mappingMap.put(mapping.getName(), mapping);

         if (oldMapping != null)
         {
            m_mappingMap.put(mapping.getName(), oldMapping);

            throw new MetadataException("err.meta.transformation.mappingDup", new Object[]
            {
               mapping.getName(),
               m_sName
            });
         }
      }

      m_mappingList.add(mapping);
   }

   /**
    * Gets a transformation mapping by name.
    * @param sName The transformation mapping name.
    * @return The transformation mapping object.
    * @throws MetadataLookupException if the transformation mapping does not exist.
    */
   public TransformationMapping getMapping(String sName)
   {
      TransformationMapping mapping = (TransformationMapping)m_mappingMap.get(sName);

      if (mapping != null)
      {
         return mapping;
      }

      throw new MetadataLookupException("err.meta.transformation.mappingLookup", sName, this);
   }

   /**
    * Finds a transformation mapping by name.
    * @param sName The transformation mapping name.
    * @return The transformation mapping object; null if not found.
    */
   public TransformationMapping findMapping(String sName)
   {
      return (TransformationMapping)m_mappingMap.get(sName);
   }

   /**
    * @return The transformation mapping count.
    */
   public int getMappingCount()
   {
      return m_mappingList.size();
   }

   /**
    * @return An iterator for the contained transformation mapping objects.
    */
   public Iterator getMappingIterator()
   {
      return m_mappingList.iterator();
   }

   /**
    * Sets the root transformation source.
    * @param root The root transformation source to set.
    */
   public void setRoot(TransformationSource root)
   {
      verifyNotReadOnly();
      m_root = root;
      root.setLevel(0);
      root.setPrimary(true);
   }

   /**
    * @return The root transformation source.
    */
   public TransformationSource getRoot()
   {
      return m_root;
   }

   /**
    * Sets the transformation function, containing all the compiled code.
    * @param function The transformation function, containing all the compiled code to set.
    */
   public void setFunction(Function function)
   {
      verifyNotReadOnly();
      m_function = function;
   }

   /**
    * @return The transformation function, containing all the compiled code.
    */
   public Function getFunction()
   {
      return m_function;
   }
   
   /**
    * Sets the code position map.
    * @param posMap The code position map to set.
    */
   public void setPosMap(Lookup posMap)
   {
      verifyNotReadOnly();
      m_posMap = posMap;
   }

   /**
    * @return The code position map.
    */
   public Lookup getPosMap()
   {
      return m_posMap;
   }

   /**
    * Adds a new mapping script to the transformation.
    * @param script The mapping script to add.
    * @return The ordinal number of the script.
    */
   public int addScript(Object script)
   {
      verifyNotReadOnly();
      m_scriptList.add(script);
      
      return m_scriptList.size() - 1;
   }

   /**
    * Gets a mapping script by ordinal number.
    * @param nOrdinal The mapping script ordinal number (0-based).
    * @return The mapping script object.
    */
   public Object getScript(int nOrdinal)
   {
      return m_scriptList.get(nOrdinal);
   }

   /**
    * @return The mapping script count.
    */
   public int getScriptCount()
   {
      return m_scriptList.size();
   }

   /**
    * @return An iterator for the contained mapping script objects.
    */
   public Iterator getScriptIterator()
   {
      return m_scriptList.iterator();
   }
   
   /**
    * Compiles the transformation code.
    * @param machine The virtual machine for compilation.
    */
   public void compile(Machine machine)
   {
      verifyNotReadOnly();

      // (lambda (#transformer destination source tf-arg1 tf-arg2 ... tf-argN)
      //    (define #mappers ())
      //    (define #base ())                         ; derived only
      //    (define #finalizeBase #t)                 ; derived only
      //    (                                         ; derived only
      //       (lambda (call-next)
      //          <Initializer>
      //          (if (null? #base) (call-next))
      //       )
      //       (lambda ()
      //          (assert (null? #base))
      //          (set! #base ((lambda (#basefn) (#basefn #transformer ... ) <base-function>))
      //          ()
      //       )
      //    )
      //    <Initializer>                             ; base only
      //    (set! #mappers
      //       (vector
      //          (lambda ()                          ; derived only
      //             (
      //                (lambda (call-next)
      //                   <Finalizer>
      //                   (if #finalizeBase (call-next))
      //                )
      //                (lambda ()
      //                   (assert #finalizeBase)
      //                   ((#base 0))
      //                   (set! #finalizeBase #f)
      //                   ()
      //                )
      //             )
      //             destination
      //          )
      //          (lambda () <Finalizer> destination) ; base only
      //          (#base mapperI)
      //          (#base mapperK)
      //          ...
      //          (lambda (arg1 ... argL)
      //             (if condition ((lambda () <Script>)) <Undefined.VALUE>)
      //          )
      //          (lambda (arg1 ... argM)
      //             (if condition
      //                (#transformer'transformNested <Source> <Transformation> <Arguments>)
      //                <Undefined.VALUE>
      //             )
      //          )
      //          ...
      //       )
      //    )
      //    (vector-set! #base X (vector-ref #mappers X)) ; for derived mapping that overrides base mapping
      //    (vector-set! #base Y (vector-ref #mappers Y))
      //    #mappers
      // )

      // Finalizer
      if (m_base == null)
      {
         // (lambda () <Finalizer> destination)
         m_scriptList.set(0, new Pair(Symbol.LAMBDA, new Pair(null,
            Pair.append(m_finalizer, Pair.list(Symbol.DESTINATION)))));
      }
      else
      {
         // (lambda ()
         //    (
         //       (lambda (call-next)
         //          <Finalizer>
         //          (if #finalizeBase (call-next))
         //       )
         //       (lambda ()
         //          (assert #finalizeBase)
         //          ((#base 0))
         //          (set! #finalizeBase #f)
         //          ()
         //       )
         //    )
         //    destination
         // )
         m_scriptList.set(0,
            Pair.list(
               Symbol.LAMBDA,
               null,
               Pair.list(
                  new Pair(
                     Symbol.LAMBDA,
                     new Pair(
                        Pair.list(Symbol.CALL_NEXT),
                        Pair.append(
                           m_finalizer,
                           Pair.list(
                              Pair.list(Symbol.IF, FINALIZE_BASE, Pair.list(Symbol.CALL_NEXT))
                           )
                        )
                     )
                  ),
                  // call-next:
                  Pair.list(
                     Symbol.LAMBDA,
                     null,
                     Pair.list(ASSERT, FINALIZE_BASE),
                     Pair.list(Pair.list(BASE, Primitive.ZERO_INTEGER)),
                     Pair.list(Symbol.SET, FINALIZE_BASE, Boolean.FALSE),
                     null
                  )
               ),
               Symbol.DESTINATION
            )
         );
      }

      m_logger = Logger.getLogger(getClass().getName() + '.' + m_sName);

      Pair argSymbols = new Pair(
         TRANSFORMER,
         new Pair(
            Symbol.DESTINATION,
            new Pair(
               Symbol.SOURCE,
               (m_argumentList == null) ? null : Pair.fromIterator(m_argumentList.iterator())
            )
         )
      );

      if (m_scriptList.size() == 1 && m_initializer == null && m_finalizer == null)
      {
         // (lambda (#transformer destination source tf-arg1 tf-arg2 ... tf-argN)
         //    (vector
         //       (lambda () destination)
         //    )
         // )
         Object code = Pair.list(
            Symbol.LAMBDA,
            argSymbols,
            Pair.list(
               Symbol.VECTOR,
               Pair.list(Symbol.LAMBDA, null, Symbol.DESTINATION)
            )
         );

         m_function = compile(code, null, machine);

         return;
      }

      Pair initializer = m_initializer;

      if (m_base != null)
      {
         assert m_base.getFunction() != null : "Derived transformation " + m_sName + " compiled before base transformation " + m_base.m_sName;

         // (define #base ())
         // (define #finalizeBase #t)
         // (
         //    (lambda (call-next)
         //       <Initializer>
         //       (if (null? #base) (call-next))
         //    )
         //    (lambda ()
         //       (assert (null? #base))
         //       (set! #base ((lambda (#basefn) (#basefn #transformer ...)) <base-function>))
         //       ()
         //    )
         // )
         initializer = Pair.list(
            Pair.list(Symbol.DEFINE, BASE, null),
            Pair.list(Symbol.DEFINE, FINALIZE_BASE, Boolean.TRUE),
            Pair.list(
               new Pair(
                  Symbol.LAMBDA,
                  new Pair(
                     Pair.list(Symbol.CALL_NEXT),
                     Pair.append(
                        m_initializer,
                        Pair.list(
                           Pair.list(Symbol.IF, Pair.list(Symbol.NULL_P, BASE), Pair.list(Symbol.CALL_NEXT))
                        )
                     )
                  )
               ),
               // call-next:
               Pair.list(
                  Symbol.LAMBDA,
                  null,
                  Pair.list(ASSERT, Pair.list(Symbol.NULL_P, BASE)),
                  Pair.list(
                     Symbol.SET,
                     BASE,
                     Pair.list(
                        Pair.list(
                           Symbol.LAMBDA,
                           Pair.list(BASE_FN),
                           new Pair(BASE_FN, argSymbols)
                        ),
                        m_base.getFunction()
                     )
                  ),
                  null
               )
            )
         );
      }

      // (vector-set! #base X (vector-ref #mappers X)) ; for derived mapping that overrides base mapping
      // (vector-set! #base Y (vector-ref #mappers Y))
      // #mappers
      Pair derivedReplacement = new Pair(MAPPERS);

      if (m_base != null)
      {
         assert (m_base.m_scriptList.size() <= m_scriptList.size());

         for (int i = 1, nCount = m_base.m_scriptList.size(); i < nCount; i++)
         {
            Pair script = (Pair)m_scriptList.get(i);

            if (!BASE.equals(script.getHead()))
            {
               derivedReplacement = new Pair(
                  Pair.list(
                     Symbol.VECTOR_SET,
                     BASE,
                     Primitive.createInteger(i),
                     Pair.list(Symbol.VECTOR_REF, MAPPERS, Primitive.createInteger(i))
                  ),
                  derivedReplacement
               );
            }
         }
      }

      if (derivedReplacement.getHead() == MAPPERS)
      {
         derivedReplacement = null;
      }

      // Create transformation function body.
      Object code = new Pair(
         Symbol.LAMBDA,
         new Pair(
            argSymbols,
            new Pair(
               Pair.list(Symbol.DEFINE, MAPPERS, null),
               Pair.append(
                  initializer,
                  new Pair(
                     Pair.list(
                        Symbol.SET,
                        MAPPERS,
                        new Pair(
                           Symbol.VECTOR,
                           Pair.fromIterator(m_scriptList.iterator())
                        )
                     ),
                     derivedReplacement
                  )
               )
            )
         )
      );

      m_function = compile(code, null, machine);
   }

   /**
    * Compiles a given code in the context of the transformation.
    * @param code The code to compile.
    * @param sName The URL part name.
    * @param machine The VM to use for compilation.
    */
   protected PCodeFunction compile(Object code, String sName, Machine machine)
   {
      try
      {
         m_posMap.put(code, new TextPosition(0, 0, URL_PREFIX + m_sName));
         machine.getGlobalEnvironment().defineVariable(Symbol.SYS_CURRENT_LOGGER, m_logger);

         return new Compiler().compile(code, m_posMap, machine, false);
      }
      catch (CompilerException e)
      {
         MetadataValidationException x = new MetadataValidationException(e);

         setProperties(x);

         String sURL = e.getURL();

         if (sURL != null)
         {
            int i = URL_PREFIX.length() + m_sName.length() + 1;
            int k = sURL.indexOf('.', i);

            if (k >= 0)
            {
               int m = sURL.indexOf('.', k + 1);

               if (m >= 0)
               {
                  x.setProperty("argument", sURL.substring(m + 1));
               }
               else
               {
                  m = sURL.length();
               }

               x.setProperty("mapping", sURL.substring(k + 1, m));
            }
            else
            {
               x.setProperty("script", sURL.substring(i));
            }
         }

         throw x;
      }
      finally
      {
         machine.getGlobalEnvironment().removeVariable(Symbol.SYS_CURRENT_LOGGER);
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      marker.setTypeName("Transformation");
      marker.setResourceName(m_sResourceName);
      marker.setProperty("transformation", m_sName);
   }

   /**
    * @see nexj.core.util.LoggerHolder#getLogger()
    */
   public Logger getLogger()
   {
      return m_logger;
   }

   /**
    * Adds a named argument to the transformation.
    * @param sName The name of the argument to add.
    */
   public void addArgument(String sName)
   {
      if (m_argumentList == null)
      {
         m_argumentList = new ArrayList(2);
      }

      m_argumentList.add(Symbol.define(sName));
   }

   /**
    * Gets the count of transformation arguments.
    * @return The number of arguments to this transformation.
    */
   public int getArgumentCount()
   {
      return (m_argumentList == null) ? 0 : m_argumentList.size();
   }

   /**
    * Gets a transformation argument by ordinal number.
    * @param nOrdinal The ordinal of the argument Symbol to retrieve.
    * @return The argument Symbol.
    */
   public Symbol getArgument(int nOrdinal)
   {
      if (m_argumentList == null || nOrdinal < 0 || nOrdinal >= m_argumentList.size())
      {
         throw new IndexOutOfBoundsException();
      }

      return (Symbol)m_argumentList.get(nOrdinal);
   }

   /**
    * Resolves inheritance for the transformations specified by the iterator.
    * @param txfIterator An iterator over the set of transformations for which
    * inheritance resolution shall be performed.
    */
   public static void resolveInheritance(Iterator txfIterator)
   {
      MetadataCompoundValidationException comp = null;
      Holder circularTxfSet = new IdentityHashHolder();
      Holder resolvedTxfSet = new IdentityHashHolder();

      while (txfIterator.hasNext())
      {
         Transformation transformation = (Transformation)txfIterator.next();

         circularTxfSet.add(transformation);

         if (transformation.m_base == null)
         {
            try
            {
               transformation.resolveInheritance(resolvedTxfSet);
            }
            catch (UncheckedException ex)
            {
               if (comp == null)
               {
                  comp = new MetadataCompoundValidationException();
               }

               transformation.addException(comp, ex);
            }
         }
      }

      // Inheritance cycles have no transformation where m_base == null, so they are never resolved.
      if (circularTxfSet.size() != resolvedTxfSet.size())
      {
         circularTxfSet.removeAll(resolvedTxfSet);

         if (comp == null)
         {
            comp = new MetadataCompoundValidationException();
         }

         // Add an error marker for every transformation in the cycle(s).
         for (Iterator itr = circularTxfSet.iterator(); itr.hasNext(); )
         {
            Transformation circularTxf = (Transformation)itr.next();
            MetadataException x = new MetadataException(
               "err.meta.transformation.circularInheritance",
               new Object[] {circularTxf.getName()});

            circularTxf.addException(comp, x);
         }
      }

      if (comp != null)
      {
         throw comp;
      }
   }

   /**
    * Resolves inheritance for the transformation hierarchy starting at
    * this transformation.
    * @param resolvedTxfSet The set to which transformations shall be added
    * after processing.
    */
   protected void resolveInheritance(Holder resolvedTxfSet)
   {
      verifyNotReadOnly();
      resolvedTxfSet.add(this);

      for (int i = 0, nSize = getDerivedTransformationCount(); i < nSize; i++)
      {
         Transformation child = (Transformation)m_derivedTransformationList.get(i);

         child.resolveInheritance(this);
         child.resolveInheritance(resolvedTxfSet);
      }
   }

   /**
    * Resolves inheritance for this transformation.
    * @param base The base transformation of this transformation.
    */
   protected void resolveInheritance(Transformation base)
   {
      verifyNotReadOnly();

      if (!base.getSource().isUpcast(getSource()))
      {
         throw new MetadataException("err.meta.transformation.sourceTypeMismatch",
            new Object[]{getName(), base.getSource().getName(), getSource().getName()});
      }

      if (!base.getDestination().isUpcast(getDestination()))
      {
         throw new MetadataException("err.meta.transformation.destinationTypeMismatch",
            new Object[]{getName(), base.getDestination().getName(), getDestination().getName()});
      }

      if (base.getArgumentCount() != getArgumentCount())
      {
         throw new MetadataException("err.meta.transformation.baseArgumentMismatch",
            new Object[]{base.getName(), getName()});
      }

      for (int i = 0, nCount = getArgumentCount(); i < nCount; i++)
      {
         if (!base.getArgument(i).equals(getArgument(i)))
         {
            throw new MetadataException("err.meta.transformation.baseArgumentMismatch",
               new Object[]{base.getName(), getName()});
         }
      }

      // Process sources
      m_root.resolveInheritance(base.m_root);

      // Process mappings
      Lookup mappingMap = m_mappingMap;
      Set mappingSet = new HashHolder(m_mappingList.size()); // TransformationMapping[]

      mappingSet.addAll(m_mappingList);
      m_mappingList.clear();
      m_mappingMap = new HashTab(mappingSet.size());

      for (int i = 0, nCount = base.m_mappingList.size(); i < nCount; ++i)
      {
         TransformationMapping baseMapping = (TransformationMapping)base.m_mappingList.get(i);
         TransformationMapping mapping = null;

         if (baseMapping.getName() != null)
         {
            mapping = (TransformationMapping)mappingMap.get(baseMapping.getName());
         }

         if (mapping == null)
         {
            // Copy base transformation mapping to derived transformation
            TransformationMapping copy = (TransformationMapping)baseMapping.clone();

            // Argument sources must point to the derived transformation
            for (Iterator argItr = copy.getArgumentIterator(); argItr.hasNext(); )
            {
               TransformationArgument arg = (TransformationArgument)argItr.next();
               TransformationSource source = arg.getSource();
               TransformationSource newSource = null;

               if (source != null)
               {
                  newSource = m_root;

                  for (int k = 1; k <= source.getLevel(); ++k)
                  {
                     newSource = newSource.findChild(source.getPath(k).getPart().getName());
                  }

                  arg.setSource(newSource);

                  if (arg.isNull())
                  {
                     newSource.setNull(true);
                  }
               }
            }

            // Inherited mappings should reference the base script.
            if (baseMapping.getScript() != null)
            {
               copy.setScript(
                  new Pair(Transformation.BASE, baseMapping)
               );
            }

            addMapping(copy);
         }
         else
         {
            // Base transformation mapping overridden in derived transformation, so use
            // mapping from derived transformation.
            addMapping(mapping);
            mappingSet.remove(mapping);
         }
      }

      // Put the mappings defined only in the derived transformation on the end
      for (Iterator itr = mappingSet.iterator(); itr.hasNext(); )
      {
         addMapping((TransformationMapping)itr.next());
      }

      for (Lookup.Iterator it = base.m_posMap.iterator(); it.hasNext();)
      {
         it.next();
         m_posMap.put(it.getKey(), it.getValue());
      }
   }

   /**
    * Adds a mapping from a source end-point to its transformation.
    * @param endpoint The source end-point of the transformation.
    * @param mappedTxf The transformation.
    */
   private void addEndpointTransformation(TransformationEndpoint endpoint, Transformation mappedTxf)
   {
      if (m_endpointTransformationMap == null)
      {
         m_endpointTransformationMap = new HashTab();
      }

      Transformation other = (Transformation)m_endpointTransformationMap.put(endpoint, mappedTxf);
      Transformation baseTxf;

      if (other == null)
      {
         // No collision, so add mapping to parent as well.
         if ((baseTxf = getBaseTransformation()) != null)
         {
            baseTxf.addEndpointTransformation(endpoint, mappedTxf);
         }
      }
      else
      {
         // If old entry is base of new entry, new entry is left in map: it is more specific.
         // Otherwise, update map with a more general entry.
         if (!other.isUpcast(mappedTxf))
         {
            if (!mappedTxf.isUpcast(other))
            {
               // The old entry and new entry do not inherit from each other.
               if (other.isPrimary() ^ mappedTxf.isPrimary())
               {
                  if (other.isPrimary())
                  {
                     // The old entry takes precedence over the new entry.
                     m_endpointTransformationMap.put(endpoint, mappedTxf = other);
                  }
               }
               else
               {
                  // Unable to disambiguate; map to their common base.
                  assert isUpcast(other) && isUpcast(mappedTxf);
                  m_endpointTransformationMap.put(endpoint, mappedTxf = this);
               }
            }

            // Add the mapping to our parent as well.
            if ((baseTxf = getBaseTransformation()) != null)
            {
               baseTxf.addEndpointTransformation(endpoint, mappedTxf);
            }
         }
      }
   }

   /**
    * Finds a transformation that can be up-cast to this transformation and
    * that best represents the given end-point (i.e. most-strictly encloses the end-point).
    * @param endpoint The end-point.
    * @return The transformation that most-strictly encloses the end-point.
    */
   public Transformation findTransformation(TransformationEndpoint endpoint)
   {
      if (m_endpointTransformationMap == null)
      {
         return (m_source == endpoint) ? this : null;
      }

      return (Transformation)m_endpointTransformationMap.get(endpoint);
   }

   /**
    * Finishes loading the transformation. Finishing order is from base to derived
    * transformations.
    * @param machine The VM used for compilation.
    */
   public void finish(Machine machine)
   {
      verifyNotReadOnly();

      if (m_base == null)
      {
         finishTransformation1(machine);
         finishTransformation2(machine);
      }
   }

   /**
    * Finishes the transformation and all its derived transformations, first pass.
    * @param machine The VM used for compilation.
    */
   private void finishTransformation1(Machine machine)
   {
      // Build map only on transformations participating in inheritance.
      if (m_base != null || getDerivedTransformationCount() > 0)
      {
         addEndpointTransformation(m_source, this);
      }

      m_nMaxLevel = m_root.getMaxLevel();

      for (Iterator itr = getMappingIterator(); itr.hasNext(); )
      {
         ((TransformationMapping)itr.next()).finish1(this, machine);
      }

      // Finish the derived transformations
      for (int i = 0, nCount = getDerivedTransformationCount(); i < nCount; i++)
      {
         getDerivedTransformation(i).finishTransformation1(machine);
      }
   }

   /**
    * Finishes the transformation and all its derived transformations, second pass.
    * @param machine The VM used for compilation.
    */
   private void finishTransformation2(Machine machine)
   {
      for (Iterator itr = getMappingIterator(); itr.hasNext(); )
      {
         ((TransformationMapping)itr.next()).finish2(this, machine);
      }

      compile(machine);

      // Finish the derived transformations
      for (int i = 0, nCount = getDerivedTransformationCount(); i < nCount; i++)
      {
         getDerivedTransformation(i).finishTransformation2(machine);
      }
   }

   /**
    * Gets the maximum level of all the transformation sources.
    * @return The maximum level.
    */
   public int getMaxLevel()
   {
      return m_nMaxLevel;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();
      m_root.makeReadOnly();

      if (m_derivedTransformationList != null)
      {
         ((ArrayList)m_derivedTransformationList).trimToSize();
      }

      if (m_argumentList != null)
      {
         ((ArrayList)m_argumentList).trimToSize();
      }

      if (m_scriptList != null)
      {
         ((ArrayList)m_scriptList).trimToSize();
      }

      ((ArrayList)m_mappingList).trimToSize(); // release unused memory
      m_scriptList = null;
      m_posMap = null;
   }

   // inner classes

   /**
    * End-point part template used for implementing transformation to/from :oid, :class, and :event.
    */
   protected static abstract class SystemAttributeEndpointPart implements EndpointPart
   {
      // attributes

      /**
       * The end-point part name.
       */
      public String m_sName;

      // constructor

      /**
       * Creates a new system attribute end-point part.
       * @param sName The end-point part name.
       */
      public SystemAttributeEndpointPart(String sName)
      {
         m_sName = sName;
      }

      // operations

      /**
       * @see nexj.core.util.Named#getName()
       */
      public String getName()
      {
         return m_sName;
      }

      /**
       * @see nexj.core.meta.integration.EndpointPart#createObject()
       */
      public TransferObject createObject()
      {
         throw new IllegalStateException(m_sName);
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
         return true;
      }

      /**
       * @see nexj.core.meta.integration.EndpointPart#getChild(java.lang.String)
       */
      public EndpointPart getChild(String sName)
      {
         throw new MetadataLookupException("err.meta.namedLookup",
            new Object[]{EndpointPart.class.getName(), sName});
      }

      /**
       * @see nexj.core.meta.integration.EndpointPart#findChild(java.lang.String)
       */
      public EndpointPart findChild(String sName)
      {
         return null;
      }

      /**
       * @see nexj.core.meta.integration.EndpointPart#getChildIterator()
       */
      public Iterator getChildIterator()
      {
         return EmptyIterator.getInstance();
      }
   }
}
