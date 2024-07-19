// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.service;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Event;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.PrimitivePrivilege;
import nexj.core.meta.workflow.Flow;
import nexj.core.meta.workflow.Variable;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Service metadata.
 */
public class Service extends Flow
{
   // constants
   
   /**
    * The output variable name.
    */
   public final static String OUTPUT = ":output";

   /**
    * Default function that returns "this" for message passing in service.
    */
   protected final static PCodeFunction DEFAULT_FUNCTION = new PCodeFunction(
      new char[]
      {
         Machine.SETUP_FRAME,
            (char)1,
         Machine.PUSH_LOCAL_0,
            (char)1,
         Machine.RETURN
      },
      null
   );


   // associations

   /**
    * The output variable.
    */
   protected Variable m_output;

   /**
    * The argument collection.
    */
   protected List m_argumentList = new ArrayList(2); // of type Variable

   /**
    * The service interface.
    */
   protected Interface m_interface;

   /**
    * The privilege for service invocation.
    */
   protected PrimitivePrivilege m_privilege;

   /**
    * The root metadata object.
    */
   protected Metadata m_metadata;

   // constructors

   /**
    * Constructs the service.
    * @param sName The service name.
    * @param nVersion The flow version.
    * @param metadata The root metadata object.
    */
   public Service(String sName, int nVersion, Metadata metadata)
   {
      super(sName, nVersion);

      m_metadata = metadata;
      m_output = new Variable(OUTPUT);
      addVariable(m_output);
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
    * @see nexj.core.meta.workflow.Flow#getMetadata()
    */
   public Metadata getMetadata()
   {
      return m_metadata;
   }

   /**
    * @see nexj.core.meta.workflow.Flow#isCurrentVersion()
    */
   public boolean isCurrentVersion()
   {
      return m_metadata.getService(getName()).getVersion() == m_nVersion;
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getMetaclass()
    */
   public Metaclass getMetaclass()
   {
      return null;
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getAttributes()
    */
   public Pair getAttributes()
   {
      return null;
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getFunctions(nexj.core.scripting.Pair, nexj.core.meta.Event)
    */
   public Pair getFunctions(Pair assoc, Event event)
   {
      return null;
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getEmptyFunction()
    */
   public PCodeFunction getEmptyFunction()
   {
      return DEFAULT_FUNCTION;
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getDefaultReturnCode()
    */
   public Pair getDefaultReturnCode()
   {
      return Pair.list(Symbol.THIS);
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getTypeName()
    */
   protected String getTypeName()
   {
      return "Service";
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getPropName()
    */
   public String getPropName()
   {
      return "service";
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getURLPrefix()
    */
   protected String getURLPrefix()
   {
      return "service:";
   }

   /**
    * @return The output variable.
    */
   public Variable getOutput()
   {
      return m_output;
   }
   
   /**
    * Adds a new argument to the service.
    * @param argument The argument to add.
    * @throws MetadataException if the variable already exists.
    */
   public void addArgument(Variable argument) throws MetadataException
   {
      verifyNotReadOnly();
      
      addVariable(argument);
      argument.setFlags(Variable.ARG, true);
      m_argumentList.add(argument);
   }

   /**
    * Gets a argument by ordinal number.
    * @param nOrdinal The argument ordinal number (0-based).
    * @return The argument object.
    */
   public Variable getArgument(int nOrdinal)
   {
      return (Variable)m_argumentList.get(nOrdinal);
   }

   /**
    * Gets an argument by name.
    * @param sName The argument name.
    * @return The argument object.
    * @throws MetadataLookupException if the argument does not exist.
    */
   public Variable getArgument(String sName) throws MetadataLookupException
   {
      Variable arg = (Variable)m_variableMap.get(sName);

      if (arg != null && (arg.getFlags() & Variable.ARG) != 0)
      {
         return arg;
      }

      throw new MetadataLookupException("err.meta." + getPropName() + ".argLookup", sName, this);
   }

   /**
    * Gets the argument ordinal number.
    * @param argument The argument.
    * @return The argument ordinal number.
    */
   public int getOrdinal(Variable argument)
   {
      return argument.getOrdinal() - ((Variable)m_argumentList.get(0)).getOrdinal();
   }

   /**
    * @return The argument count.
    */
   public int getArgumentCount()
   {
      return m_argumentList.size();
   }

   /**
    * @return An iterator for the contained argument objects.
    */
   public Iterator getArgumentIterator()
   {
      return m_argumentList.iterator();
   }

   /**
    * Sets the service interface.
    * @param interface The service interface to set.
    */
   public void setInterface(Interface iface)
   {
      verifyNotReadOnly();
      m_interface = iface;
   }

   /**
    * @return The service interface.
    */
   public Interface getInterface()
   {
      return m_interface;
   }
   
   /**
    * Sets the privilege for service invocation.
    * @param privilege The privilege for service invocation to set.
    */
   public void setPrivilege(PrimitivePrivilege privilege)
   {
      verifyNotReadOnly();
      m_privilege = privilege;
   }

   /**
    * @return The privilege for service invocation.
    */
   public PrimitivePrivilege getPrivilege()
   {
      return m_privilege;
   }

   /**
    * @see nexj.core.meta.workflow.Flow#getSysMetaclass()
    */
   public String getSysMetaclass()
   {
      return "SysService";
   }
}
