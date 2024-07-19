// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.service;

import java.util.Arrays;

import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.integration.Channel;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.CompilerException;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Lookup;
import nexj.core.util.Undefined;

/**
 * Service to channel binding.
 */
public class Binding extends MetadataObject
{
   // constants
   
   /**
    * The URL prefix.
    */
   protected final static String URL_PREFIX = "binding:";
   
   // associations

   /**
    * The service.
    */
   protected Service m_service;

   /**
    * The channel.
    */
   protected Channel m_channel;

   /**
    * The output channel.
    */
   protected Channel m_output;

   /**
    * The compiled service invocation function: (lambda (this) ...).
    */
   protected PCodeFunction m_function;

   /**
    * The argument S-expression array.
    */
   protected Object[] m_argArray;

   /**
    * The expression position map.
    */
   protected Lookup m_posMap = new IdentityHashTab();
   
   /**
    * The expression URL map.
    */
   protected Lookup m_urlMap = new IdentityHashTab();

   // constructors

   /**
    * Constructs the binding.
    * @param service The service to bind.
    * @param channel The channel to bind.
    */
   public Binding(Service service, Channel channel)
   {
      setService(service);
      setChannel(channel);
   }

   // operations
   
   /**
    * Sets the service.
    * @param service The service to set.
    */
   public void setService(Service service)
   {
      verifyNotReadOnly();
      m_service = service;

      if (service != null)
      {
         m_argArray = new Object[service.getArgumentCount()];
         Arrays.fill(m_argArray, Undefined.VALUE);
      }
   }

   /**
    * @return The service.
    */
   public Service getService()
   {
      return m_service;
   }

   /**
    * Sets the channel.
    * @param channel The channel to set.
    */
   public void setChannel(Channel channel)
   {
      verifyNotReadOnly();
      m_channel = channel;
   }

   /**
    * @return The channel.
    */
   public Channel getChannel()
   {
      return m_channel;
   }

   /**
    * Sets the output channel.
    * @param output The output channel to set.
    */
   public void setOutput(Channel output)
   {
      verifyNotReadOnly();
      m_output = output;
   }

   /**
    * @return The output channel.
    */
   public Channel getOutput()
   {
      return m_output;
   }

   /**
    * @return The compiled service invocation function: (lambda (this) ...).
    */
   public PCodeFunction getFunction()
   {
      return m_function;
   }

   /**
    * @return The expression position map.
    */
   public Lookup getPosMap()
   {
      return m_posMap;
   }
   
   /**
    * @return The expression URL map.
    */
   public Lookup getURLMap()
   {
      return m_urlMap;
   }

   /**
    * Sets the argument channel.
    * Either a channel or a value can be set.
    * @param sArgument The argument name.
    * @param channel the channel to set.
    */
   public void setArgumentChannel(String sArgument, Channel channel) throws MetadataException
   {
      setArgumentValue(sArgument, channel.getName());
   }

   /**
    * Sets the argument value as an S-expression.
    * @param sArgument The argument name.
    * @param value The argument value.
    */
   public void setArgumentValue(String sArgument, Object value) throws MetadataException
   {
      verifyNotReadOnly();

      int nOrdinal = m_service.getOrdinal(m_service.getArgument(sArgument));

      if (m_argArray[nOrdinal] != Undefined.VALUE)
      {
         throw new MetadataException("err.meta.integration.binding.argumentDup",
            new Object[]{sArgument, m_service.getFullName(), m_channel.getName()});
      }

      m_argArray[nOrdinal] = value;

      Compiler.setPosURLs(value, URL_PREFIX + m_channel.getName() + ':' +
         m_service.getFullName() + ':' + sArgument, m_posMap, m_urlMap);
   }

   /**
    * Gets an argument by name.
    * @param sArgument The argument name.
    * @return The argument value as an S-expression.
    */
   public Object getArgumentValue(String sArgument) throws MetadataException
   {
      return (m_argArray == null) ? Undefined.VALUE : m_argArray[m_service.getOrdinal(m_service.getArgument(sArgument))];
   }
   
   /**
    * Compiles the service invocation.
    * @param machine The VM for compilation.
    */
   public void compile(Machine machine) throws MetadataException
   {
      verifyNotReadOnly();

      for (int i = 0; i != m_argArray.length; ++i)
      {
         if (m_argArray[i] == Undefined.VALUE)
         {
            throw new MetadataException("err.meta.integration.binding.unboundArg",
               new Object[]{m_service.getArgument(i), m_service.getFullName(), m_channel.getName()});
         }
      }

      // (lambda (this) ('<SysService>'invoke <serviceName> this <output> arg1 ... argN)) 
      Pair code = null;

      for (int i = m_argArray.length - 1; i >= 0; --i)
      {
         code = new Pair(m_argArray[i], code);
      }

      code = Pair.list(Symbol.LAMBDA, Pair.list(Symbol.THIS),
         new Pair(Pair.quote(m_service.getMetadata().getMetaclass(Metadata.SERVICE_CLASS_NAME)),
            new Pair(Pair.quote(Symbol.INVOKE),
               new Pair(m_service.getName(),
                  new Pair(Symbol.THIS,
                     new Pair((m_output == null) ? null : m_output.getName(), code))))));

      try
      {
         m_function = new Compiler().compile(code, m_posMap, m_urlMap, machine, false);
      }
      catch (CompilerException e)
      {
         MetadataValidationException x = new MetadataValidationException(e);

         setProperties(x);

         String sURL = ((CompilerException)e).getURL();

         if (sURL != null)
         {
            x.setProperty("argument", sURL.substring(URL_PREFIX.length() +
               m_channel.getName().length() + m_service.getFullName().length() + 2));
         }

         throw x;
      }

      m_argArray = null;
      m_posMap = null;
      m_urlMap = null;
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      marker.setTypeName("ServiceBinding");
      marker.setProperty("service", m_service.getFullName());
      marker.setProperty("channel", m_channel.getName());
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "Binding(" + m_channel + ", " + m_service + ")";
   }
}
