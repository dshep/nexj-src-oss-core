// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.xml;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Locale;
import java.util.Properties;
import java.util.regex.Pattern;

import nexj.core.meta.Action;
import nexj.core.meta.Argument;
import nexj.core.meta.Aspect;
import nexj.core.meta.Attribute;
import nexj.core.meta.Component;
import nexj.core.meta.ComponentCollectionPropertyInitializer;
import nexj.core.meta.ComponentPropertyInitializer;
import nexj.core.meta.Event;
import nexj.core.meta.ExternalLibrary;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Pointcut;
import nexj.core.meta.Primitive;
import nexj.core.meta.PrimitiveCollectionPropertyInitializer;
import nexj.core.meta.PrimitivePropertyInitializer;
import nexj.core.meta.PropertyInitializer;
import nexj.core.meta.Type;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.Transformation;
import nexj.core.meta.integration.TransformationArgument;
import nexj.core.meta.integration.TransformationEndpoint;
import nexj.core.meta.integration.TransformationMapping;
import nexj.core.meta.integration.TransformationSource;
import nexj.core.meta.integration.XMLIntegrationMetadataExporter;
import nexj.core.meta.integration.XMLMessageMappingExporter;
import nexj.core.meta.integration.service.Binding;
import nexj.core.meta.integration.service.Case;
import nexj.core.meta.integration.service.Dispatch;
import nexj.core.meta.integration.service.Jump;
import nexj.core.meta.integration.service.Persist;
import nexj.core.meta.integration.service.Send;
import nexj.core.meta.integration.service.SendReceive;
import nexj.core.meta.integration.service.Service;
import nexj.core.meta.integration.service.Sync;
import nexj.core.meta.integration.service.Transform;
import nexj.core.meta.j2ee.J2EEEnvRef;
import nexj.core.meta.j2ee.J2EEProperty;
import nexj.core.meta.j2ee.J2EEResourceRef;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.meta.persistence.XMLPersistenceMetadataExporter;
import nexj.core.meta.workflow.Activity;
import nexj.core.meta.workflow.Branch;
import nexj.core.meta.workflow.Catch;
import nexj.core.meta.workflow.Decision;
import nexj.core.meta.workflow.FlowMacro;
import nexj.core.meta.workflow.FlowMacroScript;
import nexj.core.meta.workflow.Script;
import nexj.core.meta.workflow.Step;
import nexj.core.meta.workflow.TryCatch;
import nexj.core.meta.workflow.Variable;
import nexj.core.scripting.FormattingWriter;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.Pair;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Named;
import nexj.core.util.ObjUtil;
import nexj.core.util.PropertyUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.Undefined;
import nexj.core.util.XMLWriter;

/**
 * Exports metadata to an output stream.
 */
public class XMLMetadataExporter
{
   // constants

   /**
    * Export a servlet distributable element.
    */
   public final static int J2EE_DISTRIBUTABLE = 0;

   /**
    * Export a servlet confidential auth constraint.
    */
   public final static int J2EE_CONFIDENTIAL = 1;

   /**
    * Export an HTTP session timeout in minutes.
    */
   public final static int J2EE_SESSION_TIMEOUT = 2;

   /**
    * Export the HTTP context root.
    */
   public final static int J2EE_CONTEXT_ROOT = 3;

   /**
    * Export the HTTP login configuration.
    */
   public final static int J2EE_WEB_LOGIN_CONFIG = 4;

   /**
    * Export a resource-ref element.
    */
   public final static int J2EE_RESOURCE_REF = 5;

   /**
    * Export a resource-env-ref element.
    */
   public final static int J2EE_RESOURCE_ENV_REF = 6;

   /**
    * Export a message-driven element.
    */
   public final static int J2EE_MESSAGE_DRIVEN = 7;

   /**
    * Export a container-transaction element.
    */
   public final static int J2EE_CONTAINER_TRANSACTION = 8;

   /**
    * Export a test filter element.
    */
   public final static int J2EE_TEST_FILTER = 9;

   /**
    * Export a test filter mapping.
    */
   public final static int J2EE_TEST_FILTER_MAPPING = 10;

   /**
    * Export a persistence filter element.
    */
   public final static int J2EE_PERSISTENCE_FILTER = 11;

   /**
    * Export a persistence filter mapping.
    */
   public final static int J2EE_PERSISTENCE_FILTER_MAPPING = 12;

   /**
    * Export a session listener element.
    */
   public final static int J2EE_SESSION_LISTENER = 13;

   /**
    * Export portlet descriptor.
    */
   public final static int J2EE_PORTLETS = 14;

   /**
    * Export a container-specific resource-ref element.
    */
   public final static int J2EE_PLATFORM_RESOURCE_REF = 15;

   /**
    * Export a container-specific resource-env-ref element.
    */
   public final static int J2EE_PLATFORM_RESOURCE_ENV_REF = 16;

   /**
    * Export a container-specific resource ref extension element.
    */
   public final static int J2EE_PLATFORM_RESOURCE_REF_EXT = 17;

   /**
    * Export a container-specific message-driven element.
    */
   public final static int J2EE_PLATFORM_MESSAGE_DRIVEN = 18;

   /**
    * Export a container-specific MDB extension element.
    */
   public final static int J2EE_PLATFORM_MESSAGE_DRIVEN_EXT = 19;

   /**
    * Exports a container-specific activation spec element.
    */
   public final static int J2EE_PLATFORM_ACTIVATION_SPEC = 20;

   /**
    * Export a container-specific connection factory element.
    */
   public final static int J2EE_PLATFORM_CONNECTION_FACTORY = 21;

   /**
    * Export a container-specific admin object element.
    */
   public final static int J2EE_PLATFORM_ADMIN_OBJECT = 22;

   /**
    * Export a container-specific invoker-proxy-binding element.
    */
   public final static int J2EE_PLATFORM_INVOKER_PROXY_BINDING = 23;

   /**
    * Export a container-specific replication-config element.
    */
   public final static int J2EE_PLATFORM_REPLICATION_CONFIG = 24;

   /**
    * Export a container-specific cluster-config element.
    */
   public final static int J2EE_PLATFORM_CLUSTER_CONFIG = 25;

   /**
    * Export a container-specific persistence interceptor element.
    */
   public final static int J2EE_PLATFORM_PERSISTENCE_INTERCEPTOR = 26;

   /**
    * Export a container-specific authentication interceptor element.
    */
   public final static int J2EE_PLATFORM_AUTH_INTERCEPTOR = 27;

   /**
    * Export the application classpath.
    */
   public final static int J2EE_PLATFORM_CLASSPATH = 28;

   /**
    * Export a portal-specific portlet deployment descriptor.
    */
   public final static int J2EE_PLATFORM_PORTLETS = 29;

   /**
    * Export in an J2EE application descriptor context.
    */
   public final static int J2EE_CONTEXT_APP = 0;

   /**
    * Export in an EJB descriptor context.
    */
   public final static int J2EE_CONTEXT_EJB = 1;

   /**
    * Export in a web descriptor context.
    */
   public final static int J2EE_CONTEXT_WEB = 2;

   /**
    * Export in a web descriptor context.
    */
   public final static int J2EE_CONTEXT_WEB_CERT = 3;

   /**
    * Export in a web descriptor context.
    */
   public final static int J2EE_CONTEXT_PORTLET = 4;

   /**
    * Export in a resource adapter context.
    */
   public final static int J2EE_CONTEXT_RA = 5;

   /**
    * Invalid characters for an XML Schema ID (xsd:ID).
    */
   public final static Pattern INVALID_ID_CHARACTERS = Pattern.compile("[/:]");

   // associations

   /**
    * The output stream.
    */
   protected XMLWriter m_writer;

   // constructors

   /**
    * Creates the exporter with an XML print stream.
    * @param writer The output character stream.
    */
   public XMLMetadataExporter(XMLWriter writer)
   {
      m_writer = writer;
   }

   /**
    * Creates the exporter with an output character stream.
    * @param writer The output character stream.
    */
   public XMLMetadataExporter(Writer writer)
   {
      m_writer = new XMLWriter(writer);
   }

   // operations

   /**
    * @return The XML writer.
    */
   public XMLWriter getWriter()
   {
      return m_writer;
   }

   /**
    * @param variableItr Iterator over Variable instances.
    * @param varFilter Optional filter to test variables for inclusion.
    * @return Space-delimited string of variable names or null if iterator is empty.
    */
   private static String variableIteratorToString(Iterator variableItr, VariableFilter varFilter)
   {
      if (variableItr.hasNext())
      {
         StringBuffer tokenBuf = new StringBuffer();

         for (;;)
         {
            Variable v = (Variable)variableItr.next();

            if (varFilter == null || varFilter.accept(v))
            {
               if (tokenBuf.length() > 0)
               {
                  tokenBuf.append(" ");
               }

               tokenBuf.append(v.getName());
            }

            if (!variableItr.hasNext())
            {
               break;
            }
         }

         return (tokenBuf.length() == 0) ? null : tokenBuf.toString();
      }

      return null;
   }

   /**
    * Exports the specified transformation mapping.
    * @param mapping The mapping to export.
    * @param transformation The containing transformation.
    * @throws IOException if an error occurs.
    */
   protected void exportTransformationMapping(TransformationMapping mapping, final Transformation transformation) throws IOException
   {
      m_writer.openElement("Mapping");

      if (mapping.getName() != null)
      {
         m_writer.writeAttribute("name", mapping.getName());
      }

      exportSExpression(mapping.getCondition(), false, false, Boolean.TRUE, "condition");

      if (mapping.getDestinationCount() > 0)
      {
         m_writer.openAttribute("destination");

         for (int i = 0; i < mapping.getDestinationCount(); ++i)
         {
            if (i > 0)
            {
               m_writer.write(' ');
            }

            m_writer.writeValue(mapping.getDestination(i).getName());

            if (mapping.isFixedDestination(i))
            {
               m_writer.write('$');
            }
         }

         m_writer.closeAttribute();
      }

      m_writer.closeElement();

      if (mapping.getArgumentCount() > 0)
      {
         m_writer.startElement("Sources");

         for (int i = 0; i < mapping.getArgumentCount(); ++i)
         {
            TransformationArgument argument = mapping.getArgument(i);

            m_writer.openElement("Source");

            if (argument.getName() != null)
            {
               m_writer.writeAttribute("name", argument.getName());
            }

            if (argument.getMapping() != null)
            {
               m_writer.writeAttribute("mapping", argument.getMapping().getName());
            }
            else if (argument.getSource() != null && argument.getSource() != transformation.getRoot())
            {
               StringBuffer tokenBuf = new StringBuffer();

               for (TransformationSource source = argument.getSource();;)
               {
                  String sPrepend = source.getPart().getName();

                  if (mapping.isFixedSource(source))
                  {
                     sPrepend += "$";
                  }

                  tokenBuf.insert(0, (tokenBuf.length() == 0) ? sPrepend : sPrepend + " ");

                  source = source.getParent();

                  if (source == transformation.getRoot())
                  {
                     break;
                  }
               }

               m_writer.writeAttribute("source", tokenBuf.toString());
            }

            if (argument.isNull())
            {
               m_writer.writeAttribute("null", true);
            }

            if (argument.isDefault())
            {
               exportSExpression(argument.getDefaultValue(), false, false, Undefined.VALUE, "default");
            }

            m_writer.closeEmptyElement();
         }

         m_writer.endElement("Sources");
      }

      if (mapping.getScript() != null)
      {
         m_writer.startElement("Script");
         exportSExpression(mapping.getScript(), true, true, null, null);
         m_writer.endElement("Script");
      }

      m_writer.endElement("Mapping");
   }

   /**
    * Exports the specified transformation.
    * @param transformation The transformation to export.
    * @throws IOException if an error occurs.
    */
   public void exportTransformation(Transformation transformation) throws IOException
   {
      m_writer.openElement("Transformation");

      TransformationEndpoint source = transformation.getSource();

      if (source == null)
      {
         throw new MetadataException("err.meta.transformation.missingSource");
      }

      m_writer.openAttribute("source");

      if (source instanceof Metaclass)
      {
         m_writer.write("class:");
      }

      m_writer.write(source.getName());
      m_writer.closeAttribute();

      TransformationEndpoint destination = transformation.getDestination();

      if (destination == null)
      {
         throw new MetadataException("err.meta.transformation.missingDestination");
      }

      m_writer.openAttribute("destination");

      if (destination instanceof Metaclass)
      {
         m_writer.write("class:");
      }

      m_writer.write(destination.getName());
      m_writer.closeAttribute();
      m_writer.closeElement();

      if (transformation.getInitializer() != null)
      {
         m_writer.startElement("Initializer");
         exportSExpression(transformation.getInitializer(), true, true, null, null);
         m_writer.endElement("Initializer");
      }

      if (transformation.getMappingCount() > 0)
      {
         m_writer.startElement("Mappings");

         for (Iterator itr = transformation.getMappingIterator(); itr.hasNext();)
         {
            exportTransformationMapping((TransformationMapping)itr.next(), transformation);
         }

         m_writer.endElement("Mappings");
      }

      if (transformation.getFinalizer() != null)
      {
         m_writer.startElement("Finalizer");
         exportSExpression(transformation.getFinalizer(), true, true, null, null);
         m_writer.endElement("Finalizer");
      }

      m_writer.endElement("Transformation");
   }

   /**
    * Exports the specified service
    * @param service The service to export.
    * @throws IOException if an error occurs.
    */
   public void exportService(final Service service) throws IOException
   {
      m_writer.openElement("Service");

      String sArgList = variableIteratorToString(service.getArgumentIterator(), null);

      if (sArgList != null)
      {
         m_writer.writeAttribute("args", sArgList);
      }

      if (service.getCaption() != null)
      {
         m_writer.writeAttribute("caption", service.getCaption());
      }

      if (service.getInterface() != null)
      {
         m_writer.writeAttribute("interface", service.getInterface().getName());
      }

      if (service.getLayout() != null)
      {
         m_writer.writeAttribute("layout", service.getLayout());
      }

      if (service.isPrivileged())
      {
         m_writer.writeAttribute("privileged", service.isPrivileged());
      }

      String sVarList = variableIteratorToString(service.getVariableIterator(), new VariableFilter()
      {
         public boolean accept(Variable variable)
         {
            return variable != service.getOutput() && (variable.getFlags() & Variable.ARG) == 0;
         }
      });

      if (sVarList != null)
      {
         m_writer.writeAttribute("variables", sVarList);
      }

      if (service.getPrivilege() != null)
      {
         m_writer.writeAttribute("privilege", service.getPrivilege().getName());
      }

      m_writer.closeElement();

      exportSequence(service);

      m_writer.endElement("Service");
   }

   /**
    * Exports a message flow sequence for an activity.
    * @param activity The activity metadata object.
    * @throws IOException if an error occurs.
    */
   public void exportSequence(Activity activity) throws IOException
   {
      for (Iterator stepItr = activity.getStepIterator(); stepItr.hasNext();)
      {
         exportStep((Step)stepItr.next());
      }
   }

   /**
    * Exports the contents of a Scheme S-expression as CDATA or an escaped attribute value.
    * @param schemeExpr The expression to write.
    * @param bList True to export elements of a list.
    * @param bMultiLine True to pretty-format the output string over multiple lines when necessary.
    * @param defValue The default value used during metadata loading.
    * @param sAttrName Optional attribute name to write as an attribute key/value pair. Null to write as CDATA.
    * @throws IOException If an I/O error occurs.
    */
   public void exportSExpression(Object schemeExpr, boolean bList, boolean bMultiLine, Object defValue, String sAttrName) throws IOException
   {
      if (schemeExpr == null || ObjUtil.equal(schemeExpr, defValue))
      {
         return;
      }

      FormattingWriter writer = (bMultiLine) ? Intrinsic.createPrettyFormatter(null, true)
         : new FormattingWriter(new StringWriter(), true);

      if (bList)
      {
         if (!(schemeExpr instanceof Pair))
         {
            throw new MetadataException("err.meta.service.expectedPairObject",
               new Object[]{schemeExpr});
         }

         for (Pair list = (Pair)schemeExpr; list != null; list = list.getNext())
         {
            writer.formatUnit(list.getHead(), false);
         }
      }
      else
      {
         writer.formatUnit(schemeExpr, false);
      }

      if (sAttrName == null)
      {
         m_writer.writeCDATA(writer.getOutputString());
      }
      else
      {
         m_writer.writeAttribute(sAttrName, writer.getOutputString());
      }
   }

   /**
    * Exports the specified step.
    * @param metaclass The step to export.
    * @param exporter The branch exporter.
    * @throws IOException if an error occurs.
    */
   public void exportDecision(Decision decision, BranchExporter exporter) throws IOException
   {
      for (Iterator itr = decision.getBranchIterator(); itr.hasNext();)
      {
         Branch branch = (Branch)itr.next();

         m_writer.openElement(exporter.getElementName());

         exportSExpression(branch.getCondition(), false, false, Boolean.TRUE, "condition");

         if (branch.getCaption() != null)
         {
            m_writer.writeAttribute("caption", branch.getCaption());
         }

         if (branch.getLayout() != null)
         {
           m_writer.writeAttribute("layout", branch.getLayout());
         }

         exporter.exportActivity(branch);

         m_writer.endElement(exporter.getElementName());
      }
   }

   /**
    * Export common attributes of a step element.
    * @param step The step containing the common attributes.
    * @param sNodeName The xml node name of the step.
    * @throws IOException if an error occurs.
    */
   private void openStepElement(Step step, String sNodeName) throws IOException
   {
      m_writer.openElement(sNodeName);

      if (!(step instanceof Jump))
      {
         m_writer.writeAttribute("name", step.getName());
      }

      if (step.getNext() != null)
      {
         m_writer.writeAttribute("next", step.getNext().getName());
      }

      if (step.getLayout() != null)
      {
         m_writer.writeAttribute("layout", step.getLayout());
      }
   }

   /**
    * Exports the specified channel.
    * @param channel The channel to export.
    * @throws IOException if an error occurs.
    */
   public void exportChannel(Channel channel) throws IOException
   {
      getIntegrationMetadataExporter(channel).exportChannel(channel);
   }

   /**
    * Exports a boolean attribute value if it does not equal a default value.
    * @param sAttrName The attribute name.
    * @param bAttrValue The attribute value.
    * @param bDefValue The default value to compare against.
    * @throws IOException if an error occurs.
    */
   public void writeAttribute(String sAttrName, boolean bAttrValue, boolean bDefValue) throws IOException
   {
      if (bAttrValue != bDefValue)
      {
         m_writer.writeAttribute(sAttrName, bAttrValue);
      }
   }

   /**
    * Exports an int attribute value if it does not equal a default value.
    * @param sAttrName The attribute name.
    * @param nAttrValue The attribute value.
    * @param nDefValue The default value to compare against.
    * @throws IOException if an error occurs.
    */
   public void writeAttribute(String sAttrName, int nAttrValue, int nDefValue) throws IOException
   {
      if (nAttrValue != nDefValue)
      {
         m_writer.writeAttribute(sAttrName, nAttrValue);
      }
   }

   /**
    * Exports an long attribute value if it does not equal a default value.
    * @param sAttrName The attribute name.
    * @param lAttrValue The attribute value.
    * @param lDefValue The default value to compare against.
    * @throws IOException if an error occurs.
    */
   public void writeAttribute(String sAttrName, long lAttrValue, long lDefValue) throws IOException
   {
      if (lAttrValue != lDefValue)
      {
         m_writer.writeAttribute(sAttrName, Long.toString(lAttrValue));
      }
   }

   /**
    * Exports an string attribute value if it does not equal a default value.
    * @param sAttrName The attribute name.
    * @param sAttrValue The attribute value.
    * @param sDefValue The default value to compare against.
    * @throws IOException if an error occurs.
    */
   public void writeAttribute(String sAttrName, String sAttrValue, String sDefValue) throws IOException
   {
      if (!ObjUtil.equal(sAttrValue, sDefValue))
      {
         m_writer.writeAttribute(sAttrName, sAttrValue);
      }
   }

   /**
    * Exports common xml child (non-root) elements.
    * @param channel The source channel.
    * @throws IOException if an error occurs.
    */
   public void exportChannelElements(Channel channel) throws IOException
   {
      if (channel.getReceiver() != null)
      {
         exportComponentProperties("ReceiverProperties", channel.getReceiver(), "server");
      }

      if (channel.getBindingCount() > 0)
      {
         m_writer.startElement("ServiceBindings");

         for (int i = 0; i < channel.getBindingCount(); ++i)
         {
            Binding binding = channel.getBinding(i);

            m_writer.openElement("ServiceBinding");
            m_writer.writeAttribute("service", binding.getService().getName());

            if (binding.getOutput() != null)
            {
               m_writer.writeAttribute("output", binding.getOutput().getName());
            }

            int nArgCount = binding.getService().getArgumentCount();

            if (nArgCount > 0)
            {
               m_writer.closeElement();
               m_writer.startElement("Arguments");

               for (int nArgIndex = 0; nArgIndex < nArgCount; ++nArgIndex)
               {
                  Variable arg = binding.getService().getArgument(nArgIndex);

                  Object value = binding.getArgumentValue(arg.getName());

                  if (value != Undefined.VALUE)
                  {
                     m_writer.openElement("Argument");
                     m_writer.writeAttribute("name", arg.getName());
                     exportSExpression(value, false, false, null, "value");
                     m_writer.closeEmptyElement();
                  }
               }

               m_writer.endElement("Arguments");
               m_writer.endElement("ServiceBinding");
            }
            else
            {
               m_writer.closeEmptyElement();
            }
         }

         m_writer.endElement("ServiceBindings");
      }
   }

   /**
    * Exports common channel attributes for the root element.
    * @param channel The source channel.
    * @throws IOException if an error occurs.
    */
   public void exportChannelAttributes(Channel channel) throws IOException
   {
      if (!channel.isSendable())
      {
         m_writer.writeAttribute("send", false);
      }

      if (!channel.isReceivable())
      {
         m_writer.writeAttribute("receive", false);
      }

      switch (channel.getCombinationMode())
      {
         case Channel.COMBINE_FIRST:
            m_writer.writeAttribute("combine", "first");
            break;

         case Channel.COMBINE_ALL:
            m_writer.writeAttribute("combine", "all");
            break;
      }
   }

   /**
    * Exports the specified component.
    * @param component The component to export
    * @throws IOException if an error occurs.
    */
   public void exportComponent(Component component) throws IOException
   {
      m_writer.openElement("Component");

      switch (component.getActivation())
      {
         case Component.SINGLETON:
            m_writer.writeAttribute("activation", "singleton");
            break;

         case Component.CONTEXT:
            m_writer.writeAttribute("activation", "context");
            break;

         case Component.NEW:
            m_writer.writeAttribute("activation", "new");
            break;
      }

      m_writer.writeAttribute("type", component.getType().getName());
      m_writer.closeElement();

      exportComponentProperties("Properties", component, null);

      m_writer.endElement("Component");
   }

   /**
    * Exports the properties for the specified component.
    * @param sElementName The XML element node name.
    * @param component The component whose properties to export
    * @param sInitializerName Optional initializer name to filter for.
    * @throws IOException if an error occurs.
    */
   public void exportComponentProperties(String sElementName, Component component, String sInitializerName) throws IOException
   {
      m_writer.startElement(sElementName);

      for (Iterator itr = component.getPropertyInitializerIterator(); itr.hasNext();)
      {
         PropertyInitializer initializer = (PropertyInitializer)itr.next();

         if (sInitializerName != null && !sInitializerName.equals(initializer.getName()))
         {
            continue;
         }

         boolean bCollection = initializer.isCollection();

         if (bCollection)
         {
            m_writer.openElement("Collection");
         }
         else
         {
            m_writer.openElement("Property");
         }

         m_writer.writeAttribute("name", initializer.getName());
         m_writer.closeElement();

         if (bCollection)
         {
            Iterator valueItr = (initializer instanceof ComponentCollectionPropertyInitializer) ? ((ComponentCollectionPropertyInitializer)initializer).getComponentIterator() :
               ((PrimitiveCollectionPropertyInitializer)initializer).getValueIterator();

            while (valueItr.hasNext())
            {
               m_writer.startElement("Item");
               exportComponentProperty(valueItr.next());
               m_writer.endElement("Item");
            }

            m_writer.endElement("Collection");
         }
         else
         {
            exportComponentProperty((initializer instanceof ComponentPropertyInitializer) ? ((ComponentPropertyInitializer)initializer).getInstanceComponent() :
               ((PrimitivePropertyInitializer)initializer).getValue());
            m_writer.endElement("Property");
         }
      }

      m_writer.endElement(sElementName);
   }

   /**
    * Exports a component property value.
    * @param value The property value.
    * @throws IOException if an error occurs.
    */
   protected void exportComponentProperty(Object value) throws IOException
   {
      if (value != null)
      {
         if (value instanceof Component)
         {
            Component c = (Component)value;

            if (c.getMetadata().findComponent(c.getName()) == null)
            {
               exportComponent(c);
            }
            else
            {
               m_writer.writeValue(c.getName());
            }
         }
         else
         {
            m_writer.writeValue(value.toString());
         }
      }
   }

   /**
    * Exports the specified step.
    * @param metaclass The step to export.
    * @throws IOException if an error occurs.
    */
   public void exportStep(Step step) throws IOException
   {
      if (step instanceof FlowMacroScript)
      {
         FlowMacroScript script = (FlowMacroScript)step;
         FlowMacro macro = script.getMacro();

         openStepElement(step, macro.getName());

         for (int i = 0, n = macro.getArgumentCount(); i < n; ++i)
         {
            Object value = script.getArgumentValue(i);

            if (value == Undefined.VALUE)
            {
               continue;
            }

            FlowMacro.Argument arg = macro.getArgument(i);

            if (arg.getType() == null || arg.getType() == Primitive.ANY)
            {
               exportSExpression(value, arg.getType() == null,
                  false, arg.getDefault(), arg.getName());
            }
            else
            {
               m_writer.writeAttribute(arg.getName(), (String)Primitive.STRING.convert(value));
            }
         }

         m_writer.closeEmptyElement();
      }
      else if (step instanceof Script)
      {
         openStepElement(step, "Script");
         m_writer.closeElement();

         Script script = (Script)step;

         exportSExpression(script.getBody(), true, true, Jump.BODY, null);

         m_writer.endElement("Script");
      }
      else if (step instanceof Transform)
      {
         openStepElement(step, "Transform");

         Transform transform = (Transform)step;

         exportSExpression(transform.getTransformationExpression(), false, false, null, "transformation");

         m_writer.closeEmptyElement();
      }
      else if (step instanceof Dispatch)
      {
         openStepElement(step, "Dispatch");
         m_writer.closeElement();

         exportDecision((Dispatch)step, new BranchExporter()
         {
            public String getElementName()
            {
               return "Case";
            }

            public void exportActivity(Activity activity) throws IOException
            {
               Case c = (Case)activity;

               if (c.getMessage() != null)
               {
                  m_writer.writeAttribute("message", c.getMessage().getName());
               }

               m_writer.closeElement();

               exportSequence(activity);
            }
         });

         m_writer.endElement("Dispatch");
      }
      else if (step instanceof TryCatch)
      {
         openStepElement(step, "TryCatch");

         exportTryCatch((TryCatch)step, new ActivityExporter()
         {
            public void exportActivity(Activity activity) throws IOException
            {
               m_writer.closeElement();
               exportSequence(activity);
            }
         });

         m_writer.endElement("TryCatch");
      }
      else if (step instanceof Sync)
      {
         openStepElement(step, "Sync");

         Sync sync = (Sync)step;

         exportSExpression(sync.getSyncLink(), false, false, null, "link");
         exportOnErrorAttribute(sync.getOnError());

         m_writer.closeElement();

         exportSExpression(sync.getSyncScript(), true, true, null, null);

         m_writer.endElement("Sync");
      }
      else if (step instanceof Persist)
      {
         openStepElement(step, "Persist");

         Persist persist = (Persist)step;

         if (persist.isRespond())
         {
            m_writer.writeAttribute("respond", true);
         }

         exportOnErrorAttribute(persist.getOnError());
         m_writer.closeEmptyElement();
      }
      else if (step instanceof SendReceive)
      {
         openStepElement(step, "SendReceive");

         SendReceive send = (SendReceive)step;

         exportSExpression(send.getOutputExpression(), false, false, Boolean.TRUE, "output");

         if (send.getInterface() != null)
         {
            m_writer.writeAttribute("interface", send.getInterface().getName());
         }

         m_writer.closeEmptyElement();
      }
      else if (step instanceof Send)
      {
         openStepElement(step, "Send");

         Send send = (Send)step;

         exportSExpression(send.getOutputExpression(), false, false, null, "output");

         if (send.getInterface() != null)
         {
            m_writer.writeAttribute("interface", send.getInterface().getName());
         }

         m_writer.closeEmptyElement();
      }
      else if (step instanceof Jump)
      {
         openStepElement(step, "Goto");

         m_writer.closeEmptyElement();
      }
      else
      {
         throw new MetadataException("err.meta.service.unsupportedExportOfStepClass",
            new Object[]{step.getClass().getName()});
      }
   }

   /**
    * Exports onError attribute if nOnError is not the default value.
    * @param nOnError The onError code.
    * @throws IOException if an error occurs.
    */
   private void exportOnErrorAttribute(byte nOnError) throws IOException
   {
      switch (nOnError)
      {
         case Persist.COLLECT_ON_ERROR:
            m_writer.writeAttribute("onError", "collect");
            break;

         case Persist.COMMIT_ON_ERROR:
            m_writer.writeAttribute("onError", "commit");
            break;
      }
   }

   /**
    * Exports a flow try-catch step. Implementation is reponsible for closing containing element.
    * @param tryCatch The try catch.
    * @param exporter The activity exporter.
    * @throws IOException if an error occurs.
    */
   public void exportTryCatch(TryCatch tryCatch, final ActivityExporter exporter) throws IOException
   {
      if (tryCatch.getLayout() != null)
      {
         m_writer.writeAttribute("layout", tryCatch.getLayout());
      }

      if (tryCatch.getExceptionVariable() != null)
      {
         m_writer.writeAttribute("variable", tryCatch.getExceptionVariable().getName());
      }

      m_writer.closeElement();

      m_writer.openElement("Try");
      exporter.exportActivity(tryCatch.getTry());
      m_writer.endElement("Try");

      exportDecision(tryCatch, new BranchExporter()
      {
         public String getElementName()
         {
            return "Catch";
         }

         public void exportActivity(Activity activity) throws IOException
         {
            Catch ctch = (Catch)activity;

            if (ctch.getException() != null)
            {
               m_writer.writeAttribute("exception", ctch.getException().getName());
            }

            if (ctch.getLayout() != null)
            {
               m_writer.writeAttribute("layout", ctch.getLayout());
            }

            exporter.exportActivity(activity);
         }
      });
   }

   /**
    * Exports the specified metaclass.
    * @param metaclass The metaclass to export.
    * @throws IOException if an error occurs.
    */
   public void exportMetaclass(Metaclass metaclass) throws IOException
   {
      m_writer.openElement("Class");

      exportSExpression(metaclass.getWhere(), false, false, null, "where");
      exportSExpression(metaclass.getValidation(), false, true, Undefined.VALUE, "validation");

      if (metaclass.getAspectOverrideCount() > 0)
      {
         m_writer.openAttribute("aspects");

         for (int i = 0; i < metaclass.getAspectOverrideCount(); ++i)
         {
            if (i > 0)
            {
               m_writer.write(' ');
            }

            if (!metaclass.isAspectOverrideInclusive(i))
            {
               m_writer.write('!');
            }

            m_writer.write(metaclass.getAspectOverride(i).getName());
         }

         m_writer.closeAttribute();
      }

      if (metaclass.getBase() != null && !metaclass.getBase().getName().equals(Metadata.ROOT_CLASS_NAME))
      {
         m_writer.writeAttribute("base", metaclass.getBase().getName());
      }

      m_writer.writeAttribute("visibility", (metaclass.getVisibility() == Metaclass.PROTECTED) ? "protected" : "public");

      if (metaclass.getDescription() != null)
      {
         m_writer.writeAttribute("description", metaclass.getDescription());
      }

      m_writer.closeElement();
      m_writer.startElement("Attributes");

      for (Iterator itr = metaclass.getInstanceAttributeIterator(); itr.hasNext();)
      {
         Attribute attribute = (Attribute)itr.next();

         if (attribute.getDeclarator() == metaclass)
         {
            exportAttribute(attribute);
         }
      }

      for (Iterator itr = metaclass.getStaticAttributeIterator(); itr.hasNext();)
      {
         Attribute attribute = (Attribute)itr.next();

         if (attribute.getDeclarator() == metaclass)
         {
            exportAttribute(attribute);
         }
      }

      m_writer.endElement("Attributes");

      m_writer.startElement("Events");

      for (Iterator itr = metaclass.getEventIterator(); itr.hasNext();)
      {
         Event event = (Event)itr.next();

         if (event.getDeclarator() == metaclass)
         {
            exportEvent(event);
         }
      }

      m_writer.endElement("Events");

      if (metaclass.getPersistenceMapping() != null)
      {
         exportPersistenceMapping(metaclass.getPersistenceMapping());
      }

      m_writer.endElement("Class");
   }

   /**
    * Exports the specified attribute.
    * @param attribute The attribute to export.
    * @throws IOException if an error occurs.
    */
   public void exportAttribute(Attribute attribute) throws IOException
   {
      m_writer.openElement("Attribute");
      m_writer.writeAttribute("name", attribute.getName());
      m_writer.writeAttribute("visibility", (attribute.getVisibility() == Metaclass.PROTECTED) ? "protected" : "public");

      String sTypeName = "";
      Type type = attribute.getType();

      if (type != null)
      {
         sTypeName = type.getName();
      }

      m_writer.writeAttribute("type", sTypeName);

      if (attribute.isRequired())
      {
         m_writer.writeAttribute("required", true);
      }

      if (attribute.isCollection())
      {
         m_writer.writeAttribute("collection", true);
      }

      if (attribute.isCompatible())
      {
         m_writer.writeAttribute("compatible", true);
      }

      if (attribute.isStatic())
      {
         m_writer.writeAttribute("static", true);
      }

      if (attribute.isReadOnly())
      {
         m_writer.writeAttribute("readOnly", true);
      }

      if (attribute.getReverse() != null)
      {
         m_writer.writeAttribute("reverse", attribute.getReverse().getName());
      }

      if (attribute.getDescription() != null)
      {
         m_writer.writeAttribute("description", attribute.getDescription());
      }

      exportSExpression(attribute.getValue(), false, false, Undefined.VALUE, "value");
      exportSExpression(attribute.getInitializer(), false, true, Undefined.VALUE, "initializer");
      exportSExpression(attribute.getValidation(), false, true, Undefined.VALUE, "validation");
      exportSExpression(attribute.getWhere(), false, false, null, "where");

      switch (attribute.getCascadeMode())
      {
         case Attribute.CASCADE_CANCEL:
            m_writer.writeAttribute("cascade", "cancel");
            break;

         case Attribute.CASCADE_CLEAR:
            m_writer.writeAttribute("cascade", "clear");
            break;

         case Attribute.CASCADE_DELETE:
            m_writer.writeAttribute("cascade", "delete");
            break;
      }

      m_writer.closeEmptyElement();
   }

   /**
    * Exports the specified event.
    * @param event The event to export.
    * @throws IOException if an error occurs.
    */
   public void exportEvent(Event event) throws IOException
   {
      m_writer.openElement("Event");
      m_writer.writeAttribute("name", event.getName());

      if (event.isCompatible())
      {
         m_writer.writeAttribute("compatible", true);
      }

      m_writer.writeAttribute("visibility", (event.getVisibility() == Metaclass.PUBLIC) ? "public" : "protected");
      m_writer.writeAttribute("static", event.isStatic());

      StringBuffer buf = new StringBuffer();

      for (Iterator itr = event.getArgumentIterator(); itr.hasNext();)
      {
         buf.append(((Argument)itr.next()).getName());

         if (itr.hasNext())
         {
            buf.append(' ');
         }
      }

      if (buf.length() != 0)
      {
         m_writer.writeAttribute("args", buf.toString());
      }

      if (event.isVarArg())
      {
         m_writer.writeAttribute("vararg", true);
      }

      if (event.getPrivilege() != null)
      {
         m_writer.writeAttribute("privilege", event.getPrivilege().getName());
      }

      if (event.getAccessAttribute() != null)
      {
         m_writer.writeAttribute("access", event.getAccessAttribute().getName());
      }

      if (event.getDescription() != null)
      {
         m_writer.writeAttribute("description", event.getDescription());
      }

      m_writer.closeElement();

      if (event.getActionCount() != 0)
      {
         m_writer.startElement("Actions");

         for (Iterator itr = event.getActionIterator(); itr.hasNext();)
         {
            Action action = (Action)itr.next();

            if (action.getDeclarator() == event.getDeclarator())
            {
               exportAction(action);
            }
         }

         m_writer.endElement("Actions");
      }

      m_writer.endElement("Event");
   }

   /**
    * Exports the specified action.
    * @param action The action to export.
    * @throws IOException if an error occurs.
    */
   public void exportAction(Action action) throws IOException
   {
      m_writer.openElement("Action");
      m_writer.writeAttribute("name", action.getName());

      String sType = null;

      switch (action.getType())
      {
         case Action.MAIN:
            sType = "main";
            break;

         case Action.BEFORE:
            sType = "before";
            break;

         case Action.AFTER:
            sType = "after";
            break;

         case Action.AROUND:
            sType = "around";
            break;
      }

      m_writer.writeAttribute("type", sType);

      if (action.getNextAction() != null)
      {
         m_writer.writeAttribute("relative", action.getNextAction().getName());
      }

      if (action.getCondition() != null)
      {
         m_writer.writeAttribute("condition", Intrinsic.toString(action.getCondition()));
      }

      if (action.getMethod() != null)
      {
         Method method = action.getMethod();

         m_writer.writeAttribute("method", method.getClass().getName(), ".", method.getName());
      }

      m_writer.closeElement();

      exportSExpression(action.getBody(), true, true, null, null);

      m_writer.endElement("Action");
   }

   /**
    * Exports the specified persistence mapping.
    * @param mapping The mapping to export.
    * @throws IOException if an error occurs.
    */
   public void exportPersistenceMapping(PersistenceMapping mapping) throws IOException
   {
      m_writer.openElement("PersistenceMapping");
      m_writer.writeAttribute("dataSource", mapping.getDataSource().getName());

      if (mapping.getLockingAttribute() != null)
      {
         m_writer.writeAttribute("lockingAttribute", mapping.getLockingAttribute().getName());
      }

      if (mapping.getTypeCodeAttribute() != null)
      {
         m_writer.writeAttribute("classCodeAttribute", mapping.getTypeCodeAttribute().getName());
      }

      if (mapping.isTypeCodeForced())
      {
         m_writer.writeAttribute("classCodeForced", true);
      }

      if (mapping.getFragmentReplication() != PersistenceMapping.REPLICATION_NONE)
      {
         m_writer.writeAttribute("fragmentReplication",
            (mapping.getFragmentReplication() == PersistenceMapping.REPLICATION_UNICAST) ? "unicast" : "broadcast");
      }

      if (mapping.getFragmentAttribute() != null)
      {
         m_writer.writeAttribute("fragmentAttribute", mapping.getFragmentAttribute().getName());
      }

      if (mapping.getCaching() != PersistenceMapping.CACHING_NONE)
      {
         m_writer.writeAttribute("caching",
            (mapping.getCaching() == PersistenceMapping.CACHING_INSTANCE) ? "instance" : "class");
      }

      m_writer.closeElement();

      getPersistenceMetadataExporter(mapping.getDataSource()).exportMapping(mapping);
      m_writer.endElement("PersistenceMapping");
   }

   /**
    * Exports the J2EE descriptors for all the data sources in the metadata.
    * @param metadata The root metadata object.
    * @param nPart The part number, one of the J2EE_* constants.
    * @param sNamespace The part namespace infix. Cannot be null.
    * @param nContainer The container number, one of the J2EEUtil.* constants.
    * @param nContext The export context, one of the J2EE_CONTEXT_* constants.
    * @throws IOException if an error occurs.
    */
   public void exportJ2EEDescriptor(Metadata metadata, int nPart, String sNamespace, int nContainer, int nContext) throws IOException
   {
      switch (nPart)
      {
         case J2EE_DISTRIBUTABLE:
            if (metadata.isDistributed() && metadata.isReplicatedSession())
            {
               m_writer.openElement("distributable");
               m_writer.closeEmptyElement();
            }

            break;

         case J2EE_CONFIDENTIAL:
            if (metadata.isSecureTransport() || nContext == J2EE_CONTEXT_WEB_CERT)
            {
               m_writer.startElement("user-data-constraint");
               m_writer.writeElement("transport-guarantee",
                  (metadata.isSecureTransport()) ? "CONFIDENTIAL" : "INTEGRAL");
               m_writer.endElement("user-data-constraint");
            }

            break;

         case J2EE_SESSION_TIMEOUT:
            m_writer.writeInt(metadata.getSessionTimeout());
            break;

         case J2EE_CONTEXT_ROOT:
            if ("anon".equals(sNamespace))
            {
               m_writer.writeValue(metadata.getHTTPAnonymousContextRoot());
            }
            else if ("form".equals(sNamespace))
            {
               m_writer.writeValue(metadata.getHTTPFormContextRoot());
            }
            else if ("push".equals(sNamespace))
            {
               m_writer.writeValue(metadata.getHTTPPushRedirectorContextRoot());
            }
            else
            {
               m_writer.writeValue(metadata.getHTTPContextRoot());
            }

            break;

         case J2EE_WEB_LOGIN_CONFIG:
            if ("form".equals(sNamespace))
            {
               m_writer.writeElement("auth-method", "FORM");
               m_writer.startElement("form-login-config");
               m_writer.writeElement("form-login-page", metadata.getHTTPFormLoginPage());
               m_writer.writeElement("form-error-page", metadata.getHTTPFormErrorPage());
               m_writer.endElement("form-login-config");
            }
            else
            {
               m_writer.startElement("auth-method");
               m_writer.writeValue(
                  (metadata.getAuthenticationProtocol() == Metadata.AUTH_PROTOCOL_CERTIFICATE)
                  ? "CLIENT-CERT" : "BASIC"
               );
               m_writer.endElement("auth-method");

               if (metadata.getAuthenticationProtocol() != Metadata.AUTH_PROTOCOL_CERTIFICATE)
               {
                  m_writer.startElement("realm-name");

                  if (metadata.getAuthenticationDomain() != null)
                  {
                     m_writer.writeValue(metadata.getAuthenticationDomain());
                  }
                  else
                  {
                     m_writer.writeValue(SysUtil.NAMESPACE.toUpperCase(Locale.ENGLISH));
                  }

                  m_writer.endElement("realm-name");
               }
            }

            break;

         case J2EE_TEST_FILTER:
            if (metadata.isTestInterceptor())
            {
               m_writer.startElement("filter");
               m_writer.writeElement("display-name", "TestFilter");
               m_writer.writeElement("filter-name", "TestFilter");
               m_writer.writeElement("filter-class", SysUtil.PACKAGE, ".core.testing.rpc.http.TestFilter");
               m_writer.endElement("filter");
            }

            break;

         case J2EE_TEST_FILTER_MAPPING:
            if (metadata.isTestInterceptor())
            {
               m_writer.startElement("filter-mapping");
               m_writer.writeElement("filter-name", "TestFilter");
               m_writer.writeElement("url-pattern", "/*");
               m_writer.endElement("filter-mapping");
            }

            break;

         case J2EE_PERSISTENCE_FILTER:
            if (metadata.isPersistentSession())
            {
               m_writer.startElement("filter");
               m_writer.writeElement("display-name", "PersistenceFilter");
               m_writer.writeElement("filter-name", "PersistenceFilter");
               m_writer.writeElement("filter-class", SysUtil.PACKAGE, ".core.rpc.http.session.PersistenceFilter");
               m_writer.endElement("filter");
            }

            break;

         case J2EE_PERSISTENCE_FILTER_MAPPING:
            if (metadata.isPersistentSession())
            {
               m_writer.startElement("filter-mapping");
               m_writer.writeElement("filter-name", "PersistenceFilter");
               m_writer.writeElement("url-pattern", "/*");
               m_writer.endElement("filter-mapping");
            }

            break;

         case J2EE_SESSION_LISTENER:
            if (metadata.isPersistentSession())
            {
               m_writer.startElement("listener");
               m_writer.writeElement("listener-class", SysUtil.PACKAGE, ".core.rpc.http.session.SessionListener");
               m_writer.endElement("listener");
            }

            break;

         case J2EE_PORTLETS:
            exportPortlets((XMLMetadata)metadata, nContainer);
            break;

         case J2EE_PLATFORM_REPLICATION_CONFIG:
            if (metadata.isDistributed() && metadata.isReplicatedSession())
            {
               switch (nContainer)
               {
                  case J2EEUtil.JBOSS:
                     m_writer.startElement("replication-config");
                     m_writer.writeElement("replication-trigger", "SET");
                     m_writer.writeElement("replication-type", "SYNC");
                     m_writer.endElement("replication-config");

                     break;
               }
            }

            break;

         case J2EE_PLATFORM_CLUSTER_CONFIG:
            if (metadata.isDistributed())
            {
               switch (nContainer)
               {
                  case J2EEUtil.JBOSS:
                     m_writer.writeElement("clustered", "True");
                     m_writer.startElement("cluster-config");
                     m_writer.writeElement("partition-name", "${jboss.partition.name:DefaultPartition}");
                     m_writer.writeElement("home-load-balance-policy", "org.jboss.ha.framework.interfaces.FirstAvailable");
                     m_writer.writeElement("bean-load-balance-policy", "org.jboss.ha.framework.interfaces.FirstAvailable");
                     m_writer.endElement("cluster-config");

                     break;
               }
            }

            break;

         case J2EE_PLATFORM_PERSISTENCE_INTERCEPTOR:
            if (SysUtil.ENTERPRISE)
            {
               switch (nContainer)
               {
                  case J2EEUtil.JBOSS:
                     m_writer.openElement("Valve");
                     m_writer.writeAttribute("className", SysUtil.PACKAGE, ".core.container.platform.jboss.JBossPersistenceValve");
                     m_writer.writeAttribute("enabled", metadata.isPersistentSession());
                     m_writer.closeEmptyElement();

                     break;
               }
            }

            break;

         case J2EE_PLATFORM_AUTH_INTERCEPTOR:
            switch (nContainer)
            {
               case J2EEUtil.JBOSS:
                  m_writer.openElement("Valve");
                  m_writer.writeAttribute("className", SysUtil.PACKAGE, ".core.container.platform.jboss.JBossAuthenticationValve");

                  if (metadata.getAuthenticationProtocol() == Metadata.AUTH_PROTOCOL_SPNEGO)
                  {
                     if (metadata.getAuthenticationDomain() != null)
                     {
                        m_writer.writeAttribute("domain", metadata.getAuthenticationDomain());
                     }

                     if (metadata.getAuthenticationService() != null)
                     {
                        m_writer.writeAttribute("service", metadata.getAuthenticationService());
                     }
                  }

                  m_writer.writeAttribute("sessionTimeout", metadata.getSessionTimeout());
                  m_writer.closeEmptyElement();

                  break;
            }

            break;

         case J2EE_PLATFORM_PORTLETS:
            exportPortlets((XMLMetadata)metadata, nContainer);
            break;

         case J2EE_PLATFORM_CLASSPATH:
            for (Iterator extLibIter = metadata.getExternalLibraryIterator(); extLibIter.hasNext();)
            {
               ExternalLibrary extLib = (ExternalLibrary)extLibIter.next();

               for (Iterator fileIter = extLib.getFileIterator(); fileIter.hasNext();)
               {
                  String sFile = (String)fileIter.next();

                  switch (nContainer)
                  {
                     case J2EEUtil.WEBSPHERE:
                        m_writer.startElement("classpath");
                        m_writer.writeValue("$(APP_INSTALL_ROOT)/$(CELL)/" + SysUtil.NAMESPACE + '-' + metadata.getEnvironment() + ".ear/" + sFile);
                        m_writer.endElement("classpath");

                        break;
                  }
               }
            }

            break;
      }

      DataSource[] dataSourceArray = new DataSource[metadata.getDataSourceCount()];
      int i = 0;

      for (Iterator itr = metadata.getDataSourceIterator(); itr.hasNext(); ++i)
      {
         dataSourceArray[i] = (DataSource)itr.next();
      }

      Arrays.sort(dataSourceArray, Named.COMPARATOR);

      for (i = 0; i < dataSourceArray.length; ++i)
      {
         DataSource dataSource = dataSourceArray[i];

         if (dataSource.getType().getExporter() != null)
         {
            getPersistenceMetadataExporter(dataSource)
               .exportJ2EEDescriptor(dataSource, nPart, sNamespace, nContainer, nContext);
         }
      }

      Channel[] channelArray = new Channel[metadata.getChannelCount()];

      i = 0;

      for (Iterator itr = metadata.getChannelIterator(); itr.hasNext(); ++i)
      {
         channelArray[i] = (Channel)itr.next();
      }

      Arrays.sort(channelArray, Named.COMPARATOR);

      for (i = 0; i < channelArray.length; ++i)
      {
         Channel channel = channelArray[i];

         if (channel.getType().getExporter() != null)
         {
            getIntegrationMetadataExporter(channel)
               .exportJ2EEDescriptor(channel, nPart, sNamespace, nContainer, nContext);
         }
      }
   }
   /**
    * Exports a J2EE environment reference.
    * @param res The environment reference.
    * @param nPart The part number, one of the J2EE_* constants.
    * @param sNamespace The part namespace infix. Cannot be null.
    * @param nContainer The container number, one of the J2EEUtil.* constants.
    * @param nContext The export context, one of the J2EE_CONTEXT_* constants.
    * @throws IOException if an error occurs.
    */
   public void exportJ2EEEnvRef(J2EEEnvRef ref, int nPart, String sNamespace, int nContainer, int nContext) throws IOException
   {
      String sId = sNamespace + ((sNamespace.length() == 0) ? "" : "-") + INVALID_ID_CHARACTERS.matcher(ref.getName()).replaceAll("-");

      switch (nPart)
      {
         case J2EE_RESOURCE_ENV_REF:
            m_writer.openElement("resource-env-ref");
            m_writer.writeAttribute("id", "resourceEnvRef-", sId);
            m_writer.closeElement();

            m_writer.writeElement("resource-env-ref-name", ref.getName());
            m_writer.writeElement("resource-env-ref-type", ref.getClassName());

            m_writer.endElement("resource-env-ref");

            break;

         case J2EE_PLATFORM_RESOURCE_ENV_REF:
            switch (nContainer)
            {
            case J2EEUtil.TEEE:
               m_writer.openElement("resource-env-ref");
               m_writer.writeAttribute("name", ref.getName());
               m_writer.writeAttribute("jndi-name", ref.getJNDIName());
               m_writer.closeEmptyElement();
               break;

            case J2EEUtil.JBOSS:
               m_writer.startElement("resource-env-ref");

               m_writer.writeElement("resource-env-ref-name", ref.getName());
               m_writer.writeElement("jndi-name", ref.getJNDIName());

               m_writer.endElement("resource-env-ref");

               break;

            case J2EEUtil.WEBSPHERE:
               m_writer.openElement("resourceEnvRefBindings");
               m_writer.writeAttribute("jndiName", ref.getJNDIName());
               m_writer.closeElement();

               m_writer.openElement("bindingResourceEnvRef");
               m_writer.writeAttribute("href",
                  (nContext == J2EE_CONTEXT_EJB) ? "META-INF/ejb-jar.xml" : "WEB-INF/web.xml",
                  "#resourceEnvRef-", sId);
               m_writer.closeEmptyElement();

               m_writer.endElement("resourceEnvRefBindings");

               break;
            }

            break;

         default:
            throw new IllegalArgumentException();
      }
   }

   /**
    * Exports a J2EE resource reference.
    * @param metadata The root metadata object.
    * @param ref The resource reference to export.
    * @param nPart The part number, one of the J2EE_* constants.
    * @param sNamespace The part namespace infix. Cannot be null.
    * @param nContainer The container number, one of the J2EEUtil.* constants.
    * @param nContext The export context, one of the J2EE_CONTEXT_* constants.
    * @throws IOException if an error occurs.
    */
   public void exportJ2EEResourceRef(Metadata metadata, J2EEResourceRef ref, int nPart, String sNamespace, int nContainer, int nContext) throws IOException
   {
      String sId = sNamespace + ((sNamespace.length() == 0) ? "" : "-") + INVALID_ID_CHARACTERS.matcher(ref.getName()).replaceAll("-");

      switch (nPart)
      {
         case J2EE_RESOURCE_REF:
            m_writer.openElement("resource-ref");
            m_writer.writeAttribute("id", "resourceRef-" + sId);
            m_writer.closeElement();

            m_writer.writeElement("res-ref-name", ref.getName());
            m_writer.writeElement("res-type", ref.getClassName());
            m_writer.writeElement("res-auth", (ref.getAuthAlias() == null) ? "Application" : "Container");
            m_writer.writeElement("res-sharing-scope", (ref.isShareable()) ? "Shareable" : "Unshareable");

            m_writer.endElement("resource-ref");

            break;

         case J2EE_PLATFORM_RESOURCE_REF:
            switch (nContainer)
            {
               case J2EEUtil.TEEE:
                  m_writer.openElement("resource-ref");
                  m_writer.writeAttribute("name", ref.getName());
                  m_writer.writeAttribute("jndi-name", ref.getJNDIName());
                  m_writer.closeEmptyElement();

                  break;

               case J2EEUtil.JBOSS:
                  m_writer.startElement("resource-ref");

                  m_writer.writeElement("res-ref-name", ref.getName());
                  m_writer.writeElement("jndi-name",
                     (ref.getJNDIName().indexOf(':') < 0) ? "java:/" : "", ref.getJNDIName());

                  m_writer.endElement("resource-ref");

                  break;

               case J2EEUtil.WEBSPHERE:
                  m_writer.openElement("resRefBindings");
                  m_writer.writeAttribute("jndiName", ref.getJNDIName());

                  if (ref.getAuthAlias() != null)
                  {
                     m_writer.writeAttribute("loginConfigurationName", "DefaultPrincipalMapping");
                  }

                  m_writer.closeElement();

                  m_writer.openElement("bindingResourceRef");
                  m_writer.writeAttribute("href",
                     (nContext == J2EE_CONTEXT_EJB) ? "META-INF/ejb-jar.xml" : "WEB-INF/web.xml",
                     "#resourceRef-", sId);
                  m_writer.closeEmptyElement();

                  if (ref.getAuthAlias() != null)
                  {
                     m_writer.openElement("properties");
                     m_writer.writeAttribute("name", "com.ibm.mapping.authDataAlias");
                     m_writer.writeAttribute("value", ref.getAuthAlias());
                     m_writer.closeEmptyElement();
                  }

                  m_writer.endElement("resRefBindings");

                  break;
            }

            break;

         case J2EE_PLATFORM_RESOURCE_REF_EXT:
            switch (nContainer)
            {
               case J2EEUtil.WEBSPHERE:
                  if (ref.getIsolationLevel() != Connection.TRANSACTION_NONE)
                  {
                     m_writer.openElement("resourceRefExtensions");

                     switch (ref.getIsolationLevel())
                     {
                        case Connection.TRANSACTION_READ_UNCOMMITTED:
                           m_writer.writeAttribute("isolationLevel", "TRANSACTION_READ_UNCOMMITTED");
                           break;

                        case Connection.TRANSACTION_READ_COMMITTED:
                           m_writer.writeAttribute("isolationLevel", "TRANSACTION_READ_COMMITTED");
                           break;

                        case Connection.TRANSACTION_REPEATABLE_READ:
                           m_writer.writeAttribute("isolationLevel", "TRANSACTION_REPEATABLE_READ");
                           break;

                        case Connection.TRANSACTION_SERIALIZABLE:
                           m_writer.writeAttribute("isolationLevel", "TRANSACTION_SERIALIZABLE");
                           break;
                     }

                     m_writer.closeElement();

                     m_writer.openElement("resourceRef");
                     m_writer.writeAttribute("href",
                        (nContext == J2EE_CONTEXT_EJB) ? "META-INF/ejb-jar.xml" : "WEB-INF/web.xml",
                        "#resourceRef-", sId);
                     m_writer.closeEmptyElement();

                     m_writer.endElement("resourceRefExtensions");
                  }

                  break;
            }

            break;

         case J2EE_PLATFORM_CONNECTION_FACTORY:
            switch (nContainer)
            {
               case J2EEUtil.TEEE:
                  m_writer.openElement("factory");
                  m_writer.writeAttribute("jndi-name", ref.getJNDIName());

                  /*
                   * Force same connection to get reused throughout an XA transaction
                   * for SHAREABLE resources.
                   */
                  if (ref.getTxMode() == J2EEResourceRef.TX_XA && ref.isShareable())
                  {
                     m_writer.writeAttribute("tx-associated", "true"); // same as JBoss below
                  }

                  m_writer.closeElement();

                  for (int i = 0, n = ref.getPropertyCount(); i < n; ++i)
                  {
                     J2EEProperty property = ref.getProperty(i);

                     if (property.getValue() != null)
                     {
                        m_writer.openElement("config-property");
                        m_writer.writeAttribute("name", property.getName());
                        m_writer.writeAttribute("value", property.getValue());
                        m_writer.closeEmptyElement();
                     }
                  }

                  m_writer.endElement("factory");

                  break;

               case J2EEUtil.JBOSS:
                  m_writer.startElement((ref.getTxMode() == J2EEResourceRef.TX_NONE) ?
                     "no-tx-connection-factory" : "tx-connection-factory");

                  m_writer.writeElement("jndi-name", ref.getJNDIName());

                  switch (ref.getTxMode())
                  {
                     case J2EEResourceRef.TX_LOCAL:
                        m_writer.openElement("local-transaction");
                        m_writer.closeEmptyElement();
                        break;

                     case J2EEResourceRef.TX_XA:
                        m_writer.openElement("xa-transaction");
                        m_writer.closeEmptyElement();
                        break;
                  }

                  /*
                   * Force same connection to get reused throughout an XA transaction
                   * for SHAREABLE resources.
                   */
                  if (ref.getTxMode() == J2EEResourceRef.TX_XA && ref.isShareable())
                  {
                     m_writer.openElement("track-connection-by-tx");
                     m_writer.closeEmptyElement();
                  }

                  m_writer.writeElement("rar-name", SysUtil.NAMESPACE, "-", metadata.getEnvironment(), ".ear#", ref.getResourceAdapterName());
                  m_writer.writeElement("connection-definition", ref.getClassName());

                  for (int i = 0, n = ref.getPropertyCount(); i < n; ++i)
                  {
                     J2EEProperty property = ref.getProperty(i);

                     if (property.getValue() != null)
                     {
                        m_writer.openElement("config-property");
                        m_writer.writeAttribute("name", property.getName());
                        m_writer.writeAttribute("type", property.getTypeName());
                        m_writer.closeElement();

                        m_writer.writeValue(property.getValue());

                        m_writer.endElement("config-property");
                     }
                  }

                  if (ref.isConnectionPoolPartitioned())
                  {
                     m_writer.openElement("application-managed-security");
                     m_writer.closeEmptyElement();
                  }

                  if (ref.getAuthAlias() != null)
                  {
                     m_writer.writeElement("security-domain-and-application", ref.getAuthAlias());
                  }

                  m_writer.writeElement("max-pool-size", Integer.toString(ref.getMaxConnections()));
                  m_writer.endElement((ref.getTxMode() == J2EEResourceRef.TX_NONE) ?
                     "no-tx-connection-factory" : "tx-connection-factory");

                  break;

               case J2EEUtil.WEBSPHERE:
                  m_writer.openElement("factories");
                  m_writer.writeAttribute("xmi:type", "resources.j2c:J2CConnectionFactory");
                  m_writer.writeAttribute("name", sId);
                  m_writer.writeAttribute("jndiName", ref.getJNDIName());
                  m_writer.writeAttribute("logMissingTransactionContext", "false");
                  m_writer.writeAttribute("connectionDefinition", "connectionDefinition-", sNamespace);
                  m_writer.closeElement();

                  m_writer.startElement("propertySet");

                  for (int i = 0, n = ref.getPropertyCount(); i < n; ++i)
                  {
                     J2EEProperty property = ref.getProperty(i);

                     m_writer.openElement("resourceProperties");
                     m_writer.writeAttribute("name", property.getName());
                     m_writer.writeAttribute("type", property.getTypeName());
                     m_writer.writeAttribute("value", property.getValue());
                     m_writer.closeEmptyElement();
                  }

                  m_writer.endElement("propertySet");

                  m_writer.openElement("connectionPool");
                  m_writer.writeAttribute("minConnections", 1);
                  m_writer.writeAttribute("maxConnections", ref.getMaxConnections());
                  m_writer.closeEmptyElement();

                  m_writer.endElement("factories");

                  break;
            }

            break;

         case J2EE_PLATFORM_ADMIN_OBJECT:
            switch (nContainer)
            {
               case J2EEUtil.TEEE:
                  m_writer.openElement("admin-object");
                  m_writer.writeAttribute("jndi-name", ref.getJNDIName());
                  m_writer.writeAttribute("interface-name", ref.getClassName());
                  m_writer.closeElement();

                  for (int i = 0, n = ref.getPropertyCount(); i < n; ++i)
                  {
                     J2EEProperty property = ref.getProperty(i);

                     if (property.getValue() != null)
                     {
                        m_writer.openElement("config-property");
                        m_writer.writeAttribute("name", property.getName());
                        m_writer.writeAttribute("value", property.getValue());
                        m_writer.closeEmptyElement();
                     }
                  }

                  m_writer.endElement("admin-object");

                  break;

               case J2EEUtil.JBOSS:
                  m_writer.openElement("mbean");
                  m_writer.writeAttribute("code", "org.jboss.resource.deployment.AdminObject");
                  m_writer.writeAttribute("name", SysUtil.NAMESPACE + ".destination:name=" + ref.getJNDIName());
                  m_writer.closeElement();

                  m_writer.openElement("attribute");
                  m_writer.writeAttribute("name", "JNDIName");
                  m_writer.closeElement();
                  m_writer.writeValue(ref.getJNDIName());
                  m_writer.endElement("attribute");

                  m_writer.openElement("attribute");
                  m_writer.writeAttribute("name", "RARName");
                  m_writer.closeElement();
                  m_writer.writeValue("jboss.jca:service=RARDeployment,name='" + SysUtil.NAMESPACE + '-' +
                     metadata.getEnvironment() + ".ear#" + ref.getResourceAdapterName() + "'");
                  m_writer.endElement("attribute");

                  m_writer.openElement("attribute");
                  m_writer.writeAttribute("name", "Type");
                  m_writer.closeElement();
                  m_writer.writeValue(ref.getClassName());
                  m_writer.endElement("attribute");

                  Properties adminObjProps = new Properties();

                  for (Iterator iter = ref.getPropertyIterator(); iter.hasNext();)
                  {
                     J2EEProperty j2eeProp = (J2EEProperty)iter.next();
                     adminObjProps.setProperty(j2eeProp.getName(), j2eeProp.getValue());
                  }

                  m_writer.openElement("attribute");
                  m_writer.writeAttribute("name", "Properties");
                  m_writer.closeElement();
                  m_writer.writeValue(PropertyUtil.toString(adminObjProps));
                  m_writer.endElement("attribute");

                  m_writer.endElement("mbean");

                  break;

               case J2EEUtil.WEBSPHERE:
                  m_writer.openElement("j2cAdminObjects");
                  m_writer.writeAttribute("name", sId);
                  m_writer.writeAttribute("jndiName", ref.getJNDIName());
                  m_writer.writeAttribute("adminObject", "adminObject-", sNamespace);
                  m_writer.closeElement();

                  for (Iterator iter = ref.getPropertyIterator(); iter.hasNext();)
                  {
                     J2EEProperty j2eeProp = (J2EEProperty)iter.next();

                     m_writer.openElement("properties");
                     m_writer.writeAttribute("name", j2eeProp.getName());
                     m_writer.writeAttribute("type", j2eeProp.getTypeName());
                     m_writer.writeAttribute("value", j2eeProp.getValue());
                     m_writer.closeEmptyElement();
                  }

                  m_writer.endElement("j2cAdminObjects");

                  break;
            }

            break;

         default:
            throw new IllegalArgumentException();
      }
   }
   /**
    * Exports a J2EE activation property.
    * @param property The activation property to export.
    * @param nContainer The container number, one of the J2EEUtil.* constants.
    * @param nContext The export context, one of the J2EE_CONTEXT_* constants.
    * @throws IOException if an error occurs.
    */
   public void exportJ2EEActivationProperty(J2EEProperty property, int nContainer, int nContext) throws IOException
   {
      if (property != null && property.getValue() != null)
      {
         if (nContext == J2EE_CONTEXT_APP)
         {
            switch (nContainer)
            {
               case J2EEUtil.WEBSPHERE:
                  m_writer.openElement("resourceProperties");
                  m_writer.writeAttribute("name", property.getName());
                  m_writer.writeAttribute("type", property.getTypeName());
                  m_writer.writeAttribute("value", property.getValue());
                  m_writer.closeEmptyElement();

                  break;
            }
         }
         else
         {
            m_writer.startElement("activation-config-property");
            m_writer.writeElement("activation-config-property-name", property.getName());
            m_writer.writeElement("activation-config-property-value", property.getValue());
            m_writer.endElement("activation-config-property");
         }
      }
   }

   /**
    * Gets a persistence metadata exporter for a given data source.
    * @param dataSource The data source.
    * @return The exporter.
    * @throws IOException if an error occurs.
    */
   protected XMLPersistenceMetadataExporter getPersistenceMetadataExporter(DataSource dataSource) throws IOException
   {
      try
      {
         return (XMLPersistenceMetadataExporter)dataSource.getType().getExporter()
            .getConstructor(new Class[]{XMLMetadataExporter.class})
            .newInstance(new Object[]{this});
      }
      catch (Exception e)
      {
         throw new IOException("Cannot instantiate the persistence metadata exporter \"" +
            dataSource.getType().getExporter().getName() + "\"");
      }
   }

   /**
    * Gets an integration metadata exporter for a given data source.
    * @param dataSource The data source.
    * @return The exporter.
    * @throws IOException if an error occurs.
    */
   protected XMLIntegrationMetadataExporter getIntegrationMetadataExporter(Channel channel) throws IOException
   {
      try
      {
         return (XMLIntegrationMetadataExporter)channel.getType().getExporter()
            .getConstructor(new Class[]{XMLMetadataExporter.class})
            .newInstance(new Object[]{this});
      }
      catch (Exception e)
      {
         throw new IOException("Cannot instantiate the integration metadata exporter \"" +
            channel.getType().getExporter().getName() + "\"");
      }
   }

   /**
    * Gets a message mapping exporter for a given format.
    * @param format The format.
    * @return The exporter.
    * @throws IOException if an error occurs.
    */
   protected XMLMessageMappingExporter getMessageMappingExporter(Format format) throws IOException
   {
      try
      {
         return (XMLMessageMappingExporter)format.getExporter()
            .getConstructor(new Class[]{XMLMetadataExporter.class})
            .newInstance(new Object[]{this});
      }
      catch (Exception e)
      {
         throw new IOException("Cannot instantiate the message mapping exporter \"" +
            format.getExporter().getName() + "\"");
      }
   }

   /**
    * Exports the specified message.
    * @param mapping The message to export.
    * @throws IOException if an error occurs.
    */
   public void exportMessage(Message message) throws IOException
   {
      XMLMessageMappingExporter mappingExporter = null;

      if (message.getFormat() != null)
      {
         mappingExporter = getMessageMappingExporter(message.getFormat());
      }

      exportCompositeMessagePart(message, message.getRoot(), mappingExporter);
   }

   /**
    * Exports a composite message.
    * @param rootMsg The root message associated with the message, if any.
    * @param message The message.
    * @param export The message mapping exporter.
    * @throws IOException if an error occurs.
    */
   protected void exportCompositeMessagePart(Message rootMsg, CompositeMessagePart message, XMLMessageMappingExporter exporter) throws IOException
   {
      m_writer.openElement("Message");

      if (rootMsg != null)
      {
         if (rootMsg.getFormat() != null)
         {
            m_writer.writeAttribute("format", rootMsg.getFormat().getName());
         }

         if (rootMsg.getResponse() != null)
         {
            m_writer.writeAttribute("response", rootMsg.getResponse().getName());
         }

         if (rootMsg.getBaseMessage() != null)
         {
            m_writer.writeAttribute("base", rootMsg.getBaseMessage().getName());
         }

         if (message.getDescription() != null)
         {
            m_writer.writeAttribute("description", message.getDescription());
         }
      }
      else
      {
         exportMessagePartAttributes(message);
      }

      if (message instanceof CompositeMessagePartRef)
      {
         m_writer.writeAttribute("ref",
            ((CompositeMessagePartRef)message).getRefPart().getName());

         if (rootMsg != null)
         {
            m_writer.closeEmptyElement();
         }
         else
         {
            m_writer.closeElement();
            exportMessagePartMapping(rootMsg, message, exporter);
            m_writer.endElement("Message");
         }
      }
      else
      {
         String sAggregation;

         switch (message.getAggregation())
         {
            case CompositeMessagePart.RANDOM:
               sAggregation = "random";
               break;

            case CompositeMessagePart.SINGLE:
               sAggregation = "single";
               break;

            default:
               sAggregation = "sequential";
               break;
         }

         m_writer.writeAttribute("aggregation", sAggregation);

         if (message.isLax())
         {
            m_writer.writeAttribute("lax", true);
         }

         m_writer.closeElement();
         exportMessagePartMapping(rootMsg, message, exporter);

         if (message.getPartCount() > 0)
         {
            m_writer.startElement("Parts");

            for (Iterator iter = message.getPartIterator(); iter.hasNext();)
            {
               MessagePart part = (MessagePart)iter.next();

               if (part instanceof CompositeMessagePart)
               {
                  exportCompositeMessagePart(null, (CompositeMessagePart)part, exporter);
               }
               else if (part instanceof PrimitiveMessagePart)
               {
                  exportPrimitiveMessagePart((PrimitiveMessagePart)part, exporter);
               }
            }

            m_writer.endElement("Parts");
         }

         m_writer.endElement("Message");
      }
   }

   /**
    * Exports a primitive message.
    * @param message The message.
    * @param export The message mapping exporter.
    * @throws IOException if an error occurs.
    */
   protected void exportPrimitiveMessagePart(PrimitiveMessagePart message, XMLMessageMappingExporter exporter) throws IOException
   {
      m_writer.openElement("Value");

      if (message.getType() != null)
      {
         m_writer.writeAttribute("type", message.getType().getName());
      }

      exportMessagePartAttributes(message);

      m_writer.closeElement();

      exportMessagePartMapping(null, message, exporter);

      if (message.getEnumerationCount() > 0)
      {
         m_writer.startElement("Enumerations");

         for (Iterator iter = message.getEnumerationIterator(); iter.hasNext();)
         {
            m_writer.openElement("Enumeration");

            Object enumValue = iter.next();
            m_writer.writeAttribute("value", Primitive.toString(enumValue));
            m_writer.closeEmptyElement();
         }

         m_writer.endElement("Enumerations");
      }

      m_writer.endElement("Value");
   }

   /**
    * Exports the message mapping using an exporter
    * @param rootMsg The root message associated with message if any
    * @param message The message with a mapping.
    * @param exporter The exporter.
    * @throws IOException if an error occurs.
    */
   protected void exportMessagePartMapping(Message rootMsg, MessagePart message, XMLMessageMappingExporter exporter) throws IOException
   {
      if (exporter != null)
      {
         exporter.exportMapping(rootMsg, message.getMapping());
      }
   }

   /**
    * Exports common attributes from a message part.
    * @param message The message part.
    * @throws IOException if an error occurs
    */
   protected void exportMessagePartAttributes(MessagePart messagePart) throws IOException
   {
      m_writer.writeAttribute("name", messagePart.getName());
      m_writer.writeAttribute("minCount", Integer.toString(messagePart.getMinCount()));
      m_writer.writeAttribute("maxCount", Integer.toString(
         (messagePart.getMaxCount() == Integer.MAX_VALUE) ? 0 : messagePart.getMaxCount()));

      if (messagePart.getDescription() != null)
      {
         m_writer.writeAttribute("description", messagePart.getDescription());
      }
   }

   /**
    * Gets the aspect override expression as a string.
    * @param pointcut The pointcut containing the aspect overrides.
    * @return The aspect override expression.
    */
   public static String getAspectOverrides(Pointcut pointcut)
   {
      StringBuffer buf = new StringBuffer();

      for (int i = 0, n = pointcut.getAspectOverrideCount(); i < n; ++i)
      {
         if (i != 0)
         {
            buf.append(' ');
         }

         if (!pointcut.isAspectOverrideInclusive(i))
         {
            buf.append('!');
         }

         buf.append(pointcut.getAspectOverride(i).getName());
      }

      return (buf.length() == 0) ? null : buf.toString();
   }

   /**
    * Gets the pointcut expression as a string.
    * @param aspect The aspect specifying the pointcuts.
    * @return The pointcut expression.
    */
   public static String getPointcuts(Aspect aspect)
   {
      StringBuffer buf = new StringBuffer();

      for (int i = 0, n = aspect.getPointcutPatternCount(); i < n; ++i)
      {
         if (i != 0)
         {
            buf.append(' ');
         }

         if (!aspect.isPointcutPatternInclusive(i))
         {
            buf.append('!');
         }

         buf.append(aspect.getPointcutPattern(i));
      }

      return (buf.length() == 0) ? null : buf.toString();
   }

   /**
    * Iterates UI metadata and exports portlet descriptor.
    * @param metadata XMLMetadata object.
    * @param nContainer The container, one of the J2EEUtil.* constants.
    * @throws IOException if an error occurs.
    */
   protected void exportPortlets(XMLMetadata metadata, int nContainer) throws IOException
   { // Portlets not supported
   }

   /**
    * Interface implemented by activity exporter strategies.
    */
   public interface ActivityExporter
   {
      /**
       * Exports an activity. The container element has been opened but not closed. Method is responsible for closing element.
       * @param activity The activity to export.
       * @throws IOException if an error occurs.
       */
      void exportActivity(Activity activity) throws IOException;
   }

   /**
    * Interface implemented by flow decision exporter strategies.
    */
   public interface BranchExporter extends ActivityExporter
   {
      /**
       * @return The branch element name.
       * @throws IOException if an error occurs.
       */
      String getElementName() throws IOException;
   }

   /**
    * Interface for filtering variables.
    */
   public interface VariableFilter
   {
      /**
       * @param variable The variable to be tested.
       * @return true to include the variable.
       */
      boolean accept(Variable variable);
   }
}