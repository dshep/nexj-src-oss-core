// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.service;

import nexj.core.integration.DelayedOutput;
import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.Output;
import nexj.core.integration.Sender;
import nexj.core.integration.DelayedOutput.Action;
import nexj.core.integration.io.ObjectOutput;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessageTable;
import nexj.core.meta.workflow.Activity;
import nexj.core.meta.workflow.ValueExpression;
import nexj.core.meta.workflow.FunctionStep;
import nexj.core.meta.workflow.State;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Logger;

/**
 * Message sending step.
 */
public class Send extends FunctionStep
{
   // constants

   /**
    * The send step arguments.
    */
   protected final static Pair ARGUMENTS = new ConstPair(Symbol.THIS, new ConstPair(Symbol._STATE));

   /**
    * Arguments required to evaluate the send channel
    */
   private final static Pair CHANNEL_ARGUMENTS = new ConstPair(Symbol.THIS);

   // associations

   /**
    * The Scheme expression that evaluates to the output channel name
    */
   protected ValueExpression m_channel;

   /**
    * The output service interface.
    */
   protected Interface m_interface;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(Send.class);

   // constructors

   /**
    * Constructs the step.
    * @param sName The step name.
    * @param activity The containing activity.
    */
   public Send(String sName, Activity activity)
   {
      super(sName);

      setActivity(activity);
   }

   // operations

   /**
    * Sets the Scheme expression that evaluates to the output channel name.
    * @param outputExpression The Scheme expression that evaluates to the output channel name to set.
    */
   public void setOutputExpression(Object expression)
   {
      verifyNotReadOnly();
      m_channel = new ValueExpression(expression);
   }

   /**
    * @return The Scheme expression that evaluates to the output channel name.
    */
   public Object getOutputExpression()
   {
      return (m_channel != null) ? m_channel.getValue() : null;
   }

   /**
    * Sets the output service interface.
    * @param interface The output service interface to set.
    */
   public void setInterface(Interface iface)
   {
      verifyNotReadOnly();
      m_interface = iface;
   }

   /**
    * @return The output service interface.
    */
   public Interface getInterface()
   {
      return m_interface;
   }

   /**
    * @see nexj.core.meta.workflow.Step#generate(nexj.core.scripting.Machine)
    */
   public void generate(Machine machine)
   {
      super.generate(machine);

      if (m_channel != null)
      {
         m_channel.compile(this, machine, CHANNEL_ARGUMENTS);
      }
   }

   /**
    * @see nexj.core.meta.workflow.FunctionStep#getArguments()
    */
   protected Pair getArguments()
   {
      return ARGUMENTS;
   }

   /**
    * @see nexj.core.meta.workflow.FunctionStep#getFunction()
    */
   protected Function getFunction()
   {
      return new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            assert nArgCount == 2;

            InvocationContext context = (InvocationContext)machine.getContext();
            TransferObject tobj = (TransferObject)machine.getArg(0, nArgCount);
            State state = (State)machine.getArg(1, nArgCount);
            String sChannelName = (m_channel != null) ? (String)m_channel.invoke(machine, state, new Object[]{tobj}) : null;

            if (sChannelName == null)
            {
               sChannelName = (String)tobj.findValue(Sender.CHANNEL);
            }

            if (sChannelName == null)
            {
               throw new IllegalArgumentException("Unspecified channel for Send");
            }

            Channel channel = context.getMetadata().getChannel(sChannelName);

            context.getUnitOfWork().addMessage(channel, format(tobj, (Sender)channel.getSender().getInstance(context), context));
            machine.returnValue(tobj, nArgCount);

            return false;
         }
      };
   }

   /**
    * Validates and formats the request, if necessary.
    * @param tobj The transfer object.
    * @param channel The output channel.
    * @param context The invocation context.
    * @return The formatted request (in the body message part).
    */
   protected TransferObject format(TransferObject tobj, Sender sender, InvocationContext context) throws IntegrationException
   {
      if (m_interface == null)
      {
         return tobj;
      }

      MessageTable table = m_interface.getRequestTable();

      if (table.getMessageCount() == 0 && table.getFormat() == null)
      {
         return tobj;
      }

      Message message = table.findMessage(tobj.getClassName());

      if (message == null)
      {
         throw new IntegrationException("err.integration.service.invalidRequest",
            new Object[]{tobj.getClassName(), getName()});
      }

      if (message.getFormat() == null)
      {
         return tobj;
      }

      ObjectOutput output = sender.createOutput();
      TransferObject raw = new TransferObject(1);

      formatToOutput(tobj, message, context, output);
      raw.setValue(Sender.BODY, output.getObject());
      sender.prepare(raw, tobj, message);

      return raw;
   }

   /**
    * Format a raw message to the specified output.
    * @param tobj The message to format.
    * @param message The Message object.
    * @param context The invocation context.
    * @param output The Output object that the message will be formatted to.
    */
   public static void formatToOutput(final TransferObject tobj, final Message message, InvocationContext context, Output output)
   {
      final MessageFormatter formatter = (MessageFormatter)message.getFormat().getFormatter().getInstance(context);

      if (output instanceof DelayedOutput)
      {
         ((DelayedOutput)output).setAction(new Action()
         {
            public void run(Output output)
            {
               formatter.format(tobj, message, output);
            }
         });
      }
      else
      {
         formatter.format(tobj, message, output);
      }
   }
}
