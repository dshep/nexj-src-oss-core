// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.service;

import nexj.core.integration.Input;
import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageParser;
import nexj.core.integration.Responder;
import nexj.core.integration.Sender;
import nexj.core.integration.io.ObjectInput;
import nexj.core.meta.Component;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.MessageTable;
import nexj.core.meta.workflow.Activity;
import nexj.core.meta.workflow.State;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;

/**
 * Message send-receive step.
 */
public class SendReceive extends Send
{
   // constructors

   /**
    * Constructs the step.
    * @param sName The step name.
    * @param activity The containing activity.
    */
   public SendReceive(String sName, Activity activity)
   {
      super(sName, activity);
   }

   // operations

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
               throw new IllegalArgumentException("Unspecified channel for SendReceive");
            }

            Channel channel = context.getMetadata().getChannel(sChannelName);

            context.getUnitOfWork().checkTransaction();

            if (channel.isSynchronous())
            {
               Component sender = channel.getSender();

               if (sender == null)
               {
                  throw new RPCException("err.rpc.notSender", new Object[]{channel.getName()});
               }

               Responder responder = (Responder)sender.getInstance(context);

               tobj = parse(responder.respond(format(tobj, (Sender)responder, context)), context);
            }
            else
            {
               // TODO: Implement message correlation and async send-receive
               throw new UnsupportedOperationException("Async send-receive not supported yet");
            }

            machine.returnValue(tobj, nArgCount);

            return false;
         }
      };
   }

   /**
    * Parses the response, if necessary.
    * @param tobj The transfer object with a body message part.
    * @param context The invocation context.
    * @return The parsed response.
    */
   protected TransferObject parse(TransferObject tobj, InvocationContext context) throws IntegrationException
   {
      Object body = tobj.findValue(Sender.BODY);

      if (m_interface == null)
      {
         if (body instanceof Input)
         {
            tobj.setValue(Sender.BODY, ((Input)body).getObject());
         }

         return tobj;
      }

      MessageTable table = m_interface.getResponseTable();
      Format format = table.getFormat();

      if (format == null)
      {
         return tobj;
      }

      Input input;

      if (body instanceof Input)
      {
         input = (Input)body;
      }
      else
      {
         input = new ObjectInput(body);
      }

      return ((MessageParser)format.getParser().getInstance(context)).parse(input, table);
   }
}
