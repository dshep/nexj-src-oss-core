// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.service;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.MessageParser;
import nexj.core.integration.format.object.ObjectMessageFormatter;
import nexj.core.integration.io.ObjectInput;
import nexj.core.integration.io.ObjectOutput;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.workflow.FunctionStep;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * Object persistence step.
 */
public class Persist extends FunctionStep
{
   public final static byte FAIL_ON_ERROR = 1;
   public final static byte COLLECT_ON_ERROR = 2;
   public final static byte COMMIT_ON_ERROR = 3;

   // attributes

   /**
    * The response flag.
    */
   protected boolean m_bRespond;

   /**
    * The exception handling strategy.
    */
   protected byte m_nOnError = FAIL_ON_ERROR;

   // constructors

   /**
    * Constructs the step.
    * @param sName The step name.
    * @param nOnError The exception handling strategy.
    */
   public Persist(String sName, byte nOnError)
   {
      super(sName);

      m_nOnError = nOnError;
   }

   // operations

   /**
    * Sets the response flag.
    * @param bRespond The response flag to set.
    */
   public void setRespond(boolean bRespond)
   {
      verifyNotReadOnly();
      m_bRespond = bRespond;
   }

   /**
    * @return The response flag.
    */
   public boolean isRespond()
   {
      return m_bRespond;
   }

   /**
    * Sets the exception handling strategy.
    * @param nOnError The exception handling strategy to set.
    */
   public void setOnError(byte nOnError)
   {
      verifyNotReadOnly();
      m_nOnError = nOnError;
   }

   /**
    * @return The exception handling strategy.
    */
   public byte getOnError()
   {
      return m_nOnError;
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
            Object cleanupToken = null;
            Logger logger = null;

            try
            {
               assert nArgCount == Pair.length(getArguments());

               InvocationContext context = (InvocationContext)machine.getContext();
               TransferObject tobj = (TransferObject)machine.getArg(0, nArgCount);
               Message message = context.getMetadata().getMessage(tobj.getClassName());
               Format format = message.getFormat();

               if (format == null || !format.getName().equals("Object"))
               {
                  throw new IntegrationException("err.integration.persistFormat",
                     new Object[]{message.getName(), (format == null) ? "" : message.getFormat().getName()});
               }

               context.getUnitOfWork().checkTransaction();

               ObjectOutput out = new ObjectOutput();
               MessageFormatter messageFormatter = (MessageFormatter)format.getFormatter().getInstance(context);

               cleanupToken = configureFormatter(messageFormatter, nArgCount, machine);

               if (messageFormatter instanceof ObjectMessageFormatter)
               {
                  ObjectMessageFormatter objMsgFmt = (ObjectMessageFormatter)messageFormatter;

                  logger = objMsgFmt.getLinkLogger();

                  if (logger != null && logger.isDumpEnabled())
                  {
                     logger.log(Logger.DUMP, "ids.sync.collectionFormattingStarted",
                        new Object[] {message.getName()}, null);
                  }
               }

               if (m_nOnError == FAIL_ON_ERROR)
               {
                  messageFormatter.format(tobj, message, out);
               }
               else
               {
                  ((ObjectMessageFormatter)messageFormatter).formatFailSafe(tobj, message, out, m_nOnError == COMMIT_ON_ERROR);
               }

               if (logger != null && logger.isDumpEnabled())
               {
                  logger.log(Logger.DUMP, "ids.sync.collectionFormattingCompleted",
                     new Object[] {message.getName()}, null);
               }

               Object obj = out.getObject();

               if (m_bRespond)
               {
                  Message response = message.getResponse();

                  if (response == null)
                  {
                     tobj = null;
                  }
                  else
                  {
                     tobj = ((MessageParser)format.getParser().getInstance(context))
                        .parse(new ObjectInput(obj), response);
                  }
               }

               machine.returnValue(tobj, nArgCount);

               return false;
            }
            catch (Throwable t)
            {
               if (logger != null)
               {
                  logger.log(Logger.DEBUG, "err.sync.nonRecoverableFormatting",
                     new Object[] {((TransferObject)machine.getArg(0, nArgCount)).getClassName()}, t);
               }

               ObjUtil.rethrow(t);

               return m_bRespond;
            }
            finally
            {
               cleanUp(machine, cleanupToken);
            }
         }
      };
   }

   /**
    * Extension point to allow additional configuration of the formatter
    * @param messageFormatter a message formatter to be configured
    * @param argCount number of arguments
    * @param machine Scheme virtual machine
    * @return a cleanup token that will be passed to the cleanUp method on completion.
    */
   protected Object configureFormatter(MessageFormatter messageFormatter, int argCount, Machine machine)
   {
      return null; // No-op here, but can be overridden
   }

   /**
    * Extension point to allow additional cleanup
    * @param machine Scheme virtual machine
    * @param cleanupToken a value returned by the configureFormatter
    */
   protected void cleanUp(Machine machine, Object cleanupToken)
   {
      // No-op here, but can be overridden
   }
}
