// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.xml;

import nexj.core.util.HashTab2D;
import nexj.core.util.Lookup2D;

/**
 * XML message parser table.
 */
public class XMLMessageParserTable
{
   // attributes

   /**
    * The envelope type, a bit-mask of the RootXMLMessagePartMapping.ENVELOPE_* flags.
    */
   protected byte m_nAcceptedEnvelopes;

   /**
    * True if the parse will need to be restarted after the proper message is
    * detected.
    */
   protected boolean m_bRestartable;

   // associations

   /**
    * The message map: Message[sNamespace, sElement].
    */
   protected Lookup2D m_messageMap = new HashTab2D();

   // operations

   /**
    * Sets the accepted envelopes.
    * @param nMask A bit mask of accepted envelopes, use the
    * RootXMLMessagePartMapping.ENVELOPE_* constants.
    */
   public void setAcceptedEnvelopes(byte nMask)
   {
      m_nAcceptedEnvelopes = nMask;
   }

   /**
    * Gets the accepted envelopes.
    * @return A bit mask of accepted envelopes, use the
    * RootXMLMessagePartMapping.ENVELOPE_* constants.
    */
   public byte getAcceptedEnvelopes()
   {
      return m_nAcceptedEnvelopes;
   }

   /**
    * Sets the restartable flag.
    * @param bRestartable True if the parse will need to be restarted after
    *                     the proper message is detected.
    */
   public void setRestartable(boolean bRestartable)
   {
      m_bRestartable = bRestartable;
   }

   /**
    * Gets the restartable flag.
    * @return True if the parse will need to be restarted after the proper
    *         message is detected.
    */
   public boolean isRestartable()
   {
      return m_bRestartable;
   }

   /**
    * @return The message map.
    */
   public Lookup2D getMessageMap()
   {
      return m_messageMap;
   }
}
