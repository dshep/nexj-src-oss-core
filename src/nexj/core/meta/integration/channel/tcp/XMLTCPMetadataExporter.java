// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.tcp;

import java.util.List;

import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter;
import nexj.core.meta.j2ee.J2EEProperty;
import nexj.core.meta.j2ee.J2EEResourceRef;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.util.CertificateUtil;
import nexj.core.util.SysUtil;

/**
 * XML TCP metadata exporter.
 */
public class XMLTCPMetadataExporter extends XMLMDBIntegrationMetadataExporter
{
   // constructors

	/**
	 * Creates the exporter with an XML print stream.
	 * @param exporter The XML metadata exporter.
	 */
	public XMLTCPMetadataExporter(XMLMetadataExporter exporter)
	{
		super(exporter);
	}

	// operations

	/**
	 * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addActivationProperties(nexj.core.meta.integration.Channel, java.util.List)
	 */
	protected void addActivationProperties(Channel channel, List list)
	{
	   super.addActivationProperties(channel, list);

      TCPChannel tcp = (TCPChannel)channel;

      list.add(new J2EEProperty("localHost", tcp.getLocalHost()));
      list.add(new J2EEProperty("localPort", tcp.getLocalPort()));
      list.add(new J2EEProperty("backlog", tcp.getBacklog()));
      list.add(new J2EEProperty("readTimeout", tcp.getReadTimeout()));
      list.add(new J2EEProperty("idleTimeout", tcp.getIdleTimeout()));
      list.add(new J2EEProperty("keepAlive", tcp.isKeepAlive()));
      list.add(new J2EEProperty("noDelay", tcp.isNoDelay()));
      list.add(new J2EEProperty("maxPoolSize", tcp.getMaxReceivers()));
      list.add(new J2EEProperty("senderBufferSize", tcp.getSenderBufferSize()));
      list.add(new J2EEProperty("receiverBufferSize", tcp.getReceiverBufferSize()));
      list.add(new J2EEProperty("secure", tcp.isSecure()));

      if (tcp.isSecure())
      {
         list.add(new J2EEProperty("authentication", tcp.getClientAuthMode()));
         list.add(new J2EEProperty("password", tcp.getPassword()));
         list.add(new J2EEProperty("certificate", CertificateUtil.formatKeyStore(tcp.getCertificateStore(), tcp.getPassword())));
         list.add(new J2EEProperty("trust", CertificateUtil.formatCertificate(tcp.getTrustedCertificate())));
      }
	}

	/**
	 * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addAdminObjectRefs(nexj.core.meta.integration.Channel, int, java.util.List)
	 */
	protected void addAdminObjectRefs(Channel channel, int nContainer, List list)
	{
	}

	/**
	 * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addConnectionRefs(nexj.core.meta.integration.Channel, int, java.util.List)
	 */
	protected void addConnectionRefs(Channel channel, int nContainer, List list)
	{
		addResourceRefs(channel, nContainer, list);
	}

	/**
	 * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addPlatformActivationProperties(nexj.core.meta.integration.Channel, int, java.util.List)
	 */
	protected void addPlatformActivationProperties(Channel channel, int nContainer, List list)
	{
	}

	/**
	 * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addResourceEnvRefs(nexj.core.meta.integration.Channel, int, java.util.List)
	 */
	protected void addResourceEnvRefs(Channel channel, int nContainer, List list)
	{
	}

	/**
	 * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addResourceRefs(nexj.core.meta.integration.Channel, int, java.util.List)
	 */
	protected void addResourceRefs(Channel channel, int nContainer, List list)
	{
		if (channel.isSendable())
		{
			J2EEResourceRef ref = new J2EEResourceRef("tcp/" + channel.getName(),
					SysUtil.NAMESPACE + '/' + getEnvironmentName(channel) + "/tcp/" + channel.getName(),
					SysUtil.PACKAGE + ".core.rpc.tcp.TCPConnectionFactory");

			// Non-transactional connections cannot be shareable
			ref.setShareable(false);
			ref.setConnectionPoolPartitioned(true);
			ref.setResourceAdapterName(getResourceAdapterName(channel));
			ref.setMaxConnections(((TCPChannel)channel).getMaxSenders());
			list.add(ref);
		}
	}

	/**
	 * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getListenerClass(nexj.core.meta.integration.Channel)
	 */
	protected String getListenerClass(Channel channel)
	{
		return SysUtil.PACKAGE + ".core.rpc.tcp.TCPListener";
	}

	/**
	 * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getMDBClass(nexj.core.meta.integration.Channel)
	 */
	protected String getMDBClass(Channel channel)
	{
		return SysUtil.PACKAGE + ".core.rpc.tcp.TCPServerMDB";
	}

	/**
	 * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getResourceAdapterName(nexj.core.meta.integration.Channel)
	 */
	protected String getResourceAdapterName(Channel channel)
	{
		return SysUtil.NAMESPACE + "-tcp.rar";
	}

	/**
	 * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getTransactionType(nexj.core.meta.integration.Channel)
	 */
	protected String getTransactionType(Channel channel)
	{
		return "Bean";
	}
}
