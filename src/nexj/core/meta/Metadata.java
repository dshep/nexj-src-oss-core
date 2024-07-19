// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.security.Principal;
import java.security.cert.Certificate;
import java.util.Iterator;
import java.util.Locale;
import java.util.Properties;

import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.ChannelType;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.Transformation;
import nexj.core.meta.integration.service.Interface;
import nexj.core.meta.integration.service.Service;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.DataSourceType;
import nexj.core.meta.testing.unit.UnitTest;
import nexj.core.meta.ui.ClassMetaMap;
import nexj.core.meta.upgrade.Upgrade;
import nexj.core.meta.workflow.FlowMacro;
import nexj.core.meta.workflow.Workflow;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;

/**
 * The interface of the root metadata object.
 */
public interface Metadata extends ContextMetadata, ClassMetaMap
{
   // constants

   /**
    * Name scope separator.
    */
   public final static char SCOPE_SEP = ':';

   /**
    * The inheritance hierarchy root class name.
    */
   public final static String ROOT_CLASS_NAME = "Object";

   /**
    * The enumeration base class name.
    */
   public final static String ENUMERATION_CLASS_NAME = "EnumBase";

   /**
    * The service class name.
    */
   public final static String SERVICE_CLASS_NAME = "SysService";

   /**
    * The synchronization class class name.
    */
   public final static String SYNC_CLASS_CLASS_NAME = "SysSyncClass";

   /**
    * The synchronization target class name.
    */
   public final static String SYNC_TARGET_CLASS_NAME = "SysSyncTarget";

   /**
    * The synchronization link class name.
    */
   public final static String SYNC_LINK_CLASS_NAME = "SysSyncLink";

   /**
    * The synchronization linkClass class name.
    */
   public final static String SYNC_LINK_CLASS_CLASS_NAME = "SysSyncLinkClass";

   /**
    * The root synchronization object map class name.
    */
   public final static String SYNC_OBJECT_CLASS_NAME = "SysRootSyncObject";

   /**
    * The child synchronization object map class name.
    */
   public final static String CHILD_SYNC_OBJECT_CLASS_NAME = "SysChildSyncObject";

   /**
    * The synchronization group class name.
    */
   public final static String SYNC_GROUP_CLASS_NAME = "SysSyncGroup";

   /**
    * The workflow class name.
    */
   public final static String WORKFLOW_CLASS_NAME = "SysWorkflow";

   /**
    * The workflow assignment class name.
    */
   public final static String WORKFLOW_ASSIGNMENT_CLASS_NAME = "SysWorkflowAssignment";

   /**
    * The audit log class name.
    */
   public final static String AUDIT_LOG_CLASS_NAME = "SysObjectLog";

   /**
    * The version class name.
    */
   public final static String VERSION_CLASS_NAME = "SysVersion";

   /**
    * The deployment class name.
    */
   public final static String DEPLOYMENT_CLASS_NAME = "SysDeployment";

   /**
    * The deployment label class name (the class that points to the current deployment).
    */
   public final static String DEPLOYMENT_LABEL_CLASS_NAME = "SysDeploymentLabel";

   /**
    * The model class name.
    */
   public final static String MODEL_CLASS_NAME = "SysModel";

   /**
    * The supported locale class name.
    */
   public final static String LOCALE_CLASS_NAME = "SupportedLocaleEnum";

   /**
    * Basic authentication protocol: user/password.
    */
   public final static int AUTH_PROTOCOL_BASIC = 0;

   /**
    * GSSAPI SPNEGO authentication protocol: Kerberos, NTLM or Basic.
    */
   public final static int AUTH_PROTOCOL_SPNEGO = 1;

   /**
    * Client certificate authentication protocol.
    */
   public final static int AUTH_PROTOCOL_CERTIFICATE = 2;
   
   /**
    * Perimeter authentication protocol: generic SSO implementation.
    */
   public final static int AUTH_PROTOCOL_PERIMETER = 3;

   // operations

   /**
    * @return True if association paths (@ ...) support collections.
    */
   boolean isCollectionPathEnabled();

   /**
    * @return True if anonymous access to the flat page client is enabled.
    */
   boolean isAnonymousFlatPageEnabled();

   /**
    * @return True if the test interceptor is installed.
    */
   boolean isTestInterceptor();

   /**
    * @return The authentication protocol, one of the AUTH_PROTOCOL_* constants.
    */
   int getAuthenticationProtocol();

   /**
    * @return The authentication service name (e.g. the Kerberos principal).
    */
   String getAuthenticationService();

   /**
    * @return The authentication domain or realm.
    */
   String getAuthenticationDomain();

   /**
    * @return The authentication properties.
    */
   Properties getAuthenticationProperties();

   /**
    * @return True if generic RPC calls are allowed; false to deny access to generic RPC.
    */
   boolean isGenericRPCAllowed();

   /**
    * @return The generic RPC privilege. Null means unrestricted.
    */
   PrimitivePrivilege getGenericRPCPrivilege();

   /**
    * @return The user principal under which anonymous HTTP requests will be processed.
    */
   Principal getAnonymousUser();

   /**
    * @return True if anonymous access to the HTTP/text, HTTP/soap, and HTTP/xml RPC protocols is enabled.
    */
   boolean isAnonymousRPCEnabled();

   /**
    * Gets the client certificate to trust for HTTP requests.
    *
    * @return The trusted certificate.
    */
   Certificate getTrustedCertificate();

   /**
    * Gets the HTTP context root (the path at which the NexJ web applications
    * will be made available).
    *
    * @return The HTTP context root.
    */
   String getHTTPContextRoot();

   /**
    * Gets the anonymous HTTP context root (the path at which the NexJ web applications
    * will be made available anonymously)
    *
    * @return The anonymous HTTP context root.
    */
   String getHTTPAnonymousContextRoot();

   /**
    * Gets the form-based-authentication HTTP context root (the path at which the NexJ
    * applications will be made available through form-based sign on)
    *
    * @return The form-based-authentication HTTP context root.
    */
   String getHTTPFormContextRoot();
   
   /**
    * Gets the push redirector HTTP context root.
    * @return The push redirector HTTP context root.
    */
   String getHTTPPushRedirectorContextRoot();

   /**
    * Gets the login page for form-based sign on.
    *
    * @return The path from the root of the WAR.
    */
   String getHTTPFormLoginPage();

   /**
    * Gets the login error page for form-based sign on.
    *
    * @return The path from the root of the WAR.
    */
   String getHTTPFormErrorPage();

   /**
    * Gets the HTTP root (the full URI path to the NexJ web applications, as specified
    * in the "httpURL" property in the .server metadata file)
    *
    * @return The HTTP root ("httpURL" property from the server metadata).
    */
   String getHTTPRoot();

   /**
    * @return The session timeout.
    */
   int getSessionTimeout();

   /**
    * @return True if the application is working in a cluster.
    */
   boolean isDistributed();

   /**
    * @return True, if max receivers property on message queues can be set dynamically.
    */
   boolean isDynamicReceiverEnabled();

   /**
    * @return True if session replication is required.
    */
   boolean isReplicatedSession();

   /**
    * @return True if session persistence is required.
    */
   boolean isPersistentSession();

   /**
    * @return True if connection integrity and confidentiality is required.
    */
   boolean isSecureTransport();

   /**
    * @return True if it is a test environment.
    */
   boolean isTestEnvironment();

   /**
    * @return True if PK validation between RelationalDatabase and Main.upgrade is enabled.
    */
   boolean isPrimaryKeyUpgradeValidationEnabled();

   /**
    * Gets the SSL key store password.
    *
    * @return The key store password.
    */
   String getKeyStorePassword();

   /**
    * Gets the default data decryption/encryption password.
    * @return The default data encryption password to use.
    */
   public String getDataPassword();

   /**
    * Adds a new data source type object to the metadata.
    * @param dstype The data source type object to add.
    * @throws MetadataException if a data source type with the same name already exists.
    */
   void addDataSourceType(DataSourceType dstype);

   /**
    * Gets a data source type object by name.
    * @param sName The name of the data source type object.
    * @return The data source type object.
    * @throws MetadataLookupException if the named data source type object does not exist.
    */
   DataSourceType getDataSourceType(String sName);

   /**
    * @return An iterator for the contained data source type objects.
    */
   Iterator/*<DataSourceType>*/ getDataSourceTypeIterator();

   /**
    * Adds a new data source to the metadata.
    * @param ds The data source to add.
    * @throws MetadataException if a data source with the same name already exists.
    */
   void addDataSource(DataSource ds);

   /**
    * Gets a data source by name.
    * @param sName The name of the data source.
    * @return The data source object.
    * @throws MetadataLookupException if the named data source does not exist.
    */
   DataSource getDataSource(String sName);

   /**
    * Finds a data source by name.
    * @param sName The name of the data source.
    * @return The data source object, or null if not found.
    */
   DataSource findDataSource(String sName);

   /**
    * @return The data source count.
    */
   int getDataSourceCount();

   /**
    * @return A data source iterator.
    */
   Iterator getDataSourceIterator();

   /**
    * @return A data source fragment iterator.
    */
   Iterator getDataSourceFragmentIterator();

   /**
    * Gets the highest-security encryption scheme used to encrypt
    * connection and server metadata.
    * @return One of the SCHEME_* Strings from CharacterStreamCipherDispatcher.
    */
   String getEncryptionScheme();

   /**
    * Sets the highest-security encryption scheme used to encrypt
    * connection and server metadata.
    * @param sScheme One of the SCHEME_* Strings from CharacterStreamCipherDispatcher.
    */
   void setEncryptionScheme(String sScheme);

   /**
    * Flag indicating whether or not the passwords in this metadata object
    * are encrypted.
    *
    * @return True if the passwords are encrypted; false if they were
    * decrypted when the metadata were loaded.
    */
   boolean isEncrypted();

   /**
    * Adds a new privilege object to the metadata.
    * @param privilege The privilege object to add.
    * @throws MetadataException if a privilege
    * with the same name already exists.
    */
   void addPrivilege(Privilege privilege);

   /**
    * Gets a privilege object by name.
    * @param sName The name of the privilege.
    * @return The privilege object.
    * @throws MetadataLookupException if the named
    * privilege does not exist.
    */
   Privilege getPrivilege(String sName);

   /**
    * Finds a privilege object by name.
    * @param sName The name of the privilege.
    * @return The privilege object, or null if not found.
    */
   Privilege findPrivilege(String sName);

   /**
    * Gets a primitive privilege object by name.
    * @param sName The name of the privilege.
    * @return The privilege object.
    * @throws MetadataLookupException if the named
    * privilege does not exist or is not a primitive privilege.
    */
   PrimitivePrivilege getPrimitivePrivilege(String sName);

   /**
    * @return The count of the primitive privileges.
    */
   int getPrimitivePrivilegeCount();

   /**
    * @return An iterator for the privilege objects.
    */
   Iterator getPrivilegeIterator();

   /**
    * @return An empty privilege set.
    */
   PrivilegeSet createPrivilegeSet();

   /**
    * @return System property delta by which to shift all RA ports.
    */
   int getPortOffset();

   /**
    * Adds a public symbol.
    * @param symbol The symbol to make public.
    */
   void addPublicSymbol(Symbol symbol);

   /**
    * Checks if a symbol is public.
    * @param symbol The symbol to check.
    * @return True is the symbol is public.
    */
   boolean isPublicSymbol(Symbol symbol);

   /**
    * Adds a client library symbol.
    * @param symbol The symbol to export to the client.
    */
   void addClientSymbol(Symbol symbol);

   /**
    * Removes a client library symbol.
    * @param symbol The symbol to remove.
    */
   void removeClientSymbol(Symbol symbol);

   /**
    * Checks if a symbol is a client library symbol.
    * @param symbol The symbol to check.
    * @return True if it is a client library symbol.
    */
   boolean isClientSymbol(Symbol symbol);

   /**
    * @return The client symbol iterator.
    */
   Iterator getClientSymbolIterator();

   /**
    * @param symbol The symbol to add. 
    */
   void addSessionSymbol(Symbol symbol);

   /**
    * @return The session symbol iterator.
    */
   Iterator getSessionSymbolIterator();

   /**
    * Adds a new metaclass object to the metadata.
    * @param metaclass The metaclass object to add.
    * @throws MetadataException if a class object
    * with the same name already exists.
    */
   void addMetaclass(Metaclass metaclass);

   /**
    * Defines a forward metaclass object by name, if it does not exist.
    * @param sName The name of the class object.
    * @param referrer The referring object. Can be null.
    * @return The metaclass object.
    */
   Metaclass defineMetaclass(String sName, MetadataObject referrer);

   /**
    * Finds a metaclass object by name.
    * @param sName The name of the class object.
    * @return The metaclass object, or null if not found.
    */
   Metaclass findMetaclass(String sName);

   /**
    * Gets a metaclass object by name.
    * @param sName The name of the class object.
    * @return The metaclass object.
    * @throws MetadataLookupException if the named
    * metaclass object does not exist.
    */
   Metaclass getMetaclass(String sName);

   /**
    * @return An iterator for the metaclass objects.
    */
   Iterator getMetaclassIterator();

   /**
    * Adds a new class aspect to the metadata.
    * @param classAspect The class aspect to add.
    * @throws MetadataException if a class aspect
    * with the same name already exists.
    */
   void addClassAspect(ClassAspect classAspect);

   /**
    * Defines a forward class aspect by name, if it does not exist.
    * @param sName The name of the class aspect.
    * @param referrer The referring object. Can be null.
    * @return The class aspect object.
    */
   ClassAspect defineClassAspect(String sName, MetadataObject referrer);

   /**
    * Gets a class aspect by name.
    * @param sName The class aspect name.
    * @return The class aspect object.
    * @throws MetadataLookupException if the class aspect does not exist.
    */
   ClassAspect getClassAspect(String sName);

   /**
    * Finds a class aspect by name.
    * @param sName The class aspect name.
    * @return The class aspect object, or null if not found.
    */
   ClassAspect findClassAspect(String sName);

   /**
    * @return The class aspect count.
    */
   int getClassAspectCount();

   /**
    * @return An iterator for the contained class aspect objects.
    */
   Iterator getClassAspectIterator();

   /**
    * Adds a new flow macro to the metadata.
    * @param flowMacro The flow macro to add.
    * @throws MetadataException if a flow macro
    * with the same name already exists.
    */
   void addFlowMacro(FlowMacro flowMacro);

   /**
    * Gets a flow macro by name.
    * @param sName The flow macro name.
    * @return The flow macro object.
    * @throws MetadataLookupException if the flow macro does not exist.
    */
   FlowMacro getFlowMacro(String sName);

   /**
    * Finds a flow macro by name.
    * @param sName The flow macro name.
    * @return The flow macro object, or null if not found.
    */
   FlowMacro findFlowMacro(String sName);

   /**
    * @return The flow macro count.
    */
   int getFlowMacroCount();

   /**
    * @return An iterator for the contained flow macro objects.
    */
   Iterator getFlowMacroIterator();

   /**
    * Adds a new workflow object to the metadata.
    * @param workflow The workflow object to add.
    * @throws MetadataException if a workflow object
    * with the same name already exists.
    */
   void addWorkflow(Workflow workflow);

   /**
    * Finds a workflow object by name and version.
    * @param sName The name of the workflow object.
    * @param nVersion The version of the workflow object.
    * @return The workflow object, or null if not found.
    */
   Workflow findWorkflow(String sName, int nVersion);

   /**
    * Gets a workflow object by name and version.
    * @param sName The name of the workflow object.
    * @param nVersion The version of the workflow object.
    * @return The workflow object.
    * @throws MetadataLookupException if the named
    * workflow object does not exist.
    */
   Workflow getWorkflow(String sName, int nVersion);

   /**
    * Gets the current version of a workflow.
    * @param sName The name of the workflow object.
    * @return The workflow object.
    * @throws MetadataLookupException if the named
    * workflow object does not exist.
    */
   Workflow getWorkflow(String sName);

   /**
    * @return An iterator for the workflow objects.
    */
   Iterator getWorkflowIterator();

   /**
    * Adds a new channel type to the metadata.
    * @param channelType The channel type to add.
    * @throws MetadataException if a channel type
    * with the same name already exists.
    */
   void addChannelType(ChannelType channelType);

   /**
    * Gets a channel type by name.
    * @param sName The channel type name.
    * @return The channel type object.
    * @throws MetadataLookupException if the channel type does not exist.
    */
   ChannelType getChannelType(String sName);

   /**
    * @return The channel type count.
    */
   int getChannelTypeCount();

   /**
    * @return An iterator for the contained channel type objects.
    */
   Iterator getChannelTypeIterator();

   /**
    * Adds a new channel to the metadata.
    * @param channel The channel to add.
    * @throws MetadataException if a channel
    * with the same name already exists.
    */
   void addChannel(Channel channel);

   /**
    * Gets a channel by name.
    * @param sName The channel name.
    * @return The channel object.
    * @throws MetadataLookupException if the channel does not exist.
    */
   Channel getChannel(String sName);

   /**
    * Finds a channel by name.
    * @param sName The channel name.
    * @return The channel object, null if not found.
    */
   Channel findChannel(String sName);

   /**
    * @return The channel count.
    */
   int getChannelCount();

   /**
    * @return An iterator for the contained channel objects.
    */
   Iterator getChannelIterator();

   /**
    * Retrieves a file from the repository.
    * @param sName Name of the file.
    * @return Binary data of a file.
    */
   Binary getFile(String sName);

   /**
    * Adds a new message format to the metadata.
    * @param format The message format to add.
    * @throws MetadataException if a message format
    * with the same name already exists.
    */
   void addFormat(Format format);

   /**
    * Gets a message format by name.
    * @param sName The message format name.
    * @return The message format object.
    * @throws MetadataLookupException if the message format does not exist.
    */
   Format getFormat(String sName);

   /**
    * @return The message format count.
    */
   int getFormatCount();

   /**
    * @return An iterator for the contained message format objects.
    */
   Iterator getFormatIterator();

   /**
    * Adds a new message to the metadata.
    * @param message The message to add.
    * @throws MetadataException if a message
    * with the same name already exists.
    */
   void addMessage(Message message);

   /**
    * Gets a message by name.
    * @param sName The message name.
    * @return The message object.
    * @throws MetadataLookupException if the message does not exist.
    */
   Message getMessage(String sName);

   /**
    * Finds a message by name.
    * @param sName The message name.
    * @return The message object; null if the message does not exist.
    */
   Message findMessage(String sName);

   /**
    * @return The message count.
    */
   int getMessageCount();

   /**
    * @return An iterator for the contained message objects.
    */
   Iterator getMessageIterator();

   /**
    * Adds a new transformation to the metadata.
    * @param transformation The transformation to add.
    * @throws MetadataException if a transformation
    * with the same name already exists.
    */
   void addTransformation(Transformation transformation);

   /**
    * Gets a transformation by name.
    * @param sName The transformation name.
    * @return The transformation object.
    * @throws MetadataLookupException if the transformation does not exist.
    */
   Transformation getTransformation(String sName);

   /**
    * Find a transformation by name.
    * @param sName The transformation name.
    * @return The transformation object, or null if not found
    */
   Transformation findTransformation(String sName);

   /**
    * @return The transformation count.
    */
   int getTransformationCount();

   /**
    * @return An iterator for the contained transformation objects.
    */
   Iterator getTransformationIterator();

   /**
    * Gets a unit test by name.
    * @param sName The unit test name.
    * @return The unit test metadata object.
    * @throws MetadataLookupException if the unit test does not exist.
    */
   UnitTest getUnitTest(String sName);

   /**
    * @return An iterator for unit test names.
    */
   Iterator getUnitTestNameIterator();

   /**
    * Adds a new service interface to the metadata.
    * @param iface The service interface to add.
    * @throws MetadataException if a service interface
    * with the same name already exists.
    */
   void addInterface(Interface iface);

   /**
    * Defines a forward service interface by name, if it does not exist.
    * @param sName The name of the service interface.
    * @param referrer The referring object. Can be null.
    * @return The service interface.
    */
   Interface defineInterface(String sName, MetadataObject referrer);

   /**
    * Gets a service interface by name.
    * @param sName The service interface name.
    * @return The service interface object.
    * @throws MetadataLookupException if the service interface does not exist.
    */
   Interface getInterface(String sName);

   /**
    * @return The service interface count.
    */
   int getInterfaceCount();

   /**
    * @return An iterator for the contained service interface objects.
    */
   Iterator getInterfaceIterator();

   /**
    * Adds a new service object to the metadata.
    * @param service The service object to add.
    * @throws MetadataException if a service object
    * with the same name already exists.
    */
   void addService(Service service);

   /**
    * Finds a service object by name and version.
    * @param sName The name of the service object.
    * @param nVersion The version of the service object.
    * @return The service object, or null if not found.
    */
   Service findService(String sName, int nVersion);

   /**
    * Gets a service object by name and version.
    * @param sName The name of the service object.
    * @param nVersion The version of the service object.
    * @return The service object.
    * @throws MetadataLookupException if the named
    * service object does not exist.
    */
   Service getService(String sName, int nVersion);

   /**
    * Gets the current version of a service.
    * @param sName The name of the service object.
    * @return The service object.
    * @throws MetadataLookupException if the named
    * service object does not exist.
    */
   Service getService(String sName);

   /**
    * @return An iterator for the service objects.
    */
   Iterator getServiceIterator();

   /**
    * Adds a new component to the metadata.
    * @param comp The component to add.
    * @throws MetadataException if a component
    * with the same name already exists.
    */
   void addComponent(Component comp);

   /**
    * Gets a component by name.
    * @param sName The name of the component.
    * @return The component.
    * @throws MetadataLookupException if the named
    * component does not exist.
    */
   Component getComponent(String sName);

   /**
    * Finds a component by name.
    * @param sName The name of the component.
    * @return The component or null if not found.
    */
   Component findComponent(String sName);

   /**
    * Adds a new external library to the metadata.
    * @param externalLibrary The external library to add.
    * @throws MetadataException if a external library with the same name already exists.
    */
   public void addExternalLibrary(ExternalLibrary externalLibrary);

   /**
    * Gets a external library by name.
    * @param sName The external library name.
    * @return The external library object.
    * @throws MetadataLookupException if the external library does not exist.
    */
   public ExternalLibrary getExternalLibrary(String sName);

   /**
    * @return The external library count.
    */
   public int getExternalLibraryCount();

   /**
    * @return An iterator for the contained external library objects.
    */
   public Iterator getExternalLibraryIterator();

   /**
    * Adds a supported locale to the metadata.
    * @param sName The locale name.
    * @return The locale object.
    */
   public Locale addLocale(String sName);

   /**
    * Gets an upgrade by name.
    * @param sName The upgrade name.
    * @return The upgrade metadata object.
    */
   public Upgrade getUpgrade(String sName);

   /**
    * Gets the properties used to load the metadata.
    * @return The properties used to load the metadata.
    */
   public Properties getProperties();

   /**
    * Sets the realmless flag.  True if kerberos-authenticated principal names do not include a realm.
    * @param bRealmless The realmless flag.  True if kerberos-authenticated principal names do not include a realm to set.
    */
   public void setRealmless(boolean bRealmless);

   /**
    * @return The realmless flag.  True if kerberos-authenticated principal names do not include a realm.
    */
   public boolean isRealmless();
   
   /**
    * @return The unique environment name that can be use to deploy multiple repositories to the same server.
    *         This can come from environment metadata or if not specified, is derived from the metadata namespace.
    */
   public String getEnvironment();
}
