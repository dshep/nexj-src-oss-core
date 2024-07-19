// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.xml;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.security.Principal;
import java.security.cert.Certificate;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.NoSuchElementException;
import java.util.Properties;

import org.w3c.dom.Element;

import nexj.core.meta.ClassAspect;
import nexj.core.meta.Component;
import nexj.core.meta.ContextMetadata;
import nexj.core.meta.ExternalLibrary;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataCompoundValidationException;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.NamedMetadataObject;
import nexj.core.meta.Primitive;
import nexj.core.meta.PrimitivePrivilege;
import nexj.core.meta.Privilege;
import nexj.core.meta.PrivilegeSet;
import nexj.core.meta.SystemResources;
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
import nexj.core.meta.testing.unit.XMLUnitTestMetadataLoader;
import nexj.core.meta.ui.ClassMeta;
import nexj.core.meta.upgrade.Upgrade;
import nexj.core.meta.upgrade.XMLUpgradeMetadataLoader;
import nexj.core.meta.workflow.FlowMacro;
import nexj.core.meta.workflow.Workflow;
import nexj.core.meta.xml.XMLMetadataHelper.ResourceHandler;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.HashTab2D;
import nexj.core.util.Holder;
import nexj.core.util.IOUtil;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.LocaleUtil;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.Named;
import nexj.core.util.ObjUtil;
import nexj.core.util.SoftHashTab;
import nexj.core.util.StringTable;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.URLUtil;
import nexj.core.util.Undefined;
import nexj.core.util.XMLUtil;

/**
 * The root object for all XML metadata definitions.
 */
public class XMLMetadata extends NamedMetadataObject implements Metadata
{
   // constants

   /**
    * The default SSL key store password.
    */
   private final static String DEFAULT_KEY_STORE_PASSWORD = "keypass";

   /**
    * The default push redirector HTTP context root.
    */
   private final static String DEFAULT_HTTP_PUSH_REDIRECTOR_CONTEXT_ROOT = "/" + SysUtil.NAMESPACE + "/pushRedirect";

   /**
    * The path to metadata upgrade classes.
    */
   public final static String METADATA_UPGRADE_PATH = "upgrade/";

   // attributes

   /**
    * The collection path flag.
    */
   protected boolean m_bCollectionPath = true;

   /**
    * The test recording flag.
    */
   protected boolean m_bTestInterceptor;

   /**
    * True if anonymous access to the flat page client is enabled.
    */
   protected boolean m_bAnonymousFlatPageEnabled;

   /**
    * The port offset to be added to any port used in call to bind().
    */
   protected int m_nPortOffset;

   /**
    * The repository root URL.
    */
   protected URL m_rootURL;

   /**
    * The repository base URL.
    */
   protected URL m_baseURL;

   /**
    * The configuration URL.
    */
   protected URL m_configURL;

   /**
    * The properties for overriding various values.
    */
   protected Properties m_properties;

   /**
    * The repository revision.
    */
   protected String m_sRevision;

   /**
    * The metadata namespace.
    */
   protected String m_sNamespace;

   /**
    * The metadata version.
    */
   protected String m_sVersion;

   /**
    * The earliest core version with compatible metadata.
    * When "+" is used at the end the metadata can use a later version of the framework.
    */
   protected String m_sCoreVersion;

   /**
    * The metadata checksum.
    */
   protected String m_sChecksum;

   /**
    * The base metadata namespace.
    */
   protected String m_sBaseNamespace;

   /**
    * The base metadata version.
    */
   protected String m_sBaseVersion;

   /**
    * The base metadata checksum.
    */
   protected String m_sBaseChecksum;

   /**
    * The unique environment name that can be use to deploy multiple repositories to the same server.
    */
   protected String m_sEnvironmentName;
   
   /**
    * The default decryption/encryption password.
    */
   protected String m_sDataPassword;

   /**
    * The highest-security encryption scheme that was used to encrypt
    * connection and server metadata.
    */
   protected String m_sEncryptionScheme;

   /**
    * The primitive privilege count.
    */
   protected int m_nPrimitivePrivilegeCount;

   /**
    * The authentication protocol.
    */
   protected int m_nAuthProtocol;

   /**
    * The authentication service name.
    */
   protected String m_sAuthService;

   /**
    * The authentication domain.
    */
   protected String m_sAuthDomain;

   /**
    * The HTTP context root: the path at which the NexJ applications
    * will be made available.
    */
   protected String m_sHTTPContextRoot;

   /**
    * The anonymous HTTP context root: the path at which the NexJ applications
    * will be made available anonymously.
    */
   protected String m_sHTTPAnonymousContextRoot;

   /**
    * The form-based-authentication HTTP context root: the path at which the
    * NexJ applications will be made available through form-based sign on.
    */
   protected String m_sHTTPFormContextRoot;

   /**
    * The push redirector HTTP context root: the path at which the
    * NexJ push redirector  will be made available.
    */
   protected String m_sHTTPPushRedirectorContextRoot = DEFAULT_HTTP_PUSH_REDIRECTOR_CONTEXT_ROOT;

   /**
    * The path from the root of the WAR to the login page for form-based sign on.
    */
   protected String m_sHTTPFormLoginPage = "/login.htm";

   /**
    * The path from the root of the WAR to the login error page for form-based sign on.
    */
   protected String m_sHTTPFormErrorPage = "/login-error.htm";

   /**
    * The HTTP root: the full URI path to the NexJ web applications, as specified
    * in the "httpURL" property in the .server metadata file.
    */
   protected String m_sHTTPRoot;

   /**
    * The distribution flag.
    */
   protected boolean m_bDistributed;

   /**
    * The dynamic receiver flag.
    */
   protected boolean m_bDynamicReceiver;

   /**
    * Flag to enable validation of PrimaryKey between RelationalDatabase and Main.upgrade
    */
   protected boolean m_bPrimaryKeyUpgradeValidation;

   /**
    * The session timeout in minutes (0 is unlimited).
    */
   protected int m_nSessionTimeout = 20;

   /**
    * The connection integrity and confidentiality flag.
    */
   protected boolean m_bSecureTransport = true;

   /**
    * The session replication flag.
    */
   protected boolean m_bReplicatedSession;

   /**
    * The session persistence flag.
    */
   protected boolean m_bPersistentSession;

   /**
    * The test environment flag.
    */
   protected boolean m_bTestEnvironment;

   /**
    * Environment only loading.
    */
   protected boolean m_bEnvironmentOnly;

   /**
    * True if generic RPC calls are allowed; false to deny access to generic RPC.
    */
   protected boolean m_bGenericRPCAllowed;

   /**
    * True if anonymous access to the HTTP/text, HTTP/soap, and HTTP/xml RPC protocols is enabled.
    */
   protected boolean m_bAnonymousRPCEnabled;

   /**
    * The SSL key store password.
    */
   protected String m_sKeyStorePassword = DEFAULT_KEY_STORE_PASSWORD;

   /**
    * Encrypted passwords flag; a value of true indicates that the passwords
    * in the metadata are encrypted.
    */
   protected boolean m_bEncrypted;

   /**
    * The realmless flag.  True if kerberos-authenticated principal names do not include a realm.
    */
   protected boolean m_bRealmless;

   // associations

   /**
    * The global scripting environment.
    */
   protected GlobalEnvironment m_globalEnv = new GlobalEnvironment();

   /**
    * The listing of available resources.
    */
   protected XMLMetadataListing m_listing;

   /**
    * The generic RPC privilege.
    */
   protected PrimitivePrivilege m_genericRPCPrivilege;

   /**
    * The trusted client certificate for HTTP requests.
    */
   protected Certificate m_trustedCertificate;

   /**
    * The authentication properties.
    */
   protected Properties m_authProperties = new Properties();

   /**
    * The user principal under which anonymous HTTP requests will be processed.
    */
   protected Principal m_anonymousUser;

   /**
    * The privilege map - maps privilege name to privilege object.
    */
   protected Lookup m_privilegeMap = new HashTab(1024); // of type Privilege[String]

   /**
    * Maps class name to class definition.
    */
   protected Lookup m_metaclassMap = new HashTab(512); // of type Metaclass

   /**
    * The class aspect map: ClassAspect[String].
    */
   protected Lookup m_classAspectMap = new HashTab(8);

   /**
    * The class aspect list, sorted by name.
    */
   protected List m_classAspectList = new ArrayList(8);

   /**
    * The flow macro map: FlowMacro[String].
    */
   protected Lookup m_flowMacroMap = new HashTab(8);

   /**
    * The workflow map: Workflow[String][Integer].
    */
   protected Lookup2D m_workflowMap = new HashTab2D();

   /**
    * The current workflow map: Workflow[String].
    */
   protected Lookup m_currentWorkflowMap = new HashTab();

   /**
    * The channel type map: ChannelType[String].
    */
   protected Lookup m_channelTypeMap = new HashTab();

   /**
    * The data source type element map: DataSourceType[String].
    */
   protected Lookup m_channelTypeElementMap = new HashTab(8);

   /**
    * The channel map: Channel[String].
    */
   protected Lookup m_channelMap = new HashTab();

   /**
    * The message format map: Format[String].
    */
   protected Lookup m_formatMap = new HashTab();

   /**
    * The message map: Message[String].
    */
   protected Lookup m_messageMap = new HashTab(8);

   /**
    * The transformation map: Transformation[String].
    */
   protected Lookup m_transformationMap = new HashTab(8);

   /**
    * Maps a unit test name to resource path: String[String].
    */
   protected Lookup m_unitTestResMap = new HashTab(8);

   /**
    * Maps a unit test name to unit test metadata: UnitTest[String].
    */
   protected Lookup m_unitTestMap = new SoftHashTab(8);

   /**
    * The service interface map: Interface[String].
    */
   protected Lookup m_interfaceMap = new HashTab(8);

   /**
    * The service map: Service[String][Integer].
    */
   protected Lookup2D m_serviceMap = new HashTab2D();

   /**
    * The current service map: Service[String].
    */
   protected Lookup m_currentServiceMap = new HashTab();

   /**
    * Maps component name to component definition.
    */
   protected Lookup m_componentMap = new HashTab(256); // of type Component

   /**
    * The data source type map: DataSourceType[String].
    */
   protected Lookup m_dataSourceTypeMap = new HashTab(8);

   /**
    * The data source type element map: DataSourceType[String].
    */
   protected Lookup m_dataSourceTypeElementMap = new HashTab(8);

   /**
    * The data source map: DataSource[String].
    */
   protected Lookup m_dataSourceMap = new HashTab(8);

   /**
    * Maps a dump name to dump metadata: String[String].
    */
   protected Lookup m_dumpResMap = new HashTab(8);

   /**
    * Maps engine name to definition.
    */
   protected Lookup m_persistenceEngineMap = new HashTab(8); // of type PersistenceEngine

   /**
    * Maps schema name to definition.
    */
   protected Lookup m_schemaMap = new HashTab(4); // of type Schema[String]

   /**
    * The public symbol set.
    */
   protected Holder m_publicSymbolSet = new HashHolder(64); // of type Symbol

   /**
    * The session symbol set. These are session local variables.
    */
   protected Holder m_sessionSymbolSet = new HashHolder(32); // of type Symbol

   /**
    * The client symbol set.
    */
   protected Holder m_clientSymbolSet = new HashHolder(32); // of type Symbol

   /**
    * The locale name to object map: Locale[String].
    */
   protected Lookup m_localeMap = new HashTab();

   /**
    * The external library map: ExternalLibrary[String].
    */
   protected Lookup m_externalLibraryMap = new LinkedHashTab();

   /**
    * Maps locale name to a set of resource paths.
    */
   protected Lookup m_stringMap = new HashTab(64)
   {
      // constants

      /**
       * Serialization version.
       */
      private final static long serialVersionUID = -8557119405683556309L;

      // operations

      public Object put(Object key, Object value)
      {
         m_stringMetaMap.put(key, Undefined.VALUE);

         return super.put(key, value);
      }
   };

   /**
    * Maps an upgrade name to a resource path: String[String].
    */
   protected Lookup m_upgradeResMap = new LinkedHashTab(1);

   /**
    * Maps a locale name to a string table name.
    * Loaded lazily.
    */
   protected Lookup m_stringMetaMap = new HashTab(64);

   /**
    * Shared xml metadata helper for validation in single-threaded mode only.
    */
   protected XMLMetadataHelper m_validationHelper;

   /**
    * Shared xml metadata upgrader for use in multi-threaded mode.
    */
   protected XMLMetadataUpgrader m_upgrader;

   // constructors

   /**
    * Creates the XMLMetadata container.
    * @param sName The metadata object name.
    * @param rootURL The repository root URL.
    * @param baseURL The repository base URL.
    * @param properties The metadata properties for overriding various values.
    * @param helper The metadata helper used for validation. Must be null when
    * creating a metadata object, which can be shared between multiple threads.
    * Unused when helper is non-null.
    */
   public XMLMetadata(String sName, URL rootURL, URL baseURL, Properties properties,
      XMLMetadataHelper helper)
   {
      super(sName);
      m_rootURL = rootURL;
      m_baseURL = baseURL;
      m_properties = properties;
      m_validationHelper = helper;
   }

   // operations

   /**
    * For framework internal use only.
    * @return A new metadata helper for on-demand loading.
    */
   public XMLMetadataHelper getHelper()
   {
      if (m_validationHelper != null)
      {
         return m_validationHelper;
      }

      return new XMLMetadataHelper(m_rootURL, m_baseURL, m_properties, m_listing, m_upgrader);
   }

   /**
    * Sets the resource listing.
    * @param listing The resource listing.
    */
   public void setListing(XMLMetadataListing listing)
   {
      verifyNotReadOnly();
      m_listing = listing;
   }

   /**
    * @return The resource listing.
    */
   public XMLMetadataListing getListing()
   {
      return m_listing;
   }

   /**
    * Sets the resource upgrader
    * @param upgrader The resource upgrader.
    */
   public void setUpgrader(XMLMetadataUpgrader upgrader)
   {
      m_upgrader = upgrader;
   }

   /**
    * @return The resource upgrader.
    */
   public XMLMetadataUpgrader getUpgrader()
   {
      return m_upgrader;
   }

   /**
    * Sets the configuration URL.
    * @param configURL The configuration URL to set.
    */
   public void setConfigurationURL(URL configURL)
   {
      verifyNotReadOnly();
      m_configURL = configURL;
   }

   /**
    * @return The configuration URL.
    */
   public URL getConfigurationURL()
   {
      return m_configURL;
   }

   /**
    * Sets the port offset to be added to base port by internal calls to bind().
    * @param nOffset The offset to be added to base port used in bind().
    */
   public void setPortOffset(int nOffset)
   {
      verifyNotReadOnly();
      m_nPortOffset = nOffset;
   }

   /**
    * @see nexj.core.meta.Metadata#getPortOffset()
    */
   public int getPortOffset()
   {
      return m_nPortOffset;
   }

   /**
    * Sets the repository revision.
    * @param sRevision The repository revision to set.
    */
   public void setRevision(String sRevision)
   {
      verifyNotReadOnly();
      m_sRevision = sRevision;
   }

   /**
    * @return The repository revision.
    */
   public String getRevision()
   {
      return m_sRevision;
   }

   /**
    * Sets the metadata namespace.
    * @param sNamespace The metadata namespace to set.
    */
   public void setNamespace(String sNamespace)
   {
      verifyNotReadOnly();
      m_sNamespace = sNamespace;
   }

   /**
    * @return The metadata namespace.
    */
   public String getNamespace()
   {
      return m_sNamespace;
   }

   /**
    * Sets the earliest core version with compatible metadata.
    * @param sVersion The framework compatibility version to set. With optional '+' suffix.
    */
   public void setCoreVersion(String sCoreVersion)
   {
      verifyNotReadOnly();
      m_sCoreVersion = sCoreVersion;
      m_bCollectionPath = (sCoreVersion == null || StringUtil.compareVersionRanges(sCoreVersion, "7.1.57.0") >= 0);

      m_bDynamicReceiver = (sCoreVersion == null
         || (sCoreVersion.startsWith("7.1.") && StringUtil.compareVersionRanges(sCoreVersion, "7.1.65.0") >= 0)
         || (sCoreVersion.startsWith("7.0.") && StringUtil.compareVersionRanges(sCoreVersion, "7.0.160.0") >= 0)
         || (sCoreVersion.startsWith("6.2.") && StringUtil.compareVersionRanges(sCoreVersion, "6.2.1.100") >= 0));

      m_bPrimaryKeyUpgradeValidation =
         (sCoreVersion == null || StringUtil.compareVersionRanges(sCoreVersion, "8.0.23.0") >= 0);
   }

   /**
    * @return The earliest core version with compatible metadata.
    */
   public String getCoreVersion()
   {
      return m_sCoreVersion;
   }

   /**
    * Sets the metadata version.
    * @param sVersion The metadata version to set.
    */
   public void setVersion(String sVersion)
   {
      verifyNotReadOnly();
      m_sVersion = sVersion;
   }

   /**
    * @return The metadata version.
    */
   public String getVersion()
   {
      return m_sVersion;
   }

   /**
    * Sets the metadata checksum.
    * @param sChecksum The metadata checksum to set.
    */
   public void setChecksum(String sChecksum)
   {
      verifyNotReadOnly();
      m_sChecksum = sChecksum;
   }

   /**
    * @return The metadata checksum.
    */
   public String getChecksum()
   {
      return m_sChecksum;
   }

   /**
    * Sets the base metadata namespace.
    * @param sBaseNamespace The base metadata namespace to set.
    */
   public void setBaseNamespace(String sBaseNamespace)
   {
      verifyNotReadOnly();
      m_sBaseNamespace = sBaseNamespace;
   }

   /**
    * @return The base metadata namespace.
    */
   public String getBaseNamespace()
   {
      return m_sBaseNamespace;
   }

   /**
    * Sets the base metadata version.
    * @param sBaseVersion The base metadata version to set.
    */
   public void setBaseVersion(String sBaseVersion)
   {
      verifyNotReadOnly();

      if (sBaseVersion == null &&
         m_sNamespace != null)
      {
         sBaseVersion = "";
      }

      m_sBaseVersion = sBaseVersion;
   }

   /**
    * @return The base metadata version.
    */
   public String getBaseVersion()
   {
      return m_sBaseVersion;
   }

   /**
    * Sets the base metadata checksum.
    * @param sBaseChecksum The base metadata checksum to set.
    */
   public void setBaseChecksum(String sBaseChecksum)
   {
      verifyNotReadOnly();

      if (sBaseChecksum == null &&
         m_sNamespace != null)
      {
         sBaseChecksum = "";
      }

      m_sBaseChecksum = sBaseChecksum;
   }

   /**
    * @return The base metadata checksum.
    */
   public String getBaseChecksum()
   {
      return m_sBaseChecksum;
   }

   /**
    * Sets the collection path flag.
    * @param bCollectionPathEnabled The collection path flag to set.
    */
   public void setCollectionPathEnabled(boolean bCollectionPathEnabled)
   {
      verifyNotReadOnly();
      m_bCollectionPath = bCollectionPathEnabled;
   }

   /**
    * @return The collection path flag.
    * @see nexj.core.meta.Metadata#isCollectionPathEnabled()
    */
   public boolean isCollectionPathEnabled()
   {
      return m_bCollectionPath;
   }

   /**
    * @see nexj.core.meta.Metadata#isDynamicReceiverEnabled()
    */
   public boolean isDynamicReceiverEnabled()
   {
      return m_bDynamicReceiver;
   }

   /**
    * Sets the dynamic receiver flag.
    * @param bDynamicReceiverEnabled The dynamic receiver flag to set.
    */
   public void setDynamicReceiverEnabled(boolean bDynamicReceiverEnabled)
   {
      verifyNotReadOnly();
      m_bDynamicReceiver = bDynamicReceiverEnabled;
   }

   /**
    * @return Enable PK validation between RelationalDatabase and Main.upgrade.
    * @see nexj.core.meta.xml.XMLMetadata#m_bPrimaryKeyUpgradeValidation
    */
   public boolean isPrimaryKeyUpgradeValidationEnabled()
   {
      return m_bPrimaryKeyUpgradeValidation;
   }

   /**
    * Enables PK validation between RelationalDatabase and Main.upgrade.
    * @param bValidateUpgradePKEnabled The PK validation flag.
    * @see nexj.core.meta.xml.XMLMetadata#m_bPrimaryKeyUpgradeValidation
    */
   public void setValidateUpgradePKEnabled(boolean bPrimaryKeyUpgradeValidationEnabled)
   {
      verifyNotReadOnly();
      m_bPrimaryKeyUpgradeValidation = bPrimaryKeyUpgradeValidationEnabled;
   }

   /**
    * Sets the authentication protocol.
    * @param nAuthProtocol The authentication protocol to set.
    */
   public void setAuthenticationProtocol(int nAuthProtocol)
   {
      verifyNotReadOnly();
      m_nAuthProtocol = nAuthProtocol;
   }

   /**
    * @return The authentication protocol.
    */
   public int getAuthenticationProtocol()
   {
      return m_nAuthProtocol;
   }

   /**
    * Sets the authentication service name.
    * @param sAuthService The authentication service name to set.
    */
   public void setAuthenticationService(String sAuthService)
   {
      verifyNotReadOnly();

      if ("".equals(sAuthService))
      {
         sAuthService = null;
      }

      if (sAuthService == null && m_nAuthProtocol == AUTH_PROTOCOL_SPNEGO)
      {
         throw new MetadataException("err.meta.missingAuthService");
      }

      m_sAuthService = sAuthService;
   }

   /**
    * @return The authentication service name.
    */
   public String getAuthenticationService()
   {
      return m_sAuthService;
   }

   /**
    * @see nexj.core.meta.Metadata#getAuthenticationProperties()
    */
   public Properties getAuthenticationProperties()
   {
      return m_authProperties;
   }

   /**
    * Sets the authentication domain.
    * @param sAuthDomain The authentication domain to set.
    */
   public void setAuthenticationDomain(String sAuthDomain)
   {
      verifyNotReadOnly();

      if ("".equals(sAuthDomain))
      {
         sAuthDomain = null;
      }

      m_sAuthDomain = sAuthDomain;
   }

   /**
    * @return The authentication domain.
    */
   public String getAuthenticationDomain()
   {
      return m_sAuthDomain;
   }

   /**
    * @param bAllowGenericRPC True if generic RPC calls are allowed;
    * false to deny access to generic RPC.
    */
   public void setGenericRPCAllowed(boolean bAllowGenericRPC)
   {
      verifyNotReadOnly();
      m_bGenericRPCAllowed = bAllowGenericRPC;
   }

   /**
    * @see nexj.core.meta.Metadata#isGenericRPCAllowed()
    */
   public boolean isGenericRPCAllowed()
   {
      return m_bGenericRPCAllowed;
   }

   /**
    * Sets the generic RPC privilege.
    * @param genericRPCPrivilege The generic RPC privilege to set.
    */
   public void setGenericRPCPrivilege(PrimitivePrivilege genericRPCPrivilege)
   {
      verifyNotReadOnly();
      m_genericRPCPrivilege = genericRPCPrivilege;
   }

   /**
    * @return The generic RPC privilege.
    */
   public PrimitivePrivilege getGenericRPCPrivilege()
   {
      return m_genericRPCPrivilege;
   }

   /**
    * Sets the user principal under which anonymous HTTP requests will be processed.
    * @param anonymousUser The anonymous user principal to set.
    */
   public void setAnonymousUser(Principal anonymousUser)
   {
      verifyNotReadOnly();
      m_anonymousUser = anonymousUser;
   }

   /**
    * @see nexj.core.meta.Metadata#getAnonymousUser()
    */
   public Principal getAnonymousUser()
   {
      return m_anonymousUser;
   }

   /**
    * @param bAnonymousRPCEnabled True if anonymous access to the HTTP/text, HTTP/soap, and HTTP/xml RPC protocols is enabled.
    */
   public void setAnonymousRPCEnabled(boolean bAnonymousRPCEnabled)
   {
      verifyNotReadOnly();
      m_bAnonymousRPCEnabled = bAnonymousRPCEnabled;
   }

   /**
    * @see nexj.core.meta.Metadata#isAnonymousRPCEnabled()
    */
   public boolean isAnonymousRPCEnabled()
   {
      return m_bAnonymousRPCEnabled;
   }

   /**
    * @param bAnonymousFlatPageEnabled True if anonymous access to the flat page client is enabled.
    */
   public void setAnonymousFlatPageEnabled(boolean bAnonymousFlatPageEnabled)
   {
      verifyNotReadOnly();
      m_bAnonymousFlatPageEnabled = bAnonymousFlatPageEnabled;
   }

   /**
    * @see nexj.core.meta.Metadata#isAnonymousFlatPageEnabled()
    */
   public boolean isAnonymousFlatPageEnabled()
   {
      return m_bAnonymousFlatPageEnabled;
   }

   /**
    * Sets the client certificate to trust for HTTP requests.
    *
    * @param certificate The trusted certificate for HTTP requests.
    */
   public void setTrustedCertificate(Certificate certificate)
   {
      verifyNotReadOnly();
      m_trustedCertificate = certificate;
   }

   /**
    * @see nexj.core.meta.Metadata#getTrustedCertificate()
    */
   public Certificate getTrustedCertificate()
   {
      return m_trustedCertificate;
   }

   /**
    * Sets the HTTP context root (the URI path at which the NexJ web applications
    * will be made available).
    *
    * @param sContextRoot The HTTP context root to set.
    */
   public void setHTTPContextRoot(String sContextRoot)
   {
      verifyNotReadOnly();
      
      if (sContextRoot != null)
      {
         m_sHTTPContextRoot = sContextRoot;
      }
   }

   /**
    * @see nexj.core.meta.Metadata#getHTTPContextRoot()
    */
   public String getHTTPContextRoot()
   {
      return m_sHTTPContextRoot;
   }

   /**
    * Sets the anonymous HTTP context root (the path at which the NexJ web applications
    * will be made available anonymously).
    *
    * @param sContextRoot The anonymous HTTP context root to set.
    */
   public void setHTTPAnonymousContextRoot(String sContextRoot)
   {
      verifyNotReadOnly();
      
      if (sContextRoot != null)
      {
         m_sHTTPAnonymousContextRoot = sContextRoot;
      }
   }

   /**
    * @see nexj.core.meta.Metadata#getHTTPAnonymousContextRoot()
    */
   public String getHTTPAnonymousContextRoot()
   {
      return m_sHTTPAnonymousContextRoot;
   }

   /**
    * Sets the form-based-authentication HTTP context root (the path at which the NexJ
    * web applications will be made available through form-based sign on).
    *
    * @param sContextRoot The form-based-authentication HTTP context root to set.
    */
   public void setHTTPFormContextRoot(String sContextRoot)
   {
      verifyNotReadOnly();
      
      if (sContextRoot != null)
      {
         m_sHTTPFormContextRoot = sContextRoot;
      }
   }

   /**
    * @see nexj.core.meta.Metadata#getHTTPFormContextRoot()
    */
   public String getHTTPFormContextRoot()
   {
      return m_sHTTPFormContextRoot;
   }

   /**
    * Sets the push redirector HTTP context root (the path at which the NexJ
    * push redirector web application will be made available).
    *
    * @param sContextRoot The push redirector HTTP context root to set.
    */
   public void setHTTPPushRedirectorContextRoot(String sContextRoot)
   {
      verifyNotReadOnly();
      m_sHTTPPushRedirectorContextRoot = (sContextRoot != null) ? sContextRoot : DEFAULT_HTTP_PUSH_REDIRECTOR_CONTEXT_ROOT;
   }

   /**
    * @see nexj.core.meta.Metadata#getHTTPFormContextRoot()
    */
   public String getHTTPPushRedirectorContextRoot()
   {
      return m_sHTTPPushRedirectorContextRoot;
   }

   /**
    * Sets the login page for form-based sign on.
    *
    * @param sPath The path from the root of the WAR.
    */
   public void setHTTPFormLoginPage(String sPath)
   {
      verifyNotReadOnly();
      m_sHTTPFormLoginPage = sPath;
   }

   /**
    * @see nexj.core.meta.Metadata#getHTTPFormLoginPage()
    */
   public String getHTTPFormLoginPage()
   {
      return m_sHTTPFormLoginPage;
   }

   /**
    * Sets the login error page for form-based sign on.
    *
    * @param sPath The path from the root of the WAR.
    */
   public void setHTTPFormErrorPage(String sPath)
   {
      verifyNotReadOnly();
      m_sHTTPFormErrorPage = sPath;
   }

   /**
    * @see nexj.core.meta.Metadata#getHTTPFormErrorPage()
    */
   public String getHTTPFormErrorPage()
   {
      return m_sHTTPFormErrorPage;
   }

   /**
    * Sets the HTTP root (the full URI path to the NexJ web applications, as specified
    * in the "httpURL" property in the .server metadata file)
    *
    * @param sHTTPRoot The HTTP root to set.
    */
   public void setHTTPRoot(String sHTTPRoot)
   {
      m_sHTTPRoot = (sHTTPRoot != null) ? sHTTPRoot : "";
   }

   /**
    * Gets the HTTP root (the full URI path to the NexJ web applications, as specified
    * in the "httpURL" property in the .server metadata file)
    *
    * @return The HTTP root ("httpURL" property from the server metadata).
    */
   public String getHTTPRoot()
   {
      return m_sHTTPRoot;
   }

   /**
    * Sets the session timeout.
    * @param nTimeout The session timeout in minutes (0 is unlimited).
    */
   public void setSessionTimeout(int nTimeout)
   {
      m_nSessionTimeout = nTimeout;
   }

   /**
    * @return The session timeout in minutes (0 is unlimited).
    */
   public int getSessionTimeout()
   {
      return m_nSessionTimeout;
   }

   /**
    * Sets the distribution flag.
    * @param bDistributed The distribution flag to set.
    */
   public void setDistributed(boolean bDistributed)
   {
      verifyNotReadOnly();
      m_bDistributed = bDistributed;
   }

   /**
    * @return The distribution flag.
    */
   public boolean isDistributed()
   {
      return m_bDistributed;
   }

   /**
    * Sets the connection integrity and confidentiality flag.
    * @param bSecure The connection integrity and confidentiality flag to set.
    */
   public void setSecureTransport(boolean bSecure)
   {
      verifyNotReadOnly();
      m_bSecureTransport = bSecure;
   }

   /**
    * @return The connection integrity and confidentiality flag.
    */
   public boolean isSecureTransport()
   {
      return m_bSecureTransport;
   }

   /**
    * Sets the test recording flag.
    * @param bTestInterceptor The test recording flag to set.
    */
   public void setTestInterceptor(boolean bTestInterceptor)
   {
      verifyNotReadOnly();
      m_bTestInterceptor = bTestInterceptor;
   }

   /**
    * @return The test recording flag.
    */
   public boolean isTestInterceptor()
   {
      return m_bTestInterceptor;
   }
   /**
    * Sets the test environment flag.
    * @param bTestEnvironment The test environment flag to set.
    */
   public void setTestEnvironment(boolean bTestEnvironment)
   {
      verifyNotReadOnly();
      m_bTestEnvironment = bTestEnvironment;
   }

   /**
    * @return True if it is a test environment.
    */
   public boolean isTestEnvironment()
   {
      return m_bTestEnvironment;
   }

   /**
    * Sets the session replication flag.
    * @param bReplicatedSession The session replication flag to set.
    */
   public void setReplicatedSession(boolean bReplicatedSession)
   {
      verifyNotReadOnly();
      m_bReplicatedSession = bReplicatedSession;
   }

   /**
    * @return The session replication flag.
    */
   public boolean isReplicatedSession()
   {
      return m_bReplicatedSession;
   }

   /**
    * Sets the session persistence flag.
    * @param bPersistentSession The session persistence flag to set.
    */
   public void setPersistentSession(boolean bPersistentSession)
   {
      verifyNotReadOnly();
      m_bPersistentSession = bPersistentSession;
   }

   /**
    * @return The session persistence flag.
    */
   public boolean isPersistentSession()
   {
      return m_bPersistentSession;
   }

   /**
    * @see nexj.core.meta.Metadata#getGlobalEnvironment()
    */
   public GlobalEnvironment getGlobalEnvironment()
   {
      return m_globalEnv;
   }

   /**
    * Sets the SSL key store password.
    *
    * @param sKeyPass The key store password.
    */
   public void setKeyStorePassword(String sKeyPass)
   {
      verifyNotReadOnly();
      m_sKeyStorePassword = (sKeyPass != null) ? sKeyPass : DEFAULT_KEY_STORE_PASSWORD;
   }

   /**
    * Gets the SSL key store password.
    *
    * @return The key store password.
    */
   public String getKeyStorePassword()
   {
      return m_sKeyStorePassword;
   }

   /**
    * Adds a new data source type to the metadata.
    * @param dataSourceType The data source type to add.
    * @throws MetadataException if a data source type
    * with the same name already exists.
    */
   public void addDataSourceType(DataSourceType dataSourceType)
   {
      verifyNotReadOnly();

      Object oldDataSourceType = m_dataSourceTypeMap.put(dataSourceType.getName(), dataSourceType);

      if (oldDataSourceType != null)
      {
         m_dataSourceTypeMap.put(dataSourceType.getName(), oldDataSourceType);

         throw new MetadataException("err.meta.dataSourceTypeDup", new Object[]
         {
            dataSourceType.getName(),
            getName()
         });
      }

      dataSourceType.setMetadata(this);
   }

   /**
    * Gets a data source type by name.
    * @param sName The data source type name.
    * @return The data source type object.
    * @throws MetadataLookupException if the data source type does not exist.
    */
   public DataSourceType getDataSourceType(String sName)
   {
      DataSourceType dataSourceType = (DataSourceType) m_dataSourceTypeMap.get(sName);

      if (dataSourceType != null)
      {
         return dataSourceType;
      }

      throw new MetadataLookupException("err.meta.dataSourceTypeLookup", sName, this);
   }

   /**
    * @return The data source type count.
    */
   public int getDataSourceTypeCount()
   {
      return m_dataSourceTypeMap.size();
   }

   /**
    * @return An iterator for the contained data source type objects.
    */
   public Iterator getDataSourceTypeIterator()
   {
      return m_dataSourceTypeMap.valueIterator();
   }

   /**
    * Adds a new data source type XML element mapping to the metadata.
    * @param sElement The XML element name.
    * @param dataSourceType The data source type object.
    * @throws MetadataException if a duplicate entry is encountered.
    */
   public void addDataSourceTypeElement(String sElement, DataSourceType dataSourceType)
   {
      verifyNotReadOnly();

      m_dataSourceTypeElementMap.put(dataSourceType, sElement);

      if (sElement.equals("DataSource"))
      {
         return;
      }

      Object oldDataSourceType = m_dataSourceTypeElementMap.put(sElement, dataSourceType);

      if (oldDataSourceType != null)
      {
         m_dataSourceTypeElementMap.put(sElement, oldDataSourceType);

         throw new MetadataException("err.meta.dataSourceTypeElementDup", new Object[]
         {
            sElement,
            dataSourceType.getName(),
            getName()
         });
      }
   }

   /**
    * Gets a data source type by element name.
    * @param element The data source type element.
    * @return The data source type object.
    * @throws MetadataLookupException if the data source type does not exist.
    */
   public DataSourceType getDataSourceTypeByElement(Element element)
   {
      if (element.getNodeName().equals("DataSource"))
      {
         return getDataSourceType(XMLUtil.getReqStringAttr(element, "type"));
      }

      DataSourceType dataSourceType = (DataSourceType)m_dataSourceTypeElementMap.get(element.getNodeName());

      if (dataSourceType != null)
      {
         return dataSourceType;
      }

      throw new MetadataLookupException("err.meta.dataSourceTypeElementLookup", element.getNodeName(), this);
   }

   /**
    * Gets a data source type element name.
    * @param type The data source type.
    */
   public String getDataSourceTypeElement(DataSourceType type)
   {
      return (String)m_dataSourceTypeElementMap.get(type);
   }

   /**
    * Adds a new data source to the metadata.
    * @param dataSource The data source to add.
    * @throws MetadataException if a data source
    * with the same name already exists.
    */
   public void addDataSource(DataSource dataSource)
   {
      verifyNotReadOnly();

      Object oldDataSource = m_dataSourceMap.put(dataSource.getName(), dataSource);

      if (oldDataSource != null)
      {
         m_dataSourceMap.put(dataSource.getName(), oldDataSource);

         throw new MetadataException("err.meta.dataSourceDup", new Object[]
         {
            dataSource.getName(),
            getName()
         });
      }
   }

   /**
    * Gets a data source by name.
    * @param sName The data source name.
    * @return The data source object.
    * @throws MetadataLookupException if the data source does not exist.
    */
   public DataSource getDataSource(String sName)
   {
      DataSource dataSource = findDataSource(sName);

      if (dataSource != null)
      {
         return dataSource;
      }

      throw new MetadataLookupException("err.meta.dataSourceLookup", sName, this);
   }

   /**
    * @see nexj.core.meta.Metadata#findDataSource(java.lang.String)
    */
   public DataSource findDataSource(String sName)
   {
      return (DataSource)m_dataSourceMap.get(sName);
   }

   /**
    * @return The data source count.
    */
   public int getDataSourceCount()
   {
      return m_dataSourceMap.size();
   }

   /**
    * @return An iterator for the contained data source objects.
    */
   public Iterator getDataSourceIterator()
   {
      return m_dataSourceMap.valueIterator();
   }

   /**
    * @see nexj.core.meta.Metadata#getDataSourceFragmentIterator()
    */
   public Iterator getDataSourceFragmentIterator()
   {
      return new Iterator()
      {
         protected Iterator m_dsItr = getDataSourceIterator();
         protected Iterator m_fragItr = null;

         public boolean hasNext()
         {
            while (m_fragItr == null || !m_fragItr.hasNext())
            {
               if (m_dsItr.hasNext())
               {
                  m_fragItr = ((DataSource)m_dsItr.next()).getFragmentIterator();
               }
               else
               {
                  m_fragItr = null;

                  return false;
               }
            }

            return true;
         }

         public Object next()
         {
            if (m_fragItr == null)
            {
               throw new NoSuchElementException();
            }

            return m_fragItr.next();
         }

         public void remove()
         {
            throw new UnsupportedOperationException();
         }
      };
   }

   /**
    * @see nexj.core.meta.Metadata#getDataPassword()
    */
   public String getDataPassword()
   {
      return m_sDataPassword;
   }

   /**
    * @see nexj.core.meta.Metadata#setDataPassword(java.lang.String)
    */
   public void setDataPassword(String sPassword)
   {
      verifyNotReadOnly();

      m_sDataPassword = sPassword;
   }

   /**
    * @see nexj.core.meta.Metadata#getEncryptionScheme()
    */
   public String getEncryptionScheme()
   {
      return m_sEncryptionScheme;
   }

   /**
    * @see nexj.core.meta.Metadata#setEncryptionScheme(java.lang.String)
    */
   public void setEncryptionScheme(String sScheme)
   {
      verifyNotReadOnly();

      m_sEncryptionScheme = sScheme;
   }

   /**
    * @see nexj.core.meta.Metadata#isEncrypted()
    */
   public boolean isEncrypted()
   {
      return m_bEncrypted;
   }

   /**
    * Sets the flag indicating whether or not the passwords in this
    * metadata object are encrypted.
    *
    * @param bEncrypted True to indicate the passwords are encrypted;
    * false for decrypted passwords.
    */
   public void setEncrypted(boolean bEncrypted)
   {
      verifyNotReadOnly();
      m_bEncrypted = bEncrypted;
   }

   /**
    * @return True if is in the environment loading mode.
    */
   public boolean isEnvironmentOnly()
   {
      return m_bEnvironmentOnly;
   }

   /**
    * @param environmentOnly True if is in the environment loading mode.
    */
   public void setEnvironmentOnly(boolean environmentOnly)
   {
      m_bEnvironmentOnly = environmentOnly;
   }

   /**
    * Adds a new privilege to the metadata.
    * @param privilege The privilege to add.
    * @throws MetadataException if a privilege
    * with the same name already exists.
    */
   public void addPrivilege(Privilege privilege)
   {
      verifyNotReadOnly();

      Object oldPrivilege = m_privilegeMap.put(privilege.getName(), privilege);

      if (oldPrivilege != null)
      {
         m_privilegeMap.put(privilege.getName(), oldPrivilege);

         MetadataException e = new MetadataException("err.meta.privilegeDup", new Object[]{privilege.getName(), getName()});

         throw e;
      }

      if (privilege instanceof PrimitivePrivilege)
      {
         ((PrimitivePrivilege)privilege).setOrdinal(m_nPrimitivePrivilegeCount++);
      }
   }

   /**
    * Gets a privilege by name.
    * @param sName The privilege name.
    * @return The privilege object.
    * @throws MetadataLookupException if the privilege does not exist.
    */
   public Privilege getPrivilege(String sName)
   {
      Privilege privilege = (Privilege)m_privilegeMap.get(sName);

      if (privilege != null)
      {
         return privilege;
      }

      throw new MetadataLookupException("err.meta.privilegeLookup", sName, this);
   }

   /**
    * @see nexj.core.meta.Metadata#findPrivilege(java.lang.String)
    */
   public Privilege findPrivilege(String sName)
   {
      if (sName == null)
      {
         return null;
      }

      return (Privilege)m_privilegeMap.get(sName);
   }

   /**
    * @return The privilege count.
    */
   public int getPrivilegeCount()
   {
      return m_privilegeMap.size();
   }

   /**
    * @return An iterator for the contained privilege objects.
    */
   public Iterator getPrivilegeIterator()
   {
      return m_privilegeMap.valueIterator();
   }

   /**
    * @see nexj.core.meta.Metadata#getPrimitivePrivilege(java.lang.String)
    */
   public PrimitivePrivilege getPrimitivePrivilege(String sName)
   {
      Privilege privilege = (Privilege) m_privilegeMap.get(sName);

      if (privilege != null && privilege.isPrimitive())
      {
         return (PrimitivePrivilege)privilege;
      }

      throw new MetadataLookupException("err.meta.privilegeLookup", sName, this);
   }

   /**
    * @see nexj.core.meta.Metadata#getPrimitivePrivilegeCount()
    */
   public int getPrimitivePrivilegeCount()
   {
      return m_nPrimitivePrivilegeCount;
   }

   /**
    * @see nexj.core.meta.Metadata#createPrivilegeSet()
    */
   public PrivilegeSet createPrivilegeSet()
   {
      return new PrivilegeSet(m_nPrimitivePrivilegeCount);
   }

   /**
    * Adds a new public symbol to the metadata.
    * @param publicSymbol The public symbol to add.
    */
   public void addPublicSymbol(Symbol publicSymbol)
   {
      verifyNotReadOnly();
      m_publicSymbolSet.add(publicSymbol);
   }

   /**
    * Checks is a symbol is public.
    * @param symbol The symbol to check.
    * @return True is the symbol is public.
    */
   public boolean isPublicSymbol(Symbol symbol)
   {
      return m_publicSymbolSet.contains(symbol);
   }

   /**
    * @see nexj.core.meta.Metadata#addClientSymbol(nexj.core.scripting.Symbol)
    */
   public void addClientSymbol(Symbol symbol)
   {
      verifyNotReadOnly();
      m_clientSymbolSet.add(symbol);
   }

   /**
    * @see nexj.core.meta.Metadata#removeClientSymbol(nexj.core.scripting.Symbol)
    */
   public void removeClientSymbol(Symbol symbol)
   {
      verifyNotReadOnly();
      m_clientSymbolSet.remove(symbol);
   }

   /**
    * @see nexj.core.meta.Metadata#isClientSymbol(nexj.core.scripting.Symbol)
    */
   public boolean isClientSymbol(Symbol symbol)
   {
      return m_clientSymbolSet.contains(symbol);
   }

   /**
    * @see nexj.core.meta.Metadata#getClientSymbolIterator()
    */
   public Iterator getClientSymbolIterator()
   {
      return m_clientSymbolSet.iterator();
   }

   /**
    * @see nexj.core.meta.Metadata#addSessionSymbol(nexj.core.scripting.Symbol)
    */
   public void addSessionSymbol(Symbol symbol)
   {
      m_sessionSymbolSet.add(symbol);
   }

   /**
    * @see nexj.core.meta.Metadata#getSessionSymbolIterator()
    */
   public Iterator getSessionSymbolIterator()
   {
      return m_sessionSymbolSet.iterator();
   }

   /**
    * Adds a new class to the metadata.
    * @param metaclass The class to add.
    * @throws MetadataException if a class
    * with the same name already exists.
    */
   public void addMetaclass(Metaclass metaclass)
   {
      verifyNotReadOnly();

      if (m_classAspectMap.contains(metaclass.getName()))
      {
         throw new MetadataException("err.meta.metaclassDup", new Object[]{metaclass.getName(), getName()});
      }

      Object oldMetaclass = m_metaclassMap.put(metaclass.getName(), metaclass);

      if (oldMetaclass != null)
      {
         m_metaclassMap.put(metaclass.getName(), oldMetaclass);

         throw new MetadataException("err.meta.metaclassDup", new Object[]{metaclass.getName(), getName()});
      }

      metaclass.setMetadata(this);
   }

   /**
    * @see nexj.core.meta.Metadata#defineMetaclass(java.lang.String, MetadataObject)
    */
   public Metaclass defineMetaclass(String sName, MetadataObject referrer)
   {
      Metaclass metaclass = findMetaclass(sName);

      if (metaclass == null)
      {
         metaclass = new Metaclass(sName);
         metaclass.setForward(true);
         addMetaclass(metaclass);
         m_globalEnv.defineVariable(sName, metaclass);
      }

      metaclass.addReferrer(referrer);

      return metaclass;
   }

   /**
    * @see nexj.core.meta.Metadata#findMetaclass(java.lang.String)
    */
   public Metaclass findMetaclass(String sName)
   {
      return (Metaclass)m_metaclassMap.get(sName);
   }

   /**
    * Gets a class by name.
    * @param sName The class name.
    * @return The class object.
    * @throws MetadataLookupException if the class does not exist.
    */
   public Metaclass getMetaclass(String sName)
   {
      Metaclass metaclass = (Metaclass)m_metaclassMap.get(sName);

      if (metaclass != null)
      {
         return metaclass;
      }

      throw new MetadataLookupException("err.meta.metaclassLookup", sName, this);
   }

   /**
    * @return The class count.
    */
   public int getMetaclassCount()
   {
      return m_metaclassMap.size();
   }

   /**
    * @return An iterator for the contained class objects.
    */
   public Iterator getMetaclassIterator()
   {
      return m_metaclassMap.valueIterator();
   }

   /**
    * Adds a new class aspect to the metadata.
    * @param classAspect The class aspect to add.
    * @throws MetadataException if a class aspect
    * with the same name already exists.
    */
   public void addClassAspect(ClassAspect classAspect)
   {
      verifyNotReadOnly();

      if (m_metaclassMap.contains(classAspect.getName()))
      {
         throw new MetadataException("err.meta.classAspectDup", new Object[]{classAspect.getName(), getName()});
      }

      Object oldClassAspect = m_classAspectMap.put(classAspect.getName(), classAspect);

      if (oldClassAspect != null)
      {
         m_classAspectMap.put(classAspect.getName(), oldClassAspect);

         throw new MetadataException("err.meta.classAspectDup", new Object[]{classAspect.getName(), getName()});
      }

      classAspect.setMetadata(this);

      int i;

      for (i = m_classAspectList.size(); i > 0; --i)
      {
         if (Named.COMPARATOR.compare(m_classAspectList.get(i - 1), classAspect) <= 0)
         {
            break;
         }
      }

      m_classAspectList.add(i, classAspect);
   }

   /**
    * @see nexj.core.meta.Metadata#defineClassAspect(java.lang.String, MetadataObject)
    */
   public ClassAspect defineClassAspect(String sName, MetadataObject referrer)
   {
      ClassAspect aspect = findClassAspect(sName);

      if (aspect == null)
      {
         aspect = new ClassAspect(sName);
         aspect.setForward(true);
         addClassAspect(aspect);
      }

      aspect.addReferrer(referrer);

      return aspect;
   }

   /**
    * Gets a class aspect by name.
    * @param sName The class aspect name.
    * @return The class aspect object.
    * @throws MetadataLookupException if the class aspect does not exist.
    */
   public ClassAspect getClassAspect(String sName)
   {
      ClassAspect classAspect = (ClassAspect)m_classAspectMap.get(sName);

      if (classAspect != null)
      {
         return classAspect;
      }

      throw new MetadataLookupException("err.meta.classAspectLookup", sName, this);
   }

   /**
    * Finds a class aspect by name.
    * @param sName The class aspect name.
    * @return The class aspect object, or null if not found.
    */
   public ClassAspect findClassAspect(String sName)
   {
      return (ClassAspect)m_classAspectMap.get(sName);
   }

   /**
    * @return The class aspect count.
    */
   public int getClassAspectCount()
   {
      return m_classAspectMap.size();
   }

   /**
    * @return An iterator for the contained class aspect objects.
    */
   public Iterator getClassAspectIterator()
   {
      return m_classAspectList.iterator();
   }

   /**
    * Adds a new flow macro to the metadata.
    * @param flowMacro The flow macro to add.
    * @throws MetadataException if a flow macro
    * with the same name already exists.
    */
   public void addFlowMacro(FlowMacro flowMacro)
   {
      verifyNotReadOnly();

      Object oldFlowMacro = m_flowMacroMap.put(flowMacro.getName(), flowMacro);

      if (oldFlowMacro != null)
      {
         m_flowMacroMap.put(flowMacro.getName(), oldFlowMacro);

         throw new MetadataException("err.meta.flowMacroDup", new Object[]
         {
            flowMacro.getName(),
            getName()
         });
      }

      flowMacro.setMetadata(this);
   }

   /**
    * Gets a flow macro by name.
    * @param sName The flow macro name.
    * @return The flow macro object.
    * @throws MetadataLookupException if the flow macro does not exist.
    */
   public FlowMacro getFlowMacro(String sName)
   {
      FlowMacro flowMacro = (FlowMacro) m_flowMacroMap.get(sName);

      if (flowMacro != null)
      {
         return flowMacro;
      }

      throw new MetadataLookupException("err.meta.flowMacroLookup", sName, this);
   }

   /**
    * Finds a flow macro by name.
    * @param sName The flow macro name.
    * @return The flow macro object, or null if not found.
    */
   public FlowMacro findFlowMacro(String sName)
   {
      return (FlowMacro) m_flowMacroMap.get(sName);
   }

   /**
    * @return The flow macro count.
    */
   public int getFlowMacroCount()
   {
      return m_flowMacroMap.size();
   }

   /**
    * @return An iterator for the contained flow macro objects.
    */
   public Iterator getFlowMacroIterator()
   {
      return m_flowMacroMap.valueIterator();
   }

   /**
    * Adds a new workflow to the metadata.
    * @param workflow The workflow to add.
    * @throws MetadataException if a workflow
    * with the same name already exists.
    */
   public void addWorkflow(Workflow workflow)
   {
      verifyNotReadOnly();

      Workflow oldWorkflow = (Workflow)m_workflowMap.put(workflow.getName(), Primitive.createInteger(workflow.getVersion()), workflow);

      if (oldWorkflow != null)
      {
         m_workflowMap.put(workflow.getName(), Primitive.createInteger(workflow.getVersion()), oldWorkflow);
         throw new MetadataException("err.meta.workflowDup",
            new Object[]{workflow.getFullName(), getName()});
      }

      oldWorkflow = (Workflow)m_currentWorkflowMap.put(workflow.getName(), workflow);

      if (oldWorkflow != null && workflow.getVersion() < oldWorkflow.getVersion())
      {
         m_currentWorkflowMap.put(workflow.getName(), oldWorkflow);
      }
   }

   /**
    * @see nexj.core.meta.Metadata#findWorkflow(java.lang.String, int)
    */
   public Workflow findWorkflow(String sName, int nVersion)
   {
      return (Workflow)m_workflowMap.get(sName, Primitive.createInteger(nVersion));
   }

   /**
    * Gets a workflow by name.
    * @param sName The workflow name.
    * @return The workflow object.
    * @throws MetadataLookupException if the workflow does not exist.
    */
   public Workflow getWorkflow(String sName, int nVersion)
   {
      Workflow workflow = (Workflow)m_workflowMap.get(sName, Primitive.createInteger(nVersion));

      if (workflow != null)
      {
         return workflow;
      }

      throw new MetadataLookupException("err.meta.workflowLookup", sName + "." + nVersion, this);
   }

   /**
    * @see nexj.core.meta.Metadata#getWorkflow(java.lang.String)
    */
   public Workflow getWorkflow(String sName)
   {
      Workflow workflow = (Workflow)m_currentWorkflowMap.get(sName);

      if (workflow != null)
      {
         return workflow;
      }

      throw new MetadataLookupException("err.meta.workflowLookup", sName, this);
   }

   /**
    * @return The workflow count.
    */
   public int getWorkflowCount()
   {
      return m_workflowMap.size();
   }

   /**
    * @return An iterator for the contained workflow objects.
    */
   public Iterator getWorkflowIterator()
   {
      return m_workflowMap.valueIterator();
   }

   /**
    * Adds a new channel type to the metadata.
    * @param channelType The channel type to add.
    * @throws MetadataException if a channel type
    * with the same name already exists.
    */
   public void addChannelType(ChannelType channelType)
   {
      verifyNotReadOnly();

      Object oldChannelType = m_channelTypeMap.put(channelType.getName(), channelType);

      if (oldChannelType != null)
      {
         m_channelTypeMap.put(channelType.getName(), oldChannelType);

         throw new MetadataException("err.meta.channelTypeDup", new Object[]
         {
            channelType.getName(),
            getName()
         });
      }

      channelType.setMetadata(this);
   }

   /**
    * Gets a channel type by name.
    * @param sName The channel type name.
    * @return The channel type object.
    * @throws MetadataLookupException if the channel type does not exist.
    */
   public ChannelType getChannelType(String sName)
   {
      ChannelType channelType = (ChannelType) m_channelTypeMap.get(sName);

      if (channelType != null)
      {
         return channelType;
      }

      throw new MetadataLookupException("err.meta.channelTypeLookup", sName, this);
   }

   /**
    * @return The channel type count.
    */
   public int getChannelTypeCount()
   {
      return m_channelTypeMap.size();
   }

   /**
    * @return An iterator for the contained channel type objects.
    */
   public Iterator getChannelTypeIterator()
   {
      return m_channelTypeMap.valueIterator();
   }

   /**
    * Adds a new channel type XML element mapping to the metadata.
    * @param sElement The XML element name.
    * @param channelType The channel type object.
    * @throws MetadataException if a duplicate entry is encountered.
    */
   public void addChannelTypeElement(String sElement, ChannelType channelType)
   {
      verifyNotReadOnly();

      m_channelTypeElementMap.put(channelType, sElement);

      if (sElement.equals("Channel"))
      {
         return;
      }

      Object oldChannelType = m_channelTypeElementMap.put(sElement, channelType);

      if (oldChannelType != null)
      {
         m_channelTypeElementMap.put(sElement, oldChannelType);

         throw new MetadataException("err.meta.channelTypeElementDup", new Object[]
         {
            sElement,
            channelType.getName(),
            getName()
         });
      }
   }

   /**
    * Gets a channel type by element name.
    * @param element The channel type element.
    * @return The channel type object.
    * @throws MetadataLookupException if the channel type does not exist.
    */
   public ChannelType getChannelTypeByElement(Element element)
   {
      if (element.getNodeName().equals("Channel"))
      {
         return getChannelType(XMLUtil.getReqStringAttr(element, "type"));
      }

      ChannelType channelType = (ChannelType)m_channelTypeMap.get(element.getNodeName());

      if (channelType != null)
      {
         return channelType;
      }

      throw new MetadataLookupException("err.meta.channelTypeElementLookup", element.getNodeName(), this);
   }

   /**
    * Gets a channel type element name.
    * @param type The channel type.
    */
   public String getChannelTypeElement(ChannelType type)
   {
      return (String)m_channelTypeElementMap.get(type);
   }

   /**
    * Adds a new channel to the metadata.
    * @param channel The channel to add.
    * @throws MetadataException if a channel
    * with the same name already exists.
    */
   public void addChannel(Channel channel)
   {
      verifyNotReadOnly();

      Object oldChannel = m_channelMap.put(channel.getName(), channel);

      if (oldChannel != null)
      {
         m_channelMap.put(channel.getName(), oldChannel);

         throw new MetadataException("err.meta.channelDup", new Object[]
         {
            channel.getName(),
            getName()
         });
      }
   }

   /**
    * Gets a channel by name.
    * @param sName The channel name.
    * @return The channel object.
    * @throws MetadataLookupException if the channel does not exist.
    */
   public Channel getChannel(String sName)
   {
      Channel channel = findChannel(sName);

      if (channel != null)
      {
         return channel;
      }

      throw new MetadataLookupException("err.meta.channelLookup", sName, this);
   }

   /**
    * Finds a channel by name.
    * @param sName The channel name.
    * @return The channel object, null if not found.
    */
   public Channel findChannel(String sName)
   {
      return (Channel) m_channelMap.get(sName);
   }

   /**
    * @return The channel count.
    */
   public int getChannelCount()
   {
      return m_channelMap.size();
   }

   /**
    * @return An iterator for the contained channel objects.
    */
   public Iterator getChannelIterator()
   {
      return m_channelMap.valueIterator();
   }

   /**
    * @return The dump file resource map.
    */
   public Lookup getDumpResourceMap()
   {
      return m_dumpResMap;
   }

   /**
    * @see nexj.core.meta.Metadata#getFile()
    */
   public Binary getFile(String sName)
   {
      InputStream istream = null;
      ByteArrayOutputStream dataStream = null;

      try
      {
         if (sName != null && sName.length() > 0 && !sName.contains("..")  && !sName.startsWith("/") && !sName.startsWith("\\"))
         {
            XMLMetadataHelper helper = getHelper();

            istream = helper.getResourceAsStream("files/" + sName);
            dataStream = new ByteArrayOutputStream();

            IOUtil.copy(dataStream, istream);

            return new Binary(dataStream.toByteArray());
         }

         throw new MetadataValidationException("err.meta.invalidFilePath", new Object[] { sName });
      }
      catch (Exception e)
      {
         throw new MetadataException("err.meta.fileLookup", new Object[] { sName, this }, e);
      }
      finally
      {
         IOUtil.close(istream);
      }
   }

   /**
    * Adds a new message format to the metadata.
    * @param format The message format to add.
    * @throws MetadataException if a message format
    * with the same name already exists.
    */
   public void addFormat(Format format)
   {
      verifyNotReadOnly();

      Object oldMessageFormat = m_formatMap.put(format.getName(), format);

      if (oldMessageFormat != null)
      {
         m_formatMap.put(format.getName(), oldMessageFormat);

         throw new MetadataException("err.meta.messageFormatDup", new Object[]
         {
            format.getName(),
            getName()
         });
      }

      format.setMetadata(this);
   }

   /**
    * Gets a message format by name.
    * @param sName The message format name.
    * @return The message format object.
    * @throws MetadataLookupException if the message format does not exist.
    */
   public Format getFormat(String sName)
   {
      Format messageFormat = (Format) m_formatMap.get(sName);

      if (messageFormat != null)
      {
         return messageFormat;
      }

      throw new MetadataLookupException("err.meta.messageFormatLookup", sName, this);
   }

   /**
    * @return The message format count.
    */
   public int getFormatCount()
   {
      return m_formatMap.size();
   }

   /**
    * @return An iterator for the contained message format objects.
    */
   public Iterator getFormatIterator()
   {
      return m_formatMap.valueIterator();
   }

   /**
    * Adds a new message to the metadata.
    * @param message The message to add.
    * @throws MetadataException if a message
    * with the same name already exists.
    */
   public void addMessage(Message message)
   {
      verifyNotReadOnly();

      Object oldMessage = m_messageMap.put(message.getName(), message);

      if (oldMessage != null)
      {
         m_messageMap.put(message.getName(), oldMessage);

         throw new MetadataException("err.meta.messageDup", new Object[]
         {
            message.getName(),
            getName()
         });
      }

      message.setMetadata(this);
   }

   /**
    * @see nexj.core.meta.Metadata#findMessage(java.lang.String)
    */
   public Message findMessage(String sName)
   {
      return (Message)m_messageMap.get(sName);
   }

   /**
    * Gets a message by name.
    * @param sName The message name.
    * @return The message object.
    * @throws MetadataLookupException if the message does not exist.
    */
   public Message getMessage(String sName)
   {
      Message message = (Message) m_messageMap.get(sName);

      if (message != null)
      {
         return message;
      }

      throw new MetadataLookupException("err.meta.messageLookup", sName, this);
   }

   /**
    * @return The message count.
    */
   public int getMessageCount()
   {
      return m_messageMap.size();
   }

   /**
    * @return An iterator for the contained message objects.
    */
   public Iterator getMessageIterator()
   {
      return m_messageMap.valueIterator();
   }

   /**
    * Adds a new transformation to the metadata.
    * @param transformation The transformation to add.
    * @throws MetadataException if a transformation
    * with the same name already exists.
    */
   public void addTransformation(Transformation transformation)
   {
      verifyNotReadOnly();

      Object oldTransformation = m_transformationMap.put(transformation.getName(), transformation);

      if (oldTransformation != null)
      {
         m_transformationMap.put(transformation.getName(), oldTransformation);

         throw new MetadataException("err.meta.transformationDup", new Object[]
         {
            transformation.getName(),
            getName()
         });
      }
   }

   /**
    * Gets a transformation by name.
    * @param sName The transformation name.
    * @return The transformation object.
    * @throws MetadataLookupException if the transformation does not exist.
    */
   public Transformation getTransformation(String sName)
   {
      Transformation transformation = (Transformation) m_transformationMap.get(sName);

      if (transformation != null)
      {
         return transformation;
      }

      throw new MetadataLookupException("err.meta.transformationLookup", sName, this);
   }

   /**
    * @see nexj.core.meta.Metadata#findTransformation(java.lang.String)
    */
   public Transformation findTransformation(String sName)
   {
      return (Transformation) m_transformationMap.get(sName);
   }

   /**
    * @return The transformation count.
    */
   public int getTransformationCount()
   {
      return m_transformationMap.size();
   }

   /**
    * @return An iterator for the contained transformation objects.
    */
   public Iterator getTransformationIterator()
   {
      return m_transformationMap.valueIterator();
   }

   /**
    * Adds a new service interface to the metadata.
    * @param iface The service interface to add.
    * @throws MetadataException if a service interface
    * with the same name already exists.
    */
   public void addInterface(Interface iface)
   {
      verifyNotReadOnly();

      Object oldInterface = m_interfaceMap.put(iface.getName(), iface);

      if (oldInterface != null)
      {
         m_interfaceMap.put(iface.getName(), oldInterface);

         throw new MetadataException("err.meta.interfaceDup", new Object[]
         {
            iface.getName(),
            getName()
         });
      }

      iface.setMetadata(this);
   }

   /**
    * @see nexj.core.meta.Metadata#defineInterface(java.lang.String, MetadataObject)
    */
   public Interface defineInterface(String sName, MetadataObject referrer)
   {
      Interface iface = (Interface)m_interfaceMap.get(sName);

      if (iface == null)
      {
         iface = new Interface(sName);
         iface.setForward(true);
         addInterface(iface);
      }

      iface.addReferrer(referrer);

      return iface;
   }

   /**
    * Gets a service interface by name.
    * @param sName The service interface name.
    * @return The service interface object.
    * @throws MetadataLookupException if the service interface does not exist.
    */
   public Interface getInterface(String sName)
   {
      Interface iface = (Interface) m_interfaceMap.get(sName);

      if (iface != null)
      {
         return iface;
      }

      throw new MetadataLookupException("err.meta.interfaceLookup", sName, this);
   }

   /**
    * @return The service interface count.
    */
   public int getInterfaceCount()
   {
      return m_interfaceMap.size();
   }

   /**
    * @return An iterator for the contained service interface objects.
    */
   public Iterator getInterfaceIterator()
   {
      return m_interfaceMap.valueIterator();
   }

   /**
    * @see Metadata#addService(Service)
    */
   public void addService(Service service)
   {
      verifyNotReadOnly();

      Service oldService = (Service)m_serviceMap.put(service.getName(), Primitive.createInteger(service.getVersion()), service);

      if (oldService != null)
      {
         m_serviceMap.put(service.getName(), Primitive.createInteger(service.getVersion()), oldService);
         throw new MetadataException("err.meta.serviceDup",
            new Object[]{service.getFullName(), getName()});
      }

      oldService = (Service)m_currentServiceMap.put(service.getName(), service);

      if (oldService != null && service.getVersion() < oldService.getVersion())
      {
         m_currentServiceMap.put(service.getName(), oldService);
      }
   }

   /**
    * @see Metadata#findService(String, int)
    */
   public Service findService(String sName, int nVersion)
   {
      return (Service)m_serviceMap.get(sName, Primitive.createInteger(nVersion));
   }

   /**
    * @see Metadata#getService(String, int)
    */
   public Service getService(String sName, int nVersion)
   {
      Service service = (Service)m_serviceMap.get(sName, Primitive.createInteger(nVersion));

      if (service != null)
      {
         return service;
      }

      throw new MetadataLookupException("err.meta.serviceLookup", sName + "." + nVersion, this);
   }

   /**
    * @see Metadata#getService(String)
    */
   public Service getService(String sName)
   {
      Service service = (Service)m_currentServiceMap.get(sName);

      if (service != null)
      {
         return service;
      }

      throw new MetadataLookupException("err.meta.serviceLookup", sName, this);
   }

   /**
    * @return The service count.
    */
   public int getServiceCount()
   {
      return m_serviceMap.size();
   }

   /**
    * @see Metadata#getServiceIterator()
    */
   public Iterator getServiceIterator()
   {
      return m_serviceMap.valueIterator();
   }

   /**
    * Adds a new component to the metadata.
    * @param component The component to add.
    * @throws MetadataException if a component
    * with the same name already exists.
    */
   public void addComponent(Component component)
   {
      verifyNotReadOnly();

      Object oldComponent = m_componentMap.put(component.getName(), component);

      if (oldComponent != null)
      {
         m_componentMap.put(component.getName(), oldComponent);

         throw new MetadataException("err.meta.componentDup", new Object[] { component.getName(), getName()});
      }

      component.setMetadata(this);
   }

   /**
    * Gets a component by name.
    * @param sName The component name.
    * @return The component object.
    * @throws MetadataLookupException if the component does not exist.
    */
   public Component getComponent(String sName)
   {
      Component component = (Component)m_componentMap.get(sName);

      if (component != null)
      {
         return component;
      }

      throw new MetadataLookupException("err.meta.componentLookup", sName, this);
   }

   /**
    * Finds a component by name.
    * @param sName The name of the component.
    * @return The component or null if not found.
    */
   public Component findComponent(String sName)
   {
      return (Component)m_componentMap.get(sName);
   }

   /**
    * @return The component count.
    */
   public int getComponentCount()
   {
      return m_componentMap.size();
   }

   /**
    * @return An iterator for the contained component objects.
    */
   public Iterator getComponentIterator()
   {
      return m_componentMap.valueIterator();
   }

   /**
    * @see nexj.core.meta.Metadata#addExternalLibrary(nexj.core.meta.ExternalLibrary)
    */
   public void addExternalLibrary(ExternalLibrary externalLibrary)
   {
      verifyNotReadOnly();

      Object oldExternalLibrary = m_externalLibraryMap.put(externalLibrary.getName(), externalLibrary);

      if (oldExternalLibrary != null)
      {
         m_externalLibraryMap.put(externalLibrary.getName(), oldExternalLibrary);

         throw new MetadataException("err.meta.externalLibraryDup", new Object[]
         {
            externalLibrary.getName(),
            getName()
         });
      }

      externalLibrary.setMetadata(this);
   }

   /**
    * @see nexj.core.meta.Metadata#getExternalLibrary(java.lang.String)
    */
   public ExternalLibrary getExternalLibrary(String sName)
   {
      ExternalLibrary externalLibrary = (ExternalLibrary)m_externalLibraryMap.get(sName);

      if (externalLibrary != null)
      {
         return externalLibrary;
      }

      throw new MetadataLookupException("err.meta.externalLibraryLookup", sName, this);
   }

   /**
    * @see nexj.core.meta.Metadata#getExternalLibraryCount()
    */
   public int getExternalLibraryCount()
   {
      return m_externalLibraryMap.size();
   }

   /**
    * @see nexj.core.meta.Metadata#getExternalLibraryIterator()
    */
   public Iterator getExternalLibraryIterator()
   {
      return m_externalLibraryMap.valueIterator();
   }

   /**
    * @return The upgrade resource map.
    */
   public Lookup getUpgradeResourceMap()
   {
      return m_upgradeResMap;
   }

   /**
    * Adds an upgrade resource path.
    * @param sName The upgrade name.
    * @param sPath The upgrade path.
    * @throws MetadataException if the named test already exists.
    */
   public void addUpgrade(String sName, String sPath)
   {
      verifyNotReadOnly();

      Object oldPath = m_upgradeResMap.put(sName, sPath);

      if (oldPath != null)
      {
         m_upgradeResMap.put(sName, oldPath);

         throw new MetadataException("err.meta.upgradeDup", new Object[] {sName, getName()});
      }
   }

   /**
    * @see nexj.core.meta.Metadata#getUpgrade(String)
    */
   public Upgrade getUpgrade(String sName)
   {
      String sResource = (String)m_upgradeResMap.get(sName);

      if (sResource == null)
      {
         throw new MetadataLookupException("err.meta.upgradeLookup", sName, this);
      }

      final Upgrade[] upgradeArray = new Upgrade[1];
      final XMLMetadataHelper helper = getHelper();
      final XMLUpgradeMetadataLoader loader = new XMLUpgradeMetadataLoader(this, helper);

      helper.loadResource(
         sResource, sName, helper.new UpgradingResourceCharacterStreamHandler(new ResourceHandler()
      {
         public void handleResource(Element element, String sName)
         {
            upgradeArray[0] = loader.loadUpgrade(sName, element);
         }
      }));

      if (m_validationHelper == null)
      {
         helper.checkForError();
      }

      return upgradeArray[0];
   }

   /**
    * @see nexj.core.meta.Metadata#addLocale(java.lang.String)
    */
   public Locale addLocale(String sName)
   {
      verifyNotReadOnly();

      Locale locale = LocaleUtil.parse(sName);
      Object oldLocale = m_localeMap.put(sName, locale);

      if (oldLocale != null)
      {
         m_localeMap.put(sName, oldLocale);

         throw new MetadataException("err.meta.localeDup", new Object[] {sName, getName()});
      }

      return locale;
   }

   /**
    * @see nexj.core.meta.ContextMetadata#isLocaleSupported(java.lang.String)
    */
   public boolean isLocaleSupported(String sLocale)
   {
      return m_localeMap.contains(sLocale);
   }

   /**
    * @see nexj.core.meta.ContextMetadata#getLocaleName(java.lang.String)
    */
   public String getLocaleName(String sLocale)
   {
      if (sLocale == null)
      {
         sLocale = DEFAULT_LOCALE;
      }

      for (;;)
      {
         if (m_localeMap.contains(sLocale) ||
            sLocale == DEFAULT_LOCALE)
         {
            return sLocale;
         }

         sLocale = LocaleUtil.getBase(sLocale, DEFAULT_LOCALE);
      }
   }

   /**
    * @see nexj.core.meta.ContextMetadata#getLocale(java.lang.String)
    */
   public Locale getLocale(String sLocale)
   {
      Locale locale;

      if (sLocale == null)
      {
         sLocale = DEFAULT_LOCALE;
      }

      for (;;)
      {
         locale = (Locale)m_localeMap.get(sLocale);

         if (locale != null)
         {
            return locale;
         }

         if (sLocale == DEFAULT_LOCALE)
         {
            return LocaleUtil.parse(DEFAULT_LOCALE);
         }

         sLocale = LocaleUtil.getBase(sLocale, DEFAULT_LOCALE);
      }
   }

   /**
    * @see nexj.core.meta.ContextMetadata#getLocaleCount()
    */
   public int getLocaleCount()
   {
      return m_localeMap.size();
   }

   /**
    * @see nexj.core.meta.ContextMetadata#getLocaleIterator()
    */
   public Iterator getLocaleIterator()
   {
      return m_localeMap.valueIterator();
   }

   /**
    * Add String resource path.
    * @param sName Locale name.
    * @param sPath String resource path.
    */
   public void addString(String sLocale, String sPath)
   {
      verifyNotReadOnly();

      Lookup map = (Lookup)m_stringMap.get(sLocale);

      if (map == null)
      {
         map = new HashTab();
         m_stringMap.put(sLocale, map);
      }

      Object oldPath = map.put(sPath, sPath);

      if (oldPath != null)
      {
         throw new MetadataException("err.meta.stringDup", new Object[] {sLocale, getName()});
      }
   }

   /**
    * Gets the string table for the specified locale.
    * @param sLocale The locale name.
    * @return The string table.
    * @throws MetadataException if the string resource
    * for the default locale was not found.
    */
   public StringTable getStringTable(String sLocale)
   {
      String sInitialLocale = sLocale;
      Object obj = null;

      synchronized (m_stringMetaMap)
      {
         while (sLocale.length() > 0)
         {
            if (sLocale.charAt(sLocale.length() - 1) == '_')
            {
               sLocale = sLocale.substring(0, sLocale.length() - 1);

               continue;
            }

            obj = m_stringMetaMap.get(sLocale);

            if (obj != null)
            {
               break;
            }

            int i = sLocale.lastIndexOf('_');

            if (i > 0)
            {
               sLocale = sLocale.substring(0, i);
            }
            else
            {
               break;
            }
         }

         if (obj == null && !sLocale.equals(DEFAULT_LOCALE))
         {
            sLocale = DEFAULT_LOCALE;
            obj = m_stringMetaMap.get(sLocale);
         }

         if (obj != null && obj != Undefined.VALUE)
         {
            // Comparison by reference is sufficient here
            if (sLocale != sInitialLocale)
            {
               m_stringMetaMap.put(sInitialLocale, obj);
            }

            return (StringTable)obj;
         }
      }

      StringTable stringTable = null;
      int i = sLocale.lastIndexOf('_');

      if (i > 0)
      {
         stringTable = getStringTable(sLocale.substring(0, i));
      }
      else if (!sLocale.equals(DEFAULT_LOCALE))
      {
         stringTable = getStringTable(DEFAULT_LOCALE);
      }

      InputStream in = null;
      Properties properties = new Properties();
      URL sysURL = null;

      // Load the system string table

      try
      {
         sysURL = SystemResources.find("client." + sLocale + ".strings");

         if (sysURL != null)
         {
            in = URLUtil.openStream(sysURL);
            properties.load(in);
         }
      }
      catch (Exception e)
      {
         if (in != null)
         {
            throw new MetadataException("err.meta.stringTableLoad", new Object[]{sysURL.toString()}, e);
         }
      }
      finally
      {
         IOUtil.close(in);
         in = null;
      }

      try
      {
         sysURL = SystemResources.find("server." + sLocale + ".strings");

         if (sysURL != null)
         {
            in = URLUtil.openStream(sysURL);
            properties.load(in);
         }
      }
      catch (Exception e)
      {
         if (in != null)
         {
            throw new MetadataException("err.meta.stringTableLoad", new Object[]{sysURL.toString()}, e);
         }
      }
      finally
      {
         IOUtil.close(in);
         in = null;
      }

      try
      {
         sysURL = SystemResources.find(sLocale + ".strings");

         if (sysURL != null)
         {
            in = URLUtil.openStream(sysURL);
            properties.load(in);
         }
      }
      catch (Exception e)
      {
         if (in != null)
         {
            throw new MetadataException("err.meta.stringTableLoad", new Object[]{sysURL.toString()}, e);
         }
      }
      finally
      {
         IOUtil.close(in);
         in = null;
      }

      // Overwrite the system string table with the repository string table

      Lookup map = (Lookup)m_stringMap.get(sLocale);
      String sResName = null;

      if (map != null)
      {
         try
         {
            for (Iterator itr = map.iterator(); itr.hasNext();)
            {
               sResName = (String)itr.next();
               in = getHelper().getResourceAsStream(sResName);
               properties.load(in);
               in.close();
               in = null;
            }
         }
         catch (IOException e)
         {
            throw new MetadataException("err.meta.stringTableLoad", new Object[]{sResName}, e);
         }
         finally
         {
            IOUtil.close(in);
         }
      }

      if (!properties.isEmpty())
      {
         stringTable = new StringTable(properties, stringTable, LocaleUtil.parse(sLocale));
      }

      synchronized (m_stringMetaMap)
      {
         Object oldMap = m_stringMetaMap.put(sLocale, stringTable);

         if (oldMap != null && oldMap != Undefined.VALUE)
         {
            m_stringMetaMap.put(sLocale, oldMap);

            stringTable = (StringTable)oldMap;
         }
      }

      return stringTable;
   }

   /**
    * @see nexj.core.meta.ui.UIMetadata#getClassMeta(java.lang.String)
    */
   public ClassMeta getClassMeta(String sName)
   {
      return getMetaclass(sName);
   }

   /**
    * @see nexj.core.meta.ui.UIMetadata#findClassMeta(java.lang.String)
    */
   public ClassMeta findClassMeta(String sName)
   {
      return findMetaclass(sName);
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      m_listing.makeReadOnly();
      m_globalEnv.makeReadOnly();

      for (Iterator itr = getDataSourceTypeIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getDataSourceIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getPrivilegeIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getMetaclassIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getClassAspectIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getWorkflowIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getFlowMacroIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getChannelTypeIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getChannelIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getServiceIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getInterfaceIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getFormatIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getMessageIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getTransformationIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      for (Iterator itr = getComponentIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      super.makeReadOnly();
   }

   /**
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, nexj.core.util.ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      MetadataObject.validate(getMetaclassIterator(), this, warnings);
      MetadataObject.validate(getMessageIterator(), this, warnings);
      MetadataObject.validate(getInterfaceIterator(), this, warnings);
      MetadataObject.validate(getTransformationIterator(), this, warnings);
      MetadataObject.validate(getServiceIterator(), this, warnings);
      MetadataObject.validate(getWorkflowIterator(), this, warnings);
      MetadataObject.validate(getChannelIterator(), this, warnings);
      MetadataObject.validate(getDataSourceIterator(), this, warnings);

      if (warnings != null)
      {
         if (m_genericRPCPrivilege == null)
         {
            MetadataValidationException e = new MetadataValidationException(
               "err.meta.missingRPCPrivilege", new Object[]{m_sName});

            setProperties(e);
            e.setProperty("item", "rpcPrivilege");

            if (m_configURL != null)
            {
               e.setResourceName(m_configURL.toString());
            }

            warnings.addException(e);
         }
      }
   }

   /**
    * Checks the loaded metadata against a compatible model.
    * @param compatible The compatible model to check against.
    * @throws MetadataCompoundValidationException if any compatibility errors
    *    have been accumulated and a validation helper is not set.
    */
   public void checkCompatibility(Metadata compatible)
   {
      final XMLMetadataHelper helper = getHelper();

      for (Iterator itr = compatible.getMetaclassIterator(); itr.hasNext();)
      {
         final Metaclass compatibleMetaclass = (Metaclass)itr.next();
         Metaclass metaclass = findMetaclass(compatibleMetaclass.getName());

         ExceptionHolder eh;

         if (metaclass != null)
         {
            eh = new ExceptionHolder()
            {
               public void addException(Throwable e)
               {
                  if (e instanceof MetadataValidationException)
                  {
                     ((MetadataValidationException)e).setResourceName(compatibleMetaclass.getResourceName());
                  }

                  helper.getException().addException(e);
               }

               public int getExceptionCount()
               {
                  return helper.getException().getExceptionCount();
               }

               public Iterator getExceptionIterator()
               {
                  return helper.getException().getExceptionIterator();
               }
            };
         }
         else
         {
            eh = helper.getException();
         }

         compatibleMetaclass.checkCompatibility(metaclass, eh);
      }

      if (m_validationHelper == null)
      {
         helper.checkForError();
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      marker.setTypeName("Metadata");
   }

   /**
    * @see nexj.core.meta.NamedMetadataObject#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(64);

      buf.append("XMLMetadata \"");
      buf.append(m_sName);
      buf.append("\"");

      if (m_sRevision != null)
      {
         buf.append(' ');
         buf.append(m_sRevision);
      }

      return buf.toString();
   }

   /**
    * @return A list of resources that are potential metadata upgrade classes.
    */
   public List getUpgradeResources()
   {
      List upgradeResourceList = new ArrayList();

      try
      {
         URLUtil.addResourceNames(upgradeResourceList, XMLMetadata.class, METADATA_UPGRADE_PATH);
      }
      catch (Exception e)
      {
         ObjUtil.rethrow(e);
      }

      return upgradeResourceList;
   }

   /**
    * @return An iterator with the names of unit test resources.
    */
   public Iterator getUnitTestNameIterator()
   {
      return m_unitTestResMap.iterator();
   }

   /**
    * Adds a unit test resource path.
    *
    * @param sName The test name.
    * @param sPath The test path.
    * @throws MetadataException if the named test already exists.
    */
   public void addUnitTest(String sName, String sPath)
   {
      verifyNotReadOnly();

      Object oldPath = m_unitTestResMap.put(sName, sPath);

      if (oldPath != null)
      {
         m_unitTestResMap.put(sName, oldPath);

         throw new MetadataException("err.meta.testing.unit.unitTestDup", new Object[]
         {
            sName,
            getName()
         });
      }
   }

   /**
    * @see nexj.core.meta.EnterpriseMetadata#getUnitTest(java.lang.String)
    */
   public UnitTest getUnitTest(String sName)
   {
      UnitTest utest = null;

      synchronized (m_unitTestMap)
      {
         utest = (UnitTest)m_unitTestMap.get(sName);
      }

      if (utest != null)
      {
         return utest;
      }

      String sResource = (String)m_unitTestResMap.get(sName);

      if (sResource == null)
      {
         throw new MetadataLookupException("err.meta.testing.unit.unitTestLookup", sName, this);
      }

      final UnitTest[] utestArray = new UnitTest[1];
      final XMLMetadataHelper helper = getHelper();
      final XMLUnitTestMetadataLoader loader = new XMLUnitTestMetadataLoader(this, helper);

      helper.loadResource(sResource, sName, new ResourceHandler()
      {
         public void handleResource(Element element, String sName)
         {
            utestArray[0] = loader.unitTest(sName, element);
         }
      });

      if (m_validationHelper == null)
      {
         helper.checkForError();
      }

      utestArray[0].makeReadOnly();
      sName = utestArray[0].getName(); // reuse interned value

      synchronized (m_unitTestMap)
      {
         utest = (UnitTest)m_unitTestMap.put(sName, utestArray[0]);

         if (utest == null)
         {
            utest = utestArray[0];
         }
         else
         {
            m_unitTestMap.put(sName, utest);
         }
      }

      return utest;
   }

   /**
    * @return The unit test resource map.
    */
   public Lookup getUnitTestResourceMap()
   {
      return m_unitTestResMap;
   }

   /**
    * @see nexj.core.meta.Metadata#getProperties()
    */
   public Properties getProperties()
   {
      return m_properties;
   }

   /**
    * Sets the realmless flag.  True if kerberos-authenticated principal names do not include a realm.
    * @param bRealmless The realmless flag.  True if kerberos-authenticated principal names do not include a realm to set.
    */
   public void setRealmless(boolean bRealmless)
   {
      m_bRealmless = bRealmless;
   }

   /**
    * @return The realmless flag.  True if kerberos-authenticated principal names do not include a realm.
    */
   public boolean isRealmless()
   {
      return m_bRealmless;
   }

   /**
    * Sets the environment name.
    */
   public void setEnvironment(String sEnvironmentName)
   {
      m_sEnvironmentName = sEnvironmentName;
      m_sHTTPContextRoot = '/' + SysUtil.NAMESPACE + '/' + m_sEnvironmentName;
      m_sHTTPAnonymousContextRoot = '/' + SysUtil.NAMESPACE + '/' + m_sEnvironmentName + "/anon";
      m_sHTTPFormContextRoot = '/' + SysUtil.NAMESPACE + '/' + m_sEnvironmentName + "/form";
   }
   
   /**
    * @see nexj.core.meta.Metadata#getEnvironment()
    */
   public String getEnvironment()
   {
      return m_sEnvironmentName;
   }
}