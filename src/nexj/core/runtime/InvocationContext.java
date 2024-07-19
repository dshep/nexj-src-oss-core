// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.lang.ref.ReferenceQueue;
import java.security.Principal;
import java.security.PrivilegedAction;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Locale;
import java.util.TimeZone;

import javax.transaction.Status;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;

import nexj.core.meta.Attribute;
import nexj.core.meta.ContextMetadata;
import nexj.core.meta.Event;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.PrimitivePrivilege;
import nexj.core.meta.Privilege;
import nexj.core.meta.PrivilegeSet;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.persistence.OID;
import nexj.core.persistence.Query;
import nexj.core.scripting.Function;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.util.Cancellable;
import nexj.core.util.DuplicateItemException;
import nexj.core.util.HashTab;
import nexj.core.util.HashTab2D;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.ObjUtil;
import nexj.core.util.SoftHashTab;
import nexj.core.util.StringTable;
import nexj.core.util.UncheckedException;
import nexj.core.util.Undefined;
import nexj.core.util.auth.SimplePrincipal;

/**
 * The temporary state for each server request.
 */
public class InvocationContext implements Context, Cancellable
{
   // constants

   /**
    * The new (current) generation.
    */
   public final static byte GEN_NEW = 0;

   /**
    * The generation since the last life cycle event.
    */
   public final static byte GEN_PRE = 1;

   /**
    * Generation at the UOW start.
    */
   public final static byte GEN_OLD = 2;

   /**
    * Count of RPCs.
    */
   public final static int STAT_RPC = 0;

   /**
    * Count of persistence RPCs.
    */
   public final static int STAT_PERSIST = 1;

   /**
    * Count of lazy loads involving RPC.
    */
   public final static int STAT_LOAD = 2;

   /**
    * Count of all the statistic categories.
    */
   protected final static int STAT_COUNT = 3;

   // attributes

   /**
    * The invocation context locale name.
    */
   protected String m_sLocaleName;

   /**
    * The invocation context fragment name.
    */
   protected String m_sFragmentName;

   /**
    * The client address.
    */
   protected String m_sClientAddress;

   /**
    * The value retrieval generation, one of the GEN_* constants.
    */
   protected byte m_nGeneration;

   /**
    * The query security mode (one of Query.SEC_* constants).
    */
   protected byte m_nQuerySecurity = Query.SEC_NODE;

   /**
    * The secure access flag. If false, then the functional
    * and object-level security will be disabled.
    */
   protected boolean m_bSecure = true;

   /**
    * The visibility protection flag.
    */
   protected boolean m_bProtected = true;

   /**
    * The partitioning isolation flag.
    */
   protected boolean m_bPartitioned = true;

   /**
    * Whether debug and dump level logging should be suppressed for this invocation context.
    */
   protected boolean m_bStealth;

   /**
    * True to set the transient flag of the first UnitOfWork created in this context.
    */
   protected boolean m_bTransient;

   /**
    * The audit flag.
    */
   protected boolean m_bAudited = true;

   /**
    * True if the current UnitOfWork is global; false to skip instance/UnitOfWork affinity checks.
    */
   protected boolean m_bUOWGlobal = true;

   /**
    * The unit of work count.
    */
   protected int m_nUOWCount;

   /**
    * The default maximum number of changed instances in a unit of work (negative for unlimited).
    */
   protected int m_nMaxUOWChangeCount = 16384;

   /**
    * The query timeout.  If non-negative, overrides the datasource connection query timeout.
    * Unlike the datasource query timeout, the invocation context timeout also applies to locking
    * queries.
    */
   protected int m_nQueryTimeout = Query.TIMEOUT_AUTO;

   /**
    * The RPC count.
    */
   protected int m_nRPCCount;

   /**
    * The persistence RPC count.
    */
   protected int m_nPersistCount;

   /**
    * The lazy load count.
    */
   protected int m_nLoadCount;

   // associations

   /**
    * The root metadata object.
    */
   protected Metadata m_metadata;

   /**
    * The user metaclass.
    */
   protected Metaclass m_userClass;

   /**
    * The user principal.
    */
   protected Principal m_principal;

   /**
    * The user instance.
    */
   protected Instance m_user;

   /**
    * The invocation context partition instance.
    */
   protected Instance m_partition;

   /**
    * The scripting virtual machine associated with this context.
    */
   protected Machine m_machine;

   /**
    * The invocation context locale.
    */
   protected Locale m_locale = Locale.ENGLISH;

   /**
    * The invocation context time zone.
    */
   protected TimeZone m_timeZone;

   /**
    * The localized string table.
    */
   protected StringTable m_stringTable;

   /**
    * The transient cache map. Used to cache data for the duration of a single request.
    */
   protected Lookup m_cacheMap = new SoftHashTab(8);

   /**
    * The map of classes and OIDs to cached Instances: Instance[Metaclass][OID].
    */
   protected Lookup2D m_instanceRefMap;

   /**
    * The weak reference finalization queue (used with the instance map).
    */
   protected ReferenceQueue m_refq;

   /**
    * Map of metaclass to a map of instance to change state: Integer[Lookup[Metaclass]].
    * Used for accumulating all the changes that occur in a unit of work.
    */
   protected Lookup m_classChangeMap;

   /**
    * The transaction manager instance.
    */
   protected TransactionManager m_txManager;

   /**
    * The current unit of work.
    */
   protected UnitOfWork m_uow;

   /**
    * The audit unit of work.
    */
   protected UnitOfWork m_auditUOW;

   /**
    * The logger unit of work. It is always committed.
    */
   protected UnitOfWork m_loggerUOW;

   /**
    * The unit of work array.
    */
   protected UnitOfWork[] m_uowArray = new UnitOfWork[4];

   /**
    * The global data cache.
    */
   protected DataCache m_globalCache;

   /**
    * The current user privilege set.
    */
   protected PrivilegeSet m_privilegeSet;

   /**
    * The tester function: (lambda (uow commit? tx?) ... )
    */
   protected Function m_tester;

   /**
    * The current cancellable object.
    */
   protected Cancellable m_cancellable;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(InvocationContext.class);

   // constructors

   /**
    * Creates a default invocation context.
    * It must be initialized afterwards with the initialize() method.
    */
   public InvocationContext()
   {
   }

   /**
    * Creates an invocation context with the anonymous principal.
    * @param metadata The root metadata object.
    */
   public InvocationContext(Metadata metadata)
   {
      this(metadata, null);
   }

   /**
    * Creates an invocation context with the anonymous principal.
    * @param metadata The root metadata object.
    * @param env The global environment.
    */
   public InvocationContext(Metadata metadata, GlobalEnvironment env)
   {
      Context ctxSaved = ThreadContextHolder.getContext();

      try
      {
         m_metadata = metadata;
         initialize(null, env);
      }
      finally
      {
         ThreadContextHolder.setContext(ctxSaved);
      }
   }

   // operations

   /**
    * Initializes the invocation context.
    * @param principal The user security principal.
    * @param env The global environment.
    */
   public void initialize(Principal principal, GlobalEnvironment env)
   {
      ThreadContextHolder.setContext(this);

      boolean bLogEnabledSaved = Logger.isEnabled();

      try
      {
         if (m_bStealth)
         {
            Logger.setEnabled(false);
         }

         if (env == null)
         {
            env = new GlobalEnvironment(m_metadata.getGlobalEnvironment());
         }

         if (m_machine != null)
         {
            complete(false);
            m_cacheMap.clear();
            m_classChangeMap = null;
            m_instanceRefMap = null;

            if (m_partition != null)
            {
               Instance partition = new Instance(m_partition.getLazyMetaclass(), this);

               partition.setOID(m_partition.getOID());
               partition.setClean();
               m_partition = partition;
            }
         }

         m_machine = new Machine(env, this);
         initUnitOfWork();
         login(principal);

         // Re-check stealth privilege now that login is complete.
         if (m_bStealth)
         {
            setStealth(true);
         }
      }
      finally
      {
         Logger.setEnabled(bLogEnabledSaved);
      }
   }

   /**
    * Initializes the invocation context.
    * @param principal The user security principal.
    */
   public void initialize(Principal principal)
   {
      initialize(principal, null);
   }

   /**
    * Creates a new unit of work, with the current transaction (if any) as an external transaction.
    * @return the unit of work;
    */
   public UnitOfWork createUnitOfWork()
   {
      Transaction tx = null;

      if (m_txManager != null)
      {
         tx = getTransaction();
      }

      if (m_uow != null)
      {
         m_uow.releaseResources(false);
      }

      m_uow = new UnitOfWork(this, tx, (tx != null) ? UnitOfWork.TX_EXTERNAL : UnitOfWork.TX_MANAGED);
      m_uow.setTransient(m_bTransient);
      m_bTransient = false;
      addUnitOfWork(m_uow);

      return m_uow;
   }

   /**
    * Initializes the unit of work.
    * @return The unit of work.
    */
   public UnitOfWork initUnitOfWork()
   {
      if (m_uow == null)
      {
         createUnitOfWork();
      }

      return m_uow;
   }

   /**
    * Sets the transaction manager instance.
    * @param txManager The transaction manager.
    */
   public void setTransactionManager(TransactionManager txManager)
   {
      m_txManager = txManager;
   }

   /**
    * @return The transaction manager instance.
    */
   public TransactionManager getTransactionManager()
   {
      if (m_txManager == null)
      {
         m_txManager = (TransactionManager)getComponentInstance("System.TransactionManager");
      }

      return m_txManager;
   }

   /**
    * Sets the metadata object.
    * @param metadata The root metadata object.
    */
   public void setMetadata(Metadata metadata)
   {
      m_metadata = metadata;
   }

   /**
    * Sets the user class object.
    * @param userClass The class object to set.
    */
   public void setUserClass(Metaclass userClass)
   {
      m_userClass = userClass;
   }

   /**
    * Sets the query timeout.
    * @param nQueryTimeout The query timeout to set.
    */
   public void setQueryTimeout(int nQueryTimeout)
   {
      m_nQueryTimeout = nQueryTimeout;
   }

   /**
    * @return The query timeout.
    */
   public int getQueryTimeout()
   {
      return m_nQueryTimeout;
   }

   /**
    * Sets the query security mode (one of Query.SEC_* constants).
    * @param nQuerySecurity The query security mode (one of Query.SEC_* constants) to set.
    */
   public void setQuerySecurity(byte nQuerySecurity) throws IllegalStateException
   {
      m_nQuerySecurity = nQuerySecurity;
   }

   /**
    * @return The query security mode (one of Query.SEC_* constants).
    */
   public byte getQuerySecurity()
   {
      return m_nQuerySecurity;
   }

   /**
    * @return The user class object.
    */
   public Metaclass getUserClass()
   {
      return m_userClass;
   }

   /**
    * Logs a user into the invocation context.
    * @param principal The JAAS principal to login.
    */
   public void login(Principal principal)
   {
      if (principal == null)
      {
         principal = ANONYMOUS_PRINCIPAL;
      }

      if (principal != ANONYMOUS_PRINCIPAL)
      {
         if (m_userClass == null)
         {
            throw new IllegalStateException("User class not set");
         }

         UnitOfWork old = suspendTransaction();
         UnitOfWork uow = m_uow;
         boolean bSecure = m_bSecure;
         String sUser = null;

         m_bSecure = false;

         try
         {
            sUser = principal.getName();

            if (sUser != null && m_metadata.getAuthenticationProtocol() == Metadata.AUTH_PROTOCOL_SPNEGO)
            {
               if (m_metadata.isRealmless())
               {
                  int nRealmIndex = sUser.indexOf('@');
                  
                  if (nRealmIndex >= 0) 
                  {
                     sUser = sUser.substring(0, nRealmIndex);
                  }
               }
               else
               {
                  if (m_metadata.getAuthenticationDomain() != null && sUser.indexOf('@') < 0)
                  {
                     sUser += '@' + m_metadata.getAuthenticationDomain();
                  }
               }
            }

            Instance user = (Instance)m_userClass.invoke("getUser", new Object[]{sUser});

            if (user == null)
            {
               if (principal == m_metadata.getAnonymousUser())
               {
                  if (s_logger.isWarnEnabled())
                  {
                     s_logger.warn("Anonymous user account \"" + m_metadata.getAnonymousUser().getName() + "\" disabled or does not exist");
                  }

                  throw new SecurityViolationException("err.runtime.anonDisabled");
               }

               s_logger.warn("Unauthorized authenticated user \"" + sUser + "\"");

               throw new SecurityViolationException("err.runtime.unauthorizedUser", new Object[]{sUser});
            }

            setUser(user);
            commitAndResume(old);
            old = null;
         }
         catch (Throwable t)
         {
            rollbackAndResume(old);
            old = null;

            if (!(t instanceof SecurityViolationException) && !ObjUtil.isSystem(t))
            {
               s_logger.error("Cannot login user \"" + sUser + "\"", t);

               t = new SecurityViolationException("err.runtime.unauthorizedUser", new Object[]{sUser}, t);
            }

            ObjUtil.rethrow(t);
         }
         finally
         {
            if (old != null)
            {
               removeUnitOfWork(uow);
            }

            m_bSecure = bSecure;
         }
      }
      else
      {
         setUser(null);
      }

      m_principal = principal;
   }

   /**
    * Logs a user into the invocation context.
    * @param user The user instance.
    */
   public void login(Instance user)
   {
      Principal principal;

      if (user == null)
      {
         principal = ANONYMOUS_PRINCIPAL;
      }
      else
      {
         principal = new SimplePrincipal((String)user.getValue("name"));
      }

      setUser(user);
      m_principal = principal;
   }

   /**
    * Sets the invocation context user instance.
    * @param user The user instance.
    */
   protected void setUser(Instance user)
   {
      PrivilegeSet privilegeSet = null;
      String sFragmentName = m_sFragmentName;
      Instance partition = m_partition;

      if (user != null)
      {
         Metaclass metaclass = user.getMetaclass();
         PersistenceMapping mapping = metaclass.getPersistenceMapping();
         Attribute attribute = null;

         assert m_userClass.isUpcast(metaclass);

         if (mapping != null)
         {
            attribute = mapping.getFragmentAttribute();
         }

         if (attribute == null)
         {
            attribute = metaclass.findAttribute("fragmentName");
         }

         if (attribute != null)
         {
            sFragmentName = (String)((attribute.isStatic()) ?
               metaclass.getValue(attribute.getOrdinal()) :
                  user.getValue(attribute.getOrdinal()));
         }

         attribute = metaclass.findAttribute("partition");

         if (attribute != null)
         {
            partition = (Instance)((attribute.isStatic()) ?
               metaclass.getValue(attribute.getOrdinal()) :
                  user.getValue(attribute.getOrdinal()));
         }

         privilegeSet = (PrivilegeSet)user.getValue("privilegeSet");
      }

      if (privilegeSet == null)
      {
         privilegeSet = new PrivilegeSet(m_metadata.getPrimitivePrivilegeCount(), false);
      }

      m_user = user;
      m_privilegeSet = privilegeSet;
      m_sFragmentName = sFragmentName;
      m_partition = partition;
   }

   /**
    * @return The user instance.
    */
   public Instance getUser()
   {
      return m_user;
   }

   /**
    * Sets the invocation context partition instance.
    * @param partition The invocation context partition instance to set.
    */
   public void setPartition(Instance partition)
   {
      m_partition = partition;
   }

   /**
    * @return The invocation context partition instance.
    */
   public Instance getPartition()
   {
      return m_partition;
   }

   /**
    * @return The root metadata object associated with the invocation context.
    */
   public Metadata getMetadata()
   {
      return m_metadata;
   }

   /**
    * @see nexj.core.runtime.Context#getContextMetadata()
    */
   public ContextMetadata getContextMetadata()
   {
      return m_metadata;
   }

   /**
    * @return The security principal of the user associated with the invocation context.
    */
   public Principal getPrincipal()
   {
      return m_principal;
   }

   /**
    * Sets the scripting virtual machine.
    * This method is for INTERNAL USE ONLY.
    * @param machine The machine to set.
    */
   public void setMachine(Machine machine)
   {
      m_machine = machine;
   }

   /**
    * @return The scripting virtual machine.
    */
   public Machine getMachine()
   {
      return m_machine;
   }

   /**
    * Sets the invocation context locale.
    * @param locale The locale to set.
    */
   public void setLocale(Locale locale)
   {
      if (locale == null)
      {
         throw new NullPointerException("Attempt to set a null locale on InvocationContext");
      }

      setLocale(locale.toString());
   }

   /**
    * Sets the invocation context locale.
    * @param sLocale The locale name.
    */
   public void setLocale(String sLocale)
   {
      m_locale = m_metadata.getLocale(sLocale);
      m_sLocaleName = null;
      m_stringTable = null;

      if (s_logger.isDebugEnabled() && !m_bStealth)
      {
         s_logger.debug("Set locale \"" + m_locale + "\"");
      }
   }

   /**
    * @return The invocation context locale.
    */
   public Locale getLocale()
   {
      return m_locale;
   }

   /**
    * @return The invocation context locale name.
    */
   public String getLocaleName()
   {
      if (m_sLocaleName == null)
      {
         m_sLocaleName = m_locale.toString();
      }

      return m_sLocaleName;
   }

   /**
    * Sets the invocation context time zone.
    * @param timeZone The time zone to set.
    */
   public void setTimeZone(TimeZone timeZone)
   {
      m_timeZone = timeZone;
   }

   /**
    * Sets the invocation context time zone.
    * @param sTimeZone The time zone name.
    */
   public void setTimeZone(String sTimeZone)
   {
      m_timeZone = TimeZone.getTimeZone(sTimeZone);
   }

   /**
    * @return The invocation context time zone.
    */
   public TimeZone getTimeZone()
   {
      if (m_timeZone == null)
      {
         m_timeZone = TimeZone.getDefault();
      }

      return m_timeZone;
   }

   /**
    * Sets the invocation context fragment name.
    * @param sFragmentName The current fragment name to set. Null for default.
    */
   public void setFragmentName(String sFragmentName)
   {
      if (sFragmentName != null && sFragmentName.length() == 0)
      {
         sFragmentName = null;
      }

      m_sFragmentName = sFragmentName;
   }

   /**
    * @return The invocation context fragment name. Null is default.
    */
   public String getFragmentName()
   {
      return m_sFragmentName;
   }

   /**
    * Sets the client address, e.g. 192.168.1.123:12345.
    * @param sClientAddress The client address to set. Can be null.
    */
   public void setClientAddress(String sClientAddress)
   {
      m_sClientAddress = sClientAddress;
   }

   /**
    * @return The client address. Can be null.
    */
   public String getClientAddress()
   {
      return m_sClientAddress;
   }

   /**
    * @return The string table for the invocation context locale.
    */
   public StringTable getStringTable()
   {
      if (m_stringTable == null)
      {
         m_stringTable = m_metadata.getStringTable(getLocaleName());
      }

      return m_stringTable;
   }

   /**
    * Gets a localized string for the invocation context locale.
    * @param sName The string name.
    * @return The localized string, or sName if not found.
    */
   public String getString(String sName)
   {
      return getStringTable().get(sName);
   }

   /**
    * Formats a string according to the invocation context locale.
    * @param sName The string name. It must correspond to a string table
    * entry specifying a java.text.MessageFormat pattern.
    * @param args The argument array. Can be null.
    * @return The formatted string.
    */
   public String formatString(String sName, Object[] args)
   {
      return getStringTable().format(sName, args, getTimeZone());
   }

   /**
    * This method is for INTERNAL USE ONLY.
    * @return The component context instance map.
    */
   public Lookup getComponentInstanceMap()
   {
      return m_cacheMap;
   }

   /**
    * Gets a component instance by name.
    * @param sName The component name.
    */
   public Object getComponentInstance(String sName)
   {
      return m_metadata.getComponent(sName).getInstance(this);
   }

   /**
    * Gets a per-context class instance.
    * @param clazz The class for which to get the instance.
    */
   public Object getClassInstance(Class clazz)
   {
      Object instance = m_cacheMap.get(clazz);

      if (instance == null)
      {
         try
         {
            instance = clazz.newInstance();

            if (instance instanceof InvocationContextAware)
            {
               ((InvocationContextAware)instance).setInvocationContext(this);
            }

            if (instance instanceof Initializable)
            {
               ((Initializable)instance).initialize();
            }
         }
         catch (Exception e)
         {
            throw new UncheckedException("err.runtime.class", new Object[]{clazz.getName()}, e);
         }

         m_cacheMap.put(clazz, instance);
      }

      return instance;
   }

   /**
    * Caches data in the transient cache. The cache is local to the invocation context and
    * expires when the invocation context is garbage collected.
    * @param itr The iterator over the key-value pairs.
    */
   public void cacheTransient(Lookup.Iterator itr)
   {
      while (itr.hasNext())
      {
         itr.next();

         Object value = itr.getValue();

         if (value == Undefined.VALUE)
         {
            m_cacheMap.remove(itr.getKey());
         }
         else
         {
            m_cacheMap.put(itr.getKey(), value);
         }
      }
   }

   /**
    * Gets a cached value from the transient cache.
    * @param key The cache key.
    * @return The cached value, or null if none.
    */
   public Object getCachedTransient(Object key)
   {
      assert key != null;

      return m_cacheMap.get(key);
   }

   /**
    * @return The global cache.
    */
   public DataCache getGlobalCache()
   {
      if (m_globalCache == null)
      {
         m_globalCache = (DataCache)getComponentInstance("System.DataCache");
      }

      return m_globalCache;
   }

   /**
    * Enhances a cache key to include the current partition, if the partitioned flag is on.
    * @param key The key to enhance.
    * @return The enhanced key.
    */
   public Object getPartitionedKey(Object key)
   {
      if (m_bPartitioned && m_partition != null)
      {
         return new Pair(key, m_partition.getOID());
      }

      return key;
   }

   /**
    * Removes the stale references from the queue and the instance map.
    */
   protected void removeStaleReferences()
   {
      InstanceRef ref;

      while ((ref = (InstanceRef)m_refq.poll()) != null)
      {
         InstanceRef oldRef = (InstanceRef)m_instanceRefMap.remove(ref.m_oid, ref.m_metaclass);

         if (oldRef == null || oldRef.get() == null)
         {
            ref.unlock();
         }
         else
         {
            m_instanceRefMap.put(oldRef.m_oid, oldRef.m_metaclass, oldRef);
         }
      }
   }

   /**
    * Adds a cached instance to the context.
    * @param instance The instance to add.
    * @return The instance reference.
    */
   public InstanceRef addInstance(Instance instance)
   {
      if (instance.getOID() == null)
      {
         return null;
      }

      if (m_instanceRefMap == null)
      {
         m_instanceRefMap = new HashTab2D(32);
         m_refq = new ReferenceQueue();
      }
      else
      {
         removeStaleReferences();
      }

      InstanceRef ref = new InstanceRef(instance, m_refq);
      InstanceRef oldRef = (InstanceRef)m_instanceRefMap.put(ref.m_oid, ref.m_metaclass, ref);

      if (oldRef != null)
      {
         Instance old = (Instance)oldRef.get();

         if (old != null)
         {
            if (old != instance)
            {
               if (old.getState() == Instance.DELETED)
               {
                  oldRef.unlock();
                  old.getUnitOfWork().changeOID(old);
               }
               else
               {
                  m_instanceRefMap.put(ref.m_oid, ref.m_metaclass, oldRef);

                  throw new DuplicateItemException("err.persistence.duplicateInstance",
                     new Object[]{ref.m_metaclass.getName(), ref.m_oid});
               }
            }
            else
            {
               m_instanceRefMap.put(ref.m_oid, ref.m_metaclass, oldRef);
            }
         }
      }

      return ref;
   }

   /**
    * Finds an instance reference by class object and OID.
    * @param metaclass The class object.
    * @param oid The instance OID.
    * @return The instance reference, or null if not found.
    */
   public InstanceRef findInstanceRef(Metaclass metaclass, OID oid)
   {
      if (m_instanceRefMap == null || oid == null)
      {
         return null;
      }

      removeStaleReferences();

      return (InstanceRef)m_instanceRefMap.get(oid, metaclass.getPersistenceRoot());
   }

   /**
    * Finds an instance by class object and OID.
    * @param metaclass The class object.
    * @param oid The instance OID.
    * @return The instance, or null if not found.
    */
   public Instance findInstance(Metaclass metaclass, OID oid)
   {
      InstanceRef ref = findInstanceRef(metaclass, oid);

      if (ref != null)
      {
         return ref.getInstance();
      }

      return null;
   }

   /**
    * Finds and share-locks an instance by class object and OID.
    * @param metaclass The class object.
    * @param oid The instance OID.
    * @param bForce True to override the UOW locking flag.
    * @return The instance, or null if not found.
    */
   public Instance lockInstance(Metaclass metaclass, OID oid, boolean bForce)
   {
      InstanceRef ref = findInstanceRef(metaclass, oid);

      if (ref != null)
      {
         initUnitOfWork().lock(ref, bForce);

         return ref.getInstance();
      }

      return null;
   }

   /**
    * Removes an instance from the context.
    * @param instance The instance to remove.
    */
   public void removeInstance(Instance instance)
   {
      if (m_instanceRefMap != null)
      {
         OID oid = instance.getOID();

         if (oid != null)
         {
            InstanceRef ref = (InstanceRef)m_instanceRefMap.remove(oid,
               instance.getLazyMetaclass().getPersistenceRoot());

            if (ref != null)
            {
               ref.unlock();
            }
         }
      }
   }

   /**
    * @return An instance reference iterator.
    * The iterator can return nulls from next().
    */
   public Iterator getInstanceRefIterator()
   {
      if (m_instanceRefMap == null)
      {
         return HashTab2D.EMPTY_ITERATOR;
      }

      removeStaleReferences();

      return m_instanceRefMap.valueIterator();
   }

   /**
    * @return The instance count.
    */
   public int getInstanceCount()
   {
      if (m_instanceRefMap == null)
      {
         return 0;
      }

      return m_instanceRefMap.size();
   }

   /**
    * Determines if an instance is share-locked.
    * @param instance The instance to check.
    * @return True if the instance is share-locked.
    */
   public boolean isLocked(Instance instance)
   {
      if (instance.getOID() != null)
      {
         InstanceRef ref = findInstanceRef(instance.getLazyMetaclass(), instance.getOID());

         return ref != null && ref.isLocked();
      }

      return false;
   }

   /**
    * Starts cumulative change tracking for a given class object.
    * @param metaclass The class object to track.
    */
   public void track(Metaclass metaclass)
   {
      if (m_classChangeMap == null)
      {
         m_classChangeMap = new HashTab();
      }

      if (m_classChangeMap.get(metaclass) == null)
      {
         m_classChangeMap.put(metaclass, new HashTab());
      }
   }

   /**
    * Determines if the changes to a given instance must be tracked.
    * @param instance The instance to check.
    * @return True if the instance must be tracked.
    */
   public boolean isTracked(Instance instance)
   {
      if (m_classChangeMap != null)
      {
         for (Metaclass metaclass = instance.getMetaclass(); metaclass != null; metaclass = metaclass.getBase())
         {
            if (m_classChangeMap.contains(metaclass))
            {
               return true;
            }
         }
      }

      return false;
   }

   /**
    * Accumulates the change from a given instance.
    * @param instance The instance, which change to accumulate.
    * @param nState The instance state.
    */
   public void accumulateChange(Instance instance, byte nState)
   {
      for (Metaclass metaclass = instance.getMetaclass(); metaclass != null; metaclass = metaclass.getBase())
      {
         Object map = m_classChangeMap.get(metaclass);

         if (map != null)
         {
            UnitOfWork.accumulateChange((Lookup)map, instance, nState);
         }
      }
   }

   /**
    * Gets the change map for a given class.
    * @param metaclass The class for which to get the change map.
    * @return The change map, or null if none.
    */
   public Lookup getClassChangeMap(Metaclass metaclass)
   {
      if (m_classChangeMap == null)
      {
         return null;
      }

      return (Lookup)m_classChangeMap.get(metaclass);
   }

   /**
    * Sets the default maximum number of changed instances in a unit of work.
    * @param nMaxUOWChangeCount The maximum number of changed instances in a unit of work to set (negative for unlimited).
    */
   public void setMaxUnitOfWorkChangeCount(int nMaxUOWChangeCount)
   {
      m_nMaxUOWChangeCount = nMaxUOWChangeCount;
   }

   /**
    * @return The default maximum number of changed instances in a unit of work (negative for unlimited).
    */
   public int getMaxUnitOfWorkChangeCount()
   {
      return m_nMaxUOWChangeCount;
   }

   /**
    * Adds a new unit of work to the invocation context.
    * @param uow The new unit of work to add.
    */
   public void addUnitOfWork(UnitOfWork uow)
   {
      if (m_nUOWCount == m_uowArray.length)
      {
         UnitOfWork[] uowArray = new UnitOfWork[m_nUOWCount << 1];

         System.arraycopy(m_uowArray, 0, uowArray, 0, m_nUOWCount);
         m_uowArray = uowArray;
      }

      m_uowArray[m_nUOWCount++] = uow;
   }

   /**
    * Removes a unit of work from the invocation context.
    * @param uow The unit of work to remove.
    */
   public void removeUnitOfWork(UnitOfWork uow)
   {
      if (uow != null)
      {
         for (int i = m_nUOWCount - 1; i >= 0; --i)
         {
            if (m_uowArray[i] == uow)
            {
               System.arraycopy(m_uowArray, i + 1, m_uowArray, i, m_uowArray.length - i - 1);
               m_uowArray[--m_nUOWCount] = null;

               return;
            }
         }
      }
   }

   /**
    * Gets a unit of work with a specified ordinal number.
    * @param nOrdinal The unit of work ordinal number.
    * @return The unit of work.
    */
   public UnitOfWork getUnitOfWork(int nOrdinal)
   {
      assert nOrdinal >= 0 && nOrdinal < m_nUOWCount;

      return m_uowArray[nOrdinal];
   }

   /**
    * @return The number of the associated units of work.
    */
   public int getUnitOfWorkCount()
   {
      return m_nUOWCount;
   }

   /**
    * Sets the current unit of work.
    * @param uow The unit of work to set.
    * @return The previous unit of work.
    */
   public UnitOfWork setUnitOfWork(UnitOfWork uow)
   {
      UnitOfWork uowOld = m_uow;

      if (uow != uowOld)
      {
         if (uowOld != null)
         {
            uowOld.releaseResources(false);
         }

         m_uow = uow;
         setTransaction((uow == null) ? null : uow.getTransaction());
      }

      return uowOld;
   }

   /**
    * @return The current unit of work.
    */
   public UnitOfWork getUnitOfWork()
   {
      if (m_uow != null)
      {
         return m_uow;
      }

      return initUnitOfWork();
   }

   /**
    * Sets the audit unit of work as the current one. Creates one if none is available.
    * @return The previous unit of work.
    */
   public UnitOfWork setAuditUnitOfWork()
   {
      UnitOfWork uowOld = m_uow;

      if (m_auditUOW == null)
      {
         if (m_uow == null || m_uow.isTransient() || m_uow.getTransaction() == null)
         {
            beginTransaction(false);
         }

         if (m_uow != uowOld)
         {
            m_auditUOW = m_uow;
         }
      }
      else
      {
         setUnitOfWork(m_auditUOW);
      }

      return uowOld;
   }

   /**
    * Sets the logger unit of work as the current one. Creates one if none is available.
    * @return The previous unit of work.
    */
   public UnitOfWork setLoggerUnitOfWork()
   {
      UnitOfWork uowOld = initUnitOfWork();

      if (m_loggerUOW == null)
      {
         beginTransaction(false);
         m_loggerUOW = m_uow;
         m_loggerUOW.setAutoCommit(true);
      }
      else
      {
         setUnitOfWork(m_loggerUOW);
      }

      return uowOld;
   }

   /**
    * Commits the logger UOW.
    */
   public void commitLog()
   {
      if (m_loggerUOW != null && m_loggerUOW != m_uow && !m_loggerUOW.isCommitting())
      {
         UnitOfWork uowOld = setLoggerUnitOfWork();

         try
         {
            m_uow.commit();
            m_loggerUOW = null;
         }
         finally
         {
            setUnitOfWork(uowOld);
         }
      }
   }

   /**
    * Joins an existing transaction, or starts a new one if none.
    * @return The old unit of work.
    */
   public UnitOfWork requireTransaction()
   {
      if (m_uow != null)
      {
         if (m_uow.getTransaction() != null)
         {
            m_bTransient = false;
            m_uow.checkTransaction();

            return m_uow;
         }

         /*
          * Propagate transient flag.
          *
          * If Request (isCommit = false) invokes Event A (TX_SUPPORTED) which invokes Event B (TX_REQUIRED),
          * then the Event B work needs to be transient as well.
          */
         m_bTransient = m_uow.isTransient();
      }

      UnitOfWork uow = beginTransaction();

      if (uow == null)
      {
         uow = m_uow;
      }

      return uow;
   }

   /**
    * Starts a new transaction after suspending the current one, if any.
    * @return The old unit of work.
    */
   public UnitOfWork beginTransaction()
   {
      return beginTransaction(true);
   }

   /**
    * Starts a new transaction after suspending the current one, if any.
    * @param bReuse True to try reusing the current UOW.
    * @return The old unit of work; null if the current unit of work will be re-used.
    */
   public UnitOfWork beginTransaction(boolean bReuse)
   {
      return beginTransaction(bReuse, -1);
   }

   /**
    * Starts a new transaction after suspending the current one, if any.
    * @param bReuse True to try reusing the current UOW.
    * @param nTimeout The transaction timeout in seconds.
    * 0 means the container default timeout.
    * Negative to not set.
    * @return The old unit of work; null if the current unit of work will be re-used.
    */
   public UnitOfWork beginTransaction(boolean bReuse, int nTimeout)
   {
      setTransaction(null);

      TransactionManager txManager = getTransactionManager();
      Transaction tx = null;

      try
      {
         if (nTimeout >= 0)
         {
            txManager.setTransactionTimeout(nTimeout);
         }

         txManager.begin();
         tx = txManager.getTransaction();

         if (s_logger.isDebugEnabled() && !m_bStealth)
         {
            s_logger.debug("Started new transaction " + tx);
         }

         if (bReuse && m_uow != null && m_uow.isReusable())
         {
            m_uow.setTransaction(tx);

            return null;
         }

         UnitOfWork uowOld = m_uow;

         m_uow = new UnitOfWork(this, tx, UnitOfWork.TX_MANAGED);
         m_uow.setTransient(m_bTransient);
         m_uow.setPrevious(uowOld);
         addUnitOfWork(m_uow);

         return uowOld;
      }
      catch (Throwable e)
      {
         if (m_uow != null && m_uow.getTransaction() != null)
         {
            if (tx != null && tx != m_uow.getTransaction())
            {
               try
               {
                  if (s_logger.isDebugEnabled() && !m_bStealth)
                  {
                     s_logger.debug("Rolling back transaction " + tx);
                  }

                  tx.rollback();
               }
               catch (Throwable x)
               {
                  if (s_logger.isWarnEnabled() && !m_bStealth)
                  {
                     s_logger.warn("Unable to rollback transaction " + tx, x);
                  }
               }
            }

            try
            {
               setTransaction(m_uow.getTransaction());
            }
            catch (Throwable x)
            {
               if (s_logger.isWarnEnabled())
               {
                  s_logger.warn("Unable to set current transaction " + m_uow.getTransaction(), x);
               }
            }
         }

         throw new TransactionException("err.runtime.txBegin", e);
      }
      finally
      {
         m_bTransient = false;

         if (nTimeout >= 0)
         {
            try
            {
               txManager.setTransactionTimeout(0);
            }
            catch (Throwable t)
            {
            }
         }
      }
   }

   /**
    * Suspends the current transaction.
    * @return The old unit of work.
    */
   public UnitOfWork suspendTransaction()
   {
      m_bTransient = false;

      if (m_uow == null || m_uow.getTransaction() == null && !m_uow.isTransient())
      {
         return null;
      }

      assert m_uow.getTransaction() == getTransaction();

      setTransaction(null);

      UnitOfWork uowOld = m_uow;

      m_uow = new UnitOfWork(this, null, UnitOfWork.TX_NONE);
      addUnitOfWork(m_uow);

      return uowOld;
   }

   /**
    * Verifies that there is a transaction.
    * @throws TransactionException if there is no transaction.
    */
   public void mandateTransaction()
   {
      m_bTransient = false;

      if (m_uow == null || m_uow.getTransaction() == null)
      {
         throw new TransactionException("err.runtime.txMandatory");
      }
   }

   /**
    * Verifies that there is no transaction.
    * @throws TransactionException if there is a transaction.
    */
   public void mandateNoTransaction()
   {
      m_bTransient = false;

      if (m_uow != null && m_uow.getTransaction() != null)
      {
         throw new TransactionException("err.runtime.txUnsupported");
      }
   }

   /**
    * Starts or suspends a transaction according to the current
    * transaction state and the supplied transaction mode.
    * @param nTxMode The transaction mode, one of the Metclass.TX_* constants.
    */
   public void manageTransaction(byte nTxMode)
   {
      switch (nTxMode)
      {
         case Event.TX_REQUIRED:
            requireTransaction();
            break;

         case Event.TX_NEW:
            beginTransaction();
            break;

         case Event.TX_NONE:
            suspendTransaction();
            break;

         case Event.TX_MANDATORY:
            mandateTransaction();
            break;

         case Event.TX_UNSUPPORTED:
            mandateNoTransaction();
            break;

         default:
            m_bTransient = false;
            break;
      }
   }

   /**
    * Completes a unit of work along with previous units of work.
    * @param uow The UOW to complete.
    * @param old The UOW to resume.
    * @param bComplete True if this is the UOW to complete.
    * @param bCommit True to commit, false to roll back. 
    */
   protected void complete(UnitOfWork uow, UnitOfWork old, boolean bComplete, boolean bCommit)
   {
      UnitOfWork prev = uow.getPrevious();

      if (prev != null && prev != old)
      {
         try
         {
            complete(prev, old, false, bCommit);
            removeUnitOfWork(prev);
            uow.setPrevious(prev.getPrevious());
            prev.setPrevious(null);
         }
         finally
         {
            if (bComplete)
            {
               setUnitOfWork(uow);
            }
         }
      }

      if (!bComplete)
      {
         setUnitOfWork(uow);
      }

      if (bCommit)
      {
         uow.commit();
      }
      else
      {
         uow.rollback();
      }
   }

   /**
    * Commits the current unit of work and resumes the old one.
    * @param old The old unit of work to resume. Can be null.
    * @param bForce Ignored.
    * @deprecated Use {@link #commitAndResume(UnitOfWork)} instead.
    */
   public void commitAndResume(UnitOfWork old, boolean bForce)
   {
      commitAndResume(old);
   }

   /**
    * Commits the current unit of work and resumes the old one.
    * @param old The old unit of work to resume. Can be null.
    */
   public void commitAndResume(UnitOfWork old)
   {
      UnitOfWork uow = m_uow;

      if (uow != old)
      {
         if (uow == null)
         {
            throw new IllegalStateException("No active UOW");
         }

         if (!uow.isCommitting())
         {
            complete(uow, old, true, true);
         }

         if (old != null)
         {
            removeUnitOfWork(uow);
            m_uow = old;

            if (old.getTransaction() != null)
            {
               setTransaction(old.getTransaction());
            }
         }
      }
   }

   /**
    * Rolls back the current unit of work and resumes the old one.
    * @param old The old unit of work to resume. Can be null.
    */
   public void rollbackAndResume(UnitOfWork old)
   {
      UnitOfWork uow = m_uow;

      if (uow != old)
      {
         if (uow != null)
         {
            complete(uow, old, true, false);
         }

         if (old != null)
         {
            removeUnitOfWork(uow);
            m_uow = old;

            if (old.getTransaction() != null)
            {
               setTransaction(old.getTransaction());
            }
         }
      }
   }

   /**
    * Moves the specified unit of work to the first position.
    * @param uow The unit of work to move. Can be null.
    */
   protected void promote(UnitOfWork uow)
   {
      if (uow != null)
      {
         for (int i = m_nUOWCount - 1; i > 0; --i)
         {
            if (m_uowArray[i] == uow)
            {
               while (--i >= 0)
               {
                  m_uowArray[i + 1] = m_uowArray[i];
               }

               break;
            }
         }

         m_uowArray[0] = uow;
      }
   }

   /**
    * Completes all the units of work.
    * @param bCommit True to commit, false to rollback.
    */
   public void complete(boolean bCommit)
   {
      complete(bCommit, true);
   }

   /**
    * Completes all the units of work.
    * @param bCommit True to commit, false to rollback.
    * @param bEndTx True to commit also the distributed transactions.
    */
   public void complete(boolean bCommit, boolean bEndTx)
   {
      Throwable e = null;
      UnitOfWork old = m_uow;
      Transaction oldTransaction = (old == null || old.getTxMode() != UnitOfWork.TX_EXTERNAL) ? null : old.getTransaction();
      int nUOWCount = m_nUOWCount;

      try
      {
         promote(m_auditUOW);
         promote(m_loggerUOW);

         // Complete the UOWs
         while (nUOWCount > 0)
         {
            UnitOfWork uow = m_uowArray[nUOWCount - 1];

            setUnitOfWork(uow);

            if (bCommit && !uow.isTransient() || uow.isAutoCommit())
            {
               uow.commit(bEndTx);
            }
            else
            {
               uow.rollback();
            }

            --nUOWCount;
         }
      }
      catch (Throwable t)
      {
         e = t;
      }

      if (bEndTx)
      {
         while (nUOWCount > 0)
         {
            UnitOfWork uow = m_uowArray[--nUOWCount];

            try
            {
               setUnitOfWork(uow);

               if (uow.isAutoCommit())
               {
                  uow.commit(bEndTx);
               }
               else
               {
                  uow.rollback();
               }
            }
            catch (Throwable t)
            {
               if (s_logger.isWarnEnabled())
               {
                  s_logger.warn("Unexpected error when " +
                     ((uow.isAutoCommit()) ? "committing" : "rolling back") +
                     " a unit of work", t);
               }
            }
         }

         nUOWCount = m_nUOWCount;

         while (nUOWCount > 0)
         {
            try
            {
               m_uowArray[--nUOWCount].releaseResources(true);
            }
            catch (Throwable t)
            {
               s_logger.warn("Unexpected error when releasing resources for a unit of work", t);
            }
         }

         Arrays.fill(m_uowArray, 0, m_nUOWCount, null);
         m_nUOWCount = 0;
         m_uow = null;
         m_auditUOW = null;
         m_loggerUOW = null;
         m_classChangeMap = null;

         if (oldTransaction != null)
         {
            setTransaction(oldTransaction);
         }
      }
      else
      {
         setUnitOfWork(old);
      }

      if (m_nRPCCount != 0 && s_logger.isDebugEnabled())
      {
         s_logger.debug("Stats: RPC=" + m_nRPCCount + ", persist=" + m_nPersistCount + ", load=" + m_nLoadCount);
      }

      if (e != null)
      {
         ObjUtil.rethrow(e);
      }
   }

   /**
    * Commits a JTA transaction.
    * @param tx The transaction to commit.
    */
   protected void commitTransaction(Transaction tx) throws TransactionException
   {
      try
      {
         if (s_logger.isDebugEnabled() && !m_bStealth)
         {
            s_logger.debug("Committing transaction " + tx);
         }

         /*
         In JBoss 5, calling commit() on the transaction object causes the transaction to be left
         associated with the calling thread with status COMMITTED which causes problems later on.
         For this reason, we must call commit() on the TransactionManager instead, which dissociates
         the current transaction from the calling thread correctly. However, this should be done ONLY
         if the transaction we are committing is actually the transaction currently associated with the
         calling thread. If it is not, we don't have a problem and commit() should be called on the
         transaction object.
         */

         if (tx == m_txManager.getTransaction())
         {
            m_txManager.commit();
         }
         else
         {
            tx.commit();
         }
      }
      catch (Throwable e)
      {
         throw new TransactionException("err.runtime.commit", e);
      }
   }

   /**
    * Rolls back a JTA transaction.
    * @param tx The transaction to roll back.
    * @param bMarkOnly True to mark for rolling back instead of rolling back.
    */
   protected void rollbackTransaction(Transaction tx, boolean bMarkOnly) throws TransactionException
   {
      try
      {
         if (tx.getStatus() != Status.STATUS_NO_TRANSACTION)
         {
            if (bMarkOnly)
            {
               if (s_logger.isDebugEnabled() && !m_bStealth)
               {
                  s_logger.debug("Marking for rollback transaction " + tx);
               }

               tx.setRollbackOnly();
            }
            else
            {
               if (s_logger.isDebugEnabled() && !m_bStealth)
               {
                  s_logger.debug("Rolling back transaction " + tx);
               }

               /*
               In JBoss 5, calling rollback() on the transaction object causes the transaction to be left
               associated with the calling thread with status ROLLEDBACK which causes problems later on.
               For this reason, we must call rollback() on the TransactionManager instead, which dissociates
               the current transaction from the calling thread correctly. However, this should be done ONLY
               if the transaction we are committing is actually the transaction currently associated with the
               calling thread. If it is not, we don't have a problem and rollback() should be called on the
               transaction object.
               */

               if (tx == m_txManager.getTransaction())
               {
                  m_txManager.rollback();
               }
               else
               {
                  tx.rollback();
               }
            }
         }
      }
      catch (Throwable e)
      {
         throw new TransactionException("err.runtime.rollback", e);
      }
   }

   /**
    * Sets the current JTA transaction.
    * @param tx The transaction to set.
    * @return The old transaction.
    */
   protected Transaction setTransaction(Transaction tx) throws TransactionException
   {
      try
      {
         TransactionManager txManager = getTransactionManager();
         Transaction txOld = txManager.getTransaction();

         if (txOld != null)
         {
            if (s_logger.isDebugEnabled() && !m_bStealth)
            {
               s_logger.debug("Suspending transaction " + txOld);
            }

            txManager.suspend();
         }

         if (tx != null)
         {
            if (s_logger.isDebugEnabled() && !m_bStealth)
            {
               s_logger.debug("Resuming transaction " + tx);
            }

            txManager.resume(tx);
         }

         return txOld;
      }
      catch (Exception e)
      {
         throw new TransactionException("err.runtime.txSetCurrent", e);
      }
   }

   /**
    * @return The current JTA transaction.
    */
   protected Transaction getTransaction() throws TransactionException
   {
      try
      {
         return getTransactionManager().getTransaction();
      }
      catch (Throwable e)
      {
         throw new TransactionException("err.runtime.txGetCurrent", e);
      }
   }

   /**
    * Sets the generation value (one of the GEN_* constants).
    * @param nGeneration The generation value (one of the GEN_* constants) to set.
    */
   public void setGeneration(byte nGeneration)
   {
      assert nGeneration >= GEN_NEW && nGeneration <= GEN_OLD;

      m_nGeneration = nGeneration;
   }

   /**
    * @return The generation value (one of the GEN_* constants).
    */
   public byte getGeneration()
   {
      return m_nGeneration;
   }

   /**
    * Sets the secure access flag.
    * @param bSecure The secure access flag to set.
    */
   public void setSecure(boolean bSecure)
   {
      m_bSecure = bSecure;
   }

   /**
    * @return The secure access flag.
    */
   public boolean isSecure()
   {
      return m_bSecure;
   }

   /**
    * Sets the visibility protection flag.
    * @param bProtected The visibility protection flag to set.
    */
   public void setProtected(boolean bProtected)
   {
      m_bProtected = bProtected;
   }

   /**
    * @return The visibility protection flag.
    */
   public boolean isProtected()
   {
      return m_bProtected;
   }

   /**
    * Sets the partitioning isolation flag.
    * @param bPartitioned The partitioning isolation flag to set.
    */
   public void setPartitioned(boolean bPartitioned)
   {
      m_bPartitioned = bPartitioned;
   }

   /**
    * @return The partitioning isolation flag.
    */
   public boolean isPartitioned()
   {
      return m_bPartitioned;
   }

   /**
    * Sets the transient flag.
    * @param bTransient True to set the transient flag of the first UnitOfWork created in this context.
    */
   public void setTransient(boolean bTransient)
   {
      m_bTransient = bTransient;
   }

   /**
    * Gets the transient flag.
    * @return True if a UnitOfWork created by this context will be transient.
    */
   public boolean isTransient()
   {
      return m_bTransient;
   }

   /**
    * Sets the audit flag.
    * @param bAudited The audit flag to set.
    */
   public void setAudited(boolean bAudited)
   {
      m_bAudited = bAudited;
   }

   /**
    * @return The audit flag.
    */
   public boolean isAudited()
   {
      return m_bAudited;
   }

   /**
    * Sets the global UnitOfWork flag.
    * @param bGlobal True if the current UnitOfWork is global; false to skip instance/UnitOfWork affinity checks.
    */
   public void setUnitOfWorkGlobal(boolean bGlobal)
   {
      m_bUOWGlobal = bGlobal;
   }

   /**
    * Gets the global UnitOfWork flag.
    * @return True if the current UnitOfWork is global; false to skip instance/UnitOfWork affinity checks.
    */
   public boolean isUnitOfWorkGlobal()
   {
      return m_bUOWGlobal;
   }

   /**
    * Sets the current cancellable object.
    * @param cancellable The current cancellable object to set. Can be null.
    * @return The old cancellable object. Can be null.
    */
   public synchronized Cancellable setCancellable(Cancellable cancellable)
   {
      Cancellable old = m_cancellable;

      m_cancellable = cancellable;

      return old;
   }

   /**
    * Cancels any blocked processes.
    * @see Cancellable#cancel()
    */
   public void cancel()
   {
      Cancellable cancellable;

      synchronized (this)
      {
         cancellable = m_cancellable;
      }

      if (cancellable != null)
      {
         cancellable.cancel();
      }
   }

   /**
    * @return The current user privilege set.
    */
   public PrivilegeSet getPrivilegeSet()
   {
      return m_privilegeSet;
   }

   /**
    * Runs the given action in privileged mode.
    * @param action The action to run.
    * @return The value returned by the action.
    */
   public Object doPrivileged(PrivilegedAction action)
   {
      boolean bSecureSaved = m_bSecure;

      m_bSecure = false;

      try
      {
         return action.run();
      }
      finally
      {
         m_bSecure = bSecureSaved;
      }
   }

   /**
    * Set whether dump and debug level logging should be suppressed for this
    * invocation context. This may be called before initialization; privileges
    * will be re-checked after initialization.
    * @param bStealth The new value for stealth
    * @throws SecurityViolationException
    * @see {@link #initialize(Principal, GlobalEnvironment)}
    */
   public void setStealth(boolean bStealth)
   {
      // Check stealth privilege
      if (bStealth && m_bSecure && m_privilegeSet != null)
      {
         Privilege privilege = m_metadata.findPrivilege("Debug");

         if (!(privilege instanceof PrimitivePrivilege) ||
            !m_privilegeSet.contains((PrimitivePrivilege)privilege))
         {
            m_bStealth = false;

            throw new SecurityViolationException("err.rpc.stealth", new Object[] { "Debug" });
         }
      }

      m_bStealth = bStealth;
   }

   /**
    * @return Whether debug and dump level logging are suppressed for this invocation context.
    */
   public boolean isStealth()
   {
      return m_bStealth;
   }

   /**
    * Sets the tester function.
    * @see #m_tester
    * @param tester The tester function to set.
    */
   public void setTester(Function tester)
   {
      m_tester = tester;
   }

   /**
    * @return The tester function.
    * @see #m_tester
    */
   public Function getTester()
   {
      return m_tester;
   }

   /**
    * Increments the RPC count.
    * @param nRPCCount The RPC count to add.
    */
   public void addRPCCount(int nRPCCount)
   {
      m_nRPCCount += nRPCCount;
   }

   /**
    * @return The RPC count.
    */
   public int getRPCCount()
   {
      return m_nRPCCount;
   }

   /**
    * Increments the persistence RPC count.
    * @param nPersistCount The persistence RPC count to add.
    */
   public void addPersistCount(int nPersistCount)
   {
      m_nPersistCount += nPersistCount;
   }

   /**
    * @return The persistence RPC count.
    */
   public int getPersistCount()
   {
      return m_nPersistCount;
   }

   /**
    * Increments the lazy load count.
    * @param nLoadCount The lazy load count to add.
    */
   public void addLoadCount(int nLoadCount)
   {
      m_nLoadCount += nLoadCount;
   }

   /**
    * @return The lazy load count.
    */
   public int getLoadCount()
   {
      return m_nLoadCount;
   }
}
