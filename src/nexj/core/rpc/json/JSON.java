package nexj.core.rpc.json;

/**
 * JSON System keys
 */
public final class JSON
{
   // constants

   /**
    * JSON System Keys for NexJ Express Server Objects
    */

   public final static String STRING_ID = ":id";

   public final static String CHARACTER = ":char";

   public final static String BINARY = ":binary";

   public final static String LOCALE = ":localeName";

   public final static String TIMEZONE = ":timezoneName";

   public final static String OID = ":OID";

   public final static String SYMBOL = ":symbol";

   public final static String PRIVILEGE_SET = ":privilegeSet";

   public final static String CHAR_ARRAY = ":char[]";

   public final static String STRING_ARRAY = ":string[]";

   public final static String OBJECT_ARRAY = ":object[]";

   public final static String BYTE_ARRAY = ":byte[]";

   public final static String EXPRESSION = ":expression";

   public final static String TIMESTAMP = ":date";

   public final static String MACRO = ":macro";

   public final static String FUNCTION = ":code";
   public final static String FUNCTION_CONSTANTS = ":constants";

   public final static String PAIR = ":head";
   public final static String PAIR_TAIL = ":tail";

   public final static String RESPONSE_RESULTS = ":results";
   public final static String RESPONSE_EVENTS = ":events";

   public final static String EXCEPTION = ":errorCode";
   public final static String EXCEPTION_ERR_MSG = ":errorMessage";
   public final static String EXCEPTION_ERR_ARGS = ":errorArgs";
   public final static String EXCEPTION_CLASS = ":errorClass";
   public final static String EXCEPTION_OID = ":errorOID";
   public final static String EXCEPTION_ORDINAL = ":ordinal";
   public final static String EXCEPTION_ATTRS = ":attributes";
   public final static String EXCEPTION_ATTR_EXCEPTIONS = ":attributeExceptions";
   public final static String EXCEPTION_EXCEPTIONS = ":exceptions";

   public final static String TRANSFER_OBJECT = ":class";
   public final static String TRANSFER_OBJECT_OID = ":oid";
   public final static String TRANSFER_OBJECT_EVENT = ":event";
   public final static String TRANSFER_OBJECT_VERSION = ":version";

   public final static String REQUEST = ":invocations";
   public final static String REQUEST_NAMESPACE = ":namespace";
   public final static String REQUEST_VERSION = ":version";
   public final static String REQUEST_ASYNC = ":async";
   public final static String REQUEST_COMMIT = ":commit";
   public final static String REQUEST_LOCALE = ":locale";
   public final static String REQUEST_TIMEZONE = ":timezone";
   public final static String REQUEST_CORRELATOR = ":correlator";
   public final static String REQUEST_FILTERS = ":filters";

   public final static String INVOCATION_OBJECT = "object";
   public final static String INVOCATION_EVENT = "event";
   public final static String INVOCATION_ARGUMENTS = "arguments";
   public final static String INVOCATION_ATTRIBUTES = "attributes";
}
