// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.io.IOException;
import java.io.Serializable;
import java.lang.reflect.Field;

import nexj.core.util.Lookup;
import nexj.core.util.PrintWriter;
import nexj.core.util.Printable;
import nexj.core.util.SoftHashTab;

/**
 * Represents a special type of interned string.
 * Symbols with the same name have the same object identity.
 */
public final class Symbol implements CharSequence, Serializable, Printable, Comparable
{
   // constants

   /**
    * Predefined symbols.
    */
   public final static Symbol _ASSIGNMENT = new Symbol(":assignment");
   public final static Symbol _ASSOCIATION = new Symbol(":association");
   public final static Symbol _ATTRIBUTES = new Symbol(":attributes");
   public final static Symbol _CLASS = new Symbol(":class");
   public final static Symbol _CONTAINS = new Symbol(":contains");
   public final static Symbol _COUNT = new Symbol(":count");
   public final static Symbol _DELETE = new Symbol(":delete");
   public final static Symbol _EMPTY = new Symbol(":empty?");
   public final static Symbol _EVENT = new Symbol(":event");
   public final static Symbol _EXCEPTION = new Symbol(":exception");
   public final static Symbol _FINDCOLLECTION = new Symbol(":findCollection");
   public final static Symbol _FLOW = new Symbol(":flow");
   public final static Symbol _FOREACHCOLLECTION = new Symbol(":forEachCollection");
   public final static Symbol _FOREACHVALUE = new Symbol(":forEachValue");
   public final static Symbol _GET = new Symbol(":get");
   public final static Symbol _ITERATOR = new Symbol(":iterator");
   public final static Symbol _LIST = new Symbol(":list");
   public final static Symbol _NAME = new Symbol(":name");
   public final static Symbol _NODE = new Symbol(":node");
   public final static Symbol _OID = new Symbol(":oid");
   public final static Symbol _OFFSET = new Symbol(":offset");
   public final static Symbol _ORDERBY = new Symbol(":orderBy");
   public final static Symbol _PARENT = new Symbol(":parent");
   public final static Symbol _REMOVE = new Symbol(":remove");
   public final static Symbol _SIZE = new Symbol(":size");
   public final static Symbol _STATE = new Symbol(":state");
   public final static Symbol _VERSION = new Symbol(":version");
   public final static Symbol _WHERE = new Symbol(":where");
   public final static Symbol _XLOCK = new Symbol(":xlock");
   public final static Symbol ABS = new Symbol("abs");
   public final static Symbol ACOS = new Symbol("acos");
   public final static Symbol ADDED = new Symbol("added");
   public final static Symbol ADDTIMER = new Symbol("addTimer");
   public final static Symbol AFTER = new Symbol("after");
   public final static Symbol AND = new Symbol("and");
   public final static Symbol ANY = new Symbol("any");
   public final static Symbol APPEND = new Symbol("append");
   public final static Symbol APPEND_M = new Symbol("append!");
   public final static Symbol APPLY = new Symbol("apply");
   public final static Symbol AROUND = new Symbol("around");
   public final static Symbol ASIN = new Symbol("asin");
   public final static Symbol ASSIGNEE = new Symbol("assignee");
   public final static Symbol ASSIGNMENTS = new Symbol("assignments");
   public final static Symbol ASSOC = new Symbol("assoc");
   public final static Symbol ASSQ = new Symbol("assq");
   public final static Symbol ASSV = new Symbol("assv");
   public final static Symbol AT = new Symbol("@");
   public final static Symbol ATAN = new Symbol("atan");
   public final static Symbol ATAT = new Symbol("@@");
   public final static Symbol ATTRIBUTE = new Symbol("attribute");
   public final static Symbol AVERAGE = new Symbol("average");
   public final static Symbol BEFORE = new Symbol("before");
   public final static Symbol BEGIN = new Symbol("begin");
   public final static Symbol BEGIN_PRIVILEGED = new Symbol("begin-privileged");
   public final static Symbol BIG = new Symbol("big");
   public final static Symbol BINARY_P = new Symbol("binary?");
   public final static Symbol BIND = new Symbol("bind");
   public final static Symbol BITWISE_AND = new Symbol("bitwise-and");
   public final static Symbol BITWISE_ARITHMETIC_SHIFT = new Symbol("bitwise-arithmetic-shift");
   public final static Symbol BITWISE_ARITHMETIC_SHIFT_LEFT = new Symbol("bitwise-arithmetic-shift-left");
   public final static Symbol BITWISE_ARITHMETIC_SHIFT_RIGHT = new Symbol("bitwise-arithmetic-shift-right");
   public final static Symbol BITWISE_BIT_COUNT = new Symbol("bitwise-bit-count");
   public final static Symbol BITWISE_BIT_FIELD = new Symbol("bitwise-bit-field");
   public final static Symbol BITWISE_BIT_SET_P = new Symbol("bitwise-bit-set?");
   public final static Symbol BITWISE_COPY_BIT = new Symbol("bitwise-copy-bit");
   public final static Symbol BITWISE_COPY_BIT_FIELD = new Symbol("bitwise-copy-bit-field");
   public final static Symbol BITWISE_FIRST_BIT_SET = new Symbol("bitwise-first-bit-set");
   public final static Symbol BITWISE_IF = new Symbol("bitwise-if");
   public final static Symbol BITWISE_IOR = new Symbol("bitwise-ior");
   public final static Symbol BITWISE_LENGTH = new Symbol("bitwise-length");
   public final static Symbol BITWISE_NOT = new Symbol("bitwise-not");
   public final static Symbol BITWISE_REVERSE_BIT_FIELD = new Symbol("bitwise-reverse-bit-field");
   public final static Symbol BITWISE_ROTATE_BIT_FIELD = new Symbol("bitwise-rotate-bit-field");
   public final static Symbol BITWISE_XOR = new Symbol("bitwise-xor");
   public final static Symbol BOOLEAN_EQ_P = new Symbol("boolean=?");
   public final static Symbol BOOLEAN_P = new Symbol("boolean?");
   public final static Symbol BOUND_IDENTIFIER_EQ_P = new Symbol("bound-identifier=?");
   public final static Symbol BRANCH = new Symbol("branch");
   public final static Symbol BYTEVECTOR = new Symbol("bytevector");
   public final static Symbol BYTEVECTOR_APPEND = new Symbol("bytevector-append");
   public final static Symbol BYTEVECTOR_COPY = new Symbol("bytevector-copy");
   public final static Symbol BYTEVECTOR_COPY_M = new Symbol("bytevector-copy!");
   public final static Symbol BYTEVECTOR_EQ_P = new Symbol("bytevector=?");
   public final static Symbol BYTEVECTOR_FILL = new Symbol("bytevector-fill!");
   public final static Symbol BYTEVECTOR_IEEE_DOUBLE_NATIVE_REF = new Symbol("bytevector-ieee-double-native-ref");
   public final static Symbol BYTEVECTOR_IEEE_DOUBLE_NATIVE_SET = new Symbol("bytevector-ieee-double-native-set!");
   public final static Symbol BYTEVECTOR_IEEE_DOUBLE_REF = new Symbol("bytevector-ieee-double-ref");
   public final static Symbol BYTEVECTOR_IEEE_DOUBLE_SET = new Symbol("bytevector-ieee-double-set!");
   public final static Symbol BYTEVECTOR_IEEE_SINGLE_NATIVE_REF = new Symbol("bytevector-ieee-single-native-ref");
   public final static Symbol BYTEVECTOR_IEEE_SINGLE_NATIVE_SET = new Symbol("bytevector-ieee-single-native-set!");
   public final static Symbol BYTEVECTOR_IEEE_SINGLE_REF = new Symbol("bytevector-ieee-single-ref");
   public final static Symbol BYTEVECTOR_IEEE_SINGLE_SET = new Symbol("bytevector-ieee-single-set!");
   public final static Symbol BYTEVECTOR_LENGTH = new Symbol("bytevector-length");
   public final static Symbol BYTEVECTOR_P = new Symbol("bytevector?");
   public final static Symbol BYTEVECTOR_S16_NATIVE_REF = new Symbol("bytevector-s16-native-ref");
   public final static Symbol BYTEVECTOR_S16_NATIVE_SET = new Symbol("bytevector-s16-native-set!");
   public final static Symbol BYTEVECTOR_S16_REF = new Symbol("bytevector-s16-ref");
   public final static Symbol BYTEVECTOR_S16_SET = new Symbol("bytevector-s16-set!");
   public final static Symbol BYTEVECTOR_S32_NATIVE_REF = new Symbol("bytevector-s32-native-ref");
   public final static Symbol BYTEVECTOR_S32_NATIVE_SET = new Symbol("bytevector-s32-native-set!");
   public final static Symbol BYTEVECTOR_S32_REF = new Symbol("bytevector-s32-ref");
   public final static Symbol BYTEVECTOR_S32_SET = new Symbol("bytevector-s32-set!");
   public final static Symbol BYTEVECTOR_S64_NATIVE_REF = new Symbol("bytevector-s64-native-ref");
   public final static Symbol BYTEVECTOR_S64_NATIVE_SET = new Symbol("bytevector-s64-native-set!");
   public final static Symbol BYTEVECTOR_S64_REF = new Symbol("bytevector-s64-ref");
   public final static Symbol BYTEVECTOR_S64_SET = new Symbol("bytevector-s64-set!");
   public final static Symbol BYTEVECTOR_S8_REF = new Symbol("bytevector-s8-ref");
   public final static Symbol BYTEVECTOR_S8_SET = new Symbol("bytevector-s8-set!");
   public final static Symbol BYTEVECTOR_SINT_LIST = new Symbol("bytevector->sint-list");
   public final static Symbol BYTEVECTOR_SINT_REF = new Symbol("bytevector-sint-ref");
   public final static Symbol BYTEVECTOR_SINT_SET = new Symbol("bytevector-sint-set!");
   public final static Symbol BYTEVECTOR_U16_NATIVE_REF = new Symbol("bytevector-u16-native-ref");
   public final static Symbol BYTEVECTOR_U16_NATIVE_SET = new Symbol("bytevector-u16-native-set!");
   public final static Symbol BYTEVECTOR_U16_REF = new Symbol("bytevector-u16-ref");
   public final static Symbol BYTEVECTOR_U16_SET = new Symbol("bytevector-u16-set!");
   public final static Symbol BYTEVECTOR_U32_NATIVE_REF = new Symbol("bytevector-u32-native-ref");
   public final static Symbol BYTEVECTOR_U32_NATIVE_SET = new Symbol("bytevector-u32-native-set!");
   public final static Symbol BYTEVECTOR_U32_REF = new Symbol("bytevector-u32-ref");
   public final static Symbol BYTEVECTOR_U32_SET = new Symbol("bytevector-u32-set!");
   public final static Symbol BYTEVECTOR_U64_NATIVE_REF = new Symbol("bytevector-u64-native-ref");
   public final static Symbol BYTEVECTOR_U64_NATIVE_SET = new Symbol("bytevector-u64-native-set!");
   public final static Symbol BYTEVECTOR_U64_REF = new Symbol("bytevector-u64-ref");
   public final static Symbol BYTEVECTOR_U64_SET = new Symbol("bytevector-u64-set!");
   public final static Symbol BYTEVECTOR_U8_LIST = new Symbol("bytevector->u8-list");
   public final static Symbol BYTEVECTOR_U8_REF = new Symbol("bytevector-u8-ref");
   public final static Symbol BYTEVECTOR_U8_SET = new Symbol("bytevector-u8-set!");
   public final static Symbol BYTEVECTOR_UINT_LIST = new Symbol("bytevector->uint-list");
   public final static Symbol BYTEVECTOR_UINT_REF = new Symbol("bytevector-uint-ref");
   public final static Symbol BYTEVECTOR_UINT_SET = new Symbol("bytevector-uint-set!");
   public final static Symbol CAAAAR = new Symbol("caaaar");
   public final static Symbol CAAADR = new Symbol("caaadr");
   public final static Symbol CAAAR = new Symbol("caaar");
   public final static Symbol CAADAR = new Symbol("caadar");
   public final static Symbol CAADDR = new Symbol("caaddr");
   public final static Symbol CAADR = new Symbol("caadr");
   public final static Symbol CAAR = new Symbol("caar");
   public final static Symbol CADAAR = new Symbol("cadaar");
   public final static Symbol CADADR = new Symbol("cadadr");
   public final static Symbol CADAR = new Symbol("cadar");
   public final static Symbol CADDAR = new Symbol("caddar");
   public final static Symbol CADDDR = new Symbol("cadddr");
   public final static Symbol CADDR = new Symbol("caddr");
   public final static Symbol CADR = new Symbol("cadr");
   public final static Symbol CALL_AS_JAAS_SUBJECT = new Symbol("call-as-jaas-subject");
   public final static Symbol CALL_CC = new Symbol("call/cc");
   public final static Symbol CALL_NEXT = new Symbol("call-next");
   public final static Symbol CALL_WITH_CURRENT_CONTINUATION = new Symbol("call-with-current-continuation");
   public final static Symbol CALL_WITH_INPUT_FILE = new Symbol("call-with-input-file");
   public final static Symbol CALL_WITH_OUTPUT_FILE = new Symbol("call-with-output-file");
   public final static Symbol CALL_WITH_VALUES = new Symbol("call-with-values");
   public final static Symbol CAPTION = new Symbol("caption");
   public final static Symbol CAR = new Symbol("car");
   public final static Symbol CASE = new Symbol("case");
   public final static Symbol CASE_LAMBDA = new Symbol("case-lambda");
   public final static Symbol CAST = new Symbol("cast");
   public final static Symbol CDAAAR = new Symbol("cdaaar");
   public final static Symbol CDAADR = new Symbol("cdaadr");
   public final static Symbol CDAAR = new Symbol("cdaar");
   public final static Symbol CDADAR = new Symbol("cdadar");
   public final static Symbol CDADDR = new Symbol("cdaddr");
   public final static Symbol CDADR = new Symbol("cdadr");
   public final static Symbol CDAR = new Symbol("cdar");
   public final static Symbol CDDAAR = new Symbol("cddaar");
   public final static Symbol CDDADR = new Symbol("cddadr");
   public final static Symbol CDDAR = new Symbol("cddar");
   public final static Symbol CDDDAR = new Symbol("cdddar");
   public final static Symbol CDDDDR = new Symbol("cddddr");
   public final static Symbol CDDDR = new Symbol("cdddr");
   public final static Symbol CDDR = new Symbol("cddr");
   public final static Symbol CDR = new Symbol("cdr");
   public final static Symbol CEILING = new Symbol("ceiling");
   public final static Symbol CHANGED = new Symbol("changed");
   public final static Symbol CHAR_ALPHABETIC_P = new Symbol("char-alphabetic?");
   public final static Symbol CHAR_CI_EQ_P = new Symbol("char-ci=?");
   public final static Symbol CHAR_CI_GE_P = new Symbol("char-ci>=?");
   public final static Symbol CHAR_CI_GT_P = new Symbol("char-ci>?");
   public final static Symbol CHAR_CI_LE_P = new Symbol("char-ci<=?");
   public final static Symbol CHAR_CI_LT_P = new Symbol("char-ci<?");
   public final static Symbol CHAR_DOWNCASE = new Symbol("char-downcase");
   public final static Symbol CHAR_EQ_P = new Symbol("char=?");
   public final static Symbol CHAR_FOLDCASE = new Symbol("char-foldcase");
   public final static Symbol CHAR_GE_P = new Symbol("char>=?");
   public final static Symbol CHAR_GENERAL_CATEGORY = new Symbol("char-general-category");
   public final static Symbol CHAR_GT_P = new Symbol("char>?");
   public final static Symbol CHAR_INTEGER = new Symbol("char->integer");
   public final static Symbol CHAR_LE_P = new Symbol("char<=?");
   public final static Symbol CHAR_LOWER_CASE_P = new Symbol("char-lower-case?");
   public final static Symbol CHAR_LT_P = new Symbol("char<?");
   public final static Symbol CHAR_NUMERIC_P = new Symbol("char-numeric?");
   public final static Symbol CHAR_P = new Symbol("char?");
   public final static Symbol CHAR_READY_P = new Symbol("char-ready?");
   public final static Symbol CHAR_TITLE_CASE_P = new Symbol("char-title-case?");
   public final static Symbol CHAR_TITLECASE = new Symbol("char-titlecase");
   public final static Symbol CHAR_UPCASE = new Symbol("char-upcase");
   public final static Symbol CHAR_UPPER_CASE_P = new Symbol("char-upper-case?");
   public final static Symbol CHAR_WHITESPACE_P = new Symbol("char-whitespace?");
   public final static Symbol CLOSE_INPUT_PORT = new Symbol("close-input-port");
   public final static Symbol CLOSE_OUTPUT_PORT = new Symbol("close-output-port");
   public final static Symbol CODE = new Symbol("code");
   public final static Symbol COLLECTION = new Symbol("collection");
   public final static Symbol COLLECTION_P = new Symbol("collection?");
   public final static Symbol COLON = new Symbol(":");
   public final static Symbol COMPLEX_P = new Symbol("complex?");
   public final static Symbol COND = new Symbol("cond");
   public final static Symbol CONS = new Symbol("cons");
   public final static Symbol CONS_A = new Symbol("cons*");
   public final static Symbol CONVERT_SYMBOLS = new Symbol("convert-symbols");
   public final static Symbol COS = new Symbol("cos");
   public final static Symbol COUNT = new Symbol("count");
   public final static Symbol CREATE = new Symbol("create");
   public final static Symbol CURRENT_INPUT_PORT = new Symbol("current-input-port");
   public final static Symbol CURRENT_OUTPUT_PORT = new Symbol("current-output-port");
   public final static Symbol DATUM_SYNTAX = new Symbol("datum->syntax");
   public final static Symbol DECLARE = new Symbol("declare");
   public final static Symbol DEFINE = new Symbol("define");
   public final static Symbol DEFINE_ENUMERATION = new Symbol("define-enumeration");
   public final static Symbol DEFINE_MACRO = new Symbol("define-macro");
   public final static Symbol DEFINE_SYNTAX = new Symbol("define-syntax");
   public final static Symbol DELAY = new Symbol("delay");
   public final static Symbol DELETE = new Symbol("delete");
   public final static Symbol DELETE_FILE = new Symbol("delete-file");
   public final static Symbol DERIVED_ENVIRONMENT = new Symbol("derived-environment");
   public final static Symbol DESTINATION = new Symbol("destination");
   public final static Symbol DISPLAY = new Symbol("display");
   public final static Symbol DISPLAY_ORDER = new Symbol("displayOrder");
   public final static Symbol DIV = new Symbol("div");
   public final static Symbol DIV_AND_MOD = new Symbol("div-and-mod");
   public final static Symbol DIVIDE = new Symbol("/");
   public final static Symbol DIV0 = new Symbol("div0");
   public final static Symbol DIV0_AND_MOD0 = new Symbol("div0-and-mod0");
   public final static Symbol ELLIPSIS = new Symbol("...");
   public final static Symbol ELSE = new Symbol("else");
   public final static Symbol ENUM_SET_COMPLEMENT = new Symbol("enum-set-complement");
   public final static Symbol ENUM_SET_CONSTRUCTOR = new Symbol("enum-set-constructor");
   public final static Symbol ENUM_SET_DIFFERENCE = new Symbol("enum-set-difference");
   public final static Symbol ENUM_SET_EQ_P = new Symbol("enum-set=?");
   public final static Symbol ENUM_SET_INDEXER = new Symbol("enum-set-indexer");
   public final static Symbol ENUM_SET_INTERSECTION = new Symbol("enum-set-intersection");
   public final static Symbol ENUM_SET_LIST = new Symbol("enum-set->list");
   public final static Symbol ENUM_SET_MEMBER_P = new Symbol("enum-set-member?");
   public final static Symbol ENUM_SET_PROJECTION = new Symbol("enum-set-projection");
   public final static Symbol ENUM_SET_SUBSET_P = new Symbol("enum-set-subset?");
   public final static Symbol ENUM_SET_UNION = new Symbol("enum-set-union");
   public final static Symbol ENUM_SET_UNIVERSE = new Symbol("enum-set-universe");
   public final static Symbol EOF_OBJECT_P = new Symbol("eof-object?");
   public final static Symbol EQ = new Symbol("=");
   public final static Symbol EQ_P = new Symbol("eq?");
   public final static Symbol EQUAL_HASH = new Symbol("equal-hash");
   public final static Symbol EQUAL_P = new Symbol("equal?");
   public final static Symbol EQV_P = new Symbol("eqv?");
   public final static Symbol ERROR = new Symbol("error");
   public final static Symbol EVAL = new Symbol("eval");
   public final static Symbol EVEN_P = new Symbol("even?");
   public final static Symbol EVENT = new Symbol("event");
   public final static Symbol EXACT_INEXACT = new Symbol("exact->inexact");
   public final static Symbol EXACT_INTEGER_SQRT = new Symbol("exact-integer-sqrt");
   public final static Symbol EXACT_P = new Symbol("exact?");
   public final static Symbol EXP = new Symbol("exp");
   public final static Symbol EXPAND_MACRO = new Symbol("expand-macro");
   public final static Symbol EXPT = new Symbol("expt");
   public final static Symbol FIELD = new Symbol("field");
   public final static Symbol FILE_EXISTS_P = new Symbol("file-exists?");
   public final static Symbol FILTER = new Symbol("filter");
   public final static Symbol FINITE_P = new Symbol("finite?");
   public final static Symbol FLOOR = new Symbol("floor");
   public final static Symbol FOLD = new Symbol("fold");
   public final static Symbol FOR_EACH = new Symbol("for-each");
   public final static Symbol FORCE = new Symbol("force");
   public final static Symbol FORM = new Symbol("form");
   public final static Symbol FORMAT = new Symbol("format");
   public final static Symbol FREE_IDENTIFIER_EQ_P = new Symbol("free-identifier=?");
   public final static Symbol GCD = new Symbol("gcd");
   public final static Symbol GE = new Symbol(">=");
   public final static Symbol GENERATE_TEMPORARIES = new Symbol("generate-temporaries");
   public final static Symbol GENSYM = new Symbol("gensym");
   public final static Symbol GET = new Symbol("get");
   public final static Symbol GET_STRING_ALL =  new Symbol("get-string-all");
   public final static Symbol GET_VALUE = new Symbol("get-value");
   public final static Symbol GETQUEUE = new Symbol("getQueue");
   public final static Symbol GETTOKEN = new Symbol("getToken");
   public final static Symbol GLOBAL = new Symbol("global");
   public final static Symbol GT = new Symbol(">");
   public final static Symbol HASHSET_ADD = new Symbol("hashset-add!");
   public final static Symbol HASHSET_CLEAR = new Symbol("hashset-clear!");
   public final static Symbol HASHSET_CONTAINS = new Symbol("hashset-contains?");
   public final static Symbol HASHSET_COPY = new Symbol("hashset-copy");
   public final static Symbol HASHSET_EQUIVALENCE_FUNCTION = new Symbol("hashset-equivalence-function");
   public final static Symbol HASHSET_HASH_FUNCTION = new Symbol("hashset-hash-function");
   public final static Symbol HASHSET_MUTABLE_P = new Symbol("hashset-mutable?");
   public final static Symbol HASHSET_P = new Symbol("hashset?");
   public final static Symbol HASHSET_REMOVE = new Symbol("hashset-remove!");
   public final static Symbol HASHSET_SIZE = new Symbol("hashset-size");
   public final static Symbol HASHSET_VALUES = new Symbol("hashset-values");
   public final static Symbol HASHTABLE_CLEAR = new Symbol("hashtable-clear!");
   public final static Symbol HASHTABLE_CONTAINS_P = new Symbol("hashtable-contains?");
   public final static Symbol HASHTABLE_COPY = new Symbol("hashtable-copy");
   public final static Symbol HASHTABLE_DELETE = new Symbol("hashtable-delete!");
   public final static Symbol HASHTABLE_ENTRIES = new Symbol("hashtable-entries");
   public final static Symbol HASHTABLE_EQUIVALENCE_FUNCTION = new Symbol("hashtable-equivalence-function");
   public final static Symbol HASHTABLE_HASH_FUNCTION = new Symbol("hashtable-hash-function");
   public final static Symbol HASHTABLE_KEYS = new Symbol("hashtable-keys");
   public final static Symbol HASHTABLE_MUTABLE_P = new Symbol("hashtable-mutable?");
   public final static Symbol HASHTABLE_P = new Symbol("hashtable?");
   public final static Symbol HASHTABLE_REF = new Symbol("hashtable-ref");
   public final static Symbol HASHTABLE_SET = new Symbol("hashtable-set!");
   public final static Symbol HASHTABLE_SIZE = new Symbol("hashtable-size");
   public final static Symbol HASHTABLE_UPDATE = new Symbol("hashtable-update!");
   public final static Symbol HASHTABLE_VALUES = new Symbol("hashtable-values");
   public final static Symbol IDENTIFIER_P = new Symbol("identifier?");
   public final static Symbol IF = new Symbol("if");
   public final static Symbol IFNULL = new Symbol("ifnull");
   public final static Symbol IMPORT = new Symbol("import");
   public final static Symbol IN_P = new Symbol("in?");
   public final static Symbol IN_PRIVILEGE_P = new Symbol("in-privilege?");
   public final static Symbol INDEX = new Symbol("index");
   public final static Symbol INEXACT_EXACT = new Symbol("inexact->exact");
   public final static Symbol INEXACT_P = new Symbol("inexact?");
   public final static Symbol INFINITE_P = new Symbol("infinite?");
   public final static Symbol INITIAL_ENVIRONMENT = new Symbol("initial-environment");
   public final static Symbol INPUT_PORT_P = new Symbol("input-port?");
   public final static Symbol INSTANCE = new Symbol("instance");
   public final static Symbol INSTANCE_COLLECTION = new Symbol("instance-collection");
   public final static Symbol INSTANCE_COLLECTION_P = new Symbol("instance-collection?");
   public final static Symbol INSTANCE_P = new Symbol("instance?");
   public final static Symbol INTEGER_CHAR = new Symbol("integer->char");
   public final static Symbol INTEGER_P = new Symbol("integer?");
   public final static Symbol INTERACTION_ENVIRONMENT = new Symbol("interaction-environment");
   public final static Symbol INVOCATION_CONTEXT = new Symbol("invocation-context");
   public final static Symbol INVOKE = new Symbol("invoke");
   public final static Symbol ITERATABLE_P = new Symbol("iteratable?");
   public final static Symbol ITERATOR = new Symbol("iterator");
   public final static Symbol LAMBDA = new Symbol("lambda");
   public final static Symbol LCM = new Symbol("lcm");
   public final static Symbol LE = new Symbol("<=");
   public final static Symbol LENGTH = new Symbol("length");
   public final static Symbol LET = new Symbol("let");
   public final static Symbol LET_A = new Symbol("let*");
   public final static Symbol LET_A_VALUES = new Symbol("let*-values");
   public final static Symbol LET_SYNTAX = new Symbol("let-syntax");
   public final static Symbol LET_VALUES = new Symbol("let-values");
   public final static Symbol LETREC = new Symbol("letrec");
   public final static Symbol LETREC_A = new Symbol("letrec*");
   public final static Symbol LETREC_SYNTAX = new Symbol("letrec-syntax");
   public final static Symbol LIKE_P = new Symbol("like?");
   public final static Symbol LIST = new Symbol("list");
   public final static Symbol LIST_FILES = new Symbol("list-files");
   public final static Symbol LIST_P = new Symbol("list?");
   public final static Symbol LIST_REF = new Symbol("list-ref");
   public final static Symbol LIST_STRING = new Symbol("list->string");
   public final static Symbol LIST_TAIL = new Symbol("list-tail");
   public final static Symbol LIST_VECTOR = new Symbol("list->vector");
   public final static Symbol LITTLE = new Symbol("little");
   public final static Symbol LOAD = new Symbol("load");
   public final static Symbol LOCALE_NAME = new Symbol("locale-name");
   public final static Symbol LOCATOR = new Symbol("locator");
   public final static Symbol LOG = new Symbol("log");
   public final static Symbol LT = new Symbol("<");
   public final static Symbol MACRO = new Symbol("macro");
   public final static Symbol MACRO_P = new Symbol("macro?");
   public final static Symbol MAIN = new Symbol("main");
   public final static Symbol MAKE_BYTEVECTOR = new Symbol("make-bytevector");
   public final static Symbol MAKE_COLLECTION = new Symbol("make-collection");
   public final static Symbol MAKE_ENUMERATION = new Symbol("make-enumeration");
   public final static Symbol MAKE_EQ_HASHSET = new Symbol("make-eq-hashset");
   public final static Symbol MAKE_EQ_HASHTABLE = new Symbol("make-eq-hashtable");
   public final static Symbol MAKE_EQV_HASHSET = new Symbol("make-eqv-hashset");
   public final static Symbol MAKE_EQV_HASHTABLE = new Symbol("make-eqv-hashtable");
   public final static Symbol MAKE_HASHSET = new Symbol("make-hashset");
   public final static Symbol MAKE_HASHTABLE = new Symbol("make-hashtable");
   public final static Symbol MAKE_STRING = new Symbol("make-string");
   public final static Symbol MAKE_VARIABLE_TRANSFORMER = new Symbol("make-variable-transformer");
   public final static Symbol MAKE_VECTOR = new Symbol("make-vector");
   public final static Symbol MAP = new Symbol("map");
   public final static Symbol MATCH = new Symbol("match");
   public final static Symbol MAX = new Symbol("max");
   public final static Symbol MAXIMUM = new Symbol("maximum");
   public final static Symbol MEMBER = new Symbol("member");
   public final static Symbol MEMQ = new Symbol("memq");
   public final static Symbol MEMV = new Symbol("memv");
   public final static Symbol MESSAGE = new Symbol("message");
   public final static Symbol MIN = new Symbol("min");
   public final static Symbol MINIMUM = new Symbol("minimum");
   public final static Symbol MINUS = new Symbol("-");
   public final static Symbol MOD = new Symbol("mod");
   public final static Symbol MOD0 = new Symbol("mod0");
   public final static Symbol MODEL = new Symbol("model");
   public final static Symbol MODULO = new Symbol("modulo");
   public final static Symbol MUL = new Symbol("*");
   public final static Symbol NAME = new Symbol("name");
   public final static Symbol NAN_P = new Symbol("nan?");
   public final static Symbol NE = new Symbol("!=");
   public final static Symbol NEGATIVE_P = new Symbol("negative?");
   public final static Symbol NEW = new Symbol("new");
   public final static Symbol NEWLINE = new Symbol("newline");
   public final static Symbol NOT = new Symbol("not");
   public final static Symbol NOW = new Symbol("now");
   public final static Symbol NULL_P = new Symbol("null?");
   public final static Symbol NUMBER_P = new Symbol("number?");
   public final static Symbol NUMBER_STRING = new Symbol("number->string");
   public final static Symbol OBJECT = new Symbol("object");
   public final static Symbol ODD_P = new Symbol("odd?");
   public final static Symbol OID = new Symbol("oid");
   public final static Symbol OLD = new Symbol("old");
   public final static Symbol OPEN_INPUT_FILE = new Symbol("open-input-file");
   public final static Symbol OPEN_INPUT_STRING = new Symbol("open-input-string");
   public final static Symbol OPEN_OUTPUT_FILE = new Symbol("open-output-file");
   public final static Symbol OPEN_OUTPUT_FORMATTER = new Symbol("open-output-formatter");
   public final static Symbol OPEN_OUTPUT_STRING = new Symbol("open-output-string");
   public final static Symbol OPTION = new Symbol("option");
   public final static Symbol OR = new Symbol("or");
   public final static Symbol ORDINAL = new Symbol("ordinal");
   public final static Symbol OUTPUT_PORT_P = new Symbol("output-port?");
   public final static Symbol OWNER = new Symbol("owner");
   public final static Symbol PAIR_P = new Symbol("pair?");
   public final static Symbol PARAMETER = new Symbol("parameter");
   public final static Symbol PEEK_CHAR = new Symbol("peek-char");
   public final static Symbol PERIOD = new Symbol(".");
   public final static Symbol PLUS = new Symbol("+");
   public final static Symbol POSITIVE_P = new Symbol("positive?");
   public final static Symbol PRETTY_FORMAT = new Symbol("pretty-format");
   public final static Symbol PRETTY_WRITE = new Symbol("pretty-write");
   public final static Symbol PRIORITY = new Symbol("priority");
   public final static Symbol PROCEDURE_P = new Symbol("procedure?");
   public final static Symbol QUASIQUOTE = new Symbol("quasiquote");
   public final static Symbol QUASISYNTAX = new Symbol("quasisyntax");
   public final static Symbol QUEUE = new Symbol("queue");
   public final static Symbol QUOTE = new Symbol("quote");
   public final static Symbol QUOTIENT = new Symbol("quotient");
   public final static Symbol RATIONAL_P = new Symbol("rational?");
   public final static Symbol READ = new Symbol("read");
   public final static Symbol READ_CHAR = new Symbol("read-char");
   public final static Symbol REAL_P = new Symbol("real?");
   public final static Symbol REDEFINE_INTRINSICS = new Symbol("redefine-intrinsics");
   public final static Symbol RELEASE = new Symbol("release");
   public final static Symbol REMAINDER = new Symbol("remainder");
   public final static Symbol REMOVE = new Symbol("remove");
   public final static Symbol REMQ = new Symbol("remq");
   public final static Symbol REMV = new Symbol("remv");
   public final static Symbol REMOVED = new Symbol("removed");
   public final static Symbol REVERSE = new Symbol("reverse");
   public final static Symbol ROLE = new Symbol("role");
   public final static Symbol ROUND = new Symbol("round");
   public final static Symbol SCOPE = new Symbol("scope");
   public final static Symbol SELECTION = new Symbol("selection");
   public final static Symbol SEND = new Symbol("send");
   public final static Symbol SET = new Symbol("set!");
   public final static Symbol SET_CAR = new Symbol("set-car!");
   public final static Symbol SET_CDR = new Symbol("set-cdr!");
   public final static Symbol SET_CURRENT_INPUT_PORT = new Symbol("set-current-input-port!");
   public final static Symbol SET_CURRENT_OUTPUT_PORT = new Symbol("set-current-output-port!");
   public final static Symbol SHARED = new Symbol("shared");
   public final static Symbol SIN = new Symbol("sin");
   public final static Symbol SINT_LIST_BYTEVECTOR = new Symbol("sint-list->bytevector");
   public final static Symbol SIZE = new Symbol("size");
   public final static Symbol SOURCE = new Symbol("source");
   public final static Symbol SQRT = new Symbol("sqrt");
   public final static Symbol START = new Symbol("start");
   public final static Symbol STRING = new Symbol("string");
   public final static Symbol STRING_AFFIX = new Symbol("string-affix");
   public final static Symbol STRING_APPEND = new Symbol("string-append");
   public final static Symbol STRING_CI_EQ_P = new Symbol("string-ci=?");
   public final static Symbol STRING_CI_GE_P = new Symbol("string-ci>=?");
   public final static Symbol STRING_CI_GT_P = new Symbol("string-ci>?");
   public final static Symbol STRING_CI_HASH = new Symbol("string-ci-hash");
   public final static Symbol STRING_CI_LE_P = new Symbol("string-ci<=?");
   public final static Symbol STRING_CI_LT_P = new Symbol("string-ci<?");
   public final static Symbol STRING_COPY = new Symbol("string-copy");
   public final static Symbol STRING_DOWNCASE = new Symbol("string-downcase");
   public final static Symbol STRING_EMPTY_P = new Symbol("string-empty?");
   public final static Symbol STRING_EQ_P = new Symbol("string=?");
   public final static Symbol STRING_EOL_EQ_P = new Symbol("string-eol=?");
   public final static Symbol STRING_EXPAND = new Symbol("string-expand");
   public final static Symbol STRING_FOLDCASE = new Symbol("string-foldcase");
   public final static Symbol STRING_GE_P = new Symbol("string>=?");
   public final static Symbol STRING_GT_P = new Symbol("string>?");
   public final static Symbol STRING_HASH = new Symbol("string-hash");
   public final static Symbol STRING_JOIN = new Symbol("string-join");
   public final static Symbol STRING_LE_P = new Symbol("string<=?");
   public final static Symbol STRING_LENGTH = new Symbol("string-length");
   public final static Symbol STRING_LIST = new Symbol("string->list");
   public final static Symbol STRING_LT_P = new Symbol("string<?");
   public final static Symbol STRING_MATCH = new Symbol("string-match");
   public final static Symbol STRING_NUMBER = new Symbol("string->number");
   public final static Symbol STRING_P = new Symbol("string?");
   public final static Symbol STRING_PATTERN = new Symbol("string-pattern");
   public final static Symbol STRING_REF = new Symbol("string-ref");
   public final static Symbol STRING_REPLACE = new Symbol("string-replace");
   public final static Symbol STRING_SPLIT = new Symbol("string-split");
   public final static Symbol STRING_SYMBOL = new Symbol("string->symbol");
   public final static Symbol STRING_TITLECASE = new Symbol("string-titlecase");
   public final static Symbol STRING_TRIM = new Symbol("string-trim");
   public final static Symbol STRING_UPCASE = new Symbol("string-upcase");
   public final static Symbol STRING_UTF16 = new Symbol("string->utf16");
   public final static Symbol STRING_UTF32 = new Symbol("string->utf32");
   public final static Symbol STRING_UTF8 = new Symbol("string->utf8");
   public final static Symbol SUBSTRING = new Symbol("substring");
   public final static Symbol SUM = new Symbol("sum");
   public final static Symbol SYMBOL_EQ_P = new Symbol("symbol=?");
   public final static Symbol SYMBOL_HASH = new Symbol("symbol-hash");
   public final static Symbol SYMBOL_P = new Symbol("symbol?");
   public final static Symbol SYMBOL_STRING = new Symbol("symbol->string");
   public final static Symbol SYNTAX = new Symbol("syntax");
   public final static Symbol SYNTAX_CASE = new Symbol("syntax-case");
   public final static Symbol SYNTAX_DATUM = new Symbol("syntax->datum");
   public final static Symbol SYNTAX_RULES = new Symbol("syntax-rules");
   public final static Symbol SYS_ASSERT = new Symbol("sys:assert");
   public final static Symbol SYS_AUDIT = new Symbol("sys:audit");
   public final static Symbol SYS_CAST = new Symbol("sys:cast");
   public final static Symbol SYS_CHECK_ACCESS = new Symbol("sys:check-access");
   public final static Symbol SYS_CHECK_PRIVILEGE = new Symbol("sys:check-privilege");
   public final static Symbol SYS_CURRENT_LOGGER = new Symbol("sys:current-logger");
   public final static Symbol SYS_DEFINE_LET_SYNTAX = new Symbol("sys:define-let-syntax");
   public final static Symbol SYS_EQUAL_P = new Symbol("sys:equal?");
   public final static Symbol SYS_EQ_P = new Symbol("sys:eq?");
   public final static Symbol SYS_FINALIZE = new Symbol("sys:finalize");
   public final static Symbol SYS_GENERATE_EVENT = new Symbol("sys:generate-event");
   public final static Symbol SYS_GENERATE_FLOW_FUNCTION = new Symbol("sys:generate-flow-function");
   public final static Symbol SYS_LOG = new Symbol("sys:log");
   public final static Symbol SYS_LOG_LOCALIZED = new Symbol("sys:log-localized");
   public final static Symbol SYS_RETURN_VALUES = new Symbol("sys:return-values");
   public final static Symbol SYS_SET_BARRIER = new Symbol("sys:set-barrier");
   public final static Symbol SYS_SET_DIRTY = new Symbol("sys:set-dirty!");
   public final static Symbol SYS_SET_NEW = new Symbol("sys:set-new!");
   public final static Symbol SYS_TRY = new Symbol("sys:try");
   public final static Symbol SYS_TX_BEGIN = new Symbol("sys:tx-begin");
   public final static Symbol SYS_TX_COMMIT = new Symbol("sys:tx-commit");
   public final static Symbol SYS_TX_COMMIT0 = new Symbol("sys:tx-commit0");
   public final static Symbol SYS_TX_MANDATE = new Symbol("sys:tx-mandate");
   public final static Symbol SYS_TX_MANDATE_NONE = new Symbol("sys:tx-mandate-none");
   public final static Symbol SYS_TX_REQUIRE = new Symbol("sys:tx-require");
   public final static Symbol SYS_TX_ROLLBACK = new Symbol("sys:tx-rollback");
   public final static Symbol SYS_TX_SUSPEND = new Symbol("sys:tx-suspend");
   public final static Symbol SYSWORKFLOWQUEUE = new Symbol("SysWorkflowQueue");
   public final static Symbol TAN = new Symbol("tan");
   public final static Symbol TARGET = new Symbol("target");
   public final static Symbol TARGETFUNCTION = new Symbol("targetFunction");
   public final static Symbol THIS = new Symbol("this");
   public final static Symbol THROW = new Symbol("throw");
   public final static Symbol TIMERS = new Symbol("timers");
   public final static Symbol TIMESTAMP_P = new Symbol("timestamp?");
   public final static Symbol TRIGGER = new Symbol("trigger");
   public final static Symbol TRUNCATE = new Symbol("truncate");
   public final static Symbol TYPE = new Symbol("type");
   public final static Symbol U8_LIST_BYTEVECTOR = new Symbol("u8-list->bytevector");
   public final static Symbol UIMODE = new Symbol("uimode");
   public final static Symbol UINT_LIST_BYTEVECTOR = new Symbol("uint-list->bytevector");
   public final static Symbol UNDERSCORE = new Symbol("_");
   public final static Symbol UNICODE_CC = new Symbol("Cc");
   public final static Symbol UNICODE_CF = new Symbol("Cf");
   public final static Symbol UNICODE_CN = new Symbol("Cn");
   public final static Symbol UNICODE_CO = new Symbol("Co");
   public final static Symbol UNICODE_CS = new Symbol("Cs");
   public final static Symbol UNICODE_LL = new Symbol("Ll");
   public final static Symbol UNICODE_LM = new Symbol("Lm");
   public final static Symbol UNICODE_LO = new Symbol("Lo");
   public final static Symbol UNICODE_LT = new Symbol("Lt");
   public final static Symbol UNICODE_LU = new Symbol("Lu");
   public final static Symbol UNICODE_MC = new Symbol("Mc");
   public final static Symbol UNICODE_ME = new Symbol("Me");
   public final static Symbol UNICODE_MN = new Symbol("Mn");
   public final static Symbol UNICODE_ND = new Symbol("Nd");
   public final static Symbol UNICODE_NL = new Symbol("Nl");
   public final static Symbol UNICODE_NO = new Symbol("No");
   public final static Symbol UNICODE_PC = new Symbol("Pc");
   public final static Symbol UNICODE_PD = new Symbol("Pd");
   public final static Symbol UNICODE_PE = new Symbol("Pe");
   public final static Symbol UNICODE_PF = new Symbol("Pf");
   public final static Symbol UNICODE_PI = new Symbol("Pi");
   public final static Symbol UNICODE_PO = new Symbol("Po");
   public final static Symbol UNICODE_PS = new Symbol("Ps");
   public final static Symbol UNICODE_SC = new Symbol("Sc");
   public final static Symbol UNICODE_SK = new Symbol("Sk");
   public final static Symbol UNICODE_SM = new Symbol("Sm");
   public final static Symbol UNICODE_SO = new Symbol("So");
   public final static Symbol UNICODE_ZL = new Symbol("Zl");
   public final static Symbol UNICODE_ZP = new Symbol("Zp");
   public final static Symbol UNICODE_ZS = new Symbol("Zs");
   public final static Symbol UNION = new Symbol("union");
   public final static Symbol UNIQUE = new Symbol("unique");
   public final static Symbol UNLESS = new Symbol("unless");
   public final static Symbol UNQUOTE = new Symbol("unquote");
   public final static Symbol UNQUOTE_SPLICING = new Symbol("unquote-splicing");
   public final static Symbol UNSYNTAX = new Symbol("unsyntax");
   public final static Symbol UNSYNTAX_SPLICING = new Symbol("unsyntax-splicing");
   public final static Symbol UPDATE = new Symbol("update");
   public final static Symbol UTF16_STRING = new Symbol("utf16->string");
   public final static Symbol UTF32_STRING = new Symbol("utf32->string");
   public final static Symbol UTF8_STRING = new Symbol("utf8->string");
   public final static Symbol VALUE = new Symbol("value");
   public final static Symbol VALUE_COLLECTION = new Symbol("value->collection");
   public final static Symbol VALUES = new Symbol("values");
   public final static Symbol VECTOR = new Symbol("vector");
   public final static Symbol VECTOR_APPEND = new Symbol("vector-append");
   public final static Symbol VECTOR_FILL = new Symbol("vector-fill!");
   public final static Symbol VECTOR_LENGTH = new Symbol("vector-length");
   public final static Symbol VECTOR_LIST = new Symbol("vector->list");
   public final static Symbol VECTOR_P = new Symbol("vector?");
   public final static Symbol VECTOR_REF = new Symbol("vector-ref");
   public final static Symbol VECTOR_SET = new Symbol("vector-set!");
   public final static Symbol VECTOR_SORT = new Symbol("vector-sort!");
   public final static Symbol WHEN = new Symbol("when");
   public final static Symbol WORKFLOW = new Symbol("workflow");
   public final static Symbol WRITE = new Symbol("write");
   public final static Symbol WRITE_CHAR = new Symbol("write-char");
   public final static Symbol ZERO_P = new Symbol("zero?");

   /**
    * The Java stream unique identifier for the class.
    */
   private final static long serialVersionUID = 1023183610528120325L;

   /**
    * The gensym prefix.
    */
   protected final static char GENSYM_PREFIX = '#';

   /**
    * The gensym identifier separator.
    */
   protected final static char GENSYM_SEP = '~';

   // attributes

   /**
    * The string to which the symbol corresponds.
    */
   private final String m_sName;

   /**
    * The global symbol map: Symbol[String]. Requires synchronized access.
    */
   private final static Lookup s_globalMap = new SoftHashTab(512);

   static
   {
      Field[] fieldArray = Symbol.class.getFields();

      try
      {
         for (int i = 0; i < fieldArray.length; ++i)
         {
            Field field = fieldArray[i];

            if (field.getType() == Symbol.class)
            {
               Symbol sym = (Symbol)field.get(null);

               s_globalMap.put(sym.getName(), sym);
            }
         }
      }
      catch (IllegalAccessException e)
      {
      }
   }

   // constructors

   /**
    * Creates a symbol with a given string name.
    * This constructor is for INTERNAL USE ONLY.
    * @param sName The name of the symbol.
    */
   protected Symbol(String sName)
   {
      m_sName = sName;
   }

   /**
    * Creates a symbol with a randomly generated string name.
    * This constructor is for INTERNAL USE ONLY.
    */
   protected Symbol()
   {
      int nHashCode = System.identityHashCode(this);
      StringBuilder sBuf = new StringBuilder(12);

      sBuf.append(GENSYM_PREFIX);
      sBuf.append(nHashCode);

      m_sName = sBuf.toString();
   }

   // operations

   /**
    * Returns the symbol name.
    * @return The symbol name.
    */
   public final String getName()
   {
      return m_sName;
   }

   /**
    * @see java.lang.CharSequence#length()
    */
   public int length()
   {
      return m_sName.length();
   }

   /**
    * @see java.lang.CharSequence#charAt(int)
    */
   public char charAt(int nIndex)
   {
      return m_sName.charAt(nIndex);
   }

   /**
    * @see java.lang.CharSequence#subSequence(int, int)
    */
   public CharSequence subSequence(int nStart, int nEnd)
   {
      return m_sName.subSequence(nStart, nEnd);
   }

   /**
    * @see java.lang.Comparable#compareTo(java.lang.Object)
    */
   public int compareTo(Object obj)
   {
      return m_sName.compareTo(((Symbol)obj).getName());
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      return obj instanceof Symbol && m_sName.equals(((Symbol)obj).getName());
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return m_sName.hashCode();
   }

   /**
    * @see nexj.core.util.Printable#printOn(PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write(m_sName);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public final String toString()
   {
      return m_sName;
   }

   /**
    * Defines a symbol.
    * @param sName The symbol name.
    * @return The defined symbol.
    */
   public final static synchronized Symbol define(String sName)
   {
      assert sName != null;

      Symbol sym = (Symbol)s_globalMap.get(sName);

      if (sym != null)
      {
         return sym;
      }

      sym = new Symbol(sName);
      s_globalMap.put(sName, sym);

      return sym;
   }

   /**
    * Clones the given symbol and returns a new Symbol object of the same name.
    * @param sym The symbol to clone.
    * @return A new symbol.
    */
   public final static Symbol cloneSymbol(Symbol sym)
   {
      return new Symbol(sym.m_sName);
   }

   /**
    * Returns a new symbol with a randomly generated name.
    * @return A new symbol.
    */
   public final static Symbol generateSymbol()
   {
      return new Symbol();
   }

   /**
    * Generates and returns a symbol comprised of the gensym prefix and a generated number.
    * @return A new symbol.
    */
   public final static Symbol generateSymbol(Symbol original, int nId)
   {
      String sName = original.getName();
      StringBuilder sBuf = new StringBuilder(sName + 16);

      sBuf.append(GENSYM_PREFIX);
      sBuf.append(sName);
      sBuf.append(GENSYM_SEP);
      sBuf.append(nId);

      return new Symbol(sBuf.toString());
   }

   /**
    * java.io serialization API method.
    * Returns a singleton reference.
    */
   private Object readResolve() throws java.io.ObjectStreamException
   {
      return define(m_sName);
   }
}