// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.text.DateFormatSymbols;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Comparator;
import java.util.Locale;
import java.util.regex.Pattern;

import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.TZ;

/**
 * A primitive type, i.e. a type of an atomic immutable value.
 */
public final class Primitive extends GenericType
{
   // constants

   /**
    * Primitive ordinal numbers.
    */
   public final static int STRING_ORDINAL = 0;
   public final static int BINARY_ORDINAL = 1;
   public final static int INTEGER_ORDINAL = 2;
   public final static int LONG_ORDINAL = 3;
   public final static int DECIMAL_ORDINAL = 4;
   public final static int FLOAT_ORDINAL = 5;
   public final static int DOUBLE_ORDINAL = 6;
   public final static int TIMESTAMP_ORDINAL = 7;
   public final static int BOOLEAN_ORDINAL = 8;
   public final static int ANY_ORDINAL = 9;
   private final static int NULL_ORDINAL = 10;

   /**
    * Primitive type constants.
    */
   public final static Primitive STRING = new Primitive("string", String.class, STRING_ORDINAL);
   public final static Primitive BINARY = new Primitive("binary", Binary.class, BINARY_ORDINAL);
   public final static Primitive INTEGER = new Primitive("integer", Integer.class, INTEGER_ORDINAL);
   public final static Primitive LONG = new Primitive("long", Long.class, LONG_ORDINAL);
   public final static Primitive DECIMAL = new Primitive("decimal", BigDecimal.class, DECIMAL_ORDINAL);
   public final static Primitive FLOAT = new Primitive("float", Float.class, FLOAT_ORDINAL);
   public final static Primitive DOUBLE = new Primitive("double", Double.class, DOUBLE_ORDINAL);
   public final static Primitive TIMESTAMP = new Primitive("timestamp", Timestamp.class, TIMESTAMP_ORDINAL);
   public final static Primitive BOOLEAN = new Primitive("boolean", Boolean.class, BOOLEAN_ORDINAL);
   public final static Primitive ANY = new Primitive("any", Symbol.class, ANY_ORDINAL);
   private final static Primitive NULL = new Primitive("null", null, NULL_ORDINAL);

   /**
    * Commonly used primitive constant values.
    */
   public final static Integer ZERO_INTEGER = new Integer(0);
   public final static Integer ONE_INTEGER = new Integer(1);
   public final static Long ZERO_LONG = new Long(0);
   public final static Long ONE_LONG = new Long(1);
   public final static BigDecimal ZERO_DECIMAL = BigDecimal.valueOf(0);
   public final static BigDecimal ONE_DECIMAL =  BigDecimal.valueOf(1);
   public final static Float ZERO_FLOAT = new Float(0);
   public final static Float ONE_FLOAT = new Float(1);
   public final static Double ZERO_DOUBLE = new Double(0);
   public final static Double ONE_DOUBLE = new Double(1);
   public final static Double POSITIVE_INF_DOUBLE = new Double(Double.POSITIVE_INFINITY);
   public final static Double NEGATIVE_INF_DOUBLE = new Double(Double.NEGATIVE_INFINITY);
   public final static Double NAN_DOUBLE = new Double(Double.NaN);

   /**
    * The scale that is used as a big decimal exactness threshold.
    */
   public final static int MAX_SCALE = 40;

   /**
    * Comparator for two primitive values.
    */
   public final static Comparator COMPARATOR = new Comparator()
   {
      public int compare(Object left, Object right)
      {
         if (lt(left, right) == Boolean.TRUE)
         {
            return -1;
         }

         if (eq(left, right) == Boolean.TRUE)
         {
            return 0;
         }

         return 1;
      }
   };

   /**
    * The identity function - just returns its argument.
    */
   public final static UnaryFunction IDENTITY_FUNCTION = new UnaryFunction()
   {
      public Object invoke(Object arg)
      {
         return arg;
      }
   };

   /**
    * Null unary function - returns null.
    */
   private final static UnaryFunction NULL_UNARY_FUNCTION = new UnaryFunction()
   {
      public Object invoke(Object arg)
      {
         return null;
      }
   };

   /**
    * Null binary function - returns null.
    */
   private final static BinaryFunction NULL_BINARY_FUNCTION = new BinaryFunction()
   {
      public Object invoke(Object left, Object right)
      {
         return null;
      }
   };

   /**
    * True binary function - returns true.
    */
   private final static BinaryFunction TRUE_BINARY_FUNCTION = new BinaryFunction()
   {
      public Object invoke(Object left, Object right)
      {
         return Boolean.TRUE;
      }
   };

   /**
    * False binary function - returns false.
    */
   private final static BinaryFunction FALSE_BINARY_FUNCTION = new BinaryFunction()
   {
      public Object invoke(Object left, Object right)
      {
         return Boolean.FALSE;
      }
   };

   /**
    * IsNull1 binary function - returns true if the 1st argument is null.
    */
   private final static BinaryFunction ISNULL1_BINARY_FUNCTION = new BinaryFunction()
   {
      public Object invoke(Object left, Object right)
      {
         return Boolean.valueOf(left == null);
      }
   };

   /**
    * IsNotNull1 binary function - returns true if the 1st argument is not null.
    */
   private final static BinaryFunction ISNOTNULL1_BINARY_FUNCTION = new BinaryFunction()
   {
      public Object invoke(Object left, Object right)
      {
         return Boolean.valueOf(left != null);
      }
   };

   /**
    * IsNull2 binary function - returns true if the 2nd argument is null.
    */
   private final static BinaryFunction ISNULL2_BINARY_FUNCTION = new BinaryFunction()
   {
      public Object invoke(Object left, Object right)
      {
         return Boolean.valueOf(right == null);
      }
   };

   /**
    * IsNotNull2 binary function - returns true if the 2nd argument is not null.
    */
   private final static BinaryFunction ISNOTNULL2_BINARY_FUNCTION = new BinaryFunction()
   {
      public Object invoke(Object left, Object right)
      {
         return Boolean.valueOf(right != null);
      }
   };

   /**
    * The number of primitive types.
    */
   public final static int MAX_COUNT = 10;

   // attributes

   /**
    * The type ordinal number.
    */
   private final int m_nOrdinal;

   // associations

   /**
    * The logical negation function.
    */
   private UnaryFunction m_notFunction;

   /**
    * The arithmetic negation function.
    */
   private UnaryFunction m_negationFunction;

   /**
    * The type converter array, indexed by Primitive ordinal number.
    */
   private final UnaryFunction[] m_converterArray = new UnaryFunction[MAX_COUNT + 1];

   /**
    * The multiplication function array, indexed by Primitive ordinal number.
    */
   private final BinaryFunction[] m_multiplicationArray = new BinaryFunction[MAX_COUNT + 1];

   /**
    * The division function array, indexed by Primitive ordinal number.
    */
   private final BinaryFunction[] m_divisionArray = new BinaryFunction[MAX_COUNT + 1];

   /**
    * The plus function array, indexed by Primitive ordinal number.
    */
   private final BinaryFunction[] m_plusArray = new BinaryFunction[MAX_COUNT + 1];

   /**
    * The minus function array, indexed by Primitive ordinal number.
    */
   private final BinaryFunction[] m_minusArray = new BinaryFunction[MAX_COUNT + 1];

   /**
    * The like function array, indexed by Primitive ordinal number.
    */
   private final BinaryFunction[] m_likeArray = new BinaryFunction[MAX_COUNT + 1];

   /**
    * The gt function array, indexed by Primitive ordinal number.
    */
   private final BinaryFunction[] m_gtArray = new BinaryFunction[MAX_COUNT + 1];

   /**
    * The ge function array, indexed by Primitive ordinal number.
    */
   private final BinaryFunction[] m_geArray = new BinaryFunction[MAX_COUNT + 1];

   /**
    * The lt function array, indexed by Primitive ordinal number.
    */
   private final BinaryFunction[] m_ltArray = new BinaryFunction[MAX_COUNT + 1];

   /**
    * The le function array, indexed by Primitive ordinal number.
    */
   private final BinaryFunction[] m_leArray = new BinaryFunction[MAX_COUNT + 1];

   /**
    * The eq function array, indexed by Primitive ordinal number.
    */
   private final BinaryFunction[] m_eqArray = new BinaryFunction[MAX_COUNT + 1];

   /**
    * The ne function array, indexed by Primitive ordinal number.
    */
   private final BinaryFunction[] m_neArray = new BinaryFunction[MAX_COUNT + 1];

   /**
    * The and function array, indexed by Primitive ordinal number.
    */
   private final BinaryFunction[] m_andArray = new BinaryFunction[MAX_COUNT + 1];

   /**
    * The or function array, indexed by Primitive ordinal number.
    */
   private final BinaryFunction[] m_orArray = new BinaryFunction[MAX_COUNT + 1];

   /**
    * The Java class representing the type.
    */
   private final Class m_classObject;

   /**
    * Map of type name to a primitive type object - Primitive[String].
    */
   private final static Lookup s_nameMap = new HashTab(MAX_COUNT);

   /**
    * The primitive type array, indexed by type ordinal number.
    */
   private final static Primitive[] s_primitiveArray = new Primitive[]
   {
      STRING,
      BINARY,
      INTEGER,
      LONG,
      DECIMAL,
      FLOAT,
      DOUBLE,
      TIMESTAMP,
      BOOLEAN,
      ANY,
      NULL,
   };

   /**
    * Maps a class object to a primitive type: Primitive[Class].
    */
   private final static Lookup s_classMap = new HashTab(MAX_COUNT - 1);

   static
   {
      // Initialize the type converters and operators

      STRING.setConverter(STRING, IDENTITY_FUNCTION);

      STRING.setConverter(BINARY, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return Primitive.toString((Binary)arg);
         }
      });

      STRING.setConverter(INTEGER, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return Primitive.toString((Integer)arg);
         }
      });

      STRING.setConverter(LONG, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return Primitive.toString((Long)arg);
         }
      });

      STRING.setConverter(DECIMAL, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return Primitive.toString((BigDecimal)arg);
         }
      });

      STRING.setConverter(FLOAT, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return Primitive.toString((Float)arg);
         }
      });

      STRING.setConverter(DOUBLE, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return Primitive.toString((Double)arg);
         }
      });

      STRING.setConverter(TIMESTAMP, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return Primitive.toString((Timestamp)arg);
         }
      });

      STRING.setConverter(BOOLEAN, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return Primitive.toString((Boolean)arg);
         }
      });

      STRING.setConverter(NULL, NULL_UNARY_FUNCTION);

      STRING.setLikeFunction(STRING, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return like((String)left, (String)right);
         }
      });

      STRING.setLikeFunction(NULL, ISNULL1_BINARY_FUNCTION);

      STRING.setGTFunction(STRING, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((String)left, (String)right);
         }
      });

      STRING.setGEFunction(STRING, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((String)left, (String)right);
         }
      });

      STRING.setLTFunction(STRING, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((String)left, (String)right);
         }
      });

      STRING.setLEFunction(STRING, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((String)left, (String)right);
         }
      });

      STRING.setEQFunction(STRING, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((String)left, (String)right);
         }
      });

      STRING.setNEFunction(STRING, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((String)left, (String)right);
         }
      });

      BINARY.setConverter(BINARY, IDENTITY_FUNCTION);

      BINARY.setConverter(STRING, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toBinary((String)arg);
         }
      });

      BINARY.setConverter(NULL, NULL_UNARY_FUNCTION);

      BINARY.setGTFunction(BINARY, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((Binary)left, (Binary)right);
         }
      });

      BINARY.setGEFunction(BINARY, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((Binary)left, (Binary)right);
         }
      });

      BINARY.setLTFunction(BINARY, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((Binary)left, (Binary)right);
         }
      });

      BINARY.setLEFunction(BINARY, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((Binary)left, (Binary)right);
         }
      });

      BINARY.setEQFunction(BINARY, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((Binary)left, (Binary)right);
         }
      });

      BINARY.setNEFunction(BINARY, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((Binary)left, (Binary)right);
         }
      });

      INTEGER.setConverter(STRING, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toInteger((String)arg);
         }
      });

      INTEGER.setConverter(INTEGER, IDENTITY_FUNCTION);

      INTEGER.setConverter(LONG, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toInteger((Long)arg);
         }
      });

      INTEGER.setConverter(DECIMAL, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toInteger((BigDecimal)arg);
         }
      });

      INTEGER.setConverter(FLOAT, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toInteger((Float)arg);
         }
      });

      INTEGER.setConverter(DOUBLE, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toInteger((Double)arg);
         }
      });

      INTEGER.setConverter(TIMESTAMP, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toInteger((Timestamp)arg);
         }
      });

      INTEGER.setConverter(BOOLEAN, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toInteger((Boolean)arg);
         }
      });

      INTEGER.setConverter(NULL, NULL_UNARY_FUNCTION);

      INTEGER.setNegationFunction(new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return negate((Integer)arg);
         }
      });

      INTEGER.setMultiplicationFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply((Integer)left, (Integer)right);
         }
      });

      INTEGER.setMultiplicationFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply(toLong((Integer)left), (Long)right);
         }
      });

      INTEGER.setMultiplicationFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply(toDecimal((Integer)left), (BigDecimal)right);
         }
      });

      INTEGER.setMultiplicationFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply(toFloat((Integer)left), (Float)right);
         }
      });

      INTEGER.setMultiplicationFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply(toDouble((Integer)left), (Double)right);
         }
      });

      INTEGER.setMultiplicationFunction(NULL, NULL_BINARY_FUNCTION);

      INTEGER.setDivisionFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((Integer)left, (Integer)right);
         }
      });

      INTEGER.setDivisionFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide(toLong((Integer)left), (Long)right);
         }
      });

      INTEGER.setDivisionFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide(toDecimal((Integer)left), (BigDecimal)right);
         }
      });

      INTEGER.setDivisionFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide(toFloat((Integer)left), (Float)right);
         }
      });

      INTEGER.setDivisionFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide(toDouble((Integer)left), (Double)right);
         }
      });

      INTEGER.setDivisionFunction(NULL, NULL_BINARY_FUNCTION);

      INTEGER.setPlusFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add((Integer)left, (Integer)right);
         }
      });

      INTEGER.setPlusFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add(toLong((Integer)left), (Long)right);
         }
      });

      INTEGER.setPlusFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add(toDecimal((Integer)left), (BigDecimal)right);
         }
      });

      INTEGER.setPlusFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add(toFloat((Integer)left), (Float)right);
         }
      });

      INTEGER.setPlusFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add(toDouble((Integer)left), (Double)right);
         }
      });

      INTEGER.setPlusFunction(NULL, NULL_BINARY_FUNCTION);

      INTEGER.setMinusFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract((Integer)left, (Integer)right);
         }
      });

      INTEGER.setMinusFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract(toLong((Integer)left), (Long)right);
         }
      });

      INTEGER.setMinusFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract(toDecimal((Integer)left), (BigDecimal)right);
         }
      });

      INTEGER.setMinusFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract(toFloat((Integer)left), (Float)right);
         }
      });

      INTEGER.setMinusFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract(toDouble((Integer)left), (Double)right);
         }
      });

      INTEGER.setMinusFunction(NULL, NULL_BINARY_FUNCTION);

      INTEGER.setGTFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((Integer)left, (Integer)right);
         }
      });

      INTEGER.setGTFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt(toLong((Integer)left), (Long)right);
         }
      });

      INTEGER.setGTFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt(toDecimal((Integer)left), (BigDecimal)right);
         }
      });

      INTEGER.setGTFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt(toFloat((Integer)left), (Float)right);
         }
      });

      INTEGER.setGTFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt(toDouble((Integer)left), (Double)right);
         }
      });

      INTEGER.setGEFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((Integer)left, (Integer)right);
         }
      });

      INTEGER.setGEFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge(toLong((Integer)left), (Long)right);
         }
      });

      INTEGER.setGEFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge(toDecimal((Integer)left), (BigDecimal)right);
         }
      });

      INTEGER.setGEFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge(toFloat((Integer)left), (Float)right);
         }
      });

      INTEGER.setGEFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge(toDouble((Integer)left), (Double)right);
         }
      });

      INTEGER.setLTFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((Integer)left, (Integer)right);
         }
      });

      INTEGER.setLTFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt(toLong((Integer)left), (Long)right);
         }
      });

      INTEGER.setLTFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt(toDecimal((Integer)left), (BigDecimal)right);
         }
      });

      INTEGER.setLTFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt(toFloat((Integer)left), (Float)right);
         }
      });

      INTEGER.setLTFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt(toDouble((Integer)left), (Double)right);
         }
      });

      INTEGER.setLEFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((Integer)left, (Integer)right);
         }
      });

      INTEGER.setLEFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le(toLong((Integer)left), (Long)right);
         }
      });

      INTEGER.setLEFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le(toDecimal((Integer)left), (BigDecimal)right);
         }
      });

      INTEGER.setLEFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le(toFloat((Integer)left), (Float)right);
         }
      });

      INTEGER.setLEFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le(toDouble((Integer)left), (Double)right);
         }
      });

      INTEGER.setEQFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((Integer)left, (Integer)right);
         }
      });

      INTEGER.setEQFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq(toLong((Integer)left), (Long)right);
         }
      });

      INTEGER.setEQFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq(toDecimal((Integer)left), (BigDecimal)right);
         }
      });

      INTEGER.setEQFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq(toFloat((Integer)left), (Float)right);
         }
      });

      INTEGER.setEQFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq(toDouble((Integer)left), (Double)right);
         }
      });

      INTEGER.setNEFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((Integer)left, (Integer)right);
         }
      });

      INTEGER.setNEFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne(toLong((Integer)left), (Long)right);
         }
      });

      INTEGER.setNEFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne(toDecimal((Integer)left), (BigDecimal)right);
         }
      });

      INTEGER.setNEFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne(toFloat((Integer)left), (Float)right);
         }
      });

      INTEGER.setNEFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne(toDouble((Integer)left), (Double)right);
         }
      });

      LONG.setConverter(STRING, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toLong((String)arg);
         }
      });

      LONG.setConverter(INTEGER, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toLong((Integer)arg);
         }
      });

      LONG.setConverter(LONG, IDENTITY_FUNCTION);

      LONG.setConverter(DECIMAL, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toLong((BigDecimal)arg);
         }
      });

      LONG.setConverter(FLOAT, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toLong((Float)arg);
         }
      });

      LONG.setConverter(DOUBLE, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toLong((Double)arg);
         }
      });

      LONG.setConverter(TIMESTAMP, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toLong((Timestamp)arg);
         }
      });

      LONG.setConverter(BOOLEAN, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toLong((Boolean)arg);
         }
      });

      LONG.setConverter(NULL, NULL_UNARY_FUNCTION);

      LONG.setNegationFunction(new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return negate((Long)arg);
         }
      });

      LONG.setMultiplicationFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply((Long)left, toLong((Integer)right));
         }
      });

      LONG.setMultiplicationFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply((Long)left, (Long)right);
         }
      });

      LONG.setMultiplicationFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply(toDecimal((Long)left), (BigDecimal)right);
         }
      });

      LONG.setMultiplicationFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply(toFloat((Long)left), (Float)right);
         }
      });

      LONG.setMultiplicationFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply(toDouble((Long)left), (Double)right);
         }
      });

      LONG.setMultiplicationFunction(NULL, NULL_BINARY_FUNCTION);

      LONG.setDivisionFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((Long)left, toLong((Integer)right));
         }
      });

      LONG.setDivisionFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((Long)left, (Long)right);
         }
      });

      LONG.setDivisionFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide(toDecimal((Long)left), (BigDecimal)right);
         }
      });

      LONG.setDivisionFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide(toFloat((Long)left), (Float)right);
         }
      });

      LONG.setDivisionFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide(toDouble((Long)left), (Double)right);
         }
      });

      LONG.setDivisionFunction(NULL, NULL_BINARY_FUNCTION);

      LONG.setPlusFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add((Long)left, toLong((Integer)right));
         }
      });

      LONG.setPlusFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add((Long)left, (Long)right);
         }
      });

      LONG.setPlusFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add(toDecimal((Long)left), (BigDecimal)right);
         }
      });

      LONG.setPlusFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add(toFloat((Long)left), (Float)right);
         }
      });

      LONG.setPlusFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add(toDouble((Long)left), (Double)right);
         }
      });

      LONG.setPlusFunction(NULL, NULL_BINARY_FUNCTION);

      LONG.setMinusFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract((Long)left, toLong((Integer)right));
         }
      });

      LONG.setMinusFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract((Long)left, (Long)right);
         }
      });

      LONG.setMinusFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract(toDecimal((Long)left), (BigDecimal)right);
         }
      });

      LONG.setMinusFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract(toFloat((Long)left), (Float)right);
         }
      });

      LONG.setMinusFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract(toDouble((Long)left), (Double)right);
         }
      });

      LONG.setMinusFunction(NULL, NULL_BINARY_FUNCTION);

      LONG.setGTFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((Long)left, toLong((Integer)right));
         }
      });

      LONG.setGTFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((Long)left, (Long)right);
         }
      });

      LONG.setGTFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt(toDecimal((Long)left), (BigDecimal)right);
         }
      });

      LONG.setGTFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt(toFloat((Long)left), (Float)right);
         }
      });

      LONG.setGTFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt(toDouble((Long)left), (Double)right);
         }
      });

      LONG.setGEFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((Long)left, toLong((Integer)right));
         }
      });

      LONG.setGEFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((Long)left, (Long)right);
         }
      });

      LONG.setGEFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge(toDecimal((Long)left), (BigDecimal)right);
         }
      });

      LONG.setGEFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge(toFloat((Long)left), (Float)right);
         }
      });

      LONG.setGEFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge(toDouble((Long)left), (Double)right);
         }
      });

      LONG.setLTFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((Long)left, toLong((Integer)right));
         }
      });

      LONG.setLTFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((Long)left, (Long)right);
         }
      });

      LONG.setLTFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt(toDecimal((Long)left), (BigDecimal)right);
         }
      });

      LONG.setLTFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt(toFloat((Long)left), (Float)right);
         }
      });

      LONG.setLTFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt(toDouble((Long)left), (Double)right);
         }
      });

      LONG.setLEFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((Long)left, toLong((Integer)right));
         }
      });

      LONG.setLEFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((Long)left, (Long)right);
         }
      });

      LONG.setLEFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le(toDecimal((Long)left), (BigDecimal)right);
         }
      });

      LONG.setLEFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le(toFloat((Long)left), (Float)right);
         }
      });

      LONG.setLEFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le(toDouble((Long)left), (Double)right);
         }
      });

      LONG.setEQFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((Long)left, toLong((Integer)right));
         }
      });

      LONG.setEQFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((Long)left, (Long)right);
         }
      });

      LONG.setEQFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq(toDecimal((Long)left), (BigDecimal)right);
         }
      });

      LONG.setEQFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq(toFloat((Long)left), (Float)right);
         }
      });

      LONG.setEQFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq(toDouble((Long)left), (Double)right);
         }
      });

      LONG.setNEFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((Long)left, toLong((Integer)right));
         }
      });

      LONG.setNEFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((Long)left, (Long)right);
         }
      });

      LONG.setNEFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne(toDecimal((Long)left), (BigDecimal)right);
         }
      });

      LONG.setNEFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne(toFloat((Long)left), (Float)right);
         }
      });

      LONG.setNEFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne(toDouble((Long)left), (Double)right);
         }
      });

      FLOAT.setConverter(STRING, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toFloat((String)arg);
         }
      });

      FLOAT.setConverter(INTEGER, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toFloat((Integer)arg);
         }
      });

      FLOAT.setConverter(LONG, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toFloat((Long)arg);
         }
      });

      FLOAT.setConverter(DECIMAL, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toFloat((BigDecimal)arg);
         }
      });

      FLOAT.setConverter(FLOAT, IDENTITY_FUNCTION);

      FLOAT.setConverter(DOUBLE, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toFloat((Double)arg);
         }
      });

      FLOAT.setConverter(TIMESTAMP, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toFloat((Timestamp)arg);
         }
      });

      FLOAT.setConverter(BOOLEAN, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toFloat((Boolean)arg);
         }
      });

      FLOAT.setConverter(NULL, NULL_UNARY_FUNCTION);

      FLOAT.setNegationFunction(new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return negate((Float)arg);
         }
      });

      FLOAT.setMultiplicationFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply((Float)left, toFloat((Integer)right));
         }
      });

      FLOAT.setMultiplicationFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply((Float)left, toFloat((Long)right));
         }
      });

      FLOAT.setMultiplicationFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply((Float)left, toFloat((BigDecimal)right));
         }
      });

      FLOAT.setMultiplicationFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply((Float)left, (Float)right);
         }
      });

      FLOAT.setMultiplicationFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply(toDouble((Float)left), (Double)right);
         }
      });

      FLOAT.setMultiplicationFunction(NULL, NULL_BINARY_FUNCTION);

      FLOAT.setDivisionFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((Float)left, toFloat((Integer)right));
         }
      });

      FLOAT.setDivisionFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((Float)left, toFloat((Long)right));
         }
      });

      FLOAT.setDivisionFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((Float)left, toFloat((BigDecimal)right));
         }
      });

      FLOAT.setDivisionFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((Float)left, (Float)right);
         }
      });

      FLOAT.setDivisionFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide(toDouble((Float)left), (Double)right);
         }
      });

      FLOAT.setDivisionFunction(NULL, NULL_BINARY_FUNCTION);

      FLOAT.setPlusFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add((Float)left, toFloat((Integer)right));
         }
      });

      FLOAT.setPlusFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add((Float)left, toFloat((Long)right));
         }
      });

      FLOAT.setPlusFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add((Float)left, toFloat((BigDecimal)right));
         }
      });

      FLOAT.setPlusFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add((Float)left, (Float)right);
         }
      });

      FLOAT.setPlusFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add(toDouble((Float)left), (Double)right);
         }
      });

      FLOAT.setPlusFunction(NULL, NULL_BINARY_FUNCTION);

      FLOAT.setMinusFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract((Float)left, toFloat((Integer)right));
         }
      });

      FLOAT.setMinusFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract((Float)left, toFloat((Long)right));
         }
      });

      FLOAT.setMinusFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract((Float)left, toFloat((BigDecimal)right));
         }
      });

      FLOAT.setMinusFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract((Float)left, (Float)right);
         }
      });

      FLOAT.setMinusFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract(toDouble((Float)left), (Double)right);
         }
      });

      FLOAT.setMinusFunction(NULL, NULL_BINARY_FUNCTION);

      FLOAT.setGTFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((Float)left, toFloat((Integer)right));
         }
      });

      FLOAT.setGTFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((Float)left, toFloat((Long)right));
         }
      });

      FLOAT.setGTFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((Float)left, toFloat((BigDecimal)right));
         }
      });

      FLOAT.setGTFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((Float)left, (Float)right);
         }
      });

      FLOAT.setGTFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt(toDouble((Float)left), (Double)right);
         }
      });

      FLOAT.setGEFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((Float)left, toFloat((Integer)right));
         }
      });

      FLOAT.setGEFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((Float)left, toFloat((Long)right));
         }
      });

      FLOAT.setGEFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((Float)left, toFloat((BigDecimal)right));
         }
      });

      FLOAT.setGEFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((Float)left, (Float)right);
         }
      });

      FLOAT.setGEFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge(toDouble((Float)left), (Double)right);
         }
      });

      FLOAT.setLTFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((Float)left, toFloat((Integer)right));
         }
      });

      FLOAT.setLTFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((Float)left, toFloat((Long)right));
         }
      });

      FLOAT.setLTFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((Float)left, toFloat((BigDecimal)right));
         }
      });

      FLOAT.setLTFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((Float)left, (Float)right);
         }
      });

      FLOAT.setLTFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt(toDouble((Float)left), (Double)right);
         }
      });

      FLOAT.setLEFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((Float)left, toFloat((Integer)right));
         }
      });

      FLOAT.setLEFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((Float)left, toFloat((Long)right));
         }
      });

      FLOAT.setLEFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((Float)left, toFloat((BigDecimal)right));
         }
      });

      FLOAT.setLEFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((Float)left, (Float)right);
         }
      });

      FLOAT.setLEFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le(toDouble((Float)left), (Double)right);
         }
      });

      FLOAT.setEQFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((Float)left, toFloat((Integer)right));
         }
      });

      FLOAT.setEQFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((Float)left, toFloat((Long)right));
         }
      });

      FLOAT.setEQFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((Float)left, toFloat((BigDecimal)right));
         }
      });

      FLOAT.setEQFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((Float)left, (Float)right);
         }
      });

      FLOAT.setEQFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq(toDouble((Float)left), (Double)right);
         }
      });

      FLOAT.setNEFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((Float)left, toFloat((Integer)right));
         }
      });

      FLOAT.setNEFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((Float)left, toFloat((Long)right));
         }
      });

      FLOAT.setNEFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((Float)left, toFloat((BigDecimal)right));
         }
      });

      FLOAT.setNEFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((Float)left, (Float)right);
         }
      });

      FLOAT.setNEFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne(toDouble((Float)left), (Double)right);
         }
      });

      DOUBLE.setConverter(STRING, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toDouble((String)arg);
         }
      });

      DOUBLE.setConverter(INTEGER, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toDouble((Integer)arg);
         }
      });

      DOUBLE.setConverter(LONG, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toDouble((Long)arg);
         }
      });

      DOUBLE.setConverter(DECIMAL, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toDouble((BigDecimal)arg);
         }
      });

      DOUBLE.setConverter(FLOAT, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toDouble((Float)arg);
         }
      });

      DOUBLE.setConverter(DOUBLE, IDENTITY_FUNCTION);

      DOUBLE.setConverter(TIMESTAMP, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toDouble((Timestamp)arg);
         }
      });

      DOUBLE.setConverter(BOOLEAN, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toDouble((Boolean)arg);
         }
      });

      DOUBLE.setConverter(NULL, NULL_UNARY_FUNCTION);

      DOUBLE.setNegationFunction(new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return negate((Double)arg);
         }
      });

      DOUBLE.setMultiplicationFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply((Double)left, toDouble((Integer)right));
         }
      });

      DOUBLE.setMultiplicationFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply((Double)left, toDouble((Long)right));
         }
      });

      DOUBLE.setMultiplicationFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply((Double)left, toDouble((BigDecimal)right));
         }
      });

      DOUBLE.setMultiplicationFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply((Double)left, toDouble((Float)right));
         }
      });

      DOUBLE.setMultiplicationFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply((Double)left, (Double)right);
         }
      });

      DOUBLE.setMultiplicationFunction(NULL, NULL_BINARY_FUNCTION);

      DOUBLE.setDivisionFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((Double)left, toDouble((Integer)right));
         }
      });

      DOUBLE.setDivisionFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((Double)left, toDouble((Long)right));
         }
      });

      DOUBLE.setDivisionFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((Double)left, toDouble((BigDecimal)right));
         }
      });

      DOUBLE.setDivisionFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((Double)left, toDouble((Float)right));
         }
      });

      DOUBLE.setDivisionFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((Double)left, (Double)right);
         }
      });

      DOUBLE.setDivisionFunction(NULL, NULL_BINARY_FUNCTION);

      DOUBLE.setPlusFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add((Double)left, toDouble((Integer)right));
         }
      });

      DOUBLE.setPlusFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add((Double)left, toDouble((Long)right));
         }
      });

      DOUBLE.setPlusFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add((Double)left, toDouble((BigDecimal)right));
         }
      });

      DOUBLE.setPlusFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add((Double)left, toDouble((Float)right));
         }
      });

      DOUBLE.setPlusFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add((Double)left, (Double)right);
         }
      });

      DOUBLE.setPlusFunction(NULL, NULL_BINARY_FUNCTION);

      DOUBLE.setMinusFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract((Double)left, toDouble((Integer)right));
         }
      });

      DOUBLE.setMinusFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract((Double)left, toDouble((Long)right));
         }
      });

      DOUBLE.setMinusFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract((Double)left, toDouble((BigDecimal)right));
         }
      });

      DOUBLE.setMinusFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract((Double)left, toDouble((Float)right));
         }
      });

      DOUBLE.setMinusFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract((Double)left, (Double)right);
         }
      });

      DOUBLE.setMinusFunction(NULL, NULL_BINARY_FUNCTION);

      DOUBLE.setGTFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((Double)left, toDouble((Integer)right));
         }
      });

      DOUBLE.setGTFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((Double)left, toDouble((Long)right));
         }
      });

      DOUBLE.setGTFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((Double)left, toDouble((BigDecimal)right));
         }
      });

      DOUBLE.setGTFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((Double)left, toDouble((Float)right));
         }
      });

      DOUBLE.setGTFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((Double)left, (Double)right);
         }
      });

      DOUBLE.setGEFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((Double)left, toDouble((Integer)right));
         }
      });

      DOUBLE.setGEFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((Double)left, toDouble((Long)right));
         }
      });

      DOUBLE.setGEFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((Double)left, toDouble((BigDecimal)right));
         }
      });

      DOUBLE.setGEFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((Double)left, toDouble((Float)right));
         }
      });

      DOUBLE.setGEFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((Double)left, (Double)right);
         }
      });

      DOUBLE.setLTFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((Double)left, toDouble((Integer)right));
         }
      });

      DOUBLE.setLTFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((Double)left, toDouble((Long)right));
         }
      });

      DOUBLE.setLTFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((Double)left, toDouble((BigDecimal)right));
         }
      });

      DOUBLE.setLTFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((Double)left, toDouble((Float)right));
         }
      });

      DOUBLE.setLTFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((Double)left, (Double)right);
         }
      });

      DOUBLE.setLEFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((Double)left, toDouble((Integer)right));
         }
      });

      DOUBLE.setLEFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((Double)left, toDouble((Long)right));
         }
      });

      DOUBLE.setLEFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((Double)left, toDouble((BigDecimal)right));
         }
      });

      DOUBLE.setLEFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((Double)left, toDouble((Float)right));
         }
      });

      DOUBLE.setLEFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((Double)left, (Double)right);
         }
      });

      DOUBLE.setEQFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((Double)left, toDouble((Integer)right));
         }
      });

      DOUBLE.setEQFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((Double)left, toDouble((Long)right));
         }
      });

      DOUBLE.setEQFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((Double)left, toDouble((BigDecimal)right));
         }
      });

      DOUBLE.setEQFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((Double)left, toDouble((Float)right));
         }
      });

      DOUBLE.setEQFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((Double)left, (Double)right);
         }
      });

      DOUBLE.setNEFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((Double)left, toDouble((Integer)right));
         }
      });

      DOUBLE.setNEFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((Double)left, toDouble((Long)right));
         }
      });

      DOUBLE.setNEFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((Double)left, toDouble((BigDecimal)right));
         }
      });

      DOUBLE.setNEFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((Double)left, toDouble((Float)right));
         }
      });

      DOUBLE.setNEFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((Double)left, (Double)right);
         }
      });

      DECIMAL.setConverter(STRING, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toDecimal((String)arg);
         }
      });

      DECIMAL.setConverter(INTEGER, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toDecimal((Integer)arg);
         }
      });

      DECIMAL.setConverter(LONG, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toDecimal((Long)arg);
         }
      });

      DECIMAL.setConverter(DECIMAL, IDENTITY_FUNCTION);

      DECIMAL.setConverter(FLOAT, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toDecimal((Float)arg);
         }
      });

      DECIMAL.setConverter(DOUBLE, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toDecimal((Double)arg);
         }
      });

      DECIMAL.setConverter(TIMESTAMP, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toDecimal((Timestamp)arg);
         }
      });

      DECIMAL.setConverter(BOOLEAN, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toDecimal((Boolean)arg);
         }
      });

      DECIMAL.setConverter(NULL, NULL_UNARY_FUNCTION);

      DECIMAL.setNegationFunction(new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return negate((BigDecimal)arg);
         }
      });

      DECIMAL.setMultiplicationFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply((BigDecimal)left, toDecimal((Integer)right));
         }
      });

      DECIMAL.setMultiplicationFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply((BigDecimal)left, toDecimal((Long)right));
         }
      });

      DECIMAL.setMultiplicationFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply((BigDecimal)left, (BigDecimal)right);
         }
      });

      DECIMAL.setMultiplicationFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply(toFloat((BigDecimal)left), (Float)right);
         }
      });

      DECIMAL.setMultiplicationFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return multiply(toDouble((BigDecimal)left), (Double)right);
         }
      });

      DECIMAL.setMultiplicationFunction(NULL, NULL_BINARY_FUNCTION);

      DECIMAL.setDivisionFunction(STRING, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((BigDecimal)left, toDecimal((String)right));
         }
      });

      DECIMAL.setDivisionFunction(BINARY, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((BigDecimal)left, toDecimal((Binary)right));
         }
      });

      DECIMAL.setDivisionFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((BigDecimal)left, toDecimal((Integer)right));
         }
      });

      DECIMAL.setDivisionFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((BigDecimal)left, toDecimal((Long)right));
         }
      });

      DECIMAL.setDivisionFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide((BigDecimal)left, (BigDecimal)right);
         }
      });

      DECIMAL.setDivisionFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide(toFloat((BigDecimal)left), (Float)right);
         }
      });

      DECIMAL.setDivisionFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return divide(toDouble((BigDecimal)left), (Double)right);
         }
      });

      DECIMAL.setDivisionFunction(NULL, NULL_BINARY_FUNCTION);

      DECIMAL.setPlusFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add((BigDecimal)left, toDecimal((Integer)right));
         }
      });

      DECIMAL.setPlusFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add((BigDecimal)left, toDecimal((Long)right));
         }
      });

      DECIMAL.setPlusFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add((BigDecimal)left, (BigDecimal)right);
         }
      });

      DECIMAL.setPlusFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add(toFloat((BigDecimal)left), (Float)right);
         }
      });

      DECIMAL.setPlusFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return add(toDouble((BigDecimal)left), (Double)right);
         }
      });

      DECIMAL.setPlusFunction(NULL, NULL_BINARY_FUNCTION);

      DECIMAL.setMinusFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract((BigDecimal)left, toDecimal((Integer)right));
         }
      });

      DECIMAL.setMinusFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract((BigDecimal)left, toDecimal((Long)right));
         }
      });

      DECIMAL.setMinusFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract((BigDecimal)left, (BigDecimal)right);
         }
      });

      DECIMAL.setMinusFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract(toFloat((BigDecimal)left), (Float)right);
         }
      });

      DECIMAL.setMinusFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return subtract(toDouble((BigDecimal)left), (Double)right);
         }
      });

      DECIMAL.setMinusFunction(NULL, NULL_BINARY_FUNCTION);

      DECIMAL.setGTFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((BigDecimal)left, toDecimal((Integer)right));
         }
      });

      DECIMAL.setGTFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((BigDecimal)left, toDecimal((Long)right));
         }
      });

      DECIMAL.setGTFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((BigDecimal)left, (BigDecimal)right);
         }
      });

      DECIMAL.setGTFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt(toFloat((BigDecimal)left), (Float)right);
         }
      });

      DECIMAL.setGTFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt(toDouble((BigDecimal)left), (Double)right);
         }
      });

      DECIMAL.setGEFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((BigDecimal)left, toDecimal((Integer)right));
         }
      });

      DECIMAL.setGEFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((BigDecimal)left, toDecimal((Long)right));
         }
      });

      DECIMAL.setGEFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((BigDecimal)left, (BigDecimal)right);
         }
      });

      DECIMAL.setGEFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge(toFloat((BigDecimal)left), (Float)right);
         }
      });

      DECIMAL.setGEFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge(toDouble((BigDecimal)left), (Double)right);
         }
      });

      DECIMAL.setLTFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((BigDecimal)left, toDecimal((Integer)right));
         }
      });

      DECIMAL.setLTFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((BigDecimal)left, toDecimal((Long)right));
         }
      });

      DECIMAL.setLTFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((BigDecimal)left, (BigDecimal)right);
         }
      });

      DECIMAL.setLTFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt(toFloat((BigDecimal)left), (Float)right);
         }
      });

      DECIMAL.setLTFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt(toDouble((BigDecimal)left), (Double)right);
         }
      });

      DECIMAL.setLEFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((BigDecimal)left, toDecimal((Integer)right));
         }
      });

      DECIMAL.setLEFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((BigDecimal)left, toDecimal((Long)right));
         }
      });

      DECIMAL.setLEFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((BigDecimal)left, (BigDecimal)right);
         }
      });

      DECIMAL.setLEFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le(toFloat((BigDecimal)left), (Float)right);
         }
      });

      DECIMAL.setLEFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le(toDouble((BigDecimal)left), (Double)right);
         }
      });

      DECIMAL.setEQFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((BigDecimal)left, toDecimal((Integer)right));
         }
      });

      DECIMAL.setEQFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((BigDecimal)left, toDecimal((Long)right));
         }
      });

      DECIMAL.setEQFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((BigDecimal)left, (BigDecimal)right);
         }
      });

      DECIMAL.setEQFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq(toFloat((BigDecimal)left), (Float)right);
         }
      });

      DECIMAL.setEQFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq(toDouble((BigDecimal)left), (Double)right);
         }
      });

      DECIMAL.setNEFunction(INTEGER, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((BigDecimal)left, toDecimal((Integer)right));
         }
      });

      DECIMAL.setNEFunction(LONG, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((BigDecimal)left, toDecimal((Long)right));
         }
      });

      DECIMAL.setNEFunction(DECIMAL, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((BigDecimal)left, (BigDecimal)right);
         }
      });

      DECIMAL.setNEFunction(FLOAT, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne(toFloat((BigDecimal)left), (Float)right);
         }
      });

      DECIMAL.setNEFunction(DOUBLE, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne(toDouble((BigDecimal)left), (Double)right);
         }
      });

      TIMESTAMP.setConverter(STRING, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toTimestamp((String)arg);
         }
      });

      TIMESTAMP.setConverter(INTEGER, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toTimestamp((Integer)arg);
         }
      });

      TIMESTAMP.setConverter(LONG, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toTimestamp((Long)arg);
         }
      });

      TIMESTAMP.setConverter(DECIMAL, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toTimestamp((BigDecimal)arg);
         }
      });

      TIMESTAMP.setConverter(FLOAT, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toTimestamp((Float)arg);
         }
      });

      TIMESTAMP.setConverter(DOUBLE, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toTimestamp((Double)arg);
         }
      });

      TIMESTAMP.setConverter(TIMESTAMP, IDENTITY_FUNCTION);

      TIMESTAMP.setConverter(NULL, NULL_UNARY_FUNCTION);

      TIMESTAMP.setGTFunction(TIMESTAMP, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((Timestamp)left, (Timestamp)right);
         }
      });

      TIMESTAMP.setGEFunction(TIMESTAMP, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((Timestamp)left, (Timestamp)right);
         }
      });

      TIMESTAMP.setLTFunction(TIMESTAMP, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((Timestamp)left, (Timestamp)right);
         }
      });

      TIMESTAMP.setLEFunction(TIMESTAMP, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((Timestamp)left, (Timestamp)right);
         }
      });

      TIMESTAMP.setEQFunction(TIMESTAMP, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((Timestamp)left, (Timestamp)right);
         }
      });

      TIMESTAMP.setNEFunction(TIMESTAMP, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((Timestamp)left, (Timestamp)right);
         }
      });

      BOOLEAN.setConverter(STRING, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toBoolean((String)arg);
         }
      });

      BOOLEAN.setConverter(INTEGER, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toBoolean((Integer)arg);
         }
      });

      BOOLEAN.setConverter(LONG, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toBoolean((Long)arg);
         }
      });

      BOOLEAN.setConverter(DECIMAL, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toBoolean((BigDecimal)arg);
         }
      });

      BOOLEAN.setConverter(FLOAT, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toBoolean((Float)arg);
         }
      });

      BOOLEAN.setConverter(DOUBLE, new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return toBoolean((Double)arg);
         }
      });

      BOOLEAN.setConverter(BOOLEAN, IDENTITY_FUNCTION);

      BOOLEAN.setConverter(NULL, NULL_UNARY_FUNCTION);

      BOOLEAN.setNotFunction(new UnaryFunction()
      {
         public Object invoke(Object arg)
         {
            return not((Boolean)arg);
         }
      });

      BOOLEAN.setGTFunction(BOOLEAN, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return gt((Boolean)left, (Boolean)right);
         }
      });

      BOOLEAN.setGEFunction(BOOLEAN, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ge((Boolean)left, (Boolean)right);
         }
      });

      BOOLEAN.setLTFunction(BOOLEAN, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return lt((Boolean)left, (Boolean)right);
         }
      });

      BOOLEAN.setLEFunction(BOOLEAN, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return le((Boolean)left, (Boolean)right);
         }
      });

      BOOLEAN.setEQFunction(BOOLEAN, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return eq((Boolean)left, (Boolean)right);
         }
      });

      BOOLEAN.setNEFunction(BOOLEAN, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return ne((Boolean)left, (Boolean)right);
         }
      });

      BOOLEAN.setAndFunction(BOOLEAN, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return and((Boolean)left, (Boolean)right);
         }
      });

      BOOLEAN.setAndFunction(NULL, NULL_BINARY_FUNCTION);

      BOOLEAN.setOrFunction(BOOLEAN, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return or((Boolean)left, (Boolean)right);
         }
      });

      BOOLEAN.setOrFunction(NULL, NULL_BINARY_FUNCTION);

      ANY.setEQFunction(ANY, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return Boolean.valueOf(ObjUtil.equal(left, right));
         }
      });

      ANY.setNEFunction(ANY, new BinaryFunction()
      {
         public Object invoke(Object left, Object right)
         {
            return Boolean.valueOf(!ObjUtil.equal(left, right));
         }
      });

      for (int i = 0; i <= MAX_COUNT; ++i)
      {
         NULL.setConverter(get(i), NULL_UNARY_FUNCTION);
         ANY.setConverter(get(i), IDENTITY_FUNCTION);
      }

      NULL.setNegationFunction(NULL_UNARY_FUNCTION);
      NULL.setNotFunction(NULL_UNARY_FUNCTION);

      NULL.setMultiplicationFunction(INTEGER, NULL_BINARY_FUNCTION);
      NULL.setMultiplicationFunction(LONG, NULL_BINARY_FUNCTION);
      NULL.setMultiplicationFunction(DECIMAL, NULL_BINARY_FUNCTION);
      NULL.setMultiplicationFunction(FLOAT, NULL_BINARY_FUNCTION);
      NULL.setMultiplicationFunction(DOUBLE, NULL_BINARY_FUNCTION);
      NULL.setMultiplicationFunction(NULL, NULL_BINARY_FUNCTION);

      NULL.setDivisionFunction(INTEGER, NULL_BINARY_FUNCTION);
      NULL.setDivisionFunction(LONG, NULL_BINARY_FUNCTION);
      NULL.setDivisionFunction(DECIMAL, NULL_BINARY_FUNCTION);
      NULL.setDivisionFunction(FLOAT, NULL_BINARY_FUNCTION);
      NULL.setDivisionFunction(DOUBLE, NULL_BINARY_FUNCTION);
      NULL.setDivisionFunction(NULL, NULL_BINARY_FUNCTION);

      NULL.setPlusFunction(INTEGER, NULL_BINARY_FUNCTION);
      NULL.setPlusFunction(LONG, NULL_BINARY_FUNCTION);
      NULL.setPlusFunction(DECIMAL, NULL_BINARY_FUNCTION);
      NULL.setPlusFunction(FLOAT, NULL_BINARY_FUNCTION);
      NULL.setPlusFunction(DOUBLE, NULL_BINARY_FUNCTION);
      NULL.setPlusFunction(NULL, NULL_BINARY_FUNCTION);

      NULL.setMinusFunction(INTEGER, NULL_BINARY_FUNCTION);
      NULL.setMinusFunction(LONG, NULL_BINARY_FUNCTION);
      NULL.setMinusFunction(DECIMAL, NULL_BINARY_FUNCTION);
      NULL.setMinusFunction(FLOAT, NULL_BINARY_FUNCTION);
      NULL.setMinusFunction(DOUBLE, NULL_BINARY_FUNCTION);
      NULL.setMinusFunction(NULL, NULL_BINARY_FUNCTION);

      NULL.setLikeFunction(STRING, ISNULL2_BINARY_FUNCTION);
      NULL.setLikeFunction(NULL, TRUE_BINARY_FUNCTION);

      NULL.setGTFunction(NULL, FALSE_BINARY_FUNCTION);
      NULL.setGEFunction(NULL, TRUE_BINARY_FUNCTION);
      NULL.setLTFunction(NULL, FALSE_BINARY_FUNCTION);
      NULL.setLEFunction(NULL, TRUE_BINARY_FUNCTION);
      NULL.setEQFunction(NULL, TRUE_BINARY_FUNCTION);
      NULL.setNEFunction(NULL, FALSE_BINARY_FUNCTION);

      NULL.setAndFunction(BOOLEAN, NULL_BINARY_FUNCTION);
      NULL.setAndFunction(NULL, NULL_BINARY_FUNCTION);

      NULL.setOrFunction(BOOLEAN, NULL_BINARY_FUNCTION);
      NULL.setOrFunction(NULL, NULL_BINARY_FUNCTION);

      // Initialize the primitive type maps

      for (int i = 0; i < s_primitiveArray.length; ++i)
      {
         Primitive type = s_primitiveArray[i];

         if (type != NULL)
         {
            s_nameMap.put(type.getName(), type);

            if (type != ANY)
            {
               type.setGTFunction(NULL, ISNOTNULL1_BINARY_FUNCTION);
               NULL.setLTFunction(type, ISNOTNULL2_BINARY_FUNCTION);
               type.setGEFunction(NULL, TRUE_BINARY_FUNCTION);
               NULL.setLEFunction(type, TRUE_BINARY_FUNCTION);
               type.setLTFunction(NULL, FALSE_BINARY_FUNCTION);
               NULL.setGTFunction(type, FALSE_BINARY_FUNCTION);
               type.setLEFunction(NULL, ISNULL1_BINARY_FUNCTION);
               NULL.setGEFunction(type, ISNULL2_BINARY_FUNCTION);

               s_classMap.put(type.getClassObject(), type);
            }

            type.setEQFunction(NULL, ISNULL1_BINARY_FUNCTION);
            NULL.setEQFunction(type, ISNULL2_BINARY_FUNCTION);
            type.setNEFunction(NULL, ISNOTNULL1_BINARY_FUNCTION);
            NULL.setNEFunction(type, ISNOTNULL2_BINARY_FUNCTION);
         }

         type.makeReadOnly();
      }

      s_classMap.put(Integer.TYPE, INTEGER);
      s_classMap.put(Long.TYPE, LONG);
      s_classMap.put(Float.TYPE, FLOAT);
      s_classMap.put(Double.TYPE, DOUBLE);
      s_classMap.put(Boolean.TYPE, BOOLEAN);
   }

   /**
    * Array of preallocated integers to optimize integer object creation.
    */
   private final static Integer[] s_integerArray = new Integer[1024];

   /**
    * Array of preallocated characters to optimize object creation.
    */
   private final static Character[] s_characterArray = new Character[1024];

   static
   {
      for (int i = 0; i < s_integerArray.length; ++i)
      {
         s_integerArray[i] = new Integer(i - 1);
      }

      for (int i = 0; i < s_characterArray.length; ++i)
      {
         s_characterArray[i] = new Character((char)i);
      }
   }

   /**
    * The timestamp parse format.
    */
   private final static SimpleDateFormat s_timestampInFormat= new SimpleDateFormat("Gyyyy'-'MM'-'dd HH':'mm':'ss", Locale.ENGLISH);

   static
   {
      s_timestampInFormat.setTimeZone(TZ.UTC);

      DateFormatSymbols dfs = s_timestampInFormat.getDateFormatSymbols();

      dfs.setEras(new String[]{"B", "A"});
      s_timestampInFormat.setDateFormatSymbols(dfs);
   }

   // constructors

   /**
    * Creates a primitive type.
    * @param sName The type name.
    * @param classObject The Java class object corresponding to the type.
    * @param nOrdinal The type ordinal number.
    */
   private Primitive(String sName, Class classObject, int nOrdinal)
   {
      super(sName);
      m_classObject = classObject;
      m_nOrdinal = nOrdinal;
   }

   // operations

   /**
    * @return true
    * @see nexj.core.meta.Type#isPrimitive()
    */
   public boolean isPrimitive()
   {
      return true;
   }

   /**
    * @see nexj.core.meta.Type#getBaseType()
    */
   public Type getBaseType()
   {
      return null;
   }

   /**
    * @see nexj.core.meta.Type#isUpcast(nexj.core.meta.Type)
    */
   public boolean isUpcast(Type type)
   {
      return type == this;
   }

   /**
    * @return The class object.
    */
   public Class getClassObject()
   {
      return m_classObject;
   }

   /**
    * @return The type ordinal number.
    */
   public int getOrdinal()
   {
      return m_nOrdinal;
   }

   /**
    * Sets a type converter from a given type.
    * @param fromType The primitive type from which to convert.
    * @param converter The converter.
    */
   protected void setConverter(Primitive fromType, UnaryFunction converter)
   {
      verifyNotReadOnly();
      m_converterArray[fromType.m_nOrdinal] = converter;
   }

   /**
    * Gets a type converter from a given primitive type to this type.
    * @param fromType The primitive type from which to convert.
    * @return The converter.
    * @throws TypeConversionException if the converter is not found.
    */
   public UnaryFunction getConverter(Primitive fromType) throws TypeConversionException
   {
      if (fromType == null)
      {
         return NULL_UNARY_FUNCTION;
      }

      UnaryFunction cvt = m_converterArray[fromType.m_nOrdinal];

      if (cvt != null)
      {
         return cvt;
      }

      throw new TypeConversionException(this);
   }

   /**
    * Gets a type converter from a given primitive type to this type.
    * @param nFromOrdinal The ordinal number of the primitive type from which to convert.
    * @return The converter.
    * @throws TypeConversionException if the converter is not found.
    */
   private UnaryFunction getConverter(int nFromOrdinal) throws TypeConversionException
   {
      UnaryFunction cvt = m_converterArray[nFromOrdinal];

      if (cvt != null)
      {
         return cvt;
      }

      throw new TypeConversionException(this);
   }

   /**
    * Finds a type converter from a given primitive type to this type.
    * @param fromType The primitive type from which to convert.
    * @return The converter, or null if such a type conversion is not supported.
    */
   public UnaryFunction findConverter(Primitive fromType)
   {
      if (fromType == null)
      {
         return NULL_UNARY_FUNCTION;
      }

      return m_converterArray[fromType.m_nOrdinal];
   }

   /**
    * Converts a value to this primitive type.
    * @param value The value to convert.
    * @return The converted value.
    * @throws TypeConversionException if the conversion fails.
    */
   public Object convert(Object value) throws TypeConversionException
   {
      int nOrdinal = ordinalOf(value);

      return getConverter(nOrdinal).invoke(value);
   }

   /**
    * Gets a type converter from a given primitive type to this type.
    * The types must be comparable with the EQ operator.
    * @param fromType The primitive type from which to convert.
    * @return The converter.
    * @throws TypeConversionException if the converter is not found.
    */
   public UnaryFunction getStrictConverter(Primitive fromType) throws TypeConversionException
   {
      if (m_eqArray[getBinaryIndex(fromType)] == null)
      {
         throw new TypeConversionException(this);
      }

      return getConverter(fromType);
   }

   /**
    * Find a type converter from a given primitive type to this type.
    * The types must be comparable with the EQ operator.
    * @param fromType The primitive type from which to convert.
    * @return The converter, or null if not found.
    */
   public UnaryFunction findStrictConverter(Primitive fromType)
   {
      if (m_eqArray[getBinaryIndex(fromType)] == null)
      {
         return null;
      }

      return findConverter(fromType);
   }

   /**
    * Converts a value to this primitive type.
    * The types must be comparable with the EQ operator.
    * @param value The value to convert.
    * @return The converted value.
    * @throws TypeConversionException if the conversion fails.
    */
   public Object strictConvert(Object value) throws TypeConversionException
   {
      int nOrdinal = ordinalOf(value);

      if (m_eqArray[nOrdinal] == null)
      {
         throw new TypeConversionException(this);
      }

      return getConverter(nOrdinal).invoke(value);
   }

   /**
    * Sets the logical negation function.
    * @param notFunction The logical negation function to set.
    */
   public void setNotFunction(UnaryFunction notFunction)
   {
      verifyNotReadOnly();
      m_notFunction = notFunction;
   }

   /**
    * @return The logical negation function.
    * @throws TypeMismatchException if the function is not set.
    */
   public UnaryFunction getNotFunction() throws TypeMismatchException
   {
      if (m_notFunction != null)
      {
         return m_notFunction;
      }

      throw new TypeMismatchException(Symbol.NOT);
   }

   /**
    * @return The logical negation function, or null if not set.
    */
   public UnaryFunction findNotFunction()
   {
      return m_notFunction;
   }

   /**
    * Sets the arithmetic negation function.
    * @param negationFunction The arithmetic negation function to set.
    */
   public void setNegationFunction(UnaryFunction negationFunction)
   {
      verifyNotReadOnly();
      m_negationFunction = negationFunction;
   }

   /**
    * @return The arithmetic negation function.
    * @throws TypeMismatchException if the function is not set.
    */
   public UnaryFunction getNegationFunction() throws TypeMismatchException
   {
      if (m_negationFunction != null)
      {
         return m_negationFunction;
      }

      throw new TypeMismatchException(Symbol.MINUS);
   }

   /**
    * @return The arithmetic negation function, or null if not set.
    */
   public UnaryFunction findNegationFunction()
   {
      return m_negationFunction;
   }

   /**
    * Computes the array index for a binary operator.
    * @param type The type of the right operand. Can be null.
    */
   private static int getBinaryIndex(Primitive type)
   {
      if (type == null)
      {
         return NULL_ORDINAL;
      }

      return type.m_nOrdinal;
   }

   /**
    * Sets the multiplication function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @param f The binary function to set.
    */
    public void setMultiplicationFunction(Primitive type, BinaryFunction f)
    {
       m_multiplicationArray[getBinaryIndex(type)] = f;
   }

   /**
    * Gets the multiplication function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function.
    * @throws TypeMismatchException if the operand type is invalid.
    */
   public BinaryFunction getMultiplicationFunction(Primitive type) throws TypeMismatchException
   {
      BinaryFunction f = m_multiplicationArray[getBinaryIndex(type)];

      if (f != null)
      {
          return f;
      }

      throw new TypeMismatchException(Symbol.MUL);
   }

   /**
    * Finds the multiplication function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function, or null if not found.
    */
   public BinaryFunction findMultiplicationFunction(Primitive type)
   {
      return m_multiplicationArray[getBinaryIndex(type)];
   }

   /**
    * Sets the division function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @param f The binary function to set.
    */
    public void setDivisionFunction(Primitive type, BinaryFunction f)
    {
       m_divisionArray[getBinaryIndex(type)] = f;
   }

   /**
    * Gets the division function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function.
    * @throws TypeMismatchException if the operand type is invalid.
    */
   public BinaryFunction getDivisionFunction(Primitive type) throws TypeMismatchException
   {
      BinaryFunction f = m_divisionArray[getBinaryIndex(type)];

      if (f != null)
      {
          return f;
      }

      throw new TypeMismatchException(Symbol.DIVIDE);
   }

   /**
    * Finds the division function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function, or null if not found.
    */
   public BinaryFunction findDivisionFunction(Primitive type)
   {
      return m_divisionArray[getBinaryIndex(type)];
   }

   /**
    * Sets the plus function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @param f The binary function to set.
    */
    public void setPlusFunction(Primitive type, BinaryFunction f)
    {
       m_plusArray[getBinaryIndex(type)] = f;
   }

   /**
    * Gets the plus function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function.
    * @throws TypeMismatchException if the operand type is invalid.
    */
   public BinaryFunction getPlusFunction(Primitive type) throws TypeMismatchException
   {
      BinaryFunction f = m_plusArray[getBinaryIndex(type)];

      if (f != null)
      {
          return f;
      }

      throw new TypeMismatchException(Symbol.PLUS);
   }

   /**
    * Finds the plus function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function, or null if not found.
    */
   public BinaryFunction findPlusFunction(Primitive type)
   {
      return m_plusArray[getBinaryIndex(type)];
   }

   /**
    * Sets the minus function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @param f The binary function to set.
    */
    public void setMinusFunction(Primitive type, BinaryFunction f)
    {
       m_minusArray[getBinaryIndex(type)] = f;
   }

   /**
    * Gets the minus function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function.
    * @throws TypeMismatchException if the operand type is invalid.
    */
   public BinaryFunction getMinusFunction(Primitive type) throws TypeMismatchException
   {
      BinaryFunction f = m_minusArray[getBinaryIndex(type)];

      if (f != null)
      {
          return f;
      }

      throw new TypeMismatchException(Symbol.MINUS);
   }

   /**
    * Finds the minus function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function, or null if not found.
    */
   public BinaryFunction findMinusFunction(Primitive type)
   {
      return m_minusArray[getBinaryIndex(type)];
   }

   /**
    * Sets the like function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @param f The binary function to set.
    */
    public void setLikeFunction(Primitive type, BinaryFunction f)
    {
       m_likeArray[getBinaryIndex(type)] = f;
   }

   /**
    * Gets the like function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function.
    * @throws TypeMismatchException if the operand type is invalid.
    */
   public BinaryFunction getLikeFunction(Primitive type) throws TypeMismatchException
   {
      BinaryFunction f = m_likeArray[getBinaryIndex(type)];

      if (f != null)
      {
          return f;
      }

      throw new TypeMismatchException(Symbol.LIKE_P);
   }

   /**
    * Finds the like function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function, or null if not found.
    */
   public BinaryFunction findLikeFunction(Primitive type)
   {
      return m_likeArray[getBinaryIndex(type)];
   }

   /**
    * Sets the gt function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @param f The binary function to set.
    */
    public void setGTFunction(Primitive type, BinaryFunction f)
    {
       m_gtArray[getBinaryIndex(type)] = f;
   }

   /**
    * Gets the gt function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function.
    * @throws TypeMismatchException if the operand type is invalid.
    */
   public BinaryFunction getGTFunction(Primitive type) throws TypeMismatchException
   {
      BinaryFunction f = m_gtArray[getBinaryIndex(type)];

      if (f != null)
      {
          return f;
      }

      throw new TypeMismatchException(Symbol.GT);
   }

   /**
    * Finds the gt function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function, or null if not found.
    */
   public BinaryFunction findGTFunction(Primitive type)
   {
      return m_gtArray[getBinaryIndex(type)];
   }

   /**
    * Sets the ge function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @param f The binary function to set.
    */
    public void setGEFunction(Primitive type, BinaryFunction f)
    {
       m_geArray[getBinaryIndex(type)] = f;
   }

   /**
    * Gets the ge function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function.
    * @throws TypeMismatchException if the operand type is invalid.
    */
   public BinaryFunction getGEFunction(Primitive type) throws TypeMismatchException
   {
      BinaryFunction f = m_geArray[getBinaryIndex(type)];

      if (f != null)
      {
          return f;
      }

      throw new TypeMismatchException(Symbol.GE);
   }

   /**
    * Finds the ge function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function, or null if not found.
    */
   public BinaryFunction findGEFunction(Primitive type)
   {
      return m_geArray[getBinaryIndex(type)];
   }

   /**
    * Sets the lt function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @param f The binary function to set.
    */
    public void setLTFunction(Primitive type, BinaryFunction f)
    {
       m_ltArray[getBinaryIndex(type)] = f;
   }

   /**
    * Gets the lt function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function.
    * @throws TypeMismatchException if the operand type is invalid.
    */
   public BinaryFunction getLTFunction(Primitive type) throws TypeMismatchException
   {
      BinaryFunction f = m_ltArray[getBinaryIndex(type)];

      if (f != null)
      {
          return f;
      }

      throw new TypeMismatchException(Symbol.LT);
   }

   /**
    * Finds the lt function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function, or null if not found.
    */
   public BinaryFunction findLTFunction(Primitive type)
   {
      return m_ltArray[getBinaryIndex(type)];
   }

   /**
    * Sets the le function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @param f The binary function to set.
    */
    public void setLEFunction(Primitive type, BinaryFunction f)
    {
       m_leArray[getBinaryIndex(type)] = f;
   }

   /**
    * Gets the le function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function.
    * @throws TypeMismatchException if the operand type is invalid.
    */
   public BinaryFunction getLEFunction(Primitive type) throws TypeMismatchException
   {
      BinaryFunction f = m_leArray[getBinaryIndex(type)];

      if (f != null)
      {
          return f;
      }

      throw new TypeMismatchException(Symbol.LE);
   }

   /**
    * Finds the le function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function, or null if not found.
    */
   public BinaryFunction findLEFunction(Primitive type)
   {
      return m_leArray[getBinaryIndex(type)];
   }

   /**
    * Sets the eq function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @param f The binary function to set.
    */
    public void setEQFunction(Primitive type, BinaryFunction f)
    {
       m_eqArray[getBinaryIndex(type)] = f;
   }

   /**
    * Gets the eq function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function.
    * @throws TypeMismatchException if the operand type is invalid.
    */
   public BinaryFunction getEQFunction(Primitive type) throws TypeMismatchException
   {
      BinaryFunction f = m_eqArray[getBinaryIndex(type)];

      if (f != null)
      {
          return f;
      }

      throw new TypeMismatchException(Symbol.EQ);
   }

   /**
    * Finds the eq function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function, or null if not found.
    */
   public BinaryFunction findEQFunction(Primitive type)
   {
      return m_eqArray[getBinaryIndex(type)];
   }

   /**
    * Sets the ne function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @param f The binary function to set.
    */
    public void setNEFunction(Primitive type, BinaryFunction f)
    {
       m_neArray[getBinaryIndex(type)] = f;
   }

   /**
    * Gets the ne function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function.
    * @throws TypeMismatchException if the operand type is invalid.
    */
   public BinaryFunction getNEFunction(Primitive type) throws TypeMismatchException
   {
      BinaryFunction f = m_neArray[getBinaryIndex(type)];

      if (f != null)
      {
          return f;
      }

      throw new TypeMismatchException(Symbol.NE);
   }

   /**
    * Finds the ne function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function, or null if not found.
    */
   public BinaryFunction findNEFunction(Primitive type)
   {
      return m_neArray[getBinaryIndex(type)];
   }

   /**
    * Sets the and function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @param f The binary function to set.
    */
    public void setAndFunction(Primitive type, BinaryFunction f)
    {
       m_andArray[getBinaryIndex(type)] = f;
   }

   /**
    * Gets the and function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function.
    * @throws TypeMismatchException if the operand type is invalid.
    */
   public BinaryFunction getAndFunction(Primitive type) throws TypeMismatchException
   {
      BinaryFunction f = m_andArray[getBinaryIndex(type)];

      if (f != null)
      {
          return f;
      }

      throw new TypeMismatchException(Symbol.AND);
   }

   /**
    * Finds the and function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function, or null if not found.
    */
   public BinaryFunction findAndFunction(Primitive type)
   {
      return m_andArray[getBinaryIndex(type)];
   }

   /**
    * Sets the or function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @param f The binary function to set.
    */
    public void setOrFunction(Primitive type, BinaryFunction f)
    {
       m_orArray[getBinaryIndex(type)] = f;
   }

   /**
    * Gets the or function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function.
    * @throws TypeMismatchException if the operand type is invalid.
    */
   public BinaryFunction getOrFunction(Primitive type) throws TypeMismatchException
   {
      BinaryFunction f = m_orArray[getBinaryIndex(type)];

      if (f != null)
      {
          return f;
      }

      throw new TypeMismatchException(Symbol.OR);
   }

   /**
    * Finds the or function for a given right operand type.
    * @param type The right operand type. Can be null.
    * @return The binary function, or null if not found.
    */
   public BinaryFunction findOrFunction(Primitive type)
   {
      return m_orArray[getBinaryIndex(type)];
   }

   /**
    * Gets a primitive type by ordinal number.
    * @param nOrdinal The primitive type ordinal number.
    * @return The primitive type.
    */
   public static Primitive get(int nOrdinal)
   {
      return s_primitiveArray[nOrdinal];
   }

   /**
    * Finds a primitive type by name.
    * @param sName The primitive type name.
    * @return The primitive type, or null if not found.
    */
   public static Primitive find(String sName)
   {
      return (Primitive)s_nameMap.get(sName);
   }

   /**
    * Parses a primitive type from a string.
    * @param sName The primitive type name to parse.
    * @return The primitive type.
    * @throws MetadataException if the type has not been recognized.
    */
   public static Primitive parse(String sName)
   {
      Primitive type = find(sName);

      if (type == null)
      {
         throw new MetadataException("err.meta.primitiveName", new Object[]{sName});
      }

      return type;
   }

   /**
    * Determines the type of a given value.
    * @param value The value of which to determine the type.
    * @return The value type (null if value is null).
    */
   public static Type typeOf(Object value)
   {
      return primitiveOf(value);
   }

   /**
    * Determines the primitive type of a given class object.
    * @param clazz The class object.
    * @return The primitive type (null if clazz is null), ANY for unrecognized types.
    */
   public static Primitive primitiveOf(Class clazz)
   {
      if (clazz == null)
      {
         return null;
      }

      Primitive type = (Primitive)s_classMap.get(clazz);

      if (type == null)
      {
         type = ANY;
      }

      return type;
   }

   /**
    * Determines the primitive type of a given value.
    * @param value The value of which to determine the primitive type.
    * @return The primitive type (null if value is null), ANY for unrecognized types.
    */
   public static Primitive primitiveOf(Object value)
   {
      // Linear search with instanceof turned out
      // to be 20% faster than hash table lookup

      if (value instanceof String)
      {
         return STRING;
      }

      if (value instanceof Number)
      {
         if (value instanceof Integer)
         {
            return INTEGER;
         }

         if (value instanceof Long)
         {
            return LONG;
         }

         if (value instanceof BigDecimal)
         {
            return DECIMAL;
         }

         if (value instanceof Float)
         {
            return FLOAT;
         }

         if (value instanceof Double)
         {
            return DOUBLE;
         }

         return ANY;
      }

      if (value instanceof Boolean)
      {
         return BOOLEAN;
      }

      if (value instanceof Timestamp)
      {
         return TIMESTAMP;
      }

      if (value instanceof Binary)
      {
         return BINARY;
      }

      if (value == null)
      {
         return null;
      }

      return ANY;
   }

   /**
    * Determines the primitive type ordinal number of a given value.
    * @param value The value of which to determine the primitive type ordinal number.
    * @return The primitive type ordinal, ANY_ORDINAL for unrecognized types.
    */
   private static int ordinalOf(Object value)
   {
      // Linear search with instanceof turned out
      // to be 20% faster than hash table lookup

      if (value instanceof String)
      {
         return STRING_ORDINAL;
      }

      if (value instanceof Number)
      {
         if (value instanceof Integer)
         {
            return INTEGER_ORDINAL;
         }

         if (value instanceof Long)
         {
            return LONG_ORDINAL;
         }

         if (value instanceof BigDecimal)
         {
            return DECIMAL_ORDINAL;
         }

         if (value instanceof Float)
         {
            return FLOAT_ORDINAL;
         }

         if (value instanceof Double)
         {
            return DOUBLE_ORDINAL;
         }

         return ANY_ORDINAL;
      }

      if (value instanceof Boolean)
      {
         return BOOLEAN_ORDINAL;
      }

      if (value instanceof Timestamp)
      {
         return TIMESTAMP_ORDINAL;
      }

      if (value instanceof Binary)
      {
         return BINARY_ORDINAL;
      }

      if (value == null)
      {
         return NULL_ORDINAL;
      }

      return ANY_ORDINAL;
   }

   /**
    * Creates an integer object. Optimized for values -1..1023.
    * @param n The primitive integer value for which to allocate an object.
    * @return an Integer object.
    */
   public static Integer createInteger(int n)
   {
      if (n >= -1 && n < 1023)
      {
         return s_integerArray[n + 1];
      }

      return new Integer(n);
   }

   /**
    * Creates a character object. Optimized for values \u0000..\u03ff.
    * @param ch The character code to allocate.
    * @return a Character object.
    */
   public static Character createCharacter(int ch)
   {
      if (ch >= 0 && ch < 1024)
      {
         return s_characterArray[ch];
      }

      return new Character((char)ch);
   }

   /**
    * Creates a long number object.
    * @param l The long number to allocate.
    * @return a Long object.
    */
   public static Long createLong(long l)
   {
      return new Long(l);
   }

   /**
    * Creates a float number object.
    * @param f The float to allocate.
    * @return a Float object.
    */
   public static Float createFloat(float f)
   {
      return new Float(f);
   }

   /**
    * Creates a double number object.
    * @param d The double to allocate.
    * @return a Double object.
    */
   public static Double createDouble(double d)
   {
      return new Double(d);
   }

   /**
    * Converts a binary value to string.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static String toString(Binary value)
   {
      if (value == null)
      {
         return null;
      }

      StringBuffer buf = new StringBuffer((int)(value.getSize() << 1));

      Binary.append(buf, value.getData(), -1);

      return buf.toString();
   }

   /**
    * Converts a numeric value to string.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static String toString(Number value)
   {
      if (value == null)
      {
         return null;
      }

      return value.toString();
   }

   /**
    * Converts a timestamp value to string.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static String toString(Timestamp value)
   {
      return StringUtil.toString(value);
   }

   /**
    * Converts a boolean value to string.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static String toString(Boolean value)
   {
      if (value == null)
      {
         return null;
      }

      return (value.booleanValue()) ? "1" : "0";
   }

   /**
    * Converts a primitive value to string.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException if the conversion fails.
    */
   public static String toString(Object value) throws TypeConversionException
   {
      return (String)STRING.convert(value);
   }

   /**
    * Converts a string value to binary.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException If the conversion fails.
    */
   public static Binary toBinary(String value) throws TypeConversionException
   {
      if (value == null)
      {
         return null;
      }

      try
      {
         return Binary.parse(value);
      }
      catch (NumberFormatException e)
      {
         throw new TypeConversionException(BINARY, e);
      }
   }

   /**
    * Converts a primitive value to binary.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException if the conversion fails.
    */
   public static Binary toBinary(Object value) throws TypeConversionException
   {
      return (Binary)BINARY.convert(value);
   }

   /**
    * Converts a string value to integer.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException If the conversion fails.
    */
   public static Integer toInteger(String value) throws TypeConversionException
   {
      if (value == null)
      {
         return null;
      }

      try
      {
         return createInteger(Integer.parseInt(value));
      }
      catch (NumberFormatException e)
      {
         throw new TypeConversionException(INTEGER, e);
      }
   }

   /**
    * Converts a numeric value to integer.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Integer toInteger(Number value)
   {
      if (value == null)
      {
         return null;
      }

      return createInteger(value.intValue());
   }

   /**
    * Converts a timestamp value to integer.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Integer toInteger(Timestamp value)
   {
      if (value == null)
      {
         return null;
      }

      return createInteger((int)value.getTime());
   }

   /**
    * Converts a boolean value to integer.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Integer toInteger(Boolean value)
   {
      if (value == null)
      {
         return null;
      }

      return (value.booleanValue()) ? ONE_INTEGER : ZERO_INTEGER;
   }

   /**
    * Converts a primitive value to integer.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException if the conversion fails.
    */
   public static Integer toInteger(Object value) throws TypeConversionException
   {
      return (Integer)INTEGER.convert(value);
   }

   /**
    * Converts a string value to long.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException If the conversion fails.
    */
   public static Long toLong(String value) throws TypeConversionException
   {
      if (value == null)
      {
         return null;
      }

      try
      {
         return createLong(Long.parseLong(value));
      }
      catch (NumberFormatException e)
      {
         throw new TypeConversionException(LONG, e);
      }
   }

   /**
    * Converts a numeric value to long.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Long toLong(Number value)
   {
      if (value == null)
      {
         return null;
      }

      return createLong(value.longValue());
   }

   /**
    * Converts a timestamp value to long.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Long toLong(Timestamp value)
   {
      if (value == null)
      {
         return null;
      }

      return createLong(value.getTime());
   }

   /**
    * Converts a boolean value to long.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Long toLong(Boolean value)
   {
      if (value == null)
      {
         return null;
      }

      return (value.booleanValue()) ? ONE_LONG : ZERO_LONG;
   }

   /**
    * Converts a primitive value to long.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException if the conversion fails.
    */
   public static Long toLong(Object value) throws TypeConversionException
   {
      return (Long)LONG.convert(value);
   }

   /**
    * Converts a string value to float.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException If the conversion fails.
    */
   public static Float toFloat(String value) throws TypeConversionException
   {
      if (value == null)
      {
         return null;
      }

      try
      {
         return createFloat(Float.parseFloat(value));
      }
      catch (NumberFormatException e)
      {
         throw new TypeConversionException(FLOAT, e);
      }
   }

   /**
    * Converts a numeric value to float.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Float toFloat(Number value)
   {
      if (value == null)
      {
         return null;
      }

      return createFloat(value.floatValue());
   }

   /**
    * Converts a timestamp value to float.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Float toFloat(Timestamp value)
   {
      if (value == null)
      {
         return null;
      }

      return createFloat((float)((double)value.getTime() + (value.getNanos() % 1000000) / 1000000.));
   }

   /**
    * Converts a boolean value to float.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Float toFloat(Boolean value)
   {
      if (value == null)
      {
         return null;
      }

      return (value.booleanValue()) ? ONE_FLOAT : ZERO_FLOAT;
   }

   /**
    * Converts a primitive value to float.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException if the conversion fails.
    */
   public static Float toFloat(Object value) throws TypeConversionException
   {
      return (Float)FLOAT.convert(value);
   }

   /**
    * Converts a string value to double.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException If the conversion fails.
    */
   public static Double toDouble(String value) throws TypeConversionException
   {
      if (value == null)
      {
         return null;
      }

      try
      {
         return createDouble(Double.parseDouble(value));
      }
      catch (NumberFormatException e)
      {
         throw new TypeConversionException(DOUBLE, e);
      }
   }

   /**
    * Converts a numeric value to double.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Double toDouble(Number value)
   {
      if (value == null)
      {
         return null;
      }

      return createDouble(value.doubleValue());
   }

   /**
    * Converts a timestamp value to double.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Double toDouble(Timestamp value)
   {
      if (value == null)
      {
         return null;
      }

      return createDouble((double)value.getTime() + (value.getNanos() % 1000000) / 1000000.);
   }

   /**
    * Converts a boolean value to double.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Double toDouble(Boolean value)
   {
      if (value == null)
      {
         return null;
      }

      return (value.booleanValue()) ? ONE_DOUBLE : ZERO_DOUBLE;
   }

   /**
    * Converts a primitive value to double.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException if the conversion fails.
    */
   public static Double toDouble(Object value) throws TypeConversionException
   {
      return (Double)DOUBLE.convert(value);
   }

   /**
    * Converts a string value to big decimal.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException If the conversion fails.
    */
   public static BigDecimal toDecimal(String value) throws TypeConversionException
   {
      if (value == null)
      {
         return null;
      }

      try
      {
         return new BigDecimal(value);
      }
      catch (NumberFormatException e)
      {
         throw new TypeConversionException(DECIMAL, e);
      }
   }

   /**
    * Converts an integer value to big decimal.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static BigDecimal toDecimal(Integer value)
   {
      if (value == null)
      {
         return null;
      }

      return BigDecimal.valueOf(value.longValue());
   }

   /**
    * Converts a long value to big decimal.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static BigDecimal toDecimal(Long value)
   {
      if (value == null)
      {
         return null;
      }

      return BigDecimal.valueOf(value.longValue());
   }

   /**
    * Converts a float value to big decimal.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static BigDecimal toDecimal(Float value)
   {
      if (value == null)
      {
         return null;
      }

      return new BigDecimal(value.doubleValue());
   }

   /**
    * Converts a double value to big decimal.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static BigDecimal toDecimal(Double value)
   {
      if (value == null)
      {
         return null;
      }

      return new BigDecimal(value.doubleValue());
   }

   /**
    * Converts a timestamp value to big decimal.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static BigDecimal toDecimal(Timestamp value)
   {
      if (value == null)
      {
         return null;
      }

      return BigDecimal.valueOf(value.getTime()).add(BigDecimal.valueOf((value.getNanos() % 1000000), 6));
   }

   /**
    * Converts a boolean value to big decimal.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static BigDecimal toDecimal(Boolean value)
   {
      if (value == null)
      {
         return null;
      }

      return (value.booleanValue()) ? ONE_DECIMAL : ZERO_DECIMAL;
   }

   /**
    * Converts a primitive value to big decimal.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException if the conversion fails.
    */
   public static BigDecimal toDecimal(Object value) throws TypeConversionException
   {
      return (BigDecimal)DECIMAL.convert(value);
   }

   /**
    * Converts a string buffer to timestamp.
    * @param buf The buffer to convert.
    * @return The converted value.
    * @throws TypeConversionException If the conversion fails.
    */
   public static Timestamp toTimestamp(StringBuffer buf) throws TypeConversionException
   {
      if (buf.length() > 0)
      {
         if (buf.charAt(0) == '-')
         {
            buf.setCharAt(0, 'B');
         }
         else
         {
            buf.insert(0, 'A');
         }

         boolean bShort = true;

         if (buf.length() >= 19)
         {
         loop:
            for (int i = 11; i < 17; ++i)
            {
               switch (buf.charAt(i))
               {
                  case 't':
                  case 'T':
                     buf.setCharAt(i, ' ');

                  case ' ':
                     bShort = false;
                     break loop;
               }
            }
         }

         if (bShort)
         {
            if (buf.indexOf("-") < 0)
            {
               buf.insert(1, "1970-01-01 ");
            }

            if (buf.lastIndexOf(":") < 0)
            {
               buf.append(" 00:00:00");
            }
         }
      }

      String sValue = buf.toString();
      ParsePosition pos = new ParsePosition(0);
      java.util.Date dt = ((SimpleDateFormat)s_timestampInFormat.clone()).parse(sValue, pos);

      if (pos.getErrorIndex() >= 0 ||
         pos.getIndex() < sValue.length() &&
         sValue.charAt(pos.getIndex()) != '.')
      {
         throw new TypeConversionException(TIMESTAMP);
      }

      int nNanos = 0;

      if (pos.getIndex() < sValue.length())
      {
         String sDec = sValue.substring(pos.getIndex() + 1);

         for (int i = 0; i < sDec.length(); ++i)
         {
            if (Character.digit(sDec.charAt(i), 10) < 0)
            {
               throw new TypeConversionException(TIMESTAMP);
            }
         }

         if (sDec.length() > 9)
         {
            throw new TypeConversionException(TIMESTAMP);
         }

         nNanos = Integer.parseInt(sDec + "000000000".substring(sDec.length()));
      }

      Timestamp ts = new Timestamp(dt.getTime());

      ts.setNanos(nNanos);

      return ts;
   }

   /**
    * Converts a string value to timestamp.
    * @param sValue The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException If the conversion fails.
    */
   public static Timestamp toTimestamp(String sValue) throws TypeConversionException
   {
      if (sValue == null)
      {
         return null;
      }

      StringBuffer buf = new StringBuffer(Math.max(sValue.length() + 1, 19));

      buf.append(sValue);

      return toTimestamp(buf);
   }

   /**
    * Converts an integer value to timestamp.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Timestamp toTimestamp(Integer value)
   {
      if (value == null)
      {
         return null;
      }

      return new Timestamp(value.longValue());
   }

   /**
    * Converts a long value to timestamp.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Timestamp toTimestamp(Long value)
   {
      if (value == null)
      {
         return null;
      }

      return new Timestamp(value.longValue());
   }

   /**
    * Converts a big decimal value to timestamp.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Timestamp toTimestamp(BigDecimal value)
   {
      if (value == null)
      {
         return null;
      }

      Timestamp ts = new Timestamp(value.longValue());

      ts.setNanos(value.subtract(value.setScale(0, BigDecimal.ROUND_FLOOR)).movePointRight(9).intValue());

      return ts;
   }

   /**
    * Converts a float value to timestamp.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Timestamp toTimestamp(Float value)
   {
      if (value == null)
      {
         return null;
      }

      Timestamp ts = new Timestamp(value.longValue());
      double d = value.doubleValue() / 1000;

      d -= Math.floor(d);
      ts.setNanos((int)(d * 1000000000));

      return ts;
   }

   /**
    * Converts a double value to timestamp.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Timestamp toTimestamp(Double value)
   {
      if (value == null)
      {
         return null;
      }

      Timestamp ts = new Timestamp(value.longValue());
      double d = value.doubleValue() / 1000;

      d -= Math.floor(d);
      ts.setNanos((int)(d * 1000000000));

      return ts;
   }

   /**
    * Converts a primitive value to timestamp.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException if the conversion fails.
    */
   public static Timestamp toTimestamp(Object value) throws TypeConversionException
   {
      return (Timestamp)TIMESTAMP.convert(value);
   }

   /**
    * Converts a string value to boolean.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException If the conversion fails.
    */
   public static Boolean toBoolean(String value) throws TypeConversionException
   {
      if (value == null)
      {
         return null;
      }

      if (value.equals("1") || value.equalsIgnoreCase("true"))
      {
         return Boolean.TRUE;
      }

      if (value.equals("0") || value.equalsIgnoreCase("false"))
      {
         return Boolean.FALSE;
      }

      throw new TypeConversionException(BOOLEAN);
   }

   /**
    * Converts an integer value to boolean.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Boolean toBoolean(Integer value)
   {
      if (value == null)
      {
         return null;
      }

      return Boolean.valueOf(value.intValue() != 0);
   }

   /**
    * Converts a long value to boolean.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Boolean toBoolean(Long value)
   {
      if (value == null)
      {
         return null;
      }

      return Boolean.valueOf(value.longValue() != 0L);
   }

   /**
    * Converts a big decimal value to boolean.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Boolean toBoolean(BigDecimal value)
   {
      if (value == null)
      {
         return null;
      }

      return Boolean.valueOf(value.compareTo(ZERO_DECIMAL) != 0);
   }

   /**
    * Converts a float value to boolean.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Boolean toBoolean(Float value)
   {
      if (value == null)
      {
         return null;
      }

      return Boolean.valueOf(value.floatValue() != 0f);
   }

   /**
    * Converts a double value to boolean.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    */
   public static Boolean toBoolean(Double value)
   {
      if (value == null)
      {
         return null;
      }

      return Boolean.valueOf(value.doubleValue() != 0.);
   }

   /**
    * Converts a primitive value to boolean.
    * @param value The value to convert. Can be null.
    * @return The converted value.
    * @throws TypeConversionException if the conversion fails.
    */
   public static Boolean toBoolean(Object value) throws TypeConversionException
   {
      return (Boolean)BOOLEAN.convert(value);
   }

   /**
    * Finds a type converter given the source and the destination types.
    * @param fromType The source type.
    * @param toType The destination (converted) type.
    * @return The converter function or null if not found.
    */
   public static UnaryFunction findConverter(Primitive fromType, Primitive toType)
   {
      if (toType == null)
      {
         return NULL_UNARY_FUNCTION;
      }

      return toType.findConverter(fromType);
   }

   /**
    * Gets a type converter given the source and the destination types.
    * @param fromType The source type.
    * @param toType The destination (converted) type.
    * @return The converter function.
    * @throws TypeConversionException if the converter is not found.
    */
   public static UnaryFunction getConverter(Primitive fromType, Primitive toType) throws TypeConversionException
   {
      if (toType == null)
      {
         return NULL_UNARY_FUNCTION;
      }

      return toType.getConverter(fromType);
   }

   /**
    * Determines if a type conversion preserves the sort order.
    * @param fromType The source type.
    * @param toType The converted type.
    * @return True if the sort order is preserved during the type conversion.
    */
   public static boolean isOrderPreserved(Primitive fromType, Primitive toType)
   {
      switch (fromType.m_nOrdinal)
      {
         case STRING_ORDINAL:
         case BINARY_ORDINAL:
            switch (toType.m_nOrdinal)
            {
               case STRING_ORDINAL:
               case BINARY_ORDINAL:
               case BOOLEAN_ORDINAL:
                  return true;

               case INTEGER_ORDINAL:
               case LONG_ORDINAL:
               case DECIMAL_ORDINAL:
               case FLOAT_ORDINAL:
               case DOUBLE_ORDINAL:
               case TIMESTAMP_ORDINAL:
                  return false;
            }

            break;

         case INTEGER_ORDINAL:
         case LONG_ORDINAL:
         case DECIMAL_ORDINAL:
         case FLOAT_ORDINAL:
         case DOUBLE_ORDINAL:
         case TIMESTAMP_ORDINAL:
            switch (toType.m_nOrdinal)
            {
               case INTEGER_ORDINAL:
               case LONG_ORDINAL:
               case DECIMAL_ORDINAL:
               case FLOAT_ORDINAL:
               case DOUBLE_ORDINAL:
               case TIMESTAMP_ORDINAL:
               case BOOLEAN_ORDINAL:
                  return true;
            }

            break;

         case BOOLEAN_ORDINAL:
            switch (toType.m_nOrdinal)
            {
               case STRING_ORDINAL:
               case BINARY_ORDINAL:
               case INTEGER_ORDINAL:
               case LONG_ORDINAL:
               case DECIMAL_ORDINAL:
               case FLOAT_ORDINAL:
               case DOUBLE_ORDINAL:
               case TIMESTAMP_ORDINAL:
               case BOOLEAN_ORDINAL:
                  return true;
            }

            break;
      }

      return false;
   }

   /**
    * Computes an estimate of a value size in bytes.
    * @param value The value to estimate. Can be null.
    * @return The value size in bytes.
    */
   public static int getDataSize(Object value)
   {
      if (value == null)
      {
         return 1;
      }

      if (value instanceof String)
      {
         int n = ((String)value).length();

         if (n > (Integer.MAX_VALUE >> 1))
         {
            return Integer.MAX_VALUE;
         }

         return n << 1;
      }

      if (value instanceof Number)
      {
         if (value instanceof Integer)
         {
            return 4;
         }

         if (value instanceof Long)
         {
            return 8;
         }

         if (value instanceof BigDecimal)
         {
            return (((BigDecimal)value).precision() + 1) >> 1;
         }

         if (value instanceof Float)
         {
            return 4;
         }

         if (value instanceof Double)
         {
            return 8;
         }

         return 2;
      }

      if (value instanceof Boolean)
      {
         return 1;
      }

      if (value instanceof Timestamp)
      {
         return 8;
      }

      if (value instanceof Binary)
      {
         long lSize = ((Binary)value).getSize();

         return (lSize > Integer.MAX_VALUE) ? Integer.MAX_VALUE : (int)lSize;
      }

      return 0;
   }

   /**
    * Computes a logical negation.
    * @param value The value to negate. Can be null.
    * @return The result. Null if value is null.
    */
   public static Boolean not(Boolean value)
   {
      if (value == null)
      {
         return null;
      }

      return Boolean.valueOf(!value.booleanValue());
   }

   /**
    * Computes a logical negation.
    * @param value The value to negate. Can be null.
    * @return The result. Null if value is null.
    * @throws TypeMismatchException if the value type is invalid.
    */
   public static Object not(Object value) throws TypeMismatchException
   {
      if (value == null)
      {
         return null;
      }

      return primitiveOf(value).getNotFunction().invoke(value);
   }

   /**
    * Finds the logical negation function for a given type.
    * @param type The type for which to find the function.
    * @return The function, or null if not found.
    */
   public static UnaryFunction findNotFunction(Primitive type)
   {
      if (type == null)
      {
         return NULL_UNARY_FUNCTION;
      }

      return type.findNotFunction();
   }

   /**
    * Gets the logical negation function for a given type.
    * @param type The type for which to get the function.
    * @return The function.
    * @throws TypeMismatchException if the function is not defined.
    */
   public static UnaryFunction getNotFunction(Primitive type) throws TypeMismatchException
   {
      if (type == null)
      {
         return NULL_UNARY_FUNCTION;
      }

      return type.getNotFunction();
   }

   /**
    * Computes an arithmetic negation.
    * @param value The value to negate. Can be null.
    * @return The result. Null if value if null.
    */
   public static Integer negate(Integer value)
   {
      if (value == null)
      {
         return value;
      }

      return createInteger(-value.intValue());
   }

   /**
    * Computes an arithmetic negation.
    * @param value The value to negate. Can be null.
    * @return The result. Null if value if null.
    */
   public static Long negate(Long value)
   {
      if (value == null)
      {
         return value;
      }

      return createLong(-value.longValue());
   }

   /**
    * Computes an arithmetic negation.
    * @param value The value to negate. Can be null.
    * @return The result. Null if value if null.
    */
   public static BigDecimal negate(BigDecimal value)
   {
      if (value == null)
      {
         return value;
      }

      return value.negate();
   }

   /**
    * Computes an arithmetic negation.
    * @param value The value to negate. Can be null.
    * @return The result. Null if value if null.
    */
   public static Float negate(Float value)
   {
      if (value == null)
      {
         return value;
      }

      return createFloat(-value.floatValue());
   }

   /**
    * Computes an arithmetic negation.
    * @param value The value to negate. Can be null.
    * @return The result. Null if value if null.
    */
   public static Double negate(Double value)
   {
      if (value == null)
      {
         return value;
      }

      return createDouble(-value.doubleValue());
   }

   /**
    * Computes an arithmetic negation.
    * @param value The value to negate. Can be null.
    * @return The result. Null if value if null.
    * @throws TypeMismatchException if the value type is invalid.
    */
   public static Object negate(Object value) throws TypeMismatchException
   {
      if (value == null)
      {
         return null;
      }

      return primitiveOf(value).getNegationFunction().invoke(value);
   }

   /**
    * Finds the arithmetic negation function for a given type.
    * @param type The type for which to find the function.
    * @return The function, or null if not found.
    */
   public static UnaryFunction findNegationFunction(Primitive type)
   {
      if (type == null)
      {
         return NULL_UNARY_FUNCTION;
      }

      return type.findNegationFunction();
   }

   /**
    * Gets the arithmetic negation function for a given type.
    * @param type The type for which to get the function.
    * @return The function.
    * @throws TypeMismatchException if the function is not defined.
    */
   public static UnaryFunction getNegationFunction(Primitive type) throws TypeMismatchException
   {
      if (type == null)
      {
         return NULL_UNARY_FUNCTION;
      }

      return type.getNegationFunction();
   }

   /**
    * Multiplies two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result. Null if either left or right is null.
    */
   public static Integer multiply(Integer left, Integer right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return createInteger(left.intValue() * right.intValue());
   }

   /**
    * Multiplies two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result. Null if either left or right is null.
    */
   public static Long multiply(Long left, Long right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return createLong(left.longValue() * right.longValue());
   }

   /**
    * Multiplies two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result. Null if either left or right is null.
    */
   public static BigDecimal multiply(BigDecimal left, BigDecimal right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return left.multiply(right);
   }

   /**
    * Multiplies two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result. Null if either left or right is null.
    */
   public static Float multiply(Float left, Float right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return createFloat(left.floatValue() * right.floatValue());
   }

   /**
    * Multiplies two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result. Null if either left or right is null.
    */
   public static Double multiply(Double left, Double right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return createDouble(left.doubleValue() * right.doubleValue());
   }

   /**
    * Multiplies two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result. Null if either left or right is null.
    * @throws TypeMismatchException if the value types are invalid.
    */
   public static Object multiply(Object left, Object right)
   {
      BinaryFunction f = s_primitiveArray[ordinalOf(left)].m_multiplicationArray[ordinalOf(right)];

      if (f != null)
      {
         return f.invoke(left, right);
      }

      throw new TypeMismatchException(Symbol.MUL);
   }

   /**
    * Divides two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static BigDecimal divide(Integer left, Integer right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      int nLeft = left.intValue();
      int nRight = right.intValue();
      int nResult = nLeft / nRight;

      if (nResult * nRight == nLeft)
      {
         return BigDecimal.valueOf(nResult);
      }

      return divide(BigDecimal.valueOf(nLeft), BigDecimal.valueOf(nRight));
   }

   /**
    * Divides two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static BigDecimal divide(Long left, Long right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      long lLeft = left.longValue();
      long lRight = right.longValue();
      long lResult = lLeft / lRight;

      if (lResult * lRight == lLeft)
      {
         return BigDecimal.valueOf(lResult);
      }

      return divide(BigDecimal.valueOf(lLeft), BigDecimal.valueOf(lRight));
   }

   /**
    * Divides two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static BigDecimal divide(BigDecimal left, BigDecimal right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      BigDecimal result = left.divide(right, MAX_SCALE, BigDecimal.ROUND_HALF_UP);

      if (left.scale() < MAX_SCALE && right.scale() < MAX_SCALE)
      {
         int nMin = 0;
         int nMax = result.scale();

         while (nMin <= nMax)
         {
            int nMid = (nMax + nMin) >> 1;

            right = result.setScale(nMid, BigDecimal.ROUND_HALF_UP);

            if (result.compareTo(right) == 0)
            {
               result = right;
               nMax = nMid - 1;
            }
            else
            {
               nMin = nMid + 1;
            }
         }
      }

      return result;
   }

   /**
    * Divides two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static Float divide(Float left, Float right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return createFloat(left.floatValue() / right.floatValue());
   }

   /**
    * Divides two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static Double divide(Double left, Double right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return createDouble(left.doubleValue() / right.doubleValue());
   }

   /**
    * Divides two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static Object divide(Object left, Object right)
   {
      BinaryFunction f = s_primitiveArray[ordinalOf(left)].m_divisionArray[ordinalOf(right)];

      if (f != null)
      {
         return f.invoke(left, right);
      }

      throw new TypeMismatchException(Symbol.DIVIDE);
   }

   /**
    * Adds two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static Integer add(Integer left, Integer right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return createInteger(left.intValue() + right.intValue());
   }

   /**
    * Adds two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static Long add(Long left, Long right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return createLong(left.longValue() + right.longValue());
   }

   /**
    * Adds two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static BigDecimal add(BigDecimal left, BigDecimal right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return left.add(right);
   }

   /**
    * Adds two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static Float add(Float left, Float right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return createFloat(left.floatValue() + right.floatValue());
   }

   /**
    * Adds two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static Double add(Double left, Double right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return createDouble(left.doubleValue() + right.doubleValue());
   }

   /**
    * Adds two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static Object add(Object left, Object right)
   {
      BinaryFunction f = s_primitiveArray[ordinalOf(left)].m_plusArray[ordinalOf(right)];

      if (f != null)
      {
         return f.invoke(left, right);
      }

      throw new TypeMismatchException(Symbol.PLUS);
   }

   /**
    * Subtracts two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static Integer subtract(Integer left, Integer right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return createInteger(left.intValue() - right.intValue());
   }

   /**
    * Subtracts two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static Long subtract(Long left, Long right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return createLong(left.longValue() - right.longValue());
   }

   /**
    * Subtracts two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static BigDecimal subtract(BigDecimal left, BigDecimal right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return left.subtract(right);
   }

   /**
    * Subtracts two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static Float subtract(Float left, Float right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return createFloat(left.floatValue() - right.floatValue());
   }

   /**
    * Subtracts two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static Double subtract(Double left, Double right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return createDouble(left.doubleValue() - right.doubleValue());
   }

   /**
    * Subtracts two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result.
    */
   public static Object subtract(Object left, Object right)
   {
      BinaryFunction f = s_primitiveArray[ordinalOf(left)].m_minusArray[ordinalOf(right)];

      if (f != null)
      {
         return f.invoke(left, right);
      }

      throw new TypeMismatchException(Symbol.MINUS);
   }

   /**
    * Gets a regexp-pattern from a like-pattern string (using * and ? only).
    * @param sLike The like-pattern string.
    * @param nFlags The pattern compilation flags (Pattern.*).
    * @return The compiled regexp-pattern.
    */
   public static Pattern likePattern(String sLike, int nFlags)
   {
      int nCount = sLike.length();
      StringBuffer buf = new StringBuffer(nCount + 8);

      for (int i = 0; i < nCount; ++i)
      {
         char ch = sLike.charAt(i);

         switch (ch)
         {
            case '\\':
            case '.':
            case '(':
            case ')':
            case '[':
            case ']':
            case '{':
            case '}':
            case '^':
            case '$':
            case '+':
            case '|':
               buf.append('\\');
               buf.append(ch);
               break;

            case '?':
               buf.append('.');
               break;

            case '*':
               buf.append('.');
               buf.append('*');
               break;

            default:
               buf.append(ch);
               break;
         }
      }

      return Pattern.compile(buf.toString(), nFlags);
   }

   /**
    * Pattern-matching comparison for two values.
    * * and ? patterns are allowed.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values match, incl. when they are both null.
    */
   public static Boolean like(String left, String right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == right);
      }

      return Boolean.valueOf(likePattern(right,
         Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE)
         .matcher(left).matches());
   }

   /**
    * Pattern-matching comparison for two values.
    * * and ? patterns are allowed.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values match, incl. when they are both null.
    */
   public static Object like(Object left, Object right)
   {
      BinaryFunction f = s_primitiveArray[ordinalOf(left)].m_likeArray[ordinalOf(right)];

      if (f != null)
      {
         return f.invoke(left, right);
      }

      throw new TypeMismatchException(Symbol.LIKE_P);
   }

   /**
    * Checks if the left value is greater than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than the right value.
    */
   public static Boolean gt(String left, String right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null && right == null);
      }

      return Boolean.valueOf(left.compareToIgnoreCase(right) > 0);
   }

   /**
    * Checks if the left value is greater than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than the right value.
    */
   public static Boolean gt(Binary left, Binary right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null && right == null);
      }

      return Boolean.valueOf(left.compareTo(right) > 0);
   }

   /**
    * Checks if the left value is greater than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than the right value.
    */
   public static Boolean gt(Integer left, Integer right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null && right == null);
      }

      return Boolean.valueOf(left.intValue() > right.intValue());
   }

   /**
    * Checks if the left value is greater than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than the right value.
    */
   public static Boolean gt(Long left, Long right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null && right == null);
      }

      return Boolean.valueOf(left.longValue() > right.longValue());
   }

   /**
    * Checks if the left value is greater than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than the right value.
    */
   public static Boolean gt(BigDecimal left, BigDecimal right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null && right == null);
      }

      return Boolean.valueOf(left.compareTo(right) > 0);
   }

   /**
    * Checks if the left value is greater than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than the right value.
    */
   public static Boolean gt(Float left, Float right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null && right == null);
      }

      return Boolean.valueOf(left.floatValue() > right.floatValue());
   }

   /**
    * Checks if the left value is greater than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than the right value.
    */
   public static Boolean gt(Double left, Double right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null && right == null);
      }

      return Boolean.valueOf(left.doubleValue() > right.doubleValue());
   }

   /**
    * Checks if the left value is greater than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than the right value.
    */
   public static Boolean gt(Timestamp left, Timestamp right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null && right == null);
      }

      return Boolean.valueOf(left.compareTo(right) > 0);
   }

   /**
    * Checks if the left value is greater than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than the right value.
    */
   public static Boolean gt(Boolean left, Boolean right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null && right == null);
      }

      return Boolean.valueOf(left.booleanValue() && !right.booleanValue());
   }

   /**
    * Checks if the left value is greater than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than the right value.
    */
   public static Object gt(Object left, Object right)
   {
      BinaryFunction f = s_primitiveArray[ordinalOf(left)].m_gtArray[ordinalOf(right)];

      if (f != null)
      {
         return f.invoke(left, right);
      }

      throw new TypeMismatchException(Symbol.GT);
   }

   /**
    * Checks if the left value is greater than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than or equal to the right value.
    */
   public static Boolean ge(String left, String right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null || right == null);
      }

      return Boolean.valueOf(left.compareToIgnoreCase(right) >= 0);
   }

   /**
    * Checks if the left value is greater than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than or equal to the right value.
    */
   public static Boolean ge(Binary left, Binary right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null || right == null);
      }

      return Boolean.valueOf(left.compareTo(right) >= 0);
   }

   /**
    * Checks if the left value is greater than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than or equal to the right value.
    */
   public static Boolean ge(Integer left, Integer right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null || right == null);
      }

      return Boolean.valueOf(left.intValue() >= right.intValue());
   }

   /**
    * Checks if the left value is greater than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than or equal to the right value.
    */
   public static Boolean ge(Long left, Long right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null || right == null);
      }

      return Boolean.valueOf(left.longValue() >= right.longValue());
   }

   /**
    * Checks if the left value is greater than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than or equal to the right value.
    */
   public static Boolean ge(BigDecimal left, BigDecimal right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null || right == null);
      }

      return Boolean.valueOf(left.compareTo(right) >= 0);
   }

   /**
    * Checks if the left value is greater than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than or equal to the right value.
    */
   public static Boolean ge(Float left, Float right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null || right == null);
      }

      return Boolean.valueOf(left.floatValue() >= right.floatValue());
   }

   /**
    * Checks if the left value is greater than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than or equal to the right value.
    */
   public static Boolean ge(Double left, Double right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null || right == null);
      }

      return Boolean.valueOf(left.doubleValue() >= right.doubleValue());
   }

   /**
    * Checks if the left value is greater than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than or equal to the right value.
    */
   public static Boolean ge(Timestamp left, Timestamp right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null || right == null);
      }

      return Boolean.valueOf(left.compareTo(right) >= 0);
   }

   /**
    * Checks if the left value is greater than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than or equal to the right value.
    */
   public static Boolean ge(Boolean left, Boolean right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != null || right == null);
      }

      return Boolean.valueOf(left.booleanValue() || !right.booleanValue());
   }

   /**
    * Checks if the left value is greater than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is greater than or equal to the right value.
    */
   public static Object ge(Object left, Object right)
   {
      BinaryFunction f = s_primitiveArray[ordinalOf(left)].m_geArray[ordinalOf(right)];

      if (f != null)
      {
         return f.invoke(left, right);
      }

      throw new TypeMismatchException(Symbol.GE);
   }

   /**
    * Checks if the left value is less than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than the right value.
    */
   public static Boolean lt(String left, String right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null && right != null);
      }

      return Boolean.valueOf(left.compareToIgnoreCase(right) < 0);
   }

   /**
    * Checks if the left value is less than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than the right value.
    */
   public static Boolean lt(Binary left, Binary right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null && right != null);
      }

      return Boolean.valueOf(left.compareTo(right) < 0);
   }

   /**
    * Checks if the left value is less than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than the right value.
    */
   public static Boolean lt(Integer left, Integer right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null && right != null);
      }

      return Boolean.valueOf(left.intValue() < right.intValue());
   }

   /**
    * Checks if the left value is less than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than the right value.
    */
   public static Boolean lt(Long left, Long right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null && right != null);
      }

      return Boolean.valueOf(left.longValue() < right.longValue());
   }

   /**
    * Checks if the left value is less than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than the right value.
    */
   public static Boolean lt(BigDecimal left, BigDecimal right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null && right != null);
      }

      return Boolean.valueOf(left.compareTo(right) < 0);
   }

   /**
    * Checks if the left value is less than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than the right value.
    */
   public static Boolean lt(Float left, Float right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null && right != null);
      }

      return Boolean.valueOf(left.floatValue() < right.floatValue());
   }

   /**
    * Checks if the left value is less than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than the right value.
    */
   public static Boolean lt(Double left, Double right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null && right != null);
      }

      return Boolean.valueOf(left.doubleValue() < right.doubleValue());
   }

   /**
    * Checks if the left value is less than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than the right value.
    */
   public static Boolean lt(Timestamp left, Timestamp right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null && right != null);
      }

      return Boolean.valueOf(left.compareTo(right) < 0);
   }

   /**
    * Checks if the left value is less than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than the right value.
    */
   public static Boolean lt(Boolean left, Boolean right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null && right != null);
      }

      return Boolean.valueOf(!left.booleanValue() && right.booleanValue());
   }

   /**
    * Checks if the left value is less than the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than the right value.
    */
   public static Object lt(Object left, Object right)
   {
      BinaryFunction f = s_primitiveArray[ordinalOf(left)].m_ltArray[ordinalOf(right)];

      if (f != null)
      {
         return f.invoke(left, right);
      }

      throw new TypeMismatchException(Symbol.LT);
   }

   /**
    * Checks if the left value is less than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than or equal to the right value.
    */
   public static Boolean le(String left, String right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null || right != null);
      }

      return Boolean.valueOf(left.compareToIgnoreCase(right) <= 0);
   }

   /**
    * Checks if the left value is less than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than or equal to the right value.
    */
   public static Boolean le(Binary left, Binary right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null || right != null);
      }

      return Boolean.valueOf(left.compareTo(right) <= 0);
   }

   /**
    * Checks if the left value is less than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than or equal to the right value.
    */
   public static Boolean le(Integer left, Integer right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null || right != null);
      }

      return Boolean.valueOf(left.intValue() <= right.intValue());
   }

   /**
    * Checks if the left value is less than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than or equal to the right value.
    */
   public static Boolean le(Long left, Long right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null || right != null);
      }

      return Boolean.valueOf(left.longValue() <= right.longValue());
   }

   /**
    * Checks if the left value is less than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than or equal to the right value.
    */
   public static Boolean le(BigDecimal left, BigDecimal right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null || right != null);
      }

      return Boolean.valueOf(left.compareTo(right) <= 0);
   }

   /**
    * Checks if the left value is less than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than or equal to the right value.
    */
   public static Boolean le(Float left, Float right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null || right != null);
      }

      return Boolean.valueOf(left.floatValue() <= right.floatValue());
   }

   /**
    * Checks if the left value is less than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than or equal to the right value.
    */
   public static Boolean le(Double left, Double right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null || right != null);
      }

      return Boolean.valueOf(left.doubleValue() <= right.doubleValue());
   }

   /**
    * Checks if the left value is less than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than or equal to the right value.
    */
   public static Boolean le(Timestamp left, Timestamp right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null || right != null);
      }

      return Boolean.valueOf(left.compareTo(right) <= 0);
   }

   /**
    * Checks if the left value is less than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than or equal to the right value.
    */
   public static Boolean le(Boolean left, Boolean right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == null || right != null);
      }

      return Boolean.valueOf(!left.booleanValue() || right.booleanValue());
   }

   /**
    * Checks if the left value is less than or equal to the right value.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the left value is less than or equal to the right value.
    */
   public static Object le(Object left, Object right)
   {
      BinaryFunction f = s_primitiveArray[ordinalOf(left)].m_leArray[ordinalOf(right)];

      if (f != null)
      {
         return f.invoke(left, right);
      }

      throw new TypeMismatchException(Symbol.LE);
   }

   /**
    * Checks if two values are equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are equal.
    */
   public static Boolean eq(String left, String right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == right);
      }

      return Boolean.valueOf(left.compareToIgnoreCase(right) == 0);
   }

   /**
    * Checks if two values are equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are equal.
    */
   public static Boolean eq(Binary left, Binary right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == right);
      }

      return Boolean.valueOf(left.compareTo(right) == 0);
   }

   /**
    * Checks if two values are equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are equal.
    */
   public static Boolean eq(Integer left, Integer right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == right);
      }

      return Boolean.valueOf(left.intValue() == right.intValue());
   }

   /**
    * Checks if two values are equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are equal.
    */
   public static Boolean eq(Long left, Long right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == right);
      }

      return Boolean.valueOf(left.longValue() == right.longValue());
   }

   /**
    * Checks if two values are equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are equal.
    */
   public static Boolean eq(BigDecimal left, BigDecimal right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == right);
      }

      return Boolean.valueOf(left.compareTo(right) == 0);
   }

   /**
    * Checks if two values are equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are equal.
    */
   public static Boolean eq(Float left, Float right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == right);
      }

      return Boolean.valueOf(left.floatValue() == right.floatValue());
   }

   /**
    * Checks if two values are equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are equal.
    */
   public static Boolean eq(Double left, Double right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == right);
      }

      return Boolean.valueOf(left.doubleValue() == right.doubleValue());
   }

   /**
    * Checks if two values are equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are equal.
    */
   public static Boolean eq(Timestamp left, Timestamp right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == right);
      }

      return Boolean.valueOf(left.compareTo(right) == 0);
   }

   /**
    * Checks if two values are equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are equal.
    */
   public static Boolean eq(Boolean left, Boolean right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left == right);
      }

      return Boolean.valueOf(left.booleanValue() == right.booleanValue());
   }

   /**
    * Checks if two values are equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are equal.
    */
   public static Object eq(Object left, Object right)
   {
      BinaryFunction f = s_primitiveArray[ordinalOf(left)].m_eqArray[ordinalOf(right)];

      if (f != null)
      {
         return f.invoke(left, right);
      }

      throw new TypeMismatchException(Symbol.EQ);
   }

   /**
    * Checks if two values are not equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are not equal.
    */
   public static Boolean ne(String left, String right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != right);
      }

      return Boolean.valueOf(left.compareToIgnoreCase(right) != 0);
   }

   /**
    * Checks if two values are not equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are not equal.
    */
   public static Boolean ne(Binary left, Binary right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != right);
      }

      return Boolean.valueOf(left.compareTo(right) != 0);
   }

   /**
    * Checks if two values are not equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are not equal.
    */
   public static Boolean ne(Integer left, Integer right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != right);
      }

      return Boolean.valueOf(left.intValue() != right.intValue());
   }

   /**
    * Checks if two values are not equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are not equal.
    */
   public static Boolean ne(Long left, Long right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != right);
      }

      return Boolean.valueOf(left.longValue() != right.longValue());
   }

   /**
    * Checks if two values are not equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are not equal.
    */
   public static Boolean ne(BigDecimal left, BigDecimal right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != right);
      }

      return Boolean.valueOf(left.compareTo(right) != 0);
   }

   /**
    * Checks if two values are not equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are not equal.
    */
   public static Boolean ne(Float left, Float right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != right);
      }

      return Boolean.valueOf(left.floatValue() != right.floatValue());
   }

   /**
    * Checks if two values are not equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are not equal.
    */
   public static Boolean ne(Double left, Double right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != right);
      }

      return Boolean.valueOf(left.doubleValue() != right.doubleValue());
   }

   /**
    * Checks if two values are not equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are not equal.
    */
   public static Boolean ne(Timestamp left, Timestamp right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != right);
      }

      return Boolean.valueOf(left.compareTo(right) != 0);
   }

   /**
    * Checks if two values are not equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are not equal.
    */
   public static Boolean ne(Boolean left, Boolean right)
   {
      if (left == null || right == null)
      {
         return Boolean.valueOf(left != right);
      }

      return Boolean.valueOf(left.booleanValue() != right.booleanValue());
   }

   /**
    * Checks if two values are not equal.
    * @param left The left value.
    * @param right The right value.
    * @return Boolean.TRUE if the values are not equal.
    */
   public static Object ne(Object left, Object right)
   {
      BinaryFunction f = s_primitiveArray[ordinalOf(left)].m_neArray[ordinalOf(right)];

      if (f != null)
      {
         return f.invoke(left, right);
      }

      throw new TypeMismatchException(Symbol.NE);
   }

   /**
    * Computes logical conjunction of two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result. Null if left or right is null.
    */
   public static Boolean and(Boolean left, Boolean right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return Boolean.valueOf(left.booleanValue() && right.booleanValue());
   }

   /**
    * Computes logical conjunction of two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result. Null if left or right is null.
    */
   public static Object and(Object left, Object right)
   {
      BinaryFunction f = s_primitiveArray[ordinalOf(left)].m_andArray[ordinalOf(right)];

      if (f != null)
      {
         return f.invoke(left, right);
      }

      throw new TypeMismatchException(Symbol.AND);
   }

   /**
    * Computes logical disjunction of two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result. Null if left or right is null.
    */
   public static Boolean or(Boolean left, Boolean right)
   {
      if (left == null || right == null)
      {
         return null;
      }

      return Boolean.valueOf(left.booleanValue() || right.booleanValue());
   }

   /**
    * Computes logical disjunction of two values.
    * @param left The left value.
    * @param right The right value.
    * @return The result. Null if left or right is null.
    */
   public static Object or(Object left, Object right)
   {
      BinaryFunction f = s_primitiveArray[ordinalOf(left)].m_orArray[ordinalOf(right)];

      if (f != null)
      {
         return f.invoke(left, right);
      }

      throw new TypeMismatchException(Symbol.OR);
   }

   /**
    * Finds the multiplication function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function, or null if not found.
    */
   public static BinaryFunction findMultiplicationFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.findMultiplicationFunction(right);
   }

   /**
    * Gets the multiplication function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function.
    * @throws TypeConversionException if the types are invalid.
    */
   public static BinaryFunction getMultiplicationFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.getMultiplicationFunction(right);
   }

   /**
    * Finds the division function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function, or null if not found.
    */
   public static BinaryFunction findDivisionFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.findDivisionFunction(right);
   }

   /**
    * Gets the division function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function.
    * @throws TypeConversionException if the types are invalid.
    */
   public static BinaryFunction getDivisionFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.getDivisionFunction(right);
   }

   /**
    * Finds the plus function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function, or null if not found.
    */
   public static BinaryFunction findPlusFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.findPlusFunction(right);
   }

   /**
    * Gets the plus function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function.
    * @throws TypeConversionException if the types are invalid.
    */
   public static BinaryFunction getPlusFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.getPlusFunction(right);
   }

   /**
    * Finds the minus function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function, or null if not found.
    */
   public static BinaryFunction findMinusFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.findMinusFunction(right);
   }

   /**
    * Gets the minus function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function.
    * @throws TypeConversionException if the types are invalid.
    */
   public static BinaryFunction getMinusFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.getMinusFunction(right);
   }

   /**
    * Finds the like function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function, or null if not found.
    */
   public static BinaryFunction findLikeFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.findLikeFunction(right);
   }

   /**
    * Gets the like function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function.
    * @throws TypeConversionException if the types are invalid.
    */
   public static BinaryFunction getLikeFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.getLikeFunction(right);
   }

   /**
    * Finds the gt function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function, or null if not found.
    */
   public static BinaryFunction findGTFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.findGTFunction(right);
   }

   /**
    * Gets the gt function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function.
    * @throws TypeConversionException if the types are invalid.
    */
   public static BinaryFunction getGTFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.getGTFunction(right);
   }

   /**
    * Finds the ge function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function, or null if not found.
    */
   public static BinaryFunction findGEFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.findGEFunction(right);
   }

   /**
    * Gets the ge function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function.
    * @throws TypeConversionException if the types are invalid.
    */
   public static BinaryFunction getGEFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.getGEFunction(right);
   }

   /**
    * Finds the lt function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function, or null if not found.
    */
   public static BinaryFunction findLTFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.findLTFunction(right);
   }

   /**
    * Gets the lt function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function.
    * @throws TypeConversionException if the types are invalid.
    */
   public static BinaryFunction getLTFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.getLTFunction(right);
   }

   /**
    * Finds the le function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function, or null if not found.
    */
   public static BinaryFunction findLEFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.findLEFunction(right);
   }

   /**
    * Gets the le function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function.
    * @throws TypeConversionException if the types are invalid.
    */
   public static BinaryFunction getLEFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.getLEFunction(right);
   }

   /**
    * Finds the eq function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function, or null if not found.
    */
   public static BinaryFunction findEQFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.findEQFunction(right);
   }

   /**
    * Gets the eq function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function.
    * @throws TypeConversionException if the types are invalid.
    */
   public static BinaryFunction getEQFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.getEQFunction(right);
   }

   /**
    * Finds the ne function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function, or null if not found.
    */
   public static BinaryFunction findNEFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.findNEFunction(right);
   }

   /**
    * Gets the ne function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function.
    * @throws TypeConversionException if the types are invalid.
    */
   public static BinaryFunction getNEFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.getNEFunction(right);
   }

   /**
    * Finds the and function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function, or null if not found.
    */
   public static BinaryFunction findAndFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.findAndFunction(right);
   }

   /**
    * Gets the and function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function.
    * @throws TypeConversionException if the types are invalid.
    */
   public static BinaryFunction getAndFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.getAndFunction(right);
   }

   /**
    * Finds the or function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function, or null if not found.
    */
   public static BinaryFunction findOrFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.findOrFunction(right);
   }

   /**
    * Gets the or function for a given pair of types.
    * @param left Left operand type.
    * @param right Right operand type.
    * @return The binary function.
    * @throws TypeConversionException if the types are invalid.
    */
   public static BinaryFunction getOrFunction(Primitive left, Primitive right)
   {
      if (left == null)
      {
         left = NULL;
      }

      return left.getOrFunction(right);
   }
}
