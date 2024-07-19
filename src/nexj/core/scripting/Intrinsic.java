// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Serializable;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.security.auth.Subject;

import nexj.core.meta.BinaryFunction;
import nexj.core.meta.Primitive;
import nexj.core.meta.PrimitivePrivilege;
import nexj.core.meta.Type;
import nexj.core.meta.TypeMismatchException;
import nexj.core.meta.Typed;
import nexj.core.persistence.OID;
import nexj.core.persistence.OIDHolder;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Context;
import nexj.core.runtime.SecurityViolationException;
import nexj.core.scripting.Compiler.CallInstruction;
import nexj.core.scripting.Compiler.PCodeBuffer;
import nexj.core.scripting.match.ExpressionParser;
import nexj.core.scripting.match.MatchNode;
import nexj.core.scripting.syntax.SyntaxFunction;
import nexj.core.scripting.syntax.SyntaxTransformer;
import nexj.core.util.ArrayIterator;
import nexj.core.util.Binary;
import nexj.core.util.BinaryUtil;
import nexj.core.util.Ditto;
import nexj.core.util.EmptyIterator;
import nexj.core.util.GenericHashHolder;
import nexj.core.util.GenericHashTab;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Holder;
import nexj.core.util.IOUtil;
import nexj.core.util.IdentityHashHolder;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Iteratable;
import nexj.core.util.Logger;
import nexj.core.util.LoggerHolder;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.Sortable;
import nexj.core.util.StringId;
import nexj.core.util.StringUtil;
import nexj.core.util.SubstReader;
import nexj.core.util.SysUtil;
import nexj.core.util.TextPositionReader;
import nexj.core.util.URLUtil;

/**
 * Intrinsic Scheme functions.
 */
public class Intrinsic
{
   // constants

   /**
    * Coercion type 2d array: Primitive[nOrdinal1 * Primitive.MAX_COUNT + nOrdinal2].
    */
   protected final static Primitive[] COERCION_ARRAY = new Primitive[Primitive.MAX_COUNT * Primitive.MAX_COUNT];

   static
   {
      setCoercedType(COERCION_ARRAY, Primitive.STRING, Primitive.STRING, Primitive.STRING);
      setCoercedType(COERCION_ARRAY, Primitive.BINARY, Primitive.BINARY, Primitive.BINARY);
      setCoercedType(COERCION_ARRAY, Primitive.TIMESTAMP, Primitive.TIMESTAMP, Primitive.TIMESTAMP);
      setCoercedType(COERCION_ARRAY, Primitive.BOOLEAN, Primitive.BOOLEAN, Primitive.BOOLEAN);
      setCoercedType(COERCION_ARRAY, Primitive.ANY, Primitive.ANY, Primitive.ANY);

      Primitive[] nums = new Primitive[]{Primitive.INTEGER, Primitive.LONG,
         Primitive.DECIMAL, Primitive.FLOAT, Primitive.DOUBLE};

      for (int i = 0; i < nums.length; ++i)
      {
         for (int k = 0; k <= i; ++k)
         {
            setCoercedType(COERCION_ARRAY, nums[i], nums[k], nums[i]);
         }
      }
   }

   /**
    * Coercion type 2d array for the division operator.
    */
   protected final static Primitive[] DIV_COERCION_ARRAY = new Primitive[COERCION_ARRAY.length];

   static
   {
      System.arraycopy(COERCION_ARRAY, 0, DIV_COERCION_ARRAY, 0, DIV_COERCION_ARRAY.length);
      setCoercedType(DIV_COERCION_ARRAY, Primitive.INTEGER, Primitive.INTEGER, Primitive.DECIMAL);
      setCoercedType(DIV_COERCION_ARRAY, Primitive.INTEGER, Primitive.LONG, Primitive.DECIMAL);
      setCoercedType(DIV_COERCION_ARRAY, Primitive.LONG, Primitive.LONG, Primitive.DECIMAL);
   }

   /**
    * Maximum long value as a decimal number.
    */
   protected final static BigDecimal MAX_LONG_DECIMAL = BigDecimal.valueOf(Long.MAX_VALUE);

   /**
    * Minimum long value as a decimal number.
    */
   protected final static BigDecimal MIN_LONG_DECIMAL = BigDecimal.valueOf(Long.MIN_VALUE);

   /**
    * Maximum int value as a decimal number.
    */
   protected final static BigDecimal MAX_INT_DECIMAL = BigDecimal.valueOf(Integer.MAX_VALUE);

   /**
    * Minimum int value as a decimal number.
    */
   protected final static BigDecimal MIN_INT_DECIMAL = BigDecimal.valueOf(Integer.MIN_VALUE);

   /**
    * Empty array.
    */
   public final static Object[] EMPTY_ARRAY = new Object[0];

   /**
    * Empty byte array.
    */
   protected final static byte[] EMPTY_BYTEARRAY = new byte[0];

   /**
    * Pattern matching flags.
    * TODO: Change back to Pattern.UNICODE_CASE when Sun fixes the case
    * insensitivity bug, which is triggered in JDK 1.5 by this flag.
    */
   protected final static int PATTERN_FLAGS = 0;

   /**
    * Array of Unicode general category symbols, at indices corresponding
    * to the return values of {@link java.lang.Character#getType(char)}.
    */
   protected final static Symbol[] UNICODE_GENERAL_CATEGORIES = new Symbol[] {
      Symbol.UNICODE_CN, Symbol.UNICODE_LU, Symbol.UNICODE_LL, Symbol.UNICODE_LT, Symbol.UNICODE_LM,
      Symbol.UNICODE_LO, Symbol.UNICODE_MN, Symbol.UNICODE_ME, Symbol.UNICODE_MC, Symbol.UNICODE_ND,
      Symbol.UNICODE_NL, Symbol.UNICODE_NO, Symbol.UNICODE_ZS, Symbol.UNICODE_ZL, Symbol.UNICODE_ZP,
      Symbol.UNICODE_CC, Symbol.UNICODE_CF, null, Symbol.UNICODE_CO, Symbol.UNICODE_CS, Symbol.UNICODE_PD,
      Symbol.UNICODE_PS, Symbol.UNICODE_PE, Symbol.UNICODE_PC, Symbol.UNICODE_PO, Symbol.UNICODE_SM,
      Symbol.UNICODE_SC, Symbol.UNICODE_SK, Symbol.UNICODE_SO, Symbol.UNICODE_PI, Symbol.UNICODE_PF
   };

   public final static IntrinsicFunction ABS = new NumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(int n)
      {
         return Primitive.createInteger(Math.abs(n));
      }

      protected Object invoke(long l)
      {
         return Primitive.createLong(Math.abs(l));
      }

      protected Object invoke(BigDecimal dec)
      {
         return dec.abs();
      }

      protected Object invoke(float f)
      {
         return Primitive.createFloat(Math.abs(f));
      }

      protected Object invoke(double d)
      {
         return Primitive.createDouble(Math.abs(d));
      }

      protected Object invokeNull()
      {
         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.ABS;
      }
   };

   public final static IntrinsicFunction ACOS = new SimpleNumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Primitive.createDouble(Math.acos(((Number)value).doubleValue()));
      }

      public Symbol getSymbol()
      {
         return Symbol.ACOS;
      }
   };

   public final static IntrinsicFunction APPEND = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         Object tail;

         if (nArgCount == 0)
         {
            tail = null;
         }
         else
         {
            tail = machine.getArg(nArgCount - 1, nArgCount);

            for (int i = nArgCount - 2; i >= 0; --i)
            {
               Object pair = machine.getArg(i, nArgCount);
               Pair head = null;
               Pair last = null;

               while (pair != null)
               {
                  if (!(pair instanceof Pair))
                  {
                     throw new ScriptingException("err.scripting.badList",
                        new Object[]{getSymbol().getName()});
                  }

                  Pair p = (Pair)pair;
                  Pair q = new Pair(p.getHead());

                  if (head == null)
                  {
                     head = last = q;
                  }
                  else
                  {
                     last.setTail(q);
                     last = q;
                  }

                  pair = p.getTail();
               }

               if (last != null)
               {
                  last.setTail(tail);
                  tail = head;
               }
            }
         }

         machine.returnValue(tail, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.APPEND;
      }
   };

   public final static IntrinsicFunction APPEND_M = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         Object head = null;

         if (nArgCount > 0)
         {
            int i = 0;

            do
            {
               head = machine.getArg(i++, nArgCount);

               if (i == nArgCount)
               {
                  machine.returnValue(head, nArgCount);

                  return false;
               }
            }
            while (head == null);

            Pair current = (Pair)head;

            for (;;)
            {
               Object tail = current.getTail();

               if (tail == null)
               {
                  tail = machine.getArg(i++, nArgCount);

                  current.setTail(tail);

                  if (i == nArgCount)
                  {
                     break;
                  }
               }
               else if (tail instanceof Pair)
               {
                  current = (Pair)tail;
               }
               else
               {
                  throw new ScriptingException("err.scripting.badList",
                     new Object[]{getSymbol().getName()});
               }
            }
         }

         machine.returnValue(head, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.APPEND_M;
      }
   };

   public final static IntrinsicFunction APPLY = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyMinArgCount(nArgCount, 1);

         Object fun = machine.getArg(0, nArgCount);

         if (nArgCount == 1)
         {
            machine.pop(1);
            nArgCount = 0;
         }
         else
         {
            Object list = machine.pop();

            if (nArgCount == 2)
            {
               machine.pop();
               nArgCount = 0;
            }
            else
            {
               machine.shiftArgs(1, nArgCount - 1);
               nArgCount -= 2;
            }

            if (list != null)
            {
               if (list instanceof Pair)
               {
                  for (;;)
                  {
                     Pair pair = (Pair)list;
                     machine.push(pair.getHead());
                     ++nArgCount;
                     list = pair.getTail();

                     if (list == null)
                     {
                        break;
                     }

                     if (!(list instanceof Pair))
                     {
                        throw new ScriptingException("err.scripting.badList",
                           new Object[]{getSymbol().getName()});
                     }
                  }
               }
               else if (list instanceof Collection)
               {
                  Collection col = (Collection)list;

                  nArgCount += col.size();

                  for (Iterator itr = col.iterator(); itr.hasNext();)
                  {
                     machine.push(itr.next());
                  }
               }
               else if (list.getClass().isArray())
               {
                  int nCount = Array.getLength(list);

                  nArgCount += nCount;

                  for (int i = 0; i != nCount; ++i)
                  {
                     machine.push(Array.get(list, i));
                  }
               }
               else
               {
                  throw new TypeMismatchException(getSymbol());
               }
            }
         }

         machine.apply(fun, nArgCount);

         return true;
      }

      public boolean isPCode()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.APPLY;
      }
   };

   public final static IntrinsicFunction ASIN = new SimpleNumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Primitive.createDouble(Math.asin(((Number)value).doubleValue()));
      }

      public Symbol getSymbol()
      {
         return Symbol.ASIN;
      }
   };

   public final static IntrinsicFunction ASSOC = new ListMemberIntrinsicFunction()
   {
      protected boolean match(Object obj, Pair pair)
      {
         if (!(pair.getHead() instanceof Pair))
         {
            throw new ScriptingException("err.scripting.badList",
               new Object[]{getSymbol().getName()});
         }

         return equal(obj, ((Pair)pair.getHead()).getHead());
      }

      protected Object getResult(Pair pair)
      {
         return pair.getHead();
      }

      public Symbol getSymbol()
      {
         return Symbol.ASSOC;
      }
   };

   public final static IntrinsicFunction ASSQ = new ListMemberIntrinsicFunction()
   {
      protected boolean match(Object obj, Pair pair)
      {
         if (!(pair.getHead() instanceof Pair))
         {
            throw new ScriptingException("err.scripting.badList",
               new Object[]{getSymbol().getName()});
         }

         return eq(obj, ((Pair)pair.getHead()).getHead());
      }

      protected Object getResult(Pair pair)
      {
         return pair.getHead();
      }

      public Symbol getSymbol()
      {
         return Symbol.ASSQ;
      }
   };

   public final static IntrinsicFunction ASSV = new ListMemberIntrinsicFunction()
   {
      protected boolean match(Object obj, Pair pair)
      {
         if (!(pair.getHead() instanceof Pair))
         {
            throw new ScriptingException("err.scripting.badList",
               new Object[]{getSymbol().getName()});
         }

         return eqv(obj, ((Pair)pair.getHead()).getHead());
      }

      protected Object getResult(Pair pair)
      {
         return pair.getHead();
      }

      public Symbol getSymbol()
      {
         return Symbol.ASSV;
      }
   };

   public final static IntrinsicFunction ATAN = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1, 2);

         Object x = machine.getArg(0, nArgCount);

         if (x == null)
         {
            machine.returnValue(null, nArgCount);

            return false;
         }

         if (!(x instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         if (nArgCount == 2)
         {
            Object y = machine.getArg(1, nArgCount);

            if (y == null)
            {
               machine.returnValue(null, nArgCount);

               return false;
            }

            if (!(y instanceof Number))
            {
               throw new TypeMismatchException(getSymbol());
            }

            machine.returnValue(Primitive.createDouble(Math.atan2(((Number)x).doubleValue(), ((Number)y).doubleValue())), nArgCount);
         }
         else
         {
            machine.returnValue(Primitive.createDouble(Math.atan(((Number)x).doubleValue())), nArgCount);
         }

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.ATAN;
      }
   };

   public final static IntrinsicFunction BINARY_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof Binary);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.BINARY_P;
      }
   };

   public final static IntrinsicFunction BITWISE_AND = new IntegerBinaryJavaIntrinsicFunction()
   {
      protected Object getNoArgResult()
      {
         return Primitive.createInteger(-1);
      }

      protected BigInteger compute(BigInteger intLeft, BigInteger intRight)
      {
         return intLeft.and(intRight);
      }

      protected long compute(long lLeft, long lRight)
      {
         return lLeft & lRight;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_AND;
      }
   };

   public final static IntrinsicFunction BITWISE_ARITHMETIC_SHIFT = new BitwiseShiftIntegerJavaIntrinsicFunction()
   {
      protected BigInteger shift(BigInteger intValue, int nBitCount)
      {
         // Avoid an infinite loop in the JDK BigInteger.shiftRight()
         if (nBitCount == Integer.MIN_VALUE)
         {
            return intValue.shiftRight(1).shiftRight(Integer.MAX_VALUE);
         }

         return intValue.shiftLeft(nBitCount);
      }

      protected long shift(long lValue, int nBitCount)
      {
         if (nBitCount > 0)
         {
            lValue = shiftLeft(lValue, nBitCount);
         }
         else
         {
            if (nBitCount == Integer.MIN_VALUE)
            {
               return (lValue < 0) ? -1 : 0;
            }

            lValue = shiftRight(lValue, -nBitCount);
         }

         return lValue;
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_ARITHMETIC_SHIFT;
      }
   };

   public final static IntrinsicFunction BITWISE_ARITHMETIC_SHIFT_LEFT = new BitwiseShiftIntegerJavaIntrinsicFunction()
   {
      protected void verifyBitCount(int nBitCount)
      {
         if (nBitCount < 0)
         {
            throw new ScriptingException("err.scripting.negativeCount",
                  new Object[]{getSymbol().getName()});
         }
      }

      protected BigInteger shift(BigInteger intValue, int nBitCount)
      {
         return intValue.shiftLeft(nBitCount);
      }

      protected long shift(long lValue, int nBitCount)
      {
         return shiftLeft(lValue, nBitCount);
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_ARITHMETIC_SHIFT_LEFT;
      }
   };

   public final static IntrinsicFunction BITWISE_ARITHMETIC_SHIFT_RIGHT = new BitwiseShiftIntegerJavaIntrinsicFunction()
   {
      protected void verifyBitCount(int nBitCount)
      {
         if (nBitCount < 0)
         {
            throw new ScriptingException("err.scripting.negativeCount",
                  new Object[]{getSymbol().getName()});
         }
      }

      protected BigInteger shift(BigInteger intValue, int nBitCount)
      {
         return intValue.shiftRight(nBitCount);
      }

      protected long shift(long lValue, int nBitCount)
      {
         return shiftRight(lValue, nBitCount);
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_ARITHMETIC_SHIFT_RIGHT;
      }
   };

   public final static IntrinsicFunction BITWISE_BIT_COUNT = new BitwiseJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         int nCount;

         if (hasBigDecimalArg(nArgCount, machine))
         {
            BigInteger intValue = getBigIntegerArg(0, nArgCount, machine);

            nCount = intValue.signum() < 0 ? ~(intValue.not().bitCount()) : intValue.bitCount();
         }
         else
         {
            long lValue = getLongArg(0, nArgCount, machine);

            nCount = (lValue < 0) ? ~(Long.bitCount(~lValue)) : Long.bitCount(lValue);
         }

         machine.returnValue(Primitive.createInteger(nCount), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_BIT_COUNT;
      }
   };

   public final static IntrinsicFunction BITWISE_BIT_FIELD = new BitwiseBitFieldIntegerJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 3);

         int nStart = getIntegerArg(1, nArgCount, machine);
         int nEnd = getIntegerArg(2, nArgCount, machine);

         verifyIndexRange(nStart, nEnd);

         Object arg = machine.getArg(0, nArgCount);

         if (arg instanceof BigDecimal)
         {
            machine.returnValue(new BigDecimal(getBitField(getBigInteger(arg), nStart, nEnd)), nArgCount);
         }
         else
         {
            machine.returnValue(Primitive.createLong(getBitField(getLong(arg), nStart, nEnd)), nArgCount);
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_BIT_FIELD;
      }
   };

   public final static IntrinsicFunction BITWISE_BIT_SET_P = new BitwiseJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         boolean bBitSet;
         int nBitIndex = getIntegerArg(1, nArgCount, machine);

         verifyIndex(nBitIndex);

         if (hasBigDecimalArg(nArgCount, machine))
         {
            bBitSet = getBigIntegerArg(0, nArgCount, machine).testBit(nBitIndex);
         }
         else
         {
            long lValue = getLongArg(0, nArgCount, machine);

            if (nBitIndex > 63)
            {
               bBitSet = lValue < 0;
            }
            else
            {
               bBitSet = (lValue & shiftLeft(1, nBitIndex)) != 0;
            }
         }

         machine.returnValue(Boolean.valueOf(bBitSet), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_BIT_SET_P;
      }
   };

   public final static IntrinsicFunction BITWISE_COPY_BIT = new BitwiseJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 3);

         int nBitIndex = getIntegerArg(1, nArgCount, machine);

         verifyIndex(nBitIndex);

         int nBitValue = getIntegerArg(2, nArgCount, machine);

         if (nBitValue != 0 && nBitValue != 1)
         {
            throw new ScriptingException("err.scripting.invalidArg",
                  new Object[]{getSymbol().getName(), new StringId("ids.zeroOrOne"),
                  Primitive.createInteger(nBitValue)});
         }

         Object arg = machine.getArg(0, nArgCount);

         if (arg instanceof BigDecimal)
         {
            BigInteger intValue = getBigInteger(arg);

            intValue = (nBitValue == 1) ? intValue.setBit(nBitIndex) : intValue.clearBit(nBitIndex);

            machine.returnValue(new BigDecimal(intValue), nArgCount);
         }
         else
         {
            long lValue = getLong(arg);

            if (nBitValue == 1)
            {
               lValue |= shiftLeft(1, nBitIndex);
            }
            else
            {
               lValue &= ~shiftLeft(1, nBitIndex);
            }

            machine.returnValue(Primitive.createLong(lValue), nArgCount);
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_COPY_BIT;
      }
   };

   public final static IntrinsicFunction BITWISE_COPY_BIT_FIELD = new BitwiseBitFieldIntegerJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 4);

         int nStart = getIntegerArg(1, nArgCount, machine);
         int nEnd = getIntegerArg(2, nArgCount, machine);

         verifyIndexRange(nStart, nEnd);

         Object dest = machine.getArg(0, nArgCount);
         Object source = machine.getArg(3, nArgCount);

         if (dest instanceof BigDecimal || source instanceof BigDecimal)
         {
            BigInteger intDest = getBigInteger(dest);
            BigInteger intSource = getBigInteger(source);

            machine.returnValue(new BigDecimal(copyBitField(intDest, nStart, nEnd, intSource)), nArgCount);
         }
         else
         {
            long lDest = getLong(dest);
            long lSource = getLong(source);

            machine.returnValue(Primitive.createLong(copyBitField(lDest, nStart, nEnd, lSource)), nArgCount);
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_COPY_BIT_FIELD;
      }
   };

   public final static IntrinsicFunction BITWISE_FIRST_BIT_SET = new BitwiseJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         int nIndex = -1;

         if (hasBigDecimalArg(nArgCount, machine))
         {
            nIndex = getBigIntegerArg(0, nArgCount, machine).getLowestSetBit();
         }
         else
         {
            long lValue = getLongArg(0, nArgCount, machine);

            if (lValue != 0)
            {
               nIndex = Long.numberOfTrailingZeros(lValue);
            }
         }

         machine.returnValue(Primitive.createInteger(nIndex), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_FIRST_BIT_SET;
      }
   };

   public final static IntrinsicFunction BITWISE_IF = new BitwiseJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 3);

         if (hasBigDecimalArg(nArgCount, machine))
         {
            BigInteger intCondValue = getBigIntegerArg(0, nArgCount, machine);
            BigInteger intThenValue = getBigIntegerArg(1, nArgCount, machine);
            BigInteger intElseValue = getBigIntegerArg(2, nArgCount, machine);

            machine.returnValue(new BigDecimal(intCondValue.and(intThenValue).or(intElseValue.andNot(intCondValue))), nArgCount);
         }
         else
         {
            long lCondValue = getLongArg(0, nArgCount, machine);
            long lThenValue = getLongArg(1, nArgCount, machine);
            long lElseValue = getLongArg(2, nArgCount, machine);

            machine.returnValue(Primitive.createLong((lCondValue & lThenValue) | (~lCondValue & lElseValue)), nArgCount);
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_IF;
      }
   };

   public final static IntrinsicFunction BITWISE_IOR = new IntegerBinaryJavaIntrinsicFunction()
   {
      protected Object getNoArgResult()
      {
         return Primitive.ZERO_INTEGER;
      }

      protected BigInteger compute(BigInteger intLeft, BigInteger intRight)
      {
         return intLeft.or(intRight);
      }

      protected long compute(long lLeft, long lRight)
      {
         return lLeft | lRight;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_IOR;
      }
   };

   public final static IntrinsicFunction BITWISE_LENGTH = new BitwiseJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         int nBitCount = 0;

         if (hasBigDecimalArg(nArgCount, machine))
         {
            nBitCount = getBigIntegerArg(0, nArgCount, machine).bitLength();
         }
         else
         {
            long lValue = getLongArg(0, nArgCount, machine);

            if (lValue < 0)
            {
               lValue = ~lValue;
            }

            nBitCount = Long.SIZE - Long.numberOfLeadingZeros(lValue);
         }

         machine.returnValue(Primitive.createInteger(nBitCount), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_LENGTH;
      }
   };

   public final static IntrinsicFunction BITWISE_NOT = new BitwiseJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         if (hasBigDecimalArg(nArgCount, machine))
         {
            machine.returnValue(new BigDecimal(getBigIntegerArg(0, nArgCount, machine).not()),
               nArgCount);
         }
         else
         {
            machine.returnValue(Primitive.createLong(~getLongArg(0, nArgCount, machine)),
               nArgCount);
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_NOT;
      }
   };

   public final static IntrinsicFunction BITWISE_REVERSE_BIT_FIELD = new BitwiseBitFieldIntegerJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 3);

         int nStart = getIntegerArg(1, nArgCount, machine);
         int nEnd = getIntegerArg(2, nArgCount, machine);

         verifyIndexRange(nStart, nEnd);

         Object arg = machine.getArg(0, nArgCount);

         if (arg instanceof BigDecimal)
         {
            BigInteger intValue = getBigInteger(arg);
            BigInteger intBitField = getBitField(intValue, nStart, nEnd);
            BigInteger intReversed = reverseBitField(intBitField, nEnd - nStart);

            intValue = copyBitField(intValue, nStart, nEnd, intReversed);
            machine.returnValue(new BigDecimal(intValue), nArgCount);
         }
         else
         {
            long lValue = getLong(arg);
            long lBitField = getBitField(lValue, nStart, nEnd);
            long lReversed = reverseBitField(lBitField, nEnd - nStart);

            lValue = copyBitField(lValue, nStart, nEnd, lReversed);
            machine.returnValue(Primitive.createLong(lValue), nArgCount);
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_REVERSE_BIT_FIELD;
      }
   };

   public final static IntrinsicFunction BITWISE_ROTATE_BIT_FIELD = new BitwiseBitFieldIntegerJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 4);

         int nStart = getIntegerArg(1, nArgCount, machine);
         int nEnd = getIntegerArg(2, nArgCount, machine);

         verifyIndexRange(nStart, nEnd);

         int nWidth = nEnd - nStart;
         int nCount = getIntegerArg(3, nArgCount, machine);

         if (nCount < 0)
         {
            throw new ScriptingException("err.scripting.negativeCount",
                  new Object[]{getSymbol().getName()});
         }

         nCount %= nWidth;

         Object arg = machine.getArg(0, nArgCount);

         if (nCount != 0)
         {
            if (arg instanceof BigDecimal)
            {
               BigInteger intValue = getBigInteger(arg);
               BigInteger intBitField = getBitField(intValue, nStart, nEnd);

               if (intBitField.signum() == 0)
               {
                  machine.returnValue(arg, nArgCount);
               }
               else
               {
                  BigInteger intRotated = intBitField.shiftLeft(nCount).or(intBitField.shiftRight(nWidth - nCount));

                  intValue = copyBitField(intValue, nStart, nEnd, intRotated);
                  machine.returnValue(new BigDecimal(intValue), nArgCount);
               }
            }
            else
            {
               long lValue = getLong(arg);
               long lBitField = getBitField(lValue, nStart, nEnd);
               long lRotated = shiftLeft(lBitField, nCount) | shiftRight(lBitField, (nWidth - nCount));

               lValue = copyBitField(lValue, nStart, nEnd, lRotated);
               machine.returnValue(Primitive.createLong(lValue), nArgCount);
            }
         }
         else
         {
            machine.returnValue(arg, nArgCount);
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_ROTATE_BIT_FIELD;
      }
   };

   public final static IntrinsicFunction BITWISE_XOR = new IntegerBinaryJavaIntrinsicFunction()
   {
      protected Object getNoArgResult()
      {
         return Primitive.ZERO_INTEGER;
      }

      protected BigInteger compute(BigInteger intLeft, BigInteger intRight)
      {
         return intLeft.xor(intRight);
      }

      protected long compute(long lLeft, long lRight)
      {
         return lLeft ^ lRight;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.BITWISE_XOR;
      }
   };

   public final static IntrinsicFunction BOOLEAN_EQ_P = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyMinArgCount(nArgCount, 2);

         boolean bValue = isTrue(machine.getArg(0, nArgCount));
         Boolean equal = Boolean.TRUE;

         for (int i = 1; i < nArgCount; i++)
         {
            if (bValue != isTrue(machine.getArg(i, nArgCount)))
            {
               equal = Boolean.FALSE;
               break;
            }
         }

         machine.returnValue(equal, nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.BOOLEAN_EQ_P;
      }
   };

   public final static IntrinsicFunction BOOLEAN_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof Boolean);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.BOOLEAN_P;
      }
   };

   public final static IntrinsicFunction BOUND_IDENTIFIER_EQ_P = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object left = machine.getArg(0, nArgCount);
         Object right = machine.getArg(1, nArgCount);

         if (!(left instanceof Symbol) || !(right instanceof Symbol))
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(
            Boolean.valueOf(machine.getTransformerContext().compareSymbols((Symbol)left, (Symbol)right)),
            nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BOUND_IDENTIFIER_EQ_P;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR = new ByteVectorJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         if (nArgCount == 0)
         {
            machine.returnValue(EMPTY_BYTEARRAY, nArgCount);
         }
         else
         {
            byte[] nArray = new byte[nArgCount];

            for (int i = 0; i < nArgCount; ++i)
            {
               nArray[i] = (byte)getExactIntArg(i, nArgCount, machine);
            }

            machine.returnValue(nArray, nArgCount);
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_APPEND = new ByteVectorJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         if (nArgCount == 0)
         {
            machine.returnValue(EMPTY_BYTEARRAY, nArgCount);

            return false;
         }

         int nCount = 0;

         for (int i = 0; i != nArgCount; ++i)
         {
            Object arg = machine.getArg(i, nArgCount);

            if (arg instanceof byte[])
            {
               nCount += ((byte[])arg).length;
            }
            else if (arg != null)
            {
               throw new TypeMismatchException(getSymbol());
            }
         }

         byte[] nDstArray = new byte[nCount];
         int nOfs = 0;

         for (int i = 0; i != nArgCount; ++i)
         {
            byte[] nSrcArray = (byte[])machine.getArg(i, nArgCount);

            if (nSrcArray != null)
            {
               int nLength = nSrcArray.length;

               System.arraycopy(nSrcArray, 0, nDstArray, nOfs, nLength);
               nOfs += nLength;
            }
         }

         machine.returnValue(nDstArray, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_APPEND;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_COPY = new ByteVectorJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         byte[] nSrcArray = getByteVectorArg(0, nArgCount, machine);
         int nLength = nSrcArray.length;

         if (nLength == 0)
         {
            machine.returnValue(EMPTY_BYTEARRAY, nArgCount);
         }
         else
         {
            byte[] nDstArray = new byte[nLength];
   
            System.arraycopy(nSrcArray, 0, nDstArray, 0, nLength);
   
            machine.returnValue(nDstArray, nArgCount);
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_COPY;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_COPY_M = new ByteVectorJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 5);

         machine.returnValue(
            copy(
               getByteVectorArg(0, nArgCount, machine),
               getExactIntArg(1, nArgCount, machine),
               getByteVectorArg(2, nArgCount, machine),
               getExactIntArg(3, nArgCount, machine),
               getExactIntArg(4, nArgCount, machine)),
            nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_COPY_M;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_EQ_P = new ByteVectorJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         machine.returnValue(
            Boolean.valueOf(Arrays.equals(getByteVectorArg(0, nArgCount, machine), getByteVectorArg(1, nArgCount, machine))),
            nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_EQ_P;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_FILL = new ByteVectorJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         byte[] nArray = getByteVectorArg(0, nArgCount, machine);

         Arrays.fill(nArray, (byte)getExactIntArg(1, nArgCount, machine));
         machine.returnValue(nArray, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_FILL;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_IEEE_DOUBLE_NATIVE_REF = new ByteVectorNativeRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeRefJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 8;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeRefJavaIntrinsicFunction#getNumber(byte[], int)
       */
      protected Number getNumber(byte[] nArray, int nIndex)
      {
         return Primitive.createDouble(Double.longBitsToDouble(BinaryUtil.getLong(nArray, nIndex, getByteCount(), true, true)));
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_IEEE_DOUBLE_NATIVE_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_IEEE_DOUBLE_NATIVE_SET = new ByteVectorNativeSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 8;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeSetJavaIntrinsicFunction#setNumber(byte[], int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, Number value)
      {
         BinaryUtil.setLong(nArray, nIndex, getByteCount(), Double.doubleToLongBits(value.doubleValue()), true);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_IEEE_DOUBLE_NATIVE_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_IEEE_DOUBLE_REF = new ByteVectorRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getByteCount()
       */
      public int getByteCount()
      {
         return 8;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getNumber(byte[], int, int, boolean)
       */
      public Number getNumber(byte[] nArray, int nIndex, int nByteCount, boolean bBigEndian)
      {
         return Primitive.createDouble(Double.longBitsToDouble(BinaryUtil.getLong(nArray, nIndex, nByteCount, bBigEndian, true)));
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_IEEE_DOUBLE_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_IEEE_DOUBLE_SET = new ByteVectorSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 8;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#setNumberBigEndian(byte[], int, int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, int nByteCount, Number value, boolean bBigEndian)
      {
         BinaryUtil.setLong(nArray, nIndex, nByteCount, Double.doubleToLongBits(value.doubleValue()), bBigEndian);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_IEEE_DOUBLE_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_IEEE_SINGLE_NATIVE_REF = new ByteVectorNativeRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeRefJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 4;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeRefJavaIntrinsicFunction#getNumber(byte[], int)
       */
      protected Number getNumber(byte[] nArray, int nIndex)
      {
         return Primitive.createFloat(Float.intBitsToFloat(BinaryUtil.getInt(nArray, nIndex, getByteCount(), true, true)));
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_IEEE_SINGLE_NATIVE_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_IEEE_SINGLE_NATIVE_SET = new ByteVectorNativeSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 4;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeSetJavaIntrinsicFunction#setNumber(byte[], int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, Number value)
      {
         BinaryUtil.setInt(nArray, nIndex, getByteCount(), Float.floatToIntBits(value.floatValue()), true);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_IEEE_SINGLE_NATIVE_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_IEEE_SINGLE_REF = new ByteVectorRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getByteCount()
       */
      public int getByteCount()
      {
         return 4;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getNumber(byte[], int, int, boolean)
       */
      public Number getNumber(byte[] nArray, int nIndex, int nByteCount, boolean bBigEndian)
      {
         return Primitive.createFloat(Float.intBitsToFloat(BinaryUtil.getInt(nArray, nIndex, nByteCount, bBigEndian, true)));
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_IEEE_SINGLE_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_IEEE_SINGLE_SET = new ByteVectorSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 4;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#setNumberBigEndian(byte[], int, int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, int nByteCount, Number value, boolean bBigEndian)
      {
         BinaryUtil.setInt(nArray, nIndex, nByteCount, Float.floatToIntBits(value.floatValue()), bBigEndian);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_IEEE_SINGLE_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_LENGTH = new ByteVectorJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         machine.returnValue(Primitive.createInteger(getByteArray(0, nArgCount, machine).length), nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_LENGTH;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object obj)
      {
         return Boolean.valueOf(obj instanceof byte[]);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_P;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_S16_NATIVE_REF = new ByteVectorNativeRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeRefJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 2;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeRefJavaIntrinsicFunction#getNumber(byte[], int)
       */
      protected Number getNumber(byte[] nArray, int nIndex)
      {
         return Primitive.createInteger(BinaryUtil.getInt(nArray, nIndex, getByteCount(), true, true));
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_S16_NATIVE_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_S16_NATIVE_SET = new ByteVectorNativeSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 2;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeSetJavaIntrinsicFunction#setNumber(byte[], int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, Number value)
      {
         BinaryUtil.setInt(nArray, nIndex, getByteCount(), value.intValue(), true);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_S16_NATIVE_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_S16_REF = new ByteVectorRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 2;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getNumber(byte[], int, int, boolean)
       */
      protected Number getNumber(byte[] nArray, int nIndex, int nByteCount, boolean bBigEndian)
      {
         return Primitive.createInteger(BinaryUtil.getInt(nArray, nIndex, nByteCount, bBigEndian, true));
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_S16_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_S16_SET = new ByteVectorSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 2;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#setNumberBigEndian(byte[], int, int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, int nByteCount, Number value, boolean bBigEndian)
      {
         BinaryUtil.setInt(nArray, nIndex, nByteCount, value.intValue(), bBigEndian);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_S16_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_S32_NATIVE_REF = new ByteVectorNativeRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeRefJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 4;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeRefJavaIntrinsicFunction#getNumber(byte[], int)
       */
      protected Number getNumber(byte[] nArray, int nIndex)
      {
         return Primitive.createInteger(BinaryUtil.getInt(nArray, nIndex, getByteCount(), true, true));
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_S32_NATIVE_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_S32_NATIVE_SET = new ByteVectorNativeSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 4;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeSetJavaIntrinsicFunction#setNumber(byte[], int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, Number value)
      {
         BinaryUtil.setInt(nArray, nIndex, getByteCount(), value.intValue(), true);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_S32_NATIVE_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_S32_REF = new ByteVectorRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 4;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getNumber(byte[], int, int, boolean)
       */
      protected Number getNumber(byte[] nArray, int nIndex, int nByteCount, boolean bBigEndian)
      {
         return Primitive.createInteger(BinaryUtil.getInt(nArray, nIndex, nByteCount, bBigEndian, true));
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_S32_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_S32_SET = new ByteVectorSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 4;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#setNumberBigEndian(byte[], int, int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, int nByteCount, Number value, boolean bBigEndian)
      {
         BinaryUtil.setInt(nArray, nIndex, nByteCount, value.intValue(), bBigEndian);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_S32_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_S64_NATIVE_REF = new ByteVectorNativeRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeRefJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 8;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeRefJavaIntrinsicFunction#getNumber(byte[], int)
       */
      protected Number getNumber(byte[] nArray, int nIndex)
      {
         return Primitive.createLong(BinaryUtil.getLong(nArray, nIndex, getByteCount(), true, true));
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_S64_NATIVE_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_S64_NATIVE_SET = new ByteVectorNativeSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 8;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeSetJavaIntrinsicFunction#setNumber(byte[], int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, Number value)
      {
         BinaryUtil.setLong(nArray, nIndex, getByteCount(), value.longValue(), true);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_S64_NATIVE_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_S64_REF = new ByteVectorRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 8;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getNumber(byte[], int, int, boolean)
       */
      protected Number getNumber(byte[] nArray, int nIndex, int nByteCount, boolean bBigEndian)
      {
         return Primitive.createLong(BinaryUtil.getLong(nArray, nIndex, nByteCount, bBigEndian, true));
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_S64_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_S64_SET = new ByteVectorSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 8;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#setNumberBigEndian(byte[], int, int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, int nByteCount, Number value, boolean bBigEndian)
      {
         BinaryUtil.setLong(nArray, nIndex, nByteCount, value.longValue(), bBigEndian);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_S64_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_S8_REF = new ByteVectorJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         machine.returnValue(
            Primitive.createInteger(getByte(getByteArray(0, nArgCount, machine), getExactIntArg(1, nArgCount, machine))),
            nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_S8_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_S8_SET = new ByteVectorJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 3);

			Object value = machine.getArg(2, nArgCount);

         if (!(value instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         setByte(getByteVectorArg(0, nArgCount, machine),
            getExactIntArg(1, nArgCount, machine),
            ((Number)value).byteValue());

         machine.returnValue(value, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_S8_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_SINT_LIST = new ByteVectorListConversionJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorListConversionJavaIntrinsicFunction#isSigned()
       */
      protected boolean isSigned()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_SINT_LIST;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_SINT_REF = new ByteVectorRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getReqCount()
       */
      protected int getReqCount()
      {
         return 4;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 0;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getNumber(byte[], int, int, boolean)
       */
      protected Number getNumber(byte[] nArray, int nIndex, int nByteCount, boolean bBigEndian)
      {
         return BinaryUtil.getNumber(nArray, nIndex, nByteCount, bBigEndian, true);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_SINT_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_SINT_SET = new ByteVectorSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#getReqCount()
       */
      protected int getReqCount()
      {
         return 5;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 0;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#setNumberBigEndian(byte[], int, int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, int nByteCount, Number value, boolean bBigEndian)
      {
         if (value instanceof BigDecimal && nByteCount > (Long.SIZE >> 3))
         {
            BinaryUtil.setBigInteger(nArray, nIndex, nByteCount, ((BigDecimal)value).toBigInteger(), bBigEndian, false);
         }
         else
         {
            BinaryUtil.setLong(nArray, nIndex, nByteCount, value.longValue(), bBigEndian);
         }
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_SINT_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_U16_NATIVE_REF = new ByteVectorNativeRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeRefJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 2;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeRefJavaIntrinsicFunction#getNumber(byte[], int)
       */
      protected Number getNumber(byte[] nArray, int nIndex)
      {
         return Primitive.createInteger(BinaryUtil.getInt(nArray, nIndex, getByteCount(), true, false));
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_U16_NATIVE_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_U16_NATIVE_SET = new ByteVectorNativeSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 2;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeSetJavaIntrinsicFunction#setNumber(byte[], int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, Number value)
      {
         BinaryUtil.setInt(nArray, nIndex, getByteCount(), value.intValue(), true);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_U16_NATIVE_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_U16_REF = new ByteVectorRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 2;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getNumber(byte[], int, int, boolean)
       */
      protected Number getNumber(byte[] nArray, int nIndex, int nByteCount, boolean bBigEndian)
      {
         return Primitive.createInteger(BinaryUtil.getInt(nArray, nIndex, nByteCount, bBigEndian, false));
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_U16_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_U16_SET = new ByteVectorSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 2;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#setNumberBigEndian(byte[], int, int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, int nByteCount, Number value, boolean bBigEndian)
      {
         BinaryUtil.setInt(nArray, nIndex, nByteCount, value.intValue(), bBigEndian);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_U16_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_U32_NATIVE_REF = new ByteVectorNativeRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeRefJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 4;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeRefJavaIntrinsicFunction#getNumber(byte[], int)
       */
      protected Number getNumber(byte[] nArray, int nIndex)
      {
         return Primitive.createLong(BinaryUtil.getLong(nArray, nIndex, getByteCount(), true, false));
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_U32_NATIVE_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_U32_NATIVE_SET = new ByteVectorNativeSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 4;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeSetJavaIntrinsicFunction#setNumber(byte[], int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, Number value)
      {
         BinaryUtil.setLong(nArray, nIndex, getByteCount(), value.longValue(), true);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_U32_NATIVE_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_U32_REF = new ByteVectorRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 4;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getNumber(byte[], int, int, boolean)
       */
      protected Number getNumber(byte[] nArray, int nIndex, int nByteCount, boolean bBigEndian)
      {
         return Primitive.createLong(BinaryUtil.getLong(nArray, nIndex, nByteCount, bBigEndian, false));
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_U32_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_U32_SET = new ByteVectorSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 4;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#setNumberBigEndian(byte[], int, int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, int nByteCount, Number value, boolean bBigEndian)
      {
         BinaryUtil.setLong(nArray, nIndex, nByteCount, value.longValue(), bBigEndian);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_U32_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_U64_NATIVE_REF = new ByteVectorNativeRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeRefJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 8;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeRefJavaIntrinsicFunction#getNumber(byte[], int)
       */
      protected Number getNumber(byte[] nArray, int nIndex)
      {
         return new BigDecimal(BinaryUtil.getBigInteger(nArray, nIndex, getByteCount(), true, false));
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_U64_NATIVE_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_U64_NATIVE_SET = new ByteVectorNativeSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 8;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorNativeSetJavaIntrinsicFunction#setNumber(byte[], int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, Number value)
      {
         BinaryUtil.setLong(nArray, nIndex, getByteCount(), value.longValue(), true);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_U64_NATIVE_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_U64_REF = new ByteVectorRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 8;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getNumber(byte[], int, int, boolean)
       */
      protected Number getNumber(byte[] nArray, int nIndex, int nByteCount, boolean bBigEndian)
      {
         return new BigDecimal(BinaryUtil.getBigInteger(nArray, nIndex, nByteCount, bBigEndian, false));
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_U64_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_U64_SET = new ByteVectorSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 8;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#setNumberBigEndian(byte[], int, int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, int nByteCount, Number value, boolean bBigEndian)
      {
         BinaryUtil.setLong(nArray, nIndex, nByteCount, value.longValue(), bBigEndian);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_U64_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_U8_LIST = new ByteVectorJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         byte[] nArray = getByteArray(0, nArgCount, machine);
         Pair tail = null;

         for (int i = nArray.length - 1; i > -1; i--)
         {
            tail = new Pair(Primitive.createInteger(nArray[i] & 0xFF), tail);
         }

         machine.returnValue(tail, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_U8_LIST;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_U8_REF = new ByteVectorJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         machine.returnValue(
            Primitive.createInteger(getByte(getByteArray(0, nArgCount, machine), getExactIntArg(1, nArgCount, machine)) & 0xFF),
            nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_U8_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_U8_SET = new ByteVectorJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 3);

         Object value = machine.getArg(2, nArgCount);

         if (!(value instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         setByte(getByteVectorArg(0, nArgCount, machine),
            getExactIntArg(1, nArgCount, machine),
            ((Number)value).byteValue());

         machine.returnValue(value, nArgCount);
         
         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_U8_SET;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_UINT_LIST = new ByteVectorListConversionJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorListConversionJavaIntrinsicFunction#isSigned()
       */
      protected boolean isSigned()
      {
         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_UINT_LIST;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_UINT_REF = new ByteVectorRefJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getReqCount()
       */
      protected int getReqCount()
      {
         return 4;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 0;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorRefJavaIntrinsicFunction#getNumber(byte[], int, int, boolean)
       */
      protected Number getNumber(byte[] nArray, int nIndex, int nByteCount, boolean bBigEndian)
      {
         return BinaryUtil.getNumber(nArray, nIndex, nByteCount, bBigEndian, false);
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_UINT_REF;
      }
   };

   public final static IntrinsicFunction BYTEVECTOR_UINT_SET = new ByteVectorSetJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#getReqCount()
       */
      protected int getReqCount()
      {
         return 5;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#getByteCount()
       */
      protected int getByteCount()
      {
         return 0;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.ByteVectorSetJavaIntrinsicFunction#setNumberBigEndian(byte[], int, int, java.lang.Number)
       */
      protected void setNumber(byte[] nArray, int nIndex, int nByteCount, Number value, boolean bBigEndian)
      {
         int nBitCount = nByteCount << 3;

         if (nBitCount > Long.SIZE && value instanceof BigDecimal)
         {
            BinaryUtil.setBigInteger(nArray, nIndex, nByteCount,
               ((BigDecimal)value).toBigInteger().andNot(BigInteger.valueOf(-1).shiftLeft(nBitCount)), bBigEndian, false);
         }
         else
         {
            BinaryUtil.setLong(nArray, nIndex, nByteCount, value.longValue(), bBigEndian);
         }
      }

      public Symbol getSymbol()
      {
         return Symbol.BYTEVECTOR_UINT_SET;
      }
   };

   public final static IntrinsicFunction CAAAAR = new CxrIntrinsicFunction(Symbol.CAAAAR, 0x10);

   public final static IntrinsicFunction CAAADR = new CxrIntrinsicFunction(Symbol.CAAADR, 0x11);

   public final static IntrinsicFunction CAAAR = new CxrIntrinsicFunction(Symbol.CAAAR, 0x8);

   public final static IntrinsicFunction CAADAR = new CxrIntrinsicFunction(Symbol.CAADAR, 0x12);

   public final static IntrinsicFunction CAADDR = new CxrIntrinsicFunction(Symbol.CAADDR, 0x13);

   public final static IntrinsicFunction CAADR = new CxrIntrinsicFunction(Symbol.CAADR, 0x9);

   public final static IntrinsicFunction CAAR = new CxrIntrinsicFunction(Symbol.CAAR, 0x4);

   public final static IntrinsicFunction CADAAR = new CxrIntrinsicFunction(Symbol.CADAAR, 0x14);

   public final static IntrinsicFunction CADADR = new CxrIntrinsicFunction(Symbol.CADADR, 0x15);

   public final static IntrinsicFunction CADAR = new CxrIntrinsicFunction(Symbol.CADAR, 0xA);

   public final static IntrinsicFunction CADDAR = new CxrIntrinsicFunction(Symbol.CADDAR, 0x16);

   public final static IntrinsicFunction CADDDR = new CxrIntrinsicFunction(Symbol.CADDDR, 0x17);

   public final static IntrinsicFunction CADDR = new CxrIntrinsicFunction(Symbol.CADDR, 0xB);

   public final static IntrinsicFunction CADR = new CxrIntrinsicFunction(Symbol.CADR, 0x5);

   public final static IntrinsicFunction CALL_AS_JAAS_SUBJECT = new CallAsJAASSubjectIntrinsicFunction(Symbol.CALL_AS_JAAS_SUBJECT);

   public final static IntrinsicFunction CALL_CC = new CallCCIntrinsicFunction(Symbol.CALL_CC);

   public final static IntrinsicFunction CALL_WITH_CURRENT_CONTINUATION =  new CallCCIntrinsicFunction(Symbol.CALL_WITH_CURRENT_CONTINUATION);

   public final static IntrinsicFunction CALL_WITH_VALUES = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object producer = machine.getArg(0, nArgCount);
         Object consumer = machine.getArg(1, nArgCount);

         machine.pop(2);
         machine.pushValuesConsumer(consumer);
         machine.apply(producer, 0);

         return true;
      }

      public boolean isPCode()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.CALL_WITH_VALUES;
      }
   };

   public final static IntrinsicFunction CAR = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         if (value instanceof Pair)
         {
            return ((Pair)value).getHead();
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.CAR;
      }

      /**
       * @see nexj.core.scripting.IntrinsicFunction#generate(nexj.core.scripting.Compiler.PCodeBuffer, nexj.core.scripting.Compiler.CallInstruction)
       */
      public boolean generate(Compiler.PCodeBuffer buf, Compiler.CallInstruction instr)
      {
         if (instr.getArgCount() == 1)
         {
            buf.addCode(Machine.CAR);

            return true;
         }

         return false;
      }
   };

   public final static IntrinsicFunction CDAAAR = new CxrIntrinsicFunction(Symbol.CDAAAR, 0x18);

   public final static IntrinsicFunction CDAADR = new CxrIntrinsicFunction(Symbol.CDAADR, 0x19);

   public final static IntrinsicFunction CDAAR = new CxrIntrinsicFunction(Symbol.CDAAR, 0xC);

   public final static IntrinsicFunction CDADAR = new CxrIntrinsicFunction(Symbol.CDADAR, 0x1A);

   public final static IntrinsicFunction CDADDR = new CxrIntrinsicFunction(Symbol.CDADDR, 0x1B);

   public final static IntrinsicFunction CDADR = new CxrIntrinsicFunction(Symbol.CDADR, 0xD);

   public final static IntrinsicFunction CDAR = new CxrIntrinsicFunction(Symbol.CDAR, 0x6);

   public final static IntrinsicFunction CDDAAR = new CxrIntrinsicFunction(Symbol.CDDAAR, 0x1C);

   public final static IntrinsicFunction CDDADR = new CxrIntrinsicFunction(Symbol.CDDADR, 0x1D);

   public final static IntrinsicFunction CDDAR = new CxrIntrinsicFunction(Symbol.CDDAR, 0xE);

   public final static IntrinsicFunction CDDDAR = new CxrIntrinsicFunction(Symbol.CDDDAR, 0x1E);

   public final static IntrinsicFunction CDDDDR = new CxrIntrinsicFunction(Symbol.CDDDDR, 0x1F);

   public final static IntrinsicFunction CDDDR = new CxrIntrinsicFunction(Symbol.CDDDR, 0xF);

   public final static IntrinsicFunction CDDR = new CxrIntrinsicFunction(Symbol.CDDR, 0x7);

   public final static IntrinsicFunction CDR = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         if (value instanceof Pair)
         {
            return ((Pair)value).getTail();
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.CDR;
      }

      /**
       * @see nexj.core.scripting.IntrinsicFunction#generate(nexj.core.scripting.Compiler.PCodeBuffer, nexj.core.scripting.Compiler.CallInstruction)
       */
      public boolean generate(Compiler.PCodeBuffer buf, Compiler.CallInstruction instr)
      {
         if (instr.getArgCount() == 1)
         {
            buf.addCode(Machine.CDR);

            return true;
         }

         return false;
      }
   };

   public final static IntrinsicFunction CEILING = new NumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(int n)
      {
         return Primitive.createInteger(n);
      }

      protected Object invoke(long l)
      {
         return Primitive.createLong(l);
      }

      protected Object invoke(BigDecimal dec)
      {
         return dec.setScale(0, BigDecimal.ROUND_CEILING);
      }

      protected Object invoke(float f)
      {
         return Primitive.createFloat((float)Math.ceil(f));
      }

      protected Object invoke(double d)
      {
         return Primitive.createDouble(Math.ceil(d));
      }

      protected Object invokeNull()
      {
         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.CEILING;
      }
   };

   public final static IntrinsicFunction CHAR_ALPHABETIC_P = new CharacterUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(char ch)
      {
         return Boolean.valueOf(Character.isLetter(ch));
      }

      protected Object invokeNull()
      {
         return Boolean.FALSE;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_ALPHABETIC_P;
      }
   };

   public final static IntrinsicFunction CHAR_CI_EQ_P = new CharacterComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(char chLeft, char chRight)
      {
         return Character.toUpperCase(chLeft) == Character.toUpperCase(chRight);
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_CI_EQ_P;
      }
   };

   public final static IntrinsicFunction CHAR_CI_GE_P = new CharacterComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(char chLeft, char chRight)
      {
         return Character.toUpperCase(chLeft) >= Character.toUpperCase(chRight);
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_CI_GE_P;
      }
   };

   public final static IntrinsicFunction CHAR_CI_GT_P = new CharacterComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(char chLeft, char chRight)
      {
         return Character.toUpperCase(chLeft) > Character.toUpperCase(chRight);
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_CI_GT_P;
      }
   };

   public final static IntrinsicFunction CHAR_CI_LE_P = new CharacterComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(char chLeft, char chRight)
      {
         return Character.toUpperCase(chLeft) <= Character.toUpperCase(chRight);
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_CI_LE_P;
      }
   };

   public final static IntrinsicFunction CHAR_CI_LT_P = new CharacterComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(char chLeft, char chRight)
      {
         return Character.toUpperCase(chLeft) < Character.toUpperCase(chRight);
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_CI_LT_P;
      }
   };

   public final static IntrinsicFunction CHAR_DOWNCASE = new CharacterUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(char ch)
      {
         return Primitive.createCharacter(Character.toLowerCase(ch));
      }

      protected Object invokeNull()
      {
         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_DOWNCASE;
      }
   };

   public final static IntrinsicFunction CHAR_EQ_P = new CharacterComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(char chLeft, char chRight)
      {
         return chLeft == chRight;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_EQ_P;
      }
   };

   public final static IntrinsicFunction CHAR_FOLDCASE = new CharacterUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(char ch)
      {
         if (ch == '\u0130' || ch == '\u0131')
         {
            return Primitive.createCharacter(ch);
         }

         return Primitive.createCharacter(Character.toLowerCase(Character.toUpperCase(ch)));
      }

      protected Object invokeNull()
      {
         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_FOLDCASE;
      }
   };

   public final static IntrinsicFunction CHAR_GE_P = new CharacterComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(char chLeft, char chRight)
      {
         return chLeft >= chRight;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_GE_P;
      }
   };

   public final static IntrinsicFunction CHAR_GENERAL_CATEGORY = new CharacterUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(char ch)
      {
         return UNICODE_GENERAL_CATEGORIES[Character.getType(ch)];
      }

      protected Object invokeNull()
      {
         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_GENERAL_CATEGORY;
      }
   };

   public final static IntrinsicFunction CHAR_GT_P = new CharacterComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(char chLeft, char chRight)
      {
         return chLeft > chRight;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_GT_P;
      }
   };

   public final static IntrinsicFunction CHAR_INTEGER = new CharacterUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(char ch)
      {
         return Primitive.createInteger(ch);
      }

      protected Object invokeNull()
      {
         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_INTEGER;
      }
   };

   public final static IntrinsicFunction CHAR_LE_P = new CharacterComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(char chLeft, char chRight)
      {
         return chLeft <= chRight;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_LE_P;
      }
   };

   public final static IntrinsicFunction CHAR_LOWER_CASE_P = new CharacterUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(char ch)
      {
         return Boolean.valueOf(Character.isLowerCase(ch));
      }

      protected Object invokeNull()
      {
         return Boolean.FALSE;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_LOWER_CASE_P;
      }
   };

   public final static IntrinsicFunction CHAR_LT_P = new CharacterComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(char chLeft, char chRight)
      {
         return chLeft < chRight;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_LT_P;
      }
   };

   public final static IntrinsicFunction CHAR_NUMERIC_P = new CharacterUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(char ch)
      {
         return Boolean.valueOf(Character.isDigit(ch));
      }

      protected Object invokeNull()
      {
         return Boolean.FALSE;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_NUMERIC_P;
      }
   };

   public final static IntrinsicFunction CHAR_READY_P = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         Reader reader = getReader(0, nArgCount, machine);

         try
         {
            machine.returnValue(Boolean.valueOf(reader.ready()), nArgCount);
         }
         catch (IOException e)
         {
            throw new ScriptingException("err.scripting.io", e);
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_READY_P;
      }
   };

   public final static IntrinsicFunction CHAR_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof Character);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_P;
      }
   };

   public final static IntrinsicFunction CHAR_TITLE_CASE_P = new CharacterUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(char ch)
      {
         return Boolean.valueOf(Character.isTitleCase(ch));
      }

      protected Object invokeNull()
      {
         return Boolean.FALSE;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_TITLE_CASE_P;
      }
   };

   public final static IntrinsicFunction CHAR_TITLECASE = new CharacterUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(char ch)
      {
         return Primitive.createCharacter(Character.toTitleCase(ch));
      }

      protected Object invokeNull()
      {
         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_TITLECASE;
      }
   };

   public final static IntrinsicFunction CHAR_UPCASE = new CharacterUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(char ch)
      {
         return Primitive.createCharacter(Character.toUpperCase(ch));
      }

      protected Object invokeNull()
      {
         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_UPCASE;
      }
   };

   public final static IntrinsicFunction CHAR_UPPER_CASE_P = new CharacterUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(char ch)
      {
         return Boolean.valueOf(Character.isUpperCase(ch));
      }

      protected Object invokeNull()
      {
         return Boolean.FALSE;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_UPPER_CASE_P;
      }
   };

   public final static IntrinsicFunction CHAR_WHITESPACE_P = new CharacterUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(char ch)
      {
         return Boolean.valueOf(Character.isWhitespace(ch));
      }

      protected Object invokeNull()
      {
         return Boolean.FALSE;
      }

      public Symbol getSymbol()
      {
         return Symbol.CHAR_WHITESPACE_P;
      }
   };

   public final static IntrinsicFunction CLOSE_INPUT_PORT = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         if (!(value instanceof Reader))
         {
            throw new TypeMismatchException(getSymbol());
         }

         try
         {
            ((Reader)value).close();
         }
         catch (IOException e)
         {
            throw new ScriptingException("err.scripting.io", e);
         }

         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.CLOSE_INPUT_PORT;
      }
   };

   public final static IntrinsicFunction CLOSE_OUTPUT_PORT = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         if (!(value instanceof Writer))
         {
            throw new TypeMismatchException(getSymbol());
         }

         try
         {
            ((Writer)value).close();
         }
         catch (IOException e)
         {
            throw new ScriptingException("err.scripting.io", e);
         }

         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.CLOSE_OUTPUT_PORT;
      }
   };

   public final static IntrinsicFunction COLLECTION = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         List list = new ArrayList(Math.max(8, nArgCount));

         for (int i = 0; i < nArgCount; ++i)
         {
            list.add(machine.getArg(i, nArgCount));
         }

         machine.returnValue(list, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.COLLECTION;
      }
   };

   public final static IntrinsicFunction COLLECTION_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof Collection);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.COLLECTION_P;
      }
   };

   public final static IntrinsicFunction COMPLEX_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof Number);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.COMPLEX_P;
      }
   };

   public final static IntrinsicFunction CONS = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);
         machine.returnValue(new Pair(machine.getArg(0, nArgCount), machine.getArg(1, nArgCount)), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.CONS;
      }

      /**
       * @see nexj.core.scripting.IntrinsicFunction#generate(nexj.core.scripting.Compiler.PCodeBuffer, nexj.core.scripting.Compiler.CallInstruction)
       */
      public boolean generate(Compiler.PCodeBuffer buf, Compiler.CallInstruction instr)
      {
         if (instr.getArgCount() == 2)
         {
            buf.addCode(Machine.CONS);

            return true;
         }

         return false;
      }
   };

   public final static IntrinsicFunction CONS_A = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyMinArgCount(nArgCount, 1);

         Object tail = machine.getArg(nArgCount - 1, nArgCount);

         for (int i = nArgCount - 2; i >= 0; i--)
         {
            tail = new Pair(machine.getArg(i, nArgCount), tail);
         }

         machine.returnValue(tail, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.CONS_A;
      }
   };

   public final static IntrinsicFunction COS = new SimpleNumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Primitive.createDouble(Math.cos(((Number)value).doubleValue()));
      }

      public Symbol getSymbol()
      {
         return Symbol.COS;
      }
   };

   public final static IntrinsicFunction CURRENT_INPUT_PORT = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 0);

         Reader reader = machine.getReader();

         if (reader == null)
         {
            throw new ScriptingException("err.scripting.defaultReader");
         }

         machine.returnValue(reader, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.CURRENT_INPUT_PORT;
      }
   };

   public final static IntrinsicFunction CURRENT_OUTPUT_PORT = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 0);

         Writer writer = machine.getWriter();

         if (writer == null)
         {
            throw new ScriptingException("err.scripting.defaultWriter");
         }

         machine.returnValue(writer, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.CURRENT_OUTPUT_PORT;
      }
   };

   public final static IntrinsicFunction DATUM_SYNTAX = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object id = machine.getArg(0, nArgCount);
         Object datum = machine.getArg(1, nArgCount);

         if (!(id instanceof Symbol))
         {
            throw new TypeMismatchException(getSymbol());
         }

         datum = machine.getTransformerContext().cloneExpansionHistory((Symbol)id, datum);
         machine.returnValue(datum, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.DATUM_SYNTAX;
      }
   };

   public final static IntrinsicFunction DELETE_FILE = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object str)
      {
         if (!(str instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         deleteFile((String)str);

         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.DELETE_FILE;
      }
   };

   public final static IntrinsicFunction DERIVED_ENVIRONMENT = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         GlobalEnvironment env = new GlobalEnvironment(machine.getGlobalEnvironment());

         for (int i = 0; i < nArgCount; ++i)
         {
            Object arg = machine.getArg(i, nArgCount);

            if (arg instanceof Pair)
            {
               Pair pair = (Pair)arg;

               if (pair.getHead() instanceof Pair)
               {
                  do
                  {
                     Pair assoc = (Pair)pair.getHead();

                     defineVariable(env, assoc.getHead(), assoc.getTail());
                     pair = pair.getNext();
                  }
                  while (pair != null);
               }
               else
               {
                  defineVariable(env, pair.getHead(), pair.getTail());
               }
            }
            else if (arg instanceof Lookup)
            {
               for (Lookup.Iterator itr = ((Lookup)arg).iterator(); itr.hasNext();)
               {
                  itr.next();
                  defineVariable(env, itr.getKey(), itr.getValue());
               }
            }
            else if (arg instanceof Map)
            {
               for (Iterator itr = ((Map)arg).entrySet().iterator(); itr.hasNext();)
               {
                  Map.Entry entry =(Map.Entry)itr.next();
                  defineVariable(env, entry.getKey(), entry.getValue());
               }
            }
            else if (arg != null)
            {
               throw new TypeMismatchException(getSymbol());
            }
         }

         machine.returnValue(env, nArgCount);

         return false;
      }

      protected void defineVariable(GlobalEnvironment env, Object name, Object value)
      {
         if (name instanceof Symbol)
         {
            env.defineVariable((Symbol)name, value);
         }
         else if (name instanceof String)
         {
            env.defineVariable((String)name, value);
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }
      }

      public Symbol getSymbol()
      {
         return Symbol.DERIVED_ENVIRONMENT;
      }
   };

   public final static IntrinsicFunction DISPLAY = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         Writer writer = getWriter(1, nArgCount, machine);

         try
         {
            write(machine.getArg(0, nArgCount), writer, true);
         }
         catch (IOException e)
         {
            throw new ScriptingException("err.scripting.io", e);
         }

         machine.returnValue(null, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.DISPLAY;
      }
   };

   public final static IntrinsicFunction DIV = new IntegerBinaryJavaIntrinsicFunction()
   {
      protected void verifyArgCount(int nArgCount)
      {
         verifyArgCount(nArgCount, 2);
      }

      protected BigInteger compute(BigInteger intLeft, BigInteger intRight)
      {
         if (intLeft.signum() >= 0)
         {
            return intLeft.divide(intRight);
         }

         return intLeft.subtract(intRight.abs()).add(BigInteger.ONE).divide(intRight);
      }

      protected long compute(long lLeft, long lRight)
      {
         return ((lLeft >= 0) ? lLeft : (lLeft - Math.abs(lRight) + 1)) / lRight;
      }

      public Symbol getSymbol()
      {
         return Symbol.DIV;
      }
   };

   public final static IntrinsicFunction DIV_AND_MOD = new IntegerBinaryJavaIntrinsicFunction()
   {
      protected void verifyArgCount(int nArgCount)
      {
         verifyArgCount(nArgCount, 2);
      }

      protected int getResultCount()
      {
         return 2;
      }

      protected void compute(BigInteger intLeft, BigInteger intRight, BigInteger[] results)
      {
         BigInteger[] intResults = intLeft.divideAndRemainder(intRight);

         if (intResults[1].signum() < 0)
         {
            intResults[1] = intResults[1].add(intRight.abs());

            results[0] = intLeft.subtract(intResults[1]).divide(intRight);
            results[1] = intResults[1];
         }
         else
         {
            results[0] = intResults[0];
            results[1] = intResults[1];
         }
      }

      protected void compute(long lLeft, long lRight, long[] lResults)
      {
         lResults[1] = lLeft % lRight;

         if (lResults[1] < 0)
         {
            lResults[1] += Math.abs(lRight);
            lResults[0] = (lLeft - lResults[1]) / lRight;
         }
         else
         {
            lResults[0] = lLeft / lRight;
         }
      }

      public Symbol getSymbol()
      {
         return Symbol.DIV_AND_MOD;
      }
   };

   public final static IntrinsicFunction DIV0 = new IntegerBinaryJavaIntrinsicFunction()
   {
      protected void verifyArgCount(int nArgCount)
      {
         verifyArgCount(nArgCount, 2);
      }

      protected BigInteger compute(BigInteger intLeft, BigInteger intRight)
      {
         if (intLeft.signum() >= 0)
         {
            intLeft = intLeft.add(intRight.abs().shiftRight(1));
         }
         else
         {
            intLeft = intLeft.subtract(intRight.abs().subtract(BigInteger.ONE).shiftRight(1));
         }

         return intLeft.divide(intRight);
      }

      protected long compute(long lLeft, long lRight)
      {
         if (lLeft >= 0)
         {
            lLeft += Math.abs(lRight) >> 1;
         }
         else
         {
            lLeft -= Math.abs(lRight) - 1 >> 1;
         }

         return lLeft / lRight;
      }

      public Symbol getSymbol()
      {
         return Symbol.DIV0;
      }
   };

   public final static IntrinsicFunction DIV0_AND_MOD0 = new IntegerBinaryJavaIntrinsicFunction()
   {
      protected void verifyArgCount(int nArgCount)
      {
         verifyArgCount(nArgCount, 2);
      }

      protected int getResultCount()
      {
         return 2;
      }

      protected void compute(BigInteger intLeft, BigInteger intRight, BigInteger[] results)
      {
         BigInteger[] intResults = intLeft.divideAndRemainder(intRight);
         BigInteger intDouble = intResults[1].shiftLeft(1);
         boolean bNegative = intRight.signum() == -1;

         intRight = intRight.abs();

         if (intDouble.compareTo(intRight) >= 0)
         {
            results[0] = (bNegative) ? intResults[0].subtract(BigInteger.ONE) : intResults[0].add(BigInteger.ONE);
            results[1] = intResults[1].subtract(intRight);
         }
         else if (intDouble.compareTo(intRight.negate()) < 0)
         {
            results[0] = (bNegative) ? intResults[0].add(BigInteger.ONE) : intResults[0].subtract(BigInteger.ONE);
            results[1] = intResults[1].add(intRight);
         }
         else
         {
            results[0] = intResults[0];
            results[1] = intResults[1];
         }
      }

      protected void compute(long lLeft, long lRight, long[] lResults)
      {
         lResults[0] = lLeft / lRight;
         lResults[1] = lLeft % lRight;

         long lDouble = lResults[1] << 1;
         boolean bNegative = lRight < 0;

         lRight = Math.abs(lRight);

         if (lDouble >= lRight)
         {
            lResults[0] += (bNegative) ? -1 : 1;
            lResults[1] -= lRight;
         }
         else if (lDouble < -lRight)
         {
            lResults[0] += (bNegative) ? 1 : -1;
            lResults[1] += lRight;
         }
      }

      public Symbol getSymbol()
      {
         return Symbol.DIV0_AND_MOD0;
      }
   };

   public final static IntrinsicFunction DIVIDE = new NumericBinaryJavaIntrinsicFunction(Machine.DIV)
   {
      /**
       * @see nexj.core.scripting.Intrinsic.JavaIntrinsicFunction#getCoercedType(nexj.core.meta.Primitive, nexj.core.meta.Primitive)
       */
      protected Primitive getCoercedType(Primitive left, Primitive right)
      {
         Primitive type = DIV_COERCION_ARRAY[left.getOrdinal() * Primitive.MAX_COUNT + right.getOrdinal()];

         if (type == null)
         {
            throw new TypeMismatchException(getSymbol());
         }

         return type;
      }

      protected BinaryFunction getBinaryFunction(Primitive type)
      {
         return type.findDivisionFunction(type);
      }

      protected Object getNoArgResult()
      {
         verifyArgCount(0, 1);

         return null;
      }

      protected Object getOneArgResult(Object arg)
      {
         if (arg != null)
         {
            return Primitive.INTEGER.getDivisionFunction(Primitive.primitiveOf(arg)).invoke(Primitive.ONE_INTEGER, arg);
         }

         return arg;
      }

      public Symbol getSymbol()
      {
         return Symbol.DIVIDE;
      }
   };

   public final static IntrinsicFunction ENUM_SET_COMPLEMENT = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object enumSet)
      {
         if (!(enumSet instanceof EnumSet))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return ((EnumSet)enumSet).complement();
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.ENUM_SET_COMPLEMENT;
      }
   };

   public final static IntrinsicFunction ENUM_SET_CONSTRUCTOR = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(final Object enumSet)
      {
         if (!(enumSet instanceof EnumSet))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return new Function()
         {
            public boolean invoke(int nArgCount, Machine machine)
            {
               if (nArgCount != 1)
               {
                  throw new ScriptingException("err.scripting.enum.constructorArgCount",
                        new Object[]{Primitive.createInteger(nArgCount)});
               }

               machine.returnValue(
                  EnumSet.createSubset((EnumSet)enumSet, machine.getArg(0, nArgCount)),
                  nArgCount);

               return false;
            }
         };
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.ENUM_SET_CONSTRUCTOR;
      }
   };

   public final static IntrinsicFunction ENUM_SET_DIFFERENCE = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object enumSet1 = machine.getArg(0, nArgCount);
         Object enumSet2 = machine.getArg(1, nArgCount);

         if (!(enumSet1 instanceof EnumSet && enumSet2 instanceof EnumSet))
         {
            throw new TypeMismatchException(getSymbol());
         }

         enumSet1 = ((EnumSet)enumSet1).difference((EnumSet)enumSet2);

         if (enumSet1 == null)
         {
            throw new ScriptingException("err.scripting.enum.typeMismatch", new Object[]{getSymbol()});
         }

         machine.returnValue(enumSet1, nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.ENUM_SET_DIFFERENCE;
      }
   };

   public final static IntrinsicFunction ENUM_SET_EQ_P = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object enumSet1 = machine.getArg(0, nArgCount);
         Object enumSet2 = machine.getArg(1, nArgCount);

         if (!(enumSet1 instanceof EnumSet && enumSet2 instanceof EnumSet))
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(
            Boolean.valueOf(((EnumSet)enumSet1).equals((EnumSet)enumSet2)),
            nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.ENUM_SET_EQ_P;
      }
   };

   public final static IntrinsicFunction ENUM_SET_INDEXER = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(final Object enumSet)
      {
         if (!(enumSet instanceof EnumSet))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return new Function()
         {
            public boolean invoke(int nArgCount, Machine machine)
            {
               if (nArgCount != 1)
               {
                  throw new ScriptingException("err.scripting.enum.indexerArgCount",
                     new Object[]{Primitive.createInteger(nArgCount)});
               }

               Object sym = machine.getArg(0, nArgCount);

               if (!(sym instanceof Symbol))
               {
                  throw new ScriptingException("err.scripting.enum.unsupportedValueType");
               }

               Object nIndex = ((EnumSet)enumSet).getMasterIndex(sym);

               machine.returnValue((nIndex == null) ? Boolean.FALSE : nIndex, nArgCount);

               return false;
            }
         };
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.ENUM_SET_INDEXER;
      }
   };

   public final static IntrinsicFunction ENUM_SET_INTERSECTION = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object enumSet1 = machine.getArg(0, nArgCount);
         Object enumSet2 = machine.getArg(1, nArgCount);

         if (!(enumSet1 instanceof EnumSet && enumSet2 instanceof EnumSet))
         {
            throw new TypeMismatchException(getSymbol());
         }

         enumSet1 = ((EnumSet)enumSet1).intersection((EnumSet)enumSet2);

         if (enumSet1 == null)
         {
            throw new ScriptingException("err.scripting.enum.typeMismatch", new Object[]{getSymbol()});
         }

         machine.returnValue(enumSet1, nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.ENUM_SET_INTERSECTION;
      }
   };

   public final static IntrinsicFunction ENUM_SET_LIST = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object obj)
      {
         if (!(obj instanceof EnumSet))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return ((EnumSet)obj).toPair();
      }

      public Symbol getSymbol()
      {
         return Symbol.ENUM_SET_LIST;
      }
   };

   public final static IntrinsicFunction ENUM_SET_MEMBER_P = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object sym = machine.getArg(0, nArgCount);
         Object enumSet = machine.getArg(1, nArgCount);

         if (!(sym instanceof Symbol && enumSet instanceof EnumSet))
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(Boolean.valueOf(((EnumSet)enumSet).contains(sym)), nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.ENUM_SET_MEMBER_P;
      }
   };

   public final static IntrinsicFunction ENUM_SET_PROJECTION = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object enumSet1 = machine.getArg(0, nArgCount);
         Object enumSet2 = machine.getArg(1, nArgCount);

         if (!(enumSet1 instanceof EnumSet && enumSet2 instanceof EnumSet))
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(((EnumSet)enumSet1).projection((EnumSet)enumSet2), nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.ENUM_SET_PROJECTION;
      }
   };

   public final static IntrinsicFunction ENUM_SET_SUBSET_P = new JavaIntrinsicFunction()
   {
      
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object enumSet1 = machine.getArg(0, nArgCount);
         Object enumSet2 = machine.getArg(1, nArgCount);

         if (!(enumSet1 instanceof EnumSet && enumSet2 instanceof EnumSet))
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(
            Boolean.valueOf(((EnumSet)enumSet1).subsets((EnumSet)enumSet2)),
            nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.ENUM_SET_SUBSET_P;
      }
   };

   public final static IntrinsicFunction ENUM_SET_UNION = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object enumSet1 = machine.getArg(0, nArgCount);
         Object enumSet2 = machine.getArg(1, nArgCount);

         if (!(enumSet1 instanceof EnumSet && enumSet2 instanceof EnumSet))
         {
            throw new TypeMismatchException(getSymbol());
         }

         enumSet1 = ((EnumSet)enumSet1).union((EnumSet)enumSet2);

         if (enumSet1 == null)
         {
            throw new ScriptingException("err.scripting.enum.typeMismatch", new Object[]{getSymbol()});
         }

         machine.returnValue(enumSet1, nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.ENUM_SET_UNION;
      }
   };

   public final static IntrinsicFunction ENUM_SET_UNIVERSE = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object enumSet)
      {
         if (!(enumSet instanceof EnumSet))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return ((EnumSet)enumSet).getMasterSet();
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.ENUM_SET_UNIVERSE;
      }
   };

   public final static IntrinsicFunction EOF_OBJECT_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value == Parser.EOF);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.EOF_OBJECT_P;
      }
   };

   public final static IntrinsicFunction EQ = new ComparisonJavaIntrinsicFunction(false, Machine.EQ)
   {
      protected boolean nullCmp(boolean bLeftNull, boolean bRightNull)
      {
         return bLeftNull == bRightNull;
      }

      protected BinaryFunction getBinaryFunction(Primitive type)
      {
         return type.findEQFunction(type);
      }

      public Symbol getSymbol()
      {
         return Symbol.EQ;
      }
   };

   public final static IntrinsicFunction EQUAL_HASH = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object obj)
      {
         return Primitive.createInteger(EqualHashTab.equalHash(obj));
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.EQUAL_HASH;
      }
   };

   public final static IntrinsicFunction EQUAL_P = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);
         machine.returnValue(Boolean.valueOf(equal(machine.getArg(0, nArgCount), machine.getArg(1, nArgCount))), nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.EQUAL_P;
      }
   };

   public final static IntrinsicFunction EQV_P = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);
         machine.returnValue(Boolean.valueOf(eqv(machine.getArg(0, nArgCount), machine.getArg(1, nArgCount))), nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.EQV_P;
      }
   };

   public final static IntrinsicFunction EQ_P = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);
         machine.returnValue(Boolean.valueOf(eq(machine.getArg(0, nArgCount), machine.getArg(1, nArgCount))), nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.EQ_P;
      }

      /**
       * @see nexj.core.scripting.IntrinsicFunction#generate(nexj.core.scripting.Compiler.PCodeBuffer, nexj.core.scripting.Compiler.CallInstruction)
       */
      public boolean generate(Compiler.PCodeBuffer buf, Compiler.CallInstruction instr)
      {
         if (instr.getArgCount() == 2)
         {
            buf.addCode(Machine.EQ_P);

            return true;
         }

         return false;
      }
   };

   public final static IntrinsicFunction ERROR = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyMinArgCount(nArgCount, 1);

         Object err = machine.getArg(0, nArgCount);

         if (!(err instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         Object[] args = null;
         Throwable cause = null;

         if (nArgCount > 1)
         {
            int nCount = nArgCount - 1;
            Object obj = machine.getArg(nCount, nArgCount);

            if (obj instanceof Throwable)
            {
               cause = (Throwable)obj;
               --nCount;
            }

            if (nCount > 0)
            {
               args = new Object[nCount];

               for (int i = 1; i <= nCount; ++i)
               {
                  args[i - 1] = machine.getArg(i, nArgCount);
               }
            }
         }

         throw new ScriptingError((String)err, args, cause);
      }

      public Symbol getSymbol()
      {
         return Symbol.ERROR;
      }
   };

   public final static IntrinsicFunction EVAL = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1, 2);

         Machine evalMachine;

         if (nArgCount == 2)
         {
            Object obj = machine.getArg(1, nArgCount);

            if (!(obj instanceof GlobalEnvironment))
            {
               throw new TypeMismatchException(getSymbol());
            }

            if (obj == machine.getGlobalEnvironment())
            {
               evalMachine = machine;
            }
            else
            {
               evalMachine = new Machine((GlobalEnvironment)obj, machine);
            }
         }
         else
         {
            evalMachine = machine;
         }

         Context context = machine.getContext();
         Machine machineSaved = context.getMachine();

         try
         {
            context.setMachine(evalMachine);

            PCodeFunction fun = new Compiler().compile(machine.getArg(0, nArgCount),
               evalMachine.getGlobalEnvironment().getTextPositionMap(), evalMachine, true);

            if (evalMachine != machine)
            {
               machine.returnValue(evalMachine.invoke(fun, (Pair)null), nArgCount);

               return Machine.EMPTY_FUNCTION.invoke(0, machine);
            }

            machine.pop(nArgCount);

            return fun.invoke(0, machine);
         }
         finally
         {
            context.setMachine(machineSaved);
         }
      }

      public boolean isPCode()
      {
         return true;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.EVAL;
      }
   };

   public final static IntrinsicFunction EVEN_P = new NumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(int n)
      {
         return Boolean.valueOf((n & 1) == 0);
      }

      protected Object invoke(long l)
      {
         return Boolean.valueOf((l & 1) == 0);
      }

      protected Object invoke(BigDecimal dec)
      {
         BigDecimal r = dec.setScale(0, BigDecimal.ROUND_DOWN);

         return Boolean.valueOf(r.compareTo(dec) == 0 && !r.toBigInteger().testBit(0));
      }

      protected Object invoke(float f)
      {
         return Boolean.valueOf(Math.IEEEremainder(f, 2) == 0);
      }

      protected Object invoke(double d)
      {
         return Boolean.valueOf(Math.IEEEremainder(d, 2) == 0);
      }

      protected Object invokeNull()
      {
         return Boolean.FALSE;
      }

      public Symbol getSymbol()
      {
         return Symbol.EVEN_P;
      }
   };

   public final static IntrinsicFunction EXACT_INEXACT = new NumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(int n)
      {
         return Primitive.createDouble(n);
      }

      protected Object invoke(long l)
      {
         return Primitive.createDouble(l);
      }

      protected Object invoke(BigDecimal dec)
      {
         if (dec.scale() >= Primitive.MAX_SCALE)
         {
            return dec;
         }

         return dec.setScale(Primitive.MAX_SCALE);
      }

      protected Object invoke(float f)
      {
         return Primitive.createFloat(f);
      }

      protected Object invoke(double d)
      {
         return Primitive.createDouble(d);
      }

      protected Object invokeNull()
      {
         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.EXACT_INEXACT;
      }
   };

   public final static IntrinsicFunction EXACT_INTEGER_SQRT = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object value = machine.getArg(0, nArgCount);

         if (value == null)
         {
            return machine.returnValues(null, null, null, nArgCount);
         }

         Number sqrt, diff;

         double dValue = ((Number)value).doubleValue();
         double dSqrt = Math.sqrt(dValue);

         if (value instanceof Integer || value instanceof Long)
         {
            long lSqrt = (long)dSqrt;

            sqrt = Primitive.createLong(lSqrt);
            diff = Primitive.createLong((long)(dValue - lSqrt * lSqrt));
         }
         else if (value instanceof Float || value instanceof Double || value instanceof BigDecimal)
         {
            BigDecimal decSqrt = BigDecimal.valueOf(dSqrt).setScale(0, BigDecimal.ROUND_DOWN);

            sqrt = decSqrt;
            diff = BigDecimal.valueOf(dValue).setScale(0, BigDecimal.ROUND_DOWN).subtract(decSqrt.multiply(decSqrt));
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         return machine.returnValues(sqrt, diff, null, nArgCount);
      }

      public Symbol getSymbol()
      {
         return Symbol.EXACT_INTEGER_SQRT;
      }
   };

   public final static IntrinsicFunction EXACT_P = new UnaryJavaIntrinsicFunction()
   {
      protected final Object invoke(Object value)
      {
         if (value == null)
         {
            return Boolean.FALSE;
         }

         switch (Primitive.primitiveOf(value).getOrdinal())
         {
            case Primitive.INTEGER_ORDINAL:
            case Primitive.LONG_ORDINAL:
               return Boolean.TRUE;

            case Primitive.DECIMAL_ORDINAL:
               return Boolean.valueOf(((BigDecimal)value).scale() < Primitive.MAX_SCALE);

            case Primitive.FLOAT_ORDINAL:
            case Primitive.DOUBLE_ORDINAL:
               return Boolean.FALSE;

            default:
               throw new TypeMismatchException(getSymbol());
         }
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.EXACT_P;
      }
   };

   public final static IntrinsicFunction EXP = new SimpleNumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Primitive.createDouble(Math.exp(((Number)value).doubleValue()));
      }

      public Symbol getSymbol()
      {
         return Symbol.EXP;
      }
   };

   public final static IntrinsicFunction EXPT = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object x = machine.getArg(0, nArgCount);
         Object y = machine.getArg(1, nArgCount);

         if (!(x instanceof Number) || !(y instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         if (x instanceof Float || x instanceof Double || y instanceof Float || y instanceof Double || y instanceof BigDecimal)
         {
            machine.returnValue(Primitive.createDouble(Math.pow(((Number)x).doubleValue(), ((Number)y).doubleValue())), nArgCount);

            return false;
         }

         BigDecimal decX;

         if (x instanceof BigDecimal)
         {
            decX = (BigDecimal)x;
         }
         else
         {
            decX = BigDecimal.valueOf(((Number)x).longValue());
         }

         long lExp = ((Number)y).longValue();
         BigDecimal decRes;

         if (lExp == 0)
         {
            decRes = Primitive.ONE_DECIMAL;
         }
         else
         {
            decRes = null;

            boolean bNeg = false;

            if (lExp < 0)
            {
               bNeg = true;
               lExp = -lExp;
            }

            for (;;)
            {
               if ((lExp & 1) != 0)
               {
                  if (decRes == null)
                  {
                     decRes = decX;
                  }
                  else
                  {
                     decRes = decRes.multiply(decX);
                  }
               }

               lExp >>>= 1;

               if (lExp == 0)
               {
                  break;
               }

               decX = decX.multiply(decX);
            }

            if (bNeg)
            {
               decRes = Primitive.ONE_DECIMAL.divide(decRes, Primitive.MAX_SCALE, BigDecimal.ROUND_HALF_UP);
            }
         }

         machine.returnValue(decRes, nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.EXPT;
      }
   };

   public final static IntrinsicFunction FILE_EXISTS_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object str)
      {
         if (!(str instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return Boolean.valueOf(fileExists((String)str));
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.FILE_EXISTS_P;
      }
   };

   public final static IntrinsicFunction FINITE_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         if (value == null)
         {
            return Boolean.FALSE;
         }

         if (value instanceof Float)
         {
            float fValue = ((Float)value).floatValue();

            return Boolean.valueOf(!Float.isInfinite(fValue) && !Float.isNaN(fValue));
         }

         if (value instanceof Double)
         {
            double dValue = ((Double)value).doubleValue();

            return Boolean.valueOf(!Double.isInfinite(dValue) && !Double.isNaN(dValue));
         }

         if (value instanceof Number)
         {
            return Boolean.TRUE;
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.FINITE_P;
      }
   };

   public final static IntrinsicFunction FLOOR = new NumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(int n)
      {
         return Primitive.createInteger(n);
      }

      protected Object invoke(long l)
      {
         return Primitive.createLong(l);
      }

      protected Object invoke(BigDecimal dec)
      {
         return dec.setScale(0, BigDecimal.ROUND_FLOOR);
      }

      protected Object invoke(float f)
      {
         return Primitive.createFloat((float)Math.floor(f));
      }

      protected Object invoke(double d)
      {
         return Primitive.createDouble(Math.floor(d));
      }

      protected Object invokeNull()
      {
         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.FLOOR;
      }
   };

   public final static IntrinsicFunction FORCE = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object fun = machine.getArg(0, nArgCount);

         if (fun instanceof Function)
         {
            machine.pop(1);
            machine.apply((Function)fun, 0);
         }
         else
         {
            machine.returnValue(fun, nArgCount);
            machine.setFunction(Machine.EMPTY_FUNCTION, 0);
         }

         return true;
      }

      public boolean isPCode()
      {
         return true;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.FORCE;
      }
   };

   public final static IntrinsicFunction FORMAT = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyMinArgCount(nArgCount, 1);

         Object id = machine.getArg(0, nArgCount);

         if (id == null)
         {
            id = "";
         }
         else if (!(id instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         if (nArgCount == 1)
         {
            machine.returnValue(machine.getContext().getString((String)id), nArgCount);
         }
         else
         {
            Object[] args = new Object[nArgCount - 1];

            for (int i = 1; i < nArgCount; ++i)
            {
               args[i - 1] = machine.getArg(i, nArgCount);
            }

            machine.returnValue(machine.getContext().formatString((String)id, args), nArgCount);
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.FORMAT;
      }
   };

   public final static IntrinsicFunction FREE_IDENTIFIER_EQ_P = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object left = machine.getArg(0, nArgCount);
         Object right = machine.getArg(1, nArgCount);

         if (!(left instanceof Symbol) || !(right instanceof Symbol))
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(Boolean.valueOf(left.equals(right)), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.FREE_IDENTIFIER_EQ_P;
      }
   };

   public final static IntrinsicFunction GCD = new IntegerBinaryJavaIntrinsicFunction()
   {
      protected Object getNoArgResult()
      {
         return Primitive.ZERO_INTEGER;
      }

      protected BigInteger getOneArgResult(BigInteger intValue)
      {
         return intValue.abs();
      }

      protected long getOneArgResult(long lValue)
      {
         return Math.abs(lValue);
      }

      protected BigInteger compute(BigInteger intLeft, BigInteger intRight)
      {
         return intLeft.abs().gcd(intRight.abs());
      }

      protected long compute(long lLeft, long lRight)
      {
         return gcd(Math.abs(lLeft), Math.abs(lRight));
      }

      public Symbol getSymbol()
      {
         return Symbol.GCD;
      }
   };

   public final static IntrinsicFunction GE = new ComparisonJavaIntrinsicFunction(false, Machine.GE)
   {
      protected boolean nullCmp(boolean bLeftNull, boolean bRightNull)
      {
         return !bLeftNull || bRightNull;
      }

      protected BinaryFunction getBinaryFunction(Primitive type)
      {
         return type.findGEFunction(type);
      }

      public Symbol getSymbol()
      {
         return Symbol.GE;
      }
   };

   public final static IntrinsicFunction GENERATE_TEMPORARIES = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object arg)
      {
         if (arg == null)
         {
            return null;
         }

         Pair tail = null;

         while (arg instanceof Pair)
         {
            tail = new Pair(Symbol.generateSymbol(), tail);
            arg = ((Pair)arg).getTail();
         }

         if (arg != null)
         {
            throw new TypeMismatchException(getSymbol());
         }

         return tail;
      }

      public Symbol getSymbol()
      {
         return Symbol.GENERATE_TEMPORARIES;
      }
   };

   public final static IntrinsicFunction GENSYM = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 0);

         machine.returnValue(Symbol.generateSymbol(), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.GENSYM;
      }
   };

   public final static IntrinsicFunction GET_STRING_ALL = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1, 2);

         Object writer = machine.getArg(0, nArgCount);

         if (!(writer instanceof Writer))
         {
            throw new TypeMismatchException(getSymbol());
         }

         boolean bClear = nArgCount == 2 && isTrue(machine.getArg(1, nArgCount));
         String sValue;

         if (writer instanceof FormattingWriter)
         {
            FormattingWriter formatter = (FormattingWriter)writer;

            sValue = formatter.getOutputString();

            if (bClear)
            {
               formatter.clear();
            }
         }
         else if (writer instanceof StringWriter)
         {
            sValue = writer.toString();

            if (bClear)
            {
               ((StringWriter)writer).getBuffer().setLength(0);
            }
         }
         else
         {
            sValue = "";
         }

         machine.returnValue(sValue, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.GET_STRING_ALL;
      }
   };

   public final static IntrinsicFunction GET_VALUE = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object name = machine.getArg(0, nArgCount);

         if (name instanceof Symbol)
         {
            machine.returnValue(machine.getGlobalEnvironment().findVariable((Symbol)name), nArgCount);
         }
         else if (name instanceof String)
         {
            machine.returnValue(machine.getGlobalEnvironment().findVariable(Symbol.define((String)name)), nArgCount);
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.GET_VALUE;
      }
   };

   public final static IntrinsicFunction GT = new ComparisonJavaIntrinsicFunction(false, Machine.GT)
   {
      protected boolean nullCmp(boolean bLeftNull, boolean bRightNull)
      {
         return !bLeftNull && bRightNull;
      }

      protected BinaryFunction getBinaryFunction(Primitive type)
      {
         return type.findGTFunction(type);
      }

      public Symbol getSymbol()
      {
         return Symbol.GT;
      }
   };

   public final static IntrinsicFunction HASHSET_ADD = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object setObj = machine.getArg(0, nArgCount);
         Object element = machine.getArg(1, nArgCount);

         if (!(setObj instanceof Set))
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(Boolean.valueOf(((Set)setObj).add(element)), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHSET_ADD;
      }
   };

   public final static IntrinsicFunction HASHSET_CLEAR = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1, 2);

         Object setObj = machine.getArg(0, nArgCount);

         if (setObj instanceof Holder)
         {
            // Can only resize GenericHashHolder
            if (nArgCount == 2 && setObj instanceof GenericHashHolder)
            {
               Object capacity = machine.getArg(1, nArgCount);

               if (!(capacity instanceof Number))
               {
                  throw new TypeMismatchException(getSymbol());
               }

               ((GenericHashHolder)setObj).clear(((Number)capacity).intValue());
            }
            else
            {
               ((Holder)setObj).clear();
            }
         }
         else if (setObj instanceof Set)
         {
            ((Set)setObj).clear();
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(null, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHSET_CLEAR;
      }
   };

   public final static IntrinsicFunction HASHSET_CONTAINS = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object setObj = machine.getArg(0, nArgCount);
         Object element = machine.getArg(1, nArgCount);

         if (!(setObj instanceof Set))
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(Boolean.valueOf(((Set)setObj).contains(element)), nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHSET_CONTAINS;
      }
   };

   public final static IntrinsicFunction HASHSET_COPY = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1, 2);

         Object setObj = machine.getArg(0, nArgCount);
         boolean bMutable = nArgCount == 2 && isTrue(machine.getArg(1, nArgCount));

         if (setObj instanceof ImmutableHolder)
         {
            setObj = (bMutable) ? ((ImmutableHolder)setObj).mutableClone() : ((ImmutableHolder)setObj).clone();
         }
         else if (setObj instanceof Holder)
         {
            setObj = ((Holder)setObj).clone();

            if (!bMutable)
            {
               setObj = new ImmutableHolder((Holder)setObj);
            }
         }
         else if (setObj instanceof Set)
         {
            setObj = new HashSet((Set)setObj);

            if (!bMutable)
            {
               setObj = new ImmutableSet((Set)setObj);
            }
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(setObj, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHSET_COPY;
      }
   };

   public final static IntrinsicFunction HASHSET_EQUIVALENCE_FUNCTION = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object setObj)
      {
         if (setObj instanceof HashFunctionHolder)
         {
            return ((HashFunctionHolder)setObj).getEquivalenceFunction();
         }

         if (setObj instanceof HashHolder)
         {
            return SYS_EQUAL_P;
         }

         if (setObj instanceof IdentityHashHolder)
         {
            return SYS_EQ_P;
         }

         if (setObj instanceof Set)
         {
            return Boolean.FALSE;
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHSET_EQUIVALENCE_FUNCTION;
      }
   };

   public final static IntrinsicFunction HASHSET_HASH_FUNCTION = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object setObj)
      {
         if (setObj instanceof HashFunctionHolder)
         {
            return ((HashFunctionHolder)setObj).getHashFunction();
         }

         if (setObj instanceof Set)
         {
            return Boolean.FALSE;
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHSET_HASH_FUNCTION;
      }
   };

   public final static IntrinsicFunction HASHSET_MUTABLE_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object setObj)
      {
         if (setObj instanceof Holder)
         {
            return Boolean.valueOf(!(setObj instanceof ImmutableHolder));
         }

         if (setObj instanceof Set)
         {
            return Boolean.valueOf(!(setObj instanceof ImmutableSet));
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHSET_MUTABLE_P;
      }
   };

   public final static IntrinsicFunction HASHSET_P = new UnaryJavaIntrinsicFunction()
   {
      public Object invoke(Object setObj)
      {
         return Boolean.valueOf(setObj instanceof Set);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHSET_P;
      }
   };

   public final static IntrinsicFunction HASHSET_REMOVE = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object setObj = machine.getArg(0, nArgCount);
         Object element = machine.getArg(1, nArgCount);

         if (!(setObj instanceof Set))
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(Boolean.valueOf(((Set)setObj).remove(element)), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHSET_REMOVE;
      }
   };

   public final static IntrinsicFunction HASHSET_SIZE = new UnaryJavaIntrinsicFunction()
   {
      public Object invoke(Object setObj)
      {
         if (!(setObj instanceof Set))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return Primitive.createInteger(((Set)setObj).size());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHSET_SIZE;
      }
   };

   public final static IntrinsicFunction HASHSET_VALUES = new UnaryJavaIntrinsicFunction()
   {
      public Object invoke(Object setObj)
      {
         if (!(setObj instanceof Set))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return ((Set)setObj).toArray();
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHSET_VALUES;
      }
   };

   public final static IntrinsicFunction HASHTABLE_CLEAR = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1, 2);

         Object mapObj = machine.getArg(0, nArgCount);

         if (mapObj instanceof Lookup)
         {
            // Can only resize GenericHashTab
            if (nArgCount == 2 && mapObj instanceof GenericHashTab)
            {
               Object capacity = machine.getArg(1, nArgCount);

               if (!(capacity instanceof Number))
               {
                  throw new TypeMismatchException(getSymbol());
               }

               ((GenericHashTab)mapObj).clear(((Number)capacity).intValue());
            }
            else
            {
               ((Lookup)mapObj).clear();
            }
         }
         else if (mapObj instanceof Map)
         {
            ((Map)mapObj).clear();
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(null, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHTABLE_CLEAR;
      }
   };

   public final static IntrinsicFunction HASHTABLE_CONTAINS_P = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object mapObj = machine.getArg(0, nArgCount);
         Object key = machine.getArg(1, nArgCount);
         boolean bContained;

         if (mapObj instanceof Lookup)
         {
            bContained = ((Lookup)mapObj).contains(key);
         }
         else if (mapObj instanceof Map)
         {
            bContained = ((Map)mapObj).containsKey(key);
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(Boolean.valueOf(bContained), nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHTABLE_CONTAINS_P;
      }
   };

   public final static IntrinsicFunction HASHTABLE_COPY = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1, 2);
         
         Object mapObj = machine.getArg(0, nArgCount);
         boolean bMutable = nArgCount == 2 && isTrue(machine.getArg(1, nArgCount));
         Object newMap;

         if (mapObj instanceof Lookup)
         {
            if (mapObj instanceof ImmutableLookup)
            {
               if (bMutable)
               {
                  newMap = ((ImmutableLookup)mapObj).mutableClone();
               }
               else
               {
                  newMap = ((ImmutableLookup)mapObj).clone(); // shallow copy
               }
            }
            else
            {
               Lookup copyMap = (Lookup)((Lookup)mapObj).clone();

               newMap = (bMutable) ? copyMap : new ImmutableLookup(copyMap);
            }
         }
         else if (mapObj instanceof Map)
         {
            Map copyMap = new HashMap((Map)mapObj);

            newMap = (bMutable) ? copyMap : new ImmutableMap(copyMap);
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(newMap, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHTABLE_COPY;
      }
   };

   public final static IntrinsicFunction HASHTABLE_DELETE = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object mapObj = machine.getArg(0, nArgCount);
         Object key = machine.getArg(1, nArgCount);

         if (mapObj instanceof Lookup)
         {
            machine.returnValue(((Lookup)mapObj).remove(key), nArgCount);
         }
         else if (mapObj instanceof Map)
         {
            machine.returnValue(((Map)mapObj).remove(key), nArgCount);
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHTABLE_DELETE;
      }
   };

   public final static IntrinsicFunction HASHTABLE_ENTRIES = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object mapObj = machine.getArg(0, nArgCount);
         Object[] keyArray, valueArray;

         if (mapObj instanceof Lookup)
         {
            Lookup map = (Lookup)mapObj;
            int nSize = map.size();

            keyArray = new Object[nSize];
            valueArray = new Object[nSize];

            Lookup.Iterator itr = map.iterator();

            for (int i = 0; itr.hasNext(); i++)
            {
               keyArray[i] = itr.next();
               valueArray[i] = itr.getValue();
            }
         }
         else if (mapObj instanceof Map)
         {
            Map map = (Map)mapObj;
            int nSize = map.size();

            keyArray = new Object[nSize];
            valueArray = new Object[nSize];

            Iterator itr = map.entrySet().iterator();

            for (int i = 0; itr.hasNext(); i++)
            {
               Entry e = (Entry)itr.next();

               keyArray[i] = e.getKey();
               valueArray[i] = e.getValue();
            }
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         return machine.returnValues(keyArray, valueArray, null, nArgCount);
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHTABLE_ENTRIES;
      }
   };

   public final static IntrinsicFunction HASHTABLE_EQUIVALENCE_FUNCTION = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object mapObj)
      {
         if (mapObj instanceof HashFunctionHolder)
         {
            return ((HashFunctionHolder)mapObj).getEquivalenceFunction();
         }

         if (mapObj instanceof HashTab)
         {
            return SYS_EQUAL_P;
         }

         if (mapObj instanceof IdentityHashTab)
         {
            return SYS_EQ_P;
         }

         if (mapObj instanceof Lookup || mapObj instanceof Map)
         {
            return Boolean.FALSE;
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHTABLE_EQUIVALENCE_FUNCTION;
      }
   };

   public final static IntrinsicFunction HASHTABLE_HASH_FUNCTION = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object mapObj)
      {
         if (mapObj instanceof HashFunctionHolder)
         {
            return ((HashFunctionHolder)mapObj).getHashFunction();
         }

         if (mapObj instanceof Lookup || mapObj instanceof Map)
         {
            return Boolean.FALSE;
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHTABLE_HASH_FUNCTION;
      }
   };

   public final static IntrinsicFunction HASHTABLE_KEYS = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object mapObj)
      {
         Iterator itr;
         int nSize;

         if (mapObj instanceof Lookup)
         {
            itr = ((Lookup)mapObj).iterator();
            nSize = ((Lookup)mapObj).size();
         }
         else if (mapObj instanceof Map)
         {
            itr = ((Map)mapObj).keySet().iterator();
            nSize = ((Map)mapObj).size();
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         Object[] vec = new Object[nSize];

         for (int i = 0; itr.hasNext(); i++)
         {
            vec[i] = itr.next();
         }

         return vec;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHTABLE_KEYS;
      }
   };

   public final static IntrinsicFunction HASHTABLE_MUTABLE_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object mapObj)
      {
         if (mapObj instanceof Lookup)
         {
            return Boolean.valueOf(!(mapObj instanceof ImmutableLookup));
         }

         if (mapObj instanceof Map)
         {
            return Boolean.valueOf(!(mapObj instanceof ImmutableMap));
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHTABLE_MUTABLE_P;
      }
   };

   public final static IntrinsicFunction HASHTABLE_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object obj)
      {
         return Boolean.valueOf(obj instanceof Lookup || obj instanceof Map);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHTABLE_P;
      }
   };

   public final static IntrinsicFunction HASHTABLE_REF = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2, 3);

         Object mapObj = machine.getArg(0, nArgCount);
         Object key = machine.getArg(1, nArgCount);
         Object defaultValue = (nArgCount < 3) ? null : machine.getArg(2, nArgCount);
         Object value;

         if (mapObj instanceof Lookup)
         {
            Lookup map = (Lookup)mapObj;

            value = map.get(key);

            if (value == null && (defaultValue == null || !map.contains(key)))
            {
               value = defaultValue;
            }
         }
         else if (mapObj instanceof Map)
         {
            Map map = (Map)mapObj;

            value = map.get(key);

            if (value == null && (defaultValue == null || !map.containsKey(key)))
            {
               value = defaultValue;
            }
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(value, nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHTABLE_REF;
      }
   };

   public final static IntrinsicFunction HASHTABLE_SET = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 3);

         Object mapObj = machine.getArg(0, nArgCount);
         Object key = machine.getArg(1, nArgCount);
         Object value = machine.getArg(2, nArgCount);

         if (mapObj instanceof Lookup)
         {
            machine.returnValue(((Lookup)mapObj).put(key, value), nArgCount);
         }
         else if (mapObj instanceof Map)
         {
            machine.returnValue(((Map)mapObj).put(key, value), nArgCount);
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHTABLE_SET;
      }
   };

   public final static IntrinsicFunction HASHTABLE_SIZE = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object mapObj)
      {
         if (mapObj instanceof Lookup)
         {
            return Primitive.createInteger(((Lookup)mapObj).size());
         }

         if (mapObj instanceof Map)
         {
            return Primitive.createInteger(((Map)mapObj).size());
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHTABLE_SIZE;
      }
   };

   public final static IntrinsicFunction HASHTABLE_UPDATE = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 4);

         Object fun = machine.getArg(2, nArgCount);

         if (!(fun instanceof Function))
         {
            throw new TypeMismatchException(getSymbol());
         }

         Object mapObj = machine.getArg(0, nArgCount);
         Object key = machine.getArg(1, nArgCount);
         Object defaultValue = machine.getArg(3, nArgCount);
         Object value;

         if (mapObj instanceof Lookup)
         {
            Lookup map = (Lookup)mapObj;

            value = map.get(key);

            map.put(key, machine.invoke(
               (Function)fun,
               (value != null || defaultValue != null && map.contains(key)) ? value : defaultValue, (Pair)null));
         }
         else if (mapObj instanceof Map)
         {
            Map map = (Map)mapObj;

            value = map.get(key);

            map.put(key, machine.invoke(
               (Function)fun,
               (value != null || defaultValue != null && map.containsKey(key)) ? value : defaultValue, (Pair)null));
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(value, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHTABLE_UPDATE;
      }
   };

   public final static IntrinsicFunction HASHTABLE_VALUES = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object mapObj)
      {
         Iterator itr;
         int nSize;

         if (mapObj instanceof Lookup)
         {
            itr = ((Lookup)mapObj).valueIterator();
            nSize = ((Lookup)mapObj).size();
         }
         else if (mapObj instanceof Map)
         {
            itr = ((Map)mapObj).values().iterator();
            nSize = ((Map)mapObj).size();
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         Object[] vec = new Object[nSize];

         for (int i = 0; itr.hasNext(); i++)
         {
            vec[i] = itr.next();
         }

         return vec;
      }

      public Symbol getSymbol()
      {
         return Symbol.HASHTABLE_VALUES;
      }
   };

   public final static IntrinsicFunction IDENTIFIER_P = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object arg = machine.getArg(0, nArgCount);

         machine.returnValue(
            Boolean.valueOf(arg instanceof Symbol && machine.getTransformerContext().isIdentifier((Symbol)arg)),
            nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.IDENTIFIER_P;
      }
   };

   public final static IntrinsicFunction IFNULL = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object arg = machine.getArg(0, nArgCount);

         machine.returnValue((arg != null) ? arg : machine.getArg(1, nArgCount), nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.IFNULL;
      }
   };

   public final static IntrinsicFunction IMPORT = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         for (int i = 0; i < nArgCount; ++i)
         {
            Object arg = machine.getArg(i, nArgCount);

            if (arg instanceof Symbol)
            {
               importClass((Symbol)arg, machine);
            }
            else if (arg instanceof Pair)
            {
               Pair pair = (Pair)arg;

               while (pair != null)
               {
                  if (pair.getHead() instanceof Symbol)
                  {
                     importClass((Symbol)pair.getHead(), machine);
                  }
                  else
                  {
                     throw new TypeMismatchException(getSymbol());
                  }

                  if (pair.getTail() == null)
                  {
                     break;
                  }

                  if (!(pair.getTail() instanceof Pair))
                  {
                     throw new ScriptingException("err.scripting.badList", new Object[]{getSymbol()});
                  }

                  pair = pair.getNext();
               }
            }
            else
            {
               throw new TypeMismatchException(getSymbol());
            }
         }

         machine.returnValue(null, nArgCount);

         return false;
      }

      private void importClass(Symbol symbol, Machine machine)
      {
         GlobalEnvironment env = machine.getGlobalEnvironment();

         if (!(env.findVariable(symbol) instanceof Class))
         {
            try
            {
               env.importJavaClass(Class.forName(symbol.getName()));
            }
            catch (NoClassDefFoundError e)
            {
               throw new ScriptingException("err.scripting.classNotFound", new Object[]{symbol}, e);
            }
            catch (ClassNotFoundException e)
            {
               throw new ScriptingException("err.scripting.classNotFound", new Object[]{symbol}, e);
            }
         }
      }

      public Symbol getSymbol()
      {
         return Symbol.IMPORT;
      }
   };

   public final static IntrinsicFunction IN_P = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyMinArgCount(nArgCount, 1);

         Object left = machine.getArg(0, nArgCount);

         if (left == null)
         {
            for (int i = 1; i < nArgCount; ++i)
            {
               Object value = machine.getArg(i, nArgCount);

               if (value instanceof Pair)
               {
                  for (Pair pair = (Pair)value; pair != null; pair = pair.getNext())
                  {
                     if (pair.getHead() == null)
                     {
                        machine.returnValue(Boolean.TRUE, nArgCount);

                        return false;
                     }
                  }
               }
               else if (value instanceof Collection)
               {
                  if (((Collection)value).contains(null))
                  {
                     machine.returnValue(Boolean.TRUE, nArgCount);

                     return false;
                  }
               }
               else
               {
                  if (value == null)
                  {
                     machine.returnValue(Boolean.TRUE, nArgCount);

                     return false;
                  }
               }
            }
         }
         else
         {
            Primitive leftType = Primitive.primitiveOf(left);

            if (leftType == Primitive.ANY)
            {
               for (int i = 1; i < nArgCount; ++i)
               {
                  Object right = machine.getArg(i, nArgCount);

                  if (right instanceof Collection)
                  {
                     if (((Collection)right).contains(left))
                     {
                        machine.returnValue(Boolean.TRUE, nArgCount);

                        return false;
                     }
                  }
                  else if (right instanceof Pair)
                  {
                     for (Pair pair = (Pair)right; pair != null; pair = pair.getNext())
                     {
                        if (left.equals(pair.getHead()))
                        {
                           machine.returnValue(Boolean.TRUE, nArgCount);

                           return false;
                        }
                     }
                  }
                  else
                  {
                     if (left.equals(right))
                     {
                        machine.returnValue(Boolean.TRUE, nArgCount);

                        return false;
                     }
                  }
               }
            }
            else
            {
               for (int i = 1; i < nArgCount; ++i)
               {
                  Object right = machine.getArg(i, nArgCount);

                  if (right instanceof Pair)
                  {
                     for (Pair pair = (Pair)right; pair != null; pair = pair.getNext())
                     {
                        if (leftType.getEQFunction(Primitive.primitiveOf(pair.getHead())).invoke(left, pair.getHead()) == Boolean.TRUE)
                        {
                           machine.returnValue(Boolean.TRUE, nArgCount);

                           return false;
                        }
                     }
                  }
                  else if (right instanceof Collection)
                  {
                     for (Iterator itr = ((Collection)right).iterator(); itr.hasNext();)
                     {
                        right = itr.next();

                        if (leftType.getEQFunction(Primitive.primitiveOf(right)).invoke(left, right) == Boolean.TRUE)
                        {
                           machine.returnValue(Boolean.TRUE, nArgCount);

                           return false;
                        }
                     }
                  }
                  else
                  {
                     if (leftType.getEQFunction(Primitive.primitiveOf(right)).invoke(left, right) == Boolean.TRUE)
                     {
                        machine.returnValue(Boolean.TRUE, nArgCount);

                        return false;
                     }
                  }
               }
            }
         }

         machine.returnValue(Boolean.FALSE, nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.IN_P;
      }
   };

   public final static IntrinsicFunction IN_PRIVILEGE_P; // Isolated

   public final static IntrinsicFunction INEXACT_EXACT = new NumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(int n)
      {
         return Primitive.createInteger(n);
      }

      protected Object invoke(long l)
      {
         return Primitive.createLong(l);
      }

      protected Object invoke(BigDecimal dec)
      {
         if (dec.scale() < Primitive.MAX_SCALE)
         {
            return dec;
         }

         return dec.setScale(Primitive.MAX_SCALE - 1, BigDecimal.ROUND_HALF_UP);
      }

      protected Object invoke(float f)
      {
         return new BigDecimal(f);
      }

      protected Object invoke(double d)
      {
         return new BigDecimal(d);
      }

      protected Object invokeNull()
      {
         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.INEXACT_EXACT;
      }
   };

   public final static IntrinsicFunction INEXACT_P = new UnaryJavaIntrinsicFunction()
   {
      protected final Object invoke(Object value)
      {
         if (value == null)
         {
            return Boolean.FALSE;
         }

         switch (Primitive.primitiveOf(value).getOrdinal())
         {
            case Primitive.INTEGER_ORDINAL:
            case Primitive.LONG_ORDINAL:
               return Boolean.FALSE;

            case Primitive.DECIMAL_ORDINAL:
               return Boolean.valueOf(((BigDecimal)value).scale() >= Primitive.MAX_SCALE);

            case Primitive.FLOAT_ORDINAL:
            case Primitive.DOUBLE_ORDINAL:
               return Boolean.TRUE;

            default:
               throw new TypeMismatchException(getSymbol());
         }
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.INEXACT_P;
      }
   };

   public final static IntrinsicFunction INFINITE_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         if (value == null)
         {
            return Boolean.FALSE;
         }

         if (value instanceof Float)
         {
            return Boolean.valueOf(Float.isInfinite(((Float)value).floatValue()));
         }

         if (value instanceof Double)
         {
            return Boolean.valueOf(Double.isInfinite(((Double)value).doubleValue()));
         }

         if (value instanceof Number)
         {
            return Boolean.FALSE;
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.INFINITE_P;
      }
   };

   public final static IntrinsicFunction INITIAL_ENVIRONMENT = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 0);

         machine.returnValue(
            new GlobalEnvironment(machine.getContext().getContextMetadata().getGlobalEnvironment()),
            nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.INITIAL_ENVIRONMENT;
      }
   };

   public final static IntrinsicFunction INPUT_PORT_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof Reader);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.INPUT_PORT_P;
      }
   };

   public final static IntrinsicFunction INSTANCE; // Isolated

   public final static IntrinsicFunction INSTANCE_COLLECTION; // Isolated

   public final static IntrinsicFunction INSTANCE_COLLECTION_P; // Isolated

   public final static IntrinsicFunction INSTANCE_P; // Isolated

   public final static IntrinsicFunction INTEGER_CHAR = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         if (value instanceof Number)
         {
            return Primitive.createCharacter(((Number)value).intValue());
         }

         if (value == null)
         {
            return null;
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.INTEGER_CHAR;
      }
   };

   public final static IntrinsicFunction INTEGER_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(isInteger(value));
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.INTEGER_P;
      }
   };

   public final static IntrinsicFunction INTERACTION_ENVIRONMENT = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 0);

         machine.returnValue(machine.getGlobalEnvironment(), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.INTERACTION_ENVIRONMENT;
      }
   };

   public final static IntrinsicFunction INVOCATION_CONTEXT = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 0);
         machine.returnValue(machine.getContext(), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.INVOCATION_CONTEXT;
      }
   };

   public final static IntrinsicFunction ITERATABLE_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value != null && !(value instanceof Pair) &&
            (value instanceof Collection || value instanceof Iterator ||
               value instanceof Iteratable ||  value.getClass().isArray()));
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.ITERATABLE_P;
      }
   };

   public final static IntrinsicFunction ITERATOR = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         Iterator itr = getIterator(value);

         if (itr == null)
         {
            throw new TypeMismatchException(getSymbol());
         }

         return itr;
      }

      public Symbol getSymbol()
      {
         return Symbol.ITERATOR;
      }
   };

   public final static IntrinsicFunction LCM = new IntegerBinaryJavaIntrinsicFunction()
   {
      protected Object getNoArgResult()
      {
         return Primitive.ONE_INTEGER;
      }

      protected BigInteger getOneArgResult(BigInteger intValue)
      {
         return intValue.abs();
      }

      protected long getOneArgResult(long lValue)
      {
         return Math.abs(lValue);
      }

      protected BigInteger compute(BigInteger intLeft, BigInteger intRight)
      {
         intRight = intRight.abs();

         BigInteger intGCD = intLeft.gcd(intRight);

         if (intGCD.signum() == 0)
         {
            return BigInteger.ZERO;
         }

         return intLeft.multiply(intRight.divide(intGCD));
      }

      protected long compute(long lLeft, long lRight)
      {
         lRight = Math.abs(lRight);

         long lGCD = gcd(lLeft, lRight);

         if (lGCD == 0)
         {
            return 0L;
         }

         return lLeft * (lRight / lGCD);
      }

      public Symbol getSymbol()
      {
         return Symbol.LCM;
      }
   };

   public final static IntrinsicFunction LE = new ComparisonJavaIntrinsicFunction(false, Machine.LE)
   {
      protected boolean nullCmp(boolean bLeftNull, boolean bRightNull)
      {
         return !bRightNull || bLeftNull;
      }

      protected BinaryFunction getBinaryFunction(Primitive type)
      {
         return type.findLEFunction(type);
      }

      public Symbol getSymbol()
      {
         return Symbol.LE;
      }
   };

   public final static IntrinsicFunction LENGTH = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object pair)
      {
         int nCount = 0;

         while (pair != null)
         {
            if (!(pair instanceof Pair))
            {
               throw new ScriptingException("err.scripting.badList",
                  new Object[]{getSymbol().getName()});
            }

            pair = ((Pair)pair).getTail();
            ++nCount;
         }

         return Primitive.createInteger(nCount);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.LENGTH;
      }
   };

   public final static IntrinsicFunction LIKE_P = new ComparisonJavaIntrinsicFunction(false, Machine.LIKE)
   {
      protected boolean nullCmp(boolean bLeftNull, boolean bRightNull)
      {
         return bLeftNull == bRightNull;
      }

      protected BinaryFunction getBinaryFunction(Primitive type)
      {
         return type.findLikeFunction(type);
      }

      public Symbol getSymbol()
      {
         return Symbol.LIKE_P;
      }
   };

   public final static IntrinsicFunction LIST = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         Pair pair = null;

         for (int i = nArgCount - 1; i >= 0; --i)
         {
            pair = new Pair(machine.getArg(i, nArgCount), pair);
         }

         machine.returnValue(pair, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.LIST;
      }

      /**
       * @see nexj.core.scripting.IntrinsicFunction#generate(nexj.core.scripting.Compiler.PCodeBuffer, nexj.core.scripting.Compiler.CallInstruction)
       */
      public boolean generate(Compiler.PCodeBuffer buf, Compiler.CallInstruction instr)
      {
         switch (instr.getArgCount())
         {
            case 0:
               buf.addCode(Machine.PUSH_NULL);
               return true;

            case 1:
               buf.addCode(Machine.LIST_1);
               return true;

            case 2:
               buf.addCode(Machine.LIST_2);
               return true;

            case 3:
               buf.addCode(Machine.LIST_3);
               return true;

            case 4:
               buf.addCode(Machine.LIST_4);
               return true;
         }

         return false;
      }
   };

   public final static IntrinsicFunction LIST_FILES = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object str)
      {
         if (!(str instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         Object[] sFileArray = listFiles((String)str);

         return (sFileArray == null) ? null : Pair.fromArray(sFileArray);
      }

      public Symbol getSymbol()
      {
         return Symbol.LIST_FILES;
      }
   };

   public final static IntrinsicFunction LIST_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object pair)
      {
         Boolean result = Boolean.TRUE;

         if (pair != null)
         {
            if (pair instanceof Pair)
            {
               Pair pair2 = (Pair)pair;
               Pair pair3 = pair2;

               for (;;)
               {
                  pair = pair3.getTail();

                  if (pair == null)
                  {
                     break;
                  }

                  if (!(pair instanceof Pair))
                  {
                     result = Boolean.FALSE;
                     break;
                  }

                  pair = ((Pair)pair).getTail();

                  if (pair == null)
                  {
                     break;
                  }

                  if (!(pair instanceof Pair))
                  {
                     result = Boolean.FALSE;
                     break;
                  }

                  pair2 = pair2.getNext();
                  pair3 = (Pair)pair;

                  if (pair2 == pair3)
                  {
                     result = Boolean.FALSE;
                     break;
                  }
               }
            }
            else
            {
               result = Boolean.FALSE;
            }
         }

         return result;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.LIST_P;
      }
   };

   public final static IntrinsicFunction LIST_REF = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object list = machine.getArg(0, nArgCount);
         Object index = machine.getArg(1, nArgCount);

         if (!(index instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         int n = ((Number)index).intValue();

         if (n < 0)
         {
            throw new ScriptingException("err.scripting.badIndex", new Object[]{index});
         }

         while (n-- != 0)
         {
            if (!(list instanceof Pair))
            {
               if (list == null)
               {
                  throw new ScriptingException("err.scripting.badIndex", new Object[]{index});
               }

               throw new ScriptingException("err.scripting.badList",
                  new Object[]{getSymbol().getName()});
            }

            list = ((Pair)list).getTail();
         }

         if (!(list instanceof Pair))
         {
            if (list == null)
            {
               throw new ScriptingException("err.scripting.badIndex", new Object[]{index});
            }

            throw new ScriptingException("err.scripting.badList",
               new Object[]{getSymbol().getName()});
         }

         machine.returnValue(((Pair)list).getHead(), nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.LIST_REF;
      }
   };

   public final static IntrinsicFunction LIST_STRING = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object arg)
      {
         int n = 0;

         for (Object list = arg; list != null; list = ((Pair)list).getTail())
         {
            if (!(list instanceof Pair))
            {
               throw new ScriptingException("err.scripting.badList",
                  new Object[]{getSymbol().getName()});
            }

            ++n;
         }

         StringBuffer buf = new StringBuffer(n);

         n = 0;

         for (Pair pair = (Pair)arg; pair != null; pair = pair.getNext())
         {
            Object ch = pair.getHead();

            if (!(ch instanceof Character))
            {
               throw new TypeMismatchException(getSymbol());
            }

            buf.append(((Character)ch).charValue());
         }

         return buf.toString();
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.LIST_STRING;
      }
   };

   public final static IntrinsicFunction LIST_TAIL = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object list = machine.getArg(0, nArgCount);
         Object index = machine.getArg(1, nArgCount);

         if (!(index instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         int n = ((Number)index).intValue();

         if (n < 0)
         {
            throw new ScriptingException("err.scripting.badIndex", new Object[]{index});
         }

         while (n-- != 0)
         {
            if (!(list instanceof Pair))
            {
               if (list == null)
               {
                  throw new ScriptingException("err.scripting.badIndex", new Object[]{index});
               }

               throw new ScriptingException("err.scripting.badList",
                  new Object[]{getSymbol().getName()});
            }

            list = ((Pair)list).getTail();
         }

         machine.returnValue(list, nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.LIST_TAIL;
      }
   };

   public final static IntrinsicFunction LIST_VECTOR = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object arg)
      {
         int n = 0;

         for (Object list = arg; list != null; list = ((Pair)list).getTail())
         {
            if (!(list instanceof Pair))
            {
               throw new ScriptingException("err.scripting.badList",
                  new Object[]{getSymbol().getName()});
            }

            ++n;
         }

         Object[] vec = new Object[n];

         n = 0;

         for (Pair pair = (Pair)arg; pair != null; pair = pair.getNext())
         {
            vec[n++] = pair.getHead();
         }

         return vec;
      }

      public Symbol getSymbol()
      {
         return Symbol.LIST_VECTOR;
      }
   };

   public final static IntrinsicFunction LOAD = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object fileName = machine.getArg(0, nArgCount);

         if (!(fileName instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         load((String)fileName, new Machine(machine));
         machine.returnValue(null, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.LOAD;
      }
   };

   public final static IntrinsicFunction LOCALE_NAME = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 0, 1);

         String sLocale;

         if (nArgCount == 0)
         {
            sLocale = machine.getContext().getLocaleName();
         }
         else
         {
            Object locale = machine.getArg(0, nArgCount);

            if (locale == null)
            {
               sLocale = machine.getContext().getLocaleName();
            }
            else if (locale instanceof String)
            {
               sLocale = (String)locale;
            }
            else if (locale instanceof Locale)
            {
               sLocale = locale.toString();
            }
            else
            {
               throw new TypeMismatchException(getSymbol());
            }

            sLocale = machine.getContext().getContextMetadata().getLocaleName(sLocale);
         }

         machine.returnValue(sLocale, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.LOCALE_NAME;
      }
   };

   public final static IntrinsicFunction LOG = new SimpleNumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Primitive.createDouble(Math.log(((Number)value).doubleValue()));
      }

      public Symbol getSymbol()
      {
         return Symbol.LOG;
      }
   };

   public final static IntrinsicFunction LT = new ComparisonJavaIntrinsicFunction(false, Machine.LT)
   {
      protected boolean nullCmp(boolean bLeftNull, boolean bRightNull)
      {
         return bLeftNull && !bRightNull;
      }

      protected BinaryFunction getBinaryFunction(Primitive type)
      {
         return type.findLTFunction(type);
      }

      public Symbol getSymbol()
      {
         return Symbol.LT;
      }
   };

   public final static IntrinsicFunction MACRO_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof Macro);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.MACRO_P;
      }
   };

   public final static IntrinsicFunction MAKE_BYTEVECTOR = new ByteVectorJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1, 2);

         int nLength = getLengthArg(0, nArgCount, machine);

         if (nLength == 0)
         {
            machine.returnValue(EMPTY_BYTEARRAY, nArgCount);
         }
         else
         {
            byte[] nArray = new byte[nLength];

            if (nArgCount == 2)
            {
               Arrays.fill(nArray, (byte)getExactIntArg(1, nArgCount, machine));
            }

            machine.returnValue(nArray, nArgCount);
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.MAKE_BYTEVECTOR;
      }
   };

   public final static IntrinsicFunction MAKE_COLLECTION = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 0, 1);

         Collection col;

         if (nArgCount == 0)
         {
            col = new ArrayList();
         }
         else
         {
            Object arg = machine.getArg(0, nArgCount);

            if (arg instanceof Number)
            {
               int nCount = ((Number)arg).intValue();

               if (nCount < 0)
               {
                  throw new ScriptingException("err.scripting.negativeCount",
                     new Object[]{getSymbol().getName()});
               }

               col = new ArrayList(nCount);
            }
            else if (arg instanceof Collection)
            {
               if (arg instanceof Ditto)
               {
                  col = (Collection)((Ditto)arg).ditto();
               }
               else
               {
                  col = new ArrayList(((Collection)arg).size());
               }
            }
            else
            {
               throw new TypeMismatchException(getSymbol());
            }
         }

         machine.returnValue(col, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.MAKE_COLLECTION;
      }
   };

   public final static IntrinsicFunction MAKE_ENUMERATION = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object symbols)
      {
         return EnumSet.createMasterSet(symbols);
      }

      public Symbol getSymbol()
      {
         return Symbol.MAKE_ENUMERATION;
      }
   };

   public final static IntrinsicFunction MAKE_EQ_HASHSET = new MakeHashObjectJavaIntrinsicFunction()
   {
      protected Object createHashObject(int nCapacity)
      {
         return new EqHashHolder(nCapacity);
      }

      public Symbol getSymbol()
      {
         return Symbol.MAKE_EQ_HASHSET;
      }
   };

   public final static IntrinsicFunction MAKE_EQ_HASHTABLE = new MakeHashObjectJavaIntrinsicFunction()
   {
      protected Object createHashObject(int nCapacity)
      {
         return new EqHashTab(nCapacity);
      }

      public Symbol getSymbol()
      {
         return Symbol.MAKE_EQ_HASHTABLE;
      }
   };

   public final static IntrinsicFunction MAKE_EQV_HASHSET = new MakeHashObjectJavaIntrinsicFunction()
   {
      protected Object createHashObject(int nCapacity)
      {
         return new EqvHashHolder(nCapacity);
      }

      public Symbol getSymbol()
      {
         return Symbol.MAKE_EQV_HASHSET;
      }
   };

   public final static IntrinsicFunction MAKE_EQV_HASHTABLE = new MakeHashObjectJavaIntrinsicFunction()
   {
      protected Object createHashObject(int nCapacity)
      {
         return new EqvHashTab(nCapacity);
      }

      public Symbol getSymbol()
      {
         return Symbol.MAKE_EQV_HASHTABLE;
      }
   };

   public final static IntrinsicFunction MAKE_HASHSET = new MakeHashObjectJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         if (nArgCount < 2)
         {
            return super.invoke(nArgCount, machine);
         }

         verifyArgCount(nArgCount, 0, 3);

         Object hash = machine.getArg(0, nArgCount);
         Object equiv = machine.getArg(1, nArgCount);

         if (hash instanceof Function && equiv instanceof Function)
         {
            machine.returnValue(
               (nArgCount == 2) ?
                  new ScriptingHashHolder((Function)hash, (Function)equiv)
                : new ScriptingHashHolder((Function)hash, (Function)equiv, getCapacityArg(2, nArgCount, machine)),
               nArgCount);

            return false;
         }

         throw new TypeMismatchException(getSymbol());
      }

      protected Object createHashObject(int nCapacity)
      {
         return new EqualHashHolder(nCapacity);
      }

      public Symbol getSymbol()
      {
         return Symbol.MAKE_HASHSET;
      }
   };

   public final static IntrinsicFunction MAKE_HASHTABLE = new MakeHashObjectJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         if (nArgCount < 2)
         {
            return super.invoke(nArgCount, machine);
         }

         verifyArgCount(nArgCount, 0, 3);

         Object hash = machine.getArg(0, nArgCount);
         Object equiv = machine.getArg(1, nArgCount);

         if (hash instanceof Function && equiv instanceof Function)
         {
            machine.returnValue(
               (nArgCount == 2) ?
                  new ScriptingHashTab((Function)hash, (Function)equiv)
                : new ScriptingHashTab((Function)hash, (Function)equiv, getCapacityArg(2, nArgCount, machine)),
               nArgCount);

            return false;
         }

         throw new TypeMismatchException(getSymbol());
      }

      protected Object createHashObject(int nCapacity)
      {
         return new EqualHashTab(nCapacity);
      }

      public Symbol getSymbol()
      {
         return Symbol.MAKE_HASHTABLE;
      }
   };

   public final static IntrinsicFunction MAKE_STRING = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1, 2);

         Object count = machine.getArg(0, nArgCount);

         if (!(count instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         int nCount = ((Number)count).intValue();

         if (nCount < 0)
         {
            throw new ScriptingException("err.scripting.negativeCount", new Object[]{getSymbol().getName()});
         }

         char ch;

         if (nArgCount == 2)
         {
            Object chr = machine.getArg(1, nArgCount);

            if (!(chr instanceof Character))
            {
               throw new TypeMismatchException(getSymbol());
            }

            ch = ((Character)chr).charValue();
         }
         else
         {
            ch = ' ';
         }

         char[] chars = new char[nCount];

         Arrays.fill(chars, ch);
         machine.returnValue(new String(chars), nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.MAKE_STRING;
      }
   };

   public final static IntrinsicFunction MAKE_VARIABLE_TRANSFORMER = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object func)
      {
         if (func instanceof SyntaxFunction)
         {
            assert ((SyntaxFunction)func).isVariable();
         }
         else if (func instanceof Function)
         {
            func = new SyntaxTransformer((Function)func);
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         return func;
      }

      public Symbol getSymbol()
      {
         return Symbol.MAKE_VARIABLE_TRANSFORMER;
      }
   };

   public final static IntrinsicFunction MAKE_VECTOR = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1, 2);

         Object count = machine.getArg(0, nArgCount);

         if (!(count instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         int nCount = ((Number)count).intValue();

         if (nCount < 0)
         {
            throw new ScriptingException("err.scripting.negativeCount",
               new Object[]{getSymbol().getName()});
         }

         Object[] vec = new Object[nCount];

         if (nArgCount == 2)
         {
            Arrays.fill(vec, machine.getArg(1, nArgCount));
         }

         machine.returnValue(vec, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.MAKE_VECTOR;
      }
   };

   public final static IntrinsicFunction MATCH = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object value = machine.getArg(0, nArgCount);
         Object expression = machine.getArg(1, nArgCount);

         if (expression instanceof String) // parse simplified infix notation
         {
            expression = new ExpressionParser().parse((String)value);
         }

         expression = MatchNode.parse(expression); // parse / validate

         double nValue = MatchNode.evaluate(
            (value == null)
            ? null : value.toString().toUpperCase(machine.getContext().getLocale()),
            expression,
            null,
            machine.getContext());
         machine.returnValue((nValue > 0) ? Primitive.createDouble(nValue) : null, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.MATCH;
      }
   };

   public final static IntrinsicFunction MAX = new MinMaxJavaIntrinsicFunction()
   {
      protected boolean nullCmp(boolean bLeftNull, boolean bRightNull)
      {
         return bLeftNull && !bRightNull;
      }

      protected BinaryFunction getBinaryFunction(Primitive type)
      {
         return type.findLTFunction(type);
      }

      public Symbol getSymbol()
      {
         return Symbol.MAX;
      }
   };

   public final static IntrinsicFunction MEMBER = new ListMemberIntrinsicFunction()
   {
      protected boolean match(Object obj, Pair pair)
      {
         return equal(obj, pair.getHead());
      }

      protected Object getResult(Pair pair)
      {
         return pair;
      }

      public Symbol getSymbol()
      {
         return Symbol.MEMBER;
      }
   };

   public final static IntrinsicFunction MEMQ = new ListMemberIntrinsicFunction()
   {
      protected boolean match(Object obj, Pair pair)
      {
         return eq(obj, pair.getHead());
      }

      protected Object getResult(Pair pair)
      {
         return pair;
      }

      public Symbol getSymbol()
      {
         return Symbol.MEMQ;
      }
   };

   public final static IntrinsicFunction MEMV = new ListMemberIntrinsicFunction()
   {
      protected boolean match(Object obj, Pair pair)
      {
         return eqv(obj, pair.getHead());
      }

      protected Object getResult(Pair pair)
      {
         return pair;
      }

      public Symbol getSymbol()
      {
         return Symbol.MEMV;
      }
   };

   public final static IntrinsicFunction MESSAGE = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         TransferObject tobj = new TransferObject(nArgCount);

         for (int i = 0; i < nArgCount; ++i)
         {
            Object arg = machine.getArg(i, nArgCount);

            if (!(arg instanceof Pair))
            {
               throw new TypeMismatchException(getSymbol());
            }

            Pair pair = (Pair)arg;
            Object head = pair.getHead();

            if (!(head instanceof Symbol))
            {
               throw new TypeMismatchException(getSymbol());
            }

            String sName = head.toString();

            if (sName.length() > 0 && sName.charAt(0) == ':')
            {
               Object value = pair.getTail();

               if (Symbol._EVENT.equals(head))
               {
                  if (value != null && !(value instanceof String))
                  {
                     throw new TypeMismatchException(Symbol._EVENT);
                  }

                  tobj.setEventName((String)value);
               }
               else if (Symbol._CLASS.equals(head))
               {
                  if (value != null && !(value instanceof String))
                  {
                     throw new TypeMismatchException(Symbol._CLASS);
                  }

                  tobj.setClassName((String)value);
               }
               else if (Symbol._OID.equals(head))
               {
                  if (value != null && !(value instanceof OID))
                  {
                     throw new TypeMismatchException(Symbol._OID);
                  }

                  tobj.setOID((OID)value);
               }
               else if (Symbol._VERSION.equals(head))
               {
                  if (!(value instanceof Number))
                  {
                     throw new TypeMismatchException(Symbol._VERSION);
                  }

                  tobj.setVersion(((Number)value).shortValue());
               }
               else
               {
                  tobj.setValue(sName, value);
               }
            }
            else
            {
               tobj.setValue(sName, pair.getTail());
            }
         }

         machine.returnValue(tobj, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.MESSAGE;
      }
   };

   public final static IntrinsicFunction MIN = new MinMaxJavaIntrinsicFunction()
   {
      protected boolean nullCmp(boolean bLeftNull, boolean bRightNull)
      {
         return !bLeftNull && bRightNull;
      }

      protected BinaryFunction getBinaryFunction(Primitive type)
      {
         return type.findGTFunction(type);
      }

      public Symbol getSymbol()
      {
         return Symbol.MIN;
      }
   };

   public final static IntrinsicFunction MINUS = new NumericBinaryJavaIntrinsicFunction(Machine.SUB)
   {
      protected BinaryFunction getBinaryFunction(Primitive type)
      {
         return type.findMinusFunction(type);
      }

      protected Object getNoArgResult()
      {
         verifyArgCount(0, 1);

         return null;
      }

      protected Object getOneArgResult(Object arg)
      {
         if (arg != null)
         {
            return Primitive.primitiveOf(arg).getNegationFunction().invoke(arg);
         }

         return arg;
      }

      public Symbol getSymbol()
      {
         return Symbol.MINUS;
      }
   };

   public final static IntrinsicFunction MOD = new IntegerBinaryJavaIntrinsicFunction()
   {
      protected void verifyArgCount(int nArgCount)
      {
         verifyArgCount(nArgCount, 2);
      }

      protected BigInteger compute(BigInteger intLeft, BigInteger intRight)
      {
         intLeft = intLeft.remainder(intRight);

         return (intLeft.signum() < 0) ? intLeft.add(intRight.abs()) : intLeft;
      }

      protected long compute(long lLeft, long lRight)
      {
         lLeft %= lRight;

         return (lLeft < 0) ? lLeft + Math.abs(lRight) : lLeft;
      }

      public Symbol getSymbol()
      {
         return Symbol.MOD;
      }
   };

   public final static IntrinsicFunction MOD0 = new IntegerBinaryJavaIntrinsicFunction()
   {
      protected void verifyArgCount(int nArgCount)
      {
         verifyArgCount(nArgCount, 2);
      }

      protected BigInteger compute(BigInteger intLeft, BigInteger intRight)
      {
         intLeft = intLeft.remainder(intRight);
         intRight = intRight.abs();

         BigInteger intDouble = intLeft.shiftLeft(1);

         if (intDouble.compareTo(intRight) >= 0)
         {
            intLeft = intLeft.subtract(intRight);
         }
         else if (intDouble.compareTo(intRight.negate()) < 0)
         {
            intLeft = intLeft.add(intRight);
         }

         return intLeft;
      }

      protected long compute(long lLeft, long lRight)
      {
         lLeft %= lRight;
         lRight = Math.abs(lRight);

         long lDouble = lLeft << 1;

         if (lDouble >= lRight)
         {
            lLeft -= lRight;
         }
         else if (lDouble < -lRight)
         {
            lLeft += lRight;
         }

         return lLeft;
      }

      public Symbol getSymbol()
      {
         return Symbol.MOD0;
      }
   };

   public final static IntrinsicFunction MODULO = new IntegerBinaryJavaIntrinsicFunction()
   {
      protected void verifyArgCount(int nArgCount)
      {
         verifyArgCount(nArgCount, 2);
      }

      protected BigInteger compute(BigInteger intLeft, BigInteger intRight)
      {
         intLeft = intLeft.remainder(intRight);

         if (intLeft.signum() * intRight.signum() == -1)
         {
            intLeft = intLeft.add(intRight);
         }

         return intLeft;
      }

      protected long compute(long lLeft, long lRight)
      {
         lLeft = lLeft % lRight;

         if (lLeft != 0 && (lLeft < 0) != (lRight < 0))
         {
            lLeft += lRight;
         }

         return lLeft;
      }

      public Symbol getSymbol()
      {
         return Symbol.MODULO;
      }
   };

   public final static IntrinsicFunction MUL = new NumericBinaryJavaIntrinsicFunction(Machine.MUL)
   {
      protected BinaryFunction getBinaryFunction(Primitive type)
      {
         return type.findMultiplicationFunction(type);
      }

      protected Object getNoArgResult()
      {
         return Primitive.ONE_INTEGER;
      }

      protected Object getOneArgResult(Object arg)
      {
         if (arg != null && !(arg instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return arg;
      }

      public Symbol getSymbol()
      {
         return Symbol.MUL;
      }
   };

   public final static IntrinsicFunction NAN_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         if (value == null)
         {
            return Boolean.FALSE;
         }

         if (value instanceof Float)
         {
            return Boolean.valueOf(Float.isNaN(((Float)value).floatValue()));
         }

         if (value instanceof Double)
         {
            return Boolean.valueOf(Double.isNaN(((Double)value).doubleValue()));
         }

         if (value instanceof Number)
         {
            return Boolean.FALSE;
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.NAN_P;
      }
   };

   public final static IntrinsicFunction NE = new ComparisonJavaIntrinsicFunction(true, Machine.NE)
   {
      protected boolean nullCmp(boolean bLeftNull, boolean bRightNull)
      {
         return bLeftNull != bRightNull;
      }

      protected BinaryFunction getBinaryFunction(Primitive type)
      {
         return type.findNEFunction(type);
      }

      public Symbol getSymbol()
      {
         return Symbol.NE;
      }
   };

   public final static IntrinsicFunction NEGATIVE_P = new NumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(int n)
      {
         return Boolean.valueOf(n < 0);
      }

      protected Object invoke(long l)
      {
         return Boolean.valueOf(l < 0);
      }

      protected Object invoke(BigDecimal dec)
      {
         return Boolean.valueOf(dec.signum() < 0);
      }

      protected Object invoke(float f)
      {
         return Boolean.valueOf(f < 0);
      }

      protected Object invoke(double d)
      {
         return Boolean.valueOf(d < 0);
      }

      protected Object invokeNull()
      {
         return Boolean.FALSE;
      }

      public Symbol getSymbol()
      {
         return Symbol.NEGATIVE_P;
      }
   };

   public final static IntrinsicFunction NEWLINE = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         Writer writer = getWriter(0, nArgCount, machine);

         try
         {
            if (writer instanceof FormattingWriter)
            {
               ((FormattingWriter)writer).writeLineSeparator();
            }
            else
            {
               writer.write(SysUtil.LINE_SEP);
            }
         }
         catch (IOException e)
         {
            throw new ScriptingException("err.scripting.io", e);
         }

         machine.returnValue(null, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.NEWLINE;
      }
   };

   public final static IntrinsicFunction NOT = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof Boolean && !((Boolean)value).booleanValue());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.NOT;
      }
   };

   public final static IntrinsicFunction NOW = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 0);
         machine.returnValue(new Timestamp(System.currentTimeMillis()), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.NOW;
      }
   };

   public final static IntrinsicFunction NULL_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value == null);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.NULL_P;
      }
   };

   public final static IntrinsicFunction NUMBER_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof Number);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.NUMBER_P;
      }
   };

   public final static IntrinsicFunction NUMBER_STRING = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1, 2);

         Object num = machine.getArg(0, nArgCount);

         if (!(num instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         int nRadix = 10;

         if (nArgCount == 2)
         {
            Object radix = machine.getArg(1, nArgCount);

            if (!(radix instanceof Number))
            {
               throw new TypeMismatchException(getSymbol());
            }

            nRadix = ((Number)radix).intValue();

            if (nRadix < Character.MIN_RADIX || nRadix > Character.MAX_RADIX)
            {
               throw new ScriptingException("err.scripting.badRadix",
                  new Object[]{radix, getSymbol()});
            }

            if (nRadix != 10)
            {
               if (num instanceof Float || num instanceof Double ||
                  num instanceof BigDecimal && ((BigDecimal)num).scale() > 0)
               {
                  throw new ScriptingException("err.scripting.badRadix",
                     new Object[]{radix, getSymbol()});
               }

               machine.returnValue(Long.toString(((Number)num).longValue(), nRadix), nArgCount);

               return false;
            }
         }

         String s = num.toString();

         if (num instanceof BigDecimal)
         {
            BigDecimal dec = (BigDecimal)num;

            if (dec.compareTo(MIN_LONG_DECIMAL) >= 0 && dec.compareTo(MAX_LONG_DECIMAL) <= 0)
            {
               s += "N";
            }
         }

         machine.returnValue(s, nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.NUMBER_STRING;
      }
   };

   public final static IntrinsicFunction OBJECT; // Isolated

   public final static IntrinsicFunction ODD_P = new NumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(int n)
      {
         return Boolean.valueOf((n & 1) != 0);
      }

      protected Object invoke(long l)
      {
         return Boolean.valueOf((l & 1) != 0);
      }

      protected Object invoke(BigDecimal dec)
      {
         BigDecimal r = dec.setScale(0, BigDecimal.ROUND_DOWN);

         return Boolean.valueOf(r.compareTo(dec) == 0 && r.toBigInteger().testBit(0));
      }

      protected Object invoke(float f)
      {
         double dblRem = Math.IEEEremainder(f, 2);

         return Boolean.valueOf(dblRem == 1 || dblRem == -1);
      }

      protected Object invoke(double d)
      {
         double dblRem = Math.IEEEremainder(d, 2);

         return Boolean.valueOf(dblRem == 1 || dblRem == -1);
      }

      protected Object invokeNull()
      {
         return Boolean.FALSE;
      }

      public Symbol getSymbol()
      {
         return Symbol.ODD_P;
      }
   };

   public final static IntrinsicFunction OID = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyMinArgCount(nArgCount, 1);

         int nCount = 0;

         for (int i = 0; i < nArgCount; ++i)
         {
            Object value = machine.getArg(i, nArgCount);

            if (value instanceof OIDHolder)
            {
               OID oid = ((OIDHolder)value).getOID();

               if (oid == null || oid.getCount() == 0)
               {
                  throw new TypeMismatchException(getSymbol());
               }

               nCount += oid.getCount();
            }
            else if (value != null && Primitive.primitiveOf(value) != Primitive.ANY)
            {
               ++nCount;
            }
            else
            {
               throw new TypeMismatchException(getSymbol());
            }
         }

         Object[] values = new Object[nCount];

         nCount = 0;

         for (int i = 0; i < nArgCount; ++i)
         {
            Object value = machine.getArg(i, nArgCount);

            if (value instanceof OIDHolder)
            {
               OID oid = ((OIDHolder)value).getOID();

               for (int k = 0; k < oid.getCount(); ++k)
               {
                  values[nCount++] = oid.getValue(k);
               }
            }
            else
            {
               values[nCount++] = value;
            }
         }

         machine.returnValue(new OID(values), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.OID;
      }
   };

   public final static IntrinsicFunction OPEN_INPUT_FILE = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         if (!(value instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return openReader((String)value);
      }

      public Symbol getSymbol()
      {
         return Symbol.OPEN_INPUT_FILE;
      }
   };

   public final static IntrinsicFunction OPEN_INPUT_STRING = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         if (!(value instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return new StringReader((String)value);
      }

      public Symbol getSymbol()
      {
         return Symbol.OPEN_INPUT_STRING;
      }
   };

   public final static IntrinsicFunction OPEN_OUTPUT_FILE = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         if (!(value instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return openWriter((String)value);
      }

      public Symbol getSymbol()
      {
         return Symbol.OPEN_OUTPUT_FILE;
      }
   };

   public final static IntrinsicFunction OPEN_OUTPUT_FORMATTER = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         FormattingWriter formatter;

         try
         {
            if (nArgCount == 0)
            {
               formatter = createPrettyFormatter(null, false);
            }
            else
            {
               Object arg = machine.getArg(0, nArgCount);

               formatter = createPrettyFormatter(
                  (Writer)arg, nArgCount == 6 && isTrue(machine.getArg(5, nArgCount)));

               if (nArgCount > 1)
               {
                  if ((arg = machine.getArg(1, nArgCount)) != null)
                  {
                     formatter.setLineLength(((Integer)arg).intValue());
                  }

                  if (nArgCount > 2)
                  {
                     if ((arg = machine.getArg(2, nArgCount)) != null)
                     {
                        formatter.setLineBreakThreshold(((Integer)arg).intValue());
                     }

                     if (nArgCount > 3)
                     {
                        if ((arg = machine.getArg(3, nArgCount)) != null)
                        {
                           Integer value = (Integer)arg;
                           int nIndentation = value.intValue();

                           if (nIndentation <= 0)
                           {
                              throw new ScriptingException("err.scripting.invalidIndentation", new Object[]{value});
                           }

                           formatter.setIndentation(nIndentation);
                        }

                        if (nArgCount == 5 || nArgCount == 6)
                        {
                           if ((arg = machine.getArg(4, nArgCount)) != null)
                           {
                              formatter.setLineSeparator((String)arg);
                           }
                        }
                        else
                        {
                           verifyArgCount(nArgCount, 0, 6); // throw exception
                        }
                     }
                  }
               }
            }
         }
         catch (ClassCastException e)
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(formatter, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.OPEN_OUTPUT_FORMATTER;
      }
   };

   public final static IntrinsicFunction OPEN_OUTPUT_STRING = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 0);
         machine.returnValue(new StringWriter(), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.OPEN_OUTPUT_STRING;
      }
   };

   public final static IntrinsicFunction OUTPUT_PORT_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof Writer);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.OUTPUT_PORT_P;
      }
   };

   public final static IntrinsicFunction PAIR_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof Pair);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.PAIR_P;
      }
   };

   public final static IntrinsicFunction PEEK_CHAR = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         Reader reader = getReader(0, nArgCount, machine);

         try
         {
            reader.mark(1);

            int ch = reader.read();

            if (ch == -1)
            {
               machine.returnValue(Parser.EOF, nArgCount);
            }
            else
            {
               reader.reset();
               machine.returnValue(Primitive.createCharacter(ch), nArgCount);
            }
         }
         catch (IOException e)
         {
            throw new ScriptingException("err.scripting.io", e);
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.PEEK_CHAR;
      }
   };

   public final static IntrinsicFunction PLUS = new NumericBinaryJavaIntrinsicFunction(Machine.ADD)
   {
      protected BinaryFunction getBinaryFunction(Primitive type)
      {
         return type.findPlusFunction(type);
      }

      protected Object getNoArgResult()
      {
         return Primitive.ZERO_INTEGER;
      }

      protected Object getOneArgResult(Object arg)
      {
         if (arg != null && !(arg instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return arg;
      }

      public Symbol getSymbol()
      {
         return Symbol.PLUS;
      }
   };

   public final static IntrinsicFunction POSITIVE_P = new NumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(int n)
      {
         return Boolean.valueOf(n > 0);
      }

      protected Object invoke(long l)
      {
         return Boolean.valueOf(l > 0);
      }

      protected Object invoke(BigDecimal dec)
      {
         return Boolean.valueOf(dec.signum() > 0);
      }

      protected Object invoke(float f)
      {
         return Boolean.valueOf(f > 0);
      }

      protected Object invoke(double d)
      {
         return Boolean.valueOf(d > 0);
      }

      protected Object invokeNull()
      {
         return Boolean.FALSE;
      }

      public Symbol getSymbol()
      {
         return Symbol.POSITIVE_P;
      }
   };

   public final static IntrinsicFunction PROCEDURE_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof Function);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.PROCEDURE_P;
      }
   };

   public final static IntrinsicFunction QUOTIENT = new IntegerBinaryJavaIntrinsicFunction()
   {
      protected void verifyArgCount(int nArgCount)
      {
         verifyArgCount(nArgCount, 2);
      }

      protected BigInteger compute(BigInteger intLeft, BigInteger intRight)
      {
         return intLeft.divide(intRight);
      }

      protected long compute(long lLeft, long lRight)
      {
         return lLeft / lRight;
      }

      public Symbol getSymbol()
      {
         return Symbol.QUOTIENT;
      }
   };

   public final static IntrinsicFunction RATIONAL_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof Number);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.RATIONAL_P;
      }
   };

   public final static IntrinsicFunction READ = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         Reader reader = getReader(0, nArgCount, machine);

         machine.returnValue(new SchemeParser(machine.getGlobalEnvironment()).parse(reader, null), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.READ;
      }
   };

   public final static IntrinsicFunction READ_CHAR = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         Reader reader = getReader(0, nArgCount, machine);

         try
         {
            int ch = reader.read();

            if (ch == -1)
            {
               machine.returnValue(Parser.EOF, nArgCount);
            }
            else
            {
               machine.returnValue(Primitive.createCharacter(ch), nArgCount);
            }
         }
         catch (IOException e)
         {
            throw new ScriptingException("err.scripting.io", e);
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.READ_CHAR;
      }
   };

   public final static IntrinsicFunction REAL_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof Number);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.REAL_P;
      }
   };

   public final static IntrinsicFunction REMAINDER = new IntegerBinaryJavaIntrinsicFunction()
   {
      protected void verifyArgCount(int nArgCount)
      {
         verifyArgCount(nArgCount, 2);
      }

      protected BigInteger compute(BigInteger intLeft, BigInteger intRight)
      {
         return intLeft.remainder(intRight);
      }

      protected long compute(long lLeft, long lRight)
      {
         return lLeft % lRight;
      }

      public Symbol getSymbol()
      {
         return Symbol.REMAINDER;
      }
   };

   public final static IntrinsicFunction REMOVE = new RemoveJavaIntrinsicFunction()
   {
      public boolean match(Object obj1, Object obj2)
      {
         return equal(obj1, obj2);
      }

      public Symbol getSymbol()
      {
         return Symbol.REMOVE;
      }
   };

   public final static IntrinsicFunction REMQ = new RemoveJavaIntrinsicFunction()
   {
      public boolean match(Object obj1, Object obj2)
      {
         return eq(obj1, obj2);
      }

      public Symbol getSymbol()
      {
         return Symbol.REMQ;
      }
   };

   public final static IntrinsicFunction REMV = new RemoveJavaIntrinsicFunction()
   {
      public boolean match(Object obj1, Object obj2)
      {
         return eqv(obj1, obj2);
      }

      public Symbol getSymbol()
      {
         return Symbol.REMV;
      }
   };

   public final static IntrinsicFunction REVERSE = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object list)
      {
         Pair rev = null;

         for (Pair pair; list != null; list = pair.getTail())
         {
            if (!(list instanceof Pair))
            {
               throw new ScriptingException("err.scripting.badList",
                  new Object[]{getSymbol().getName()});
            }

            pair = (Pair)list;
            rev = new Pair(pair.getHead(), rev);
         }

         return rev;
      }

      public Symbol getSymbol()
      {
         return Symbol.REVERSE;
      }
   };

   public final static IntrinsicFunction ROUND = new NumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(int n)
      {
         return Primitive.createInteger(n);
      }

      protected Object invoke(long l)
      {
         return Primitive.createLong(l);
      }

      protected Object invoke(BigDecimal dec)
      {
         return dec.setScale(0, BigDecimal.ROUND_HALF_EVEN);
      }

      protected Object invoke(float f)
      {
         return Primitive.createFloat((float)Math.rint(f));
      }

      protected Object invoke(double d)
      {
         return Primitive.createDouble(Math.rint(d));
      }

      protected Object invokeNull()
      {
         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.ROUND;
      }
   };

   public final static IntrinsicFunction SET_CAR = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object pair = machine.getArg(0, nArgCount);
         Object value = machine.getArg(1, nArgCount);

         if (!(pair instanceof Pair))
         {
            throw new TypeMismatchException(getSymbol());
         }

         ((Pair)pair).setHead(value);
         machine.returnValue(value, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.SET_CAR;
      }
   };

   public final static IntrinsicFunction SET_CDR = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object pair = machine.getArg(0, nArgCount);
         Object value = machine.getArg(1, nArgCount);

         if (!(pair instanceof Pair))
         {
            throw new TypeMismatchException(getSymbol());
         }

         ((Pair)pair).setTail(value);
         machine.returnValue(value, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.SET_CDR;
      }
   };

   public final static IntrinsicFunction SET_CURRENT_INPUT_PORT = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object obj = machine.getArg(0, 1);

         if (obj != null && !(obj instanceof Reader))
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(machine.getReader(), nArgCount);
         machine.setReader((Reader)obj);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.SET_CURRENT_INPUT_PORT;
      }
   };

   public final static IntrinsicFunction SET_CURRENT_OUTPUT_PORT = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object obj = machine.getArg(0, 1);

         if (obj != null && !(obj instanceof Writer))
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(machine.getWriter(), nArgCount);
         machine.setWriter((Writer)obj);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.SET_CURRENT_OUTPUT_PORT;
      }
   };

   public final static IntrinsicFunction SIN = new SimpleNumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Primitive.createDouble(Math.sin(((Number)value).doubleValue()));
      }

      public Symbol getSymbol()
      {
         return Symbol.SIN;
      }
   };

   public final static IntrinsicFunction SINT_LIST_BYTEVECTOR = new ListByteVectorConversionJavaIntrinsicFunction()
   {
      protected boolean isSigned()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.SINT_LIST_BYTEVECTOR;
      }
   };

   public final static IntrinsicFunction SQRT = new SimpleNumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Primitive.createDouble(Math.sqrt(((Number)value).doubleValue()));
      }

      public Symbol getSymbol()
      {
         return Symbol.SQRT;
      }
   };

   public final static IntrinsicFunction STRING = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         StringBuffer buf = new StringBuffer(nArgCount);

         for (int i = 0; i < nArgCount; ++i)
         {
            Object ch = machine.getArg(i, nArgCount);

            if (!(ch instanceof Character))
            {
               throw new TypeMismatchException(getSymbol());
            }

            buf.append(((Character)ch).charValue());
         }

         machine.returnValue(buf.toString(), nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING;
      }
   };

   public final static IntrinsicFunction STRING_AFFIX = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 3);

         String sLeft = fix(machine.getArg(0, nArgCount));
         String sSep = fix(machine.getArg(1, nArgCount));
         String sRight = fix(machine.getArg(2, nArgCount));
         String sResult;

         if (sLeft.length() == 0)
         {
            sResult = sRight;
         }
         else if (sRight.length() == 0)
         {
            sResult = sLeft;
         }
         else
         {
            StringBuffer buf = new StringBuffer(sLeft.length() + sSep.length() + sRight.length());

            buf.append(sLeft);
            buf.append(sSep);
            buf.append(sRight);

            sResult = buf.toString();
         }

         machine.returnValue(sResult, nArgCount);

         return false;
      }

      protected String fix(Object value) throws TypeMismatchException
      {
         if (value == null)
         {
            return "";
         }

         if (!(value instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return (String)value;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_AFFIX;
      }
   };

   public final static IntrinsicFunction STRING_APPEND = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         int nCount = 0;

         for (int i = 0; i < nArgCount; ++i)
         {
            Object str = machine.getArg(i, nArgCount);

            if (str != null)
            {
               if (!(str instanceof String))
               {
                  throw new TypeMismatchException(getSymbol());
               }

               nCount += ((String)str).length();
            }
         }

         StringBuffer buf = new StringBuffer(nCount);

         for (int i = 0; i < nArgCount; ++i)
         {
            Object str = machine.getArg(i, nArgCount);

            if (str != null)
            {
               buf.append(str);
            }
         }

         machine.returnValue(buf.toString(), nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_APPEND;
      }
   };

   public final static IntrinsicFunction STRING_CI_EQ_P = new StringComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(String sLeft, String sRight)
      {
         if (sLeft == null)
         {
            return sRight == null;
         }

         if (sRight == null)
         {
            return false;
         }

         return sLeft.equalsIgnoreCase(sRight);
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_CI_EQ_P;
      }
   };

   public final static IntrinsicFunction STRING_CI_GE_P = new StringComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(String sLeft, String sRight)
      {
         if (sLeft == null)
         {
            return sRight == null;
         }

         if (sRight == null)
         {
            return true;
         }

         return sLeft.compareToIgnoreCase(sRight) >= 0;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_CI_GE_P;
      }
   };

   public final static IntrinsicFunction STRING_CI_GT_P = new StringComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(String sLeft, String sRight)
      {
         if (sLeft == null)
         {
            return false;
         }

         if (sRight == null)
         {
            return true;
         }

         return sLeft.compareToIgnoreCase(sRight) > 0;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_CI_GT_P;
      }
   };

   public final static IntrinsicFunction STRING_CI_HASH = new UnaryJavaIntrinsicFunction()
   {
      public Object invoke(Object key)
      {
         if (key == null)
         {
            return Primitive.ZERO_INTEGER;
         }

         if (!(key instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         char charArray[] = ((String)key).toCharArray();
         int nLength = charArray.length;
         int nHash = 0;

         for (int i = 0; i < nLength; i++)
         {
            nHash = 31 * nHash + Character.toUpperCase(charArray[i]);
         }

         return Primitive.createInteger(nHash);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_CI_HASH;
      }
   };

   public final static IntrinsicFunction STRING_CI_LE_P = new StringComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(String sLeft, String sRight)
      {
         if (sLeft == null)
         {
            return true;
         }

         if (sRight == null)
         {
            return false;
         }

         return sLeft.compareToIgnoreCase(sRight) <= 0;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_CI_LE_P;
      }
   };

   public final static IntrinsicFunction STRING_CI_LT_P = new StringComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(String sLeft, String sRight)
      {
         if (sLeft == null)
         {
            return sRight != null;
         }

         if (sRight == null)
         {
            return false;
         }

         return sLeft.compareToIgnoreCase(sRight) < 0;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_CI_LT_P;
      }
   };

   public final static IntrinsicFunction STRING_COPY = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object str)
      {
         if (str == null)
         {
            return str;
         }

         if (!(str instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return new String((String)str);
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_COPY;
      }
   };

   public final static IntrinsicFunction STRING_DOWNCASE = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object str = machine.getArg(0, nArgCount);

         if (str != null)
         {
            if (!(str instanceof String))
            {
               throw new TypeMismatchException(getSymbol());
            }

            str = ((String)str).toLowerCase(machine.getContext().getLocale());
         }

         machine.returnValue(str, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_DOWNCASE;
      }
   };

   public final static IntrinsicFunction STRING_EQ_P = new StringComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(String sLeft, String sRight)
      {
         if (sLeft == null)
         {
            return sRight == null;
         }

         if (sRight == null)
         {
            return false;
         }

         return sLeft.equals(sRight);
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_EQ_P;
      }
   };

   public final static IntrinsicFunction STRING_EOL_EQ_P = new StringComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(String sLeft, String sRight)
      {
         return StringUtil.equalEOL(sLeft, sRight);
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_EOL_EQ_P;
      }
   };

   public final static IntrinsicFunction STRING_EMPTY_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         if (value == null)
         {
            return Boolean.TRUE;
         }

         if (value instanceof String)
         {
            return Boolean.valueOf(((String)value).length() == 0);
         }

         return Boolean.FALSE;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_EMPTY_P;
      }
   };

   public final static IntrinsicFunction STRING_EXPAND = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, final Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object arg0 = machine.getArg(0, nArgCount);
         Object arg1 = machine.getArg(1, nArgCount);

         if (!(arg0 instanceof String) || !(arg1 instanceof Function))
         {
            throw new TypeMismatchException(getSymbol());
         }

         String str = (String)arg0;
         final Function fun = (Function)arg1;

         try
         {
            StringWriter writer = new StringWriter(str.length());
            SubstReader reader = new SubstReader(new StringReader(str))
            {
               protected String getValue(String sName) throws IOException
               {
                  Object result = machine.invoke(fun, sName, (Object[])null);

                  if (result == null)
                  {
                     return "";
                  }

                  return Primitive.toString(result);
               }
            };

            IOUtil.copy(writer, reader);
            machine.returnValue(writer.toString(), nArgCount);
         }
         catch (IOException e)
         {
            throw new ScriptingException("err.scripting.io", null, e);
         }

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_EXPAND;
      }
   };

   public final static IntrinsicFunction STRING_FOLDCASE = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object str = machine.getArg(0, nArgCount);

         if (str != null)
         {
            if (!(str instanceof String))
            {
               throw new TypeMismatchException(getSymbol());
            }

            Locale locale = machine.getContext().getLocale();

            str = ((String)str).toUpperCase(locale).toLowerCase(locale);
         }

         machine.returnValue(str, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_FOLDCASE;
      }
   };

   public final static IntrinsicFunction STRING_GE_P = new StringComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(String sLeft, String sRight)
      {
         if (sLeft == null)
         {
            return sRight == null;
         }

         if (sRight == null)
         {
            return true;
         }

         return sLeft.compareTo(sRight) >= 0;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_GE_P;
      }
   };

   public final static IntrinsicFunction STRING_GT_P = new StringComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(String sLeft, String sRight)
      {
         if (sLeft == null)
         {
            return false;
         }

         if (sRight == null)
         {
            return true;
         }

         return sLeft.compareTo(sRight) > 0;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_GT_P;
      }
   };

   public final static IntrinsicFunction STRING_HASH = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object key)
      {
         if (key == null)
         {
            return Primitive.ZERO_INTEGER;
         }

         if (key instanceof String)
         {
            return Primitive.createInteger(((String)key).hashCode());
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_HASH;
      }
   };

   public final static IntrinsicFunction STRING_JOIN = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, final Machine machine)
      {
         verifyArgCount(nArgCount, 1, 2);

         String sSep;

         if (nArgCount == 2)
         {
            Object sep = machine.getArg(1, nArgCount);

            if (sep == null)
            {
               sSep = "";
            }
            else if (sep instanceof String)
            {
               sSep = (String)sep;
            }
            else
            {
               throw new TypeMismatchException(getSymbol());
            }
         }
         else
         {
            sSep = "";
         }

         Object arg = machine.getArg(0, nArgCount);

         if (arg == null)
         {
            machine.returnValue("", nArgCount);
         }
         else if (arg instanceof Pair)
         {
            int nCount = 0;

            for (Pair pair = (Pair)arg; pair != null; pair = pair.getNext())
            {
               nCount += getLength(pair.getHead());
            }

            StringBuffer buf = new StringBuffer(nCount + Math.max(nCount - 1, 0) * sSep.length());

            for (Pair pair = (Pair)arg; pair != null; pair = pair.getNext())
            {
               if (pair != arg)
               {
                  buf.append(sSep);
               }

               if (pair.getHead() != null)
               {
                  buf.append((String)pair.getHead());
               }
            }

            machine.returnValue(buf.toString(), nArgCount);
         }
         else if (arg instanceof Collection)
         {
            int nCount = 0;

            for (Iterator itr = ((Collection)arg).iterator(); itr.hasNext();)
            {
               nCount += getLength(itr.next());
            }

            StringBuffer buf = new StringBuffer(nCount + Math.max(nCount - 1, 0) * sSep.length());
            boolean bFirst = true;

            for (Iterator itr = ((Collection)arg).iterator(); itr.hasNext();)
            {
               String s = (String)itr.next();

               if (bFirst)
               {
                  bFirst = false;
               }
               else
               {
                  buf.append(sSep);
               }

               if (s != null)
               {
                  buf.append(s);
               }
            }

            machine.returnValue(buf.toString(), nArgCount);
         }
         else if (arg.getClass().isArray())
         {
            int nCount = 0;
            int n = Array.getLength(arg);

            for (int i = 0; i != n; ++i)
            {
               nCount += getLength(Array.get(arg, i));
            }

            StringBuffer buf = new StringBuffer(nCount + Math.max(nCount - 1, 0) * sSep.length());

            for (int i = 0; i != n; ++i)
            {
               String s = (String)Array.get(arg, i);

               if (i != 0)
               {
                  buf.append(sSep);
               }

               if (s != null)
               {
                  buf.append(s);
               }
            }

            machine.returnValue(buf.toString(), nArgCount);
         }
         else if (arg instanceof Iterator)
         {
            StringBuffer buf = new StringBuffer();
            boolean bFirst = true;

            for (Iterator itr = (Iterator)arg; itr.hasNext();)
            {
               Object obj = itr.next();

               if (bFirst)
               {
                  bFirst = false;
               }
               else
               {
                  buf.append(sSep);
               }

               if (obj != null)
               {
                  if (obj instanceof String)
                  {
                     buf.append((String)obj);
                  }
                  else
                  {
                     throw new TypeMismatchException(getSymbol());
                  }
               }
            }

            machine.returnValue(buf.toString(), nArgCount);
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         return false;
      }

      protected int getLength(Object value)
      {
         if (value == null)
         {
            return 0;
         }

         if (value instanceof String)
         {
            return ((String)value).length();
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_JOIN;
      }
   };

   public final static IntrinsicFunction STRING_LENGTH = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object str)
      {
         if (!(str instanceof String))
         {
            if (str == null)
            {
               return null;
            }

            throw new TypeMismatchException(getSymbol());
         }

         return Primitive.createInteger(((String)str).length());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_LENGTH;
      }
   };

   public final static IntrinsicFunction STRING_LE_P = new StringComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(String sLeft, String sRight)
      {
         if (sLeft == null)
         {
            return true;
         }

         if (sRight == null)
         {
            return false;
         }

         return sLeft.compareTo(sRight) <= 0;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_LE_P;
      }
   };

   public final static IntrinsicFunction STRING_LIST = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object str)
      {
         if (!(str instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         String s = (String)str;
         Pair pair = null;

         for (int i = s.length() - 1; i >= 0; --i)
         {
            pair = new Pair(Primitive.createCharacter(s.charAt(i)), pair);
         }

         return pair;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_LIST;
      }
   };

   public final static IntrinsicFunction STRING_LT_P = new StringComparisonJavaIntrinsicFunction()
   {
      protected boolean cmp(String sLeft, String sRight)
      {
         if (sLeft == null)
         {
            return sRight != null;
         }

         if (sRight == null)
         {
            return false;
         }

         return sLeft.compareTo(sRight) < 0;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_LT_P;
      }
   };

   public final static IntrinsicFunction STRING_MATCH = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object str = machine.getArg(0, nArgCount);

         if (str == null)
         {
            machine.returnValue(Boolean.FALSE, nArgCount);
         }
         else if (str instanceof String)
         {
            Object regexp = machine.getArg(1, nArgCount);
            Pattern pattern;

            if (regexp instanceof Pattern)
            {
               pattern = (Pattern)regexp;
            }
            else if (regexp instanceof String)
            {
               pattern = Pattern.compile((String)regexp, PATTERN_FLAGS);
            }
            else
            {
               throw new TypeMismatchException(getSymbol());
            }

            Matcher matcher = pattern.matcher((String)str);

            if (matcher.find())
            {
               int n = matcher.groupCount() + 1;
               String[] matches = new String[n];

               for (int i = 0; i != n; ++i)
               {
                  matches[i] = matcher.group(i);
               }

               machine.returnValue(matches, nArgCount);
            }
            else
            {
               machine.returnValue(Boolean.FALSE, nArgCount);
            }
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_MATCH;
      }
   };

   public final static IntrinsicFunction STRING_NUMBER = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1, 2);

         Object str = machine.getArg(0, nArgCount);

         if (!(str instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         int nRadix;

         if (nArgCount == 2)
         {
            Object radix = machine.getArg(1, nArgCount);

            if (!(radix instanceof Number))
            {
               throw new TypeMismatchException(getSymbol());
            }

            nRadix = ((Number)radix).intValue();

            if (nRadix < Character.MIN_RADIX || nRadix > Character.MAX_RADIX)
            {
               throw new ScriptingException("err.scripting.badRadix",
                  new Object[]{radix, getSymbol()});
            }
         }
         else
         {
            nRadix = 0;
         }

         try
         {
            machine.returnValue(new SchemeParser(machine.getGlobalEnvironment())
               .parseNumber(new StringReader((String)str), nRadix), nArgCount);
         }
         catch (ParserException e)
         {
            machine.returnValue(Boolean.FALSE, nArgCount);
         }

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_NUMBER;
      }
   };

   public final static IntrinsicFunction STRING_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof String);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_P;
      }
   };

   public final static IntrinsicFunction STRING_PATTERN = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object str)
      {
         if (!(str instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return Pattern.compile((String)str, PATTERN_FLAGS);
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_PATTERN;
      }
   };

   public final static IntrinsicFunction STRING_REF = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object str = machine.getArg(0, nArgCount);
         Object index = machine.getArg(1, nArgCount);

         if (!(str instanceof String) || !(index instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         try
         {
            machine.returnValue(Primitive.createCharacter(((String)str).charAt(((Number)index).intValue())), nArgCount);
         }
         catch (StringIndexOutOfBoundsException e)
         {
            throw new ScriptingException("err.scripting.badIndex", new Object[]{index}, e);
         }

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_REF;
      }
   };


   public final static IntrinsicFunction STRING_REPLACE = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 3, 4);

         Object str = machine.getArg(0, nArgCount);
         Object regexp = machine.getArg(1, nArgCount);
         Object replacement = machine.getArg(2, nArgCount);

         Pattern pattern;

         if (regexp instanceof Pattern)
         {
            pattern = (Pattern)regexp;
         }
         else if (regexp instanceof String)
         {
            pattern = Pattern.compile((String)regexp, PATTERN_FLAGS);
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         if (replacement == null)
         {
            replacement = "";
         }
         else if (!(replacement instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         if (str == null)
         {
            machine.returnValue(null, nArgCount);
         }
         else if (str instanceof String)
         {
            String sText = (String)str;
            String sReplacement = (String)replacement;
            Matcher matcher = pattern.matcher(sText);
            StringBuffer buf = new StringBuffer(sText.length());
            boolean bAll = nArgCount < 4 || isTrue(machine.getArg(3, nArgCount));

            for (boolean bFound = matcher.find(); bFound ; bFound = bAll && matcher.find())
            {
                matcher.appendReplacement(buf, sReplacement);
            }

            matcher.appendTail(buf);
            machine.returnValue(buf.toString(), nArgCount);
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_REPLACE;
      }
   };

   public final static IntrinsicFunction STRING_SPLIT = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2, 3);

         Object str = machine.getArg(0, nArgCount);
         Object regexp = machine.getArg(1, nArgCount);
         Pattern pattern;

         if (regexp instanceof Pattern)
         {
            pattern = (Pattern)regexp;
         }
         else if (regexp instanceof String)
         {
            pattern = Pattern.compile((String)regexp, PATTERN_FLAGS);
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         int nLimit = 0;

         if (nArgCount > 2)
         {
            Object limit = machine.getArg(2, nArgCount);

            if (!(limit instanceof Number))
            {
               throw new TypeMismatchException(getSymbol());
            }

            nLimit = ((Number)limit).intValue();
         }

         if (str == null)
         {
            machine.returnValue(null, nArgCount);
         }
         else if (str instanceof String)
         {
            machine.returnValue(Pair.fromArray(pattern.split((String)str, nLimit)), nArgCount);
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_SPLIT;
      }
   };

   public final static IntrinsicFunction STRING_SYMBOL = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object value = machine.getArg(0, nArgCount);

         if (value instanceof String)
         {
            machine.returnValue(Symbol.define((String)value), nArgCount);

            return false;
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_SYMBOL;
      }
   };

   public final static IntrinsicFunction STRING_TITLECASE = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object str = machine.getArg(0, nArgCount);

         if (str != null)
         {
            if (!(str instanceof String))
            {
               throw new TypeMismatchException(getSymbol());
            }

            str = StringUtil.toTitleCase((String)str, machine.getContext().getLocale());
         }

         machine.returnValue(str, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_TITLECASE;
      }
   };

   public final static IntrinsicFunction STRING_TRIM = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object str)
      {
         if (str == null)
         {
            return null;
         }

         if (!(str instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return ((String)str).trim();
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_TRIM;
      }
   };

   public final static IntrinsicFunction STRING_UPCASE = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object str = machine.getArg(0, nArgCount);

         if (str != null)
         {
            if (!(str instanceof String))
            {
               throw new TypeMismatchException(getSymbol());
            }

            str = ((String)str).toUpperCase(machine.getContext().getLocale());
         }

         machine.returnValue(str, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_UPCASE;
      }
   };

   public final static IntrinsicFunction STRING_UTF16 = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1, 2);

         Object str = machine.getArg(0, nArgCount);
         byte[] nArray = EMPTY_BYTEARRAY;

         if (str != null)
         {
            if (!(str instanceof String))
            {
               throw new TypeMismatchException(getSymbol());
            }

            String sValue = (String)str;

            if (sValue.length() != 0)
            {
               String sCharSet = "UTF-16BE";

               if (nArgCount == 2)
               {
                  Object endianness = machine.getArg(1, nArgCount);

                  if (!Symbol.BIG.equals(endianness))
                  {
                     if (Symbol.LITTLE.equals(endianness))
                     {
                        sCharSet = "UTF-16LE";
                     }
                     else if (endianness instanceof Symbol)
                     {
                        throw new ScriptingException("err.scripting.invalidEndianness", new Object[]{endianness, getSymbol().getName()});
                     }
                     else
                     {
                        throw new TypeMismatchException(getSymbol());
                     }
                  }
               }

               try
               {
                  nArray = sValue.getBytes(sCharSet);
               }
               catch (UnsupportedEncodingException e)
               {
                  throw ObjUtil.rethrow(e);
               }
            }
         }

         machine.returnValue(nArray, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_UTF16;
      }
   };

   public final static IntrinsicFunction STRING_UTF32 = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1, 2);

         Object str = machine.getArg(0, nArgCount);
         byte[] nArray = EMPTY_BYTEARRAY;

         if (str != null)
         {
            if (!(str instanceof String))
            {
               throw new TypeMismatchException(getSymbol());
            }

            String sValue = (String)str;
   
            if (sValue.length() != 0)
            {
               boolean bBigEndian = true;
      
               if (nArgCount == 2)
               {
                  Object endianness = machine.getArg(1, nArgCount);

                  if (Symbol.LITTLE.equals(endianness))
                  {
                     bBigEndian = false;
                  }
                  else if (!Symbol.BIG.equals(endianness))
                  {
                     if (endianness instanceof Symbol)
                     {
                        throw new ScriptingException("err.scripting.invalidEndianness", new Object[]{endianness, getSymbol().getName()});
                     }

                     throw new TypeMismatchException(getSymbol());
                  }
               }

               nArray = BinaryUtil.getBytesUTF32(sValue, bBigEndian);
            }
         }

         machine.returnValue(nArray, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_UTF32;
      }
   };

   public final static IntrinsicFunction STRING_UTF8 = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object str)
      {
         if (str != null)
         {
            if (!(str instanceof String))
            {
               throw new TypeMismatchException(getSymbol());
            }

            String sValue = (String)str;

            if (sValue.length() != 0)
            {
               try
               {
                  return sValue.getBytes("UTF-8");
               }
               catch (UnsupportedEncodingException e)
               {
                  throw ObjUtil.rethrow(e);
               }
            }
         }

         return EMPTY_BYTEARRAY;
      }

      public Symbol getSymbol()
      {
         return Symbol.STRING_UTF8;
      }
   };

   public final static IntrinsicFunction SUBSTRING = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 3);

         Object index = machine.getArg(1, nArgCount);
         Object end = machine.getArg(2, nArgCount);

         if (!(index instanceof Number) || !(end instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         Object str = machine.getArg(0, nArgCount);

         if (!(str instanceof String))
         {
            if (str == null)
            {
               machine.returnValue(null, nArgCount);

               return false;
            }

            throw new TypeMismatchException(getSymbol());
         }

         String s = (String)str;
         int nLength = s.length();

         try
         {
            machine.returnValue(s.substring(Math.min(((Number)index).intValue(), nLength),
               Math.min(((Number)end).intValue(), nLength)), nArgCount);
         }
         catch (StringIndexOutOfBoundsException e)
         {
            int nIndex = ((Number)index).intValue();

            throw new ScriptingException("err.scripting.badIndex",
               new Object[]{(nIndex >= 0 && nIndex <= nLength) ? end : index}, e);
         }

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.SUBSTRING;
      }
   };

   public final static IntrinsicFunction SYMBOL_EQ_P = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyMinArgCount(nArgCount, 2);

         Object value = machine.getArg(0, nArgCount);

         if (!(value instanceof Symbol))
         {
            throw new TypeMismatchException(getSymbol());
         }

         Symbol sym = (Symbol)value;
         Boolean equal = Boolean.TRUE;

         for (int i = 1; i < nArgCount; i++)
         {
            value = machine.getArg(i, nArgCount);

            if (!sym.equals(value))
            {
               if (!(value instanceof Symbol))
               {
                  throw new TypeMismatchException(getSymbol());
               }

               equal = Boolean.FALSE;
               break;
            }
         }

         machine.returnValue(equal, nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.SYMBOL_EQ_P;
      }
   };

   public final static IntrinsicFunction SYMBOL_HASH = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object key)
      {
         if (key == null)
         {
            return Primitive.ZERO_INTEGER;
         }

         if (key instanceof Symbol)
         {
            return Primitive.createInteger(((Symbol)key).hashCode());
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.SYMBOL_HASH;
      }
   };

   public final static IntrinsicFunction SYMBOL_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof Symbol);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.SYMBOL_P;
      }
   };

   public final static IntrinsicFunction SYMBOL_STRING = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         if (value instanceof Symbol)
         {
            return ((Symbol)value).getName();
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.SYMBOL_STRING;
      }
   };

   public final static IntrinsicFunction SYNTAX_DATUM = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object expr = machine.getArg(0, nArgCount);

         expr = machine.getTransformerContext().removeExpansionHistory(expr);
         machine.returnValue(expr, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.SYNTAX_DATUM;
      }
   };

   public final static IntrinsicFunction SYS_ASSERT = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         if (!isTrue(value))
         {
            throw new ScriptingException("err.assert");
         }

         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.SYS_ASSERT;
      }
   };

   public final static IntrinsicFunction SYS_AUDIT; // Isolated

   public final static IntrinsicFunction SYS_CAST = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object type = machine.getArg(0, nArgCount);

         if (!(type instanceof Type))
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(((Type)type).convert(machine.getArg(1, nArgCount)), nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.SYS_CAST;
      }
   };

   public final static IntrinsicFunction SYS_CHECK_ACCESS; // Isolated

   public final static IntrinsicFunction SYS_CHECK_PRIVILEGE = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Context context = machine.getContext();

         if (context.isSecure())
         {
            Object privilege = machine.getArg(0, nArgCount);

            if (!(privilege instanceof PrimitivePrivilege))
            {
               throw new TypeMismatchException(getSymbol());
            }

            if (!context.getPrivilegeSet().contains((PrimitivePrivilege)privilege))
            {
               throw new SecurityViolationException("err.runtime.privilege",
                  new Object[]{((PrimitivePrivilege)privilege).getName()});
            }
         }

         machine.returnValue(null, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.SYS_CHECK_PRIVILEGE;
      }
   };

   public final static IntrinsicFunction SYS_EQUAL_P = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         machine.returnValue(
            Boolean.valueOf(ObjUtil.equal(machine.getArg(0, nArgCount), machine.getArg(1, nArgCount))),
            nArgCount);

         return true;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.SYS_EQUAL_P;
      }
   };

   public final static IntrinsicFunction SYS_EQ_P = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         machine.returnValue(
            Boolean.valueOf(machine.getArg(0, nArgCount) == machine.getArg(1, nArgCount)),
            nArgCount);

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.SYS_EQ_P;
      }
   };

   public final static IntrinsicFunction SYS_FINALIZE = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 0);
         machine.apply(machine.popExceptionHandler(), 0);

         return true;
      }

      public boolean isPCode()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.SYS_FINALIZE;
      }
   };

   public final static IntrinsicFunction SYS_LOG = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyMinArgCount(nArgCount, 2);

         Object logged = machine.getArg(0, nArgCount);
         Object level = machine.getArg(1, nArgCount);
         Logger logger;

         if (logged instanceof LoggerHolder)
         {
            logger = ((LoggerHolder)logged).getLogger();
         }
         else if (logged == null)
         {
            logger = GlobalEnvironment.LOGGER;
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         if (!(level instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         int nLevel = ((Number)level).intValue();
         boolean bEnabled = logger.isLevelEnabled(nLevel);

         if (bEnabled && nArgCount > 2)
         {
            int nLastArg = nArgCount;
            Object arg = machine.getArg(nArgCount - 1, nArgCount);
            Throwable throwable = null;

            if (arg instanceof Throwable)
            {
               throwable = (Throwable)arg;
               nLastArg--;
            }

            StringWriter sw = new StringWriter(128);

            for (int i = 2; i < nLastArg; ++i)
            {
               if (i > 2)
               {
                  sw.write(' ');
               }

               try
               {
                  write(machine.getArg(i, nArgCount), sw, true);
               }
               catch (IOException e)
               {
                  sw.write("I/O Error: " + ObjUtil.getMessage(e));
               }
            }

            if (throwable == null)
            {
               logger.log(nLevel, sw.toString());
            }
            else
            {
               logger.log(nLevel, sw.toString(), throwable);
            }
         }

         machine.returnValue(Boolean.valueOf(bEnabled), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.SYS_LOG;
      }
   };

   public final static IntrinsicFunction SYS_LOG_LOCALIZED = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyMinArgCount(nArgCount, 3);

         Object logged = machine.getArg(0, nArgCount);
         Object level = machine.getArg(1, nArgCount);
         Logger logger;

         if (logged instanceof LoggerHolder)
         {
            logger = ((LoggerHolder)logged).getLogger();
         }
         else if (logged == null)
         {
            logger = GlobalEnvironment.LOGGER;
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         if (!(level instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         int nLevel = ((Number)level).intValue();
         boolean bEnabled = logger.isLevelEnabled(nLevel);

         if (bEnabled)
         {
            Object code = machine.getArg(2, nArgCount);

            if (!(code instanceof String))
            {
               throw new TypeMismatchException(getSymbol());
            }

            int nLastArg = nArgCount;
            Object arg = machine.getArg(nArgCount - 1, nArgCount);
            Throwable throwable = null;

            if (arg instanceof Throwable)
            {
               throwable = (Throwable)arg;
               nLastArg--;
            }

            Object args[] = new Object[nLastArg - 3];

            for (int i = 3; i < nLastArg; ++i)
            {
               args[i - 3] = machine.getArg(i, nArgCount);
            }

            logger.log(nLevel, (String)code, args, throwable);
         }

         machine.returnValue(Boolean.valueOf(bEnabled), nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.SYS_LOG_LOCALIZED;
      }
   };

   public final static IntrinsicFunction SYS_RETURN_VALUES = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object arg = machine.getArg(0, nArgCount);

         if (!(arg instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.pop();

         return machine.returnValues(((Number)arg).intValue());
      }

      public Symbol getSymbol()
      {
         return Symbol.SYS_RETURN_VALUES;
      }
   };

   public final static IntrinsicFunction SYS_SET_BARRIER = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 0, 1);

         Object cookie;

         if (nArgCount == 0)
         {
            cookie = machine.createBarrier();
         }
         else
         {
            cookie = machine.getArg(0, nArgCount);
            machine.restoreBarrier(cookie);
         }

         machine.returnValue(cookie, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.SYS_SET_BARRIER;
      }
   };

   public final static IntrinsicFunction SYS_SET_DIRTY; // Isolated

   public final static IntrinsicFunction SYS_SET_NEW; // Isolated

   public final static IntrinsicFunction SYS_TRY = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 3, 4);

         Object fun = machine.getArg(0, nArgCount);
         Object handler = machine.getArg(1, nArgCount);
         Object finalizer = machine.getArg(2, nArgCount);

         if (!(fun instanceof PCodeFunction) ||
            !(handler instanceof PCodeFunction) ||
            !(finalizer instanceof PCodeFunction))
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.pop(nArgCount);
         machine.setTry((PCodeFunction)fun, (PCodeFunction)handler, (PCodeFunction)finalizer,
            nArgCount == 4 && isTrue(machine.getArg(3, nArgCount)));

         return true;
      }

      public boolean isPCode()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.SYS_TRY;
      }
   };

   public final static IntrinsicFunction SYS_TX_BEGIN; // Isolated

   public final static IntrinsicFunction SYS_TX_COMMIT; // Isolated

   public final static IntrinsicFunction SYS_TX_MANDATE; // Isolated

   public final static IntrinsicFunction SYS_TX_MANDATE_NONE; // Isolated

   public final static IntrinsicFunction SYS_TX_REQUIRE; // Isolated

   public final static IntrinsicFunction SYS_TX_ROLLBACK; // Isolated

   public final static IntrinsicFunction SYS_TX_SUSPEND; // Isolated

   public final static IntrinsicFunction TAN = new SimpleNumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Primitive.createDouble(Math.tan(((Number)value).doubleValue()));
      }

      public Symbol getSymbol()
      {
         return Symbol.TAN;
      }
   };

   public final static IntrinsicFunction THROW = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object e = machine.getArg(0, nArgCount);

         if (e instanceof RuntimeException)
         {
            throw (RuntimeException)e;
         }

         if (e instanceof Throwable)
         {
            Throwable t = (Throwable)e;

            throw new ScriptingException("err.scripting.exception",
               new Object[]{ObjUtil.getMessage(t)}, t);
         }

         throw new TypeMismatchException(getSymbol());
      }

      public Symbol getSymbol()
      {
         return Symbol.THROW;
      }
   };

   public final static IntrinsicFunction TIMESTAMP_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value instanceof Timestamp);
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.TIMESTAMP_P;
      }
   };

   public final static IntrinsicFunction TRUNCATE = new NumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(int n)
      {
         return Primitive.createInteger(n);
      }

      protected Object invoke(long l)
      {
         return Primitive.createLong(l);
      }

      protected Object invoke(BigDecimal dec)
      {
         return dec.setScale(0, BigDecimal.ROUND_DOWN);
      }

      protected Object invoke(float f)
      {
         return Primitive.createFloat((float)((f < 0) ? Math.ceil(f) : Math.floor(f)));
      }

      protected Object invoke(double d)
      {
         return Primitive.createDouble((d < 0) ? Math.ceil(d) : Math.floor(d));
      }

      protected Object invokeNull()
      {
         return null;
      }

      public Symbol getSymbol()
      {
         return Symbol.TRUNCATE;
      }
   };

   public final static IntrinsicFunction U8_LIST_BYTEVECTOR = new ByteVectorJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object head = machine.getArg(0, nArgCount);
         Pair pair;
         int nLength = 0;

         for (Object current = head; current != null; current = pair.getTail())
         {
            if (!(current instanceof Pair))
            {
               throw new ScriptingException("err.scripting.badList", new Object[]{getSymbol().getName()});
            }

            pair = (Pair)current;

            Object value = pair.getHead();

            if (!(value instanceof Number))
            {
               throw new TypeMismatchException(getSymbol());
            }

            nLength++;
         }

         if (nLength == 0)
         {
            machine.returnValue(EMPTY_BYTEARRAY, nArgCount);
         }
         else
         {
            byte[] nArray = new byte[nLength];
   
            pair = (Pair)head;
   
            for (int i = 0; i < nLength; i++)
            {
               nArray[i] = ((Number)pair.getHead()).byteValue();
               pair = pair.getNext();
            }
   
            machine.returnValue(nArray, nArgCount);
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.U8_LIST_BYTEVECTOR;
      }
   };

   public final static IntrinsicFunction UINT_LIST_BYTEVECTOR = new ListByteVectorConversionJavaIntrinsicFunction()
   {
      protected boolean isSigned()
      {
         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.UINT_LIST_BYTEVECTOR;
      }
   };

   public final static IntrinsicFunction UTF16_STRING = new StringByteVectorConversionJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.StringByteVectorConversionJavaIntrinsicFunction#byteArrayToString(byte[], int, boolean)
       */
      protected String byteArrayToString(byte[] nArray, int nStartIndex, boolean bBigEndian)
      {
         try
         {
            return new String(nArray, nStartIndex, nArray.length - nStartIndex,
               (bBigEndian) ? "UTF-16BE" : "UTF-16LE");
         }
         catch (UnsupportedEncodingException e)
         {
            throw ObjUtil.rethrow(e);
         }
      }

      /**
       * @see nexj.core.scripting.Intrinsic.StringByteVectorConversionJavaIntrinsicFunction#getBigEndianByteOrderMark()
       */
      protected byte[] getBigEndianByteOrderMark()
      {
         return UTF16BE_BYTE_ORDER_MARK;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.StringByteVectorConversionJavaIntrinsicFunction#getLittleEndianByteOrderMark()
       */
      protected byte[] getLittleEndianByteOrderMark()
      {
         return UTF16LE_BYTE_ORDER_MARK;
      }

      public Symbol getSymbol()
      {
         return Symbol.UTF16_STRING;
      }
   };

   public final static IntrinsicFunction UTF32_STRING = new StringByteVectorConversionJavaIntrinsicFunction()
   {
      /**
       * @see nexj.core.scripting.Intrinsic.StringByteVectorConversionJavaIntrinsicFunction#byteArrayToString(byte[], int, boolean)
       */
      protected String byteArrayToString(byte[] nArray, int i, boolean bBigEndian)
      {
         return BinaryUtil.getStringUTF32(nArray, i, nArray.length, bBigEndian);
      }

      /**
       * @see nexj.core.scripting.Intrinsic.StringByteVectorConversionJavaIntrinsicFunction#getBigEndianByteOrderMark()
       */
      protected byte[] getBigEndianByteOrderMark()
      {
         return UTF32BE_BYTE_ORDER_MARK;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.StringByteVectorConversionJavaIntrinsicFunction#getLittleEndianByteOrderMark()
       */
      protected byte[] getLittleEndianByteOrderMark()
      {
         return UTF32LE_BYTE_ORDER_MARK;
      }

      public Symbol getSymbol()
      {
         return Symbol.UTF32_STRING;
      }
   };

   public final static IntrinsicFunction UTF8_STRING = new ByteVectorJavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         try
         {
            machine.returnValue(new String(getByteVectorArg(0, nArgCount, machine), "UTF-8"), nArgCount);
         }
         catch (UnsupportedEncodingException e)
         {
            ObjUtil.rethrow(e);
         }

         return false;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.UTF8_STRING;
      }
   };

   public final static IntrinsicFunction VALUES = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         return machine.returnValues(nArgCount);
      }

      public Symbol getSymbol()
      {
         return Symbol.VALUES;
      }

      /**
       * @see nexj.core.scripting.Intrinsic.JavaIntrinsicFunction#generate(nexj.core.scripting.Compiler.PCodeBuffer, nexj.core.scripting.Compiler.CallInstruction)
       */
      public boolean generate(PCodeBuffer buf, CallInstruction instr)
      {
         if (!instr.isLast())
         {
            int n = instr.getArgCount();

            if (n == 0)
            {
               throw new CompilerException("err.scripting.minRetCount",
                  new Object[]{Primitive.ONE_INTEGER, Primitive.ZERO_INTEGER},
                  null, instr.getTextPos());
            }

            while (--n != 0)
            {
               buf.addCode(Machine.POP);
            }

            return true;
         }

         return false;
      }
   };

   public final static IntrinsicFunction VECTOR = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         Object[] vec = new Object[nArgCount];

         for (int i = 0; i < nArgCount; ++i)
         {
            vec[i] = machine.getArg(i, nArgCount);
         }

         machine.returnValue(vec, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.VECTOR;
      }
   };

   public final static IntrinsicFunction VECTOR_APPEND = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         if (nArgCount == 0)
         {
            machine.returnValue(EMPTY_ARRAY, nArgCount);
         }
         else
         {
            Object arg = machine.getArg(0, nArgCount);

            if (arg instanceof Collection)
            {
               Collection col;

               if (arg instanceof Cloneable)
               {
                  try
                  {
                     col = (Collection)arg.getClass().getDeclaredMethod("clone", null).invoke(arg, null);
                  }
                  catch (Exception e)
                  {
                     throw new ScriptingException("err.scripting.clone", e);
                  }
               }
               else
               {
                  col = new ArrayList((Collection)arg);
               }

               for (int i = 1; i != nArgCount; ++i)
               {
                  arg = machine.getArg(i, nArgCount);

                  if (arg != null)
                  {
                     if (arg instanceof Collection)
                     {
                        col.addAll((Collection)arg);
                     }
                     else if (arg.getClass().isArray())
                     {
                        for (int k = 0, n = Array.getLength(arg); k != n; ++k)
                        {
                           col.add(Array.get(arg, k));
                        }
                     }
                     else
                     {
                        throw new TypeMismatchException(getSymbol());
                     }
                  }
               }

               machine.returnValue(col, nArgCount);
            }
            else if (arg == null || arg.getClass().isArray())
            {
               int nCount = 0;

               for (int i = 0; i != nArgCount; ++i)
               {
                  arg = machine.getArg(i, nArgCount);

                  if (arg != null)
                  {
                     if (arg.getClass().isArray())
                     {
                        nCount += Array.getLength(arg);
                     }
                     else if (arg instanceof Collection)
                     {
                        nCount += ((Collection)arg).size();
                     }
                     else
                     {
                        throw new TypeMismatchException(getSymbol());
                     }
                  }
               }

               Object[] vec = new Object[nCount];
               int nOfs = 0;

               for (int i = 0; i != nArgCount; ++i)
               {
                  arg = machine.getArg(i, nArgCount);

                  if (arg instanceof Collection)
                  {
                     for (Iterator itr = ((Collection)arg).iterator(); itr.hasNext();)
                     {
                        vec[nOfs++] = itr.next();
                     }
                  }
                  else if (arg != null)
                  {
                     for (int k = 0, n = Array.getLength(arg); k != n; ++k)
                     {
                        vec[nOfs++] = Array.get(arg, k);
                     }
                  }
               }

               machine.returnValue(vec, nArgCount);
            }
            else
            {
               throw new TypeMismatchException(getSymbol());
            }
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.VECTOR_APPEND;
      }
   };

   public final static IntrinsicFunction VECTOR_FILL = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object vec = machine.getArg(0, nArgCount);
         Object obj = machine.getArg(1, nArgCount);

         if (vec != null)
         {
            if (vec.getClass().isArray())
            {
               for (int i = 0; i < Array.getLength(vec); i++)
               {
                  Array.set(vec, i, obj);
               }
            }
            else if (vec instanceof List)
            {
               List list = (List)vec;

               for (int i = 0, nCount = list.size(); i < nCount; i++)
               {
                  list.set(i, obj);
               }
            }
            else if (vec instanceof Collection)
            {
               Collection col = (Collection)vec;
               int nCount = col.size();

               col.clear();

               for (int i = 0; i < nCount; i++)
               {
                  col.add(obj);
               }
            }
            else
            {
               throw new TypeMismatchException(getSymbol());
            }
         }

         machine.returnValue(vec, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.VECTOR_FILL;
      }
   };

   public final static IntrinsicFunction VECTOR_LENGTH = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object vec)
      {
         if (vec != null)
         {
            if (vec.getClass().isArray())
            {
               return Primitive.createInteger(Array.getLength(vec));
            }

            if (vec instanceof Collection)
            {
               return Primitive.createInteger(((Collection)vec).size());
            }
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.VECTOR_LENGTH;
      }
   };

   public final static IntrinsicFunction VECTOR_LIST = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object vec)
      {
         if (vec == null)
         {
            return null;
         }

         if (vec.getClass().isArray())
         {
            Pair pair = null;

            for (int i = Array.getLength(vec) - 1; i >= 0; --i)
            {
               pair = new Pair(Array.get(vec, i), pair);
            }

            return pair;
         }

         if (vec instanceof List)
         {
            Pair pair = null;
            List list = (List)vec;

            for (int i = list.size() - 1; i >= 0; --i)
            {
               pair = new Pair(list.get(i), pair);
            }

            return pair;
         }

         throw new TypeMismatchException(getSymbol());
      }

      public Symbol getSymbol()
      {
         return Symbol.VECTOR_LIST;
      }
   };

   public final static IntrinsicFunction VECTOR_P = new UnaryJavaIntrinsicFunction()
   {
      protected Object invoke(Object value)
      {
         return Boolean.valueOf(value != null &&
            (value.getClass().isArray() || value instanceof List));
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.VECTOR_P;
      }
   };

   public final static IntrinsicFunction VECTOR_REF = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object vec = machine.getArg(0, nArgCount);
         Object index = machine.getArg(1, nArgCount);

         if (vec != null && index instanceof Number)
         {
            try
            {
               if (vec.getClass().isArray())
               {
                  machine.returnValue(Array.get(vec, ((Number)index).intValue()), nArgCount);

                  return false;
               }

               if (vec instanceof List)
               {
                  machine.returnValue(((List)vec).get(((Number)index).intValue()), nArgCount);

                  return false;
               }
            }
            catch (IndexOutOfBoundsException e)
            {
               throw new ScriptingException("err.scripting.badIndex", new Object[]{index}, e);
            }
         }

         throw new TypeMismatchException(getSymbol());
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return Symbol.VECTOR_REF;
      }
   };

   public final static IntrinsicFunction VECTOR_SET = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 3);

         Object vec = machine.getArg(0, nArgCount);
         Object index = machine.getArg(1, nArgCount);
         Object value = machine.getArg(2, nArgCount);

         if (vec != null && index instanceof Number)
         {
            try
            {
               if (vec.getClass().isArray())
               {
                  Array.set(vec, ((Number)index).intValue(), value);
                  machine.returnValue(value, nArgCount);

                  return false;
               }

               if (vec instanceof List)
               {
                  ((List)vec).set(((Number)index).intValue(), value);
                  machine.returnValue(value, nArgCount);

                  return false;
               }
            }
            catch (IndexOutOfBoundsException e)
            {
               throw new ScriptingException("err.scripting.badIndex", new Object[]{index}, e);
            }
         }

         throw new TypeMismatchException(getSymbol());
      }

      public Symbol getSymbol()
      {
         return Symbol.VECTOR_SET;
      }
   };

   public final static IntrinsicFunction VECTOR_SORT = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, final Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object col = machine.getArg(0, nArgCount);
         Object less = machine.getArg(1, nArgCount);

         if (!(less instanceof Function))
         {
            throw new TypeMismatchException(getSymbol());
         }

         final Function fun = (Function)less;
         Comparator cmp = new Comparator()
         {
            public int compare(Object left, Object right)
            {
               return (isTrue(machine.invoke(fun, left, right, null))) ? -1 : 1;
            }
         };

         if (col instanceof Sortable)
         {
            ((Sortable)col).sort(cmp);
         }
         else if (col instanceof List)
         {
            Collections.sort((List)col, cmp);
         }
         else if (col instanceof Object[])
         {
            Arrays.sort((Object[])col, cmp);
         }
         else if (col != null)
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(col, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.VECTOR_SORT;
      }
   };

   public final static IntrinsicFunction WRITE = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         Writer writer = getWriter(1, nArgCount, machine);

         try
         {
            write(machine.getArg(0, nArgCount), writer, false);
         }
         catch (IOException e)
         {
            throw new ScriptingException("err.scripting.io", e);
         }

         machine.returnValue(null, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.WRITE;
      }
   };

   public final static IntrinsicFunction WRITE_CHAR = new JavaIntrinsicFunction()
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         Writer writer = getWriter(1, nArgCount, machine);
         Object ch = machine.getArg(0, nArgCount);

         if (!(ch instanceof Character))
         {
            throw new TypeMismatchException(getSymbol());
         }

         try
         {
            writer.write(((Character)ch).charValue());
         }
         catch (IOException e)
         {
            throw new ScriptingException("err.scripting.io", e);
         }

         machine.returnValue(null, nArgCount);

         return false;
      }

      public Symbol getSymbol()
      {
         return Symbol.WRITE_CHAR;
      }
   };

   public final static IntrinsicFunction ZERO_P = new NumericUnaryJavaIntrinsicFunction()
   {
      protected Object invoke(int n)
      {
         return Boolean.valueOf(n == 0);
      }

      protected Object invoke(long l)
      {
         return Boolean.valueOf(l == 0);
      }

      protected Object invoke(BigDecimal dec)
      {
         return Boolean.valueOf(dec.signum() == 0);
      }

      protected Object invoke(float f)
      {
         return Boolean.valueOf(f == 0);
      }

      protected Object invoke(double d)
      {
         return Boolean.valueOf(d == 0);
      }

      protected Object invokeNull()
      {
         return Boolean.FALSE;
      }

      public Symbol getSymbol()
      {
         return Symbol.ZERO_P;
      }
   };

   static
   {
      IsolatedFunctions functions;

      try
      {
         try
         {
            functions = (IsolatedFunctions)Class.forName("nexj.core.scripting.ServerFunctions").newInstance();
         }
         catch (ClassNotFoundException e1)
         {
            try
            {
               functions = (IsolatedFunctions)Class.forName("nexj.core.scripting.ClientFunctions").newInstance();
            }
            catch (ClassNotFoundException e2)
            {
               functions = new IsolatedFunctions();
            }
         }
      }
      catch (Exception e)
      {
         throw new ScriptingException("err.scripting.exception", new Object[]{ObjUtil.getMessage(e)});
      }

      IN_PRIVILEGE_P = functions.getInPrivilegePFunction();
      INSTANCE = functions.getInstanceFunction();
      INSTANCE_COLLECTION = functions.getInstanceCollectionFunction();
      INSTANCE_COLLECTION_P = functions.getInstanceCollectionPFunction();
      SYS_AUDIT = functions.getSysAuditFunction();
      SYS_CHECK_ACCESS = functions.getSysCheckAccessFunction();
      SYS_SET_DIRTY = functions.getSysSetDirtyFunction();
      SYS_SET_NEW = functions.getSysSetNewFunction();
      SYS_TX_BEGIN = functions.getSysTxBeginFunction();
      SYS_TX_COMMIT = functions.getSysTxCommitFunction();
      SYS_TX_MANDATE = functions.getSysTxMandateFunction();
      SYS_TX_MANDATE_NONE = functions.getSysTxMandateNoneFunction();
      SYS_TX_REQUIRE = functions.getSysTxRequireFunction();
      SYS_TX_ROLLBACK = functions.getSysTxRollbackFunction();
      SYS_TX_SUSPEND = functions.getSysTxSuspendFunction();
      OBJECT = functions.getObjectFunction();
      INSTANCE_P = functions.getInstancePFunction();
   }

   /**
    * Array containing the intrinsic functions, indexed by their ordinal number.
    */
   protected final static IntrinsicFunction[] s_functionArray =
   {
      SYS_FINALIZE, // Must be at index 0
      THROW, // Must be at index 1
      APPLY, // Must be at index 2
      SYS_RETURN_VALUES, // Must be at index 3
      ABS,
      ACOS,
      APPEND,
      APPEND_M,
      ASIN,
      ASSOC,
      ASSQ,
      ASSV,
      ATAN,
      BINARY_P,
      BITWISE_AND,
      BITWISE_ARITHMETIC_SHIFT,
      BITWISE_ARITHMETIC_SHIFT_LEFT,
      BITWISE_ARITHMETIC_SHIFT_RIGHT,
      BITWISE_BIT_COUNT,
      BITWISE_BIT_FIELD,
      BITWISE_BIT_SET_P,
      BITWISE_COPY_BIT,
      BITWISE_COPY_BIT_FIELD,
      BITWISE_FIRST_BIT_SET,
      BITWISE_IF,
      BITWISE_IOR,
      BITWISE_LENGTH,
      BITWISE_NOT,
      BITWISE_REVERSE_BIT_FIELD,
      BITWISE_ROTATE_BIT_FIELD,
      BITWISE_XOR,
      BOOLEAN_EQ_P,
      BOOLEAN_P,
      BOUND_IDENTIFIER_EQ_P,
      BYTEVECTOR,
      BYTEVECTOR_APPEND,
      BYTEVECTOR_COPY,
      BYTEVECTOR_COPY_M,
      BYTEVECTOR_EQ_P,
      BYTEVECTOR_FILL,
      BYTEVECTOR_IEEE_DOUBLE_NATIVE_REF,
      BYTEVECTOR_IEEE_DOUBLE_NATIVE_SET,
      BYTEVECTOR_IEEE_DOUBLE_REF,
      BYTEVECTOR_IEEE_DOUBLE_SET,
      BYTEVECTOR_IEEE_SINGLE_NATIVE_REF,
      BYTEVECTOR_IEEE_SINGLE_NATIVE_SET,
      BYTEVECTOR_IEEE_SINGLE_REF,
      BYTEVECTOR_IEEE_SINGLE_SET,
      BYTEVECTOR_LENGTH,
      BYTEVECTOR_P,
      BYTEVECTOR_S16_NATIVE_REF,
      BYTEVECTOR_S16_NATIVE_SET,
      BYTEVECTOR_S16_REF,
      BYTEVECTOR_S16_SET,
      BYTEVECTOR_S32_NATIVE_REF,
      BYTEVECTOR_S32_NATIVE_SET,
      BYTEVECTOR_S32_REF,
      BYTEVECTOR_S32_SET,
      BYTEVECTOR_S64_NATIVE_REF,
      BYTEVECTOR_S64_NATIVE_SET,
      BYTEVECTOR_S64_REF,
      BYTEVECTOR_S64_SET,
      BYTEVECTOR_S8_REF,
      BYTEVECTOR_S8_SET,
      BYTEVECTOR_SINT_LIST,
      BYTEVECTOR_SINT_REF,
      BYTEVECTOR_SINT_SET,
      BYTEVECTOR_U16_NATIVE_REF,
      BYTEVECTOR_U16_NATIVE_SET,
      BYTEVECTOR_U16_REF,
      BYTEVECTOR_U16_SET,
      BYTEVECTOR_U32_NATIVE_REF,
      BYTEVECTOR_U32_NATIVE_SET,
      BYTEVECTOR_U32_REF,
      BYTEVECTOR_U32_SET,
      BYTEVECTOR_U64_NATIVE_REF,
      BYTEVECTOR_U64_NATIVE_SET,
      BYTEVECTOR_U64_REF,
      BYTEVECTOR_U64_SET,
      BYTEVECTOR_U8_LIST,
      BYTEVECTOR_U8_REF,
      BYTEVECTOR_U8_SET,
      BYTEVECTOR_UINT_LIST,
      BYTEVECTOR_UINT_REF,
      BYTEVECTOR_UINT_SET,
      CAAAAR,
      CAAADR,
      CAAAR,
      CAADAR,
      CAADDR,
      CAADR,
      CAAR,
      CADAAR,
      CADADR,
      CADAR,
      CADDAR,
      CADDDR,
      CADDR,
      CADR,
      CALL_AS_JAAS_SUBJECT,
      CALL_CC,
      CALL_WITH_CURRENT_CONTINUATION,
      CALL_WITH_VALUES,
      CAR,
      CDAAAR,
      CDAADR,
      CDAAR,
      CDADAR,
      CDADDR,
      CDADR,
      CDAR,
      CDDAAR,
      CDDADR,
      CDDAR,
      CDDDAR,
      CDDDDR,
      CDDDR,
      CDDR,
      CDR,
      CEILING,
      CHAR_ALPHABETIC_P,
      CHAR_CI_EQ_P,
      CHAR_CI_GE_P,
      CHAR_CI_GT_P,
      CHAR_CI_LE_P,
      CHAR_CI_LT_P,
      CHAR_DOWNCASE,
      CHAR_EQ_P,
      CHAR_FOLDCASE,
      CHAR_GE_P,
      CHAR_GENERAL_CATEGORY,
      CHAR_GT_P,
      CHAR_INTEGER,
      CHAR_LE_P,
      CHAR_LOWER_CASE_P,
      CHAR_LT_P,
      CHAR_NUMERIC_P,
      CHAR_READY_P,
      CHAR_P,
      CHAR_TITLE_CASE_P,
      CHAR_TITLECASE,
      CHAR_UPCASE,
      CHAR_UPPER_CASE_P,
      CHAR_WHITESPACE_P,
      CLOSE_INPUT_PORT,
      CLOSE_OUTPUT_PORT,
      COLLECTION,
      COLLECTION_P,
      COMPLEX_P,
      CONS,
      CONS_A,
      COS,
      CURRENT_INPUT_PORT,
      CURRENT_OUTPUT_PORT,
      DATUM_SYNTAX,
      DELETE_FILE,
      DERIVED_ENVIRONMENT,
      DISPLAY,
      DIV,
      DIV_AND_MOD,
      DIV0,
      DIV0_AND_MOD0,
      DIVIDE,
      ENUM_SET_COMPLEMENT,
      ENUM_SET_CONSTRUCTOR,
      ENUM_SET_DIFFERENCE,
      ENUM_SET_EQ_P,
      ENUM_SET_INDEXER,
      ENUM_SET_INTERSECTION,
      ENUM_SET_LIST,
      ENUM_SET_MEMBER_P,
      ENUM_SET_PROJECTION,
      ENUM_SET_SUBSET_P,
      ENUM_SET_UNION,
      ENUM_SET_UNIVERSE,
      EOF_OBJECT_P,
      EQ,
      EQUAL_HASH,
      EQUAL_P,
      EQV_P,
      EQ_P,
      ERROR,
      EVAL,
      EVEN_P,
      EXACT_INEXACT,
      EXACT_INTEGER_SQRT,
      EXACT_P,
      EXP,
      EXPT,
      FILE_EXISTS_P,
      FINITE_P,
      FLOOR,
      FORCE,
      FORMAT,
      FREE_IDENTIFIER_EQ_P,
      GCD,
      GE,
      GENERATE_TEMPORARIES,
      GENSYM,
      GET_STRING_ALL,
      GET_VALUE,
      GT,
      HASHSET_ADD,
      HASHSET_CLEAR,
      HASHSET_CONTAINS,
      HASHSET_COPY,
      HASHSET_EQUIVALENCE_FUNCTION,
      HASHSET_HASH_FUNCTION,
      HASHSET_MUTABLE_P,
      HASHSET_P,
      HASHSET_REMOVE,
      HASHSET_SIZE,
      HASHSET_VALUES,
      HASHTABLE_CLEAR,
      HASHTABLE_CONTAINS_P,
      HASHTABLE_COPY,
      HASHTABLE_DELETE,
      HASHTABLE_ENTRIES,
      HASHTABLE_EQUIVALENCE_FUNCTION,
      HASHTABLE_HASH_FUNCTION,
      HASHTABLE_KEYS,
      HASHTABLE_MUTABLE_P,
      HASHTABLE_P,
      HASHTABLE_REF,
      HASHTABLE_SET,
      HASHTABLE_SIZE,
      HASHTABLE_UPDATE,
      HASHTABLE_VALUES,
      IDENTIFIER_P,
      IFNULL,
      IMPORT,
      IN_P,
      IN_PRIVILEGE_P,
      INEXACT_EXACT,
      INEXACT_P,
      INFINITE_P,
      INITIAL_ENVIRONMENT,
      INPUT_PORT_P,
      INSTANCE,
      INSTANCE_COLLECTION,
      INSTANCE_COLLECTION_P,
      INSTANCE_P,
      INTEGER_CHAR,
      INTEGER_P,
      INTERACTION_ENVIRONMENT,
      INVOCATION_CONTEXT,
      ITERATABLE_P,
      ITERATOR,
      LCM,
      LE,
      LENGTH,
      LIKE_P,
      LIST,
      LIST_FILES,
      LIST_P,
      LIST_REF,
      LIST_STRING,
      LIST_TAIL,
      LIST_VECTOR,
      LOAD,
      LOCALE_NAME,
      LOG,
      LT,
      MACRO_P,
      MAKE_BYTEVECTOR,
      MAKE_COLLECTION,
      MAKE_ENUMERATION,
      MAKE_EQ_HASHSET,
      MAKE_EQ_HASHTABLE,
      MAKE_EQV_HASHSET,
      MAKE_EQV_HASHTABLE,
      MAKE_HASHSET,
      MAKE_HASHTABLE,
      MAKE_STRING,
      MAKE_VARIABLE_TRANSFORMER,
      MAKE_VECTOR,
      MATCH,
      MAX,
      MEMBER,
      MEMQ,
      MEMV,
      MESSAGE,
      MIN,
      MINUS,
      MOD,
      MOD0,
      MODULO,
      MUL,
      NAN_P,
      NE,
      NEGATIVE_P,
      NEWLINE,
      NOT,
      NOW,
      NULL_P,
      NUMBER_P,
      NUMBER_STRING,
      OBJECT,
      ODD_P,
      OID,
      OPEN_INPUT_FILE,
      OPEN_INPUT_STRING,
      OPEN_OUTPUT_FILE,
      OPEN_OUTPUT_FORMATTER,
      OPEN_OUTPUT_STRING,
      OUTPUT_PORT_P,
      PAIR_P,
      PEEK_CHAR,
      PLUS,
      POSITIVE_P,
      PROCEDURE_P,
      QUOTIENT,
      RATIONAL_P,
      READ,
      READ_CHAR,
      REAL_P,
      REMAINDER,
      REMOVE,
      REMQ,
      REMV,
      REVERSE,
      ROUND,
      SET_CAR,
      SET_CDR,
      SET_CURRENT_INPUT_PORT,
      SET_CURRENT_OUTPUT_PORT,
      SIN,
      SINT_LIST_BYTEVECTOR,
      SQRT,
      STRING,
      STRING_AFFIX,
      STRING_APPEND,
      STRING_CI_EQ_P,
      STRING_CI_GE_P,
      STRING_CI_GT_P,
      STRING_CI_HASH,
      STRING_CI_LE_P,
      STRING_CI_LT_P,
      STRING_COPY,
      STRING_DOWNCASE,
      STRING_EMPTY_P,
      STRING_EOL_EQ_P,
      STRING_EQ_P,
      STRING_EXPAND,
      STRING_FOLDCASE,
      STRING_GE_P,
      STRING_GT_P,
      STRING_HASH,
      STRING_JOIN,
      STRING_LENGTH,
      STRING_LE_P,
      STRING_LIST,
      STRING_LT_P,
      STRING_MATCH,
      STRING_NUMBER,
      STRING_P,
      STRING_PATTERN,
      STRING_REF,
      STRING_REPLACE,
      STRING_SPLIT,
      STRING_SYMBOL,
      STRING_TITLECASE,
      STRING_TRIM,
      STRING_UPCASE,
      STRING_UTF16,
      STRING_UTF32,
      STRING_UTF8,
      SUBSTRING,
      SYMBOL_EQ_P,
      SYMBOL_HASH,
      SYMBOL_P,
      SYMBOL_STRING,
      SYNTAX_DATUM,
      SYS_ASSERT,
      SYS_AUDIT,
      SYS_CAST,
      SYS_CHECK_ACCESS,
      SYS_CHECK_PRIVILEGE,
      SYS_EQUAL_P,
      SYS_EQ_P,
      SYS_LOG,
      SYS_LOG_LOCALIZED,
      SYS_SET_BARRIER,
      SYS_SET_DIRTY,
      SYS_SET_NEW,
      SYS_TRY,
      SYS_TX_BEGIN,
      SYS_TX_COMMIT,
      SYS_TX_MANDATE,
      SYS_TX_MANDATE_NONE,
      SYS_TX_REQUIRE,
      SYS_TX_ROLLBACK,
      SYS_TX_SUSPEND,
      TAN,
      TIMESTAMP_P,
      TRUNCATE,
      U8_LIST_BYTEVECTOR,
      UINT_LIST_BYTEVECTOR,
      UTF16_STRING,
      UTF32_STRING,
      UTF8_STRING,
      VALUES,
      VECTOR,
      VECTOR_APPEND,
      VECTOR_FILL,
      VECTOR_LENGTH,
      VECTOR_LIST,
      VECTOR_P,
      VECTOR_REF,
      VECTOR_SET,
      VECTOR_SORT,
      WRITE,
      WRITE_CHAR,
      ZERO_P,
   };

   /**
    * Map of intrinsic function symbols to implementations: IntrinsicFunction[Symbol].
    */
   protected final static Lookup s_functionMap = new HashTab(s_functionArray.length);

   /**
    * Map of intrinsic function implementations to ordinal numbers: Integer[IntrinsicFunction].
    */
   protected final static Lookup s_ordinalMap = new HashTab(s_functionArray.length);

   static
   {
      for (int i = 0; i < s_functionArray.length; ++i)
      {
         IntrinsicFunction f = s_functionArray[i];

         if (f.getSymbol() != null)
         {
            s_functionMap.put(f.getSymbol(), f);
            s_ordinalMap.put(f, Primitive.createInteger(i));
         }
      }
   }

   // constructors

   /**
    * Prevents construction.
    */
   protected Intrinsic()
   {
   }

   // operations

   /**
    * Gets an intrinsic function by ordinal number.
    * @param nOrdinal The function ordinal number.
    * @return The intrinsic function.
    */
   public static IntrinsicFunction getFunction(int nOrdinal)
   {
      return s_functionArray[nOrdinal];
   }

   /**
    * Finds an intrinsic function by symbol.
    * @param symbol The function symbol.
    * @return The intrinsic function, or null if not found.
    */
   public static IntrinsicFunction findFunction(Symbol symbol)
   {
      return (IntrinsicFunction)s_functionMap.get(symbol);
   }

   /**
    * Gets an intrinsic function ordinal number.
    * @param fun The intrinsic function.
    * @return The function ordinal number.
    */
   public static int getOrdinal(IntrinsicFunction fun)
   {
      Object ordinal = s_ordinalMap.get(fun);

      if (ordinal == null)
      {
         return -1;
      }

      return ((Integer)ordinal).intValue();
   }

   /**
    * @return The intrinsic function iterator.
    */
   public static Iterator getFunctionIterator()
   {
      return s_ordinalMap.iterator();
   }

   /**
    * Determines if a value is an integer (including non-integer types holding an integer).
    * @param value The value to check.
    * @return True if the value is integer.
    */
   public static boolean isInteger(Object value)
   {
      if (value instanceof BigDecimal)
      {
         BigDecimal dec = (BigDecimal)value;

         return dec.compareTo(dec.setScale(0, BigDecimal.ROUND_DOWN)) == 0;
      }

      if (value instanceof Float || value instanceof Double)
      {
         double d = ((Number)value).doubleValue();

         return Math.rint(d) == d;
      }

      return value instanceof Number;
   }

   /**
    * Determines if the value is true as defined by the R4RS.
    * @param value The value to test.
    * @return True if the value is not the boolean #f.
    */
   public static boolean isTrue(Object value)
   {
      if (value instanceof Boolean)
      {
         return ((Boolean)value).booleanValue();
      }

      return true;
   }

   /**
    * Compares two objects for eq? equivalence.
    * @param obj1 The left object.
    * @param obj2 The right object.
    * @return True if the objects are eq? equivalent.
    */
   public static boolean eq(Object obj1, Object obj2)
   {
      return (obj1 == obj2 || (obj1 instanceof Symbol || obj1 instanceof Boolean) && obj1.equals(obj2));
   }

   /**
    * Compares two objects for eqv? equivalence.
    * @param obj1 The left object.
    * @param obj2 The right object.
    * @return True if the objects are eqv? equivalent.
    */
   public static boolean eqv(Object obj1, Object obj2)
   {
      if (obj1 == obj2)
      {
         return true;
      }

      if (obj1 instanceof Number)
      {
         if (obj2 instanceof Number)
         {
            if (obj1 instanceof BigDecimal)
            {
               if (obj2 instanceof BigDecimal)
               {
                  BigDecimal dec1 = (BigDecimal)obj1;
                  BigDecimal dec2 = (BigDecimal)obj2;

                  return (dec1.scale() < Primitive.MAX_SCALE) == (dec2.scale() < Primitive.MAX_SCALE) &&
                     dec1.compareTo(dec2) == 0;
               }

               if (obj2 instanceof Float || obj2 instanceof Double)
               {
                  BigDecimal dec1 = (BigDecimal)obj1;

                  return dec1.scale() >= Primitive.MAX_SCALE && dec1.doubleValue() == ((Number)obj2).doubleValue();
               }

               BigDecimal dec = (BigDecimal)obj1;

               return dec.scale() < Primitive.MAX_SCALE &&
                  dec.compareTo(BigDecimal.valueOf(((Number)obj2).longValue())) == 0;
            }

            if (obj1 instanceof Float || obj1 instanceof Double)
            {
               if (obj2 instanceof Float || obj2 instanceof Double)
               {
                  return ((Number)obj1).doubleValue() == ((Number)obj2).doubleValue();
               }

               if (obj2 instanceof BigDecimal)
               {
                  BigDecimal dec2 = (BigDecimal)obj2;

                  return dec2.scale() >= Primitive.MAX_SCALE && dec2.doubleValue() == ((Number)obj1).doubleValue();
               }

               return false;
            }

            if (obj2 instanceof BigDecimal)
            {
               BigDecimal dec = (BigDecimal)obj2;

               return dec.scale() < Primitive.MAX_SCALE &&
                  dec.compareTo(BigDecimal.valueOf(((Number)obj1).longValue())) == 0;
            }

            if (obj2 instanceof Float || obj2 instanceof Double)
            {
               return false;
            }

            return ((Number)obj1).longValue() == ((Number)obj2).longValue();
         }
      }
      else if (obj1 instanceof Boolean || obj1 instanceof Character || obj1 instanceof Timestamp || obj1 instanceof Symbol)
      {
         return obj1.equals(obj2);
      }

      return false;
   }

   /**
    * Compares two objects for equal? equivalence.
    * @param obj1 The left object.
    * @param obj2 The right object.
    * @return True if the objects are equal? equivalent.
    */
   public static boolean equal(Object obj1, Object obj2)
   {
      if (obj1 == obj2)
      {
         return true;
      }

      if (obj1 instanceof Number)
      {
         if (obj2 instanceof Number)
         {
            if (obj1 instanceof BigDecimal)
            {
               if (obj2 instanceof BigDecimal)
               {
                  BigDecimal dec1 = (BigDecimal)obj1;
                  BigDecimal dec2 = (BigDecimal)obj2;

                  return (dec1.scale() < Primitive.MAX_SCALE) == (dec2.scale() < Primitive.MAX_SCALE) &&
                     dec1.compareTo(dec2) == 0;
               }

               if (obj2 instanceof Float || obj2 instanceof Double)
               {
                  BigDecimal dec1 = (BigDecimal)obj1;

                  return dec1.scale() >= Primitive.MAX_SCALE && dec1.doubleValue() == ((Number)obj2).doubleValue();
               }

               BigDecimal dec = (BigDecimal)obj1;

               return dec.scale() < Primitive.MAX_SCALE &&
                  dec.compareTo(BigDecimal.valueOf(((Number)obj2).longValue())) == 0;
            }

            if (obj1 instanceof Float || obj1 instanceof Double)
            {
               if (obj2 instanceof Float || obj2 instanceof Double)
               {
                  return ((Number)obj1).doubleValue() == ((Number)obj2).doubleValue();
               }

               if (obj2 instanceof BigDecimal)
               {
                  BigDecimal dec2 = (BigDecimal)obj2;

                  return dec2.scale() >= Primitive.MAX_SCALE && dec2.doubleValue() == ((Number)obj1).doubleValue();
               }

               return false;
            }

            if (obj2 instanceof BigDecimal)
            {
               BigDecimal dec = (BigDecimal)obj2;

               return dec.scale() < Primitive.MAX_SCALE &&
                  dec.compareTo(BigDecimal.valueOf(((Number)obj1).longValue())) == 0;
            }

            if (obj2 instanceof Float || obj2 instanceof Double)
            {
               return false;
            }

            return ((Number)obj1).longValue() == ((Number)obj2).longValue();
         }
      }
      else if (obj1 instanceof Pair)
      {
         if (obj2 instanceof Pair)
         {
            Pair pair1 = (Pair)obj1;
            Pair pair2 = (Pair)obj2;

            for (;;)
            {
               if (!equal(pair1.getHead(), pair2.getHead()))
               {
                  return false;
               }

               obj1 = pair1.getTail();
               obj2 = pair2.getTail();

               if (obj1 instanceof Pair && obj2 instanceof Pair)
               {
                  pair1 = (Pair)obj1;
                  pair2 = (Pair)obj2;
               }
               else
               {
                  return equal(obj1, obj2);
               }
            }
         }
      }
      else if (obj1 != null && obj2 != null)
      {
         if (obj1.getClass().isArray())
         {
            if (obj2.getClass().isArray())
            {
               int nCount = Array.getLength(obj1);

               if (Array.getLength(obj2) != nCount)
               {
                  return false;
               }

               for (int i = 0; i < nCount; ++i)
               {
                  if (!equal(Array.get(obj1, i), Array.get(obj2, i)))
                  {
                     return false;
                  }
               }

               return true;
            }
         }
         else
         {
            return obj1.equals(obj2);
         }
      }

      return false;
   }

   /**
    * Opens a character stream reader on a given URL.
    * @param sURL The URL or file name to open.
    * @return The character stream reader.
    * @throws ScriptingException if the stream cannot be opened.
    */
   public static Reader openReader(String sURL) throws ScriptingException
   {
      try
      {
         InputStream istream;

         if (URLUtil.isURL(sURL))
         {
            istream = URLUtil.openStream(new URL(sURL));
         }
         else
         {
            istream = new FileInputStream(sURL);
         }

         return new BufferedReader(new InputStreamReader(istream, IOUtil.ENCODING));
      }
      catch (IOException e)
      {
         if (e instanceof FileNotFoundException || e instanceof MalformedURLException)
         {
            throw new ScriptingException("err.scripting.fileNotFound", new Object[]{sURL}, e);
         }

         throw new ScriptingException("err.scripting.io", null, e);
      }
   }

   /**
    * Opens a character stream writer on a given URL.
    * @param sURL The URL or file name to open.
    * @return The character stream writer.
    * @throws ScriptingException if the stream cannot be opened.
    */
   public static Writer openWriter(String sURL) throws ScriptingException
   {
      try
      {
         OutputStream ostream;

         if (URLUtil.isURL(sURL))
         {
            // Open using caching/etc. provided by Java library
            URLConnection con = new URL(sURL).openConnection();

            con.setDoInput(false);
            con.setDoOutput(true);

            if (con instanceof HttpURLConnection)
            {
               ((HttpURLConnection)con).setRequestMethod("POST");
            }

            con.connect();
            ostream = con.getOutputStream();
         }
         else
         {
            ostream = new FileOutputStream(sURL);
         }

         return new OutputStreamWriter(new BufferedOutputStream(ostream), IOUtil.ENCODING);
      }
      catch (IOException e)
      {
         throw new ScriptingException("err.scripting.io", null, e);
      }
   }

   /**
    * Loads Scheme expressions from the specified character stream.
    * @param reader The character stream reader.
    * @param sURL The scripting URL of the file being loaded. Can be null.
    * @param machine The VM where to execute the code.
    * @throws IOException if an I/O error occurs.
    * @throws ParserException, CompilerException, or ScriptingException,
    * if the loading fails.
    */
   public static void load(Reader reader, String sURL, Machine machine)
      throws IOException, ParserException, CompilerException, ScriptingException
   {
      GlobalEnvironment env = machine.getGlobalEnvironment();
      Parser parser = new SchemeParser(env);
      Compiler compiler = new Compiler();
      Lookup posMapSaved = env.getTextPositionMap();
      Lookup posMap = new IdentityHashTab();

      if (!reader.markSupported())
      {
         reader = new BufferedReader(reader);
      }

      reader = new TextPositionReader(reader, (sURL == null) ? "<reader>" : sURL);

      try
      {
         env.setTextPositionMap(posMap);

         for (;;)
         {
            Object expr = parser.parse(reader, posMap);

            if (expr == Parser.EOF)
            {
               break;
            }

            machine.invoke(compiler.compile(expr, posMap, machine, true), (Pair)null);
         }
      }
      finally
      {
         env.setTextPositionMap(posMapSaved);
      }
   }

   /**
    * Loads Scheme expressions from a file found by the class loader.
    * @param sURL The scripting URL of the file being loaded. Can be null.
    * @param sClassPathResource The path in the class path to get the resource from
    * @param machine The VM where the code is to be executed.
    * @throws IOException if an I/O error occurs.
    * @throws ParserException, CompilerException, or ScriptingException,
    * if the loading fails.
    */
   public static void load(String sURL, String sClassPathResource, Machine machine) throws IOException
   {
      InputStream istream = null;
      Reader reader = null;

      try
      {
         istream = URLUtil.openResource(Intrinsic.class, sClassPathResource);
         reader = new BufferedReader(new InputStreamReader(istream, IOUtil.ENCODING));
         load(reader, sURL, machine);
      }
      finally
      {
         IOUtil.close(reader);
         IOUtil.close(istream);
      }
   }

   /**
    * Loads Scheme expressions from the specified file and executes them sequentially.
    * Wraps IO exceptions in scripting exceptions.
    * @param sFileName The source file name.
    * @param machine The VM where code is to be executed.
    * @throws ParserException, CompilerException, or ScriptingException,
    * if the loading fails.
    */
   public static void load(String sFileName, Machine machine)
      throws ParserException, CompilerException, ScriptingException
   {
      Reader reader = null;

      try
      {
         reader = openReader(sFileName);
         load(reader, sFileName, machine);
      }
      catch (IOException e)
      {
         if (e instanceof FileNotFoundException || e instanceof MalformedURLException)
         {
            throw new ScriptingException("err.scripting.fileNotFound", new Object[]{sFileName}, e);
         }

         throw new ScriptingException("err.scripting.io", null, e);
      }
      finally
      {
         IOUtil.close(reader);
      }
   }

   /**
    * Deletes the file or directory denoted by the given pathname or URL.
    * A directory must be empty to be deleted successfully.
    * @param str The pathname or URL of the file or directory to delete.
    * @return True if the file or directory is successfully deleted.
    */
   public static void deleteFile(String str) throws ScriptingException
   {
      try
      {
         URL url = URLUtil.getURL(str);
         File file;

         if (url == null)
         {
            file = new File(str);
         }
         else
         {
            file = URLUtil.fileFromURL(url);
         }

         if (file == null || !file.delete())
         {
            throw new ScriptingException("err.scripting.fileDeletionFailed", new Object[]{str});
         }
      }
      catch (SecurityException e)
      {
         throw new ScriptingException("err.scripting.fileSystemOpFailed", new Object[]{str}, e);
      }
   }

   /**
    * Returns true if the file or directory denoted by the given pathname or URL exists.
    * @param str The pathname or URL of the file or directory.
    * @return True if the file or directory denoted by the given pathname or URL exists.
    */
   public static boolean fileExists(String str)
   {
      try
      {
         URL url = URLUtil.getURL(str);
         File file;

         if (url == null)
         {
            file = new File(str);
         }
         else
         {
            file = URLUtil.fileFromURL(url);
         }

         return file != null && file.exists();
      }
      catch (SecurityException e)
      {
         throw new ScriptingException("err.scripting.fileSystemOpFailed", new Object[]{str}, e);
      }
   }

   /**
    * If the argument is the pathname of an existing directory, returns all the files
    * and directories contained in this directory as abstract pathnames or URL strings,
    * depending on the type of the argument; otherwise returns null.
    * @param sDir The pathname or URL of the file or directory.
    * @return The pathnames or URLs (depending on the type of the argument string)
    * of the files or directories contained in this directory, or null if the argument
    * does not denote a directory. 
    */
   public static Object[] listFiles(String sDir)
   {
      try
      {
         URL url = URLUtil.getURL(sDir);

         if (url == null)
         {
            return new File(sDir).list();
         }

         Collection col = new ArrayList();

         URLUtil.addResourceNames(col, url, "", false);

         return col.toArray();
      }
      catch (SecurityException e)
      {
         throw new ScriptingException("err.scripting.fileSystemOpFailed", new Object[]{sDir}, e);
      }
      catch (MalformedURLException e)
      {
         throw new ScriptingException("err.scripting.fileSystemOpFailed", new Object[]{sDir}, e);
      }
      catch (IOException e)
      {
         throw new ScriptingException("err.scripting.fileSystemOpFailed", new Object[]{sDir}, e);
      }
   }

   /**
    * Creates a pretty formatter for Scheme expressions, using the given argument
    * as the underlying writer.
    * @param writer The underlying writer. May be null.
    * @param bLineSepEnabled True to automatically add a line break separator between code units.
    * @return A pretty formatter for Scheme expressions.
    */
   public static FormattingWriter createPrettyFormatter(Writer writer, boolean bLineSepEnabled)
   {
      if (writer == null)
      {
         writer = new StringWriter(256);
      }
      else if (writer instanceof PrettyWriter)
      {
         return (PrettyWriter)writer;
      }
      else if (writer instanceof FormattingWriter)
      {
         writer = ((FormattingWriter)writer).getWriter();
      }

      return new PrettyWriter(writer, bLineSepEnabled);
   }

   /**
    * Writes an S-expression to an output character stream.
    * @param expr The S-expression to write.
    * @param writer The character stream writer.
    * @param bDisplay True to display strings without quotes and number without suffixes.
    * @throws IOException in an I/O error occurs.
    */
   public static void write(Object expr, Writer writer, boolean bDisplay) throws IOException
   {
      if (writer instanceof FormattingWriter)
      {
         ((FormattingWriter)writer).formatUnit(expr, bDisplay);
      }
      else
      {
         (new FormattingWriter(writer, false)).formatUnit(expr, bDisplay);
      }
   }

   /**
    * Converts an S-expression to string.
    * @param expr The S-expression to convert.
    * @return The resulting string.
    */
   public static String toString(Object expr)
   {
      StringWriter writer = new StringWriter(128);

      try
      {
         write(expr, writer, false);
      }
      catch (IOException e)
      {
      }

      return writer.toString();
   }

   /**
    * Finds the greatest common divisor of two long numbers.
    * @param x The first number.
    * @param y The second number.
    * @return The greatest common divisor.
    */
   public static long gcd(long x, long y)
   {
      for (;;)
      {
         if (x == 0)
         {
            return y;
         }

         long l = x;

         x = y % x;
         y = l;
      }
   }

   /**
    * Verifies that certain number of arguments is passed.
    * @param nArgCount The argument count.
    * @param nReqCount The required count.
    * @param sName The function name.
    * @throws ScriptingException if the argument count is outside the allowed range.
    */
   public static void verifyArgCount(int nArgCount, int nReqCount, String sName) throws ScriptingException
   {
      if (nArgCount != nReqCount)
      {
         throw new ScriptingException(
            (nArgCount < nReqCount) ? "err.scripting.minArgCount" : "err.scripting.maxArgCount",
               new Object[]{sName,
                  Primitive.createInteger(nReqCount),
                  Primitive.createInteger(nArgCount)});
      }
   }

   /**
    * Sets the coerced type for a given combination of arguments.
    * @param array The destination array.
    * @param left The left argument type.
    * @param right The right argument type.
    * @param result The result type.
    */
   private static void setCoercedType(Primitive[] array, Primitive left, Primitive right, Primitive result)
   {
      array[left.getOrdinal() * Primitive.MAX_COUNT + right.getOrdinal()] = result;

      if (left != right)
      {
         array[right.getOrdinal() * Primitive.MAX_COUNT + left.getOrdinal()] = result;
      }
   }

   /**
    * Gets an iterator for the given collection, iteratable, array, or Pair.
    *
    * @param value The collection to iterate.
    * @return The iterator; null if no iterator can be created.
    */
   public static Iterator getIterator(Object value)
   {
      if (value == null)
      {
         return EmptyIterator.getInstance();
      }

      if (value instanceof Collection)
      {
         return ((Collection)value).iterator();
      }

      if (value instanceof Iterator)
      {
         return (Iterator)value;
      }

      if (value instanceof Iteratable)
      {
         return ((Iteratable)value).iterator();
      }

      if (value.getClass().isArray())
      {
         return new ArrayIterator(value);
      }

      if (value instanceof Pair)
      {
         return Pair.getIterator((Pair)value);
      }

      return null;
   }

   // inner classes

   /**
    * Implementation class for p-code intrinsic functions.
    */
   public static class PCodeIntrinsicFunction extends PCodeFunction implements IntrinsicFunction
   {
      // constants

      /**
       * Serialization version.
       */
      private final static long serialVersionUID = 7465665023711497202L;

      // associations

      /**
       * The function symbol.
       */
      private Symbol m_symbol;

      // constructors

      /**
       * Constructs the p-code function.
       * @param symbol The function symbol.
       * @param code The p-code array.
       * @param constants The constant array.
       * @param frame The closure frame.
       */
      public PCodeIntrinsicFunction(Symbol symbol, char[] code, Object[] constants, Object[] frame)
      {
         super(code, constants, frame);
         m_symbol = symbol;
      }

      // operations

      /**
       * @see nexj.core.scripting.IntrinsicFunction#getSymbol()
       */
      public Symbol getSymbol()
      {
         return m_symbol;
      }

      /**
       * @see nexj.core.scripting.IntrinsicFunction#isPCode()
       */
      public boolean isPCode()
      {
         return true;
      }

      /**
       * @see nexj.core.scripting.IntrinsicFunction#isDeterministic()
       */
      public boolean isDeterministic()
      {
         return false;
      }

      /**
       * @see nexj.core.scripting.IntrinsicFunction#generate(nexj.core.scripting.Compiler.PCodeBuffer, nexj.core.scripting.Compiler.CallInstruction)
       */
      public boolean generate(Compiler.PCodeBuffer buf, Compiler.CallInstruction instr)
      {
         return false;
      }
   }

   /**
    * Base class for Java intrinsic functions.
    */
   public abstract static class JavaIntrinsicFunction implements IntrinsicFunction
   {
      // operations

      /**
       * @see nexj.core.scripting.IntrinsicFunction#isPCode()
       */
      public boolean isPCode()
      {
         return false;
      }

      /**
       * @see nexj.core.scripting.IntrinsicFunction#isDeterministic()
       */
      public boolean isDeterministic()
      {
         return false;
      }

      /**
       * @see nexj.core.scripting.IntrinsicFunction#generate(nexj.core.scripting.Compiler.PCodeBuffer, nexj.core.scripting.Compiler.CallInstruction)
       */
      public boolean generate(Compiler.PCodeBuffer buf, Compiler.CallInstruction instr)
      {
         return false;
      }

      /**
       * Verifies that the argument count is within the allowed range.
       * @param nArgCount The argument count.
       * @param nMinCount The minimum allowed count.
       * @param nMaxCount The maximum allowed count.
       * @throws ScriptingException if the argument count is outside the allowed range.
       */
      protected final void verifyArgCount(int nArgCount, int nMinCount, int nMaxCount) throws ScriptingException
      {
         if (nArgCount < nMinCount)
         {
            throw new ScriptingException("err.scripting.minArgCount",
                  new Object[]{getSymbol().toString(),
                     Primitive.createInteger(nMinCount),
                     Primitive.createInteger(nArgCount)});
         }
         else if (nArgCount > nMaxCount)
         {
            throw new ScriptingException("err.scripting.maxArgCount",
                  new Object[]{getSymbol().toString(),
                     Primitive.createInteger(nMaxCount),
                     Primitive.createInteger(nArgCount)});
         }
      }

      /**
       * Verifies that certain number of arguments is passed.
       * @param nArgCount The argument count.
       * @param nReqCount The required count.
       * @throws ScriptingException if the argument count is outside the allowed range.
       */
      protected final void verifyArgCount(int nArgCount, int nReqCount) throws ScriptingException
      {
         if (nArgCount != nReqCount)
         {
            throw new ScriptingException(
               (nArgCount < nReqCount) ? "err.scripting.minArgCount" : "err.scripting.maxArgCount",
                  new Object[]{getSymbol().toString(),
                     Primitive.createInteger(nReqCount),
                     Primitive.createInteger(nArgCount)});
         }
      }

      /**
       * Verifies that at least a certain number of arguments is supplied.
       * @param nArgCount The argument count.
       * @param nMinCount The minimum allowed count.
       * @throws ScriptingException if the argument count is outside the allowed range.
       */
      protected final void verifyMinArgCount(int nArgCount, int nMinCount) throws ScriptingException
      {
         if (nArgCount < nMinCount)
         {
            throw new ScriptingException("err.scripting.minArgCount",
                  new Object[]{getSymbol().toString(),
                     Primitive.createInteger(nMinCount),
                     Primitive.createInteger(nArgCount)});
         }
      }

      /**
       * Verifies that the supplied index is non-negative.
       * @param nIndex The index.
       * @throws ScriptingException if the index is negative.
       */
      protected final void verifyIndex(int nIndex) throws ScriptingException
      {
         if (nIndex < 0)
         {
            throw new ScriptingException("err.scripting.negativeIndex",
                  new Object[]{getSymbol().getName()});
         }
      }

      /**
       * Verifies that the supplied index is within bounds.
       * @param nIndex The index.
       * @param nLength The length of the referenced array or vector.
       * @throws ScriptingException if the index is out of bounds.
       */
      protected final void verifyIndex(int nIndex, int nLength) throws ScriptingException
      {
         verifyIndex(nIndex);

         if (nIndex >= nLength)
         {
            throw new ScriptingException("err.scripting.badIndex",
                  new Object[]{Primitive.createInteger(nIndex)});
         }
      }

      /**
       * Verifies that the supplied index range is valid.
       * @param nStartIndex The start index (inclusive).
       * @param nEndIndex The end index (exclusive).
       * @throws ScriptingException if either of the indices is negative, or if
       * the start index is greater than the end index.
       */
      protected final void verifyIndexRange(int nStartIndex, int nEndIndex) throws ScriptingException
      {
         verifyIndex(nStartIndex);
         verifyIndex(nEndIndex);

         if (nStartIndex > nEndIndex)
         {
            throw new ScriptingException("err.scripting.invalidIndexRange",
                  new Object[]{getSymbol().getName()});
         }
      }

      /**
       * Verifies that the supplied index range is valid.
       * @param nStartIndex The start index.
       * @param nCount The number of elements in the index range.
       * @param nLength The length of the referenced array or vector. Assumed to be non-negative.
       * @throws ScriptingException if nStartIndex is negative, or if nCount is negative,
       * or if the index range goes out of bound.
       */
      protected final void verifyIndexRange(int nStartIndex, int nCount, int nLength) throws ScriptingException
      {
         verifyIndex(nStartIndex);

         if (nStartIndex + nCount > nLength || nCount < 0)
         {
            throw new ScriptingException("err.scripting.invalidIndexRange",
                  new Object[]{getSymbol().getName()});
         }
      }

      /**
       * Gets the coerced type for a given combination of argument types.
       * @param left The left argument type.
       * @param right The right argument type.
       * @return The coerced type.
       * @throws TypeMismatchException if the coercion is not supported.
       */
      protected Primitive getCoercedType(Primitive left, Primitive right)
      {
         Primitive type = COERCION_ARRAY[left.getOrdinal() * Primitive.MAX_COUNT + right.getOrdinal()];

         if (type == null)
         {
            throw new TypeMismatchException(getSymbol());
         }

         return type;
      }

      /**
       * Gets the character stream reader from the last argument.
       * @param nArg The ordinal number of the argument from which to get the reader.
       * @param nArgCount The total argument count.
       * @param machine The VM.
       * @throws ScriptingException if an error occurs.
       */
      protected final Reader getReader(int nArg, int nArgCount, Machine machine) throws ScriptingException
      {
         verifyArgCount(nArgCount, nArg, nArg + 1);

         Reader reader;

         if (nArg + 1 == nArgCount)
         {
            Object obj = machine.getArg(nArg, nArgCount);

            if (!(obj instanceof Reader))
            {
               throw new TypeMismatchException(getSymbol());
            }

            reader = (Reader)obj;
         }
         else
         {
            reader = machine.getReader();

            if (reader == null)
            {
               throw new ScriptingException("err.scripting.defaultReader");
            }
         }

         return reader;
      }

      /**
       * Gets the character stream writer from the last argument.
       * @param nArg The ordinal number of the argument from which to get the writer.
       * @param nArgCount The total argument count.
       * @param machine The VM.
       * @throws ScriptingException if an error occurs.
       */
      protected final Writer getWriter(int nArg, int nArgCount, Machine machine) throws ScriptingException
      {
         verifyArgCount(nArgCount, nArg, nArg + 1);

         Writer writer;

         if (nArg + 1 == nArgCount)
         {
            Object obj = machine.getArg(nArg, nArgCount);

            if (!(obj instanceof Writer))
            {
               throw new TypeMismatchException(getSymbol());
            }

            writer = (Writer)obj;
         }
         else
         {
            writer = machine.getWriter();

            if (writer == null)
            {
               throw new ScriptingException("err.scripting.defaultWriter");
            }
         }

         return writer;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return "intrinsic:" + getSymbol();
      }
   }

   /**
    * Base class for unary java intrinsic functions.
    */
   public abstract static class UnaryJavaIntrinsicFunction extends JavaIntrinsicFunction
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);
         machine.returnValue(invoke(machine.getArg(0, nArgCount)), nArgCount);

         return false;
      }

      protected abstract Object invoke(Object value);
   }

   /**
    * Base class for non-dispatching numeric unary java intrinsic functions.
    */
   public abstract static class SimpleNumericUnaryJavaIntrinsicFunction extends UnaryJavaIntrinsicFunction
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         try
         {
            verifyArgCount(nArgCount, 1);

            Object value = machine.getArg(0, nArgCount);

            if (value != null)
            {
               value = invoke(value);
            }

            machine.returnValue(value, nArgCount);

            return false;
         }
         catch (ClassCastException e)
         {
            throw new TypeMismatchException(getSymbol());
         }
      }

      public boolean isDeterministic()
      {
         return true;
      }
   }

   /**
    * Base class for dispatching numeric unary java intrinsic functions.
    */
   public abstract static class NumericUnaryJavaIntrinsicFunction extends UnaryJavaIntrinsicFunction
   {
      protected final Object invoke(Object value)
      {
         if (value == null)
         {
            return invokeNull();
         }

         switch (Primitive.primitiveOf(value).getOrdinal())
         {
            case Primitive.INTEGER_ORDINAL:
               return invoke(((Integer)value).intValue());

            case Primitive.LONG_ORDINAL:
               return invoke(((Long)value).longValue());

            case Primitive.DECIMAL_ORDINAL:
               return invoke((BigDecimal)value);

            case Primitive.FLOAT_ORDINAL:
               return invoke(((Float)value).floatValue());

            case Primitive.DOUBLE_ORDINAL:
               return invoke(((Double)value).doubleValue());

            default:
               throw new TypeMismatchException(getSymbol());
         }
      }

      protected abstract Object invoke(int n);
      protected abstract Object invoke(long l);
      protected abstract Object invoke(BigDecimal dec);
      protected abstract Object invoke(float f);
      protected abstract Object invoke(double d);
      protected abstract Object invokeNull();

      public boolean isDeterministic()
      {
         return true;
      }
   }

   /**
    * Base class for character unary java intrinsic functions.
    */
   protected abstract static class CharacterUnaryJavaIntrinsicFunction extends UnaryJavaIntrinsicFunction
   {
      protected final Object invoke(Object value)
      {
         if (value instanceof Character)
         {
            return invoke(((Character)value).charValue());
         }

         if (value == null)
         {
            return invokeNull();
         }

         throw new TypeMismatchException(getSymbol());
      }

      protected abstract Object invokeNull();
      protected abstract Object invoke(char ch);

      public boolean isDeterministic()
      {
         return true;
      }
   }

   /**
    * Implementation of c..r functions.
    */
   protected final static class CxrIntrinsicFunction extends UnaryJavaIntrinsicFunction
   {
      private Symbol m_symbol;
      private int m_nMask;

      public CxrIntrinsicFunction(Symbol symbol, int nMask)
      {
         m_symbol = symbol;
         m_nMask = nMask;
      }

      protected Object invoke(Object value)
      {
         for (int n = m_nMask; n > 1; n >>= 1)
         {
            if (value instanceof Pair)
            {
               if ((n & 1) == 0)
               {
                  value = ((Pair)value).getHead();
               }
               else
               {
                  value = ((Pair)value).getTail();
               }
            }
            else
            {
               throw new TypeMismatchException(getSymbol());
            }
         }

         return value;
      }

      public boolean isDeterministic()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return m_symbol;
      }
   }

   /**
    * Base class for list member intrinsic functions.
    */
   protected abstract static class ListMemberIntrinsicFunction extends JavaIntrinsicFunction
   {
      public final boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object obj = machine.getArg(0, nArgCount);
         Pair pair;

         for (Object list = machine.getArg(1, nArgCount); list != null; list = pair.getTail())
         {
            if (!(list instanceof Pair))
            {
               throw new ScriptingException("err.scripting.badList",
                  new Object[]{getSymbol().getName()});
            }

            pair = (Pair)list;

            if (match(obj, pair))
            {
               machine.returnValue(getResult(pair), nArgCount);

               return false;
            }
         }

         machine.returnValue(Boolean.FALSE, nArgCount);

         return false;
      }

      protected abstract boolean match(Object obj, Pair pair);
      protected abstract Object getResult(Pair pair);

      public boolean isDeterministic()
      {
         return true;
      }
   }

   /**
    * Base class for binary multiarg java intrinsic functions.
    */
   protected abstract static class BinaryJavaIntrinsicFunction extends JavaIntrinsicFunction
   {
      /**
       * Array of binary functions, indexed by primitive type ordinal number.
       * Null means that the function is not supported for a given type.
       */
      protected BinaryFunction[] m_functionArray = new BinaryFunction[Primitive.MAX_COUNT];

      protected BinaryJavaIntrinsicFunction()
      {
         for (int i = 0; i < Primitive.MAX_COUNT; ++i)
         {
            m_functionArray[i] = getBinaryFunction(Primitive.get(i));
         }
      }

      protected abstract BinaryFunction getBinaryFunction(Primitive type);

      public boolean isDeterministic()
      {
         return true;
      }
   }

   /**
    * Base class for numeric binary multiarg java intrinsic functions.
    */
   protected abstract static class NumericBinaryJavaIntrinsicFunction extends BinaryJavaIntrinsicFunction
   {
      protected char m_pcode;

      public NumericBinaryJavaIntrinsicFunction(char pcode)
      {
         m_pcode = pcode;
      }

      public final boolean invoke(int nArgCount, Machine machine)
      {
         if (nArgCount == 0)
         {
            machine.returnValue(getNoArgResult(), nArgCount);
         }
         else if (nArgCount == 1)
         {
            machine.returnValue(getOneArgResult(machine.getArg(0, nArgCount)), nArgCount);
         }
         else
         {
            Object left = machine.getArg(0, nArgCount);

            if (left != null)
            {
               Primitive leftType = Primitive.primitiveOf(left);
               Primitive rightType, resultType;
               BinaryFunction fun;
               Object right;

               for (int i = 1; i < nArgCount; ++i)
               {
                  right = machine.getArg(i, nArgCount);

                  if (right == null)
                  {
                     left = null;

                     break;
                  }

                  rightType = Primitive.primitiveOf(right);
                  resultType = getCoercedType(leftType, rightType);

                  if (leftType != resultType)
                  {
                     left = resultType.getConverter(leftType).invoke(left);
                     leftType = resultType;
                  }

                  if (rightType != resultType)
                  {
                     right = resultType.getConverter(rightType).invoke(right);
                  }

                  if ((fun = m_functionArray[resultType.getOrdinal()]) == null)
                  {
                     throw new TypeMismatchException(getSymbol());
                  }

                  left = fun.invoke(left, right);
               }
            }

            machine.returnValue(left, nArgCount);
         }

         return false;
      }

      protected abstract Object getNoArgResult();
      protected abstract Object getOneArgResult(Object arg);

      /**
       * @see nexj.core.scripting.IntrinsicFunction#generate(nexj.core.scripting.Compiler.PCodeBuffer, nexj.core.scripting.Compiler.CallInstruction)
       */
      public boolean generate(Compiler.PCodeBuffer buf, Compiler.CallInstruction instr)
      {
         if (instr.getArgCount() == 2)
         {
            buf.addCode(m_pcode);

            return true;
         }

         return false;
      }
   }

   /**
    * Base class for comparison multiarg java intrinsic functions.
    */
   protected abstract static class ComparisonJavaIntrinsicFunction extends BinaryJavaIntrinsicFunction
   {
      protected Boolean m_stopResult;
      protected Boolean m_endResult;
      protected char m_pcode;

      public ComparisonJavaIntrinsicFunction(boolean bStopResult, char pcode)
      {
         m_stopResult = Boolean.valueOf(bStopResult);
         m_endResult = Boolean.valueOf(!bStopResult);
         m_pcode = pcode;
      }

      public final boolean invoke(int nArgCount, Machine machine)
      {
         verifyMinArgCount(nArgCount, 2);

         Object left = machine.getArg(0, nArgCount);

         Primitive leftType = Primitive.primitiveOf(left);
         Primitive rightType, coercedType = null;
         BinaryFunction fun;
         Object right;

         for (int i = 1; i < nArgCount; ++i)
         {
            right = machine.getArg(i, nArgCount);
            rightType = Primitive.primitiveOf(right);

            if (leftType == null)
            {
               if (rightType != null)
               {
                  coercedType = rightType;
               }
            }
            else if (rightType == null)
            {
               coercedType = leftType;
            }
            else
            {
               coercedType = getCoercedType(leftType, rightType);
            }

            if (coercedType != null)
            {
               if (leftType != coercedType)
               {
                  left = coercedType.getConverter(leftType).invoke(left);
                  leftType = coercedType;
               }

               if (rightType != coercedType)
               {
                  right = coercedType.getConverter(rightType).invoke(right);
               }

               if ((fun = m_functionArray[coercedType.getOrdinal()]) == null)
               {
                  throw new TypeMismatchException(getSymbol());
               }

               if (fun.invoke(left, right) == m_stopResult)
               {
                  machine.returnValue(m_stopResult, nArgCount);

                  return false;
               }
            }
            else
            {
               if (nullCmp(left == null, right == null) == m_stopResult.booleanValue())
               {
                  machine.returnValue(m_stopResult, nArgCount);

                  return false;
               }
            }

            left = right;
         }

         machine.returnValue(m_endResult, nArgCount);

         return false;
      }

      protected abstract boolean nullCmp(boolean bLeftNull, boolean bRightNull);

      /**
       * @see nexj.core.scripting.IntrinsicFunction#generate(nexj.core.scripting.Compiler.PCodeBuffer, nexj.core.scripting.Compiler.CallInstruction)
       */
      public boolean generate(Compiler.PCodeBuffer buf, Compiler.CallInstruction instr)
      {
         if (instr.getArgCount() == 2)
         {
            buf.addCode(m_pcode);

            return true;
         }

         return false;
      }
   }

   /**
    * Base class for min and max multiarg java intrinsic functions.
    */
   protected abstract static class MinMaxJavaIntrinsicFunction extends BinaryJavaIntrinsicFunction
   {
      public final boolean invoke(int nArgCount, Machine machine)
      {
         verifyMinArgCount(nArgCount, 1);

         Object left = machine.getArg(0, nArgCount);
         Primitive leftType = Primitive.primitiveOf(left);
         Primitive rightType, coercedType = null;
         BinaryFunction fun;
         Object right;

         for (int i = 1; i < nArgCount; ++i)
         {
            right = machine.getArg(i, nArgCount);
            rightType = Primitive.primitiveOf(right);

            if (leftType == null)
            {
               if (rightType != null)
               {
                  coercedType = rightType;
               }
            }
            else if (rightType == null)
            {
               coercedType = leftType;
            }
            else
            {
               coercedType = getCoercedType(leftType, rightType);
            }

            if (coercedType != null)
            {
               if (leftType != coercedType)
               {
                  left = coercedType.getConverter(leftType).invoke(left);
                  leftType = coercedType;
               }

               if (rightType != coercedType)
               {
                  right = coercedType.getConverter(rightType).invoke(right);
               }

               if ((fun = m_functionArray[coercedType.getOrdinal()]) == null)
               {
                  throw new TypeMismatchException(getSymbol());
               }

               if (fun.invoke(left, right) == Boolean.TRUE)
               {
                  left = right;
               }
            }
            else
            {
               if (nullCmp(left == null, right == null))
               {
                  left = right;
               }
            }
         }

         machine.returnValue(left, nArgCount);

         return false;
      }

      protected abstract boolean nullCmp(boolean bLeftNull, boolean bRightNull);
   }

   /**
    * Base class for string comparison functions.
    */
   protected abstract static class StringComparisonJavaIntrinsicFunction extends JavaIntrinsicFunction
   {
      public final boolean invoke(int nArgCount, Machine machine)
      {
         verifyMinArgCount(nArgCount, 2);

         Object left = machine.getArg(0, nArgCount);

         if (left != null && !(left instanceof String))
         {
            throw new TypeMismatchException(getSymbol());
         }

         String sLeft = (String)left;

         for (int i = 1; i < nArgCount; ++i)
         {
            Object right = machine.getArg(i, nArgCount);

            if (right != null && !(right instanceof String))
            {
               throw new TypeMismatchException(getSymbol());
            }

            String sRight = (String)right;

            if (!cmp(sLeft, sRight))
            {
               machine.returnValue(Boolean.FALSE, nArgCount);

               return false;
            }

            sLeft = sRight;
         }

         machine.returnValue(Boolean.TRUE, nArgCount);

         return false;
      }

      protected abstract boolean cmp(String sLeft, String sRight);

      public boolean isDeterministic()
      {
         return true;
      }
   }

   /**
    * Base class for character comparison functions.
    */
   protected abstract static class CharacterComparisonJavaIntrinsicFunction extends JavaIntrinsicFunction
   {
      public final boolean invoke(int nArgCount, Machine machine)
      {
         verifyMinArgCount(nArgCount, 2);

         Object left = machine.getArg(0, nArgCount);

         if (!(left instanceof Character))
         {
            throw new TypeMismatchException(getSymbol());
         }

         char chLeft = ((Character)left).charValue();

         for (int i = 1; i < nArgCount; ++i)
         {
            Object right = machine.getArg(i, nArgCount);

            if (!(right instanceof Character))
            {
               throw new TypeMismatchException(getSymbol());
            }

            char chRight = ((Character)right).charValue();

            if (!cmp(chLeft, chRight))
            {
               machine.returnValue(Boolean.FALSE, nArgCount);

               return false;
            }

            chLeft = chRight;
         }

         machine.returnValue(Boolean.TRUE, nArgCount);

         return false;
      }

      protected abstract boolean cmp(char chLeft, char chRight);

      public boolean isDeterministic()
      {
         return true;
      }
   }

   /**
    * Base class for multiarg integer binary intrinsic functions.
    */
   protected abstract static class IntegerBinaryJavaIntrinsicFunction extends JavaIntrinsicFunction
   {
      public final boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount);

         if (nArgCount == 0)
         {
            machine.returnValue(getNoArgResult(), nArgCount);

            return false;
         }

         Object left = machine.getArg(0, nArgCount);

         if (left == null)
         {
            machine.returnValue(null, nArgCount);

            return false;
         }

         long lLeft;
         BigInteger intLeft;
         boolean bInexactLeft;
         int nOrdinal = 0;

         if (!(left instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         if (left instanceof Float || left instanceof Double)
         {
            Number num = (Number)left;

            lLeft = num.longValue();
            intLeft = null;
            bInexactLeft = true;
            nOrdinal = (left instanceof Float) ? Primitive.FLOAT_ORDINAL : Primitive.DOUBLE_ORDINAL;

            if (lLeft != num.doubleValue())
            {
               throw new TypeMismatchException(getSymbol());
            }
         }
         else if (left instanceof BigDecimal)
         {
            BigDecimal dec = (BigDecimal)left;

            if (dec.compareTo(dec.setScale(0, BigDecimal.ROUND_DOWN)) != 0)
            {
               throw new TypeMismatchException(getSymbol());
            }

            lLeft = 0;
            intLeft = dec.toBigInteger();
            bInexactLeft = (dec.scale() >= Primitive.MAX_SCALE);
            nOrdinal = Primitive.DECIMAL_ORDINAL;
         }
         else
         {
            lLeft = ((Number)left).longValue();
            intLeft = null;
            bInexactLeft = false;
            nOrdinal = (left instanceof Long) ? Primitive.LONG_ORDINAL : Primitive.INTEGER_ORDINAL;
         }

         BigInteger[] intResults = null;
         long[] lResults = null;
         int nResultCount = getResultCount();

         if (nResultCount > 1)
         {
            if (intLeft != null)
            {
               intResults = new BigInteger[nResultCount];
            }
            else
            {
               lResults = new long[nResultCount];
            }
         }

         if (nArgCount == 1)
         {
            if (intLeft != null)
            {
               if (nResultCount < 2)
               {
                  intLeft = getOneArgResult(intLeft);  
               }
               else
               {
                  getOneArgResult(intLeft, intResults);
               }
            }
            else
            {
               if (nResultCount < 2)
               {
                  lLeft = getOneArgResult(lLeft);
               }
               else
               {
                  getOneArgResult(lLeft, lResults);
               }
            }
         }
         else
         {
            for (int i = 1; i < nArgCount; ++i)
            {
               long lRight = 0;
               BigInteger intRight = null;
               boolean bInexactRight;
               Object right = machine.getArg(i, nArgCount);

               if (right == null)
               {
                  machine.returnValue(null, nArgCount);
               }

               if (!(right instanceof Number))
               {
                  throw new TypeMismatchException(getSymbol());
               }

               if (right instanceof Float || right instanceof Double)
               {
                  Number num = (Number)right;

                  lRight = num.longValue();
                  intRight = null;
                  bInexactRight = true;
                  nOrdinal = Math.max(nOrdinal, (right instanceof Float) ? Primitive.FLOAT_ORDINAL : Primitive.DOUBLE_ORDINAL);

                  if (lRight != num.doubleValue())
                  {
                     throw new TypeMismatchException(getSymbol());
                  }
               }
               else if (right instanceof BigDecimal)
               {
                  BigDecimal dec = (BigDecimal)right;

                  if (dec.compareTo(dec.setScale(0, BigDecimal.ROUND_DOWN)) != 0)
                  {
                     throw new TypeMismatchException(getSymbol());
                  }

                  lRight = 0;
                  intRight = dec.toBigInteger();
                  bInexactRight = (dec.scale() >= Primitive.MAX_SCALE);
                  nOrdinal = Math.max(nOrdinal, Primitive.DECIMAL_ORDINAL);
               }
               else
               {
                  lRight = ((Number)right).longValue();
                  intRight = null;
                  bInexactRight = false;
                  nOrdinal = Math.max(nOrdinal, (right instanceof Long) ? Primitive.LONG_ORDINAL : Primitive.INTEGER_ORDINAL);
               }

               if (intLeft != null)
               {
                  if (intRight == null)
                  {
                     if (bInexactRight)
                     {
                        lLeft = intLeft.longValue();
                        intLeft = null;
                     }
                     else
                     {
                        intRight = BigInteger.valueOf(lRight);
                     }
                  }
               }
               else if (intRight != null)
               {
                  if (bInexactLeft)
                  {
                     lRight = intRight.longValue();
                  }
                  else
                  {
                     intLeft = BigInteger.valueOf(lLeft);
                  }
               }

               bInexactLeft |= bInexactRight;

               if (intLeft != null)
               {
                  if (nResultCount < 2)
                  {
                     intLeft = compute(intLeft, intRight);
                  }
                  else
                  {
                     compute(intLeft, intRight, intResults);
                  }
               }
               else
               {
                  if (nResultCount < 2)
                  {
                     lLeft = compute(lLeft, lRight);
                  }
                  else
                  {
                     compute(lLeft, lRight, lResults);
                  }
               }
            }
         }

         if (nResultCount < 2)
         {
            if (intLeft != null)
            {
               if (bInexactLeft)
               {
                  left = new BigDecimal(intLeft).setScale(Primitive.MAX_SCALE);
               }
               else
               {
                  left = new BigDecimal(intLeft);
               }
            }
            else
            {
               if (bInexactLeft)
               {
                  if (nOrdinal == Primitive.DOUBLE_ORDINAL)
                  {
                     left = Primitive.createDouble((double)lLeft);
                  }
                  else
                  {
                     left = Primitive.createFloat((float)lLeft);
                  }
               }
               else
               {
                  if (nOrdinal == Primitive.LONG_ORDINAL)
                  {
                     left = Primitive.createLong(lLeft);
                  }
                  else
                  {
                     left = Primitive.createInteger((int)lLeft);
                  }
               }
            }

            machine.returnValue(left, nArgCount);

            return false;
         }

         Object[] results = new Object[nResultCount];

         if (intResults != null)
         {
            if (bInexactLeft)
            {
               for (int i = 0; i < nResultCount; i++)
               {
                  results[i] = new BigDecimal(intResults[i]).setScale(Primitive.MAX_SCALE);
               }
            }
            else
            {
               for (int i = 0; i < nResultCount; i++)
               {
                  results[i] = new BigDecimal(intResults[i]);
               }
            }
         }
         else if (bInexactLeft)
         {
            if (nOrdinal == Primitive.DOUBLE_ORDINAL)
            {
               for (int i = 0; i < nResultCount; i++)
               {
                  results[i] = Primitive.createDouble((double)lResults[i]);
               }
            }
            else
            {
               for (int i = 0; i < nResultCount; i++)
               {
                  results[i] = Primitive.createFloat((float)lResults[i]);
               }
            }
         }
         else
         {
            if (nOrdinal == Primitive.LONG_ORDINAL)
            {
               for (int i = 0; i < nResultCount; i++)
               {
                  results[i] = Primitive.createLong(lResults[i]);
               }
            }
            else
            {
               for (int i = 0; i < nResultCount; i++)
               {
                  results[i] = Primitive.createInteger((int)lResults[i]);
               }
            }
         }

         return machine.returnValues(results, nArgCount);
      }

      protected void verifyArgCount(int nArgCount)
      {
      }

      protected int getResultCount()
      {
         return 1;
      }

      protected Object getNoArgResult()
      {
         return null;
      }

      protected BigInteger getOneArgResult(BigInteger intValue)
      {
         return intValue;
      }

      protected void getOneArgResult(BigInteger intLeft, BigInteger[] intResults)
      {
      }

      protected long getOneArgResult(long lValue)
      {
         return lValue;
      }

      protected void getOneArgResult(long lLeft, long[] lResults)
      {
      }

      protected BigInteger compute(BigInteger intLeft, BigInteger intRight)
      {
         return null;
      }

      protected long compute(long lLeft, long lRight)
      {
         return -1L;
      }

      protected void compute(BigInteger intLeft, BigInteger intRight, BigInteger[] intResults)
      {
      }

      protected void compute(long lLeft, long lRight, long[] lResults)
      {
      }

      public boolean isDeterministic()
      {
         return true;
      }
   }

   /**
    * Base class for multiarg integer intrinsic functions.
    */
   protected abstract static class BitwiseJavaIntrinsicFunction extends JavaIntrinsicFunction
   {
      protected final static BigInteger NEGATIVE_ONE = BigInteger.valueOf(-1);

      /**
       * @return The given argument at index nOrdinal as an int, performing truncation if necessary.
       */
      protected int getIntegerArg(int nOrdinal, int nArgCount, Machine machine)
      {
         return getInteger(machine.getArg(nOrdinal, nArgCount));
      }

      /**
       * @return The given argument at index nOrdinal as a long, performing truncation if necessary.
       */
      protected long getLongArg(int nOrdinal, int nArgCount, Machine machine)
      {
         return getLong(machine.getArg(nOrdinal, nArgCount));
      }

      /**
       * @return The given argument at index nOrdinal as a BigInteger, performing truncation if necessary.
       */
      protected BigInteger getBigIntegerArg(int nOrdinal, int nArgCount, Machine machine)
      {
         return getBigInteger(machine.getArg(nOrdinal, nArgCount));
      }

      /**
       * @return The given argument as an int, performing truncation if necessary.
       */
      protected int getInteger(Object value)
      {
         int nValue;

         if (!(value instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         if (value instanceof Float || value instanceof Double)
         {
            Number num = (Number)value;

            nValue = num.intValue();

            if (nValue != num.doubleValue())
            {
               throw new TypeMismatchException(getSymbol());
            }
         }
         else if (value instanceof BigDecimal)
         {
            BigDecimal dec = (BigDecimal)value;

            if (dec.compareTo(dec.setScale(0, BigDecimal.ROUND_DOWN)) != 0)
            {
               throw new TypeMismatchException(getSymbol());
            }

            nValue = dec.intValue();
         }
         else
         {
            nValue = ((Number)value).intValue();
         }

         return nValue;
      }

      /**
       * @return The given argument as a long, performing truncation if necessary.
       */
      protected long getLong(Object value)
      {
         long lValue;

         if (!(value instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         if (value instanceof Float || value instanceof Double)
         {
            Number num = (Number)value;

            lValue = num.longValue();

            if (lValue != num.doubleValue())
            {
               throw new TypeMismatchException(getSymbol());
            }
         }
         else if (value instanceof BigDecimal)
         {
            BigDecimal dec = (BigDecimal)value;

            if (dec.compareTo(dec.setScale(0, BigDecimal.ROUND_DOWN)) != 0)
            {
               throw new TypeMismatchException(getSymbol());
            }

            lValue = dec.longValue();
         }
         else
         {
            lValue = ((Number)value).longValue();
         }

         return lValue;
      }

      /**
       * @return The given argument as a BigInteger, performing truncation if necessary.
       */
      protected BigInteger getBigInteger(Object value)
      {
         if (!(value instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         if (value instanceof BigDecimal)
         {
            BigDecimal dec = (BigDecimal)value;

            if (dec.compareTo(dec.setScale(0, BigDecimal.ROUND_DOWN)) != 0)
            {
               throw new TypeMismatchException(getSymbol());
            }

            return dec.toBigInteger();
         }

         Number num = (Number)value;
         long lValue = num.longValue();

         if (lValue != num.doubleValue())
         {
            throw new TypeMismatchException(getSymbol());
         }

         return BigInteger.valueOf(lValue);
      }

      /**
       * @return True if any of the arguments does not fit into a long.
       */
      protected static boolean hasBigDecimalArg(int nArgCount, Machine machine)
      {
         for (int i = 0; i < nArgCount; i++)
         {
            if (machine.getArg(i, nArgCount) instanceof BigDecimal)
            {
               return true;
            }
         }

         return false;
      }

      /**
       * Shifts nValue by nCount bits to the left.  
       * @param lValue The long value to shift.
       * @param nCount The number of bits to shift to the left. Assumed to be non-negative.
       * @return The long value resulting from the shift operation, or 0 when the shift distance
       * is greater than 63. 
       */
      protected static long shiftLeft(long lValue, int nCount)
      {
         // Java's shift distance for << is between 0 and 63
         return (nCount > 63) ? 0 : lValue << nCount;
      }

      /**
       * Shifts nValue by nCount bits to the right, preserving the sign.
       * @param lValue The long value to shift.
       * @param nCount The number of bits to shift to the right. Assumed to be non-negative.
       * @return The long value resulting from the shift operation, or 0 or -1 when the shift
       * distance is greater than 63 (depending on the sign of lValue).
       */
      protected static long shiftRight(long lValue, int nCount)
      {
         if (nCount > 63)
         {
            return (lValue < 0) ? -1L : 0L; // Java's shift distance for >> is between 0 and 63
         }

         return lValue >> nCount;
      }

      /**
       * Shifts nValue by nCount bits to the right, without preserving the sign.
       * @param lValue The long value to shift.
       * @param nCount The number of bits to shift to the right. Assumed to be non-negative.
       * @return The long value resulting from the shift operation, or 0 when the shift distance
       * is greater than 63.
       */
      protected static long unsignedShiftRight(long lValue, int nCount)
      {
         // Java's shift distance for >>> is between 0 and 63
         return (nCount > 63) ? 0 : lValue >>> nCount;
      }

      public boolean isDeterministic()
      {
         return true;
      }
   }

   /**
    * Base class for intrinsic functions that deal with bitwise bit fields.
    */
   protected abstract static class BitwiseBitFieldIntegerJavaIntrinsicFunction extends BitwiseJavaIntrinsicFunction
   {
      /**
       * Returns a new BigInteger represented by the bits in intValue from indices nStart
       * (inclusive) to nEnd (exclusive). The bits are indexed from right to left, so that
       * the least significant bit is at index 0.
       * @param intValue The exact integer from which the bits are drawn.
       * @param nStart The starting index (inclusive).
       * @param nEnd The ending index (exclusive).
       * @return The new BigInteger object represented by the specified bits.
       */
      protected static BigInteger getBitField(BigInteger intValue, int nStart, int nEnd)
      {
         return intValue.andNot(NEGATIVE_ONE.shiftLeft(nEnd)).shiftRight(nStart);
      }

      /**
       * Returns a long represented by the bits in lValue from indices nStart (inclusive)
       * to nEnd (exclusive). The bits are indexed from right to left, so that the least
       * significant bit is at index 0.
       * @param lValue The exact integer from which the bits are drawn.
       * @param nStart The starting index (inclusive).
       * @param nEnd The ending index (exclusive).
       * @return The long represented by the specified bits.
       */
      protected static long getBitField(long lValue, int nStart, int nEnd)
      {
         return shiftRight(lValue, nStart) & ~shiftLeft(-1, nEnd - nStart);
      }

      /**
       * Replaces the bits in intDest from indices nStart (inclusive) to nEnd (exclusive)
       * with the bits in intSource, starting from index 0, and returns the resulting
       * BigInteger object. The bits are indexed from right to left, so that the least
       * significant bit is at index 0.
       * @param intDest The exact integer to copy to.
       * @param nStart The starting index (inclusive).
       * @param nEnd The ending index (exclusive).
       * @param intSource The exact integer to copy from.
       * @return The new BigInteger object resulting from the bit replacement.
       */
      protected static BigInteger copyBitField(BigInteger intDest, int nStart, int nEnd, BigInteger intSource)
      {
         BigInteger intMask = NEGATIVE_ONE.shiftLeft(nStart).andNot(NEGATIVE_ONE.shiftLeft(nEnd));

         return intSource.shiftLeft(nStart).and(intMask).or(intDest.andNot(intMask));
      }

      /**
       * Replaces the bits in lDest from indices nStart (inclusive) to nEnd (exclusive)
       * with the bits in lSource, starting from index 0, and returns the resulting long.
       * The bits are indexed from right to left, so that the least significant bit is
       * at index 0.
       * @param lDest The long to copy to.
       * @param nStart The starting index (inclusive).
       * @param nEnd The ending index (exclusive).
       * @param lSource The long to copy from.
       * @return The long value resulting from the bit replacement.
       */
      protected static long copyBitField(long lDest, int nStart, int nEnd, long lSource)
      {
         long lMask = shiftLeft(-1, nStart) & ~shiftLeft(-1, nEnd);

         return (shiftLeft(lSource, nStart) & lMask) | (lDest & ~lMask);
      }

      /** 
       * Reverses the nBitCount least significant bits in intValue and returns a new
       * BigInteger object represented by these bits. The bits in intValue from indices
       * nBitCount and above are not included in the return value.
       * @param intValue The exact integer object whose bits are to be reversed.
       * @param nBitCount The number of least significant bits to reverse.
       * @return The new BigInteger object whose value is represented by the reversal of
       * the nBitCount least significant bits of intValue.
       */
      protected static BigInteger reverseBitField(BigInteger intValue, int nBitCount)
      {
         if (intValue.signum() != 0 && !intValue.equals(NEGATIVE_ONE))
         {
            byte[] byteArray = intValue.toByteArray();

            int i = 0;
            int k = byteArray.length - 1;

            while (i <= k)
            {
               byte nTemp = byteArray[i];

               byteArray[i] = reverseByte(byteArray[k]);
               byteArray[k] = reverseByte(nTemp);

               i++;
               k--;
            }

            intValue = new BigInteger(byteArray).shiftRight(byteArray.length * Byte.SIZE - nBitCount);
         }

         return intValue;
      }

      /**
       * Reverses the bits in the given byte and returns the new byte as an int.
       * @param n The byte whose bits are to be reversed.
       * @return The byte represented by the reversal of the bits in nByte.
       */
      protected static byte reverseByte(byte n)
      {
         n = (byte)(((n >>> 4) & 0x0F) | ((n & 0x0F) << 4));
         n = (byte)(((n >>> 2) & 0x33) | ((n & 0x33) << 2));

         return (byte)(((n >>> 1) & 0x55) | ((n & 0x55) << 1));
      }

      /**
       * Reverses the nBitCount least significant bits in lValue and returns the new
       * value as a long. The bits in lValue from indices nBitCount and above are not
       * included in the return value. The sign of the return value is determined by
       * the least significant bit in lValue. 
       * @param lValue The long whose bits are to be reversed.
       * @param nBitCount The number of least significant bits to reverse.
       * @return The long whose value is represented by the reversal of the nBitCount
       * least significant bits of lValue.
       */
      protected static long reverseBitField(long lValue, int nBitCount)
      {
         boolean bNegative = lValue < 0;

         lValue = ((lValue >>>  1) & 0x5555555555555555L) | ((lValue & 0x5555555555555555L) <<  1);
         lValue = ((lValue >>>  2) & 0x3333333333333333L) | ((lValue & 0x3333333333333333L) <<  2);
         lValue = ((lValue >>>  4) & 0x0F0F0F0F0F0F0F0FL) | ((lValue & 0x0F0F0F0F0F0F0F0FL) <<  4);
         lValue = ((lValue >>>  8) & 0x00FF00FF00FF00FFL) | ((lValue & 0x00FF00FF00FF00FFL) <<  8);
         lValue = ((lValue >>> 16) & 0x00000000FFFF0000L) | ((lValue & 0x00000000FFFF0000L) << 16) |
            (lValue >>> 48) | (lValue << 48);

         if (nBitCount > Long.SIZE)
         {
            lValue = shiftLeft(lValue, nBitCount - Long.SIZE);

            if (bNegative)
            {
               lValue |= ~(-1L << (nBitCount - Long.SIZE));
            }
         }
         else
         {
            lValue >>= (Long.SIZE - nBitCount);
         }

         return lValue;
      }
   }

   /**
    * Base class for bitwise-shifting intrinsic functions.
    */
   protected abstract static class BitwiseShiftIntegerJavaIntrinsicFunction extends BitwiseJavaIntrinsicFunction
   {
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         int nBitCount = getIntegerArg(1, nArgCount, machine);

         verifyBitCount(nBitCount);

         Object arg = machine.getArg(0, nArgCount);

         if (arg instanceof BigDecimal)
         {
            machine.returnValue(new BigDecimal(shift(getBigInteger(arg), nBitCount)), nArgCount);
         }
         else
         {
            machine.returnValue(Primitive.createLong(shift(getLong(arg), nBitCount)), nArgCount);
         }

         return false;
      }

      /**
       * Checks that the bit count meets the requirements of the intrinsic function. 
       * @param nBitCount The number of bits to shift.
       */
      protected void verifyBitCount(int nBitCount)
      {
      }

      /**
       * The shift function.
       * @param intValue The exact integer to shift.
       * @param nBitCount The number of bits to shift.
       * @return The value resulting from the shift operation.
       */
      protected abstract BigInteger shift(BigInteger intValue, int nBitCount);

      /**
       * The shift function 
       * @param lValue The exact integer to shift.
       * @param nBitCount The number of bits to shift.
       * @return The value resulting from the shift operation.
       */
      protected abstract long shift(long lValue, int nBitCount);
   }
   
   /**
    * Base class to construct new hash tables of different types.
    */
   protected abstract static class MakeHashObjectJavaIntrinsicFunction extends JavaIntrinsicFunction
   {
      /**
       * The invoke function for the case of 0 or 1 arguments.
       * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
       */
      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 0, 1);

         machine.returnValue((nArgCount == 0) ?
            createHashObject(8) :
            createHashObject(getCapacityArg(0, nArgCount, machine)),
            nArgCount);

         return false;
      }

      /**
       * Checks that the capacity argument is valid and returns it.
       * @param nCapacityIndex The 0-based index of the capacity argument.
       * @param nArgCount The argument count.
       * @param machine The machine.
       * @return The int value of the capacity argument.
       */
      protected final int getCapacityArg(int nCapacityIndex, int nArgCount, Machine machine)
      {
         Object value = machine.getArg(nCapacityIndex, nArgCount);

         if (!(value instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         int nCapacity = ((Number)value).intValue();

         if (nCapacity < 0)
         {
            throw new ScriptingException("err.scripting.negativeCount",
               new Object[]{getSymbol().getName()});
         }

         return nCapacity;
      }

      /**
       * Create a new hash object with an initial capacity argument.
       * @param nCapacity The initial capacity of the hash object.
       * @return The new hash object.
       */
      protected abstract Object createHashObject(int nCapacity);
   }

   /**
    * Base class for bytevector intrinsic functions.
    */
   protected abstract static class ByteVectorJavaIntrinsicFunction extends JavaIntrinsicFunction
   {
      // operations that handle arguments obtained from the VM

      /**
       * Verifies that the argument at nArgIndex is an exact integer and returns its int value.
       */
      protected int getExactIntArg(int nArgIndex, int nArgCount, Machine machine)
      {
         Object value = machine.getArg(nArgIndex, nArgCount);

         if (!(value instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return ((Number)value).intValue();
      }

      /**
       * Verifies that the argument at nArgIndex is a valid length argument and returns its int value.
       * @throws TypeMismatchException if the length value does not fit into a 32-bit integer.
       * @throws ScriptingException if length value is negative.
       */
      protected int getLengthArg(int nArgIndex, int nArgCount, Machine machine)
      {
         int nLength = getExactIntArg(nArgIndex, nArgCount, machine);

         if (nLength < 0)
         {
            throw new ScriptingException("err.scripting.negativeCount",
               new Object[]{getSymbol().getName()});
         }

         return nLength;
      }

      /**
       * Verifies that the argument at nArgIndex is a byte array and returns it.
       * @throws TypeMismatchException if the argument is not a byte array.
       */
      protected byte[] getByteVectorArg(int nArgIndex, int nArgCount, Machine machine)
      {
         Object obj = machine.getArg(nArgIndex, nArgCount);

         if (!(obj instanceof byte[]))
         {
            throw new TypeMismatchException(getSymbol());
         }

         return (byte[])obj;
      }

      /**
       * Verifies that the argument at nArgIndex is a byte array or a Binary object
       * with byte array data, and returns the byte array.
       * @throws TypeMismatchException if the argument is not a byte array or a Binary object.
       */
      protected byte[] getByteArray(int nArgIndex, int nArgCount, Machine machine)
      {
         Object obj = machine.getArg(nArgIndex, nArgCount);

         if (obj instanceof byte[])
         {
            return (byte[])obj;
         }

         if (obj instanceof Binary)
         {
            return ((Binary)obj).getData();
         }

         throw new TypeMismatchException(getSymbol());
      }

      // operations that handle bytes and byte arrays.

      /**
       * Copies n elements from nSrcArray, starting from nSrcIndex, to nDstArray,
       * starting from nDstIndex, in the same order as in nSrcArray.
       * @param nSrcArray The source array to copy from.
       * @param nSrcIndex The starting index in the source array.
       * @param nDstArray The destination array to copy to.
       * @param nDstIndex The starting index in the destination array.
       * @param n The number of elements to copy.
       * @return The modified destination array.
       * @throws ScriptingException if either of the given index ranges is invalid.
       */
      protected byte[] copy(byte[] nSrcArray, int nSrcIndex, byte[] nDstArray, int nDstIndex, int n)
      {
         try
         {
            System.arraycopy(nSrcArray, nSrcIndex, nDstArray, nDstIndex, n);
         }
         catch (Exception e)
         {
            // Provide a more detailed error message
            verifyIndexRange(nSrcIndex, n, nSrcArray.length);
            verifyIndexRange(nDstIndex, n, nDstArray.length);
         }

         return nDstArray;
      }

      /**
       * Returns the value in the byte array at the given index as a byte.
       * @param nArray The byte array to reference.
       * @param nIndex The index of the byte.
       * @return The byte value at nIndex.
       * @throws ScriptingException if nIndex is out of bounds.
       */
      protected byte getByte(byte[] nArray, int nIndex)
      {
         verifyIndex(nIndex, nArray.length);

         return nArray[nIndex];
      }

      /**
       * Replaces the byte in nArray at nIndex with nByte and returns
       * the old byte value.
       * @param nArray The byte array to modify.
       * @param nIndex The index of the byte to be replaced.
       * @param nByte The new byte value.
       * @return The previous byte value.
       * @throws ScriptingException if nIndex is out of bounds.
       */
      protected byte setByte(byte[] nArray, int nIndex, byte nByte)
      {
         verifyIndex(nIndex, nArray.length);

         byte nOldValue = nArray[nIndex];

         nArray[nIndex] = (byte)nByte;

         return nOldValue;
      }
   }

   /**
    * Base class for intrinsic functions that return an integer value composed from a byte vector,
    * using native endian byte order.
    */
   protected abstract static class ByteVectorNativeRefJavaIntrinsicFunction extends ByteVectorJavaIntrinsicFunction
   {
      public final boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object indexArg = machine.getArg(1, nArgCount);
         
         if (!(indexArg instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         int nIndex = ((Number)indexArg).intValue();
         int nByteCount = getByteCount();

         if ((nIndex & (nByteCount - 1)) != 0)
         {
            throw new ScriptingException("err.scripting.invalidNativeAlignment",
               new Object[]{getSymbol().getName(), Primitive.createInteger(nByteCount), indexArg});
         }

         byte[] nArray = getByteArray(0, nArgCount, machine);

         verifyIndexRange(nIndex, nByteCount, nArray.length);

         machine.returnValue(getNumber(nArray, nIndex), nArgCount);

         return false;
      }

      /**
       * @return The number of bytes that make up the integer value.
       */
      protected abstract int getByteCount();

      /**
       * @return The numerical value represented by the bytes in nArray, in native endian byte order,
       * starting from nIndex. The number of bytes that make up the return value is the integer returned
       * by {@link #getByteCount()}.
       */
      protected abstract Number getNumber(byte[] nArray, int nIndex);

      public boolean isDeterministic()
      {
         return true;
      }
   }

   /**
    * Base class for intrinsic functions that return an integer value composed from a byte vector.
    */
   protected abstract static class ByteVectorRefJavaIntrinsicFunction extends ByteVectorJavaIntrinsicFunction
   {
      public final boolean invoke(int nArgCount, Machine machine)
      {
         int nReqCount = getReqCount();

         verifyArgCount(nArgCount, nReqCount);

         int nByteCount = getByteCount();

         if (nByteCount == 0) // variable-size case
         {
            nByteCount = getLengthArg(3, nArgCount, machine);
         }

         byte[] nArray = getByteArray(0, nArgCount, machine);
         int nIndex = getExactIntArg(1, nArgCount, machine);

         verifyIndexRange(nIndex, nByteCount, nArray.length);

         Object endianness = machine.getArg(2, nArgCount);
         boolean bBigEndian;

         if (Symbol.BIG.equals(endianness))
         {
            bBigEndian = true;
         }
         else if (Symbol.LITTLE.equals(endianness))
         {
            bBigEndian = false;
         }
         else if (endianness instanceof Symbol)
         {
            throw new ScriptingException("err.scripting.invalidEndianness", new Object[]{endianness, getSymbol().getName()});
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(getNumber(nArray, nIndex, nByteCount, bBigEndian), nArgCount);

         return false;
      }

      /**
       * @return The number of required arguments.
       */
      protected int getReqCount()
      {
         return 3;
      }

      /**
       * @return The number of bytes that make up the integer value.
       */
      protected abstract int getByteCount();

      /**
       * Returns the numerical value represented by the bytes in nArray, in the specified endian byte order,
       * starting from nIndex. The number of bytes that make up the return value is the integer returned
       * by {@link #getByteCount()}.
       * @param nArray The array to reference.
       * @param nIndex The start index.
       * @param nByteCount The number of bytes that make up the return value.
       * @param bBigEndian Indicates whether to use big-endian (if true) or little-endian byte order (if false).
       * @return The numerical value represented by the specified bytes.
       */
      protected abstract Number getNumber(byte[] nArray, int nIndex, int nByteCount, boolean bBigEndian);

      public boolean isDeterministic()
      {
         return true;
      }
   }

   /**
    * Base class for intrinsic functions that modify a byte vector using an integer value,
    * interpreted in native endian byte order.
    */
   protected abstract static class ByteVectorNativeSetJavaIntrinsicFunction extends ByteVectorJavaIntrinsicFunction
   {
      public final boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 3);

         Object indexArg = machine.getArg(1, nArgCount);

         if (!(indexArg instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         int nIndex = ((Number)indexArg).intValue();
         int nByteCount = getByteCount();
         byte[] nArray = getByteVectorArg(0, nArgCount, machine);

         verifyIndexRange(nIndex, nByteCount, nArray.length);

         if ((nIndex & (nByteCount - 1)) != 0)
         {
            throw new ScriptingException("err.scripting.invalidNativeAlignment",
               new Object[]{getSymbol().getName(), Primitive.createInteger(nByteCount), indexArg});
         }

         Object value = machine.getArg(2, nArgCount);

         if (!(value instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         setNumber(nArray, nIndex, (Number)value);
         machine.returnValue(value, nArgCount);

         return false;
      }

      /**
       * @return The number of bytes that make up the integer value argument to set the byte array.
       * The number of bytes must be no less than the minimum number of bytes needed to represent
       * that integer.
       * @see #verifyValue
       */
      protected abstract int getByteCount();

      /**
       * Modify nArray by replacing the bytes starting at nIndex with the bytes that represent
       * value in native endian byte order.
       * @param nArray The byte array to be modified.
       * @param nIndex The index from which modification starts.
       * @param value The number whose byte representation is used to replace the bytes in nArray.
       */
      protected abstract void setNumber(byte[] nArray, int nIndex, Number value);
   }

   /**
    * Base class for intrinsic functions that modify a byte vector using an integer value.
    */
   protected abstract static class ByteVectorSetJavaIntrinsicFunction extends ByteVectorJavaIntrinsicFunction
   {
      public final boolean invoke(int nArgCount, Machine machine)
      {
         int nReqCount = getReqCount();

         verifyArgCount(nArgCount, nReqCount);

         byte[] nArray = getByteVectorArg(0, nArgCount, machine);
         int nIndex = getExactIntArg(1, nArgCount, machine);
         int nByteCount = getByteCount();

         if (nByteCount == 0) // variable-size case
         {
            nByteCount = getLengthArg(4, nArgCount, machine);
         }

         verifyIndexRange(nIndex, nByteCount, nArray.length);

         Object value = machine.getArg(2, nArgCount);
         Object endianness = machine.getArg(3, nArgCount);
         boolean bBigEndian;

         if (!(value instanceof Number))
         {
            throw new TypeMismatchException(getSymbol());
         }

         if (Symbol.BIG.equals(endianness))
         {
            bBigEndian = true;
         }
         else if (Symbol.LITTLE.equals(endianness))
         {
            bBigEndian = false;
         }
         else if (endianness instanceof Symbol)
         {
            throw new ScriptingException("err.scripting.invalidEndianness", new Object[]{endianness, getSymbol().getName()});
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         setNumber(nArray, nIndex, nByteCount, (Number)value, bBigEndian);
         machine.returnValue(value, nArgCount);

         return false;
      }

      /**
       * @return The number of required arguments.
       */
      protected int getReqCount()
      {
         return 4;
      }

      /**
       * @return The number of bytes that make up the integer value argument to set the byte array.
       * The number of bytes must be no less than the minimum number of bytes needed to represent
       * that integer.
       */
      protected abstract int getByteCount();

      /**
       * Modify nArray by replacing the bytes starting at nIndex with the bytes that represent the given value.
       * @param nArray The byte array to be modified.
       * @param nIndex The index from which modification starts.
       * @param nByteCount The number of bytes to replace.
       * @param value The number whose byte representation is used to replace the bytes in nArray.
       * @param bBigEndian Indicates whether to use big-endian (if true) or little-endian byte order (if false).
       */
      protected abstract void setNumber(byte[] nArray, int nIndex, int nByteCount, Number value, boolean bBigEndian);
   }

   public abstract static class ByteVectorListConversionJavaIntrinsicFunction extends ByteVectorJavaIntrinsicFunction
   {
      public final boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 3);

         byte[] nArray = getByteArray(0, nArgCount, machine);
         int nByteCount = getLengthArg(2, nArgCount, machine);

         if (nArray.length % nByteCount != 0)
         {
            throw new ScriptingException("err.scripting.byteVectorLength",
               new Object[]{getSymbol().getName(), Primitive.createInteger(nByteCount)});
         }

         Object endianness = machine.getArg(1, nArgCount);
         boolean bBigEndian;

         if (Symbol.BIG.equals(endianness))
         {
            bBigEndian = true;
         }
         else if (Symbol.LITTLE.equals(endianness))
         {
            bBigEndian = false;
         }
         else if (endianness instanceof Symbol)
         {
            throw new ScriptingException("err.scripting.invalidEndianness", new Object[]{endianness, getSymbol().getName()});
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         machine.returnValue(toNumberList(nArray, nByteCount, bBigEndian, isSigned()), nArgCount);

         return false;
      }

      /**
       * @return True if signed numbers are used.
       */
      protected abstract boolean isSigned();

      /**
       * Returns the integer list corresponding to the given byte array. Each consecutive sequence of
       * nCount bytes in the byte array makes up an integer in the integer list.
       * @param nArray The byte array to form the integer list.
       * @param nCount The number of bytes that form each integer. Must divide the length of nArray.
       * @param bBigEndian Indicates whether to use big-endian (if true) or little-endian byte order (if false).
       * @param bSigned Indicates whether to use the signed (if true) or unsigned representation (if false).
       * @return The integer list corresponding to the given byte array.
       */
      protected static Pair toNumberList(byte[] nArray, int nCount, boolean bBigEndian, boolean bSigned)
      {
         Pair tail = null;

         if (nCount < (Long.SIZE >> 3) + ((bSigned) ? 1 : 0))
         {
            for (int i = nArray.length - nCount; i > -1; i -= nCount)
            {
               tail = new Pair(Primitive.createLong(BinaryUtil.getLong(nArray, i, nCount, bBigEndian, bSigned)), tail);
            }
         }
         else
         {
            for (int i = nArray.length - nCount; i > -1; i -= nCount)
            {
               tail = new Pair(new BigDecimal(BinaryUtil.getBigInteger(nArray, i, nCount, bBigEndian, bSigned)), tail);
            }
         }

         return tail;
      }
   }

   /**
    * Base class for intrinsic functions that convert integer lists to byte vectors.
    */
   public abstract static class ListByteVectorConversionJavaIntrinsicFunction extends ByteVectorJavaIntrinsicFunction
   {
      public final boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 3);

         Object endianness = machine.getArg(1, nArgCount);

         if (!Symbol.BIG.equals(endianness) && !Symbol.LITTLE.equals(endianness))
         {
            if (endianness instanceof Symbol)
            {
               throw new ScriptingException("err.scripting.invalidEndianness",
                  new Object[]{endianness, getSymbol().getName()});
            }

            throw new TypeMismatchException(getSymbol());
         }

         Object head = machine.getArg(0, nArgCount);
         Pair pair;
         int nLength = 0;
         int nByteCount = getLengthArg(2, nArgCount, machine);

         for (Object next = head; next != null; next = pair.getTail())
         {
            if (!(next instanceof Pair))
            {
               throw new ScriptingException("err.scripting.badList", new Object[]{getSymbol().getName()});
            }

            pair = (Pair)next;

            Object num = pair.getHead();

            if (!(num instanceof Number))
            {
               throw new TypeMismatchException(getSymbol());
            }

            nLength++;
         }

         machine.returnValue((nLength == 0) ? EMPTY_BYTEARRAY :
            fromList((Pair)head, nByteCount, nLength * nByteCount, Symbol.BIG.equals(endianness), isSigned()), nArgCount);

         return false;
      }

      /**
       * @return True if signed numbers are used.
       */
      protected abstract boolean isSigned();

      /**
       * Creates a new byte array made up from the numerical values in the given list of exact integers.
       * @param head The list of integers that make up the byte array.
       * @param nCount The number of bytes that each integer in intList comprises.
       * @param nLength The length of the byte array.
       * @param bBigEndian Indicates whether to use big-endian (if true) or little-endian byte order (if false).
       * @param bSigned Indicates whether to use the signed (if true) or unsigned representation (if false).
       * @return The new byte array created from the integer list.
       */
      protected static byte[] fromList(Pair head, int nCount, int nLength, boolean bBigEndian, boolean bSigned)
      {
         byte[] nArray = new byte[nLength];

         if (nCount <= Long.SIZE >> 3)
         {
            for (int i = 0; i < nLength; i += nCount)
            {
               BinaryUtil.setLong(nArray, i, nCount, ((Number)head.getHead()).longValue(), bBigEndian);
               head = head.getNext();
            }
         }
         else
         {
            for (int i = 0; i < nLength; i += nCount)
            {
               BinaryUtil.setBigInteger(nArray, i, nCount, ((BigDecimal)head.getHead()).toBigInteger(), bBigEndian, bSigned);
               head = head.getNext();
            }
         }

         return nArray;
      }
   }

   /**
    * Base class to convert between a string and a byte vector. 
    */
   public abstract static class StringByteVectorConversionJavaIntrinsicFunction extends ByteVectorJavaIntrinsicFunction
   {
      /**
       * The byte order mark for UTF-32BE.
       */
      protected final static byte[] UTF32BE_BYTE_ORDER_MARK = new byte[]{0, 0, (byte)0xFE, (byte)0xFF};

      /**
       * The byte order mark for UTF-32LE.
       */
      protected final static byte[] UTF32LE_BYTE_ORDER_MARK = new byte[]{(byte)0xFF, (byte)0xFE, 0, 0};

      /**
       * The byte order mark for UTF-16BE.
       */
      protected final static byte[] UTF16BE_BYTE_ORDER_MARK = new byte[]{(byte)0xFE, (byte)0xFF};

      /**
       * The byte order mark for UTF-16LE.
       */
      protected final static byte[] UTF16LE_BYTE_ORDER_MARK = new byte[]{(byte)0xFF, (byte)0xFE};

      public final boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2, 3);

         byte[] nArray = getByteVectorArg(0, nArgCount, machine);
         Object endianness = machine.getArg(1, nArgCount);
         boolean bBigEndian = true;
         int nStartIndex = 0;

         if (Symbol.LITTLE.equals(endianness))
         {
            bBigEndian = false;
         }
         else if (!Symbol.BIG.equals(endianness))
         {
            if (endianness instanceof Symbol)
            {
               throw new ScriptingException("err.scripting.invalidEndianness", new Object[]{endianness, getSymbol().getName()});
            }

            throw new TypeMismatchException(getSymbol());
         }

         if (nArgCount == 2 || !isTrue(machine.getArg(2, nArgCount)))
         {
            byte[] nBOMArray = getBigEndianByteOrderMark();
            int nLength = nBOMArray.length;

            if (nArray.length >= nLength)
            {
               if (Binary.compare(nArray, nBOMArray, nLength, nLength) == 0)
               {
                  bBigEndian = true;
                  nStartIndex += nLength;
               }
               else if (Binary.compare(nArray, getLittleEndianByteOrderMark(), nLength, nLength) == 0)
               {
                  bBigEndian = false;
                  nStartIndex += nLength;
               }
            }
         }

         machine.returnValue(byteArrayToString(nArray, nStartIndex, bBigEndian), nArgCount);

         return false;
      }

      /**
       * @return The byte array whose bytes make up the big-endian byte order mark of the encoding in use.
       */
      protected abstract byte[] getBigEndianByteOrderMark();

      /**
       * @return The byte array whose bytes make up the little-endian byte order mark of the encoding in use.
       */
      protected abstract byte[] getLittleEndianByteOrderMark();

      /**
       * Convert the given byte array to a String according to the encoding in use,
       * starting from the given index.
       * @param nArray The byte array to convert.
       * @param nStartIndex The index into nArray from which conversion starts.
       * @param bBigEndian The byte array is to be in big-endian byte order, if true,
       * and in little-endian byte order, if false.
       * @return The resulting string.
       */
      protected abstract String byteArrayToString(byte[] nArray, int nStartIndex, boolean bBigEndian);

      public boolean isDeterministic()
      {
         return true;
      }
   }

   /**
    * Continuation implementation class.
    */
   public static class Continuation implements Function, Serializable
   {
      private final static long serialVersionUID = 2072443986653018581L;

      private Object[] m_stack;

      public Continuation(Object[] stack)
      {
         m_stack = stack;
      }

      /**
       * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
       */
      public boolean invoke(int nArgCount, Machine machine)
      {
         return machine.setContinuation(m_stack, nArgCount);
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return "intrinsic:continuation";
      }

      /**
       * @return The stack for this continuation.
       */
      public Object[] getStack()
      {
         return m_stack;
      }
   }

   /**
    * call-as-jaas-subject implementation class.
    */
   protected static class CallAsJAASSubjectIntrinsicFunction extends JavaIntrinsicFunction
   {
      private Symbol m_sym;

      public CallAsJAASSubjectIntrinsicFunction(Symbol sym)
      {
         m_sym = sym;
      }

      public boolean invoke(final int nArgCount, final Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         final Object fun = machine.getArg(0, nArgCount);
         Subject subject = (Subject) machine.getArg(1, nArgCount);

         if (fun instanceof Function)
         {
            try
            {
               Subject.doAs(subject, new PrivilegedExceptionAction()
               {
                  public Object run() throws Exception
                  {
                     Object result =  machine.invoke((Function)fun, (Object[])null);
                     machine.returnValue(result, nArgCount);
                     return result;
                  }
               });
            }
            catch (PrivilegedActionException e)
            {
               throw new ScriptingException("err.scripting.privilegedAction", e);
            }
         }
         else
         {
            throw new ScriptingException("err.scripting.funCall");
         }

         return false;
      }

      public Symbol getSymbol()
      {
         return m_sym;
      }
   }

   /**
    * Call/cc implementation class.
    */
   protected static class CallCCIntrinsicFunction extends JavaIntrinsicFunction
   {
      private Symbol m_sym;

      public CallCCIntrinsicFunction(Symbol sym)
      {
         m_sym = sym;
      }

      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 1);

         Object fun = machine.getArg(0, nArgCount);

         if (fun instanceof Function)
         {
            machine.pop(1);
            machine.push(new Continuation(machine.cloneStack()));
            machine.apply((Function)fun, 1);
         }
         else
         {
            throw new ScriptingException("err.scripting.funCall");
         }

         return true;
      }

      public boolean isPCode()
      {
         return true;
      }

      public Symbol getSymbol()
      {
         return m_sym;
      }
   }

   /**
    * Base class for the remove family of functions (remove, remq, remv).
    */
   public abstract static class RemoveJavaIntrinsicFunction extends JavaIntrinsicFunction
   {
      /**
       * Tests for equality.
       * @param left The object to test for equality.
       * @param right The object to test for equality.
       * @return True if left and right are considered the same.
       */
      public abstract boolean match(Object left, Object right);

      public boolean invoke(int nArgCount, Machine machine)
      {
         verifyArgCount(nArgCount, 2);

         Object obj = machine.getArg(0, nArgCount);
         Object arg = machine.getArg(1, nArgCount);

         if (arg == null)
         {
            machine.returnValue(null, nArgCount);
         }
         else if (arg instanceof Pair)
         {
            Pair prev = new Pair(null);
            Pair first = prev;
            Pair current = null;

            for (Pair pair = (Pair)arg; pair != null; pair = pair.getNext())
            {
               if (!match(obj, pair.getHead()))
               {
                  current = new Pair(pair.getHead());
                  prev.setTail(current);
                  prev = current;
               }
            }

            machine.returnValue(first.getTail(), nArgCount);
         }
         else if (arg instanceof Collection)
         {
            Collection col = (arg instanceof Ditto) ?
               (Collection)((Ditto)arg).ditto() :
               new ArrayList(((Collection)arg).size());

            for (Iterator itr = ((Collection)arg).iterator(); itr.hasNext();)
            {
               Object item = itr.next();

               if (!match(obj, item))
               {
                  col.add(item);
               }
            }

            machine.returnValue(col, nArgCount);
         }
         else if (arg.getClass().isArray())
         {
            int nCount = Array.getLength(arg);
            List list = new ArrayList(nCount);

            for (int i = 0; i != nCount; ++i)
            {
               Object item = Array.get(arg, i);

               if (!match(obj, item))
               {
                  list.add(item);
               }
            }

            machine.returnValue(list.toArray(), nArgCount);
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }

         return false;
      }
   }


   /**
    * Unimplemented intrinsic function.
    */
   protected static class UnimplementedIntrinsicFunction extends JavaIntrinsicFunction
   {
      private final Symbol m_sym;

      public UnimplementedIntrinsicFunction(Symbol sym)
      {
         m_sym = sym;
      }

      /**
       * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
       */
      public boolean invoke(int nArgCount, Machine machine)
      {
         throw new ScriptingException("err.scripting.unimplemented", new Object[]{getSymbol()});
      }

      public boolean isDeterministic()
      {
         return true;
      }

      /**
       * @see nexj.core.scripting.IntrinsicFunction#getSymbol()
       */
      public Symbol getSymbol()
      {
         return m_sym;
      }
   }

   /**
    * Functions which availability varies according to the installed libraries (client/server).
    */
   protected static class IsolatedFunctions
   {
      public IntrinsicFunction getInPrivilegePFunction()
      {
         return new UnimplementedIntrinsicFunction(Symbol.IN_PRIVILEGE_P);
      }

      public IntrinsicFunction getInstanceFunction()
      {
         return new UnimplementedIntrinsicFunction(Symbol.INSTANCE);
      }

      public IntrinsicFunction getInstanceCollectionFunction()
      {
         return new UnimplementedIntrinsicFunction(Symbol.INSTANCE_COLLECTION);
      }

      public IntrinsicFunction getInstanceCollectionPFunction()
      {
         return new UnimplementedIntrinsicFunction(Symbol.INSTANCE_COLLECTION_P);
      }

      public IntrinsicFunction getSysAuditFunction()
      {
         return new UnimplementedIntrinsicFunction(Symbol.SYS_AUDIT);
      }

      public IntrinsicFunction getSysCheckAccessFunction()
      {
         return new UnimplementedIntrinsicFunction(Symbol.SYS_CHECK_ACCESS);
      }

      public IntrinsicFunction getSysSetDirtyFunction()
      {
         return new UnimplementedIntrinsicFunction(Symbol.SYS_SET_DIRTY);
      }

      public IntrinsicFunction getSysSetNewFunction()
      {
         return new UnimplementedIntrinsicFunction(Symbol.SYS_SET_NEW);
      }

      public IntrinsicFunction getSysTxBeginFunction()
      {
         return new UnimplementedIntrinsicFunction(Symbol.SYS_TX_BEGIN);
      }

      public IntrinsicFunction getSysTxCommitFunction()
      {
         return new UnimplementedIntrinsicFunction(Symbol.SYS_TX_COMMIT);
      }

      public IntrinsicFunction getSysTxMandateFunction()
      {
         return new UnimplementedIntrinsicFunction(Symbol.SYS_TX_MANDATE);
      }

      public IntrinsicFunction getSysTxMandateNoneFunction()
      {
         return new UnimplementedIntrinsicFunction(Symbol.SYS_TX_MANDATE_NONE);
      }

      public IntrinsicFunction getSysTxRequireFunction()
      {
         return new UnimplementedIntrinsicFunction(Symbol.SYS_TX_REQUIRE);
      }

      public IntrinsicFunction getSysTxRollbackFunction()
      {
         return new UnimplementedIntrinsicFunction(Symbol.SYS_TX_ROLLBACK);
      }

      public IntrinsicFunction getSysTxSuspendFunction()
      {
         return new UnimplementedIntrinsicFunction(Symbol.SYS_TX_SUSPEND);
      }

      public IntrinsicFunction getObjectFunction()
      {
         return new UnimplementedIntrinsicFunction(Symbol.OBJECT);
      }

      public IntrinsicFunction getInstancePFunction()
      {
         return new JavaIntrinsicFunction()
         {
            /**
             * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
             */
            public boolean invoke(int nArgCount, Machine machine)
            {
               verifyArgCount(nArgCount, 2);

               Object obj = machine.getArg(0, nArgCount);
               Object clazz = machine.getArg(1, nArgCount);
               boolean bResult;

               if (clazz instanceof Type)
               {
                  if (obj instanceof Typed)
                  {
                     bResult = ((Type)clazz).isUpcast(((Typed)obj).getType());
                  }
                  else if (obj != null)
                  {
                     Class objClass = obj.getClass();
                     Primitive objType = Primitive.primitiveOf(objClass);

                     bResult = ((Type)clazz).isUpcast(objType) || clazz == Primitive.ANY;
                  }
                  else
                  {
                     bResult = false;
                  }
               }
               else if (clazz instanceof Class)
               {
                  bResult = (obj != null) && ((Class)clazz).isAssignableFrom(obj.getClass());
               }
               else
               {
                  throw new TypeMismatchException(getSymbol());
               }

               machine.returnValue(Boolean.valueOf(bResult), nArgCount);

               return false;
            }

            public Symbol getSymbol()
            {
               return Symbol.INSTANCE_P;
            }
         };
      }
   }
}
