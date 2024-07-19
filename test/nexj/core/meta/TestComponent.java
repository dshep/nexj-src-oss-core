// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import nexj.core.runtime.Initializable;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;

/**
 * A test component for a JUnit test case.
 */
public class TestComponent implements Initializable, InvocationContextAware
{
   private int m_nInitializationCount;
   /**
    * The invocation context.
    */
   private InvocationContext m_context;

   /**
    * The cookie.
    */
   private int m_nCookie = -1;

   /**
    * The byte object.
    */
   private Byte m_byteObj;

   /**
    * The byte.
    */
   private byte m_nByte;

   /**
    * The short object.
    */
   private Short m_shortObj;

   /**
    * The short.
    */
   private short m_nShort;

   /**
    * The integer object.
    */
   private Integer m_integerObj;

   /**
    * The integer.
    */
   private int m_nInt;

   /**
    * The long object.
    */
   private Long m_longObj;

   /**
    * The long.
    */
   private long m_lLong;

   /**
    * The float object.
    */
   private Float m_floatObj;

   /**
    * The float.
    */
   private float m_fFloat;

   /**
    * The double object.
    */
   private Double m_doubleObj;

   /**
    * The double.
    */
   private double m_dDouble;

   /**
    * The big decimal.
    */
   private BigDecimal m_decBigDecimal;

   /**
    * The big integer.
    */
   private BigInteger m_intBigInteger;
   
   /**
    * The character object.
    */
   private Character m_characterObj;

   /**
    * The character.
    */
   private char m_chCharacter;

   /**
    * The string.
    */
   private String m_sString;

   /**
    * The boolean object.
    */
   private Boolean m_booleanObj;

   /**
    * The boolean.
    */
   private boolean m_bBoolean;

   /**
    * The util date.
    */
   private java.util.Date m_utilDate;

   /**
    * The date.
    */
   private java.sql.Date m_dtDate;

   /**
    * The time.
    */
   private java.sql.Time m_tmTime;

   /**
    * The timestamp.
    */
   private java.sql.Timestamp m_tsTimestamp;

   /**
    * The primitive type.
    */
   private Primitive m_primitive;

   /**
    * The metaclass.
    */
   private Metaclass m_metaclass;

   /**
    * The type.
    */
   private Type m_type;

   /**
    * The component.
    */
   private Component m_component;

   /**
    * The instance.
    */
   private TestComponent m_instance;

   /**
    * The integer collection.
    */
   private List m_integerList = new ArrayList(8); // of type Integer

   /**
    * The test component collection.
    */
   private List m_testComponentList = new ArrayList(8); // of type TestComponent

   /**
    * The properties.
    */
   private Properties m_properties;

   // constructors
   
   public TestComponent()
   {
   }
   
   protected TestComponent(int nCookie)
   {
      m_nCookie = nCookie;
   }

   // operations

   /**
    * @see nexj.core.meta.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      ++m_nInitializationCount;
   }

   /**
    * @see nexj.core.meta.InvocationContextAware#setInvocationContext(nexj.core.meta.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }
   
   /**
    * @return The invocation context.
    */
   public InvocationContext getInvocationContext()
   {
      return m_context;
   }
   
   /**
    * Sets the cookie.
    * @param nCookie The cookie to set.
    */
   public void setCookie(int nCookie)
   {
      m_nCookie = nCookie;
   }

   /**
    * @return The cookie.
    */
   public int getCookie()
   {
      return m_nCookie;
   }
   
   public Object createTestComponent()
   {
      return new TestComponent(m_nCookie + 7);
   }
   
   /**
    * Sets the byte object.
    * @param byteObj The byte object to set.
    */
   public void setByteObj(Byte byteObj)
   {
      m_byteObj = byteObj;
   }

   /**
    * @return The byte object.
    */
   public Byte getByteObj()
   {
      return m_byteObj;
   }
   
   /**
    * Sets the byte.
    * @param nByte The byte to set.
    */
   public void setByte(byte nByte)
   {
      m_nByte = nByte;
   }

   /**
    * @return The byte.
    */
   public byte getByte()
   {
      return m_nByte;
   }
   
   /**
    * Sets the short object.
    * @param shortObj The short object to set.
    */
   public void setShortObj(Short shortObj)
   {
      m_shortObj = shortObj;
   }

   /**
    * @return The short object.
    */
   public Short getShortObj()
   {
      return m_shortObj;
   }
   
   /**
    * Sets the short.
    * @param nShort The short to set.
    */
   public void setShort(short nShort)
   {
      m_nShort = nShort;
   }

   /**
    * @return The short.
    */
   public short getShort()
   {
      return m_nShort;
   }
   
   /**
    * Sets the integer object.
    * @param integerObj The integer object to set.
    */
   public void setIntegerObj(Integer integerObj)
   {
      m_integerObj = integerObj;
   }

   /**
    * @return The integer object.
    */
   public Integer getIntegerObj()
   {
      return m_integerObj;
   }
   
   /**
    * Sets the integer.
    * @param nInt The integer to set.
    */
   public void setInteger(int nInt)
   {
      m_nInt = nInt;
   }

   /**
    * @return The integer.
    */
   public int getInteger()
   {
      return m_nInt;
   }
   
   /**
    * Sets the long object.
    * @param longObj The long object to set.
    */
   public void setLongObj(Long longObj)
   {
      m_longObj = longObj;
   }

   /**
    * @return The long object.
    */
   public Long getLongObj()
   {
      return m_longObj;
   }
   
   /**
    * Sets the long.
    * @param lLong The long to set.
    */
   public void setLong(long lLong)
   {
      m_lLong = lLong;
   }

   /**
    * @return The long.
    */
   public long getLong()
   {
      return m_lLong;
   }

   /**
    * Sets the float object.
    * @param floatObj The float object to set.
    */
   public void setFloatObj(Float floatObj)
   {
      m_floatObj = floatObj;
   }

   /**
    * @return The float object.
    */
   public Float getFloatObj()
   {
      return m_floatObj;
   }
   
   /**
    * Sets the float.
    * @param fFloat The float to set.
    */
   public void setFloat(float fFloat)
   {
      m_fFloat = fFloat;
   }

   /**
    * @return The float.
    */
   public float getFloat()
   {
      return m_fFloat;
   }
   
   /**
    * Sets the double object.
    * @param doubleObj The double object to set.
    */
   public void setDoubleObj(Double doubleObj)
   {
      m_doubleObj = doubleObj;
   }

   /**
    * @return The double object.
    */
   public Double getDoubleObj()
   {
      return m_doubleObj;
   }
   
   /**
    * Sets the double.
    * @param dDouble The double to set.
    */
   public void setDouble(double dDouble)
   {
      m_dDouble = dDouble;
   }

   /**
    * @return The double.
    */
   public double getDouble()
   {
      return m_dDouble;
   }
   
   /**
    * Sets the big decimal.
    * @param decBigDecimal The big decimal to set.
    */
   public void setBigDecimal(BigDecimal decBigDecimal)
   {
      m_decBigDecimal = decBigDecimal;
   }

   /**
    * @return The big decimal.
    */
   public BigDecimal getBigDecimal()
   {
      return m_decBigDecimal;
   }
   
   /**
    * Sets the big integer.
    * @param intBigInteger The big integer to set.
    */
   public void setBigInteger(BigInteger intBigInteger)
   {
      m_intBigInteger = intBigInteger;
   }

   /**
    * @return The big integer.
    */
   public BigInteger getBigInteger()
   {
      return m_intBigInteger;
   }

   /**
    * Sets the character object.
    * @param characterObj The character object to set.
    */
   public void setCharacterObj(Character characterObj)
   {
      m_characterObj = characterObj;
   }

   /**
    * @return The character object.
    */
   public Character getCharacterObj()
   {
      return m_characterObj;
   }

   /**
    * Sets the character.
    * @param chCharacter The character to set.
    */
   public void setCharacter(char chCharacter)
   {
      m_chCharacter = chCharacter;
   }

   /**
    * @return The character.
    */
   public char getCharacter()
   {
      return m_chCharacter;
   }
   
   /**
    * Sets the string.
    * @param sString The string to set.
    */
   public void setString(String sString)
   {
      m_sString = sString;
   }

   /**
    * @return The string.
    */
   public String getString()
   {
      return m_sString;
   }
   
   /**
    * Sets the boolean object.
    * @param booleanObj The boolean object to set.
    */
   public void setBooleanObj(Boolean booleanObj)
   {
      m_booleanObj = booleanObj;
   }

   /**
    * @return The boolean object.
    */
   public Boolean getBooleanObj()
   {
      return m_booleanObj;
   }
   
   /**
    * Sets the boolean.
    * @param bBoolean The boolean to set.
    */
   public void setBoolean(boolean bBoolean)
   {
      m_bBoolean = bBoolean;
   }

   /**
    * @return The boolean.
    */
   public boolean getBoolean()
   {
      return m_bBoolean;
   }
   
   /**
    * Sets the util date.
    * @param utilDate The util date to set.
    */
   public void setUtilDate(java.util.Date utilDate)
   {
      m_utilDate = utilDate;
   }

   /**
    * @return The util date.
    */
   public java.util.Date getUtilDate()
   {
      return m_utilDate;
   }

   /**
    * Sets the date.
    * @param dtDate The date to set.
    */
   public void setDate(java.sql.Date dtDate)
   {
      m_dtDate = dtDate;
   }

   /**
    * @return The date.
    */
   public java.sql.Date getDate()
   {
      return m_dtDate;
   }
   
   /**
    * Sets the time.
    * @param tmTime The time to set.
    */
   public void setTime(java.sql.Time tmTime)
   {
      m_tmTime = tmTime;
   }

   /**
    * @return The time.
    */
   public java.sql.Time getTime()
   {
      return m_tmTime;
   }
   
   /**
    * Sets the timestamp.
    * @param tsTimestamp The timestamp to set.
    */
   public void setTimestamp(java.sql.Timestamp tsTimestamp)
   {
      m_tsTimestamp = tsTimestamp;
   }

   /**
    * @return The timestamp.
    */
   public java.sql.Timestamp getTimestamp()
   {
      return m_tsTimestamp;
   }
   
   /**
    * Sets the primitive type.
    * @param primitive The primitive type to set.
    */
   public void setPrimitive(Primitive primitive)
   {
      m_primitive = primitive;
   }

   /**
    * @return The primitive type.
    */
   public Primitive getPrimitive()
   {
      return m_primitive;
   }   
   
   /**
    * Sets the metaclass.
    * @param metaclass The metaclass to set.
    */
   public void setMetaclass(Metaclass metaclass)
   {
      m_metaclass = metaclass;
   }

   /**
    * @return The metaclass.
    */
   public Metaclass getMetaclass()
   {
      return m_metaclass;
   }
   
   /**
    * Sets the type.
    * @param type The type to set.
    */
   public void setType(Type type)
   {
      m_type = type;
   }

   /**
    * @return The type.
    */
   public Type getType()
   {
      return m_type;
   }
   
   /**
    * Sets the component.
    * @param component The component to set.
    */
   public void setComponent(Component component)
   {
      m_component = component;
   }

   /**
    * @return The component.
    */
   public Component getComponent()
   {
      return m_component;
   }
   
   /**
    * Sets the instance.
    * @param instance The instance to set.
    */
   public void setInstance(TestComponent instance)
   {
      m_instance = instance;
   }

   /**
    * @return The instance.
    */
   public TestComponent getInstance()
   {
      return m_instance;
   }
   
   /**
    * Adds a new integer to the component.
    * @param integer The integer to add.
    */
   public void addInteger(Integer integer)
   {
      m_integerList.add(integer);
   }

   /**
    * Gets a integer by ordinal number.
    * @param nOrdinal The integer ordinal number (0-based).
    * @return The integer object.
    */
   public Integer getInteger(int nOrdinal)
   {
      return (Integer)m_integerList.get(nOrdinal);
   }

   /**
    * @return The integer count.
    */
   public int getIntegerCount()
   {
      return m_integerList.size();
   }

   /**
    * @return An iterator for the contained integer objects.
    */
   public Iterator getIntegerIterator()
   {
      return m_integerList.iterator();
   }
   
   /**
    * Adds a new test component to the component.
    * @param testComponent The test component to add.
    */
   public void addTestComponent(TestComponent testComponent)
   {
      m_testComponentList.add(testComponent);
   }

   /**
    * Gets a test component by ordinal number.
    * @param nOrdinal The test component ordinal number (0-based).
    * @return The test component object.
    */
   public TestComponent getTestComponent(int nOrdinal)
   {
      return (TestComponent)m_testComponentList.get(nOrdinal);
   }

   /**
    * @return The test component count.
    */
   public int getTestComponentCount()
   {
      return m_testComponentList.size();
   }

   /**
    * @return An iterator for the contained test component objects.
    */
   public Iterator getTestComponentIterator()
   {
      return m_testComponentList.iterator();
   }
   
   /**
    * Sets the properties.
    * @param properties The properties to set.
    */
   public void setProperties(Properties properties)
   {
      m_properties = properties;
   }

   /**
    * @return The properties.
    */
   public Properties getProperties()
   {
      return m_properties;
   }
   
   public int getInitializationCount()
   {
      return m_nInitializationCount;
   }
   
   public void testAction(Instance obj, double x, double y)
   {
   }
}
