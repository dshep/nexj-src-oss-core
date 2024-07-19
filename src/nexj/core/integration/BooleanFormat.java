// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import java.text.FieldPosition;
import java.text.Format;
import java.text.ParseException;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.List;

import nexj.core.meta.Primitive;

/**
 * The boolean string format.
 */
public class BooleanFormat extends Format
{
   // associations

   /**
    * The serialization id.
    */
   private static final long serialVersionUID = 7708248684409190994L;

   /**
    * Array of true literals.
    */
   protected String[] m_sTrueArray;

   /**
    * Array of false literals.
    */
   protected String[] m_sFalseArray;

   // constructors

   /**
    * Constructs a new boolean value format.
    * @param sSpec The specification; format is e.g.: "Yes,True,#t;No,False,#f".
    */
   public BooleanFormat(String sSpec)
   {
      int nSep = sSpec.indexOf(';');

      if (nSep < 0)
      {
         nSep = sSpec.length();
      }

      ArrayList valueList = new ArrayList(5);

      parseCommaList(sSpec.substring(0, nSep), valueList);
      m_sTrueArray = (String[])valueList.toArray(new String[valueList.size()]);

      if (nSep < sSpec.length())
      {
         valueList.clear();
         parseCommaList(sSpec.substring(nSep + 1), valueList);
         m_sFalseArray = (String[])valueList.toArray(new String[valueList.size()]);
      }
   }

   // operations

   /**
    * @see java.text.Format#format(java.lang.Object, java.lang.StringBuffer, java.text.FieldPosition)
    */
   public StringBuffer format(Object obj, StringBuffer toAppendTo, FieldPosition pos)
   {
      Boolean bValue = Primitive.toBoolean(obj);

      return toAppendTo.append((bValue.booleanValue()) ? m_sTrueArray[0] : m_sFalseArray[0]);
   }

   /**
    * Overridden to handle empty strings.
    * @see java.text.Format#parseObject(java.lang.String)
    */
   public Object parseObject(String sSource) throws ParseException
   {
      ParsePosition pos = new ParsePosition(0);
      Object result = parseObject(sSource, pos);

      if (sSource != null && sSource.length() > 0 && pos.getIndex() == 0)
      {
         throw new ParseException("Format.parseObject(String) failed",
            pos.getErrorIndex());
      }

      return result;
   }

   /**
    * @see java.text.Format#parseObject(java.lang.String, java.text.ParsePosition)
    */
   public Object parseObject(String sSource, ParsePosition pos)
   {
      if (sSource == null)
      {
         return null;
      }

      int nStart = pos.getIndex();

      for (int i = 0; i < m_sTrueArray.length; i++)
      {
         if (sSource.regionMatches(nStart, m_sTrueArray[i], 0, m_sTrueArray[i].length()))
         {
            pos.setIndex(nStart + m_sTrueArray[i].length());

            return Boolean.TRUE;
         }
      }

      for (int i = 0; i < m_sFalseArray.length; i++)
      {
         if (sSource.regionMatches(nStart, m_sFalseArray[i], 0, m_sFalseArray[i].length()))
         {
            pos.setIndex(nStart + m_sFalseArray[i].length());

            return Boolean.FALSE;
         }
      }

      return null;
   }

   /**
    * Gets the true value literals.
    * @return The array of true values.
    */
   public String[] getTrueValues()
   {
      return m_sTrueArray;
   }

   /**
    * Gets the false value literals.
    * @return The array of false values.
    */
   public String[] getFalseValues()
   {
      return m_sFalseArray;
   }

   /**
    * Parses a comma-delimited string, adding the trimmed values to the given list.
    * @param sList The comma-delimited string to parse.
    * @param valueList The list to add the trimmed values.
    */
   private static void parseCommaList(String sList, List valueList)
   {
      int nStart = 0;

      while (nStart <= sList.length())
      {
         int nEnd = sList.indexOf(',', nStart);

         if (nEnd < 0)
         {
            nEnd = sList.length();
         }

         valueList.add(sList.substring(nStart, nEnd).trim());
         nStart = nEnd + 1;
      }
  }
}