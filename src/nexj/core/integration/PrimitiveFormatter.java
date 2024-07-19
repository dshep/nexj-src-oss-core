// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import java.sql.Timestamp;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.Format;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import nexj.core.meta.Primitive;
import nexj.core.meta.integration.FormatHolder;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.Localization;
import nexj.core.util.Lookup;
import nexj.core.util.StringTable;
import nexj.core.util.WeakHashTab;

/**
 * Formatter/parser for primitive message parts.
 */
public class PrimitiveFormatter
{
   // associations

   /**
    * A cache of Format instances, indexed by MessagePartMapping.
    */
   protected Lookup m_partFormatMap = new WeakHashTab();

   /**
    * The invocation context for strings, locale, and time zone.
    */
   protected InvocationContext m_context;

   // constructors

   /**
    * Constructs a new primitive message part formatter.
    * @param context The invocation context.
    */
   public PrimitiveFormatter(InvocationContext context)
   {
      m_context = context;
   }

   // operations

   /**
    * Formats the value according to the format string on its mapping.
    * @param value The value to format.
    * @param part The part being formatted.
    * @return The formatted string.
    */
   public String format(Object value, PrimitiveMessagePart part)
   {
      MessagePartMapping mapping = part.getMapping();
      Format format = (Format)m_partFormatMap.get(mapping);
      Object cvtValue = part.convertValue(value);

      if (format == null)
      {
         m_partFormatMap.put(mapping, format = getFormat(((FormatHolder)mapping).getFormat(), part));
      }

      return format.format(cvtValue);
   }

   /**
    * Parses the string according to the format string in its mapping.
    * @param sValue The string to parse.
    * @param part The part being parsed.
    * @return The parsed value.
    */
   public Object parse(String sValue, PrimitiveMessagePart part)
   {
      MessagePartMapping mapping = part.getMapping();
      Format format = (Format)m_partFormatMap.get(mapping);

      if (format == null)
      {
         m_partFormatMap.put(mapping, format = getFormat(((FormatHolder)mapping).getFormat(), part));
      }

      try
      {
         Object value = format.parseObject((sValue == null) ? "" : sValue);

         if (part.getType().getOrdinal() == Primitive.TIMESTAMP_ORDINAL)
         {
            return part.validateValue(new Timestamp(((Date)value).getTime()));
         }

         return part.convertValue(value);  // Format doesn't necessarily parse to the part type
      }
      catch (ParseException ex)
      {
         throw new IntegrationException("err.integration.primitive.parse",
            new Object[] {part.getFullPath()}, ex);
      }
   }

   /**
    * Gets the format pattern from a format.
    * @param sFormat The format; may be a string id or a pattern.
    * @param table The table in which the string id lookup shall be performed.
    * @return The format pattern.
    */
   public static String getFormatPattern(String sFormat, StringTable table)
   {
      String sFormatPattern = table.get(sFormat);

      if (sFormatPattern == sFormat)
      {
         String sLongId = Localization.PREFIXID + sFormat;

         sFormatPattern = table.get(sLongId);

         if (sFormatPattern == sLongId)
         {
            sFormatPattern = sFormat;
         }
      }

      return sFormatPattern;
   }

   /**
    * Gets the format to use for the message part.
    * @param sFormat The format string.
    * @param part The message part.
    * @return The format.
    */
   protected Format getFormat(String sFormat, PrimitiveMessagePart part)
   {
      String sFormatPattern = getFormatPattern(sFormat, m_context.getStringTable());
      Format format;

      switch (part.getType().getOrdinal())
      {
         case Primitive.TIMESTAMP_ORDINAL:
            SimpleDateFormat dtFormat = new SimpleDateFormat();

            dtFormat.setTimeZone(m_context.getTimeZone());
            dtFormat.applyPattern(sFormatPattern);

            format = dtFormat;

            break;

         case Primitive.DECIMAL_ORDINAL:
         case Primitive.DOUBLE_ORDINAL:
         case Primitive.FLOAT_ORDINAL:
         case Primitive.INTEGER_ORDINAL:
         case Primitive.LONG_ORDINAL:
            format = new DecimalFormat(sFormatPattern, new DecimalFormatSymbols(m_context.getLocale()));

            if (part.getType().getOrdinal() == Primitive.DECIMAL_ORDINAL)
            {
               ((DecimalFormat)format).setParseBigDecimal(true);
            }

            break;

         case Primitive.BOOLEAN_ORDINAL:
            format = new BooleanFormat(sFormatPattern);
            break;

         default:
            throw new IllegalStateException(); // should have been caught during metadata loading/validation
      }

      return format;
   }
}