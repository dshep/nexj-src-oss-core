// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;

import nexj.core.util.IOUtil;

import junit.framework.TestCase;

public class SQLTemplateSubstReaderTest extends TestCase
{
   public void testSubstitution() throws IOException
   {
      StringBuffer ref = new StringBuffer();
      StringBuffer src = new StringBuffer();

      ref.append("value == 'value'").append("\n");
      ref.append("abc == 'abc'").append("\n");
      ref.append("abc == 'abc'").append("\n");
      ref.append("value == 'value'").append("\n");

      src.append("${exists} == 'value'").append("\n");
      src.append("${:abc} == 'abc'").append("\n");
      src.append("${empty:abc} == 'abc'").append("\n");
      src.append("${exists:abc} == 'value'").append("\n");

      SQLTemplateSubstReader reader = new SQLTemplateSubstReader(new StringReader(src.toString()))
      {
         protected Object getValue(String sKey, String sDefaultValue)
         {
            return "exists".equals(sKey) ? "value" : sDefaultValue; 
         }
      };

      StringWriter writer = new StringWriter();

      IOUtil.copy(writer, reader);

      assertEquals(ref.toString(), writer.toString());
   }
}