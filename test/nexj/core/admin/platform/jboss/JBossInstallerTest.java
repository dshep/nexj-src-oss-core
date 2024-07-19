// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.admin.platform.jboss;

import java.util.Arrays;
import java.util.Properties;

import nexj.core.admin.platform.jboss.JBossInstaller.RemoteResourceConnection.CommandSpec;

import junit.framework.ComparisonFailure;
import junit.framework.TestCase;

/**
 * JBoss installer tests.
 */
public class JBossInstallerTest extends TestCase
{
   // operations

   /**
    * Tests the command specification parser.
    */
   public void testCommandSpec() throws Exception
   {
      CommandSpec cmd;
      Properties props = new Properties();

      props.setProperty("123", "def");

      cmd = new CommandSpec("c:\\go 123abc");
      assertEqual(new String[] {"c:\\go", "123abc"}, cmd.getCommand(props));

      cmd = new CommandSpec("\"c:\\go\" 123 abc");
      assertEqual(new String[] {"c:\\go", "123", "abc"}, cmd.getCommand(props));

      cmd = new CommandSpec("\"c:\\go\" ${123} abc");
      assertEqual(new String[] {"c:\\go", "def", "abc"}, cmd.getCommand(props));

      cmd = new CommandSpec("c:\\go \"123 abc\"");
      assertEqual(new String[] {"c:\\go", "123 abc"}, cmd.getCommand(props));

      cmd = new CommandSpec("c:\\go \"\\\"123;abc\"");
      assertEqual(new String[] {"c:\\go", "\"123;abc"}, cmd.getCommand(props));

      cmd = new CommandSpec("c:\\go 123\";\"abc");
      assertEqual(new String[] {"c:\\go", "123;abc"}, cmd.getCommand(props));

      cmd = new CommandSpec("c:\\go 123\\\\;a*bc 123${123} \"Hello there\" \"\"");
      assertEqual(new String[] {"c:\\go", "123\\"}, cmd.getCommand(props));
      assertEquals("123def", cmd.trigger("I know my abc's", props));
      assertEquals("123def", cmd.trigger("I know my bc's", props));
      assertEquals("123def", cmd.trigger("I know my aaabc's", props));
      assertEquals("", cmd.trigger("This is a Hello there test", props));
   }

   /**
    * Asserts that two arrays match.
    * 
    * @param expectedArray The expected array.
    * @param actualArray The actual array.
    */
   public static void assertEqual(Object[] expectedArray, Object[] actualArray)
   {
      if (!Arrays.equals(expectedArray, actualArray))
      {
         throw new ComparisonFailure("Arrays do not match", Arrays.toString(expectedArray), Arrays.toString(actualArray));
      }
   }
}
