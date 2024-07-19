package nexj.core.meta.integration.format.xml;

import nexj.core.util.StringUtil;

/**
 * Utility class to create valid names.
 */
public class NameResolver
{
   // operations

   /**
    * Generates a valid name from a suggested name. Valid names contain
    * letters, digits, and underscores and start with a letter or underscore.
    * @param sName The suggested name.
    * @return A valid name.
    */
   public String generateName(String sName)
   {
      sName = transform(sName);

      //replace invalid characters with underscores
      if (!StringUtil.isEmpty(sName))
      {
         final StringBuilder buf = new StringBuilder(sName);

         char c = sName.charAt(0);

         if (!Character.isLetter(c) && c != '_')
         {
            buf.setCharAt(0, '_');
         }

         for (int i = 1; i < sName.length(); ++i)
         {
            c = sName.charAt(i);

            if (!Character.isLetterOrDigit(c) && c != '_')
            {
               buf.setCharAt(i, '_');
            }
         }

         sName = buf.toString();
      }
      else
      {
         sName = "anonymous";
      }

      if (isValid(sName))
      {
         return sName;
      }

      for (int i = 2;; ++i)
      {
         String sMsgName = sName + i;

         if (isValid(sMsgName))
         {
            return sMsgName;
         }
      }
   }

   /**
    * Arbitrarily transforms the name.
    * @param sName A recommended name.
    * @return A transformed name.
    */
   protected String transform(String sName)
   {
      return sName;
   }

   /**
    * Checks to see if a name is valid.
    * @param sName The name to check.
    * @return True if the name is valid; false otherwise.
    */
   protected boolean isValid(String sName)
   {
      return true;
   }
}
