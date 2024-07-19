// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0

package nexj.core.build;

import org.apache.tools.ant.filters.TokenFilter.ChainableReaderFilter;
import org.apache.tools.ant.BuildException;

import java.util.Locale;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.regex.PatternSyntaxException;



public class VersionFilter extends ChainableReaderFilter 
{

   public final static String PATTERN_PADDINGFORMAT = "\\d+(\\.\\d+)*";

   public final static int ACTION_GET = 0;
   public final static int ACTION_INCREMENT = 1; //also uses pad pattern
   public final static int ACTION_REPLACE = 2;
   public final static int ACTION_PAD = 3;

   /**
    * The user-provided Pattern to match the area in which we want to change the version 
    */ 
   private Pattern m_linePattern;
   
   /**
    * The user-provided group number the version is specified in, in the line pattern.
    */
   private int m_nVersionGroup = 1; // default value
   
   /**
    * The property to assign the new version number value
    */
   private String m_sProperty = null;
   
   /**
    * The value to set if the REPLACE action is used 
    */
   private String m_sValue = null;

   /**
    * The action for the filter to take. It can either 'Replace' the version
    * with a provided one, 'Get' it, or 'Increment' the last number.
    */
   private int m_nAction;

   /**
    * If true, increments the last component for version numbers only, instead
    * of default scheme of incrementing the last component if it is non-zero, 
    * 2nd last otherwise.
    */
   private boolean m_bFixedComponent = false;
   /**
    * A string to hold the padding pattern set by the user, in version number format
    * to correspond to the version number acted upon.
    */
   private String m_sPadPattern = null;
   
   public void setPattern(String sLinePattern)
   {
      try 
      {
         m_linePattern = Pattern.compile(sLinePattern, Pattern.MULTILINE | Pattern.DOTALL);
      }
      catch (PatternSyntaxException pse)
      {
         raiseBuildException("Incorrect regular expression syntax specified.", pse);
      }
   }
   
   public void setVersionGroup(String sGroupVersion)
   {
      try 
      {
         m_nVersionGroup = Integer.parseInt(sGroupVersion);
         if (m_nVersionGroup < 1) 
         {
            raiseBuildException("Invalid group number specified: \"" + m_nVersionGroup + "\".", null);
         }
      }
      catch (NumberFormatException nfe)
      {
         raiseBuildException("Group number specified was not a number: \"" + sGroupVersion + "\".", nfe);
      }
   }
   
   public void setProperty(String sProperty)
   {
      m_sProperty = sProperty;
   }
   
   public void setAction(String sAction)
   {
      String sLcAction = sAction.toLowerCase(Locale.ENGLISH);
      
      if ("get".equals(sLcAction))
      {
         m_nAction = ACTION_GET;
      }
      else if ("increment".equals(sLcAction))
      {
         m_nAction = ACTION_INCREMENT;
      }
      else if ("replace".equals(sLcAction))
      {
         m_nAction = ACTION_REPLACE;
      }
      else if ("pad".equals(sLcAction))
      {
         m_nAction = ACTION_PAD;
      }
      else
      {
         raiseBuildException("Unsupported action \"" + sAction + "\" specified.", null);
      }   
   }

   public void setValue(String sValue)
   {
      m_sValue = sValue;
   }   

   public void setFixed(boolean bFixed)
   {
      m_bFixedComponent = bFixed;
   }

   public void setPadPattern(String nPadPattern)
   {
      Matcher padPatternMatcher = Pattern.compile(PATTERN_PADDINGFORMAT).matcher(nPadPattern);

      if (!padPatternMatcher.matches())
      {
         raiseBuildException("\"" + nPadPattern + "\" does not match version number padding format \"" + PATTERN_PADDINGFORMAT + "\"", null);
      }

      m_sPadPattern = nPadPattern;
   }

   public String filter(String sTokenString)
   {
      Matcher lineMatcher;
      String sOriginalValue = null, sNewValue = null; // set to null to avoid "may not be initialized" compile warnings
      StringBuffer sb;

      if (m_nAction == ACTION_REPLACE && m_sValue == null) 
      {
         raiseBuildException("Replacement value not specified.", null);
      }
      if (m_nAction == ACTION_PAD && m_sPadPattern == null) 
      {
         raiseBuildException("Pad Pattern value not specified.", null);
      }
      if (m_linePattern == null) 
      {
         raiseBuildException("Pattern to match not specified.", null);   
      }
      
      // match the input token to the provided pattern
      lineMatcher = m_linePattern.matcher(sTokenString);
      sb = new StringBuffer();

      while (lineMatcher.find()) 
      { 
         // for each match to the line pattern
         int nLastValueIndex, nSecondLastValueIndex, nGroupIndex = -1; // set to -1 to avoid "may not be initialized" compile warnings

         try
         {
            sOriginalValue = lineMatcher.group(m_nVersionGroup);
            nGroupIndex = lineMatcher.start(m_nVersionGroup) - lineMatcher.start();
         }
         catch (IndexOutOfBoundsException ioobe)
         {
            raiseBuildException("Version capture group \"" + m_nVersionGroup + "\" does not exist.", null);   
         }

         // get new value of version
         switch(m_nAction)
         {
            case ACTION_PAD: // nothing happens here
            case ACTION_GET:
               sNewValue = sOriginalValue; // set this only because we may return it as a property
               break;
            case ACTION_INCREMENT: // get incremented version
               // Watch last two components
               // If last value is 0, increment 2nd last, otherwise increment last
               StringBuffer newValueBuffer = new StringBuffer(sOriginalValue.length() + 1);

               nLastValueIndex = sOriginalValue.lastIndexOf('.') + 1;

               if (nLastValueIndex < 1) 
               {
                  raiseBuildException("Capture group " + m_nVersionGroup + ", \"" + sOriginalValue + "\", must have more than one component.", null);
               }

               nSecondLastValueIndex = sOriginalValue.lastIndexOf('.', (nLastValueIndex - 2)) + 1; // search from before the last '.'

               try
               {
                  newValueBuffer.append(sOriginalValue.substring(0, nSecondLastValueIndex));
                  
                  if ((Integer.parseInt(sOriginalValue.substring(nLastValueIndex)) != 0) || m_bFixedComponent)
                  {
                     // increment last value
                     newValueBuffer.append(sOriginalValue.substring(nSecondLastValueIndex, nLastValueIndex));
                     newValueBuffer.append(Integer.parseInt(sOriginalValue.substring(nLastValueIndex)) + 1);
                  }
                  else
                  {
                     // increment 2nd last value
                     newValueBuffer.append(Integer.parseInt(sOriginalValue.substring(nSecondLastValueIndex, (nLastValueIndex - 1))) + 1);
                     newValueBuffer.append('.');
                     newValueBuffer.append(sOriginalValue.substring(nLastValueIndex));
                  }
               }
               catch (NumberFormatException nfe)
               {
                  raiseBuildException("String \"" + sOriginalValue + "\" specified was not a compatible version number. Last/only component must be an integer.", nfe);
               }
               
               sNewValue = newValueBuffer.toString();
               break;
            case ACTION_REPLACE: // use user-provided value
               sNewValue = m_sValue;
               break;
            default:
               throw new IllegalStateException("Invalid action value \"" + m_nAction + "\".");
         }

         // handle padding by traversing the version number parts in both the 
         // new value and the padding pattern.
         if (m_sPadPattern != null && (m_nAction == ACTION_PAD || m_nAction == ACTION_INCREMENT))
         {
            StringBuffer sbPaddedValue = new StringBuffer(sNewValue.length() * 3);
            int nPadLen = 0, nPartLen = 0;
            int nNewValuePartStart = 0;
            int nNewValuePartEnd = sNewValue.indexOf('.');

            int nPadPatPartStart = 0;
            int nPadPatPartEnd = m_sPadPattern.indexOf('.');

            while ((nPadPatPartEnd != -1) && (nNewValuePartEnd != -1))
            {
               nPadLen = Integer.parseInt(m_sPadPattern.substring(nPadPatPartStart, nPadPatPartEnd));
               nPartLen = nNewValuePartEnd - nNewValuePartStart;

               appendZeros(sbPaddedValue, (nPadLen - nPartLen));

               sbPaddedValue.append(sNewValue.substring(nNewValuePartStart, nNewValuePartEnd));
               sbPaddedValue.append(".");

               nNewValuePartStart = nNewValuePartEnd + 1;
               nPadPatPartStart = nPadPatPartEnd + 1;

               nNewValuePartEnd = sNewValue.indexOf(".", nNewValuePartStart);
               nPadPatPartEnd = m_sPadPattern.indexOf(".", nPadPatPartStart);
            }

            // last pad pattern and last version number part
            if ((nPadPatPartEnd == -1) && (nNewValuePartEnd == -1))
            {
               nPadLen = Integer.parseInt(m_sPadPattern.substring(nPadPatPartStart));
               nPartLen = sNewValue.length() - nNewValuePartStart;
            }            
            else if (nPadPatPartEnd == -1) // just last pad pattern part...
            {
               nPadLen = Integer.parseInt(m_sPadPattern.substring(nPadPatPartStart));
               nPartLen = nNewValuePartEnd - nNewValuePartStart;
            }
            else if (nNewValuePartEnd == -1) // ...or just last version number part...
            {
               nPadLen = Integer.parseInt(m_sPadPattern.substring(nPadPatPartStart, nPadPatPartEnd));
               nPartLen = sNewValue.length() - nNewValuePartStart;
            }
            else
            { 
               throw new IllegalStateException("Invalid padding pattern parse state.");
            }

            appendZeros(sbPaddedValue, (nPadLen - nPartLen));
            sbPaddedValue.append(sNewValue.substring(nNewValuePartStart, nNewValuePartStart + nPartLen));

            sNewValue = sbPaddedValue.toString();
         }

         // reassemble string
         switch(m_nAction)
         {
            case ACTION_GET: // no action, we don't change the string
               break;
            case ACTION_INCREMENT:
            case ACTION_PAD:
            case ACTION_REPLACE: // use user-provided value    
               lineMatcher.appendReplacement(sb, 
                  // assemble replacement string
                  lineMatcher.group(0).substring(0, nGroupIndex) + // the start of the match, minus version (group(m_nVersionGroup))
                  sNewValue + // new version string
                  lineMatcher.group(0).substring(nGroupIndex + sOriginalValue.length())); // rest of the string
               break;
            default:
               throw new IllegalStateException("Invalid action value \"" + m_nAction + "\".");
         }

         if ((m_sProperty != null) && (sNewValue != null)) 
         {
            getProject().setProperty(m_sProperty, sNewValue);
         }

      } // end while

      if (m_nAction == ACTION_GET) 
      {
         // return original string since we're only reading
         return sTokenString;
      }
      else
      {
         // otherwise return reassembled string
         lineMatcher.appendTail(sb);
         return sb.toString();
      }
   }

   /**
    * Appends zeroes to a given StringBuffer object.
    *
    * @param sb The StringBuffer to append zeroes to.
    * @param nCount The number of zeroes to append to the StringBuffer in int.
    */
   private void appendZeros(StringBuffer sb, int nCount)
   {
      int nIndex;
      
      for (nIndex = 0; nIndex < nCount; nIndex++)
      {
         sb.append("0");
      }
   }

   /**
    * From WixWriter.java
    * Throws a BuildException, printing additional details to stdout.
    * 
    * Where a BuildException will only convey the error message to the 
    * ant output log, raiseBuildException will print the stack
    * trace and indicate where in the java class our exception was raised
    * (line number and call flow). 
    * 
    * @param sMesssage The build error.
    * @param e The cause exception.
    * @throws BuildException to halt the build.
    */
   private void raiseBuildException(String sMesssage, Exception e) throws BuildException
   {
      log(sMesssage); // log to stdout
      if (e != null) { e.printStackTrace(); }
      throw new BuildException(sMesssage, e);
   }
}
