package nexj.core.build;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import nexj.core.integration.io.StreamInput;
import nexj.core.util.HTTP;
import nexj.core.util.HTTPClient;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.StringUtil;
import nexj.core.util.XMLUtil;
import nexj.core.util.HTTPClient.ResponseHandler;

/**
 * This class will facilitate REST commands issued by the Ant build.
 */
public class BuildStatus extends Task
{
   /** SimpleDateFormat for the build format timestamp */
   protected final static SimpleDateFormat BUILD_TIMESTAMP_FORMAT = new SimpleDateFormat("yyyyMMddHHmm");

   /** SimpleDateFormat for the non-numerical month representation */
   protected final static SimpleDateFormat MONTH_TEXT_FORMAT = new SimpleDateFormat("MMMMM");

   /** Regex Pattern for finding the section titles for Build Status wiki page contents */
   protected final static Pattern TITLE_REGEX_PATTERN = Pattern.compile("=\\w{3,9} \\d{1,2} \\d{4}=");

   /** API REST RPC response format */
   public final static String FORMAT = "xml";

   /** URL encoding */
   public final static String ENCODING = "UTF-8";

   /** The handle to the HTTPClient used for server accesses */
   protected HTTPClient m_httpClient;

   /** Domain for login account */
   protected String m_sDomain;

   /** Wiki page title */
   protected String m_sPageTitle = "Build Status";

   /** Wiki API Root URL */
   protected String m_sWikiURLRoot;

   /** User specified project status to be updated */
   protected String m_sProjectStatus;

   /** User specified project timestamp to be updated */
   protected String m_sProjectTime;

   /** User specified project name to be updated */
   protected String m_sProjectName;

   /** User specified project branch to be updated */
   protected String m_sProjectBranch;

   /** User specified project status background color to be updated */
   protected String m_sStatusColour;

   /** User specified wiki login name */
   protected String m_sLoginName;

   /** User specified wiki password */
   protected String m_sLoginPassword;

   /** Fail build on BuildStatus error flag */
   protected boolean m_bFailOnError = true;

   /** The holder for the section to edit */
   private String m_sEditSection;

   /**
    * @param sStatus the user specified status to be added to the wiki
    */
   public void setStatus(String sStatus)
   {
      m_sProjectStatus = sStatus;
   }

   /**
    * @param sTimeStamp the user specified build timestamp to update
    */
   public void setTimestamp(String sTimeStamp)
   {
      m_sProjectTime = sTimeStamp;
   }

   /**
    * @param sProject the user specified project to update
    */
   public void setProject(String sProject)
   {
      m_sProjectName = sProject;
   }

   /**
    * @param sBranch the user specified project branch to update
    */
   public void setBranch(String sBranch)
   {
      m_sProjectBranch = sBranch;
   }

   /**
    * Optional. If not specified the status cell will be given no background color.
    * HTML readable and Hex colors are valid. Hex colors need to be prefixed with a pound sign '#'.
    *
    * @param sColourCode the user specified color, if any, to add to the status update
    */
   public void setColour(String sColourCode)
   {
      m_sStatusColour = sColourCode;
   }

   /**
    * @param sUsername the login username to be used for the wiki API
    */
   public void setUsername(String sUsername)
   {
      m_sLoginName = sUsername;
   }

   /**
    * @param sPassword the login password to be used for the wiki API
    */
   public void setPassword(String sPassword)
   {
      m_sLoginPassword = sPassword;
   }

   /**
    * Optional. If a user domain is not specified the default "NEXJSYSTEMS.LOCAL" is used.
    *
    * @param sDomain the login user domain to be used for the wiki API
    */
   public void setDomain(String sDomain)
   {
      m_sDomain = sDomain;
   }

   /**
    * Optional. If no root is specified the default NexJ Company wiki at "https://wiki.nexj.com/wiki" is used.
    *
    * @param sWikiRoot the root of the wiki to update. The root URL must include the path to index or api page. E.G.: "https://www.wiki.com/wiki" or "https://www.wiki.com/w".
    */
   public void setWikiRoot(String sWikiRoot)
   {
      m_sWikiURLRoot = sWikiRoot;
   }

   /**
    * Optional. If no page title is specified the default page "Build Status" is used.
    *
    * @param sPageTitle the wiki page to update.
    */
   public void setPageTitle(String sPageTitle)
   {
      m_sPageTitle = sPageTitle;
   }

   /**
    * Optional. Flag to indicate whether to halt the build on any error.
    * The default value is true.
    *
    * @param bFailOnError whether the build should fail on a BuildStatus error.
    */
   public void setFailOnError(boolean bFailOnError)
   {
      m_bFailOnError = bFailOnError;
   }

   /**
    * @see org.apache.tools.ant.Task#execute()
    */
   public void execute() throws BuildException
   {
      String sEditToken, sPageContents, sModifiedPageContents;

      checkAttributes();

      try
      {
         m_httpClient = new HTTPClient();

         attemptLogin(); // login

         sEditToken = getEditToken(); // get edit token for target page
         log("Edit token: " + sEditToken, Project.MSG_DEBUG);

         sPageContents = getContents(); // get wiki text contents of target page
         log("Contents of page:\n" + sPageContents, Project.MSG_DEBUG);

         sModifiedPageContents = generatedModifiedPage(sPageContents); // generate modified wiki text with new status update
         log("Modified section \"" + m_sEditSection + "\":\n" + sModifiedPageContents, Project.MSG_DEBUG);

         try
         {
            editWikiPage(sEditToken, sModifiedPageContents, 
                  "Updating build \"" + m_sProjectTime + ", " + m_sProjectName + ", " + m_sProjectBranch + "\" with status \"" + m_sProjectStatus + "\"."); // submit complete page edit request
         }
         catch (BuildException be)
         {
            if (be.getMessage().contains("Url Too Long"))
            {
               handleUrlTooLong(sEditToken);
            } 
            else
            {
               throw be;
            }
         }

      }
      catch (UnsupportedEncodingException e)
      {
         handleException("UnsupportedEncodingException thrown while attempting to edit page.", e);
      }
      catch (IOException e)
      {
         handleException("IOException thrown while attempting to edit page.", e);
      }
      catch (BuildException be)
      {
         handleException(be);
      }
      catch (Exception generalException)
      {
         handleException(generalException.getMessage(), generalException);
      }
   }

   /**
    * Error handling function to add a warning to the build status page in case of a 'URL Too Long' error.
    * 
    * @param m_sEditToken the edit token for the status page
    */
   private void handleUrlTooLong(String sEditToken) throws Exception
   {
      String sMessage = "Section for timestamp \"" + m_sProjectTime + "\" too large, could not edit via REST.";

      m_sEditSection = "0";
      editWikiPage(sEditToken, "'''Warning''': " + sMessage, "Warning: " + sMessage);
   }

   /**
    * Handle exceptions thrown in such a way as to accommodate the failonerror setting.
    * 
    * @param e The exception
    * @throws BuildException thrown if failonerror is true
    */
   private void handleException(BuildException e) throws BuildException
   {
      handleException(e.getMessage(), e);
   }

   /**
    * Handle exceptions thrown in such a way as to accommodate the failonerror setting.
    * 
    * @param sMessage The custom message to accompany the exception 
    * @param e The exception
    * @throws BuildException thrown with a custom message if failonerror is true.
    */
   private void handleException(String sMessage, Exception e) throws BuildException
   {
      e.printStackTrace();
      
      if (m_bFailOnError)
      {
         if (e instanceof BuildException)
         {
            throw (BuildException)e;
         }
         else
         {
            throw new BuildException(sMessage, e);
         }
      }
      else
      {
         log(sMessage, Project.MSG_ERR);
      }
   }

   /**
    * Invokes a given URI as a REST RPC call with the provided handler.
    *
    * @param URI the URI to execute
    * @param rHandler the response handler
    */
   private Object invokeURI(URI targetURI, ResponseHandler rHandler)
   {
      try
      {
         return m_httpClient.invoke(targetURI, HTTP.METHOD_POST, null, rHandler);
      }
      catch (IOException e)
      {
         throw new BuildException("IOException \"" + e.getMessage() + "\" encountered while invoking URI \"" + targetURI.toString() + "\".", e);
      }
   }

   /**
    * Verify all the required parameters are set.
    */
   private void checkAttributes()
   {
      if (StringUtil.isEmpty(m_sProjectStatus))
      {
         throw new BuildException("No \"status\" attribute specified.");
      }

      if (StringUtil.isEmpty(m_sProjectTime))
      {
         throw new BuildException("No \"timestamp\" attribute specified.");
      }

      if (StringUtil.isEmpty(m_sProjectName))
      {
         throw new BuildException("No \"project\" attribute specified.");
      }

      if (StringUtil.isEmpty(m_sProjectName))
      {
         throw new BuildException("No \"branch\" attribute specified.");
      }

      if (StringUtil.isEmpty(m_sLoginName))
      {
         throw new BuildException("No login \"username\" attribute specified.");
      }

      if (StringUtil.isEmpty(m_sLoginPassword))
      {
         throw new BuildException("No login \"password\" attribute specified.");
      }

      if (StringUtil.isEmpty(m_sDomain))
      {
         throw new BuildException("No login \"domain\" attribute specified.");
      }

      if (StringUtil.isEmpty(m_sWikiURLRoot))
      {
         throw new BuildException("No \"wikiroot\" URL attribute specified.");
      }
   }

   /**
    * @return the connection URL for a wiki REST action
    */
   private String getWikiAPIURL()
   {
      return appendFormat(m_sWikiURLRoot + "/api.php", FORMAT);
   }

   /**
    * @return the connection URL for a wiki REST action
    */
   private String getWikiIndexURL()
   {
      return m_sWikiURLRoot + "/index.php";
   }

   /**
    * @param sURL the base URL to connect to
    * @param sFormat the requested format of the response 
    * @return the URL with inline format parameter set
    */
   private static String appendFormat(String sURL, String sFormat)
   {
      if (sURL.indexOf('?') < 0)
      {
         return sURL + "?format=" + sFormat;
      }
      else
      {
         return sURL + "&format=" + sFormat;
      }
   }

   /**
    * Login to the wiki to retrieve authentication cookies
    *
    * @throws Exception on OutputStreamWriter errors.
    */
   private void attemptLogin() throws Exception
   {
      WikiRestURI loginURI = new WikiRestURI();

      loginURI.addParam("action", "login");
      loginURI.addParam("lgname", m_sLoginName);
      loginURI.addParam("lgpassword", m_sLoginPassword);
      loginURI.addParam("lgdomain", m_sDomain);

      String sResult = (String)invokeURI(loginURI.getURI(), new WikiAPIAttributeHandler("login", "result"));

      if ("Success".equals(sResult))
      {
         log("Login succeeded.", Project.MSG_DEBUG);
      }
      else
      {
         throw new BuildException("Wiki API Login failed for user \"" + m_sLoginName + "\" with result \"" + sResult + "\".");
      }
   }

   /**
    * Retrieves the Edit Token of the specified wiki page via a REST query action.
    *
    * @return the Edit Token string for the specified wiki page.
    * @throws Exception on OutputStreamWriter errors.
    */
   private String getEditToken() throws Exception 
   {
      WikiRestURI queryURI = new WikiRestURI();

      queryURI.addParam("action", "query");
      queryURI.addParam("prop", "info");
      queryURI.addParam("intoken", "edit");
      queryURI.addParam("titles", m_sPageTitle);

      String sEditToken = (String)invokeURI(queryURI.getURI(), new WikiAPIQueryHandler(m_sPageTitle));

      if (sEditToken == null)
      {
         throw new BuildException("Could not find token for page \"" + m_sPageTitle + "\"");
      }

      return sEditToken;
   }

   /**
    * Parse the wiki-text for the specified page.
    *
    * @return the wiki-text for the specified page.
    * @throws Exception on OutputStreamWriter errors.
    */
   private String getContents() throws Exception
   {
      WikiRestURI contentsURI = new WikiRestURI(getWikiIndexURL());

      contentsURI.addParam("action", "raw");
      contentsURI.addParam("title", m_sPageTitle);

      return (String)invokeURI(contentsURI.getURI(), new ContentStringResponseHandler());
   }

   /**
    * Generate the wiki-text contents with the updated status specified.
    *
    * @param sPageContents the original page wiki-text to modify
    * @return the wiki-text contents with the updated status specified
    */
   private String generatedModifiedPage(String sPageContents)
   {
      StringBuffer newContentsBuffer = new StringBuffer(sPageContents);

      // calculate readable date/time
      String sDateTitle = getDateTitle();
      String sDateId = m_sProjectTime.substring(0, 8); // used as table ID
      int nTableIndex = sPageContents.indexOf(sDateTitle);

      //if -1, new section needed, if not then need to find section number
      if (nTableIndex < 0)
      {
         m_sEditSection = "0"; // new top section
         newContentsBuffer = new StringBuffer();

         newContentsBuffer.append("="); // section title
         newContentsBuffer.append(sDateTitle);
         newContentsBuffer.append("=\n\n{| id=\""); // insert table headers
         newContentsBuffer.append(sDateId);
         newContentsBuffer.append("\" border=\"1\" cellpadding=\"3\" |\n| '''Time Stamp''' || '''Project''' || '''Branch''' || colspan=\"0\" | '''Status'''\n|-\n| "); // add new entry
         newContentsBuffer.append(m_sProjectTime);
         newContentsBuffer.append(" || '''");
         newContentsBuffer.append(m_sProjectName);
         newContentsBuffer.append("''' || ");
         newContentsBuffer.append(m_sProjectBranch);
         newContentsBuffer.append(" || ");
         newContentsBuffer.append(m_sProjectStatus);
         newContentsBuffer.append("\n|}<br/><br/>\n\n"); // close table
      }
      else
      {
         // Find Section Number of target table
         Matcher pageTitleMatcher = TITLE_REGEX_PATTERN.matcher(sPageContents);
         int nSectionCount = 0;
         int nSectionStart = 0;
         int nSectionEnd = 0;

         // iterate title indices
         while (pageTitleMatcher.find())
         {
            nSectionCount++;
            nSectionStart = nSectionEnd;
            nSectionEnd = pageTitleMatcher.start();

            if (nSectionEnd > nTableIndex) 
            {
               // this is the section after the target table
               nSectionCount--;
               break;
            }
         }

         if (nSectionCount < 1)
         {
            throw new BuildException("Illegal state exception: existing entry not found.");
         }

         m_sEditSection = "" + nSectionCount; // convert to string
         newContentsBuffer = new StringBuffer(sPageContents.substring(nSectionStart, nSectionEnd));

         // found date section
         // find table via ID and entry
         String sEntryKey = m_sProjectTime + " || '''" + m_sProjectName + "''' || " + m_sProjectBranch;
         int nEntryResumeIndex = -1;
         int nEntryStartIndex = newContentsBuffer.indexOf(sEntryKey);

         if (nEntryStartIndex < 0)
         {
            // no entry found, add one at the top of table
            nEntryStartIndex = newContentsBuffer.indexOf("|-") + 3;

            newContentsBuffer.insert(nEntryStartIndex, "\n|-\n");
            newContentsBuffer.insert(nEntryStartIndex, sEntryKey);
            newContentsBuffer.insert(nEntryStartIndex, "| ");
            nEntryResumeIndex = newContentsBuffer.indexOf("\n", nEntryStartIndex); //end of line
            log("Added new entry \"" + m_sProjectTime + "\", \"" + m_sProjectName + "\", \"" + m_sProjectBranch + "\".", Project.MSG_DEBUG);
         }
         else
         {
            // append to existing entry
            nEntryResumeIndex = newContentsBuffer.indexOf("\n", nEntryStartIndex); //end of line
         }

         //timestamp entry exists, so append new status
         //newContentsBuffer.insert(nEntryResumeIndex, "\n");
         newContentsBuffer.insert(nEntryResumeIndex, m_sProjectStatus);

         if (!StringUtil.isEmpty(m_sStatusColour))
         {
            newContentsBuffer.insert(nEntryResumeIndex, ";\" | ");
            newContentsBuffer.insert(nEntryResumeIndex, m_sStatusColour);
            newContentsBuffer.insert(nEntryResumeIndex, "style=\"background-color:");
         }

         newContentsBuffer.insert(nEntryResumeIndex, " || ");
         log("Added new status \"" + m_sProjectStatus + "\".", Project.MSG_DEBUG);
      }

      return newContentsBuffer.toString();
   }

   /**
    * Returns the human readable date section title based on the user specified timestamp.
    *
    * @return the human readable date section title.
    */
   private String getDateTitle()
   {
      Date buildDate;
      StringBuffer dateTitleBuffer = new StringBuffer(17); //4 + 9 + 2 + 2
      int nYear, nDay, nMonth, nHour, nMinute;

      if (StringUtil.isEmpty(m_sProjectTime) || !Pattern.matches("\\d{12}", m_sProjectTime))
      {
         throw new BuildException("Invalid timestamp.");
      }

      nYear = Integer.parseInt(m_sProjectTime.substring(0, 4));
      nMonth = Integer.parseInt(m_sProjectTime.substring(4, 6));
      nDay = Integer.parseInt(m_sProjectTime.substring(6, 8));
      nHour = Integer.parseInt(m_sProjectTime.substring(8, 10));
      nMinute = Integer.parseInt(m_sProjectTime.substring(10, 12));

      if ((nMonth > 12) || (nDay > 31) || (nHour > 24) || (nMinute > 59))
      {
         throw new BuildException("Invalid timestamp value \"" + m_sProjectTime + "\".");
      }

      try
      {
         buildDate = BUILD_TIMESTAMP_FORMAT.parse(m_sProjectTime);
      }
      catch (ParseException e)
      {
         throw new BuildException("Could not parse timestamp \"" + m_sProjectTime + "\".");
      }

      dateTitleBuffer.append(MONTH_TEXT_FORMAT.format(buildDate));
      dateTitleBuffer.append(" ");
      dateTitleBuffer.append(nDay);
      dateTitleBuffer.append(" ");
      dateTitleBuffer.append(nYear);

      return dateTitleBuffer.toString();
   }

   /**
    * Submit the wiki REST edit request for the modified page contents. 
    *
    * @throws Exception on OutputStreamWriter errors.
    */
   private void editWikiPage(String sEditToken, String sModifiedPageContents, String sEditComment) throws Exception
   {
      WikiRestURI editURI = new WikiRestURI();

      editURI.addParam("action", "edit");
      editURI.addParam("title", m_sPageTitle);
      editURI.addParam("token", sEditToken);
      editURI.addParam("summary", sEditComment);
      editURI.addParam("text", sModifiedPageContents);
      editURI.addParam("section", m_sEditSection);

      String sResult = (String)invokeURI(editURI.getURI(), new WikiAPIAttributeHandler("edit", "result"));

      if ("Success".equals(sResult))
      {
         log(sEditComment, Project.MSG_INFO);
      }
      else
      {
         throw new BuildException("Edit action failed with result \"" + sResult + "\".");
      }
   }

   /**
    * Helper class for REST RPC URI
    */
   private class WikiRestURI
   {
      private StringBuffer m_sb;
      private boolean m_bHasParams = false;

      /**
       * Constructor for a custom API endpoint.
       *
       * @param sUrl the custom API endpoint url
       */
      public WikiRestURI(String sUrl)
      {
         m_sb = new StringBuffer(50);

         m_sb.append(sUrl);

         if (sUrl.indexOf("?") > -1)
         {
            m_bHasParams = true;
         }
      }

      /**
       * Default constructor for the default Wiki API endpoint.
       */
      public WikiRestURI()
      {
         this(getWikiAPIURL());
      }

      /**
       * To add an inline parameter to the REST call.
       *
       * @param sParam the parameter name
       * @param sValue the parameter value
       */
      public void addParam(String sParam, String sValue)
      {
         try
         {
            if (m_bHasParams)
            {
               m_sb.append("&");
            }
            else
            {
               m_sb.append("?");
               m_bHasParams = true;
            }

            m_sb.append(URLEncoder.encode(sParam, ENCODING));
            m_sb.append("=");
            m_sb.append(URLEncoder.encode(sValue, ENCODING));
         }
         catch (UnsupportedEncodingException e)
         {
            throw new BuildException("UnsupportedEncodingException thrown while building REST URI with parameter \"" + sParam + "\"=\"" + sValue + "\".", e);
         }
      }

      /**
       * @return the URI object for this REST RPC call.
       */
      public URI getURI()
      {
         try
         {
            return new URI(m_sb.toString());
         }
         catch (URISyntaxException e)
         {
            throw new BuildException("URISyntaxException encountered while forming URI object: \"" + m_sb.toString() + "\".", e);
         }
      }
   }

   /**
    * SAX Handler for parsing an attribute from an element.
    */
   private static class InputAttributeHandler extends DefaultHandler
   {
      private String m_sAttributeName;
      private String m_sElementName;
      private String m_sResult;

      /**
       * @param sElementName the element to parse for the attribute value
       * @param sAttributeName the attribute to parse
       */
      public InputAttributeHandler(String sElementName, String sAttributeName)
      {
         m_sElementName = sElementName;
         m_sAttributeName = sAttributeName;
      }

      /**
       * @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String,
       *      java.lang.String, java.lang.String, org.xml.sax.Attributes)
       */
      public void startElement(String sNamespace, String sLocalName, String sQName, Attributes attrs) throws SAXException
      {
         if (sLocalName.equals(m_sElementName))
         {
            m_sResult = attrs.getValue("", m_sAttributeName);
         }
      }

      /**
       * @return the attribute value in the specified element. If more than one the last value is returned. Or null if the value doesn't exist. 
       */
      public String getResult()
      {
         return m_sResult;
      }
   }

   /**
    * HTTPClient ResponseHandler for parsing the attribute of a given element
    * in the invoke response.
    *
    * Used with attemptLogin() and editWikiPAge to parse the result of the action.
    */
   private static class WikiAPIAttributeHandler extends InputAttributeHandler implements ResponseHandler
   {
      public WikiAPIAttributeHandler(String sElementName, String sAttributeName)
      {
         super(sElementName, sAttributeName);
      }

      /**
       * @see nexj.core.util.HTTPClient.ResponseHandler#handleResponse(nexj.core.util.HTTPClient, java.io.InputStream)
       */
      public Object handleResponse(HTTPClient client, InputStream istream) throws IOException
      {
         String sResponse = new Scanner(istream).useDelimiter("\\A").next();

         XMLUtil.parse(new InputSource(new StringReader(sResponse)), this);

         String sResult = getResult();

         if (sResult == null)
         {
            throw new BuildException("Unexpected response:\n" + sResponse);
         }

         return sResult;
      }
   }

   /**
    * HTTPClient ResponseHandler for parsing the edittoken of a query.
    * Also accounts for page title normalizations.
    *
    * Used with getEditToken().
    */
   private class WikiAPIQueryHandler implements ResponseHandler
   {
      private Lookup m_pageTokenMap;
      private Lookup m_normalizationMap;
      private String m_sPageTitle;

      private WikiAPIQueryHandler(String sPageTitle)
      {
         m_pageTokenMap = new HashTab();
         m_normalizationMap = new HashTab();
         m_sPageTitle = sPageTitle;
      }

      /**
       * @see nexj.core.util.HTTPClient.ResponseHandler#handleResponse(nexj.core.util.HTTPClient, java.io.InputStream)
       */
      public Object handleResponse(HTTPClient client, InputStream istream) throws IOException
      {
         XMLUtil.parse(new InputSource(istream), new DefaultHandler()
         {
            /**
             * @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String,
             *      java.lang.String, java.lang.String, org.xml.sax.Attributes)
             */
            public void startElement(String sNamespace, String sLocalName, String sQName, Attributes attrs) throws SAXException
            {
               if (sLocalName.equals("page")) // need to collect page edit tokens
               {
                  String sTitle = attrs.getValue("title");
                  String sToken = attrs.getValue("edittoken");

                  if ((sTitle != null) && (sToken != null))
                  {
                     m_pageTokenMap.put(sTitle, sToken);
                     log("Adding edittoken: " + sTitle + ", " + sToken, Project.MSG_DEBUG);
                  }
                  else
                  {
                     log("Page element not complete: title=\"" + sTitle + "\" edittoken=\"" + sToken + "\".", Project.MSG_WARN);
                  }
               }
               else if (sLocalName.equals("n")) // also need any page title normalizations
               {
                  String sNormalizedFrom = attrs.getValue("from");
                  String sNormalizedTo = attrs.getValue("to");

                  if ((sNormalizedTo != null) && (sNormalizedFrom != null))
                  {
                     m_normalizationMap.put(sNormalizedFrom, sNormalizedTo);
                     log("Adding normalization: " + sNormalizedFrom + ", " + sNormalizedTo, Project.MSG_DEBUG);
                  }
                  else
                  {
                     log("Normalization not complete: from=\"" + sNormalizedFrom + "\" to=\"" + sNormalizedTo + "\".", Project.MSG_WARN);
                  }
               }
            }
         });

         String sDisplayTitle = (String)m_normalizationMap.get(m_sPageTitle); // resolve page title normalization

         if (sDisplayTitle == null) 
         {
            sDisplayTitle = m_sPageTitle; 
         }

         return m_pageTokenMap.get(sDisplayTitle);
      }
   }

   /**
    * HTTPClient ResponseHandler for getting the contents of a wiki page.
    *
    * Used with getContents().
    */
   private static class ContentStringResponseHandler implements ResponseHandler
   {
      public Object handleResponse(HTTPClient client, InputStream istream) throws IOException
      {
         return new StreamInput(istream).getString();
      }
   }
}
