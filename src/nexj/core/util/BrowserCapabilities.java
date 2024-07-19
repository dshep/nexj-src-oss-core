// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Class containing information about browser capabilities and specific view classes.
 */
public final class BrowserCapabilities
{
   /**
    * Browser Type Constants.
    */
   public final static int BROWSER_DEFAULT = 0;

   public final static int BROWSER_IE = 5;

   public final static int BROWSER_HTML4 = 1;

   public final static int BROWSER_MOBILE = 2;

   public final static int BROWSER_MOZILLA = 3;

   public final static int BROWSER_SAFARI = 4;
   
   /**
    * Browser capability constants. 
    */
   public final static int BROWSER_CAP_RICH_RENDERING = 1;
   
   public final static int BROWSER_CAP_ONCONTEXTMENU_EVT = 1 << 1;
   
   public final static int BROWSER_CAP_RESIZEABLE_SPLITTERS = 1 << 2;
   
   public final static int BROWSER_CAP_RESIZEABLE_GRIDCOLS = 1 << 3;
   
   public final static int BROWSER_CAP_GROUP_TAB_SELECTOR = 1 << 4;
   
   public final static int BROWSER_CAP_HISTORY = 1 << 5;
   
   public final static int BROWSER_CAP_ONRESIZE_ON_ELEMENTS = 1 << 6;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(BrowserCapabilities.class);

   /**
    * Map of capabilities supported by a given browser type.
    */
   private final static int[] SUPPORTED_CAPABILITIES = new int[6];

   static
   {
      int nCommonCapabilities                   = BROWSER_CAP_RICH_RENDERING
                                                | BROWSER_CAP_HISTORY
                                                | BROWSER_CAP_ONCONTEXTMENU_EVT
                                                | BROWSER_CAP_RESIZEABLE_SPLITTERS
                                                | BROWSER_CAP_RESIZEABLE_GRIDCOLS
                                                | BROWSER_CAP_GROUP_TAB_SELECTOR;

      // IE capabilities
      SUPPORTED_CAPABILITIES[BROWSER_IE]        = nCommonCapabilities
                                                | BROWSER_CAP_ONRESIZE_ON_ELEMENTS;

      // Mozilla capabilities
      SUPPORTED_CAPABILITIES[BROWSER_MOZILLA]   = nCommonCapabilities;

      // Safari capabilities
      SUPPORTED_CAPABILITIES[BROWSER_SAFARI]    = nCommonCapabilities;
   }
   
   /**
    * Determine the type of browser being used to submit the current request.
    * @param sUserAgent String containing the user agent of the browser.
    * @return One of the BROWSER_* major browser type constants.
    */
   public static int getBrowserType(String sUserAgent)
   {
      if (sUserAgent != null)
      {
         if (sUserAgent.indexOf("MSIE") > 0)
         {
            return BROWSER_IE;
         }
         else if (sUserAgent.indexOf("Gecko/") > 0)
         {
            return BROWSER_MOZILLA;
         }
         else if (sUserAgent.indexOf("AppleWebKit/") > 0 || sUserAgent.indexOf("BlackBerry") >= 0)
         {
            return BROWSER_SAFARI;
         }
         else if (sUserAgent.indexOf("Windows CE") >= 0 || sUserAgent.indexOf("Smartphone") >= 0)
         {
            return BROWSER_MOBILE;
         }
      }
      
      return BROWSER_HTML4;
   }
   
   /**
    * Determines the full version number of a browser based on the user agent and type
    * of browser detected.  Use {@link BrowserCapabilities#getBrowserType(String)} for the general
    * type of browser making the request.
    * 
    * @param sUserAgent String containing the user agent of the browser.
    * @return An int[] containing the numeric parts of the version string.  NULL if unable to
    * determine version. 
    */
   public static int[] getBrowserVersion(String sUserAgent)
   {
      int nBrowserType = getBrowserType(sUserAgent);
      int nStartIdx = -1;
      int nEndIdx = -1;

      switch (nBrowserType)
      {
         case BROWSER_IE:
         {
            nStartIdx = sUserAgent.indexOf("MSIE ");

            if (nStartIdx >= 0)
            {
               nStartIdx += 5;
               nEndIdx = sUserAgent.indexOf(';', nStartIdx);
            }

            break;
         }
         case BROWSER_MOZILLA:
         {
            nStartIdx = sUserAgent.indexOf("Firefox/");

            if (nStartIdx >= 0)
            {
               nStartIdx += 8;
               nEndIdx = sUserAgent.indexOf(' ', nStartIdx);
   
               if (nEndIdx < 0)
               {
                  nEndIdx = sUserAgent.length();
               }
            }

            break;
         }
         case BROWSER_SAFARI:
         {
            nStartIdx = sUserAgent.indexOf("Version/");

            if (nStartIdx >= 0)
            {
               nStartIdx += 8;
               nEndIdx = sUserAgent.indexOf(' ', nStartIdx);
            }

            break;
         }
      }

      if (nStartIdx >= 0 && nEndIdx >= 0)
      {
         int nVersionCount = 1;
         int nVerEndIndex = 0;
         int[] nVersionArray = null;

         try
         {
            // find the version array size
            for (int i = nStartIdx; i < nEndIdx; i++)
            {
               if (sUserAgent.charAt(i) == '.')
               {
                  nVersionCount++;
               }
            }

            nVersionArray = new int[nVersionCount];

            for (int k = 0; k < nVersionCount; k++)
            {
               nVerEndIndex = sUserAgent.indexOf('.', nStartIdx + 1);

               if (nVerEndIndex < 0 || nVerEndIndex > nEndIdx)
               {
                  nVerEndIndex = nEndIdx;
               }

               nVersionArray[k] = StringUtil.parseInt(sUserAgent, nStartIdx, nVerEndIndex);
               nStartIdx = nVerEndIndex + 1;
            }
            
            if (nBrowserType == BROWSER_IE && nVersionArray[0] == 7 && sUserAgent.contains("Trident/5.0"))
            {
               nVersionArray[0] = 9; // IE9 Compatibility mode.
            }

            // IE8 and IE9 says the version is 7 but it will also pass a Trident version of 4 and 5, respectively
            if (nBrowserType == BROWSER_IE && nVersionArray[0] == 7)
            {
               int nTridentIndex = sUserAgent.indexOf("Trident/") + 8;

               if (nTridentIndex != -1)
               {
                  int nTridentVersion = StringUtil.parseInt(sUserAgent, nTridentIndex, nTridentIndex + 1);

                  switch (nTridentVersion)
                  {
                     case 4:
                     {
                        nVersionArray[0] = 8; // IE8 Compatibility mode.
                        break;
                     }
                     case 5:
                     {
                        nVersionArray[0] = 9; // IE9 Compatibility mode.
                        break;
                     }
                  }
               }
            }

            return nVersionArray;
         }
         catch(Exception e)
         {
            s_logger.debug("Unable to parse browser version", e);
         }
      }

      return null;
   }
   
   /**
    * Determines if the given browser type supports the specific capability.
    * 
    * @param nBrowserType One of the BROWSER_* major browser type constants.
    * @param nCapability One of the BROWSER_CAP_* capabilities to check for compliance against the type.
    * @return True if the nBrowserType supports the requested capability.
    */
   public static boolean isBrowserCapable(int nBrowserType, int nCapability)
   {
      return ((SUPPORTED_CAPABILITIES[nBrowserType] & nCapability) > 0);
   }
   
   /**
    * Determines whether the DirectX filter is required for images. DirectX is only supported in Internet
    * Explorer and needed to work around IE6's lack of transparency support for png images.
    */
   public static boolean isImageDirectXFilterRequired(int nBrowserType, int[] nBrowserVersionArray)
   {
      return (nBrowserVersionArray == null) ? false : nBrowserType == BROWSER_IE && nBrowserVersionArray[0] == 6;
   }

   /**
    * @return Whether the browser supports push notifications
    */
   public static boolean isPushNotificationSupported(int nBrowserType, int[] nBrowserVersionArray)
   {
      if (nBrowserVersionArray == null) 
      {
         return  false;
      }

      // Source: http://stackoverflow.com/questions/4403865/which-browsers-allow-cross-domain-ajax-calls-with-access-control-allow-origin
      // TODO: Google Chrome
      // Warning: mobile browsers have different version numbers.
      switch (nBrowserType)
      {
         case BROWSER_IE:
            return nBrowserVersionArray[0] >= 8;

         case BROWSER_MOZILLA:
            if (nBrowserVersionArray[0] == 3)
            {
               return nBrowserVersionArray[1] >= 6;
            }

            return nBrowserVersionArray[0] >= 4;

         case BROWSER_SAFARI:
            return nBrowserVersionArray[0] >= 4;
      }

      return false;
   }
}
