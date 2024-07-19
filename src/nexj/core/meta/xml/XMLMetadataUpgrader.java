package nexj.core.meta.xml;

import java.io.Reader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import org.w3c.dom.Element;

import nexj.core.meta.MetadataException;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.MultiMap;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.XMLUtil;

/**
 * Applies framework upgrade steps to resources from older metadata repositories,
 * as the resources are loaded.
 *
 * Instances of this class should be treated as immutable, to ensure thread-safety.
 */
public class XMLMetadataUpgrader
{
   // constants

   /**
    * The prefix of metadata upgrade classes.
    */
   protected final static String METADATA_UPGRADE_PREFIX = "XMLMetadataUpgrade";

   // associations

   /**
    * A map of ordered lists of upgrade steps to apply to elements, keyed on element name.
    * The lists have Methods on elements and corresponding upgrade class on odd elements.
    */
   protected Lookup/*<String, List<Object>>*/ m_upgradeMap; // lazy init

   /**
    * The resources added by the metadata upgrades. Map of resource full name (URI) to content URL.
    */
   protected Lookup m_newResourceURLMap; // of type URL[String]

   /**
    * The names of the resources added by the metadata upgrades, indexed by resource
    * extension. Map of extension to multiple full names (URIs).
    */
   protected MultiMap m_newResourcesMap; // of type String[][String]

   // constructors

   /**
    * Sets the metadata core version to start the upgrade steps from.
    * @param sVersion The metadata core version to start the upgrade from.
    * @param upgradeResourceList List of resources that are potential metadata upgraders.
    */
   public XMLMetadataUpgrader(String sVersion, List upgradeResourceList) throws MetadataException
   {
      if (sVersion == null)
      {
         sVersion = "0";
      }

      List/*<Class>*/ upgradeList = new ArrayList/*<Class>*/();
      String sPackage = XMLMetadata.class.getPackage().getName() + ".";

      m_upgradeMap = new HashTab/*<String, List<Object>>*/();

      try
      {
         for (int i = 0; i < upgradeResourceList.size(); ++i)
         {
            String sResourcePath = (String)upgradeResourceList.get(i);
            String sResource = sResourcePath.substring(sResourcePath.lastIndexOf('/') + 1); // filename

            if (sResource.startsWith(METADATA_UPGRADE_PREFIX) && // consider only specific prefix
                sResource.endsWith(".class") && // consider only classes
                !sResource.contains("$")) // ignore inner classes
            {
               String sClass = sPackage + sResourcePath.substring(0, sResourcePath.lastIndexOf('.')).replace('/', '.');
               Class upgrade = Class.forName(sClass);
               String sUpgradeVersion = (String)upgrade.getDeclaredField("VERSION").get(null);

               if (StringUtil.compareVersions(sUpgradeVersion, sVersion) > 0)
               {
                  upgradeList.add(upgrade);
               }
            }
         }

         // sort all valid upgrade classes by the value of their version variable
         Collections.sort(upgradeList, new Comparator()
         {
            public int compare(Object left, Object right)
            {
               try
               {
                  String sLeft = (String)((Class)left).getDeclaredField("VERSION").get(null);
                  String sRight = (String)((Class)right).getDeclaredField("VERSION").get(null);

                  return StringUtil.compareVersions(sLeft, sRight);
               }
               catch (Exception e)
               {
                  throw ObjUtil.rethrow(e);
               }
            }
         });

         // for every applicable upgrade class find all applicable upgrade* methods
         for (int i = 0, nCount = upgradeList.size(); i < nCount; ++i)
         {
            Class upgrade = (Class)upgradeList.get(i);
            Method[] methodArray = upgrade.getMethods();

            for (int k = 0; k < methodArray.length; ++k)
            {
               String sName = methodArray[k].getName();
               Class[] argTypeArray = methodArray[k].getParameterTypes();

               // find all upgrade* methods with appropriate args
               if (argTypeArray.length == 3 &&
                   Element.class.isAssignableFrom(argTypeArray[0]) &&
                   String.class.isAssignableFrom(argTypeArray[1]) &&
                   XMLMetadataHelper.class.isAssignableFrom(argTypeArray[2]) &&
                   sName.startsWith("upgrade") &&
                   sName.length() > "upgrade".length())
               {
                  String sElement = sName.substring("upgrade".length());
                  List/*<Object>*/ methodList = (List)m_upgradeMap.get(sElement);

                  if (methodList == null)
                  {
                     methodList = new ArrayList/*<Object>*/();
                     m_upgradeMap.put(sElement, methodList);
                  }

                  methodList.add(methodArray[k]);
                  methodList.add(upgrade);
               }
               else if (argTypeArray.length == 1 &&
                  Lookup.class.isAssignableFrom(argTypeArray[0]) &&
                  sName.equals("addResources"))
               {
                  if (m_newResourceURLMap == null)
                  {
                     m_newResourceURLMap = new HashTab();
                  }

                  methodArray[k].invoke(upgrade.newInstance(), new Object[] {m_newResourceURLMap});
               }
            }
         }

         if (m_newResourceURLMap != null)
         {
            m_newResourcesMap = new MultiMap(new HashTab(m_newResourceURLMap.size()));

            for (Iterator itr = m_newResourceURLMap.iterator(); itr.hasNext(); )
            {
               String sURI = (String)itr.next();

               m_newResourcesMap.add(getExtension(sURI), sURI);
            }
         }
      }
      catch (Exception e)
      {
         throw new MetadataException("err.meta.upgradeLoad", new Object[]{sVersion}, e);
      }
   }

   // operations

   /**
    * Gets the resource extension.
    * @param sURI The resource URI.
    * @return The resource extension (with leading '.').
    */
   public static String getExtension(String sURI)
   {
      return sURI.substring(sURI.lastIndexOf('.'));
   }

   /**
    * Gets iterator over resources URIs added by framework upgrade steps, having the given extension.
    * @param sExt The extension for which to get the iterator.
    * @return The iterator over resources URIs added by framework upgrade steps, having the extension sExt.
    **/
   public Iterator getNewResourceIterator(String sExt)
   {
      if (m_newResourcesMap == null)
      {
         return Collections.EMPTY_LIST.iterator();
      }
      else
      {
         return m_newResourcesMap.get(sExt).iterator();
      }
   }

   /**
    * Gets the URL of a resource added by a framework upgrade step.
    * @param sName The name of the resource.
    * @return The URL of the resource.
    */
   public URL getNewResourceURL(String sName)
   {
      return (m_newResourceURLMap == null) ? null : (URL)m_newResourceURLMap.get(sName);
   }

   /**
    * Apply framework upgrades to a resource as it is read.
    * @param reader The reader that must have its output upgraded.
    * @param sName The name of the resource providing the element.
    * @param helper The XMLMetadataHelper providing the resource.
    * @return The upgraded element, or null if no upgrade was applied.
    */
   public Element upgrade(Reader reader, String sName, XMLMetadataHelper helper)
   {
      if (m_upgradeMap == null) // nothing to upgrade with
      {
         return null;
      }

      Element element = XMLUtil.parse(reader).getDocumentElement();
      String sElementName = element.getNodeName();
      List upgradeList = (List)m_upgradeMap.get(sElementName);

      if (upgradeList != null)
      {
         // retrieve cached upgrade instances from helper
         List instanceList = getUpgradeInstances(sElementName, upgradeList, helper);

         try
         {
            for (int i = 0, nCount = instanceList.size(); i < nCount; i++)
            {
               ((Method)upgradeList.get(i * 2)).invoke(instanceList.get(i), new Object[]{element, sName, helper});
            }
         }
         catch (InvocationTargetException e)
         {
            ObjUtil.rethrow(e.getCause());
         }
         catch (Exception e)
         {
            ObjUtil.rethrow(e);
         }
      }

      return element;
   }

   /**
    * Gets the list of XML upgrade instances corresponding to a list of upgrade classes.
    * @param sName The name of the element to upgrade.
    * @param upgradeList The ordered list of upgrade method/class pairs (even offset is a method, odd offset is class).
    * @param helper The XMLMetadataHelper providing the resource.
    * @return The ordered list of upgrade instances, half the length of upgrades.
    */
   protected List getUpgradeInstances(String sName, List upgradeList, XMLMetadataHelper helper)
   {
      Lookup instanceMap = (Lookup)helper.getUpgraderState();

      if (instanceMap == null)
      {
         instanceMap = new HashTab();
         helper.setUpgraderState(instanceMap);
      }

      List upgradeInstances = (List)instanceMap.get(sName);

      if (upgradeInstances == null)
      {
         try
         {
            upgradeInstances = new ArrayList(upgradeList.size() / 2);

            for (int i = 0; i < upgradeList.size(); i += 2)
            {
               upgradeInstances.add(((Class)upgradeList.get(i + 1)).newInstance());
            }
         }
         catch (Exception e)
         {
            ObjUtil.rethrow(e);
         }

         instanceMap.put(sName, upgradeInstances);
      }

      return upgradeInstances;
   }
}
