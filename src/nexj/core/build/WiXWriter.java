// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.build;

import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.Locale;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.transform.stream.StreamSource;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.taskdefs.Execute;
import org.apache.tools.ant.taskdefs.Expand;
import org.apache.tools.ant.types.DataType;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.ZipFileSet;
import org.apache.tools.ant.util.FileUtils;

import nexj.core.util.GUIDUtil;
import nexj.core.util.HashTab;
import nexj.core.util.IOUtil;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.XMLUtil;
import nexj.core.util.XMLWriter;

/**
 * Writes a WiX fragment configured by Ant and compiles/links the MSI using WiX.
 */
public class WiXWriter extends Task
{
   // constants

   /**
    * Ant debug log level (change to Project.MSG_WARN if "ant -debug" is to
    * noisy).
    */
   private final static int DEBUG_LEVEL = Project.MSG_DEBUG;

   /**
    * Ant error log level.
    */
   private final static int ERROR_LEVEL = Project.MSG_ERR;

   /**
    * Name of automatically generated file, not including the extension (.wxs or
    * .wxsobj) and path.
    */
   private final static String TEMP_FILE_PREFIX = "nexj_wixwriter_";

   /**
    * ID of the directory where all directories are rooted.
    */
   private final static String ROOT_DIRECTORY_ID = "TARGETDIR";

   /**
    * Pattern for matching system folder reference in the install path.
    */
   private final static Pattern SYSTEM_FOLDER_REF_PATTERN = Pattern.compile("^\\[[A-Za-z0-9]+\\]");

   /**
    * Pattern for matching bad 8.3 filename characters.
    */
   private final static Pattern BAD_83_FILE_CHAR_PATTERN = Pattern.compile("[^0-9A-Z_]");

   /**
    * String encoder used for getting binary data to hash.
    */
   private final static CharsetEncoder UTF_ENCODER = Charset.forName("UTF-8").newEncoder();

   /**
    * Namspace for hashing paths.
    */
   private final static byte[] GUID_NAMESPACE = getEncodedData(SysUtil.NAMESPACE + ":installer:");

   /**
    * Namspace for hashing short names.
    */
   private final static byte[] SHORTNAME_NAMESPACE = getEncodedData(SysUtil.NAMESPACE + ":installer:shortname:");

   // member variables

   /**
    * Name of installer type.
    */
   private String m_sInstallName;

   /**
    * ID of a root system folder that the install root is under.
    */
   private String m_sSystemFolderRoot;

   /**
    * Prefix applied to every archivegroup.
    */
   private String m_sInstallDirectory;

   /**
    * Version identifier.
    */
   private String m_sVersion;
   
   /**
    * Path where WIX Candle and light are found.
    */
   private File m_wixDirectory;

   /**
    * Path where the WXS files are found.
    */
   private File m_sourceDirectory;

   /**
    * Path where MSIs are written to.
    */
   private File m_destinationDirectory;

   /**
    * Product name used by the MSI filename.
    */
   private String m_sProductName;

   /**
    * Product name used by the MSI filename.
    */
   private String m_sMSIFilenameProperty;

   /**
    * Product name used by the EXE filename, if applicable.
    */
   private String m_sEXEFilenameProperty;

   /**
    * List of disk id's and their associated file patterns (ArchiveGroup
    * objects).
    */
   private ArrayList m_archiveGroupList = new ArrayList();

   /**
    * List of Resources (WixBinary or WixIcon objects).
    */
   private ArrayList m_resourceList = new ArrayList();

   /**
    * List of Variables.
    */
   private ArrayList m_variableList = new ArrayList();

   /**
    * NSIS interop bootstrapper.
    */
   private InteropBootstrapper m_interopBootstrapper;

   /**
    * Debug mode.
    */
   private boolean m_bDebugModeEnabled;

   /**
    * Path to the initial output file.
    */
   private File m_fragmentFile;

   /**
    * Writer for the initial output file.
    */
   private XMLWriter m_writer;

   /**
    * Folder where temporary files are held.
    */
   private File m_tempDirectory;

   /**
    * Base name of the setup file.
    */
   private String m_sSetupFileBaseName;

   /**
    * Name of resulting MSI file.
    */
   private String m_sDestinationFile;

   /**
    * The Ant project.
    */
   private Project m_project;

   /**
    * Sorted list of disk IDs Strings.
    */
   private ArrayList m_componentGroupIdList = new ArrayList();

   /**
    * Map a String disk ID to an ArrayList of String component IDs.
    */
   private Lookup m_componentGroupMap = new HashTab();

   /**
    * Get the raw data of a string encoded in UTF-8.
    * 
    * @param sInput The string.
    * @return The data.
    */
   private static byte[] getEncodedData(String sInput)
   {
      ByteBuffer buffer = null;

      // Encode the string
      try
      {
         buffer = UTF_ENCODER.encode(CharBuffer.wrap(sInput));
      }
      catch (CharacterCodingException e)
      {
         ObjUtil.rethrow(e);
      }

      // Allocate the array
      byte nDataArray[] = new byte[buffer.remaining()];

      // Copy the data
      buffer.get(nDataArray);

      return nDataArray;
   }

   /**
    * Calculate an archaic FAT 8.3 filename. If the filename won't fit regularly
    * into 8.3, take the first two letters and have the next 6 be a hash of the
    * name.
    * 
    * @param sName The original name.
    * @return The 8.3 compatible name.
    */
   private static String getShortName(String sName)
   {
      // Convert to uppercase
      String sUpperCase = sName.toUpperCase(Locale.ENGLISH);

      String sBase = sUpperCase;
      String sExt = "";
      int nExtIndex = sUpperCase.lastIndexOf('.');

      if (nExtIndex >= 0)
      {
         sExt = sBase.substring(nExtIndex + 1);
         sBase = sBase.substring(0, nExtIndex);
      }

      // If the name has no bad characters and the base name is at most eight characters and the extension is at most
      // three
      if (sBase.length() > 0 && sBase.length() <= 8 && sExt.length() <= 3 && !BAD_83_FILE_CHAR_PATTERN.matcher(sBase).find() 
         && !BAD_83_FILE_CHAR_PATTERN.matcher(sExt).find())
      {
         return sUpperCase;
      }

      // First two characters are suffixed by a hash of the filename

      StringBuilder shortBuf = new StringBuilder(12);

      append83(shortBuf, sBase.length() > 2 ? sBase.substring(0, 2) : sBase);

      // Calculate the hash and convert it to a hexidecimal string
      byte[] nDataArray = getEncodedData(sName);
      String sHash = GUIDUtil.generateGUID(SHORTNAME_NAMESPACE, nDataArray).toString();

      // Truncate and append the hash: hopefully we have enough entropy
      shortBuf.append(sHash, 0, 6);

      if (sExt.length() > 0)
      {
         shortBuf.append('.');
         append83(shortBuf, sExt.length() > 3 ? sExt.substring(0, 3) : sExt);
      }

      return shortBuf.toString();
   }
   
   /**
    * Helper function for getShortName() which appends a string to StringBuilder
    * replacing bad 8.3 file names with an underscore. 
    * 
    * @param target StringBuilder to append to.
    * @param sSrc String source to append.
    */
   private static void append83(StringBuilder target, String sSrc)
   {
      for (int nIndex = 0; nIndex < sSrc.length(); nIndex++)
      {
         String sChar = sSrc.substring(nIndex, nIndex + 1);

         if (Pattern.matches(BAD_83_FILE_CHAR_PATTERN.pattern(), sChar))
         {
            target.append('_');
         }
         else
         {
            target.append(sChar);
         }
      }
   }


   // object construction (by Ant)

   /**
    * Basic constructor used by Ant (no exceptions should be thrown from here).
    */
   public WiXWriter()
   {
      m_tempDirectory = new File(System.getProperty("java.io.tmpdir"));
   }

   /**
    * Set the alias of the install package (office, cardscan...).
    * 
    * @param sInstallName The install name.
    */
   public void setInstallName(String sInstallName)
   {
      m_sInstallName = sInstallName;
   }

   /**
    * Set the default install root path. If the path starts with
    * "[SystemFolderID]", root the install directory at that directory. Warning:
    * IDs are not validated. (See
    * http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/system_folder_properties.asp
    * for available IDs)
    * 
    * 
    * @param sInstallDirectory A path omitting the leading "C:\"
    * @throws BuildException if the string has invalid formatting.
    */
   public void setInstallDir(String sInstallDirectory) throws BuildException
   {
      Matcher matcher = SYSTEM_FOLDER_REF_PATTERN.matcher(sInstallDirectory);

      // See if a system folder is specified
      if (matcher.find())
      {
         // The system folder comes from inside the brackets
         m_sSystemFolderRoot = sInstallDirectory.substring(1, matcher.end() - 1);

         // The rest of the path comes after
         m_sInstallDirectory = sInstallDirectory.substring(matcher.end());

         // Convert to the correct format for files
         m_sInstallDirectory = new File(m_sInstallDirectory).getPath();

         // Check if it starts with a file separator
         if (m_sInstallDirectory.startsWith(File.separator))
         {
            // Remove the leading separator
            m_sInstallDirectory = m_sInstallDirectory.substring(File.separator.length());
         }
      }
      // Otherwise, no system folder is needed
      else
      {
         m_sSystemFolderRoot = null;

         // Convert to the correct format for files
         m_sInstallDirectory = new File(sInstallDirectory).getPath();
      }
   }
   
   /**
    * Set the version number of the install 
    * 
    * @param sVersion The version provided.
    */
   public void setVersion(String sVersion)
   {
      m_sVersion = sVersion;
   }

   /**
    * Set the directory where WiX is installed.
    * 
    * @param wixDir The WiX directory.
    */
   public void setWixDir(File wixDir)
   {
      m_wixDirectory = wixDir;
   }

   /**
    * Set the source directory where the WXS files are found.
    * 
    * @param sourceDir The source directory.
    */
   public void setSrcDir(File sourceDir)
   {
      m_sourceDirectory = sourceDir;
   }

   /**
    * Set the destination directory MSIs are written to.
    * 
    * @param destinationDir The destination directory.
    */
   public void setDestDir(File destinationDir)
   {
      m_destinationDirectory = destinationDir;
   }

   /**
    * Set the prodcut name used by the MSI filename.
    * 
    * @param productName The product name.
    */
   public void setProduct(String productName)
   {
      m_sProductName = productName;
   }

   /**
    * Ant attribute setter for property to store the compiled filename.
    * 
    * @param sMSIFilenameProperty The filename of the resulting MSI.
    */
   public void setMSIFileProperty(String sMSIFilenameProperty)
   {
      m_sMSIFilenameProperty = sMSIFilenameProperty;
   }

   /**
    * Ant attribute setter for property to store the parsed revision.
    * 
    * @param sExeFilenameProperty The filename of the resulting EXE, if applicable.
    */
   public void setExeFileProperty(String sExeFilenameProperty)
   {
      m_sEXEFilenameProperty = sExeFilenameProperty;
   }

   /**
    * Add a nested ArchiveGroup.
    * 
    * @param archiveGroup The ArchiveGroup being added.
    */
   public void addArchiveGroup(ArchiveGroup archiveGroup)
   {
      m_archiveGroupList.add(archiveGroup);
   }

   /**
    * Factory method for creating nested Resource object.
    * 
    * @param resource The Resource being added.
    */
   public void addResource(Resource resource)
   {
      m_resourceList.add(resource);
   }

   /**
    * Factory method for creating nested Variable object.
    * 
    * @return The created Variable.
    */
   public Variable createVariable()
   {
      Variable variable = new Variable();
      m_variableList.add(variable);
      return variable;
   }

   /**
    * Factory method for creating nested InteropBootstrapper object.
    * 
    * @return The created InteropBootstrapper.
    */
   public InteropBootstrapper createInteropBootstrapper()
   {
      if (m_interopBootstrapper != null)
      {
         throw new BuildException("Duplicate 'interopbootstrapper' element detected");
      }

      m_interopBootstrapper = new InteropBootstrapper();
      return m_interopBootstrapper;
   }

   // public method

   /**
    * Entry point of the Ant task.
    * 
    * @throws BuildException if invalid parameters are used or the build fails.
    */
   public void execute() throws BuildException
   {
      // Initialize: check parameters, create files
      initTask();

      log("Generating WiX fragment");

      // Generate the Wix Fragment
      generateWixFragment();

      // Format the file human readable for debug purposes
      if (m_bDebugModeEnabled)
      {
         formatWixFile();
      }

      // Compile and link the Wix files into an MSI
      log("Building MSI");
      runCandle();
      runLight();
      log("MSI sucessfully built: " + m_sDestinationFile);

      // See if interop bootstrapper is needed
      if (m_interopBootstrapper != null)
      {
         log("Building boostrapper EXE");
         m_interopBootstrapper.makeBootstrapper(m_sourceDirectory, m_destinationDirectory, m_sSetupFileBaseName);
         log("EXE sucessfully built: " + new File(m_destinationDirectory, m_sSetupFileBaseName + ".exe").getAbsolutePath());

         if (m_sEXEFilenameProperty != null)
         {
            m_project.setProperty(m_sEXEFilenameProperty, m_sSetupFileBaseName + ".exe");
         }
      }

      if (m_sMSIFilenameProperty != null)
      {
         m_project.setProperty(m_sMSIFilenameProperty, m_sSetupFileBaseName + ".msi");
      }
   }

   // parameter setup

   /**
    * Set up all of the task parameters.
    */
   private void initTask()
   {
      // Grab a reference to the current project
      m_project = getProject();

      // Get the debug flag
      String sDebug = m_project.getProperty("wix.wixwriter.debug.enabled");

      if (sDebug == null)
      {
         m_bDebugModeEnabled = false;
      }
      else
      {
         m_bDebugModeEnabled = StringUtil.parseBoolean(sDebug);
      }

      // Set the path to the final MSI file which looks like
      // "...\out\deploy\install\nexj-cardscan-setup-x86.msi"
      m_sSetupFileBaseName = m_sProductName + "-" + m_sInstallName + "-setup-" + m_sVersion;
      m_sDestinationFile = new File(m_destinationDirectory, m_sSetupFileBaseName + ".msi").getAbsolutePath();

      // Check that the task attributes are set
      checkAtttributes();

      // Create the temporary file
      try
      {
         m_fragmentFile = File.createTempFile(TEMP_FILE_PREFIX, ".wxs", m_tempDirectory);
         m_fragmentFile.deleteOnExit();

         // Double buffer the XML writer: both the input stream and the output
         // writer
         // Note: Buffering the ouput stream may cause too much overhead to have
         // significant performance advantages
         OutputStream outputStream = new BufferedOutputStream(new FileOutputStream(m_fragmentFile));
         Writer writer = new BufferedWriter(new OutputStreamWriter(outputStream, XMLUtil.ENCODING));
         m_writer = new XMLWriter(writer);
      }
      catch (IOException e)
      {
         raiseIOException("Could not create temporary \"" + TEMP_FILE_PREFIX + ".wxs\" file", e);
      }
   }

   /**
    * Validate that Ant attributes are set.
    */
   private void checkAtttributes()
   {
      checkAttribute(m_sInstallName, "installname");
      checkAttribute(m_sInstallDirectory, "installdir");
      checkAttribute(m_wixDirectory, "wixdir");
      checkAttribute(m_sourceDirectory, "srcdir");
      checkAttribute(m_destinationDirectory, "destdir");
      checkAttribute(m_sProductName, "product");
   }

   /**
    * Assert that an Ant task attribute has been set.
    * 
    * @param value The value to check.
    * @param sAttributeName The name of the Ant attribute.
    * @throws BuildException if the attribute wasn't set.
    */
   private void checkAttribute(Object value, String sAttributeName) throws BuildException
   {
      // If the value hasn't been set, raise a build exception
      if (value == null)
      {
         throw new BuildException("Undefined '" + sAttributeName + "' attribute of " + getTaskName());
      }
   }

   // wix fragment generation

   /**
    * Do the work of filling up the XML WiX fragment file.
    * 
    * @throws BuildException if unable to write to the temporary file.
    */
   private void generateWixFragment() throws BuildException
   {
      try
      {
         // Write the start of the WXS file
         writeWixHeader();

         // Nothing to do if no archive groups
         if (!m_archiveGroupList.isEmpty())
         {
            // Write out the install directories
            writeInstallDirectories();

            // Organize the disk IDs
            organizeDiskIds();

            // Write out each archive group, continuing to increment the
            // component ID
            writeArchiveGroups();

            // Write the component groups
            writeComponentGroups();
         }

         // Write the resource (Binary and Icon) elements
         writeResources();

         // Write the end of the WXS file
         writeWixFooter();
      }
      catch (IOException e)
      {
         raiseIOException("Could not write to temporary \"" + TEMP_FILE_PREFIX + ".wxs\" file", e);
      }
   }

   // wix fragment generation

   /**
    * Write the install directories of the archive groups so they can be
    * referenced later.
    * 
    * @throws BuildException if an archive group's install directory ID isn't
    *            set.
    * @throws IOException if an I/O error occurs.
    */
   private void writeInstallDirectories() throws IOException
   {
      IdentifierFactory identifierFactory = new IdentifierFactory();

      // Open the root directory reference
      m_writer.openElement("DirectoryRef");
      m_writer.writeAttribute("Id", ROOT_DIRECTORY_ID);
      m_writer.closeElement();

      // Open the system folder if needed
      // Note: if the ID has been used in another WXS file, writing it here
      // will cause a collision
      if (m_sSystemFolderRoot != null)
      {
         openDirectoryElement(m_sSystemFolderRoot, m_sSystemFolderRoot);
      }

      // Break the install directory into parent and child parts
      File installDirFile = new File(m_sInstallDirectory);
      File installDirParent = installDirFile.getParentFile();
      String sInstallDirChild;

      if (installDirParent == null)
      {
         sInstallDirChild = m_sInstallDirectory;
      }
      else
      {
         sInstallDirChild = installDirFile.getName();
      }

      // Open the install directory parent if needed
      int nInstallDirParentDepth = 0;

      if (installDirParent != null)
      {
         // Get the stack of directories
         Stack parents = new Stack();
         File parentDir = installDirParent;

         do
         {
            parents.push(parentDir);
            parentDir = parentDir.getParentFile();
         }
         while (parentDir != null);

         // Mark the depth
         nInstallDirParentDepth = parents.size();

         // Open each directory
         do
         {
            parentDir = (File)parents.pop();

            openDirectoryElement(parentDir.getName(), identifierFactory.getUniqueId(parentDir.getPath()));
         }
         while (!parents.isEmpty());

      }

      // Keep track of which root directories have been written
      Lookup installDirIdSet = new HashTab();

      // Write out each of the non-subdirectory install directories
      for (Iterator it = m_archiveGroupList.iterator(); it.hasNext();)
      {
         ArchiveGroup archiveGroup = (ArchiveGroup)it.next();
         String sInstallDirId = archiveGroup.getInstallDirId();
         String sSubdir = archiveGroup.getSubdir();

         // Check the install directory
         if (sInstallDirId == null)
         {
            throw new BuildException("Undefined 'installdirid' attribute of archivegroup");
         }

         // Only write each install directory once
         if (sSubdir == null && !installDirIdSet.contains(sInstallDirId))
         {
            // Write out the ID'd top directory
            openDirectoryElement(sInstallDirChild, sInstallDirId);
            closeDirectoryElement();

            installDirIdSet.put(sInstallDirId, null);
         }
      }

      // Write out each of the subdirectory install directories
      openDirectoryElement(sInstallDirChild, identifierFactory.getUniqueId(m_sInstallDirectory));

      for (Iterator it = m_archiveGroupList.iterator(); it.hasNext();)
      {
         ArchiveGroup archiveGroup = (ArchiveGroup)it.next();
         String sInstallDirId = archiveGroup.getInstallDirId();
         String sSubdir = archiveGroup.getSubdir();

         // Only write each install directory once
         if (sSubdir != null && !installDirIdSet.contains(sInstallDirId))
         {
            // Write out the ID'd top directory
            openDirectoryElement(sSubdir, sInstallDirId);
            closeDirectoryElement();

            installDirIdSet.put(sInstallDirId, null);
         }
      }

      closeDirectoryElement();

      // Close the install directory parent if needed
      for (int i = 0; i < nInstallDirParentDepth; i++)
      {
         closeDirectoryElement();
      }

      // Close the system folder if it was opened
      if (m_sSystemFolderRoot != null)
      {
         closeDirectoryElement();
      }

      // Close the root directory reference
      m_writer.endElement("DirectoryRef");
   }

   /**
    * Open a directory element.
    * 
    * @param sName The name of the directory.
    * @param sId The directory's unique ID.
    * @throws IOException if an I/O error occurs.
    */
   private void openDirectoryElement(String sName, String sId) throws IOException
   {
      String sShortName = getShortName(sName);

      m_writer.openElement("Directory");
      m_writer.writeAttribute("Id", sId);
      m_writer.writeAttribute("Name", sShortName);

      if (!sShortName.equals(sName))
      {
         m_writer.writeAttribute("LongName", sName);
      }

      m_writer.closeElement();
   }

   /**
    * Close a directory element.
    * 
    * @throws IOException if an I/O error occurs.
    */
   private void closeDirectoryElement() throws IOException
   {
      m_writer.endElement("Directory");
   }

   /**
    * Retrieve the component group IDs from the archive groups. These IDs are
    * "keys" to the numeric disk IDs: all components in the same component group
    * have the same disk ID. A component group's position in the sorted list
    * corresponds to the disk ID.
    */
   private void organizeDiskIds()
   {
      // Get each ID (assume each is unique)
      for (Iterator it = m_archiveGroupList.iterator(); it.hasNext();)
      {
         ArchiveGroup archiveGroup = (ArchiveGroup)it.next();
         String componentGroupId = archiveGroup.getComponentGroupId();

         // Check that the ID's been set
         if (componentGroupId == null)
         {
            throw new BuildException("Neither 'id' nor 'diskid' attribute set on archivegroup");
         }

         // Add the ID to the list
         m_componentGroupIdList.add(componentGroupId);
      }

      // Sort the list
      Collections.sort(m_componentGroupIdList);
   }

   /**
    * Write the archive groups specified the Ant build file.
    * 
    * @throws IOException if an I/O error occurs.
    */
   private void writeArchiveGroups() throws IOException
   {
      // Star the component numbers at zero
      int nStartComponentNumber = 0;

      // Write each archive group, continuing component numbering between them
      // Each archive group will be rooted at a directory reference to the
      // previously written install directory
      for (Iterator it = m_archiveGroupList.iterator(); it.hasNext();)
      {
         ArchiveGroup archiveGroup = (ArchiveGroup)it.next();

         // Get the component group ID
         String sComponentGroupId = archiveGroup.getComponentGroupId();

         // Determine the numeric disk ID (its position in the list)
         int nDiskIdNumber = Collections.binarySearch(m_componentGroupIdList, sComponentGroupId) + 1;

         // Write the archive group
         archiveGroup.write(m_writer, nDiskIdNumber, nStartComponentNumber);

         // Get the next component number
         int nEndComponentNumber = archiveGroup.getNextComponentNumber();

         // Add the components just written to the component group
         addToComponentGroup(nStartComponentNumber, nEndComponentNumber, sComponentGroupId);

         // Restart the component numbering
         nStartComponentNumber = nEndComponentNumber;
      }
   }

   /**
    * Add a set of components (from an archive group) to a component group.
    * 
    * @param nStartComponentNumber The first component number inclusive.
    * @param nEndComponentNumber The last component number exclusive.
    * @param sComponentGroupId The ID of the component group.
    */
   private void addToComponentGroup(int nStartComponentNumber, int nEndComponentNumber, String sComponentGroupId)
   {
      // Add this component to the list of other components in this component
      // group
      ArrayList diskIdList = (ArrayList)m_componentGroupMap.get(sComponentGroupId);

      // If needed start a new list
      if (diskIdList == null)
      {
         diskIdList = new ArrayList();
         m_componentGroupMap.put(sComponentGroupId, diskIdList);
      }

      // Add each component ID to the list
      for (int i = nStartComponentNumber; i < nEndComponentNumber; i++)
      {
         String sId = "Component" + i;
         diskIdList.add(sId);
      }
   }

   /**
    * Write WiX ComponentGroup element for grouping components into their
    * respective archive.
    * 
    * @throws IOException if an I/O error occurs.
    */
   private void writeComponentGroups() throws IOException
   {
      for (Iterator itMap = m_componentGroupMap.iterator(); itMap.hasNext();)
      {
         String sDiskId = (String)itMap.next();
         ArrayList list = (ArrayList)m_componentGroupMap.get(sDiskId);

         m_writer.openElement("ComponentGroup");
         m_writer.writeAttribute("Id", sDiskId);
         m_writer.closeElement();

         for (Iterator itList = list.iterator(); itList.hasNext();)
         {
            m_writer.openElement("ComponentRef");
            m_writer.writeAttribute("Id", itList.next().toString());
            m_writer.closeEmptyElement();
         }

         m_writer.endElement("ComponentGroup");
      }
   }

   /**
    * Write Wix resource (Binary and Icon) elements, based on the custom ANT
    * subtask Resource and its subtasks WixBinary and WixIcon.
    * 
    * @throws IOException if an I/O error occurs.
    */
   private void writeResources() throws IOException
   {
      for (Iterator it = m_resourceList.iterator(); it.hasNext();)
      {
         Resource resource = (Resource)it.next();
         resource.write(m_writer);
      }
   }

   /**
    * Write the start of the WiX fragment.
    * 
    * @throws IOException if an I/O error occurs.
    */
   private void writeWixHeader() throws IOException
   {
      m_writer.openElement("Wix");
      m_writer.writeAttribute("xmlns", "http://schemas.microsoft.com/wix/2003/01/wi");
      m_writer.closeElement();

      m_writer.startElement("Fragment");
   }

   /**
    * Write the end of the WiX fragment.
    * 
    * @throws IOException if an I/O error occurs.
    */
   private void writeWixFooter() throws IOException
   {
      m_writer.endElement("Fragment");
      m_writer.endElement("Wix");
      m_writer.close();
   }

   // reformatting

   /**
    * Perform fomatting on the WiX fragment before it's given to Candle.
    * 
    * @throws BuildException if cannot write to temporary file.
    */
   private void formatWixFile() throws BuildException
   {
      Writer writer = null;

      try
      {
         StreamSource ss = new StreamSource(m_fragmentFile);
         File formattedFile = File.createTempFile(TEMP_FILE_PREFIX + "formatted_", ".wxs", m_tempDirectory);

         writer = IOUtil.openBufferedWriter(formattedFile, XMLUtil.ENCODING);
         XMLUtil.formatXML(ss, true, writer);
         writer.close();
         writer = null;
      }
      catch (IOException e)
      {
         raiseIOException("Could not write to temporary \"" + TEMP_FILE_PREFIX + ".wxs\" file", e);
      }
      finally
      {
         if (writer != null)
         {
            try
            {
               writer.close();
            }
            catch (IOException e)
            {
            }
         }
      }
   }

   // io exception

   /**
    * Give IO exception details and halt the build.
    * 
    * @param sMesssage The build error.
    * @param e The cause exception.
    * @throws BuildException to halt the build.
    */
   private void raiseIOException(String sMesssage, IOException e) throws BuildException
   {
      log(sMesssage, ERROR_LEVEL);
      e.printStackTrace();
      throw new BuildException(sMesssage, e);
   }

   // wix execution

   /**
    * Runs Candle on the fragment and the base file, checking that it runs
    * successfully.
    * 
    * @throws BuildException if failed to run Candle or Candle fails.
    */
   private void runCandle() throws BuildException
   {
      int nExitValue;

      try
      {
         ArrayList argList = new ArrayList();

         // Candle executable
         argList.add(new File(m_wixDirectory, "candle.exe").getAbsolutePath());

         // Generated input file
         argList.add(m_fragmentFile.getAbsolutePath());

         // Specific base WXS input file
         argList.add(new File(m_sourceDirectory, m_sInstallName + ".wxs").getAbsolutePath());

         // Disk ID defines (the name of the variable is the component group ID)
         int nDiskIdNumber = 1;
         for (Iterator it = m_componentGroupIdList.iterator(); it.hasNext();)
         {
            String sDiskId = (String)it.next();
            argList.add("-d" + sDiskId + "=" + nDiskIdNumber);
            nDiskIdNumber++;
         }

         // Custom variable defines
         for (Iterator it = m_variableList.iterator(); it.hasNext();)
         {
            Variable variable = (Variable)it.next();
            argList.add(variable.getDefineParameter());
         }

         // Output folder
         // NOTE: in WIX, if you specify quotes around a directory (e.g. a path
         // that ends in \) you must end it // with \\ instead.
         // E.g. "C:\temp\" will give an error, use "C:\temp\\" instead.
         argList.add("-out");
         argList.add(m_tempDirectory.getAbsolutePath() + File.separator + File.separator);

         // Run candle
         Execute candle = new Execute();
         candle.setAntRun(m_project);
         candle.setCommandline((String[])argList.toArray(new String[0]));
         candle.execute();

         nExitValue = candle.getExitValue();
      }
      catch (Exception e)
      {
         e.printStackTrace();
         throw new BuildException("Fatal exception occurred while running WiX compiler Candle", e);
      }

      if (nExitValue != 0)
      {
         log("Candle failed (exit value " + nExitValue + ")", ERROR_LEVEL);
         throw new BuildException("WiX compiler Candle failed");
      }
   }

   /**
    * Runs Light to generate the MSI, checking that it runs successfully, and
    * deleting the .wixobj files afterwards.
    * 
    * @throws BuildException if failed to run Light or Light fails.
    */
   private void runLight() throws BuildException
   {
      int nExitValue;

      try
      {
         ArrayList argList = new ArrayList();

         // Light executable
         argList.add(new File(m_wixDirectory, "light.exe").getAbsolutePath());

         // Generated input file's object file
         String sGeneratedObj = m_fragmentFile.getAbsolutePath();
         sGeneratedObj = sGeneratedObj.substring(0, sGeneratedObj.lastIndexOf('.')) + ".wixobj";
         argList.add(sGeneratedObj);

         // Specific base WXS input file's object file
         String sSpecificObj = new File(m_tempDirectory, m_sInstallName + ".wixobj").getAbsolutePath();
         argList.add(sSpecificObj);

         // Wixlib file
         argList.add(new File(m_wixDirectory, "sca.wixlib").getAbsolutePath());

         // Output MSI file
         argList.add("-out");
         argList.add(m_sDestinationFile);

         // Run light
         Execute light = new Execute();
         light.setAntRun(m_project);
         light.setCommandline((String[])argList.toArray(new String[0]));
         light.execute();

         nExitValue = light.getExitValue();

         // Delete *.wixobj files
         new File(sGeneratedObj).deleteOnExit();
         new File(sSpecificObj).deleteOnExit();
      }
      catch (Exception e)
      {
         e.printStackTrace();
         throw new BuildException("Fatal exception occurred while running WiX linker Light", e);
      }

      if (nExitValue != 0)
      {
         log("Light failed (exit value " + nExitValue + ")", ERROR_LEVEL);
         throw new BuildException("WiX linker Light failed");
      }
   }

   /**
    * Ant type for packaging file structures (supports Ant references).
    */
   public static class ArchiveGroup extends DataType
   {
      /**
       * Pseudonym for the Global Assembly Cache, used as a prefix for
       * calculating hashes.
       */
      private final static String GAC_FOLDER_NAME = "Global Assembly Cache";

      /**
       * ID for reference, doubles as disk ID.
       */
      private String m_sId = null;

      /**
       * Component group ID for the archive group.
       */
      private String m_sComponentGroupId = null;

      /**
       * Installation directory for the archive group.
       */
      private String m_sInstallDirectoryId;

      /**
       * The name of the subdirectory where the archive group is rooted (if
       * applicable).
       */
      private String m_sRootSubdirectory;

      /** 
       * The WiX condition for a given build archivegroup / Wix component
       */
      private String m_sCondition;

      /**
       * List of all FileSets in the ArchiveGroup.
       */
      private ArrayList m_fileSetList = new ArrayList();

      /**
       * List of all Assemblies in the ArchiveGroup.
       */
      private ArrayList m_assemblyList = new ArrayList();

      /**
       * For generating unique or random IDs.
       */
      private IdentifierFactory m_identifierFactory = new IdentifierFactory();

      /**
       * The Ant project.
       */
      private Project m_project;

      /**
       * Handle to the writer for the WiX file fragment.
       */
      private static XMLWriter m_writer = null;

      /**
       * Numeric disk ID for the archive group.
       */
      private int m_nDiskIdNumber;

      /**
       * Next component number to use.
       */
      private int m_nComponentNumber;

      /**
       * Architecture specified in components.
       */
      private String m_sArch;

      /**
       * Constructor used by Ant.
       */
      public ArchiveGroup()
      {
      }

      /**
       * Set the processsor architecture for which the install is intended (x86 or
       * x84).
       * 
       * @param sArch The architecture, "x86" or "x64".
       */
      public void setArch(String sArch)
      {
         m_sArch = sArch;
      }

      /**
       * Set the ID used for reference.
       * 
       * @param sId The ID.
       */
      public void setId(String sId)
      {
         m_sId = sId;
      }

      /**
       * Set the media ID used by all components. As a string, it specifies the
       * component group ID: the real disk ID number will be calculated later.
       * 
       * @param sId The media ID.
       */
      public void setDiskId(String sId)
      {
         m_sComponentGroupId = sId;
      }

      /**
       * Set the ID of the install directory for reference in WXS files.
       * 
       * @param sInstallDirectoryId The ID of the install directory.
       */
      public void setInstallDirId(String sInstallDirectoryId)
      {
         m_sInstallDirectoryId = sInstallDirectoryId;
      }

      /**
       * Set the root subdirectory.
       * 
       * @param sRootSubdirectory The root subdirectory.
       */
      public void setSubdir(String sRootSubdirectory)
      {
         m_sRootSubdirectory = sRootSubdirectory;
      }

      /**
       * Set condition for this ArchiveGroup.
       * 
       * @param sRootSubdirectory The root subdirectory.
       */
      public void setCondition(String sCondition)
      {
         m_sCondition = sCondition;
      }

      /**
       * Add a file set to the archive group.
       * 
       * @param fileSet The FileSet to add.
       */
      public void addFileSet(FileSet fileSet)
      {
         m_fileSetList.add(fileSet);
      }

      /**
       * Add a zip file set to the archive group.
       * 
       * @param fileSet The FileSet to add.
       */
      public void addZipFileSet(ZipFileSet fileSet)
      {
         m_fileSetList.add(fileSet);
      }

      /**
       * Add an assembly (for the GAC) to the archive group.
       * 
       * @return The created Assembly.
       */
      public Assembly createAssembly()
      {
         Assembly assembly = new Assembly();
         m_assemblyList.add(assembly);
         return assembly;
      }

      /**
       * Get the root directory ID (dereference if needed).
       * 
       * @return The root directory ID.
       */
      public String getInstallDirId()
      {
         if (isReference())
         {
            return ((ArchiveGroup)getCheckedRef(ArchiveGroup.class, "archivegroup")).getInstallDirId();
         }
         else
         {
            return m_sInstallDirectoryId;
         }
      }

      /**
       * Get the root subdirectory.
       * 
       * @return The root subdirectory.
       */
      public String getSubdir()
      {
         if (isReference())
         {
            return ((ArchiveGroup)getCheckedRef(ArchiveGroup.class, "archivegroup")).getSubdir();
         }
         else
         {
            return m_sRootSubdirectory;
         }
      }

      /**
       * Get the disk ID (dereference if needed). If unset, default to the
       * archive group's ID.
       * 
       * @return The disk ID.
       */
      public String getComponentGroupId()
      {
         if (isReference())
         {
            return ((ArchiveGroup)getCheckedRef(ArchiveGroup.class, "archivegroup")).getComponentGroupId();
         }
         else
         {
            if (m_sComponentGroupId != null)
            {
               return m_sComponentGroupId;
            }
            else
            {
               return m_sId;
            }
         }
      }

      /**
       * Write the contents of the archive group: file sets and assemblies
       * (dereference if needed).
       * 
       * @param writer The writer for the WiX fragment.
       * @param nDiskIdNumber The numeric disk ID used by all components.
       * @param nComponentNumber The starting component number.
       * @param sArch The target processor architecture used by components.
       * @throws IOException if an I/O error occurs.
       */
      public void write(XMLWriter writer, int nDiskIdNumber, int nComponentNumber) throws IOException
      {
         if (isReference())
         {
            ((ArchiveGroup)getCheckedRef(ArchiveGroup.class, "archivegroup")).write(writer, nDiskIdNumber, nComponentNumber);
         }
         else
         {
            // Grab a reference to the current project
            m_project = getProject();

            // Set the XML writer
            m_writer = writer;

            // Make sure the component group ID is set
            if (m_sComponentGroupId == null)
            {
               m_sComponentGroupId = m_sId;
            }

            // Start the component counting
            m_nComponentNumber = nComponentNumber;

            // Save the disk ID number
            m_nDiskIdNumber = nDiskIdNumber;

            // Open a directory reference to the install directory
            openDirReference();

            // Start with empty lists
            FileTree fileTree = new FileTree();

            // Scan each file set
            for (Iterator it = m_fileSetList.iterator(); it.hasNext();)
            {
               FileSet fileSet = (FileSet)it.next();
               scanFileSet(fileSet, fileTree);
            }

            // The hashed prefix is the ID of the component group (to allow
            // duplicate trees)
            m_identifierFactory.setPrefixDir(m_sComponentGroupId);

            // Write out the filesets
            fileTree.write();

            // Write the assemblies
            m_identifierFactory.setPrefixDir(GAC_FOLDER_NAME);

            for (Iterator it = m_assemblyList.iterator(); it.hasNext();)
            {
               Assembly assembly = (Assembly)it.next();
               assembly.write();
            }

            // Close the directory reference
            closeDirReference();
         }
      }

      /**
       * Get the next component number that the next component should use.
       * 
       * @return The next component number.
       */
      public int getNextComponentNumber()
      {
         if (isReference())
         {
            return ((ArchiveGroup)getCheckedRef(ArchiveGroup.class, "archivegroup")).getNextComponentNumber();
         }
         else
         {
            return m_nComponentNumber;
         }
      }

      /**
       * Open the reference to the root directory.
       * 
       * @throws IOException if an I/O error occurs.
       */
      private void openDirReference() throws IOException
      {
         m_writer.openElement("DirectoryRef");
         m_writer.writeAttribute("Id", m_sInstallDirectoryId);
         m_writer.closeElement();
         log("<directoryref id=\"" + m_sInstallDirectoryId + "\">", DEBUG_LEVEL);
      }

      /**
       * Close the reference to the root directory.
       * 
       * @throws IOException if an I/O error occurs.
       */
      private void closeDirReference() throws IOException
      {
         m_writer.endElement("DirectoryRef");
         log("</directoryref>", DEBUG_LEVEL);
      }

      /**
       * Add the files and directories from the file set to the file tree.
       * Process any extra parameters on the file set.
       * 
       * @param fileSet The file set.
       * @param fileTree The file tree.
       * @throws BuildException if the file set is invalid.
       */
      private void scanFileSet(FileSet fileSet, FileTree fileTree) throws BuildException
      {
         // The prefix can be set using a zip file set
         String sPrefix = "";

         // Check for ZipFileSet-specific features
         if (fileSet instanceof ZipFileSet)
         {
            ZipFileSet zipFileSet = (ZipFileSet)fileSet;

            // Check if the file set comes from inside a zip file
            if (zipFileSet.getSrc(m_project) != null)
            {
               unzipFileSet(zipFileSet);
            }

            // Check if the full path modes is being used
            if (zipFileSet.getFullpath(m_project) != null && !zipFileSet.getFullpath(m_project).equals(""))
            {
               // Add the single file instead of following the regular procedure
               fileTree.addFullPathFile(zipFileSet);
               return;
            }

            // Use prefix directories if needed
            sPrefix = ((ZipFileSet)fileSet).getPrefix(m_project);

            if (!sPrefix.equals(""))
            {
               // Fix the formatting and append a separator
               sPrefix = new File(sPrefix).getPath() + File.separator;
            }
         }

         // Use the file set's scanner
         // If the file set is invalid, FileSet.getDirectoryScanner will
         // raise an exception
         DirectoryScanner scanner = fileSet.getDirectoryScanner(m_project);

         // Check that the file set isn't empty
         if (scanner.getIncludedDirsCount() == 0 && scanner.getIncludedFilesCount() == 0)
         {
            throw new BuildException("Fileset at \"" + scanner.getBasedir().getAbsolutePath()
               + "\" does not include any files or directories (for more info, run Ant with \"-debug\")");
         }

         // Grab the directories and the files
         fileTree.addScanner(scanner, sPrefix);
      }

      /**
       * Unzip the zip file, point the file set to the unzipped files, and make
       * sure the files get deleted on exit.
       * 
       * @param zipFileSet The file set.
       */
      private void unzipFileSet(ZipFileSet zipFileSet)
      {
         // Get the name of a temporary output folder
         File outFolder = FileUtils.getFileUtils().createTempFile(TEMP_FILE_PREFIX, "", null, false, false);

         // Unzip the zip file
         Expand unzipper = (Expand)m_project.createTask("unzip");
         unzipper.setSrc(zipFileSet.getSrc(m_project));
         unzipper.setDest(outFolder);
         unzipper.execute();

         // Change the file set to use the new directory
         zipFileSet.setSrcResource(null); 
         zipFileSet.setDir(outFolder);

         // Delete the folder on exit
         deleteDirectoryOnExit(outFolder);
      }

      /**
       * Preorder recursively mark each item under the directory as "delete on
       * exit" so that they actually get deleted postorder on exit.
       * 
       * @param directory The starting directory.
       */
      private void deleteDirectoryOnExit(File directory)
      {
         // Delete base directory
         directory.deleteOnExit();

         // Delete files and folders inside
         File[] memberList = directory.listFiles();

         for (int i = 0; i < memberList.length; i++)
         {
            if (memberList[i].isDirectory())
            {
               // Go into directory
               deleteDirectoryOnExit(memberList[i]);
            }
            else
            {
               // Delete file
               memberList[i].deleteOnExit();
            }
         }
      }

      /**
       * Open a component. Key the GUID of the component usually by using the
       * first file in it.
       * 
       * @param sKeyPath The path which the component's GUID is generated from.
       * @throws IOException if an I/O error occurs.
       */
      private void openComponentElement(String sKeyPath) throws IOException
      {
         // Generate Component IDs by incrementing m_nComponents. This is better
         // than the other method of Component IDs being path names
         String sId = "Component" + m_nComponentNumber;
         String sGuid = m_identifierFactory.getGUID(sKeyPath);

         m_writer.openElement("Component");
         m_writer.writeAttribute("Id", sId);
         m_writer.writeAttribute("Guid", sGuid);
         m_writer.writeAttribute("DiskId", m_nDiskIdNumber);

         // Writing Win64 attribute based on architecture type
         if (m_sArch != null)
         {
            if (m_sArch.equals("x64"))
            {
               m_writer.writeAttribute("Win64", "yes");
            }
            else
            {
               m_writer.writeAttribute("Win64", "no");
            }
         }

         m_writer.closeElement();

         log("..<component>", DEBUG_LEVEL);

         m_nComponentNumber++;
      }

      /**
       * Close a component.
       * 
       * @throws IOException if an I/O error occurs.
       */
      private void closeComponentElement() throws IOException
      {
         // Write the condition            
         if (m_sCondition != null)
         {
            m_writer.startElement("Condition");
            m_writer.write(m_sCondition);
            m_writer.endElement("Condition");
         }

         m_writer.endElement("Component");
         log("..</component>", DEBUG_LEVEL);
      }

      /**
       * A file that needs to go into the Global Assembly Cache (set up by Ant).
       */
      public class Assembly
      {
         /**
          * Source file.
          */
         private File m_file;

         /**
          * Constructor.
          */
         public Assembly()
         {
         }

         /**
          * Set the source file.
          * 
          * @param file The source file.
          */
         public void setFile(File file)
         {
            m_file = file;
         }

         /**
          * Write out the assembly in its own component.
          * 
          * @throws BuildException if source file doesn't exist.
          * @throws IOException if an I/O error occurs.
          */
         public void write() throws BuildException, IOException
         {
            // Open a new component
            openComponentElement(m_file.getName());

            String sName = m_file.getName();
            String sShortName = getShortName(sName);
            String sId = m_identifierFactory.getUniqueId(sName);

            String sSourcePath;

            try
            {
               sSourcePath = m_file.getCanonicalPath();
            }
            catch (IOException e)
            {
               throw new BuildException("File \"" + m_file.getAbsolutePath() + "\" does not exist");
            }

            // Write the assembly file element
            m_writer.openElement("File");
            m_writer.writeAttribute("Id", sId);
            m_writer.writeAttribute("Name", sShortName);

            if (!sShortName.equals(sName))
            {
               m_writer.writeAttribute("LongName", sName);
            }

            m_writer.writeAttribute("Source", sSourcePath);
            m_writer.writeAttribute("Vital", "yes");

            // Tell the installer this is an assembly
            m_writer.writeAttribute("KeyPath", "yes");
            m_writer.writeAttribute("Assembly", ".net");

            // Close the file element
            m_writer.closeEmptyElement();
            
            log("....<assembly name=\"" + sName + "\"/>", DEBUG_LEVEL);

            // Close the component
            closeComponentElement();
         }
      }
      
      /**
       * A structure of files and directories. Build using directory scanners,
       * writes out the structure as an XML tree.
       */
      private class FileTree
      {
         /**
          * Preorder list of directories and files.
          */
         private ArrayList m_installItemList;

         /**
          * The directory hierarchy.
          */
         Stack m_stack;

         /**
          * The path of the current directory.
          */
         String m_sCurrentDirectory;

         /**
          * Whether this directory has anything in it.
          */
         boolean m_bEmptyDir = true;

         /**
          * Whether a component is open.
          */
         boolean m_bComponentOpened;

         /**
          * Create an empty tree.
          */
         public FileTree()
         {
            // The list is empty
            m_installItemList = new ArrayList();
         }

         /**
          * Add the included directories and directories from a scanner to the
          * tree. The prefix is prefixed to each path and source paths for files
          * are noted.
          * 
          * @param scanner The scanner.
          * @param sPrefix The prefix.
          */
         public void addScanner(DirectoryScanner scanner, String sPrefix)
         {
            // Grab the directory list
            String list[] = scanner.getIncludedDirectories();

            // Make room for the list
            m_installItemList.ensureCapacity(m_installItemList.size() + list.length);

            // Add anything not empty
            for (int i = 0; i < list.length; i++)
            {
               if (!list[i].equals(""))
               {
                  // Use the prefix
                  // Directories need to end in '\' so that we get the correct
                  // order
                  // "a-b\", "a-b\c\", "a\", "a\c\" (NOT "a", "a-b", "a-b\c",
                  // "a\c")
                  // Correct order is important for initial sorting and later
                  // comparing with other paths
                  m_installItemList.add(new InstallDirectory(sPrefix + list[i] + File.separator));
               }
            }

            // Grab the file list
            list = scanner.getIncludedFiles();

            // Get the base directory
            String sBaseDir = scanner.getBasedir().getPath();

            // Make room for the files
            m_installItemList.ensureCapacity(m_installItemList.size() + list.length);

            // Add each file to the list
            for (int i = 0; i < list.length; i++)
            {
               m_installItemList.add(new InstallFile(list[i], sPrefix, sBaseDir));
            }
         }

         /**
          * Add the full path file included by the file set.
          * 
          * @param zipFileSet The file set.
          * @throws BuildException if file set is invalid.
          */
         public void addFullPathFile(ZipFileSet zipFileSet) throws BuildException
         {
            // Grab the file list
            DirectoryScanner scanner = zipFileSet.getDirectoryScanner(m_project);
            String sFiles[] = scanner.getIncludedFiles();

            // Assert there's only one file
            if (sFiles.length != 1)
            {
               throw new BuildException("zipfileset 'fullpath' attribute set but fileset didn't include only one file");
            }

            // Get the install path
            String sInstallPath = zipFileSet.getFullpath(m_project);

            // Find the full path to the real file
            String sSourcePath = new File(scanner.getBasedir(), sFiles[0]).getPath();

            // Add the file to the list
            m_installItemList.add(new InstallFile(sInstallPath, sSourcePath));
         }

         /**
          * Write out the file tree.
          * 
          * @throws IOException if an I/O error occurs.
          */
         public void write() throws IOException
         {
            // Start with the empty directory
            m_stack = new Stack();
            m_stack.push("");
            m_sCurrentDirectory = "";
            m_bEmptyDir = true;
            m_bComponentOpened = false;

            // Sort the list into a preorder tree
            Collections.sort(m_installItemList);

            // Write out each item
            for (int i = 0; i < m_installItemList.size(); i++)
            {
               ((InstallItem)m_installItemList.get(i)).write();
            }

            // Close any open component
            if (m_bComponentOpened)
            {
               closeComponentElement();
            }

            // Close the remaining directories
            while (m_stack.size() > 1)
            {
               pop();
            }

            // Check if the root directory was empty
            if (m_bEmptyDir)
            {
               makeEmptyDirectory();
            }
         }

         /**
          * Close and open directory elements as needed to travserse to the
          * target directory.
          * 
          * @param sTargetDir The target directory.
          * @throws IOException if an I/O error occurs.
          */
         private void traverseTo(String sTargetDir) throws IOException
         {
            // See if traversal needed
            if (sTargetDir.equals(m_sCurrentDirectory))
            {
               return;
            }

            // Close any open component
            if (m_bComponentOpened)
            {
               closeComponentElement();
               m_bComponentOpened = false;
            }

            // Pop directories until a common directory is found
            while (!sTargetDir.startsWith(m_sCurrentDirectory))
            {
               pop();
            }

            // See traversal was just pops
            if (sTargetDir.equals(m_sCurrentDirectory))
            {
               return;
            }

            // See if parent directories are needed
            String sParentDir = new File(sTargetDir).getParent();
            boolean bParentNeeded;

            if (sParentDir == null)
            {
               bParentNeeded = false;
            }
            else
            {
               bParentNeeded = !m_sCurrentDirectory.startsWith(sParentDir)
                  || sParentDir.length() != m_sCurrentDirectory.length() - 1;
            }

            if (bParentNeeded)
            {
               Stack parents = new Stack();

               // See how many levels of parents need to be pushed onto the
               // stack
               sParentDir = sParentDir + File.separator;

               do
               {
                  parents.push(sParentDir);

                  sParentDir = new File(sParentDir).getParent();

                  if (sParentDir == null)
                  {
                     break;
                  }
                  else
                  {
                     sParentDir = sParentDir + File.separator;
                  }
               }
               while (!sParentDir.equals(m_sCurrentDirectory));

               // Write the 'tower' of directories
               do
               {
                  push((String)parents.pop());
               }
               while (!parents.isEmpty());
            }

            // Open the target itself
            push(sTargetDir);
         }

         /**
          * Push a directory onto the stack.
          * 
          * @param sDirectory The directory.
          * @throws IOException if an I/O error occurs.
          */
         private void push(String sDirectory) throws IOException
         {
            String sName = new File(sDirectory).getName();
            String sShortName = getShortName(sName);
            String sId = m_identifierFactory.getUniqueId(sDirectory);

            // Open the directory element
            m_writer.openElement("Directory");
            m_writer.writeAttribute("Id", sId);
            m_writer.writeAttribute("Name", sShortName);

            if (!sShortName.equals(sName))
            {
               m_writer.writeAttribute("LongName", sName);
            }

            m_writer.closeElement();

            log("<directory name=\"" + sName + "\">", DEBUG_LEVEL);

            // Push this directory onto the stack
            m_stack.push(sDirectory);
            m_sCurrentDirectory = (String)m_stack.peek();

            // Nothing has been written in this directory
            m_bEmptyDir = true;
         }

         /**
          * Take a directory off the stack.
          * 
          * @throws IOException if an I/O error occurs.
          */
         private void pop() throws IOException
         {
            // If the last directory had nothing in it
            if (m_bEmptyDir)
            {
               // Use a 'create directory' element
               makeEmptyDirectory();

               // If we go down another level, that directory isn't empty
               m_bEmptyDir = false;
            }

            // Close the current directory element
            m_writer.endElement("Directory");
            log("</directory>", DEBUG_LEVEL);

            // Pop it off the stack
            m_stack.pop();
            m_sCurrentDirectory = (String)m_stack.peek();
         }

         /**
          * Make an empty directory using the WiX CreateFolder element.
          * 
          * @throws IOException if an I/O error occurs.
          */
         private void makeEmptyDirectory() throws IOException
         {
            // Open a new component
            openComponentElement(m_sCurrentDirectory);

            // Write the "create folder" element
            m_writer.openElement("CreateFolder");
            m_writer.closeEmptyElement();
            log("....<emptydir/>", DEBUG_LEVEL);

            // Close the component
            closeComponentElement();
         }

         /**
          * A file or directory to be installed. Sortable based on the install
          * path.
          */
         private abstract class InstallItem implements Comparable
         {
            /**
             * Path to be installed at.
             */
            private String m_sInstallPath;

            /**
             * Create an item.
             * 
             * @param sInstallPath The install path.
             */
            protected InstallItem(String sInstallPath)
            {
               m_sInstallPath = sInstallPath;
            }

            /**
             * Compare by install path.
             * 
             * @see java.lang.Comparable#compareTo(java.lang.Object)
             */
            public int compareTo(Object other)
            {
               return m_sInstallPath.compareTo(((InstallItem)other).m_sInstallPath);
            }

            /**
             * Write out item, traversing to directories as needed.
             * 
             * @throws IOException if an I/O error occurs.
             */
            public abstract void write() throws IOException;

            /**
             * Get the install path.
             * 
             * @return The install path.
             */
            protected String getPath()
            {
               return m_sInstallPath;
            }
         }

         /**
          * A directory to be installed.
          */
         private class InstallDirectory extends InstallItem
         {
            /**
             * Create a directory.
             * 
             * @param sDirectory The path.
             */
            public InstallDirectory(String sDirectory)
            {
               super(sDirectory);
            }

            /**
             * @see nexj.core.build.WiXWriter.ArchiveGroup.FileTree.InstallItem#write()
             */
            public void write() throws IOException
            {
               log("@directory \"" + getPath() + "\"", DEBUG_LEVEL);
               traverseTo(getPath());
            }
         }

         /**
          * A file to be installed. The path to install to as well as the path
          * to the local file being included in the install are kept track of.
          */
         private class InstallFile extends InstallItem
         {
            /**
             * The length of the prefix (if it was added).
             */
            private int m_nPrefixLength;

            /**
             * The name of the base directory where the local file is found.
             */
            private String m_sBaseDirectory;

            /**
             * Alternatively, a full path to source file is stored.
             */
            private String m_sFullSourcePath;

            /**
             * Create a regular install file .
             * 
             * @param sInstallPath The install path as DirectoryScanner
             *           determined.
             * @param sPrefix The path prefix either "" or ending in a file
             *           separator.
             * @param sBaseDirectory The base directory where the source file
             *           was included from.
             */
            public InstallFile(String sInstallPath, String sPrefix, String sBaseDirectory)
            {
               // Set the install path (add the prefix)
               super(sPrefix + sInstallPath);

               // Store the prefix
               m_nPrefixLength = sPrefix.length();

               // Store the base directory
               m_sBaseDirectory = sBaseDirectory;

               // This isn't a full path file
               m_sFullSourcePath = null;
            }

            /**
             * Create a full path install file.
             * 
             * @param sInstallPath The install path.
             * @param sSourcePath The full path to the source file.
             */
            public InstallFile(String sInstallPath, String sSourcePath)
            {
               // Set the install path, fix the formatting
               super(new File(sInstallPath).getPath());

               // No prefix or base directory
               m_nPrefixLength = 0;
               m_sBaseDirectory = null;

               // This is a full path file, store the source path
               m_sFullSourcePath = sSourcePath;
            }

            /**
             * @see nexj.core.build.WiXWriter.ArchiveGroup.FileTree.InstallItem#write()
             */
            public void write() throws IOException
            {
               String sPath = getPath();
               File file = new File(sPath);

               // Get its parent directory
               String sParentDir = file.getParent();

               if (sParentDir == null)
               {
                  sParentDir = "";
               }
               else
               {
                  // Postfix with a separator so that director comparisons work
                  // consistently
                  sParentDir = sParentDir + File.separator;
               }

               // Go to the correct directory
               traverseTo(sParentDir);

               // Open the component
               if (!m_bComponentOpened)
               {
                  openComponentElement(sPath);
                  m_bComponentOpened = true;
                  m_bEmptyDir = false;
               }

               String sName = file.getName();
               String sShortName = getShortName(sName);
               String sId = m_identifierFactory.getUniqueId(sPath);
               String sSourcePath = getSourcePath();

               // Write the file element
               m_writer.openElement("File");
               m_writer.writeAttribute("Id", sId);
               m_writer.writeAttribute("Name", sShortName);

               if (!sShortName.equals(sName))
               {
                  m_writer.writeAttribute("LongName", sName);
               }

               m_writer.writeAttribute("Source", sSourcePath);
               m_writer.writeAttribute("Vital", "yes");
               m_writer.closeEmptyElement();

               log("....<file name=\"" + sName + "\"/>", DEBUG_LEVEL);
            }

            /**
             * Get the path to the source file.
             * 
             * @return The source path.
             */
            private String getSourcePath()
            {
               // Check if full path was used
               if (m_sFullSourcePath != null)
               {
                  return m_sFullSourcePath;
               }

               // Start from the install path
               String sSourcePath = getPath();

               // Remove the prefix
               if (m_nPrefixLength > 0)
               {
                  sSourcePath = sSourcePath.substring(m_nPrefixLength);
               }

               // Root at the base directory
               return new File(m_sBaseDirectory, sSourcePath).getAbsolutePath();
            }
         }
      }
   }

   /**
    * Resource class that contains Wix binary and icon elements. NOTE: The
    * subclasses are called Wixbinary and Wixicon which correspond to their
    * respective custom ANT tasks. It makes more sense to call it "Wixbinary"
    * instead of "Binary", to avoid confusion with nexj.core.util.Binary which
    * is imported. "Wixicon" is similarly named for consistency. (Supports Ant
    * references.)
    */
   public static class Resource extends DataType
   {
      /**
       * Constructor.
       */
      public Resource()
      {
      }

      /**
       * List of all Binaries in the Resource.
       */
      private ArrayList m_wixBinaryList = new ArrayList();

      /**
       * List of all Icons in the Resource.
       */
      private ArrayList m_wixIconList = new ArrayList();

      /**
       * Creates a new WixBinary.
       * 
       * @return The new WixBinary.
       */
      public GenericResource createBinary()
      {
         GenericResource binary = new GenericResource();
         m_wixBinaryList.add(binary);
         return binary;
      }

      /**
       * Creates a new WixIcon.
       * 
       * @return The new WixIcon.
       */
      public GenericResource createIcon()
      {
         GenericResource icon = new GenericResource();
         m_wixIconList.add(icon);
         return icon;
      }

      /**
       * Write out the references.
       * 
       * @param writer The writer for the Wix fragment.
       * @throws IOException if an I/O error occurs.
       */
      public void write(XMLWriter writer) throws IOException
      {
         if (isReference())
         {
            ((Resource)getCheckedRef(Resource.class, "resource")).write(writer);
         }
         else
         {
            // Write binaries
            for (Iterator binaryIt = m_wixBinaryList.iterator(); binaryIt.hasNext();)
            {
               GenericResource binary = (GenericResource)binaryIt.next();

               // Check the atributes
               binary.checkAttributes("binary");

               // Write the element
               writer.openElement("Binary");
               writer.writeAttribute("Id", binary.getResId());
               writer.writeAttribute("SourceFile", binary.getFile());
               writer.closeEmptyElement();
               log("<binary id=\"" + binary.getResId() + "\"/>", DEBUG_LEVEL);
            }

            // Write icons
            for (Iterator iconIt = m_wixIconList.iterator(); iconIt.hasNext();)
            {
               GenericResource icon = (GenericResource)iconIt.next();

               // Check the atributes
               icon.checkAttributes("icon");

               // Write the element
               writer.openElement("Icon");
               writer.writeAttribute("Id", icon.getResId());
               writer.writeAttribute("SourceFile", icon.getFile());
               writer.closeEmptyElement();
               log("<icon id=\"" + icon.getResId() + "\"/>", DEBUG_LEVEL);
            }
         }
      }

      /**
       * Superclass which contains code common to a ResourceBase (WixBinary or
       * WixIcon, which are custom ANT subtasks of Resource).
       */
      public class GenericResource
      {
         /**
          * The ID used in Wix.
          */
         private String m_sResourceId;

         /**
          * The local source file.
          */
         private String m_sFile;

         /**
          * Constructor.
          */
         public GenericResource()
         {
         }

         /**
          * Set the ID of the resource (used by Ant).
          * 
          * @param sId The ID.
          */
         public void setResId(String sId)
         {
            m_sResourceId = sId;
         }

         /**
          * Set the source file (used by Ant).
          * 
          * @param file The source file.
          * @throws BuildException if the source file doesn't exist.
          */
         public void setFile(File file) throws BuildException
         {
            try
            {
               m_sFile = file.getCanonicalPath();
            }
            catch (IOException e)
            {
               throw new BuildException("File \"" + file.getAbsolutePath() + "\" does not exist");
            }
         }

         /**
          * Get the ID of the resource.
          * 
          * @return The ID.
          */
         public String getResId()
         {
            return m_sResourceId;
         }

         /**
          * Get the path to the source file.
          * 
          * @return The path.
          */
         public String getFile()
         {
            return m_sFile;
         }

         /**
          * Check that attributes have been set.
          * 
          * @param elementName The Ant name of the element.
          */
         public void checkAttributes(String elementName)
         {
            if (m_sResourceId == null)
            {
               throw new BuildException("Undefined 'resid' attribute of " + elementName);
            }
            if (m_sFile == null)
            {
               throw new BuildException("Undefined 'file' attribute of " + elementName);
            }
         }
      }
   }

   /**
    * Definition of a WiX Candle preprocessor variable.
    */
   public class Variable
   {
      /**
       * Variable name.
       */
      private String m_sName;

      /**
       * Variable value.
       */
      private String m_sValue;

      /**
       * Constructor.
       */
      public Variable()
      {
      }

      /**
       * Set the name.
       * 
       * @param sName The name.
       */
      public void setName(String sName)
      {
         m_sName = sName;
      }

      /**
       * Set the value.
       * 
       * @param sValue The value.
       */
      public void setValue(String sValue)
      {
         m_sValue = sValue;
      }

      /**
       * Set the value to a location.
       * 
       * @param location The location.
       */
      public void setLocation(File location)
      {
         m_sValue = location.getPath();
      }

      /**
       * Get the define parameter used by Candle.
       * 
       * @return The define parameter.
       */
      public String getDefineParameter()
      {
         return "-d" + m_sName + "=\"" + m_sValue + "\"";
      }
   }

   /**
    * Ant type for building a bootstrapper that includes Microsoft Office Primary Interop Assemblies.
    */
   public class InteropBootstrapper
   {
      /**
       * The name of the .nsi file.
       */
      private String m_sNsiFileName;

      /**
       * The name of the setup.
       */
      private String m_sSetupName;

      /**
       * The icon file.
       */
      private File m_iconFile;

      /**
       * The directory where the interops are found.
       */
      private File m_interopDirectory;

      /**
       * The install directory of NSIS.
       */
      private File m_nsisDirectory;

      /**
       * The version of EXE installer.
       */
      private String m_sInstallVersion;

      /**
       * The version of installed product.
       */
      private String m_sProductVersion;

      /**
       * Constructor.
       */
      public InteropBootstrapper()
      {
      }

      /**
       * Set the NSI file's name.
       * 
       * @param sNsiFileName The NSI file's name.
       */
      public void setNsiFile(String sNsiFileName)
      {
         m_sNsiFileName = sNsiFileName;
      }

      /**
       * Set the setup name.
       * 
       * @param sSetupName The setup name.
       */
      public void setSetupName(String sSetupName)
      {
         m_sSetupName = sSetupName;
      }

      /**
       * Set the icon file.
       * 
       * @param iconFile The icon file.
       */
      public void setIcon(File iconFile)
      {
         m_iconFile = iconFile;
      }

      /**
       * Set the interop directory.
       * 
       * @param interopDirectory The interop directory.
       */
      public void setInteropDir(File interopDirectory)
      {
         m_interopDirectory = interopDirectory;
      }

      /**
       * Set the NSIS directory.
       * @param nsisDirectory The NSIS directory.
       */
      public void setNsisDir(File nsisDirectory)
      {
         m_nsisDirectory = nsisDirectory;
      }

      /**
       * Set the version of the installer.
       * @param sInstallVersion The version of the Installer.
       */
      public void setInstallVersion(String sInstallVersion)
      {
         m_sInstallVersion = sInstallVersion;
      }

      /**
       * Set the version of the product.
       * @param sInstallerVersion The version of the Product.
       */
      public void setProductVersion(String sProductVersion)
      {
         m_sProductVersion = sProductVersion;
      }

      /**
       * Run MakeNSIS to make the interop bookstrapper.
       * 
       * @param sourceDirectory The source directory where the NSI file is.
       * @param destinationDirectory The destination directory.
       * @param sSetupFileBaseName The base name of the setup file.
       */
      public void makeBootstrapper(File sourceDirectory, File destinationDirectory, String sSetupFileBaseName)
      {
         int nExitValue;

         try
         {
            ArrayList argList = new ArrayList();

            // Set the exe to run
            argList.add(new File(m_nsisDirectory, "makensis.exe").getAbsolutePath());

            // Set up NSIS defines
            argList.add("/DSETUPNAME=" + m_sSetupName);
            argList.add("/DICON=" + m_iconFile.getAbsolutePath());
            argList.add("/DDESTDIR=" + m_destinationDirectory.getAbsolutePath());
            argList.add("/DNEXJEXE=" + sSetupFileBaseName + ".exe");
            argList.add("/DNEXJMSI=" + sSetupFileBaseName + ".msi");
            argList.add("/DINTEROPDIR=" + m_interopDirectory.getAbsolutePath());
            argList.add("/DINSTALLVERSION=" + m_sInstallVersion);
            argList.add("/DPRODUCTVERSION=" + m_sProductVersion);

            // Don't print the script
            argList.add("/V2");

            // Set the NSI input file
            argList.add(new File(sourceDirectory, m_sNsiFileName).getPath());

            // Run MakeNSIS, create the bootstrapper
            Execute makensis = new Execute();
            makensis.setAntRun(getProject());
            makensis.setCommandline((String[])argList.toArray(new String[0]));
            makensis.execute();

            // Check if successful
            nExitValue = makensis.getExitValue();
         }
         catch (Exception e)
         {
            e.printStackTrace();
            throw new BuildException("Fatal exception occurred while running bootstrap compiler MakeNSIS", e);
         }

         if (nExitValue != 0)
         {
            log("MakeNSIS failed (exit value " + nExitValue + ")", ERROR_LEVEL);
            throw new BuildException("Bootstrap compiler MakeNSIS failed");
         }
      }
   }

   /**
    * Creates IDs for files, directories, components.
    */
   private static class IdentifierFactory
   {
      /**
       * Pattern for matching bad Wix ID characters.
       */
      private final static Pattern BAD_ID_CHAR_PATTERN = Pattern.compile("[^A-Za-z0-9._]");

      /**
       * Namespace to use for hashing (incorporates the prefix).
       */
      private byte[] m_guidNamespace = GUID_NAMESPACE;

      /**
       * Preallocated output buffer, max ID length is 72 characters.
       */
      private StringBuffer m_idBuffer = new StringBuffer(72);

      /**
       * Constructor.
       */
      public IdentifierFactory()
      {
      }

      /**
       * Set the prefix directory.
       * 
       * @param sPrefixDir The prefix directory.
       */
      public void setPrefixDir(String sPrefixDir)
      {
         m_guidNamespace = (sPrefixDir == null) ? GUID_NAMESPACE : getEncodedData(SysUtil.NAMESPACE + ":installer:" + sPrefixDir + File.separator);
      }

      /**
       * Get a unique ID based on a path. Constructed by appending a hash of the
       * install path to the file or folder's name. All ID's must contain only
       * alphanumeric characters, periods, or underscores and must either start
       * with a letter or an underscore.
       * 
       * @param sPath The path which the ID is keyed by.
       * @return The unique ID.
       */
      public String getUniqueId(String sPath)
      {
         // Clear the buffer
         m_idBuffer.delete(0, m_idBuffer.length());

         // Prefix the name and the hash with an underscore for consistency
         m_idBuffer.append('_');

         // Get the name
         CharSequence truncatedName = new File(sPath).getName();

         // Make sure it's not too long
         if (truncatedName.length() > 34)
         {
            truncatedName = CharBuffer.wrap(truncatedName, 0, 34);
         }

         // Append it to the ID, replacing bad characters in the process
         Matcher badCharMatcher = BAD_ID_CHAR_PATTERN.matcher(truncatedName);

         while (badCharMatcher.find())
         {
            badCharMatcher.appendReplacement(m_idBuffer, "_");
         }

         badCharMatcher.appendTail(m_idBuffer);

         // Separate the name and the hash
         m_idBuffer.append('_');

         // Append the hash (in hex) of the full install path
         appendHash(m_idBuffer, sPath, '_');

         return m_idBuffer.toString();
      }

      /**
       * Get a GUID based on the hash of a path.
       * 
       * @param sPath The path by which the GUID is keyed.
       * @return The GUID.
       */
      public String getGUID(String sPath)
      {
         // Get the hash (in hex) of the full install path
         // A GUID is just the hash with dashes inside it
         m_idBuffer.delete(0, m_idBuffer.length());
         appendHash(m_idBuffer, sPath, '-');
         return m_idBuffer.toString();
      }

      /**
       * Calculate a hash of the data prefixed with context data and append it
       * to the buffer with separators inserted.
       * 
       * @param buffer The buffer.
       * @param sData The data to hash.
       * @param chSeparator The character to insert as separators.
       */
      private void appendHash(StringBuffer buffer, String sData, char chSeparator)
      {
         // Convert the prefix directory and the current data to binary data
         byte[] nDataArray = getEncodedData(sData);

         // Calculate the hash and convert it to a hexidecimal string
         String sHash = GUIDUtil.generateGUID(m_guidNamespace, nDataArray).toString();

         // Separate the hash string
         buffer.append(sHash, 0, 8).append(chSeparator).append(sHash, 8, 12).append(chSeparator).append(sHash, 12, 16).append(
            chSeparator).append(sHash, 16, 20).append(chSeparator).append(sHash, 20, 32);
      }
   }

}
