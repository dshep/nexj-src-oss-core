// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.build;

import java.lang.Class; 
import java.lang.reflect.Field;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.MagicNames;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.PropertyHelper;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.taskdefs.Ant;
import org.apache.tools.ant.taskdefs.Property;
import org.apache.tools.ant.taskdefs.Sequential;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import nexj.core.util.IOUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.XMLUtil;

/**
 * Ant task to enable iterations of a target or nested tasks described 
 * by an xml file, properties file or list attribute.
 * Also adds the ability to specify additional properties as parameters 
 * to those iterations based on the contents of the referenced files.
 */
public class ForEach extends Task
{
   //constants

   /**
    * Pattern for non commented "key[= ]value" format property entry.
    */
   private final static Pattern PROPERTY_LINE = Pattern.compile("^([^!#\\s=]+)(?:[\\s|=]([^!#\\s]+))?(?:\\s*[!#].*)?");

   //attributes

   /**
    * See Ant#inheritAll
    */
   private boolean m_bInheritAll = true;

   /**
    * See Ant#inheritRefs
    */
   private boolean m_bInheritRefs = false;

   /**
    * If false, if an iteration throws an Exception ForEach will catch and report it.
    */
   private boolean m_bFailOnError = true;

   /**
    * If true, don't ignore empty strings in CSV lists
    */
   private boolean m_bUseEmptyValue = false;

   /**
    * String to contain the xml/properties file that contains
    * values to be parsed.
    */
   private String m_sFileName;

   /**
    * String to contain the Ant target to be called for each iteration.
    */
   private String m_sTarget;

   /**
    * String to contain the Ant property to set for each iteration.
    */
   private String m_sProperty;

   /**
    * String to contain the XPath to retrieve the value from a
    * referenced XML file.
    */
   private String m_sXPath;

   /**
    * String to contain the CSV list of values to set m_sProperty as on
    * each iteration.
    */
   private String m_sList;

   /**
    * String to contain the Regular Expression to parse the properties
    * file value with.
    */
   private String m_sPattern;

   /**
    * A Pattern array to match against a Project's properties to find
    * properties to persist through multiple iterations.
    */
   private Pattern[] m_keepPropertyArray;

   /**
    * Sequential element to contain the tasks to be executed during each
    * iteration made.
    */
   private Sequential m_nestedTasks;

   /**
    * Catch element to contain the tasks to be executed when an iteration
    * throws an exception
    */
   private Sequential m_catchTasks;

   /**
    * Finally element to contain the tasks to be executed after an iteration,
    * exception or no exception.
    */
   private Sequential m_finalTasks;

   //associations
   
   /**
    * An array list of parameters passed as nested elements.
    */
   private List m_paramList = new ArrayList();

   /**
    * An array list of iterations to be run in CallTarget style.
    */
   private List m_iterationList = new ArrayList();

   //operations

   /**
    * See CallTarget#setInheritAll
    *
    * @param bInheritAll set true to inherit all properties.
    */
   public void setInheritAll(boolean bInheritAll)
   {
      m_bInheritAll = bInheritAll;
   }

   /**
    * See CallTarget#setInheritAll
    *
    * @param bInheritRefs set true to inherit all references.
    */
   public void setInheritRefs(boolean bInheritRefs)
   {
       m_bInheritRefs = bInheritRefs;
   }

   /**
    * Ant attribute setter for file that contains list of projects.
    *
    * @param sFileName The path and name of the file to parse for projects.
    */
   public void setFile(String sFileName)
   {
      m_sFileName = sFileName;
   }

   /**
    * Ant attribute setter for Ant target to be called.
    *
    * @param sTarget The target to be called for each project.
    */
   public void setTarget(String sTarget)
   {
      m_sTarget = sTarget;
   }

   /**
    * Ant attribute setter for property to store key name of a property file or value from a list.
    *
    * @param sProperty The property to set for the value in each call to the target.
    */
   public void setProperty(String sProperty)
   {
      m_sProperty = sProperty;
   }

   /**
    * Ant attribute setter for property to store value name.
    *
    * @param sProperty The property to set for the value in each call to the target.
    */
   public void setXPath(String sXPath)
   {
      m_sXPath = sXPath;
   }

   /**
    * Ant attribute setter for inline list iterations.
    *
    * @param sList The property to set for the value in each call to the target.
    */
   public void setList(String sList)
   {
      m_sList = sList;
   }

   /**
    * Ant attribute setter for pattern to extract the properties in a properties file
    *
    * @param sPattern The property to set for the value in each call to the target.
    */
   public void setPattern(String sPattern)
   {
      m_sPattern = sPattern;
   }

   /**
    * Ant attribute setter for the comma separated list of patterns of properties
    * that are to persist through iterations in this ForEach task.
    * 
    * @param sPattern A comma separated list of Patterns to match against iteration properties.
    */
   public void setKeep(String sPatternList)
   {
      String[] sTokenArray = sPatternList.split(",");
      int nIndex;

      if (sTokenArray.length > 0)
      {
         m_keepPropertyArray = new Pattern[sTokenArray.length];
   
         for (nIndex = 0; nIndex < sTokenArray.length; nIndex++)
         {
            m_keepPropertyArray[nIndex] = Pattern.compile(sTokenArray[nIndex]);
         }
      }
   }

   /**
    * Ant attribute setter for catching iteration exceptions.
    * 
    * @param bCatchException Whether to catch and report exceptions in an iteration.
    */
   public void setFailOnError(boolean bFailOnError)
   {
      m_bFailOnError = bFailOnError;
   }

   /**
    * Ant attribute setter for using empty strings in CSV lists.
    * 
    * @param bUseEmptyValue Whether to use empty strings in CSV lists.
    */
   public void setWithEmpty(boolean bWithEmpty)
   {
      m_bUseEmptyValue = bWithEmpty;
   }

   /**
    * Creation of nested Parameter elements to be evaluated for each iteration.
    *
    * @return The Parameter object to setup for evaluation during a given iteration.
    */
   public Parameter createParameter()
   {
      Parameter param = new Parameter();

      m_paramList.add(param);

      return param;
   }

   /**
    * Creation of the nested Sequential element which contains the Ant Tasks to be
    * executed for each iteration.
    * 
    * @return The Parameter object to populated for each iteration.
    */
   public Sequential createSequential()
   {
      if (m_nestedTasks != null)
      {
         throw new BuildException("Cannot have two nested Sequential elements.");
      }

      m_nestedTasks = new Sequential();

      return m_nestedTasks;
   }

   /**
    * Returns a nested Sequential element for the Catch part of the
    * ForEach task.
    *
    * @return The Sequential element for the user to add Catch Tasks to.
    */
   public Sequential createCatch()
   {
      if (m_catchTasks != null)
      {
         throw new BuildException("Cannot have two nested Catch elements");
      }

      m_catchTasks = new Sequential();

      return m_catchTasks;
   }

   /**
    * Returns a nested Sequential element for the Finally part of the
    * ForEach task.
    *
    * @return The Sequential element for the user to add Finally Tasks to.
    */
   public Sequential createFinally()
   {
      if (m_finalTasks != null)
      {
         throw new BuildException("Cannot have two nested Finally elements");
      }

      m_finalTasks = new Sequential();

      return m_finalTasks;
   }

   /**
    * Check if all the mandatory Ant Task attribute combinations are set.
    */
   private void checkAttributes()
   {
      if (m_sTarget == null && m_nestedTasks == null)
      {
         throw new BuildException("Undefined target or nested Sequential element to execute for "  + getTaskName());
      }

      if (m_sTarget != null && m_nestedTasks != null)
      {
         throw new BuildException("Cannot define both attribute target and nested Sequential element for "  + getTaskName());
      }

      // check incompatible attributes: different methods of iteration
      if (m_sPattern != null && m_sXPath != null)
      {
         throw new BuildException("Cannot define both attributes \"pattern\" and \"xpath\".");
      }

      if (m_sList != null && m_sXPath != null)
      {
         throw new BuildException("Cannot define both attributes \"list\" and \"xpath\".");
      }

      if (m_sList != null && m_sPattern != null)
      {
         throw new BuildException("Cannot define both attributes \"list\" and \"pattern\".");
      }

      log("Persist property patterns: " + Arrays.toString(m_keepPropertyArray), Project.MSG_DEBUG);

      // Required attributes by case

      // if using properties file or inline list (i.e. NOT xpath)
      // we need to have property set
      if (m_sXPath == null && m_sProperty == null)
      {
         throw new BuildException("You must define a property attribute when not using the \"xpath\" attribute.");
      }

      // if using properties file or xml file (i.e. NOT inline lists)
      // we need to have filename set
      if (m_sList == null && m_sFileName == null)
      {
         throw new BuildException("You must define a filename attribute when not using the \"list\" attribute.");
      }

      // if not the case we're parsing xpath, properties file or inline list
      if (!((m_sXPath != null && m_sFileName != null)
         || (m_sList != null && m_sProperty != null)
         || (m_sProperty != null && m_sFileName != null)))
      {
         throw new BuildException("Insufficient parameters.");
      }

      // validate parameters
      for (int nIndex = 0; nIndex < m_paramList.size(); nIndex++)
      {
         Parameter param = (Parameter)m_paramList.get(nIndex);

         if (param.getGroup() != Parameter.GROUP_DEFAULT && m_sPattern == null)
         {
            throw new BuildException("You must specify a \"pattern\" attribute if using Parameter \"group\" attributes.");
         }

         if (param.getName() == null)
         {
            throw new BuildException("You must specify a \"name\" for Parameter parameters.");
         }

         // ensure a value is expected
         if (param.getXPath() == null && param.getValue() == null && param.getGroup() == Parameter.GROUP_DEFAULT)
         {
            throw new BuildException("You must specify a \"value\", \"group\" or \"xpath\" for nested Parameter elements.");
         }
      }
   }

   /**
    * Parse a inline list attribute to determine the iterations to be run.
    */
   private void parseList()
   {
      String[] sTokenArray = m_sList.split(",");

      for (int nParamIndex = 0; nParamIndex < m_paramList.size(); nParamIndex++)
      {
         // check the parameter
         Parameter userParam = (Parameter)((Parameter)m_paramList.get(nParamIndex)).clone();

         if (userParam.getValue() == null
            || userParam.getXPath() != null
            || userParam.getGroup() != Parameter.GROUP_DEFAULT)
         {
            throw new BuildException("Nested Parameter elements used with \"list\" " +
                    "attribute must specify a explicit value, and cannot contain " +
                    "\"xpath\" or \"group\" attributes.");
         }
      }

      for (int nIndex = 0; nIndex < sTokenArray.length; nIndex++)
      {
         String sPropertyValue = sTokenArray[nIndex];

         if (!StringUtil.isEmpty(sPropertyValue) || m_bUseEmptyValue)
         {
            Iteration iter = new Iteration();
            Parameter param = new Parameter();

            param.setName(m_sProperty);
            param.setValue(sPropertyValue);
            iter.addParam(param);
            iter.m_parameterList.addAll(m_paramList);
            m_iterationList.add(iter);
         }
      }
   }

   /**
    * Compile an XPath string into an XPathExpression
    * 
    * @param sXPath the XPath as a String
    * @return the XPath argument as a XPathExpression
    * @throws XPathExpressionException
    */
   private static XPathExpression compile(String sXPath) throws XPathExpressionException
   {
      XPath xpath = javax.xml.xpath.XPathFactory.newInstance().newXPath(); // New XPath object
      
      return xpath.compile(sXPath);
   }
   
   /**
    * Parse an XML file using XPath to determine the iterations to be run
    * and retrieve a list of parameters to set for each.
    */
   private void parseXPath()
   {
      File file = new File(m_sFileName);

      if (!file.exists())
      {
         throw new BuildException("Missing file: " + file.getPath());
      }

      XPathExpression expression;
      NodeList nodeList;

      try
      {
         expression = compile(m_sXPath); // Compile an XPath string into an XPathExpression
         nodeList = (NodeList)expression.evaluate(new org.xml.sax.InputSource(m_sFileName), XPathConstants.NODESET); // Evaluate the XPath expression on an input document
      }
      catch (XPathExpressionException e)
      {
         throw new BuildException("Could not compile and evaluate XPath \"" + m_sXPath + "\" with document \"" + m_sFileName + "\".");
      }

      for (int nIndex = 0; nIndex < nodeList.getLength(); nIndex++)
      {
         Node context = nodeList.item(nIndex);
         Iteration iteration = new Iteration();

         for (int nParamIndex = 0; nParamIndex < m_paramList.size(); nParamIndex++)
         {
            // clone the original parameter for each iteration
            Parameter userParam = (Parameter)((Parameter)m_paramList.get(nParamIndex)).clone();
            XPathExpression xpath = userParam.getXPath();

            // check the parameters for validity
            if (xpath != null)
            {
               try
               {
                  NodeList paramResults = (NodeList)xpath.evaluate(context, XPathConstants.NODESET); // Evaluate the XPath expression on an input document

                  if (paramResults.getLength() >= 1)
                  {
                     String sNodeValue;
                     
                     userParam.setValue(sNodeValue = paramResults.item(paramResults.getLength()-1).getNodeValue());

                     if (paramResults.getLength() > 1)
                     {
                        log("Warning: Multiple results for parameter \"" + userParam.getName() + "\". Using \"" + sNodeValue + "\".");
                     }
                  }
               }
               catch (XPathExpressionException e)
               {
                  log("Could not evaluate XPath expression for \"" + userParam.getName() + "\" from context \"" + context.getBaseURI() + "\". Skipping..", Project.MSG_WARN);
               }
            }

            if (userParam.getValue() != null)
            {
               iteration.addParam(userParam);
            }
            else if (userParam.isRequired())
            {
               throw new BuildException("No value found for paramter name \"" + userParam.getName() + "\".");
            }
         }

         m_iterationList.add(iteration);
      }
   }

   /**
    * Parse an .properties formatted file using regular expressions
    * to determine the iterations to be run and retrieve a list of
    * parameters to set for each.
    */
   private void parseProperties()
   {
      File file = new File(m_sFileName);

      if (file.exists())
      {
         BufferedReader reader = null;
         InputStream istream = null;

         try
         {
            Pattern valuePattern = Pattern.compile(m_sPattern);
            String sLine;

            istream = new FileInputStream(file);
            reader = new BufferedReader(new InputStreamReader(istream, XMLUtil.ENCODING));

            for (;;)
            {
               sLine = reader.readLine();

               if (sLine == null)
               {
                  break;
               }

               if (sLine.length() > 0)
               {
                  Matcher match = PROPERTY_LINE.matcher(sLine.trim()); // Want property name and value to parse further

                  if (match.matches() || m_sPattern.equals("line"))
                  {
                     Iteration iteration = new Iteration();
                     String sKey = m_sPattern.equals("line")? sLine.trim() : match.group(1);
                     String sValue = m_sPattern.equals("line")? null : match.group(2);

                     if (sKey != null)
                     {
                        Parameter propParam = new Parameter();
                        Matcher valueMatcher = null; // to match the value part of the property

                        propParam.setName(m_sProperty);
                        propParam.setValue(sKey);
                        iteration.addParam(propParam);
                        log("Found iteration: " + m_sProperty + " = " + sKey + ".", Project.MSG_DEBUG);

                        if (sValue != null)
                        {
                           valueMatcher = valuePattern.matcher(sValue);

                           if (!valueMatcher.matches())
                           {
                              log("Warning: Could not evaluate supplied pattern \"" + m_sPattern + "\" against value: \"" + match.group(2) + "\". Skipping..", Project.MSG_WARN);
                              valueMatcher = null; // just so we only have to check a single condition from now on
                           }
                        }
                        else
                        {
                           log("Warning: no value to evaluate pattern \"" + m_sPattern + "\" against in line \"" + sLine + "\". Skipping..", Project.MSG_DEBUG);
                        }

                        int nParams = m_paramList.size();
                        int nParamIndex;

                        for (nParamIndex = 0; nParamIndex < nParams; nParamIndex++)
                        {
                           // check the parameters
                           Parameter userParam = (Parameter)((Parameter)m_paramList.get(nParamIndex)).clone();
                           int nParamGroupNum = userParam.getGroup();

                           if (valueMatcher != null) {

                              if (nParamGroupNum != Parameter.GROUP_DEFAULT)
                              {
                                 String sResult = null;

                                 if (nParamGroupNum > valueMatcher.groupCount() || (sResult = valueMatcher.group(nParamGroupNum)) == null)
                                 {
                                    throw new BuildException("No pattern group " + nParamGroupNum + " found in string \"" + sValue + "\".");
                                 }
                                 else
                                 {
                                    userParam.setValue(sResult);
                                 }
                              }
                           }

                           if (userParam.getValue() != null)
                           {
                              iteration.addParam(userParam);
                           }
                           else if (userParam.isRequired())
                           {
                              throw new BuildException("No value found for paramter name \"" + userParam.getName() + "\".");
                           }
                        } // end param checking

                        m_iterationList.add(iteration);
                     } // end if sKey != null
                     else
                     {
                        log("Warning: could not evaluate the following line against .properties format: \"" + sLine + "\". Skipping..", Project.MSG_WARN);
                     }
                  }
                  else
                  {
                     //evaluate as a line
                     log("Warning: the following line could not be parsed due to format: \"" + sLine + "\". Skipping..", Project.MSG_WARN);
                  }
               }
            }
         }
         catch (IOException e)
         {
            throw new BuildException("Unable to parse file: " + file.getPath());
         }
         finally
         {
            IOUtil.close(reader);
            IOUtil.close(istream);
         }
      }
      else
      {
         throw new BuildException("Missing file: " + file.getPath());
      }
   }

   /**
    * This static method reflectively retreieves the newProject Field
    * from a given Ant class object.
    * 
    * @param callee The target Ant object
    * @return The Project object that the given Ant object uses to execute the target call. 
    */
   private static Project extractNewProject(Ant callee)
   {
      Class antClass = callee.getClass();
      Field newProjectField = null;
      Project newProject = null;

      try
      {
         newProjectField = antClass.getDeclaredField("newProject");
         newProjectField.setAccessible(true);
         
         newProject = (Project)newProjectField.get(callee);
      }
      catch (Exception e)
      {
         throw new BuildException(e); // from Class.getDeclaredField() or Field.get()
      }

      return newProject;
   }

   /**
    * Handle a thrown exception thrown during an iteration, specifically
    * regarding exception type thrown by the iteration and what
    * failOnError is set to. 
    * 
    * @param exception The exception thrown by the iteration.
    * @throws BuildException The throwable cause, thrown as a BuildException for Ant if failOnError is true.
    */
   private void handleException(Exception exception) throws BuildException
   {
      if (m_catchTasks != null)
      {
         m_catchTasks.execute();
      }

      if (m_bFailOnError) 
      {
         if (exception instanceof BuildException)
         {
            throw (BuildException)exception;
         }
         else
         {
            throw new BuildException(exception);
         }
      }
      else
      {
         log("ITERATION FAILED", Project.MSG_ERR);
         log(exception.toString(), Project.MSG_ERR);
      }
   }

   /**
    * Handle the try-catch's finally section for an iteration.
    */
   private void handleFinally()
   {
      if (m_finalTasks != null)
      {
         m_finalTasks.execute();
      }
   }

   /**
    * Entry point for Ant Task.
    * Call a specified Ant target for a number of iterations described by
    * attributes set.
    */
   public void execute() throws BuildException
   {
      checkAttributes();

      if (m_sXPath != null)
      {
         parseXPath();
      }
      else if (m_sList != null)
      {
         parseList();
      }
      else
      {
         parseProperties();
      }

      if (m_sTarget != null) 
      {
         ProjectState persistentState = new ProjectState(m_keepPropertyArray);

         for (Iterator it = m_iterationList.iterator(); it.hasNext(); )
         {
            Iteration iteration = (Iteration)it.next();
            List parameters = iteration.getParams();
            Ant callee = new Ant(this);

            callee.init();
            callee.setAntfile(getProject().getProperty("ant.file"));
            callee.setInheritAll(m_bInheritAll);
            callee.setInheritRefs(m_bInheritRefs);
            log("Executing task \"" + m_sTarget + "\" for:", Project.MSG_DEBUG);

            // set properties to persist as Parameters
            persistentState.restore(callee);

            // set nested parameters
            for (Iterator paramIt = parameters.iterator(); paramIt.hasNext(); )
            {
               Parameter param = (Parameter)paramIt.next();
               Property calleeParam = callee.createProperty();

               if (getProject().getProperty(param.getName()) != null)
               {
                  log("  Overriding existing property \"" + param.getName()  + "\".", Project.MSG_DEBUG);
               }

               log("  \"" + param.getName()  + "\"=\"" + param.getValue() + "\".", Project.MSG_DEBUG);
               calleeParam.setName(param.getName());
               calleeParam.setValue(param.getValue());
            }

            callee.setTarget(m_sTarget);
            
            try 
            {
               // get Project handle
               Project calleeNewProject = extractNewProject(callee);

               //target execution
               try
               {                  
                  callee.execute();
               }
               catch (Exception exception)
               {
                  handleException(exception);
               }
               finally
               {
                  handleFinally();
               }

               if (m_keepPropertyArray != null)
               {
                  persistentState.updatePersistent(calleeNewProject);
               }
            }
            catch (Exception e)
            {
               // exception in the ForEach overhead, throw exception (failonerror attribute doesn't apply)
               throw new BuildException(e);
            }
         }

         // set properties to persist as Parameters
         persistentState.restore(getProject());
      }
      else
      { // Sequential
         // get original properties to restore once iterations are complete
         ProjectState persistentState = new ProjectState(m_keepPropertyArray);

         persistentState.saveAll(getProject());

         for (Iterator it = m_iterationList.iterator(); it.hasNext(); )
         {
            Iteration iteration = (Iteration)it.next();
            List parameters = iteration.getParams();

            //Restore original set of user and normal properties with persistent properties.
            persistentState.restore(getProject());

            // set nested parameters
            for (Iterator paramIt = parameters.iterator(); paramIt.hasNext(); )
            {
               Parameter param = (Parameter)paramIt.next();

               if (getProject().getProperty(param.getName()) != null)
               {
                  log("  Overriding existing property \"" + param.getName()  + "\".", Project.MSG_DEBUG);                  
               }

               log("  \"" + param.getName()  + "\"=\"" + param.getValue() + "\".", Project.MSG_DEBUG);
               getProject().setProperty(param.getName(), param.getValue());
            }

            try 
            {
               m_nestedTasks.execute();
            } 
            catch (Exception exception)
            {
               handleException(exception);
            }
            finally
            {
               handleFinally();
            }

            if (m_keepPropertyArray != null)
            {
               persistentState.updatePersistent(getProject()); //get persisting properties from this iteration
            }
         } // end iterations

         //Restore original set of user and normal properties with persistent properties.
         persistentState.restore(getProject());
      }
   }


   /**
    * Inner class to represent each iteration that this task will
    * execute.
    */
   public static class Iteration
   {
      /**
       * List to store Parameter objects used in this Iteration.
       */
      List m_parameterList; // of Nodes

      /**
       * To create the list.
       */
      public Iteration()
      {
         m_parameterList = new ArrayList();
      }

      /**
       * Adds a Parameter to this Iteration.
       *
       *  @param param A Parameter to be set for this Iteration.
       */
      public void addParam(Parameter param)
      {
         m_parameterList.add(param);
      }

      /**
       * Gets the List of Parameters to be set for this Iteration.
       *
       * @return The Parameters to be set for this Iteration.
       */
      public List getParams()
      {
         return m_parameterList;
      }
   }

   /**
    * Inner class to represent the parameter and its multiple
    * ways of evaluation for each iteration.
    */
   public static class Parameter
   {
      /**
       * A value to represent that no group attribute has been set.
       */
      public final static int GROUP_DEFAULT = -1;

      /**
       * A value to represent that the corresponding Ant Property must be set.
       */
      private boolean m_bRequired = false;

      /**
       * Name of the current Parameter object when it is to be set as an Ant Property.
       */
      private String m_sName;

      /**
       * Value to assign to the Parameter object.
       */
      private String m_sValue;

      /**
       * Default value to assign to the Parameter object if no other value can be determined.
       */
      private String m_sDefault;

      /**
       * XPath to be used to determine the Parameter's value if applicable.
       */
      private XPathExpression m_xpathExpression;

      /**
       * Regex pattern group number to be used to determine the Parameter object's value if applicable.
       */
      private int m_nPatternGroup = GROUP_DEFAULT;

      /**
       * Default constructor
       */
      public Parameter()
      {
      }

      /**
       * Constructor allows the initialization of the name and value inline.
       * 
       * @param sName The name of this Parameter.
       * @param sValue The value to be assigned to this Parameter.
       */
      public Parameter(String sName, String sValue)
      {
         m_sName = sName;
         m_sValue = sValue;
      }

      /**
       * Sets whether this property must have a value when used in the associated
       * target call or nested task execution. If the property has no Default or 
       * Value attribute and the xpath or regular expression could not be resolved
       * to a value, an error will occur if this is set to true.
       * 
       * Defaults to false, which means if the target or nested tasks are executed and 
       * this parameter could not be given a value, no error will be thrown.
       *
       * @param bRequired Whether or not this parameter must be set when the target or nested tasks is called.
       */
      public void setRequired(boolean bRequired)
      {
         m_bRequired = bRequired;
      }

      /**
       * Returns whether a parameter is required to be set when the iterations 
       * are run.
       * 
       * @return Whether this parameter is required to be set in the iteration.
       */
      public boolean isRequired()
      {
         return m_bRequired;
      }

      /**
       * Set the name of the Parameter.
       *
       * @param sName the name to set
       */
      public void setName(String sName)
      {
         m_sName = sName;
      }

      /**
       * Return the same of this Parameter.
       *
       * @return the name of the Parameter when set as an Ant Property, or null if none was assigned.
       */
      public String getName()
      {
         return m_sName;
      }

      /**
       * The value to set this Parameter to.
       *
       * @param sValue the value to set the Parameter to.
       */
      public void setValue(String sValue)
      {
         m_sValue = sValue;
      }

      /**
       * Returns the value of this Parameter, whether the value is explicitly assigned, evaluated through XPath or regular expression, or set to the default value.
       *
       * @return the value of the Parameter, or null if none can evaluated.
       */
      public String getValue()
      {
         return (m_sValue == null) ? m_sDefault : m_sValue;
      }

      /**
       * Sets the default value for the Parameter.
       *
       * @param sDefault the default to set this Parameter to.
       */
      public void setDefault(String sDefault)
      {
         m_sDefault = sDefault;
      }

      /**
       * Returns the default value of this Parameter, whatever the calculated value.
       *
       * @return the default value of the Parameter, or null if none is assigned.
       */
      public String getDefault()
      {
         return m_sDefault;
      }

      /**
       * Sets the XPath the Parameter value will be evaluated to.
       *
       * @param xPath the XPath to set.
       */
      public void setXPath(String sXPath)
      {
         try
         {
            m_xpathExpression = compile(sXPath); // Compile an XPath string into an XPathExpression
         }
         catch (XPathExpressionException e)
         {
            throw new BuildException("Invalid XPath expression \"" + sXPath + "\".");
         }
      }

      /**
       * Returns the XPath value of the Parameter.
       *
       * @return the XPath of the Parameter, or null if none was assigned.
       */
      public XPathExpression getXPath()
      {
         return m_xpathExpression;
      }

      /**
       * Set the pattern group number this Parameter will be evaluated to.
       * After the parent element's pattern attribute is evaluated against the property file line, the user can use one of the pattern's groupings to assign a Parameter a value.
       *
       * @param patternGroup the group number to set this Parameter to.
       */
      public void setGroup(String sPatternGroup)
      {
         try
         {
            m_nPatternGroup = Integer.parseInt(sPatternGroup);
         }
         catch (NumberFormatException nfe)
         {
            throw new BuildException("Invalid number format: " + sPatternGroup);
         }

         if (m_nPatternGroup < 0)
         {
            throw new BuildException("Invalid group value: " + sPatternGroup);
         }
      }

      /**
       * Return the pattern group number this Parameter will be evaluated to.
       *
       * @return the pattern group number of this Parameter, or Parameter.GROUP_DEFAULT if none was assigned.
       */
      public int getGroup()
      {
         return m_nPatternGroup;
      }

      /**
       * Allow this class to be cloned, duplicating all the relevant fields.
       *
       * @return A copy of this Parameter object including it's field values.
       */
      public Object clone()
      {
         Parameter param = new Parameter();

         param.setName(m_sName);
         param.setValue(m_sValue);
         param.setDefault(m_sDefault);
         param.setRequired(m_bRequired);
         param.m_xpathExpression = m_xpathExpression;
         param.m_nPatternGroup = this.m_nPatternGroup;
         return param;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return m_sName;
      }
   }

   /**
    * Inner class to represent the original properties of a Project and
    * store properties that may be needed to persist from iteration to 
    * iteration. 
    */
   public class ProjectState
   {
      /**
       * A Hashtable representing original Properties of a Project object
       */
      private Hashtable m_properties;

      /**
       * A Hashtable representing original User Properties of a Project object 
       */
      private Hashtable m_userProperties;

      /**
       * An array of Patterns used to match persistent properties
       */
      private Pattern[] m_keepPropertyPatternsArray;

      /**
       * A Hashtable to store persistent properties
       */
      private Hashtable m_persistentProperties;

      /**
       * ProjectState constructor. Can take an array of Patterns
       * which each specify a regular expression that can be used
       * to determine if a property fall into a category of
       * "persistent".
       * 
       * @param keepPropertyPatternsArray
       */
      public ProjectState(Pattern[] keepPropertyPatternsArray)
      {
         m_keepPropertyPatternsArray = keepPropertyPatternsArray;
      }

      /**
       * Saves the user and non-user Properties of the given Project object
       * in this ProjectState.
       * 
       * @param targetProject The target Project object to save properties from.
       */
      public void saveAll(Project targetProject)
      {
         m_properties = targetProject.getProperties(); //getProperties() creates a new HashTable
         m_userProperties = targetProject.getUserProperties(); //getUserProperties() creates a new HashTable
      }

      /**
       * Restores the persistent properties of this ProjectState to a given
       * Project object, as well as the original properties saved using saveAll()
       * method.
       * 
       * @param targetProject The Project target to set the relevant 
       * properties in.
       */
      public void restore(Project targetProject)
      {
         // Restore the original user and non-user properties to this Project object
         try
         {
            PropertyHelper helper = (PropertyHelper) targetProject.getReference(MagicNames.REFID_PROPERTY_HELPER);
            Class propertyHelperClass = helper.getClass();

            //check if Properties Maps are null
            if (m_properties != null)
            {
               Field propertiesField = propertyHelperClass.getDeclaredField("properties");

               propertiesField.setAccessible(true);
               propertiesField.set(helper, new Hashtable(m_properties));
            }

            if (m_userProperties != null)
            {
               Field userPropertiesField = propertyHelperClass.getDeclaredField("userProperties");

               userPropertiesField.setAccessible(true);
               userPropertiesField.set(helper, new Hashtable(m_userProperties));
            }
         }
         catch (Exception e)
         {
            throw new BuildException(e); // from Class.getDeclaredField() or Field.set()
         }

         // Add persistent properties to this Project object
         if (m_persistentProperties != null)
         {
            Iterator persistentPropertyIterator = 
               m_persistentProperties.entrySet().iterator();

            while (persistentPropertyIterator.hasNext())
            {
               Map.Entry propertyEntry = (Map.Entry)persistentPropertyIterator.next();
               String sName = (String)propertyEntry.getKey();
               String sValue = (String)propertyEntry.getValue();
   
               log("  Set persistent property: \"" + sName  + "\"=\"" + sValue + "\".", Project.MSG_DEBUG);
               targetProject.setProperty(sName, sValue);            
            }
         }
      }

      /**
       * Restores the persistent properties of this ProjectState to a given
       * Ant object. Original properties saved using saveAll() are not restored
       * as Ant objects are not re-used. 
       * 
       * @param callee The Ant target environment to set persistent properties in.
       */
      public void restore(Ant callee)
      {
         // doesn't restore Properties Maps, Ant objects aren't reused.

         // Add persistent properties to this Ant object
         if (m_persistentProperties != null)
         {
            Iterator persistentPropertyIterator = 
               m_persistentProperties.entrySet().iterator();

            while (persistentPropertyIterator.hasNext())
            {
               Map.Entry propertyEntry = (Map.Entry)persistentPropertyIterator.next();
               Property calleeParam = callee.createProperty();
               String sName = (String)propertyEntry.getKey();
               String sValue = (String)propertyEntry.getValue();

               log("  Set persistent property: \"" + sName  + "\"=\"" + sValue + "\".", Project.MSG_DEBUG);
               calleeParam.setName(sName);
               calleeParam.setValue(sValue);     
            }
         }
      }

      /**
       * Based on the targetProject's current properties and this ProjectState
       * object's collection of Pattern's, updates the properties that will
       * persist across iterations.
       * 
       * @param targetProject The Project to collect persistent properties from.
       */
      public void updatePersistent(Project targetProject)
      {
         assert(m_keepPropertyPatternsArray != null);

         Map properties = targetProject.getProperties();
         Iterator propertyEntyIterator = properties.entrySet().iterator(); //current project properties
         int nIndex;

         m_persistentProperties = new Hashtable();

         while (propertyEntyIterator.hasNext())
         {
            Map.Entry propertyEntry = (Map.Entry)propertyEntyIterator.next();

            //iterate persistent property Patterns to match regex
            for (nIndex = 0; nIndex < m_keepPropertyPatternsArray.length; nIndex++)
            {
               String sKey = (String)propertyEntry.getKey();
               Matcher match = m_keepPropertyPatternsArray[nIndex].matcher(sKey);

               if (match.matches())
               {
                  m_persistentProperties.put(sKey, (String)propertyEntry.getValue());
               }
            }
         }

         log("Found " + m_persistentProperties.size() + " parameters to persist: " + m_persistentProperties.toString(), Project.MSG_DEBUG);
      }

   }
}
