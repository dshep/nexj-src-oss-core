// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.admin;

import java.util.Properties;

import nexj.core.meta.Metadata;

/**
 * Interface implemented by platform-specific installers.
 */
public interface Installer
{
   /**
    * Sets the root metadata object.
    * @param metadata The root metadata object.
    */
   void setMetadata(Metadata metadata);

   /**
    * Sets the installer properties.
    * @param properties The properties to set.
    */
   void setProperties(Properties properties);

   /**
    * Sets the server or cluster platform-specific locator string,
    * identifying the installation target.
    * @param sLocation The installation target location.
    * @param sUser The administrator user name.
    * @param sPassword The administrator password.
    */
   void setLocation(String sLocation, String sUser, String sPassword) throws Exception;
   
   /**
    * Deploys an application archive and starts the application.
    * @param sEARName The application archive file name.
    */
   void deploy(String sEARName) throws Exception;

   /**
    * Undeploys an application archive.
    * @param sEARName The application archive file name.
    */
   void undeploy(String sEARName) throws Exception;
   
   /**
    * Installs the application server resources.
    */
   void install() throws Exception;
   
   /**
    * Uninstalls the application server resources.
    */
   void uninstall() throws Exception;
}
