This directory contains a patch for JBoss Application Server version 5.1.0.GA
distributed under LGPL by JBoss, a division of Red Hat.
  http://www.redhat.com, http://www.redhat.com/jboss
  http://www.jboss.org, http://www.jboss.org/jbossas


jboss-deployers-impl-2.0.7.GA.patch:
  This patch patches the source code of the jboss-deployers-impl project, version 2.0.7.GA.
  It is available for download from:
    http://anonsvn.jboss.org/repos/jbossas/projects/jboss-deployers/tags/2.0.7.GA/deployers-impl

  It resolves the issue of undeploying components during server shutdown in an incorrect order.
  Further details are available at:
    http://community.jboss.org/message/259847#259847
    https://jira.jboss.org/browse/JBDEPLOY-218


jbossxb-2.0.1.GA.patch:
  This patch patches the source code of the jbossxb (jboss xml binding) project, version 2.0.1.GA
  It is available for download from:
    http://anonsvn.jboss.org/repos/common/jbossxb/tags/2.0.1.GA

  It resolves the issue of exceptions thrown during server startup if a conflicting xml parser library
  is included in an application archive deployed on the server. Further details are available at:
    http://community.jboss.org/message/205307#205307
    http://community.jboss.org/message/407714#407714
    https://jira.jboss.org/browse/JBAS-7210


jboss-common-jdbc-wrapper-5.1.0.GA.patch:
  This patch patches the source code of the connector project, version 5.1.0.GA

  It resolves the issue of original exceptions missing from stack trace if the ManagedConnection is
  already closed.


jboss-jca-5.1.0.GA.patch
  This patch patches the source code of jboss-jca, version 5.1.0.GA

  It resolves the issue of transactions unable to retrieve their associated connections if the said
  connections were idle longer than <idle-timeout-minutes/>. Further details are available at:
  https://issues.jboss.org/browse/JBAS-8367
  https://issues.jboss.org/browse/JBPAPP-4964


jbossjts-5.1.0.GA.patch
  This patch patches the source code of jbossjts, version 5.1.0.GA

  It resolves the issue of recovery attempts never stopping polluting the logs even though the recovery
  can never be completed due to the fact that the XAResource that participated in the failed transaction
  during recovery returns an empty array of XIDs. Further details are available at:
  http://community.jboss.org/thread/167103?tstart=0


jbossjts-integration-5.1.0.GA.patch
  This patch patches the source code of jbossjts-integration, version 5.1.0.GA

  It resolves the issue of inability to specify credentials for datasources for recovery.

jboss-5.1.0.GA.patch
  This patch patches the source code of jboss, version 5.1.0.GA.
  This is a backport of a fix that was made in a later version of jboss (6.0.0.M1)
  
  It resolves the issue of JMX MBean name collisions resulting from having ejb modules
  with the same name included in different ear archives. Further details are available at:
  https://community.jboss.org/thread/65092?tstart=0
  https://issues.jboss.org/browse/JBAS-6335

For convenience, the jars with the patches already applied are located in: ../../config/jboss/lib and ../../config/jboss/common/lib
These jars should be placed in <JBOSS_HOME>/lib and <JBOSS_HOME>/common/lib respectively replacing the same jars shipped with JBoss 5.1.0.