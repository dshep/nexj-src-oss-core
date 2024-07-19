This directory contains a patch for ActiveMQ version 5.3.1
distributed under Apache 2.0 License by Apache.
  http://www.apache.org, http://activemq.apache.org


activemq-core-5.3.1.patch:
  This patch patches the source code of the activemq-core project, version 5.3.1.
  It is available for download from:
    https://svn.apache.org/repos/asf/activemq/tags/activemq-5.3.1/activemq-core

  It resolves 3 issues:
    Delivering more than the configured maximum number of messages per server session.
    Further details are available at:
      http://old.nabble.com/prefetchExtension-off-by-1-for-transacted-consumers-with-prefetchSize-%3E-0-ts27866123.html
      https://issues.apache.org/activemq/browse/AMQ-2651
    
    Exception thrown when using XASession outside of an XA transaction.
    Further details are available at:
      https://issues.apache.org/activemq/browse/AMQ-2659

    JMSX_ standard properties not handled by propertyExists() and getPropertyNames() in ActiveMQMessage.
    Further details are available at:
      https://issues.apache.org/activemq/browse/AMQ-2840

For convenience, the jar with the patch already applied is located in: ../../config/activemq/lib
This jar should replace the one shipped with ActiveMQ 5.3.1.