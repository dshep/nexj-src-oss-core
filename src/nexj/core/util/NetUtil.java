// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

/**
 * IP network-related utility functions.
 */
public class NetUtil
{
   /**
    * Prevents construction.
    */
   protected NetUtil()
   {
   }

   /**
    * Return the port to bind to taking into consideration the system defined port offset.
    * @param nPort The base port to consider (0x0 to 0xffff inclusive).
    * @return The port to use for binding, taking into consideration the system defined port offset.
    */
   public static int getBindPort(int nPort)
   {
      String sOffset = SysUtil.getConfigProperties().getProperty("port.offset");

      // dynamic bind port must remain unaltered or no offset required
      return (nPort == 0 || sOffset == null || "0".equals(sOffset))
             ? nPort : getBindPort(nPort, Integer.parseInt(sOffset));
   }

   /**
    * Return the port to bind to taking into consideration the requested port offset.
    * @param nPort The base port to consider (0x0 to 0xffff inclusive).
    * @param nOffset The offset to apply to the requested port.
    * @return The port to use for binding, taking into consideration the system defined port offset.
    */
   public static int getBindPort(int nPort, int nOffset)
   {
      if (nPort == 0 || nOffset == 0)
      {
         return nPort; // dynamic bind port must remain unaltered or no offset required
      }

      assert nPort >= 0 && nPort < 0x10000;

      int nDelta = 1; // super-user reserved start port, 0'th port skipped since special value
      int nRange = 0x400 - nDelta; // super-user reserved end port - 0'th port

      if (nPort >= 0x400) // port outside super-user reserved port range
      {
         nDelta = 0x400;
         nRange = 0x10000 - nDelta;
      }

      nPort += nOffset - nDelta; // subtract reserved ports from nPort and compute bindPort - nDelta
      nPort %= nRange; // compute remainder

      if (nPort < 0)
      {
         nPort += nRange; // convert remainder to modulus
      }

      return nPort + nDelta;
   }

   /**
    * @return The node DNS name.
    */
   public static String getNodeName()
   {
      try
      {
         return (String)AccessController.doPrivileged(new PrivilegedExceptionAction()
         {
            public Object run() throws Exception
            {
               return InetAddress.getLocalHost().getHostName();
            }
         });
      }
      catch (Exception e)
      {
         return "localhost";
      }
   }
   
   /**
    * Return a collection of all InetAddresses on this host
    * @return a collection of InetAddresses
    * @throws SocketException in case of a network error
    */
   public static List getInetAddresses() throws SocketException
   {
      try
      {
         return (List)AccessController.doPrivileged(new PrivilegedExceptionAction()
         {
            public Object run() throws Exception
            {
               List addresses = new ArrayList();
               
               for (Enumeration networkEnum = NetworkInterface.getNetworkInterfaces(); networkEnum.hasMoreElements(); )
               {
                  NetworkInterface network = (NetworkInterface)networkEnum.nextElement();
                  
                  for (Enumeration inetEnum = network.getInetAddresses(); inetEnum.hasMoreElements(); )
                  {
                     InetAddress nextAddress = (InetAddress)inetEnum.nextElement();
                     
                     addresses.add(nextAddress);
                  }
               }
               
               return addresses;
            }
         });
      }
      catch (PrivilegedActionException e)
      {
         Exception cause = e.getException();
         
         if (cause instanceof SocketException)
         {
            throw (SocketException)cause;
         }

         throw ObjUtil.rethrow(cause);
      }
   }
}
