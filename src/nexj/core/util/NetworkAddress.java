// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Arrays;

/**
 * A network address with a mask.
 */
public class NetworkAddress
{
   // attributes

   /**
    * Masked raw address.
    */
   protected final byte[] m_rawAddress;

   /**
    * Mask length.
    */
   protected final int m_nMaskLength;

   // associations

   /**
    * Network type.
    */
   protected final Class m_type;

   // constructors

   /**
    * Constructs a universal network address (accepts all addresses).
    */
   public NetworkAddress()
   {
      m_type = null;
      m_rawAddress = null;
      m_nMaskLength = 0;
   }

   /**
    * Constructs the address.
    * @param address The InetAddress.
    * @param nMaskLength Mask length.
    * @throws IllegalArgumentException if the mask length is invalid.
    */
   public NetworkAddress(InetAddress address, int nMaskLength)
   {
      this(address.getAddress(), nMaskLength, address.getClass());
   }

   /**
    * Constructs the address.
    * @param rawAddress The raw address.
    * @param nMaskLength Mask length.
    * @param type The address type.
    * @throws IllegalArgumentException if the mask length is invalid.
    */
   public NetworkAddress(byte[] rawAddress, int nMaskLength, Class type)
   {
      m_rawAddress = rawAddress;

      if (nMaskLength < 0 || nMaskLength > m_rawAddress.length * 8)
      {
         throw new IllegalArgumentException("Invalid mask length " + nMaskLength);
      }

      m_nMaskLength = nMaskLength;
      mask(m_rawAddress, m_nMaskLength);
      m_type = type;
   }

   // operations

   /**
    * Return whether this network contains a given InetAddress.
    * @param address The address.
    * @return True if the address matches this network.
    */
   public boolean contains(InetAddress address)
   {
      if (address.isLoopbackAddress())
      {
         return false;
      }

      if (m_type == null)
      {
         return true;
      }

      if (address.getClass() != m_type)
      {
         return false; // Different network types
      }

      byte[] rawAddress = address.getAddress();

      mask(rawAddress, m_nMaskLength);

      return Arrays.equals(rawAddress, m_rawAddress);
   }

   /**
    * Masks off lower bits in a byte array.
    * @param byteArray The byte array.
    * @param nMaskLength Number of bits to mask off.
    */
   protected static void mask(byte[] byteArray, int nMaskLength)
   {
      int i = nMaskLength >> 3;

      if (i < byteArray.length)
      {
         byteArray[i] &= -1 << (8 - (nMaskLength & 7));

         while (++i < byteArray.length)
         {
            byteArray[i] = 0;
         }
      }
   }

   /**
    * Parses the network address from a string representation.
    * @param sNetwork The IP network address to match IP address for multi-homed hosts
    * e.g. 192.168.0.0/22 or 2001:DB8::/48.
    * @return The network address.
    * @throws UnknownHostException for invalid sNetwork values.
    */
   public static NetworkAddress parse(String sNetwork) throws UnknownHostException
   {
      if (StringUtil.isEmpty(sNetwork))
      {
         return new NetworkAddress();
      }

      try
      {
         int nMaskIndex = sNetwork.lastIndexOf('/');
         InetAddress address = InetAddress.getByName((nMaskIndex < 0) ? sNetwork
            : sNetwork.substring(0, nMaskIndex));
         byte[] rawAddress = address.getAddress();

         return new NetworkAddress(rawAddress,
            (nMaskIndex < 0) ? rawAddress.length * 8 :
            Integer.parseInt(sNetwork.substring(nMaskIndex + 1)),
            address.getClass());
      }
      catch (UnknownHostException e)
      {
         throw e;
      }
      catch (Exception e)
      {
         throw new UnknownHostException(e.getMessage());
      }
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (!(obj instanceof NetworkAddress))
      {
         return false;
      }

      NetworkAddress a = (NetworkAddress)obj;

      return Arrays.equals(m_rawAddress, a.m_rawAddress) && m_nMaskLength == a.m_nMaskLength;
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      if (m_type == null)
      {
         return 0;
      }

      return Arrays.hashCode(m_rawAddress) ^ m_nMaskLength;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      if (m_type == null)
      {
         return "";
      }

      try
      {
         return InetAddress.getByAddress(m_rawAddress).getHostAddress() + "/" + m_nMaskLength;
      }
      catch (UnknownHostException e)
      {
         return "";
      }
   }
}
