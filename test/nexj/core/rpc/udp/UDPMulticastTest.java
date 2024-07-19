package nexj.core.rpc.udp;

import nexj.core.meta.integration.channel.udp.UDPChannel;
import nexj.core.util.RandUtil;

public class UDPMulticastTest extends UDPTest
{
   /**
    * @see nexj.core.rpc.udp.UDPTest#init(nexj.core.meta.integration.channel.udp.UDPChannel)
    */
   protected void init(UDPChannel udp)
   {
      super.init(udp);

      byte[] addr = new byte[3];

      RandUtil.getSecureRandom().nextBytes(addr);
      udp.setName("UDPMulticastTest");
      udp.setGroup("239." + (addr[0] & 0xff) + "." + (addr[1] & 0xff) + "." + (addr[2] & 0xff));
      udp.setHost(udp.getGroup());
      udp.setLocalHost(null);
   }
}
