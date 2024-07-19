package nexj.core.rpc.cluster;

import nexj.core.util.J2EEUtil;

/**
 * Node manager component.
 */
public class NodeManager
{
   // operations

   /**
    * @return Current node name.
    */
   public String getHTTPNode()
   {
      return J2EEUtil.ISOLATED_NODE_NAME;
   }
}
