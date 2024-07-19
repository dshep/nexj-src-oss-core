// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import nexj.core.meta.integration.Channel;

/**
 * An object with which a Channel can be associated.
 */
public interface ChannelAware
{
   /**
    * Set the Channel.
    * @param channel The channel to associate with this.
    */
   public void setChannel(Channel channel);
}
