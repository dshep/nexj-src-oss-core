package nexj.core.meta.xml;

import java.util.Properties;
import java.util.Set;

import org.w3c.dom.Element;

/**
 * External metadata loading hook.
 */
public interface XMLMetadataLoaderHook
{
   // operations

   /**
    * Initialization.
    * @param loader The current loader to wrap.
    * @param properties The environment properties.
    */
   public abstract void initialize(XMLMetadataLoader loader, Properties properties);

   /**
    * Loads a channel from a DOM element.
    * @param channelElement The DOM element containing the channel.
    * @param sName The data source name.
    * @return True, if channel is loaded.
    */
   public abstract boolean loadChannel(Element channelElement, String sName);

   /**
    * Loads a connection channel from a DOM element.
    * @param connectionElement The DOM element containing channel connection.
    * @param sConnectionsName The name of the environment.
    * @param sChannelName The connection channel name.
    * @param connectionSet Set of loaded connections.
    * @return True, if connection channel is loaded.
    */
   public abstract boolean loadConnection(Element connectionElement, String sConnectionsName,
      String sChannelName, Set connectionSet);
}
