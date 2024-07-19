package nexj.core.rpc.http;

/**
 * Configuration object bean for an HTTP Channel to be used for authentication.
 */
public class AuthenticationChannel
{
   // attributes

   /**
    * The channel name.
    */
   protected String m_sChannel;
   
   /**
    * The enabled flag.
    */
   protected boolean m_bEnabled;

   // accessors

   /**
    * Sets the enabled flag.
    * @param bEnabled The enabled flag to set.
    */
   public void setEnabled(boolean bEnabled)
   {
      m_bEnabled = bEnabled;
   }

   /**
    * @return The enabled flag.
    */
   public boolean isEnabled()
   {
      return m_bEnabled;
   }

   /**
    * Sets the channel name.
    * @param sChannel The channel name to set.
    */
   public void setChannel(String sChannel)
   {
      m_sChannel = sChannel;
   }

   /**
    * @return The channel name.
    */
   public String getChannel()
   {
      return m_sChannel;
   }
   
}
