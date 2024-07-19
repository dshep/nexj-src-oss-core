package nexj.core.meta;

/**
 * Interface implemented by stand-alone metadata resources.
 */
public interface MetadataResource
{
   /**
    * Sets the resource name.
    * @param sName The resource name. Can be null, if unknown.
    */
   void setResourceName(String sName);

   /**
    * @return The resource name. Can be null, if unknown.
    */
   String getResourceName();
}
