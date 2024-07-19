package nexj.core.meta;

/**
 * An interface for objects with which a MetadataLoader can be associated
 */
public interface MetadataLoaderAware
{
   /**
    * Sets the MetadataLoader.
    * @param metadataLoader The metadata loader to associate with this object.
    */
   public void setMetadataLoader(MetadataLoader metadataLoader);
}