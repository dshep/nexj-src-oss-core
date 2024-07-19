package nexj.core.meta;

/**
 * Interface implemented by metadata objects that can find
 * themselves under a different root metadata object.
 */
public interface MetadataFinder
{
   /**
    * Finds the same metadata object under a different root metadata object.
    * @param metadata The root metadata object under which to look for the metadata.
    * @return The found metadata object, or null if not found.
    */
   MetadataObject find(Metadata metadata);
}
