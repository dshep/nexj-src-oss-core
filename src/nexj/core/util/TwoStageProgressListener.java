package nexj.core.util;

/**
 * Interface for handling progress notifications of a nested operation separately from the progress notifications of the outer operation.
 */
public interface TwoStageProgressListener extends ProgressListener
{
   /**
    * @return The progress listener to use for handling progress notifications of the nested operation.
    */
   public ProgressListener getSecondStageProgressListener();
}