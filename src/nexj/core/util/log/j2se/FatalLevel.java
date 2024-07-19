// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.log.j2se;

import java.util.logging.Level;

/**
 * Fatal level implementation
 */
public class FatalLevel extends Level
{
   // constants
   
   /**
    * The serial version UID.
    */
   private final static long serialVersionUID = -7963681999263856895L;

   /**
    * The level instance.
    */
   public final static Level FATAL = new FatalLevel();
   
   // constructors
   
   public FatalLevel()
   {
      super("FATAL", 1100);
   }
}
