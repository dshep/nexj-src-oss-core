<Workflow class="Patient" event="triggerTestStart" layout="startSrcAssocAnchorPos:1024,14;startX:293;endY:435;startY:6;endX:290" variables="savedThis savedChild">
   <Action caption="Initialize" layout="y:58;x:267" name="scrInit"><![CDATA[(set! savedThis this)
(set! savedChild
   (if (= ((this'children)'size) 1)
      ((this'children) 0)
      (error "More than one child")
   )
)]]></Action>
   <Queue layout="y:137;x:270" name="queue" queue="&quot;Semaphore&quot;">
      <ClassEvent association="children" condition="(begin (unless (eq? savedChild this) (error &quot;Triggered with incorrect activity token {0}, expected {1}&quot; this savedChild)) #t)" event="triggerTest" name="classEvent">
         <Action caption="Verify Activity Token" layout="y:224;x:236" name="scrVerifyThis"/>
         <Action association="children" caption="Wait for Event on Association" event="triggerTest" layout="y:297;x:214" name="scrWaitOnAssocEvent"/>
         <Action caption="Wait for Event on This" event="triggerTest" layout="srcAssocAnchorPos:1024,80;y:356;x:227" name="scrWaitOnThisEvent"/>
      </ClassEvent>
   </Queue>
</Workflow>
