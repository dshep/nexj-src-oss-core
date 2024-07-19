<Workflow class="WorkflowTrace" event="goForkSemaphore" layout="startSrcAssocAnchorPos:1024,14;startX:433;endY:448;startY:26;endX:441" variables="exception">
   <Action layout="y:60;x:409" name="scrStart"><![CDATA[(this'traceAppend "scrStart;")]]></Action>
   <Join layout="y:145;x:188" name="join">
      <Activity>
         <Semaphore caption="semaphoreLeftCaption" layout="w:218;h:146;y:0;x:0" name="semaphoreLeft" queue="SemaphoreLowConcurrency">
            <Action layout="y:41;x:57" name="scrLeft1"><![CDATA[(this'traceAppend "scrLeft1;")
(if (this'throwInnerEx)
   (error "LEFT Uncaught Exception")
)]]></Action>
         </Semaphore>
      </Activity>
      <Activity>
         <Semaphore caption="semaphoreRightCaption" layout="w:231;h:151;y:0;x:300" name="semaphoreRight" queue="SemaphoreLowConcurrency" title="(string-append &quot;semaphoreRight:&quot; (cast string (+ 1 1)))">
            <Action layout="y:36;x:72" name="scrRight1"><![CDATA[(this'traceAppend "scrRight1;")
(if (this'throwOuterEx)
   (error "RIGHT Uncaught Exception")
)]]></Action>
         </Semaphore>
      </Activity>
   </Join>
   <Action layout="y:391;x:417" name="scrFinish"><![CDATA[(this'traceAppend "scrFinish;")]]></Action>
</Workflow>
