<Workflow class="WorkflowTrace" event="goForkTry" layout="startSrcAssocAnchorPos:1024,14;startX:344;endY:576;startY:28;endX:425" variables="exception">
   <Action layout="y:62;x:320" name="scrStart"><![CDATA[(this'traceAppend "scrStart;")]]></Action>
   <TryCatch layout="w:615;h:390;y:103;x:126" name="tryCatch1">
      <Try>
         <Join layout="y:42;x:62" name="join">
            <Activity>
               <Action layout="y:32;x:0" name="scrLeft1"><![CDATA[;(define wf ((SysWorkflow'forInstance this) 0))
;(define state (wf'state))
;(define step '())

(this'traceAppend "scrLeft1;")

;(logger'info "STEPS: " (state'stepNames))
;(set! step ((state'lastStep)'next))
;(state'remove step)
;(state'add step)
;(logger'info "STEPS: " (state'stepNames))]]></Action>
               <Action layout="y:95;x:5" name="scrLeft2"><![CDATA[(this'traceAppend "scrLeft2;")]]></Action>
               <Action layout="y:147;x:5" name="scrLeft3"><![CDATA[(this'traceAppend "scrLeft3;")]]></Action>
               <Action layout="y:204;x:2" name="scrLeft4"><![CDATA[(import 'java.lang.IllegalStateException)
(this'traceAppend "scrLeft4;")
(if (this'throwInnerEx)
   (throw (java.lang.IllegalStateException'new "Expected Exception"))
)
(if (this'throwUncaughtEx)
   (error "Uncaught Ex")
)]]></Action>
            </Activity>
            <Activity>
               <TryCatch layout="w:289;h:248;y:0;x:91" name="tryCatch">
                  <Try>
                     <Action layout="y:23;x:74" name="scrRight1"><![CDATA[(this'traceAppend "scrRight1;")]]></Action>
                     <Action layout="y:92;x:74" name="scrRight2"><![CDATA[(this'traceAppend "scrRight2;")]]></Action>
                     <Action layout="y:158;x:74" name="scrRight3"><![CDATA[(this'traceAppend "scrRight3;")]]></Action>
                  </Try>
                  <Finally><![CDATA[(this'traceAppend "scrInnerFinally;")]]></Finally>
               </TryCatch>
            </Activity>
            <Activity>
               <Queue layout="y:64;x:417" name="queue" queue="&quot;Semaphore&quot;">
                  <ClassEvent event="resumeForkTry" name="classEvent"/>
               </Queue>
            </Activity>
         </Join>
      </Try>
      <Catch exception="java.lang.IllegalStateException" name="catch">
         <Action layout="y:279;x:830;srcAssoc0:875,539" name="scrCatch"><![CDATA[(this'traceAppend "scrCatch;")]]></Action>
      </Catch>
      <Finally><![CDATA[(this'traceAppend "scrOuterFinally;")]]></Finally>
   </TryCatch>
   <Action layout="y:519;x:397" name="scrFinish"><![CDATA[(this'traceAppend "scrFinish;")]]></Action>
</Workflow>
