<Workflow class="WorkflowTrace" event="goSemaphore" layout="startSrcAssocAnchorPos:1024,14;startX:285;endY:450;startY:22;endX:301" variables="exception wf assignment">
   <TryCatch caption="Outer Try" layout="w:483;h:316;y:93;x:145" name="outerTry" variable="exception">
      <Try>
         <Action caption="Before Semaphore" layout="y:15;x:102" name="scrBeforeSemaphore"><![CDATA[(this'traceAppend "BEFORE;")]]></Action>
         <Semaphore factory="AssignmentSubclass 'createWithArg &quot;Hello, World!&quot;" owner="(User'getUser &quot;jtest&quot;)" caption="Semaphore" layout="w:225;h:172;srcAssocAnchorPos:1024,119;y:63;x:37" name="semaphore" queue="(this'queueName)">
            <Action caption="Throw Exceptions" layout="y:45;x:52" name="throwBlock"><![CDATA[(import 'java.lang.IllegalStateException)
(import 'java.lang.NumberFormatException)

(this'traceAppend "EXECUTE_SEMAPHORE;")

(set! wf ((SysWorkflow'forInstance this) 0))
(set! assignment ((wf'assignments) 0))

(unless (= (SysWorkflowAssignment'RUNNING) (assignment'status))
   (error "Status is not equal to RUNNING!")
)

(cond
   ((this'throwOuterEx) (throw (java.lang.NumberFormatException'new "Outer Ex")) )
   ((this'throwUncaughtEx) (error "Uncaught Ex") )
)
]]></Action>
            <Action caption="Done Semaphore" layout="y:92;x:53" name="scrDoneSemaphore"><![CDATA[(this'traceAppend "END_SEMAPHORE;")]]></Action>
         </Semaphore>
         <Action caption="After Semaphore" layout="y:249;x:117" name="scrAfterSemaphore"><![CDATA[(this'traceAppend "AFTER;")]]></Action>
      </Try>
      <Catch exception="java.lang.NumberFormatException" name="catchOuter">
         <Action caption="Catch Outer" layout="y:242;x:737" name="scrCatchOuter"><![CDATA[(this'traceAppend "CAUGHT_OUTER;")]]></Action>
      </Catch>
   </TryCatch>
</Workflow>
