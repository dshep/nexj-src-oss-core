<Workflow class="WorkflowTrace" event="goProcessEvent" layout="startX:143;endY:546;startY:9;endX:295" variables="wf assignment">
   <Action layout="y:60;x:224" name="scrBegin"><![CDATA[(this'traceAppend "BEGIN;")]]></Action>
   <Queue layout="y:168;x:250" name="queue" queue="(this'queueName)">
      <TimerEvent name="timerEvent" value="30000">
         <Action caption="Run Anyway" layout="y:159;srcAssoc0:100,461;x:15" name="scrTimeOut"><![CDATA[(this'traceAppend "TIMED_OUT;")]]></Action>
      </TimerEvent>
      <ProcessEvent layout="w:173;h:133;y:250;x:190" name="processEvent">
         <Action caption="Run Batch Task" layout="y:40;x:14" name="scrProcessEvent"><![CDATA[(this'traceAppend "PROCESS_EVENT;")

(set! wf ((SysWorkflow'forInstance this) 0))
(set! assignment ((wf'assignments) 0))

(unless (= (SysWorkflowAssignment'RUNNING) (assignment'status))
   (error "Status is not equal to RUNNING!")
)

(this'traceAppend "PROCESS_EVENT_DONE;")]]></Action>
      </ProcessEvent>
      <ClassEvent event="resumeProcessEvent" name="classEvent">
         <Action layout="srcAssocAnchorPos:1024,0;y:173;srcAssoc0:528,459;x:475" name="scrClassEvent"><![CDATA[(this'traceAppend "CLASS_EVENT;")]]></Action>
      </ClassEvent>
   </Queue>
   <Action layout="y:447;x:273" name="scrEnd"><![CDATA[(this'traceAppend "END;")]]></Action>
</Workflow>
