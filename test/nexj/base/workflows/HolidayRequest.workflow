<Workflow caption="Holiday Request" class="HRRequest" description="Workflow around a holiday request" layout="startX:332;endY:617;startY:5;endX:740" variables="cnt">
   <Action layout="y:59;x:247" name="forward to manager for approval"><![CDATA[
         (WorkItem'new
            (: actor (@ applicant manager))
            (: name "manager's decision")
            (: description "Holiday request")
            (: hrRequest this)
         )
      ]]></Action>
   <Action condition="(and (not (null? (@ approved))) (= (@ reviewed) #t))" event="update" layout="y:110;x:277" name="manager's decision"><![CDATA[
         (for-each (lambda (wi) (wi'delete)) (@ workItems))
         (set! cnt 1)
      ]]></Action>
   <Decision layout="y:209;mergeX:733;mergeY:496;x:324" name="branch on manager's decision">
      <Branch condition="(@ approved)" name="if approved">
         <Join layout="y:287;x:32;srcAssoc0:349,515" name="escalationJoin" type="any">
            <Activity>
               <Join layout="y:0;x:244" name="approvedJoin">
                  <Activity>
                     <Action name="notify HR about approval"><![CDATA[
                           (WorkItem'new
                              (: actor (read-instance Principal '() '(= name "users") #f))
                              (: name "HR approval notification")
                              (: description "Holiday request approval notification")
                              (: hrRequest this)
                           )
                        ]]></Action>
                     <Action condition="(= (@ reviewed) #t)" event="update" layout="y:46;x:5" name="HR approval notification"><![CDATA[
                           (for-each (lambda (wi) (if (= (wi'name) "HR approval notification") (wi'delete))) (@ workItems))
                           (set! cnt (+ cnt 1))
                        ]]></Action>
                  </Activity>
                  <Activity>
                     <Action layout="y:0;x:183" name="notify applicant about approval"><![CDATA[
                           (WorkItem'new
                              (: actor (@ applicant))
                              (: name "applicant approval notification")
                              (: description "Holiday request approval notification")
                              (: hrRequest this)
                           )
                        ]]></Action>
                     <Action condition="(= (@ reviewed) #t)" event="update" layout="y:48;x:186" name="applicant approval notification"><![CDATA[
                           (for-each (lambda (wi) (if (= (wi'name) "applicant approval notification") (wi'delete))) (@ workItems list))
                           (set! cnt (+ cnt 1))
                        ]]></Action>
                  </Activity>
               </Join>
            </Activity>
            <Activity>
               <Timeout layout="y:22;x:0" name="approve by default if no action is taken ;-)" value="120000"/>
               <Action layout="y:86;x:53" name="approved by default"><![CDATA[
                     (for-each (lambda (wi) (wi'delete)) (@ workItems))
                     (set! cnt (+ cnt 1))
                  ]]></Action>
            </Activity>
         </Join>
      </Branch>
      <Branch name="if disapproved">
         <Action layout="y:215;x:650" name="notify applicant about disapproval"><![CDATA[
               (WorkItem'new
                  (: actor (@ applicant))
                  (: name  "applicant disapproval notification")
                  (: description "Holiday request disapproval notification")
                  (: hrRequest this)
               )
            ]]></Action>
         <Action condition="(= (@ reviewed) #t)" event="update" layout="y:288;srcAssocAnchorPos:1024,67;x:676" name="applicant disapproval notification"><![CDATA[
               (for-each (lambda (wi) (wi'delete)) (@ workItems))
               (set! cnt (+ cnt 1))
            ]]></Action>
      </Branch>
   </Decision>
   <Action layout="y:561;x:687" name="step counter logging"><![CDATA[
      (logger'info "Step counter =" cnt)
      ]]></Action>
</Workflow>
