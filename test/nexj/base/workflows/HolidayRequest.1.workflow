<Workflow caption="Holiday Request" class="HRRequest" description="Workflow around a holiday request" layout="startX:274;endY:659;startY:16;endX:442" variables="cnt">
   <Action layout="y:55;srcAssocAnchorPos:1024,88;x:199" name="forward to manager for approval"><![CDATA[
         (WorkItem'new
            (: actor (@ applicant manager))
            (: name "manager's decision")
            (: description "Holiday request")
            (: hrRequest this)
         )
      ]]></Action>
   <Action condition="(and (not (null? (@ approved))) (= (@ reviewed) #t))" event="update" layout="y:110;x:223" name="manager's decision"><![CDATA[
         (for-each (lambda (wi) (wi'delete)) (@ workItems))
         (set! cnt 1)
      ]]></Action>
   <Decision layout="y:222;mergeX:436;mergeY:541;x:270" name="branch on manager's decision">
      <Branch condition="(@ approved)" layout="assoc0:141,241" name="if approved">
         <Join layout="y:325;srcAssoc0:293,560;x:56" name="escalationJoin" type="any">
            <Activity>
               <Join layout="y:12;x:241" name="approvedJoin">
                  <Activity>
                     <Action layout="y:4;x:0" name="notify HR about approval"><![CDATA[
                           (WorkItem'new
                              (: actor (read-instance Principal '() '(= name "users") #f))
                              (: name "HR approval notification")
                              (: description "Holiday request approval notification")
                              (: hrRequest this)
                           )
                        ]]></Action>
                     <Action condition="(= (@ reviewed) #t)" event="update" layout="y:49;x:7" name="HR approval notification"><![CDATA[
                           (for-each (lambda (wi) (if (= (wi'name) "HR approval notification") (wi'delete))) (@ workItems))
                           (set! cnt (+ cnt 1))
                        ]]></Action>
                  </Activity>
                  <Activity>
                     <Action layout="y:4;x:163" name="notify applicant about approval"><![CDATA[
                           (WorkItem'new
                              (: actor (@ applicant))
                              (: name "applicant approval notification")
                              (: description "Holiday request approval notification")
                              (: hrRequest this)
                           )
                        ]]></Action>
                     <Action condition="(= (@ reviewed) #t)" event="update" layout="y:49;x:167" name="applicant approval notification"><![CDATA[
                           (for-each (lambda (wi) (if (= (wi'name) "applicant approval notification") (wi'delete))) (@ workItems list))
                           (set! cnt (+ cnt 1))
                        ]]></Action>
                  </Activity>
               </Join>
            </Activity>
            <Activity>
               <Timeout layout="srcAssocAnchorPos:1024,106;y:1;srcAssoc0:147,401;x:0" name="approve by default if no action is taken ;-)" value="120000"/>
               <Action layout="y:72;x:27" name="approved by default"><![CDATA[
                     (for-each (lambda (wi) (wi'delete)) (@ workItems))
                     (set! cnt (+ cnt 1))
                  ]]></Action>
            </Activity>
         </Join>
      </Branch>
      <Branch name="if disapproved">
         <Action layout="srcAssocAnchorPos:1024,106;y:221;x:686" name="notify applicant about disapproval"><![CDATA[
               (WorkItem'new
                  (: actor (@ applicant))
                  (: name "applicant disapproval notification")
                  (: description "Holiday request disapproval notification")
                  (: hrRequest this)
               )
            ]]></Action>
         <Action condition="(= (@ reviewed) #t)" event="update" layout="y:328;srcAssoc0:792,560;x:693" name="applicant disapproval notification"><![CDATA[
               (for-each (lambda (wi) (wi'delete)) (@ workItems))
               (set! cnt (+ cnt 1))
            ]]></Action>
      </Branch>
   </Decision>
   <Action layout="y:596;x:386" name="step counter logging"><![CDATA[
      (logger'info "Step counter =" cnt)
      ]]></Action>
</Workflow>
