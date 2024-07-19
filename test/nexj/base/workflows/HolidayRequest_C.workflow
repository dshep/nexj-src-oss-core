<Workflow caption="Holiday Request" class="HRRequest" description="Workflow around a holiday request" layout="startX:261;endY:370;startY:6;endX:645">
   <Queue caption="Review a holiday request" layout="y:81;x:190" name="Review by the manager" owner="(@ applicant manager)">
      <ClassEvent condition="(= (@ approved) #t)" event="update" layout="assocAnchorPos:1024,82" name="approved">
         <Join layout="y:212;srcAssoc0:273,382;x:38" name="approvedJoin">
            <Activity>
               <Queue caption="Review an approved holiday request" layout="y:1;x:0" name="Review by the HR" owner="(@ applicant manager)">
                  <ManualEvent layout="assocAnchorPos:1024,169" name="reviewed"/>
                  <TimerEvent layout="assocAnchorPos:1024,46" name="timedout" value="120000"/>
               </Queue>
            </Activity>
            <Activity>
               <Queue caption="Your holiday request has been approved" layout="y:0;x:234" name="Review by the applicant" owner="(@ applicant)">
                  <ManualEvent layout="assocAnchorPos:1024,58" name="reviewed"/>
                  <TimerEvent layout="assocAnchorPos:1024,160" name="timedout" value="120000"/>
               </Queue>
            </Activity>
         </Join>
      </ClassEvent>
      <ClassEvent condition="(= (@ approved) #f)" event="update" name="disapproved">
         <Queue caption="Your holiday request has been disapproved" layout="y:82;x:534" name="Applicant notification" owner="(@ applicant)">
            <ManualEvent layout="assocAnchorPos:1024,142" name="reviewed"/>
         </Queue>
      </ClassEvent>
   </Queue>
</Workflow>
